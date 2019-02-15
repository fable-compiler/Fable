// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

// Open up the compiler as an incremental service for parsing,
// type checking and intellisense-like environment-reporting.

namespace Microsoft.FSharp.Compiler.SourceCodeServices

#nowarn "1182"

open Internal.Utilities
open Internal.Utilities.Collections
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Control

open System
open System.Text
open System.Threading
open System.Collections.Concurrent
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.AbstractIL
open Microsoft.FSharp.Compiler.AbstractIL.IL
open Microsoft.FSharp.Compiler.AbstractIL.ILBinaryReader
open Microsoft.FSharp.Compiler.AbstractIL.Diagnostics
open Microsoft.FSharp.Compiler.AbstractIL.Internal
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library

open Microsoft.FSharp.Compiler.AccessibilityLogic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.CompileOps
open Microsoft.FSharp.Compiler.CompileOptions
open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.Lib
open Microsoft.FSharp.Compiler.ReferenceResolver
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.Parser
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.Lexhelp
open Microsoft.FSharp.Compiler.Layout
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.Tastops
open Microsoft.FSharp.Compiler.Tastops.DebugPrint
open Microsoft.FSharp.Compiler.TcGlobals
open Microsoft.FSharp.Compiler.Infos
open Microsoft.FSharp.Compiler.InfoReader
open Microsoft.FSharp.Compiler.NameResolution
open Microsoft.FSharp.Compiler.TypeChecker


//-------------------------------------------------------------------------
// InteractiveChecker
//-------------------------------------------------------------------------

type internal TcResult = TcEnv * TopAttribs * TypedImplFile option * ModuleOrNamespaceType
type internal TcErrors = FSharpErrorInfo[]

type InteractiveChecker internal (tcConfig, tcGlobals, tcImports, tcInitialState, ctok, reactorOps, parseCache, checkCache) =
    let userOpName = "Unknown"

    static member internal BuildTcImports (tcConfig: TcConfig, references: string[], readAllBytes: string -> byte[]) =
        let tcImports = TcImports ()
        let ilGlobals = IL.EcmaMscorlibILGlobals

        let sigDataReaders ilModule =
            [ for resource in ilModule.Resources.AsList do
                if IsSignatureDataResource resource then 
                    let ccuName = GetSignatureDataResourceName resource
                    yield resource.GetBytes() ]

        let optDataReaders ilModule =
            [ for resource in ilModule.Resources.AsList do
                if IsOptimizationDataResource resource then
                    let ccuName = GetOptimizationDataResourceName resource
                    yield resource.GetBytes() ]

        let LoadMod (ccuName: string) =
            let fileName =
                if ccuName.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
                then ccuName
                else ccuName + ".dll"
            let bytes = readAllBytes fileName
            let opts: ILReaderOptions =
                  { ilGlobals = ilGlobals
                    metadataOnly = MetadataOnlyFlag.Yes
                    reduceMemoryUsage = ReduceMemoryFlag.Yes
                    pdbDirPath = None
                    tryGetMetadataSnapshot = (fun _ -> None) }

            let reader = ILBinaryReader.OpenILModuleReaderFromBytes fileName bytes opts
            reader.ILModuleDef //reader.ILAssemblyRefs

        let GetSignatureData (fileName:string, ilScopeRef, ilModule:ILModuleDef option, bytes:byte[]) = 
            TastPickle.unpickleObjWithDanglingCcus fileName ilScopeRef ilModule TastPickle.unpickleCcuInfo bytes

        let GetOptimizationData (fileName:string, ilScopeRef, ilModule:ILModuleDef option, bytes:byte[]) = 
            TastPickle.unpickleObjWithDanglingCcus fileName ilScopeRef ilModule Optimizer.u_CcuOptimizationInfo bytes

        let memoize_mod = new MemoizationTable<_,_> (LoadMod, keyComparer=HashIdentity.Structural)

        let LoadSigData ccuName =
            let ilModule = memoize_mod.Apply ccuName
            let ilShortAssemName = ilModule.ManifestOfAssembly.Name
            let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssemblyRef ilShortAssemName)
            let fileName = ilModule.Name //TODO: try with ".sigdata" extension
            match sigDataReaders ilModule with
            | [] -> None
            | bytes::_ -> Some (GetSignatureData (fileName, ilScopeRef, Some ilModule, bytes))

        let LoadOptData ccuName =
            let ilModule = memoize_mod.Apply ccuName
            let ilShortAssemName = ilModule.ManifestOfAssembly.Name
            let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssemblyRef ilShortAssemName)
            let fileName = ilModule.Name //TODO: try with ".optdata" extension
            match optDataReaders ilModule with
            | [] -> None
            | bytes::_ -> Some (GetOptimizationData (fileName, ilScopeRef, Some ilModule, bytes))

        let memoize_sig = new MemoizationTable<_,_> (LoadSigData, keyComparer=HashIdentity.Structural)
        let memoize_opt = new MemoizationTable<_,_> (LoadOptData, keyComparer=HashIdentity.Structural)

        let GetCustomAttributesOfIlModule (ilModule: ILModuleDef) = 
            (match ilModule.Manifest with Some m -> m.CustomAttrs | None -> ilModule.CustomAttrs).AsList 

        let GetAutoOpenAttributes ilg ilModule = 
            ilModule |> GetCustomAttributesOfIlModule |> List.choose (TryFindAutoOpenAttr ilg)

        let GetInternalsVisibleToAttributes ilg ilModule = 
            ilModule |> GetCustomAttributesOfIlModule |> List.choose (TryFindInternalsVisibleToAttr ilg)

        let HasAnyFSharpSignatureDataAttribute ilModule = 
            let attrs = GetCustomAttributesOfIlModule ilModule
            List.exists IsSignatureDataVersionAttr attrs

        let mkCcuInfo ilg ilScopeRef ilModule ccu : ImportedAssembly =
              { ILScopeRef = ilScopeRef
                FSharpViewOfMetadata = ccu
                AssemblyAutoOpenAttributes = GetAutoOpenAttributes ilg ilModule
                AssemblyInternalsVisibleToAttributes = GetInternalsVisibleToAttributes ilg ilModule
#if !NO_EXTENSIONTYPING
                IsProviderGenerated = false
                TypeProviders = []
#endif
                FSharpOptimizationData = notlazy None }

        let GetCcuIL m ccuName =
            let auxModuleLoader = function
                | ILScopeRef.Local -> failwith "Unsupported reference"
                | ILScopeRef.Module x -> memoize_mod.Apply x.Name
                | ILScopeRef.Assembly x -> memoize_mod.Apply x.Name
            let ilModule = memoize_mod.Apply ccuName
            let ilShortAssemName = ilModule.ManifestOfAssembly.Name
            let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssemblyRef ilShortAssemName)
            let fileName = ilModule.Name
            let invalidateCcu = new Event<_>()
            let ccu = Import.ImportILAssembly(
                        tcImports.GetImportMap, m, auxModuleLoader, ilScopeRef,
                        tcConfig.implicitIncludeDir, Some fileName, ilModule, invalidateCcu.Publish)
            let ccuInfo = mkCcuInfo ilGlobals ilScopeRef ilModule ccu
            ccuInfo, None

        let GetCcuFS m ccuName =
            let sigdata = memoize_sig.Apply ccuName
            let ilModule = memoize_mod.Apply ccuName
            let ilShortAssemName = ilModule.ManifestOfAssembly.Name
            let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssemblyRef ilShortAssemName)
            let fileName = ilModule.Name
            let GetRawTypeForwarders ilModule =
                match ilModule.Manifest with 
                | Some manifest -> manifest.ExportedTypes
                | None -> mkILExportedTypes []
#if !NO_EXTENSIONTYPING
            let invalidateCcu = new Event<_>()
#endif
            let minfo: PickledCcuInfo = sigdata.Value.RawData //TODO: handle missing sigdata
            let codeDir = minfo.compileTimeWorkingDir
            let ccuData: CcuData = 
                  { ILScopeRef = ilScopeRef
                    Stamp = newStamp()
                    FileName = Some fileName 
                    QualifiedName = Some (ilScopeRef.QualifiedName)
                    SourceCodeDirectory = codeDir
                    IsFSharp = true
                    Contents = minfo.mspec
#if !NO_EXTENSIONTYPING
                    InvalidateEvent=invalidateCcu.Publish
                    IsProviderGenerated = false
                    ImportProvidedType = (fun ty -> Import.ImportProvidedType (tcImports.GetImportMap()) m ty)
#endif
                    UsesFSharp20PlusQuotations = minfo.usesQuotations
                    MemberSignatureEquality = (fun ty1 ty2 -> Tastops.typeEquivAux EraseAll (tcImports.GetTcGlobals()) ty1 ty2)
                    TryGetILModuleDef = (fun () -> Some ilModule)
                    TypeForwarders = Import.ImportILAssemblyTypeForwarders(tcImports.GetImportMap, m, GetRawTypeForwarders ilModule)
                    }

            let optdata = lazy (
                match memoize_opt.Apply ccuName with 
                | None -> None
                | Some data ->
                    let findCcuInfo name = tcImports.FindCcu (m, name)
                    Some (data.OptionalFixup findCcuInfo) )

            let ccu = CcuThunk.Create(ilShortAssemName, ccuData)
            let ccuInfo = mkCcuInfo ilGlobals ilScopeRef ilModule ccu
            let ccuOptInfo = { ccuInfo with FSharpOptimizationData = optdata }
            ccuOptInfo, sigdata

        let rec GetCcu m ccuName =
            let ilModule = memoize_mod.Apply ccuName
            if HasAnyFSharpSignatureDataAttribute ilModule then
                GetCcuFS m ccuName
            else
                GetCcuIL m ccuName

        let fixupCcuInfo refCcusUnfixed =
            let refCcus = refCcusUnfixed |> List.map fst
            let findCcuInfo name =
                refCcus
                |> List.tryFind (fun (x: ImportedAssembly) -> x.FSharpViewOfMetadata.AssemblyName = name)
                |> Option.map (fun x -> x.FSharpViewOfMetadata)
            let fixup (data: TastPickle.PickledDataWithReferences<_>) =
                data.OptionalFixup findCcuInfo |> ignore
            refCcusUnfixed |> List.choose snd |> List.iter fixup
            refCcus

        let m = range.Zero
        let refCcusUnfixed = List.ofArray references |> List.map (GetCcu m)
        let refCcus = fixupCcuInfo refCcusUnfixed
        let sysCcus = refCcus |> List.filter (fun x -> x.FSharpViewOfMetadata.AssemblyName <> "FSharp.Core")
        let fslibCcu = refCcus |> List.find (fun x -> x.FSharpViewOfMetadata.AssemblyName = "FSharp.Core")

        let ccuInfos = [fslibCcu] @ sysCcus
        let ccuMap = ccuInfos |> List.map (fun ccuInfo -> ccuInfo.FSharpViewOfMetadata.AssemblyName, ccuInfo) |> Map.ofList

        // search over all imported CCUs for each cached type
        let ccuHasType (ccu: CcuThunk) (nsname: string list) (tname: string) =
            let findEntity (entityOpt: Entity option) n =
                match entityOpt with
                | None -> None
                | Some entity -> entity.ModuleOrNamespaceType.AllEntitiesByCompiledAndLogicalMangledNames.TryFind n
            let entityOpt = (Some ccu.Contents, nsname) ||> List.fold findEntity
            match entityOpt with
            | Some ns ->
                match Map.tryFind tname ns.ModuleOrNamespaceType.TypesByMangledName with
                | Some _ -> true
                | None -> false
            | None -> false

        // Search for a type
        let tryFindSysTypeCcu nsname typeName =
            let search = sysCcus |> List.tryFind (fun ccuInfo -> ccuHasType ccuInfo.FSharpViewOfMetadata nsname typeName)
            match search with
            | Some x -> Some x.FSharpViewOfMetadata
            | None ->
#if DEBUG
                printfn "Cannot find type %s.%s" (String.concat "." nsname) typeName
#endif
                None

        let tcGlobals = TcGlobals (
                            tcConfig.compilingFslib, ilGlobals, fslibCcu.FSharpViewOfMetadata,
                            tcConfig.implicitIncludeDir, tcConfig.mlCompatibility,
                            tcConfig.isInteractive, tryFindSysTypeCcu,
                            tcConfig.emitDebugInfoInQuotations, tcConfig.noDebugData)

#if DEBUG
        // the global_g reference cell is used only for debug printing
        do global_g := Some tcGlobals
#endif
        // do this prior to parsing, since parsing IL assembly code may refer to mscorlib
        do tcImports.SetCcuMap(ccuMap)
        do tcImports.SetTcGlobals(tcGlobals)
        tcImports, tcGlobals

    static member Create(references: string[], readAllBytes: string -> byte[], defines: string[], optimize: bool) =
        let dllName (fileName: string) =
            if fileName.EndsWith(".dll", StringComparison.OrdinalIgnoreCase)
            then fileName
            else fileName + ".dll"
        let argv = [|
            for d in defines do yield "-d:" + d
            for r in references do yield "-r:" + (dllName r)
            yield "--optimize" + (if optimize then "+" else "-")
        |]
        let options: FSharpProjectOptions =
          { ProjectFileName = "Project"
            ProjectId = None
            SourceFiles = [| |]
            OtherOptions = argv
            ReferencedProjects = [| |]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = System.DateTime.MaxValue
            UnresolvedReferences = None
            OriginalLoadReferences = []
            ExtraProjectInfo = None
            Stamp = None }

        InteractiveChecker.Create(readAllBytes, options)

    static member Create(readAllBytes: string -> byte[], options: FSharpProjectOptions) =
        let references =
            options.OtherOptions
            |> Array.filter (fun s -> s.StartsWith("-r:"))
            |> Array.map (fun s -> s.Replace("-r:", ""))

        let tcConfig =
            let tcConfigB = TcConfigBuilder.Initial
            let sourceFiles = options.SourceFiles |> Array.toList
            let argv = options.OtherOptions |> Array.toList
            let _sourceFiles = ApplyCommandLineArgs(tcConfigB, sourceFiles, argv)
            TcConfig.Create(tcConfigB, validate=false)

        let tcImports, tcGlobals =
            InteractiveChecker.BuildTcImports (tcConfig, references, readAllBytes)

        let assemblyName = "Project"
        let niceNameGen = NiceNameGenerator()
        let tcInitialEnv = GetInitialTcEnv (assemblyName, rangeStartup, tcConfig, tcImports, tcGlobals)
        let tcInitialState = GetInitialTcState (rangeStartup, assemblyName, tcConfig, tcGlobals, tcImports, niceNameGen, tcInitialEnv)

        let ctok = CompilationThreadToken()
        let reactorOps = 
            { new IReactorOperations with
                member __.EnqueueAndAwaitOpAsync (userOpName, opName, opArg, op) =
                    async.Return (Cancellable.runWithoutCancellation (op ctok))
                member __.EnqueueOp (userOpName, opName, opArg, op) = (op ctok) }

        // parse cache, keyed on file name and source hash
        let parseCache = ConcurrentDictionary<string * int, FSharpParseFileResults>(HashIdentity.Structural)
        // type check cache, keyed on file name
        let checkCache = ConcurrentDictionary<string, (TcResult * TcErrors) * (TcState * ModuleNamesDict)>(HashIdentity.Structural)

        InteractiveChecker (tcConfig, tcGlobals, tcImports, tcInitialState, ctok, reactorOps, parseCache, checkCache)

    member private x.MakeProjectResults (projectFileName: string, parseResults: FSharpParseFileResults[], tcState: TcState, errors: FSharpErrorInfo[],
                                         symbolUses: TcSymbolUses list, topAttrsOpt: TopAttribs option, tcImplFilesOpt: TypedImplFile list option) =
        let assemblyRef = mkSimpleAssemblyRef "stdin"
        let assemblyDataOpt = None
        let access = tcState.TcEnvFromImpls.AccessRights
        let dependencyFiles = parseResults |> Seq.map (fun x -> x.DependencyFiles) |> Array.concat
        let details = (tcGlobals, tcImports, tcState.Ccu, tcState.CcuSig, symbolUses, topAttrsOpt, assemblyDataOpt, assemblyRef, access, tcImplFilesOpt, dependencyFiles)
        let keepAssemblyContents = true
        FSharpCheckProjectResults (projectFileName, Some tcConfig, keepAssemblyContents, errors, Some details)

    member private x.ClearStaleCache (fileName: string, parsingOptions: FSharpParsingOptions) =
        let fileIndex = parsingOptions.SourceFiles |> Array.findIndex ((=) fileName)
        let filesAbove = parsingOptions.SourceFiles |> Array.take fileIndex
        // backup all cached typecheck entries above file
        let cachedAbove = filesAbove |> Array.choose (fun key ->
            match checkCache.TryGetValue(key) with
            | true, value -> Some (key, value)
            | false, _ -> None)
        // remove all parse cache entries with the same file name
        let staleParseKeys = parseCache.Keys |> Seq.filter (fun (n,_) -> n = fileName) |> Seq.toArray
        staleParseKeys |> Array.iter (fun key -> parseCache.TryRemove(key) |> ignore)
        checkCache.Clear(); // clear all typecheck cache
        // restore all cached typecheck entries above file
        cachedAbove |> Array.iter (fun (key, value) -> checkCache.TryAdd(key, value) |> ignore)

    member private x.ParseFile (fileName: string, source: string, parsingOptions: FSharpParsingOptions) =
        let parseCacheKey = fileName, hash source
        parseCache.GetOrAdd(parseCacheKey, fun _ ->
            x.ClearStaleCache(fileName, parsingOptions)
            let parseErrors, parseTreeOpt, anyErrors = Parser.parseFile (source, fileName, parsingOptions, userOpName)
            let dependencyFiles = [||] // interactions have no dependencies
            FSharpParseFileResults (parseErrors, parseTreeOpt, anyErrors, dependencyFiles) )

    member private x.TypeCheckOneInput (parseResults: FSharpParseFileResults, tcSink: TcResultsSink, tcState: TcState, moduleNamesDict: ModuleNamesDict) =
        let input = parseResults.ParseTree.Value
        let capturingErrorLogger = CompilationErrorLogger("TypeCheckFile", tcConfig.errorSeverityOptions)
        let errorLogger = GetErrorLoggerFilteringByScopedPragmas(false, GetScopedPragmasForInput(input), capturingErrorLogger)
        use _errorScope = new CompilationGlobalsScope (errorLogger, BuildPhase.TypeCheck)

        let checkForErrors () = parseResults.ParseHadErrors || errorLogger.ErrorCount > 0
        let prefixPathOpt = None

        let input, moduleNamesDict = input |> DeduplicateParsedInputModuleName moduleNamesDict
        let tcResult, tcState =
            TypeCheckOneInputEventually (checkForErrors, tcConfig, tcImports, tcGlobals, prefixPathOpt, tcSink, tcState, input)
            |> Eventually.force ctok

        let fileName = parseResults.FileName
        let tcErrors = ErrorHelpers.CreateErrorInfos (tcConfig.errorSeverityOptions, false, fileName, (capturingErrorLogger.GetErrors()))
        (tcResult, tcErrors), (tcState, moduleNamesDict)

    member private x.CheckFile (projectFileName: string, parseResults: FSharpParseFileResults, tcState: TcState, moduleNamesDict: ModuleNamesDict) =
        match parseResults.ParseTree with
        | Some input ->
            let sink = TcResultsSinkImpl(tcGlobals)
            let tcSink = TcResultsSink.WithSink sink
            let (tcResult, tcErrors), (tcState, moduleNamesDict) =
                x.TypeCheckOneInput (parseResults, tcSink, tcState, moduleNamesDict)
            let fileName = parseResults.FileName
            checkCache.[fileName] <- ((tcResult, tcErrors), (tcState, moduleNamesDict))

            let loadClosure = None
            let checkAlive () = true
            let textSnapshotInfo = None
            let keepAssemblyContents = true

            let tcEnvAtEnd, _topAttrs, implFile, ccuSigForFile = tcResult
            let errors = Array.append parseResults.Errors tcErrors

            let scope = TypeCheckInfo (tcConfig, tcGlobals, ccuSigForFile, tcState.Ccu, tcImports, tcEnvAtEnd.AccessRights,
                                    projectFileName, fileName, sink.GetResolutions(), sink.GetSymbolUses(), tcEnvAtEnd.NameEnv,
                                    loadClosure, reactorOps, checkAlive, textSnapshotInfo, implFile, sink.GetOpenDeclarations())
            FSharpCheckFileResults (fileName, errors, Some scope, parseResults.DependencyFiles, None, reactorOps, keepAssemblyContents)
            |> Some
        | None ->
            None

    member private x.TypeCheckClosedInputSet (parseResults: FSharpParseFileResults[], tcConfig, tcImports, tcGlobals, tcState) =
        let cachedTypeCheck (tcState, moduleNamesDict) (parseRes: FSharpParseFileResults) =
            let checkCacheKey = parseRes.FileName
            let typeCheckOneInput _fileName =
                x.TypeCheckOneInput (parseRes, TcResultsSink.NoSink, tcState, moduleNamesDict)
            checkCache.GetOrAdd(checkCacheKey, typeCheckOneInput)
        let results, (tcState, moduleNamesDict) =
            ((tcState, Map.empty), parseResults) ||> Array.mapFold cachedTypeCheck
        let tcResults, tcErrors = Array.unzip results
        let (tcEnvAtEndOfLastFile, topAttrs, implFiles, _ccuSigsForFiles), tcState =
            TypeCheckMultipleInputsFinish(tcResults |> Array.toList, tcState)
        let tcState, declaredImpls = TypeCheckClosedInputSetFinish (implFiles, tcState)
        tcState, topAttrs, declaredImpls, tcEnvAtEndOfLastFile, moduleNamesDict, tcErrors

    /// Errors grouped by file, sorted by line, column
    member private x.ErrorsByFile (fileNames: string[], errorList: FSharpErrorInfo[] list) =
        let errorMap = errorList |> Array.concat |> Array.groupBy (fun x -> x.FileName) |> Map.ofArray
        let errors = fileNames |> Array.choose errorMap.TryFind
        errors |> Array.iter (Array.sortInPlaceBy (fun x -> x.StartLineAlternate, x.StartColumn))
        errors |> Array.concat

    /// Clears parse and typecheck caches.
    member x.ClearCache () =
        parseCache.Clear()
        checkCache.Clear()

    /// Parses and checks single file only, left as is for backwards compatibility.
    /// Despite the name, there is no support for #load etc.
    member x.ParseAndCheckScript (projectFileName: string, fileName: string, source: string) =
        let fileNames = [| fileName |]
        let parsingOptions = FSharpParsingOptions.FromTcConfig(tcConfig, fileNames, false)
        let parseResults = x.ParseFile (fileName, source, parsingOptions)
        let moduleNamesDict = Map.empty
        let loadClosure = None
        let backgroundErrors = [||]
        let checkAlive () = true
        let textSnapshotInfo = None
        let tcState = tcInitialState
        let tcResults = Parser.CheckOneFile(
                            parseResults, source, fileName, projectFileName, tcConfig, tcGlobals, tcImports, tcState,
                            moduleNamesDict, loadClosure, backgroundErrors, reactorOps, checkAlive, textSnapshotInfo, userOpName)
        match tcResults with
        | tcErrors, Parser.TypeCheckAborted.No scope ->
            let errors = Array.append parseResults.Errors tcErrors
            let tcImplFilesOpt = match scope.ImplementationFile with Some x -> Some [x] | None -> None
            let typeCheckResults = FSharpCheckFileResults (fileName, errors, Some scope, parseResults.DependencyFiles, None, reactorOps, true)
            let symbolUses = [scope.ScopeSymbolUses]
            let projectResults = x.MakeProjectResults (projectFileName, [|parseResults|], tcState, errors, symbolUses, None, tcImplFilesOpt)
            parseResults, typeCheckResults, projectResults
        | _ ->
            failwith "unexpected aborted"

    /// Parses and checks the whole project, good for compilers (Fable etc.)
    /// Does not retain name resolutions and symbol uses which are quite memory hungry (so no intellisense etc.).
    /// Already parsed files will be cached so subsequent compilations will be faster.
    member x.ParseAndCheckProject (projectFileName: string, fileNames: string[], sources: string[]) =
        // parse files
        let parsingOptions = FSharpParsingOptions.FromTcConfig(tcConfig, fileNames, false)
        let parseFile (fileName, source) = x.ParseFile (fileName, source, parsingOptions)
        let parseResults = Array.zip fileNames sources |> Array.map parseFile

        // type check files
        let tcState, topAttrs, tcImplFiles, _tcEnvAtEnd, _moduleNamesDict, tcErrors =
            x.TypeCheckClosedInputSet (parseResults, tcConfig, tcImports, tcGlobals, tcInitialState)

        // make project results
        let parseErrors = parseResults |> Array.collect (fun p -> p.Errors)
        let typedErrors = tcErrors |> Array.concat
        let errors = x.ErrorsByFile (fileNames, [ parseErrors; typedErrors ])
        let symbolUses = [] //TODO:
        let projectResults = x.MakeProjectResults (projectFileName, parseResults, tcState, errors, symbolUses, Some topAttrs, Some tcImplFiles)

        projectResults

    /// Parses and checks file in project, will compile and cache all the files up to this one
    /// (if not already done before), or fetch them from cache. Returns partial project results,
    /// up to and including the file requested. Returns parse and typecheck results containing
    /// name resolutions and symbol uses for the file requested only, so intellisense etc. works.
    member x.ParseAndCheckFileInProject (fileName: string, projectFileName: string, fileNames: string[], sources: string[]) =
        // get files before file
        let fileIndex = fileNames |> Array.findIndex ((=) fileName)
        let fileNamesBeforeFile = fileNames |> Array.take fileIndex
        let sourcesBeforeFile = sources |> Array.take fileIndex

        // parse files before file
        let parsingOptions = FSharpParsingOptions.FromTcConfig(tcConfig, fileNames, false)
        let parseFile (fileName, source) = x.ParseFile (fileName, source, parsingOptions)
        let parseResults = Array.zip fileNamesBeforeFile sourcesBeforeFile |> Array.map parseFile

        // type check files before file
        let tcState, topAttrs, tcImplFiles, _tcEnvAtEnd, moduleNamesDict, tcErrors =
            x.TypeCheckClosedInputSet (parseResults, tcConfig, tcImports, tcGlobals, tcInitialState)

        // parse and type check file
        let parseFileResults = parseFile (fileName, sources.[fileIndex])
        let checkFileResults = x.CheckFile (projectFileName, parseFileResults, tcState, moduleNamesDict)
        let (tcResult, _tcErrors), (tcState, _moduleNamesDict) = checkCache.[fileName]
        let _tcEnvAtEndFile, topAttrsFile, implFile, _ccuSigForFile = tcResult

        // collect errors
        let parseErrorsBefore = parseResults |> Array.collect (fun p -> p.Errors)
        let typedErrorsBefore = tcErrors |> Array.concat
        let newErrors = match checkFileResults with | Some res -> res.Errors | None -> [||]
        let errors = x.ErrorsByFile (fileNames, [ parseErrorsBefore; typedErrorsBefore; newErrors ])

        // make partial project results
        let parseResults = Array.append parseResults [| parseFileResults |]
        let tcImplFiles = List.append tcImplFiles (Option.toList implFile)
        let topAttrs = CombineTopAttrs topAttrsFile topAttrs
        let symbolUses = [] //TODO:
        let projectResults = x.MakeProjectResults (projectFileName, parseResults, tcState, errors, symbolUses, Some topAttrs, Some tcImplFiles)

        parseFileResults, checkFileResults, projectResults
