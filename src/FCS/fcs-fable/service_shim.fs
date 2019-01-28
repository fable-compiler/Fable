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

type InteractiveChecker internal (tcConfig, tcGlobals, tcImports, tcInitialState, ctok, reactorOps, moduleNamesDict, parseCache, checkCache) =
    let userOpName = "Unknown"

    static member Create(references: string[], readAllBytes: string -> byte[], defines: string[], optimize: bool) =

        let GetSignatureData ((fileName:string), ilScopeRef, (ilModule:ILModuleDef option), (bytes:byte[])) = 
            TastPickle.unpickleObjWithDanglingCcus fileName ilScopeRef ilModule TastPickle.unpickleCcuInfo bytes
        let GetOptimizationData ((fileName:string), ilScopeRef, (ilModule:ILModuleDef option), (bytes:byte[])) = 
            TastPickle.unpickleObjWithDanglingCcus fileName ilScopeRef ilModule Optimizer.u_CcuOptimizationInfo bytes

        let tcConfig = TcConfig (optimize, defines = Array.toList defines)
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

        let LoadMod ccuName =
            let fileName = ccuName + ".dll"
            let bytes = readAllBytes fileName
            let opts : ILReaderOptions =
                  { ilGlobals = ilGlobals
                    metadataOnly = MetadataOnlyFlag.Yes
                    reduceMemoryUsage = ReduceMemoryFlag.Yes
                    pdbDirPath = None
                    tryGetMetadataSnapshot = (fun _ -> None) }

            let reader = ILBinaryReader.OpenILModuleReaderFromBytes fileName bytes opts
            reader.ILModuleDef //reader.ILAssemblyRefs

        let memoize_mod = new MemoizationTable<_,_> (LoadMod, keyComparer=HashIdentity.Structural)

        let LoadSigData ccuName =
            let fileName = ccuName + ".dll" // ccuName + ".sigdata"
            let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssRef ccuName)
            let ilModule = memoize_mod.Apply ccuName
            match sigDataReaders ilModule with
            | [] -> None
            | bytes::_ -> Some (GetSignatureData (fileName, ilScopeRef, Some ilModule, bytes))

        let LoadOptData ccuName =
            let fileName = ccuName + ".dll" // ccuName + ".optdata"
            let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssRef ccuName)
            let ilModule = memoize_mod.Apply ccuName
            match optDataReaders ilModule with
            | [] -> None
            | bytes::_ -> Some (GetOptimizationData (fileName, ilScopeRef, Some ilModule, bytes))

        let memoize_sig = new MemoizationTable<_,_> (LoadSigData, keyComparer=HashIdentity.Structural)
        let memoize_opt = new MemoizationTable<_,_> (LoadOptData, keyComparer=HashIdentity.Structural)

        let GetCustomAttributesOfIlModule (ilModule:ILModuleDef) = 
            (match ilModule.Manifest with Some m -> m.CustomAttrs | None -> ilModule.CustomAttrs).AsList 

        let GetAutoOpenAttributes ilg ilModule = 
            ilModule |> GetCustomAttributesOfIlModule |> List.choose (TryFindAutoOpenAttr ilg)

        let GetInternalsVisibleToAttributes ilg ilModule = 
            ilModule |> GetCustomAttributesOfIlModule |> List.choose (TryFindInternalsVisibleToAttr ilg)

        let HasAnyFSharpSignatureDataAttribute ilModule = 
            let attrs = GetCustomAttributesOfIlModule ilModule
            List.exists IsSignatureDataVersionAttr attrs

        let mkCcuInfo ilg ilScopeRef ilModule ccu =
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
            let fileName = ilModule.Name
            let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssRef ccuName)
            let invalidateCcu = new Event<_>()
            let ccu = Import.ImportILAssembly(
                        tcImports.GetImportMap, m, auxModuleLoader, ilScopeRef,
                        tcConfig.implicitIncludeDir, Some fileName, ilModule, invalidateCcu.Publish)
            let ccuInfo = mkCcuInfo ilGlobals ilScopeRef ilModule ccu
            ccuInfo, None

        let GetCcuFS m ccuName =
            let sigdata = memoize_sig.Apply ccuName
            let ilModule = memoize_mod.Apply ccuName
            let fileName = ilModule.Name
            let ilScopeRef = ILScopeRef.Assembly (mkSimpleAssRef ccuName)
            let GetRawTypeForwarders ilModule =
                match ilModule.Manifest with 
                | Some manifest -> manifest.ExportedTypes
                | None -> mkILExportedTypes []
#if !NO_EXTENSIONTYPING
            let invalidateCcu = new Event<_>()
#endif
            let minfo : PickledCcuInfo = sigdata.Value.RawData //TODO: handle missing sigdata
            let codeDir = minfo.compileTimeWorkingDir
            let ccuData : CcuData = 
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

            let ccu = CcuThunk.Create(ccuName, ccuData)
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
                |> List.tryFind (fun x -> x.FSharpViewOfMetadata.AssemblyName = name)
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

        let niceNameGen = NiceNameGenerator()
        let amap = tcImports.GetImportMap()
        let rng = rangeN Lexhelp.stdinMockFilename 0

        let assemblyName = "Project"
        let ccus = ccuInfos |> List.map (fun x -> x.FSharpViewOfMetadata, x.AssemblyAutoOpenAttributes, x.AssemblyInternalsVisibleToAttributes)
        let tcInitialEnv = CreateInitialTcEnv (tcGlobals, amap, rng, assemblyName, ccus)
        let tcInitialState = GetInitialTcState (rangeStartup, assemblyName, tcConfig, tcGlobals, tcImports, niceNameGen, tcInitialEnv)
        let ctok = CompilationThreadToken()

        let reactorOps = 
            { new IReactorOperations with
                member __.EnqueueAndAwaitOpAsync (userOpName, opName, opArg, op) =
                    async.Return (Cancellable.runWithoutCancellation (op ctok))
                member __.EnqueueOp (userOpName, opName, opArg, op) = (op ctok) }

        // dictionary for de-duplicating module names
        let moduleNamesDict = ConcurrentDictionary<string, Set<string>>()
        // parse cache, keyed on file name and source hash
        let parseCache = ConcurrentDictionary<string * int, FSharpParseFileResults>(HashIdentity.Structural)
        // type check cache, keyed on file name
        let checkCache = ConcurrentDictionary<string, TcResult * TcState>(HashIdentity.Structural)

        InteractiveChecker (tcConfig, tcGlobals, tcImports, tcInitialState, ctok, reactorOps, moduleNamesDict, parseCache, checkCache)

    member private x.MakeProjectResults (projectFileName: string, parseResults: FSharpParseFileResults[], tcState: TcState, errors: FSharpErrorInfo[],
                                         symbolUses: TcSymbolUses list, topAttrsOpt: TopAttribs option, tcImplFilesOpt: TypedImplFile list option) =
        let assemblyRef = mkSimpleAssRef "stdin"
        let assemblyDataOpt = None
        let access = tcState.TcEnvFromImpls.AccessRights
        let dependencyFiles = parseResults |> Seq.map (fun x -> x.DependencyFiles) |> Array.concat
        let details = (tcGlobals, tcImports, tcState.Ccu, tcState.CcuSig, symbolUses, topAttrsOpt, assemblyDataOpt, assemblyRef, access, tcImplFilesOpt, dependencyFiles)
        let keepAssemblyContents = true
        FSharpCheckProjectResults (projectFileName, Some tcConfig, keepAssemblyContents, errors, Some details)

    member private x.ClearStaleCache (fileName: string, parsingOptions: FSharpParsingOptions) =
        let fileIndex = parsingOptions.SourceFiles |> Array.findIndex ((=) fileName)
        let staleParseKeys = parseCache.Keys |> Seq.filter (fun (n,_) -> n = fileName) |> Seq.toArray
        let staleCheckKeys = parsingOptions.SourceFiles |> Array.skip fileIndex
        staleParseKeys |> Array.iter (fun key -> parseCache.Remove(key) |> ignore)
        staleCheckKeys |> Array.iter (fun key -> checkCache.Remove(key) |> ignore)

    member private x.ParseFile (fileName: string, source: string, parsingOptions: FSharpParsingOptions) =
        let parseCacheKey = fileName, hash source
        parseCache.GetOrAdd(parseCacheKey, fun _ ->
            x.ClearStaleCache(fileName, parsingOptions)
            let parseErrors, parseTreeOpt, anyErrors = Parser.parseFile (source, fileName, parsingOptions, userOpName)
            let parseTreeOpt = parseTreeOpt |> Option.map (DeduplicateParsedInputModuleName moduleNamesDict)
            let dependencyFiles = [||] // interactions have no dependencies
            FSharpParseFileResults (parseErrors, parseTreeOpt, anyErrors, dependencyFiles) )

    // member private x.CheckFile (source: string, projectFileName: string, parseResults: FSharpParseFileResults, tcState: TcState) =
    //     let fileName = parseResults.FileName
    //     let loadClosure = None
    //     let backgroundErrors = [||]
    //     let checkAlive () = true
    //     let textSnapshotInfo = None
    //     let tcResults = Parser.CheckOneFile(
    //                         parseResults, source, fileName, projectFileName, tcConfig, tcGlobals, tcImports, tcState,
    //                         loadClosure, backgroundErrors, reactorOps, checkAlive, textSnapshotInfo, userOpName)
    //     match tcResults with
    //     | tcErrors, Parser.TypeCheckAborted.No scope ->
    //         let errors = Array.append parseResults.Errors tcErrors
    //         let checkResults = FSharpCheckFileResults (fileName, errors, Some scope, parseResults.DependencyFiles, None, reactorOps, true)
    //         FSharpCheckFileAnswer.Succeeded checkResults
    //     | _ ->
    //         FSharpCheckFileAnswer.Aborted

    member private x.CheckFile (projectFileName: string, parseResults: FSharpParseFileResults, tcState: TcState) =
        match parseResults.ParseTree with
        | Some input ->
            let capturingErrorLogger = CompilationErrorLogger("TypeCheckFile", tcConfig.errorSeverityOptions)
            let errorLogger = GetErrorLoggerFilteringByScopedPragmas(false, GetScopedPragmasForInput(input), capturingErrorLogger)
            use _errorScope = new CompilationGlobalsScope (errorLogger, BuildPhase.TypeCheck)

            let sink = TcResultsSinkImpl(tcGlobals)
            let tcSink = TcResultsSink.WithSink sink
            let checkForErrors () = parseResults.ParseHadErrors || errorLogger.ErrorCount > 0
            let prefixPathOpt = None
            let (tcEnvAtEnd, topAttrs, implFile, ccuSigForFile), tcState =
                TypeCheckOneInputEventually (checkForErrors, tcConfig, tcImports, tcGlobals, prefixPathOpt, tcSink, tcState, input)
                |> Eventually.force ctok

            let fileName = parseResults.FileName
            checkCache.[fileName] <- ((tcEnvAtEnd, topAttrs, implFile, ccuSigForFile), tcState)

            let loadClosure = None
            let checkAlive () = true
            let textSnapshotInfo = None
            let keepAssemblyContents = true

            let tcErrors = ErrorHelpers.CreateErrorInfos (tcConfig.errorSeverityOptions, false, fileName, (capturingErrorLogger.GetErrors()))
            let errors = Array.append parseResults.Errors tcErrors

            let scope = TypeCheckInfo (tcConfig, tcGlobals, ccuSigForFile, tcState.Ccu, tcImports, tcEnvAtEnd.AccessRights,
                                    projectFileName, fileName, sink.GetResolutions(), sink.GetSymbolUses(), tcEnvAtEnd.NameEnv,
                                    loadClosure, reactorOps, checkAlive, textSnapshotInfo, implFile, sink.GetOpenDeclarations())
            FSharpCheckFileResults (fileName, errors, Some scope, parseResults.DependencyFiles, None, reactorOps, keepAssemblyContents)
            |> Some
        | None ->
            None

    member private x.TypeCheckClosedInputSet (ctok, checkForErrors, tcConfig, tcImports, tcGlobals, prefixPathOpt, tcState, inputs) =
        let fileNameOf = function
            | ParsedInput.SigFile (ParsedSigFileInput(fileName,_,_,_,_)) -> fileName
            | ParsedInput.ImplFile (ParsedImplFileInput(fileName,_,_,_,_,_,_)) -> fileName
        let cachedTypeCheck tcState (input: ParsedInput) =
            let checkCacheKey = fileNameOf input
            checkCache.GetOrAdd(checkCacheKey, fun _ ->
                TypeCheckOneInput (ctok, checkForErrors, tcConfig, tcImports, tcGlobals, prefixPathOpt) tcState input)
        let results, tcState = (tcState, inputs) ||> List.mapFold cachedTypeCheck
        let (tcEnvAtEndOfLastFile, topAttrs, implFiles, _ccuSigsForFiles), tcState =
            TypeCheckMultipleInputsFinish(results, tcState)
        let tcState, declaredImpls = TypeCheckClosedInputSetFinish (implFiles, tcState)
        tcState, topAttrs, declaredImpls, tcEnvAtEndOfLastFile

    member x.ClearCache () =
        parseCache.Clear()
        checkCache.Clear()

    member x.ParseAndCheckScript (projectFileName: string, fileName: string, source: string) =
        let fileNames = [| fileName |]
        let parsingOptions = FSharpParsingOptions.FromTcConfig(tcConfig, fileNames, false)
        let parseResults = x.ParseFile (fileName, source, parsingOptions)
        let loadClosure = None
        let backgroundErrors = [||]
        let checkAlive () = true
        let textSnapshotInfo = None
        let tcState = tcInitialState
        let tcResults = Parser.CheckOneFile(
                            parseResults, source, fileName, projectFileName, tcConfig, tcGlobals, tcImports, tcState,
                            loadClosure, backgroundErrors, reactorOps, checkAlive, textSnapshotInfo, userOpName)
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

    member x.ParseAndCheckProject (projectFileName: string, fileNames: string[], sources: string[]) =
        // parse files
        let parsingOptions = FSharpParsingOptions.FromTcConfig(tcConfig, fileNames, false)
        let parseFile (fileName, source) = x.ParseFile(fileName, source, parsingOptions)
        let parseResults = Array.zip fileNames sources |> Array.map parseFile
        let parseHadErrors = parseResults |> Array.exists (fun p -> p.ParseHadErrors)
        let parsedInputs = parseResults |> Array.choose (fun p -> p.ParseTree) |> Array.toList

        // type check files
        use errorScope = new ErrorScope()
        let hasTypedErrors () = errorScope.Diagnostics |> List.exists (fun e -> e.Severity = FSharpErrorSeverity.Error)
        let checkForErrors () = parseHadErrors || hasTypedErrors ()
        let prefixPathOpt = None
        let tcState, topAttrs, tcImplFiles, _tcEnvAtEnd =
            x.TypeCheckClosedInputSet (ctok, checkForErrors, tcConfig, tcImports, tcGlobals, prefixPathOpt, tcInitialState, parsedInputs)

        // make project results
        let parseErrors = parseResults |> Array.collect (fun p -> p.Errors)
        let typedErrors = errorScope.Diagnostics |> List.toArray
        let errors = Array.append parseErrors typedErrors //TODO: order by file
        let symbolUses = [] //TODO:
        let projectResults = x.MakeProjectResults (projectFileName, parseResults, tcState, errors, symbolUses, Some topAttrs, Some tcImplFiles)

        projectResults

    member x.ParseAndCheckFileInProject (fileName: string, projectFileName: string, fileNames: string[], sources: string[]) =
        // get files before file
        let fileIndex = fileNames |> Array.findIndex ((=) fileName)
        let fileNamesBeforeFile = fileNames |> Array.take fileIndex
        let sourcesBeforeFile = sources |> Array.take fileIndex

        // parse files before file
        let parsingOptions = FSharpParsingOptions.FromTcConfig(tcConfig, fileNames, false)
        let parseFile (fileName, source) = x.ParseFile(fileName, source, parsingOptions)
        let parseResults = Array.zip fileNamesBeforeFile sourcesBeforeFile |> Array.map parseFile
        let parseHadErrors = parseResults |> Array.exists (fun p -> p.ParseHadErrors)
        let parsedInputs = parseResults |> Array.choose (fun p -> p.ParseTree) |> Array.toList

        // type check files before file
        use errorScope = new ErrorScope()
        let hasTypedErrors () = errorScope.Diagnostics |> List.exists (fun e -> e.Severity = FSharpErrorSeverity.Error)
        let checkForErrors () = parseHadErrors || hasTypedErrors ()
        let prefixPathOpt = None
        let tcState, topAttrs, tcImplFiles, _tcEnvAtEnd =
            x.TypeCheckClosedInputSet (ctok, checkForErrors, tcConfig, tcImports, tcGlobals, prefixPathOpt, tcInitialState, parsedInputs)

        // parse and type check file
        let parseFileResults = parseFile (fileName, sources.[fileIndex])
        let checkFileResults = x.CheckFile (projectFileName, parseFileResults, tcState)
        let (_tcEnvAtEndFile, topAttrsFile, implFile, _ccuSigForFile), tcState = checkCache.[fileName]

        // collect errors
        let parseErrorsBefore = parseResults |> Array.collect (fun p -> p.Errors)
        let typedErrorsBefore = errorScope.Diagnostics |> List.toArray
        let newErrors = match checkFileResults with | Some res -> res.Errors | None -> [||]
        let errors = [| yield! parseErrorsBefore; yield! typedErrorsBefore; yield! newErrors |] //TODO: order by file

        // make partial project results
        let parseResults = Array.append parseResults [| parseFileResults |]
        let tcImplFiles = List.append tcImplFiles (Option.toList implFile)
        let topAttrs = CombineTopAttrs topAttrsFile topAttrs
        let symbolUses = [] //TODO:
        let projectResults = x.MakeProjectResults (projectFileName, parseResults, tcState, errors, symbolUses, Some topAttrs, Some tcImplFiles)

        parseFileResults, checkFileResults, projectResults
