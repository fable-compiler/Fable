// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.SourceCodeServices

open System.Collections.Generic
open System.Collections.Concurrent

open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.Internal.Library
open FSharp.Compiler.CheckExpressions
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerDiagnostics
open FSharp.Compiler.CompilerGlobalState
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.CompilerOptions
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.NameResolution
open FSharp.Compiler.Parser
open FSharp.Compiler.ParseAndCheckInputs
open FSharp.Compiler.Range
open FSharp.Compiler.ScriptClosure
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.TypedTree

open Internal.Utilities
open Internal.Utilities.Collections


//-------------------------------------------------------------------------
// InteractiveChecker
//-------------------------------------------------------------------------

type internal TcResult = TcEnv * TopAttribs * TypedImplFile option * ModuleOrNamespaceType
type internal TcErrors = FSharpErrorInfo[]

type InteractiveChecker internal (tcConfig, tcGlobals, tcImports, tcInitialState, ctok, reactorOps, parseCache, checkCache) =
    let userOpName = "Unknown"
    let suggestNamesForErrors = true

    static member Create(references: string[], readAllBytes: string -> byte[], defines: string[], optimize: bool) =
        let otherOptions = [|
            for d in defines do yield "-d:" + d
            yield "--optimize" + (if optimize then "+" else "-")
        |]
        InteractiveChecker.Create(references, readAllBytes, otherOptions)

    static member Create(references: string[], readAllBytes: string -> byte[], otherOptions: string[]) =
        let projectFileName = "Project"
        let toRefOption (fileName: string) =
            if fileName.EndsWith(".dll", System.StringComparison.OrdinalIgnoreCase)
            then "-r:" + fileName
            else "-r:" + fileName + ".dll"
        let otherOptions = references |> Array.map toRefOption |> Array.append otherOptions
        let projectOptions: FSharpProjectOptions = {
            ProjectFileName = projectFileName
            ProjectId = None
            SourceFiles = [| |]
            OtherOptions = otherOptions
            ReferencedProjects = [| |]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = System.DateTime.MaxValue
            UnresolvedReferences = None
            OriginalLoadReferences = []
            ExtraProjectInfo = None
            Stamp = None
        }
        InteractiveChecker.Create(readAllBytes, projectOptions)

    static member Create(readAllBytes: string -> byte[], projectOptions: FSharpProjectOptions) =
        let references =
            projectOptions.OtherOptions
            |> Array.filter (fun s -> s.StartsWith("-r:"))
            |> Array.map (fun s -> s.Replace("-r:", ""))

        let tcConfig =
            let tcConfigB = TcConfigBuilder.Initial
            tcConfigB.implicitIncludeDir <- System.IO.Path.GetDirectoryName (projectOptions.ProjectFileName)
            let sourceFiles = projectOptions.SourceFiles |> Array.toList
            let argv = projectOptions.OtherOptions |> Array.toList
            let _sourceFiles = ApplyCommandLineArgs(tcConfigB, sourceFiles, argv)
            TcConfig.Create(tcConfigB, validate=false)

        let ctok = CompilationThreadToken()
        let tcImports, tcGlobals =
            TcImports.BuildTcImports (tcConfig, references, readAllBytes)

        let niceNameGen = NiceNameGenerator()
        let assemblyName = projectOptions.ProjectFileName |> System.IO.Path.GetFileNameWithoutExtension
        let tcInitialEnv = GetInitialTcEnv (assemblyName, rangeStartup, tcConfig, tcImports, tcGlobals)
        let tcInitialState = GetInitialTcState (rangeStartup, assemblyName, tcConfig, tcGlobals, tcImports, niceNameGen, tcInitialEnv)

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
            let sourceText = SourceText.ofString source
            let parseErrors, parseTreeOpt, anyErrors = ParseAndCheckFile.parseFile (sourceText, fileName, parsingOptions, userOpName, suggestNamesForErrors)
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
            TypeCheckOneInputEventually (checkForErrors, tcConfig, tcImports, tcGlobals, prefixPathOpt, tcSink, tcState, input, false)
            |> Eventually.force ctok

        let fileName = parseResults.FileName
        let tcErrors = ErrorHelpers.CreateErrorInfos (tcConfig.errorSeverityOptions, false, fileName, (capturingErrorLogger.GetErrors()), suggestNamesForErrors)
        (tcResult, tcErrors), (tcState, moduleNamesDict)

    member private x.CheckFile (projectFileName: string, parseResults: FSharpParseFileResults, tcState: TcState, moduleNamesDict: ModuleNamesDict) =
        match parseResults.ParseTree with
        | Some _input ->
            let sink = TcResultsSinkImpl(tcGlobals)
            let tcSink = TcResultsSink.WithSink sink
            let (tcResult, tcErrors), (tcState, moduleNamesDict) =
                x.TypeCheckOneInput (parseResults, tcSink, tcState, moduleNamesDict)
            let fileName = parseResults.FileName
            checkCache.[fileName] <- ((tcResult, tcErrors), (tcState, moduleNamesDict))

            let loadClosure = None
            let textSnapshotInfo = None
            let keepAssemblyContents = true

            let tcEnvAtEnd, _topAttrs, implFile, ccuSigForFile = tcResult
            let errors = Array.append parseResults.Errors tcErrors

            let scope = TypeCheckInfo (tcConfig, tcGlobals, ccuSigForFile, tcState.Ccu, tcImports, tcEnvAtEnd.AccessRights,
                                    projectFileName, fileName, sink.GetResolutions(), sink.GetSymbolUses(), tcEnvAtEnd.NameEnv,
                                    loadClosure, reactorOps, textSnapshotInfo, implFile, sink.GetOpenDeclarations())
            FSharpCheckFileResults (fileName, errors, Some scope, parseResults.DependencyFiles, None, reactorOps, keepAssemblyContents)
            |> Some
        | None ->
            None

    member private x.TypeCheckClosedInputSet (parseResults: FSharpParseFileResults[], tcState) =
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
        let sourceText = SourceText.ofString source
        let fileNames = [| fileName |]
        let parsingOptions = FSharpParsingOptions.FromTcConfig(tcConfig, fileNames, false)
        let parseResults = x.ParseFile (fileName, source, parsingOptions)
        let moduleNamesDict = Map.empty
        let loadClosure = None
        let backgroundErrors = [||]
        let textSnapshotInfo = None
        let tcState = tcInitialState
        let tcResults = ParseAndCheckFile.CheckOneFile(
                            parseResults, sourceText, fileName, projectFileName, tcConfig, tcGlobals, tcImports, tcState,
                            moduleNamesDict, loadClosure, backgroundErrors, reactorOps, textSnapshotInfo, userOpName, suggestNamesForErrors)
        match tcResults with
        | tcErrors, Result.Ok tcFileInfo ->
            let errors = Array.append parseResults.Errors tcErrors
            let tcImplFilesOpt = match tcFileInfo.ImplementationFile with Some x -> Some [x] | None -> None
            let typeCheckResults = FSharpCheckFileResults (fileName, errors, Some tcFileInfo, parseResults.DependencyFiles, None, reactorOps, true)
            let symbolUses = [tcFileInfo.ScopeSymbolUses]
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
            x.TypeCheckClosedInputSet (parseResults, tcInitialState)

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
            x.TypeCheckClosedInputSet (parseResults, tcInitialState)

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
