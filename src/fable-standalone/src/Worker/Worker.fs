module Fable.WebWorker.Main

open Fable.Core
open Fable.Core.JsInterop
open Fable.Standalone
open Fable.WebWorker

let FILE_NAME = "test.fs"
let PROJECT_NAME = "project.fsproj"

type IFableInit =
    abstract member init: unit -> IFableManager

[<Global>]
let self: obj = jsNative

[<Global>]
let importScripts (_path: string) : unit = jsNative

// Load FCS+Fable bundle
importScripts "bundle.min.js"

[<Global("__FABLE_STANDALONE__")>]
let FableInit: IFableInit = jsNative

let getAssemblyReader
    (
        _getBlobUrl: string -> string,
        _refs: string[]
    )
    : JS.Promise<string -> byte[]>
    =
    importMember "./util.js"

let escapeJsStringLiteral (str: string) : string = importMember "./util.js"

let measureTime f arg =
    let before: float = self?performance?now ()
    let res = f arg
    let after: float = self?performance?now ()
    res, after - before

type FableState =
    {
        Manager: IFableManager
        Checker: IChecker
        LoadTime: float
        References: string[]
        Reader: string -> byte[]
        OtherFSharpOptions: string[]
    }

type FableStateConfig =
    | Init of
        refsDirUrl: string *
        extraRefs: string[] *
        refsExtraSuffix: string option
    | Initialized of FableState

type State =
    {
        Fable: FableState option
        Worker: ObservableWorker<WorkerRequest>
        CurrentResults: Map<string, IParseAndCheckResults>
    }

type SourceWriter(sourceMaps: bool, language: string) =
    let sb = System.Text.StringBuilder()

    interface Fable.Standalone.IWriter with
        member _.Write(str) =
            async { return sb.Append(str) |> ignore }

        member _.MakeImportPath(path) =
            match language with
            | "Python" ->
                path
                    .Replace("/", ".")
                    .Replace("-", "_")
                    .Replace(".py", "")
                    .ToLowerInvariant()
            | _ -> path

        member _.AddSourceMapping(mapping) = ()
        member _.Dispose() = ()

    member _.Result = sb.ToString()

let makeFableState (config: FableStateConfig) otherFSharpOptions =
    async {
        match config with
        | Init(refsDirUrl, extraRefs, refsExtraSuffix) ->
            let getBlobUrl name =
                refsDirUrl.TrimEnd('/')
                + "/"
                + name
                + ".dll"
                + (defaultArg refsExtraSuffix "")

            let manager = FableInit.init ()

            let references =
                Array.append Fable.Metadata.coreAssemblies extraRefs

            let! reader =
                getAssemblyReader (getBlobUrl, references) |> Async.AwaitPromise

            let (checker, checkerTime) =
                measureTime
                    (fun () ->
                        manager.CreateChecker(
                            references,
                            reader,
                            otherFSharpOptions
                        )
                    )
                    ()

            return
                {
                    Manager = manager
                    Checker = checker
                    LoadTime = checkerTime
                    References = references
                    Reader = reader
                    OtherFSharpOptions = otherFSharpOptions
                }

        | Initialized fable ->
            // We don't need to recreate the checker
            if fable.OtherFSharpOptions = otherFSharpOptions then
                return fable
            else
                let (checker, checkerTime) =
                    measureTime
                        (fun () ->
                            fable.Manager.CreateChecker(
                                fable.References,
                                fable.Reader,
                                otherFSharpOptions
                            )
                        )
                        ()

                return
                    { fable with
                        Checker = checker
                        LoadTime = checkerTime
                        OtherFSharpOptions = otherFSharpOptions
                    }
    }

let private compileCode
    fable
    fileName
    fsharpNames
    fsharpCodes
    language
    otherFSharpOptions
    =
    async {
        // detect (and remove) the non-F# compiler options to avoid changing msg contract
        let nonFSharpOptions =
            set
                [
                    "--typedArrays"
                    "--clampByteArrays"
                    "--sourceMaps"
                ]

        let fableOptions, otherFSharpOptions =
            otherFSharpOptions
            |> Array.partition (fun x -> Set.contains x nonFSharpOptions)

        //let fileName = fsharpNames |> Array.last
        // Check if we need to recreate the FableState because otherFSharpOptions have changed
        let! fable = makeFableState (Initialized fable) otherFSharpOptions

        let (parseResults, parsingTime) =
            measureTime
                (fun () ->
                    // fable.Manager.ParseFSharpScript(fable.Checker, FILE_NAME, fsharpCode, otherFSharpOptions)) ()
                    fable.Manager.ParseAndCheckFileInProject(
                        fable.Checker,
                        fileName,
                        PROJECT_NAME,
                        fsharpNames,
                        fsharpCodes,
                        otherFSharpOptions
                    )
                )
                ()

        let! compiledCode, errors, fableTransformTime =
            async {
                if
                    parseResults.Errors
                    |> Array.exists (fun e -> not e.IsWarning)
                then
                    return "", parseResults.Errors, 0.
                else
                    let options =
                        {|
                            typedArrays =
                                Array.contains "--typedArrays" fableOptions
                            sourceMaps =
                                Array.contains "--sourceMaps" fableOptions
                        |}

                    let typedArrays =
                        if options.typedArrays then
                            Some true
                        else
                            None

                    let (res, fableTransformTime) =
                        measureTime
                            (fun () ->
                                fable.Manager.CompileToTargetAst(
                                    "fable-library",
                                    parseResults,
                                    fileName,
                                    typedArrays,
                                    language
                                )
                            )
                            ()
                    // Print target language AST
                    let writer = new SourceWriter(options.sourceMaps, language)
                    do! fable.Manager.PrintTargetAst(res, writer)
                    let compiledCode = writer.Result

                    return
                        compiledCode,
                        Array.append parseResults.Errors res.FableErrors,
                        fableTransformTime
            }

        let stats: CompileStats =
            {
                FCS_checker = fable.LoadTime
                FCS_parsing = parsingTime
                Fable_transform = fableTransformTime
            }

        return (compiledCode, errors, stats)
    }

let private combineStats (a: CompileStats) (b: CompileStats) : CompileStats =
    {
        FCS_checker = a.FCS_checker + b.FCS_checker
        FCS_parsing = a.FCS_parsing + b.FCS_parsing
        Fable_transform = a.Fable_transform + b.Fable_transform
    }

let private asyncSequential (calc: Async<'T> array) : Async<'T array> =
    async {
        let mutable result = []: 'T list

        for c in calc do
            let! res = c
            result <- result @ [ res ]

        return Array.ofList result
    }

let private truncate (s: string) =
    if s.Length > 80 then
        s.Substring(0, 80) + "..."
    else
        s

let rec loop (box: MailboxProcessor<WorkerRequest>) (state: State) =
    async {

        let! msg = box.Receive()

        match state.Fable, msg with

        | None,
          CreateChecker(refsDirUrl,
                        extraRefs,
                        refsExtraSuffix,
                        otherFSharpOptions) ->

            try
                let! fable =
                    makeFableState
                        (Init(refsDirUrl, extraRefs, refsExtraSuffix))
                        otherFSharpOptions

                state.Worker.Post(Loaded fable.Manager.Version)
                return! loop box { state with Fable = Some fable }
            with err ->
                JS.console.error ("Cannot create F# checker", err) // Beware, you might be catching an exception from the next recursion of loop
                state.Worker.Post LoadFailed
                return! loop box state

        // These combination of messages are ignored
        | None, _
        | Some _, CreateChecker _ -> return! loop box state

        | Some fable, ParseCode(fsharpCode, otherFSharpOptions) ->
            // Check if we need to recreate the FableState because otherFSharpOptions have changed
            let! fable = makeFableState (Initialized fable) otherFSharpOptions
            // let res = fable.Manager.ParseFSharpScript(fable.Checker, FILE_NAME, fsharpCode, otherFSharpOptions)
            let res =
                fable.Manager.ParseAndCheckFileInProject(
                    fable.Checker,
                    FILE_NAME,
                    PROJECT_NAME,
                    [| FILE_NAME |],
                    [| fsharpCode |],
                    otherFSharpOptions
                )

            ParsedCode res.Errors |> state.Worker.Post

            return!
                loop
                    box
                    { state with
                        CurrentResults =
                            state.CurrentResults.Add(FILE_NAME, res)
                    }

        | Some fable, ParseFile(file, fsharpCode, otherFSharpOptions) ->
            try
                // Check if we need to recreate the FableState because otherFSharpOptions have changed
                let! fable =
                    makeFableState (Initialized fable) otherFSharpOptions

                // let res = fable.Manager.ParseFSharpScript(fable.Checker, FILE_NAME, fsharpCode, otherFSharpOptions)

                let names = fsharpCode |> Array.map (fun x -> x.Name)
                let contents = fsharpCode |> Array.map (fun x -> x.Content)

                let res =
                    fable.Manager.ParseAndCheckFileInProject(
                        fable.Checker,
                        file,
                        PROJECT_NAME,
                        names,
                        contents,
                        otherFSharpOptions
                    )

                ParsedCode res.Errors |> state.Worker.Post

                let newResults = state.CurrentResults.Add(file, res)
                return! loop box { state with CurrentResults = newResults }
            with err ->
                JS.console.error ("ParseNamedCode", err)
                return! loop box state

        | Some fable, CompileCode(fsharpCode, language, otherFSharpOptions) ->
            try
                let! (compiledCode, errors, stats) =
                    compileCode
                        fable
                        FILE_NAME
                        ([| FILE_NAME |])
                        ([| fsharpCode |])
                        language
                        otherFSharpOptions

                CompilationFinished(compiledCode, language, errors, stats)
                |> state.Worker.Post
            with er ->
                JS.console.error er
                CompilerCrashed er.Message |> state.Worker.Post

            return! loop box state

        | Some fable, CompileFiles(fsharpCode, language, otherFSharpOptions) ->
            try
                let codes = fsharpCode |> Array.map (fun c -> c.Content)

                let names =
                    fsharpCode
                    |> Array.mapi (fun i c ->
                        if c.Name = "" then
                            $"test{i}.fs"
                        else
                            c.Name
                    )

                let! results =
                    names
                    |> Array.map (fun name ->
                        compileCode
                            fable
                            name
                            names
                            codes
                            language
                            otherFSharpOptions
                    )
                    |> asyncSequential

                let code, errors, stats =
                    results
                    |> Array.map (fun (a, b, c) -> [| a |], b, c)
                    |> Array.reduce (fun (a, b, c) (d, e, f) ->
                        Array.append a d, // Compiled code
                        Array.append b e, // Errors
                        combineStats c f // Stats
                    )

                CompilationsFinished(code, language, errors, stats)
                |> state.Worker.Post
            with er ->
                JS.console.error er
                CompilerCrashed er.Message |> state.Worker.Post

            return! loop box state

        | Some fable, GetTooltip(id, line, col, lineText) ->
            let tooltipLines =
                match FILE_NAME |> state.CurrentResults.TryFind with
                | None -> [||]
                | Some res ->
                    fable.Manager.GetToolTipText(
                        res,
                        int line,
                        int col,
                        lineText
                    )

            FoundTooltip(id, tooltipLines) |> state.Worker.Post
            return! loop box state

        | Some fable, GetCompletions(id, line, col, lineText) ->
            let completions =
                match FILE_NAME |> state.CurrentResults.TryFind with
                | None -> [||]
                | Some res ->
                    fable.Manager.GetCompletionsAtLocation(
                        res,
                        int line,
                        int col,
                        lineText
                    )

            FoundCompletions(id, completions) |> state.Worker.Post
            return! loop box state

        | Some fable, GetDeclarationLocation(id, line, col, lineText) ->
            let result =
                match FILE_NAME |> state.CurrentResults.TryFind with
                | None -> None
                | Some res ->
                    fable.Manager.GetDeclarationLocation(
                        res,
                        int line,
                        int col,
                        lineText
                    )

            match result with
            | Some x ->
                FoundDeclarationLocation(
                    id,
                    Some(x.StartLine, x.StartColumn, x.EndLine, x.EndColumn)
                )
            | None -> FoundDeclarationLocation(id, None)
            |> state.Worker.Post

            return! loop box state

        | Some fable, GetTooltipForFile(id, file, line, col, lineText) ->
            let tooltipLines =
                match file |> state.CurrentResults.TryFind with
                | None -> [||]
                | Some res ->
                    fable.Manager.GetToolTipText(
                        res,
                        int line,
                        int col,
                        lineText
                    )

            FoundTooltip(id, tooltipLines) |> state.Worker.Post
            return! loop box state

        | Some fable, GetCompletionsForFile(id, file, line, col, lineText) ->
            let completions =
                match file |> state.CurrentResults.TryFind with
                | None -> [||]
                | Some res ->
                    fable.Manager.GetCompletionsAtLocation(
                        res,
                        int line,
                        int col,
                        lineText
                    )

            FoundCompletions(id, completions) |> state.Worker.Post
            return! loop box state

        | Some fable,
          GetDeclarationLocationForFile(id, file, line, col, lineText) ->
            let result =
                match file |> state.CurrentResults.TryFind with
                | None -> None
                | Some res ->
                    fable.Manager.GetDeclarationLocation(
                        res,
                        int line,
                        int col,
                        lineText
                    )

            match result with
            | Some x ->
                FoundDeclarationLocation(
                    id,
                    Some(x.StartLine, x.StartColumn, x.EndLine, x.EndColumn)
                )
            | None -> FoundDeclarationLocation(id, None)
            |> state.Worker.Post

            return! loop box state
    }

let worker = ObservableWorker(self, WorkerRequest.Decoder)

let box =
    MailboxProcessor.Start(fun box ->
        {
            Fable = None
            Worker = worker
            CurrentResults = Map.empty
        }
        |> loop box
    )

worker |> Observable.add box.Post
