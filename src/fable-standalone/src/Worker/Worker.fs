module Fable.WebWorker.Main

open Fable.Core
open Fable.Core.JsInterop
open Fable.Standalone
open Fable.WebWorker

let FILE_NAME = "test.fs"
let PROJECT_NAME = "project.fsproj"

type IFableInit =
    abstract member init: unit -> IFableManager

let [<Global>] self: obj = jsNative
let [<Global>] importScripts(_path: string): unit = jsNative

// Load FCS+Fable bundle
importScripts "bundle.min.js"
let [<Global("__FABLE_STANDALONE__")>] FableInit: IFableInit = jsNative

let getAssemblyReader(_getBlobUrl: string->string, _refs: string[]): JS.Promise<string->byte[]> = importMember "./util.js"
let escapeJsStringLiteral (str: string): string = importMember "./util.js"

let measureTime f arg =
    let before: float = self?performance?now()
    let res = f arg
    let after: float = self?performance?now()
    res, after - before

type FableState =
    { Manager: IFableManager
      Checker: IChecker
      LoadTime: float
      References: string[]
      Reader: string->byte[]
      OtherFSharpOptions: string[] }

type FableStateConfig =
    | Init of refsDirUrl: string * extraRefs: string[] * refsExtraSuffix: string option
    | Initialized of FableState

type State =
    { Fable: FableState option
      Worker: ObservableWorker<WorkerRequest>
      CurrentResults: IParseResults option }

type SourceWriter(sourceMaps: bool) =
    let sb = System.Text.StringBuilder()
    interface Fable.Standalone.IWriter with
        member _.Write(str) = async { return sb.Append(str) |> ignore }
        member _.EscapeJsStringLiteral(str) = escapeJsStringLiteral(str)
        member _.MakeImportPath(path) = path
        member _.AddSourceMapping(mapping) = ()
        member _.Dispose() = ()
    member __.Result = sb.ToString()

let makeFableState (config: FableStateConfig) otherFSharpOptions =
    async {
        match config with
        | Init(refsDirUrl, extraRefs, refsExtraSuffix) ->
            let getBlobUrl name =
                refsDirUrl.TrimEnd('/') + "/" + name + ".dll" + (defaultArg refsExtraSuffix "")
            let manager = FableInit.init()
            let references = Array.append Fable.Metadata.coreAssemblies extraRefs
            let! reader = getAssemblyReader(getBlobUrl, references) |> Async.AwaitPromise
            let (checker, checkerTime) = measureTime (fun () ->
                manager.CreateChecker(references, reader, otherFSharpOptions)) ()
            return { Manager = manager
                     Checker = checker
                     LoadTime = checkerTime
                     References = references
                     Reader = reader
                     OtherFSharpOptions = otherFSharpOptions }

        | Initialized fable ->
            // We don't need to recreate the checker
            if fable.OtherFSharpOptions = otherFSharpOptions then
                return fable
            else
                let (checker, checkerTime) = measureTime (fun () ->
                    fable.Manager.CreateChecker(fable.References, fable.Reader, otherFSharpOptions)) ()
                return { fable with Checker = checker
                                    LoadTime = checkerTime
                                    OtherFSharpOptions = otherFSharpOptions }
    }

let rec loop (box: MailboxProcessor<WorkerRequest>) (state: State) = async {
    let! msg = box.Receive()
    match state.Fable, msg with
    | None, CreateChecker(refsDirUrl, extraRefs, refsExtraSuffix, otherFSharpOptions) ->
        try
            let! fable = makeFableState (Init(refsDirUrl, extraRefs, refsExtraSuffix)) otherFSharpOptions
            state.Worker.Post(Loaded fable.Manager.Version)
            return! loop box { state with Fable = Some fable }
        with err ->
            JS.console.error("Cannot create F# checker", err)
            state.Worker.Post LoadFailed
            return! loop box state

    // These combination of messages are ignored
    | None, _
    | Some _, CreateChecker _ -> return! loop box state

    | Some fable, ParseCode(fsharpCode, otherFSharpOptions) ->
        // Check if we need to recreate the FableState because otherFSharpOptions have changed
        let! fable = makeFableState (Initialized fable) otherFSharpOptions
        // let res = fable.Manager.ParseFSharpScript(fable.Checker, FILE_NAME, fsharpCode, otherFSharpOptions)
        let res = fable.Manager.ParseFSharpFileInProject(fable.Checker, FILE_NAME, PROJECT_NAME, [|FILE_NAME|], [|fsharpCode|], otherFSharpOptions)

        ParsedCode res.Errors |> state.Worker.Post
        return! loop box { state with CurrentResults = Some res }

    | Some fable, CompileCode(fsharpCode, otherFSharpOptions) ->
        try
            // detect (and remove) the non-F# compiler options to avoid changing msg contract
            let nonFSharpOptions = set [
                "--typedArrays"
                "--clampByteArrays"
                "--typescript"
                "--sourceMaps"
            ]
            let fableOptions, otherFSharpOptions =
                otherFSharpOptions |> Array.partition (fun x -> Set.contains x nonFSharpOptions)

            // Check if we need to recreate the FableState because otherFSharpOptions have changed
            let! fable = makeFableState (Initialized fable) otherFSharpOptions
            let (parseResults, parsingTime) = measureTime (fun () ->
                // fable.Manager.ParseFSharpScript(fable.Checker, FILE_NAME, fsharpCode, otherFSharpOptions)) ()
                fable.Manager.ParseFSharpFileInProject(fable.Checker, FILE_NAME, PROJECT_NAME, [|FILE_NAME|], [|fsharpCode|], otherFSharpOptions)) ()

            let! jsCode, errors, fableTransformTime = async {
                if parseResults.Errors |> Array.exists (fun e -> not e.IsWarning) then
                    return "", parseResults.Errors, 0.
                else
                    let options = {|
                        typedArrays = Array.contains "--typedArrays" fableOptions
                        typescript = Array.contains "--typescript" fableOptions
                        sourceMaps = Array.contains "--sourceMaps" fableOptions
                    |}
                    let (res, fableTransformTime) =
                        measureTime (fun () ->
                            fable.Manager.CompileToBabelAst("fable-library", parseResults, FILE_NAME, typedArrays = options.typedArrays, typescript = options.typescript)
                        ) ()
                    // Print Babel AST
                    let writer = new SourceWriter(options.sourceMaps)
                    do! fable.Manager.PrintBabelAst(res, writer)
                    let jsCode = writer.Result

                    return jsCode, Array.append parseResults.Errors res.FableErrors, fableTransformTime
            }

            let stats : CompileStats =
                { FCS_checker = fable.LoadTime
                  FCS_parsing = parsingTime
                  Fable_transform = fableTransformTime }

            CompilationFinished (jsCode, errors, stats) |> state.Worker.Post
        with er ->
            JS.console.error er
            CompilerCrashed er.Message |> state.Worker.Post
        return! loop box state

    | Some fable, GetTooltip(id, line, col, lineText) ->
        let tooltipLines =
            match state.CurrentResults with
            | None -> [||]
            | Some res -> fable.Manager.GetToolTipText(res, int line, int col, lineText)
        FoundTooltip(id, tooltipLines) |> state.Worker.Post
        return! loop box state

    | Some fable, GetCompletions(id, line, col, lineText) ->
        let completions =
            match state.CurrentResults with
            | None -> [||]
            | Some res -> fable.Manager.GetCompletionsAtLocation(res, int line, int col, lineText)
        FoundCompletions(id, completions) |> state.Worker.Post
        return! loop box state

    | Some fable, GetDeclarationLocation(id, line, col, lineText) ->
        let result =
            match state.CurrentResults with
            | None -> None
            | Some res -> fable.Manager.GetDeclarationLocation(res, int line, int col, lineText)
        match result with
        | Some x -> FoundDeclarationLocation(id, Some(x.StartLine, x.StartColumn, x.EndLine, x.EndColumn))
        | None -> FoundDeclarationLocation(id, None)
        |> state.Worker.Post
        return! loop box state
}

let worker = ObservableWorker(self, WorkerRequest.Decoder)
let box = MailboxProcessor.Start(fun box ->
    { Fable = None
      Worker = worker
      CurrentResults = None }
    |> loop box)

worker
|> Observable.add box.Post
