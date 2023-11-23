namespace Fable.WebWorker

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Thoth.Json

type FSharpCodeFile =
    {
        Name: string
        Content: string
    }

type WorkerRequest =
    /// * refsExtraSuffix: e.g. add .txt extension to enable gzipping in Github Pages
    | CreateChecker of
        refsDirUrl: string *
        extraRefs: string[] *
        refsExtraSuffix: string option *
        otherFSharpOptions: string[]
    | ParseCode of fsharpCode: string * otherFSharpOptions: string[]
    | ParseFile of
        file: string *
        fsharpCode: FSharpCodeFile[] *
        otherFSharpOptions: string[]
    | CompileCode of
        fsharpCode: string *
        language: string *
        otherFSharpOptions: string[]
    | CompileFiles of
        fsharpCode: FSharpCodeFile[] *
        language: string *
        otherFSharpOptions: string[]
    | GetTooltip of id: Guid * line: int * column: int * lineText: string
    | GetCompletions of id: Guid * line: int * column: int * lineText: string
    | GetDeclarationLocation of
        id: Guid *
        line: int *
        column: int *
        lineText: string
    | GetTooltipForFile of
        id: Guid *
        file: string *
        line: int *
        column: int *
        lineText: string
    | GetCompletionsForFile of
        id: Guid *
        file: string *
        line: int *
        column: int *
        lineText: string
    | GetDeclarationLocationForFile of
        id: Guid *
        file: string *
        line: int *
        column: int *
        lineText: string

    static member Decoder = Decode.Auto.generateDecoder<WorkerRequest> ()

type CompileStats =
    {
        FCS_checker: float
        FCS_parsing: float
        Fable_transform: float
    }

type WorkerAnswer =
    | Loaded of version: string
    | LoadFailed
    | ParsedCode of errors: Fable.Standalone.Error[]
    | CompilationFinished of
        code: string *
        language: string *
        errors: Fable.Standalone.Error[] *
        stats: CompileStats
    | CompilationsFinished of
        code: string[] *
        language: string *
        errors: Fable.Standalone.Error[] *
        stats: CompileStats
    | CompilerCrashed of message: string
    | FoundTooltip of id: Guid * lines: string[]
    | FoundCompletions of id: Guid * Fable.Standalone.Completion[]
    | FoundDeclarationLocation of
        id: Guid (* line1, col1, line2, col2 *) *
        (int * int * int * int) option

    static member Decoder = Decode.Auto.generateDecoder<WorkerAnswer> ()

type ObservableWorker<'InMsg>
    (worker: obj, decoder: Decoder<'InMsg>, ?name: string)
    =
    let name = defaultArg name "FABLE WORKER"
    let listeners = Dictionary<Guid, IObserver<'InMsg>>()

    do
        worker?addEventListener (
            "message",
            fun ev ->
                match ev?data: obj with
                | :? string as msg when not (String.IsNullOrEmpty(msg)) ->
                    match Decode.fromString decoder msg with
                    | Ok msg ->
                        // JS.console.log("[" + name + "] Received:", msg)
                        for listener in listeners.Values do
                            listener.OnNext(msg)
                    | Error err ->
                        JS.console.error ("[" + name + "] Cannot decode:", err)
                | _ -> ()
        )

    member _.Worker = worker
    member _.HasListeners = listeners.Count > 0

    member inline this.Post(msg: 'OutMsg) : unit =
        this.Worker?postMessage(Encode.Auto.toString (0, msg))

    member inline this.PostAndAwaitResponse
        (
            msg: 'OutMsg,
            picker: 'InMsg -> 'Res option
        )
        : Async<'Res>
        =
        Async.FromContinuations(fun (cont, _err, _cancel) ->
            let mutable disp = Unchecked.defaultof<IDisposable>

            disp <-
                this
                |> Observable.subscribe (fun msg ->
                    match picker msg with
                    | Some res ->
                        disp.Dispose()
                        cont res
                    | None -> ()
                )

            this.Worker?postMessage(Encode.Auto.toString (0, msg))
        )

    member _.Subscribe obs =
        let id = Guid.NewGuid()
        listeners.Add(id, obs)

        { new IDisposable with
            member _.Dispose() = listeners.Remove(id) |> ignore
        }

    interface IObservable<'InMsg> with
        member this.Subscribe obs = this.Subscribe(obs)
