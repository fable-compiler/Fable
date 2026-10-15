module Fable.Tests.AsyncTryCancelled

open System
open System.Threading

let private check expected actual =
    if expected <> actual then failwithf "Expected %A, got %A" expected actual

let private runOffThreadPool action =
#if FABLE_COMPILER
    action ()
#else
    // .NET reschedules deferred FromContinuations callbacks invoked on a pool thread.
    // Use a dedicated thread so the probe can observe completion and thrown callbacks.
    let mutable failure = None
    let thread = Thread(ThreadStart(fun () ->
        try action ()
        with error -> failure <- Some error))
    thread.Start()
    thread.Join()
    failure |> Option.iter raise
#endif

let private observe token (work: Async<int>) =
    let calls = ResizeArray<string>()
    Async.StartWithContinuations(
        work,
        (fun value -> calls.Add(string value)),
        (fun error -> calls.Add("error: " + error.Message)),
        (fun error -> calls.Add("cancel: " + error.Message)),
        token)
    calls

let successAndFailure () =
    let mutable compensations = 0
    let wrap work = Async.TryCancelled(work, fun _ -> compensations <- compensations + 1)
    check [|"42"|] ((observe CancellationToken.None (wrap (async.Return 42))).ToArray())
    for error in [Exception "body"; OperationCanceledException "thrown"] do
        let work: Async<int> = async { return raise error }
        check [|"error: " + error.Message|] ((observe CancellationToken.None (wrap work)).ToArray())
    let mutable completed = false
    Async.StartWithContinuations(
        Async.TryCancelled(async.Return (), fun _ -> compensations <- compensations + 1),
        (fun () -> completed <- true), raise, raise)
    check true completed
    check 0 compensations

let cancellationIdentityAndReuse () =
    for throws in [false; true] do
        let original = OperationCanceledException "original"
        let calls = ResizeArray<string>()
        let mutable handlerSawOriginal = true
        let body: Async<int> = Async.FromContinuations(fun (_, _, cancel) -> cancel original)
        let work = Async.TryCancelled(body, fun error ->
            handlerSawOriginal <- handlerSawOriginal && obj.ReferenceEquals(original, error)
            calls.Add "compensation"
            if throws then failwith "compensation defect")
        for _ in 1 .. 2 do
            Async.StartWithContinuations(work,
                (fun _ -> failwith "unexpected success"),
                (fun _ -> failwith "unexpected error"),
                (fun error ->
                    check true (obj.ReferenceEquals(original, error))
                    calls.Add "cancel"))
        check [|"compensation"; "cancel"; "compensation"; "cancel"|] (calls.ToArray())
        check true handlerSawOriginal

let preCancelledNesting () =
    use source = new CancellationTokenSource()
    source.Cancel()
    let mutable entered = false
    let calls = ResizeArray<string>()
    let body = async { entered <- true; return 42 }
    let inner = Async.TryCancelled(body, fun _ -> calls.Add "inner")
    let outer = Async.TryCancelled(inner, fun _ -> calls.Add "outer")
    Async.StartWithContinuations(outer,
        (fun _ -> failwith "unexpected success"), raise,
        (fun _ -> calls.Add "cancel"), source.Token)
    check false entered
    check [|"inner"; "outer"; "cancel"|] (calls.ToArray())

let deferredCancellation () = runOffThreadPool (fun () ->
    let original = OperationCanceledException "deferred"
    let mutable cancel = fun (_: OperationCanceledException) -> failwith "not started"
    let calls = ResizeArray<string>()
    let body: Async<int> = Async.FromContinuations(fun (_, _, continuation) -> cancel <- continuation)
    let work = Async.TryCancelled(body, fun _ -> calls.Add "compensation")
    Async.StartWithContinuations(work,
        (fun _ -> failwith "unexpected success"), raise,
        (fun error -> check true (obj.ReferenceEquals(original, error)); calls.Add "cancel"))
    check 0 calls.Count
    cancel original
    check [|"compensation"; "cancel"|] (calls.ToArray()))

let finalizerOrdering () =
    let calls = ResizeArray<string>()
    let body: Async<int> = Async.FromContinuations(fun (_, _, cancel) -> cancel (OperationCanceledException "original"))
    let finalized = async.TryFinally(body, fun () -> calls.Add "finally")
    let inner = Async.TryCancelled(finalized, fun _ -> calls.Add "inner"; failwith "inner defect")
    let outer = Async.TryCancelled(inner, fun _ -> calls.Add "outer"; failwith "outer defect")
    check [|"cancel: original"|] ((observe CancellationToken.None outer).ToArray())
    check [|"finally"; "inner"; "outer"|] (calls.ToArray())

let downstreamExceptions () = runOffThreadPool (fun () ->
    for channel in ["success"; "error"; "cancel"] do
        let mutable compensations = 0
        let calls = ResizeArray<string>()
        let mutable complete = fun () -> failwith "not started"
        let body: Async<int> = Async.FromContinuations(fun (ok, error, cancel) ->
            complete <- fun () ->
                match channel with
                | "success" -> ok 42
                | "error" -> error (Exception "body")
                | _ -> cancel (OperationCanceledException "original"))
        let work = Async.TryCancelled(body, fun _ -> compensations <- compensations + 1)
        let terminal name = calls.Add name; failwith "downstream"
        Async.StartWithContinuations(work,
            (fun _ -> terminal "success"),
            (fun _ -> terminal "error"),
            (fun _ -> terminal "cancel"))
        let caught =
            try
                complete ()
                "no exception"
            with error -> error.Message
        check "downstream" caught
        check [|channel|] (calls.ToArray())
        check (if channel = "cancel" then 1 else 0) compensations)

let trampoline () = async {
    let original = OperationCanceledException "deep cancellation"
    let mutable compensations = 0
    let rec loop remaining =
        if remaining = 0 then
            Async.FromContinuations(fun (_, _, cancel) -> cancel original)
        else
            Async.TryCancelled(async {
                do! async.Return ()
                return! loop (remaining - 1)
            }, fun _ -> compensations <- compensations + 1)
    do! Async.FromContinuations(fun (ok, error, _) ->
        Async.StartWithContinuations(loop 10000,
            (fun () -> error (Exception "unexpected success")), error,
            (fun cancelled ->
                if obj.ReferenceEquals(original, cancelled) then ok ()
                else error (Exception "cancellation identity changed"))))
    check 10000 compensations
}
