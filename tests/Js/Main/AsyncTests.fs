module Fable.Tests.Async

open System
open System.Threading
open Util
open Util.Testing

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

let private observeTryCancelled token (work: Async<int>) =
    let calls = ResizeArray<string>()
    Async.StartWithContinuations(
        work,
        (fun value -> calls.Add(string value)),
        (fun error -> calls.Add("error: " + error.Message)),
        (fun error -> calls.Add("cancel: " + error.Message)),
        token)
    calls

module private FinalizerRegressions =
    open System
    open System.Threading

    let private check expected actual =
        if expected <> actual then failwithf "Expected %A, got %A" expected actual

    let private observe (token: CancellationToken) work =
        let terminal = ResizeArray<string>()
        Async.StartWithContinuations(work, (fun _ -> terminal.Add "success"), (fun e -> terminal.Add e.Message), (fun _ -> terminal.Add "cancel"), token)
        terminal

    let preCancelled () =
        for throws in [false; true] do
            use source = new CancellationTokenSource()
            source.Cancel()
            let mutable cleanups = 0
            let mutable entered = false
            let work = async.TryFinally(async { entered <- true }, fun () ->
                cleanups <- cleanups + 1
                if throws then failwith "cleanup")
            let terminal = observe source.Token work
            check false entered
            check 1 cleanups
            check [|"cancel"|] (terminal.ToArray())

    let nestedPreCancelled () =
        use source = new CancellationTokenSource()
        source.Cancel()
        let cleanups = ResizeArray<string>()
        let inner = async.TryFinally(async.Return 42, fun () -> cleanups.Add "inner")
        let outer = async.TryFinally(inner, fun () -> cleanups.Add "outer")
        let terminal = observe source.Token outer
        check [|"outer"|] (cleanups.ToArray())
        check [|"cancel"|] (terminal.ToArray())

    let cancelledAcquisition () =
        for acquisition in [0; 1; 2] do
            use source = new CancellationTokenSource()
            let mutable disposals = 0
            let mutable entered = false
            let resource = { new IDisposable with member _.Dispose() = disposals <- disposals + 1 }
            let acquire = Async.FromContinuations(fun (ok, _, _) ->
                source.Cancel()
                ok resource)
            let work =
                if acquisition = 1 then async {
                    use! owned = acquire
                    entered <- true
                }
                elif acquisition = 0 then async {
                    let! acquired = acquire
                    use owned = acquired
                    entered <- true
                }
                else async {
                    use owned =
                        source.Cancel()
                        resource
                    entered <- true
                }
            let terminal = observe source.Token work
            check false entered
            check 1 disposals
            check [|"cancel"|] (terminal.ToArray())

    let normalAndFailure () =
        for fails in [false; true] do
            let mutable disposals = 0
            let resource = { new IDisposable with member _.Dispose() = disposals <- disposals + 1 }
            let work = async {
                use owned = resource
                if fails then failwith "body"
                return 42
            }
            let terminal = observe CancellationToken.None work
            check 1 disposals
            check [|if fails then "body" else "success"|] (terminal.ToArray())

    let trampoline () = async {
        let mutable cleanups = 0
        for _ in 1 .. 10000 do
            do! async.TryFinally(async { return () }, fun () -> cleanups <- cleanups + 1)
        check 10000 cleanups
    }

    let downstreamExceptions () =
        for channel in ["success"; "error"; "cancel"] do
            let mutable cleanups = 0
            let calls = ResizeArray<string>()
            let body: Async<unit> = Async.FromContinuations(fun (ok, error, cancelled) ->
                match channel with
                | "success" -> ok ()
                | "error" -> error (Exception "body")
                | _ -> cancelled (OperationCanceledException()))
            let terminal name = calls.Add name; failwith "downstream"
            let caught =
                try
                    Async.StartWithContinuations(
                        async.TryFinally(body, fun () -> cleanups <- cleanups + 1),
                        (fun () -> terminal "success"), (fun _ -> terminal "error"), (fun _ -> terminal "cancel"))
                    "no exception"
                with e -> e.Message
            check "downstream" caught
            check 1 cleanups
            check [|channel|] (calls.ToArray())

#if FABLE_COMPILER
open Fable.Core

type private Deferred =
    [<ImportMember("./js/async-deferred.js")>]
    static member startDeferred(computation: Async<int>, token: System.Threading.CancellationToken, onSuccess: int -> unit, onError: exn -> unit, onCancel: exn -> unit): unit -> unit = jsNative

    [<ImportMember("./js/async-deferred.js")>]
    static member handlerCount(event: Event<int>): int = jsNative

module private Runtime =
    open Fable.Core.JsInterop

    type Timer =
        abstract retired: int
        abstract invalidHandles: int
        abstract fire: unit -> unit

    let withTimers (action: Timer -> unit): unit = importMember "./js/async-runtime.js"
    let listenerCount (token: System.Threading.CancellationToken): int = importMember "./js/async-runtime.js"
    let cancellationErrorCount (error: exn): int = importMember "./js/async-runtime.js"
#endif

type DisposableAction(f) =
    interface IDisposable with
        member _.Dispose() = f()

let successWork: Async<string> = Async.FromContinuations(fun (onSuccess,_,_) -> onSuccess "success")
let errorWork: Async<string> = Async.FromContinuations(fun (_,onError,_) -> onError (exn "error"))
let cancelWork: Async<string> = Async.FromContinuations(fun (_,_,onCancel) ->
        System.OperationCanceledException("cancelled") |> onCancel)

type Message = string * AsyncReplyChannel<string>

type Get =
    | GetZero of replyChannel: AsyncReplyChannel<int>
    | GetOne of replyChannel: AsyncReplyChannel<int>

type MyException(value) =
    inherit Exception()
    member _.Value: int = value

let sleepAndAssign token res =
    Async.StartImmediate(async {
        do! Async.Sleep 200
        res := true
    }, token)

let asyncMap f a = async {
    let! a = a
    return f a
}

let private fromGenericCallback (callback: ('T -> unit) -> (exn -> unit) -> (OperationCanceledException -> unit) -> unit) : Async<'T> =
    Async.FromContinuations(fun (ok, error, cancelled) -> callback ok error cancelled)

let tests =
  testList "Async" [
    testCase "TryCancelled preserves success and ordinary errors" <| fun () ->
        let mutable compensations = 0
        let wrap work = Async.TryCancelled(work, fun _ -> compensations <- compensations + 1)
        equal [|"42"|] ((observeTryCancelled CancellationToken.None (wrap (async.Return 42))).ToArray())
        for error in [Exception "body"; OperationCanceledException "thrown"] do
            let work: Async<int> = async { return raise error }
            equal [|"error: " + error.Message|] ((observeTryCancelled CancellationToken.None (wrap work)).ToArray())
        let mutable completed = false
        Async.StartWithContinuations(
            Async.TryCancelled(async.Return (), fun _ -> compensations <- compensations + 1),
            (fun () -> completed <- true), raise, raise)
        equal true completed
        equal 0 compensations

    testCase "TryCancelled preserves cancellation identity and supports reuse" <| fun () ->
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
                        equal true (obj.ReferenceEquals(original, error))
                        calls.Add "cancel"))
            equal [|"compensation"; "cancel"; "compensation"; "cancel"|] (calls.ToArray())
            equal true handlerSawOriginal

    testCase "TryCancelled installs nested handlers before cancellation" <| fun () ->
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
        equal false entered
        equal [|"inner"; "outer"; "cancel"|] (calls.ToArray())

    testCase "TryCancelled handles deferred cancellation" <| fun () ->
        runOffThreadPool (fun () ->
            let original = OperationCanceledException "deferred"
            let mutable cancel = fun (_: OperationCanceledException) -> failwith "not started"
            let calls = ResizeArray<string>()
            let body: Async<int> = Async.FromContinuations(fun (_, _, continuation) -> cancel <- continuation)
            let work = Async.TryCancelled(body, fun _ -> calls.Add "compensation")
            Async.StartWithContinuations(work,
                (fun _ -> failwith "unexpected success"), raise,
                (fun error -> equal true (obj.ReferenceEquals(original, error)); calls.Add "cancel"))
            equal 0 calls.Count
            cancel original
            equal [|"compensation"; "cancel"|] (calls.ToArray()))

    testCase "TryCancelled preserves finalizer order when handlers throw" <| fun () ->
        let calls = ResizeArray<string>()
        let body: Async<int> = Async.FromContinuations(fun (_, _, cancel) -> cancel (OperationCanceledException "original"))
        let finalized = async.TryFinally(body, fun () -> calls.Add "finally")
        let inner = Async.TryCancelled(finalized, fun _ -> calls.Add "inner"; failwith "inner defect")
        let outer = Async.TryCancelled(inner, fun _ -> calls.Add "outer"; failwith "outer defect")
        equal [|"cancel: original"|] ((observeTryCancelled CancellationToken.None outer).ToArray())
        equal [|"finally"; "inner"; "outer"|] (calls.ToArray())

    testCase "TryCancelled propagates downstream errors once" <| fun () ->
        runOffThreadPool (fun () ->
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
                equal "downstream" caught
                equal [|channel|] (calls.ToArray())
                equal (if channel = "cancel" then 1 else 0) compensations)

    testCaseAsync "TryCancelled preserves bounded trampoline execution" <| fun () -> async {
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
        equal 10000 compensations
    }

    testCase "TryFinally installs cleanup before pre-cancellation" <| FinalizerRegressions.preCancelled
    testCase "Pre-cancelled nested TryFinally only runs outer cleanup" <| FinalizerRegressions.nestedPreCancelled
    testCase "Cancelled acquisition disposes use and use bang resources" <| FinalizerRegressions.cancelledAcquisition
    testCase "Resource cleanup runs once on success and failure" <| FinalizerRegressions.normalAndFailure
    testCase "TryFinally propagates downstream continuation errors once" <| FinalizerRegressions.downstreamExceptions
    testCaseAsync "TryFinally preserves bounded trampoline execution" <| FinalizerRegressions.trampoline

    testCase "Generic FromContinuations accepts erased unit callbacks" <| fun () ->
        let calls = ResizeArray<string>()
        let start work = Async.StartWithContinuations(work, (fun _ -> calls.Add "success"), (fun e -> calls.Add e.Message), (fun e -> calls.Add e.Message))
        let unitWork: Async<unit> = fromGenericCallback (fun ok _ _ -> ok ())
        start unitWork
        let valueWork = fromGenericCallback (fun ok _ _ -> ok 42)
        Async.StartWithContinuations(valueWork, (fun value -> equal 42 value; calls.Add "value"), raise, raise)
        let errorWork: Async<unit> = fromGenericCallback (fun _ error _ -> error (exn "error"))
        start errorWork
        let cancelWork: Async<unit> = fromGenericCallback (fun _ _ cancelled -> cancelled (OperationCanceledException("cancel")))
        start cancelWork
        equal [|"success"; "value"; "error"; "cancel"|] (calls.ToArray())

    testCase "FromContinuations unit and generic values" <| fun () ->
        let fromValue value = Async.FromContinuations(fun (ok, _, _) -> ok value)
        let mutable completed = 0
        let unitWork: Async<unit> = Async.FromContinuations(fun (ok, _, _) -> ok ())
        Async.StartWithContinuations(unitWork, (fun () -> completed <- completed + 1), raise, raise)
        Async.StartWithContinuations(fromValue (), (fun () -> completed <- completed + 1), raise, raise)
        Async.StartWithContinuations(fromValue 42, (fun value -> equal 42 value; completed <- completed + 1), raise, raise)
        equal 3 completed

    testCase "FromContinuations rejects a second completion" <| fun () ->
        let unitCalls = ResizeArray<string>()
        let unitWork: Async<unit> = Async.FromContinuations(fun (ok, _, _) -> ok (); ok ())
        throwsAnyError (fun () ->
            Async.StartWithContinuations(unitWork, (fun () -> unitCalls.Add "success"), (fun _ -> unitCalls.Add "error"), (fun _ -> unitCalls.Add "cancel")))
        equal true (unitCalls.Count <= 1)
        equal false (unitCalls.Contains "error")
        equal false (unitCalls.Contains "cancel")

        let valueCalls = ResizeArray<string>()
        let valueWork = Async.FromContinuations(fun (ok, _, _) -> ok 42; ok 43)
        throwsAnyError (fun () ->
            Async.StartWithContinuations(valueWork, (fun v -> valueCalls.Add (string v)), (fun _ -> valueCalls.Add "error"), (fun _ -> valueCalls.Add "cancel")))
        equal true (valueCalls.Count <= 1)
        equal false (valueCalls.Contains "43")
        equal false (valueCalls.Contains "error")
        equal false (valueCalls.Contains "cancel")

    testCase "Generic unit continuations work through recursive bindings and aliases" <| fun () ->
        let mutable recurse = true
        let rec fromValue value =
            if recurse then
                recurse <- false
                fromValue value
            else Async.FromContinuations(fun (ok, _, _) -> ok value)
        let alias = fromValue
        let mutable completed = 0
        Async.StartWithContinuations(fromValue (), (fun () -> completed <- completed + 1), raise, raise)
        recurse <- true
        Async.StartWithContinuations(alias (), (fun () -> completed <- completed + 1), raise, raise)
        Async.StartWithContinuations(alias 42, (fun value -> equal 42 value; completed <- completed + 1), raise, raise)
        equal 3 completed

    testCase "Generic unit continuations work through tuple pattern bindings" <| fun () ->
        let fromValue, fromOther =
            (fun (value: 'T) -> Async.FromContinuations(fun (ok, _, _) -> ok value)),
            (fun (value: 'U) -> Async.FromContinuations(fun (ok, _, _) -> ok value))
        let mutable completed = 0
        Async.StartWithContinuations(fromValue (), (fun () -> completed <- completed + 1), raise, raise)
        Async.StartWithContinuations(fromValue 42, (fun value -> equal 42 value; completed <- completed + 1), raise, raise)
        Async.StartWithContinuations(fromOther (), (fun () -> completed <- completed + 1), raise, raise)
        Async.StartWithContinuations(fromOther "a", (fun value -> equal "a" value; completed <- completed + 1), raise, raise)
        equal 4 completed

    testCase "Generic unit continuations work inside nested lambdas and object expressions" <| fun () ->
        let fromValue value = Async.FromContinuations(fun (ok, _, _) -> ok value)
        let mutable completed = 0
        let startUnit () = Async.StartWithContinuations(fromValue (), (fun () -> completed <- completed + 1), raise, raise)
        let disposable =
            { new IDisposable with
                member _.Dispose() =
                    Async.StartWithContinuations(fromValue (), (fun () -> completed <- completed + 1), raise, raise) }
        startUnit ()
        startUnit ()
        disposable.Dispose()
        Async.StartWithContinuations(fromValue 42, (fun value -> equal 42 value; completed <- completed + 1), raise, raise)
        equal 4 completed

    testCase "Unit continuations preserve captured callbacks" <| fun () ->
        let mutable calls = 0
        let rec genericDeadline callback value remaining =
            if remaining > 0 then genericDeadline callback value (remaining - 1)
            else Async.StartWithContinuations(async { return value }, (fun result -> callback result), raise, raise)
        let rec unitDeadline (callback: unit -> unit) remaining =
            if remaining > 0 then unitDeadline callback (remaining - 1)
            else Async.StartWithContinuations(async { return () }, (fun () -> callback ()), raise, raise)
        genericDeadline (fun () -> calls <- calls + 1) () 2
        unitDeadline (fun () -> calls <- calls + 1) 2
        equal 2 calls

    testCase "Finalizer failure overrides success once" <| fun () ->
        let mutable finalizers = 0
        let terminal = ResizeArray<string>()
        let work = async {
            try return! successWork
            finally
                finalizers <- finalizers + 1
                failwith "finalizer"
        }
        Async.StartWithContinuations(work, (fun _ -> terminal.Add "success"), (fun error -> terminal.Add error.Message), (fun _ -> terminal.Add "cancel"))
        equal [| "finalizer" |] (terminal.ToArray())
        equal 1 finalizers

    testCase "Finalizer failure overrides body failure once" <| fun () ->
        let mutable finalizers = 0
        let terminal = ResizeArray<string>()
        let work = async {
            try return! errorWork
            finally
                finalizers <- finalizers + 1
                failwith "finalizer"
        }
        Async.StartWithContinuations(work, (fun _ -> terminal.Add "success"), (fun error -> terminal.Add error.Message), (fun _ -> terminal.Add "cancel"))
        equal [| "finalizer" |] (terminal.ToArray())
        equal 1 finalizers

    testCase "Finalizer failure preserves cancellation once" <| fun () ->
        let mutable finalizers = 0
        let terminal = ResizeArray<string>()
        let work = async {
            try return! cancelWork
            finally
                finalizers <- finalizers + 1
                failwith "finalizer"
        }
        Async.StartWithContinuations(work, (fun _ -> terminal.Add "success"), (fun _ -> terminal.Add "error"), (fun _ -> terminal.Add "cancel"))
        equal [| "cancel" |] (terminal.ToArray())
        equal 1 finalizers

    testCase "Cancellation registrations dispose pending callbacks and invoke late callbacks" <| fun () ->
        use source = new System.Threading.CancellationTokenSource()
        let calls = ResizeArray<int>()
        let first: IDisposable = source.Token.Register(fun () -> calls.Add 1)
        let mutable second: IDisposable = null
        second <- source.Token.Register(fun () ->
            second.Dispose()
            first.Dispose()
            calls.Add 2)
        source.Cancel()
        let late: IDisposable = source.Token.Register((fun state -> calls.Add (unbox<int> state)), box 0)
        late.Dispose()
        second.Dispose()
        equal [| 2; 0 |] (calls.ToArray())
#if FABLE_COMPILER
        equal 0 (Runtime.listenerCount source.Token)
#endif

    testCase "Cancellation callback failure preserves remaining listeners" <| fun () ->
        use source = new System.Threading.CancellationTokenSource()
        let mutable remaining = 0
        let mutable errors = -1
        source.Token.Register(fun () -> remaining <- remaining + 1) |> ignore
        source.Token.Register(fun () -> failwith "callback") |> ignore
        try source.Cancel()
        with error ->
#if FABLE_COMPILER
            errors <- Runtime.cancellationErrorCount error
#else
            errors <- (error :?> AggregateException).InnerExceptions.Count
#endif
        equal 1 errors
        equal 1 remaining
#if FABLE_COMPILER
        equal 0 (Runtime.listenerCount source.Token)
#endif

#if FABLE_COMPILER
    testCase "Sleep settles once and retires its timer and listener" <| fun () ->
        for cancelFirst in [ true; false ] do
            for throws in [ true; false ] do
                Runtime.withTimers (fun timer ->
                    use source = new System.Threading.CancellationTokenSource()
                    let terminal = ResizeArray<string>()
                    let mutable finalizers = 0
                    let work = async {
                        try do! Async.Sleep 100
                        finally
                            finalizers <- finalizers + 1
                            if throws then failwith "finalizer"
                    }
                    Async.StartWithContinuations(work, (fun () -> terminal.Add "success"), (fun _ -> terminal.Add "error"), (fun _ -> terminal.Add "cancel"), source.Token)
                    if cancelFirst then
                        source.Cancel()
                        timer.fire()
                    else
                        timer.fire()
                        source.Cancel()
                    timer.fire()
                    let expected = if cancelFirst then "cancel" elif throws then "error" else "success"
                    equal [| expected |] (terminal.ToArray())
                    equal 1 finalizers
                    equal 0 (Runtime.listenerCount source.Token)
                    equal 1 timer.retired
                    equal 0 timer.invalidHandles)

    testCase "Idle Receive is cancelled by its token and keeps later posts queued" <| fun () ->
        use source = new System.Threading.CancellationTokenSource()
        let mailbox = new MailboxProcessor<int>((fun _ -> async { return () }), source.Token)
        let terminal = ResizeArray<string>()
        Async.StartWithContinuations(mailbox.Receive(), (fun value -> terminal.Add (string value)), raise, (fun _ -> terminal.Add "cancel"), source.Token)
        source.Cancel()
        equal [| "cancel" |] (terminal.ToArray())
        equal 0 (Runtime.listenerCount source.Token)
        mailbox.Post 42
        equal [| "cancel" |] (terminal.ToArray())
        Async.StartWithContinuations(mailbox.Receive(), (fun value -> terminal.Add $"next {value}"), raise, (fun _ -> terminal.Add "next cancel"))
        equal [| "cancel"; "next 42" |] (terminal.ToArray())
#endif

    testCaseAsync "Cancelling a waiting Receive reports cancel and keeps later posts queued" <| fun () -> async {
        use source = new System.Threading.CancellationTokenSource()
        let mailbox = new MailboxProcessor<int>((fun _ -> async { return () }), source.Token)
        let! outcome =
            Async.FromContinuations(fun (ok, _, _) ->
                Async.StartWithContinuations(mailbox.Receive(), (fun value -> ok $"value {value}"), (fun _ -> ok "error"), (fun _ -> ok "cancel"), source.Token)
                source.Cancel())
        equal "cancel" outcome
        mailbox.Post 42
        let! next = mailbox.Receive()
        equal 42 next
    }

    testCaseAsync "Cancelling the mailbox token cancels a body waiting in Receive" <| fun () -> async {
        use source = new System.Threading.CancellationTokenSource()
        let! outcome =
            Async.FromContinuations(fun (ok, _, _) ->
                MailboxProcessor<int>.Start((fun inbox -> async {
                    let received = ref None
                    try
                        source.CancelAfter 50
                        let! message = inbox.Receive()
                        received.Value <- Some message
                    finally
                        match received.Value with
                        | Some message -> ok $"received {message}"
                        | None -> ok "cancelled" }), source.Token)
                |> ignore)
        equal "cancelled" outcome
    }

    testCase "Simple async translates without exception" <| fun () ->
        async { return () }
        |> Async.StartImmediate

    testCase "Cancellation callback errors expose the first InnerException" <| fun () ->
        for count in [1; 2] do
            use cts = new System.Threading.CancellationTokenSource()
            let errors = Array.init count (fun i -> exn (string i))
            let mutable calls = 0
            errors |> Array.iter (fun error ->
                cts.Token.Register(fun () ->
                    calls <- calls + 1
                    raise error) |> ignore)
            let caught =
                try
                    cts.Cancel()
                    None
                with ex -> Some ex
            match caught with
            | Some ex ->
                equal errors[count - 1].Message ex.InnerException.Message
                equal true (obj.ReferenceEquals(errors[count - 1], ex.InnerException))
            | None -> failwith "Expected cancellation callback errors"
            equal count calls
            equal true cts.IsCancellationRequested
            cts.Cancel()
            equal count calls

    testCase "Async while binding works correctly" <| fun () ->
        let mutable result = 0
        async {
            while result < 10 do
                result <- result + 1
        } |> Async.StartImmediate
        equal result 10

    testCase "Async for binding works correctly" <| fun () ->
        let inputs = [|1; 2; 3|]
        let result = ref 0
        async {
            for inp in inputs do
                result := !result + inp
        } |> Async.StartImmediate
        equal !result 6

    testCase "Async exceptions are handled correctly" <| fun () ->
        let result = ref 0
        let f shouldThrow =
            async {
                try
                    if shouldThrow then failwith "boom!"
                    else result := 12
                with _ -> result := 10
            } |> Async.StartImmediate
            !result
        f true + f false |> equal 22

    testCase "Non captured exception in async is propagated when using Async.StartImmediate" <| fun () ->
        throwsAnyError (fun _ ->
            async {
                failwith "boom!"
            } |> Async.StartImmediate
        )

    #if FABLE_COMPILER
    // Behaviour of Async.Start, is the same as Async.StartImmediate in JS
    // We disable this test for .NET, because it seems like we can't capture the exception
    // This should be fine, because Fable generate a warning about the Async.Start
    // behaviour being the same as Async.StartImmediate
    testCase "Non captured exception in async is propagated when using Async.Start" <| fun () ->
        throwsAnyError (fun _ ->
            async {
                failwith "boom!"
            } |> Async.Start
        )
    #endif

    testCase "Simple async is executed correctly" <| fun () ->
        let result = ref false
        let x = async { return 99 }
        async {
            let! x = x
            let y = 99
            result := x = y
        }
        //TODO: RunSynchronously would make more sense here but in JS I think this will be ok.
        |> Async.StartImmediate
        equal !result true

    testCase "async use statements should dispose of resources when they go out of scope" <| fun () ->
        let isDisposed = ref false
        let step1ok = ref false
        let step2ok = ref false
        let resource = async {
            return new DisposableAction(fun () -> isDisposed := true)
        }
        async {
            use! r = resource
            step1ok := not !isDisposed
        }
        //TODO: RunSynchronously would make more sense here but in JS I think this will be ok.
        |> Async.StartImmediate
        step2ok := !isDisposed
        (!step1ok && !step2ok) |> equal true

    testCase "Try ... with ... expressions inside async expressions work the same" <| fun () ->
        let result = ref ""
        let throw() : unit =
            raise(exn "Boo!")
        let append(x) =
            result := !result + x
        let innerAsync() =
            async {
                append "b"
                try append "c"
                    throw()
                    append "1"
                with _ -> append "d"
                append "e"
            }
        async {
            append "a"
            try do! innerAsync()
            with _ -> append "2"
            append "f"
        } |> Async.StartImmediate
        equal !result "abcdef"

    testCaseAsync "Async cancellation works" <| fun () ->
        async {
            let res1, res2, res3 = ref false, ref false, ref false
            let tcs1 = new System.Threading.CancellationTokenSource(50)
            let tcs2 = new System.Threading.CancellationTokenSource()
            let tcs3 = new System.Threading.CancellationTokenSource()
            sleepAndAssign tcs1.Token res1
            sleepAndAssign tcs2.Token res2
            sleepAndAssign tcs3.Token res3
            tcs2.Cancel()
            tcs3.CancelAfter(1000)
            do! Async.Sleep 500
            equal false !res1
            equal false !res2
            equal true !res3
        }

    testCaseAsync "CancellationTokenSource.Register works" <| fun () ->
        async {
            let mutable x = 0
            let res1 = ref false
            let tcs1 = new System.Threading.CancellationTokenSource(50)
            let foo = tcs1.Token.Register(fun () ->
                x <- x + 1)
            sleepAndAssign tcs1.Token res1
            do! Async.Sleep 500
            equal false !res1
            equal 1 x
        }

    testCaseAsync "CancellationToken can be disposed" <| fun () -> // See #2879
        async {
            let res1 = ref false
            do
                use tcs1 = new System.Threading.CancellationTokenSource()
                sleepAndAssign tcs1.Token res1
            do! Async.Sleep 300
            // Disposing the CancellationToken doesn't cancel the operation
            equal true !res1
        }

    testCase "Async.StartWithContinuations works" <| fun () ->
        let res1, res2, res3 = ref "", ref "", ref ""
        Async.StartWithContinuations(successWork, (fun x -> res1 := x), ignore, ignore)
        Async.StartWithContinuations(errorWork, ignore, (fun x -> res2 := x.Message), ignore)
        Async.StartWithContinuations(cancelWork, ignore, ignore, (fun x -> res3 := x.Message))
        equal "success" !res1
        equal "error" !res2
        equal "cancelled" !res3

    testCase "Async.Catch works" <| fun () ->
        let assign res = function
            | Choice1Of2 msg -> res := msg
            | Choice2Of2 (ex: Exception) -> res := "ERROR: " + ex.Message
        let res1 = ref ""
        let res2 = ref ""
        async {
            let! x1 = successWork |> Async.Catch
            assign res1 x1
            let! x2 = errorWork |> Async.Catch
            assign res2 x2
        } |> Async.StartImmediate
        equal "success" !res1
        equal "ERROR: error" !res2

    testCase "Async.Ignore works" <| fun () ->
        let res = ref false
        async {
            do! successWork |> Async.Ignore
            res := true
        } |> Async.StartImmediate
        equal true !res

    // testCaseAsync "Async.OnCancel works" <| fun () ->
    //     async {
    //         let res = ref false
    //         let tcs = new System.Threading.CancellationTokenSource()
    //         Async.StartImmediate(async {
    //             let! _ = Async.OnCancel(fun () -> res := true)
    //             do! Async.Sleep 100
    //         }, tcs.Token)
    //         tcs.CancelAfter 50
    //         do! Async.Sleep 75
    //         equal true !res
    //     }

    testCaseAsync "Async.Parallel works" <| fun () ->
        async {
            let makeWork i =
                async {
                    do! Async.Sleep 200
                    return i
                }
            let res: int[] ref = ref [||]
            let works = [makeWork 1; makeWork 2; makeWork 3]
            async {
                let! x = Async.Parallel works
                res := x
            } |> Async.StartImmediate
            do! Async.Sleep 500
            !res |> Array.sum |> equal 6
        }

    testCaseAsync "Async.Parallel is lazy" <| fun () ->
        async {
            let mutable x = 0

            let add i =
#if FABLE_COMPILER
                x <- x + i
#else
                System.Threading.Interlocked.Add(&x, i) |> ignore<int>
#endif

            let a = Async.Parallel [
                async { add 1 }
                async { add 2 }
            ]

            do! Async.Sleep 100

            equal 0 x

            let! _ = a

            equal 3 x
        }

    testCaseAsync "Async.Sequential works" <| fun () ->
        async {
            let mutable _aggregate = 0

            let makeWork i =
                async {
                    // check that the individual work items run sequentially and not interleaved
                    _aggregate <- _aggregate + i
                    let copyOfI = _aggregate
                    do! Async.Sleep 100
                    equal copyOfI _aggregate
                    do! Async.Sleep 100
                    equal copyOfI _aggregate
                    return i
                }
            let works = [ for i in 1 .. 5 -> makeWork i ]
            let now = DateTimeOffset.Now
            let! result = Async.Sequential works
            let ``then`` = DateTimeOffset.Now
            let d = ``then`` - now
            // For some reason this sometimes fail in CI if d is exactly 1s so give it some breadth
            if d < TimeSpan.FromSeconds 0.9 then
                failwithf "expected sequential operations to take longer than 1 second, but took %0.00f" d.TotalSeconds
            result |> equal [| 1 .. 5 |]
            result |> Seq.sum |> equal _aggregate
        }

    testCaseAsync "Async.Sequential is lazy" <| fun () ->
        async {
            let mutable x = 0

            let a = Async.Sequential [
                async { x <- x + 1 }
                async { x <- x + 2 }
            ]

            do! Async.Sleep 100

            equal 0 x

            let! _ = a

            equal 3 x
        }

    #if FABLE_COMPILER
    testCaseAsync "Interaction between Async and Promise works" <| fun () ->
        async {
            let res = ref false
            async { res := true }
            |> Async.StartAsPromise
            |> Async.AwaitPromise
            |> Async.StartImmediate
            equal true !res
        }

    testCaseAsync "Promises can be cancelled" <| fun () ->
        async {
            let res = ref 0
            let tcs = new System.Threading.CancellationTokenSource(50)
            let work =
                let work = async {
                    do! Async.Sleep 75
                    res := -1
                }
                Async.StartAsPromise(work, tcs.Token) |> Async.AwaitPromise
            // behavior change: a cancelled task is now triggering the exception continuation instead of
            // the cancellation continuation, see: https://github.com/Microsoft/visualfsharp/issues/1416
            // also, System.OperationCanceledException will be changed to TaskCanceledException (not yet)
            Async.StartWithContinuations(work, ignore, ignore, (fun _ -> res := 1))
            do! Async.Sleep 100
            equal 1 !res
        }
    #endif

    testCaseAsync "MailboxProcessor.post works" <| fun () ->
        async {
            let mutable res = None
            let agent = new MailboxProcessor<int>(fun inbox ->
                let rec messageLoop() = async {
                    let! msg = inbox.Receive()
                    match msg with
                    | 3 -> () // Finish loop
                    | _ ->
                        res <- Some msg
                        return! messageLoop()
                }
                messageLoop()
            )
            agent.Post(1)
            equal None res // Mailbox hasn't started yet
            agent.Start()
            agent.Post(2)
            // Not necessary in the JS implementation, but in .NET
            // MailboxProcessor works in the background so we must wait a bit
            do! Async.Sleep 200
            equal (Some 2) res
            agent.Post(3)
            equal (Some 2) res  // Mailbox has finished
        }

    testCaseAsync "MailboxProcessor.postAndAsyncReply works" <| fun () ->
        async {
            let formatString = "Msg: {0} - {1}"
            let agent = MailboxProcessor<Message>.Start(fun inbox ->
                let rec loop n = async {
                    let! (message, replyChannel) = inbox.Receive()
                    do! Async.Sleep(100) // Delay a bit
                    replyChannel.Reply(String.Format(formatString, n, message))
                    if message <> "Bye" then do! loop (n + 1)
                }
                loop 0)
            let! resp = agent.PostAndAsyncReply(fun replyChannel -> "Hi", replyChannel)
            equal "Msg: 0 - Hi" resp
            let! resp = agent.PostAndAsyncReply(fun replyChannel -> "Bye", replyChannel)
            equal "Msg: 1 - Bye" resp
        }

    testCaseAsync "MailboxProcessor.postAndAsyncReply works with falsy values" <| fun () -> // See #1588
        async {
            let agent = MailboxProcessor.Start(fun inbox ->
                let rec loop () =
                    async {
                        let! msg = inbox.Receive()
                        match msg with
                        | GetZero replyChannel ->
                            // printfn "Replying 0"
                            replyChannel.Reply 0
                            return! loop ()
                        | GetOne replyChannel ->
                            // printfn "Replying 1"
                            replyChannel.Reply 1
                            return! loop ()
                    }
                loop () )

            let! resp = agent.PostAndAsyncReply(GetOne)
            equal 1 resp
            let! resp = agent.PostAndAsyncReply(GetZero)
            equal 0 resp
        }

    testCaseAsync "MailboxProcessor delivers falsy messages in order" <| fun () -> // See #1588
        async {
            let mutable collected = []
            let agent = MailboxProcessor<int>.Start(fun inbox ->
                let rec loop () = async {
                    let! msg = inbox.Receive()
                    if msg = -1 then ()
                    else
                        collected <- msg :: collected
                        return! loop ()
                }
                loop ())
            for msg in [1; 0; 2; 0; 3] do
                agent.Post(msg)
            agent.Post(-1)
            do! Async.Sleep 200
            equal [1; 0; 2; 0; 3] (List.rev collected)
        }

    testCaseAsync "MailboxProcessor.postAndAsyncReply completes with unit reply" <| fun () ->
        async {
            let mutable ran = false
            let agent = MailboxProcessor<AsyncReplyChannel<unit>>.Start(fun inbox ->
                let rec loop () = async {
                    let! chan = inbox.Receive()
                    chan.Reply()
                    return! loop ()
                }
                loop ())
            do! agent.PostAndAsyncReply(fun ch -> ch)
            ran <- true
            equal true ran
        }

    testCaseAsync "MailboxProcessor second Reply is ignored" <| fun () ->
        async {
            let log = ResizeArray<string>()
            let completed = new MailboxProcessor<unit>(fun _ -> async { return () })
            let agent = MailboxProcessor<AsyncReplyChannel<int>>.Start(fun inbox -> async {
                let! channel = inbox.Receive()
                channel.Reply 1
                channel.Reply 2
                log.Add "body done"
                completed.Post() })
            let! reply = agent.PostAndAsyncReply(id)
            equal 1 reply
            do! completed.Receive()
            equal [|"body done"|] (log.ToArray())
        }

    testCaseAsync "MailboxProcessor reply computation can be awaited twice" <| fun () ->
        async {
            let agent = MailboxProcessor<AsyncReplyChannel<int>>.Start(fun inbox -> async {
                let! channel = inbox.Receive()
                channel.Reply 42 })
            let work = agent.PostAndAsyncReply(id)
            let! first = work
            let! second = work
            equal 42 first
            equal 42 second
        }

    testCase "Async try .. with returns correctly from 'with' branch" <| fun () ->
        let work = async {
            try
              failwith "testing"
              return -1
            with e ->
              return 42 }
        let mutable result = 0
        Async.StartWithContinuations(work, (fun r -> result <- r), ignore, ignore)
        equal result 42


    testCaseAsync "Deep recursion with async doesn't cause stack overflow" <| fun () ->
        async {
            let result = ref false
            let rec trampolineTest res i = async {
                if i > 100000
                then res := true
                else return! trampolineTest res (i+1)
            }
            do! trampolineTest result 0
            equal !result true
        }

    testCaseAsync "Nested failure propagates in async expressions" <| fun () ->
        async {
            let data = ref ""
            let f1 x =
                async {
                    try
                        failwith "1"
                        return x
                    with
                    | e -> return! failwith ("2 " + e.Message)
                }
            let f2 x =
                async {
                    try
                        return! f1 x
                    with
                    | e -> return! failwith ("3 " + e.Message)
                }
            let f() =
                async {
                    try
                        let! y = f2 4
                        return ()
                    with
                    | e -> data := e.Message
                }
                |> Async.StartImmediate
            f()
            do! Async.Sleep 100
            equal "3 2 1" !data
        }

    testCaseAsync "Try .. finally expressions inside async expressions work" <| fun () ->
        async {
            let data = ref ""
            async {
                try data := !data + "1 "
                finally data := !data + "2 "
            } |> Async.StartImmediate
            async {
                try
                    try failwith "boom!"
                    finally data := !data + "3"
                with _ -> ()
            } |> Async.StartImmediate
            do! Async.Sleep 100
            equal "1 2 3" !data
        }

    testCaseAsync "Final statement inside async expressions can throw" <| fun () ->
        async {
            let data = ref ""
            let f() = async {
                try data := !data + "1 "
                finally failwith "boom!"
            }
            async {
                try
                    do! f()
                    return ()
                with
                | e -> data := !data + e.Message
            }
            |> Async.StartImmediate
            do! Async.Sleep 100
            equal "1 boom!" !data
        }

    testCaseAsync "Async.Bind propagates exceptions" <| fun () -> // See #724
        async {
            let task1 name = async {
                // printfn "testing with %s" name
                if name = "fail" then
                    failwith "Invalid access credentials"
                return "Ok"
            }

            let task2 name = async {
                // printfn "testing with %s" name
                do! Async.Sleep 100 //difference between task1 and task2
                if name = "fail" then
                    failwith "Invalid access credentials"
                return "Ok"
            }

            let doWork name task =
                let catch comp = async {
                    let! res = Async.Catch comp
                    return
                        match res with
                        | Choice1Of2 str -> str
                        | Choice2Of2 ex -> ex.Message
                }
                // printfn "doing work - %s" name
                async {
                    let! a = task "work" |> catch
                    // printfn "work - %A" a
                    let! b = task "fail" |> catch
                    // printfn "fail - %A" b
                    return a, b
                }

            let! res1 = doWork "task1" task1
            let! res2 = doWork "task2" task2
            equal ("Ok", "Invalid access credentials") res1
            equal ("Ok", "Invalid access credentials") res2
        }

    testCaseAsync "Async.StartChild works" <| fun () ->
      async {
        let mutable x = ""
        let taskA = async {
            do! Async.Sleep 500
            x <- x + "D"
            return "E"
        }
        let taskB = async {
            do! Async.Sleep 100
            x <- x + "C"
            return "F"
        }
        let! result1Async = taskA |> Async.StartChild // start first request but do not wait
        let! result2Async = taskB |> Async.StartChild // start second request in parallel
        x <- x + "AB"
        let! result1 = result1Async
        let! result2 = result2Async
        x <- x + result1 + result2
        equal x "ABCDEF"
      }

    testCaseAsync "Async.StartChild applies timeout" <| fun () ->
        async {
            let mutable x = ""

            let task = async {
                x <- x + "A"
                do! Async.Sleep 1_000
                x <- x + "X" // Never hit
            }

            try
                let! childTask = Async.StartChild (task, 200)

                do! childTask
            with
                | :? TimeoutException ->
                    x <- x + "B"

            x <- x + "C"

            equal x "ABC"
        }

    testCaseAsync "Async.StartChild with timeout completes when computation finishes before timeout" <| fun () -> // See #4481
        async {
            let fast = async { do! Async.Sleep 10 }
            try
                let! child = Async.StartChild(fast, 1_000)
                do! child
                equal true true // should reach here
            with
                | :? TimeoutException ->
                    failwith "should not time out"
        }

    testCaseAsync "Unit arguments are erased" <| fun () -> // See #1832
        let mutable token = 0
        async {
            let! res =
                async.Return 5
                |> asyncMap (fun x -> token <- x)
            equal 5 token
            res
        }

    testCaseAsync "Can use custom exceptions in async workflows #2396" <| fun () ->
        let workflow(): Async<unit> = async {
            return MyException(7) |> raise
        }
        let parentWorkflow() =
            async {
                try
                    do! workflow()
                    return 100
                with
                | :? MyException as ex -> return ex.Value
            }
        async {
            let! res = parentWorkflow()
            equal 7 res
        }

    testCaseAsync "Async.AwaitEvent fires continuation when event is triggered" <| fun () ->
        let ev = Event<int>()
        Async.StartImmediate(async {
            let! v = Async.AwaitEvent ev.Publish
            equal 42 v
        })
        ev.Trigger(42)
        async.Return()

    testCaseAsync "Async.AwaitEvent with cancelAction invokes it on cancellation" <| fun () ->
        let ev = Event<int>()
        let cts = new System.Threading.CancellationTokenSource()
        let mutable cancelCalled = false
        Async.StartImmediate(async {
            let! _ = Async.AwaitEvent(ev.Publish, fun () -> cancelCalled <- true)
            ()
        }, cts.Token)
        cts.Cancel()
        async { equal true cancelCalled }

#if FABLE_COMPILER
    testCase "Async.AwaitEvent cancelled before its body runs does not subscribe" <| fun () ->
        let ev = Event<int>()
        let cts = new System.Threading.CancellationTokenSource()
        let calls = ResizeArray<string>()
        let resume =
            Deferred.startDeferred(Async.AwaitEvent ev.Publish, cts.Token,
                (fun v -> calls.Add $"success {v}"), (fun _ -> calls.Add "error"), (fun _ -> calls.Add "cancel"))
        cts.Cancel()
        resume ()
        ev.Trigger(42)
        equal [ "cancel" ] (List.ofSeq calls)
        equal 0 (Deferred.handlerCount ev)
#endif

    testCase "Async try .. with does not run 'with' branch when body succeeds" <| fun () ->
        let work = async {
            try
              return 1
            with _ ->
              return 99 }
        let mutable result = 0
        Async.StartWithContinuations(work, (fun r -> result <- r), ignore, ignore)
        equal result 1

    testCase "Exception thrown by the continuation is not redirected to the 'with' branch" <| fun () ->
        let work = async {
            try
              return 1
            with _ ->
              return 99 }
        let mutable calls = 0
        throwsAnyError (fun () ->
            Async.StartWithContinuations(
                work,
                (fun _ -> calls <- calls + 1; failwith "boom from continuation"),
                ignore,
                ignore))
        equal 1 calls

    testCaseAsync "Async.StartChild cancels the child when the parent's token is cancelled" <| fun () ->
        async {
            let mutable finallyRan = false
            let mutable completed = false
            let cts = new System.Threading.CancellationTokenSource()
            let child = async {
                try
                    do! Async.Sleep 500
                    completed <- true
                finally
                    finallyRan <- true
            }
            let parent = async {
                let! childResult = Async.StartChild child
                do! childResult
            }
            Async.StartImmediate(parent, cts.Token)
            do! Async.Sleep 200
            cts.Cancel()
            do! Async.Sleep 600
            equal true finallyRan
            equal false completed
        }

    testCaseAsync "Async.Parallel children are cancelled when the parent's token is cancelled" <| fun () ->
        async {
            let mutable completed = 0
            let cts = new System.Threading.CancellationTokenSource()
            let mkChild () = async {
                do! Async.Sleep 500
                completed <- completed + 1
            }
            let work = async {
                let! _ = Async.Parallel [ mkChild(); mkChild(); mkChild() ]
                return ()
            }
            Async.StartImmediate(work, cts.Token)
            do! Async.Sleep 200
            cts.Cancel()
            do! Async.Sleep 600
            equal 0 completed
        }

    testCaseAsync "Async.Sequential children are cancelled when the parent's token is cancelled" <| fun () ->
        async {
            let mutable completed = 0
            let cts = new System.Threading.CancellationTokenSource()
            let mkChild () = async {
                do! Async.Sleep 500
                completed <- completed + 1
            }
            let work = async {
                let! _ = Async.Sequential [ mkChild(); mkChild(); mkChild() ]
                return ()
            }
            Async.StartImmediate(work, cts.Token)
            do! Async.Sleep 200
            cts.Cancel()
            do! Async.Sleep 600
            equal 0 completed
        }

    testCaseAsync "Async.StartChild with timeout cancels the child when the timeout expires" <| fun () ->
        async {
            let mutable completed = false
            let child = async {
                do! Async.Sleep 300
                completed <- true
            }
            let mutable timedOut = false
            try
                let! childResult = Async.StartChild(child, 100)
                do! childResult
            with
            | :? TimeoutException -> timedOut <- true
            do! Async.Sleep 400
            equal true timedOut
            equal false completed
        }

    testCaseAsync "Async.StartChild with timeout leaves the parent running when the timeout expires" <| fun () ->
        async {
            let child = async { do! Async.Sleep 300 }
            try
                let! childResult = Async.StartChild(child, 100)
                do! childResult
            with
            | :? TimeoutException -> ()
            let! token = Async.CancellationToken
            equal false token.IsCancellationRequested
        }

    testCaseAsync "Async.StartChild timeout only runs while the result is awaited" <| fun () ->
        async {
            let child = async {
                do! Async.Sleep 150
                return 42
            }
            let! childResult = Async.StartChild(child, 100)
            do! Async.Sleep 250
            let! result = childResult
            equal 42 result
        }

    testCaseAsync "Async.StartChild with a timeout of -1 never times out" <| fun () ->
        async {
            let child = async {
                do! Async.Sleep 50
                return 42
            }
            let! childResult = Async.StartChild(child, -1)
            let! result = childResult
            equal 42 result
        }

    testCaseAsync "Async.StartChild with a timeout of 0 returns an available result" <| fun () ->
        async {
            let child = async {
                do! Async.Sleep 50
                return 42
            }
            let! childResult = Async.StartChild(child, 0)
            do! Async.Sleep 200
            let! result = childResult
            equal 42 result
        }

    testCaseAsync "Async.StartChild with a timeout of 0 raises when the result isn't available and leaves the child running" <| fun () ->
        async {
            let mutable completed = false
            let child = async {
                do! Async.Sleep 300
                completed <- true
            }
            let mutable timedOut = false
            try
                let! childResult = Async.StartChild(child, 0)
                do! childResult
            with
            | :? TimeoutException -> timedOut <- true
            do! Async.Sleep 400
            equal true timedOut
            equal true completed
        }

    testCaseAsync "Async.StartChild with timeout raises the child's cancellation callback failure" <| fun () ->
        async {
            let child = async {
                let! token = Async.CancellationToken
                token.Register(fun () -> failwith "cleanup") |> ignore
                do! Async.Sleep 300
            }
            let mutable errors = -1
            try
                let! childResult = Async.StartChild(child, 100)
                do! childResult
            with error ->
#if FABLE_COMPILER
                errors <- Runtime.cancellationErrorCount error
#else
                errors <- (error :?> AggregateException).InnerExceptions.Count
#endif
            equal 1 errors
        }

    testCaseAsync "Async.OnCancel ignores a failing interruption" <| fun () ->
        async {
            let mutable interrupted = false
            let mutable cancelled = false
            let cts = new CancellationTokenSource()
            Async.StartWithContinuations(
                async {
                    use! _ = Async.OnCancel(fun () ->
                        interrupted <- true
                        failwith "interruption")
                    do! Async.Sleep 300
                },
                ignore,
                ignore,
                (fun _ -> cancelled <- true),
                cts.Token)
            do! Async.Sleep 50
            cts.Cancel()
            do! Async.Sleep 50
            equal true interrupted
            equal true cancelled
        }
    testCaseAsync "Async.Parallel respects maxDegreeOfParallelism" <| fun () ->
        async {
            let lockObj = obj ()
            let mutable running = 0
            let mutable peak = 0
            let work i = async {
                lock lockObj (fun () ->
                    running <- running + 1
                    peak <- max peak running)
                do! Async.Sleep 50
                lock lockObj (fun () -> running <- running - 1)
                return i
            }
            let! results = Async.Parallel([ for i in 1..10 -> work i ], maxDegreeOfParallelism = 3)
            equal [| 1..10 |] results
            equal 3 peak
        }

    testCaseAsync "Async.Parallel cancels the other computations when one fails" <| fun () ->
        async {
            let mutable completed = 0
            let work i = async {
                if i = 2 then
                    do! Async.Sleep 20
                    failwith "boom"
                do! Async.Sleep 300
#if FABLE_COMPILER
                completed <- completed + 1
#else
                System.Threading.Interlocked.Increment(&completed) |> ignore<int>
#endif
            }
            let mutable error = ""
            try
                let! _ = Async.Parallel [ for i in 0..4 -> work i ]
                ()
            with ex -> error <- ex.Message
            do! Async.Sleep 400
            equal "boom" error
            equal 0 completed
        }

    testCaseAsync "Async.Parallel with an empty sequence returns an empty array" <| fun () ->
        async {
            let! results = Async.Parallel(([]: Async<int> list), maxDegreeOfParallelism = 2)
            equal 0 results.Length
        }

    testCase "Async.Parallel rejects a maxDegreeOfParallelism below 1" <| fun () ->
        let mutable raised = false
        try
            Async.Parallel([ async { return 1 } ], maxDegreeOfParallelism = 0) |> ignore
        with :? ArgumentException -> raised <- true
        equal true raised

    testCaseAsync "Async.Choice returns the first Some and cancels the others" <| fun () ->
        async {
            let mutable completed = false
            let fast = async {
                do! Async.Sleep 20
                return Some 1
            }
            let slow = async {
                do! Async.Sleep 300
                completed <- true
                return Some 2
            }
            let none = async { return None }
            let! result = Async.Choice [ slow; none; fast ]
            do! Async.Sleep 400
            equal (Some 1) result
            equal false completed
        }

    testCaseAsync "Async.Choice returns None when every computation does" <| fun () ->
        async {
            let none = async {
                do! Async.Sleep 10
                return (None: int option)
            }
            let! result = Async.Choice [ none; none ]
            equal None result
            let! empty = Async.Choice ([]: Async<int option> list)
            equal None empty
        }

    testCaseAsync "Async.Choice raises when a computation fails" <| fun () ->
        async {
            let fails = async {
                do! Async.Sleep 10
                return failwith "boom"
            }
            let slow = async {
                do! Async.Sleep 300
                return Some 1
            }
            let mutable error = ""
            try
                let! _ = Async.Choice [ slow; fails ]
                ()
            with ex -> error <- ex.Message
            equal "boom" error
        }

    testCaseAsync "Async.OnCancel runs the interruption when cancelled" <| fun () ->
        async {
            let mutable interrupted = false
            let cts = new CancellationTokenSource()
            Async.StartImmediate(async {
                use! _ = Async.OnCancel(fun () -> interrupted <- true)
                do! Async.Sleep 300
            }, cts.Token)
            do! Async.Sleep 50
            cts.Cancel()
            do! Async.Sleep 50
            equal true interrupted
        }

    testCaseAsync "Async.OnCancel doesn't run the interruption once disposed" <| fun () ->
        async {
            let mutable interrupted = false
            let cts = new CancellationTokenSource()
            Async.StartImmediate(async {
                let! registration = Async.OnCancel(fun () -> interrupted <- true)
                registration.Dispose()
                do! Async.Sleep 300
            }, cts.Token)
            do! Async.Sleep 50
            cts.Cancel()
            do! Async.Sleep 50
            equal false interrupted
        }

    testCaseAsync "Async.OnCancel doesn't run the interruption when disposed before the cancellation and again by use!" <| fun () ->
        async {
            let mutable interrupted = false
            let cts = new CancellationTokenSource()
            Async.StartImmediate(async {
                use! registration = Async.OnCancel(fun () -> interrupted <- true)
                registration.Dispose()
                do! Async.Sleep 300
            }, cts.Token)
            do! Async.Sleep 50
            cts.Cancel()
            do! Async.Sleep 50
            equal false interrupted
        }

    testCaseAsync "Async.StartAsTask works" <| fun () ->
        async {
            let t = Async.StartAsTask(async {
                do! Async.Sleep 10
                return 42
            })
            let! result = Async.AwaitTask t
            equal 42 result
        }

    testCaseAsync "Async.StartAsTask passes the cancellation token" <| fun () ->
        async {
            let mutable completed = false
            let cts = new CancellationTokenSource()
            let t = Async.StartAsTask(async {
                do! Async.Sleep 200
                completed <- true
            }, cancellationToken = cts.Token)
            cts.Cancel()
            let mutable outcome = "pending"
            // AwaitTask reports a cancelled task as an exception on .NET and as a cancellation in JS
            Async.StartWithContinuations(
                Async.AwaitTask t,
                (fun () -> outcome <- "completed"),
                (fun _ -> outcome <- "cancelled"),
                (fun _ -> outcome <- "cancelled"))
            do! Async.Sleep 300
            equal "cancelled" outcome
            equal false completed
        }

    testCaseAsync "Async.StartImmediateAsTask works" <| fun () ->
        async {
            let t = Async.StartImmediateAsTask(async { return 42 })
            let! result = Async.AwaitTask t
            equal 42 result
        }
  ]
