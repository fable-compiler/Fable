module Fable.Tests.AsyncTests

open System
open Fable.Tests.Util
open Util.Testing
open System.Threading

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

let private raiseCaughtCancellation (error: exn) : unit = raise error

[<Fact>]
let ``test TryCancelled preserves success and ordinary errors`` () =
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

[<Fact>]
let ``test TryCancelled preserves cancellation identity and supports reuse`` () =
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

[<Fact>]
let ``test TryCancelled installs nested handlers before cancellation`` () =
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

[<Fact>]
let ``test TryCancelled handles deferred cancellation`` () =
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

[<Fact>]
let ``test TryCancelled preserves finalizer order when handlers throw`` () =
    let calls = ResizeArray<string>()
    let body: Async<int> = Async.FromContinuations(fun (_, _, cancel) -> cancel (OperationCanceledException "original"))
    let finalized = async.TryFinally(body, fun () -> calls.Add "finally")
    let inner = Async.TryCancelled(finalized, fun _ -> calls.Add "inner"; failwith "inner defect")
    let outer = Async.TryCancelled(inner, fun _ -> calls.Add "outer"; failwith "outer defect")
    equal [|"cancel: original"|] ((observeTryCancelled CancellationToken.None outer).ToArray())
    equal [|"finally"; "inner"; "outer"|] (calls.ToArray())

[<Fact>]
let ``test TryCancelled propagates downstream errors once`` () =
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

[<Fact>]
let ``test TryCancelled preserves bounded trampoline execution`` () =
    async {
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
    |> Async.RunSynchronously

[<Fact>]
let ``test RunSynchronously optional timeout and cancellation token`` () =
    let work = async { return 42 }
    Async.RunSynchronously work |> equal 42
    Async.RunSynchronously(work, timeout = -1) |> equal 42
    Async.RunSynchronously(work, timeout = 10000) |> equal 42
    let invalidTimeout =
        try
            Async.RunSynchronously(work, timeout = -2) |> ignore
            false
        with :? ArgumentException -> true
    equal true invalidTimeout
    use cts = new CancellationTokenSource()
    Async.RunSynchronously(work, cancellationToken = cts.Token) |> equal 42
    Async.RunSynchronously(work, timeout = 10000, cancellationToken = cts.Token) |> equal 42
    cts.Cancel()
    let mutable entered = false
    let cancelled =
        try
            Async.RunSynchronously(async { entered <- true }, cancellationToken = cts.Token)
            false
        with :? OperationCanceledException -> true
    equal true cancelled
    equal false entered

[<Fact>]
let ``test cancellation identity survives ordinary catch and raise`` () =
    let token = CancellationToken(true)
    let mutable caught = false
    let cancelled =
        try
            try token.ThrowIfCancellationRequested()
            with error ->
                caught <- true
                raiseCaughtCancellation error
            false
        with :? OperationCanceledException -> true
    equal true caught
    equal true cancelled
    let ordinary =
        try raise (Exception("operation_cancelled"))
        with
        | :? OperationCanceledException -> false
        | error -> error.Message = "operation_cancelled"
    equal true ordinary

[<Fact>]
let ``test Async TryWith catches explicitly thrown cancellation exceptions`` () =
    async {
        try
            CancellationToken(true).ThrowIfCancellationRequested()
            return false
        with :? OperationCanceledException -> return true
    }
    |> Async.RunSynchronously
    |> equal true

[<Fact>]
let ``test a real Async defect racing cancellation remains a failure`` () =
    use cts = new CancellationTokenSource()
    let work = async {
        cts.Cancel()
        return failwith "real defect"
    }
    let outcome =
        try
            Async.RunSynchronously(work, cancellationToken = cts.Token) |> ignore
            "success"
        with
        | :? OperationCanceledException -> "cancelled"
        | error -> error.Message
    equal "real defect" outcome

[<Fact>]
let ``test Async finalizers run once and retain throwing compensation`` () =
    for bodyFails in [ false; true ] do
        let mutable count = 0
        let work = async {
            try
                if bodyFails then failwith "body defect"
                return 42
            finally
                count <- count + 1
                failwith "cleanup defect"
        }
        let failure =
            try Async.RunSynchronously work |> ignore; "success"
            with error -> error.Message
        equal "cleanup defect" failure
        equal 1 count

[<Fact>]
let ``test StartImmediate propagates errors after a finalizer containing a bind`` () =
    let mutable count = 0
    let work = async {
        try
            let! value = async { return 42 }
            equal 42 value
        finally
            count <- count + 1
        failwith "downstream defect"
    }
    let failure =
        try Async.StartImmediate work; "success"
        with error -> error.Message
    equal "downstream defect" failure
    equal 1 count

[<Fact>]
let ``test nested Async finalizers retain outer failure and run once`` () =
    let mutable inner = 0
    let mutable outer = 0
    let work = async {
        try
            try return 42
            finally
                inner <- inner + 1
                failwith "inner cleanup"
        finally
            outer <- outer + 1
            failwith "outer cleanup"
    }
    let failure =
        try Async.RunSynchronously work |> ignore; "success"
        with error -> error.Message
    equal "outer cleanup" failure
    equal 1 inner
    equal 1 outer

[<Fact>]
let ``test Async Using disposes once and retains disposal defects`` () =
    for bodyFails in [ false; true ] do
        for disposalFails in [ false; true ] do
            let mutable count = 0
            let resource =
                { new IDisposable with
                    member _.Dispose() =
                        count <- count + 1
                        if disposalFails then failwith "disposal defect" }
            let work = async {
                use owned = resource
                if bodyFails then failwith "body defect"
                return 42
            }
            let outcome =
                try Async.RunSynchronously work |> string
                with error -> error.Message
            equal (if disposalFails then "disposal defect" elif bodyFails then "body defect" else "42") outcome
            equal 1 count
    async {
        use resource = Unchecked.defaultof<IDisposable>
        return 42
    }
    |> Async.RunSynchronously
    |> equal 42

let private cancelledComputation : Async<unit> =
    Async.FromContinuations(fun (_, _, cancel) ->
        cancel (OperationCanceledException("original cancellation")))

let private cancellationOutcome (work: Async<unit>) =
    try
        Async.RunSynchronously work
        "success"
    with
    | :? OperationCanceledException as error -> error.Message
    | error -> "failure: " + error.Message

[<Fact>]
let ``test Async cancellation remains terminal when compensation throws`` () =
    for throws in [ false; true ] do
        let mutable count = 0
        let work = async {
            try do! cancelledComputation
            finally
                count <- count + 1
                if throws then failwith "cleanup defect"
        }
        equal "original cancellation" (cancellationOutcome work)
        equal 1 count

[<Fact>]
let ``test nested throwing Async finalizers preserve cancellation`` () =
    for outerThrows in [ false; true ] do
        let mutable inner = 0
        let mutable outer = 0
        let work = async {
            try
                try do! cancelledComputation
                finally
                    inner <- inner + 1
                    failwith "inner cleanup"
            finally
                outer <- outer + 1
                if outerThrows then failwith "outer cleanup"
        }
        equal "original cancellation" (cancellationOutcome work)
        equal 1 inner
        equal 1 outer

[<Fact>]
let ``test Async Using preserves cancellation when Dispose throws`` () =
    for throws in [ false; true ] do
        let mutable count = 0
        let resource =
            { new IDisposable with
                member _.Dispose() =
                    count <- count + 1
                    if throws then failwith "disposal defect" }
        let work = async {
            use owned = resource
            do! cancelledComputation
        }
        equal "original cancellation" (cancellationOutcome work)
        equal 1 count

type MyException(value) =
    inherit Exception()
    member _.Value: int = value

[<Fact>]
let ``test async return works`` () =
    let comp = async { return 42 }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async let bang works`` () =
    let comp = async {
        let! x = async { return 10 }
        let! y = async { return 20 }
        return x + y
    }
    Async.RunSynchronously comp |> equal 30

[<Fact>]
let ``test async do bang works`` () =
    let mutable x = 0
    let comp = async {
        do! async { x <- 42; return () }
        return x
    }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async return from works`` () =
    let inner = async { return "hello" }
    let outer = async { return! inner }
    Async.RunSynchronously outer |> equal "hello"

[<Fact>]
let ``test async try-with works`` () =
    let comp = async {
        try
            failwith "boom"
            return 0
        with _e ->
            return 42
    }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test async sleep works`` () =
    let comp = async {
        do! Async.Sleep 10
        return 1
    }
    Async.RunSynchronously comp |> equal 1

[<Fact>]
let ``test async parallel works`` () =
    let comps = [|
        async { return 1 }
        async { return 2 }
        async { return 3 }
    |]
    let results = Async.RunSynchronously (Async.Parallel comps)
    Array.sum results |> equal 6

[<Fact>]
let ``test Async.Parallel limits workers and preserves result order`` () =
#if FABLE_COMPILER
    let state: obj = Fable.Core.BeamInterop.emitErlExpr () "atomics:new(3, [])"
    let enter () : int = Fable.Core.BeamInterop.emitErlExpr state "atomics:add_get($0, 1, 1)"
    let leave () : unit = Fable.Core.BeamInterop.emitErlExpr state "atomics:sub_get($0, 1, 1)"
    let mark (slot: int) : unit = Fable.Core.BeamInterop.emitErlExpr (state, slot) "atomics:put($0, $1, 1)"
    let read (slot: int) : int = Fable.Core.BeamInterop.emitErlExpr (state, slot) "atomics:get($0, $1)"
#else
    let gate = obj ()
    let mutable running = 0
    let mutable seenTwo = 0
    let mutable exceeded = 0
    let enter () = lock gate (fun () -> running <- running + 1; running)
    let leave () = lock gate (fun () -> running <- running - 1)
    let mark slot = lock gate (fun () -> if slot = 2 then seenTwo <- 1 else exceeded <- 1)
    let read slot = if slot = 2 then seenTwo else exceeded
#endif
    let work i = async {
        let running = enter ()
        if running = 2 then mark 2
        if running > 2 then mark 3
        do! Async.Sleep 50
        leave ()
        return i
    }
    let result = Async.RunSynchronously(Async.Parallel([ for i in 1..6 -> work i ], maxDegreeOfParallelism = 2))
    result |> equal [| 1..6 |]
    read 2 |> equal 1
    read 3 |> equal 0

[<Fact>]
let ``test Async.Parallel cancels siblings after a failure`` () =
#if FABLE_COMPILER
    let completed: obj = Fable.Core.BeamInterop.emitErlExpr () "atomics:new(1, [])"
    let finish () : unit = Fable.Core.BeamInterop.emitErlExpr completed "atomics:add_get($0, 1, 1)"
    let count () : int = Fable.Core.BeamInterop.emitErlExpr completed "atomics:get($0, 1)"
#else
    let mutable completed = 0
    let finish () = System.Threading.Interlocked.Increment(&completed) |> ignore<int>
    let count () = completed
#endif
    let work i = async {
        if i = 2 then
            do! Async.Sleep 20
            failwith "parallel failure"
        do! Async.Sleep 200
        finish ()
    }
    let outcome = Async.RunSynchronously(Async.Catch(Async.Parallel [ for i in 0..4 -> work i ]))
    match outcome with
    | Choice1Of2 _ -> failwith "Expected parallel failure"
    | Choice2Of2 error -> error.Message |> equal "parallel failure"
    Async.RunSynchronously(Async.Sleep 250)
    count () |> equal 0

[<Fact>]
let ``test Async.Parallel validates degree and accepts empty input`` () =
    let mutable raised = false
    try
        Async.Parallel([ async { return 1 } ], maxDegreeOfParallelism = 0) |> ignore
    with :? ArgumentException -> raised <- true
    raised |> equal true
    Async.RunSynchronously(Async.Parallel(([]: Async<int> list), maxDegreeOfParallelism = 2))
    |> equal [||]

[<Fact>]
let ``test Async.Choice returns first Some and handles empty input`` () =
    let slow = async {
        do! Async.Sleep 200
        return Some 2
    }
    let fast = async {
        do! Async.Sleep 20
        return Some 1
    }
    let winner = Async.RunSynchronously(Async.Choice [ slow; async { return None }; fast ])
    winner |> equal (Some 1)
    Async.RunSynchronously(Async.Choice([]: Async<int option> list)) |> equal None

[<Fact>]
let ``test Async.OnCancel runs active interruption once`` () =
    use source = new CancellationTokenSource()
#if FABLE_COMPILER
    let counts: obj = Fable.Core.BeamInterop.emitErlExpr () "atomics:new(2, [])"
    let mark slot : unit = Fable.Core.BeamInterop.emitErlExpr (counts, slot) "atomics:add_get($0, $1, 1)"
    let read slot : int = Fable.Core.BeamInterop.emitErlExpr (counts, slot) "atomics:get($0, $1)"
    let parent: obj = Fable.Core.BeamInterop.emitErlExpr () "erlang:self()"
    let ready () : unit = Fable.Core.BeamInterop.emitErlExpr parent "$0 ! oncancel_ready"
    let doneSignal () : unit = Fable.Core.BeamInterop.emitErlExpr parent "$0 ! oncancel_done"
    let work = async {
        use! _ = Async.OnCancel(fun () -> mark 1)
        let! registration = Async.OnCancel(fun () -> mark 2)
        registration.Dispose()
        ready ()
        do! Async.Sleep 500
    }
    let run () =
        try Async.RunSynchronously(work, cancellationToken = source.Token)
        with :? OperationCanceledException -> ()
        doneSignal ()
    let _: obj = Fable.Core.BeamInterop.emitErlExpr run "erlang:spawn(fun() -> $0(ok) end)"
    let wasReady: bool = Fable.Core.BeamInterop.emitErlExpr () "receive oncancel_ready -> true after 5000 -> false end"
    wasReady |> equal true
    source.Cancel()
    let finished: bool = Fable.Core.BeamInterop.emitErlExpr () "receive oncancel_done -> true after 5000 -> false end"
    finished |> equal true
    read 1 |> equal 1
    read 2 |> equal 0
#else
    let mutable active = 0
    let mutable disposed = 0
    use ready = new System.Threading.ManualResetEventSlim(false)
    use interrupted = new System.Threading.ManualResetEventSlim(false)
    Async.StartImmediate(async {
        use! _ = Async.OnCancel(fun () ->
            System.Threading.Interlocked.Increment(&active) |> ignore<int>
            interrupted.Set())
        let! registration = Async.OnCancel(fun () ->
            System.Threading.Interlocked.Increment(&disposed) |> ignore<int>)
        registration.Dispose()
        ready.Set()
        do! Async.Sleep 500
    }, source.Token)
    ready.Wait 5000 |> equal true
    source.Cancel()
    interrupted.Wait 5000 |> equal true
    active |> equal 1
    disposed |> equal 0
#endif

[<Fact>]
let ``test Async.StartAsTask and StartImmediateAsTask return awaitable results`` () =
    let task1 = Async.StartAsTask(async {
        do! Async.Sleep 10
        return 41
    })
    let task2 = Async.StartImmediateAsTask(async { return 1 })
    let result = Async.RunSynchronously(async {
        let! first = Async.AwaitTask task1
        let! second = Async.AwaitTask task2
        return first + second
    })
    result |> equal 42

[<Fact>]
let ``test async sequential composition works`` () =
    let mutable count = 0
    let comp = async {
        let! a = async { count <- count + 1; return count }
        let! b = async { count <- count + 1; return count }
        return a + b
    }
    Async.RunSynchronously comp |> equal 3

[<Fact>]
let ``test async ignore works`` () =
    let comp = async {
        do! Async.Ignore (async { return 42 })
        return 1
    }
    Async.RunSynchronously comp |> equal 1

[<Fact>]
let ``test async start immediate works`` () =
    let mutable x = 0
    Async.StartImmediate (async { x <- 42 })
    equal 42 x

[<Fact>]
let ``test Async.StartChild works`` () =
    let comp = async {
        let! c = Async.StartChild(async { return 42 })
        let! r = c
        return r
    }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test Async.StartChild runs children concurrently`` () =
    // Assert that the children's execution *intervals overlap*, rather than that
    // the whole computation finished quickly.
    //
    // Measuring duration -- against either an absolute threshold or a sequential
    // control run -- is unfixably flaky here, because on a loaded runner the
    // scheduling jitter is larger than the signal. Measured on a 2-core box under
    // load, this pair of 500ms sleeps took anywhere from 504ms to 1580ms wall
    // clock, so a genuinely-concurrent run routinely outlasts a sequential one and
    // no choice of threshold separates them.
    //
    // Overlap is immune to that. Jitter delays both children together, so a
    // common-mode delay cancels out of `min(end) - max(start)` while it is exactly
    // what a duration measures. Under the same load that made the duration vary by
    // ~1080ms, the overlap stayed within 500-984ms.
    //
    // Note: unlike JS/Python, the BEAM children run in *separate processes* (see
    // fable_async:start_child), so the shared-mutable-state ordering test those
    // targets use cannot observe them. A MailboxProcessor cannot bridge the gap
    // either -- fable_mailbox holds its queue in the process dictionary, so a
    // child's Post lands in the child's own copy. Timestamps returned by value
    // from each child are the one channel that does cross the process boundary.
    let stamped n =
        async {
            let t0 = System.DateTime.UtcNow.Ticks
            do! Async.Sleep 500
            let t1 = System.DateTime.UtcNow.Ticks
            return n, t0, t1
        }
    // Both children are started before either is awaited, so their sleeps should
    // run at the same time.
    let comp = async {
        let! c1 = Async.StartChild(stamped 1)
        let! c2 = Async.StartChild(stamped 2)
        let! (n1, a0, a1) = c1
        let! (n2, b0, b1) = c2
        let overlapMs = (min a1 b1 - max a0 b0) / TimeSpan.TicksPerMillisecond
        return n1 + n2, overlapMs
    }
    let sum, overlapMs = Async.RunSynchronously comp
    equal 3 sum
    // Concurrent execution overlaps by the full ~500ms sleep. Were StartChild to
    // regress to running each child only once awaited, the second child would
    // start as the first one ended and the overlap would be 0 (verified: 0ms on
    // both .NET and BEAM). The 250ms threshold sits midway between the two.
    (overlapMs > 250L) |> equal true

[<Fact>]
let ``test Async.StartChild applies timeout`` () =
    let slow = async {
        do! Async.Sleep 1000
        return 1
    }
    let comp = async {
        let! c = Async.StartChild(slow, 50)
        try
            let! _ = c
            return "no timeout"
        with ex ->
            return ex.Message
    }
    Async.RunSynchronously comp |> equal "The operation has timed out."

[<Fact>]
let ``test Async.StartChild with timeout completes when computation finishes before timeout`` () =
    let fast = async {
        do! Async.Sleep 10
        return 99
    }
    let comp = async {
        let! c = Async.StartChild(fast, 1000)
        let! r = c
        return r
    }
    Async.RunSynchronously comp |> equal 99

[<Fact>]
let ``test Async.StartChild result can be awaited multiple times`` () =
    let comp = async {
        let! c = Async.StartChild(async { return 21 })
        let! r1 = c
        let! r2 = c
        return r1 + r2
    }
    Async.RunSynchronously comp |> equal 42

[<Fact>]
let ``test Async.StartChild result can be awaited from another process`` () =
    // On Beam, Async.Parallel runs each computation in its own process, so
    // awaiting the child from inside a parallel worker exercises cross-process
    // await: the awaiter is not the process that called StartChild.
    let comp = async {
        let! c = Async.StartChild(async {
            do! Async.Sleep 50
            return 7
        })
        let! results =
            Async.Parallel [ async {
                let! r = c
                return r
            } ]
        return Array.sum results
    }
    Async.RunSynchronously comp |> equal 7

let asyncMap f a = async {
    let! a = a
    return f a
}

let successWork: Async<string> = Async.FromContinuations(fun (onSuccess,_,_) -> onSuccess "success")
let errorWork: Async<string> = Async.FromContinuations(fun (_,onError,_) -> onError (exn "error"))

[<Fact>]
let ``test Async while binding works correctly`` () =
    let mutable result = 0
    async {
        while result < 10 do
            result <- result + 1
    } |> Async.StartImmediate
    equal result 10

[<Fact>]
let ``test Async for binding works correctly`` () =
    let inputs = [|1; 2; 3|]
    let mutable result = 0
    async {
        for inp in inputs do
            result <- result + inp
    } |> Async.StartImmediate
    equal result 6

[<Fact>]
let ``test Async exceptions are handled correctly`` () =
    let mutable result = 0
    let f shouldThrow =
        async {
            try
                if shouldThrow then failwith "boom!"
                else result <- 12
            with _ -> result <- 10
        } |> Async.StartImmediate
        result
    f true + f false |> equal 22

[<Fact>]
let ``test Simple async is executed correctly`` () =
    let mutable result = false
    let x = async { return 99 }
    async {
        let! x = x
        let y = 99
        result <- x = y
    }
    |> Async.StartImmediate
    equal result true

[<Fact>]
let ``test Try ... with ... expressions inside async expressions work the same`` () =
    let result = ref ""
    let throw() : unit =
        raise(exn "Boo!")
    let append(x) =
        result.Value <- result.Value + x
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
    equal "abcdef" result.Value

[<Fact>]
let ``test Async StartWithContinuations works`` () =
    let res1, res2 = ref "", ref ""
    Async.StartWithContinuations(successWork, (fun x -> res1.Value <- x), ignore, ignore)
    Async.StartWithContinuations(errorWork, ignore, (fun x -> res2.Value <- x.Message), ignore)
    equal "success" res1.Value
    equal "error" res2.Value

[<Fact>]
let ``test Async.Catch works`` () =
    let assign (res: Ref<string>) = function
        | Choice1Of2 msg -> res.Value <- msg
        | Choice2Of2 (ex: Exception) -> res.Value <- "ERROR: " + ex.Message
    let res1 = ref ""
    let res2 = ref ""
    async {
        let! x1 = successWork |> Async.Catch
        assign res1 x1
        let! x2 = errorWork |> Async.Catch
        assign res2 x2
    } |> Async.StartImmediate
    equal "success" res1.Value
    equal "ERROR: error" res2.Value

[<Fact>]
let ``test Deep recursion with async doesn't cause stack overflow`` () =
    async {
        let result = ref false
        let rec trampolineTest (res: bool ref) i = async {
            if i > 100000
            then res.Value <- true
            else return! trampolineTest res (i+1)
        }
        do! trampolineTest result 0
        equal result.Value true
    } |> Async.StartImmediate

[<Fact>]
let ``test Nested failure propagates in async expressions`` () =
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
                let! _y = f2 4
                return ()
            with
            | e -> data.Value <- e.Message
        }
        |> Async.StartImmediate
    f()
    equal "3 2 1" data.Value

[<Fact>]
let ``test Try .. finally expressions inside async expressions work`` () =
    let data = ref ""
    async {
        try data.Value <- data.Value + "1 "
        finally data.Value <- data.Value + "2 "
    } |> Async.StartImmediate
    async {
        try
            try failwith "boom!"
            finally data.Value <- data.Value + "3"
        with _ -> ()
    } |> Async.StartImmediate
    equal "1 2 3" data.Value

[<Fact>]
let ``test Final statement inside async expressions can throw`` () =
    let data = ref ""
    let f() = async {
        try data.Value <- data.Value + "1 "
        finally failwith "boom!"
    }
    async {
        try
            do! f()
            return ()
        with
        | e -> data.Value <- data.Value + e.Message
    }
    |> Async.StartImmediate
    equal "1 boom!" data.Value

[<Fact>]
let ``test Async.Bind propagates exceptions`` () =
    async {
        let task1 name = async {
            if name = "fail" then
                failwith "Invalid access credentials"
            return "Ok"
        }

        let task2 name = async {
            do! Async.Sleep 100
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
            async {
                let! a = task "work" |> catch
                let! b = task "fail" |> catch
                return a, b
            }

        let! res1 = doWork "task1" task1
        let! res2 = doWork "task2" task2
        equal ("Ok", "Invalid access credentials") res1
        equal ("Ok", "Invalid access credentials") res2
    } |> Async.StartImmediate

[<Fact>]
let ``test Unit arguments are erased`` () =
    let mutable token = 0
    async {
        let! _res =
            async.Return 5
            |> asyncMap (fun x -> token <- x)
        equal 5 token
    } |> Async.StartImmediate

#if FABLE_COMPILER

open Fable.Core.BeamInterop

type private CancellationProbeMessage =
    | CancellationProbeReady of worker: obj
    | CancellationProbeContinue
    | CancellationCallbackInvoked
    | CancellationCallbackStarted of worker: obj
    | CancellationDisposeReturned

// Run a thunk in a fresh BEAM process and return its result to the caller.
[<Fable.Core.Emit("(fun() -> CancellationParent = self(), CancellationFun = $0, spawn(fun() -> CancellationParent ! {cancellation_result, case erlang:fun_info(CancellationFun, arity) of {arity, 0} -> CancellationFun(); _ -> CancellationFun(ok) end} end), receive {cancellation_result, CancellationResult} -> CancellationResult after 1000 -> timeout end end)()")>]
let private runInCancellationProcess (f: unit -> 'a) : 'a = Fable.Core.Util.nativeOnly

[<Fable.Core.Emit("(fun() -> CancellationFun = $0, spawn(fun() -> case erlang:fun_info(CancellationFun, arity) of {arity, 0} -> CancellationFun(); _ -> CancellationFun(ok) end end) end)()")>]
let private startCancellationProcess (f: unit -> unit) : obj = Fable.Core.Util.nativeOnly

let private runSynchronouslyWithCancellation (workflow: Async<'a>) (token: System.Threading.CancellationToken) : 'a =
    Async.RunSynchronously(workflow, cancellationToken = token)

[<Fable.Core.Emit("async_lifecycle_tests:pending($0)")>]
let private pendingCancellation (_parent: obj) : Async<unit> = Fable.Core.Util.nativeOnly

[<Fact>]
let ``test legacy runtime cancellation remains typed through normalization and raise`` () =
    let cancelled =
        try
            try emitErlExpr () "erlang:error(operation_cancelled)"
            with error -> raiseCaughtCancellation error
            false
        with :? OperationCanceledException -> true
    equal true cancelled

[<Fact>]
let ``test pending RunSynchronously cancellation bypasses Async TryWith and disposes once`` () =
    use cts = new CancellationTokenSource()
    let parent: obj = emitErlExpr () "erlang:self()"
    let work = async {
        let! child = Async.StartChild(async {
            let mutable handled = false
            let mutable finalized = 0
            let mutable disposed = 0
            let resource =
                { new IDisposable with
                    member _.Dispose() = disposed <- disposed + 1 }
            let cancelled =
                try
                    Async.RunSynchronously(
                        async {
                            use owned = resource
                            try
                                try do! pendingCancellation parent
                                with _ -> handled <- true
                            finally finalized <- finalized + 1
                        }, cancellationToken = cts.Token)
                    false
                with :? OperationCanceledException -> true
            return cancelled, handled, finalized, disposed
        })
        let ready: bool = emitErlExpr () "receive {pending, _} -> true after 5000 -> false end"
        equal true ready
        cts.Cancel()
        return! child
    }
    Async.RunSynchronously work |> equal (true, false, 1, 1)

let private sendCancellationReady (parent: obj) =
    let worker: obj = emitErlExpr () "erlang:self()"
    emitErlExpr (parent, worker) "$0 ! {cancellation_probe_ready, $1}"

let private waitForCancellationReady () =
    match Erlang.receive<CancellationProbeMessage> 1000 with
    | Some(CancellationProbeReady worker) -> Some worker
    | _ -> None

[<Fact>]
let ``test cancellation token state crosses process boundaries`` () =
    let cts = new System.Threading.CancellationTokenSource()

    runInCancellationProcess (fun () -> cts.Token.IsCancellationRequested)
    |> equal false

[<Fact>]
let ``test running process observes cancellation from source process`` () =
    let cts = new System.Threading.CancellationTokenSource()
    let parent: obj = emitErlExpr () "erlang:self()"

    let comp = async {
        let! child =
            Async.StartChild(async {
                sendCancellationReady parent

                match Erlang.receive<CancellationProbeMessage> 1000 with
                | Some CancellationProbeContinue -> return cts.Token.IsCancellationRequested
                | _ -> return false
            })

        match waitForCancellationReady () with
        | Some worker ->
            cts.Cancel()
            emitErlExpr worker "$0 ! cancellation_probe_continue"
            return! child
        | None -> return false
    }

    Async.RunSynchronously comp |> equal true

[<Fact>]
let ``test Async Sleep in another process is interrupted by live cancellation`` () =
    let cts = new System.Threading.CancellationTokenSource()
    let parent: obj = emitErlExpr () "erlang:self()"

    let comp = async {
        let! child =
            Async.StartChild(async {
                let wasCancelledBeforeStart = cts.Token.IsCancellationRequested
                sendCancellationReady parent
                let mutable cancelled = false

                Async.StartWithContinuations(
                    async { do! Async.Sleep 1000 },
                    ignore,
                    ignore,
                    (fun _ -> cancelled <- true),
                    cts.Token
                )

                return
                    not wasCancelledBeforeStart
                    && cancelled
                    && cts.Token.IsCancellationRequested
            })

        match waitForCancellationReady () with
        | Some _ ->
            cts.Cancel()
            return! child
        | None -> return false
    }

    Async.RunSynchronously comp |> equal true

[<Fact>]
let ``test cross-process cancellation registration runs once`` () =
    let cts = new System.Threading.CancellationTokenSource()
    let parent: obj = emitErlExpr () "erlang:self()"

    let comp = async {
        let! child =
            Async.StartChild(async {
                cts.Token.Register(fun () ->
                    emitErlExpr parent "$0 ! cancellation_callback_invoked"
                )
                |> ignore

                sendCancellationReady parent

                match Erlang.receive<CancellationProbeMessage> 1000 with
                | Some CancellationProbeContinue -> return true
                | _ -> return false
            })

        match waitForCancellationReady () with
        | Some worker ->
            cts.Cancel()
            cts.Cancel()

            let invokedOnce =
                match Erlang.receive<CancellationProbeMessage> 1000 with
                | Some CancellationCallbackInvoked ->
                    Erlang.receive<CancellationProbeMessage> 0 |> Option.isNone
                | _ -> false

            emitErlExpr worker "$0 ! cancellation_probe_continue"
            let! childCompleted = child
            return invokedOnce && childCompleted
        | None -> return false
    }

    Async.RunSynchronously comp |> equal true

[<Fact>]
let ``test CancellationToken registration Dispose waits for a running callback`` () =
    let cts = new System.Threading.CancellationTokenSource()
    let parent: obj = emitErlExpr () "erlang:self()"
    let registration =
        cts.Token.Register(fun () ->
            let worker: obj = emitErlExpr () "erlang:self()"
            emitErlExpr (parent, worker) "$0 ! {cancellation_callback_started, $1}"
            Erlang.receive<CancellationProbeMessage> 1000 |> ignore
        )

    startCancellationProcess (fun () -> cts.Cancel()) |> ignore

    match Erlang.receive<CancellationProbeMessage> 1000 with
    | Some(CancellationCallbackStarted worker) ->
        startCancellationProcess (fun () ->
            registration.Dispose()
            emitErlExpr parent "$0 ! cancellation_dispose_returned"
        ) |> ignore

        Erlang.receive<CancellationProbeMessage> 0 |> Option.isNone |> equal true
        emitErlExpr worker "$0 ! cancellation_probe_continue"

        match Erlang.receive<CancellationProbeMessage> 1000 with
        | Some CancellationDisposeReturned -> ()
        | _ -> failwith "Dispose did not return after the callback completed"
    | _ -> failwith "Cancellation callback did not start"

[<Fact>]
let ``test Async AwaitEvent cancellation runs in the event owner process`` () =
    let cts = new System.Threading.CancellationTokenSource()
    let parent: obj = emitErlExpr () "erlang:self()"

    let comp = async {
        let! child =
            Async.StartChild(async {
                let ev = Event<int>()
                let mutable cancelCalled = false
                sendCancellationReady parent

                let wasCancelled =
                    try
                        runSynchronouslyWithCancellation
                            (async {
                                let! _ = Async.AwaitEvent(ev.Publish, fun () -> cancelCalled <- true)
                                return false
                            })
                            cts.Token
                    with _ -> true

                return wasCancelled && cancelCalled
            })

        match waitForCancellationReady () with
        | Some _ ->
            cts.Cancel()
            return! child
        | None -> return false
    }

    Async.RunSynchronously comp |> equal true

[<Fact>]
let ``test CancellationTokenSource create and cancel works`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    equal false tcs.Token.IsCancellationRequested
    tcs.Cancel()
    equal true tcs.Token.IsCancellationRequested

[<Fact>]
let ``test CancellationTokenSource pre-cancelled works`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Cancel()
    equal true tcs.Token.IsCancellationRequested

[<Fact>]
let ``test CancellationTokenSourceRegister works`` () =
    let mutable x = 0
    let tcs = new System.Threading.CancellationTokenSource()
    let _reg = tcs.Token.Register(fun () -> x <- x + 1)
    equal 0 x
    tcs.Cancel()
    equal 1 x

[<Fact>]
let ``test CancellationTokenSource multiple registers work`` () =
    let mutable x = 0
    let tcs = new System.Threading.CancellationTokenSource()
    let _reg1 = tcs.Token.Register(fun () -> x <- x + 1)
    let _reg2 = tcs.Token.Register(fun () -> x <- x + 10)
    tcs.Cancel()
    equal 11 x

[<Fact>]
let ``test CancellationToken registration after cancellation runs immediately`` () =
    let mutable calls = 0
    let cts = new System.Threading.CancellationTokenSource()
    cts.Cancel()

    cts.Token.Register(fun () -> calls <- calls + 1) |> ignore

    equal 1 calls

[<Fact>]
let ``test CancellationToken registration can be disposed`` () =
    let mutable calls = 0
    let cts = new System.Threading.CancellationTokenSource()
    let registration = cts.Token.Register(fun () -> calls <- calls + 1)
    registration.Dispose()

    cts.Cancel()

    equal 0 calls

[<Fact>]
let ``test async cancellation with pre-cancelled token`` () =
    let mutable res = false
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Cancel()
    Async.StartWithContinuations(
        async {
            do! Async.Sleep 10
            res <- true
        },
        ignore,
        ignore,
        (fun _ -> ()),  // on cancel
        tcs.Token)
    equal false res

[<Fact>]
let ``test async cancellation with auto-cancel token`` () =
    let mutable res = false
    let tcs = new System.Threading.CancellationTokenSource(50)
    Async.StartImmediate(async {
        do! Async.Sleep 200
        res <- true
    }, tcs.Token)
    equal false res

[<Fact>]
let ``test CancellationToken Dispose is no-op`` () =
    let tcs = new System.Threading.CancellationTokenSource()
    tcs.Dispose()
    // Should not throw - Dispose is a no-op
    equal false tcs.Token.IsCancellationRequested

[<Fact>]
let ``test Can use custom exceptions in async workflows`` () =
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
    } |> Async.RunSynchronously

[<Fact>]
let ``test Async.StartChild timeout cancels child and runs finalizer`` () =
#if FABLE_COMPILER
    let state: obj = Fable.Core.BeamInterop.emitErlExpr () "atomics:new(2, [])"
    let mark (slot: int) : unit = Fable.Core.BeamInterop.emitErlExpr (state, slot) "atomics:put($0, $1, 1)"
    let read (slot: int) : int = Fable.Core.BeamInterop.emitErlExpr (state, slot) "atomics:get($0, $1)"
#else
    let mutable completed = 0
    let mutable finalized = 0
    let mark slot = if slot = 1 then System.Threading.Interlocked.Exchange(&completed, 1) |> ignore<int> else System.Threading.Interlocked.Exchange(&finalized, 1) |> ignore<int>
    let read slot = if slot = 1 then completed else finalized
#endif
    let child = async {
        try
            do! Async.Sleep 200
            mark 1
        finally
            mark 2
    }
    Async.RunSynchronously(async {
        let! inner = Async.StartChild(child, 50)
        let! result = Async.Catch inner
        match result with
        | Choice1Of2 _ -> failwith "Expected timeout"
        | Choice2Of2 error -> (error :? TimeoutException) |> equal true
        let! token = Async.CancellationToken
        token.IsCancellationRequested |> equal false
        do! Async.Sleep 250
    })
    read 1 |> equal 0
    read 2 |> equal 1

[<Fact>]
let ``test Async.AwaitEvent fires continuation when event is triggered`` () =
    let ev = Event<int>()
    let cts = new System.Threading.CancellationTokenSource()
    let mutable cancelCalled = false
    let mutable completed = false
    Async.StartImmediate(async {
        let! v = Async.AwaitEvent(ev.Publish, fun () -> cancelCalled <- true)
        equal 42 v
        completed <- true
    }, cts.Token)
    ev.Trigger(42)
    cts.Cancel()
    equal true completed
    equal false cancelCalled

[<Fact>]
let ``test Async.AwaitEvent with cancelAction invokes it on cancellation`` () =
    let ev = Event<int>()
    let cts = new System.Threading.CancellationTokenSource()
    let mutable cancelCalled = false
    Async.StartImmediate(async {
        let! _ = Async.AwaitEvent(ev.Publish, fun () -> cancelCalled <- true)
        ()
    }, cts.Token)
    cts.Cancel()
    equal true cancelCalled

#endif
[<Fact>]
let ``test FromContinuations unit and generic values`` () =
    let fromValue value = Async.FromContinuations(fun (ok, _, _) -> ok value)
    let mutable completed = 0
    let unitWork: Async<unit> = Async.FromContinuations(fun (ok, _, _) -> ok ())
    Async.StartWithContinuations(unitWork, (fun () -> completed <- completed + 1), raise, raise)
    Async.StartWithContinuations(fromValue (), (fun () -> completed <- completed + 1), raise, raise)
    Async.StartWithContinuations(fromValue 42, (fun value -> equal 42 value; completed <- completed + 1), raise, raise)
    equal 3 completed

[<Fact>]
let ``test unit continuations preserve captured callbacks`` () =
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
