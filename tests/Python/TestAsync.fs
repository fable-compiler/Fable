module Fable.Tests.Async

open System
open Util.Testing

#if FABLE_COMPILER
module private Runtime =
    open Fable.Core.PyInterop

    type Observation =
        abstract terminal: string array
        abstract errors: string array
        abstract before: string array
        abstract outcome: string array
        abstract listeners: int
        abstract scheduled: int
        abstract cancel_count: int
        abstract retired: bool
        abstract owner_loop: bool
        abstract threads_finished: bool
        abstract pending_before: bool
        abstract pending_after: bool

    type ObserveSleep = delegate of Async<unit> * System.Threading.CancellationToken * string -> Observation
    type ObserveMailbox = delegate of Async<int> * MailboxProcessor<int> * System.Threading.CancellationToken -> Observation
    type ObserveOwnerLoop = delegate of Async<unit> * System.Threading.CancellationToken -> Observation
    type ObserveSettlement = delegate of ((unit -> unit) -> Async<unit>) * System.Threading.CancellationToken * string -> Observation

    let observeSleep: ObserveSleep = import "observe_sleep" "./py/async_runtime.py"
    let observeMailbox: ObserveMailbox = import "observe_mailbox" "./py/async_runtime.py"
    let observeOwnerLoop: ObserveOwnerLoop = import "observe_owner_loop" "./py/async_runtime.py"
    let observeSettlement: ObserveSettlement = import "observe_settlement" "./py/async_runtime.py"
#endif

type DisposableAction(f) =
    interface IDisposable with
        member _.Dispose() = f()

type MyException(value) =
    inherit Exception()
    member _.Value: int = value

let asyncMap f a = async {
    let! a = a
    return f a
}

let sleepAndAssign token (res : Ref<bool>) =
    Async.StartImmediate(async {
        do! Async.Sleep 200
        res.Value <- true
    }, token)

let successWork: Async<string> = Async.FromContinuations(fun (onSuccess,_,_) -> onSuccess "success")
let errorWork: Async<string> = Async.FromContinuations(fun (_,onError,_) -> onError (exn "error"))
let cancelWork: Async<string> = Async.FromContinuations(fun (_,_,onCancel) ->
        System.OperationCanceledException("cancelled") |> onCancel)

[<Fact>]
let ``test Simple async translates without exception`` () =
    async { return () }
    |> Async.StartImmediate


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
let ``test async use statements should dispose of resources when they go out of scope`` () =
    let mutable isDisposed = false
    let mutable step1ok = false
    let mutable step2ok = false
    let resource = async {
        return new DisposableAction(fun () -> isDisposed <- true)
    }
    async {
        use! r = resource
        step1ok <- not isDisposed
    }
    //TODO: RunSynchronously would make more sense here but in JS I think this will be ok.
    |> Async.StartImmediate
    step2ok <- isDisposed
    (step1ok && step2ok) |> equal true

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

// Disable this test for dotnet as it's failing too many times in Appveyor
#if FABLE_COMPILER

[<Fact>]
let ``test async cancellation works`` () =
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
        equal false res1.Value
        equal false res2.Value
        equal true res3.Value
    } |> Async.StartImmediate

[<Fact>]
let ``test CancellationTokenSourceRegister works`` () =
    async {
        let mutable x = 0
        let res1 = ref false
        let tcs1 = new System.Threading.CancellationTokenSource(50)
        let foo = tcs1.Token.Register(fun () ->
            x <- x + 1)
        sleepAndAssign tcs1.Token res1
        do! Async.Sleep 500
        equal false res1.Value
        equal 1 x
    } |> Async.StartImmediate
#endif

[<Fact>]
let ``test Async StartWithContinuations works`` () =
    let res1, res2, res3 = ref "", ref "", ref ""
    Async.StartWithContinuations(successWork, (fun x -> res1.Value <- x), ignore, ignore)
    Async.StartWithContinuations(errorWork, ignore, (fun x -> res2.Value <- x.Message), ignore)
    Async.StartWithContinuations(cancelWork, ignore, ignore, (fun x -> res3.Value <- x.Message))
    equal "success" res1.Value
    equal "error" res2.Value
    equal "cancelled" res3.Value

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
let ``test Async.Ignore works`` () =
    let res = ref false
    async {
        do! successWork |> Async.Ignore
        res.Value <- true
    } |> Async.StartImmediate
    equal true res.Value

[<Fact>]
let ``test Async.Parallel works`` () =
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
            res.Value <- x
        } |> Async.StartImmediate
        do! Async.Sleep 500
        res.Value |> Array.sum |> equal 6
    } |> Async.RunSynchronously

[<Fact>]
let ``test Async.Parallel is lazy`` () =
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
    } |> Async.RunSynchronously

[<Fact>]
let ``test Async.Sequential works`` () =
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
        if d.TotalSeconds < 0.99 then
            failwithf "expected sequential operations to take 1 second or more, but took %.3f" d.TotalSeconds
        result |> equal [| 1 .. 5 |]
        result |> Seq.sum |> equal _aggregate
    } |> Async.RunSynchronously

[<Fact>]
let ``test Async.Sequential is lazy`` () =
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
    } |> Async.RunSynchronously

[<Fact>]
let ``test Interaction between Async and Task works`` () =
    async {
        let mutable res = false
        do!
            async { res <- true }
            |> Async.StartAsTask
            |> Async.AwaitTask

        equal true res
    } |> Async.RunSynchronously

#if FABLE_COMPILER
[<Fact>]
let ``test Tasks can be cancelled`` () =
    async {
        let mutable res = 0
        let tcs = new System.Threading.CancellationTokenSource(50)
        let work =
            let work = async {
                do! Async.Sleep 75
                res <- -1
            }
            Async.StartAsTask(work, cancellationToken=tcs.Token) |> Async.AwaitTask
        // behavior change: a cancelled task is now triggering the exception continuation instead of
        // the cancellation continuation, see: https://github.com/Microsoft/visualfsharp/issues/1416
        // also, System.OperationCanceledException will be changed to TaskCanceledException (not yet)
        Async.StartWithContinuations(work, ignore, ignore, (fun _ -> res <- 1))
        do! Async.Sleep 100
        equal 1 res
    } |> Async.StartImmediate
#endif

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
                | e -> data.Value <- e.Message
            }
            |> Async.StartImmediate
        f()
        do! Async.Sleep 100
        equal "3 2 1" data.Value
    } |> Async.StartImmediate

[<Fact>]
let ``test Try .. finally expressions inside async expressions work`` () =
    async {
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
        do! Async.Sleep 100
        equal "1 2 3" data.Value
    } |> Async.StartImmediate

[<Fact>]
let ``test Final statement inside async expressions can throw`` () =
    async {
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
        do! Async.Sleep 100
        equal "1 boom!" data.Value
    } |> Async.StartImmediate

[<Fact>]
let ``test Async.Bind propagates exceptions`` () = // See #724
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
    } |> Async.StartImmediate


[<Fact>]
let ``test Async.StartChild works`` () =
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
    } |> Async.StartImmediate

[<Fact>]
let ``test Async.StartChild applies timeout`` () =
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
    } |> Async.StartImmediate

[<Fact>]
let ``test Async.StartChild with timeout completes when computation finishes before timeout`` () = // See #4481
    async {
        let fast = async { do! Async.Sleep 10 }
        try
            let! child = Async.StartChild(fast, 1_000)
            do! child
            equal true true // should reach here
        with
            | :? TimeoutException ->
                failwith "should not time out"
    } |> Async.StartImmediate

[<Fact>]
let ``test Unit arguments are erased`` () = // See #1832
    let mutable token = 0
    async {
        let! res =
            async.Return 5
            |> asyncMap (fun x -> token <- x)
        equal 5 token
        res
    } |> Async.StartImmediate

[<Fact>]
let ``test Can use custom exceptions in async workflows #2396`` () =
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
    } |> Async.StartImmediate

[<Fact>]
let ``test Async.Sleep works correctly with TimeSpan argument`` () =
    async {
        let mutable executionOrder = ""

        // Test that TimeSpan(0, 1, 0) means 1 minute = 60,000 milliseconds, not 600,000,000 milliseconds
        let shortTask = async {
            do! Async.Sleep(System.TimeSpan(0, 0, 0, 0, 100)) // 100 milliseconds
            executionOrder <- executionOrder + "A"
        }

        let mediumTask = async {
            do! Async.Sleep(System.TimeSpan(0, 0, 0, 0, 300)) // 300 milliseconds
            executionOrder <- executionOrder + "B"
        }

        // Start both tasks in parallel
        let! shortTaskAsync = shortTask |> Async.StartChild
        let! mediumTaskAsync = mediumTask |> Async.StartChild

        // Wait for both to complete
        do! shortTaskAsync
        do! mediumTaskAsync

        // Verify execution order - shorter TimeSpan should complete first
        equal "AB" executionOrder

        // Test direct comparison with integer milliseconds
        let startTime = System.DateTime.Now
        do! Async.Sleep(System.TimeSpan(0, 0, 0, 0, 200)) // 200ms TimeSpan
        let endTime = System.DateTime.Now
        let elapsedMs = (endTime - startTime).TotalMilliseconds

        // Should be approximately 200ms, not 2,000,000ms (which would be 2000 seconds)
        // Allow some tolerance for timing variations
        let isReasonableTime = elapsedMs >= 150.0 && elapsedMs <= 500.0
        equal true isReasonableTime
    } |> Async.StartImmediate

[<Fact>]
let ``test Async.AwaitEvent fires continuation when event is triggered`` () =
    let ev = Event<int>()
    Async.StartImmediate(async {
        let! v = Async.AwaitEvent ev.Publish
        equal 42 v
    })
    ev.Trigger(42)

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

[<Fact>]
let ``test cooperative cancellation stops at the next async boundary`` () =
    use source = new System.Threading.CancellationTokenSource()
    let mutable finalizers = 0
    let mutable terminal = ""
    let mutable reached = false
    let work = async {
        try
            source.Cancel()
            do! async { return () }
            reached <- true
        finally
            finalizers <- finalizers + 1
    }
    Async.StartWithContinuations(work, (fun () -> terminal <- "success"), (fun _ -> terminal <- "error"), (fun _ -> terminal <- "cancel"), source.Token)
    equal false reached
    equal 1 finalizers
    equal "cancel" terminal


[<Fact>]
let ``test finalizer failure overrides success once`` () =
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

[<Fact>]
let ``test finalizer failure overrides body failure once`` () =
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

[<Fact>]
let ``test finalizer failure preserves cancellation once`` () =
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

#if FABLE_COMPILER
let private checkSleep cancelFirst throws =
    use source = new System.Threading.CancellationTokenSource()
    let mutable finalizers = 0
    let work = async {
        try do! Async.Sleep 100
        finally
            finalizers <- finalizers + 1
            if throws then failwith "finalizer"
    }
    let result = Runtime.observeSleep.Invoke(work, source.Token, if cancelFirst then "cancel" else "timeout")
    let expected = if cancelFirst then "cancel" elif throws then "error" else "success"
    equal [| expected |] result.terminal
    equal 1 finalizers
    equal 0 result.listeners
    equal 1 result.scheduled
    equal true result.retired
    equal 1 result.cancel_count
    if throws && not cancelFirst then equal [| "finalizer" |] result.errors
    else equal [||] result.errors

[<Fact>]
let ``test cancelled Sleep ignores stale timers and cleans up`` () =
    checkSleep true false

[<Fact>]
let ``test completed Sleep ignores cancellation and stale timers`` () =
    checkSleep false false

[<Fact>]
let ``test cancelled Sleep ignores a throwing finalizer and stale timers`` () =
    checkSleep true true

[<Fact>]
let ``test precancelled Sleep does not execute or schedule`` () =
    use source = new System.Threading.CancellationTokenSource()
    let mutable reached = false
    let work = async {
        reached <- true
        do! Async.Sleep 100
    }
    let result = Runtime.observeSleep.Invoke(work, source.Token, "precancel")
    equal [| "cancel" |] result.terminal
    equal false reached
    equal 0 result.scheduled
    equal 0 result.listeners

[<Fact>]
let ``test idle Receive observes cancellation only after a post`` () =
    use source = new System.Threading.CancellationTokenSource()
    let mailbox = new MailboxProcessor<int>((fun _ -> async { return () }), source.Token)
    let result = Runtime.observeMailbox.Invoke(mailbox.Receive(), mailbox, source.Token)
    equal [||] result.before
    equal true result.pending_before
    equal [| "cancel" |] result.terminal
    equal false result.pending_after
    equal 0 result.listeners

[<Fact>]
let ``test Sleep cancellation from another thread runs on the owner loop`` () =
    use source = new System.Threading.CancellationTokenSource()
    let result = Runtime.observeOwnerLoop.Invoke(Async.Sleep 100000, source.Token)
    equal [| "cancel" |] result.terminal
    equal true result.owner_loop
    equal true result.threads_finished
    equal 0 result.listeners

let private checkSettlement winner =
    use source = new System.Threading.CancellationTokenSource()
    let mutable finalizers = 0
    let makeWork timeout = async {
        try do! Async.Sleep 100
        finally
            finalizers <- finalizers + 1
            timeout ()
    }
    let result = Runtime.observeSettlement.Invoke(makeWork, source.Token, winner)
    equal [| winner |] result.outcome
    equal [| if winner = "timeout" then "success" else "cancel" |] result.terminal
    equal 1 finalizers
    equal true result.threads_finished
    equal 0 result.listeners
    equal true result.retired
    equal 1 result.cancel_count

[<Fact>]
let ``test reply wins concurrent reply cancellation and timeout settlement`` () =
    checkSettlement "reply"

[<Fact>]
let ``test cancellation wins concurrent reply cancellation and timeout settlement`` () =
    checkSettlement "cancel"

[<Fact>]
let ``test timeout wins concurrent reply cancellation and timeout settlement`` () =
    checkSettlement "timeout"

[<Fact>]
let ``test cancellation during Sleep installation releases resources`` () =
    use source = new System.Threading.CancellationTokenSource()
    let result = Runtime.observeSleep.Invoke(Async.Sleep 100, source.Token, "install_cancel")
    equal [| "cancel" |] result.terminal
    equal 0 result.listeners
    equal 1 result.scheduled
    equal true result.retired
    equal 1 result.cancel_count

[<Fact>]
let ``test failed Sleep installation releases its registration`` () =
    use source = new System.Threading.CancellationTokenSource()
    let result = Runtime.observeSleep.Invoke(Async.Sleep 100, source.Token, "install_error")
    equal [| "error" |] result.terminal
    equal [| "scheduler" |] result.errors
    equal 0 result.listeners
    equal 0 result.scheduled
#endif
