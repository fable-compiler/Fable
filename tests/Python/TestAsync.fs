module Fable.Tests.Async

open System
open Util.Testing

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
let ``test MailboxProcessor.post works`` () =
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
    } |> Async.StartImmediate

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
