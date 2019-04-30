module Fable.Tests.Async

open System
open Util.Testing

#if FABLE_COMPILER
open Fable.Core
#endif

type DisposableAction(f) =
    interface IDisposable with
        member __.Dispose() = f()

let successWork: Async<string> = Async.FromContinuations(fun (onSuccess,_,_) -> onSuccess "success")
let errorWork: Async<string> = Async.FromContinuations(fun (_,onError,_) -> onError (exn "error"))
let cancelWork: Async<string> = Async.FromContinuations(fun (_,_,onCancel) ->
        System.OperationCanceledException("cancelled") |> onCancel)

type Message = string * AsyncReplyChannel<string>

type Get =
    | GetZero of replyChannel: AsyncReplyChannel<int>
    | GetOne of replyChannel: AsyncReplyChannel<int>

let sleepAndAssign token res =
    Async.StartImmediate(async {
        do! Async.Sleep 200
        res := true
    }, token)

let tests =
  testList "Async" [
    testCase "Simple async translates without exception" <| fun () ->
        async { return () }
        |> Async.StartImmediate

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

    // Disable this test for dotnet as it's failing too many times in Appveyor
    #if FABLE_COMPILER
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
    #endif

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
        let! result2Async = taskB |> Async.StartChild  // start second request in parallel
        x <- x + "AB"
        let! result1 = result1Async
        let! result2 = result2Async
        x <- x + result1 + result2
        equal x "ABCDEF"
      }
  ]