[<Fable.Core.Rust.OuterAttr("cfg", [|"feature = \"threaded\""|])>]
module Fable.Tests.AsyncTests

open Util.Testing
open System.Threading
open System.Threading.Tasks

[<Fact>]
let shouldExecPrim () =
    let z = async { return 3 }
    let comp = async {
        let x = 1
        let y = 2
        let! z = z
        return x + y + z
    }
    let t = Async.StartAsTask comp
    t.Result |> equal 6

[<Fact>]
let shouldExecSynchronously () =
    let y = async { return 3 }
    let comp = async {
        let x = 1
        let! y = y
        return x + y
    }
    let t = Async.RunSynchronously comp
    t |> equal 4

[<Fact>]
let shouldConvertTaskToASyncAndEvalCorrectly () =
    let t = task { return 1 } |> Async.AwaitTask
    t |> Async.RunSynchronously |> equal 1

// [<Fact>]
// let shouldExecAsParallelStructurallyCorrect () =
//     let t = Async.Parallel [
//         async { return 1 }
//         async { return 2 }
//     ]
//     t |> Async.RunSynchronously |> Array.sum |> equal 3

// [<Fact>]
// let shouldMarshalMutOverAsyncClosureCorrectly () =
//     let mutable x = 1
//     let comp = async {
//         let! z = async { return 3 }
//         x <- x + 1
//         return x + z
//     }
//     let t = Async.StartAsTask comp
//     t.Result |> equal 5

[<Fable.Core.Rust.ReferenceType(Fable.Core.Rust.PointerType.Arc)>]
type ArcRecord = {
    A: int
}

[<Fact>]
let shouldCorrectlyScopeArcRecord () =
    let a = { A = 3 }
    let ab = async { return a }
    let comp = async {
        let x = 2
        let! a = ab
        let y = a.A
        return x + y
    }

    let t = Async.StartAsTask comp
    t.Result |> equal 5

[<Fact>]
let shouldExecuteTask () =
    let a = Task.FromResult 1
    let b = Task.FromResult 2
    let comp = task {
        let! a = a
        let! b = b
        return a + b
    }
    comp.Result |> equal 3

[<Fact>]
let shouldExecuteMutationOnTask () =
    let a = Task.FromResult 0
    let mutable x = 0
    let comp = task {
        let! _ = a
        x <- x + 1
    }
    do comp.Result
    x |> equal 1

// [<Fact>]
// let ``should execute mutation on thread unsafe`` () =
//     let mutable x = 1
//     let t = Thread(fun () -> x <- x + 1)
//     t.Start()
//     t.Join()
//     x |> equal 2


// [<Fact>]
// let monitorShouldWorkWithSystemObj () =
//     let o = new System.Object() // todo - this doesn't work, and outputs unit
//     Monitor.Enter(o)
//     Monitor.Exit(o)

type Data = {
    x: string //deliberately use reference type to confirm nested Lrc
}

[<Fact>]
let ``Monitor should enter and exit correctly with Lrc`` () =
    let o = { x = "test" }
    Monitor.Enter(o)
    Monitor.Exit(o)

[<Fact>]
let ``Monitor Should block on thread until lock has been released with Lrc`` () =
    let mutable events = []
    let o = { x = "test" }
    Monitor.Enter(o)
    let z = async { return 3 }
    let a1 = async {
        let! _ = z //ensuring subsequent lines are continuation, or you get a deadlock as same thread - may not be consistent with .NET, need to investigate
        Monitor.Enter(o)
        events <- 1::events
        Monitor.Exit(o)
    }
    let t = a1 |> Async.StartAsTask
    Thread.Sleep(100);

    events <- 2::events
    Monitor.Exit(o)

    do t.Result
    events |> equal [1; 2]

[<Fact>]
let ``For monitor - Should block on thread until lock has been released`` () =
    let mutable events = []
    //let o = new System.Object()
    let o = { x = "test" }
    Monitor.Enter(o)
    let t1 = new Thread(fun () -> lock o (fun () -> events <- 1::events))
    t1.Start()
    Thread.Sleep(100);

    events <- 2::events
    Monitor.Exit(o)

    t1.Join()
    events |> equal [1; 2]

[<Fact>]
let ``Lock should return result`` () =
    let o = { x = "test" }
    let res = lock o (fun () -> { x = "42"})
    res.x |> equal "42"

//[<Fact>]
// let testShouldMutateAndLock () =
//     let o = new System.Object()
//     let mutable x = 1
//     let lazyGet = async { return 1 }
//     let comp = async {
//         let! _ = lazyGet
//         lock o (fun () -> x <- x + 1)
//     }
//     let t = Async.StartAsTask comp
//     do t.Result
//     x |> equal 2


// type DisposableAction(f) =
//     interface IDisposable with
//         member _.Dispose() = f()

// let successWork: Async<string> = Async.FromContinuations(fun (onSuccess,_,_) -> onSuccess "success")
// let errorWork: Async<string> = Async.FromContinuations(fun (_,onError,_) -> onError (exn "error"))
// let cancelWork: Async<string> = Async.FromContinuations(fun (_,_,onCancel) ->
//         System.OperationCanceledException("cancelled") |> onCancel)

// type Message = string * AsyncReplyChannel<string>

// type Get =
//     | GetZero of replyChannel: AsyncReplyChannel<int>
//     | GetOne of replyChannel: AsyncReplyChannel<int>

// type MyException(value) =
//     inherit Exception()
//     member _.Value: int = value

// let sleepAndAssign token res =
//     Async.StartImmediate(async {
//         do! Async.Sleep 200
//         res := true
//     }, token)

// let asyncMap f a = async {
//     let! a = a
//     return f a
// }


// [<Fact>]
// let ``Simple async translates without exception`` () =
//     async { return () }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async while binding works correctly`` () =
//     let mutable result = 0
//     async {
//         while result < 10 do
//             result <- result + 1
//     } |> Async.StartImmediate
//     equal result 10

// [<Fact>]
// let ``Async for binding works correctly`` () =
//     let inputs = [|1; 2; 3|]
//     let result = ref 0
//     async {
//         for inp in inputs do
//             result := !result + inp
//     } |> Async.StartImmediate
//     equal !result 6

// [<Fact>]
// let ``Async exceptions are handled correctly`` () =
//     let result = ref 0
//     let f shouldThrow =
//         async {
//             try
//                 if shouldThrow then failwith "boom!"
//                 else result := 12
//             with _ -> result := 10
//         } |> Async.StartImmediate
//         !result
//     f true + f false |> equal 22

// [<Fact>]
// let ``Simple async is executed correctly`` () =
//     let result = ref false
//     let x = async { return 99 }
//     async {
//         let! x = x
//         let y = 99
//         result := x = y
//     }
//     //TODO: RunSynchronously would make more sense here but in JS I think this will be ok.
//     |> Async.StartImmediate
//     equal !result true

// [<Fact>]
// let ``async use statements should dispose of resources when they go out of scope`` () =
//     let isDisposed = ref false
//     let step1ok = ref false
//     let step2ok = ref false
//     let resource = async {
//         return new DisposableAction(fun () -> isDisposed := true)
//     }
//     async {
//         use! r = resource
//         step1ok := not !isDisposed
//     }
//     //TODO: RunSynchronously would make more sense here but in JS I think this will be ok.
//     |> Async.StartImmediate
//     step2ok := !isDisposed
//     (!step1ok && !step2ok) |> equal true

// [<Fact>]
// let ``Try _ with _ expressions inside async expressions work the same`` () =
//     let result = ref ""
//     let throw() : unit =
//         raise(exn "Boo!")
//     let append(x) =
//         result := !result + x
//     let innerAsync() =
//         async {
//             append "b"
//             try append "c"
//                 throw()
//                 append "1"
//             with _ -> append "d"
//             append "e"
//         }
//     async {
//         append "a"
//         try do! innerAsync()
//         with _ -> append "2"
//         append "f"
//     } |> Async.StartImmediate
//     equal !result "abcdef"

// // Disable this test for dotnet as it's failing too many times in Appveyor
// #if FABLE_COMPILER
// [<Fact>]
// let ``Async cancellation works`` () =
//     async {
//         let res1, res2, res3 = ref false, ref false, ref false
//         let tcs1 = new CancellationTokenSource(50)
//         let tcs2 = new CancellationTokenSource()
//         let tcs3 = new CancellationTokenSource()
//         sleepAndAssign tcs1.Token res1
//         sleepAndAssign tcs2.Token res2
//         sleepAndAssign tcs3.Token res3
//         tcs2.Cancel()
//         tcs3.CancelAfter(1000)
//         do! Async.Sleep 500
//         equal false !res1
//         equal false !res2
//         equal true !res3
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``CancellationTokenSource.Register works`` () =
//     async {
//         let mutable x = 0
//         let res1 = ref false
//         let tcs1 = new CancellationTokenSource(50)
//         let foo = tcs1.Token.Register(fun () ->
//             x <- x + 1)
//         sleepAndAssign tcs1.Token res1
//         do! Async.Sleep 500
//         equal false !res1
//         equal 1 x
//     }
//     |> Async.StartImmediate
// #endif

// [<Fact>]
// let ``Async.StartWithContinuations works`` () =
//     let res1, res2, res3 = ref "", ref "", ref ""
//     Async.StartWithContinuations(successWork, (fun x -> res1 := x), ignore, ignore)
//     Async.StartWithContinuations(errorWork, ignore, (fun x -> res2 := x.Message), ignore)
//     Async.StartWithContinuations(cancelWork, ignore, ignore, (fun x -> res3 := x.Message))
//     equal "success" !res1
//     equal "error" !res2
//     equal "cancelled" !res3

// [<Fact>]
// let ``Async.Catch works`` () =
//     let assign res = function
//         | Choice1Of2 msg -> res := msg
//         | Choice2Of2 (ex: Exception) -> res := "ERROR: " + ex.Message
//     let res1 = ref ""
//     let res2 = ref ""
//     async {
//         let! x1 = successWork |> Async.Catch
//         assign res1 x1
//         let! x2 = errorWork |> Async.Catch
//         assign res2 x2
//     } |> Async.StartImmediate
//     equal "success" !res1
//     equal "ERROR: error" !res2

// [<Fact>]
// let ``Async.Ignore works`` () =
//     let res = ref false
//     async {
//         do! successWork |> Async.Ignore
//         res := true
//     } |> Async.StartImmediate
//     equal true !res

// [<Fact>]
// let ``Async.OnCancel works`` () =
//     async {
//         let res = ref false
//         let tcs = new CancellationTokenSource()
//         Async.StartImmediate(async {
//             let! _ = Async.OnCancel(fun () -> res := true)
//             do! Async.Sleep 100
//         }, tcs.Token)
//         tcs.CancelAfter 50
//         do! Async.Sleep 75
//         equal true !res
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async.Parallel works`` () =
//     async {
//         let makeWork i =
//             async {
//                 do! Async.Sleep 200
//                 return i
//             }
//         let res: int[] ref = ref [||]
//         let works = [makeWork 1; makeWork 2; makeWork 3]
//         async {
//             let! x = Async.Parallel works
//             res := x
//         } |> Async.StartImmediate
//         do! Async.Sleep 500
//         !res |> Array.sum |> equal 6
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async.Parallel is lazy`` () =
//     async {
//         let mutable x = 0
//         let add i =
// #if FABLE_COMPILER
//             x <- x + i
// #else
//             Interlocked.Add(&x, i) |> ignore<int>
// #endif
//         let a = Async.Parallel [
//             async { add 1 }
//             async { add 2 }
//         ]
//         do! Async.Sleep 100
//         equal 0 x
//         let! _ = a
//         equal 3 x
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async.Sequential works`` () =
//     async {
//         let mutable _aggregate = 0

//         let makeWork i =
//             async {
//                 // check that the individual work items run sequentially and not interleaved
//                 _aggregate <- _aggregate + i
//                 let copyOfI = _aggregate
//                 do! Async.Sleep 100
//                 equal copyOfI _aggregate
//                 do! Async.Sleep 100
//                 equal copyOfI _aggregate
//                 return i
//             }
//         let works = [ for i in 1 .. 5 -> makeWork i ]
//         let now = DateTimeOffset.Now
//         let! result = Async.Sequential works
//         let ``then`` = DateTimeOffset.Now
//         let d = ``then`` - now
//         // For some reason this sometimes fail in CI if d is exactly 1s so give it some breadth
//         if d < TimeSpan.FromSeconds 0.9 then
//             failwithf "expected sequential operations to take longer than 1 second, but took %0.00f" d.TotalSeconds
//         result |> equal [| 1 .. 5 |]
//         result |> Seq.sum |> equal _aggregate
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async.Sequential is lazy`` () =
//     async {
//         let mutable x = 0
//         let a = Async.Sequential [
//             async { x <- x + 1 }
//             async { x <- x + 2 }
//         ]
//         do! Async.Sleep 100
//         equal 0 x
//         let! _ = a
//         equal 3 x
//     }
//     |> Async.StartImmediate

// #if FABLE_COMPILER
// [<Fact>]
// let ``Interaction between Async and Promise works`` () =
//     async {
//         let res = ref false
//         async { res := true }
//         |> Async.StartAsPromise
//         |> Async.AwaitPromise
//         |> Async.StartImmediate
//         equal true !res
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Promises can be cancelled`` () =
//     async {
//         let res = ref 0
//         let tcs = new CancellationTokenSource(50)
//         let work =
//             let work = async {
//                 do! Async.Sleep 75
//                 res := -1
//             }
//             Async.StartAsPromise(work, tcs.Token) |> Async.AwaitPromise
//         // behavior change: a cancelled task is now triggering the exception continuation instead of
//         // the cancellation continuation, see: https://github.com/Microsoft/visualfsharp/issues/1416
//         // also, System.OperationCanceledException will be changed to TaskCanceledException (not yet)
//         Async.StartWithContinuations(work, ignore, ignore, (fun _ -> res := 1))
//         do! Async.Sleep 100
//         equal 1 !res
//     }
//     |> Async.StartImmediate
// #endif

// [<Fact>]
// let ``MailboxProcessor.post works`` () =
//     async {
//         let mutable res = None
//         let agent = new MailboxProcessor<int>(fun inbox ->
//             let rec messageLoop() = async {
//                 let! msg = inbox.Receive()
//                 match msg with
//                 | 3 -> () // Finish loop
//                 | _ ->
//                     res <- Some msg
//                     return! messageLoop()
//             }
//             messageLoop()
//         )
//         agent.Post(1)
//         equal None res // Mailbox hasn't started yet
//         agent.Start()
//         agent.Post(2)
//         // Not necessary in the JS implementation, but in .NET
//         // MailboxProcessor works in the background so we must wait a bit
//         do! Async.Sleep 200
//         equal (Some 2) res
//         agent.Post(3)
//         equal (Some 2) res  // Mailbox has finished
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``MailboxProcessor.postAndAsyncReply works`` () =
//     async {
//         let formatString = "Msg: {0} - {1}"
//         let agent = MailboxProcessor<Message>.Start(fun inbox ->
//             let rec loop n = async {
//                 let! (message, replyChannel) = inbox.Receive()
//                 do! Async.Sleep(100) // Delay a bit
//                 replyChannel.Reply(String.Format(formatString, n, message))
//                 if message <> "Bye" then do! loop (n + 1)
//             }
//             loop 0)
//         let! resp = agent.PostAndAsyncReply(fun replyChannel -> "Hi", replyChannel)
//         equal "Msg: 0 - Hi" resp
//         let! resp = agent.PostAndAsyncReply(fun replyChannel -> "Bye", replyChannel)
//         equal "Msg: 1 - Bye" resp
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``MailboxProcessor.postAndAsyncReply works with falsy values`` () = // See #1588
//     async {
//         let agent = MailboxProcessor.Start(fun inbox ->
//             let rec loop () =
//                 async {
//                     let! msg = inbox.Receive()
//                     match msg with
//                     | GetZero replyChannel ->
//                         // printfn "Replying 0"
//                         replyChannel.Reply 0
//                         return! loop ()
//                     | GetOne replyChannel ->
//                         // printfn "Replying 1"
//                         replyChannel.Reply 1
//                         return! loop ()
//                 }
//             loop () )

//         let! resp = agent.PostAndAsyncReply(GetOne)
//         equal 1 resp
//         let! resp = agent.PostAndAsyncReply(GetZero)
//         equal 0 resp
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async try _ with _ returns correctly from the with branch`` () =
//     let work = async {
//         try
//             failwith "testing"
//             return -1
//         with e ->
//             return 42 }
//     let mutable result = 0
//     Async.StartWithContinuations(work, (fun r -> result <- r), ignore, ignore)
//     equal result 42


// [<Fact>]
// let ``Deep recursion with async doesn't cause stack overflow`` () =
//     async {
//         let result = ref false
//         let rec trampolineTest res i = async {
//             if i > 100000
//             then res := true
//             else return! trampolineTest res (i+1)
//         }
//         do! trampolineTest result 0
//         equal !result true
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Nested failure propagates in async expressions`` () =
//     async {
//         let data = ref ""
//         let f1 x =
//             async {
//                 try
//                     failwith "1"
//                     return x
//                 with
//                 | e -> return! failwith ("2 " + e.Message)
//             }
//         let f2 x =
//             async {
//                 try
//                     return! f1 x
//                 with
//                 | e -> return! failwith ("3 " + e.Message)
//             }
//         let f() =
//             async {
//                 try
//                     let! y = f2 4
//                     return ()
//                 with
//                 | e -> data := e.Message
//             }
//             |> Async.StartImmediate
//         f()
//         do! Async.Sleep 100
//         equal "3 2 1" !data
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Try _ finally _ expressions inside async expressions work`` () =
//     async {
//         let data = ref ""
//         async {
//             try data := !data + "1 "
//             finally data := !data + "2 "
//         } |> Async.StartImmediate
//         async {
//             try
//                 try failwith "boom!"
//                 finally data := !data + "3"
//             with _ -> ()
//         } |> Async.StartImmediate
//         do! Async.Sleep 100
//         equal "1 2 3" !data
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Final statement inside async expressions can throw`` () =
//     async {
//         let data = ref ""
//         let f() = async {
//             try data := !data + "1 "
//             finally failwith "boom!"
//         }
//         async {
//             try
//                 do! f()
//                 return ()
//             with
//             | e -> data := !data + e.Message
//         }
//         |> Async.StartImmediate
//         do! Async.Sleep 100
//         equal "1 boom!" !data
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async.Bind propagates exceptions`` () = // See #724
//     async {
//         let task1 name = async {
//             // printfn "testing with %s" name
//             if name = "fail" then
//                 failwith "Invalid access credentials"
//             return "Ok"
//         }

//         let task2 name = async {
//             // printfn "testing with %s" name
//             do! Async.Sleep 100 //difference between task1 and task2
//             if name = "fail" then
//                 failwith "Invalid access credentials"
//             return "Ok"
//         }

//         let doWork name task =
//             let catch comp = async {
//                 let! res = Async.Catch comp
//                 return
//                     match res with
//                     | Choice1Of2 str -> str
//                     | Choice2Of2 ex -> ex.Message
//             }
//             // printfn "doing work - %s" name
//             async {
//                 let! a = task "work" |> catch
//                 // printfn "work - %A" a
//                 let! b = task "fail" |> catch
//                 // printfn "fail - %A" b
//                 return a, b
//             }

//         let! res1 = doWork "task1" task1
//         let! res2 = doWork "task2" task2
//         equal ("Ok", "Invalid access credentials") res1
//         equal ("Ok", "Invalid access credentials") res2
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async.StartChild works`` () =
//     async {
//         let mutable x = ""
//         let taskA = async {
//             do! Async.Sleep 500
//             x <- x + "D"
//             return "E"
//         }
//         let taskB = async {
//             do! Async.Sleep 100
//             x <- x + "C"
//             return "F"
//         }
//         let! result1Async = taskA |> Async.StartChild // start first request but do not wait
//         let! result2Async = taskB |> Async.StartChild // start second request in parallel
//         x <- x + "AB"
//         let! result1 = result1Async
//         let! result2 = result2Async
//         x <- x + result1 + result2
//         equal x "ABCDEF"
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Async.StartChild applys timeout`` () =
//     async {
//         let mutable x = ""
//         let task = async {
//             x <- x + "A"
//             do! Async.Sleep 1_000
//             x <- x + "X" // Never hit
//         }
//         try
//             let! childTask = Async.StartChild (task, 200)
//             do! childTask
//         with
//             | :? TimeoutException ->
//                 x <- x + "B"
//         x <- x + "C"
//         equal x "ABC"
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Unit arguments are erased`` () = // See #1832
//     let mutable token = 0
//     async {
//         let! res =
//             async.Return 5
//             |> asyncMap (fun x -> token <- x)
//         equal 5 token
//         res
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Can use custom exceptions in async workflows #2396`` () =
//     let workflow(): Async<unit> = async {
//         return MyException(7) |> raise
//     }
//     let parentWorkflow() =
//         async {
//             try
//                 do! workflow()
//                 return 100
//             with
//             | :? MyException as ex -> return ex.Value
//         }
//     async {
//         let! res = parentWorkflow()
//         equal 7 res
//     }
//     |> Async.StartImmediate


// [<Fact>]
// let ``Timer with AutoReset = true works`` () =
//     async {
//         let res = ref 0
//         let t = new Timers.Timer(50.)
//         t.Elapsed.Add(fun ev -> res := !res + 5)
//         t.Start()
//         do! Async.Sleep 125
//         t.Stop()
//         do! Async.Sleep 50
//         equal 10 !res
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Timer with AutoReset = false works`` () =
//     async {
//         let res = ref 0
//         let t = new Timers.Timer()
//         t.Elapsed.Add(fun ev -> res := !res + 5)
//         t.AutoReset <- false
//         t.Interval <- 25.
//         t.Enabled <- true
//         do! Async.Sleep 100
//         equal 5 !res
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Timer.Elapsed.Subscribe works`` () =
//     async {
//         let res = ref 0
//         let t = new Timers.Timer(50.)
//         let disp = t.Elapsed.Subscribe(fun ev -> res := !res + 5)
//         t.Start()
//         do! Async.Sleep 125
//         disp.Dispose()
//         do! Async.Sleep 50
//         equal 10 !res
//         t.Stop()
//     }
//     |> Async.StartImmediate

// [<Fact>]
// let ``Assigning an event to a variable works`` () = // See #1863
//     let createTimerAndObservable timerInterval =
//         // setup a timer
//         let timer = new System.Timers.Timer(float timerInterval)
//         timer.AutoReset <- true
//         // events are automatically IObservable
//         let observable = timer.Elapsed
//         // return an async task
//         let task = async {
//             timer.Start()
//             do! Async.Sleep 200
//             timer.Stop()
//         }
//         // return a async task and the observable
//         (task,observable)
//     // create the timer and the corresponding observable
//     let basicTimer2 , timerEventStream = createTimerAndObservable 50

//     let mutable acc = 1
//     // register that everytime something happens on the
//     // event stream, print the time.
//     timerEventStream |> Observable.subscribe (fun _ ->
//         acc <- acc + 1) |>ignore

//     async {
//         do! basicTimer2
//         // printfn "%i" acc
//         acc > 2 |> equal true
//     }
//     |> Async.StartImmediate
