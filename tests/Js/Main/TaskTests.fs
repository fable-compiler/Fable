module Fable.Tests.Task

open System
open System.Threading.Tasks
open Util.Testing
open Fable.Tests.Util

type DisposableAction(f) =
    interface IDisposable with
        member _.Dispose() = f ()

// Bridge pattern: works on .NET (Async.AwaitTask wraps Task) and JS (awaitPromise wraps Promise)
let awaitTask (t: Task<'T>) = Async.AwaitTask t

let tests =
  testList "Task" [
    testCaseAsync "Simple task translates without exception" <| fun () ->
        async {
            let! _ = awaitTask (task { return () })
            ()
        }

    testCaseAsync "Simple task result works" <| fun () ->
        async {
            let! result = awaitTask (task { return 42 })
            equal 42 result
        }

    testCaseAsync "Task.FromResult works" <| fun () ->
        async {
            let! result = awaitTask (Task.FromResult 42)
            equal 42 result
        }

    testCaseAsync "task while binding works correctly" <| fun () ->
        async {
            let mutable result = 0
            let! _ = awaitTask (task {
                while result < 10 do
                    result <- result + 1
            })
            equal 10 result
        }

    testCaseAsync "task while loop handles large number of iterations without stack overflow" <| fun () ->
        async {
            let mutable result = 0
            let! _ = awaitTask (task {
                while result < 10000 do
                    result <- result + 1
            })
            equal 10000 result
        }

    testCaseAsync "Task for binding works correctly" <| fun () ->
        async {
            let inputs = [| 1; 2; 3 |]
            let mutable result = 0
            let! _ = awaitTask (task {
                for inp in inputs do
                    result <- result + inp
            })
            equal 6 result
        }

    testCaseAsync "Task exceptions are handled correctly" <| fun () ->
        async {
            let mutable result = 0
            let run shouldThrow = task {
                try
                    if shouldThrow then failwith "boom!"
                    else result <- 12
                with _ -> result <- 10
            }
            let! _ = awaitTask (run true)
            let r1 = result
            let! _ = awaitTask (run false)
            let r2 = result
            equal 22 (r1 + r2)
        }

    testCaseAsync "Simple task is executed correctly" <| fun () ->
        async {
            let mutable result = false
            let x = task { return 99 }
            let! r = awaitTask (task {
                let! x = x
                let y = 99
                result <- x = y
            })
            equal true result
        }

    testCaseAsync "task use statements should dispose of resources when they go out of scope" <| fun () ->
        async {
            let mutable isDisposed = false
            let mutable step1ok = false
            let mutable step2ok = false
            let resource = task { return new DisposableAction(fun () -> isDisposed <- true) }
            let! _ = awaitTask (task {
                use! r = resource
                step1ok <- not isDisposed
                step2ok <- not isDisposed
            })
            equal true step1ok
            equal true step2ok
            equal true isDisposed
        }

    testCaseAsync "task let bang works" <| fun () ->
        async {
            let! result = awaitTask (task {
                let! x = task { return 10 }
                let! y = task { return 20 }
                return x + y
            })
            equal 30 result
        }

    testCaseAsync "task do bang works" <| fun () ->
        async {
            let mutable x = 0
            let! result = awaitTask (task {
                do! task { x <- 42 }
                return x
            })
            equal 42 result
        }

    testCaseAsync "task return from works" <| fun () ->
        async {
            let inner = task { return "hello" }
            let! result = awaitTask (task { return! inner })
            equal "hello" result
        }

    testCaseAsync "task try-with works" <| fun () ->
        async {
            let mutable result = 0
            let! _ = awaitTask (task {
                try failwith "boom"
                with _e -> result <- 42
            })
            equal 42 result
        }

    testCaseAsync "task sequential composition works" <| fun () ->
        async {
            let mutable count = 0
            let! result = awaitTask (task {
                let! a = task { count <- count + 1; return count }
                let! b = task { count <- count + 1; return count }
                return a + b
            })
            equal 3 result
        }

    testCaseAsync "Task exceptions propagate through bind" <| fun () ->
        async {
            let mutable result = ""
            let! _ = awaitTask (task {
                try
                    let! _ = task { failwith "inner"; return 0 }
                    result <- "no exception"
                with ex ->
                    result <- ex.Message
            })
            equal "inner" result
        }

    testCaseAsync "TaskCompletionSource works" <| fun () ->
        async {
            let tcs = TaskCompletionSource<int>()
            tcs.SetResult(42)
            let! result = awaitTask tcs.Task
            equal 42 result
        }

    testCaseAsync "TaskCompletionSource.SetCanceled works" <| fun () ->
        async {
            let tcs = TaskCompletionSource<int>()
            tcs.SetCanceled()
            let! result = awaitTask (task {
                try
                    let! _ = tcs.Task
                    return "completed"
                with :? OperationCanceledException -> return "cancelled"
            })
            equal "cancelled" result
        }

    testCaseAsync "TaskCompletionSource.TrySet methods return whether they completed the task" <| fun () ->
        async {
            let tcs = TaskCompletionSource<int>()
            equal true (tcs.TrySetResult 1)
            equal false (tcs.TrySetResult 2)
            equal false (tcs.TrySetException(Exception "late"))
            equal false (tcs.TrySetCanceled())
            let! result = awaitTask tcs.Task
            equal 1 result
            let failed = TaskCompletionSource<int>()
            equal true (failed.TrySetException(Exception "bad"))
            equal false (failed.TrySetResult 3)
            let cancelled = TaskCompletionSource<int>()
            equal true (cancelled.TrySetCanceled())
            equal false (cancelled.TrySetCanceled())
        }

    testCase "TaskCompletionSource raises InvalidOperationException when completed twice" <| fun () ->
        let message = "An attempt was made to transition a task to a final state when it had already completed."
        let tcs = TaskCompletionSource<int>()
        tcs.SetResult 1
        raisedExceptionType (fun () -> tcs.SetResult 2) |> equal "InvalidOperationException"
        throwsError message (fun () -> tcs.SetException(Exception "x"))
        throwsError message (fun () -> tcs.SetCanceled())
        let tried = TaskCompletionSource<int>()
        tried.TrySetCanceled() |> ignore
        throwsError message (fun () -> tried.SetResult 1)

    testCaseAsync "TaskCompletionSource.SetException with a sequence faults with the first exception" <| fun () ->
        async {
            let tcs = TaskCompletionSource<int>()
            tcs.SetException([ Exception "a"; Exception "b" ])
            equal false (tcs.TrySetException([ Exception "c" ]))
            let! result = awaitTask (task {
                try
                    let! _ = tcs.Task
                    return "completed"
                with e -> return e.Message
            })
            equal "a" result
            let empty = TaskCompletionSource<int>()
            raisedExceptionType (fun () -> empty.SetException(List.empty<exn>)) |> equal "ArgumentException"
            raisedExceptionType (fun () -> empty.TrySetException([ Exception "a"; null ])) |> equal "ArgumentException"
            equal true (empty.TrySetResult 1)
        }

    testCaseAsync "Non-generic TaskCompletionSource works" <| fun () ->
        async {
            let tcs = TaskCompletionSource()
            equal true (tcs.TrySetResult())
            equal false (tcs.TrySetResult())
            do! Async.AwaitTask tcs.Task
            let cancelled = TaskCompletionSource()
            cancelled.SetCanceled()
            let! result = awaitTask (task {
                try
                    do! cancelled.Task
                    return "completed"
                with :? OperationCanceledException -> return "cancelled"
            })
            equal "cancelled" result
        }

    testCaseAsync "Task.Run works" <| fun () ->
        async {
            let! a = awaitTask (Task.Run(fun () -> 41 + 1))
            equal 42 a
            let! b = awaitTask (Task.Run<int>(Func<Task<int>>(fun () -> task { return 7 })))
            equal 7 b
            let mutable ran = 0
            do! Async.AwaitTask(Task.Run(fun () -> ran <- ran + 1))
            do! Async.AwaitTask(Task.Run(fun () -> (task { ran <- ran + 1 } : Task)))
            equal 2 ran
            let cts = new System.Threading.CancellationTokenSource()
            let! c = awaitTask (Task.Run((fun () -> 5), cts.Token))
            equal 5 c
        }

    testCaseAsync "Task.Run with a cancelled token gives a cancelled task" <| fun () ->
        async {
            let cts = new System.Threading.CancellationTokenSource()
            cts.Cancel()
            let mutable ran = false
            let t = Task.Run((fun () -> ran <- true; 1), cts.Token)
            let! result = awaitTask (task {
                try
                    let! _ = t
                    return "completed"
                with :? OperationCanceledException -> return "cancelled"
            })
            equal "cancelled" result
            equal false ran
        }

    testCaseAsync "Task.Run faults with the exception of the function" <| fun () ->
        async {
            let t = Task.Run(fun () -> failwith "boom"; 1)
            let! result = awaitTask (task {
                try
                    let! _ = t
                    return "completed"
                with e -> return e.Message
            })
            equal "boom" result
        }

    testCaseAsync "Task.Delay works" <| fun () ->
        async {
            let mutable steps = []
            let! _ = awaitTask (task {
                steps <- "before" :: steps
                do! Task.Delay 20
                do! Task.Delay(TimeSpan.FromMilliseconds 10.)
                steps <- "after" :: steps
            })
            equal [ "after"; "before" ] steps
        }

    testCaseAsync "Task.Delay can be cancelled" <| fun () ->
        async {
            let cts = new System.Threading.CancellationTokenSource()
            let delay = Task.Delay(1000, cts.Token)
            cts.Cancel()
            let! cancelled = awaitTask (task {
                try
                    do! delay
                    return false
                with _ -> return true
            })
            equal true cancelled
        }

    testCaseAsync "task can bind an Async" <| fun () ->
        async {
            let work = async {
                do! Async.Sleep 10
                return 41
            }
            let! result = awaitTask (task {
                let! x = work
                return x + 1
            })
            equal 42 result
        }

    testCaseAsync "task can return! an Async" <| fun () ->
        async {
            let work = async {
                do! Async.Sleep 10
                return 42
            }
            let! result = awaitTask (task { return! work })
            equal 42 result
        }

    testCaseAsync "task can do! an Async" <| fun () ->
        async {
            let mutable ran = false
            let work = async {
                do! Async.Sleep 10
                ran <- true
            }
            let! _ = awaitTask (task { do! work })
            equal true ran
        }
  ]
