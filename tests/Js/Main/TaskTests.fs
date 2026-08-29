module Fable.Tests.Task

open System
open System.Threading.Tasks
open Util.Testing

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
  ]
