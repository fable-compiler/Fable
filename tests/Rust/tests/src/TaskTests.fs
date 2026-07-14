// Task support requires the `threaded` feature, as the Task runtime
// (thread pool + futures) in the library is gated behind it.
[<Fable.Core.Rust.OuterAttr("cfg", [|"feature = \"threaded\""|])>]
module Fable.Tests.TaskTests

// Supported so far: return, let!, do!, Task.FromResult and .Result at concrete
// types, plus Async.AwaitTask and Async.StartAsTask. A body need not end in
// `return`; without one it is just `return ()`.
// The tests kept commented out below track what is still missing. They cannot be
// marked with OuterAttr("ignore") instead: Fable rejects these builder members
// outright, so they fail the build rather than compiling into a skipped test.
//
//   - while / for / try-with / try-finally / use!: TaskBuilderBase.While, For,
//     TryWith, TryFinally, Using and Combine are not mapped in the Rust
//     Replacements, and Task_ has no matching runtime combinators. Note the
//     builder body of a loop would also have to be re-runnable, whereas a Task
//     here is hot-started and caches its result.
//
//   - generic helpers over Task<'T> (such as `let run (t: Task<'T>) = t.Result`)
//     need Send + Sync bounds that are not emitted, so use concrete types.
//
//   - Task.Delay is not implemented, and `do! Task.Delay ...` also needs
//     binding of the non-generic Task, which goes through
//     TaskBuilderExtensions.LowPriority.Bind and Task.GetAwaiter. Awaiting a
//     delay works via `let! _ = Async.Sleep ms |> Async.StartAsTask`.
//
//   - GetAwaiter().GetResult() is not supported, so .Result is used to wait.
//
//   - TaskCompletionSource is not implemented.
//
//   - GetAwaiter().GetResult() is not supported, so .Result is used to wait.

open Util.Testing
open System.Threading.Tasks

[<Fact>]
let ``Simple task translates without exception`` () =
    let tsk = task { return () }
    tsk.Result

[<Fact>]
let ``Simple task result translates without exception`` () =
    let tsk = task { return 42 }
    tsk.Result |> equal 42

[<Fact>]
let ``Simple Task.FromResult translates without exception`` () =
    let tsk = Task.FromResult 42
    let result = tsk.Result
    result |> equal 42

[<Fact>]
let ``task let! binding works`` () =
    let inner = task { return 21 }
    let tsk =
        task {
            let! x = inner
            return x * 2
        }
    tsk.Result |> equal 42

[<Fact>]
let ``task do! binding works`` () =
    let inner = task { return () }
    let tsk =
        task {
            do! inner
            return 7
        }
    tsk.Result |> equal 7

[<Fact>]
let ``task with two let! bindings works`` () =
    let a = task { return 20 }
    let b = task { return 22 }
    let tsk =
        task {
            let! x = a
            let! y = b
            return x + y
        }
    tsk.Result |> equal 42

[<Fact>]
let ``task with sequential let! bindings works`` () =
    let tsk =
        task {
            let! x = task { return 1 }
            let! y = task { return 2 }
            let! z = task { return 3 }
            return x + y + z
        }
    tsk.Result |> equal 6

[<Fact>]
let ``Async.AwaitTask works`` () =
    let tsk = task { return 42 }
    let a =
        async {
            let! x = Async.AwaitTask tsk
            return x
        }
    a |> Async.RunSynchronously |> equal 42

[<Fact>]
let ``Async.StartAsTask works`` () =
    let a = async { return 42 }
    let tsk = Async.StartAsTask a
    tsk.Result |> equal 42

[<Fact>]
let ``task awaiting slow work waits for the result`` () =
    let mutable finished = false
    let slow =
        async {
            do! Async.Sleep 300
            finished <- true
            return 42
        }
        |> Async.StartAsTask
    let tsk =
        task {
            let! x = slow
            return (x, finished)
        }
    let (x, wasFinished) = tsk.Result
    x |> equal 42
    wasFinished |> equal true

[<Fact>]
let ``Task.Result blocks until the work completes`` () =
    let mutable counter = 0
    let slow =
        async {
            do! Async.Sleep 300
            counter <- counter + 1
            return counter
        }
        |> Async.StartAsTask
    // the work is queued, not run inline
    counter |> equal 0
    slow.Result |> equal 1
    counter |> equal 1

[<Fact>]
let ``task awaiting a delay inside the body waits for it`` () =
    let mutable finished = false
    let tsk =
        task {
            let! c = Async.Sleep 300 |> Async.StartAsTask
            finished <- true
            return 42
        }
    tsk.Result |> equal 42
    finished |> equal true

[<Fact>]
let ``task without a trailing return works`` () =
    let mutable r = 0
    let tsk =
        task {
            let! x = task { return 42 }
            r <- x
        }
    tsk.Result
    r |> equal 42

[<Fact>]
let ``task with two let! and no trailing return works`` () =
    let mutable r = 0
    let tsk =
        task {
            let! x = task { return 20 }
            let! y = task { return 22 }
            r <- x + y
        }
    tsk.Result
    r |> equal 42

// type DisposableAction(f) =
//     interface System.IDisposable with
//         member _.Dispose() = f ()

// [<Fact>]
// let ``task while binding works correctly`` () =
//     let mutable result = 0
//     let tsk =
//         task {
//             while result < 10 do
//                 result <- result + 1
//         }
//     tsk.Result
//     result |> equal 10

// [<Fact>]
// let ``Task for binding works correctly`` () =
//     let inputs = [| 1; 2; 3 |]
//     let mutable result = 0
//     let tsk =
//         task {
//             for inp in inputs do
//                 result <- result + inp
//         }
//     tsk.Result
//     result |> equal 6

// [<Fact>]
// let ``Task exceptions are handled correctly`` () =
//     let mutable result = 0
//     let f shouldThrow =
//         let tsk =
//             task {
//                 try
//                     if shouldThrow then
//                         failwith "boom!"
//                     else
//                         result <- 12
//                 with
//                 | _ -> result <- 10
//             }
//         tsk.Result
//         result
//     f true + f false |> equal 22

// [<Fact>]
// let ``task use statements should dispose of resources when they go out of scope`` () =
//     let mutable isDisposed = false
//     let mutable step1ok = false
//     let mutable step2ok = false
//     let resource = task { return new DisposableAction(fun () -> isDisposed <- true) }
//     task {
//         use! r = resource
//         step1ok <- not isDisposed
//     }
//     |> fun t -> t.Result
//     step2ok <- isDisposed
//     (step1ok && step2ok) |> equal true

// [<Fact>]
// let ``TaskCompletionSource is executed correctly`` () =
//     let x =
//         task {
//             let tcs = TaskCompletionSource<int>()
//             tcs.SetResult 42
//             return! tcs.Task
//         }
//     x.Result |> equal 42
