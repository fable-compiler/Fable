module Fable.Tests.TaskTests

open Util.Testing
open System.Threading.Tasks

type DisposableAction(f) =
    interface System.IDisposable with
        member _.Dispose() = f ()

let runSynchronously<'T> (tsk: Task<'T>) : 'T =
    //tsk.GetAwaiter().GetResult()
    tsk.Result

[<Fact>]
let ``Simple task translates without exception`` () =
    let tsk = task { return () }
    tsk |> runSynchronously

[<Fact>]
let ``Simple task result translates without exception`` () =
    let tsk = task { return () }
    tsk.Result

[<Fact>]
let ``Simple Task.FromResult translates without exception`` () =
    let tsk = Task.FromResult 42
    let result = tsk.Result
    result |> equal 42

[<Fact>]
let ``task while binding works correctly`` () =
    let mutable result = 0
    let tsk =
        task {
            while result < 10 do
                result <- result + 1
        }
    tsk |> runSynchronously
    result |> equal 10

[<Fact>]
let ``Task for binding works correctly`` () =
    let inputs = [| 1; 2; 3 |]
    let mutable result = 0
    let tsk =
        task {
            for inp in inputs do
                result <- result + inp
        }
    tsk |> runSynchronously
    result |> equal 6

[<Fact>]
let ``Task exceptions are handled correctly`` () =
    let mutable result = 0
    let f shouldThrow =
        let tsk =
            task {
                try
                    if shouldThrow then
                        failwith "boom!"
                    else
                        result <- 12
                with
                | _ -> result <- 10
            }
        tsk |> runSynchronously
        result
    f true + f false |> equal 22

[<Fact>]
let ``Simple task is executed correctly`` () =
    let mutable result = false
    let x = task { return 99 }
    task {
        let! x = x
        let y = 99
        result <- x = y
    }
    |> runSynchronously
    result |> equal true

[<Fact>]
let ``task use statements should dispose of resources when they go out of scope`` () =
    let mutable isDisposed = false
    let mutable step1ok = false
    let mutable step2ok = false

    let resource =
        task { return new DisposableAction(fun () -> isDisposed <- true) }

    task {
        use! r = resource
        step1ok <- not isDisposed
    }
    |> runSynchronously

    step2ok <- isDisposed
    (step1ok && step2ok) |> equal true

[<Fact>]
let ``Try ... with ... expressions inside async expressions work the same`` () =
    let mutable result = ""
    let throw () : unit = raise (exn "Boo!")
    let append (x) = result <- result + x

    let innerAsync () =
        task {
            append "b"
            try
                append "c"
                throw ()
                append "1"
            with
            | _ -> append "d"
            append "e"
        }

    task {
        append "a"
        try
            do! innerAsync ()
        with
        | _ -> append "2"
        append "f"
    }
    |> runSynchronously

    result |> equal "abcdef"

[<Fact>]
let ``TaskCompletionSource is executed correctly`` () =
    let x =
        task {
            let tcs = TaskCompletionSource<int>()
            tcs.SetResult 42
            return! tcs.Task
        }
    let result =
        x |> runSynchronously
    result |> equal 42
