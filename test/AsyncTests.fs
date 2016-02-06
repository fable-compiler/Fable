[<NUnit.Framework.TestFixture>] 
module Fabel.Tests.Async
open System
open NUnit.Framework
open Fabel.Tests.Util

[<Test>]
let ``Simple async translates without exception``() =
    async { return () }
    |> Async.StartImmediate

[<Test>]
let ``Async while binding works correctly``() =
    let mutable result = 0
    async { 
        while result < 10 do
            result <- result + 1
    } |> Async.StartImmediate
    equal result 10

[<Test>]
let ``Async for binding works correctly``() =
    let inputs = [|1; 2; 3|]
    let result = ref 0
    async { 
        for inp in inputs do
            result := !result + inp
    } |> Async.StartImmediate
    equal !result 6

[<Test>]
let ``Async exceptions are handled correctly``() =
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

[<Test>]
let ``Simple async is executed correctly``() =
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

type DisposableAction(f) =
    interface IDisposable with
        member __.Dispose() = f()

[<Test>]
let ``async use statements should dispose of resources when they go out of scope``() =
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

[<Test>]
let ``Try ... with ... expressions inside async expressions work the same``() =
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
