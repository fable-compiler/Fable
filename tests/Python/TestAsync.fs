module Fable.Tests.Async

open System
open Util.Testing

type DisposableAction(f) =
    interface IDisposable with
        member __.Dispose() = f()

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
    let result = ref 0
    async {
        for inp in inputs do
            result := !result + inp
    } |> Async.StartImmediate
    equal !result 6

[<Fact>]
let ``test Async exceptions are handled correctly`` () =
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

[<Fact>]
let ``test Simple async is executed correctly`` () =
    let result = ref false
    let x = async { return 99 }
    async {
        let! x = x
        let y = 99
        result := x = y
    }
    |> Async.StartImmediate
    equal !result true

[<Fact>]
let ``test async use statements should dispose of resources when they go out of scope`` () =
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
