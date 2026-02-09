module Fable.Tests.TaskTests

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test task return works`` () =
    let tsk = task { return 42 }
    tsk.Result |> equal 42

[<Fact>]
let ``test task let bang works`` () =
    let tsk = task {
        let! x = task { return 10 }
        let! y = task { return 20 }
        return x + y
    }
    tsk.Result |> equal 30

[<Fact>]
let ``test task do bang works`` () =
    let mutable x = 0
    let tsk = task {
        do! task { x <- 42; return () }
        return x
    }
    tsk.Result |> equal 42

[<Fact>]
let ``test task return from works`` () =
    let inner = task { return "hello" }
    let outer = task { return! inner }
    outer.Result |> equal "hello"

[<Fact>]
let ``test task try-with works`` () =
    let mutable result = 0
    let tsk = task {
        try
            failwith "boom"
        with _e ->
            result <- 42
    }
    tsk.Result
    equal 42 result

[<Fact>]
let ``test task while works`` () =
    let mutable result = 0
    let tsk = task {
        while result < 10 do
            result <- result + 1
    }
    tsk.Result
    equal 10 result

[<Fact>]
let ``test task for works`` () =
    let inputs = [| 1; 2; 3 |]
    let mutable result = 0
    let tsk = task {
        for inp in inputs do
            result <- result + inp
    }
    tsk.Result
    equal 6 result

[<Fact>]
let ``test task sequential composition works`` () =
    let mutable count = 0
    let tsk = task {
        let! a = task { count <- count + 1; return count }
        let! b = task { count <- count + 1; return count }
        return a + b
    }
    tsk.Result |> equal 3
