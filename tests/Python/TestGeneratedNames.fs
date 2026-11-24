module Fable.Tests.GeneratedNames

open System
open Util.Testing

let mutable counter = 0

let private next () =
    counter <- counter + 1
    counter

[<Fact>]
let ``test generated function names do not shadow user locals`` () =
    let _expr0 = next ()
    let _arrow1 = next ()

    let addOne =
        fun () ->
            counter <- counter + 1
            counter

    addOne () |> ignore

    match Ok "foo" with
    | Ok x -> Some x
    | Error (_: int) -> None
    |> equal (Some "foo")

    _arrow1 - _expr0 |> equal 1

type ObjectExpr4(v: int) =
    member _.Value = v

type ObjectExpr5(v: int) =
    member _.Value = v

let private disposable =
    { new IDisposable with
        member _.Dispose() = counter <- counter + 10
    }

[<Fact>]
let ``test generated object expression names do not replace user types`` () =
    disposable.Dispose()
    ObjectExpr4(5).Value |> equal 5
    ObjectExpr5(6).Value |> equal 6
