module Fable.Tests.Option

open Util.Testing

[<Fact>]
let ``Option value works`` () =
    let x = Some 5
    let y = x.Value
    let z = (Some 6).Value
    x |> equal (Some 5)
    y |> equal 5
    z |> equal 6

[<Fact>]
let ``Option matching works`` () =
    let x = Some 5
    let y =
        match x with
        | Some n -> n
        | None -> 0
    let z =
        match Some 6 with
        | Some n -> n
        | None -> 0
    y |> equal 5
    z |> equal 6
