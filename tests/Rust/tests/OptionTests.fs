module Fable.Tests.Option

open Util.Testing

type Point<'T> = { x: 'T; y: 'T }

[<Fact>]
let ``Option Get value works`` () =
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

[<Fact>]
let ``Option with reference type works`` () =
    let p = { Point.x = 1; y = 2 }
    let a = Some p
    let b =
        // a |> Option.map (fun v -> v.x)
        match a with
        | Some p -> Some p.x
        | None -> None
    let c =
        match b with
        | Some n -> n
        | None -> 0
    a |> equal (Some p)
    b |> equal (Some 1)
    c |> equal 1
