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
let ``Option matching with reference type works`` () =
    let p = { Point.x = 1; y = 2 }
    let a = Some p
    let b =
        a |> Option.map (fun p -> p.x)
    let b' =
        match a with
        | Some p -> Some p.x
        | None -> None
    let c =
        match b with
        | Some n -> n
        | None -> 0
    b |> equal b'
    a |> equal (Some p)
    b |> equal (Some 1)
    c |> equal 1

let getValue opt = Option.get opt

[<Fact>]
let ``Option Get with reference type works`` () =
    let o = Some { Point.x = 1; y = 2 }
    let p = Option.get o
    let p2 = getValue o
    p.x |> equal 1
    p2.y |> equal 2
