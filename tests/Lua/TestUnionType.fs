module Fable.Tests.UnionTypes

open Util.Testing

type Shape = Square | Circle

[<Fact>]
let testMakeUnion () =
    let r = Square
    r |> equal Square

[<Fact>]
let testMakeUnion2 () =
    let r = Circle
    r |> equal Circle

type Stuff =
  | A of string * int
  | B
  | C of bool

[<Fact>]
let testMakeUnionContent () =
    let r = A ("abc", 42)
    r |> equal (A ("abc", 42))

[<Fact>]
let testMakeUnionContent2 () =
    let r = C true
    r |> equal (C true)

[<Fact>]
let testMakeUnionContent3 () =
    let r = B
    r |> equal B

[<Fact>]
let testMatch1 () =
    let thing = A("abc", 123)
    let res =
        match thing with
        | A(s, i) -> Some(s, i)
        | B -> None
        | C(_) -> None
    res |> equal (Some("abc", 123))