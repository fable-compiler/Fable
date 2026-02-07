module Fable.Tests.OptionTest

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test Option.IsSome works`` () =
    let x: int option = Some 42
    x.IsSome |> equal true

[<Fact>]
let ``test Option.IsNone works`` () =
    let x: int option = None
    x.IsNone |> equal true

[<Fact>]
let ``test Option.IsSome with None works`` () =
    let x: int option = None
    x.IsSome |> equal false

[<Fact>]
let ``test Option.IsNone with Some works`` () =
    let x: int option = Some 42
    x.IsNone |> equal false

[<Fact>]
let ``test Option.Value works`` () =
    let x: int option = Some 42
    x.Value |> equal 42

[<Fact>]
let ``test Option.defaultValue works`` () =
    let x: int option = None
    Option.defaultValue 99 x |> equal 99

[<Fact>]
let ``test Option.defaultValue with Some works`` () =
    let x: int option = Some 42
    Option.defaultValue 99 x |> equal 42

[<Fact>]
let ``test Option.map works`` () =
    let x: int option = Some 21
    Option.map (fun v -> v * 2) x |> equal (Some 42)

[<Fact>]
let ``test Option.map with None works`` () =
    let x: int option = None
    Option.map (fun v -> v * 2) x |> equal None

[<Fact>]
let ``test Option.bind works`` () =
    let x: int option = Some 21
    Option.bind (fun v -> Some(v * 2)) x |> equal (Some 42)

[<Fact>]
let ``test Option.bind with None works`` () =
    let x: int option = None
    Option.bind (fun v -> Some(v * 2)) x |> equal None

[<Fact>]
let ``test Option module isSome works`` () =
    Option.isSome (Some 42) |> equal true
    Option.isSome None |> equal false

[<Fact>]
let ``test Option module isNone works`` () =
    Option.isNone None |> equal true
    Option.isNone (Some 42) |> equal false
