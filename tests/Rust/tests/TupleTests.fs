module Fable.Tests.Tuple

open Util.Testing


[<Fact>]
let ``Basic tuple works`` () =
    let a = 3,2
    a |> equal (3, 2)

[<Fact>]
let ``Basic tuple destructuring works`` () =
    let (a, b) = 3,2
    a |> equal 3
    b |> equal 2

//todo : this is not being transpiled as a 1 parameter tuple as expected.
//Fable is (rather cleverly) optimizing away the tuple, and turning them into parameters
let destructureAndAdd1 x =
    let (a, b) = x
    (a + 1, b + 1)

[<Fact>]
let ``Basic tuple fn call works`` () =
    let (a, b) = destructureAndAdd1(3, 2)
    a |> equal 4
    b |> equal 3

[<Fact>]
let ``Basic tuple fn multiple calls works`` () =
    let x = (3, 2)
    let y = destructureAndAdd1 x
    let z = x |> destructureAndAdd1 |> destructureAndAdd1
    y |> equal (4, 3)
    z |> equal (5, 4)

[<Fact>]
let ``Basic struct tuple works`` () =
    let a = struct (3, 2)
    a |> equal (struct (3, 2))

[<Fact>]
let ``Big tuple works`` () =
    let a = (3, "hello", 2, 1, 3.141, "world", 42)
    let (_, _, _, _, five, six, _) = a
    five |> equal 3.141
    six |> equal "world"

let tuplePatternMatchFn = function
    | (a, b) when b = "hello" -> a
    | _ -> -1

let ``Pattern matching works`` () =
    let resA = tuplePatternMatchFn (1, "hello")
    let resB = tuplePatternMatchFn (2, "fail")
    resA |> equal 1
    resB |> equal -1