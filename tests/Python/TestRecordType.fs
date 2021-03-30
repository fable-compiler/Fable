module Fable.Tests.Record

open Util.Testing

let makeAnonRec() =
    {| X = 5; Y = "Foo"; F = fun x y -> x + y |}


[<Fact>]
let ``test Anonymous records work`` () =
    let r = makeAnonRec()
    sprintf "Tell me %s %i times" r.Y (r.F r.X 3)
    |> equal "Tell me Foo 8 times"
    let x = {| Foo = "baz"; Bar = 23 |}
    let y = {| Foo = "baz" |}
    x = {| y with Bar = 23 |} |> equal true
    // x = {| y with Baz = 23 |} |> equal true // Doesn't compile
    x = {| y with Bar = 14 |} |> equal false
