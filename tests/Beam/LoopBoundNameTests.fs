module Fable.Tests.LoopBoundName

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test for loop bounds bound in different passes get distinct names`` () =
    let xs = ResizeArray [ 1; 2; 3 ]
    let mutable a = [| 1; 2 |]
    let mutable n = 0
    for _ = 0 to xs.Count - 1 do
        n <- n + 1
    for _ = 0 to xs.Count - 1 do
        n <- n + 1
    for _ = 0 to xs.Count - 1 do
        n <- n + 1
    for _ = 0 to Array.length a - 1 do
        n <- n + 1
    for _ = 0 to Array.length a - 1 do
        n <- n + 1
    for _ = 0 to Array.length a - 1 do
        n <- n + 1
    for _ = 0 to Array.length a - 1 do
        n <- n + 1
    for _ = 0 to Array.length a - 1 do
        n <- n + 1
    n |> equal 19
