module Fable.Tests.Loops

open Util.Testing


[<Fact>]
let ``test For-loop upto works`` () =
    let mutable result = 0

    for i = 0 to 10 do
        result <- result + i
    done

    result
    |> equal 55

[<Fact>]
let ``test For-loop upto minus one works`` () =
    let mutable result = 0

    for i = 0 to 10 - 1 do
        result <- result + i
    done

    result
    |> equal 45

[<Fact>]
let ``test For-loop downto works`` () =
    let mutable result = 0
    for i = 10 downto 0 do
        result <- result + i

    result
    |> equal 55
