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

[<Fact>]
let ``test for-in descending range with -1 step works`` () =
    let mutable result = ""
    for i in 5 .. -1 .. 1 do
        result <- result + string i
    result |> equal "54321"

[<Fact>]
let ``test for-in ascending range with 1 step works`` () =
    let mutable result = 0
    for i in 1 .. 1 .. 5 do
        result <- result + i
    result |> equal 15

[<Fact>]
let ``test for-in const-step range with zero iterations works`` () =
    let mutable count = 0
    for _i in 0 .. -1 .. 5 do
        count <- count + 1
    count |> equal 0

[<Fact>]
let ``test for-in step range other than one still works`` () =
    let mutable result = ""
    for i in 9 .. -2 .. 0 do
        result <- result + string i
    result |> equal "97531"

[<Fact>]
let ``test for-in over a mutable array reassigned in the body iterates the array held at loop start`` () =
    let mutable a = [| 1; 2; 3 |]
    let mutable sum = 0
    for x in a do
        a <- [| 100 |]
        sum <- sum + x
    sum |> equal 6
    let mutable pairs = [| (1, 2); (3, 4) |]
    let mutable total = 0
    for (x, y) in pairs do
        pairs <- [| (100, 100) |]
        total <- total + x + y
    total |> equal 10
