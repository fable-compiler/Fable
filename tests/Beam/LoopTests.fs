module Fable.Tests.Loop

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test for loop counting up works`` () =
    let mutable sum = 0
    for i = 1 to 5 do
        sum <- sum + i
    sum |> equal 15

[<Fact>]
let ``test for loop counting down works`` () =
    let mutable result = ""
    for i = 3 downto 1 do
        result <- result + string i
    result |> equal "321"

[<Fact>]
let ``test for loop with zero iterations works`` () =
    let mutable count = 0
    for _i = 1 to 0 do
        count <- count + 1
    count |> equal 0

[<Fact>]
let ``test for loop single iteration works`` () =
    let mutable value = 0
    for i = 5 to 5 do
        value <- i
    value |> equal 5

[<Fact>]
let ``test while loop works`` () =
    let mutable i = 0
    let mutable sum = 0
    while i < 5 do
        i <- i + 1
        sum <- sum + i
    sum |> equal 15

[<Fact>]
let ``test while loop with false guard does not execute`` () =
    let mutable executed = false
    while false do
        executed <- true
    executed |> equal false

[<Fact>]
let ``test nested for loops work`` () =
    let mutable sum = 0
    for i = 1 to 3 do
        for j = 1 to 3 do
            sum <- sum + (i * j)
    sum |> equal 36

[<Fact>]
let ``test for loop upto minus one works`` () =
    let mutable result = 0
    for i = 0 to 10 - 1 do
        result <- result + i
    result |> equal 45

[<Fact>]
let ``test for-in loop over list works`` () =
    let mutable sum = 0
    for x in [1; 2; 3; 4; 5] do
        sum <- sum + x
    sum |> equal 15

[<Fact>]
let ``test for-in loop over array works`` () =
    let mutable sum = 0
    for x in [| 10; 20; 30 |] do
        sum <- sum + x
    sum |> equal 60

[<Fact>]
let ``test for-in loop over string works`` () =
    let mutable count = 0
    for _c in "hello" do
        count <- count + 1
    count |> equal 5

[<Fact>]
let ``test while loop with counter works`` () =
    let mutable x = 10
    let mutable count = 0
    while x > 0 do
        x <- x - 3
        count <- count + 1
    count |> equal 4
