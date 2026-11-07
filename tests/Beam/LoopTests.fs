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
let ``test for-in descending range with -1 step works`` () =
    let mutable result = ""
    for i in 3 .. -1 .. 1 do
        result <- result + string i
    result |> equal "321"

[<Fact>]
let ``test for-in ascending range with 1 step works`` () =
    let mutable sum = 0
    for i in 1 .. 1 .. 5 do
        sum <- sum + i
    sum |> equal 15

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

[<Fact>]
let ``test for-in loop with negative step range works`` () =
    let mutable result = []
    for i in 5 .. -1 .. 1 do
        result <- result @ [i]
    result |> equal [5; 4; 3; 2; 1]

[<Fact>]
let ``test for-in loop with large negative step range works`` () =
    let mutable count = 0
    for _i in 1000 .. -1 .. 0 do
        count <- count + 1
    count |> equal 1001

[<Fact>]
let ``test for-in loop with negative step range works inside async`` () =
    async {
        let mutable count = 0
        for _i in 5 .. -1 .. 1 do
            count <- count + 1
        equal 5 count
    } |> Async.StartImmediate

[<Fact>]
let ``test for bound over a collection is evaluated once`` () =
    let xs = ResizeArray [ 1; 2; 3; 4 ]
    let mutable n = 0
    for _ in 0 .. xs.Count - 1 do
        xs.RemoveAt 0
        n <- n + 1
    equal 4 n
    let ys = ResizeArray [ 1; 2; 3; 4 ]
    let mutable m = 0
    for _ = ys.Count - 1 downto ys.Count - 2 do
        ys.RemoveAt 0
        m <- m + 1
    equal 2 m

[<Fact>]
let ``test for bound with side effects is evaluated once`` () =
    let calls = ref 0
    let f x =
        calls.Value <- calls.Value + 1
        x
    let mutable n = 0
    for _ in 0 .. (let x = f 1 in x + f 1) do
        n <- n + 1
    equal 3 n
    equal 2 calls.Value
    let log = ResizeArray<string>()
    let tick name value =
        log.Add name
        value
    for _ = tick "start" 0 to tick "limit" 1 do
        log.Add "body"
    String.concat "," log |> equal "start,limit,body,body"

[<Fact>]
let ``test for bound reading the length of a mutable array minus one is evaluated each iteration`` () =
    let mutable a = [| 1; 2; 3 |]
    let mutable n = 0
    for _ = 0 to a.Length - 1 do
        a <- [| 1; 2; 3; 4; 5; 6 |]
        n <- n + 1
    equal 6 n

[<Fact>]
let ``test for bound reading the length of a mutable array is evaluated once in other shapes`` () =
    let mutable a = [| 1; 2; 3 |]
    let mutable n = 0
    for _ = 0 to Array.length a - 1 do
        a <- [| 1 |]
        n <- n + 1
    equal 3 n
    a <- [| 1; 2; 3 |]
    let mutable m = 0
    for _ in 0 .. 1 .. a.Length - 1 do
        a <- [| 1 |]
        m <- m + 1
    equal 3 m
    a <- [| 1; 2; 3 |]
    let mutable p = 0
    for _ = (a <- [| 1; 2; 3; 4 |]; 0) to Array.length a - 1 do
        a <- [| 1 |]
        p <- p + 1
    equal 4 p

[<Fact>]
let ``test for in over a mutable array reassigned in the body iterates the array held at loop start`` () =
    let mutable a = [| 1; 2; 3 |]
    let mutable sum = 0
    for x in a do
        a <- [| 100 |]
        sum <- sum + x
    equal 6 sum
    let mutable pairs = [| (1, 2); (3, 4) |]
    let mutable total = 0
    for (x, y) in pairs do
        pairs <- [| (100, 100) |]
        total <- total + x + y
    equal 10 total
