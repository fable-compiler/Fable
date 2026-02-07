module Fable.Tests.ArrayTest

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test Array literal works`` () =
    let arr = [| 1; 2; 3 |]
    arr.Length |> equal 3

[<Fact>]
let ``test Array indexing works`` () =
    let arr = [| 10; 20; 30 |]
    arr.[0] |> equal 10
    arr.[1] |> equal 20
    arr.[2] |> equal 30

[<Fact>]
let ``test Array.length works`` () =
    Array.length [| 1; 2; 3; 4; 5 |] |> equal 5

[<Fact>]
let ``test Array.map works`` () =
    let arr = [| 1; 2; 3 |]
    let result = Array.map (fun x -> x * 2) arr
    result.[0] |> equal 2
    result.[1] |> equal 4
    result.[2] |> equal 6

[<Fact>]
let ``test Array.filter works`` () =
    let arr = [| 1; 2; 3; 4; 5 |]
    let result = Array.filter (fun x -> x > 3) arr
    Array.length result |> equal 2

[<Fact>]
let ``test Array.fold works`` () =
    let arr = [| 1; 2; 3; 4; 5 |]
    Array.fold (fun acc x -> acc + x) 0 arr |> equal 15

[<Fact>]
let ``test Array.sum works`` () =
    [| 1; 2; 3; 4; 5 |] |> Array.sum |> equal 15

[<Fact>]
let ``test Array.isEmpty works`` () =
    Array.isEmpty [||] |> equal true
    Array.isEmpty [| 1 |] |> equal false

[<Fact>]
let ``test Array.reverse works`` () =
    let arr = [| 1; 2; 3 |]
    let result = Array.rev arr
    result.[0] |> equal 3
    result.[1] |> equal 2
    result.[2] |> equal 1

[<Fact>]
let ``test Array.append works`` () =
    let result = Array.append [| 1; 2 |] [| 3; 4 |]
    Array.length result |> equal 4
    result.[2] |> equal 3

[<Fact>]
let ``test Array.head works`` () =
    Array.head [| 10; 20; 30 |] |> equal 10

[<Fact>]
let ``test Array.last works`` () =
    Array.last [| 10; 20; 30 |] |> equal 30

[<Fact>]
let ``test Array.exists works`` () =
    Array.exists (fun x -> x > 3) [| 1; 2; 3; 4 |] |> equal true
    Array.exists (fun x -> x > 10) [| 1; 2; 3; 4 |] |> equal false

[<Fact>]
let ``test Array.forall works`` () =
    Array.forall (fun x -> x > 0) [| 1; 2; 3 |] |> equal true
    Array.forall (fun x -> x > 2) [| 1; 2; 3 |] |> equal false

[<Fact>]
let ``test Array.contains works`` () =
    Array.contains 3 [| 1; 2; 3; 4 |] |> equal true
    Array.contains 5 [| 1; 2; 3; 4 |] |> equal false

[<Fact>]
let ``test Array.sort works`` () =
    let arr = [| 3; 1; 4; 1; 5 |]
    let sorted = Array.sort arr
    sorted.[0] |> equal 1
    sorted.[4] |> equal 5

[<Fact>]
let ``test Array.reduce works`` () =
    [| 1; 2; 3; 4; 5 |] |> Array.reduce (+) |> equal 15

[<Fact>]
let ``test Array.collect works`` () =
    let arr = [| 1; 2; 3 |]
    let result = Array.collect (fun x -> [| x; x * 10 |]) arr
    Array.length result |> equal 6
    result.[1] |> equal 10

[<Fact>]
let ``test Empty array works`` () =
    let arr: int array = [||]
    Array.length arr |> equal 0
    Array.isEmpty arr |> equal true
