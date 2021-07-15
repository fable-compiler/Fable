module Fable.Tests.Set

open Util.Testing

[<Fact>]
let ``test set function works`` () =
    let xs = set [1]
    xs |> Set.isEmpty
    |> equal false

[<Fact>]
let ``test Set.isEmpty works`` () =
    let xs = set []
    Set.isEmpty xs |> equal true
    let ys = set [1]
    Set.isEmpty ys |> equal false

[<Fact>]
let ``test Set.IsEmpty works`` () =
    let xs = Set.empty<int>
    xs.IsEmpty |> equal true
    let ys = set [1; 1]
    ys.IsEmpty |> equal false

[<Fact>]
let ``test Set.Count works`` () =
    let xs = Set.empty |> Set.add 1
    xs.Count
    |> equal 1

[<Fact>]
let ``test Seq.isEmpty function works on Set`` () =
    let xs = set [1]
    xs |> Seq.isEmpty
    |> equal false

[<Fact>]
let ``test Set.add works`` () =
    let xs = Set.empty |> Set.add 1
    Set.count xs
    |> equal 1
