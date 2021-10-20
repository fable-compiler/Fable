module Fable.Tests.ListTests

open Util.Testing

[<Fact>]
let ``List.Empty works`` () =
    // Using mutable to suppress inlining which is not supported yet,
    // as it prevents the generic type being instantiated, and breaks.
    // WRONG: let actual: bool = List::isEmpty(&List::empty())
    // RIGHT: let actual: bool = List::isEmpty(&List::empty::<i32>()) // somehow
    let mutable xs: int list = []
    List.isEmpty xs |> equal true
    let mutable xs = List.empty<int>
    List.isEmpty xs |> equal true

[<Fact>]
let ``List.Cons works`` () =
    let xs = 1::2::3::[]
    List.head xs |> equal 1
    let xs = List.tail xs
    List.head xs |> equal 2
    let xs = List.tail xs
    List.head xs |> equal 3
    let xs = List.tail xs
    List.isEmpty xs |> equal true

[<Fact>]
let ``List.length works`` () =
    let xs = 1::2::3::[]
    List.length xs |> equal 3

[<Fact>]
let ``List.last works`` () =
    let xs = 1::2::3::[]
    List.last xs |> equal 3

[<Fact>]
let ``List.tryLast works`` () =
    let xs = 1::2::3::[]
    List.tryLast xs |> equal (Some(3))

[<Fact>]
let ``List.toArray works`` () =
    let xs = 1::2::3::[]
    List.toArray xs |> equal [|1;2;3|]
