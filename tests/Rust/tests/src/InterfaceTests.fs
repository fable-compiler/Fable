module Fable.Tests.InterfaceTests

open Util.Testing
open Common.Interfaces

type Adder(m: int) =
    interface IHasAdd with
      member this.Add x y = x + y + m

[<Fact>]
let ``Class interface impl works trivial`` () =
    let a = Adder(1)
    let aCasted = (a :> IHasAdd)
    let res = aCasted.Add 2 1
    res |> equal 4

let addWithAdder (i: IHasAdd) =
    i.Add 3 4

[<Fact>]
let ``Class interface with callout works`` () =
    let a = Adder(1)
    let aCasted = (a :> IHasAdd)
    let res = addWithAdder aCasted
    let res2 = addWithAdder a
    res |> equal 8
    res2 |> equal 8

type Adder2 (m: int) = //todo parameterless constructors fail catastrophically - TODO_EXPR_ObjectExpr ([], Any, None)
    interface IHasAdd with
      member this.Add x y = x + y - m

[<Fact>]
let ``Second class implementing same interface also works`` () =
    let a = Adder2(1)
    let res = addWithAdder a
    res |> equal 6

type ISomeContainer<'a> =
    abstract SomeItem: int -> 'a
    abstract OnlyItem: unit -> 'a

type SomeContainer<'a> (m: 'a) = //todo parameterless constructors fail catastrophically - TODO_EXPR_ObjectExpr ([], Any, None)
    interface ISomeContainer<'a> with
        member this.SomeItem x = m
        member this.OnlyItem () = m

[<Fact>]
let ``Generic container works`` () =
    let a = SomeContainer(1)
    let res = (a :> ISomeContainer<_>).SomeItem 1
    let res2 = (a :> ISomeContainer<_>).OnlyItem()
    res |> equal 1
    res2 |> equal 1