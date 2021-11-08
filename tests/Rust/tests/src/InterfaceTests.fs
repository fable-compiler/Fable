module Fable.Tests.InterfaceTests

open Util.Testing

//PROBLEM - Interfaces are not on the AST - this is erased
type IHasAdd =
    abstract Add: x: int -> y: int -> int

type WithInterface(m: int) =
    interface IHasAdd with
      member this.Add x y = x + y + m

[<Fact>]
let ``Class interface impl works trivial`` () =
    let a = WithInterface(1)
    let aCasted = (a :> IHasAdd)
    let res = aCasted.Add 2 1
    res |> equal 4

let doAddWithInterface (i: IHasAdd) =
    i.Add 3 4

[<Fact>]
let ``Class interface with callout works`` () =
    let a = WithInterface(1)
    let aCasted = (a :> IHasAdd)
    let res = doAddWithInterface aCasted
    let res2 = doAddWithInterface a
    res |> equal 8
    res2 |> equal 8

type WithInterface2 (m: int) = //todo parameterless constructors fail catastrophically - TODO_EXPR_ObjectExpr ([], Any, None)
    interface IHasAdd with
      member this.Add x y = x + y - m

[<Fact>]
let ``Second class implementing same interface also works`` () =
    let a = WithInterface2(1)
    let res = doAddWithInterface a
    res |> equal 6

type ISomeContainer<'a> =
    abstract SomeItem: int -> 'a
    abstract OnlyItem: unit -> 'a

type WithSomeComtainer<'a> (m: 'a) = //todo parameterless constructors fail catastrophically - TODO_EXPR_ObjectExpr ([], Any, None)
    interface ISomeContainer<'a> with
        member this.SomeItem x = m
        member this.OnlyItem () = m

[<Fact>]
let ``Generic container works`` () =
    let a = WithSomeComtainer(1)
    let res = (a :> ISomeContainer<_>).SomeItem 1
    let res2 = (a :> ISomeContainer<_>).OnlyItem()
    res |> equal 1
    res2 |> equal 1