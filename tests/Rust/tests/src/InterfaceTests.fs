module Fable.Tests.InterfaceTests

open Util.Testing
open Common.Imports

type Adder() =
    interface IHasAdd with
      member _.Add x y = x + y

type Adder2(m: int) =
    interface IHasAdd with
      member _.Add x y = x + y + m

type Adder3 (m: int) =
    interface IHasAdd with
      member _.Add x y = x + y - m

let addWithAdder (i: IHasAdd) =
    i.Add 3 4

// let adderObj = Adder()
// let adderInt = Adder() :> IHasAdd

// [<Fact>]
// let ``Module let object value works`` () =
//     let a = adderObj :> IHasAdd
//     let res = a.Add 2 1
//     res |> equal 3

// [<Fact>]
// let ``Module let interface value works`` () =
//     let a = adderInt
//     let res = a.Add 2 1
//     res |> equal 3

[<Fact>]
let ``Class interface impl works`` () =
    let a = Adder()
    let aCasted = (a :> IHasAdd)
    let res = aCasted.Add 2 1
    res |> equal 3

[<Fact>]
let ``Class interface impl works II`` () =
    let a = Adder2(1)
    let aCasted = (a :> IHasAdd)
    let res = aCasted.Add 2 1
    res |> equal 4

[<Fact>]
let ``Class interface with callout works`` () =
    let a = Adder2(1)
    let aCasted = (a :> IHasAdd)
    let res = addWithAdder aCasted
    let res2 = addWithAdder a
    res |> equal 8
    res2 |> equal 8

[<Fact>]
let ``Another class implementing same interface also works`` () =
    let a = Adder3(1)
    let res = addWithAdder a
    res |> equal 6

type ISomeContainer<'a> =
    abstract SomeItem: int -> 'a
    abstract OnlyItem: unit -> 'a

type SomeContainer<'a> (m: 'a) =
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

type IConstrained<'a when 'a :> IHasAdd> =
    abstract AddThroughCaptured: int -> int -> int

type AdderWrapper<'a when 'a :> IHasAdd> (adder: 'a) =
    interface IConstrained<'a> with
        member this.AddThroughCaptured x y = adder.Add x y

[<Fact>]
let ``Interface generic interface constraints work`` () =
    let a = Adder3(1)
    let w = AdderWrapper(a)
    let res = (w :> IConstrained<_>).AddThroughCaptured 2 5
    res |> equal 6
