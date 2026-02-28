module Fable.Tests.ClassTests

open Fable.Tests.Util
open Util.Testing
open Fable.Tests.Imports

type NTest(x: int, y: int) =
    let a = x + y
    let one = x - x + 1
    let mutable b = one
    member this.A = a
    member this.B = b
    member this.Add m = a + m
    member this.Minus m = a - m
    member this.Mul (c, d) = a * c * d

type STest(s: string) =
    member this.Append x = s + x

type SWrapper(a: STest, b: STest, c: NTest) =
    member this.AddStrings x =
        a.Append (b.Append x)
    member this.AddNum x =
        c.Add x

type FluentA(x: int) =
    let mutable a = x
    member this.X = a
    member this.Add1 () =
        a <- a + 1
        this

type FluentB(a: FluentA) =
    member this.DoFluentAndReturnSelf (x: int) =
        a.Add1() |> ignore
        this
    member this.DoFluentAndReturnInner (x: int) =
        a

type WithCrossModuleInterface(m: int) =
    interface IHasAdd with
      member this.Add x y = x + y + m

type AdderWrapper<'a when 'a :> IHasAdd> (adder: 'a) =
    member this.AddThroughCaptured x y = adder.Add x y

[<Fact>]
let ``test Class methods and props work`` () =
    let a = NTest(1, 2)
    let b = NTest(3, 4)
    let r = a.Add(1)
    r |> equal 4
    a.Add(2) |> equal 5
    b.Add(2) |> equal 9
    a.Minus(3) |> equal 0
    b.Mul(3, 2) |> equal 42
    a.A |> equal 3
    b.A |> equal 7
    a.B |> equal 1

// TODO: Class reference equality not yet working in Beam.
// fable_comparison:equals does structural comparison on process dict maps,
// but F# classes use reference equality by default.
// [<Fact>]
// let ``test Class reference equality works`` () =
//     let a = NTest(1, 2)
//     let b = NTest(1, 2)
//     (a = a) |> equal true
//     (a = b) |> equal false

[<Fact>]
let ``test Class methods and props work II`` () =
    let a = STest("hello")
    let res = a.Append " world"
    res |> equal "hello world"

[<Fact>]
let ``test Class taking other classes as params should work`` () =
    let a = SWrapper(STest("a"), STest("b"), NTest(1, 2))
    a.AddStrings "c" |> equal "abc"
    a.AddNum 2 |> equal 5

[<Fact>]
let ``test Class fluent builder internal clone pattern should work`` () =
    let a = FluentA(42)
    let b = FluentB(a)
    let res = b.DoFluentAndReturnSelf(1).DoFluentAndReturnSelf(2).DoFluentAndReturnInner(3).X
    res |> equal 44
    a.X |> equal 44

[<Fact>]
let ``test Class fluent builder should be sharing same reference`` () =
    let a = FluentA(1)
    let b = a.Add1().Add1()
    a.X |> equal 3
    b.X |> equal 3

[<Fact>]
let ``test Class interface from another module works`` () =
    let a = WithCrossModuleInterface(1)
    let res = (a :> IHasAdd).Add 2 1
    res |> equal 4

[<Fact>]
let ``test Class generic interface constraints work`` () =
    let a = WithCrossModuleInterface(1)
    let w = AdderWrapper(a)
    let res = w.AddThroughCaptured 2 5
    res |> equal 8

[<Fact>]
let ``test Class methods imported from another file work`` () =
    let a = MyClass()
    let res = (a :> IHasAdd).Add 2 3
    res |> equal 5
    a.Sub 2 3 |> equal -1
    MyClass.Mul 2 3 |> equal 6
