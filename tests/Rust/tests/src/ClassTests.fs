module Fable.Tests.ClassTests

open Util.Testing

type NTest(x: int, y: int) =
    let a = x + y
    let one = x - x + 1
    let mutable b = one
    // print "%i %i" x y
    member this.A = a
    member this.B = b
    member this.Add m = a + m
    member this.Minus m = a - m
    member this.Mul (c, d) = a * c * d

type STest(s: string) =
    member this.Append x = s + x

[<Fact>]
let ``Class imm methods and props work for prim ints`` () =
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

// [<Fact>]
// let ``Class reference equality works`` () =
//     let a = NTest(1, 2)
//     let b = NTest(3, 4)
//     (a = a) |> equal true
//     (a = b) |> equal false

[<Fact>]
let ``Class imm methods and props work for strings`` () =
    let a = STest("hello")
    let res = a.Append " world"
    res |> equal "hello world"

// TODO - not yet working - weird NULL and type: Any expressions coming out
type SWrapper(a: STest, b: STest, c: NTest) =
    member this.AddStrings x =
        a.Append (b.Append x)
    member this.AddNum x =
        c.Add x

[<Fact>]
let ``Class taking other classes as params should work`` () =
    let a = SWrapper(STest("a"), STest("b"), NTest(1, 2))
    a.AddStrings "c" |> equal "abc"
    a.AddNum 2 |> equal 5

type FluentA(x: int) =
    member this.X = x

type FluentB(a: FluentA) =
    member this.DoFluentAndReturnSelf (x: int) =
        this
    member this.DoFluentAndReturnInner (x: int) =
        a

[<Fact>]
let ``Class fluent/builder internal clone pattern should work`` () =
    let a = FluentA(42)
    let b = FluentB(a)
    let res = b.DoFluentAndReturnSelf(1).DoFluentAndReturnSelf(2).DoFluentAndReturnInner(3).X
    res |> equal 42

type WithCrossModuleInterface(m: int) =
    interface Common.Interfaces.IHasAdd with
      member this.Add x y = x + y + m

[<Fact>]
let ``Class interface from another module works`` () =
    let a = WithCrossModuleInterface(1)
    let res = (a :> Common.Interfaces.IHasAdd).Add 2 1
    res |> equal 4
    //let res2 = Fable.Tests.InterfaceTests.doAddWithInterface(a) // todo: this breaks because duplicate interface + module not imported
    //res2 |> equal 8