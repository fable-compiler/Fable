module Fable.Tests.ClassTests

open Util.Testing

type NTest(x: int, y: int) =
    let a = x + y
    // print "%i %i" x y
    member this.A = a
    member this.B = 1
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

[<Fact>]
let ``Class comparisons work`` () = //this is inconsistent with .net since structural equality is not the default with classes
    let a = NTest(1, 2)
    let b = NTest(3, 4)
    a |> equal a
    a |> notEqual b

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