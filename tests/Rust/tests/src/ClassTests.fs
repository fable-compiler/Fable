module Fable.Tests.ClassTests

open Util.Testing

[<Struct>]
type Point(x: float, y: float) =
    member _.X = x
    member _.Y = y
    static member (+) (a: Point, b: Point) = Point(a.X + b.X, a.Y + b.Y)

[<Struct>]
type SPoint =
    val X: float
    val Y: float
    new (x, y) = { X = x; Y = y }
    static member (+) (a: SPoint, b: SPoint) = SPoint(a.X + b.X, a.Y + b.Y)

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

// TODO - not yet working - weird NULL and type: Any expressions coming out
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
    interface Common.Imports.IHasAdd with
      member this.Add x y = x + y + m

type AdderWrapper<'a when 'a :> Common.Imports.IHasAdd> (adder: 'a) =
    member this.AddThroughCaptured x y = adder.Add x y

type TestClass(name: string) =
    member _.Name = name
    abstract Print: unit -> string
    default _.Print() = "abc"

type IPrintable =
    abstract Print: unit -> string

[<Fact>]
let ``Object expressions work`` () =
    let a = { new IPrintable with member x.Print() = "Hello" }
    a.Print() |> equal "Hello"
    let b = { new Common.Imports.IHasAdd with member _.Add x y = x + y }
    b.Add 2 3 |> equal 5

// [<Fact>]
// let ``Object expressions with type constructors work`` () =
//     let a = { new TestClass("Hello") with member x.Print() = x.Name }
//     a.Print() |> equal "Hello"

[<Fact>]
let ``Struct constructors work`` () =
    let a = Point(1, 2)
    let b = Point(3, 4)
    let c = Point()
    let p = a + b + c
    (p.X, p.Y) |> equal (4., 6.)
    p |> equal (Point(4, 6))

[<Fact>]
let ``Struct constructors work II`` () =
    let a = SPoint(1, 2)
    let b = SPoint(3, 4)
    let c = SPoint()
    let p = a + b + c
    (p.X, p.Y) |> equal (4., 6.)
    p |> equal (SPoint(4, 6))

let addStructxExpl (a: SPoint) (b: SPoint inref) =
    SPoint(a.X + b.X, a.Y + b.Y)

[<Fact>]
let ``Struct functions work`` () =
    let a = SPoint(1, 2)
    let b = SPoint(3, 4)
    let p = addStructxExpl a &b
    (p.X, p.Y) |> equal (4., 6.)
    p |> equal (SPoint(4, 6))

[<Fact>]
let ``Class methods and props work`` () =
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
let ``Class reference equality works`` () =
    let a = NTest(1, 2)
    let b = NTest(1, 2)
    (a = a) |> equal true
    (a = b) |> equal false

[<Fact>]
let ``Class methods and props work II`` () =
    let a = STest("hello")
    let res = a.Append " world"
    res |> equal "hello world"

[<Fact>]
let ``Class taking other classes as params should work`` () =
    let a = SWrapper(STest("a"), STest("b"), NTest(1, 2))
    a.AddStrings "c" |> equal "abc"
    a.AddNum 2 |> equal 5

[<Fact>]
let ``Class fluent/builder internal clone pattern should work`` () =
    let a = FluentA(42)
    let b = FluentB(a)
    let res = b.DoFluentAndReturnSelf(1).DoFluentAndReturnSelf(2).DoFluentAndReturnInner(3).X
    res |> equal 44 // assuming add 1 for each DoFluent
    a.X |> equal 44 // original should match as it is internally mutated

[<Fact>]
let ``Class fluent/builder should be sharing same reference and not cloning when returning this`` () =
    let a = FluentA(1);
    let b = a.Add1().Add1();
    a.X |> equal 3
    b.X |> equal 3

[<Fact>]
let ``Class interface from another module works`` () =
    let a = WithCrossModuleInterface(1)
    let res = (a :> Common.Imports.IHasAdd).Add 2 1
    res |> equal 4

[<Fact>]
let ``Class generic interface constraints work`` () =
    let a = WithCrossModuleInterface(1)
    let w = AdderWrapper(a)
    let res = w.AddThroughCaptured 2 5
    res |> equal 8

[<Fact>]
let ``Class methods imported from another file work`` () =
    let a = Common.Imports.MyClass()
    let res = (a :> Common.Imports.IHasAdd).Add 2 3
    res |> equal 5
    a.Sub 2 3 |> equal -1
    Common.Imports.MyClass.Mul 2 3 |> equal 6

#if FABLE_COMPILER
open Fable.Core

[<Emit("$0 as Lrc<NTest>")>]
let ensureMyUnionWrapped s = nativeOnly

[<Emit("$0 as Point")>]
let ensureIsClassUnionUnwrapped s = nativeOnly

[<Fact>]
let ``Normal class should be wrapped in a Lrc`` () =
    NTest(1, 1) |> ensureMyUnionWrapped |> ignore

[<Fact>]
let ``Struct class should not be wrapped in a Lrc`` () =
    Point(1, 1) |> ensureIsClassUnionUnwrapped |> ignore

#endif
