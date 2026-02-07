module Fable.Tests.TypeTests

open Util.Testing
open Xunit

type Type5Test(greeting: string) =
    member _.Value = greeting
    member _.Overload(x: int) = x + x
    member _.Overload(x: int, y: int) = x + y

type Type6Test(x: int) =
    let mutable i = x
    member _.Value2 = i + i

    member _.Value3
        with get () = i * i
        and set (v) = i <- v

type SecondaryCons(x: int) =
    new() = SecondaryCons(5)
    member _.Value = x

type MultipleCons(x: int, y: int) =
    new() = MultipleCons(2, 3)
    new(x: int) = MultipleCons(x, 4)
    member _.Value = x + y

type ThisContextInConstructor(v) =
    member _.GetValue() = v

[<Fact>]
let ``test class constructor and property getter work`` () =
    let t = Type5Test("hello")
    equal "hello" t.Value

[<Fact>]
let ``test class method overload with one arg works`` () =
    let t = Type5Test("")
    equal 4 (t.Overload(2))

[<Fact>]
let ``test class method overload with two args works`` () =
    let t = Type5Test("")
    equal 5 (t.Overload(2, 3))

[<Fact>]
let ``test class mutable field getter works`` () =
    let t = Type6Test(5)
    equal 10 t.Value2

[<Fact>]
let ``test class mutable field getter and setter work`` () =
    let t = Type6Test(5)
    equal 25 t.Value3
    t.Value3 <- 10
    equal 20 t.Value2
    equal 100 t.Value3

[<Fact>]
let ``test secondary constructor works`` () =
    let s1 = SecondaryCons(3)
    let s2 = SecondaryCons()
    equal 3 s1.Value
    equal 5 s2.Value

[<Fact>]
let ``test multiple constructors work`` () =
    let m1 = MultipleCons()
    let m2 = MultipleCons(5)
    let m3 = MultipleCons(7, 7)
    equal 5 m1.Value
    equal 9 m2.Value
    equal 14 m3.Value

[<Fact>]
let ``test closure in constructor captures value`` () =
    ThisContextInConstructor(7).GetValue() |> equal 7
