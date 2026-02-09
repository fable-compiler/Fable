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

// Object expression tests

type IGreeter =
    abstract Greet: string -> string

type IAdder =
    abstract Add: int -> int -> int

type IValueHolder =
    abstract Value: int

[<Fact>]
let ``test object expression with single method works`` () =
    let greeter = { new IGreeter with member _.Greet(name) = "Hello, " + name }
    equal "Hello, World" (greeter.Greet("World"))

[<Fact>]
let ``test object expression with curried method works`` () =
    let adder = { new IAdder with member _.Add x y = x + y }
    equal 7 (adder.Add 3 4)

[<Fact>]
let ``test object expression with property getter works`` () =
    let holder = { new IValueHolder with member _.Value = 42 }
    equal 42 holder.Value

[<Fact>]
let ``test object expression passed as function argument works`` () =
    let greet (g: IGreeter) name = g.Greet(name)
    let greeter = { new IGreeter with member _.Greet(name) = "Hi, " + name }
    equal "Hi, Bob" (greet greeter "Bob")

[<Fact>]
let ``test object expression captures closure values`` () =
    let prefix = "Dear"
    let greeter = { new IGreeter with member _.Greet(name) = prefix + " " + name }
    equal "Dear Alice" (greeter.Greet("Alice"))

// Type testing tests

[<Fact>]
let ``test type test string works`` () =
    let test (o: obj) =
        match o with
        | :? string -> true
        | _ -> false
    box "hello" |> test |> equal true
    box 5 |> test |> equal false

[<Fact>]
let ``test type test int works`` () =
    let test (o: obj) =
        match o with
        | :? int -> true
        | _ -> false
    box 5 |> test |> equal true
    box "hello" |> test |> equal false

[<Fact>]
let ``test type test float works`` () =
    let test (o: obj) =
        match o with
        | :? float -> true
        | _ -> false
    box 3.14 |> test |> equal true
    box 5 |> test |> equal false

[<Fact>]
let ``test type test bool works`` () =
    let test (o: obj) =
        match o with
        | :? bool -> true
        | _ -> false
    box true |> test |> equal true
    box "true" |> test |> equal false

[<Fact>]
let ``test type test with pattern binding works`` () =
    let test (o: obj) =
        match o with
        | :? string as s -> "string:" + s
        | :? int as i -> "int:" + string i
        | _ -> "other"
    box "hello" |> test |> equal "string:hello"
    box 42 |> test |> equal "int:42"

[<Fact>]
let ``test type test multiple types works`` () =
    let classify (o: obj) =
        match o with
        | :? string -> "string"
        | :? int -> "int"
        | :? float -> "float"
        | :? bool -> "bool"
        | _ -> "unknown"
    box "hi" |> classify |> equal "string"
    box 1 |> classify |> equal "int"
    box 2.5 |> classify |> equal "float"
    box false |> classify |> equal "bool"

[<Fact>]
let ``test type test list works`` () =
    let test (o: obj) =
        match o with
        | :? list<int> -> true
        | _ -> false
    box [ 1; 2; 3 ] |> test |> equal true
    box "hello" |> test |> equal false

[<Fact>]
let ``test box and unbox roundtrip works`` () =
    let x = 42
    let boxed: obj = box x
    let unboxed: int = unbox boxed
    equal 42 unboxed

[<Fact>]
let ``test box and unbox string works`` () =
    let s = "hello"
    let boxed: obj = box s
    let unboxed: string = unbox boxed
    equal "hello" unboxed
