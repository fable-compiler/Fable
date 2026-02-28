module Fable.Tests.MiscTests2

open Fable.Tests.Util
open Util.Testing

// --- Units of measure ---
[<Measure>] type km
[<Measure>] type mi
[<Measure>] type h
[<Measure>] type m
[<Measure>] type s

[<Measure>] type Measure1
[<Measure>] type Measure2 = Measure1

type MeasureTest() =
    member _.Method(x: float<Measure2>) = x

[<Fact>]
let ``test Units of measure division works`` () =
    let a = 4<m>
    let b = 2<s>
    let c = a / b
    c |> equal (2<m/s>)

// --- Uncurrying in union/class fields ---

type TestUnion =
    | UncurryUnion of add: (int -> int -> int)

let applyUncurryUnion x y = function
    | UncurryUnion f -> f x y

type TestClass(add: (int -> int -> int)) =
    member _.Add(x, y) = add x y

[<Fact>]
let ``test Functions in union fields are uncurried`` () =
    let res = UncurryUnion (-) |> applyUncurryUnion 5 2
    res |> equal 3

[<Fact>]
let ``test Functions in class fields are uncurried`` () =
    let adder = TestClass((+))
    let res = adder.Add(2, 3)
    res |> equal 5

// --- Generic names ---

type MyRecord<'a> =
    { Value: 'a }
    static member Stringify v = v

[<Fact>]
let ``test automatically generated generic names don't conflict`` () =
    MyRecord<string>.Stringify 456
    |> equal 456

// --- Classes with explicit fields and base constructors ---

type ExplicitBaseClass =
    val string1 : string
    new (str) = { string1 = str }
    new () = { string1 = "D" }

type ExplicitDerivedClass =
    inherit ExplicitBaseClass

    val string2 : string
    new (str1, str2) = { inherit ExplicitBaseClass(str1); string2 = str2 }
    new (str2) = { inherit ExplicitBaseClass(); string2 = str2 }

[<Fact>]
let ``test Classes with explicit fields can call base constructors`` () =
    let o1 = ExplicitDerivedClass("A", "B")
    let o2 = ExplicitDerivedClass("C")
    o1.string1 |> equal "A"
    o1.string2 |> equal "B"
    o2.string1 |> equal "D"
    o2.string2 |> equal "C"

// --- Additional UoM tests ---

[<Fact>]
let ``test Units of measure addition works`` () =
    3<km/h> + 2<km/h> |> equal 5<km/h>

[<Fact>]
let ``test Units of measure work with longs`` () =
    3L<km/h> + 2L<km/h> |> equal 5L<km/h>

[<Fact>]
let ``test Abbreviated units of measure work`` () =
    let x = 5.<Measure1>
    let c = MeasureTest()
    c.Method(5.<Measure2>) |> equal x

// --- Module bindings ---

module ModuleBindings =
    let modx = 3

open ModuleBindings

// TODO: Mutable module-level bindings don't work in Erlang because Erlang modules
// don't support mutable state at module scope. The `<-` mutation is silently ignored.
// [<Fact>]
// let ``test Module let mutable bindings work`` () =
//     let mutable mody = 4  // (originally a module-level mutable)
//     mody <- mody + 1
//     let z = modx + mody
//     z |> equal 8

[<Fact>]
let ``test Module let bindings work`` () =
    let z = modx + 5
    z |> equal 8
