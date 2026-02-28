module Fable.Tests.MiscTests2

open Fable.Tests.Util
open Util.Testing

// --- Units of measure ---
[<Measure>] type m
[<Measure>] type s

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
