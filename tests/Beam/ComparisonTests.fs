module Fable.Tests.Comparison

open System
open Fable.Tests.Util
open Util.Testing

type UTest = A of int | B of int
type RTest = { a: int; b: int }

[<Fact>]
let ``test Typed array equality works`` () =
    let xs1 = [| 1; 2; 3 |]
    let xs2 = [| 1; 2; 3 |]
    let xs3 = [| 1; 2; 4 |]
    let xs4 = [| 1; 2 |]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 <> xs4)

[<Fact>]
let ``test Typed array option equality works`` () =
    let xs1 = Some [| 1; 2; 3 |]
    let xs2 = Some [| 1; 2; 3 |]
    let xs3 = Some [| 1; 2; 4 |]
    let xs4 = Some [| 1; 2 |]
    let xs5 = None
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal false (xs1 = xs4)
    equal true (xs1 <> xs4)
    equal false (xs1 = xs5)
    equal true (xs1 <> xs5)
    equal true (xs5 = None)
    equal false (xs5 <> None)

[<Fact>]
let ``test Array equality works`` () =
    let xs1 = [| "1"; "2"; "3" |]
    let xs2 = [| "1"; "2"; "3" |]
    let xs3 = [| "1"; "2"; "4" |]
    let xs4 = [| "1"; "2" |]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 <> xs4)

[<Fact>]
let ``test Tuple equality works`` () =
    let xs1 = ( 1, 2, 3 )
    let xs2 = ( 1, 2, 3 )
    let xs3 = ( 1, 2, 4 )
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

[<Fact>]
let ``test Tuple option equality works`` () =
    let xs1 = Some ( 1, 2, 3 )
    let xs2 = Some ( 1, 2, 3 )
    let xs3 = Some ( 1, 2, 4 )
    let xs5 = None
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal false (xs1 = xs5)
    equal true (xs1 <> xs5)
    equal true (xs5 = None)
    equal false (xs5 <> None)

[<Fact>]
let ``test List equality works`` () =
    let xs1 = [ 1; 2; 3 ]
    let xs2 = [ 1; 2; 3 ]
    let xs3 = [ 1; 2; 4 ]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

[<Fact>]
let ``test List option equality works`` () =
    let xs1 = Some [ 1; 2; 3 ]
    let xs2 = Some [ 1; 2; 3 ]
    let xs3 = Some [ 1; 2; 4 ]
    let xs4 = Some [ 1; 2; 3; 1 ]
    let xs5 = None
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal false (xs1 = xs4)
    equal true (xs1 <> xs4)
    equal false (xs1 = xs5)
    equal true (xs1 <> xs5)
    equal true (xs5 = None)
    equal false (xs5 <> None)

[<Fact>]
let ``test Map equality works`` () =
    let xs1 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs2 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs3 = Map [ ("a", 1); ("b", 2); ("c", 4) ]
    let xs4 = Map [ ("c", 3); ("b", 2); ("a", 1) ]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 = xs4)

[<Fact>]
let ``test Union equality works`` () =
    let u1 = A 2
    let u2 = A 2
    let u3 = A 4
    let u4 = B 2
    equal true (u1 = u2)
    equal false (u1 = u3)
    equal true (u1 <> u3)
    equal false (u1 <> u2)
    equal false (u1 = u4)

[<Fact>]
let ``test Record equality works`` () =
    let r1 = { a = 1; b = 2 }
    let r2 = { a = 1; b = 2 }
    let r3 = { a = 1; b = 4 }
    equal true (r1 = r2)
    equal false (r1 = r3)
    equal true (r1 <> r3)
    equal false (r1 <> r2)

[<Fact>]
let ``test Typed array comparison works`` () =
    let xs1 = [| 1; 2; 3 |]
    let xs2 = [| 1; 2; 3 |]
    let xs3 = [| 1; 2; 4 |]
    let xs4 = [| 1; 2; 2 |]
    let xs5 = [| 1; 2 |]
    let xs6 = [| 1; 2; 3; 1 |]
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)
    equal 1 (compare xs1 xs5)
    equal true (xs1 > xs5)
    equal -1 (compare xs1 xs6)
    equal false (xs1 > xs6)

[<Fact>]
let ``test Tuple comparison works`` () =
    let xs1 = ( 1, 2, 3 )
    let xs2 = ( 1, 2, 3 )
    let xs3 = ( 1, 2, 4 )
    let xs4 = ( 1, 2, 2 )
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)

[<Fact>]
let ``test List comparison works`` () =
    let xs1 = [ 1; 2; 3 ]
    let xs2 = [ 1; 2; 3 ]
    let xs3 = [ 1; 2; 4 ]
    let xs4 = [ 1; 2; 2 ]
    let xs5 = [ 1; 2 ]
    let xs6 = [ 1; 2; 3; 1 ]
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)
    equal 1 (compare xs1 xs5)
    equal true (xs1 > xs5)
    equal -1 (compare xs1 xs6)
    equal false (xs1 > xs6)

[<Fact>]
let ``test Union comparison works`` () =
    let u1 = A 2
    let u2 = A 2
    let u3 = A 4
    let u4 = A 1
    let u5 = B 2
    equal 0 (compare u1 u2)
    equal -1 (compare u1 u3)
    equal true (u1 < u3)
    equal 1 (compare u1 u4)
    equal false (u1 < u4)
    (compare u1 u5) = 0 |> equal false

[<Fact>]
let ``test Record comparison works`` () =
    let r1 = { a = 1; b = 2 }
    let r2 = { a = 1; b = 2 }
    let r3 = { a = 1; b = 4 }
    equal 0 (compare r1 r2)
    (compare r1 r3) = 0 |> equal false

[<Fact>]
let ``test compare returns minus 1 zero or 1`` () =
    equal 0 (compare 1 1)
    equal -1 (compare 1 2)
    equal 1 (compare 2 1)

[<Fact>]
let ``test max works with primitives`` () =
    max 1 2 |> equal 2
    Math.Max(1, 2) |> equal 2
    max "a" "b" |> equal "b"

[<Fact>]
let ``test max works with records`` () =
    let r1 = {a=1; b=1}
    let r2 = {a=1; b=2}
    max r1 r2 |> equal r2

[<Fact>]
let ``test min works with primitives`` () =
    min 1 2 |> equal 1
    Math.Min(1, 2) |> equal 1
    min "a" "b" |> equal "a"

[<Fact>]
let ``test min works with records`` () =
    let r1 = {a=1; b=1}
    let r2 = {a=1; b=2}
    min r1 r2 |> equal r1

[<Fact>]
let ``test nullArg works`` () =
    try
        nullArg "test"
        true
    with _ex ->
        false
    |> equal false

[<Fact>]
let ``test isNull with primitives works`` () =
    isNull "" |> equal false
    isNull "0" |> equal false
    isNull "hello" |> equal false

[<Fact>]
let ``test hash with primitives works`` () =
    let h1 = hash 1
    let h2 = hash 1
    let h3 = hash 2
    equal h1 h2
    notEqual h1 h3

[<Fact>]
let ``test hash with strings works`` () =
    let h1 = hash "hello"
    let h2 = hash "hello"
    let h3 = hash "world"
    equal h1 h2
    notEqual h1 h3

[<Fact>]
let ``test GetHashCode with primitives works`` () =
    ((1).GetHashCode(), (1).GetHashCode()) ||> equal
    ((2).GetHashCode(), (1).GetHashCode()) ||> notEqual
    ("1".GetHashCode(), "1".GetHashCode()) ||> equal
    ("2".GetHashCode(), "1".GetHashCode()) ||> notEqual

[<Fact>]
let ``test Equals with primitives works`` () =
    (1).Equals(1) |> equal true
    (1).Equals(2) |> equal false
    (1L).Equals(1L) |> equal true
    (1.).Equals(1.) |> equal true
    ("1").Equals("1") |> equal true
    ("1").Equals("2") |> equal false
    (true).Equals(true) |> equal true
    (true).Equals(false) |> equal false

[<Fact>]
let ``test CompareTo with primitives works`` () =
    (1).CompareTo(1) |> equal 0
    (1).CompareTo(2) |> equal -1
    (2).CompareTo(1) |> equal 1
    (1L).CompareTo(1L) |> equal 0
    (1.).CompareTo(1.) |> equal 0
    ("1").CompareTo("1") |> equal 0
    ("a").CompareTo("b") |> equal -1

[<Fact>]
let ``test Object ReferenceEquals with same ref works`` () =
    let a = "hello"
    Object.ReferenceEquals(a, a) |> equal true

[<Fact>]
let ``test PhysicalEquality with same ref works`` () =
    let a = [1; 2; 3]
    LanguagePrimitives.PhysicalEquality a a |> equal true

[<Fact>]
let ``test generic less than works`` () =
    1 < 2 |> equal true
    2 < 1 |> equal false
    "a" < "b" |> equal true

[<Fact>]
let ``test generic greater than works`` () =
    2 > 1 |> equal true
    1 > 2 |> equal false
    "b" > "a" |> equal true

[<Fact>]
let ``test generic less or equal works`` () =
    1 <= 2 |> equal true
    1 <= 1 |> equal true
    2 <= 1 |> equal false

[<Fact>]
let ``test generic greater or equal works`` () =
    2 >= 1 |> equal true
    1 >= 1 |> equal true
    1 >= 2 |> equal false

[<Fact>]
let ``test Map comparison works`` () =
    let xs1 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs2 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs3 = Map [ ("a", 1); ("b", 2); ("c", 4) ]
    let xs4 = Map [ ("a", 1); ("b", 2); ("c", 2) ]
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)
