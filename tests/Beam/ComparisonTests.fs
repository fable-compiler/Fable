module Fable.Tests.Comparison

open System
open Fable.Tests.Util
open Util.Testing

type UTest = A of int | B of int
type RTest = { a: int; b: int }

exception Ex of int

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

[<Fact>]
let ``test Array option equality works`` () =
    let xs1 = Some [| "1"; "2"; "3" |]
    let xs2 = Some [| "1"; "2"; "3" |]
    let xs3 = Some [| "1"; "2"; "4" |]
    let xs4 = Some [| "1"; "2" |]
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
let ``test Map option equality works`` () =
    let xs1 = Some (Map [ ("a", 1); ("b", 2); ("c", 3) ])
    let xs2 = Some (Map [ ("a", 1); ("b", 2); ("c", 3) ])
    let xs3 = Some (Map [ ("a", 1); ("b", 2); ("c", 4) ])
    let xs4 = Some (Map [ ("c", 3); ("b", 2); ("a", 1) ])
    let xs5 = Some (Map [ ("a", 1); ("b", 2); ("c", 3); ("d", 1) ])
    let xs6 = None
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 = xs4)
    equal true (xs1 <> xs5)
    equal false (xs1 = xs6)
    equal true (xs1 <> xs6)
    equal true (xs6 = None)
    equal false (xs6 <> None)

[<Fact>]
let ``test Exception equality works`` () =
    equal true ((Ex 1) = (Ex 1))
    equal false ((Ex 1) = (Ex 2))

[<Fact>]
let ``test Array comparison with strings works`` () =
    let xs1 = [| "1"; "2"; "3" |]
    let xs2 = [| "1"; "2"; "3" |]
    let xs3 = [| "1"; "2"; "4" |]
    let xs4 = [| "1"; "2"; "2" |]
    let xs5 = [| "1"; "2" |]
    let xs6 = [| "1"; "2"; "3"; "1" |]
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
let ``test Equals with more primitive types works`` () =
    ('1').Equals('1') |> equal true
    ('1').Equals('2') |> equal false
    (1y).Equals(1y) |> equal true
    (1uy).Equals(1uy) |> equal true
    (1s).Equals(1s) |> equal true
    (1u).Equals(1u) |> equal true
    (1us).Equals(1us) |> equal true
    (1ul).Equals(1ul) |> equal true
    (1uL).Equals(1uL) |> equal true
    (1.f).Equals(1.f) |> equal true

[<Fact>]
let ``test CompareTo with more primitive types works`` () =
    ('a').CompareTo('b') |> equal -1
    ('b').CompareTo('a') |> equal 1
    (1y).CompareTo(1y) |> equal 0
    (1uy).CompareTo(1uy) |> equal 0
    (1s).CompareTo(1s) |> equal 0
    (1u).CompareTo(1u) |> equal 0
    (1us).CompareTo(1us) |> equal 0
    (1ul).CompareTo(1ul) |> equal 0
    (1uL).CompareTo(1uL) |> equal 0
    (1.f).CompareTo(1.f) |> equal 0

[<Fact>]
let ``test hash with tuples works`` () =
    let h1 = hash (1, 2)
    let h2 = hash (1, 2)
    let h3 = hash (2, 1)
    equal h1 h2
    notEqual h1 h3

[<Fact>]
let ``test hash with lists works`` () =
    let h1 = hash [1; 2]
    let h2 = hash [1; 2]
    let h3 = hash [2; 1]
    equal h1 h2
    notEqual h1 h3

[<Fact>]
let ``test hash with records works`` () =
    let h1 = hash {a=1; b=2}
    let h2 = hash {a=1; b=2}
    let h3 = hash {a=2; b=1}
    equal h1 h2
    notEqual h1 h3

[<Fact>]
let ``test compare with strings works`` () =
    equal 0 (compare "a" "a")
    equal -1 (compare "a" "b")
    equal 1 (compare "b" "a")

// --- Set equality ---

[<Fact>]
let ``test Set equality works`` () =
    let xs1 = Set [ 1; 2; 3 ]
    let xs2 = Set [ 1; 2; 3 ]
    let xs3 = Set [ 1; 2; 4 ]
    let xs4 = Set [ 3; 2; 1 ]
    let xs5 = Set [ 1; 2; 3; 1 ]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 = xs4)
    equal false (xs1 <> xs5)

[<Fact>]
let ``test Set comparison works`` () =
    let xs1 = Set [ 1; 2; 3 ]
    let xs2 = Set [ 1; 2; 3 ]
    let xs3 = Set [ 1; 2; 4 ]
    let xs4 = Set [ 1; 2; 2 ]
    let xs5 = Set [ 1; 2 ]
    let xs6 = Set [ 1; 2; 3; 1 ]
    equal 0 (compare xs1 xs2)
    equal -1 (compare xs1 xs3)
    equal true (xs1 < xs3)
    equal 1 (compare xs1 xs4)
    equal false (xs1 < xs4)
    equal 1 (compare xs1 xs5)
    equal true (xs1 > xs5)
    equal 0 (compare xs1 xs6)

// --- hash with more types ---

[<Fact>]
let ``test hash with arrays works`` () =
    (hash [|1; 2|], hash [|1; 2|]) ||> equal
    (hash [|2; 1|], hash [|1; 2|]) ||> notEqual

[<Fact>]
let ``test hash with options works`` () =
    (hash (Some 1), hash (Some 1)) ||> equal
    (hash (Some 2), hash (Some 1)) ||> notEqual

[<Fact>]
let ``test hash with unions works`` () =
    (hash (A 1), hash (A 1)) ||> equal
    (hash (A 2), hash (A 1)) ||> notEqual
    (hash (B 1), hash (A 1)) ||> notEqual

// --- Unchecked ---

[<Fact>]
let ``test Unchecked.equals works`` () =
    Unchecked.equals 111 111 |> equal true
    Unchecked.equals 222 333 |> equal false
    Unchecked.equals "1" "1" |> equal true
    Unchecked.equals "2" "3" |> equal false
    Unchecked.equals [1] [1] |> equal true
    Unchecked.equals [2] [3] |> equal false

[<Fact>]
let ``test Unchecked.compare works`` () =
    Unchecked.compare 111 111 |> equal 0
    Unchecked.compare 222 333 |> equal -1
    Unchecked.compare 333 222 |> equal 1
    Unchecked.compare "1" "1" |> equal 0
    Unchecked.compare "2" "3" |> equal -1
    Unchecked.compare "3" "2" |> equal 1
    Unchecked.compare [1] [1] |> equal 0
    Unchecked.compare [2] [3] |> equal -1
    Unchecked.compare [3] [2] |> equal 1

[<Fact>]
let ``test Unchecked.hash with primitives works`` () =
    (Unchecked.hash 111, Unchecked.hash 111) ||> equal
    (Unchecked.hash 222, Unchecked.hash 333) ||> notEqual
    (Unchecked.hash "1", Unchecked.hash "1") ||> equal
    (Unchecked.hash "2", Unchecked.hash "3") ||> notEqual

[<Fact>]
let ``test Unchecked.hash with lists works`` () =
    (Unchecked.hash [1;2], Unchecked.hash [1;2]) ||> equal
    (Unchecked.hash [2;1], Unchecked.hash [1;2]) ||> notEqual

[<Fact>]
let ``test Unchecked.hash with arrays works`` () =
    (Unchecked.hash [|1;2|], Unchecked.hash [|1;2|]) ||> equal
    (Unchecked.hash [|2;1|], Unchecked.hash [|1;2|]) ||> notEqual

[<Fact>]
let ``test Unchecked.hash with tuples works`` () =
    (Unchecked.hash (1,2), Unchecked.hash (1,2)) ||> equal
    (Unchecked.hash (2,1), Unchecked.hash (1,2)) ||> notEqual

// --- LanguagePrimitives ---

[<Fact>]
let ``test LanguagePrimitives.GenericEquality works`` () =
    LanguagePrimitives.GenericEquality 111 111 |> equal true
    LanguagePrimitives.GenericEquality 222 333 |> equal false
    LanguagePrimitives.GenericEquality "1" "1" |> equal true
    LanguagePrimitives.GenericEquality "2" "3" |> equal false
    LanguagePrimitives.GenericEquality [1] [1] |> equal true
    LanguagePrimitives.GenericEquality [2] [3] |> equal false

[<Fact>]
let ``test LanguagePrimitives.GenericComparison works`` () =
    LanguagePrimitives.GenericComparison 111 111 |> equal 0
    LanguagePrimitives.GenericComparison 222 333 |> equal -1
    LanguagePrimitives.GenericComparison 333 222 |> equal 1
    LanguagePrimitives.GenericComparison "1" "1" |> equal 0
    LanguagePrimitives.GenericComparison "2" "3" |> equal -1
    LanguagePrimitives.GenericComparison "3" "2" |> equal 1

[<Fact>]
let ``test LanguagePrimitives.GenericHash with primitives works`` () =
    (LanguagePrimitives.GenericHash 111, LanguagePrimitives.GenericHash 111) ||> equal
    (LanguagePrimitives.GenericHash 222, LanguagePrimitives.GenericHash 111) ||> notEqual
    (LanguagePrimitives.GenericHash "1", LanguagePrimitives.GenericHash "1") ||> equal
    (LanguagePrimitives.GenericHash "2", LanguagePrimitives.GenericHash "1") ||> notEqual

[<Fact>]
let ``test LanguagePrimitives.GenericHash with lists works`` () =
    (LanguagePrimitives.GenericHash [1;2], LanguagePrimitives.GenericHash [1;2]) ||> equal
    (LanguagePrimitives.GenericHash [2;1], LanguagePrimitives.GenericHash [1;2]) ||> notEqual

[<Fact>]
let ``test LanguagePrimitives.GenericHash with arrays works`` () =
    (LanguagePrimitives.GenericHash [|1;2|], LanguagePrimitives.GenericHash [|1;2|]) ||> equal
    (LanguagePrimitives.GenericHash [|2;1|], LanguagePrimitives.GenericHash [|1;2|]) ||> notEqual

[<Fact>]
let ``test LanguagePrimitives.GenericHash with tuples works`` () =
    (LanguagePrimitives.GenericHash (1,2), LanguagePrimitives.GenericHash (1,2)) ||> equal
    (LanguagePrimitives.GenericHash (2,1), LanguagePrimitives.GenericHash (1,2)) ||> notEqual

// --- GetHashCode with more types ---

[<Fact>]
let ``test GetHashCode with lists works`` () =
    ([1; 2].GetHashCode(), [1; 2].GetHashCode()) ||> equal
    ([2; 1].GetHashCode(), [1; 2].GetHashCode()) ||> notEqual

[<Fact>]
let ``test GetHashCode with tuples works`` () =
    ((1, 2).GetHashCode(), (1, 2).GetHashCode()) ||> equal
    ((2, 1).GetHashCode(), (1, 2).GetHashCode()) ||> notEqual

[<Fact>]
let ``test GetHashCode with options works`` () =
    ((Some 1).GetHashCode(), (Some 1).GetHashCode()) ||> equal
    ((Some 2).GetHashCode(), (Some 1).GetHashCode()) ||> notEqual

[<Fact>]
let ``test GetHashCode with unions works`` () =
    ((A 1).GetHashCode(), (A 1).GetHashCode()) ||> equal
    ((A 2).GetHashCode(), (A 1).GetHashCode()) ||> notEqual
    ((B 1).GetHashCode(), (A 1).GetHashCode()) ||> notEqual

[<Fact>]
let ``test GetHashCode with records works`` () =
    ({a=1; b=2}.GetHashCode(), {a=1; b=2}.GetHashCode()) ||> equal
    ({a=2; b=1}.GetHashCode(), {a=1; b=2}.GetHashCode()) ||> notEqual

