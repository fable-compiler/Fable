module Fable.Tests.Comparison

open System
open Util.Testing
open Fable.Tests.Util
open Fable.Core.PyInterop
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols


type UTest = A of int | B of int
type RTest = { a: int; b: int }
type STest = struct val A: int; new(a: int) = { A = a }; end
type OTest(a) = member val A = a with get, set

[<CustomEquality; CustomComparison>]
type UTest2 =
    | String of string
    override x.GetHashCode() = x.GetHashCode()
    override x.Equals(yobj) =
       match yobj with
         | :? UTest2 as y ->
            match x, y with
            | String s1, String s2 -> (s1 + s1) = s2
         | _ -> false
    interface System.IEquatable<UTest2> with
        member x.Equals(y) =
            match x, y with
            | String s1, String s2 -> (s1 + s1) = s2
    interface System.IComparable with
        member x.CompareTo(yobj) =
            match yobj with
            | :? UTest2 as y ->
                match x, y with
                | String s1, String s2 -> compare (s1 + s1) s2
            | _ -> invalidArg "yobj" "cannot compare values of different types"


exception Ex of int

type MyTest(i: int) =
    member x.Value = i
    override x.GetHashCode() = i
    override x.Equals(yobj) =
       match yobj with
         | :? MyTest as y -> y.Value + 1 = x.Value
         | _ -> false
    interface System.IComparable with
        member x.CompareTo(yobj) =
            match yobj with
            | :? MyTest as y -> compare (y.Value + 1) x.Value
            | _ -> invalidArg "yobj" "cannot compare values of different types"
            // | _ -> -1

    interface System.IEquatable<MyTest> with
        member x.Equals(y) =
            y.Value + 1 = x.Value

type Status =
| CreateScenePicture
| ReadingOldDevice
| CreateOldMeterReadingPicture
| SelectingNewDevice
| ReadingNewDevice
| CreateNewMeterReadingPicture
| GetSignature
| Done

type MyClass(v) =
    member val Value: int = v with get, set

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
let ``test Tuple equality works`` () =
    let xs1 = ( 1, 2, 3 )
    let xs2 = ( 1, 2, 3 )
    let xs3 = ( 1, 2, 4 )
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

[<Fact>]
let ``test Tuple option equality works``() =
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
let ``test List option equality works``() =
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
let ``test Set option equality works`` () =
    let xs1 = Some (Set [ 1; 2; 3 ])
    let xs2 = Some (Set [ 1; 2; 3 ])
    let xs3 = Some (Set [ 1; 2; 4 ])
    let xs4 = Some (Set [ 3; 2; 1 ])
    let xs5 = Some (Set [ 1; 2; 3; 1 ])
    let xs6 = None
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 = xs4)
    equal false (xs1 <> xs5)
    equal false (xs1 = xs6)
    equal true (xs1 <> xs6)
    equal true (xs6 = None)
    equal false (xs6 <> None)

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
    Object.ReferenceEquals(u1, u1) |> equal true
    Object.ReferenceEquals(u1, u2) |> equal false

[<Fact>]
let ``test Union custom equality works`` () =
    let u1 = String "A"
    let u2 = String "A"
    let u3 = String "AA"
    equal false (u1 = u2)
    equal true (u1 = u3)

[<Fact>]
let ``test Record equality works`` () =
    let r1 = { a = 1; b = 2 }
    let r2 = { a = 1; b = 2 }
    let r3 = { a = 1; b = 4 }
    equal true (r1 = r2)
    equal false (r1 = r3)
    equal true (r1 <> r3)
    equal false (r1 <> r2)
    Object.ReferenceEquals(r1, r1) |> equal true
    Object.ReferenceEquals(r1, r2) |> equal false

[<Fact>]
let ``test Exception equality works`` () =
    equal true ((Ex 1) = (Ex 1))
    equal false ((Ex 1) = (Ex 2))

[<Fact>]
let ``test Equality with objects implementing IEquatable works`` () =
    let c1 = MyTest(5)
    let c2 = MyTest(4)
    let c3 = MyTest(5)
    equal true (c1 = c2)
    equal false (c1 = c3)
    equal true (c1 <> c3)
    equal false (c1 <> c2)
    Object.ReferenceEquals(c1, c1) |> equal true
    Object.ReferenceEquals(c1, c2) |> equal false

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
let ``test Array comparison works`` () =
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

[<Fact>]
let ``test Map comparison works`` () =
    let xs1 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs2 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
    let xs3 = Map [ ("a", 1); ("b", 2); ("c", 4) ]
    let xs4 = Map [ ("a", 1); ("b", 2); ("c", 2) ]
    let xs5 = Map [ ("a", 1); ("b", 2) ]
    let xs6 = Map [ ("a", 1); ("b", 2); ("c", 3); ("d", 1) ]
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
let ``test Union custom comparison works`` () =
    let u1 = String "A"
    let u2 = String "A"
    let u3 = String "AA"
    equal 0 (compare u1 u3)
    equal true (compare u1 u2 > 0)

[<Fact>]
let ``test Record comparison works`` () =
    let r1 = { a = 1; b = 2 }
    let r2 = { a = 1; b = 2 }
    let r3 = { a = 1; b = 4 }
    equal 0 (compare r1 r2)
    (compare r1 r3) = 0 |> equal false

[<Fact>]
let ``test Comparison with objects implementing IComparable works`` () =
    let c1 = MyTest(5)
    let c2 = MyTest(4)
    let c3 = MyTest(5)
    equal 0 (compare c1 c2)
    equal 1 (compare c1 c3)
    equal true (c1 > c3)

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
let ``test min with objects implementing IComparable works`` () =
    let c1 = MyTest(5)
    let c2 = MyTest(5)
    Object.ReferenceEquals(min c1 c2, c2) |> equal true

[<Fact>]
let ``test nullArg works`` () =
    try
        nullArg null
        true
    with _ex ->
        false
    |> equal false


[<Fact>]
let ``test using function disposes the resource when action finishes`` () =
    let mutable disposed = false
    let resource = { new IDisposable with member _.Dispose() = disposed <- true }
    using resource (fun _resource -> ())
    equal true disposed

[<Fact>]
let ``test using function disposes the resource when action fails`` () =
    let mutable disposed = false
    let resource = { new IDisposable with member _.Dispose() = disposed <- true }
    try
        using resource (fun _resource -> failwith "action failed")
    with
    | _ -> () // ignore
    equal true disposed

[<Fact>]
let ``test isNull with primitives works`` () =
    isNull null |> equal true
    isNull "" |> equal false
    isNull "0" |> equal false
    isNull "hello" |> equal false

[<Fact>]
let ``test isNull with objects works`` () =
    let s1: String = null
    isNull s1 |> equal true
    let s2: String = "hello"
    isNull s2 |> equal false

[<Fact>]
let ``test Classes must use identity hashing by default`` () =
    let x = MyClass(5)
    let y = MyClass(5)
    let h1 = hash(box x)
    let h2 = hash(box y)
    x.Value <- 8
    let h3 = hash(box x)
    h1 = h2 |> equal false
    h1 = h3 |> equal true

// #if FABLE_COMPILER
// [<Fact>]
// let ``test GetHashCode with arrays works`` () =
//     ([|1; 2|].GetHashCode(), [|1; 2|].GetHashCode()) ||> equal // Equal in Python (not in .NET)
//     ([|2; 1|].GetHashCode(), [|1; 2|].GetHashCode()) ||> notEqual
// #endif

// [<Fact>]
// let ``test GetHashCode with arrays works II`` () =
//     // Assigning the arrays makes them different
//     let a = [|1; 2|]
//     let b = [|1; 2|]
//     (a.GetHashCode(), b.GetHashCode()) ||> notEqual
//     ([|2; 1|].GetHashCode(), [|1; 2|].GetHashCode()) ||> notEqual

// [<Fact>]
// let ``test GetHashCode with lists works`` () =
//     ([1; 2].GetHashCode(), [1; 2].GetHashCode()) ||> equal
//     ([2; 1].GetHashCode(), [1; 2].GetHashCode()) ||> notEqual
