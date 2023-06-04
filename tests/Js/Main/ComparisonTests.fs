module Fable.Tests.Comparison

open System
open Util.Testing
open Fable.Tests.Util
open Fable.Core.JsInterop
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

[<ReferenceEquality>]
type RTest2 = { a2: int; b2: int }

type Test(i: int) =
    member x.Value = i
    override x.GetHashCode() = i
    override x.Equals(yobj) =
       match yobj with
         | :? Test as y -> y.Value + 1 = x.Value
         | _ -> false
    interface System.IComparable with
        member x.CompareTo(yobj) =
            match yobj with
            | :? Test as y -> compare (y.Value + 1) x.Value
            | _ -> invalidArg "yobj" "cannot compare values of different types"
            // | _ -> -1

    interface System.IEquatable<Test> with
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

[<CustomEquality; NoComparison>]
type FuzzyInt =
    | FuzzyInt of int
    override x.GetHashCode() =
        let (FuzzyInt x) = x
        x.GetHashCode()

    override x.Equals(y: obj) =
        match y with
        | :? FuzzyInt as y ->
            let (FuzzyInt x) = x
            let (FuzzyInt y) = y
            x - 2 <= y && y <= x + 2
        | _ -> false

let tests =
  testList "Comparison" [
    testCase "Typed array equality works" <| fun () ->
        let xs1 = [| 1; 2; 3 |]
        let xs2 = [| 1; 2; 3 |]
        let xs3 = [| 1; 2; 4 |]
        let xs4 = [| 1; 2 |]
        equal true (xs1 = xs2)
        equal false (xs1 = xs3)
        equal true (xs1 <> xs3)
        equal false (xs1 <> xs2)
        equal true (xs1 <> xs4)

    testCase "Array equality works" <| fun () ->
        let xs1 = [| "1"; "2"; "3" |]
        let xs2 = [| "1"; "2"; "3" |]
        let xs3 = [| "1"; "2"; "4" |]
        let xs4 = [| "1"; "2" |]
        equal true (xs1 = xs2)
        equal false (xs1 = xs3)
        equal true (xs1 <> xs3)
        equal false (xs1 <> xs2)
        equal true (xs1 <> xs4)

    testCase "Array custom equality works" <| fun () ->
        let xs = [| FuzzyInt 3; FuzzyInt 5 |]
        let ys = [| FuzzyInt 2; FuzzyInt 4 |]
        let zs = [| FuzzyInt 2; FuzzyInt 8 |]
        xs = ys |> equal true
        xs = zs |> equal false

    testCase "Tuple equality works" <| fun () ->
        let xs1 = ( 1, 2, 3 )
        let xs2 = ( 1, 2, 3 )
        let xs3 = ( 1, 2, 4 )
        equal true (xs1 = xs2)
        equal false (xs1 = xs3)
        equal true (xs1 <> xs3)
        equal false (xs1 <> xs2)

    testCase "List equality works" <| fun () ->
        let xs1 = [ 1; 2; 3 ]
        let xs2 = [ 1; 2; 3 ]
        let xs3 = [ 1; 2; 4 ]
        equal true (xs1 = xs2)
        equal false (xs1 = xs3)
        equal true (xs1 <> xs3)
        equal false (xs1 <> xs2)

    testCase "Set equality works" <| fun () ->
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

    testCase "Map equality works" <| fun () ->
        let xs1 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
        let xs2 = Map [ ("a", 1); ("b", 2); ("c", 3) ]
        let xs3 = Map [ ("a", 1); ("b", 2); ("c", 4) ]
        let xs4 = Map [ ("c", 3); ("b", 2); ("a", 1) ]
        equal true (xs1 = xs2)
        equal false (xs1 = xs3)
        equal true (xs1 <> xs3)
        equal false (xs1 <> xs2)
        equal true (xs1 = xs4)

    testCase "Union equality works" <| fun () ->
        // Prevents Fable from inlining so TypeScript doesn't complain
        let mutable run = fun u1 u2 u3 u4 ->
            equal true (u1 = u2)
            equal false (u1 = u3)
            equal true (u1 <> u3)
            equal false (u1 <> u2)
            equal false (u1 = u4)
            Object.ReferenceEquals(u1, u1) |> equal true
            Object.ReferenceEquals(u1, u2) |> equal false
        let u1 = A 2
        let u2 = A 2
        let u3 = A 4
        let u4 = B 2
        run u1 u2 u3 u4

    testCase "Union custom equality works" <| fun () ->
        let u1 = String "A"
        let u2 = String "A"
        let u3 = String "AA"
        equal false (u1 = u2)
        equal true (u1 = u3)

    testCase "Record equality works" <| fun () ->
        let r1 = { a = 1; b = 2 }
        let r2 = { a = 1; b = 2 }
        let r3 = { a = 1; b = 4 }
        equal true (r1 = r2)
        equal false (r1 = r3)
        equal true (r1 <> r3)
        equal false (r1 <> r2)
        Object.ReferenceEquals(r1, r1) |> equal true
        Object.ReferenceEquals(r1, r2) |> equal false

    testCase "Exception equality works" <| fun () ->
        equal true ((Ex 1) = (Ex 1))
        equal false ((Ex 1) = (Ex 2))

    // TODO: If we want to make this work in Fable 2 we'll have
    // to access reflection info for records

    // #if FABLE_COMPILER
    // testCase "Record equality ignores dynamic fields" <| fun () ->
    //     let r1 = { a = 1; b = 2 }
    //     r1?c <- 1
    //     let r2 = { a = 1; b = 2 }
    //     r2?c <- 2
    //     equal true (r1 = r2)
    //     equal false (r1 <> r2)

    // testCase "Record comparison ignores dynamic fields" <| fun () ->
    //     let r1 = { a = 1; b = 2 }
    //     r1?c <- 1
    //     let r2 = { a = 1; b = 2 }
    //     r2?c <- 2
    //     equal 0 (compare r1 r2)
    // #endif

    testCase "Equality with objects implementing IEquatable works" <| fun () ->
        let c1 = Test(5)
        let c2 = Test(4)
        let c3 = Test(5)
        equal true (c1 = c2)
        equal false (c1 = c3)
        equal true (c1 <> c3)
        equal false (c1 <> c2)
        Object.ReferenceEquals(c1, c1) |> equal true
        Object.ReferenceEquals(c1, c2) |> equal false

    testCase "Typed array comparison works" <| fun () ->
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

    testCase "Array comparison works" <| fun () ->
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

    testCase "Tuple comparison works" <| fun () ->
        let xs1 = ( 1, 2, 3 )
        let xs2 = ( 1, 2, 3 )
        let xs3 = ( 1, 2, 4 )
        let xs4 = ( 1, 2, 2 )
        equal 0 (compare xs1 xs2)
        equal -1 (compare xs1 xs3)
        equal true (xs1 < xs3)
        equal 1 (compare xs1 xs4)
        equal false (xs1 < xs4)

    testCase "List comparison works" <| fun () ->
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

    testCase "Set comparison works" <| fun () ->
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

    testCase "Map comparison works" <| fun () ->
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

    testCase "Union comparison works" <| fun () ->
        let mutable run = fun u1 u2 u3 u4 u5 ->
            equal 0 (compare u1 u2)
            equal -1 (compare u1 u3)
            equal true (u1 < u3)
            equal 1 (compare u1 u4)
            equal false (u1 < u4)
            (compare u1 u5) = 0 |> equal false
        let u1 = A 2
        let u2 = A 2
        let u3 = A 4
        let u4 = A 1
        let u5 = B 2
        run u1 u2 u3 u4 u5

    testCase "Union custom comparison works" <| fun () ->
        let u1 = String "A"
        let u2 = String "A"
        let u3 = String "AA"
        equal 0 (compare u1 u3)
        equal true (compare u1 u2 > 0)

    testCase "Record comparison works" <| fun () ->
        let r1 = { a = 1; b = 2 }
        let r2 = { a = 1; b = 2 }
        let r3 = { a = 1; b = 4 }
        equal 0 (compare r1 r2)
        (compare r1 r3) = 0 |> equal false

    testCase "Comparison with objects implementing IComparable works" <| fun () ->
        let c1 = Test(5)
        let c2 = Test(4)
        let c3 = Test(5)
        equal 0 (compare c1 c2)
        equal 1 (compare c1 c3)
        equal true (c1 > c3)

    testCase "max works with primitives" <| fun () ->
        max 1 2 |> equal 2
        Math.Max(1, 2) |> equal 2
        max "a" "b" |> equal "b"

    testCase "max works with records" <| fun () ->
        let r1 = {a=1; b=1}
        let r2 = {a=1; b=2}
        max r1 r2 |> equal r2

    testCase "max with objects implementing IComparable works" <| fun () ->
        let c1 = Test(5)
        let c2 = Test(5)
        Object.ReferenceEquals(max c1 c2, c1) |> equal true

    testCase "min works with primitives" <| fun () ->
        min 1 2 |> equal 1
        Math.Min(1, 2) |> equal 1
        min "a" "b" |> equal "a"

    testCase "min works with records" <| fun () ->
        let r1 = {a=1; b=1}
        let r2 = {a=1; b=2}
        min r1 r2 |> equal r1

    testCase "min with objects implementing IComparable works" <| fun () ->
        let c1 = Test(5)
        let c2 = Test(5)
        Object.ReferenceEquals(min c1 c2, c2) |> equal true

    testCase "nullArg works" <| fun () ->
        try
            nullArg null
            true
        with _ex ->
            false
        |> equal false

    testCase "using function disposes the resource when action finishes" <| fun () ->
        let mutable disposed = false
        let resource = { new IDisposable with member _.Dispose() = disposed <- true }
        using resource (fun _resource -> ())
        equal true disposed

    testCase "using function disposes the resource when action fails" <| fun () ->
        let mutable disposed = false
        let resource = { new IDisposable with member _.Dispose() = disposed <- true }
        try
            using resource (fun _resource -> failwith "action failed")
        with
        | _ -> () // ignore
        equal true disposed

    testCase "isNull with primitives works" <| fun () ->
        isNull null |> equal true
        isNull "" |> equal false
        isNull "0" |> equal false
        isNull "hello" |> equal false

    testCase "isNull with objects works" <| fun () ->
        let s1: String = null
        isNull s1 |> equal true
        let s2: String = "hello"
        isNull s2 |> equal false

    testCase "Classes must use identity hashing by default" <| fun () -> // See #2291
        let x = MyClass(5)
        let y = MyClass(5)
        let h1 = hash(box x)
        let h2 = hash(box y)
        x.Value <- 8
        let h3 = hash(box x)
        h1 = h2 |> equal false
        h1 = h3 |> equal true

    testCase "GetHashCode with arrays works" <| fun () ->
        ([|1; 2|].GetHashCode(), [|1; 2|].GetHashCode()) ||> notEqual
        ([|2; 1|].GetHashCode(), [|1; 2|].GetHashCode()) ||> notEqual

    testCase "GetHashCode with lists works" <| fun () ->
        ([1; 2].GetHashCode(), [1; 2].GetHashCode()) ||> equal
        ([2; 1].GetHashCode(), [1; 2].GetHashCode()) ||> notEqual

    testCase "GetHashCode with tuples works" <| fun () ->
        ((1, 2).GetHashCode(), (1, 2).GetHashCode()) ||> equal
        ((2, 1).GetHashCode(), (1, 2).GetHashCode()) ||> notEqual

    testCase "GetHashCode with options works" <| fun () ->
        ((Some 1).GetHashCode(), (Some 1).GetHashCode()) ||> equal
        ((Some 2).GetHashCode(), (Some 1).GetHashCode()) ||> notEqual
        ((Some None).GetHashCode(), (Some 1).GetHashCode()) ||> notEqual

    testCase "GetHashCode with unions works" <| fun () ->
        ((UTest.A 1).GetHashCode(), (UTest.A 1).GetHashCode()) ||> equal
        ((UTest.A 2).GetHashCode(), (UTest.A 1).GetHashCode()) ||> notEqual
        ((UTest.B 1).GetHashCode(), (UTest.A 1).GetHashCode()) ||> notEqual

    testCase "GetHashCode with records works" <| fun () ->
        ({a=1; b=2}.GetHashCode(), {a=1; b=2}.GetHashCode()) ||> equal
        ({a=2; b=1}.GetHashCode(), {a=1; b=2}.GetHashCode()) ||> notEqual

    testCase "GetHashCode with structs works" <| fun () ->
        (STest(1).GetHashCode(), STest(1).GetHashCode()) ||> equal
        (STest(2).GetHashCode(), STest(1).GetHashCode()) ||> notEqual

    testCase "GetHashCode with objects works" <| fun () ->
        (OTest(1).GetHashCode(), OTest(1).GetHashCode()) ||> notEqual
        (OTest(2).GetHashCode(), OTest(1).GetHashCode()) ||> notEqual

    testCase "GetHashCode with objects that overwrite it works" <| fun () ->
        (Test(1).GetHashCode(), Test(1).GetHashCode()) ||> equal
        (Test(2).GetHashCode(), Test(1).GetHashCode()) ||> notEqual

    testCase "GetHashCode with same object works" <| fun () ->
        let o = OTest(1)
        let h1 = o.GetHashCode()
        o.A <- 2
        let h2 = o.GetHashCode()
        (h1, h2) ||> equal

    testCase "GetHashCode with primitives works" <| fun () ->
        ("1".GetHashCode(), "1".GetHashCode()) ||> equal
        ("2".GetHashCode(), "1".GetHashCode()) ||> notEqual

    // This is going to give different results in .NET and JS
    // Just check no exception is thrown
    testCase "hash works with JS objects" <| fun () -> // See #2281
        let _ = obj () |> hash
        ()

    testCase "hash with arrays works" <| fun () ->
        (hash [|1; 2|], hash [|1; 2|]) ||> equal
        (hash [|2; 1|], hash [|1; 2|]) ||> notEqual

    testCase "hash with lists works" <| fun () ->
        (hash [1; 2], hash [1; 2]) ||> equal
        (hash [2; 1], hash [1; 2]) ||> notEqual

    testCase "hash with tuples works" <| fun () ->
        (hash (1, 2), hash (1, 2)) ||> equal
        (hash (2, 1), hash (1, 2)) ||> notEqual

    testCase "hash with options works" <| fun () ->
        (hash (Some 1), hash (Some 1)) ||> equal
        (hash (Some 2), hash (Some 1)) ||> notEqual
        (hash (Some None), hash (Some 1)) ||> notEqual

    testCase "hash with unions works" <| fun () ->
        (hash (UTest.A 1), hash (UTest.A 1)) ||> equal
        (hash (UTest.A 2), hash (UTest.A 1)) ||> notEqual
        (hash (UTest.B 1), hash (UTest.A 1)) ||> notEqual

    testCase "hash with records works" <| fun () ->
        (hash {a=1; b=2}, hash {a=1; b=2}) ||> equal
        (hash {a=2; b=1}, hash {a=1; b=2}) ||> notEqual

    testCase "hash with structs works" <| fun () ->
        (hash (STest(1)), hash (STest(1))) ||> equal
        (hash (STest(2)), hash (STest(1))) ||> notEqual

    testCase "hash with objects works" <| fun () ->
        (hash (OTest(1)), hash (OTest(1))) ||> notEqual
        (hash (OTest(2)), hash (OTest(1))) ||> notEqual

    testCase "hash with same object works" <| fun () ->
        let o = OTest(1)
        let h1 = hash o
        o.A <- 2
        let h2 = hash o
        (h1, h2) ||> equal

    testCase "hash with longs works" <| fun () ->
        (hash (1L<<<33), hash (1L<<<33)) ||> equal
        (hash (1L<<<34), hash (1L<<<33)) ||> notEqual
        (hash 3L, hash (3L + (1L<<<33))) ||> notEqual
        (hash (-3L), hash (3L))          ||> notEqual

    testCase "hash with primitives works" <| fun () ->
        (hash 111, hash 111) ||> equal
        (hash 222, hash 111) ||> notEqual
        (hash "1", hash "1") ||> equal
        (hash "2", hash "1") ||> notEqual

    testCase "Unchecked.hash with primitives works" <| fun () ->
        (Unchecked.hash 111, Unchecked.hash 111) ||> equal
        (Unchecked.hash 222, Unchecked.hash 333) ||> notEqual
        (Unchecked.hash "1", Unchecked.hash "1") ||> equal
        (Unchecked.hash "2", Unchecked.hash "3") ||> notEqual

    testCase "Unchecked.hash with lists works" <| fun () ->
        (Unchecked.hash [1;2], Unchecked.hash [1;2]) ||> equal
        (Unchecked.hash [2;1], Unchecked.hash [1;2]) ||> notEqual

    testCase "Unchecked.hash with arrays works" <| fun () ->
        (Unchecked.hash [|1;2|], Unchecked.hash [|1;2|]) ||> equal
        (Unchecked.hash [|2;1|], Unchecked.hash [|1;2|]) ||> notEqual

    testCase "Unchecked.hash with tuples works" <| fun () ->
        (Unchecked.hash (1,2), Unchecked.hash (1,2)) ||> equal
        (Unchecked.hash (2,1), Unchecked.hash (1,2)) ||> notEqual

    testCase "Unchecked.equals works" <| fun () ->
        Unchecked.equals 111 111 |> equal true
        Unchecked.equals 222 333 |> equal false
        Unchecked.equals "1" "1" |> equal true
        Unchecked.equals "2" "3" |> equal false
        Unchecked.equals [1] [1] |> equal true
        Unchecked.equals [2] [3] |> equal false
        // Use mutable values to prevent inlining
        let mutable ones = 111
        let mutable twos = 222
        let mutable threes = 333
        let mutable oneStr = "1"
        let mutable twoStr = "2"
        let mutable threeStr = "3"
        Unchecked.equals ones ones |> equal true
        Unchecked.equals twos threes |> equal false
        Unchecked.equals oneStr oneStr |> equal true
        Unchecked.equals twoStr threeStr |> equal false

    testCase "Unchecked.compare works" <| fun () ->
        Unchecked.compare 111 111 |> equal 0
        Unchecked.compare 222 333 |> equal -1
        Unchecked.compare 333 222 |> equal 1
        Unchecked.compare "1" "1" |> equal 0
        Unchecked.compare "2" "3" |> equal -1
        Unchecked.compare "3" "2" |> equal 1
        Unchecked.compare [1] [1] |> equal 0
        Unchecked.compare [2] [3] |> equal -1
        Unchecked.compare [3] [2] |> equal 1

    testCase "DU comparison works" <| fun () ->
        let hasStatusReached expectedStatus status =
            status >= expectedStatus
        let mutable run = fun x y ->
            x >= y |> equal true
            hasStatusReached y x |> equal true
        run Status.CreateNewMeterReadingPicture Status.SelectingNewDevice

    testCase "LanguagePrimitives.GenericHash with primitives works" <| fun () ->
        (LanguagePrimitives.GenericHash 111, LanguagePrimitives.GenericHash 111) ||> equal
        (LanguagePrimitives.GenericHash 222, LanguagePrimitives.GenericHash 111) ||> notEqual
        (LanguagePrimitives.GenericHash "1", LanguagePrimitives.GenericHash "1") ||> equal
        (LanguagePrimitives.GenericHash "2", LanguagePrimitives.GenericHash "1") ||> notEqual

    testCase "LanguagePrimitives.GenericHash with lists works" <| fun () ->
        (LanguagePrimitives.GenericHash [1;2], LanguagePrimitives.GenericHash [1;2]) ||> equal
        (LanguagePrimitives.GenericHash [2;1], LanguagePrimitives.GenericHash [1;2]) ||> notEqual

    testCase "LanguagePrimitives.GenericHash with arrays works" <| fun () ->
        (LanguagePrimitives.GenericHash [|1;2|], LanguagePrimitives.GenericHash [|1;2|]) ||> equal
        (LanguagePrimitives.GenericHash [|2;1|], LanguagePrimitives.GenericHash [|1;2|]) ||> notEqual

    testCase "LanguagePrimitives.GenericHash with tuples works" <| fun () ->
        (LanguagePrimitives.GenericHash (1,2), LanguagePrimitives.GenericHash (1,2)) ||> equal
        (LanguagePrimitives.GenericHash (2,1), LanguagePrimitives.GenericHash (1,2)) ||> notEqual

    testCase "LanguagePrimitives.PhysicalHash with primitives works" <| fun () ->
        (LanguagePrimitives.PhysicalHash "1", LanguagePrimitives.PhysicalHash "1") ||> equal
        (LanguagePrimitives.PhysicalHash "2", LanguagePrimitives.PhysicalHash "1") ||> notEqual

    testCase "LanguagePrimitives.PhysicalHash with lists works" <| fun () ->
        (LanguagePrimitives.PhysicalHash [1;2], LanguagePrimitives.PhysicalHash [1;2]) ||> notEqual
        (LanguagePrimitives.PhysicalHash [2;1], LanguagePrimitives.PhysicalHash [1;2]) ||> notEqual

    testCase "LanguagePrimitives.PhysicalHash with arrays works" <| fun () ->
        (LanguagePrimitives.PhysicalHash [|1;2|], LanguagePrimitives.PhysicalHash [|1;2|]) ||> notEqual
        (LanguagePrimitives.PhysicalHash [|2;1|], LanguagePrimitives.PhysicalHash [|1;2|]) ||> notEqual

    testCase "LanguagePrimitives.PhysicalHash with tuples works" <| fun () ->
        (LanguagePrimitives.PhysicalHash (1,2), LanguagePrimitives.PhysicalHash (1,2)) ||> notEqual
        (LanguagePrimitives.PhysicalHash (2,1), LanguagePrimitives.PhysicalHash (1,2)) ||> notEqual

    testCase "LanguagePrimitives.GenericComparison works" <| fun () ->
        LanguagePrimitives.GenericComparison 111 111 |> equal 0
        LanguagePrimitives.GenericComparison 222 333 |> equal -1
        LanguagePrimitives.GenericComparison 333 222 |> equal 1
        LanguagePrimitives.GenericComparison "1" "1" |> equal 0
        LanguagePrimitives.GenericComparison "2" "3" |> equal -1
        LanguagePrimitives.GenericComparison "3" "2" |> equal 1
        LanguagePrimitives.GenericComparison [1] [1] |> equal 0
        LanguagePrimitives.GenericComparison [2] [3] |> equal -1
        LanguagePrimitives.GenericComparison [3] [2] |> equal 1

    testCase "LanguagePrimitives.GenericEquality works" <| fun () ->
        LanguagePrimitives.GenericEquality 111 111 |> equal true
        LanguagePrimitives.GenericEquality 222 333 |> equal false
        LanguagePrimitives.GenericEquality "1" "1" |> equal true
        LanguagePrimitives.GenericEquality "2" "3" |> equal false
        LanguagePrimitives.GenericEquality [1] [1] |> equal true
        LanguagePrimitives.GenericEquality [2] [3] |> equal false
        // Use mutable values to prevent inlining
        let mutable ones = 111
        let mutable twos = 222
        let mutable threes = 333
        let mutable oneStr = "1"
        let mutable twoStr = "2"
        let mutable threeStr = "3"
        LanguagePrimitives.GenericEquality ones ones |> equal true
        LanguagePrimitives.GenericEquality twos threes |> equal false
        LanguagePrimitives.GenericEquality oneStr oneStr |> equal true
        LanguagePrimitives.GenericEquality twoStr threeStr |> equal false

    testCase "LanguagePrimitives.PhysicalEquality works" <| fun () ->
        LanguagePrimitives.PhysicalEquality "1" "1" |> equal true
        LanguagePrimitives.PhysicalEquality "2" "3" |> equal false
        LanguagePrimitives.PhysicalEquality [1] [1] |> equal false
        LanguagePrimitives.PhysicalEquality [2] [3] |> equal false
        // Use mutable values to prevent inlining
        let mutable oneStr = "1"
        let mutable twoStr = "2"
        let mutable threeStr = "3"
        LanguagePrimitives.PhysicalEquality oneStr oneStr |> equal true
        LanguagePrimitives.PhysicalEquality twoStr threeStr |> equal false

    testCase "LanguagePrimitives.SByteWithMeasure works" <| fun () ->
        let distance: sbyte<m> = LanguagePrimitives.SByteWithMeasure 1y
        distance |> equal 1y<m>

    testCase "LanguagePrimitives.Int16WithMeasure works" <| fun () ->
        let distance: int16<m> = LanguagePrimitives.Int16WithMeasure 1s
        distance |> equal 1s<m>

    testCase "LanguagePrimitives.Int32WithMeasure works" <| fun () ->
        let distance: int<m> = LanguagePrimitives.Int32WithMeasure 1
        distance |> equal 1<m>

    testCase "LanguagePrimitives.Int64WithMeasure works" <| fun () ->
        let distance: int64<m> = LanguagePrimitives.Int64WithMeasure 1L
        distance |> equal 1L<m>

    testCase "LanguagePrimitives.Float32WithMeasure works" <| fun () ->
        let distance: float32<m> = LanguagePrimitives.Float32WithMeasure 1.0f
        distance |> equal 1.0f<m>

    testCase "LanguagePrimitives.FloatWithMeasure works" <| fun () ->
        let distance: float<m> = LanguagePrimitives.FloatWithMeasure 1.0
        distance |> equal 1.0<m>

    testCase "LanguagePrimitives.DecimalWithMeasure works" <| fun () ->
        let distance: decimal<m> = LanguagePrimitives.DecimalWithMeasure 1.0m
        distance |> equal 1.0m<m>
  ]