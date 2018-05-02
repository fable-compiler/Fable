module Fable.Tests.Comparison

open System
open Util.Testing
open Fable.Tests.Util
open Fable.Core.JsInterop
open System.Collections.Generic

type UTest = A of int | B of int

// [<CustomEquality; CustomComparison>]
// type UTest2 =
//     | String of string
//     override x.GetHashCode() = x.GetHashCode()
//     override x.Equals(yobj) =
//        match yobj with
//          | :? UTest2 as y ->
//             match x, y with
//             | String s1, String s2 -> (s1 + s1) = s2
//          | _ -> false
//     interface System.IEquatable<UTest2> with
//         member x.Equals(y) =
//             match x, y with
//             | String s1, String s2 -> (s1 + s1) = s2
//     interface System.IComparable with
//         member x.CompareTo(yobj) =
//             match yobj with
//             | :? UTest2 as y ->
//                 match x, y with
//                 | String s1, String s2 -> compare (s1 + s1) s2
//             | _ -> invalidArg "yobj" "cannot compare values of different types"
//             | _ -> -1

type RTest = { a: int; b: int }

exception Ex of int

// [<ReferenceEquality>]
// type RTest2 = { a2: int; b2: int }

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
            | _ -> -1

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

    // testCase "Union custom equality works" <| fun () ->
    //     let u1 = String "A"
    //     let u2 = String "A"
    //     let u3 = String "AA"
    //     equal false (u1 = u2)
    //     equal true (u1 = u3)

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

    // testCase "Record reference equality works" <| fun () ->
    //     let r1 = { a2 = 1; b2 = 2 }
    //     let r2 = { a2 = 1; b2 = 2 }
    //     equal false (r1 = r2)

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

    // testCase "Union custom comparison works" <| fun () ->
    //     let u1 = String "A"
    //     let u2 = String "A"
    //     let u3 = String "AA"
    //     equal 0 (compare u1 u3)
    //     equal true (compare u1 u2 > 0)

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

    testCase "isNull works with primitives" <| fun () ->
        isNull null |> equal true
        isNull "" |> equal false
        isNull "0" |> equal false
        isNull "hello" |> equal false

    testCase "isNull works with objects" <| fun () ->
        let s1: String = null
        isNull s1 |> equal true

        let s2: String = "hello";
        isNull s2 |> equal false

    testCase "hash works" <| fun () ->
        (hash 111) = (hash 111) |> equal true
        (hash 222) = (hash 333) |> equal false
        (hash "1") = (hash "1") |> equal true
        (hash "2") = (hash "3") |> equal false
        (hash [1]) = (hash [1]) |> equal true
        (hash [2]) = (hash [3]) |> equal false

    testCase "Unchecked.hash works" <| fun () ->
        (Unchecked.hash 111) = (Unchecked.hash 111) |> equal true
        (Unchecked.hash 222) = (Unchecked.hash 333) |> equal false
        (Unchecked.hash "1") = (Unchecked.hash "1") |> equal true
        (Unchecked.hash "2") = (Unchecked.hash "3") |> equal false
        (Unchecked.hash [1]) = (Unchecked.hash [1]) |> equal true
        (Unchecked.hash [2]) = (Unchecked.hash [3]) |> equal false

    testCase "Unchecked.equals works" <| fun () ->
        Unchecked.equals 111 111 |> equal true
        Unchecked.equals 222 333 |> equal false
        Unchecked.equals "1" "1" |> equal true
        Unchecked.equals "2" "3" |> equal false
        Unchecked.equals [1] [1] |> equal true
        Unchecked.equals [2] [3] |> equal false

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

    // TODO!!! Union case comparison in Fable 2
    // testCase "DU comparison works" <| fun () ->
    //     // let hasStatusReached expectedStatus status =
    //     //     status >= expectedStatus

    //     Status.CreateNewMeterReadingPicture >= Status.SelectingNewDevice
    //     |> equal true

    // TODO!!!
    // testCase "LanguagePrimitives.GenericHash works" <| fun () ->
    //     (LanguagePrimitives.GenericHash 111) = (LanguagePrimitives.GenericHash 111) |> equal true
    //     (LanguagePrimitives.GenericHash 222) = (LanguagePrimitives.GenericHash 333) |> equal false
    //     (LanguagePrimitives.GenericHash "1") = (LanguagePrimitives.GenericHash "1") |> equal true
    //     (LanguagePrimitives.GenericHash "2") = (LanguagePrimitives.GenericHash "3") |> equal false
    //     (LanguagePrimitives.GenericHash [1]) = (LanguagePrimitives.GenericHash [1]) |> equal true
    //     (LanguagePrimitives.GenericHash [2]) = (LanguagePrimitives.GenericHash [3]) |> equal false

    // testCase "LanguagePrimitives.GenericComparison works" <| fun () ->
    //     LanguagePrimitives.GenericComparison 111 111 |> equal 0
    //     LanguagePrimitives.GenericComparison 222 333 |> equal -1
    //     LanguagePrimitives.GenericComparison 333 222 |> equal 1
    //     LanguagePrimitives.GenericComparison "1" "1" |> equal 0
    //     LanguagePrimitives.GenericComparison "2" "3" |> equal -1
    //     LanguagePrimitives.GenericComparison "3" "2" |> equal 1
    //     LanguagePrimitives.GenericComparison [1] [1] |> equal 0
    //     LanguagePrimitives.GenericComparison [2] [3] |> equal -1
    //     LanguagePrimitives.GenericComparison [3] [2] |> equal 1

    // testCase "LanguagePrimitives.GenericEquality works" <| fun () ->
    //     LanguagePrimitives.GenericEquality 111 111 |> equal true
    //     LanguagePrimitives.GenericEquality 222 333 |> equal false
    //     LanguagePrimitives.GenericEquality "1" "1" |> equal true
    //     LanguagePrimitives.GenericEquality "2" "3" |> equal false
    //     LanguagePrimitives.GenericEquality [1] [1] |> equal true
    //     LanguagePrimitives.GenericEquality [2] [3] |> equal false

    // testCase "LanguagePrimitives.PhysicalEquality works" <| fun () ->
    //     LanguagePrimitives.PhysicalEquality "1" "1" |> equal true
    //     LanguagePrimitives.PhysicalEquality "2" "3" |> equal false
    //     LanguagePrimitives.PhysicalEquality [1] [1] |> equal false
    //     LanguagePrimitives.PhysicalEquality [2] [3] |> equal false

  ]