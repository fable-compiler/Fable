module Fable.Tests.ComparisonTests

open Util.Testing
open System.Collections.Generic
open FSharp.Data.UnitSystems.SI.UnitSymbols

type UTest = A of int | B of int
type UFloatTest = AFloat of float | BFloat of float
type RTest = { a: int; b: int }
type RFloatTest = { a: float; b: int }
type RMutableFloatTest = { mutable value: float; tag: int }
type STest = struct val A: int; new(a: int) = { A = a }; end
type SFloatTest = struct val A: float; new(a: float) = { A = a }; end
type SMutableFloatTest = struct val mutable A: float; new(a: float) = { A = a }; end
type OTest(a) = member val A = a with get, set

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

// exception Ex of int

// [<ReferenceEquality>]
// type RTest2 = { a2: int; b2: int }

// type Test(i: int) =
//     member x.Value = i
//     override x.GetHashCode() = i
//     override x.Equals(yobj) =
//        match yobj with
//          | :? Test as y -> y.Value + 1 = x.Value
//          | _ -> false
//     interface System.IComparable with
//         member x.CompareTo(yobj) =
//             match yobj with
//             | :? Test as y -> compare (y.Value + 1) x.Value
//             | _ -> invalidArg "yobj" "cannot compare values of different types"
//             // | _ -> -1

//     interface System.IEquatable<Test> with
//         member x.Equals(y) =
//             y.Value + 1 = x.Value

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

// [<CustomEquality; NoComparison>]
// type FuzzyInt =
//     | FuzzyInt of int
//     override x.GetHashCode() =
//         let (FuzzyInt x) = x
//         x.GetHashCode()
//     override x.Equals(y: obj) =
//         match y with
//         | :? FuzzyInt as y ->
//             let (FuzzyInt x) = x
//             let (FuzzyInt y) = y
//             x - 2 <= y && y <= x + 2
//         | _ -> false

let genericEquals<'T when 'T: equality> (a: 'T) (b: 'T) : bool =
    let cmp = EqualityComparer<'T>.Default
    cmp.Equals(a, b)

let genericHash<'T when 'T: equality> (x: 'T) : int =
    let cmp = EqualityComparer<'T>.Default
    cmp.GetHashCode(x)

let genericCompare<'T when 'T: comparison> (a: 'T) (b: 'T) : int =
    let cmp = Comparer<'T>.Default
    cmp.Compare(a, b)

[<Fact>]
let ``Typed array equality works`` () =
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
let ``Array equality works`` () =
    let xs1 = [| "1"; "2"; "3" |]
    let xs2 = [| "1"; "2"; "3" |]
    let xs3 = [| "1"; "2"; "4" |]
    let xs4 = [| "1"; "2" |]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)
    equal true (xs1 <> xs4)

// [<Fact>]
// let ``Array custom equality works`` () =
//     let xs = [| FuzzyInt 3; FuzzyInt 5 |]
//     let ys = [| FuzzyInt 2; FuzzyInt 4 |]
//     let zs = [| FuzzyInt 2; FuzzyInt 8 |]
//     xs = ys |> equal true
//     xs = zs |> equal false

[<Fact>]
let ``Tuple equality works`` () =
    let xs1 = ( 1, 2, 3 )
    let xs2 = ( 1, 2, 3 )
    let xs3 = ( 1, 2, 4 )
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

[<Fact>]
let ``List equality works`` () =
    let xs1 = [ 1; 2; 3 ]
    let xs2 = [ 1; 2; 3 ]
    let xs3 = [ 1; 2; 4 ]
    equal true (xs1 = xs2)
    equal false (xs1 = xs3)
    equal true (xs1 <> xs3)
    equal false (xs1 <> xs2)

[<Fact>]
let ``Set equality works`` () =
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
let ``Map equality works`` () =
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
let ``Union equality works`` () =
    let u1 = A 2
    let u2 = A 2
    let u3 = A 4
    let u4 = B 2
    equal true (u1 = u2)
    equal false (u1 = u3)
    equal true (u1 <> u3)
    equal false (u1 <> u2)
    equal false (u1 = u4)
    System.Object.ReferenceEquals(u1, u1) |> equal true
    System.Object.ReferenceEquals(u1, u2) |> equal false

// [<Fact>]
// let ``Union custom equality works`` () =
//     let u1 = String "A"
//     let u2 = String "A"
//     let u3 = String "AA"
//     equal false (u1 = u2)
//     equal true (u1 = u3)

[<Fact>]
let ``Record equality works`` () =
    let r1 = { a = 1; b = 2 }
    let r2 = { a = 1; b = 2 }
    let r3 = { a = 1; b = 4 }
    equal true (r1 = r2)
    equal false (r1 = r3)
    equal true (r1 <> r3)
    equal false (r1 <> r2)
    System.Object.ReferenceEquals(r1, r1) |> equal true
    System.Object.ReferenceEquals(r1, r2) |> equal false

// [<Fact>]
// let ``Exception equality works`` () =
//     equal true ((Ex 1) = (Ex 1))
//     equal false ((Ex 1) = (Ex 2))

// TODO: If we want to make this work in Fable 2 we'll have
// to access reflection info for records

// #if FABLE_COMPILER
// [<Fact>]
// let ``Record equality ignores dynamic fields`` () =
//     let r1 = { a = 1; b = 2 }
//     r1?c <- 1
//     let r2 = { a = 1; b = 2 }
//     r2?c <- 2
//     equal true (r1 = r2)
//     equal false (r1 <> r2)

// [<Fact>]
// let ``Record comparison ignores dynamic fields`` () =
//     let r1 = { a = 1; b = 2 }
//     r1?c <- 1
//     let r2 = { a = 1; b = 2 }
//     r2?c <- 2
//     equal 0 (compare r1 r2)
// #endif

// [<Fact>]
// let ``Equality with objects implementing IEquatable works`` () =
//     let c1 = Test(5)
//     let c2 = Test(4)
//     let c3 = Test(5)
//     equal true (c1 = c2)
//     equal false (c1 = c3)
//     equal true (c1 <> c3)
//     equal false (c1 <> c2)
//     System.Object.ReferenceEquals(c1, c1) |> equal true
//     System.Object.ReferenceEquals(c1, c2) |> equal false

[<Fact>]
let ``Typed array comparison works`` () =
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
let ``Array comparison works`` () =
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
let ``Tuple comparison works`` () =
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
let ``List comparison works`` () =
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
let ``Set comparison works`` () =
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
let ``Map comparison works`` () =
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
let ``Union comparison works`` () =
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

// [<Fact>]
// let ``Union custom comparison works`` () =
//     let u1 = String "A"
//     let u2 = String "A"
//     let u3 = String "AA"
//     equal 0 (compare u1 u3)
//     equal true (compare u1 u2 > 0)

[<Fact>]
let ``Record comparison works`` () =
    let r1 = { a = 1; b = 2 }
    let r2 = { a = 1; b = 2 }
    let r3 = { a = 1; b = 4 }
    equal 0 (compare r1 r2)
    (compare r1 r3) = 0 |> equal false

// [<Fact>]
// let ``Comparison with objects implementing IComparable works`` () =
//     let c1 = Test(5)
//     let c2 = Test(4)
//     let c3 = Test(5)
//     equal 0 (compare c1 c2)
//     equal 1 (compare c1 c3)
//     equal true (c1 > c3)

[<Fact>]
let ``max works with primitives`` () =
    max 1 2 |> equal 2
    max 10m 2m |> equal 10m
    System.Math.Max(1, 2) |> equal 2
    max "a" "b" |> equal "b"

[<Fact>]
let ``max works with records`` () =
    let r1 = {a=1; b=1}
    let r2 = {a=1; b=2}
    max r1 r2 |> equal r2

// [<Fact>]
// let ``max with objects implementing IComparable works`` () =
//     let c1 = Test(5)
//     let c2 = Test(5)
//     System.Object.ReferenceEquals(max c1 c2, c1) |> equal true

[<Fact>]
let ``min works with primitives`` () =
    min 1 2 |> equal 1
    min 10m 2m |> equal 2m
    System.Math.Min(1, 2) |> equal 1
    min "a" "b" |> equal "a"

[<Fact>]
let ``min works with records`` () =
    let r1 = {a=1; b=1}
    let r2 = {a=1; b=2}
    min r1 r2 |> equal r1

// [<Fact>]
// let ``min with objects implementing IComparable works`` () =
//     let c1 = Test(5)
//     let c2 = Test(5)
//     System.Object.ReferenceEquals(min c1 c2, c2) |> equal true

// [<Fact>]
// let ``nullArg works`` () =
//     try
//         nullArg null
//         true
//     with _ex ->
//         false
//     |> equal false

// [<Fact>]
// let ``using function disposes the resource when action finishes`` () =
//     let mutable disposed = false
//     let resource = { new System.IDisposable with member _.Dispose() = disposed <- true }
//     using resource (fun _resource -> ())
//     equal true disposed

// [<Fact>]
// let ``using function disposes the resource when action fails`` () =
//     let mutable disposed = false
//     let resource = { new System.IDisposable with member _.Dispose() = disposed <- true }
//     try
//         using resource (fun _resource -> failwith "action failed")
//     with
//     | _ -> () // ignore
//     equal true disposed

// [<Fact>]
// let ``isNull with primitives works`` () =
//     isNull null |> equal true
//     isNull (box 5) |> equal false
//     isNull "" |> equal false
//     isNull "0" |> equal false
//     isNull "hello" |> equal false

// [<Fact>]
// let ``isNull with objects works`` () =
//     let s1: System.String = null
//     isNull s1 |> equal true
//     let s2: System.String = "hello"
//     isNull s2 |> equal false

// [<Fact>]
// let ``Classes must use identity hashing by default`` () = // See #2291
//     let x = MyClass(5)
//     let y = MyClass(5)
//     let h1 = hash(box x)
//     let h2 = hash(box y)
//     x.Value <- 8
//     let h3 = hash(box x)
//     h1 = h2 |> equal false
//     h1 = h3 |> equal true

[<Fact>]
let ``GetHashCode with arrays works`` () =
    let o1 = [|1; 2|]
    let o2 = [|1; 2|]
    let o3 = [|2; 1|]
    (o1.GetHashCode(), o1.GetHashCode()) ||> equal
    (o2.GetHashCode(), o1.GetHashCode()) ||> notEqual
    (o3.GetHashCode(), o1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with lists works`` () =
    ([1; 2].GetHashCode(), [1; 2].GetHashCode()) ||> equal
    ([2; 1].GetHashCode(), [1; 2].GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with tuples works`` () =
    ((1, 2).GetHashCode(), (1, 2).GetHashCode()) ||> equal
    ((2, 1).GetHashCode(), (1, 2).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with float tuples works`` () =
    ((0.0f, 1).GetHashCode(), (-0.0f, 1).GetHashCode()) ||> equal
    ((2.0f, 1).GetHashCode(), (1.0f, 1).GetHashCode()) ||> notEqual
    ((0.0, 1).GetHashCode(), (-0.0, 1).GetHashCode()) ||> equal
    ((2.0, 1).GetHashCode(), (1.0, 1).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with options works`` () =
    let None_0: int option option = Some None
    ((Some 1).GetHashCode(), (Some 1).GetHashCode()) ||> equal
    ((Some 2).GetHashCode(), (Some 1).GetHashCode()) ||> notEqual
    ((None_0).GetHashCode(), (Some 1).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with float options works`` () =
    let none32: float32 option option = Some None
    let none64: float option option = Some None
    ((Some 0.0f).GetHashCode(), (Some -0.0f).GetHashCode()) ||> equal
    ((Some 2.0f).GetHashCode(), (Some 1.0f).GetHashCode()) ||> notEqual
    (none32.GetHashCode(), (Some (Some 1.0f)).GetHashCode()) ||> notEqual
    ((Some 0.0).GetHashCode(), (Some -0.0).GetHashCode()) ||> equal
    ((Some 2.0).GetHashCode(), (Some 1.0).GetHashCode()) ||> notEqual
    (none64.GetHashCode(), (Some (Some 1.0)).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with results works`` () =
    let ok1: Result<int, int> = Ok 1
    let ok2: Result<int, int> = Ok 2
    let error1: Result<int, int> = Error 1
    (ok1.GetHashCode(), ok1.GetHashCode()) ||> equal
    (ok2.GetHashCode(), ok1.GetHashCode()) ||> notEqual
    (error1.GetHashCode(), ok1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with float results works`` () =
    let ok32Zero: Result<float32, float32> = Ok 0.0f
    let ok32NegZero: Result<float32, float32> = Ok -0.0f
    let ok32One: Result<float32, float32> = Ok 1.0f
    let ok32Two: Result<float32, float32> = Ok 2.0f
    let error32One: Result<float32, float32> = Error 1.0f
    let ok64Zero: Result<float, float> = Ok 0.0
    let ok64NegZero: Result<float, float> = Ok -0.0
    let ok64One: Result<float, float> = Ok 1.0
    let ok64Two: Result<float, float> = Ok 2.0
    let error64One: Result<float, float> = Error 1.0
    (ok32Zero.GetHashCode(), ok32NegZero.GetHashCode()) ||> equal
    (ok32Two.GetHashCode(), ok32One.GetHashCode()) ||> notEqual
    (error32One.GetHashCode(), ok32One.GetHashCode()) ||> notEqual
    (ok64Zero.GetHashCode(), ok64NegZero.GetHashCode()) ||> equal
    (ok64Two.GetHashCode(), ok64One.GetHashCode()) ||> notEqual
    (error64One.GetHashCode(), ok64One.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with unions works`` () =
    ((UTest.A 1).GetHashCode(), (UTest.A 1).GetHashCode()) ||> equal
    ((UTest.A 2).GetHashCode(), (UTest.A 1).GetHashCode()) ||> notEqual
    ((UTest.B 1).GetHashCode(), (UTest.A 1).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with float unions works`` () =
    ((UFloatTest.AFloat 0.0).GetHashCode(), (UFloatTest.AFloat -0.0).GetHashCode()) ||> equal
    ((UFloatTest.AFloat 2.0).GetHashCode(), (UFloatTest.AFloat 1.0).GetHashCode()) ||> notEqual
    ((UFloatTest.BFloat 1.0).GetHashCode(), (UFloatTest.AFloat 1.0).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with records works`` () =
    ({a=1; b=2}.GetHashCode(), {a=1; b=2}.GetHashCode()) ||> equal
    ({a=2; b=1}.GetHashCode(), {a=1; b=2}.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with float records works`` () =
    ({ a = 0.0; b = 1 }.GetHashCode(), { a = -0.0; b = 1 }.GetHashCode()) ||> equal
    ({ a = 2.0; b = 1 }.GetHashCode(), { a = 1.0; b = 1 }.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with mutable float records works`` () =
    ({ value = 0.0; tag = 1 }.GetHashCode(), { value = -0.0; tag = 1 }.GetHashCode()) ||> equal
    let record = { value = 1.0; tag = 1 }
    let h1 = record.GetHashCode()
    record.value <- 2.0
    let h2 = record.GetHashCode()
    (h1, h2) ||> notEqual
    h2 |> equal ({ value = 2.0; tag = 1 }.GetHashCode())

[<Fact>]
let ``GetHashCode with DateTime works`` () =
    let ticks = System.DateTime(2024, 1, 2, 3, 4, 5, System.DateTimeKind.Utc).Ticks
    let dt1 = System.DateTime(ticks, System.DateTimeKind.Utc)
    let dt2 = System.DateTime(ticks, System.DateTimeKind.Local)
    let dt3 = System.DateTime(ticks + 1L, System.DateTimeKind.Utc)
    dt1 = dt2 |> equal true
    (dt1.GetHashCode(), dt2.GetHashCode()) ||> equal
    dt1 = dt3 |> equal false
    (dt3.GetHashCode(), dt1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with DateTimeOffset works`` () =
    let dto1 = System.DateTimeOffset(2024, 1, 2, 3, 4, 5, System.TimeSpan.Zero)
    let dto2 = System.DateTimeOffset(2024, 1, 2, 4, 4, 5, System.TimeSpan.FromHours 1.0)
    let dto3 = System.DateTimeOffset(2024, 1, 2, 3, 4, 6, System.TimeSpan.Zero)
    dto1 = dto2 |> equal true
    (dto1.GetHashCode(), dto2.GetHashCode()) ||> equal
    dto1 = dto3 |> equal false
    (dto3.GetHashCode(), dto1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with DateOnly works`` () =
    let date1 = System.DateOnly(2024, 1, 2)
    let date2 = System.DateOnly.FromDayNumber(date1.DayNumber)
    let date3 = date1.AddDays 1
    date1 = date2 |> equal true
    (date1.GetHashCode(), date2.GetHashCode()) ||> equal
    date1 = date3 |> equal false
    (date3.GetHashCode(), date1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with TimeOnly works`` () =
    let time1 = System.TimeOnly(3, 4, 5)
    let time2 = System.TimeOnly.FromDateTime(System.DateTime(2024, 1, 2, 3, 4, 5))
    let time3 = time1.Add(System.TimeSpan.FromSeconds 1.0)
    time1 = time2 |> equal true
    (time1.GetHashCode(), time2.GetHashCode()) ||> equal
    time1 = time3 |> equal false
    (time3.GetHashCode(), time1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with decimal works`` () =
    let decimal1 = 1.0m
    let decimal2 = 1.00m
    let decimal3 = 1.01m
    decimal1 = decimal2 |> equal true
    (decimal1.GetHashCode(), decimal2.GetHashCode()) ||> equal
    decimal1 = decimal3 |> equal false
    (decimal3.GetHashCode(), decimal1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with bigint works`` () =
    let bigint1 = 123456789123456789123456789I
    let bigint2 = System.Numerics.BigInteger.Parse("123456789123456789123456789")
    let bigint3 = bigint1 + 1I
    bigint1 = bigint2 |> equal true
    (bigint1.GetHashCode(), bigint2.GetHashCode()) ||> equal
    bigint1 = bigint3 |> equal false
    (bigint3.GetHashCode(), bigint1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with Guid works`` () =
    let guid1 = System.Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let guid2 = System.Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let guid3 = System.Guid.Parse("6f1f2d4a-90e5-4d26-a49d-bc8938af0d0e")
    guid1 = guid2 |> equal true
    (guid1.GetHashCode(), guid2.GetHashCode()) ||> equal
    guid1 = guid3 |> equal false
    (guid3.GetHashCode(), guid1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with TimeSpan works`` () =
    let ts1 = System.TimeSpan.FromMinutes 90.0
    let ts2 = System.TimeSpan(1, 30, 0)
    let ts3 = System.TimeSpan(1, 31, 0)
    ts1 = ts2 |> equal true
    (ts1.GetHashCode(), ts2.GetHashCode()) ||> equal
    ts1 = ts3 |> equal false
    (ts3.GetHashCode(), ts1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with float sets works`` () =
    let s1 = Set [ 0.0; 1.0; 2.0 ]
    let s2 = Set [ 2.0; 1.0; -0.0 ]
    let s3 = Set [ 0.0; 1.0; 3.0 ]
    s1 = s2 |> equal true
    (s1.GetHashCode(), s2.GetHashCode()) ||> equal
    s1 = s3 |> equal false
    (s3.GetHashCode(), s1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with float maps works`` () =
    let m1 = Map [ (0.0, 1); (1.0, 2) ]
    let m2 = Map [ (1.0, 2); (-0.0, 1) ]
    let m3 = Map [ (0.0, 1); (1.0, 3) ]
    m1 = m2 |> equal true
    (m1.GetHashCode(), m2.GetHashCode()) ||> equal
    m1 = m3 |> equal false
    (m3.GetHashCode(), m1.GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with structs works`` () =
    (STest(1).GetHashCode(), STest(1).GetHashCode()) ||> equal
    (STest(2).GetHashCode(), STest(1).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with float structs works`` () =
    SFloatTest(0.0).GetHashCode() |> equal (SFloatTest(-0.0).GetHashCode())
    SFloatTest(2.0).GetHashCode() |> notEqual (SFloatTest(1.0).GetHashCode())

[<Fact>]
let ``GetHashCode with mutable float structs works`` () =
    SMutableFloatTest(0.0).GetHashCode() |> equal (SMutableFloatTest(-0.0).GetHashCode())
    let mutable s = SMutableFloatTest(1.0)
    let h1 = s.GetHashCode()
    s.A <- 2.0
    let h2 = s.GetHashCode()
    (h1, h2) ||> notEqual
    h2 |> equal (SMutableFloatTest(2.0).GetHashCode())

[<Fact>]
let ``GetHashCode with objects works`` () =
    let o1 = OTest(1)
    let o2 = OTest(1)
    let o3 = OTest(2)
    (o1.GetHashCode(), o1.GetHashCode()) ||> equal
    (o2.GetHashCode(), o1.GetHashCode()) ||> notEqual
    (o3.GetHashCode(), o1.GetHashCode()) ||> notEqual

// [<Fact>]
// let ``GetHashCode with objects that overwrite it works`` () =
//     (Test(1).GetHashCode(), Test(1).GetHashCode()) ||> equal
//     (Test(2).GetHashCode(), Test(1).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with same object works`` () =
    let o = OTest(1)
    let h1 = o.GetHashCode()
    o.A <- 2
    let h2 = o.GetHashCode()
    (h1, h2) ||> equal

[<Fact>]
let ``GetHashCode with longs works`` () =
    ((1L<<<33).GetHashCode(), (1L<<<33).GetHashCode()) ||> equal
    ((1L<<<34).GetHashCode(), (1L<<<33).GetHashCode()) ||> notEqual
    ((3L).GetHashCode(), (3L + (1L<<<33)).GetHashCode()) ||> notEqual
    ((-3L).GetHashCode(), (3L).GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with primitives works`` () =
    ((1).GetHashCode(), (1).GetHashCode()) ||> equal
    ((2).GetHashCode(), (1).GetHashCode()) ||> notEqual
    ("1".GetHashCode(), "1".GetHashCode()) ||> equal
    ("2".GetHashCode(), "1".GetHashCode()) ||> notEqual

[<Fact>]
let ``GetHashCode with floats works`` () =
    ((0.0f).GetHashCode(), (-0.0f).GetHashCode()) ||> equal
    ((2.0f).GetHashCode(), (1.0f).GetHashCode()) ||> notEqual
    ((0.0).GetHashCode(), (-0.0).GetHashCode()) ||> equal
    ((2.0).GetHashCode(), (1.0).GetHashCode()) ||> notEqual
    (System.Single.NaN.GetHashCode(), System.Single.NaN.GetHashCode()) ||> equal
    (System.Double.NaN.GetHashCode(), System.Double.NaN.GetHashCode()) ||> equal
    (System.Single.PositiveInfinity.GetHashCode(), System.Single.NegativeInfinity.GetHashCode())
    ||> notEqual
    (System.Double.PositiveInfinity.GetHashCode(), System.Double.NegativeInfinity.GetHashCode())
    ||> notEqual

[<Fact>]
let ``Equals with primitives works`` () =
    (true).Equals(true) |> equal true
    ('1').Equals('1') |> equal true
    (1y).Equals(1y) |> equal true
    (1uy).Equals(1uy) |> equal true
    (1s).Equals(1s) |> equal true
    (1).Equals(1) |> equal true
    (1L).Equals(1L) |> equal true
    (1u).Equals(1u) |> equal true
    (1us).Equals(1us) |> equal true
    (1ul).Equals(1ul) |> equal true
    (1uL).Equals(1uL) |> equal true
    (1.f).Equals(1.f) |> equal true
    (1.).Equals(1.) |> equal true
    (1.m).Equals(1.m) |> equal true
    ("1").Equals("1") |> equal true

[<Fact>]
let ``CompareTo with primitives works`` () =
    (true).CompareTo(true) |> equal 0
    ('1').CompareTo('1') |> equal 0
    (1y).CompareTo(1y) |> equal 0
    (1uy).CompareTo(1uy) |> equal 0
    (1s).CompareTo(1s) |> equal 0
    (1).CompareTo(1) |> equal 0
    (1L).CompareTo(1L) |> equal 0
    (1u).CompareTo(1u) |> equal 0
    (1us).CompareTo(1us) |> equal 0
    (1ul).CompareTo(1ul) |> equal 0
    (1uL).CompareTo(1uL) |> equal 0
    (1.f).CompareTo(1.f) |> equal 0
    (1.).CompareTo(1.) |> equal 0
    (1.m).CompareTo(1.m) |> equal 0
    ("1").CompareTo("1") |> equal 0

// // This is going to give different results in .NET and JS
// // Just check no exception is thrown
// [<Fact>]
// let ``hash works with JS objects`` () = // See #2281
//     let _ = obj () |> hash
//     ()

[<Fact>]
let ``hash with arrays works`` () =
    (hash [|1; 2|], hash [|1; 2|]) ||> equal
    (hash [|2; 1|], hash [|1; 2|]) ||> notEqual

[<Fact>]
let ``hash with lists works`` () =
    (hash [1; 2], hash [1; 2]) ||> equal
    (hash [2; 1], hash [1; 2]) ||> notEqual

[<Fact>]
let ``hash with tuples works`` () =
    (hash (1, 2), hash (1, 2)) ||> equal
    (hash (2, 1), hash (1, 2)) ||> notEqual

[<Fact>]
let ``hash with float tuples works`` () =
    (hash (0.0f, 1), hash (-0.0f, 1)) ||> equal
    (hash (2.0f, 1), hash (1.0f, 1)) ||> notEqual
    (hash (0.0, 1), hash (-0.0, 1)) ||> equal
    (hash (2.0, 1), hash (1.0, 1)) ||> notEqual

[<Fact>]
let ``hash with options works`` () =
    let None_0: int option option = Some None
    (hash (Some 1), hash (Some 1)) ||> equal
    (hash (Some 2), hash (Some 1)) ||> notEqual
    (hash (None_0), hash (Some 1)) ||> notEqual

[<Fact>]
let ``hash with float options works`` () =
    let none32: float32 option option = Some None
    let none64: float option option = Some None
    (hash (Some 0.0f), hash (Some -0.0f)) ||> equal
    (hash (Some 2.0f), hash (Some 1.0f)) ||> notEqual
    (hash none32, hash (Some (Some 1.0f))) ||> notEqual
    (hash (Some 0.0), hash (Some -0.0)) ||> equal
    (hash (Some 2.0), hash (Some 1.0)) ||> notEqual
    (hash none64, hash (Some (Some 1.0))) ||> notEqual

[<Fact>]
let ``hash with results works`` () =
    let ok1: Result<int, int> = Ok 1
    let ok2: Result<int, int> = Ok 2
    let error1: Result<int, int> = Error 1
    (hash ok1, hash ok1) ||> equal
    (hash ok2, hash ok1) ||> notEqual
    (hash error1, hash ok1) ||> notEqual

[<Fact>]
let ``hash with float results works`` () =
    let ok32Zero: Result<float32, float32> = Ok 0.0f
    let ok32NegZero: Result<float32, float32> = Ok -0.0f
    let ok32One: Result<float32, float32> = Ok 1.0f
    let ok32Two: Result<float32, float32> = Ok 2.0f
    let error32One: Result<float32, float32> = Error 1.0f
    let ok64Zero: Result<float, float> = Ok 0.0
    let ok64NegZero: Result<float, float> = Ok -0.0
    let ok64One: Result<float, float> = Ok 1.0
    let ok64Two: Result<float, float> = Ok 2.0
    let error64One: Result<float, float> = Error 1.0
    (hash ok32Zero, hash ok32NegZero) ||> equal
    (hash ok32Two, hash ok32One) ||> notEqual
    (hash error32One, hash ok32One) ||> notEqual
    (hash ok64Zero, hash ok64NegZero) ||> equal
    (hash ok64Two, hash ok64One) ||> notEqual
    (hash error64One, hash ok64One) ||> notEqual

[<Fact>]
let ``hash with unions works`` () =
    (hash (UTest.A 1), hash (UTest.A 1)) ||> equal
    (hash (UTest.A 2), hash (UTest.A 1)) ||> notEqual
    (hash (UTest.B 1), hash (UTest.A 1)) ||> notEqual

[<Fact>]
let ``hash with float unions works`` () =
    (hash (UFloatTest.AFloat 0.0), hash (UFloatTest.AFloat -0.0)) ||> equal
    (hash (UFloatTest.AFloat 2.0), hash (UFloatTest.AFloat 1.0)) ||> notEqual
    (hash (UFloatTest.BFloat 1.0), hash (UFloatTest.AFloat 1.0)) ||> notEqual

[<Fact>]
let ``hash with records works`` () =
    (hash {a=1; b=2}, hash {a=1; b=2}) ||> equal
    (hash {a=2; b=1}, hash {a=1; b=2}) ||> notEqual

[<Fact>]
let ``hash with float records works`` () =
    (hash { a = 0.0; b = 1 }, hash { a = -0.0; b = 1 }) ||> equal
    (hash { a = 2.0; b = 1 }, hash { a = 1.0; b = 1 }) ||> notEqual

[<Fact>]
let ``hash with mutable float records works`` () =
    (hash { value = 0.0; tag = 1 }, hash { value = -0.0; tag = 1 }) ||> equal
    let record = { value = 1.0; tag = 1 }
    let h1 = hash record
    record.value <- 2.0
    let h2 = hash record
    (h1, h2) ||> notEqual
    (h2, hash { value = 2.0; tag = 1 }) ||> equal

[<Fact>]
let ``hash with DateTime works`` () =
    let ticks = System.DateTime(2024, 1, 2, 3, 4, 5, System.DateTimeKind.Utc).Ticks
    let dt1 = System.DateTime(ticks, System.DateTimeKind.Utc)
    let dt2 = System.DateTime(ticks, System.DateTimeKind.Local)
    let dt3 = System.DateTime(ticks + 1L, System.DateTimeKind.Utc)
    dt1 = dt2 |> equal true
    (hash dt1, hash dt2) ||> equal
    dt1 = dt3 |> equal false
    (hash dt3, hash dt1) ||> notEqual

[<Fact>]
let ``hash with DateTimeOffset works`` () =
    let dto1 = System.DateTimeOffset(2024, 1, 2, 3, 4, 5, System.TimeSpan.Zero)
    let dto2 = System.DateTimeOffset(2024, 1, 2, 4, 4, 5, System.TimeSpan.FromHours 1.0)
    let dto3 = System.DateTimeOffset(2024, 1, 2, 3, 4, 6, System.TimeSpan.Zero)
    dto1 = dto2 |> equal true
    (hash dto1, hash dto2) ||> equal
    dto1 = dto3 |> equal false
    (hash dto3, hash dto1) ||> notEqual

[<Fact>]
let ``hash with DateOnly works`` () =
    let date1 = System.DateOnly(2024, 1, 2)
    let date2 = System.DateOnly.FromDayNumber(date1.DayNumber)
    let date3 = date1.AddDays 1
    date1 = date2 |> equal true
    (hash date1, hash date2) ||> equal
    date1 = date3 |> equal false
    (hash date3, hash date1) ||> notEqual

[<Fact>]
let ``hash with TimeOnly works`` () =
    let time1 = System.TimeOnly(3, 4, 5)
    let time2 = System.TimeOnly.FromDateTime(System.DateTime(2024, 1, 2, 3, 4, 5))
    let time3 = time1.Add(System.TimeSpan.FromSeconds 1.0)
    time1 = time2 |> equal true
    (hash time1, hash time2) ||> equal
    time1 = time3 |> equal false
    (hash time3, hash time1) ||> notEqual

[<Fact>]
let ``hash with decimal works`` () =
    let decimal1 = 1.0m
    let decimal2 = 1.00m
    let decimal3 = 1.01m
    decimal1 = decimal2 |> equal true
    (hash decimal1, hash decimal2) ||> equal
    decimal1 = decimal3 |> equal false
    (hash decimal3, hash decimal1) ||> notEqual

[<Fact>]
let ``hash with bigint works`` () =
    let bigint1 = 123456789123456789123456789I
    let bigint2 = System.Numerics.BigInteger.Parse("123456789123456789123456789")
    let bigint3 = bigint1 + 1I
    bigint1 = bigint2 |> equal true
    (hash bigint1, hash bigint2) ||> equal
    bigint1 = bigint3 |> equal false
    (hash bigint3, hash bigint1) ||> notEqual

[<Fact>]
let ``hash with Guid works`` () =
    let guid1 = System.Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let guid2 = System.Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let guid3 = System.Guid.Parse("6f1f2d4a-90e5-4d26-a49d-bc8938af0d0e")
    guid1 = guid2 |> equal true
    (hash guid1, hash guid2) ||> equal
    guid1 = guid3 |> equal false
    (hash guid3, hash guid1) ||> notEqual

[<Fact>]
let ``hash with TimeSpan works`` () =
    let ts1 = System.TimeSpan.FromMinutes 90.0
    let ts2 = System.TimeSpan(1, 30, 0)
    let ts3 = System.TimeSpan(1, 31, 0)
    ts1 = ts2 |> equal true
    (hash ts1, hash ts2) ||> equal
    ts1 = ts3 |> equal false
    (hash ts3, hash ts1) ||> notEqual

[<Fact>]
let ``hash with float sets works`` () =
    let s1 = Set [ 0.0; 1.0; 2.0 ]
    let s2 = Set [ 2.0; 1.0; -0.0 ]
    let s3 = Set [ 0.0; 1.0; 3.0 ]
    s1 = s2 |> equal true
    (hash s1, hash s2) ||> equal
    s1 = s3 |> equal false
    (hash s3, hash s1) ||> notEqual

[<Fact>]
let ``hash with float maps works`` () =
    let m1 = Map [ (0.0, 1); (1.0, 2) ]
    let m2 = Map [ (1.0, 2); (-0.0, 1) ]
    let m3 = Map [ (0.0, 1); (1.0, 3) ]
    m1 = m2 |> equal true
    (hash m1, hash m2) ||> equal
    m1 = m3 |> equal false
    (hash m3, hash m1) ||> notEqual

[<Fact>]
let ``hash with structs works`` () =
    (hash (STest(1)), hash (STest(1))) ||> equal
    (hash (STest(2)), hash (STest(1))) ||> notEqual

[<Fact>]
let ``hash with float structs works`` () =
    (hash (SFloatTest(0.0)), hash (SFloatTest(-0.0))) ||> equal
    (hash (SFloatTest(2.0)), hash (SFloatTest(1.0))) ||> notEqual

[<Fact>]
let ``hash with mutable float structs works`` () =
    (hash (SMutableFloatTest(0.0)), hash (SMutableFloatTest(-0.0))) ||> equal
    let mutable s = SMutableFloatTest(1.0)
    let h1 = hash s
    s.A <- 2.0
    let h2 = hash s
    (h1, h2) ||> notEqual
    (h2, hash (SMutableFloatTest(2.0))) ||> equal

[<Fact>]
let ``hash with objects works`` () =
    // In Release mode for Rust, sequentially allocated objects that
    // are immediately released can get allocated at the same address.
    // This breaks referential equality, so delaying their release by
    // increasing their scope makes it work. See ReferenceEquals tests.
    //
    // (hash (OTest(1)), hash (OTest(1))) ||> notEqual // broken in Release mode
    // (hash (OTest(2)), hash (OTest(1))) ||> notEqual // broken in Release mode
    let o1 = OTest(1)
    let o2 = OTest(1)
    let o3 = OTest(2)
    (hash o1, hash o1) ||> equal
    (hash o2, hash o1) ||> notEqual
    (hash o3, hash o1) ||> notEqual

[<Fact>]
let ``hash with same object works`` () =
    let o = OTest(1)
    let h1 = hash o
    o.A <- 2
    let h2 = hash o
    (h1, h2) ||> equal

[<Fact>]
let ``hash with longs works`` () =
    (hash (1L<<<33), hash (1L<<<33)) ||> equal
    (hash (1L<<<34), hash (1L<<<33)) ||> notEqual
    (hash 3L, hash (3L + (1L<<<33))) ||> notEqual
    (hash (-3L), hash (3L))          ||> notEqual

[<Fact>]
let ``hash with primitives works`` () =
    (hash 111, hash 111) ||> equal
    (hash 222, hash 111) ||> notEqual
    (hash "1", hash "1") ||> equal
    (hash "2", hash "1") ||> notEqual

[<Fact>]
let ``hash with floats works`` () =
    (hash 0.0f, hash -0.0f) ||> equal
    (hash 2.0f, hash 1.0f) ||> notEqual
    (hash 0.0, hash -0.0) ||> equal
    (hash 2.0, hash 1.0) ||> notEqual
    (hash System.Single.NaN, hash System.Single.NaN) ||> equal
    (hash System.Double.NaN, hash System.Double.NaN) ||> equal
    (hash System.Single.PositiveInfinity, hash System.Single.NegativeInfinity) ||> notEqual
    (hash System.Double.PositiveInfinity, hash System.Double.NegativeInfinity) ||> notEqual

[<Fact>]
let ``Unchecked.hash with arrays works`` () =
    (Unchecked.hash [|1;2|], Unchecked.hash [|1;2|]) ||> equal
    (Unchecked.hash [|2;1|], Unchecked.hash [|1;2|]) ||> notEqual

[<Fact>]
let ``Unchecked.hash with lists works`` () =
    (Unchecked.hash [1;2], Unchecked.hash [1;2]) ||> equal
    (Unchecked.hash [2;1], Unchecked.hash [1;2]) ||> notEqual

[<Fact>]
let ``Unchecked.hash with tuples works`` () =
    (Unchecked.hash (1,2), Unchecked.hash (1,2)) ||> equal
    (Unchecked.hash (2,1), Unchecked.hash (1,2)) ||> notEqual

[<Fact>]
let ``Unchecked.hash with float tuples works`` () =
    (Unchecked.hash (0.0f, 1), Unchecked.hash (-0.0f, 1)) ||> equal
    (Unchecked.hash (2.0f, 1), Unchecked.hash (1.0f, 1)) ||> notEqual
    (Unchecked.hash (0.0, 1), Unchecked.hash (-0.0, 1)) ||> equal
    (Unchecked.hash (2.0, 1), Unchecked.hash (1.0, 1)) ||> notEqual

[<Fact>]
let ``Unchecked.hash with options works`` () =
    let None_0: int option option = Some None
    (Unchecked.hash (Some 1), Unchecked.hash (Some 1)) ||> equal
    (Unchecked.hash (Some 2), Unchecked.hash (Some 1)) ||> notEqual
    (Unchecked.hash (None_0), Unchecked.hash (Some 1)) ||> notEqual

[<Fact>]
let ``Unchecked.hash with float options works`` () =
    let none32: float32 option option = Some None
    let none64: float option option = Some None
    (Unchecked.hash (Some 0.0f), Unchecked.hash (Some -0.0f)) ||> equal
    (Unchecked.hash (Some 2.0f), Unchecked.hash (Some 1.0f)) ||> notEqual
    (Unchecked.hash none32, Unchecked.hash (Some (Some 1.0f))) ||> notEqual
    (Unchecked.hash (Some 0.0), Unchecked.hash (Some -0.0)) ||> equal
    (Unchecked.hash (Some 2.0), Unchecked.hash (Some 1.0)) ||> notEqual
    (Unchecked.hash none64, Unchecked.hash (Some (Some 1.0))) ||> notEqual

[<Fact>]
let ``Unchecked.hash with results works`` () =
    let ok1: Result<int, int> = Ok 1
    let ok2: Result<int, int> = Ok 2
    let error1: Result<int, int> = Error 1
    (Unchecked.hash ok1, Unchecked.hash ok1) ||> equal
    (Unchecked.hash ok2, Unchecked.hash ok1) ||> notEqual
    (Unchecked.hash error1, Unchecked.hash ok1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with float results works`` () =
    let ok32Zero: Result<float32, float32> = Ok 0.0f
    let ok32NegZero: Result<float32, float32> = Ok -0.0f
    let ok32One: Result<float32, float32> = Ok 1.0f
    let ok32Two: Result<float32, float32> = Ok 2.0f
    let error32One: Result<float32, float32> = Error 1.0f
    let ok64Zero: Result<float, float> = Ok 0.0
    let ok64NegZero: Result<float, float> = Ok -0.0
    let ok64One: Result<float, float> = Ok 1.0
    let ok64Two: Result<float, float> = Ok 2.0
    let error64One: Result<float, float> = Error 1.0
    (Unchecked.hash ok32Zero, Unchecked.hash ok32NegZero) ||> equal
    (Unchecked.hash ok32Two, Unchecked.hash ok32One) ||> notEqual
    (Unchecked.hash error32One, Unchecked.hash ok32One) ||> notEqual
    (Unchecked.hash ok64Zero, Unchecked.hash ok64NegZero) ||> equal
    (Unchecked.hash ok64Two, Unchecked.hash ok64One) ||> notEqual
    (Unchecked.hash error64One, Unchecked.hash ok64One) ||> notEqual

[<Fact>]
let ``Unchecked.hash with unions works`` () =
    (Unchecked.hash (UTest.A 1), Unchecked.hash (UTest.A 1)) ||> equal
    (Unchecked.hash (UTest.A 2), Unchecked.hash (UTest.A 1)) ||> notEqual
    (Unchecked.hash (UTest.B 1), Unchecked.hash (UTest.A 1)) ||> notEqual

[<Fact>]
let ``Unchecked.hash with float unions works`` () =
    (Unchecked.hash (UFloatTest.AFloat 0.0), Unchecked.hash (UFloatTest.AFloat -0.0)) ||> equal
    (Unchecked.hash (UFloatTest.AFloat 2.0), Unchecked.hash (UFloatTest.AFloat 1.0)) ||> notEqual
    (Unchecked.hash (UFloatTest.BFloat 1.0), Unchecked.hash (UFloatTest.AFloat 1.0)) ||> notEqual

[<Fact>]
let ``Unchecked.hash with records works`` () =
    (Unchecked.hash {a=1; b=2}, Unchecked.hash {a=1; b=2}) ||> equal
    (Unchecked.hash {a=2; b=1}, Unchecked.hash {a=1; b=2}) ||> notEqual

[<Fact>]
let ``Unchecked.hash with float records works`` () =
    (Unchecked.hash { a = 0.0; b = 1 }, Unchecked.hash { a = -0.0; b = 1 }) ||> equal
    (Unchecked.hash { a = 2.0; b = 1 }, Unchecked.hash { a = 1.0; b = 1 }) ||> notEqual

[<Fact>]
let ``Unchecked.hash with mutable float records works`` () =
    (Unchecked.hash { value = 0.0; tag = 1 }, Unchecked.hash { value = -0.0; tag = 1 }) ||> equal
    let record = { value = 1.0; tag = 1 }
    let h1 = Unchecked.hash record
    record.value <- 2.0
    let h2 = Unchecked.hash record
    (h1, h2) ||> notEqual
    (h2, Unchecked.hash { value = 2.0; tag = 1 }) ||> equal

[<Fact>]
let ``Unchecked.hash with DateTime works`` () =
    let ticks = System.DateTime(2024, 1, 2, 3, 4, 5, System.DateTimeKind.Utc).Ticks
    let dt1 = System.DateTime(ticks, System.DateTimeKind.Utc)
    let dt2 = System.DateTime(ticks, System.DateTimeKind.Local)
    let dt3 = System.DateTime(ticks + 1L, System.DateTimeKind.Utc)
    dt1 = dt2 |> equal true
    (Unchecked.hash dt1, Unchecked.hash dt2) ||> equal
    dt1 = dt3 |> equal false
    (Unchecked.hash dt3, Unchecked.hash dt1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with DateTimeOffset works`` () =
    let dto1 = System.DateTimeOffset(2024, 1, 2, 3, 4, 5, System.TimeSpan.Zero)
    let dto2 = System.DateTimeOffset(2024, 1, 2, 4, 4, 5, System.TimeSpan.FromHours 1.0)
    let dto3 = System.DateTimeOffset(2024, 1, 2, 3, 4, 6, System.TimeSpan.Zero)
    dto1 = dto2 |> equal true
    (Unchecked.hash dto1, Unchecked.hash dto2) ||> equal
    dto1 = dto3 |> equal false
    (Unchecked.hash dto3, Unchecked.hash dto1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with DateOnly works`` () =
    let date1 = System.DateOnly(2024, 1, 2)
    let date2 = System.DateOnly.FromDayNumber(date1.DayNumber)
    let date3 = date1.AddDays 1
    date1 = date2 |> equal true
    (Unchecked.hash date1, Unchecked.hash date2) ||> equal
    date1 = date3 |> equal false
    (Unchecked.hash date3, Unchecked.hash date1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with TimeOnly works`` () =
    let time1 = System.TimeOnly(3, 4, 5)
    let time2 = System.TimeOnly.FromDateTime(System.DateTime(2024, 1, 2, 3, 4, 5))
    let time3 = time1.Add(System.TimeSpan.FromSeconds 1.0)
    time1 = time2 |> equal true
    (Unchecked.hash time1, Unchecked.hash time2) ||> equal
    time1 = time3 |> equal false
    (Unchecked.hash time3, Unchecked.hash time1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with decimal works`` () =
    let decimal1 = 1.0m
    let decimal2 = 1.00m
    let decimal3 = 1.01m
    decimal1 = decimal2 |> equal true
    (Unchecked.hash decimal1, Unchecked.hash decimal2) ||> equal
    decimal1 = decimal3 |> equal false
    (Unchecked.hash decimal3, Unchecked.hash decimal1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with bigint works`` () =
    let bigint1 = 123456789123456789123456789I
    let bigint2 = System.Numerics.BigInteger.Parse("123456789123456789123456789")
    let bigint3 = bigint1 + 1I
    bigint1 = bigint2 |> equal true
    (Unchecked.hash bigint1, Unchecked.hash bigint2) ||> equal
    bigint1 = bigint3 |> equal false
    (Unchecked.hash bigint3, Unchecked.hash bigint1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with Guid works`` () =
    let guid1 = System.Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let guid2 = System.Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let guid3 = System.Guid.Parse("6f1f2d4a-90e5-4d26-a49d-bc8938af0d0e")
    guid1 = guid2 |> equal true
    (Unchecked.hash guid1, Unchecked.hash guid2) ||> equal
    guid1 = guid3 |> equal false
    (Unchecked.hash guid3, Unchecked.hash guid1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with TimeSpan works`` () =
    let ts1 = System.TimeSpan.FromMinutes 90.0
    let ts2 = System.TimeSpan(1, 30, 0)
    let ts3 = System.TimeSpan(1, 31, 0)
    ts1 = ts2 |> equal true
    (Unchecked.hash ts1, Unchecked.hash ts2) ||> equal
    ts1 = ts3 |> equal false
    (Unchecked.hash ts3, Unchecked.hash ts1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with float sets works`` () =
    let s1 = Set [ 0.0; 1.0; 2.0 ]
    let s2 = Set [ 2.0; 1.0; -0.0 ]
    let s3 = Set [ 0.0; 1.0; 3.0 ]
    s1 = s2 |> equal true
    (Unchecked.hash s1, Unchecked.hash s2) ||> equal
    s1 = s3 |> equal false
    (Unchecked.hash s3, Unchecked.hash s1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with float maps works`` () =
    let m1 = Map [ (0.0, 1); (1.0, 2) ]
    let m2 = Map [ (1.0, 2); (-0.0, 1) ]
    let m3 = Map [ (0.0, 1); (1.0, 3) ]
    m1 = m2 |> equal true
    (Unchecked.hash m1, Unchecked.hash m2) ||> equal
    m1 = m3 |> equal false
    (Unchecked.hash m3, Unchecked.hash m1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with structs works`` () =
    (Unchecked.hash (STest(1)), Unchecked.hash (STest(1))) ||> equal
    (Unchecked.hash (STest(2)), Unchecked.hash (STest(1))) ||> notEqual

[<Fact>]
let ``Unchecked.hash with float structs works`` () =
    (Unchecked.hash (SFloatTest(0.0)), Unchecked.hash (SFloatTest(-0.0))) ||> equal
    (Unchecked.hash (SFloatTest(2.0)), Unchecked.hash (SFloatTest(1.0))) ||> notEqual

[<Fact>]
let ``Unchecked.hash with mutable float structs works`` () =
    (Unchecked.hash (SMutableFloatTest(0.0)), Unchecked.hash (SMutableFloatTest(-0.0))) ||> equal
    let mutable s = SMutableFloatTest(1.0)
    let h1 = Unchecked.hash s
    s.A <- 2.0
    let h2 = Unchecked.hash s
    (h1, h2) ||> notEqual
    (h2, Unchecked.hash (SMutableFloatTest(2.0))) ||> equal

[<Fact>]
let ``Unchecked.hash with objects works`` () =
    // In Release mode for Rust, sequentially allocated objects that
    // are immediately released can get allocated at the same address.
    // This breaks referential equality, so delaying their release by
    // increasing their scope makes it work. See ReferenceEquals tests.
    let o1 = OTest(1)
    let o2 = OTest(1)
    let o3 = OTest(2)
    (Unchecked.hash o1, Unchecked.hash o1) ||> equal
    (Unchecked.hash o2, Unchecked.hash o1) ||> notEqual
    (Unchecked.hash o3, Unchecked.hash o1) ||> notEqual

[<Fact>]
let ``Unchecked.hash with same object works`` () =
    let o = OTest(1)
    let h1 = Unchecked.hash o
    o.A <- 2
    let h2 = Unchecked.hash o
    (h1, h2) ||> equal

[<Fact>]
let ``Unchecked.hash with longs works`` () =
    (Unchecked.hash (1L<<<33), Unchecked.hash (1L<<<33)) ||> equal
    (Unchecked.hash (1L<<<34), Unchecked.hash (1L<<<33)) ||> notEqual
    (Unchecked.hash 3L, Unchecked.hash (3L + (1L<<<33))) ||> notEqual
    (Unchecked.hash (-3L), Unchecked.hash (3L)) ||> notEqual

[<Fact>]
let ``Unchecked.hash with primitives works`` () =
    (Unchecked.hash 111, Unchecked.hash 111) ||> equal
    (Unchecked.hash 222, Unchecked.hash 333) ||> notEqual
    (Unchecked.hash "1", Unchecked.hash "1") ||> equal
    (Unchecked.hash "2", Unchecked.hash "3") ||> notEqual

[<Fact>]
let ``Unchecked.hash with floats works`` () =
    (Unchecked.hash 0.0f, Unchecked.hash -0.0f) ||> equal
    (Unchecked.hash 2.0f, Unchecked.hash 1.0f) ||> notEqual
    (Unchecked.hash 0.0, Unchecked.hash -0.0) ||> equal
    (Unchecked.hash 2.0, Unchecked.hash 1.0) ||> notEqual
    (Unchecked.hash System.Single.NaN, Unchecked.hash System.Single.NaN) ||> equal
    (Unchecked.hash System.Double.NaN, Unchecked.hash System.Double.NaN) ||> equal
    (Unchecked.hash System.Single.PositiveInfinity, Unchecked.hash System.Single.NegativeInfinity)
    ||> notEqual
    (Unchecked.hash System.Double.PositiveInfinity, Unchecked.hash System.Double.NegativeInfinity)
    ||> notEqual

[<Fact>]
let ``Unchecked.equals works`` () =
    Unchecked.equals 111 111 |> equal true
    Unchecked.equals 222 333 |> equal false
    Unchecked.equals "1" "1" |> equal true
    Unchecked.equals "2" "3" |> equal false
    Unchecked.equals [1] [1] |> equal true
    Unchecked.equals [2] [3] |> equal false

[<Fact>]
let ``Unchecked.compare works`` () =
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
let ``DU comparison works`` () =
    let hasStatusReached expectedStatus status =
        status >= expectedStatus
    Status.CreateNewMeterReadingPicture >= Status.SelectingNewDevice
    |> equal true
    hasStatusReached Status.SelectingNewDevice Status.CreateNewMeterReadingPicture
    |> equal true

[<Fact>]
let ``LanguagePrimitives.GenericHash with primitives works`` () =
    (LanguagePrimitives.GenericHash 111, LanguagePrimitives.GenericHash 111) ||> equal
    (LanguagePrimitives.GenericHash 222, LanguagePrimitives.GenericHash 111) ||> notEqual
    (LanguagePrimitives.GenericHash "1", LanguagePrimitives.GenericHash "1") ||> equal
    (LanguagePrimitives.GenericHash "2", LanguagePrimitives.GenericHash "1") ||> notEqual

[<Fact>]
let ``LanguagePrimitives.GenericHash with floats works`` () =
    (LanguagePrimitives.GenericHash 0.0f, LanguagePrimitives.GenericHash -0.0f) ||> equal
    (LanguagePrimitives.GenericHash 2.0f, LanguagePrimitives.GenericHash 1.0f) ||> notEqual
    (LanguagePrimitives.GenericHash 0.0, LanguagePrimitives.GenericHash -0.0) ||> equal
    (LanguagePrimitives.GenericHash 2.0, LanguagePrimitives.GenericHash 1.0) ||> notEqual
    (LanguagePrimitives.GenericHash System.Single.NaN, LanguagePrimitives.GenericHash System.Single.NaN)
    ||> equal
    (LanguagePrimitives.GenericHash System.Double.NaN, LanguagePrimitives.GenericHash System.Double.NaN)
    ||> equal
    (LanguagePrimitives.GenericHash System.Single.PositiveInfinity, LanguagePrimitives.GenericHash System.Single.NegativeInfinity)
    ||> notEqual
    (LanguagePrimitives.GenericHash System.Double.PositiveInfinity, LanguagePrimitives.GenericHash System.Double.NegativeInfinity)
    ||> notEqual

[<Fact>]
let ``LanguagePrimitives.GenericHash with lists works`` () =
    (LanguagePrimitives.GenericHash [1;2], LanguagePrimitives.GenericHash [1;2]) ||> equal
    (LanguagePrimitives.GenericHash [2;1], LanguagePrimitives.GenericHash [1;2]) ||> notEqual

[<Fact>]
let ``LanguagePrimitives.GenericHash with arrays works`` () =
    (LanguagePrimitives.GenericHash [|1;2|], LanguagePrimitives.GenericHash [|1;2|]) ||> equal
    (LanguagePrimitives.GenericHash [|2;1|], LanguagePrimitives.GenericHash [|1;2|]) ||> notEqual

[<Fact>]
let ``LanguagePrimitives.GenericHash with tuples works`` () =
    (LanguagePrimitives.GenericHash (1,2), LanguagePrimitives.GenericHash (1,2)) ||> equal
    (LanguagePrimitives.GenericHash (2,1), LanguagePrimitives.GenericHash (1,2)) ||> notEqual

[<Fact>]
let ``LanguagePrimitives.GenericHash with float tuples works`` () =
    (LanguagePrimitives.GenericHash (0.0f, 1), LanguagePrimitives.GenericHash (-0.0f, 1)) ||> equal
    (LanguagePrimitives.GenericHash (2.0f, 1), LanguagePrimitives.GenericHash (1.0f, 1)) ||> notEqual
    (LanguagePrimitives.GenericHash (0.0, 1), LanguagePrimitives.GenericHash (-0.0, 1)) ||> equal
    (LanguagePrimitives.GenericHash (2.0, 1), LanguagePrimitives.GenericHash (1.0, 1)) ||> notEqual

[<Fact>]
let ``LanguagePrimitives.PhysicalHash with primitives works`` () =
    (LanguagePrimitives.PhysicalHash "1", LanguagePrimitives.PhysicalHash "1") ||> equal
    (LanguagePrimitives.PhysicalHash "2", LanguagePrimitives.PhysicalHash "1") ||> notEqual

[<Fact>]
let ``LanguagePrimitives.PhysicalHash with lists works`` () =
    let o1 = [1; 2]
    let o2 = [1; 2]
    let o3 = [2; 1]
    (LanguagePrimitives.PhysicalHash o1, LanguagePrimitives.PhysicalHash o1) ||> equal
    (LanguagePrimitives.PhysicalHash o2, LanguagePrimitives.PhysicalHash o1) ||> notEqual
    (LanguagePrimitives.PhysicalHash o3, LanguagePrimitives.PhysicalHash o1) ||> notEqual

[<Fact>]
let ``LanguagePrimitives.PhysicalHash with arrays works`` () =
    let o1 = [|1; 2|]
    let o2 = [|1; 2|]
    let o3 = [|2; 1|]
    (LanguagePrimitives.PhysicalHash o1, LanguagePrimitives.PhysicalHash o1) ||> equal
    (LanguagePrimitives.PhysicalHash o2, LanguagePrimitives.PhysicalHash o1) ||> notEqual
    (LanguagePrimitives.PhysicalHash o3, LanguagePrimitives.PhysicalHash o1) ||> notEqual

[<Fact>]
let ``LanguagePrimitives.PhysicalHash with tuples works`` () =
    let o1 = (1, 2)
    let o2 = (1, 2)
    let o3 = (2, 1)
    (LanguagePrimitives.PhysicalHash o1, LanguagePrimitives.PhysicalHash o1) ||> equal
    (LanguagePrimitives.PhysicalHash o2, LanguagePrimitives.PhysicalHash o1) ||> notEqual
    (LanguagePrimitives.PhysicalHash o3, LanguagePrimitives.PhysicalHash o1) ||> notEqual

[<Fact>]
let ``LanguagePrimitives.GenericComparison works`` () =
    LanguagePrimitives.GenericComparison 111 111 |> equal 0
    LanguagePrimitives.GenericComparison 222 333 |> equal -1
    LanguagePrimitives.GenericComparison 333 222 |> equal 1
    LanguagePrimitives.GenericComparison "1" "1" |> equal 0
    LanguagePrimitives.GenericComparison "2" "3" |> equal -1
    LanguagePrimitives.GenericComparison "3" "2" |> equal 1
    LanguagePrimitives.GenericComparison [1] [1] |> equal 0
    LanguagePrimitives.GenericComparison [2] [3] |> equal -1
    LanguagePrimitives.GenericComparison [3] [2] |> equal 1

[<Fact>]
let ``LanguagePrimitives.GenericEquality works`` () =
    LanguagePrimitives.GenericEquality 111 111 |> equal true
    LanguagePrimitives.GenericEquality 222 333 |> equal false
    LanguagePrimitives.GenericEquality "1" "1" |> equal true
    LanguagePrimitives.GenericEquality "2" "3" |> equal false
    LanguagePrimitives.GenericEquality [1] [1] |> equal true
    LanguagePrimitives.GenericEquality [2] [3] |> equal false

[<Fact>]
let ``LanguagePrimitives.PhysicalEquality works`` () =
    LanguagePrimitives.PhysicalEquality "1" "1" |> equal true
    LanguagePrimitives.PhysicalEquality "2" "3" |> equal false
    LanguagePrimitives.PhysicalEquality [1] [1] |> equal false
    LanguagePrimitives.PhysicalEquality [2] [3] |> equal false

[<Fact>]
let ``LanguagePrimitives.SByteWithMeasure works`` () =
    let distance: sbyte<m> = LanguagePrimitives.SByteWithMeasure 1y
    distance |> equal 1y<m>

[<Fact>]
let ``LanguagePrimitives.Int16WithMeasure works`` () =
    let distance: int16<m> = LanguagePrimitives.Int16WithMeasure 1s
    distance |> equal 1s<m>

[<Fact>]
let ``LanguagePrimitives.Int32WithMeasure works`` () =
    let distance: int<m> = LanguagePrimitives.Int32WithMeasure 1
    distance |> equal 1<m>

[<Fact>]
let ``LanguagePrimitives.Int64WithMeasure works`` () =
    let distance: int64<m> = LanguagePrimitives.Int64WithMeasure 1L
    distance |> equal 1L<m>

[<Fact>]
let ``LanguagePrimitives.Float32WithMeasure works`` () =
    let distance: float32<m> = LanguagePrimitives.Float32WithMeasure 1.0f
    distance |> equal 1.0f<m>

[<Fact>]
let ``LanguagePrimitives.FloatWithMeasure works`` () =
    let distance: float<m> = LanguagePrimitives.FloatWithMeasure 1.0
    distance |> equal 1.0<m>

[<Fact>]
let ``LanguagePrimitives.DecimalWithMeasure works`` () =
    let distance: decimal<m> = LanguagePrimitives.DecimalWithMeasure 1.0m
    distance |> equal 1.0m<m>

[<Fact>]
let ``EqualityComparer.Create works`` () =
    let cmp = EqualityComparer.Create((<>), hash)
    cmp.Equals(1, 1) |> equal false
    cmp.Equals(1, 2) |> equal true

[<Fact>]
let ``EqualityComparer.Equals works`` () =
    genericEquals 1 1 |> equal true
    genericEquals 1 2 |> equal false
    genericEquals "1" "1" |> equal true
    genericEquals "1" "2" |> equal false

[<Fact>]
let ``EqualityComparer.GetHashCode with arrays works`` () =
    let o1 = [|1; 2|]
    let o2 = [|1; 2|]
    let o3 = [|2; 1|]
    (genericHash o1, genericHash o1) ||> equal
    (genericHash o2, genericHash o2) ||> equal
    (genericHash o3, genericHash o1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with lists works`` () =
    (genericHash [1; 2], genericHash [1; 2]) ||> equal
    (genericHash [2; 1], genericHash [1; 2]) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with tuples works`` () =
    (genericHash (1, 2), genericHash (1, 2)) ||> equal
    (genericHash (2, 1), genericHash (1, 2)) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with float tuples works`` () =
    (genericHash (0.0f, 1), genericHash (-0.0f, 1)) ||> equal
    (genericHash (2.0f, 1), genericHash (1.0f, 1)) ||> notEqual
    (genericHash (0.0, 1), genericHash (-0.0, 1)) ||> equal
    (genericHash (2.0, 1), genericHash (1.0, 1)) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with options works`` () =
    let None_0: int option option = Some None
    (genericHash (Some 1), genericHash (Some 1)) ||> equal
    (genericHash (Some 2), genericHash (Some 1)) ||> notEqual
    (genericHash (None_0), genericHash (Some 1)) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with float options works`` () =
    let none32: float32 option option = Some None
    let none64: float option option = Some None
    (genericHash (Some 0.0f), genericHash (Some -0.0f)) ||> equal
    (genericHash (Some 2.0f), genericHash (Some 1.0f)) ||> notEqual
    (genericHash none32, genericHash (Some (Some 1.0f))) ||> notEqual
    (genericHash (Some 0.0), genericHash (Some -0.0)) ||> equal
    (genericHash (Some 2.0), genericHash (Some 1.0)) ||> notEqual
    (genericHash none64, genericHash (Some (Some 1.0))) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with results works`` () =
    let ok1: Result<int, int> = Ok 1
    let ok2: Result<int, int> = Ok 2
    let error1: Result<int, int> = Error 1
    (genericHash ok1, genericHash ok1) ||> equal
    (genericHash ok2, genericHash ok1) ||> notEqual
    (genericHash error1, genericHash ok1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with float results works`` () =
    let ok32Zero: Result<float32, float32> = Ok 0.0f
    let ok32NegZero: Result<float32, float32> = Ok -0.0f
    let ok32One: Result<float32, float32> = Ok 1.0f
    let ok32Two: Result<float32, float32> = Ok 2.0f
    let error32One: Result<float32, float32> = Error 1.0f
    let ok64Zero: Result<float, float> = Ok 0.0
    let ok64NegZero: Result<float, float> = Ok -0.0
    let ok64One: Result<float, float> = Ok 1.0
    let ok64Two: Result<float, float> = Ok 2.0
    let error64One: Result<float, float> = Error 1.0
    (genericHash ok32Zero, genericHash ok32NegZero) ||> equal
    (genericHash ok32Two, genericHash ok32One) ||> notEqual
    (genericHash error32One, genericHash ok32One) ||> notEqual
    (genericHash ok64Zero, genericHash ok64NegZero) ||> equal
    (genericHash ok64Two, genericHash ok64One) ||> notEqual
    (genericHash error64One, genericHash ok64One) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with unions works`` () =
    (genericHash (UTest.A 1), genericHash (UTest.A 1)) ||> equal
    (genericHash (UTest.A 2), genericHash (UTest.A 1)) ||> notEqual
    (genericHash (UTest.B 1), genericHash (UTest.A 1)) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with float unions works`` () =
    (genericHash (UFloatTest.AFloat 0.0), genericHash (UFloatTest.AFloat -0.0)) ||> equal
    (genericHash (UFloatTest.AFloat 2.0), genericHash (UFloatTest.AFloat 1.0)) ||> notEqual
    (genericHash (UFloatTest.BFloat 1.0), genericHash (UFloatTest.AFloat 1.0)) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with records works`` () =
    (genericHash {a=1; b=2}, genericHash {a=1; b=2}) ||> equal
    (genericHash {a=2; b=1}, genericHash {a=1; b=2}) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with float records works`` () =
    (genericHash { a = 0.0; b = 1 }, genericHash { a = -0.0; b = 1 }) ||> equal
    (genericHash { a = 2.0; b = 1 }, genericHash { a = 1.0; b = 1 }) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with mutable float records works`` () =
    (genericHash { value = 0.0; tag = 1 }, genericHash { value = -0.0; tag = 1 }) ||> equal
    let record = { value = 1.0; tag = 1 }
    let h1 = genericHash record
    record.value <- 2.0
    let h2 = genericHash record
    (h1, h2) ||> notEqual
    (h2, genericHash { value = 2.0; tag = 1 }) ||> equal

[<Fact>]
let ``EqualityComparer.GetHashCode with DateTime works`` () =
    let ticks = System.DateTime(2024, 1, 2, 3, 4, 5, System.DateTimeKind.Utc).Ticks
    let dt1 = System.DateTime(ticks, System.DateTimeKind.Utc)
    let dt2 = System.DateTime(ticks, System.DateTimeKind.Local)
    let dt3 = System.DateTime(ticks + 1L, System.DateTimeKind.Utc)
    dt1 = dt2 |> equal true
    (genericHash dt1, genericHash dt2) ||> equal
    dt1 = dt3 |> equal false
    (genericHash dt3, genericHash dt1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with DateTimeOffset works`` () =
    let dto1 = System.DateTimeOffset(2024, 1, 2, 3, 4, 5, System.TimeSpan.Zero)
    let dto2 = System.DateTimeOffset(2024, 1, 2, 4, 4, 5, System.TimeSpan.FromHours 1.0)
    let dto3 = System.DateTimeOffset(2024, 1, 2, 3, 4, 6, System.TimeSpan.Zero)
    dto1 = dto2 |> equal true
    (genericHash dto1, genericHash dto2) ||> equal
    dto1 = dto3 |> equal false
    (genericHash dto3, genericHash dto1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with DateOnly works`` () =
    let date1 = System.DateOnly(2024, 1, 2)
    let date2 = System.DateOnly.FromDayNumber(date1.DayNumber)
    let date3 = date1.AddDays 1
    date1 = date2 |> equal true
    (genericHash date1, genericHash date2) ||> equal
    date1 = date3 |> equal false
    (genericHash date3, genericHash date1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with TimeOnly works`` () =
    let time1 = System.TimeOnly(3, 4, 5)
    let time2 = System.TimeOnly.FromDateTime(System.DateTime(2024, 1, 2, 3, 4, 5))
    let time3 = time1.Add(System.TimeSpan.FromSeconds 1.0)
    time1 = time2 |> equal true
    (genericHash time1, genericHash time2) ||> equal
    time1 = time3 |> equal false
    (genericHash time3, genericHash time1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with decimal works`` () =
    let decimal1 = 1.0m
    let decimal2 = 1.00m
    let decimal3 = 1.01m
    decimal1 = decimal2 |> equal true
    (genericHash decimal1, genericHash decimal2) ||> equal
    decimal1 = decimal3 |> equal false
    (genericHash decimal3, genericHash decimal1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with bigint works`` () =
    let bigint1 = 123456789123456789123456789I
    let bigint2 = System.Numerics.BigInteger.Parse("123456789123456789123456789")
    let bigint3 = bigint1 + 1I
    bigint1 = bigint2 |> equal true
    (genericHash bigint1, genericHash bigint2) ||> equal
    bigint1 = bigint3 |> equal false
    (genericHash bigint3, genericHash bigint1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with Guid works`` () =
    let guid1 = System.Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let guid2 = System.Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let guid3 = System.Guid.Parse("6f1f2d4a-90e5-4d26-a49d-bc8938af0d0e")
    guid1 = guid2 |> equal true
    (genericHash guid1, genericHash guid2) ||> equal
    guid1 = guid3 |> equal false
    (genericHash guid3, genericHash guid1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with TimeSpan works`` () =
    let ts1 = System.TimeSpan.FromMinutes 90.0
    let ts2 = System.TimeSpan(1, 30, 0)
    let ts3 = System.TimeSpan(1, 31, 0)
    ts1 = ts2 |> equal true
    (genericHash ts1, genericHash ts2) ||> equal
    ts1 = ts3 |> equal false
    (genericHash ts3, genericHash ts1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with float sets works`` () =
    let s1 = Set [ 0.0; 1.0; 2.0 ]
    let s2 = Set [ 2.0; 1.0; -0.0 ]
    let s3 = Set [ 0.0; 1.0; 3.0 ]
    s1 = s2 |> equal true
    (genericHash s1, genericHash s2) ||> equal
    s1 = s3 |> equal false
    (genericHash s3, genericHash s1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with float maps works`` () =
    let m1 = Map [ (0.0, 1); (1.0, 2) ]
    let m2 = Map [ (1.0, 2); (-0.0, 1) ]
    let m3 = Map [ (0.0, 1); (1.0, 3) ]
    m1 = m2 |> equal true
    (genericHash m1, genericHash m2) ||> equal
    m1 = m3 |> equal false
    (genericHash m3, genericHash m1) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with structs works`` () =
    (genericHash (STest(1)), genericHash (STest(1))) ||> equal
    (genericHash (STest(2)), genericHash (STest(1))) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with float structs works`` () =
    (genericHash (SFloatTest(0.0)), genericHash (SFloatTest(-0.0))) ||> equal
    (genericHash (SFloatTest(2.0)), genericHash (SFloatTest(1.0))) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with mutable float structs works`` () =
    (genericHash (SMutableFloatTest(0.0)), genericHash (SMutableFloatTest(-0.0))) ||> equal
    let mutable s = SMutableFloatTest(1.0)
    let h1 = genericHash s
    s.A <- 2.0
    let h2 = genericHash s
    (h1, h2) ||> notEqual
    (h2, genericHash (SMutableFloatTest(2.0))) ||> equal

[<Fact>]
let ``EqualityComparer.GetHashCode with objects works`` () =
    let cmp = EqualityComparer<OTest>.Default
    let o1 = OTest(1)
    let o2 = OTest(1)
    let o3 = OTest(2)
    (cmp.GetHashCode(o1), cmp.GetHashCode(o1)) ||> equal
    (cmp.GetHashCode(o2), cmp.GetHashCode(o1)) ||> notEqual
    (cmp.GetHashCode(o3), cmp.GetHashCode(o1)) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with same object works`` () =
    let cmp = EqualityComparer<OTest>.Default
    let o = OTest(1)
    let h1 = cmp.GetHashCode(o)
    o.A <- 2
    let h2 = cmp.GetHashCode(o)
    (h1, h2) ||> equal

[<Fact>]
let ``EqualityComparer.GetHashCode with longs works`` () =
    (genericHash (1L<<<33), genericHash (1L<<<33)) ||> equal
    (genericHash (1L<<<34), genericHash (1L<<<33)) ||> notEqual
    (genericHash 3L, genericHash (3L + (1L<<<33))) ||> notEqual
    (genericHash (-3L), genericHash (3L)) ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with primitives works`` () =
    (genericHash 111, genericHash 111) ||> equal
    (genericHash 222, genericHash 111) ||> notEqual
    (genericHash "1", genericHash "1") ||> equal
    (genericHash "2", genericHash "1") ||> notEqual

[<Fact>]
let ``EqualityComparer.GetHashCode with floats works`` () =
    (genericHash 0.0f, genericHash -0.0f) ||> equal
    (genericHash 2.0f, genericHash 1.0f) ||> notEqual
    (genericHash 0.0, genericHash -0.0) ||> equal
    (genericHash 2.0, genericHash 1.0) ||> notEqual
    (genericHash System.Single.NaN, genericHash System.Single.NaN) ||> equal
    (genericHash System.Double.NaN, genericHash System.Double.NaN) ||> equal
    (genericHash System.Single.PositiveInfinity, genericHash System.Single.NegativeInfinity) ||> notEqual
    (genericHash System.Double.PositiveInfinity, genericHash System.Double.NegativeInfinity) ||> notEqual

[<Fact>]
let ``Comparer.Compare works`` () =
    genericCompare 1 1 |> equal 0
    genericCompare 1 2 |> equal -1
    genericCompare 2 1 |> equal 1

[<Fact>]
let ``Comparer.Create works`` () =
    let cmp = Comparer.Create(fun x y -> -(compare x y))
    cmp.Compare(1, 1) |> equal 0
    cmp.Compare(1, 2) |> equal 1
    cmp.Compare(2, 1) |> equal -1
