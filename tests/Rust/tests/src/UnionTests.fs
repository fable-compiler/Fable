module Fable.Tests.UnionTests

open Util.Testing

type IntUnion =
    | IntCase1 of int
    | IntCase2 of int
    | IntCase3 of int

[<Struct>]
type Point2D =
    | Planar2D of float * float
    | Infinity

[<Fact>]
let ``Struct unions work`` () =
    let distance p =
        match p with
        | Planar2D (x, y) -> sqrt (x * x + y * y)
        | Infinity -> infinity
    let p = Planar2D (3, 4)
    let res = distance p
    res |> equal 5.

[<Fact>]
let ``Union case matching works`` () =
    let x = IntCase1 5
    let res =
        match x with
        | IntCase1 a -> a
        | IntCase2 b -> b
        | IntCase3 c -> c
    res |> equal 5

[<Fact>]
let ``Union case equality works`` () =
    IntCase1 5 = IntCase1 5 |> equal true
    IntCase1 5 = IntCase2 5 |> equal false
    IntCase3 2 = IntCase3 3 |> equal false
    IntCase2 1 = IntCase2 1 |> equal true
    IntCase3 1 = IntCase3 1 |> equal true

let unionFnAlways1 = function
    | IntCase1 x -> x
    | _ -> -1

let unionFnRetNum = function
    | IntCase1 a -> a
    | IntCase2 b -> b
    | IntCase3 c -> c

[<Fact>]
let ``Union fn call works`` () =
    let x = IntCase1 3
    let res = unionFnAlways1 x
    let res2 = unionFnAlways1 x //deliberately force clone/borrow break
    let res3 = unionFnRetNum x
    let res4 = unionFnRetNum (IntCase2 24)
    res |> equal 3
    res2 |> equal 3
    res3 |> equal 3
    res4 |> equal 24

type WrappedUnion =
    | AString of string

[<Fact>]
let ``Union with wrapped type works`` () =
    let a = AString "hello"
    let b = match a with AString s -> s + " world"
    b |> equal "hello world"

type DeepRecord = {Value: string}

type DeepWrappedUnion =
    | DeepWrappedA of string * DeepRecord
    | DeepWrappedB of string
    | DeepWrappedC of int
    | DeepWrappedD of DeepRecord
    | DeepWrappedE of int * int
    | DeepWrappedF of WrappedUnion
    | DeepWrappedG of {| X: DeepRecord; Y: int|}

let matchStrings = function
    | DeepWrappedA (s, d) -> d.Value + s
    | DeepWrappedB s -> s
    | DeepWrappedC c -> "nothing"
    | DeepWrappedD d -> d.Value
    | DeepWrappedE(a, b) -> "nothing2"
    | DeepWrappedF (AString s) -> s
    | DeepWrappedG x -> x.X.Value

let matchNumbers = function
    | DeepWrappedA (s, d) -> 0
    | DeepWrappedB s -> 0
    | DeepWrappedC c -> c
    | DeepWrappedD d -> 0
    | DeepWrappedE(a, b) -> a + b
    | DeepWrappedF _ -> 0
    | DeepWrappedG x -> x.Y

[<Fact>]
let ``Deep union with wrapped type works`` () =
    let a = DeepWrappedA (" world", {Value = "hello"})
    let b = DeepWrappedB "world"
    let c = DeepWrappedC 42
    let d = DeepWrappedD { Value = "hello" }
    let f = DeepWrappedF (AString "doublewrapped")
    let g = DeepWrappedG {|X={Value="G"}; Y=365|}
    a |> matchStrings |> equal "hello world"
    b |> matchStrings |> equal "world"
    c |> matchStrings |> equal "nothing"
    d |> matchStrings |> equal "hello"
    f |> matchStrings |> equal "doublewrapped"
    g |> matchStrings |> equal "G"

[<Fact>]
let ``Deep union with tuped prim type works`` () =
    let e = DeepWrappedE (3, 2)
    let c = DeepWrappedC 42//todo to string!
    let g = DeepWrappedG {|X={Value="G"}; Y=365|}
    e |> matchNumbers |> equal 5
    c |> matchNumbers |> equal 42
    g |> matchNumbers |> equal 365

[<Fact>]
let ``Multi-case Union with wrapped type works`` () =
    let b = DeepWrappedB "hello"
    let res = match b with DeepWrappedB s -> s + " world" | _ -> ""
    res |> equal "hello world"

[<Fact>]
let ``Should correctly import union in other file and allow creation`` =
    let r = Common.Imports.MyUnion.A 1
    let expected = Common.Imports.MyUnion.createA 1
    r |> equal expected


let matchStringWhenStringNotHello = function
    | DeepWrappedB s when s <> "hello" -> "not hello"
    | _ -> "hello"

[<Fact>]
let ``Match with condition works`` () =
    let b1 = DeepWrappedB "hello"
    let b2 = DeepWrappedB "not"
    b1 |> matchStringWhenStringNotHello |> equal "hello"
    b2 |> matchStringWhenStringNotHello |> equal "not hello"

[<Fable.Core.Rust.ReferenceType(Fable.Core.Rust.PointerType.Arc)>]
type ArcUnion =
    | ArcS of string
    | ArcI of int

let add1 = function
    | ArcS s -> s + "1" |> ArcS
    | ArcI i -> i + 1 |> ArcI

[<Fact>]
let ``Union with pointer type works`` () =
    let a = ArcS "hello"
    let b = ArcI 42
    let aa = add1 a
    let bb = add1 b
    aa |> equal (ArcS "hello1")
    bb |> equal (ArcI 43)

#if FABLE_COMPILER
open Fable.Core

[<Emit("$0 as LrcPtr<IntUnion>")>]
let ensureMyUnionWrapped s = nativeOnly

[<Emit("$0 as Point2D")>]
let ensureIsStructUnionUnwrapped s = nativeOnly

[<Fact>]
let ``Normal union should be wrapped in a Lrc`` () =
    IntCase1 1 |> ensureMyUnionWrapped |> ignore

[<Fact>]
let ``Struct union should not be wrapped in a Lrc`` () =
    Infinity |> ensureIsStructUnionUnwrapped |> ignore

#endif


// type Gender = Male | Female

// type Either<'TL,'TR> =
//     | Left of 'TL
//     | Right of 'TR
//     member x.AsString() =
//       match x with
//       | Left y -> y.ToString()
//       | Right y -> y.ToString()

type TestUnion =
   | Case0
   | Case1 of string
   | Case2 of string * string
   | Case3 of string * string * string

type TestUnion2 =
    | Tag of string
    | NewTag of string

// let (|Functional|NonFunctional|) (s: string) =
//     match s with
//     | "fsharp" | "haskell" | "ocaml" -> Functional
//     | _ -> NonFunctional

// let (|Small|Medium|Large|) i =
//     if i < 3 then Small 5
//     elif i >= 3 && i < 6 then Medium "foo"
//     else Large

// let (|FSharp|_|) (document : string) =
//     if document = "fsharp" then Some FSharp else None

let (|A|) n = n

type JsonTypeInner = {
    Prop1: string
    Prop2: int
}

type JsonTestUnion =
    | IntType of int
    | StringType of string
    | TupleType of string * int
    | ObjectType of JsonTypeInner

type Tree =
    | Leaf of int
    | Branch of Tree[]
    member this.Sum() =
        match this with
        | Leaf i -> i
        | Branch trees -> trees |> Seq.map (fun x -> x.Sum()) |> Seq.sum

// type MyExUnion = MyExUnionCase of exn

// type Wrapper(s: string) =
//     member x.Value = s |> Seq.rev |> Seq.map string |> String.concat ""

[<RequireQualifiedAccess>]
type MyUnion =
| Case1
| Case2
| Case3

type R = {
    Name: string
    Case: MyUnion
}

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.RustInterop

[<Erase>]
#endif
type DU = Int of int | Str of string

type T1 = T1
type T2 = T2
type T3 = T3
type T4 = T4
type T5 = T5
type T6 = T6
type T7 = T7
type T8 = T8
type T9 = T9
type 'a more = More of 'a

// [<Fact>]
// let ``Union cases matches with no arguments can be generated`` () =
//     let x = Male
//     match x with
//     | Female -> true
//     | Male -> false
//     |> equal false

// [<Fact>]
// let ``Union cases matches with one argument can be generated`` () =
//     let x = Left "abc"
//     match x with
//     | Left data -> data
//     | Right _ -> failwith "unexpected"
//     |> equal "abc"

// // TODO: .ToString() with records and unions
// [<Fact>]
// let ``Union methods can be generated`` () =
//     let x = Left 5
//     x.AsString()
//     |> equal "5"

// [<Fact>]
// let ``Nested pattern matching works`` () =
//     let x = Right(Left 5)
//     match x with
//     | Left _ -> failwith "unexpected"
//     | Right x ->
//         match x with
//         | Left x -> x
//         | Right _ -> failwith "unexpected"
//     |> equal 5

[<Fact>]
let ``Union cases matches with many arguments can be generated`` () =
    let x = TestUnion.Case3("a", "b", "c")
    match x with
    | TestUnion.Case3(a, b, c) -> a + b + c
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``Pattern matching with common targets works`` () =
    let x = TestUnion.Case2("a", "b")
    match x with
    | TestUnion.Case0 -> failwith "unexpected"
    | TestUnion.Case1 _
    | TestUnion.Case2 _ -> "a"
    | TestUnion.Case3(a, b, c) -> a + b + c
    |> equal "a"

[<Fact>]
let ``Union cases called Tag still work (bug due to Tag field)`` () =
    let x = Tag "abc"
    match x with
    | Tag x -> x
    | _ -> failwith "unexpected"
    |> equal "abc"

// [<Fact>]
// let ``Comprehensive active patterns work`` () =
//     let isFunctional = function
//         | Functional -> true
//         | NonFunctional -> false
//     isFunctional "fsharp" |> equal true
//     isFunctional "csharp" |> equal false
//     isFunctional "haskell" |> equal true

// [<Fact>]
// let ``Comprehensive active patterns can return values`` () =
//     let measure = function
//         | Small i -> string i
//         | Medium s -> s
//         | Large -> "bar"
//     measure 0 |> equal "5"
//     measure 10 |> equal "bar"
//     measure 5 |> equal "foo"

// [<Fact>]
// let ``Partial active patterns which don't return values work`` () = // See #478
//     let isFunctional = function
//         | FSharp -> "yes"
//         | "scala" -> "fifty-fifty"
//         | _ -> "dunno"
//     isFunctional "scala" |> equal "fifty-fifty"
//     isFunctional "smalltalk" |> equal "dunno"
//     isFunctional "fsharp" |> equal "yes"

// [<Fact>]
// let ``Active patterns can be combined with union case matching`` () = // See #306
//     let test = function
//         | Some(A n, Some(A m)) -> n + m
//         | _ -> 0
//     Some(5, Some 2) |> test |> equal 7
//     Some(5, None) |> test |> equal 0
//     None |> test |> equal 0

// [<Fact>]
// let ``Types can have Exception fields`` () =
//     let (MyExUnionCase ex) =
//         try
//             exn "foo" |> raise
//         with ex -> MyExUnionCase ex
//     ex.Message |> equal "foo"

[<Fact>]
let ``Equality works in filter`` () =
    let original = [| { Name = "1"; Case = MyUnion.Case1 } ; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
    original
    |> Array.filter (fun r -> r.Case = MyUnion.Case1)
    |> Array.length
    |> equal 2

[<Fact>]
let ``Pattern matching works in filter`` () =
    let original = [| { Name = "1"; Case = MyUnion.Case1 } ; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
    original
    |> Array.filter (fun r -> match r.Case with MyUnion.Case1 -> true | _ -> false)
    |> Array.length
    |> equal 2

// #if FABLE_COMPILER
// [<Fact>]
// let ``Erased union type testing works`` () =
//     let toString (arg: U3<string, int, Wrapper>) =
//         match arg with
//         | U3.Case1 s -> s
//         | U3.Case2 i -> i * 2 |> string
//         | U3.Case3 t -> t.Value
//     U3.Case1 "HELLO" |> toString |> equal "HELLO"
//     U3.Case2 3 |> toString |> equal "6"
//     "HELLO" |> Wrapper |> U3.Case3 |> toString |> equal "OLLEH"
// #endif

// #if FABLE_COMPILER
// [<Fact>]
// let ``Case testing with generic erased unions works`` () =
//     let strify (x: U2<int,string>) =
//         match x with
//         | U2.Case1 i -> "i: " + string i
//         | U2.Case2 s -> "s: " + s
//     U2.Case2 "foo" |> strify |> equal "s: foo"
//     U2.Case1 42 |> strify |> equal "i: 42"
// #endif

// [<Fact>]
// let ``Case testing with erased unions works`` () =
//     let strify = function
//         | Int i -> "i: " + string i
//         | Str s -> "s: " + s
//     Str "foo" |> strify |> equal "s: foo"
//     Int 42 |> strify |> equal "i: 42"

#if FABLE_COMPILER

[<Fact>]
let ``U9 works when not nested`` () =
    let x : U9<T1,T2,T3,T4,T5,T6,T7,T8,T9> = !^T9
    x |> equal (U9.Case9 T9)

[<Fact>]
let ``U9 works when nested`` () =
    let x : U9<T1,T2,T3,T4,T5,T6,T7,T8,U2<T9, int>> = !^42
    x |> equal (U9.Case9 (U2.Case2 42))

[<Fact>]
let ``U9 works when nested 2 times`` () =
    let x : U9<T1,T2,T3,T4,T5,T6,T7,T8,
                U9<T1 more, T2 more, T3 more, T4 more, T5 more, T6 more, T7 more, T8 more, U2<T9 more, int>>> = !^42
    x |> equal (U9.Case9 (U9.Case9 (U2.Case2 42)))

[<Fact>]
let ``Non-nested U9 inside nested U9 works`` () =
    let x : U9<T1,T2,T3,T4,T5,T6,T7,T8,
                U9<T1 more, T2 more, T3 more, T4 more, T5 more, T6 more, T7 more, T8 more, T9>> = !^T9
    x |> equal (U9.Case9 (U9.Case9 T9))

[<Fact>]
let ``Compiler does not complain if the same type appears twice in nested U9`` () =
    let x : U9<T1,T2,T3,int,T5,T6,T7,T8,
                U9<T1 more, T2 more, T3 more, int, T5 more, T6 more, T7 more, T8 more, T9 more>> = !^42
    x |> equal (U9.Case4 42)

#endif
