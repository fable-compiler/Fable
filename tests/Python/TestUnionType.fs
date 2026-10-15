module Fable.Tests.UnionTypes

open Util.Testing

type Gender = Male | Female

type Either<'TL,'TR> =
    | Left of 'TL
    | Right of 'TR
    member x.AsString() =
      match x with
      | Left y -> y.ToString()
      | Right y -> y.ToString()

type TestUnion =
   | Case0
   | Case1 of string
   | Case2 of string * string
   | Case3 of string * string * string

type TestUnion2 =
    | Tag of string
    | NewTag of string

let (|Functional|NonFunctional|) (s: string) =
    match s with
    | "fsharp" | "haskell" | "ocaml" -> Functional
    | _ -> NonFunctional

let (|Small|Medium|Large|) i =
    if i < 3 then Small 5
    elif i >= 3 && i < 6 then Medium "foo"
    else Large

let (|FSharp|_|) (document : string) =
    if document = "fsharp" then Some FSharp else None

let (|A|) n = n

// type JsonTypeInner = {
//     Prop1: string
//     Prop2: int
// }

// type JsonTestUnion =
//     | IntType of int
//     | StringType of string
//     | TupleType of string * int
//     | ObjectType of JsonTypeInner

// type Tree =
//     | Leaf of int
//     | Branch of Tree[]
//     member this.Sum() =
//         match this with
//         | Leaf i -> i
//         | Branch trees -> trees |> Seq.map (fun x -> x.Sum()) |> Seq.sum

type MyExUnion = MyExUnionCase of exn

type Wrapper(s: string) =
    member x.Value = s |> Seq.rev |> Seq.map string |> String.concat ""

[<RequireQualifiedAccess>]
type MyUnion =
| Case1
| Case2
| Case3

type R = {
    Name: string
    Case: MyUnion
}

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

// [<RequireQualifiedAccess>]
// type LangCode =
//     | EN
//     | ``pt-Br``

type S = S of string

type Id = Id of string

type UnionWithIdField = WithId of Id: Id

// See https://github.com/fable-compiler/Fable/issues/4645
// A named union case field called `name` collided with the inherited `Union.name`
// property, which Python's @dataclass treated as a default value.
type NamedFieldUnion =
    | NamedCase of name: string * optional: string

// See https://github.com/fable-compiler/Fable/issues/4736
// A static member whose type references the enclosing union used to emit a class-body
// `StaticLazyProperty[...]` subscript that evaluated the union type before its name was bound,
// raising `NameError` at import time. The reference can be nested in any type shape, so each
// member below exercises a different one: array, option (rendered with `|`), nested list/array,
// tuple, and a function type (`unit -> Example`).
[<Fable.Core.AttachMembers>]
[<RequireQualifiedAccess>]
type Example =
    | First
    | Second
    | Other of string

    static member All = [| Example.First; Example.Second |]
    static member Head = Some Example.First
    static member Nested = [ [| Example.First |] ]
    static member Pair = (Example.First, Example.Second)
    static member Factory: unit -> Example = fun () -> Example.First

[<Struct>]
type StructPoint2D =
    | Planar2D of float * float
    | Infinity

[<Struct>]
type StructShape =
    | Square of side: int
    | Circle of radius: float

    member this.Area =
        match this with
        | Square side -> float (side * side)
        | Circle radius -> System.Math.PI * radius * radius

[<Struct>]
type StructShape2 =
    | Square of float
    | Circle of float

    member this.Area =
        match this with
        | Square side -> side * side
        | Circle radius -> System.Math.PI * radius * radius

[<Struct>]
type NestedMatchUnion =
    | NestedA of int
    | NestedB of int

let private nestedUnionCaseAccess (u: NestedMatchUnion) =
    match u with
    | NestedA _ -> 0
    | NestedB _ ->
        match u with
        | NestedA value -> value
        | _ -> -1

type IntUnion =
    | IntCase1 of int
    | IntCase2 of int
    | IntCase3 of int

let unionFnAlways1 = function
    | IntCase1 x -> x
    | _ -> -1

let unionFnRetNum = function
    | IntCase1 a -> a
    | IntCase2 b -> b
    | IntCase3 c -> c

type WrappedUnion =
    | AString of string

type DeepRecord = { Value: string }

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

let matchStringWhenStringNotHello = function
    | DeepWrappedB s when s <> "hello" -> "not hello"
    | _ -> "hello"

#if FABLE_COMPILER
open Fable.Core

[<Erase>]
#endif
type DU = Int of int | Str of string

type ErasedPayload = { Description: string; Membership: int }

[<Fable.Core.Erase>]
type ErasedWrapper =
    | ErasedSuccess of ErasedPayload
    | ErasedFailure of string

[<Fact>]
let ``test Union cases matches with no arguments can be generated`` () =
    let x = Male
    match x with
    | Female -> true
    | Male -> false
    |> equal false

[<Fact>]
let ``test Union cases matches with one argument can be generated`` () =
    let x = Left "abc"
    match x with
    | Left data -> data
    | Right _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Union methods can be generated`` () =
    let x = Left 5
    x.AsString()
    |> equal "5"

[<Fact>]
let ``test Nested pattern matching works`` () =
    let x = Right(Left 5)
    match x with
    | Left _ -> failwith "unexpected"
    | Right x ->
        match x with
        | Left x -> x
        | Right _ -> failwith "unexpected"
    |> equal 5

[<Fact>]
let ``test Union cases matches with many arguments can be generated`` () =
    let x = Case3("a", "b", "c")
    match x with
    | Case3(a, b, c) -> a + b + c
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Pattern matching with common targets works`` () =
    let x = TestUnion.Case2("a", "b")
    match x with
    | TestUnion.Case0 -> failwith "unexpected"
    | TestUnion.Case1 _
    | TestUnion.Case2 _ -> "a"
    | TestUnion.Case3(a, b, c) -> a + b + c
    |> equal "a"

[<Fact>]
let ``test Union cases called Tag still work (bug due to Tag field)`` () =
    let x = Tag "abc"
    match x with
    | Tag x -> x
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Comprehensive active patterns work`` () =
    let isFunctional = function
        | Functional -> true
        | NonFunctional -> false
    isFunctional "fsharp" |> equal true
    isFunctional "csharp" |> equal false
    isFunctional "haskell" |> equal true

[<Fact>]
let ``test Comprehensive active patterns can return values`` () =
    let measure = function
        | Small i -> string i
        | Medium s -> s
        | Large -> "bar"
    measure 0 |> equal "5"
    measure 10 |> equal "bar"
    measure 5 |> equal "foo"

[<Fact>]
let ``test Partial active patterns which don't return values work`` () = // See #478
    let isFunctional = function
        | FSharp -> "yes"
        | "scala" -> "fifty-fifty"
        | _ -> "dunno"
    isFunctional "scala" |> equal "fifty-fifty"
    isFunctional "smalltalk" |> equal "dunno"
    isFunctional "fsharp" |> equal "yes"

[<Fact>]
let ``test Active patterns can be combined with union case matching`` () = // See #306
    let test = function
        | Some(A n, Some(A m)) -> n + m
        | _ -> 0
    Some(5, Some 2) |> test |> equal 7
    Some(5, None) |> test |> equal 0
    None |> test |> equal 0

[<Fact>]
let ``test Types can have Exception fields`` () =
    let (MyExUnionCase ex) =
        try
            exn "foo" |> raise
        with ex -> MyExUnionCase ex
    ex.Message |> equal "foo"

#if FABLE_COMPILER
[<Fact>]
let ``test Erased union type testing works`` () =
    let toString (arg: U3<string, int, Wrapper>) =
        match arg with
        | U3.Case1 s -> s
        | U3.Case2 i -> i * 2 |> string
        | U3.Case3 t -> t.Value
    U3.Case1 "HELLO" |> toString |> equal "HELLO"
    U3.Case2 3 |> toString |> equal "6"
    "HELLO" |> Wrapper |> U3.Case3 |> toString |> equal "OLLEH"

[<Fact>]
let ``test Implicit conversion to erased unions works`` () =
    let strify (x: U3<int, string, bool>) =
        match x with
        | U3.Case1 i -> "i: " + string i
        | U3.Case2 s -> "s: " + s
        | U3.Case3 b -> if b then "b: yes" else "b: no"
    let s: U2<int, string> = "foo"
    s |> equal (U2.Case2 "foo")
    strify 42 |> equal "i: 42"
    strify "foo" |> equal "s: foo"
    strify true |> equal "b: yes"
#endif

[<Fact>]
let ``test Erased union payload fields can be read`` () =
    let payload = { Description = "a"; Membership = 2 }

    match ErasedSuccess payload with
    | ErasedSuccess details -> details.Membership
    | ErasedFailure _ -> -1
    |> equal 2

[<Fact>]
let ``test Equality works in filter`` () =
    let original = [| { Name = "1"; Case = MyUnion.Case1 }; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
    original
    |> Array.filter (fun r -> r.Case = MyUnion.Case1)
    |> Array.length
    |> equal 2

[<Fact>]
let ``test Pattern matching works in filter`` () =
    let original = [| { Name = "1"; Case = MyUnion.Case1 }; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
    original
    |> Array.filter (fun r -> match r.Case with MyUnion.Case1 -> true | _ -> false)
    |> Array.length
    |> equal 2

[<Fact>]
let ``test Union field can have the same name as its type`` () =
    let value = WithId(Id "foo")
    match value with
    | WithId id ->
        match id with
        | Id value -> value |> equal "foo"

// [<Fact>]
// let ``test Verify that union cases can have `-` in their names`` () =
//     // See https://github.com/fable-compiler/Fable/issues/4071
//     let x = LangCode.``pt-Br``
//     equal x LangCode.``pt-Br``

[<Fact>]
let ``test sprintf formats strings cases correctly`` () =
    let s = sprintf "%A" (S "1")
    s |> equal "S \"1\""

[<Fact>]
let ``test Union cases with no fields are physically equal`` () =
    obj.ReferenceEquals(Gender.Male, Gender.Male) |> equal true
    obj.ReferenceEquals(Gender.Male, Gender.Female) |> equal false
    obj.ReferenceEquals(TestUnion.Case0, TestUnion.Case0) |> equal true
    obj.ReferenceEquals(TestUnion.Case1 "a", TestUnion.Case1 "a") |> equal false
    obj.ReferenceEquals(T1, T1) |> equal true

[<Fact>]
let ``test Named union case fields work when a field is called name`` () =
    let value = NamedCase("v1", "v2")
    match value with
    | NamedCase(name, optional) ->
        name |> equal "v1"
        optional |> equal "v2"

[<Fact>]
let ``test Static union member returning the union type works`` () =
    Example.All |> Array.length |> equal 2
    Example.All.[0] |> equal Example.First
    Example.All.[1] |> equal Example.Second

[<Fact>]
let ``test Static union member with nested self-reference works`` () =
    // Option (`_Example | None`): a bare quoted leaf would leave the `|` to run eagerly and fail.
    Example.Head |> equal (Some Example.First)
    // Nested list of array (`FSharpList[Array[_Example]]`).
    Example.Nested |> List.head |> Array.head |> equal Example.First
    // Tuple (`tuple[_Example, _Example]`).
    Example.Pair |> fst |> equal Example.First
    Example.Pair |> snd |> equal Example.Second
    // Function type (`Callable[[], _Example]`) — missed by the original narrower detection.
    Example.Factory() |> equal Example.First

[<Fact>]
let ``test Struct unions work`` () =
    let distance p =
        match p with
        | Planar2D (x, y) -> sqrt (x * x + y * y)
        | Infinity -> infinity
    let p = Planar2D (3, 4)
    let res = distance p
    res |> equal 5.

[<Fact>]
let ``test Struct unions with fields work`` () =
    let radius = 3.0
    let circle = StructShape.Circle radius
    let square = StructShape.Square(int (2.0 * radius))
    circle.Area |> equal (System.Math.PI * radius * radius)
    square.Area |> equal (4.0 * radius * radius)

[<Fact>]
let ``test Struct unions without fields work`` () =
    let radius = 3.0
    let circle = StructShape2.Circle radius
    let square = StructShape2.Square(2.0 * radius)
    circle.Area |> equal (System.Math.PI * radius * radius)
    square.Area |> equal (4.0 * radius * radius)

[<Fact>]
let ``test Nested union match keeps case-specific field access`` () =
    nestedUnionCaseAccess (NestedA 7) |> equal 0
    nestedUnionCaseAccess (NestedB 11) |> equal -1

[<Fact>]
let ``test Union case matching works`` () =
    let x = IntCase1 5
    let res =
        match x with
        | IntCase1 a -> a
        | IntCase2 b -> b
        | IntCase3 c -> c
    res |> equal 5

[<Fact>]
let ``test Union case equality works`` () =
    IntCase1 5 = IntCase1 5 |> equal true
    IntCase1 5 = IntCase2 5 |> equal false
    IntCase3 2 = IntCase3 3 |> equal false
    IntCase2 1 = IntCase2 1 |> equal true
    IntCase3 1 = IntCase3 1 |> equal true

[<Fact>]
let ``test Union fn call works`` () =
    let x = IntCase1 3
    let res = unionFnAlways1 x
    let res2 = unionFnAlways1 x //deliberately force clone/borrow break
    let res3 = unionFnRetNum x
    let res4 = unionFnRetNum (IntCase2 24)
    res |> equal 3
    res2 |> equal 3
    res3 |> equal 3
    res4 |> equal 24

[<Fact>]
let ``test Deep union with wrapped type works`` () =
    let a = DeepWrappedA (" world", {Value = "hello"})
    let b = DeepWrappedB "world"
    let c = DeepWrappedC 42
    let d = DeepWrappedD { Value = "hello" }
    let f = DeepWrappedF (AString "doublewrapped")
    let g = DeepWrappedG {| X = { Value = "G" }; Y = 365 |}
    a |> matchStrings |> equal "hello world"
    b |> matchStrings |> equal "world"
    c |> matchStrings |> equal "nothing"
    d |> matchStrings |> equal "hello"
    f |> matchStrings |> equal "doublewrapped"
    g |> matchStrings |> equal "G"

[<Fact>]
let ``test Deep union with tuped prim type works`` () =
    let e = DeepWrappedE (3, 2)
    let c = DeepWrappedC 42 //todo to string!
    let g = DeepWrappedG {| X = { Value = "G" }; Y = 365 |}
    e |> matchNumbers |> equal 5
    c |> matchNumbers |> equal 42
    g |> matchNumbers |> equal 365

[<Fact>]
let ``test Multi-case Union with wrapped type works`` () =
    let b = DeepWrappedB "hello"
    let res = match b with DeepWrappedB s -> s + " world" | _ -> ""
    res |> equal "hello world"

[<Fact>]
let ``test Match with condition works`` () =
    let b1 = DeepWrappedB "hello"
    let b2 = DeepWrappedB "not"
    b1 |> matchStringWhenStringNotHello |> equal "hello"
    b2 |> matchStringWhenStringNotHello |> equal "not hello"
