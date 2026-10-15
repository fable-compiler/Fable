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

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

[<Erase>]
#endif
type DU = Int of int | Str of string

type ErasedPayload = { Description: string; Membership: int }

[<Fable.Core.Erase>]
type ErasedWrapper =
    | ErasedSuccess of ErasedPayload
    | ErasedFailure of string

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

[<RequireQualifiedAccess>]
type LangCode =
    | EN
    | ``pt-Br``

type S = S of string

type Id = Id of string

type UnionWithIdField = WithId of Id: Id

type NamedFieldUnion =
    | NamedCase of name: string * optional: string

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

let tests =
  testList "Unions" [
    testCase "Union cases matches with no arguments can be generated" <| fun () ->
        let x = Male
        match x with
        | Female -> true
        | Male -> false
        |> equal false

    testCase "Union cases matches with one argument can be generated" <| fun () ->
        // Prevent inlining so Typescript doesn't complain
        let mutable f = fun x ->
            match x with
            | Left data -> data
            | Right _ -> failwith "unexpected"
            |> equal "abc"
        Left "abc" |> f

    // TODO: .ToString() with records and unions
    testCase "Union methods can be generated" <| fun () ->
        let x = Left 5
        x.AsString()
        |> equal "5"

    testCase "Nested pattern matching works" <| fun () ->
        let x = Right(Left 5)
        match x with
        | Left _ -> failwith "unexpected"
        | Right x ->
            match x with
            | Left x -> x
            | Right _ -> failwith "unexpected"
        |> equal 5

    testCase "Union cases matches with many arguments can be generated" <| fun () ->
        let x = Case3("a", "b", "c")
        match x with
        | Case3(a, b, c) -> a + b + c
        | _ -> failwith "unexpected"
        |> equal "abc"

    testCase "Pattern matching with common targets works" <| fun () ->
        // Prevent inlining so Typescript doesn't complain
        let mutable f = fun x ->
            match x with
            | TestUnion.Case0 -> failwith "unexpected"
            | TestUnion.Case1 _
            | TestUnion.Case2 _ -> "a"
            | TestUnion.Case3(a, b, c) -> a + b + c
            |> equal "a"
        TestUnion.Case2("a", "b") |> f

    testCase "Union cases called Tag still work (bug due to Tag field)" <| fun () ->
        // Prevent inlining so Typescript doesn't complain
        let mutable f = fun x ->
            match x with
            | Tag x -> x
            | _ -> failwith "unexpected"
            |> equal "abc"
        Tag "abc" |> f

    testCase "Comprehensive active patterns work" <| fun () ->
        let isFunctional = function
            | Functional -> true
            | NonFunctional -> false
        isFunctional "fsharp" |> equal true
        isFunctional "csharp" |> equal false
        isFunctional "haskell" |> equal true

    testCase "Comprehensive active patterns can return values" <| fun () ->
        let measure = function
            | Small i -> string i
            | Medium s -> s
            | Large -> "bar"
        measure 0 |> equal "5"
        measure 10 |> equal "bar"
        measure 5 |> equal "foo"

    testCase "Partial active patterns which don't return values work" <| fun () -> // See #478
        let isFunctional = function
            | FSharp -> "yes"
            | "scala" -> "fifty-fifty"
            | _ -> "dunno"
        isFunctional "scala" |> equal "fifty-fifty"
        isFunctional "smalltalk" |> equal "dunno"
        isFunctional "fsharp" |> equal "yes"

    testCase "Active patterns can be combined with union case matching" <| fun () -> // See #306
        let test = function
            | Some(A n, Some(A m)) -> n + m
            | _ -> 0
        Some(5, Some 2) |> test |> equal 7
        Some(5, None) |> test |> equal 0
        None |> test |> equal 0

    testCase "Types can have Exception fields" <| fun () ->
        let (MyExUnionCase ex) =
            try
                exn "foo" |> raise
            with ex -> MyExUnionCase ex
        ex.Message |> equal "foo"

    #if FABLE_COMPILER
    testCase "Erased union type testing works" <| fun () ->
        let toString (arg: U3<string, int, Wrapper>) =
            match arg with
            | U3.Case1 s -> s
            | U3.Case2 i -> i * 2 |> string
            | U3.Case3 t -> t.Value
        U3.Case1 "HELLO" |> toString |> equal "HELLO"
        U3.Case2 3 |> toString |> equal "6"
        "HELLO" |> Wrapper |> U3.Case3 |> toString |> equal "OLLEH"
    #endif

    testCase "Erased union payload fields can be read" <| fun () ->
        let payload = { Description = "a"; Membership = 2 }

        match ErasedSuccess payload with
        | ErasedSuccess details -> details.Membership
        | ErasedFailure _ -> -1
        |> equal 2

    testCase "Equality works in filter" <| fun () ->
        let original = [| { Name = "1"; Case = MyUnion.Case1 }; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
        original
        |> Array.filter (fun r -> r.Case = MyUnion.Case1)
        |> Array.length
        |> equal 2

    testCase "Pattern matching works in filter" <| fun () ->
        let original = [| { Name = "1"; Case = MyUnion.Case1 }; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
        original
        |> Array.filter (fun r -> match r.Case with MyUnion.Case1 -> true | _ -> false)
        |> Array.length
        |> equal 2

    testCase "Union field can have the same name as its type" <| fun () ->
        let value = WithId(Id "foo")
        match value with
        | WithId id ->
            match id with
            | Id value -> value |> equal "foo"

    #if FABLE_COMPILER
    testCase "Case testing with generic erased unions works" <| fun () ->
        let strify (x: U2<int,string>) =
            match x with
            | U2.Case1 i -> "i: " + string i
            | U2.Case2 s -> "s: " + s
        U2.Case2 "foo" |> strify |> equal "s: foo"
        U2.Case1 42 |> strify |> equal "i: 42"
    #endif

    testCase "Case testing with erased unions works" <| fun () ->
        let strify = function
            | Int i -> "i: " + string i
            | Str s -> "s: " + s
        Str "foo" |> strify |> equal "s: foo"
        Int 42 |> strify |> equal "i: 42"

    #if FABLE_COMPILER
    testCase "U9 works when not nested" <| fun () ->
        let x : U9<T1,T2,T3,T4,T5,T6,T7,T8,T9> = !^T9
        x |> equal (U9.Case9 T9)

    testCase "U9 works when nested" <| fun () ->
        let x : U9<T1,T2,T3,T4,T5,T6,T7,T8,U2<T9, int>> = !^42
        x |> equal (U9.Case9 (U2.Case2 42))

    testCase "U9 works when nested 2 times" <| fun () ->
        let x : U9<T1,T2,T3,T4,T5,T6,T7,T8,
                   U9<T1 more, T2 more, T3 more, T4 more, T5 more, T6 more, T7 more, T8 more, U2<T9 more, int>>> = !^42
        x |> equal (U9.Case9 (U9.Case9 (U2.Case2 42)))

    testCase "Non-nested U9 inside nested U9 works" <| fun () ->
        let x : U9<T1,T2,T3,T4,T5,T6,T7,T8,
                   U9<T1 more, T2 more, T3 more, T4 more, T5 more, T6 more, T7 more, T8 more, T9>> = !^T9
        x |> equal (U9.Case9 (U9.Case9 T9))

    testCase "Compiler does not complain if the same type appears twice in nested U9" <| fun () ->
        let x : U9<T1,T2,T3,int,T5,T6,T7,T8,
                   U9<T1 more, T2 more, T3 more, int, T5 more, T6 more, T7 more, T8 more, T9 more>> = !^42
        x |> equal (U9.Case4 42)

    testCase "Implicit conversion to erased unions works" <| fun () ->
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

    testCase "Verify that union cases can have `-` in their names" <| fun () ->
        // See https://github.com/fable-compiler/Fable/issues/4071
        let x = LangCode.``pt-Br``
        equal x LangCode.``pt-Br``

    testCase "sprintf formats strings cases correctly" <| fun () ->
        let s = sprintf "%A" (S "1")
        equal s "S \"1\""

    testCase "Union cases with no fields are physically equal" <| fun () ->
        obj.ReferenceEquals(Gender.Male, Gender.Male) |> equal true
        obj.ReferenceEquals(Gender.Male, Gender.Female) |> equal false
        obj.ReferenceEquals(TestUnion.Case0, TestUnion.Case0) |> equal true
        obj.ReferenceEquals(TestUnion.Case1 "a", TestUnion.Case1 "a") |> equal false
        obj.ReferenceEquals(T1, T1) |> equal true

    testCase "Named union case fields work when a field is called name" <| fun () ->
        let value = NamedCase("v1", "v2")
        match value with
        | NamedCase(name, optional) ->
            name |> equal "v1"
            optional |> equal "v2"

    testCase "Static union member returning the union type works" <| fun () ->
        Example.All |> Array.length |> equal 2
        Example.All.[0] |> equal Example.First
        Example.All.[1] |> equal Example.Second

    testCase "Static union member with nested self-reference works" <| fun () ->
        Example.Head |> equal (Some Example.First)
        Example.Nested |> List.head |> Array.head |> equal Example.First
        Example.Pair |> fst |> equal Example.First
        Example.Pair |> snd |> equal Example.Second
        Example.Factory() |> equal Example.First

    testCase "Struct unions work" <| fun () ->
        let distance p =
            match p with
            | Planar2D (x, y) -> sqrt (x * x + y * y)
            | Infinity -> infinity
        let p = Planar2D (3, 4)
        let res = distance p
        res |> equal 5.

    testCase "Struct unions with fields work" <| fun () ->
        let radius = 3.0
        let circle = StructShape.Circle radius
        let square = StructShape.Square(int (2.0 * radius))
        circle.Area |> equal (System.Math.PI * radius * radius)
        square.Area |> equal (4.0 * radius * radius)

    testCase "Struct unions without fields work" <| fun () ->
        let radius = 3.0
        let circle = StructShape2.Circle radius
        let square = StructShape2.Square(2.0 * radius)
        circle.Area |> equal (System.Math.PI * radius * radius)
        square.Area |> equal (4.0 * radius * radius)

    testCase "Nested union match keeps case-specific field access" <| fun () ->
        nestedUnionCaseAccess (NestedA 7) |> equal 0
        nestedUnionCaseAccess (NestedB 11) |> equal -1

    testCase "Union case matching works" <| fun () ->
        let x = IntCase1 5
        let res =
            match x with
            | IntCase1 a -> a
            | IntCase2 b -> b
            | IntCase3 c -> c
        res |> equal 5

    // testCase "Union case equality works" <| fun () ->
    //     IntCase1 5 = IntCase1 5 |> equal true
    //     IntCase1 5 = IntCase2 5 |> equal false
    //     IntCase3 2 = IntCase3 3 |> equal false
    //     IntCase2 1 = IntCase2 1 |> equal true
    //     IntCase3 1 = IntCase3 1 |> equal true

    testCase "Union fn call works" <| fun () ->
        let x = IntCase1 3
        let res = unionFnAlways1 x
        let res2 = unionFnAlways1 x //deliberately force clone/borrow break
        let res3 = unionFnRetNum x
        let res4 = unionFnRetNum (IntCase2 24)
        res |> equal 3
        res2 |> equal 3
        res3 |> equal 3
        res4 |> equal 24

    testCase "Union with wrapped type works" <| fun () ->
        let a = AString "hello"
        let b = match a with AString s -> s + " world"
        b |> equal "hello world"

    testCase "Deep union with wrapped type works" <| fun () ->
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

    testCase "Deep union with tuped prim type works" <| fun () ->
        let e = DeepWrappedE (3, 2)
        let c = DeepWrappedC 42 //todo to string!
        let g = DeepWrappedG {| X = { Value = "G" }; Y = 365 |}
        e |> matchNumbers |> equal 5
        c |> matchNumbers |> equal 42
        g |> matchNumbers |> equal 365

    testCase "Multi-case Union with wrapped type works" <| fun () ->
        let b = DeepWrappedB "hello"
        let res = match b with DeepWrappedB s -> s + " world" | _ -> ""
        res |> equal "hello world"

    testCase "Match with condition works" <| fun () ->
        let b1 = DeepWrappedB "hello"
        let b2 = DeepWrappedB "not"
        b1 |> matchStringWhenStringNotHello |> equal "hello"
        b2 |> matchStringWhenStringNotHello |> equal "not hello"

  ]
