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

[<Erase>]
#endif
type DU = Int of int | Str of string


type DD = {
        a: string
        b: string
    }


type D =
    | DA of string

type EE = {
    a: string
    b: string
}

type E =
    | EA of string

type DDD = DD * D list
type EEE = EE * E list

type UnionType = Fable.Core.U2<DDD, EEE>


let tests =
  testList "Unions" [
    testCase "Union cases matches with no arguments can be generated" <| fun () ->
        let x = Male
        match x with
        | Female -> true
        | Male -> false
        |> equal false

    testCase "Union cases matches with one argument can be generated" <| fun () ->
        let x = Left "abc"
        match x with
        | Left data -> data
        | Right _ -> failwith "unexpected"
        |> equal "abc"

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
        let x = TestUnion.Case2("a", "b")
        match x with
        | TestUnion.Case0 -> failwith "unexpected"
        | TestUnion.Case1 _
        | TestUnion.Case2 _ -> "a"
        | TestUnion.Case3(a, b, c) -> a + b + c
        |> equal "a"

    testCase "Union cases called Tag still work (bug due to Tag field)" <| fun () ->
        let x = Tag "abc"
        match x with
        | Tag x -> x
        | _ -> failwith "unexpected"
        |> equal "abc"

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

    testCase "Equality works in filter" <| fun () ->
        let original = [| { Name = "1"; Case = MyUnion.Case1 } ; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
        original
        |> Array.filter (fun r -> r.Case = MyUnion.Case1)
        |> Array.length
        |> equal 2

    testCase "Pattern matching works in filter" <| fun () ->
        let original = [| { Name = "1"; Case = MyUnion.Case1 } ; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
        original
        |> Array.filter (fun r -> match r.Case with MyUnion.Case1 -> true | _ -> false)
        |> Array.length
        |> equal 2

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


    testCase "Case testing with U2 works" <| fun () ->
        let a = UnionType.Case1 ({a = "abc"; b = "def"}, [])


        match a with
        | UnionType.Case1 x ->
            ()

        | UnionType.Case2 x ->
            failwith "Matched wrong union case"

  ]