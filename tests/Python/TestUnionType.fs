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

type MyUnion =
   | Case0
   | Case1 of string
   | Case2 of string * string
   | Case3 of string * string * string


type MyUnion2 =
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
type MyUnion3 =
| Case1
| Case2
| Case3

type R = {
    Name: string
    Case: MyUnion3
}

#if FABLE_COMPILER
open Fable.Core

[<Erase>]
#endif
type DU = Int of int | Str of string

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
    let x = MyUnion.Case2("a", "b")
    match x with
    | MyUnion.Case0 -> failwith "unexpected"
    | MyUnion.Case1 _
    | MyUnion.Case2 _ -> "a"
    | MyUnion.Case3(a, b, c) -> a + b + c
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
#endif

// FIXME:
// [<Fact>]
// let ``test Equality works in filter`` () =
//     let original = [| { Name = "1"; Case = MyUnion3.Case1 } ; { Name = "2"; Case = MyUnion3.Case1 }; { Name = "3"; Case = MyUnion3.Case2 }; { Name = "4"; Case = MyUnion3.Case3 } |]
//     original
//     |> Array.filter (fun r -> r.Case = MyUnion3.Case1)
//     |> Array.length
//     |> equal 2
