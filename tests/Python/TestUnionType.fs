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

// type TestUnion =
//    | Case0
//    | Case1 of string
//    | Case2 of string * string
//    | Case3 of string * string * string


// type TestUnion2 =
//     | Tag of string
//     | NewTag of string

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

// let (|A|) n = n

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

// type Wrapper(s: string) =
//     member x.Value = s |> Seq.rev |> Seq.map string |> String.concat ""

// [<RequireQualifiedAccess>]
// type MyUnion =
// | Case1
// | Case2
// | Case3

// type R = {
//     Name: string
//     Case: MyUnion
// }

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
