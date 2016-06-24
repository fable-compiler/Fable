[<NUnit.Framework.TestFixture>] 
module Fable.Tests.UnionTypes
open NUnit.Framework
open Fable.Tests.Util
open Fable.Core

type Gender = Male | Female

[<Test>]
let ``Union cases matches with no arguments can be generated``() =
    let x = Male
    match x with
    | Female -> true
    | Male -> false
    |> equal false

type Either<'TL,'TR> =
    | Left of 'TL
    | Right of 'TR
    override x.ToString() =
      match x with
      | Left y -> y.ToString()
      | Right y -> y.ToString()

[<Test>]
let ``Union cases matches with one argument can be generated``() =
    let x = Left "abc"
    match x with
    | Left data -> data
    | Right _ -> failwith "unexpected"
    |> equal "abc"

[<Test>]
let ``Union methods can be generated``() =
    let x = Left 5 
    x.ToString()
    |> equal "5"
    
[<Test>]
let ``Nested pattern matching works``() =
    let x = Right(Left 5)
    match x with
    | Left _ -> failwith "unexpected"
    | Right x ->
        match x with
        | Left x -> x
        | Right _ -> failwith "unexpected"
    |> equal 5

type TestUnion =
   | Case0
   | Case1 of string
   | Case2 of string * string
   | Case3 of string * string * string

[<Test>]
let ``Union cases matches with many arguments can be generated``() =
    let x = Case3("a", "b", "c")
    match x with
    | Case3(a, b, c) -> a + b + c
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Test>]
let ``Pattern matching with common targets works``() =
    let x = Case2("a", "b")
    match x with
    | Case0 -> failwith "unexpected"
    | Case1 _
    | Case2 _ -> "a"
    | Case3(a, b, c) -> a + b + c
    |> equal "a"

type TestUnion2 =
    | Tag of string
    | NewTag of string

[<Test>]
let ``Union cases called Tag still work (bug due to Tag field)``() =
    let x = Tag "abc"
    match x with
    | Tag x -> x
    | _ -> failwith "unexpected"
    |> equal "abc"

#if MOCHA
type JsonTypeInner = {
    Prop1: string
    Prop2: int
}

type JsonTestUnion =
    | IntType of int
    | StringType of string
    | TupleType of string * int
    | ObjectType of JsonTypeInner

[<Emit("JSON.parse($0)")>]
let jsonParse (json: string) = failwith "JS Only"

[<Emit("JSON.stringify($0)")>]
let jsonStringify (json): string = failwith "JS Only"

[<Test>]
let ``Pattern matching json parse union cases still works``() =
    // Test IntType
    match jsonParse """{"Case":"IntType","Fields":[1]}""" with
    | IntType x -> x
    | _ -> failwith "unexpected"
    |> equal 1
    // Test StringType
    match jsonParse """{"Case":"StringType","Fields":["value1"]}""" with
    | StringType x -> x
    | _ -> failwith "unexpected"
    |> equal "value1"
    // Test TupleType
    match jsonParse """{"Case":"TupleType","Fields":["value1",2]}""" with
    | TupleType(x, y) -> x, y
    | _ -> failwith "unexpected"
    |> fun (x, y) ->
        x |> equal "value1"
        y |> equal 2
    // Test ObjectType
    match jsonParse """{"Case":"ObjectType","Fields":[{"Prop1":"value1","Prop2":2}]}""" with
    | ObjectType(x) -> x
    | _ -> failwith "unexpected"
    |> fun x ->
        x.Prop1 |> equal "value1"
        x.Prop2 |> equal 2

[<Test>]
let ``Union cases json stringify is as we expect``() =
    ObjectType({Prop1 = "value1"; Prop2 = 2})
    |> jsonStringify
    |> equal """{"Case":"ObjectType","Fields":[{"Prop1":"value1","Prop2":2}]}"""

type Tree =
    | Leaf of int
    | Branch of Tree[]
    member this.Sum() =
        match this with
        | Leaf i -> i
        | Branch trees -> trees |> Seq.map (fun x -> x.Sum()) |> Seq.sum 

[<Test>]
let ``Unions can be JSON serialized forth and back``() =
    let tree = Branch [|Leaf 1; Leaf 2; Branch [|Leaf 3; Leaf 4|]|]
    let sum1 = tree.Sum() 
    let json = Serialize.toJson tree
    let tree2 = Serialize.ofJson<Tree> json
    let sum2 = tree2.Sum()
    equal true (box tree2 :? Tree) // Type is kept
    equal true (sum1 = sum2) // Prototype methods can be accessed
#endif

[<Test>]
let ``Option.isSome/isNone works``() =
    let o1 = None
    let o2 = Some 5
    Option.isNone o1 |> equal true
    Option.isSome o1 |> equal false
    Option.isNone o2 |> equal false
    Option.isSome o2 |> equal true

[<Test>]
let ``Option.IsSome/IsNone works``() =
    let o1 = None
    let o2 = Some 5
    o1.IsNone |> equal true
    o1.IsSome |> equal false
    o2.IsNone |> equal false
    o2.IsSome |> equal true

[<Test>]
let ``Option.iter works``() = // See #198
    let mutable res = false
    let getOnlyOnce =
        let mutable value = Some "Hello"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.iter (fun s -> if s = "Hello" then res <- true)
    equal true res

[<Test>]
let ``Option.map works``() =
    let mutable res = false
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.map ((+) "Hello ") |> equal (Some "Hello Alfonso")

[<Test>]
let ``Option.bind works``() =
    let mutable res = false
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.bind ((+) "Hello " >> Some) |> equal (Some "Hello Alfonso")
