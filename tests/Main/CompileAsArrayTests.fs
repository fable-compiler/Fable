[<Util.Testing.TestFixture>]
module Fable.Tests.CompileAsArray

open Fable.Core
open Util.Testing
open Fable.Tests.Util

[<CompileAsArray>]
type Gender = Male | Female

[<Test>]
let ``Union cases matches with no arguments can be generated``() =
    let x = Male
    match x with
    | Female -> true
    | Male -> false
    |> equal false

[<CompileAsArray>]
type Either<'TL,'TR> =
    | Left of 'TL
    | Right of 'TR
    // override x.ToString() =
    //   match x with
    //   | Left y -> y.ToString()
    //   | Right y -> y.ToString()

[<Test>]
let ``Union cases matches with one argument can be generated``() =
    let x = Left "abc"
    match x with
    | Left data -> data
    | Right _ -> failwith "unexpected"
    |> equal "abc"

// [<Test>]
// let ``Union methods can be generated``() =
//     let x = Left 5
//     x.ToString()
//     |> equal "5"

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

[<CompileAsArray>]
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

[<CompileAsArray>]
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
