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

[<CompileAsArray>]
type RecursiveRecord =
    { things : RecursiveRecord list }

[<CompileAsArray>]
type Person =
    { name: string; mutable luckyNumber: int }

[<Test>]
let ``Recursive record does not cause issues``() =
    let r = { things = [ { things = [] } ] }
    equal r.things.Length 1

[<Test>]
let ``Record property access can be generated``() =
    let x = { name = "Alfonso"; luckyNumber = 7 }
    equal "Alfonso" x.name
    equal 7 x.luckyNumber
    x.luckyNumber <- 14
    equal 14 x.luckyNumber
    // Order of members in constructor shouldn't alter result
    let x = { luckyNumber = 9; name = "Maxime" }
    equal "Maxime" x.name
    equal 9 x.luckyNumber
    x.luckyNumber <- 12
    equal 12 x.luckyNumber


[<Test>]
let ``Record expression constructors can be generated``() =
    let x = { name = "Alfonso"; luckyNumber = 7 }
    let y = { x with luckyNumber = 14 }
    equal "Alfonso" y.name
    equal 14 y.luckyNumber

[<CompileAsArray>]
type JSKiller =
   { ``for`` : float; ``class`` : float }

[<CompileAsArray>]
type JSKiller2 =
   { ``s p a c e`` : float; ``s*y*m*b*o*l`` : float }

[<Test>]
let ``Records with key/reserved words are mapped correctly``() =
    let x = { ``for`` = 1.0; ``class`` = 2.0 }
    equal 2. x.``class``

[<Test>]
let ``Records with special characters are mapped correctly``() =
    let x = { ``s p a c e`` = 1.0; ``s*y*m*b*o*l`` = 2.0 }
    equal 1. x.``s p a c e``
    equal 2. x.``s*y*m*b*o*l``
