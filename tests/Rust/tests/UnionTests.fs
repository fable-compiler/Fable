module Fable.Tests.Union

open Util.Testing

type MyUnion =
    | Case1 of int
    | Case2 of int
    | Case3 of int

[<Fact>]
let ``Union case matching works`` () =
    let x = Case1 5
    let res =
        match x with
        | Case1 a -> a
        | Case2 b -> b
        | Case3 c -> c
    res |> equal 5

[<Fact>]
let ``Union case equality works`` () =
    Case1 5 = Case1 5 |> equal true
    Case1 5 = Case2 5 |> equal false
    Case3 2 = Case3 3 |> equal false
    Case2 1 = Case2 1 |> equal true
    Case3 1 = Case3 1 |> equal true

let unionFnAlways1 = function
    | Case1 x -> x
    | _ -> -1
let unionFnRetNum = function
    | Case1 a -> a
    | Case2 b -> b
    | Case3 c -> c

[<Fact>]
let ``Union fn call works`` () =
    let x = Case1 3
    let res = unionFnAlways1 x
    let res2 = unionFnAlways1 x //deliberately force clone/borrow break
    let res3 = unionFnRetNum x
    let res4 = unionFnRetNum (Case2 24)
    res |> equal 3
    res2 |> equal 3
    res3 |> equal 3
    res4 |> equal 24

type WrappedUnion =
    | AString of string
    // | BString of string

let ``Union with wrapped type works`` () =
    let a = AString "hello"
    let b = match a with AString s -> s + " world" //| _ -> ""
    b |> equal "hello world"

type DeepRecord = {Value: string}
type DeepWrappedUnion =
    //| DeepWrappedA of string * DeepRecord
    | DeepWrappedB of string
    | DeepWrappedC of int
    //| DeepWrappedD of DeepRecord    //does not yet work

let matchStrings = function
    //| DeepWrappedA (s, d) -> d.Value + s //todo - not working
    | DeepWrappedB s -> s
    | DeepWrappedC c -> "nothing"
    //| DeepWrappedD d -> d.Value //todo - not working

let ``Deep union with wrapped type works`` () =
    //let a = DeepWrappedA (" world", {Value = "hello"})
    let b = DeepWrappedB "world"
    let c = DeepWrappedC 42//todo to string!
    //let d = DeepWrappedD { Value = "hello" }//todo to string!
    //a |> matchStrings |> equal "hello world"
    b |> matchStrings |> equal "world"
    c |> matchStrings |> equal "nothing"
   // d |> matchStrings |> equal "hello"