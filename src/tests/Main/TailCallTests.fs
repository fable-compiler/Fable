[<Util.Testing.TestFixture>]
module Fable.Tests.TailCalls

open Util.Testing
open Fable.Tests.Util

open System
open System.Collections.Generic

let rec factorial1 aux n =
  if n = 0 then aux
  else factorial1 (aux * n) (n - 1)

[<Test>]
let ``Recursive functions can be tailcall optimized``() =
    factorial1 1 10 |> equal 3628800

let rec factorial2 n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial2(n-1)

[<Test>]
let ``Non-tailcall recursive functions work``() =
    factorial2 10 |> equal 3628800

let factorial3 n =
    let rec loop i acc =
        match i with
        | 0 | 1 -> acc
        | _ -> loop (i-1) (acc * i)
    loop n 1

[<Test>]
let ``Nested functions can be tailcall optimized``() =
    factorial3 10 |> equal 3628800

let rec sum v m s =
    if v >= m
    then s
    else sum (v + 1L) m (s + v)

[<Test>]
let ``Arguments can be consumed after being "passed" in tailcall optimizations``() =
    sum 0L 100000L 0L
    |> equal 4999950000L

type MyTailCall() =
    member x.Sum(v, m, s) =
        if v >= m
        then s
        else x.Sum(v + 1L, m, s + v)

[<Test>]
let ``Class methods can be tailcall optimized``() =
    let x = MyTailCall()
    x.Sum(0L, 100000L, 0L)
    |> equal 4999950000L

let rec parseNum tokens acc = function
  | x::xs when x >= '0' && x <= '9' ->
      parseNum tokens (x::acc) xs
  | xs -> parseTokens ((List.rev acc)::tokens) xs

and parseTokens tokens = function
  | x::xs when x >= '0' && x <= '9' ->
      parseNum tokens [x] xs
  | x::xs -> parseTokens tokens xs
  | [] -> List.rev tokens

[<Test>]
let ``Mutually recursive functions can be partially optimized``() =
    let s = "a5b6c"
    s.ToCharArray() |> Seq.toList |> parseTokens []
    |> Seq.concat |> Seq.map string |> String.concat ""
    |> equal "56"
