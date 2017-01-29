[<Util.Testing.TestFixture>]
module Fable.Tests.TailCalls

open Util.Testing
open Fable.Tests.Util

open System
open System.Collections.Generic

module Functions =
    let rec factorial1 aux n =
        if n = 0 then aux
        else factorial1 (aux * n) (n - 1)

    let rec factorial2 n =
        match n with
        | 0 | 1 -> 1
        | _ -> n * factorial2(n-1)

    let factorial3 n =
        let rec loop i acc =
            match i with
            | 0 | 1 -> acc
            | _ -> loop (i-1) (acc * i)
        loop n 1

    let rec sum v m s =
        if v >= m
        then s
        else sum (v + 1L) m (s + v)

    let rec iife a =
        2 * (match id a with
            | []  -> 0
            | [x]  -> x
            | h::t -> iife t)

    type Tree<'Key,'T> =
        | Empty
        | Node of 'Key * 'T * Tree<'Key,'T> *  Tree<'Key,'T> * int

    let One(k,v) = Node(k,v,Empty,Empty,1)

    let rec tryFind k m =
        match m with
        | Empty -> None
        | Node(k2,v2,l,r,_) ->
            let c = compare k k2
            if c < 0 then tryFind k l
            elif c = 0 then Some v2
            else tryFind k r

    let rec functionArguments x f =
        match x with
        | [] -> f x
        | h::t -> functionArguments t (f << id)

    let recWithFinally () =
        let mutable log = ""
        let rec test n =   
          try 
            log <- log + string "abcde".[n] 
            if n < 4 then test (n+1) 
          finally
            log <- log + string "ABCDE".[n] 
        test 0    
        log

open Functions

[<Test>]
let ``Recursive functions can be tailcall optimized``() =
    factorial1 1 10 |> equal 3628800

[<Test>]
let ``Non-tailcall recursive functions work``() =
    factorial2 10 |> equal 3628800

[<Test>]
let ``Nested functions can be tailcall optimized``() =
    factorial3 10 |> equal 3628800

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

[<Test>]
let ``IIFEs prevent tailcall optimization``() = // See #674
    iife [5; 4; 3] |> equal 24

[<Test>]
let ``Tailcall optimization doesn't cause endless loops``() = // See #675
    One("a", 42)
    |> tryFind "a"
    |> equal (Some 42)
    Tree.Empty
    |> tryFind "a"
    |> equal None

[<Test>]
let ``Recursive functions containing finally work``() =
    recWithFinally () |> equal "abcdeEDCBA"

let ``Function arguments prevent tail call optimization``() = // See #681
    functionArguments [1;2;3] id
    |> equal []
    

    
