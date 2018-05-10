module Fable.Tests.TailCalls

open Util.Testing

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
        let f2 = f >> ((+) 3)
        match x with
        | [] -> failwith "empty list"
        | [x] -> f x
        | h::t -> functionArguments t f2

    let rec iterate f n x =
        match n with
        | 0 -> x
        | _ -> iterate f (n - 1) (f x)

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

    let recWithUse () =
        let mutable log = ""
        let disp(n) =
          { new System.IDisposable with
              member x.Dispose() = log <- log + string "ABCDE".[n] }
        let rec test n =
          use _disp = disp(n)
          log <- log + string "abcde".[n]
          if n < 4 then test (n+1) else 0
        test 0 |> ignore
        log

open Functions

type MyTailCall() =
    member x.Sum(v, m, s) =
        if v >= m
        then s
        else x.Sum(v + 1L, m, s + v)

let rec parseNum tokens acc = function
  | x::xs when x >= '0' && x <= '9' ->
      parseNum tokens (x::acc) xs
  | xs -> parseTokens ((List.rev acc)::tokens) xs

and parseTokens tokens = function
  | x::xs when x >= '0' && x <= '9' ->
      parseNum tokens [x] xs
  | x::xs -> parseTokens tokens xs
  | [] -> List.rev tokens

let tests =
  testList "TailCalls" [
    testCase "Recursive functions can be tailcall optimized" <| fun () ->
        factorial1 1 10 |> equal 3628800

    testCase "Non-tailcall recursive functions work" <| fun () ->
        factorial2 10 |> equal 3628800

    testCase "Nested functions can be tailcall optimized" <| fun () ->
        factorial3 10 |> equal 3628800

    testCase "Arguments can be consumed after being \"passed\" in tailcall optimizations" <| fun () ->
        sum 0L 100000L 0L
        |> equal 4999950000L

    testCase "Class methods can be tailcall optimized" <| fun () ->
        let x = MyTailCall()
        x.Sum(0L, 100000L, 0L)
        |> equal 4999950000L

    testCase "Mutually recursive functions can be partially optimized" <| fun () ->
        let s = "a5b6c"
        s.ToCharArray() |> Seq.toList |> parseTokens []
        |> Seq.concat |> Seq.map string |> String.concat ""
        |> equal "56"

    testCase "IIFEs prevent tailcall optimization" <| fun () -> // See #674
        iife [5; 4; 3] |> equal 24

    testCase "Tailcall optimization doesn't cause endless loops" <| fun () -> // See #675
        One("a", 42)
        |> tryFind "a"
        |> equal (Some 42)
        Tree.Empty
        |> tryFind "a"
        |> equal None

    testCase "Recursive functions containing finally work" <| fun () ->
        recWithFinally () |> equal "abcdeEDCBA"

    testCase "Recursive functions containing use work" <| fun () ->
        recWithUse () |> equal "abcdeEDCBA"

    testCase "Function arguments can be optimized" <| fun () -> // See #681
        functionArguments [1;2;3] ((+) 2) |> equal 11

    testCase "Function arguments can be optimized II" <| fun () -> // See #681
        iterate ((*) 2) 5 10 |> equal 320
  ]