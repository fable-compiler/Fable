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

open Functions

module Issue3301 =
    let rec simple x a =
        if x <= 0 then a
        else
            simple (x-1) (a+1)

    let rec binding x a =
        if x <= 0 then a
        else
            let bb = binding (x-1)
            bb (a+1)

    let rec tupleDeconstruction x a =
        if x <= 0 then a
        else
            let (bb, _) = (tupleDeconstruction, 1)
            bb (x-1) (a+1)

type MyTailCall() =
    member x.Sum(v, m, s) =
        if v >= m
        then s
        else x.Sum(v + 1L, m, s + v)

[<Fact>]
let ``test Tailcall works in tail position`` () =
#if FABLE_COMPILER
    Issue3301.simple 100000 1 |> equal 100001
#else
    Issue3301.simple 1000 1 |> equal 1001
#endif

[<Fact>]
let ``test Tailcall works with bindings`` () =
#if FABLE_COMPILER
    Issue3301.binding 100000 1 |> equal 100001
#else
    Issue3301.binding 100 1 |> equal 101
#endif

[<Fact>]
let ``test Tailcall works with tuple deconstruction`` () =
#if FABLE_COMPILER
    Issue3301.tupleDeconstruction 100000 1 |> equal 100001
#else
    Issue3301.tupleDeconstruction 100 1 |> equal 101
#endif

[<Fact>]
let ``test Recursive functions can be tailcall optimized`` () =
    factorial1 1 10 |> equal 3628800

[<Fact>]
let ``test Non-tailcall recursive functions work`` () =
    factorial2 10 |> equal 3628800

[<Fact>]
let ``test Nested functions can be tailcall optimized`` () =
    factorial3 10 |> equal 3628800

[<Fact>]
let ``test Arguments can be consumed after being passed in tailcall optimizations`` () =
#if FABLE_COMPILER
    sum 0L 100000L 0L
    |> equal 4999950000L
#else
    sum 0L 1000L 0L
    |> equal 499500L
#endif

[<Fact>]
let ``test Class methods can be tailcall optimized`` () =
    let x = MyTailCall()
#if FABLE_COMPILER
    x.Sum(0L, 100000L, 0L)
    |> equal 4999950000L
#else
    x.Sum(0L, 1000L, 0L)
    |> equal 499500L
#endif

[<Fact>]
let ``test IIFEs prevent tailcall optimization`` () =
    iife [5; 4; 3] |> equal 24

[<Fact>]
let ``test Tailcall optimization doesnt cause endless loops`` () =
    One("a", 42)
    |> tryFind "a"
    |> equal (Some 42)
    Tree.Empty
    |> tryFind "a"
    |> equal None

[<Fact>]
let ``test Function arguments can be optimized`` () =
    functionArguments [1;2;3] ((+) 2) |> equal 11

[<Fact>]
let ``test Function arguments can be optimized II`` () =
    iterate ((*) 2) 5 10 |> equal 320

[<Fact>]
let ``test State of internally mutated tail called function parameters is preserved properly`` () =
    let rec loop i lst =
        if i <= 0
        then lst
        else loop (i - 1) ((fun () -> i) :: lst)
    loop 3 [] |> List.map (fun f -> f()) |> equal [1;2;3]

[<Fact>]
let ``test State of internally mutated tail called function parameters is preserved properly II`` () =
    let rec loop lst i =
        if i <= 0
        then lst
        else loop ((fun () -> i) :: lst) (i - 1)
    loop [] 3 |> List.map (fun f -> f()) |> equal [1;2;3]
