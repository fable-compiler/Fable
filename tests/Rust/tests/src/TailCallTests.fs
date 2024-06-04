module Fable.Tests.TailCallTests

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

    let inc_tail n =
        let rec inc n acc =
            if n = 0 then acc
            else inc (n - 1) (acc + 1)
        inc n 0

    let inc_tail2 n =
        let mutable acc = 0
        let rec inc n =
            match n with
            | 0 -> acc
            | _ ->
                acc <- acc + 1
                inc (n - 1)
        inc n

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

    // let recWithFinally () =
    //     let mutable log = ""
    //     let rec test n =
    //       try
    //         log <- log + string "abcde"[n]
    //         if n < 4 then test (n+1)
    //       finally
    //         log <- log + string "ABCDE"[n]
    //     test 0
    //     log

    // let recWithUse () =
    //     let mutable log = ""
    //     let disp(n) =
    //       { new System.IDisposable with
    //           member x.Dispose() = log <- log + string "ABCDE"[n] }
    //     let rec test n =
    //       use _disp = disp(n)
    //       log <- log + string "abcde"[n]
    //       if n < 4 then test (n+1) else 0
    //     test 0 |> ignore
    //     log

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

let rec parseNum tokens acc = function
    | x::xs when x >= '0' && x <= '9' ->
        parseNum tokens (x::acc) xs
    | xs -> parseTokens ((List.rev acc)::tokens) xs

and parseTokens tokens = function
    | x::xs when x >= '0' && x <= '9' ->
        parseNum tokens [x] xs
    | x::xs -> parseTokens tokens xs
    | [] -> List.rev tokens

// type Element =
//     | Element of action: (unit->unit) * children: Element list
//     member this.Activate() =
//         match this with
//         | Element(action, children) ->
//             action()
//             for child in children do child.Activate()

[<Fact>]
let ``Tailcall works in tail position`` () =
    Issue3301.simple 100000 1 |> equal 100001

[<Fact>]
let ``Tailcall works with bindings`` () =
    Issue3301.binding 100000 1 |> equal 100001

[<Fact>]
let ``Tailcall works with tuple deconstruction`` () =
    Issue3301.tupleDeconstruction 100000 1 |> equal 100001

[<Fact>]
let ``Recursive functions can be tailcall optimized`` () =
    factorial1 1 10 |> equal 3628800

[<Fact>]
let ``Non-tailcall recursive functions work`` () =
    factorial2 10 |> equal 3628800

[<Fact>]
let ``Nested functions can be tailcall optimized`` () =
    factorial3 10 |> equal 3628800

[<Fact>]
let ``Recursive non-capturing lambdas can be tailcall optimized`` () =
    inc_tail 1_000_000 |> equal 1_000_000

[<Fact>]
let ``Recursive capturing lambdas can be tailcall optimized`` () =
    inc_tail2 1_000_000 |> equal 1_000_000

[<Fact>]
let ``Arguments can be consumed after being \"passed\" in tailcall optimizations`` () =
    sum 0L 1_000_000L 0L
    |> equal 499999500000L

[<Fact>]
let ``Class methods can be tailcall optimized`` () =
    let x = MyTailCall()
    x.Sum(0L, 1_000_000L, 0L)
    |> equal 499999500000L

[<Fact>]
let ``Mutually recursive functions can be partially optimized`` () =
    let s = "a5b6c"
    s.ToCharArray() |> Array.toList |> parseTokens []
    |> List.concat |> List.map string |> String.concat ""
    |> equal "56"

[<Fact>]
let ``IIFEs prevent tailcall optimization`` () = // See #674
    iife [5; 4; 3] |> equal 24

[<Fact>]
let ``Tailcall optimization doesn't cause endless loops`` () = // See #675
    One("a", 42)
    |> tryFind "a"
    |> equal (Some 42)
    Tree<string, int>.Empty
    |> tryFind "a"
    |> equal None

// [<Fact>]
// let ``Recursive functions containing finally work`` () =
//     recWithFinally () |> equal "abcdeEDCBA"

// [<Fact>]
// let ``Recursive functions containing use work`` () =
//     recWithUse () |> equal "abcdeEDCBA"

[<Fact>]
let ``Function arguments can be optimized`` () = // See #681
    functionArguments [1;2;3] ((+) 2) |> equal 11

[<Fact>]
let ``Function arguments can be optimized II`` () = // See #681
    iterate ((*) 2) 5 10 |> equal 320

// // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
// [<Fact>]
// let ``State of internally mutated tail called function parameters is preserved properly`` () =
//     let rec loop i lst =
//         if i <= 0
//         then lst
//         else loop (i - 1) ((fun () -> i) :: lst)
//     loop 3 [] |> List.map (fun f -> f()) |> equal [1;2;3]

// [<Fact>]
// let ``State of internally mutated tail called function parameters is preserved properly II`` () =
//     let rec loop lst i =
//         if i <= 0
//         then lst
//         else loop ((fun () -> i) :: lst) (i - 1)
//     loop [] 3 |> List.map (fun f -> f()) |> equal [1;2;3]

// See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
[<Fact>]
let ``Functions returning unit can be tail called`` () =
    let cmpsts = Array.zeroCreate 16384
    let rec loop x =
        if x > 0 then
            let rec loopi i =
                if i <= 254 then
                    if cmpsts[i >>> 3] &&& (1uy <<< (i &&& 7)) <> 0uy then loopi (i + 1) else
                    let p = i + i + 3
                    let s0 = 2 * i * (i + 3) + 3
                    let lmt = min 131072 <| s0 + (p <<< 3)
                    let rec loops s =
                        if s < lmt then
                            let msk = 1uy <<< (s &&& 7)
                            let rec loopj j =
                                let cmpsts = cmpsts
                                if j < 16384 then
                                    cmpsts[j] <- cmpsts[j] ||| msk
                                    loopj (j + p)
                            loopj (s >>> 3); loops (s + p)
                    loops s0; loopi (i + 1)
            loopi 0; loop (x - 1)
    loop 100
    let oddprms = seq {
        for i in 0 .. 131071 do
            if cmpsts[i >>> 3] &&& (1uy <<< (i &&& 7)) = 0uy
            then yield i + i + 3 }
    seq { yield 2; yield! oddprms }
    |> Seq.length |> equal 23000

// [<Fact>]
// let ``Expressiones captured in a closure don't change after tail-call optimizations`` () = // See #1859
//     let mutable accValue = 0
//     let el action children = Element(action, children)

//     let init () =
//         [|1 .. 4|] |> Array.map (fun i -> i, false) |> Map.ofArray

//     let update msg (model: Map<int,bool>) =
//         accValue <- accValue + msg
//         let b = model[msg]
//         Map.add msg (not b) model

//     let view model dispatch =
//         let rec looper acc x =
//             match x with
//             |(i1,b1)::(i2,b2)::rest ->
//                 let newParent =
//                     el ignore [
//                         el (fun _ ->
//                             // Force the x argument to be captured
//                             // by the closure
//                             match x with
//                             |(i1,_)::_ -> i1 |> dispatch
//                             | _ -> ()) []
//                         el (fun _ -> i2 |> dispatch) []
//                     ]
//                 looper (newParent::acc) rest
//             |(i,b)::rest ->
//                 let newParent =
//                     el (fun _ -> i |> dispatch) []
//                 looper (newParent::acc) rest
//             |[] -> acc

//         // Use two looper references to prevent the binding
//         // beta reduction and force the tail-call optimization
//         model |> Map.toList |> looper [] |> ignore
//         model |> Map.toList |> looper [] |> el ignore

//     let main init view update =
//         let mutable model = init()
//         let dispatch m msg =
//             model <- update msg m
//         let el: Element = view model (dispatch model)
//         el.Activate()

//     main init view update
//     accValue |> equal 10
