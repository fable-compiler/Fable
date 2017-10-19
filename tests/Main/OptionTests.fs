[<Util.Testing.TestFixture>]
module Fable.Tests.Option
open Util.Testing
open Fable.Tests.Util

type Tree =
    | Leaf of int
    | Branch of Tree[]
    member this.Sum() =
        match this with
        | Leaf i -> i
        | Branch trees -> trees |> Seq.map (fun x -> x.Sum()) |> Seq.sum

[<Test>]
let ``Option.defaultValue works``() =
    Some 5 |> Option.defaultValue 4 |> equal 5
    None |> Option.defaultValue "foo" |> equal "foo"

[<Test>]
let ``Option.orElse works``() =
    Some 5 |> Option.orElse (Some 4) |> equal (Some 5)
    None |> Option.orElse (Some "foo") |> equal (Some "foo")

[<Test>]
let ``Option.defaultWith works``() =
    Some 5 |> Option.defaultWith (fun () -> 4) |> equal 5
    None |> Option.defaultWith (fun () -> "foo") |> equal "foo"

[<Test>]
let ``Option.orElseWith works``() =
    Some 5 |> Option.orElseWith (fun () -> Some 4) |> equal (Some 5)
    None |> Option.orElseWith (fun () -> Some "foo") |> equal (Some "foo")

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

[<Test>]
let ``Option.filter works``() = // See #390
    let optionToString opt =
        match opt with
        | None -> "None"
        | Some value -> sprintf "Some %s" value
    Some 7 |> Option.filter (fun _ -> false) |> Option.map string |> optionToString |> equal "None"
    Some 7 |> Option.filter (fun _ -> true)  |> Option.map string |> optionToString |> equal "Some 7"
    Some "A" |> Option.filter (fun _ -> false) |> optionToString |> equal "None"
    Some "A" |> Option.filter (fun _ -> true) |> optionToString |> equal "Some A"

[<Test>]
let ``Option.fold works``() =
    (5, None) ||> Option.fold (*) |> equal 5
    (5, Some 7) ||> Option.fold (*) |> equal 35

[<Test>]
let ``Option.foldBack works``() =
    (None, 5) ||> Option.foldBack (*) |> equal 5
    (Some 7, 5) ||> Option.foldBack (*) |> equal 35

[<NoEquality; NoComparison>]
type FoldTest =
| FoldA of FoldTest option
| FoldB of int

let rec folding1 test acc =
  let f2 (opt:FoldTest option) acc = Option.fold (fun a b -> folding1 b a) acc opt
  match test with
  | FoldA d -> f2 d acc
  | FoldB i -> i::acc

let rec folding2 test acc =
  let f2 (opt:FoldTest option) acc = Option.foldBack folding2 opt acc
  match test with
  | FoldA d -> f2 d acc
  | FoldB i -> i::acc

[<Test>]
let ``Option.fold works II``() = // See #660
    folding1 (FoldA (Some (FoldB 1))) [] |> equal [1]

[<Test>]
let ``Option.foldBack works II``() = // See #660
    folding2 (FoldA (Some (FoldB 1))) [] |> equal [1]

[<Test>]
let ``Option.toArray works``() =
    None |> Option.toArray |> equal [||]
    Some (Leaf 7) |> Option.toArray |> equal [|Leaf 7|]

[<Test>]
let ``Option.toList works``() =
    None |> Option.toList |> equal []
    Some (Leaf 7) |> Option.toList |> equal [Leaf 7]

[<Test>]
let ``Option.flatten works``() =
    Some (Some 1) |> Option.flatten |> equal (Some 1)

// https://github.com/fable-compiler/Fable/issues/1136
[<Test>]
let ``Calling Some with side-effects works``() =
    let mutable state = 0
    let f x = state <- x
    let fo = f 3 |> Some

    state |> equal 3

type OptTest = OptTest of int option

[<Test>]
let ``Different ways of providing None to a union case should be equal``() = // See #231
    let value = None
    equal true ((OptTest None) = (value |> OptTest))

[<Test>]
let ``Different ways of providing None to a function should be equal``() = // See #231
    let f x = x
    let f2 x = x = None
    let value = None
    equal true ((f None) = (value |> f))
    equal true (f2 None)
    equal true (f2 value)
    equal false (Some 5 |> f2)

[<Test>]
let ``Accessing an option value gives correct expression type``() = // See #285
    let test (x: float option) =
        match x with
        | Some y -> y + 3.
        | None -> 0.
    test(Some 4.) |> equal 7.

[<Test>]
let ``Mixing refs and options works``() = // See #238
    let res = ref 0
    let setter, getter =
        let slot = ref None
        (fun f -> slot.Value <- Some f),
        (fun v -> slot.Value.Value v)
    setter (fun i -> res := i + 2)
    getter 5
    equal 7 !res

let makeSome (x: 'a): 'a option =
    Some x

[<Test>]
let ``Generic options work``() =
    let x1 = makeSome ()
    let x2 = makeSome None
    let x3 = makeSome null |> makeSome
    let x4 = makeSome 5
    Option.isSome x1 |> equal true
    Option.isNone x1 |> equal false
    x1.IsSome |> equal true
    x1.IsNone |> equal false
    match x1 with Some _ -> true | None -> false
    |> equal true
    Option.isSome x2 |> equal true
    Option.isNone x2 |> equal false
    x2.IsSome |> equal true
    x2.IsNone |> equal false
    match x2 with
    | Some(Some _) -> 0
    | Some(None) -> 1
    | None -> 2
    |> equal 1
    Option.isSome x3 |> equal true
    Option.isNone x3 |> equal false
    x3.IsSome |> equal true
    x3.IsNone |> equal false
    match x3 with
    | None -> 0
    | Some(None) -> 1
    | Some(Some _) -> 2
    |> equal 2
    match x4 with Some i -> i = 5 | None -> false
    |> equal true
    x4.Value = 5 |> equal true
    Option.get x4 = 5 |> equal true

[<Test>]
let ``Nested options work``() =
    let x1 = Some(Some 5)
    let x2 = Some(Some ())
    let x3 = Some(None)
    Option.isSome x1 |> equal true
    Option.isNone x1 |> equal false
    x1.IsSome |> equal true
    x1.IsNone |> equal false
    match x1 with
    | Some(Some 5) -> 0
    | Some(Some _) -> 1
    | Some(None) -> 2
    | None -> 3
    |> equal 0
    Option.isSome x2 |> equal true
    Option.isNone x2 |> equal false
    x2.IsSome |> equal true
    x2.IsNone |> equal false
    match x2 with
    | Some(None) -> 0
    | Some(Some _) -> 1
    | None -> 2
    |> equal 1
    Option.isSome x3 |> equal true
    Option.isNone x3 |> equal false
    x3.IsSome |> equal true
    x3.IsNone |> equal false
    match x3 with
    | None -> 0
    | Some(Some _) -> 1
    | Some(None) -> 2
    |> equal 2
