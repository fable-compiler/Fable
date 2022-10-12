module Fable.Tests.Option

open System
open Util.Testing

type Tree =
    | Leaf of int
    | Branch of Tree[]
    member this.Sum() =
        match this with
        | Leaf i -> i
        | Branch trees -> trees |> Seq.sumBy (fun x -> x.Sum())

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

type OptTest = OptTest of int option

let makeSome (x: 'a): 'a option =
    Some x


[<Fact>]
let ``test defaultArg works`` () =
    let f o = defaultArg o 5
    f (Some 2) |> equal 2
    f None |> equal 5

[<Fact>]
let ``test Option.defaultValue works`` () =
    let a = Some "MyValue"
    let b = None

    a |> Option.defaultValue "" |> equal "MyValue"
    b |> Option.defaultValue "default" |> equal "default"

[<Fact>]
let ``test Option.defaultValue works II`` () =
    Some 5 |> Option.defaultValue 4 |> equal 5
    None |> Option.defaultValue "foo" |> equal "foo"

[<Fact>]
let ``test Option.orElse works`` () =
    Some 5 |> Option.orElse (Some 4) |> equal (Some 5)
    None |> Option.orElse (Some "foo") |> equal (Some "foo")
    Some None |> Option.orElse (Some (Some "foo")) |> equal (Some None)
    Some (Some "foo") |> Option.orElse (Some (Some "bar")) |> equal (Some (Some "foo"))

[<Fact>]
let ``test Option.defaultWith works`` () =
    Some 5 |> Option.defaultWith (fun () -> 4) |> equal 5
    None |> Option.defaultWith (fun () -> "foo") |> equal "foo"

[<Fact>]
let ``test Option.orElseWith works`` () =
    Some 5 |> Option.orElseWith (fun () -> Some 4) |> equal (Some 5)
    None |> Option.orElseWith (fun () -> Some "foo") |> equal (Some "foo")
    Some None |> Option.orElseWith (fun () -> Some (Some "foo")) |> equal (Some None)
    Some (Some "foo") |> Option.orElseWith (fun () -> Some (Some "bar")) |> equal (Some (Some "foo"))

[<Fact>]
let ``test Option.isSome/isNone works`` () =
    let o1 = None
    let o2 = Some 5
    Option.isNone o1 |> equal true
    Option.isSome o1 |> equal false
    Option.isNone o2 |> equal false
    Option.isSome o2 |> equal true

[<Fact>]
let ``test Option.IsSome/IsNone works II`` () =
    let o1 = None
    let o2 = Some 5
    o1.IsNone |> equal true
    o1.IsSome |> equal false
    o2.IsNone |> equal false
    o2.IsSome |> equal true

[<Fact>]
let ``test Option.iter works`` () =
    let mutable res = false
    let getOnlyOnce =
        let mutable value = Some "Hello"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.iter (fun s -> if s = "Hello" then res <- true)
    equal true res

[<Fact>]
let ``test Option.map works`` () =
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.map ((+) "Hello ") |> equal (Some "Hello Alfonso")
    getOnlyOnce() |> Option.map ((+) "Hello ") |> equal None

[<Fact>]
let ``test Option.map2 works`` () =
    (Some 2, Some 3) ||> Option.map2 (+) |> equal (Some 5)
    (None, Some 3) ||> Option.map2 (+) |> equal None
    (Some 2, None) ||> Option.map2 (+) |> equal None

[<Fact>]
let ``test Option.map3 works`` () =
    (Some 2, Some 3, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal (Some 9)
    (None, Some 3, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal None
    (Some 2, None, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal None
    (Some 2, Some 3, None) |||> Option.map3 (fun x y z -> x + y + z) |> equal None

[<Fact>]
let ``test Option.bind works`` () =
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.bind ((+) "Hello " >> Some) |> equal (Some "Hello Alfonso")

[<Fact>]
let ``test Option.contains works`` () =
    Some "test" |> Option.contains "test" |> equal true
    Some "123" |> Option.contains "test" |> equal false
    None |> Option.contains "test" |> equal false

[<Fact>]
let ``test Option.filter works`` () =
    let optionToString opt =
        match opt with
        | None -> "None"
        | Some value -> sprintf "Some %s" value
    Some 7 |> Option.filter (fun _ -> false) |> Option.map string |> optionToString |> equal "None"
    Some 7 |> Option.filter (fun _ -> true)  |> Option.map string |> optionToString |> equal "Some 7"
    Some "A" |> Option.filter (fun _ -> false) |> optionToString |> equal "None"
    Some "A" |> Option.filter (fun _ -> true) |> optionToString |> equal "Some A"

[<Fact>]
let ``test Option.fold works`` () =
    (5, None) ||> Option.fold (*) |> equal 5
    (5, Some 7) ||> Option.fold (*) |> equal 35

[<Fact>]
let ``test Option.foldBack works`` () =
    (None, 5) ||> Option.foldBack (*) |> equal 5
    (Some 7, 5) ||> Option.foldBack (*) |> equal 35

[<Fact>]
let ``test Option.fold works II`` () =
    folding1 (FoldA (Some (FoldB 1))) [] |> equal [1]

[<Fact>]
let ``test Option.foldBack works II`` ( )=
    folding2 (FoldA (Some (FoldB 1))) [] |> equal [1]

[<Fact>]
let ``test Option.toArray works`` () =
    None |> Option.toArray |> equal [||]
    Some (Leaf 7) |> Option.toArray |> equal [|Leaf 7|]

[<Fact>]
let ``test Option.toList works`` () =
    None |> Option.toList |> equal []
    Some (Leaf 7) |> Option.toList |> equal [Leaf 7]

[<Fact>]
let ``test Option.flatten works`` () =
    let o1: int option option = Some (Some 1)
    let o2: int option option = Some None
    let o3: int option option = None
    Option.flatten o1 |> equal (Some 1)
    Option.flatten o2 |> equal None
    Option.flatten o3 |> equal None

[<Fact>]
let ``test Option.toObj works`` () =
    let o1: string option = Some "foo"
    let o2: string option = None
    Option.toObj o1 |> equal "foo"
    Option.toObj o2 |> equal null

[<Fact>]
let ``test Option.ofObj works`` () =
    let o1: string = "foo"
    let o2: string = null
    Option.ofObj o1 |> equal (Some "foo")
    Option.ofObj o2 |> equal None

// https://github.com/fable-compiler/Fable/issues/1136
[<Fact>]
let ``test Calling Some with side-effects works`` () =
    let mutable state = 0
    let f x = state <- x
    let _fo = f 3 |> Some
    state |> equal 3

[<Fact>]
let ``test Different ways of providing None to a union case should be equal`` () =
    let value = None
    equal true ((OptTest None) = (value |> OptTest))

[<Fact>]
let ``test Different ways of providing None to a function should be equal`` () =
    let f x = x
    let f2 x = x = None
    let value = None
    equal true ((f None) = (value |> f))
    equal true (f2 None)
    equal true (f2 value)
    equal false (Some 5 |> f2)

[<Fact>]
let ``test Accessing an option value gives correct expression type`` () =
    let test (x: float option) =
        match x with
        | Some y -> y + 3.
        | None -> 0.
    test(Some 4.) |> equal 7.

[<Fact>]
let ``test Mixing refs and options works`` () =
    let res = ref 0
    let setter, getter =
        let slot = ref None
        (fun f -> slot.Value <- Some f),
        // TODO!!! If we change this to `slot.Value.Value` it fails
        (fun v -> slot.Value.Value v)
    setter (fun i -> res.Value <- i + 2)
    getter 5
    equal 7 res.Value

[<Fact>]
let ``test Generic options work`` () =
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

[<Fact>]
let ``test Nested options work`` () =
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

[<Fact>]
let ``test Option.map ignore generates Some ()`` () =
    let mySome = Some ()
    let myOtherSome = mySome |> Option.map (ignore)
    equal mySome myOtherSome

[<Fact>]
let ``test Some (box null) |> Option.isSome evals to true`` () =
    Some (box null) |> Option.isSome |> equal true
    Some (null) |> Option.isSome |> equal true

[<Fact>]
let ``test Nullable works`` () =
    let x = Nullable 5
    x.HasValue |> equal true
    x.Value |> equal 5

    let y: Nullable<int> = Nullable()
    y.HasValue |> equal false
    let errorThrown = try y.Value |> ignore; false with _ -> true
    equal true errorThrown
