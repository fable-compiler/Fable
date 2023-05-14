module Fable.Tests.OptionTests

open Util.Testing

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

let getValue opt = Option.get opt

type Point<'T> = { x: 'T; y: 'T }

let getX<'T> (opt: Point<'T> option): 'T =
    (Option.get opt).x

[<Fact>]
let ``Option generic None works`` () =
    let opt = None
    opt = None |> equal true

[<Fact>]
let ``Option get value works`` () =
    let x = Some 5
    let y = x.Value
    let z = (Some 6).Value
    x |> equal (Some 5)
    y |> equal 5
    z |> equal 6

[<Fact>]
let ``Option matching works`` () =
    let x = Some 5
    let y =
        match x with
        | Some n -> n
        | None -> 0
    let z =
        match Some 6 with
        | Some n -> n
        | None -> 0
    y |> equal 5
    z |> equal 6

[<Fact>]
let ``Option matching with reference type works`` () =
    let p = { Point.x = 1; y = 2 }
    let a = Some p
    let b =
        a |> Option.map (fun p -> p.x)
    let b' =
        match a with
        | Some p -> Some p.x
        | None -> None
    let c =
        match b with
        | Some n -> n
        | None -> 0
    b |> equal b'
    a |> equal (Some p)
    b |> equal (Some 1)
    c |> equal 1

[<Fact>]
let ``Option.get with reference type works`` () =
    let o = Some { Point.x = 1; y = 2 }
    let p = Option.get o
    let p2 = getValue o
    p.x |> equal 1
    p2.y |> equal 2

[<Fact>]
let ``Option return value works`` () =
    let o = Some { Point.x = 1; y = 2 }
    let x = getX o
    x |> equal 1

[<Fact>]
let ``defaultArg works`` () =
    let f o = defaultArg o 5
    f (Some 2) |> equal 2
    f None |> equal 5

[<Fact>]
let ``defaultValueArg works`` () =
    let f o = defaultValueArg o 5
    f (ValueSome 2) |> equal 2
    f ValueNone |> equal 5

[<Fact>]
let ``Option.defaultValue works`` () =
    let a = Some "MyValue"
    let b: string option = None
    a |> Option.defaultValue "" |> equal "MyValue"
    b |> Option.defaultValue "default" |> equal "default"

[<Fact>]
let ``Option.defaultValue works II`` () =
    Option.defaultValue 4 (Some 5) |> equal 5
    Option.defaultValue "foo" None |> equal "foo"

[<Fact>]
let ``Option.orElse works`` () =
    Option.orElse (Some 4) (Some 5) |> equal (Some 5)
    Option.orElse (Some "foo") None |> equal (Some "foo")

[<Fact>]
let ``Option.defaultWith works`` () =
    Some 5 |> Option.defaultWith (fun () -> 4) |> equal 5
    None |> Option.defaultWith (fun () -> "foo") |> equal "foo"

[<Fact>]
let ``Option.orElseWith works`` () =
    Some 5 |> Option.orElseWith (fun () -> Some 4) |> equal (Some 5)
    None |> Option.orElseWith (fun () -> Some "foo") |> equal (Some "foo")

[<Fact>]
let ``Option.isSome,isNone works`` () =
    let o1: int option = None //TODO: handle generic option None
    let o2 = Some 5
    Option.isNone o1 |> equal true
    Option.isSome o1 |> equal false
    Option.isNone o2 |> equal false
    Option.isSome o2 |> equal true

[<Fact>]
let ``Option.IsSome,IsNone works`` () =
    let o1 = None
    let o2 = Some 5
    o1.IsNone |> equal true
    o1.IsSome |> equal false
    o2.IsNone |> equal false
    o2.IsSome |> equal true

[<Fact>]
let ``ValueOption.isSome,isNone works`` () =
    let o1 = ValueNone
    let o2 = ValueSome 5
    ValueOption.isNone o1 |> equal true
    ValueOption.isSome o1 |> equal false
    ValueOption.isNone o2 |> equal false
    ValueOption.isSome o2 |> equal true

[<Fact>]
let ``ValueOption.IsSome,IsNone works`` () =
    let o1 = ValueNone
    let o2 = Some 5
    o1.IsNone |> equal true
    o1.IsSome |> equal false
    o2.IsNone |> equal false
    o2.IsSome |> equal true

[<Fact>]
let ``Option.iter works`` () =
    let mutable res = 0
    Some 2 |> Option.iter (fun i -> res <- i)
    res |> equal 2

[<Fact>]
let ``Option.iter works II`` () = // See #198
    let mutable res = false
    let getOnlyOnce =
        let mutable value = Some "Hello"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.iter (fun s -> if s = "Hello" then res <- true)
    equal true res

[<Fact>]
let ``Option.map works`` () =
    Some 2 |> Option.map (fun i -> i + 1) |> equal (Some 3)

[<Fact>]
let ``Option.map works II`` () =
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.map ((+) "Hello ") |> equal (Some "Hello Alfonso")
    getOnlyOnce() |> Option.map ((+) "Hello ") |> equal None

[<Fact>]
let ``Option.map2 works`` () =
    (Some 2, Some 3) ||> Option.map2 (+) |> equal (Some 5)
    (None, Some 3) ||> Option.map2 (+) |> equal None
    (Some 2, None) ||> Option.map2 (+) |> equal None

[<Fact>]
let ``Option.map3 works`` () =
    (Some 2, Some 3, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal (Some 9)
    (None, Some 3, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal None
    (Some 2, None, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal None
    (Some 2, Some 3, None) |||> Option.map3 (fun x y z -> x + y + z) |> equal None

[<Fact>]
let ``Option.bind works`` () =
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.bind ((+) "Hello " >> Some) |> equal (Some "Hello Alfonso")

[<Fact>]
let ``Option.contains works`` () =
    Some "test" |> Option.contains "test" |> equal true
    Some "123" |> Option.contains "test" |> equal false
    None |> Option.contains "test" |> equal false

[<Fact>]
let ``Option.filter works`` () =
    Some 7 |> Option.filter (fun x -> x = 7) |> equal (Some 7)
    Some 7 |> Option.filter (fun x -> x = 8) |> equal None

[<Fact>]
let ``Option.filter works II`` () = // See #390
    let optionToString opt =
        match opt with
        | None -> "None"
        | Some value -> "Some " + value
    Some 7 |> Option.filter (fun _ -> false) |> Option.map string |> optionToString |> equal "None"
    Some 7 |> Option.filter (fun _ -> true)  |> Option.map string |> optionToString |> equal "Some 7"
    Some "A" |> Option.filter (fun _ -> false) |> optionToString |> equal "None"
    Some "A" |> Option.filter (fun _ -> true) |> optionToString |> equal "Some A"

[<Fact>]
let ``Option.fold works`` () =
    (5, None) ||> Option.fold (+) |> equal 5
    (5, Some 7) ||> Option.fold (+) |> equal 12

[<Fact>]
let ``Option.foldBack works`` () =
    (None, 5) ||> Option.foldBack (+) |> equal 5
    (Some 7, 5) ||> Option.foldBack (+) |> equal 12

[<Fact>]
let ``Option.fold works II`` () = // See #660
    folding1 (FoldA (Some (FoldB 1))) [] |> equal [1]

[<Fact>]
let ``Option.foldBack works II`` () =  // See #660
    folding2 (FoldA (Some (FoldB 1))) [] |> equal [1]

[<Fact>]
let ``Option.toArray works`` () =
    None |> Option.toArray<int> |> equal [||]
    Some 42 |> Option.toArray |> equal [|42|]
    Some (Some 7) |> Option.toArray |> equal [|Some 7|]

[<Fact>]
let ``Option.toList works`` () =
    None |> Option.toList<int> |> equal []
    Some 42 |> Option.toList |> equal [42]
    Some (Some 7) |> Option.toList |> equal [Some 7]

// [<Fact>]
// let ``Option.toArray works II`` () =
//     None |> Option.toArray |> equal [||]

// [<Fact>]
// let ``Option.toList works II`` () =
//     None |> Option.toList |> equal []

[<Fact>]
let ``Option.flatten works`` () =
    let o1: int option option = Some (Some 1)
    let o2: int option option = Some None
    let o3: int option option = None
    Option.flatten o1 |> equal (Some 1)
    Option.flatten o2 |> equal None
    Option.flatten o3 |> equal None

// [<Fact>]
// let ``Option.toObj works`` () =
//     let o1: string option = Some "foo"
//     let o2: string option = None
//     Option.toObj o1 |> equal "foo"
//     Option.toObj o2 |> equal null

// [<Fact>]
// let ``Option.ofObj works`` () =
//     let o1: string = "foo"
//     let o2: string = null
//     Option.ofObj o1 |> equal (Some "foo")
//     Option.ofObj o2 |> equal None

// https://github.com/fable-compiler/Fable/issues/1136
[<Fact>]
let ``Calling Some with side-effects works`` () =
    let mutable state = 0
    let f x = state <- x
    let _fo = f 3 |> Some
    state |> equal 3

[<Fact>]
let ``Different ways of providing None to a union case should be equal`` () = // See #231
    let value: int option = None
    equal true ((OptTest None) = (value |> OptTest))

[<Fact>]
let ``Different ways of providing None to a function should be equal`` () = // See #231
    let f x = x
    let f2 x = x = None
    let value: int option = None
    equal true ((f None) = (value |> f))
    equal true (f2 (None: uint option))
    equal true (f2 value)
    equal false (Some 5 |> f2)

[<Fact>]
let ``Accessing an option value gives correct expression type`` () = // See #285
    let test (x: float option) =
        match x with
        | Some y -> y + 3.
        | None -> 0.
    test (Some 4.) |> equal 7.

[<Fact>]
let ``Mixing refs and options works`` () = // See #238
    let res = ref 0
    let setter, getter =
        let slot = ref None
        (fun f -> slot.Value <- Some f),
        // TODO!!! If we change this to `slot.Value.Value` it fails
        (fun v -> slot.Value.Value v)
    setter (fun i -> res := i + 2)
    getter 5
    equal 7 !res

[<Fact>]
let ``Generic options work`` () =
    let x1 = makeSome ()
    let x2 = makeSome (None: int option)
    let x3 = makeSome "null" |> makeSome
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
let ``Nested options work`` () =
    let x1 = Some(Some 5)
    let x2 = Some(Some ())
    let x3: int option option = Some(None)
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
let ``Option.map ignore generates Some ()`` () = // See #1923
    let mySome = Some ()
    let myOtherSome = mySome |> Option.map ignore
    equal mySome myOtherSome

// [<Fact>]
// let ``Some (box null) |> Option.isSome evals to true`` () = // See #1948
//     Some (box null) |> Option.isSome |> equal true
//     Some (null) |> Option.isSome |> equal true

// [<Fact>]
// let ``System.Nullable works`` () =
//     let x = System.Nullable 5
//     x.HasValue |> equal true
//     x.Value |> equal 5
//     let y: System.Nullable<int> = System.Nullable()
//     y.HasValue |> equal false
//     throwsAnyError (fun () -> y.Value |> ignore)
