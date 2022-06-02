module Fable.Tests.Dart.Option

open Util

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

let tests() =
    testCase "defaultArg works" <| fun () ->
        let f o = defaultArg o 5
        f (Some 2) |> equal 2
        f None |> equal 5

    testCase "defaultValueArg works" <| fun () ->
        let f o = defaultValueArg o 5
        f (ValueSome 2) |> equal 2
        f ValueNone |> equal 5

    testCase "Option.defaultValue works" <| fun () ->
        let a = Some "MyValue"
        let b: string option = None

        a |> Option.defaultValue "" |> equal "MyValue"
        b |> Option.defaultValue "default" |> equal "default"

    testCase "Option.defaultValue works II" <| fun () ->
        Some 5 |> Option.defaultValue 4 |> equal 5
        None |> Option.defaultValue "foo" |> equal "foo"

    testCase "Option.orElse works" <| fun () ->
        Some 5 |> Option.orElse (Some 4) |> equal (Some 5)
        None |> Option.orElse (Some "foo") |> equal (Some "foo")

    testCase "Option.defaultWith works" <| fun () ->
        Some 5 |> Option.defaultWith (fun () -> 4) |> equal 5
        None |> Option.defaultWith (fun () -> "foo") |> equal "foo"

    testCase "Option.orElseWith works" <| fun () ->
        Some 5 |> Option.orElseWith (fun () -> Some 4) |> equal (Some 5)
        None |> Option.orElseWith (fun () -> Some "foo") |> equal (Some "foo")

    testCase "Option.isSome/isNone works" <| fun () ->
        let o1: int option = None
        let o2 = Some 5
        Option.isNone o1 |> equal true
        Option.isSome o1 |> equal false
        Option.isNone o2 |> equal false
        Option.isSome o2 |> equal true

    testCase "Option.IsSome/IsNone works" <| fun () ->
        let o1: int option = None
        let o2 = Some 5
        o1.IsNone |> equal true
        o1.IsSome |> equal false
        o2.IsNone |> equal false
        o2.IsSome |> equal true

    testCase "Option.iter works" <| fun () -> // See #198
        let mutable res = false
        let getOnlyOnce =
            let mutable value = Some "Hello"
            fun () -> match value with Some x -> value <- None; Some x | None -> None
        getOnlyOnce() |> Option.iter (fun s -> if s = "Hello" then res <- true)
        equal true res

    testCase "Option.map works" <| fun () ->
        let getOnlyOnce =
            let mutable value = Some "Alfonso"
            fun () -> match value with Some x -> value <- None; Some x | None -> None
        getOnlyOnce() |> Option.map ((+) "Hello ") |> equal (Some "Hello Alfonso")
        getOnlyOnce() |> Option.map ((+) "Hello ") |> equal None

    testCase "Option.map2 works" <| fun () ->
        (Some 2, Some 3) ||> Option.map2 (+) |> equal (Some 5)
        (None, Some 3) ||> Option.map2 (+) |> equal None
        (Some 2, None) ||> Option.map2 (+) |> equal None

    testCase "Option.map3 works" <| fun () ->
        (Some 2, Some 3, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal (Some 9)
        (None, Some 3, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal None
        (Some 2, None, Some 4) |||> Option.map3 (fun x y z -> x + y + z) |> equal None
        (Some 2, Some 3, None) |||> Option.map3 (fun x y z -> x + y + z) |> equal None

    testCase "Option.bind works" <| fun () ->
        let getOnlyOnce =
            let mutable value = Some "Alfonso"
            fun () -> match value with Some x -> value <- None; Some x | None -> None
        getOnlyOnce() |> Option.bind ((+) "Hello " >> Some) |> equal (Some "Hello Alfonso")

    testCase "Option.contains works" <| fun () ->
        Some "test" |> Option.contains "test" |> equal true
        Some "123" |> Option.contains "test" |> equal false
        None |> Option.contains "test" |> equal false

    testCase "Option.filter works" <| fun () -> // See #390
        let optionToString opt =
            match opt with
            | None -> "None"
            | Some value -> $"Some %s{value}"
        Some 7 |> Option.filter (fun _ -> false) |> Option.map string |> optionToString |> equal "None"
        Some 7 |> Option.filter (fun _ -> true)  |> Option.map string |> optionToString |> equal "Some 7"
        Some "A" |> Option.filter (fun _ -> false) |> optionToString |> equal "None"
        Some "A" |> Option.filter (fun _ -> true) |> optionToString |> equal "Some A"

    testCase "Option.fold works" <| fun () ->
        (5, None) ||> Option.fold (*) |> equal 5
        (5, Some 7) ||> Option.fold (*) |> equal 35

    testCase "Option.foldBack works" <| fun () ->
        (None, 5) ||> Option.foldBack (*) |> equal 5
        (Some 7, 5) ||> Option.foldBack (*) |> equal 35

    testCase "Option.fold works II" <| fun () -> // See #660
        folding1 (FoldA (Some (FoldB 1))) [] |> equal [1]

    testCase "Option.foldBack works II" <| fun () ->  // See #660
        folding2 (FoldA (Some (FoldB 1))) [] |> equal [1]

    testCase "Option.toArray works" <| fun () ->
        None |> Option.toArray |> equal [||]
        Some (Leaf 7) |> Option.toArray |> equal [|Leaf 7|]

    testCase "Option.toList works" <| fun () ->
        None |> Option.toList |> equal []
        Some (Leaf 7) |> Option.toList |> equal [Leaf 7]

    // https://github.com/fable-compiler/Fable/issues/1136
    testCase "Calling Some with side-effects works" <| fun () ->
        let mutable state = 0
        let f x = state <- x
        let _fo = f 3 |> Some
        state |> equal 3

    testCase "Different ways of providing None to a union case should be equal" <| fun () -> // See #231
        let value: int option = None
        equal true ((OptTest None) = (value |> OptTest))

    testCase "Different ways of providing None to a function should be equal" <| fun () -> // See #231
        let f x = x
        let f2 x = x = None
        let value: int option = None
        equal true ((f None) = (value |> f))
        equal true (f2 None)
        equal true (f2 value)
        equal false (Some 5 |> f2)

    testCase "Accessing an option value gives correct expression type" <| fun () -> // See #285
        let test (x: float option) =
            match x with
            | Some y -> y + 3.
            | None -> 0.
        test(Some 4.) |> equal 7.

    // TODO
//    testCase "Mixing refs and options works" <| fun () -> // See #238
//        let res = ref 0
//        let setter, getter =
//            let slot = ref None
//            (fun f -> slot.Value <- Some f),
//            // TODO!!! If we change this to `slot.Value.Value` it fails
//            (fun v -> slot.Value.Value v)
//        setter (fun i -> res := i + 2)
//        getter 5
//        equal 7 !res

    // TODO: What to do when unit is used as a generic arg
//    testCase "Option.map ignore generates Some ()" <| fun () -> // See #1923
//        let mySome = Some ()
//        let myOtherSome = mySome |> Option.map (ignore)
//        equal mySome myOtherSome

    testCase "Generic options work" <| fun () ->
//        let x1 = makeSome ()
//        let x2 = makeSome None
//        let x3 = makeSome null |> makeSome
        let x4 = makeSome 5
//        Option.isSome x1 |> equal true
//        Option.isNone x1 |> equal false
//        x1.IsSome |> equal true
//        x1.IsNone |> equal false
//        match x1 with Some _ -> true | None -> false
//        |> equal true
//        Option.isSome x2 |> equal true
//        Option.isNone x2 |> equal false
//        x2.IsSome |> equal true
//        x2.IsNone |> equal false
//        match x2 with
//        | Some(Some _) -> 0
//        | Some(None) -> 1
//        | None -> 2
//        |> equal 1
//        Option.isSome x3 |> equal true
//        Option.isNone x3 |> equal false
//        x3.IsSome |> equal true
//        x3.IsNone |> equal false
//        match x3 with
//        | None -> 0
//        | Some(None) -> 1
//        | Some(Some _) -> 2
//        |> equal 2
        match x4 with Some i -> i = 5 | None -> false
        |> equal true
        x4.Value = 5 |> equal true
        Option.get x4 = 5 |> equal true

//    testCase "Option.flatten works" <| fun () ->
//        let o1: int option option = Some (Some 1)
//        let o2: int option option = Some None
//        let o3: int option option = None
//        Option.flatten o1 |> equal (Some 1)
//        Option.flatten o2 |> equal None
//        Option.flatten o3 |> equal None

//    testCase "Option.toObj works" <| fun () ->
//        let o1: string option = Some "foo"
//        let o2: string option = None
//        Option.toObj o1 |> equal "foo"
//        Option.toObj o2 |> equal null
//
//    testCase "Option.ofObj works" <| fun () ->
//        let o1: string = "foo"
//        let o2: string = null
//        Option.ofObj o1 |> equal (Some "foo")
//        Option.ofObj o2 |> equal None

    testCase "Nested options work" <| fun () ->
        let test x1 x3 =
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
            // Option.isSome x2 |> equal true
            // Option.isNone x2 |> equal false
            // x2.IsSome |> equal true
            // x2.IsNone |> equal false
            // match x2 with
            // | Some(None) -> 0
            // | Some(Some _) -> 1
            // | None -> 2
            // |> equal 1
            Option.isSome x3 |> equal true
            Option.isNone x3 |> equal false
            x3.IsSome |> equal true
            x3.IsNone |> equal false
            match x3 with
            | None -> 0
            | Some(Some _) -> 1
            | Some(None) -> 2
            |> equal 2

            // FIXME: Passing nested options to Option module functions
            // x1 |> Option.map (Option.map ((+) 3)) |> equal (Some(Some 8))
            // x3 |> Option.map (Option.map ((+) 3)) |> equal None

        let x1 = Some(Some 5)
        // FIXME: Nested unit options
        // let x2 = Some(Some ())
        let x3: int option option = Some(None)
        test x1 x3

//    testCase "Some (box null) |> Option.isSome evals to true" <| fun () -> // See #1948
//        Some (box null) |> Option.isSome |> equal true
//        Some (null) |> Option.isSome |> equal true

//    testCase "Nullable works" <| fun () ->
//        let x = Nullable 5
//        x.HasValue |> equal true
//        x.Value |> equal 5
//
//        let y: Nullable<int> = Nullable()
//        y.HasValue |> equal false
//        let errorThrown = try y.Value |> ignore; false with _ -> true
//        equal true errorThrown
