module Fable.Tests.OptionTest

open Fable.Tests.Util
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

[<Fact>]
let ``test Option.IsSome works`` () =
    let x: int option = Some 42
    x.IsSome |> equal true

[<Fact>]
let ``test Option.IsNone works`` () =
    let x: int option = None
    x.IsNone |> equal true

[<Fact>]
let ``test Option.IsSome with None works`` () =
    let x: int option = None
    x.IsSome |> equal false

[<Fact>]
let ``test Option.IsNone with Some works`` () =
    let x: int option = Some 42
    x.IsNone |> equal false

[<Fact>]
let ``test Option.Value works`` () =
    let x: int option = Some 42
    x.Value |> equal 42

[<Fact>]
let ``test Option.defaultValue works`` () =
    let x: int option = None
    Option.defaultValue 99 x |> equal 99

[<Fact>]
let ``test Option.defaultValue with Some works`` () =
    let x: int option = Some 42
    Option.defaultValue 99 x |> equal 42

[<Fact>]
let ``test Option.map works`` () =
    let x: int option = Some 21
    Option.map (fun v -> v * 2) x |> equal (Some 42)

[<Fact>]
let ``test Option.map with None works`` () =
    let x: int option = None
    Option.map (fun v -> v * 2) x |> equal None

[<Fact>]
let ``test Option.bind works`` () =
    let x: int option = Some 21
    Option.bind (fun v -> Some(v * 2)) x |> equal (Some 42)

[<Fact>]
let ``test Option.bind with None works`` () =
    let x: int option = None
    Option.bind (fun v -> Some(v * 2)) x |> equal None

[<Fact>]
let ``test Option module isSome works`` () =
    Option.isSome (Some 42) |> equal true
    Option.isSome None |> equal false

[<Fact>]
let ``test Option module isNone works`` () =
    Option.isNone None |> equal true
    Option.isNone (Some 42) |> equal false

[<Fact>]
let ``test defaultArg works`` () =
    let f o = defaultArg o 5
    f (Some 2) |> equal 2
    f None |> equal 5

[<Fact>]
let ``test Option.defaultValue works II`` () =
    Some 5 |> Option.defaultValue 4 |> equal 5
    None |> Option.defaultValue 4 |> equal 4

[<Fact>]
let ``test Option.orElse works`` () =
    Some 5 |> Option.orElse (Some 4) |> equal (Some 5)
    None |> Option.orElse (Some 4) |> equal (Some 4)

[<Fact>]
let ``test Option.defaultWith works`` () =
    Some 5 |> Option.defaultWith (fun () -> 4) |> equal 5
    None |> Option.defaultWith (fun () -> 4) |> equal 4

[<Fact>]
let ``test Option.orElseWith works`` () =
    Some 5 |> Option.orElseWith (fun () -> Some 4) |> equal (Some 5)
    None |> Option.orElseWith (fun () -> Some 4) |> equal (Some 4)

[<Fact>]
let ``test Option.iter works`` () =
    let mutable res = false
    Some "Hello" |> Option.iter (fun s -> if s = "Hello" then res <- true)
    equal true res

[<Fact>]
let ``test Option.iter with None works`` () =
    let mutable res = false
    None |> Option.iter (fun (_s: string) -> res <- true)
    equal false res

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
let ``test Option.contains works`` () =
    Some "test" |> Option.contains "test" |> equal true
    Some "123" |> Option.contains "test" |> equal false
    None |> Option.contains "test" |> equal false

[<Fact>]
let ``test Option.filter works`` () =
    Some 7 |> Option.filter (fun _ -> false) |> equal None
    Some 7 |> Option.filter (fun _ -> true) |> equal (Some 7)
    None |> Option.filter (fun _ -> true) |> equal None

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
let ``test Option.foldBack works II`` () =
    folding2 (FoldA (Some (FoldB 1))) [] |> equal [1]

[<Fact>]
let ``test Option.toArray works`` () =
    None |> Option.toArray |> equal [||]
    Some 7 |> Option.toArray |> equal [|7|]

[<Fact>]
let ``test Option.toList works`` () =
    None |> Option.toList |> equal []
    Some 7 |> Option.toList |> equal [7]

[<Fact>]
let ``test Option.flatten works`` () =
    let o1: int option option = Some (Some 1)
    Option.flatten o1 |> equal (Some 1)

[<Fact>]
let ``test Option.count works`` () =
    Option.count (Some 7) |> equal 1
    Option.count None |> equal 0

[<Fact>]
let ``test Option.forAll works`` () =
    Option.forall (fun x -> x > 5) (Some 7) |> equal true
    Option.forall (fun x -> x > 5) (Some 3) |> equal false
    Option.forall (fun x -> x > 5) None |> equal true

[<Fact>]
let ``test Option.exists works`` () =
    Option.exists (fun x -> x > 5) (Some 7) |> equal true
    Option.exists (fun x -> x > 5) (Some 3) |> equal false
    Option.exists (fun x -> x > 5) None |> equal false

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
let ``test Option.map ignore generates Some unit`` () =
    let mySome = Some ()
    let myOtherSome = mySome |> Option.map (ignore)
    equal mySome myOtherSome

// Nested option tests (require option wrapping support)
[<Fact>]
let ``test Nested option Some Some works`` () =
    let x: int option option = Some(Some 42)
    x.IsSome |> equal true

[<Fact>]
let ``test Nested option Some None works`` () =
    let x: int option option = Some(None)
    x.IsSome |> equal true

[<Fact>]
let ``test Nested option None works`` () =
    let x: int option option = None
    x.IsNone |> equal true

[<Fact>]
let ``test Nested option value extraction works`` () =
    let x: int option option = Some(Some 42)
    match x with
    | Some inner ->
        inner.IsSome |> equal true
        inner.Value |> equal 42
    | None -> failwith "should be Some"

[<Fact>]
let ``test Nested option Some None value is None`` () =
    let x: int option option = Some(None)
    match x with
    | Some inner ->
        inner.IsNone |> equal true
    | None -> failwith "should be Some"

[<Fact>]
let ``test Option flatten with nested Some works`` () =
    Option.flatten (Some(Some 1)) |> equal (Some 1)

[<Fact>]
let ``test Option flatten with nested None works`` () =
    Option.flatten (Some(None)) |> equal None

[<Fact>]
let ``test Option flatten with None works`` () =
    let x: int option option = None
    Option.flatten x |> equal None

[<Fact>]
let ``test Option with unit Some works`` () =
    let x: unit option = Some()
    x.IsSome |> equal true

[<Fact>]
let ``test Option with unit None works`` () =
    let x: unit option = None
    x.IsNone |> equal true

[<Fact>]
let ``test Generic option function works`` () =
    let makeSomeGeneric (x: 'a) : 'a option = Some x
    makeSomeGeneric 42 |> Option.isSome |> equal true
    makeSomeGeneric "hello" |> Option.isSome |> equal true
