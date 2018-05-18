module QuickTest

// Use this template to make quick tests when adding new features to Fable.
// You must run a full build at least once (from repo root directory,
// type `sh build.sh` on OSX/Linux or just `build` on Windows). Then:
// - When making changes to Fable.Compiler run `build QuickFableCompilerTest`
// - When making changes to fable-core run `build QuickFableCoreTest`

// Please don't add this file to your commits

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing
open Fable.Import

let log (o: obj) =
    printfn "%O" o

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > %b" expected actual areEqual
    if not areEqual then
        failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual

let testCase (msg: string) f: unit =
    try
        printfn "%s" msg
        f ()
    with ex ->
        printfn "%s" ex.Message
        if ex.Message.StartsWith("[ASSERT ERROR]") |> not then
            printfn "%s" ex.StackTrace
    printfn ""

let testCaseAsync msg f =
    testCase msg (fun () -> f () |> Async.StartImmediate)

// Write here your unit test, you can later move it
// to Fable.Tests project. For example:
// testCase "Addition works" <| fun () ->
//     2 + 2 |> equal 4

let testListChoose xss =
    let f xss = xss |> List.choose (function Some a -> Some a | _ -> None)
    xss |> f |> List.collect (fun xs -> [ for s in xs do yield s ])

let rec sumFirstList (zs: float list) (n: int): float =
   match n with
   | 0 -> 0.
   | 1 -> zs.Head
   | _ -> zs.Head + sumFirstList zs.Tail (n-1)

testCase "List.truncate works" <| fun () ->
    [1..3] = (List.truncate 3 [1..5]) |> equal true
    [1..5] = (List.truncate 10 [1..5]) |> equal true
    [] = (List.truncate 0 [1..5]) |> equal true
    ["str1";"str2"] = (List.truncate 2 ["str1";"str2";"str3"]) |> equal true
    [] = (List.truncate 0 []) |> equal true
    [] = (List.truncate 1 []) |> equal true

testCase "List.choose works with generic arguments" <| fun () ->
  let res = testListChoose [ Some [ "a" ] ]
  equal ["a"] res

testCase "List.collect works" <| fun () ->
    let xs = [[1]; [2]; [3]; [4]]
    let ys = xs |> List.collect id
    ys.Head + ys.Tail.Head
    |> equal 3

    let list1 = [10.; 20.; 30.]
    let collectList = List.collect (fun x -> [for i in 1.0..3.0 -> x * i]) list1
    sumFirstList collectList 9 |> equal 360.

    let xs = [[1.; 2.]; [3.]; [4.; 5.; 6.;]; [7.]]
    let ys = xs |> List.collect id
    sumFirstList ys 5
    |> equal 15.

testCase "List.concat works" <| fun () ->
    let xs = [[1]; [2]; [3]; [4]]
    let ys = xs |> List.concat
    ys.Head  + ys.Tail.Head
    |> equal 3

    let xs1 = [[1.; 2.; 3.]; [4.; 5.; 6.]; [7.; 8.; 9.]]
    let ys1 = xs1 |> List.concat
    sumFirstList ys1 7
    |> equal 28.

testCase "List.contains works" <| fun () ->
    let xs = [1; 2; 3; 4]
    xs |> List.contains 2 |> equal true
    xs |> List.contains 0 |> equal false

testCase "List.contains lambda doesn't clash" <| fun () ->
    let modifyList current x =
        let contains = current |> List.contains x
        match contains with
            | true -> current |> (List.filter (fun a -> a <> x))
            | false -> x::current
    let l = [1;2;3;4]
    (modifyList l 1) |> List.contains 1 |> equal false
    (modifyList l 5) |> List.contains 5 |> equal true

testCase "List.average works" <| fun () ->
    List.average [1.; 2.; 3.; 4.]
    |> equal 2.5

testCase "List.averageBy works" <| fun () ->
    [1.; 2.; 3.; 4.]
    |> List.averageBy (fun x -> x * 2.)
    |> equal 5.

testCase "List.distinct works" <| fun () ->
  let xs = [1; 1; 1; 2; 2; 3; 3]
  let ys = xs |> List.distinct
  ys |> List.length
  |> equal 3

testCase "List.distinctBy works" <| fun () ->
  [1; 1; 1; 2; 2; 3; 3]
  |> List.distinctBy (fun x -> x % 2)
  |> List.length
  |> equal 2

testCase "List.distinct works with non-primitive types" <| fun () ->
  List.distinct [(1, 2); (1, 3); (1, 2)] |> List.length |> equal 2

testCase "List.findBack works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  xs |> List.find ((>) 4.) |> equal 1.
  xs |> List.findBack ((>) 4.) |> equal 3.

testCase "List.findIndexBack works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  xs |> List.findIndex ((>) 4.) |> equal 0
  xs |> List.findIndexBack ((>) 4.) |> equal 2

testCase "List.tryFindBack works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  xs |> List.tryFind ((>) 4.) |> equal (Some 1.)
  xs |> List.tryFindBack ((>) 4.) |> equal (Some 3.)
  xs |> List.tryFindBack ((=) 5.) |> equal None

testCase "List.tryFindIndexBack works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  xs |> List.tryFindIndex ((>) 4.) |> equal (Some 0)
  xs |> List.tryFindIndexBack ((>) 4.) |> equal (Some 2)
  xs |> List.tryFindIndexBack ((=) 5.) |> equal None

testCase "List.foldBack2 works" <| fun () ->
    ([1; 2; 3; 4], [1; 2; 3; 4], 0)
    |||> List.foldBack2 (fun x y acc -> acc - y * x)
    |> equal -30

testCase "List.indexed works" <| fun () ->
  let xs = ["a"; "b"; "c"] |> List.indexed
  xs.Length |> equal 3
  fst xs.[2] |> equal 2
  snd xs.[2] |> equal "c"

testCase "List.map3 works" <| fun () ->
    let xs = [1;2;3]
    let ys = [5;4;3]
    let zs = [7;8;9]
    let ks = List.map3 (fun x y z -> z - y - x) xs ys zs
    List.sum ks
    |> equal 6

testCase "List.mapi2 works" <| fun () ->
    let xs = [7;8;9]
    let ys = [5;4;3]
    let zs = List.mapi2 (fun i x y -> i * (x - y)) xs ys
    List.sum zs |> equal 16

testCase "List.mapFold works" <| fun () ->
  let xs = [1y; 2y; 3y; 4y]
  let result = xs |> List.mapFold (fun acc x -> (x * 2y, acc + x)) 0y
  fst result |> List.sum |> equal 20y
  snd result |> equal 10y

testCase "List.mapFoldBack works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  let result = List.mapFoldBack (fun x acc -> (x * -2., acc - x)) xs 0.
  fst result |> List.sum |> equal -20.
  snd result |> equal -10.

// TODO: Runtime uncurry to arity 2
testCase "List.mapFold works II" <| fun () -> // See #842
  let f x y = x,y
  let xs,_ = List.mapFold f "a" ["b"]
  equal "a" xs.Head

testCase "List.mapFoldBack works II" <| fun () ->
  let f x y = x,y
  let xs,_ = List.mapFoldBack f ["a"] "b"
  equal "a" xs.Head

testCase "List.partition works" <| fun () ->
    let xs = [1; 2; 3; 4; 5; 6]
    let ys, zs = xs |> List.partition (fun x -> x % 2 = 0)
    List.sum zs |> equal 9
    equal 2 ys.[0]
    equal 5 zs.[2]

testCase "List.permute works" <| fun () ->
    let xs = [1; 2; 3; 4; 5; 6]
    let ys = xs |> List.permute (fun i -> i + 1 - 2 * (i % 2))
    equal 4 ys.[2]
    equal 6 ys.[4]

testCase "List.range works" <| fun () ->
  [1..5]
  |> List.reduce (+)
  |> equal 15
  [0..2..9]
  |> List.reduce (+)
  |> equal 20

testCase "List.zip3 works" <| fun () ->
    let xs = [1; 2; 3]
    let ys = [4; 5; 6]
    let zs = [7; 8; 9]
    let ks = List.zip3 xs ys zs
    let x, y, z = List.last ks
    equal 3 x
    equal 6 y
    equal 9 z

testCase "List.tryItem works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  List.tryItem 3 xs |> equal (Some 4.)
  List.tryItem 4 xs |> equal None
  List.tryItem -1 xs |> equal None

testCase "List.tryHead works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  List.tryHead xs |> equal (Some 1.)
  List.tryHead [] |> equal None

testCase "List.last works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  xs |> List.last
  |> equal 4.

testCase "List.tryLast works" <| fun () ->
  let xs = [1.; 2.; 3.; 4.]
  List.tryLast xs |> equal (Some 4.)
  List.tryLast [] |> equal None

testCase "List.countBy works" <| fun () ->
  let xs = [1; 2; 3; 4]
  xs |> List.countBy (fun x -> x % 2)
  |> List.length |> equal 2

testCase "List.groupBy returns valid list" <| fun () ->
  let xs = [1; 2]
  let worked =
    match List.groupBy (fun _ -> true) xs with
    | [true, [1; 2]] -> true
    | _ -> false
  worked |> equal true

testCase "List.unfold works" <| fun () ->
  let xs = 0. |> List.unfold (fun n -> if n < 3.0 then Some(n+1., n+1.) else None)
  let sum =
    match xs with
    | n1::n2::n3::[] -> n1 + n2 + n3
    | _ -> 0.0
  sum |> equal 6.0

testCase "List.splitAt works" <| fun () ->
  let li = [1;2;3;4]
  List.splitAt 0 li |> equal ([], [1;2;3;4])
  List.splitAt 3 li |> equal ([1;2;3], [4])
  List.splitAt 4 li |> equal ([1;2;3;4], [])

testCase "List.Item throws exception when index is out of range" <| fun () ->
  let xs = [0]
  (try (xs.Item 1) |> ignore; false with | _ -> true) |> equal true

testCase "List.except works" <| fun () ->
  List.except [2] [1; 3; 2] |> List.last |> equal 3
  List.except [2] [2; 4; 6] |> List.head |> equal 4
  List.except [1] [1; 1; 1; 1] |> List.isEmpty |> equal true
  List.except ['t'; 'e'; 's'; 't'] ['t'; 'e'; 's'; 't'] |> List.isEmpty |> equal true
  List.except ['t'; 'e'; 's'; 't'] ['t'; 't'] |> List.isEmpty |> equal true
  List.except [(1, 2)] [(1, 2)] |> List.isEmpty |> equal true
  List.except [Map.empty |> (fun m -> m.Add(1, 2))] [Map.ofList [(1, 2)]] |> List.isEmpty |> equal true
//   List.except [{ Bar= "test" }] [{ Bar = "test" }] |> List.isEmpty |> equal true