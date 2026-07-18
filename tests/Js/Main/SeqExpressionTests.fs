module Fable.Tests.SeqExpressions

open System
open Util.Testing

type 'a Tree =
    | Leaf
    | Node of 'a Tree * 'a * 'a Tree

type DisposableAction(f) =
    interface IDisposable with
        member _.Dispose() = f()

let tests =
  testList "SeqExpressions" [
    testCase "empty seq expressions work" <| fun () ->
        seq { do () }
        |> Seq.length |> float |> equal 0.

    testCase "yield in seq expressions works" <| fun () ->
        seq { yield 1 }
        |> Seq.length |> equal 1

    testCase "yield from in seq expressions works" <| fun () ->
        let ys = seq { yield 1 }
        seq { yield! ys }
        |> Seq.length |> equal 1

    testCase "infinite seq expressions work" <| fun () ->
        let rec green () = seq {
            yield "green"
            yield! yellow() }
        and yellow () = seq {
            yield "yellow"
            yield! red() }
        and red () = seq {
            yield "red"
            yield! green() }
        green() |> Seq.item 12 |> equal "green"

    testCase "combine in seq expressions works" <| fun () ->
        seq { yield 1; yield 2 }
        |> Seq.length |> equal 2

    testCase "multiple yields in seq expressions work" <| fun () ->
        let upTo n = seq {
            for i = 1 to n do yield n
        }
        let numbers () = seq {
            yield 8
            yield! upTo 5
            yield 4
            yield! upTo 3
            yield 2
        }
        numbers() |> Seq.sum |> equal 48

    testCase "for in seq expressions works" <| fun () ->
        seq { for x in 1 .. 10 do yield x }
        |> Seq.length |> equal 10

    testCase "while in seq expressions works" <| fun () ->
        let mutable n = 0
        seq {
            while n < 10 do
                n <- n + 1
                yield n
        } |> Seq.sum |> equal 55

    testCase "recursive seq expressions work" <| fun () ->
        let rec traverse t = seq {
            match t with
            | Leaf -> ()
            | Node(xs, y, zs) ->
                yield y
                yield! traverse xs
                yield! traverse zs |> Seq.map (fun x -> 2 * x)
        }
        let t = Node(Node(Leaf, 1, Leaf), 2, Node(Leaf, 3, Leaf))
        traverse t |> Seq.sum |> equal 9

    testCase "try...finally in seq expressions works" <| fun () ->
        let mutable n = 0
        try seq {
            try
                raise (exn "My message")
            finally
                n <- n + 1
            } |> Seq.iter ignore
        with _ -> ()
        equal 1 n

    testCase "try...with in list expressions works" <| fun () ->
        [ try 1 with _ -> 0 ]
        |> equal [1]

    testCase "try...with in list expressions catches exceptions" <| fun () ->
        [ try raise (exn "boom") with _ -> 42 ]
        |> equal [42]

    testCase "try...with in array expressions works" <| fun () ->
        [| try raise (exn "boom") with _ -> 7 |]
        |> equal [| 7 |]

    testCase "try...with in seq expressions preserves yielded elements before the exception" <| fun () ->
        let mutable caught = false
        let xs =
            seq {
                try
                    yield 1
                    raise (exn "boom")
                    yield 2
                with _ ->
                    caught <- true
                    yield 3
            } |> Seq.toList
        equal [1; 3] xs
        equal true caught

    testCase "try...with in seq expressions rethrows unmatched exceptions" <| fun () ->
        let mutable propagated = false
        try
            seq {
                try
                    yield 1
                    raise (System.InvalidOperationException "boom")
                    yield 2
                with :? System.ArgumentException ->
                    yield 0
            }
            |> Seq.toList
            |> ignore
        with :? System.InvalidOperationException ->
            propagated <- true
        equal true propagated

    testCase "use in seq expressions works" <| fun () ->
        let mutable n = 0
        seq {
            use x = new DisposableAction(fun () ->
                n <- n + 1)
            ignore x
        } |> Seq.iter ignore
        equal 1 n

    testCase "array expressions work" <| fun () ->
        [| for x in 1 .. 10 do yield x |]
        |> Array.length |> equal 10

    testCase "list expressions work" <| fun () ->
        [ for x in 1 .. 10 do yield x ]
        |> List.length |> equal 10
  ]