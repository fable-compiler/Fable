module Fable.Tests.Stack

open Util.Testing
open System.Collections.Generic

let tests =
    testList "System.Collections.Generic.Stack" [
        testCase "Stack works for simple cases" <| fun () ->
            let stack = Stack<string>()
            stack.Count |> equal 0
            stack.Push("a")
            stack.Contains("a") |> equal true
            stack.Count |> equal 1
            stack.Peek() |> equal "a"
            stack.Pop() |> equal "a"
            stack.Count |> equal 0
            stack.Push("a")
            stack.Push("b")
            stack.Push("c")
            stack.Contains("a") |> equal true
            stack.Contains("b") |> equal true
            stack.Contains("c") |> equal true
            stack.Contains("d") |> equal false
            stack.Peek() |> equal "c"
            stack.Pop() |> equal "c"
            stack.Peek() |> equal "b"
            stack.Count |> equal 2
            stack.Clear()
            stack.Count |> equal 0
            stack.Contains("a") |> equal false

        testCase "Stack Contains works as expected" <| fun () ->
            let stack = Stack<string>()
            stack.Contains("a") |> equal false
            stack.Push("a")
            stack.Push("a")
            stack.Contains("a") |> equal true
            stack.Contains("x") |> equal false
            stack.Pop() |> ignore
            stack.Contains("a") |> equal true
            stack.Pop() |> ignore
            stack.Contains("a") |> equal false
            stack.Contains(null) |> equal false
            stack.Push(null)
            stack.Contains(null) |> equal true

        testCase "Stack ToArray works as expected" <| fun () ->
            let stack = Stack<string>()

            stack.ToArray() |> equal [||]

            stack.Push("a")

            stack.ToArray() |> equal [| "a" |]

            stack.Push("b")
            stack.Push("c")

            stack.ToArray() |> equal [| "c"; "b"; "a" |]

            stack.Pop() |> ignore

            stack.ToArray() |> equal [| "b"; "a" |]

        testCase "Stack TryPeek works as expected" <| fun () ->
            let stack = Stack<string>()

            (
                match stack.TryPeek() with
                | (true, x) -> Some x
                | (false, _) -> None
            )
            |> equal None

            stack.Push("a")

            (
                match stack.TryPeek() with
                | (true, x) -> Some x
                | (false, _) -> None
            )
            |> equal (Some "a")

            stack.TryPeek() |> equal (true, "a")

        testCase "Stack TryPop works as expected" <| fun () ->
            let stack = Stack<string>()

            (
                match stack.TryPop() with
                | (true, x) -> Some x
                | (false, _) -> None
            )
            |> equal None

            stack.Push("a")
            stack.Push("b")

            (
                match stack.TryPop() with
                | (true, x) -> Some x
                | (false, _) -> None
            )
            |> equal (Some "b")

            (
                match stack.TryPop() with
                | (true, x) -> Some x
                | (false, _) -> None
            )
            |> equal (Some "a")

            (
                match stack.TryPop() with
                | (true, x) -> Some x
                | (false, _) -> None
            )
            |> equal None

        testCase "Stack can be created with an initial size" <| fun () ->
            let stack = Stack<string>(4)
            stack.Count |> equal 0
            stack.Push("a")
            stack.Count |> equal 1
            stack.Pop() |> equal "a"

        testCase "Stack can be created from an IEnumerable<_>" <| fun () ->
            let xs = [ "a"; "b"; "c" ] :> IEnumerable<_>

            let stack = Stack<string>(xs)

            stack.ToArray() |> equal [| "c"; "b"; "a" |]

            stack.Pop() |> equal "c"
            stack.Pop() |> equal "b"
            stack.Pop() |> equal "a"

            stack.Count |> equal 0

        testCase "Stack implements IEnumerable<_>" <| fun () ->
            let stack = Stack<string>()

            List.ofSeq stack |> equal []

            stack.Push("a")
            stack.Push("b")
            stack.Push("c")
            stack.Push("d")
            stack.Push("e")

            List.ofSeq stack |> equal [ "e"; "d"; "c"; "b"; "a" ]

            stack.Pop() |> ignore
            stack.Pop() |> ignore

            List.ofSeq stack |> equal [ "c"; "b"; "a" ]
    ]
