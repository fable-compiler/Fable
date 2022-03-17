module Fable.Tests.Dart.Comparison

open System
open Util

type UTest = A of int | B of int
type RTest = { a: int; b: int }

let tests() =
    testCase "Union equality works" <| fun () ->
        // Prevent objects being compiled as const
        // so the ReferenceEquals assertion below works
        let mutable x = 1
        x <- x + 1
        let u1 = A x
        let u2 = A x
        let u3 = A 4
        let u4 = B 2
        equal true (u1 = u2)
        equal false (u1 = u3)
        equal true (u1 <> u3)
        equal false (u1 <> u2)
        equal false (u1 = u4)
        Object.ReferenceEquals(u1, u1) |> equal true
        Object.ReferenceEquals(u1, u2) |> equal false

    testCase "Record equality works" <| fun () ->
        // Prevent objects being compiled as const
        // so the ReferenceEquals assertion below works
        let mutable x = 1
        x <- x + 1
        let r1 = { a = 1; b = x }
        let r2 = { a = 1; b = x }
        let r3 = { a = 1; b = 4 }
        equal true (r1 = r2)
        equal false (r1 = r3)
        equal true (r1 <> r3)
        equal false (r1 <> r2)
        Object.ReferenceEquals(r1, r1) |> equal true
        Object.ReferenceEquals(r1, r2) |> equal false

    testCase "Union comparison works" <| fun () ->
        let u1 = A 2
        let u2 = A 2
        let u3 = A 4
        let u4 = A 1
        let u5 = B 2
        equal 0 (compare u1 u2)
        equal -1 (compare u1 u3)
        equal true (u1 < u3)
        equal 1 (compare u1 u4)
        equal false (u1 < u4)
        (compare u1 u5) = 0 |> equal false

    testCase "Record comparison works" <| fun () ->
        let r1 = { a = 1; b = 2 }
        let r2 = { a = 1; b = 2 }
        let r3 = { a = 1; b = 4 }
        equal 0 (compare r1 r2)
        (compare r1 r3) = 0 |> equal false
