module Fable.Tests.RandomTests

open System
open Util.Testing
open Fable.Tests.Util

let tests =
  testList "Random" [
    testCase "System.Random works" <| fun () ->
        let rnd = Random()
        let x = rnd.Next()
        (x >= 0 && x < 0x7fffffff) |> equal true
        rnd.Next(0) |> equal 0
        let x = rnd.Next(5)
        (x >= 0 && x < 5) |> equal true
        let x = rnd.Next(14, 20)
        (x >= 14 && x < 20) |> equal true
        let x = rnd.Next(-14, -10)
        (x >= -14 && x < -10) |> equal true
        let x = rnd.NextDouble()
        (x >= 0.0 && x < 1.0) |> equal true

    testCase "System.Random seeded works" <| fun () ->
        let rnd = Random(1234)
        rnd.Next() |> equal 857019877
        rnd.Next(100) |> equal 89
        rnd.Next(1000, 10000) |> equal 3872
        rnd.NextDouble() |> equal 0.9467375338760845

    testCase "System.Random seeded validates arguments" <| fun () ->
        let rnd = Random(42)
        throwsAnyError <| fun () -> rnd.Next(-10)
        throwsAnyError <| fun () -> rnd.Next(14, 10)
        throwsAnyError <| fun () -> rnd.NextBytes(null)

    // Note: Test could fail sometime during life of universe, if it picks all zeroes.
    testCase "System.Random.NextBytes works" <| fun () ->
        let buffer = Array.create 16 0uy // guid-sized buffer
        Random().NextBytes(buffer)
        buffer.Length |> equal 16
        buffer = Array.create 16 0uy |> equal false

    testCase "System.Random.NextBytes seeded works" <| fun () ->
        let buffer = Array.create 4 0uy
        Random(5432).NextBytes(buffer)
        buffer |> equal [|152uy; 238uy; 227uy; 30uy|]

  ]