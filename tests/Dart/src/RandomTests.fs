module Fable.Tests.Dart.RandomTests

open Util
open System

let tests () =

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
        let rnd1 = Random(1234)
        let rnd2 = Random(1234)
        rnd1.Next() |> equal (rnd2.Next())
        rnd1.Next(100) |> equal (rnd2.Next(100))
        rnd1.Next(1000, 10000) |> equal (rnd2.Next(1000, 10000))
        rnd1.NextDouble() |> equal (rnd2.NextDouble())

    testCase "System.Random seeded validates arguments" <| fun () ->
        let rnd = Random(42)
        throwsAnyError <| fun () -> rnd.Next(-10)
        throwsAnyError <| fun () -> rnd.Next(14, 10)
        throwsAnyError <| fun () -> rnd.NextBytes(null)

    testCase "System.Random.NextBytes works" <| fun () ->
        let buffer = Array.create 16 0uy
        Random().NextBytes(buffer)
        buffer.Length |> equal 16
        buffer = Array.create 16 0uy |> equal false

    testCase "System.Random.NextBytes seeded works" <| fun () ->
        let buffer1 = Array.create 4 0uy
        let buffer2 = Array.create 4 0uy
        Random(5432).NextBytes(buffer1)
        Random(5432).NextBytes(buffer2)
        buffer1 |> equal buffer2
