module Fable.Tests.RandomTests

open System
open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test System.Random works`` () =
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

// // Broken test, every new Random seeded instance resets a global seed
// [<Fact>]
// let ``test System.Random seeded works`` () =
//     let rnd1 = Random(1234)
//     let rnd2 = Random(1234)
//     rnd1.Next() |> equal (rnd2.Next())
//     rnd1.Next(100) |> equal (rnd2.Next(100))
//     rnd1.Next(1000, 10000) |> equal (rnd2.Next(1000, 10000))
//     rnd1.NextDouble() |> equal (rnd2.NextDouble())

// Testing the seeded random sequences separately works, but is not ideal
[<Fact>]
let ``test System.Random seeded works`` () =
    let rnd1 = Random(1234)
    let a1 = rnd1.Next()
    let a2 = rnd1.Next(100)
    let a3 = rnd1.Next(1000, 10000)
    let a4 = rnd1.NextDouble()
    let rnd2 = Random(1234)
    let b1 = rnd2.Next()
    let b2 = rnd2.Next(100)
    let b3 = rnd2.Next(1000, 10000)
    let b4 = rnd2.NextDouble()
    a1 |> equal b1
    a2 |> equal b2
    a3 |> equal b3
    a4 |> equal b4

[<Fact>]
let ``test System.Random seeded validates arguments`` () =
    let rnd = Random(42)
    throwsAnyError <| fun () -> rnd.Next(-10)
    throwsAnyError <| fun () -> rnd.Next(14, 10)
    // throwsAnyError <| fun () -> rnd.NextBytes(null)

[<Fact>]
let ``test System.Random.NextBytes works`` () =
    let buffer = Array.create 16 0uy
    Random().NextBytes(buffer)
    buffer.Length |> equal 16
    buffer = Array.create 16 0uy |> equal false

// // Broken test, every new Random seeded instance resets a global seed
// [<Fact>]
// let ``test System.Random.NextBytes seeded works`` () =
//     let buffer1 = Array.create 4 0uy
//     let buffer2 = Array.create 4 0uy
//     Random(5432).NextBytes(buffer1)
//     Random(5432).NextBytes(buffer2)
//     buffer1 |> equal buffer2

// Testing the seeded random sequences separately works, but is not ideal
[<Fact>]
let ``test System.Random.NextBytes seeded works`` () =
    let buffer1 = Array.create 4 0uy
    let buffer2 = Array.create 4 0uy
    let rnd1 = Random(5432)
    rnd1.NextBytes(buffer1)
    let rnd2 = Random(5432)
    rnd2.NextBytes(buffer2)
    buffer1 |> equal buffer2
