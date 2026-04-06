module Fable.Tests.RandomTests

open System
open Util.Testing
open Fable.Tests.Util

let chiSquaredStatistic bucketCount sampleCount (nextBucket: unit -> int) =
    let counts = Array.zeroCreate bucketCount

    for _ in 1 .. sampleCount do
        let bucket = nextBucket()
        counts[bucket] <- counts[bucket] + 1

    let expected = float sampleCount / float bucketCount

    counts
    |> Array.sumBy (fun count ->
        let diff = float count - expected
        diff * diff / expected)

let chiSquaredUpperBound degreesOfFreedom sigma =
    float degreesOfFreedom + sigma * Math.Sqrt(2.0 * float degreesOfFreedom)

let runsZScore sampleCount (nextBit: unit -> bool) =
    let mutable ones = 0
    let mutable zeros = 0
    let mutable runs = 0
    let mutable hasPrevious = false
    let mutable previous = false

    for _ in 1 .. sampleCount do
        let current = nextBit()

        if current then
            ones <- ones + 1
        else
            zeros <- zeros + 1

        if not hasPrevious || current <> previous then
            runs <- runs + 1
            previous <- current
            hasPrevious <- true

    if ones = 0 || zeros = 0 then
        Double.PositiveInfinity
    else
        let ones' = float ones
        let zeros' = float zeros
        let total = float sampleCount
        let expectedRuns = 1.0 + 2.0 * ones' * zeros' / total

        let variance =
            (2.0 * ones' * zeros' * (2.0 * ones' * zeros' - ones' - zeros'))
            / (total * total * (total - 1.0))

        Math.Abs(float runs - expectedRuns) / Math.Sqrt(variance)

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

    testCase "System.Random seeded passes chi-squared test" <| fun () ->
        let rnd = Random(2026)
        let statistic = chiSquaredStatistic 16 10000 (fun () -> rnd.Next(16))
        statistic < chiSquaredUpperBound 15 4.0 |> equal true

    testCase "System.Random seeded passes Wald-Wolfowitz runs test" <| fun () ->
        let rnd = Random(2027)
        let zScore = runsZScore 10000 (fun () -> rnd.NextDouble() >= 0.5)
        zScore < 4.0 |> equal true

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