module Fable.Tests.Encoding

open System
open System.Text
open System.Collections.Generic
open Fable.Tests.Util
open Util.Testing

// --- Encoding.UTF8.GetBytes ---

[<Fact>]
let ``test Encoding.UTF8.GetBytes works`` () =
    let bytes = Encoding.UTF8.GetBytes("hello")
    // In Erlang, string IS a binary, so this should be identity
    let str = Encoding.UTF8.GetString(bytes)
    str |> equal "hello"

[<Fact>]
let ``test Encoding.UTF8 roundtrip works`` () =
    let original = "hello world"
    let bytes = Encoding.UTF8.GetBytes(original)
    let result = Encoding.UTF8.GetString(bytes)
    result |> equal original

[<Fact>]
let ``test Encoding.UTF8.GetBytes with empty string works`` () =
    let bytes = Encoding.UTF8.GetBytes("")
    let str = Encoding.UTF8.GetString(bytes)
    str |> equal ""

[<Fact>]
let ``test Encoding.UTF8 roundtrip with special chars works`` () =
    let original = "héllo wörld"
    let bytes = Encoding.UTF8.GetBytes(original)
    let result = Encoding.UTF8.GetString(bytes)
    result |> equal original

// --- Stopwatch ---

[<Fact>]
let ``test Stopwatch.Frequency returns positive value`` () =
    let freq = System.Diagnostics.Stopwatch.Frequency
    (freq > 0L) |> equal true

[<Fact>]
let ``test Stopwatch.GetTimestamp returns increasing values`` () =
    let t1 = System.Diagnostics.Stopwatch.GetTimestamp()
    let t2 = System.Diagnostics.Stopwatch.GetTimestamp()
    (t2 >= t1) |> equal true

[<Fact>]
let ``test Stopwatch elapsed time calculation works`` () =
    let freq = double System.Diagnostics.Stopwatch.Frequency
    let start = System.Diagnostics.Stopwatch.GetTimestamp()
    // Do some work
    let mutable sum = 0
    for i in 1..1000 do
        sum <- sum + i
    let stop = System.Diagnostics.Stopwatch.GetTimestamp()
    let elapsedMs = (double (stop - start)) * 1000.0 / freq
    // Should be non-negative
    (elapsedMs >= 0.0) |> equal true

// --- BitConverter ---

[<Fact>]
let ``test BitConverter.IsLittleEndian works`` () =
    // Match .NET behavior: little-endian
    System.BitConverter.IsLittleEndian |> equal true

[<Fact>]
let ``test BitConverter.GetBytes UInt64 works`` () =
    let bytes = System.BitConverter.GetBytes(1UL)
    bytes.Length |> equal 8

[<Fact>]
let ``test BitConverter.GetBytes Int32 works`` () =
    let bytes = System.BitConverter.GetBytes(256)
    bytes.Length |> equal 4

[<Fact>]
let ``test BitConverter roundtrip UInt64 works`` () =
    let value = 12345678901234UL
    let bytes = System.BitConverter.GetBytes(value)
    let result = System.BitConverter.ToUInt64(bytes, 0)
    result |> equal value

[<Fact>]
let ``test BitConverter roundtrip Int32 works`` () =
    let value = 42
    let bytes = System.BitConverter.GetBytes(value)
    let result = System.BitConverter.ToInt32(bytes, 0)
    result |> equal value

[<Fact>]
let ``test BitConverter roundtrip Int64 works`` () =
    let value = -12345678901234L
    let bytes = System.BitConverter.GetBytes(value)
    let result = System.BitConverter.ToInt64(bytes, 0)
    result |> equal value

// --- Nullable ---

[<Fact>]
let ``test Nullable constructor works`` () =
    let x = Nullable(42)
    x.HasValue |> equal true
    x.Value |> equal 42

[<Fact>]
let ``test Nullable with float works`` () =
    let x = Nullable(1.0)
    x.HasValue |> equal true
    x.Value |> equal 1.0

// --- Base64 ---

[<Fact>]
let ``test Convert.ToBase64String works`` () =
    let bytes = [| 2uy; 4uy; 6uy; 8uy; 10uy; 12uy; 14uy; 16uy; 18uy; 20uy |]
    Convert.ToBase64String(bytes) |> equal "AgQGCAoMDhASFA=="

[<Fact>]
let ``test Convert.FromBase64String works`` () =
    let result = Convert.FromBase64String("AgQGCAoMDhASFA==")
    result.Length |> equal 10
    result.[0] |> equal 2uy
    result.[9] |> equal 20uy

[<Fact>]
let ``test Base64 roundtrip works`` () =
    let original = [| 0uy; 127uy; 255uy; 1uy; 42uy |]
    let encoded = Convert.ToBase64String(original)
    let decoded = Convert.FromBase64String(encoded)
    decoded |> equal original

[<Fact>]
let ``test Base64 empty array works`` () =
    let bytes = [||] : byte array
    let encoded = Convert.ToBase64String(bytes)
    encoded |> equal ""

// --- Guid ---

[<Fact>]
let ``test Guid.NewGuid works`` () =
    let g1 = Guid.NewGuid()
    let g2 = Guid.NewGuid()
    g1 <> g2 |> equal true

[<Fact>]
let ``test Guid.Empty works`` () =
    let g = Guid.Empty
    g.ToString() |> equal "00000000-0000-0000-0000-000000000000"

[<Fact>]
let ``test Guid.Parse works`` () =
    let g = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"

[<Fact>]
let ``test Guid.ToString returns D format`` () =
    let g = Guid.NewGuid()
    let s = g.ToString()
    // D format: 8-4-4-4-12 with dashes
    s.Length |> equal 36
    s.[8] |> equal '-'
    s.[13] |> equal '-'
    s.[18] |> equal '-'
    s.[23] |> equal '-'

[<Fact>]
let ``test Guid constructor with string works`` () =
    let g = Guid("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"

[<Fact>]
let ``test Guid equality works`` () =
    let g1 = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let g2 = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    g1 = g2 |> equal true

[<Fact>]
let ``test Guid inequality works`` () =
    let g1 = Guid.NewGuid()
    let g2 = Guid.NewGuid()
    g1 <> g2 |> equal true

// --- Random ---

[<Fact>]
let ``test Random.Next works`` () =
    let rnd = Random()
    let x = rnd.Next()
    (x >= 0) |> equal true

[<Fact>]
let ``test Random.Next with max works`` () =
    let rnd = Random()
    let x = rnd.Next(100)
    (x >= 0 && x < 100) |> equal true

[<Fact>]
let ``test Random.Next with min and max works`` () =
    let rnd = Random()
    let x = rnd.Next(10, 20)
    (x >= 10 && x < 20) |> equal true

[<Fact>]
let ``test Random.NextDouble works`` () =
    let rnd = Random()
    let x = rnd.NextDouble()
    (x >= 0.0 && x < 1.0) |> equal true
