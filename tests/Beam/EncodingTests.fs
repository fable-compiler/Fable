module Fable.Tests.Encoding

open System
open System.Text
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
