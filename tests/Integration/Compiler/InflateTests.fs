module Fable.Tests.Compiler.Inflate

// Direct unit tests for the pure-managed RFC 1951 inflate used by fcs-fable when
// running as JavaScript (where System.IO.Compression is unavailable). We compress
// here with the real `DeflateStream` and assert `FableInflate.inflateRaw` round-trips
// it back. `DeflateStream` emits raw DEFLATE (no zlib/gzip wrapper), exactly what
// `decompressResource` feeds to `inflateRaw`, so this exercises the same source that
// gets transpiled to JS — deliberately covering every block type and the LZ77 path.

open System.IO
open System.IO.Compression
open Util.Testing
open Internal.Utilities

let private deflate (level: CompressionLevel) (data: byte[]) : byte[] =
    let ms = new MemoryStream()

    (use ds = new DeflateStream(ms, level, true)
     ds.Write(data, 0, data.Length))

    let result = ms.ToArray()
    ms.Dispose()
    result

let private roundtrip (level: CompressionLevel) (data: byte[]) =
    let compressed = deflate level data
    let inflated = FableInflate.inflateRaw compressed
    inflated |> equal data

let private pseudoRandom (n: int) : byte[] =
    // Deterministic so the test is reproducible; incompressible enough that
    // NoCompression yields stored blocks and Optimal yields dynamic Huffman.
    let rnd = System.Random(1234)
    let arr = Array.zeroCreate n
    rnd.NextBytes(arr)
    arr

let private text =
    "The quick brown fox jumps over the lazy dog. "
    |> String.replicate 200
    |> System.Text.Encoding.UTF8.GetBytes

let tests =
    testList "Inflate" [
        // A raw DEFLATE stream encodes an empty payload as a single fixed-Huffman
        // block holding only the end-of-block symbol (bytes 0x03 0x00). Feeding
        // that canonical vector directly is deterministic coverage of btype 1
        // (fixed Huffman) — DeflateStream itself emits nothing for empty input.
        testCase "fixed Huffman block (canonical empty stream)" <| fun _ ->
            FableInflate.inflateRaw [| 0x03uy; 0x00uy |] |> equal [||]

        testCase "single byte" <| fun _ ->
            roundtrip CompressionLevel.Optimal [| 0x42uy |]

        testCase "short ASCII text (dynamic Huffman)" <| fun _ ->
            roundtrip CompressionLevel.Optimal (System.Text.Encoding.UTF8.GetBytes "Hello, Fable!")

        testCase "repeated text exercises LZ77 back-references" <| fun _ ->
            roundtrip CompressionLevel.Optimal text

        testCase "run of identical bytes exercises overlapping copy" <| fun _ ->
            // distance 1 with a long length forces the overlapping back-reference
            // copy (each appended byte is read back immediately).
            roundtrip CompressionLevel.Optimal (Array.create 10000 0x7Auy)

        testCase "incompressible data with dynamic Huffman" <| fun _ ->
            roundtrip CompressionLevel.Optimal (pseudoRandom 5000)

        testCase "stored blocks (no compression)" <| fun _ ->
            roundtrip CompressionLevel.NoCompression (pseudoRandom 5000)

        testCase "multiple stored blocks (>64KB)" <| fun _ ->
            // A single stored block holds at most 65535 bytes, so this spans more
            // than one btype 0 block.
            roundtrip CompressionLevel.NoCompression (pseudoRandom 70000)

        testCase "large repetitive payload" <| fun _ ->
            roundtrip CompressionLevel.Optimal (String.replicate 5000 "abcdefgh" |> System.Text.Encoding.UTF8.GetBytes)
    ]
