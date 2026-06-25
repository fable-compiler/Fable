// Fable-only: a small, dependency-free, synchronous raw-DEFLATE (RFC 1951)
// decompressor.
//
// Newer F# compilers (F# 8+) compress the embedded signature/optimization data
// resources of an assembly using a raw `DeflateStream`. When fcs-fable runs as
// JavaScript (browser REPL / fable-compiler-js) `System.IO.Compression` is not
// available, so `CompilerImports.decompressResource` cannot use `DeflateStream`.
// This module provides the inflate it needs, compiled to JS like the rest of
// fcs-fable so it works identically in the browser and Node without adding any
// runtime dependency.

module internal Internal.Utilities.FableInflate

// Length codes 257..285: base length and number of extra bits (RFC 1951 §3.2.5)
let private lengthBase =
    [| 3; 4; 5; 6; 7; 8; 9; 10; 11; 13; 15; 17; 19; 23; 27; 31; 35; 43; 51; 59; 67; 83; 99; 115; 131; 163; 195; 227; 258 |]

let private lengthExtra =
    [| 0; 0; 0; 0; 0; 0; 0; 0; 1; 1; 1; 1; 2; 2; 2; 2; 3; 3; 3; 3; 4; 4; 4; 4; 5; 5; 5; 5; 0 |]

// Distance codes 0..29: base distance and number of extra bits
let private distBase =
    [| 1; 2; 3; 4; 5; 7; 9; 13; 17; 25; 33; 49; 65; 97; 129; 193; 257; 385; 513; 769; 1025; 1537; 2049; 3073; 4097; 6145; 8193; 12289; 16385; 24577 |]

let private distExtra =
    [| 0; 0; 0; 0; 1; 1; 2; 2; 3; 3; 4; 4; 5; 5; 6; 6; 7; 7; 8; 8; 9; 9; 10; 10; 11; 11; 12; 12; 13; 13 |]

// Order in which code-length code lengths are stored for dynamic blocks
let private clcOrder =
    [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]

[<Literal>]
let private MaxBits = 15

// LSB-first bit reader over a byte array
type private BitReader =
    { Data: byte[]
      mutable BytePos: int
      mutable BitBuf: int
      mutable BitCnt: int }

let private getBit (r: BitReader) : int =
    if r.BitCnt = 0 then
        r.BitBuf <- int r.Data[r.BytePos]
        r.BytePos <- r.BytePos + 1
        r.BitCnt <- 8

    let bit = r.BitBuf &&& 1
    r.BitBuf <- r.BitBuf >>> 1
    r.BitCnt <- r.BitCnt - 1
    bit

let private getBits (r: BitReader) (n: int) : int =
    let mutable v = 0

    for i in 0 .. n - 1 do
        v <- v ||| (getBit r <<< i)

    v

// Canonical Huffman table represented as a count-of-codes-per-length array plus
// the symbols ordered by (length, symbol). Decoded with the classic puff.c walk.
type private Huffman = { Counts: int[]; Symbols: int[] }

let private buildHuffman (lengths: int[]) (n: int) : Huffman =
    let counts = Array.zeroCreate (MaxBits + 1)

    for i in 0 .. n - 1 do
        counts[lengths[i]] <- counts[lengths[i]] + 1

    counts[0] <- 0

    // Starting offset in Symbols for each code length
    let offsets = Array.zeroCreate (MaxBits + 1)
    let mutable sum = 0

    for len in 1..MaxBits do
        offsets[len] <- sum
        sum <- sum + counts[len]

    let symbols = Array.zeroCreate n

    for i in 0 .. n - 1 do
        if lengths[i] <> 0 then
            symbols[offsets[lengths[i]]] <- i
            offsets[lengths[i]] <- offsets[lengths[i]] + 1

    { Counts = counts; Symbols = symbols }

let private decodeSymbol (r: BitReader) (h: Huffman) : int =
    let mutable code = 0
    let mutable first = 0
    let mutable index = 0
    let mutable len = 1
    let mutable result = -1

    while result < 0 do
        if len > MaxBits then
            failwith "inflate: invalid Huffman code"

        code <- code ||| (getBit r)
        let count = h.Counts[len]

        if code - first < count then
            result <- h.Symbols[index + (code - first)]
        else
            index <- index + count
            first <- (first + count) <<< 1
            code <- code <<< 1
            len <- len + 1

    result

let private inflateBlock (r: BitReader) (out: ResizeArray<byte>) (lit: Huffman) (dist: Huffman) =
    let mutable stop = false

    while not stop do
        let sym = decodeSymbol r lit

        if sym = 256 then
            stop <- true
        elif sym < 256 then
            out.Add(byte sym)
        else
            let s = sym - 257
            let length = lengthBase[s] + getBits r lengthExtra[s]
            let dsym = decodeSymbol r dist
            let distance = distBase[dsym] + getBits r distExtra[dsym]
            let start = out.Count - distance

            // Copy may overlap with the data being produced (LZ77 back-reference)
            for i in 0 .. length - 1 do
                out.Add(out[start + i])

// Fixed Huffman tables (RFC 1951 §3.2.6)
let private fixedLit =
    let lengths = Array.zeroCreate 288

    for i in 0..143 do
        lengths[i] <- 8

    for i in 144..255 do
        lengths[i] <- 9

    for i in 256..279 do
        lengths[i] <- 7

    for i in 280..287 do
        lengths[i] <- 8

    buildHuffman lengths 288

let private fixedDist = buildHuffman (Array.create 30 5) 30

let private readDynamicTables (r: BitReader) : Huffman * Huffman =
    let hlit = getBits r 5 + 257
    let hdist = getBits r 5 + 1
    let hclen = getBits r 4 + 4

    let clcLengths = Array.zeroCreate 19

    for i in 0 .. hclen - 1 do
        clcLengths[clcOrder[i]] <- getBits r 3

    let clcHuff = buildHuffman clcLengths 19

    // Decode the literal/length and distance code lengths as a single run
    let lengths = Array.zeroCreate (hlit + hdist)
    let mutable i = 0

    while i < hlit + hdist do
        let sym = decodeSymbol r clcHuff

        if sym < 16 then
            lengths[i] <- sym
            i <- i + 1
        elif sym = 16 then
            // Repeat previous length 3..6 times
            let repeat = getBits r 2 + 3
            let prev = lengths[i - 1]

            for _ in 1..repeat do
                lengths[i] <- prev
                i <- i + 1
        elif sym = 17 then
            // Repeat zero 3..10 times
            let repeat = getBits r 3 + 3

            for _ in 1..repeat do
                lengths[i] <- 0
                i <- i + 1
        else
            // sym = 18: repeat zero 11..138 times
            let repeat = getBits r 7 + 11

            for _ in 1..repeat do
                lengths[i] <- 0
                i <- i + 1

    let litHuff = buildHuffman (Array.sub lengths 0 hlit) hlit
    let distHuff = buildHuffman (Array.sub lengths hlit hdist) hdist
    litHuff, distHuff

/// Decompress a raw DEFLATE (RFC 1951) stream, as produced by
/// `System.IO.Compression.DeflateStream`.
let inflateRaw (input: byte[]) : byte[] =
    let r =
        { Data = input
          BytePos = 0
          BitBuf = 0
          BitCnt = 0 }

    let out = ResizeArray<byte>()
    let mutable final = false

    while not final do
        final <- getBit r = 1
        let btype = getBits r 2

        match btype with
        | 0 ->
            // Stored (uncompressed) block: skip to the next byte boundary, then
            // read LEN (and the one's-complement NLEN we don't need) and copy.
            r.BitBuf <- 0
            r.BitCnt <- 0
            let len = int r.Data[r.BytePos] ||| (int r.Data[r.BytePos + 1] <<< 8)
            r.BytePos <- r.BytePos + 4

            for _ in 1..len do
                out.Add(r.Data[r.BytePos])
                r.BytePos <- r.BytePos + 1
        | 1 -> inflateBlock r out fixedLit fixedDist
        | 2 ->
            let litHuff, distHuff = readDynamicTables r
            inflateBlock r out litHuff distHuff
        | _ -> failwith "inflate: invalid block type"

    out.ToArray()
