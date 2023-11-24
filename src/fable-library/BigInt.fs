module BigInt.Exports

type bigint = BigInt.BigInteger

let isBigInt (x: obj) = x :? bigint

let tryParse (str: string) (res: byref<_>) =
    try
        res <- bigint.Parse str
        true
    with _ ->
        false

let divRem x y (remainder: byref<_>) =
    let quotient, remainder' = bigint.DivRem(x, y)
    remainder <- remainder'
    quotient

let parse = bigint.Parse
let greatestCommonDivisor = bigint.GreatestCommonDivisor
let pow = bigint.Pow
let abs = bigint.Abs

let zero = bigint.Zero
let one = bigint.One
let two = bigint.Two

let fromString (s: string) = bigint.Parse s
let fromZero () = bigint.Zero
let fromOne () = bigint.One
let fromInt64 (i: int64) = new bigint (i)
// We're feeding uint32 here too, so it may happen the value is bigger than Int32.MaxValue
// In that case we need to convert it to int64 to prevent errors. See #1745
let fromInt32 (i: int32) =
    if i > System.Int32.MaxValue then
        new bigint (box i :?> uint32 |> int64)
    else
        new bigint (i)

let toSByte (x: bigint) = x.ToSByte
let toByte (x: bigint) = x.ToByte
let toInt16 (x: bigint) = x.ToInt16
let toUInt16 (x: bigint) = x.ToUInt16
let toInt32 (x: bigint) = x.ToInt32
let toUInt32 (x: bigint) = x.ToUInt32
let toInt64 (x: bigint) = x.ToInt64
let toUInt64 (x: bigint) = x.ToUInt64
let toSingle (x: bigint) = x.ToSingle
let toDouble (x: bigint) = x.ToDouble
let toDecimal (x: bigint) = x.ToDecimal

let sign (x: bigint) = x.Sign
let isZero (x: bigint) = x.IsZero
let isOne (x: bigint) = x.IsOne

let hash (x: bigint) = x.GetHashCode()
let compare (x: bigint) (y: bigint) = (x :> System.IComparable).CompareTo(y)
let equals (x: bigint) (y: bigint) = x.Equals(y)
let toString (x: bigint) = x.ToString()

let get_Zero = bigint.Zero
let get_One = bigint.One
let op_Addition = bigint.(+)
let op_Subtraction = bigint.(-)
let op_Multiply = bigint.(*)
let op_Division = bigint.(/)
let op_Modulus = bigint.(%)
let op_UnaryNegation = bigint.(~-)
let op_UnaryPlus = bigint.(~+)

let op_RightShift = bigint.(>>>)
let op_LeftShift = bigint.(<<<)
let op_BitwiseAnd = bigint.(&&&)
let op_BitwiseOr = bigint.(|||)
let op_ExclusiveOr = bigint.(^^^)

let op_LessThan = bigint.op_LessThan
let op_LessThanOrEqual = bigint.op_LessThanOrEqual
let op_GreaterThan = bigint.op_GreaterThan
let op_GreaterThanOrEqual = bigint.op_GreaterThanOrEqual
let op_Equality = bigint.op_Equality
let op_Inequality = bigint.op_Inequality

open Microsoft.FSharp.Core.Operators

let private flipTwosComplement currByte lowBitFound =
    // Two's complement conversion: starting from LSB, copy up to (and including) first 1 bit, then flip all higher bits.
    match currByte, lowBitFound with
    | _, true -> currByte ^^^ 255uy &&& 255uy, true // Flip all bits since low bit already found
    | 0uy, false -> 0uy, false // Haven't found first bit yet and no chance to do so with zero byte
    | _, false ->
        // Found first byte containing a 1, flip higher bits and all future bytes
        let firstBitIndex =
            [|
                0
                1
                2
                3
                4
                5
                6
                7
            |]
            |> Array.find (fun i -> currByte &&& (1uy <<< i) > 0uy)

        (currByte ^^^ (0b11111110uy <<< firstBitIndex)) &&& 255uy, true

// Spec:
// https://docs.microsoft.com/en-us/dotnet/api/system.numerics.biginteger.tobytearray?view=netstandard-2.0
let toByteArray (value: bigint) =
    if value = zero then
        [| 0uy |]
    else
        // If negative, we will encode the bits of the positive value in two's complement form.
        let isPositive = value > zero

        let value =
            if isPositive then
                value
            else
                bigint (-1) * value

        let mask32 = System.UInt32.MaxValue |> int64 |> fromInt64

        let rec loop (accumBytes: byte list) consumeValue lowBitFound =
            if consumeValue <= zero then
                // Return, with high byte added to indicate sign if current high bit does not represent correct sign.
                let accumBytes =
                    // Trim excess leading zeros (or 255s if negative i.e. two's complement)
                    if isPositive then
                        accumBytes |> List.skipWhile (fun b -> b = 0uy)
                    else
                        accumBytes |> List.skipWhile (fun b -> b = 255uy)

                let isHighBitOne = List.head accumBytes &&& 0b10000000uy <> 0uy

                let accumBytes =
                    if isPositive && isHighBitOne then
                        0uy :: accumBytes
                    elif not isPositive && not isHighBitOne then
                        0b11111111uy :: accumBytes
                    else
                        accumBytes

                accumBytes |> List.toArray |> Array.rev
            else
                let currValue = consumeValue &&& mask32 |> toUInt32

                if isPositive then
                    let b0 = currValue |> byte
                    let b1 = currValue >>> 8 |> byte
                    let b2 = currValue >>> 16 |> byte
                    let b3 = currValue >>> 24 |> byte

                    loop
                        (b3 :: b2 :: b1 :: b0 :: accumBytes)
                        (consumeValue >>> 32)
                        false
                else
                    let b0, lowBitFound =
                        flipTwosComplement (currValue |> byte) lowBitFound

                    let b1, lowBitFound =
                        flipTwosComplement (currValue >>> 8 |> byte) lowBitFound

                    let b2, lowBitFound =
                        flipTwosComplement
                            (currValue >>> 16 |> byte)
                            lowBitFound

                    let b3, lowBitFound =
                        flipTwosComplement
                            (currValue >>> 24 |> byte)
                            lowBitFound

                    loop
                        (b3 :: b2 :: b1 :: b0 :: accumBytes)
                        (consumeValue >>> 32)
                        lowBitFound

        loop [] value false

// Spec:
// https://docs.microsoft.com/en-us/dotnet/api/system.numerics.biginteger.-ctor?view=netstandard-2.0#System_Numerics_BigInteger__ctor_System_Byte___
let fromByteArray (bytes: byte array) =
    if isNull bytes then
        raise (System.ArgumentNullException("bytes"))

    if bytes.Length = 0 then
        zero
    else
        // If negative, bits are the two's complement of the positive value.
        // We will reverse the two's complement back to the positive value, and then multiply by -1.
        let isPositive = bytes.[bytes.Length - 1] &&& 0b10000000uy = 0uy
        let buffer = Array.create 4 0uy

        let rec loop
            (accumUInt32: uint32 list)
            currIndex
            bytesRemaining
            lowBitFound
            =
            if bytesRemaining = 0 then
                accumUInt32
                |> List.fold
                    (fun acc value ->
                        (acc <<< 32) + (value |> int64 |> fromInt64)
                    )
                    zero
                |> fun value ->
                    if isPositive then
                        value
                    else
                        bigint (-1) * value
            else
                let bytesToProcess = min bytesRemaining 4

                for i = 0 to bytesToProcess - 1 do
                    buffer.[i] <- bytes.[currIndex + i] // fill buffer with up to 4 bytes

                if isPositive then
                    Array.fill buffer bytesToProcess (4 - bytesToProcess) 0uy // clear any unfilled bytes in buffer

                    let value =
                        uint32 buffer.[0]
                        ||| (uint32 buffer.[1] <<< 8)
                        ||| (uint32 buffer.[2] <<< 16)
                        ||| (uint32 buffer.[3] <<< 24)

                    loop
                        (value :: accumUInt32)
                        (currIndex + bytesToProcess)
                        (bytesRemaining - bytesToProcess)
                        false
                else
                    Array.fill buffer bytesToProcess (4 - bytesToProcess) 255uy // clear any unfilled bytes in buffer (255 for two's complement)

                    let b0, lowBitFound =
                        flipTwosComplement buffer.[0] lowBitFound

                    let b1, lowBitFound =
                        flipTwosComplement buffer.[1] lowBitFound

                    let b2, lowBitFound =
                        flipTwosComplement buffer.[2] lowBitFound

                    let b3, lowBitFound =
                        flipTwosComplement buffer.[3] lowBitFound

                    let value =
                        uint32 b0
                        ||| (uint32 b1 <<< 8)
                        ||| (uint32 b2 <<< 16)
                        ||| (uint32 b3 <<< 24)

                    loop
                        (value :: accumUInt32)
                        (currIndex + bytesToProcess)
                        (bytesRemaining - bytesToProcess)
                        lowBitFound

        loop [] 0 bytes.Length false
