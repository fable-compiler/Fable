module BigInt.Exports

type bigint = BigInt.BigInteger

let isBigInt (x: obj) = x :? bigint

let tryParse str =
    try
        let res = bigint.Parse str
        true, res
    with _ ->
        false, bigint.Zero

let parse = bigint.Parse
let divRem = bigint.DivRem
let greatestCommonDivisor = bigint.GreatestCommonDivisor
let pow = bigint.Pow
let abs = bigint.Abs

let zero = bigint.Zero
let one = bigint.One
let two = bigint.Two

let fromString (s:string) = bigint.Parse s
let fromZero () = bigint.Zero
let fromOne () = bigint.One
let fromInt64 (i:int64) = new bigint(i)
// We're feeding uint32 here too, so it may happen the value is bigger than Int32.MaxValue
// In that case we need to convert it to int64 to prevent errors. See #1745
let fromInt32 (i:int32) =
    if i > System.Int32.MaxValue then new bigint(box  i :?> uint32 |> int64)
    else new bigint(i)

let toSByte (x:bigint) = x.ToSByte
let toByte (x:bigint) = x.ToByte
let toInt16 (x:bigint) = x.ToInt16
let toUInt16 (x:bigint) = x.ToUInt16
let toInt32 (x:bigint) = x.ToInt32
let toUInt32 (x:bigint) = x.ToUInt32
let toInt64 (x:bigint) = x.ToInt64
let toUInt64 (x:bigint) = x.ToUInt64
let toSingle (x:bigint) = x.ToSingle
let toDouble (x:bigint) = x.ToDouble
let toDecimal (x:bigint) = x.ToDecimal

let sign (x:bigint) = x.Sign
let isZero (x:bigint) = x.IsZero
let isOne (x:bigint) = x.IsOne

let hash (x:bigint) = x.GetHashCode()
let compare (x:bigint) (y:bigint) = (x :> System.IComparable).CompareTo(y)
let equals (x:bigint) (y:bigint) = x.Equals(y)
let toString (x:bigint) = x.ToString()

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
        let firstBitIndex = [0..7] |> List.find (fun i -> currByte &&& (1uy <<< i) > 0uy)
        (currByte ^^^ (0b11111110uy <<< firstBitIndex)) &&& 255uy, true

// Spec:
// https://docs.microsoft.com/en-us/dotnet/api/system.numerics.biginteger.tobytearray?view=netstandard-2.0
let toByteArray (value:bigint) =
    if value = zero then [|0uy|]
    else
        // If negative, we will encode the bits of the positive value in two's complement form.
        let isPositive = value > zero
        let value = if isPositive then value else bigint(-1) * value
        let rec loop (accumBytes:byte list) consumeValue lowBitFound =
            if consumeValue <= zero then
                // Return, with high byte added to indicate sign if current high bit does not represent correct sign.
                let isHighBitOne = List.head accumBytes &&& 0b10000000uy <> 0uy
                let accumBytes =
                    if isPositive && isHighBitOne then 0uy :: accumBytes
                    elif not isPositive && not isHighBitOne then 0b11111111uy :: accumBytes
                    else accumBytes
                accumBytes
                |> List.toArray
                |> Array.rev
            else
                let currByte = (consumeValue &&& bigint(255) |> toByte)
                if isPositive then
                    loop (currByte :: accumBytes) (consumeValue >>> 8) false
                else
                    let currByte, lowBitFound = flipTwosComplement currByte lowBitFound
                    loop (currByte :: accumBytes) (consumeValue >>> 8) lowBitFound
        loop [] value false

// Spec:
// https://docs.microsoft.com/en-us/dotnet/api/system.numerics.biginteger.-ctor?view=netstandard-2.0#System_Numerics_BigInteger__ctor_System_Byte___
let fromByteArray (bytes:byte array) =
    if isNull bytes then raise (System.ArgumentNullException("bytes"))
    if bytes.Length = 0 then zero
    else
        // If negative, bits are the two's complement of the positive value.
        // We will reverse the two's complement back to the positive value, and then multiply by -1.
        let isPositive = bytes.[bytes.Length - 1] &&& 0b10000000uy = 0uy
        let rec loop (accumBytes:byte list) currIndex lowBitFound =
            if currIndex = bytes.Length then
                accumBytes
                |> List.fold (fun acc b -> (acc <<< 8) ||| (b |> int |> fromInt32)) zero
                |> fun value -> if isPositive then value else bigint(-1) * value
            else
                let currByte = bytes.[currIndex]
                if isPositive then
                    loop (currByte :: accumBytes) (currIndex + 1) false
                else
                    let currByte, lowBitFound = flipTwosComplement currByte lowBitFound
                    loop (currByte :: accumBytes) (currIndex + 1) lowBitFound
        loop [] 0 false
