module BigInt.Exports

type bigint = BigInt.BigInteger

let isBigInt (x: obj) = x :? bigint
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

let op_LessThan = bigint.op_LessThan
let op_LessThanOrEqual = bigint.op_LessThanOrEqual
let op_GreaterThan = bigint.op_GreaterThan
let op_GreaterThanOrEqual = bigint.op_GreaterThanOrEqual
let op_Equality = bigint.op_Equality
let op_Inequality = bigint.op_Inequality

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

let fromZero () = bigint.Zero
let fromOne () = bigint.One
let fromInt32 (i:int32) = new bigint(i)
let fromInt64 (i:int64) = new bigint(i)
let fromString (s:string) = bigint.Parse s

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
