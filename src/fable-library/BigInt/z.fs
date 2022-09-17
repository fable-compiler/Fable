// source: https://github.com/fsharp/fsharp
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

#nowarn "44" // This construct is deprecated. This function is for use by compiled F# code and should not be used directly
namespace BigInt

#if FX_NO_BIGINT
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open Microsoft.FSharp.Primitives.Basics
    open Microsoft.FSharp.Math
    open System
    open System.Globalization

    // INVARIANT: signInt = 1 or -1
    //            value(z) = signInt * v
    // NOTE: 0 has two repns (+1,0) or (-1,0).
    [<Struct>]
    [<CustomEquality; CustomComparison>]
#if !NETSTANDARD1_6
    [<StructuredFormatDisplay("{StructuredDisplayString}I")>]
#endif
    type BigInteger(signInt:int, v : BigNat) =

        static let smallLim =  4096
        static let smallPosTab = Array.init smallLim BigNatModule.ofInt32
        static let one = BigInteger(1)
        static let two = BigInteger(2)
        static let zero = BigInteger(0)

        static member internal nat n =
            if BigNatModule.isSmall n && BigNatModule.getSmall n < smallLim
            then smallPosTab.[BigNatModule.getSmall n]
            else n

        static member internal create (s,n) = BigInteger(s,BigInteger.nat n)

        static member internal posn n = BigInteger(1,BigInteger.nat n)

        static member internal negn n = BigInteger(-1,BigInteger.nat n)

        member x.Sign = if x.IsZero then 0 else signInt

        member x.SignInt = signInt

        member internal x.V = v

        static member op_Equality (x:BigInteger, y:BigInteger) =
            //System.Console.WriteLine("x = {0}",box x)
            //System.Console.WriteLine("y = {0}",box y)
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigNatModule.equal x.V y.V                    // +1.xv = +1.yv iff xv = yv
            | -1, -1 -> BigNatModule.equal x.V y.V                     // -1.xv = -1.yv iff xv = yv
            |  1,-1 -> BigNatModule.isZero x.V && BigNatModule.isZero y.V       //  1.xv = -1.yv iff xv=0 and yv=0
            | -1, 1 -> BigNatModule.isZero x.V && BigNatModule.isZero y.V       // -1.xv =  1.yv iff xv=0 and yv=0
            |  0, 0 -> true
            |  0, 1 -> BigNatModule.isZero y.V
            |  0, -1 -> BigNatModule.isZero y.V
            |  1, 0 -> BigNatModule.isZero x.V
            | -1, 0 -> BigNatModule.isZero x.V
            | _ -> invalidArg "x" "signs should be +/- 1 or 0"

        static member op_Inequality (x:BigInteger, y:BigInteger) = not (BigInteger.op_Equality(x,y)) // CA2226: OperatorsShouldHaveSymmetricalOverloads

        static member op_LessThan (x:BigInteger, y:BigInteger) =
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigNatModule.lt x.V y.V                       //  1.xv <  1.yv iff xv < yv
            | -1,-1 -> BigNatModule.lt y.V x.V                       // -1.xv < -1.yv iff yv < xv
            |  1,-1 -> false                              //  1.xv < -1.yv iff 0 <= 1.xv < -1.yv <= 0 iff false
            | -1, 1 -> not (BigNatModule.isZero x.V) || not (BigNatModule.isZero y.V)
                                                          // -1.xv <  1.yv
                                   // (a) xv=0 and yv=0,  then false
                                   // (b) xv<>0,          -1.xv <  0 <= 1.yv, so true
                                   // (c) yv<>0,          -1.xv <= 0 <  1.yv, so true
            |  0, 0 -> false
            |  0, 1 -> not (BigNatModule.isZero y.V)
            |  0,-1 -> false
            |  1, 0 -> false
            | -1, 0 -> not (BigNatModule.isZero x.V)
            | _ -> invalidArg "x" "signs should be +/- 1 or 0"

        static member op_GreaterThan (x:BigInteger, y:BigInteger) = // Follow lt by +/- symmetry
            match x.SignInt,y.SignInt with
            | 1, 1 -> BigNatModule.gt x.V y.V
            | -1,-1 -> BigNatModule.gt y.V x.V
            |  1,-1 -> not (BigNatModule.isZero x.V) || not (BigNatModule.isZero y.V)
            | -1, 1 -> false
            |  0, 0 -> false
            |  0, 1 -> false
            |  0,-1 -> not (BigNatModule.isZero y.V)
            |  1, 0 -> not (BigNatModule.isZero x.V)
            | -1, 0 -> false
            | _ -> invalidArg "x" "signs should be +/- 1 or 0"

        static member internal compare(n,nn) = if BigInteger.op_LessThan(n,nn) then -1 elif BigInteger.op_Equality(n,nn) then 0 else 1

        static member internal hash (z:BigInteger) =
            if z.SignInt = 0 then 1   // 1 is hashcode for initialized BigInteger.Zero
            else z.SignInt + BigNatModule.hash(z.V)

        override x.ToString() =
            match x.SignInt with
            |  1 -> BigNatModule.toString x.V                       // positive
            | -1 ->
                if BigNatModule.isZero x.V
                then "0"                    // not negative in fact, but zero.
                else "-" + BigNatModule.toString x.V  // negative
            |  0 -> "0"
            | _ -> invalidOp "signs should be +/- 1 or 0"

        member x.StructuredDisplayString = x.ToString()

        interface Fable.Core.IJsonSerializable with
            member this.toJSON() =
                this.ToString() |> box

        interface System.IComparable with
            member this.CompareTo(obj:obj) =
                match obj with
                | :? BigInteger as that -> BigInteger.compare(this,that)
                | _ -> invalidArg "obj" "the objects are not comparable"

        override this.Equals(obj) =
            match obj with
            | :? BigInteger as that -> BigInteger.op_Equality(this, that)
            | _ -> false

        override x.GetHashCode() = BigInteger.hash(x)

        new (n:int32) =
            if n>=0
            then BigInteger (1,BigInteger.nat(BigNatModule.ofInt32   n))
            elif (n = System.Int32.MinValue)
            then BigInteger(-1,BigInteger.nat(BigNatModule.ofInt64 (-(int64 n))))
            else BigInteger(-1,BigInteger.nat(BigNatModule.ofInt32 (-n)))

        new (n:int64) =
            if n>=0L
            then BigInteger(1,BigInteger.nat (BigNatModule.ofInt64   n))
            elif (n = System.Int64.MinValue)
            then BigInteger(-1,BigInteger.nat (BigNatModule.add (BigNatModule.ofInt64 System.Int64.MaxValue) BigNatModule.one) )
            else BigInteger(-1,BigInteger.nat (BigNatModule.ofInt64 (-n)))

        static member One = one
        static member Two = two
        static member Zero = zero

        static member (~-) (z:BigInteger)  =
            match z.SignInt with
            | 0 -> BigInteger.Zero
            | i -> BigInteger.create(-i, z.V)

        static member Scale(k, z:BigInteger) =
            if z.SignInt = 0 then BigInteger.Zero else
            if k<0
            then BigInteger.create(-z.SignInt, (BigNatModule.scale (-k) z.V))  // k.zsign.zv = -zsign.(-k.zv)
            else BigInteger.create(z.SignInt, (BigNatModule.scale k z.V))     // k.zsign.zv =  zsign.k.zv

        // Result: 1.nx - 1.ny  (integer subtraction)
        static member internal subnn (nx,ny) =
            if BigNatModule.gte nx ny
            then BigInteger.posn (BigNatModule.sub nx ny)          // nx >= ny, result +ve,  +1.(nx - ny)
            else BigInteger.negn (BigNatModule.sub ny nx)          // nx < ny,  result -ve,  -1.(ny - nx)

        static member internal addnn (nx,ny) =
            BigInteger.posn (BigNatModule.add nx ny)              // Compute "nx + ny" to be integer

        member x.IsZero = x.SignInt = 0 || BigNatModule.isZero x.V

        member x.IsOne = (x.SignInt = 1) && BigNatModule.isOne x.V       // signx.xv = 1 iff signx = +1 and xv = 1

        static member (+) (x:BigInteger,y:BigInteger) =
            if y.IsZero then x else
            if x.IsZero then y else
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigInteger.addnn(x.V,y.V)                //  1.xv +  1.yv =  (xv + yv)
            | -1,-1 -> -(BigInteger.addnn(x.V,y.V))          // -1.xv + -1.yv = -(xv + yv)
            |  1,-1 -> BigInteger.subnn (x.V,y.V)                //  1.xv + -1.yv =  (xv - yv)
            | -1, 1 -> BigInteger.subnn(y.V,x.V)                // -1.xv +  1.yv =  (yv - xv)
            | _ -> invalidArg "x" "signs should be +/- 1"

        static member (-) (x:BigInteger,y:BigInteger) =
            if y.IsZero then x else
            if x.IsZero then -y else
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigInteger.subnn(x.V,y.V)                //  1.xv -  1.yv =  (xv - yv)
            | -1,-1 -> BigInteger.subnn(y.V,x.V)                // -1.xv - -1.yv =  (yv - xv)
            |  1,-1 -> BigInteger.addnn(x.V,y.V)                //  1.xv - -1.yv =  (xv + yv)
            | -1, 1 -> -(BigInteger.addnn(x.V,y.V))          // -1.xv -  1.yv = -(xv + yv)
            | _ -> invalidArg "x" "signs should be +/- 1"

        static member ( * ) (x:BigInteger,y:BigInteger) =
            if x.IsZero then x
            elif y.IsZero then y
            elif x.IsOne then y
            elif y.IsOne then x
            else
                let m = (BigNatModule.mul x.V y.V)
                BigInteger.create (x.SignInt * y.SignInt,m)  // xsign.xv * ysign.yv = (xsign.ysign).(xv.yv)

        static member DivRem (x:BigInteger, y:BigInteger) =
            if y.IsZero then raise (new System.DivideByZeroException())
            if x.IsZero then
                BigInteger.Zero, BigInteger.Zero
            else
            let d,r = BigNatModule.divmod x.V y.V
            // HAVE: |x| = d.|y| + r and 0 <= r < |y|
            // HAVE: xv  = d.yv  + r and 0 <= r < yv
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigInteger.posn d, BigInteger.posn r     //  1.xv =  1.d.( 1.yv) + ( 1.r)
            | -1,-1 -> BigInteger.posn d, BigInteger.negn r     // -1.xv =  1.d.(-1.yv) + (-1.r)
            |  1,-1 -> BigInteger.negn d, BigInteger.posn r     //  1.xv = -1.d.(-1.yv) + ( 1.r)
            | -1, 1 -> BigInteger.negn d, BigInteger.negn r     // -1.xv = -1.d.( 1.yv) + (-1.r)
            | _ -> invalidArg "x" "signs should be +/- 1"

        static member (/) (x:BigInteger,y:BigInteger) =
            BigInteger.DivRem(x,y) |> fst

        static member (%) (x:BigInteger,y:BigInteger) =
            BigInteger.DivRem(x,y) |> snd

        static member (>>>) (x:BigInteger,y:int32) =
            x / BigInteger.Pow(BigInteger.Two, y)

        static member (<<<) (x:BigInteger,y:int32) =
            x * BigInteger.Pow(BigInteger.Two, y)

        static member (&&&) (x:BigInteger,y:BigInteger) =
            BigInteger.posn (BigNatModule.bitAnd x.V y.V) // sign is ignored

        static member (|||) (x:BigInteger,y:BigInteger) =
            BigInteger.posn (BigNatModule.bitOr x.V y.V)  // sign is ignored

        static member (^^^) (x:BigInteger,y:BigInteger) =
            BigInteger.posn (BigNatModule.bitXor x.V y.V)  // sign is ignored

        static member GreatestCommonDivisor (x:BigInteger,y:BigInteger) =
            match x.SignInt,y.SignInt with
            |  0, 0 -> BigInteger.Zero
            |  0, _ -> BigInteger.posn y.V
            |  _, 0 -> BigInteger.posn x.V
            |  _    -> BigInteger.posn (BigNatModule.hcf x.V y.V) // hcf (xsign.xv,ysign.yv) = hcf (xv,yv)

        member x.IsNegative = x.SignInt = -1 && not (x.IsZero)  // signx.xv < 0 iff signx = -1 and xv<>0

        member x.IsPositive = x.SignInt =  1 && not (x.IsZero)  // signx.xv > 0 iff signx = +1 and xv<>0

        static member Abs (x:BigInteger)  = if x.SignInt = -1 then -x else x

        static member op_LessThanOrEqual (x:BigInteger,y:BigInteger) =
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigNatModule.lte x.V y.V                      //  1.xv <=  1.yv iff xv <= yv
            | -1,-1 -> BigNatModule.lte y.V x.V                      // -1.xv <= -1.yv iff yv <= xv
            |  1,-1 -> BigNatModule.isZero x.V && BigNatModule.isZero y.V       //  1.xv <= -1.yv,
                                                          // (a) if xv=0 and yv=0 then true
                                                          // (b) otherwise false, only meet at zero.

            | -1, 1 -> true                               // -1.xv <= 1.yv, true
            |  0, 0 -> true
            |  1, 0 -> BigNatModule.isZero x.V
            | -1, 0 -> true
            |  0, 1 -> true
            |  0,-1 -> BigNatModule.isZero y.V
            | _ -> invalidArg "x" "signs should be +/- 1 or 0"

        static member op_GreaterThanOrEqual (x:BigInteger,y:BigInteger) = // Follow lte by +/- symmetry
            match x.SignInt,y.SignInt with
            |  1, 1 -> BigNatModule.gte x.V y.V
            | -1,-1 -> BigNatModule.gte y.V x.V
            |  1,-1 -> true
            | -1, 1 -> BigNatModule.isZero x.V && BigNatModule.isZero y.V
            |  0, 0 -> true
            |  1, 0 -> true
            | -1, 0 -> BigNatModule.isZero x.V
            |  0, 1 -> BigNatModule.isZero y.V
            |  0,-1 -> true
            | _ -> invalidArg "x" "signs should be +/- 1 or 0"

        static member Pow (x:BigInteger,y:int32) =
            if y < 0 then raise (new System.ArgumentOutOfRangeException("y"))//, (SR.GetString(SR.inputMustBeNonNegative))))
            match x.IsZero, y with
            | true, 0 -> BigInteger.One
            | true, _ -> BigInteger.Zero
            | _ ->
                let yval = BigInteger(y)
                BigInteger.create ((if BigNatModule.isZero (BigNatModule.rem yval.V BigNatModule.two) then 1 else x.SignInt), BigNatModule.pow x.V yval.V)

        member x.ToInt32 =
            if x.IsZero then 0 else
            let u = BigNatModule.toUInt32 x.V
            if u <= uint32 System.Int32.MaxValue then
                // Handle range [-MaxValue,MaxValue]
                x.SignInt * int32 u
            elif x.SignInt = -1 &&       u = uint32 (System.Int32.MaxValue + 1) then
                //assert(System.Int32.MinValue = 0 - System.Int32.MaxValue - 1)
                // Handle MinValue = -(MaxValue+1) special case not covered by the above
                System.Int32.MinValue
            else
                raise (System.OverflowException())

        member x.ToUInt32 =
            if x.IsZero then 0u else BigNatModule.toUInt32 x.V

        member x.ToInt64 =
            if x.IsZero then 0L else
            let u = BigNatModule.toUInt64 x.V
            if u <= uint64 System.Int64.MaxValue then
                (* Handle range [-MaxValue,MaxValue] *)
                int64 x.SignInt * int64 u
            elif x.SignInt = -1 &&       u = uint64 (System.Int64.MaxValue + 1L) then
                //assert(System.Int64.MinValue = 0 - System.Int64.MaxValue - 1L)
                (* Handle MinValue = -(MaxValue+1) special case not covered by the above *)
                System.Int64.MinValue
            else
                raise (System.OverflowException())

        member x.ToUInt64 =
            if x.IsZero then 0UL else BigNatModule.toUInt64 x.V

        member x.ToDouble =
            match x.SignInt with
            |  1 ->    BigNatModule.toFloat x.V                     // float (1.xv)  =   float (xv)
            | -1 -> - (BigNatModule.toFloat x.V)                    // float (-1.xv) = - float (xv)
            |  0 -> 0.
            | _ -> invalidArg "x" "signs should be +/- 1 or 0"

        member x.ToSByte = sbyte x.ToInt32
        member x.ToByte = byte x.ToUInt32
        member x.ToInt16 = int16 x.ToInt32
        member x.ToUInt16 = uint16 x.ToUInt32
        member x.ToSingle = single x.ToDouble
        member x.ToDecimal = decimal x.ToDouble //TODO: proper implementation

        // static member op_Explicit (x:BigInteger) = x.ToSByte
        // static member op_Explicit (x:BigInteger) = x.ToByte
        // static member op_Explicit (x:BigInteger) = x.ToInt16
        // static member op_Explicit (x:BigInteger) = x.ToUInt16
        // static member op_Explicit (x:BigInteger) = x.ToInt32
        // static member op_Explicit (x:BigInteger) = x.ToUInt32
        // static member op_Explicit (x:BigInteger) = x.ToInt64
        // static member op_Explicit (x:BigInteger) = x.ToUInt64
        // static member op_Explicit (x:BigInteger) = x.ToSingle
        // static member op_Explicit (x:BigInteger) = x.ToDouble
        // static member op_Explicit (x:BigInteger) = x.ToDecimal

        static member Parse(text:string) =
            if isNull text then raise (new ArgumentNullException("text"))
            let text = text.Trim()
            let len = text.Length
            if len = 0 then raise (new System.FormatException())//SR.GetString(SR.badFormatString)))
            match text.[0], len with
            | '-', 1 -> raise (new System.FormatException())//SR.GetString(SR.badFormatString)))
            | '-', _ -> BigInteger.negn (BigNatModule.ofString text.[1..len-1])
            | '+', 1 -> raise (new System.FormatException())//SR.GetString(SR.badFormatString)))
            | '+', _ -> BigInteger.posn (BigNatModule.ofString text.[1..len-1])
            | _ ->      BigInteger.posn (BigNatModule.ofString text)

        member internal x.IsSmall = x.IsZero || BigNatModule.isSmall (x.V)

        static member Factorial (x:BigInteger) =
            if x.IsNegative then invalidArg "x" "mustBeNonNegative"//(SR.GetString(SR.inputMustBeNonNegative))
            if x.IsPositive then BigInteger.posn (BigNatModule.factorial x.V)
            else BigInteger.One

        static member ( ~+ )(n1:BigInteger) = n1

        static member FromInt64(x:int64) = new BigInteger(x)

        static member FromInt32(x:int32) = new BigInteger(x)
#endif
(*
namespace Microsoft.FSharp.Core


    type bigint = System.Numerics.BigInteger

    open System
    open System.Diagnostics.CodeAnalysis
    open System.Globalization
    open Microsoft.FSharp.Core.Operators
    open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
    open System.Numerics

#if FX_NO_BIGINT
    // FxCop suppressions
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Addition(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Division(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_GreaterThan(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_GreaterThanOrEqual(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_LessThan(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_LessThanOrEqual(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Modulus(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Multiply(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_Subtraction(System.Numerics.BigInteger,System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_UnaryNegation(System.Numerics.BigInteger)")>]
    [<assembly: SuppressMessage("Microsoft.Usage", "CA2225:OperatorOverloadsHaveNamedAlternates", Scope="member", Target="System.Numerics.BigInteger.#op_UnaryPlus(System.Numerics.BigInteger)")>]
    do()
#endif

    [<AutoOpen>]
    module NumericLiterals =

        module NumericLiteralI =
            let tab64 = new System.Collections.Generic.Dictionary<int64,obj>()
            let tabParse = new System.Collections.Generic.Dictionary<string,obj>()

            let FromInt64Dynamic (value:int64) : obj =
                lock tab64 (fun () ->
                    let mutable res = Unchecked.defaultof<_>
                    let ok = tab64.TryGetValue(value,&res)
                    if ok then res else
                    res <- BigInteger(value)
                    tab64.[value] <- res
                    res)

            let inline get32 (x32:int32) =  FromInt64Dynamic (int64 x32)

            let inline isOX s = not (System.String.IsNullOrEmpty(s)) && s.Length > 2 && s.[0] = '0' && s.[1] = 'x'

            let FromZero () : 'T =
                (get32 0 :?> 'T)
                when 'T : BigInteger = BigInteger.Zero

            let FromOne () : 'T =
                (get32 1 :?> 'T)
                when 'T : BigInteger = BigInteger.One

            let FromInt32 (value:int32): 'T =
                (get32 value :?> 'T)
                when 'T : BigInteger = new BigInteger(value)

            let FromInt64 (value:int64): 'T =
                (FromInt64Dynamic value :?> 'T)
                when 'T : BigInteger = new BigInteger(value)

            let getParse s =
                lock tabParse (fun () ->
                let mutable res = Unchecked.defaultof<_>
                let ok = tabParse.TryGetValue(s,&res)
                if ok then
                    res
                else
                    let v =
#if FX_NO_BIGINT
                       BigInteger.Parse s
#else
                       if  isOX s then
                          BigInteger.Parse (s.[2..],NumberStyles.AllowHexSpecifier,CultureInfo.InvariantCulture)
                       else
                          BigInteger.Parse (s,NumberStyles.AllowLeadingSign,CultureInfo.InvariantCulture)
#endif
                    res <-  v
                    tabParse.[s] <- res
                    res)

            let FromStringDynamic (text:string) : obj =
                getParse text

            let FromString (text:string) : 'T =
                (FromStringDynamic text :?> 'T)
                when 'T : BigInteger = getParse text
*)