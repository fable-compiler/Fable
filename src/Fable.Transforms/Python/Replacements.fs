[<RequireQualifiedAccess>]
module Fable.Transforms.Py.Replacements

#nowarn "1182"

open System
open Fable
open Fable.AST
open Fable.AST.Fable

open Fable.Py
open Fable.Transforms
open Replacements.Util

type Context = FSharp2Fable.Context
type ICompiler = FSharp2Fable.IFableCompiler
type CallInfo = ReplaceCallInfo

let (|TypedArrayCompatible|_|) (com: Compiler) (arrayKind: ArrayKind) t =
    match arrayKind, t with
    | ResizeArray, _ -> None
    | _, Number(kind, _) when com.Options.TypedArrays ->
        match kind with
        | Int8 -> Some "Int8Array"
        | UInt8 -> Some "Uint8Array"
        | Int16 -> Some "Int16Array"
        | UInt16 -> Some "Uint16Array"
        | Int32 -> Some "Int32Array"
        | UInt32 -> Some "Uint32Array"
        | Float32 -> Some "Float32Array"
        | Float64 -> Some "Float64Array"
        // Don't use typed array for int64 until we remove our int64 polyfill
        // and use JS BigInt to represent int64
        //        | Int64 -> Some "BigInt64Array"
        //        | UInt64 -> Some "BigUint64Array"
        | Int128
        | UInt128
        | Float16
        | Int64
        | UInt64
        | BigInt
        | Decimal
        | NativeInt
        | UNativeInt -> None
    | _ -> None

let error msg =
    Helper.ConstructorCall(makeIdentExpr "Exception", Any, [ msg ])

let coreModFor =
    function
    | BclGuid -> "guid"
    | BclDateTime -> "date"
    | BclDateTimeOffset -> "date_offset"
    | BclTimer -> "timer"
    | BclTimeSpan -> "time_span"
    | FSharpSet _ -> "set"
    | FSharpMap _ -> "map"
    | FSharpResult _ -> "choice"
    | FSharpChoice _ -> "choice"
    | FSharpReference _ -> "types"
    | BclHashSet _ -> "mutable_set"
    | BclDictionary _ -> "mutable_map"
    | BclKeyValuePair _
    | BclDateOnly
    | BclTimeOnly -> FableError "Cannot decide core module" |> raise

let makeDecimal com r t (x: decimal) =
    let str = x.ToString(System.Globalization.CultureInfo.InvariantCulture)

    Helper.LibCall(
        com,
        "decimal",
        "Decimal",
        t,
        [ makeStrConst str ],
        isConstructor = true,
        ?loc = r
    )

let makeDecimalFromExpr com r t (e: Expr) =
    match e with
    | Value(Fable.NumberConstant(:? float32 as x, Float32, _), _) ->
        makeDecimal com r t (decimal x)
    | Value(Fable.NumberConstant(:? float as x, Float64, _), _) ->
        makeDecimal com r t (decimal x)
    | Value(Fable.NumberConstant(:? decimal as x, Decimal, _), _) ->
        makeDecimal com r t x
    | _ ->
        Helper.LibCall(
            com,
            "decimal",
            "Decimal",
            t,
            [ e ],
            isConstructor = true,
            ?loc = r
        )

let createAtom com (value: Expr) =
    let typ = value.Type
    Helper.LibCall(com, "util", "createAtom", typ, [ value ], [ typ ])

let getRefCell com r typ (expr: Expr) = getFieldWith r typ expr "contents"

let setRefCell com r (expr: Expr) (value: Expr) =
    setExpr r expr (makeStrConst "contents") value

let makeRefCell com r genArg args =
    let typ = makeFSharpCoreType [ genArg ] Types.refCell

    Helper.LibCall(
        com,
        "types",
        "FSharpRef",
        typ,
        args,
        isConstructor = true,
        ?loc = r
    )

let makeRefCellFromValue com r (value: Expr) =
    let typ = value.Type
    makeRefCell com r typ [ value ]

let makeRefFromMutableValue com ctx r t (value: Expr) =
    let getter = Delegate([], value, None, Tags.empty)

    let setter =
        let v = makeUniqueIdent ctx t "v"

        Delegate(
            [ v ],
            Set(value, ValueSet, t, IdentExpr v, None),
            None,
            Tags.empty
        )

    makeRefCell
        com
        r
        t
        [
            getter
            setter
        ]

let makeRefFromMutableField com ctx r t callee key =
    let getter =
        Delegate(
            [],
            Get(callee, FieldInfo.Create(key, isMutable = true), t, r),
            None,
            Tags.empty
        )

    let setter =
        let v = makeUniqueIdent ctx t "v"

        Delegate(
            [ v ],
            Set(callee, FieldSet(key), t, IdentExpr v, r),
            None,
            Tags.empty
        )

    makeRefCell
        com
        r
        t
        [
            getter
            setter
        ]

// Mutable and public module values are compiled as functions, because
// values imported from ES2015 modules cannot be modified (see #986)
let makeRefFromMutableFunc com ctx r t (value: Expr) =
    let getter =
        let info = makeCallInfo None [] []
        let value = makeCall r t info value
        Delegate([], value, None, Tags.empty)

    let setter =
        let v = makeUniqueIdent ctx t "v"
        let args = [ IdentExpr v ]

        let info =
            makeCallInfo
                None
                args
                [
                    t
                    Boolean
                ]

        let value = makeCall r Unit info value
        Delegate([ v ], value, None, Tags.empty)

    makeRefCell
        com
        r
        t
        [
            getter
            setter
        ]

let makeEqOpStrict range left right op =
    Operation(Binary(op, left, right), [ "strict" ], Boolean, range)

let toChar (arg: Expr) =
    match arg.Type with
    | Char
    | String -> arg
    | _ -> Helper.GlobalCall("chr", Char, [ arg ])

let toString com (ctx: Context) r (args: Expr list) =
    match args with
    | [] ->
        "toString is called with empty args"
        |> addErrorAndReturnNull com ctx.InlinePath r
    | head :: tail ->
        match head.Type with
        | Char -> TypeCast(head, String)
        | String -> head
        | Builtin BclGuid when tail.IsEmpty ->
            Helper.GlobalCall("str", String, [ head ], ?loc = r)
        | Builtin(BclGuid | BclTimeSpan as bt) ->
            Helper.LibCall(com, coreModFor bt, "toString", String, args)
        | Number((Int64 | UInt64 | BigInt), _) ->
            Helper.LibCall(com, "util", "int64_to_string", String, args)
        | Number(Int8, _)
        | Number(UInt8, _) ->
            Helper.LibCall(com, "util", "int8_to_string", String, args)
        | Number(Int16, _) ->
            Helper.LibCall(com, "util", "int16_to_string", String, args)
        | Number(Int32, _) ->
            Helper.LibCall(com, "util", "int32_to_string", String, args)
        | Number(Decimal, _) ->
            Helper.LibCall(com, "decimal", "toString", String, args)
        | Number _ ->
            Helper.LibCall(com, "types", "toString", String, [ head ], ?loc = r)
        | Array _
        | List _ ->
            Helper.LibCall(
                com,
                "types",
                "seqToString",
                String,
                [ head ],
                ?loc = r
            )
        // | DeclaredType(ent, _) when ent.IsFSharpUnion || ent.IsFSharpRecord || ent.IsValueType ->
        //     Helper.InstanceCall(head, "toString", String, [], ?loc=r)
        // | DeclaredType(ent, _) ->
        | _ ->
            Helper.LibCall(com, "types", "toString", String, [ head ], ?loc = r)

let getParseParams (kind: NumberKind) =
    let isFloatOrDecimal, numberModule, unsigned, bitsize =
        match kind with
        | Int8 -> false, "Int32", false, 8
        | UInt8 -> false, "Int32", true, 8
        | Int16 -> false, "Int32", false, 16
        | UInt16 -> false, "Int32", true, 16
        | Int32 -> false, "Int32", false, 32
        | UInt32 -> false, "Int32", true, 32
        | Int64 -> false, "Long", false, 64
        | UInt64 -> false, "Long", true, 64
        | Float32 -> true, "Double", false, 32
        | Float64 -> true, "Double", false, 64
        | Decimal -> true, "Decimal", false, 128
        | x -> FableError $"Unexpected kind in getParseParams: %A{x}" |> raise

    isFloatOrDecimal, numberModule, unsigned, bitsize

let castBigIntMethod typeTo =
    match typeTo with
    | Number(kind, _) ->
        match kind with
        | Int8 -> "toSByte"
        | Int16 -> "toInt16"
        | Int32 -> "toInt32"
        | Int64 -> "toInt64"
        | UInt8 -> "toByte"
        | UInt16 -> "toUInt16"
        | UInt32 -> "toUInt32"
        | UInt64 -> "toUInt64"
        | Float32 -> "toSingle"
        | Float64 -> "toDouble"
        | Decimal -> "toDecimal"
        | Int128
        | UInt128
        | Float16
        | BigInt
        | NativeInt
        | UNativeInt ->
            FableError $"Unexpected BigInt/%A{kind} conversion" |> raise
    | _ -> FableError $"Unexpected non-number type %A{typeTo}" |> raise

let kindIndex kind = //         0   1   2   3   4   5   6   7   8   9  10  11
    match kind with //         i8 i16 i32 i64  u8 u16 u32 u64 f32 f64 dec big
    | Int8 -> 0 //  0 i8   -   -   -   -   +   +   +   +   -   -   -   +
    | Int16 -> 1 //  1 i16  +   -   -   -   +   +   +   +   -   -   -   +
    | Int32 -> 2 //  2 i32  +   +   -   -   +   +   +   +   -   -   -   +
    | Int64 -> 3 //  3 i64  +   +   +   -   +   +   +   +   -   -   -   +
    | UInt8 -> 4 //  4 u8   +   +   +   +   -   -   -   -   -   -   -   +
    | UInt16 -> 5 //  5 u16  +   +   +   +   +   -   -   -   -   -   -   +
    | UInt32 -> 6 //  6 u32  +   +   +   +   +   +   -   -   -   -   -   +
    | UInt64 -> 7 //  7 u64  +   +   +   +   +   +   +   -   -   -   -   +
    | Float32 -> 8 //  8 f32  +   +   +   +   +   +   +   +   -   -   -   +
    | Float64 -> 9 //  9 f64  +   +   +   +   +   +   +   +   -   -   -   +
    | Decimal -> 10 // 10 dec  +   +   +   +   +   +   +   +   -   -   -   +
    | BigInt -> 11 // 11 big  +   +   +   +   +   +   +   +   +   +   +   -
    | Float16 -> FableError "Casting to/from float16 is unsupported" |> raise
    | Int128
    | UInt128 -> FableError "Casting to/from (u)int128 is unsupported" |> raise
    | NativeInt
    | UNativeInt ->
        FableError "Casting to/from (u)nativeint is unsupported" |> raise

let needToCast fromKind toKind =
    let v = kindIndex fromKind // argument type (vertical)
    let h = kindIndex toKind // return type (horizontal)

    ((v > h) || (v < 4 && h > 3)) && (h < 8) || (h <> v && (h = 11 || v = 11))

/// Conversions to floating point
let toFloat com (ctx: Context) r targetType (args: Expr list) : Expr =
    match args.Head.Type with
    | Char ->
        //Helper.InstanceCall(args.Head, "charCodeAt", Int32.Number, [ makeIntConst 0 ])
        Helper.LibCall(
            com,
            "char",
            "char_code_at",
            targetType,
            [
                args.Head
                makeIntConst 0
            ]
        )
    | String -> Helper.LibCall(com, "double", "parse", targetType, args)
    | Number(kind, _) ->
        match kind with
        | BigInt ->
            Helper.LibCall(
                com,
                "big_int",
                castBigIntMethod targetType,
                targetType,
                args
            )
        | Decimal ->
            Helper.LibCall(com, "decimal", "toNumber", targetType, args)
        | Int64
        | UInt64 -> Helper.LibCall(com, "long", "toNumber", targetType, args)
        | _ -> TypeCast(args.Head, targetType)
    | _ ->
        addWarning
            com
            ctx.InlinePath
            r
            "Cannot make conversion because source type is unknown"

        TypeCast(args.Head, targetType)

let toDecimal com (ctx: Context) r targetType (args: Expr list) : Expr =
    match args.Head.Type with
    | Char ->
        //Helper.InstanceCall(args.Head, "charCodeAt", Int32.Number, [ makeIntConst 0 ])
        Helper.LibCall(
            com,
            "char",
            "char_code_at",
            targetType,
            [
                args.Head
                makeIntConst 0
            ]
        )
        |> makeDecimalFromExpr com r targetType
    | String -> makeDecimalFromExpr com r targetType args.Head
    | Number(kind, _) ->
        match kind with
        | Decimal -> args.Head
        | BigInt ->
            Helper.LibCall(
                com,
                "big_int",
                castBigIntMethod targetType,
                targetType,
                args
            )
        | Int64
        | UInt64 ->
            Helper.LibCall(com, "long", "toNumber", Float64.Number, args)
            |> makeDecimalFromExpr com r targetType
        | _ -> makeDecimalFromExpr com r targetType args.Head
    | _ ->
        addWarning
            com
            ctx.InlinePath
            r
            "Cannot make conversion because source type is unknown"

        TypeCast(args.Head, targetType)

// Apparently ~~ is faster than Math.floor (see https://coderwall.com/p/9b6ksa/is-faster-than-math-floor)
let fastIntFloor expr =
    let inner = makeUnOp None Any expr UnaryNotBitwise
    makeUnOp None (Int32.Number) inner UnaryNotBitwise

let stringToInt com (ctx: Context) r targetType (args: Expr list) : Expr =
    let kind =
        match targetType with
        | Number(kind, _) -> kind
        | x -> FableError $"Unexpected type in stringToInt: %A{x}" |> raise

    let style = int System.Globalization.NumberStyles.Any

    let _isFloatOrDecimal, numberModule, unsigned, bitsize = getParseParams kind

    let parseArgs =
        [
            makeIntConst style
            makeBoolConst unsigned
            makeIntConst bitsize
        ]

    Helper.LibCall(
        com,
        numberModule,
        "parse",
        targetType,
        [ args.Head ] @ parseArgs @ args.Tail,
        ?loc = r
    )

let toLong
    com
    (ctx: Context)
    r
    (unsigned: bool)
    targetType
    (args: Expr list)
    : Expr
    =
    let fromInteger kind arg =
        let kind = makeIntConst (kindIndex kind)

        Helper.LibCall(
            com,
            "long",
            "fromInteger",
            targetType,
            [
                arg
                makeBoolConst unsigned
                kind
            ]
        )

    let sourceType = args.Head.Type

    match sourceType with
    | Char ->
        //Helper.InstanceCall(args.Head, "charCodeAt", Int32.Number, [ makeIntConst 0 ])
        Helper.LibCall(
            com,
            "char",
            "char_code_at",
            targetType,
            [
                args.Head
                makeIntConst 0
            ]
        )
        |> fromInteger UInt16
    | String -> stringToInt com ctx r targetType args
    | Number(kind, _) ->
        match kind with
        | Decimal ->
            let n =
                Helper.LibCall(com, "decimal", "toNumber", Float64.Number, args)

            Helper.LibCall(
                com,
                "long",
                "fromNumber",
                targetType,
                [
                    n
                    makeBoolConst unsigned
                ]
            )
        | BigInt ->
            Helper.LibCall(
                com,
                "big_int",
                castBigIntMethod targetType,
                targetType,
                args
            )
        | Int64
        | UInt64 ->
            Helper.LibCall(
                com,
                "long",
                "fromValue",
                targetType,
                args @ [ makeBoolConst unsigned ]
            )
        | Int8
        | Int16
        | Int32
        | UInt8
        | UInt16
        | UInt32 as kind -> fromInteger kind args.Head
        | Float32
        | Float64 ->
            Helper.LibCall(
                com,
                "long",
                "fromNumber",
                targetType,
                args @ [ makeBoolConst unsigned ]
            )
        | Float16 ->
            FableError "Casting float16 to long is not supported" |> raise
        | Int128
        | UInt128 ->
            FableError "Casting (u)int128 to long is not supported" |> raise
        | NativeInt
        | UNativeInt ->
            FableError "Converting (u)nativeint to long is not supported"
            |> raise
    | _ ->
        addWarning
            com
            ctx.InlinePath
            r
            "Cannot make conversion because source type is unknown"

        TypeCast(args.Head, targetType)

/// Conversion to integers (excluding longs and bigints)
let toInt com (ctx: Context) r targetType (args: Expr list) =
    let sourceType = args.Head.Type

    let emitCast typeTo arg =
        match typeTo with
        | Int8 ->
            emitExpr None Int8.Number [ arg ] "(int($0) + 0x80 & 0xFF) - 0x80"
        | Int16 ->
            emitExpr
                None
                Int16.Number
                [ arg ]
                "(int($0) + 0x8000 & 0xFFFF) - 0x8000"
        | Int32 -> fastIntFloor arg
        | UInt8 ->
            emitExpr
                None
                UInt8.Number
                [ arg ]
                "int($0+0x100 if $0 < 0 else $0) & 0xFF"
        | UInt16 ->
            emitExpr
                None
                UInt16.Number
                [ arg ]
                "int($0+0x10000 if $0 < 0 else $0) & 0xFFFF"
        | UInt32 ->
            emitExpr
                None
                UInt32.Number
                [ arg ]
                "int($0+0x100000000 if $0 < 0 else $0)"
        | _ -> FableError $"Unexpected non-integer type %A{typeTo}" |> raise

    match sourceType, targetType with
    | Char, _ ->
        //Helper.InstanceCall(args.Head, "charCodeAt", targetType, [ makeIntConst 0 ])
        Helper.LibCall(
            com,
            "char",
            "char_code_at",
            targetType,
            [
                args.Head
                makeIntConst 0
            ]
        )
    | String, _ -> stringToInt com ctx r targetType args
    | Number(BigInt, _), _ ->
        Helper.LibCall(
            com,
            "big_int",
            castBigIntMethod targetType,
            targetType,
            args
        )
    | Number(typeFrom, _), Number(typeTo, _) ->
        if needToCast typeFrom typeTo then
            match typeFrom with
            | Int64
            | UInt64 -> Helper.LibCall(com, "Long", "to_int", targetType, args) // TODO: make no-op
            | Decimal ->
                Helper.LibCall(com, "Decimal", "to_number", targetType, args)
            | _ -> args.Head
            |> emitCast typeTo
        else
            TypeCast(args.Head, targetType)
    | _ ->
        addWarning
            com
            ctx.InlinePath
            r
            "Cannot make conversion because source type is unknown"

        TypeCast(args.Head, targetType)

let round com (args: Expr list) =
    match args.Head.Type with
    | Number(Decimal, _) ->
        let n =
            Helper.LibCall(
                com,
                "decimal",
                "toNumber",
                Float64.Number,
                [ args.Head ]
            )

        let rounded =
            Helper.LibCall(com, "util", "round", Float64.Number, [ n ])

        rounded :: args.Tail
    | Number((Float32 | Float64), _) ->
        let rounded =
            Helper.LibCall(com, "util", "round", Float64.Number, [ args.Head ])

        rounded :: args.Tail
    | _ -> args

let toList com returnType expr =
    Helper.LibCall(com, "list", "ofSeq", returnType, [ expr ])

let stringToCharArray t e =
    //Helper.InstanceCall(e, "split", t, [ makeStrConst "" ])
    // Write as global call `list` instead
    Helper.GlobalCall("list", t, [ e ])

let toSeq t (e: Expr) =
    match e.Type with
    // Convert to array to get 16-bit code units, see #1279
    | String -> stringToCharArray t e
    | _ -> TypeCast(e, t)

let applyOp (com: ICompiler) (ctx: Context) r t opName (args: Expr list) =
    let unOp operator operand =
        Operation(Unary(operator, operand), Tags.empty, t, r)

    let binOp op left right =
        Operation(Binary(op, left, right), Tags.empty, t, r)

    let binOpChar op left right =
        let toUInt16 e = toInt com ctx None UInt16.Number [ e ]

        Operation(
            Binary(op, toUInt16 left, toUInt16 right),
            Tags.empty,
            UInt16.Number,
            r
        )
        |> toChar

    let truncateUnsigned operation = // see #1550
        match t with
        | Number(UInt32, _) ->
            Operation(
                Binary(BinaryShiftRightZeroFill, operation, makeIntConst 0),
                Tags.empty,
                t,
                r
            )
        | _ -> operation

    let logicOp op left right =
        Operation(Logical(op, left, right), Tags.empty, Boolean, r)

    let nativeOp opName argTypes args =
        match opName, args with
        | Operators.addition, [ left; right ] ->
            match argTypes with
            | Char :: _ -> binOpChar BinaryPlus left right
            | _ -> binOp BinaryPlus left right
        | Operators.subtraction, [ left; right ] ->
            match argTypes with
            | Char :: _ -> binOpChar BinaryMinus left right
            | _ -> binOp BinaryMinus left right
        | Operators.multiply, [ left; right ] -> binOp BinaryMultiply left right
        | (Operators.division | Operators.divideByInt), [ left; right ] ->
            match argTypes with
            // Floor result of integer divisions (see #172)
            | Number((Int8 | Int16 | Int32 | UInt8 | UInt16 | UInt32 | Int64 | UInt64 | BigInt),
                     _) :: _ -> binOp BinaryDivide left right |> fastIntFloor
            | _ ->
                Helper.LibCall(
                    com,
                    "double",
                    "divide",
                    t,
                    [
                        left
                        right
                    ],
                    argTypes,
                    ?loc = r
                )
        | Operators.modulus, [ left; right ] -> binOp BinaryModulus left right
        | Operators.leftShift, [ left; right ] ->
            binOp BinaryShiftLeft left right |> truncateUnsigned // See #1530
        | Operators.rightShift, [ left; right ] ->
            match argTypes with
            | Number(UInt32, _) :: _ ->
                binOp BinaryShiftRightZeroFill left right // See #646
            | _ -> binOp BinaryShiftRightSignPropagating left right
        | Operators.bitwiseAnd, [ left; right ] ->
            binOp BinaryAndBitwise left right |> truncateUnsigned
        | Operators.bitwiseOr, [ left; right ] ->
            binOp BinaryOrBitwise left right |> truncateUnsigned
        | Operators.exclusiveOr, [ left; right ] ->
            binOp BinaryXorBitwise left right |> truncateUnsigned
        | Operators.booleanAnd, [ left; right ] -> logicOp LogicalAnd left right
        | Operators.booleanOr, [ left; right ] -> logicOp LogicalOr left right
        | Operators.logicalNot, [ operand ] ->
            unOp UnaryNotBitwise operand |> truncateUnsigned
        | Operators.unaryNegation, [ operand ] ->
            match argTypes with
            | Number(Int8, _) :: _ ->
                Helper.LibCall(
                    com,
                    "int32",
                    "op_UnaryNegation_Int8",
                    t,
                    args,
                    ?loc = r
                )
            | Number(Int16, _) :: _ ->
                Helper.LibCall(
                    com,
                    "int32",
                    "op_UnaryNegation_Int16",
                    t,
                    args,
                    ?loc = r
                )
            | Number(Int32, _) :: _ ->
                Helper.LibCall(
                    com,
                    "int32",
                    "op_UnaryNegation_Int32",
                    t,
                    args,
                    ?loc = r
                )
            | _ -> unOp UnaryMinus operand
        | Operators.unaryPlus, [ operand ] -> unOp UnaryPlus operand
        | _ ->
            $"Operator %s{opName} not found in %A{argTypes}"
            |> addErrorAndReturnNull com ctx.InlinePath r

    let argTypes = args |> List.map (fun a -> a.Type)

    match argTypes with
    | Number(Int64 | UInt64 | BigInt | Decimal as kind, _) :: _ ->
        let modName, opName =
            match kind, opName with
            // | UInt64, Operators.rightShift -> "long", "op_RightShiftUnsigned" // See #1482
            | Decimal, Operators.divideByInt -> "decimal", Operators.division
            | Decimal, _ -> "decimal", opName
            | BigInt, _ -> "big_int", opName
            | _ -> "long", opName

        Helper.LibCall(com, modName, opName, t, args, argTypes, ?loc = r)

    | Builtin(BclDateTime | BclDateTimeOffset as bt) :: _ ->
        Helper.LibCall(com, coreModFor bt, opName, t, args, argTypes, ?loc = r)
    | Builtin(FSharpSet _) :: _ ->
        let mangledName =
            Naming.buildNameWithoutSanitationFrom "FSharpSet" true opName ""

        Helper.LibCall(com, "set", mangledName, t, args, argTypes, ?loc = r)
    // | Builtin (FSharpMap _)::_ ->
    //     let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpMap" true opName overloadSuffix.Value
    //     Helper.LibCall(com, "Map", mangledName, t, args, argTypes, ?loc=r)
    | Builtin BclTimeSpan :: _ -> nativeOp opName argTypes args
    | CustomOp com ctx r t opName args e -> e
    | _ -> nativeOp opName argTypes args

let isCompatibleWithNativeComparison =
    function
    | Builtin(BclGuid | BclTimeSpan | BclTimeOnly)
    | Boolean
    | Char
    | String
    | Number _ -> true
    // TODO: Non-record/union declared types without custom equality
    // should be compatible with Py comparison
    | _ -> false


// Overview of hash rules:
// * `hash`, `Unchecked.hash` first check if GetHashCode is implemented and then default to structural hash.
// * `.GetHashCode` called directly defaults to identity hash (for reference types except string) if not implemented.
// * `LanguagePrimitive.PhysicalHash` creates an identity hash no matter whether GetHashCode is implemented or not.

let identityHash com r (arg: Expr) =
    let methodName =
        match arg.Type with
        // These are the same for identity/structural hashing
        | Char
        | String
        | Builtin BclGuid -> "stringHash"
        | Number((Decimal | BigInt | Int64 | UInt64), _) -> "safeHash"
        | Number _
        | Builtin BclTimeSpan -> "numberHash"
        | List _ -> "safeHash"
        | Tuple _ -> "arrayHash" // F# tuples must use structural hashing
        // These are only used for structural hashing
        // | Array _ -> "arrayHash"
        // | Builtin (BclDateTime|BclDateTimeOffset) -> "dateHash"
        | DeclaredType _ -> "safeHash"
        | _ -> "identityHash"

    Helper.LibCall(com, "Util", methodName, Int32.Number, [ arg ], ?loc = r)

let structuralHash (com: ICompiler) r (arg: Expr) =
    let methodName =
        match arg.Type with
        | Char
        | String
        | Builtin BclGuid -> "stringHash"
        | Number _
        | Builtin BclTimeSpan -> "numberHash"
        | List _ -> "safeHash"
        // TODO: Get hash functions of the generic arguments
        // for better performance when using tuples as map keys
        | Tuple _
        | Array _ -> "arrayHash"
        | Builtin(BclDateTime | BclDateTimeOffset) -> "dateHash"
        | DeclaredType(ent, _) ->
            let ent = com.GetEntity(ent)

            if not ent.IsInterface then
                "safeHash"
            else
                "structuralHash"
        | _ -> "structuralHash"

    Helper.LibCall(com, "Util", methodName, Int32.Number, [ arg ], ?loc = r)

let rec equals (com: ICompiler) ctx r equal (left: Expr) (right: Expr) =
    let is equal expr =
        if equal then
            expr
        else
            makeUnOp None Boolean expr UnaryNot

    match left.Type with
    | Number(Decimal, _) ->
        Helper.LibCall(
            com,
            "decimal",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | Number(BigInt, _) ->
        Helper.LibCall(
            com,
            "big_int",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | Builtin(BclGuid | BclTimeSpan)
    | Boolean
    | Char
    | String
    | Number _ ->
        let op =
            if equal then
                BinaryEqual
            else
                BinaryUnequal

        makeBinOp r Boolean left right op
    | Builtin(BclDateTime | BclDateTimeOffset) ->
        Helper.LibCall(
            com,
            "date",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | Builtin(FSharpSet _ | FSharpMap _) ->
        Helper.InstanceCall(left, "Equals", Boolean, [ right ]) |> is equal
    | DeclaredType _ ->
        Helper.LibCall(
            com,
            "util",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | Array(t, _) ->
        let f = makeEqualityFunction com ctx t

        Helper.LibCall(
            com,
            "array",
            "equalsWith",
            Boolean,
            [
                f
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | List _ ->
        Helper.LibCall(
            com,
            "util",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | MetaType ->
        Helper.LibCall(
            com,
            "reflection",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | Tuple _ ->
        Helper.LibCall(
            com,
            "util",
            "equalArrays",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | _ ->
        Helper.LibCall(
            com,
            "util",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal

/// Compare function that will call Util.compare or instance `CompareTo` as appropriate
and compare (com: ICompiler) ctx r (left: Expr) (right: Expr) =
    let t = Int32.Number

    match left.Type with
    | Number(Decimal, _) ->
        Helper.LibCall(
            com,
            "decimal",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Number(BigInt, _) ->
        Helper.LibCall(
            com,
            "big_int",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Builtin(BclGuid | BclTimeSpan)
    | Boolean
    | Char
    | String
    | Number _ ->
        Helper.LibCall(
            com,
            "util",
            "comparePrimitives",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Builtin(BclDateTime | BclDateTimeOffset) ->
        Helper.LibCall(
            com,
            "date",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | DeclaredType _ ->
        Helper.LibCall(
            com,
            "util",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Array(genArg, _) ->
        let f = makeComparerFunction com ctx genArg
        // TODO: change to compareTo after main sync. See #2961
        Helper.LibCall(
            com,
            "array",
            "compareWith",
            t,
            [
                f
                left
                right
            ],
            ?loc = r
        )
    | List _ ->
        Helper.LibCall(
            com,
            "util",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Tuple _ ->
        Helper.LibCall(
            com,
            "util",
            "compareArrays",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | _ ->
        Helper.LibCall(
            com,
            "util",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )

/// Boolean comparison operators like <, >, <=, >=
and booleanCompare (com: ICompiler) ctx r (left: Expr) (right: Expr) op =
    if isCompatibleWithNativeComparison left.Type then
        makeEqOp r left right op
    else
        let comparison = compare com ctx r left right
        makeEqOp r comparison (makeIntConst 0) op

and makeComparerFunction (com: ICompiler) ctx typArg =
    let x = makeUniqueIdent ctx typArg "x"
    let y = makeUniqueIdent ctx typArg "y"
    let body = compare com ctx None (IdentExpr x) (IdentExpr y)

    Delegate(
        [
            x
            y
        ],
        body,
        None,
        Tags.empty
    )

and makeComparer (com: ICompiler) ctx typArg =
    objExpr [ "Compare", makeComparerFunction com ctx typArg ]

and makeEqualityFunction (com: ICompiler) ctx typArg =
    let x = makeUniqueIdent ctx typArg "x"
    let y = makeUniqueIdent ctx typArg "y"
    let body = equals com ctx None true (IdentExpr x) (IdentExpr y)

    Delegate(
        [
            x
            y
        ],
        body,
        None,
        Tags.empty
    )

let makeEqualityComparer (com: ICompiler) ctx typArg =
    let x = makeUniqueIdent ctx typArg "x"
    let y = makeUniqueIdent ctx typArg "y"

    objExpr
        [
            "Equals",
            Delegate(
                [
                    x
                    y
                ],
                equals com ctx None true (IdentExpr x) (IdentExpr y),
                None,
                Tags.empty
            )
            "GetHashCode",
            Delegate(
                [ x ],
                structuralHash com None (IdentExpr x),
                None,
                Tags.empty
            )
        ]

// TODO: Try to detect at compile-time if the object already implements `Compare`?
let inline makeComparerFromEqualityComparer e = e // leave it as is, if implementation supports it
// Helper.LibCall(com, "Util", "comparerFromEqualityComparer", Any, [e])

/// Adds comparer as last argument for set creator methods
let makeSet (com: ICompiler) ctx r t methName args genArg =
    let args = args @ [ makeComparer com ctx genArg ]
    Helper.LibCall(com, "set", Naming.lowerFirst methName, t, args, ?loc = r)

/// Adds comparer as last argument for map creator methods
let makeMap (com: ICompiler) ctx r t methName args genArg =
    let args = args @ [ makeComparer com ctx genArg ]
    Helper.LibCall(com, "map", Naming.lowerFirst methName, t, args, ?loc = r)

let makeDictionaryWithComparer com r t sourceSeq comparer =
    Helper.LibCall(
        com,
        "mutable_map",
        "Dictionary",
        t,
        [
            sourceSeq
            comparer
        ],
        isConstructor = true,
        ?loc = r
    )

let makeDictionary (com: ICompiler) ctx r t sourceSeq =
    match t with
    | DeclaredType(_, [ key; _ ]) when
        not (isCompatibleWithNativeComparison key)
        ->
        // makeComparer com ctx key
        makeEqualityComparer com ctx key
        |> makeDictionaryWithComparer com r t sourceSeq
    | _ ->
        Helper.GlobalCall(
            "dict",
            t,
            [ sourceSeq ],
            isConstructor = true,
            ?loc = r
        )

let makeHashSetWithComparer com r t sourceSeq comparer =
    Helper.LibCall(
        com,
        "mutable_set",
        "HashSet",
        t,
        [
            sourceSeq
            comparer
        ],
        isConstructor = true,
        ?loc = r
    )

let makeHashSet (com: ICompiler) ctx r t sourceSeq =
    match t with
    | DeclaredType(_, [ key ]) when not (isCompatibleWithNativeComparison key) ->
        // makeComparer com ctx key
        makeEqualityComparer com ctx key
        |> makeHashSetWithComparer com r t sourceSeq
    | _ ->
        Helper.GlobalCall(
            "set",
            t,
            [ sourceSeq ],
            isConstructor = true,
            ?loc = r
        )

let rec getZero (com: ICompiler) ctx (t: Type) =
    match t with
    | Boolean -> makeBoolConst false
    | Number(BigInt, _) as t ->
        Helper.LibCall(com, "big_int", "fromInt32", t, [ makeIntConst 0 ])
    | Number(Decimal, _) as t ->
        makeIntConst 0 |> makeDecimalFromExpr com None t
    | Number(kind, uom) ->
        NumberConstant(getBoxedZero kind, kind, uom) |> makeValue None
    | Char
    | String -> makeStrConst "" // TODO: Use null for string?
    | Builtin BclTimeSpan ->
        Helper.LibCall(com, "time_span", "create", t, [ makeIntConst 0 ])
    | Builtin BclDateTime as t -> Helper.LibCall(com, "date", "minValue", t, [])
    | Builtin BclDateTimeOffset as t ->
        Helper.LibCall(com, "DateOffset", "minValue", t, [])
    | Builtin(FSharpSet genArg) as t -> makeSet com ctx None t "Empty" [] genArg
    | Builtin(BclKeyValuePair(k, v)) ->
        makeTuple
            None
            true
            [
                getZero com ctx k
                getZero com ctx v
            ]
    | ListSingleton(CustomOp com ctx None t "get_Zero" [] e) -> e
    | _ -> Value(Null Any, None) // null

let getOne (com: ICompiler) ctx (t: Type) =
    match t with
    | Boolean -> makeBoolConst true
    | Number(kind, uom) ->
        NumberConstant(getBoxedOne kind, kind, uom) |> makeValue None
    | ListSingleton(CustomOp com ctx None t "get_One" [] e) -> e
    | _ -> makeIntConst 1

let makeAddFunction (com: ICompiler) ctx t =
    let x = makeUniqueIdent ctx t "x"
    let y = makeUniqueIdent ctx t "y"

    let body =
        applyOp
            com
            ctx
            None
            t
            Operators.addition
            [
                IdentExpr x
                IdentExpr y
            ]

    Delegate(
        [
            x
            y
        ],
        body,
        None,
        Tags.empty
    )

let makeGenericAdder (com: ICompiler) ctx t =
    objExpr
        [
            "GetZero", getZero com ctx t |> makeDelegate []
            "Add", makeAddFunction com ctx t
        ]

let makeGenericAverager (com: ICompiler) ctx t =
    let divideFn =
        let x = makeUniqueIdent ctx t "x"
        let i = makeUniqueIdent ctx (Int32.Number) "i"

        let body =
            applyOp
                com
                ctx
                None
                t
                Operators.divideByInt
                [
                    IdentExpr x
                    IdentExpr i
                ]

        Delegate(
            [
                x
                i
            ],
            body,
            None,
            Tags.empty
        )

    objExpr
        [
            "GetZero", getZero com ctx t |> makeDelegate []
            "Add", makeAddFunction com ctx t
            "DivideByInt", divideFn
        ]

let injectArg
    (com: ICompiler)
    (ctx: Context)
    r
    moduleName
    methName
    (genArgs: Type list)
    args
    =
    let injectArgInner args (injectType, injectGenArgIndex) =
        let fail () =
            $"Cannot inject arg to %s{moduleName}.%s{methName} (genArgs %A{genArgs} - expected index %i{injectGenArgIndex})"
            |> addError com ctx.InlinePath r

            args

        match List.tryItem injectGenArgIndex genArgs with
        | None -> fail ()
        | Some genArg ->
            match injectType with
            | Types.icomparerGeneric -> args @ [ makeComparer com ctx genArg ]
            | Types.iequalityComparerGeneric ->
                args @ [ makeEqualityComparer com ctx genArg ]
            | Types.arrayCons ->
                match genArg with
                // We don't have a module for ResizeArray so let's assume the kind is MutableArray
                | TypedArrayCompatible com MutableArray consName ->
                    let cons = [ makeImportLib com Any consName "types" ]
                    args @ cons
                | _ ->
                    let cons =
                        [
                            Expr.Value(
                                ValueKind.NewOption(None, genArg, false),
                                None
                            )
                        ]

                    args @ cons
            | Types.adder -> args @ [ makeGenericAdder com ctx genArg ]
            | Types.averager -> args @ [ makeGenericAverager com ctx genArg ]
            | _ -> fail ()

    Map.tryFind moduleName ReplacementsInject.fableReplacementsModules
    |> Option.bind (Map.tryFind methName)
    |> function
        | None -> args
        | Some injectInfo -> injectArgInner args injectInfo

let tryEntityIdent (com: Compiler) entFullName =
    match entFullName with
    | BuiltinDefinition BclDateOnly
    | BuiltinDefinition BclDateTime
    | BuiltinDefinition BclDateTimeOffset -> makeIdentExpr "Date" |> Some
    | BuiltinDefinition BclTimer ->
        makeImportLib com Any "default" "Timer" |> Some
    | BuiltinDefinition(FSharpReference _) ->
        makeImportLib com Any "FSharpRef" "Types" |> Some
    | BuiltinDefinition(FSharpResult _) ->
        makeImportLib com Any "FSharpResult_2" "Choice" |> Some
    | BuiltinDefinition(FSharpChoice genArgs) ->
        let membName = $"FSharpChoice_{List.length genArgs}"
        makeImportLib com Any membName "Choice" |> Some
    // | BuiltinDefinition BclGuid -> jsTypeof "string" expr
    // | BuiltinDefinition BclTimeSpan -> jsTypeof "number" expr
    // | BuiltinDefinition BclHashSet _ -> fail "MutableSet" // TODO:
    // | BuiltinDefinition BclDictionary _ -> fail "MutableMap" // TODO:
    // | BuiltinDefinition BclKeyValuePair _ -> fail "KeyValuePair" // TODO:
    // | BuiltinDefinition FSharpSet _ -> fail "Set" // TODO:
    // | BuiltinDefinition FSharpMap _ -> fail "Map" // TODO:
    | Types.matchFail ->
        makeImportLib com Any "MatchFailureException" "Types" |> Some
    | Types.exception_ -> makeIdentExpr "Exception" |> Some
    | Types.systemException ->
        makeImportLib com Any "SystemException" "SystemException" |> Some
    | Types.timeoutException ->
        makeImportLib com Any "TimeoutException" "SystemException" |> Some
    | _ -> None

let tryConstructor com (ent: Entity) =
    if FSharp2Fable.Util.isReplacementCandidate ent.Ref then
        tryEntityIdent com ent.FullName
    else
        FSharp2Fable.Util.tryEntityIdentMaybeGlobalOrImported com ent

let constructor com ent =
    match tryConstructor com ent with
    | Some e -> e
    | None ->
        ent.FullName
        |> sprintf "Cannot find %s constructor"
        |> addErrorAndReturnNull com [] None

let tryOp com r t op args =
    Helper.LibCall(com, "option", "tryOp", t, op :: args, ?loc = r)

let tryCoreOp com r t coreModule coreMember args =
    let op = Helper.LibValue(com, coreModule, coreMember, Any)
    tryOp com r t op args

let emptyGuid () =
    makeStrConst "00000000-0000-0000-0000-000000000000"

let rec defaultof (com: ICompiler) ctx r t =
    match t with
    | Tuple(args, true) ->
        NewTuple(args |> List.map (defaultof com ctx r), true) |> makeValue None
    | Boolean
    | Number _
    | Builtin BclTimeSpan
    | Builtin BclDateTime
    | Builtin BclDateTimeOffset -> getZero com ctx t
    | Builtin BclGuid -> emptyGuid ()
    | DeclaredType(ent, _) ->
        let ent = com.GetEntity(ent)
        // TODO: For BCL types we cannot access the constructor, raise error or warning?
        if ent.IsValueType then
            tryConstructor com ent
        else
            None
        |> Option.map (fun e -> Helper.ConstructorCall(e, t, []))
        |> Option.defaultWith (fun () -> Null t |> makeValue None)
    // TODO: Fail (or raise warning) if this is an unresolved generic parameter?
    | _ -> Null t |> makeValue None

let fableCoreLib
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.DeclaringEntityFullName, i.CompiledName with
    | _, UniversalFableCoreHelpers com ctx r t i args error expr -> Some expr
    | "Fable.Core.Testing.Assert", _ ->
        match i.CompiledName with
        | "AreEqual" ->
            Helper.LibCall(com, "util", "assertEqual", t, args, ?loc = r)
            |> Some
        | "NotEqual" ->
            Helper.LibCall(com, "util", "assertNotEqual", t, args, ?loc = r)
            |> Some
        | _ -> None
    | "Fable.Core.Reflection", meth ->
        Helper.LibCall(com, "reflection", meth, t, args, ?loc = r) |> Some
    | "Fable.Core.Compiler", meth ->
        match meth with
        | "version" -> makeStrConst Literals.VERSION |> Some
        | "majorMinorVersion" ->
            try
                let m =
                    System.Text.RegularExpressions.Regex.Match(
                        Literals.VERSION,
                        @"^\d+\.\d+"
                    )

                float m.Value |> makeFloatConst |> Some
            with _ ->
                "Cannot parse compiler version"
                |> addErrorAndReturnNull com ctx.InlinePath r
                |> Some
        | "debugMode" -> makeBoolConst com.Options.DebugMode |> Some
        | "typedArrays" -> makeBoolConst com.Options.TypedArrays |> Some
        | "extension" -> makeStrConst com.Options.FileExtension |> Some
        | _ -> None
    | "Fable.Core.Py", ("python" | "expr_python" as meth) ->
        let isStatement = meth <> "expr_python"

        match args with
        | RequireStringConstOrTemplate com ctx r template :: _ ->
            emitTemplate r t [] isStatement template |> Some
        | _ -> None
    | "Fable.Core.PyInterop", _ ->
        match i.CompiledName, args with
        | Naming.StartsWith "import" suffix, _ ->
            match suffix, args with
            | "Member", [ RequireStringConst com ctx r path ] ->
                makeImportUserGenerated r t Naming.placeholder path |> Some
            | "Default", [ RequireStringConst com ctx r path ] ->
                makeImportUserGenerated r t "default" path |> Some
            | "SideEffects", [ RequireStringConst com ctx r path ] ->
                makeImportUserGenerated r t "" path |> Some
            | "All", [ RequireStringConst com ctx r path ] ->
                makeImportUserGenerated r t "*" path |> Some
            | _,
              [ RequireStringConst com ctx r selector
                RequireStringConst com ctx r path ] ->
                makeImportUserGenerated r t selector path |> Some
            | _ -> None
        // Dynamic casting, erase
        | "op_BangHat", [ arg ] -> Some arg
        | "op_BangBang", [ arg ] ->
            match arg, i.GenericArgs with
            | IsNewAnonymousRecord(_, exprs, fieldNames, _, _, _),
              [ _; DeclaredType(ent, []) ] ->
                let ent = com.GetEntity(ent)

                if ent.IsInterface then
                    AnonRecords.fitsInInterface com r exprs fieldNames ent
                    |> function
                        | Error errors ->
                            errors
                            |> List.iter (fun (range, error) ->
                                addWarning com ctx.InlinePath range error
                            )

                            Some arg
                        | Ok() -> Some arg
                else
                    Some arg
            | _ -> Some arg
        | "op_Dynamic", [ left; memb ] -> getExpr r t left memb |> Some
        | "op_DynamicAssignment",
          [ callee; prop; MaybeLambdaUncurriedAtCompileTime value ] ->
            setExpr r callee prop value |> Some
        | ("op_Dollar" | "createNew" as m), callee :: args ->
            let args = destructureTupleArgs args

            if m = "createNew" then
                "new $0($1...)"
            else
                "$0($1...)"
            |> emitExpr r t (callee :: args)
            |> Some
        | Naming.StartsWith "emitPy" rest, [ args; macro ] ->
            match macro with
            | RequireStringConstOrTemplate com ctx r template ->
                let args = destructureTupleArgs [ args ]
                let isStatement = rest = "Statement"
                emitTemplate r t args isStatement template |> Some
        | "op_EqualsEqualsGreater",
          [ name; MaybeLambdaUncurriedAtCompileTime value ] ->
            makeTuple
                r
                false
                [
                    name
                    value
                ]
            |> Some
        | "createObj", _ ->
            Helper.LibCall(com, "util", "createObj", Any, args)
            |> withTag "pojo"
            |> Some
        | "keyValueList", [ caseRule; keyValueList ] ->
            // makePojo com ctx caseRule keyValueList
            let args =
                [
                    keyValueList
                    caseRule
                ]

            Helper.LibCall(com, "map_util", "keyValueList", Any, args)
            |> withTag "pojo"
            |> Some
        | "createEmpty", _ -> typedObjExpr t [] |> Some
        | _ -> None
    | _ -> None

let refCells
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | "get_Value", Some callee, _ -> getRefCell com r t callee |> Some
    | "set_Value", Some callee, [ value ] ->
        setRefCell com r callee value |> Some
    | _ -> None

let getMangledNames (i: CallInfo) (thisArg: Expr option) =
    let isStatic = Option.isNone thisArg
    let pos = i.DeclaringEntityFullName.LastIndexOf('.')

    let moduleName =
        i.DeclaringEntityFullName.Substring(0, pos).Replace("Microsoft.", "")

    let entityName =
        i.DeclaringEntityFullName.Substring(pos + 1)
        |> Naming.cleanNameAsPyIdentifier

    let memberName = i.CompiledName |> Naming.cleanNameAsPyIdentifier

    let mangledName =
        Naming.buildNameWithoutSanitationFrom
            entityName
            isStatic
            memberName
            i.OverloadSuffix

    moduleName, mangledName

let bclType
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let moduleName, mangledName = getMangledNames i thisArg

    let args =
        match thisArg with
        | Some callee -> callee :: args
        | _ -> args

    Helper.LibCall(
        com,
        moduleName,
        mangledName,
        t,
        args,
        i.SignatureArgTypes,
        ?loc = r
    )
    |> Some

let fsharpModule
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let moduleName, mangledName = getMangledNames i thisArg

    Helper.LibCall(
        com,
        moduleName,
        mangledName,
        t,
        args,
        i.SignatureArgTypes,
        ?loc = r
    )
    |> Some

// TODO: This is likely broken
let getPrecompiledLibMangledName entityName memberName overloadSuffix isStatic =
    let memberName = Naming.sanitizeIdentForbiddenChars memberName
    let entityName = Naming.sanitizeIdentForbiddenChars entityName

    let name, memberPart =
        match entityName, isStatic with
        | "", _ -> memberName, Naming.NoMemberPart
        | _, true ->
            entityName, Naming.StaticMemberPart(memberName, overloadSuffix)
        | _, false ->
            entityName, Naming.InstanceMemberPart(memberName, overloadSuffix)

    Naming.buildNameWithoutSanitation name memberPart |> Naming.checkJsKeywords

let fsFormat
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | "get_Value", Some callee, _ -> getFieldWith None t callee "input" |> Some
    | "PrintFormatToStringThen", _, _ ->
        match args with
        | [ _ ] ->
            Helper.LibCall(
                com,
                "string",
                "toText",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | [ cont; fmt ] -> Helper.InstanceCall(fmt, "cont", t, [ cont ]) |> Some
        | _ -> None
    | "PrintFormatToString", _, _ ->
        match args with
        | [ template ] when template.Type = String -> Some template
        | _ ->
            Helper.LibCall(
                com,
                "string",
                "toText",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
    | "PrintFormatLine", _, _ ->
        Helper.LibCall(
            com,
            "string",
            "toConsole",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("PrintFormatToError" | "PrintFormatLineToError"), _, _ ->
        // addWarning com ctx.FileName r "eprintf will behave as eprintfn"
        Helper.LibCall(
            com,
            "string",
            "toConsoleError",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("PrintFormatToTextWriter" | "PrintFormatLineToTextWriter"), _, _ :: args ->
        // addWarning com ctx.FileName r "fprintfn will behave as printfn"
        Helper.LibCall(
            com,
            "string",
            "toConsole",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "PrintFormat", _, _ ->
        // addWarning com ctx.FileName r "Printf will behave as printfn"
        Helper.LibCall(
            com,
            "string",
            "toConsole",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "PrintFormatThen", _, arg :: callee :: _ ->
        Helper.InstanceCall(callee, "cont", t, [ arg ]) |> Some
    | "PrintFormatToStringThenFail", _, _ ->
        Helper.LibCall(
            com,
            "string",
            "toFail",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("PrintFormatToStringBuilder" | "PrintFormatToStringBuilderThen"), // bprintf
      _,
      _ -> fsharpModule com ctx r t i thisArg args
    | ".ctor",
      _,
      str :: (Value(NewArray(ArrayValues templateArgs, _, _), _) as values) :: _ ->
        match
            makeStringTemplateFrom
                [|
                    "%s"
                    "%i"
                |]
                templateArgs
                str
        with
        | Some v -> makeValue r v |> Some
        | None ->
            Helper.LibCall(
                com,
                "string",
                "interpolate",
                t,
                [
                    str
                    values
                ],
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
    | ".ctor", _, arg :: _ ->
        Helper.LibCall(
            com,
            "string",
            "printf",
            t,
            [ arg ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let operators
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let math r t (args: Expr list) argTypes methName =
        let meth = Naming.lowerFirst methName
        Helper.ImportedCall("math", meth, t, args, argTypes, ?loc = r)

    match i.CompiledName, args with
    | ("DefaultArg" | "DefaultValueArg"), [ opt; defValue ] ->
        match opt with
        | MaybeInScope ctx (Value(NewOption(opt, _, _), _)) ->
            match opt with
            | Some value -> Some value
            | None -> Some defValue
        | _ ->
            Helper.LibCall(
                com,
                "option",
                "defaultArg",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
    | "DefaultAsyncBuilder", _ ->
        makeImportLib com t "singleton" "async_builder" |> Some
    // Erased operators.
    // KeyValuePair is already compiled as a tuple
    | ("KeyValuePattern" | "Identity" | "Box" | "Unbox" | "ToEnum"), [ arg ] ->
        TypeCast(arg, t) |> Some
    // Cast to unit to make sure nothing is returned when wrapped in a lambda, see #1360
    | "Ignore", _ ->
        Helper.LibCall(
            com,
            "util",
            "ignore",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

    // Number and String conversions
    | ("ToSByte" | "ToByte" | "ToInt8" | "ToUInt8" | "ToInt16" | "ToUInt16" | "ToInt" | "ToUInt" | "ToInt32" | "ToUInt32"),
      _ -> toInt com ctx r t args |> Some
    | "ToInt64", _ -> toLong com ctx r false t args |> Some
    | "ToUInt64", _ -> toLong com ctx r true t args |> Some
    | ("ToSingle" | "ToDouble"), _ -> toFloat com ctx r t args |> Some
    | "ToDecimal", _ -> toDecimal com ctx r t args |> Some
    | "ToChar", _ -> toChar args.Head |> Some
    | "ToString", _ -> toString com ctx r args |> Some
    | "CreateSequence", [ xs ] -> toSeq t xs |> Some
    | ("CreateDictionary" | "CreateReadOnlyDictionary"), [ arg ] ->
        makeDictionary com ctx r t arg |> Some
    | "CreateSet", _ ->
        (genArg com ctx r 0 i.GenericArgs)
        |> makeSet com ctx r t "OfSeq" args
        |> Some
    // Ranges
    | ("op_Range" | "op_RangeStep"), _ ->
        let genArg = genArg com ctx r 0 i.GenericArgs

        let addStep args =
            match args with
            | [ first; last ] ->
                [
                    first
                    getOne com ctx genArg
                    last
                ]
            | _ -> args

        let modul, meth, args =
            match genArg with
            | Char -> "Range", "rangeChar", args
            | Number(Decimal, _) -> "Range", "rangeDecimal", addStep args
            | Number(BigInt, _)
            | Number(Int32, _)
            | Number(UInt32, _) -> "Range", "range_big_int", addStep args
            | Number(Int64, _)
            | Number(UInt64, _) -> "Range", "range_int64", addStep args
            | _ -> "Range", "rangeDouble", addStep args

        Helper.LibCall(com, modul, meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    // Pipes and composition
    | "op_PipeRight", [ x; f ]
    | "op_PipeLeft", [ f; x ] -> curriedApply r t f [ x ] |> Some
    | "op_PipeRight2", [ x; y; f ]
    | "op_PipeLeft2", [ f; x; y ] ->
        curriedApply
            r
            t
            f
            [
                x
                y
            ]
        |> Some
    | "op_PipeRight3", [ x; y; z; f ]
    | "op_PipeLeft3", [ f; x; y; z ] ->
        curriedApply
            r
            t
            f
            [
                x
                y
                z
            ]
        |> Some
    | "op_ComposeRight", [ f1; f2 ] -> compose com ctx r t f1 f2 |> Some
    | "op_ComposeLeft", [ f2; f1 ] -> compose com ctx r t f1 f2 |> Some
    // Strings
    | ("PrintFormatToString" | "PrintFormatToStringThen" | "PrintFormat" | "PrintFormatLine" | "PrintFormatToError" | "PrintFormatLineToError" | "PrintFormatThen" | "PrintFormatToStringThenFail" | "PrintFormatToStringBuilder" | "PrintFormatToStringBuilderThen"), // bprintf
      _ -> fsFormat com ctx r t i thisArg args
    | ("Failure" | "FailurePattern" | "LazyPattern" | "NullArg" | "Using"), // nullArg
      _ -> fsharpModule com ctx r t i thisArg args
    | "Lock", _ -> // lock
        Helper.LibCall(
            com,
            "util",
            "lock",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    // Exceptions
    | "FailWith", [ msg ]
    | "InvalidOp", [ msg ] -> makeThrow r t (error msg) |> Some
    | "InvalidArg", [ argName; msg ] ->
        let msg = add (add msg (str "\\nParameter name: ")) argName
        makeThrow r t (error msg) |> Some
    | "Raise", [ arg ] -> makeThrow r t arg |> Some
    | "Reraise", _ ->
        match ctx.CaughtException with
        | Some ex -> makeThrow r t (IdentExpr ex) |> Some
        | None ->
            "`reraise` used in context where caught exception is not available, please report"
            |> addError com ctx.InlinePath r

            makeThrow r t (error (str "")) |> Some
    // Math functions
    // TODO: optimize square pow: x * x
    | "Pow", _
    | "PowInteger", _
    | "op_Exponentiation", _ ->
        let argTypes = args |> List.map (fun a -> a.Type)

        match argTypes with
        | Number(Decimal, _) :: _ ->
            Helper.LibCall(
                com,
                "decimal",
                "pow",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | CustomOp com ctx r t "Pow" args e -> Some e
        | _ -> math r t args i.SignatureArgTypes "pow" |> Some
    | ("Ceiling" | "Floor" as meth), _ ->
        let meth = Naming.lowerFirst meth

        match args with
        | ExprType(Number(Decimal, _)) :: _ ->
            Helper.LibCall(
                com,
                "decimal",
                meth,
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ ->
            let meth =
                if meth = "ceiling" then
                    "ceil"
                else
                    meth

            math r t args i.SignatureArgTypes meth |> Some
    | "Log", [ arg1; arg2 ] ->
        // "Math.log($0) / Math.log($1)"
        let dividend =
            math None t [ arg1 ] (List.take 1 i.SignatureArgTypes) "log"

        let divisor =
            math None t [ arg2 ] (List.skip 1 i.SignatureArgTypes) "log"

        makeBinOp r t dividend divisor BinaryDivide |> Some
    | "Abs", _ -> Helper.GlobalCall("abs", t, args, [ t ], ?loc = r) |> Some
    | "Acos", _
    | "Asin", _
    | "Atan", _
    | "Atan2", _
    | "Cos", _
    | "Cosh", _
    | "Exp", _
    | "Log10", _
    | "Sin", _
    | "Sinh", _
    | "Tan", _
    | "Tanh", _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    | "Log", _
    | "Sqrt", _ ->
        Helper.LibCall(
            com,
            "double",
            i.CompiledName.ToLower(),
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | "Round", _ ->
        match args with
        | ExprType(Number(Decimal, _)) :: _ ->
            Helper.LibCall(
                com,
                "decimal",
                "round",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ ->
            Helper.LibCall(
                com,
                "util",
                "round",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
    | "Truncate", _ ->
        match args with
        | ExprType(Number(Decimal, _)) :: _ ->
            Helper.LibCall(
                com,
                "decimal",
                "truncate",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ ->
            Helper.ImportedCall(
                "math",
                "trunc",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
    | "Sign", _ ->
        match args with
        | ExprType(Number(Decimal, _)) :: _ ->
            Helper.LibCall(
                com,
                "decimal",
                "sign",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | ExprType(Number(BigInt, _)) :: _ ->
            Helper.LibCall(
                com,
                "big_int",
                "sign",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | ExprType(Number((Float16 | Float32 | Float64), _)) :: _ ->
            Helper.LibCall(
                com,
                "double",
                "sign",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | ExprType(Number(_)) :: _ ->
            Helper.LibCall(
                com,
                "long",
                "sign",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ -> None
    // Numbers
    | ("Infinity" | "InfinitySingle"), _ ->
        Helper.ImportedValue(com, "math", "inf", t) |> Some
    | ("NaN" | "NaNSingle"), _ ->
        Helper.ImportedValue(com, "math", "nan", t) |> Some
    | "Fst", [ tup ] -> Get(tup, TupleIndex 0, t, r) |> Some
    | "Snd", [ tup ] -> Get(tup, TupleIndex 1, t, r) |> Some
    // Reference
    | "op_Dereference", [ arg ] -> getRefCell com r t arg |> Some
    | "op_ColonEquals", [ o; v ] -> setRefCell com r o v |> Some
    | "Ref", [ arg ] -> makeRefCellFromValue com r arg |> Some
    | ("Increment" | "Decrement"), _ ->
        if i.CompiledName = "Increment" then
            "$0.contents +=1"
        else
            "$0.contents -=1"
        |> emitExpr r t args
        |> Some
    // Concatenates two lists
    | "op_Append", _ ->
        Helper.LibCall(
            com,
            "list",
            "append",
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | (Operators.inequality | "Neq"), [ left; right ] ->
        equals com ctx r false left right |> Some
    | (Operators.equality | "Eq"), [ left; right ] ->
        equals com ctx r true left right |> Some
    | "IsNull", [ arg ] -> nullCheck r true arg |> Some
    | "Hash", [ arg ] -> structuralHash com r arg |> Some
    // Comparison
    | "Compare", [ left; right ] -> compare com ctx r left right |> Some
    | (Operators.lessThan | "Lt"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryLess |> Some
    | (Operators.lessThanOrEqual | "Lte"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryLessOrEqual |> Some
    | (Operators.greaterThan | "Gt"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryGreater |> Some
    | (Operators.greaterThanOrEqual | "Gte"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryGreaterOrEqual |> Some
    | ("Min" | "Max" | "Clamp" as meth), _ ->
        let f = makeComparerFunction com ctx t

        Helper.LibCall(
            com,
            "util",
            Naming.lowerFirst meth,
            t,
            f :: args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "Not", [ operand ] -> // TODO: Check custom operator?
        makeUnOp r t operand UnaryNot |> Some
    | Patterns.SetContains Operators.standardSet, _ ->
        applyOp com ctx r t i.CompiledName args |> Some
    // Type info
    | "TypeOf", _ ->
        (genArg com ctx r 0 i.GenericArgs) |> makeTypeInfo r |> Some
    | "TypeDefOf", _ ->
        (genArg com ctx r 0 i.GenericArgs) |> makeTypeDefinitionInfo r |> Some
    | _ -> None

let chars
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    let icall r t args argTypes memb =
        match args, argTypes with
        | thisArg :: args, _ :: argTypes ->
            let info = makeCallInfo None args argTypes

            getField thisArg memb |> makeCall r t info |> Some
        | _ -> None

    match i.CompiledName with
    | "ToUpper" -> icall r t args i.SignatureArgTypes "upper"
    | "ToUpperInvariant" -> icall r t args i.SignatureArgTypes "upper"
    | "ToLower" -> icall r t args i.SignatureArgTypes "lower"
    | "ToLowerInvariant" -> icall r t args i.SignatureArgTypes "lower"
    | "ToString" -> toString com ctx r args |> Some
    | "GetUnicodeCategory"
    | "IsControl"
    | "IsDigit"
    | "IsLetter"
    | "IsLetterOrDigit"
    | "IsUpper"
    | "IsLower"
    | "IsNumber"
    | "IsPunctuation"
    | "IsSeparator"
    | "IsSymbol"
    | "IsWhiteSpace"
    | "IsHighSurrogate"
    | "IsLowSurrogate"
    | "IsSurrogate" ->
        let methName = Naming.lowerFirst i.CompiledName

        Helper.LibCall(
            com,
            "char",
            methName,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "IsSurrogatePair"
    | "Parse" ->
        let methName = Naming.lowerFirst i.CompiledName

        Helper.LibCall(
            com,
            "char",
            methName,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let implementedStringFunctions =
    set
        [|
            "Compare"
            "CompareTo"
            "EndsWith"
            "Format"
            "IndexOfAny"
            "Insert"
            "IsNullOrEmpty"
            "IsNullOrWhiteSpace"
            "PadLeft"
            "PadRight"
            "Remove"
            "Replace"
            "Substring"
        |]

let getEnumerator com r t expr =
    Helper.LibCall(
        com,
        "util",
        "getEnumerator",
        t,
        [ toSeq Any expr ],
        ?loc = r
    )

let strings
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, fstArg :: _ ->
        match fstArg.Type with
        | Char ->
            match args with
            | [ _; _ ] -> emitExpr r t args "$0 * $1" |> Some // String(char, int)
            | _ ->
                "Unexpected arguments in System.String constructor."
                |> addErrorAndReturnNull com ctx.InlinePath r
                |> Some
        | Array _ ->
            match args with
            | [ _ ] -> emitExpr r t args "''.join($0)" |> Some // String(char[])
            | [ _; _; _ ] -> emitExpr r t args "''.join($0)[$1:$2+1]" |> Some // String(char[], int, int)
            | _ ->
                "Unexpected arguments in System.String constructor."
                |> addErrorAndReturnNull com ctx.InlinePath r
                |> Some
        | _ -> fsFormat com ctx r t i thisArg args
    | "get_Length", Some c, _ ->
        Helper.GlobalCall("len", t, [ c ], [ t ], ?loc = r) |> Some
    | "get_Chars", Some c, _ ->
        Helper.LibCall(
            com,
            "string",
            "getCharAtIndex",
            t,
            args,
            i.SignatureArgTypes,
            thisArg = c,
            ?loc = r
        )
        |> Some
    | "Equals", Some x, [ y ]
    | "Equals", None, [ x; y ] -> makeEqOp r x y BinaryEqual |> Some
    | "Equals", Some x, [ y; kind ]
    | "Equals", None, [ x; y; kind ] ->
        let left =
            Helper.LibCall(
                com,
                "string",
                "compare",
                Int32.Number,
                [
                    x
                    y
                    kind
                ]
            )

        makeEqOp r left (makeIntConst 0) BinaryEqual |> Some
    | "GetEnumerator", Some c, _ -> getEnumerator com r t c |> Some
    | "Contains", Some c, arg :: _ ->
        if (List.length args) > 1 then
            addWarning
                com
                ctx.InlinePath
                r
                "String.Contains: second argument is ignored"

        let left = Helper.InstanceCall(c, "find", Int32.Number, [ arg ])

        makeEqOp r left (makeIntConst 0) BinaryGreaterOrEqual |> Some
    | "StartsWith", Some c, [ _str ] ->
        let left = Helper.InstanceCall(c, "find", Int32.Number, args)

        makeEqOp r left (makeIntConst 0) BinaryEqual |> Some
    | "StartsWith", Some c, [ _str; _comp ] ->
        Helper.LibCall(
            com,
            "string",
            "startsWith",
            t,
            args,
            i.SignatureArgTypes,
            thisArg = c,
            ?loc = r
        )
        |> Some
    | ReplaceName [ "ToUpper", "upper"
                    "ToUpperInvariant", "upper"
                    "ToLower", "lower"
                    "ToLowerInvariant", "lower" ] methName,
      Some c,
      args ->
        Helper.InstanceCall(c, methName, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | "IndexOf", Some c, _ ->
        match args with
        | [ ExprType Char ]
        | [ ExprType String ]
        | [ ExprType Char; ExprType(Number(Int32, NumberInfo.Empty)) ]
        | [ ExprType String; ExprType(Number(Int32, NumberInfo.Empty)) ] ->
            Helper.InstanceCall(
                c,
                "find",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ ->
            "The only extra argument accepted for String.IndexOf/LastIndexOf is startIndex."
            |> addErrorAndReturnNull com ctx.InlinePath r
            |> Some
    | "LastIndexOf", Some c, _ ->
        match args with
        | [ ExprType Char ]
        | [ ExprType String ] ->
            Helper.InstanceCall(
                c,
                "rfind",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | [ ExprType Char as str
            ExprType(Number(Int32, NumberInfo.Empty)) as start ]
        | [ ExprType String as str
            ExprType(Number(Int32, NumberInfo.Empty)) as start ] ->
            Helper.InstanceCall(
                c,
                "rfind",
                t,
                [
                    str
                    Value(NumberConstant(0, Int32, NumberInfo.Empty), None)
                    start
                ],
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ ->
            "The only extra argument accepted for String.IndexOf/LastIndexOf is startIndex."
            |> addErrorAndReturnNull com ctx.InlinePath r
            |> Some
    | ("Trim" | "TrimStart" | "TrimEnd"), Some c, _ ->
        let methName =
            match i.CompiledName with
            | "TrimStart" -> "lstrip"
            | "TrimEnd" -> "rstrip"
            | _ -> "strip"

        match args with
        | [] ->
            Helper.InstanceCall(
                c,
                methName,
                t,
                [],
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | head :: tail ->
            let spread =
                match head.Type, tail with
                | Array _, [] -> true
                | _ -> false

            Helper.LibCall(
                com,
                "string",
                Naming.lowerFirst i.CompiledName,
                t,
                c :: args,
                hasSpread = spread,
                ?loc = r
            )
            |> Some
    | "ToCharArray", Some c, _ -> stringToCharArray t c |> Some
    | "Split", Some c, _ ->
        match args with
        // Optimization
        | [] -> Helper.InstanceCall(c, "split", t, [ makeStrConst "" ]) |> Some
        | [ Value(CharConstant _, _) as separator ]
        | [ StringConst _ as separator ]
        | [ Value(NewArray(ArrayValues [ separator ], _, _), _) ] ->
            Helper.InstanceCall(c, "split", t, [ separator ]) |> Some
        | [ arg1; ExprType(Number(_, NumberInfo.IsEnum _)) as arg2 ] ->
            let arg1 =
                match arg1.Type with
                | Array _ -> arg1
                | _ ->
                    Value(
                        NewArray(ArrayValues [ arg1 ], String, MutableArray),
                        None
                    )

            let args =
                [
                    arg1
                    Value(Null Any, None)
                    arg2
                ]

            Helper.LibCall(com, "string", "split", t, c :: args, ?loc = r)
            |> Some
        | args ->
            Helper.LibCall(
                com,
                "string",
                "split",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
    | "Join", None, _ ->
        let methName =
            match i.SignatureArgTypes with
            | [ _; Array _; Number _; Number _ ] -> "joinWithIndices"
            | _ -> "join"

        Helper.LibCall(com, "string", methName, t, args, ?loc = r) |> Some
    | "Concat", None, _ ->
        match i.SignatureArgTypes with
        | [ Array _ | IEnumerable ] ->
            Helper.LibCall(
                com,
                "string",
                "join",
                t,
                ((makeStrConst "") :: args),
                ?loc = r
            )
            |> Some
        | _ ->
            Helper.LibCall(
                com,
                "string",
                "concat",
                t,
                args,
                hasSpread = true,
                ?loc = r
            )
            |> Some
    | "CompareOrdinal", None, _ ->
        Helper.LibCall(com, "string", "compareOrdinal", t, args, ?loc = r)
        |> Some
    | Patterns.SetContains implementedStringFunctions, thisArg, args ->
        Helper.LibCall(
            com,
            "string",
            Naming.lowerFirst i.CompiledName,
            t,
            args,
            i.SignatureArgTypes,
            hasSpread = i.HasSpread,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | _ -> None

let stringModule
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, args with
    | "Length", [ arg ] ->
        Helper.GlobalCall("len", t, [ arg ], [ t ], ?loc = r) |> Some
    | ("Iterate" | "IterateIndexed" | "ForAll" | "Exists"), _ ->
        // Cast the string to char[], see #1279
        let args =
            args |> List.replaceLast (fun e -> stringToCharArray e.Type e)

        Helper.LibCall(
            com,
            "seq",
            Naming.lowerFirst i.CompiledName,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("Map" | "MapIndexed" | "Collect"), _ ->
        // Cast the string to char[], see #1279
        let args =
            args |> List.replaceLast (fun e -> stringToCharArray e.Type e)

        let name = Naming.lowerFirst i.CompiledName

        emitExpr
            r
            t
            [ Helper.LibCall(com, "seq", name, Any, args, i.SignatureArgTypes) ]
            "''.join(list($0))"
        |> Some
    | "Concat", _ ->
        Helper.LibCall(com, "string", "join", t, args, ?loc = r) |> Some
    // Rest of StringModule methods
    | meth, args ->
        Helper.LibCall(
            com,
            "string",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let formattableString
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | "Create", None, [ str; args ] ->
        objExpr
            [
                "str", str
                "args", args
            ]
        |> Some
    | "get_Format", Some x, _ -> getFieldWith r t x "str" |> Some
    | "get_ArgumentCount", Some x, _ ->
        Helper.GlobalCall("len", t, [ getField x "args" ], [ t ], ?loc = r)
        |> Some
    | "GetArgument", Some x, [ idx ] ->
        getExpr r t (getField x "args") idx |> Some
    | "GetArguments", Some x, [] -> getFieldWith r t x "args" |> Some
    | _ -> None

let seqModule
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, args with
    | "Cast", [ arg ] -> Some arg // Erase
    | "CreateEvent", [ addHandler; removeHandler; createHandler ] ->
        Helper.LibCall(
            com,
            "event",
            "createEvent",
            t,
            [
                addHandler
                removeHandler
            ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth, args ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq2" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "seq2",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "seq",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some

let injectIndexOfArgs com ctx r genArgs args =
    let args =
        match args with
        | [ ar; item; start; count ] ->
            [
                ar
                item
                start
                count
            ]
        | [ ar; item; start ] ->
            [
                ar
                item
                start
                makeNone (Int32.Number)
            ]
        | [ ar; item ] ->
            [
                ar
                item
                makeNone (Int32.Number)
                makeNone (Int32.Number)
            ]
        | _ -> failwith "Unexpected number of arguments"

    injectArg com ctx r "Array" "indexOf" genArgs args

let resizeArrays
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, [] -> makeResizeArray (getElementType t) [] |> Some
    // Don't pass the size to `new Array()` because that would fill the array with null values
    | ".ctor", _, [ ExprType(Number _) ] ->
        makeResizeArray (getElementType t) [] |> Some
    // Optimize expressions like `ResizeArray [|1|]` or `ResizeArray [1]`
    | ".ctor", _, [ ArrayOrListLiteral(vals, _) ] ->
        makeResizeArray (getElementType t) vals |> Some
    | ".ctor", _, args ->
        Helper.GlobalCall("list", t, args, ?loc = r) |> withTag "array" |> Some
    | "get_Item", Some ar, [ idx ] -> getExpr r t ar idx |> Some
    | "set_Item", Some ar, [ idx; value ] -> setExpr r ar idx value |> Some
    | "Add", Some ar, [ arg ] ->
        "void ($0)"
        |> emitExpr r t [ Helper.InstanceCall(ar, "append", t, [ arg ]) ]
        |> Some
    | "Remove", Some ar, [ arg ] ->
        let args =
            injectArg
                com
                ctx
                r
                "Array"
                "removeInPlace"
                i.GenericArgs
                [
                    arg
                    ar
                ]

        Helper.LibCall(com, "array", "removeInPlace", t, args, ?loc = r) |> Some
    | "RemoveAll", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "array",
            "removeAllInPlace",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "FindIndex", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "resize_array",
            "find_index",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "FindLastIndex", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "array",
            "findLastIndex",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "ForEach", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "array",
            "iterate",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "GetEnumerator", Some ar, _ -> getEnumerator com r t ar |> Some
    // ICollection members, implemented in dictionaries and sets too. We need runtime checks (see #1120)
    | "get_Count", Some(MaybeCasted(ar)), _ ->
        match ar.Type with
        // Fable translates System.Collections.Generic.List as Array
        // TODO: Check also IList?
        | Array _ ->
            Helper.GlobalCall("len", t, [ ar ], [ t ], ?loc = r) |> Some
        | _ -> Helper.LibCall(com, "util", "count", t, [ ar ], ?loc = r) |> Some
    | "Clear", Some ar, _ ->
        Helper.LibCall(com, "Util", "clear", t, [ ar ], ?loc = r) |> Some
    | "Find", Some ar, [ arg ] ->
        let opt =
            Helper.LibCall(
                com,
                "array",
                "tryFind",
                t,
                [
                    arg
                    ar
                ],
                ?loc = r
            )

        Helper.LibCall(
            com,
            "Option",
            "defaultArg",
            t,
            [
                opt
                defaultof com ctx r t
            ],
            ?loc = r
        )
        |> Some
    | "Exists", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "resize_array",
            "exists",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "FindLast", Some ar, [ arg ] ->
        let opt =
            Helper.LibCall(
                com,
                "array",
                "tryFindBack",
                t,
                [
                    arg
                    ar
                ],
                ?loc = r
            )

        Helper.LibCall(
            com,
            "Option",
            "defaultArg",
            t,
            [
                opt
                defaultof com ctx r t
            ],
            ?loc = r
        )
        |> Some
    | "FindAll", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "Array",
            "filter",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "AddRange", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "Array",
            "addRangeInPlace",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "GetRange", Some ar, [ idx; cnt ] ->
        Helper.LibCall(
            com,
            "Array",
            "getSubArray",
            t,
            [
                ar
                idx
                cnt
            ],
            ?loc = r
        )
        |> Some
    | "Contains", Some(MaybeCasted(ar)), [ arg ] ->
        // emitExpr r t [ ar; arg ] "$1 in $0" |> Some
        let args =
            injectArg
                com
                ctx
                r
                "Array"
                "contains"
                i.GenericArgs
                [
                    arg
                    ar
                ]

        let moduleName =
            match ar.Type with
            | Array _ -> "array"
            | _ -> "seq"

        Helper.LibCall(com, moduleName, "contains", t, args, ?loc = r) |> Some
    | "IndexOf", Some ar, args ->
        let args = injectIndexOfArgs com ctx r i.GenericArgs (ar :: args)
        Helper.LibCall(com, "array", "index_of", t, args, ?loc = r) |> Some
    | "Insert", Some ar, [ idx; arg ] ->
        Helper.InstanceCall(
            ar,
            "insert",
            t,
            [
                idx
                arg
            ],
            ?loc = r
        )
        |> Some
    | "InsertRange", Some ar, [ idx; arg ] ->
        Helper.LibCall(
            com,
            "array",
            "insert_range_in_place",
            t,
            [
                idx
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "RemoveRange", Some ar, args ->
        Helper.LibCall(
            com,
            "resize_array",
            "remove_range",
            t,
            args @ [ ar ],
            ?loc = r
        )
        |> Some
    | "RemoveAt", Some ar, [ idx ] ->
        Helper.InstanceCall(ar, "pop", t, [ idx ], ?loc = r) |> Some
    | "Reverse", Some ar, [] ->
        Helper.InstanceCall(ar, "reverse", t, args, ?loc = r) |> Some
    | "Sort", Some ar, [] ->
        let compareFn =
            (genArg com ctx r 0 i.GenericArgs) |> makeComparerFunction com ctx

        Helper.InstanceCall(ar, "sort", t, [ compareFn ], ?loc = r) |> Some
    | "Sort", Some ar, [ ExprType(DelegateType _) ] ->
        Helper.InstanceCall(ar, "sort", t, args, ?loc = r) |> Some
    | "Sort", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "array",
            "sortInPlace",
            t,
            [
                ar
                arg
            ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "ToArray", Some ar, [] ->
        Helper.InstanceCall(ar, "to_array", t, args, ?loc = r) |> Some
    | _ -> None

let collectionExtensions
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | "AddRange", None, [ ar; arg ] ->
        Helper.LibCall(
            com,
            "Array",
            "addRangeInPlace",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "InsertRange", None, [ ar; idx; arg ] ->
        Helper.LibCall(
            com,
            "array",
            "insert_range_in_place",
            t,
            [
                idx
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | _ -> None

let readOnlySpans
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, args with
    | "op_Implicit", [ arg ] -> arg |> Some
    | _ -> None

let nativeArrayFunctions =
    dict
        [| //"Exists", "some"
        //"Filter", "filter"
        //"Find", "find"
        //"FindIndex", "index"
        //"ForAll", "all"
        //"Iterate", "forEach"
        //"Reduce", "reduce"
        //"ReduceBack", "reduceRight"
        |]

let tuples
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let changeKind isStruct =
        function
        | Value(NewTuple(args, _), r) :: _ ->
            Value(NewTuple(args, isStruct), r) |> Some
        | ExprType(Tuple(genArgs, _)) as e :: _ ->
            TypeCast(e, Tuple(genArgs, isStruct)) |> Some
        | _ -> None

    match i.CompiledName, thisArg with
    | (".ctor" | "Create"), _ ->
        let isStruct =
            i.DeclaringEntityFullName.StartsWith(
                "System.ValueTuple",
                StringComparison.Ordinal
            )

        Value(NewTuple(args, isStruct), r) |> Some
    | "get_Item1", Some x -> Get(x, TupleIndex 0, t, r) |> Some
    | "get_Item2", Some x -> Get(x, TupleIndex 1, t, r) |> Some
    | "get_Item3", Some x -> Get(x, TupleIndex 2, t, r) |> Some
    | "get_Item4", Some x -> Get(x, TupleIndex 3, t, r) |> Some
    | "get_Item5", Some x -> Get(x, TupleIndex 4, t, r) |> Some
    | "get_Item6", Some x -> Get(x, TupleIndex 5, t, r) |> Some
    | "get_Item7", Some x -> Get(x, TupleIndex 6, t, r) |> Some
    | "get_Rest", Some x -> Get(x, TupleIndex 7, t, r) |> Some
    // System.TupleExtensions
    | "ToValueTuple", _ -> changeKind true args
    | "ToTuple", _ -> changeKind false args
    | _ -> None

let copyToArray (com: ICompiler) r t (i: CallInfo) args =
    Helper.LibCall(
        com,
        "Util",
        "copyToArray",
        t,
        args,
        i.SignatureArgTypes,
        ?loc = r
    )
    |> Some

let arrays
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | "get_Length", Some arg, _ ->
        Helper.GlobalCall("len", t, [ arg ], [ t ], ?loc = r) |> Some
    | "get_Item", Some arg, [ idx ] -> getExpr r t arg idx |> Some
    | "set_Item", Some arg, [ idx; value ] -> setExpr r arg idx value |> Some
    | "Copy", None, [ _source; _sourceIndex; _target; _targetIndex; _count ] ->
        copyToArray com r t i args
    | "Copy", None, [ source; target; count ] ->
        copyToArray
            com
            r
            t
            i
            [
                source
                makeIntConst 0
                target
                makeIntConst 0
                count
            ]
    | "IndexOf", None, args ->
        let args = injectIndexOfArgs com ctx r i.GenericArgs args

        Helper.LibCall(
            com,
            "array",
            "index_of",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "GetEnumerator", Some arg, _ -> getEnumerator com r t arg |> Some
    | _ -> None

let arrayModule
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    let newArray size t =
        Value(NewArray(ArrayAlloc size, t, MutableArray), None)

    let createArray size value =
        match t, value with
        | Array(Number _ as t2, _), None when com.Options.TypedArrays ->
            newArray size t2
        | Array(t2, _), value ->
            let value =
                value |> Option.defaultWith (fun () -> getZero com ctx t2)
            // If we don't fill the array some operations may behave unexpectedly, like Array.prototype.reduce
            Helper.LibCall(
                com,
                "array",
                "fill",
                t,
                [
                    newArray size t2
                    makeIntConst 0
                    size
                    value
                ]
            )
        | _ ->
            $"Expecting an array type but got {t}"
            |> addErrorAndReturnNull com ctx.InlinePath r

    match i.CompiledName, args with
    | "ToSeq", [ arg ] -> Some arg
    | "OfSeq", [ arg ] -> toArray r t arg |> Some
    | "OfList", [ arg ] ->
        Helper.LibCall(
            com,
            "list",
            "toArray",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "ToList", args ->
        Helper.LibCall(
            com,
            "list",
            "ofArray",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("Length" | "Count"), [ arg ] ->
        Helper.GlobalCall("len", t, [ arg ], [ t ], ?loc = r) |> Some
    | "Item", [ idx; ar ] -> getExpr r t ar idx |> Some
    | "Get", [ ar; idx ] -> getExpr r t ar idx |> Some
    | "Set", [ ar; idx; value ] -> setExpr r ar idx value |> Some
    | "ZeroCreate", [ count ] -> createArray count None |> Some
    | "Create", [ count; value ] -> createArray count (Some value) |> Some
    | "Empty", _ ->
        let t =
            match t with
            | Array(t, _) -> t
            | _ -> Any

        newArray (makeIntConst 0) t |> Some
    | "IsEmpty", [ ar ] ->
        eq
            (Helper.GlobalCall("len", t, [ ar ], [ t ], ?loc = r))
            (makeIntConst 0)
        |> Some
    | Patterns.DicContains nativeArrayFunctions meth, _ ->
        let args, thisArg = List.splitLast args
        let argTypes = List.take (List.length args) i.SignatureArgTypes

        let call = Helper.GlobalCall(meth, t, args @ [ thisArg ], ?loc = r)

        Helper.GlobalCall("list", t, [ call ], ?loc = r) |> Some
    | "Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth, args ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq2" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "seq2",
            "Array_" + meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Array" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "array",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let lists
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    // Use methods for Head and Tail (instead of Get(ListHead) for example) to check for empty lists
    | ReplaceName [ "get_Head", "head"
                    "get_Tail", "tail"
                    "get_Item", "item"
                    "get_Length", "length"
                    "GetSlice", "getSlice" ] methName,
      Some x,
      _ ->
        let args =
            match args with
            | [ ExprType Unit ] -> [ x ]
            | args -> args @ [ x ]

        Helper.LibCall(
            com,
            "list",
            methName,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "get_IsEmpty", Some x, _ -> Test(x, ListTest false, r) |> Some
    | "get_Empty", None, _ ->
        NewList(None, (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    | "Cons", None, [ h; t ] ->
        NewList(Some(h, t), (genArg com ctx r 0 i.GenericArgs))
        |> makeValue r
        |> Some
    | ("GetHashCode" | "Equals" | "CompareTo"), Some callee, _ ->
        Helper.InstanceCall(
            callee,
            i.CompiledName,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let listModule
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, args with
    | "IsEmpty", [ x ] -> Test(x, ListTest false, r) |> Some
    | "Empty", _ ->
        NewList(None, (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    | "Singleton", [ x ] ->
        NewList(
            Some(x, Value(NewList(None, t), None)),
            (genArg com ctx r 0 i.GenericArgs)
        )
        |> makeValue r
        |> Some
    // Use a cast to give it better chances of optimization (e.g. converting list
    // literals to arrays) after the beta reduction pass
    | "ToSeq", [ x ] -> toSeq t x |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth),
      args ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq2" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "seq2",
            "List_" + meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "List" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "list",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let sets
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | ".ctor" ->
        (genArg com ctx r 0 i.GenericArgs)
        |> makeSet com ctx r t "OfSeq" args
        |> Some
    | _ ->
        let isStatic = Option.isNone thisArg

        let mangledName =
            Naming.buildNameWithoutSanitationFrom
                "FSharpSet"
                isStatic
                i.CompiledName
                ""

        let args = injectArg com ctx r "Set" mangledName i.GenericArgs args

        Helper.LibCall(
            com,
            "set",
            mangledName,
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some

let setModule
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    let meth = Naming.lowerFirst i.CompiledName
    let args = injectArg com ctx r "Set" meth i.GenericArgs args

    Helper.LibCall(com, "set", meth, t, args, i.SignatureArgTypes, ?loc = r)
    |> Some

let maps
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | ".ctor" ->
        (genArg com ctx r 0 i.GenericArgs)
        |> makeMap com ctx r t "OfSeq" args
        |> Some
    | _ ->
        let isStatic = Option.isNone thisArg

        let mangledName =
            Naming.buildNameWithoutSanitationFrom
                "FSharpMap"
                isStatic
                i.CompiledName
                ""

        let args = injectArg com ctx r "Map" mangledName i.GenericArgs args

        Helper.LibCall(
            com,
            "map",
            mangledName,
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some

let mapModule
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    let meth = Naming.lowerFirst i.CompiledName
    let args = injectArg com ctx r "Map" meth i.GenericArgs args

    Helper.LibCall(com, "map", meth, t, args, i.SignatureArgTypes, ?loc = r)
    |> Some

let results
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | ("Bind" | "Map" | "MapError") as meth -> Some("Result_" + meth)
    | _ -> None
    |> Option.map (fun meth ->
        Helper.LibCall(
            com,
            "choice",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
    )

let nullables
    (com: ICompiler)
    (_: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    | ".ctor", None -> List.tryHead args
    // | "get_Value", Some c -> Get(c, OptionValue, t, r) |> Some // Get(OptionValueOptionValue) doesn't do a null check
    | "get_Value", Some c ->
        Helper.LibCall(com, "option", "value", t, [ c ], ?loc = r) |> Some
    | "get_HasValue", Some c -> Test(c, OptionTest true, r) |> Some
    | _ -> None

// See fable-library/Option.ts for more info on how options behave in Fable runtime
let options
    isStruct
    (com: ICompiler)
    (_: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    | "Some", _ ->
        NewOption(List.tryHead args, t.Generics.Head, isStruct)
        |> makeValue r
        |> Some
    | "get_None", _ ->
        NewOption(None, t.Generics.Head, isStruct) |> makeValue r |> Some
    | "get_Value", Some c ->
        Helper.LibCall(com, "option", "value", t, [ c ], ?loc = r) |> Some
    | "get_IsSome", Some c -> Test(c, OptionTest true, r) |> Some
    | "get_IsNone", Some c -> Test(c, OptionTest false, r) |> Some
    | _ -> None

let optionModule
    isStruct
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    let toArray r t arg =
        Helper.LibCall(
            com,
            "option",
            "toArray",
            Array(t, MutableArray),
            [ arg ],
            ?loc = r
        )

    match i.CompiledName, args with
    | "None", _ -> NewOption(None, t, isStruct) |> makeValue r |> Some
    | "GetValue", [ c ] ->
        Helper.LibCall(com, "option", "value", t, args, ?loc = r) |> Some
    | ("OfObj" | "OfNullable"), _ ->
        Helper.LibCall(com, "option", "ofNullable", t, args, ?loc = r) |> Some
    | ("ToObj" | "ToNullable"), _ ->
        Helper.LibCall(com, "option", "toNullable", t, args, ?loc = r) |> Some
    | "IsSome", [ c ] -> Test(c, OptionTest true, r) |> Some
    | "IsNone", [ c ] -> Test(c, OptionTest false, r) |> Some
    | ("Filter" | "Flatten" | "Map" | "Map2" | "Map3" | "Bind" as meth), args ->
        Helper.LibCall(
            com,
            "option",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "ToArray", [ arg ] -> toArray r t arg |> Some
    | "ToList", [ arg ] ->
        let args = args |> List.replaceLast (toArray None t)

        Helper.LibCall(com, "list", "ofArray", t, args, ?loc = r) |> Some
    | "FoldBack", [ folder; opt; state ] ->
        Helper.LibCall(
            com,
            "seq",
            "foldBack",
            t,
            [
                folder
                toArray None t opt
                state
            ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "DefaultValue", _ ->
        Helper.LibCall(com, "option", "defaultArg", t, List.rev args, ?loc = r)
        |> Some
    | "DefaultWith", _ ->
        Helper.LibCall(
            com,
            "option",
            "defaultArgWith",
            t,
            List.rev args,
            List.rev i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "OrElse", _ ->
        Helper.LibCall(com, "Option", "or_else", t, List.rev args, ?loc = r)
        |> Some
    | "OrElseWith", _ ->
        Helper.LibCall(
            com,
            "Option",
            "or_else_with",
            t,
            List.rev args,
            List.rev i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("Count" | "Contains" | "Exists" | "Fold" | "ForAll" | "Iterate" as meth),
      _ ->
        let meth = Naming.lowerFirst meth
        let args = args |> List.replaceLast (toArray None t)
        let args = injectArg com ctx r "Seq" meth i.GenericArgs args

        Helper.LibCall(com, "seq", meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | _ -> None

let parseBool
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, args with
    | ("Parse" | "TryParse" as method), args ->
        let func = Naming.lowerFirst method

        Helper.LibCall(
            com,
            "boolean",
            func,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let parseNum
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let parseCall meth str args style =
        let kind =
            match i.DeclaringEntityFullName with
            | Patterns.DicContains FSharp2Fable.TypeHelpers.numberTypes kind ->
                kind
            | x -> FableError $"Unexpected type in parse: %A{x}" |> raise

        let isFloatOrDecimal, numberModule, unsigned, bitsize =
            getParseParams kind

        let outValue =
            if meth = "TryParse" then
                [ List.last args ]
            else
                []

        let args =
            if isFloatOrDecimal then
                [ str ] @ outValue
            else
                [
                    str
                    makeIntConst style
                    makeBoolConst unsigned
                    makeIntConst bitsize
                ]
                @ outValue

        Helper.LibCall(
            com,
            numberModule,
            Naming.lowerFirst meth,
            t,
            args,
            ?loc = r
        )
        |> Some

    let isFloat =
        match i.SignatureArgTypes with
        | Number((Float32 | Float64), _) :: _ -> true
        | _ -> false

    match i.CompiledName, args with
    | "IsNaN", [ _ ] when isFloat ->
        Helper.ImportedCall("math", "isnan", t, args, ?loc = r) |> Some
    | "IsInfinity", [ _ ] when isFloat ->
        Helper.ImportedCall("math", "isinf", t, args, ?loc = r) |> Some
    | "IsNegativeInfinity", [ _ ] when isFloat ->
        Helper.LibCall(
            com,
            "double",
            "is_negative_inf",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("Parse" | "TryParse") as meth,
      str :: NumberConst(:? int as style, _, _) :: _ ->
        let hexConst = int System.Globalization.NumberStyles.HexNumber
        let intConst = int System.Globalization.NumberStyles.Integer

        if style <> hexConst && style <> intConst then
            $"%s{i.DeclaringEntityFullName}.%s{meth}(): NumberStyle %d{style} is ignored"
            |> addWarning com ctx.InlinePath r

        let acceptedArgs =
            if meth = "Parse" then
                2
            else
                3

        if List.length args > acceptedArgs then
            // e.g. Double.Parse(string, style, IFormatProvider) etc.
            $"%s{i.DeclaringEntityFullName}.%s{meth}(): provider argument is ignored"
            |> addWarning com ctx.InlinePath r

        parseCall meth str args style
    | ("Parse" | "TryParse") as meth, str :: _ ->
        let acceptedArgs =
            if meth = "Parse" then
                1
            else
                2

        if List.length args > acceptedArgs then
            // e.g. Double.Parse(string, IFormatProvider) etc.
            $"%s{i.DeclaringEntityFullName}.%s{meth}(): provider argument is ignored"
            |> addWarning com ctx.InlinePath r

        let style = int System.Globalization.NumberStyles.Any
        parseCall meth str args style
    | "Pow", _ ->
        Helper.ImportedCall(
            "math",
            "pow",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "ToString", [ ExprTypeAs(String, format) ] ->
        let format = emitExpr r String [ format ] "'{0:' + $0 + '}'"

        Helper.LibCall(
            com,
            "string",
            "format",
            t,
            [
                format
                thisArg.Value
            ],
            [
                format.Type
                thisArg.Value.Type
            ],
            ?loc = r
        )
        |> Some
    | "ToString", _ ->
        Helper.GlobalCall("str", String, [ thisArg.Value ], ?loc = r) |> Some
    | _ -> None

let decimals
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, args with
    | (".ctor" | "MakeDecimal"), ([ low; mid; high; isNegative; scale ] as args) ->
        Helper.LibCall(
            com,
            "decimal",
            "fromParts",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ".ctor",
      [ Value(NewArray(ArrayValues([ low; mid; high; signExp ] as args), _, _),
              _) ] ->
        Helper.LibCall(
            com,
            "decimal",
            "fromInts",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ".ctor", [ arg ] ->
        match arg.Type with
        | Array(Number(Int32, NumberInfo.Empty), _) ->
            Helper.LibCall(
                com,
                "decimal",
                "fromIntArray",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ -> makeDecimalFromExpr com r t arg |> Some
    | "GetBits", _ ->
        Helper.LibCall(
            com,
            "decimal",
            "getBits",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("Parse" | "TryParse"), _ -> parseNum com ctx r t i thisArg args
    | Operators.lessThan, [ left; right ] ->
        booleanCompare com ctx r left right BinaryLess |> Some
    | Operators.lessThanOrEqual, [ left; right ] ->
        booleanCompare com ctx r left right BinaryLessOrEqual |> Some
    | Operators.greaterThan, [ left; right ] ->
        booleanCompare com ctx r left right BinaryGreater |> Some
    | Operators.greaterThanOrEqual, [ left; right ] ->
        booleanCompare com ctx r left right BinaryGreaterOrEqual |> Some
    | (Operators.addition | Operators.subtraction | Operators.multiply | Operators.division | Operators.divideByInt | Operators.modulus | Operators.unaryNegation),
      _ -> applyOp com ctx r t i.CompiledName args |> Some
    | "op_Explicit", _ ->
        match t with
        | Number(kind, _) ->
            match kind with
            | Int64 -> toLong com ctx r false t args |> Some
            | UInt64 -> toLong com ctx r true t args |> Some
            | Int8
            | Int16
            | Int32
            | UInt8
            | UInt16
            | UInt32 -> toInt com ctx r t args |> Some
            | Float32
            | Float64 -> toFloat com ctx r t args |> Some
            | Decimal -> toDecimal com ctx r t args |> Some
            | Int128
            | UInt128
            | Float16
            | BigInt
            | NativeInt
            | UNativeInt -> None
        | _ -> None
    | ("Ceiling" | "Floor" | "Round" | "Truncate" | "Add" | "Subtract" | "Multiply" | "Divide" | "Remainder" | "Negate" as meth),
      _ ->
        let meth = Naming.lowerFirst meth

        Helper.LibCall(
            com,
            "decimal",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "ToString", [ ExprTypeAs(String, format) ] ->
        let format = emitExpr r String [ format ] "'{0:' + $0 + '}'"

        Helper.LibCall(
            com,
            "string",
            "format",
            t,
            [
                format
                thisArg.Value
            ],
            [
                format.Type
                thisArg.Value.Type
            ],
            ?loc = r
        )
        |> Some
    | "ToString", _ ->
        Helper.GlobalCall("str", String, [ thisArg.Value ], ?loc = r) |> Some
    | _, _ -> None

let bigints
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match thisArg, i.CompiledName with
    | None, ".ctor" ->
        match i.SignatureArgTypes with
        | [ Array _ ] ->
            Helper.LibCall(
                com,
                "big_int",
                "fromByteArray",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | [ Number((Int64 | UInt64), _) ] ->
            Helper.LibCall(
                com,
                "big_int",
                "fromInt64",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ ->
            Helper.LibCall(
                com,
                "big_int",
                "fromInt32",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
    | None, "op_Explicit" ->
        match t with
        | Number(kind, _) ->
            match kind with
            | Int64 -> toLong com ctx r false t args |> Some
            | UInt64 -> toLong com ctx r true t args |> Some
            | Int8
            | Int16
            | Int32
            | UInt8
            | UInt16
            | UInt32 -> toInt com ctx r t args |> Some
            | Float32
            | Float64 -> toFloat com ctx r t args |> Some
            | Decimal -> toDecimal com ctx r t args |> Some
            | Int128
            | UInt128
            | Float16
            | BigInt
            | NativeInt
            | UNativeInt -> None
        | _ -> None
    | None, "DivRem" ->
        Helper.LibCall(
            com,
            "big_int",
            "divRem",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | None, meth when meth.StartsWith("get_", StringComparison.Ordinal) ->
        Helper.LibValue(com, "big_int", meth, t) |> Some
    | callee, meth ->
        let args =
            match callee, meth with
            | None, _ -> args
            | Some c, _ -> c :: args

        Helper.LibCall(
            com,
            "big_int",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

// Compile static strings to their constant values
// reference: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/languageprimitives.errorstrings-module-%5bfsharp%5d
let errorStrings =
    function
    | "InputArrayEmptyString" -> str "The input array was empty" |> Some
    | "InputSequenceEmptyString" -> str "The input sequence was empty" |> Some
    | "InputMustBeNonNegativeString" ->
        str "The input must be non-negative" |> Some
    | _ -> None

let languagePrimitives
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, args with
    | Naming.EndsWith "Dynamic" operation, arg :: _ ->
        let operation =
            if operation = Operators.divideByInt then
                operation
            else
                "op_" + operation

        if operation = "op_Explicit" then
            Some arg // TODO
        else
            applyOp com ctx r t operation args |> Some
    | "DivideByInt", _ -> applyOp com ctx r t i.CompiledName args |> Some
    | "GenericZero", _ -> getZero com ctx t |> Some
    | "GenericOne", _ -> getOne com ctx t |> Some
    | ("SByteWithMeasure" | "Int16WithMeasure" | "Int32WithMeasure" | "Int64WithMeasure" | "Float32WithMeasure" | "FloatWithMeasure" | "DecimalWithMeasure"),
      [ arg ] -> arg |> Some
    | "EnumOfValue", [ arg ] -> TypeCast(arg, t) |> Some
    | "EnumToValue", [ arg ] -> TypeCast(arg, t) |> Some
    | ("GenericHash" | "GenericHashIntrinsic"), [ arg ] ->
        structuralHash com r arg |> Some
    | ("FastHashTuple2" | "FastHashTuple3" | "FastHashTuple4" | "FastHashTuple5" | "GenericHashWithComparer" | "GenericHashWithComparerIntrinsic"),
      [ comp; arg ] ->
        Helper.InstanceCall(
            comp,
            "GetHashCode",
            t,
            [ arg ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("GenericComparison" | "GenericComparisonIntrinsic"), [ left; right ] ->
        compare com ctx r left right |> Some
    | ("FastCompareTuple2" | "FastCompareTuple3" | "FastCompareTuple4" | "FastCompareTuple5" | "GenericComparisonWithComparer" | "GenericComparisonWithComparerIntrinsic"),
      [ comp; left; right ] ->
        Helper.InstanceCall(
            comp,
            "Compare",
            t,
            [
                left
                right
            ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("GenericLessThan" | "GenericLessThanIntrinsic"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryLess |> Some
    | ("GenericLessOrEqual" | "GenericLessOrEqualIntrinsic"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryLessOrEqual |> Some
    | ("GenericGreaterThan" | "GenericGreaterThanIntrinsic"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryGreater |> Some
    | ("GenericGreaterOrEqual" | "GenericGreaterOrEqualIntrinsic"),
      [ left; right ] ->
        booleanCompare com ctx r left right BinaryGreaterOrEqual |> Some
    | ("GenericEquality" | "GenericEqualityIntrinsic"), [ left; right ] ->
        equals com ctx r true left right |> Some
    | ("GenericEqualityER" | "GenericEqualityERIntrinsic"), [ left; right ] ->
        // TODO: In ER mode, equality on two NaNs returns "true".
        equals com ctx r true left right |> Some
    | ("FastEqualsTuple2" | "FastEqualsTuple3" | "FastEqualsTuple4" | "FastEqualsTuple5" | "GenericEqualityWithComparer" | "GenericEqualityWithComparerIntrinsic"),
      [ comp; left; right ] ->
        Helper.InstanceCall(
            comp,
            "Equals",
            t,
            [
                left
                right
            ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("PhysicalEquality" | "PhysicalEqualityIntrinsic"), [ left; right ] ->
        makeEqOp r left right BinaryEqual |> Some
    | ("PhysicalHash" | "PhysicalHashIntrinsic"), [ arg ] ->
        Helper.LibCall(
            com,
            "util",
            "physicalHash",
            Int32.Number,
            [ arg ],
            ?loc = r
        )
        |> Some
    | ("GenericEqualityComparer" | "GenericEqualityERComparer" | "FastGenericComparer" | "FastGenericComparerFromTable" | "FastGenericEqualityComparer" | "FastGenericEqualityComparerFromTable"),
      _ -> fsharpModule com ctx r t i thisArg args
    | ("ParseInt32" | "ParseUInt32"), [ arg ] ->
        toInt com ctx r t [ arg ] |> Some
    | "ParseInt64", [ arg ] -> toLong com ctx r false t [ arg ] |> Some
    | "ParseUInt64", [ arg ] -> toLong com ctx r true t [ arg ] |> Some
    | _ -> None

let intrinsicFunctions
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    // Erased operators
    | "CheckThis", _, [ arg ]
    | "UnboxFast", _, [ arg ]
    | "UnboxGeneric", _, [ arg ] -> Some arg
    | "MakeDecimal", _, _ -> decimals com ctx r t i thisArg args
    | "GetString", _, [ ar; idx ]
    | "GetArray", _, [ ar; idx ] -> getExpr r t ar idx |> Some
    | "SetArray", _, [ ar; idx; value ] -> setExpr r ar idx value |> Some
    | ("GetArraySlice" | "GetStringSlice"), None, [ ar; lower; upper ] ->
        let upper =
            match upper with
            | Value(NewOption(None, _, _), _) ->
                Helper.GlobalCall("len", t, [ ar ], [ t ], ?loc = r)
            //getExpr None (Int32.Number) ar (makeStrConst "length2")
            | _ -> add upper (makeIntConst 1)

        Helper.InstanceCall(
            ar,
            "slice",
            t,
            [
                lower
                upper
            ],
            ?loc = r
        )
        |> Some
    | "SetArraySlice", None, args ->
        Helper.LibCall(
            com,
            "array",
            "setSlice",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("TypeTestGeneric" | "TypeTestFast"), None, [ expr ] ->
        Test(expr, TypeTest((genArg com ctx r 0 i.GenericArgs)), r) |> Some
    | "CreateInstance", None, _ ->
        match genArg com ctx r 0 i.GenericArgs with
        | DeclaredType(ent, _) ->
            let ent = com.GetEntity(ent)

            Helper.ConstructorCall(constructor com ent, t, [], ?loc = r) |> Some
        | t ->
            $"Cannot create instance of type unresolved at compile time: %A{t}"
            |> addErrorAndReturnNull com ctx.InlinePath r
            |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.powdouble-function-%5bfsharp%5d
    // Type: PowDouble : float -> int -> float
    // Usage: PowDouble x n
    | "PowDouble", None, _ ->
        Helper.ImportedCall(
            "math",
            "pow",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "PowDecimal", None, _ ->
        Helper.LibCall(
            com,
            "decimal",
            "pow",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangechar-function-%5bfsharp%5d
    // Type: RangeChar : char -> char -> seq<char>
    // Usage: RangeChar start stop
    | "RangeChar", None, _ ->
        Helper.LibCall(
            com,
            "range",
            "rangeChar",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangedouble-function-%5bfsharp%5d
    // Type: RangeDouble: float -> float -> float -> seq<float>
    // Usage: RangeDouble start step stop
    | ("RangeSByte" | "RangeByte" | "RangeInt16" | "RangeUInt16" | "RangeInt32" | "RangeUInt32" | "RangeSingle" | "RangeDouble"),
      None,
      args ->
        Helper.LibCall(
            com,
            "range",
            "rangeDouble",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "RangeInt64", None, args ->
        Helper.LibCall(
            com,
            "range",
            "rangeInt64",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "RangeUInt64", None, args ->
        Helper.LibCall(
            com,
            "range",
            "rangeUInt64",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let runtimeHelpers
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    thisArg
    args
    =
    match i.CompiledName, args with
    | "GetHashCode", [ arg ] -> identityHash com r arg |> Some
    | _ -> None

// ExceptionDispatchInfo is used to raise exceptions through different threads in async workflows
// We don't need to do anything in JS, see #2396
let exceptionDispatchInfo
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    thisArg
    args
    =
    match i.CompiledName, thisArg, args with
    | "Capture", _, [ arg ] -> Some arg
    | "Throw", Some arg, _ -> makeThrow r t arg |> Some
    | _ -> None

let funcs (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg with
    // Just use Emit to change the type of the arg, Fable will automatically uncurry the function
    | "Adapt", _ -> emitExpr r t args "$0" |> Some
    | "Invoke", Some callee ->
        Helper.Application(callee, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | _ -> None

let keyValuePairs
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    thisArg
    args
    =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> makeTuple r false args |> Some
    | "get_Key", Some c -> Get(c, TupleIndex 0, t, r) |> Some
    | "get_Value", Some c -> Get(c, TupleIndex 1, t, r) |> Some
    | _ -> None

let dictionaries
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match i.SignatureArgTypes, args with
        | ([] | [ Number _ ]), _ ->
            makeDictionary com ctx r t (makeArray Any []) |> Some
        | [ IDictionary ], [ arg ] -> makeDictionary com ctx r t arg |> Some
        | [ IDictionary; IEqualityComparer ], [ arg; eqComp ] ->
            makeComparerFromEqualityComparer eqComp
            |> makeDictionaryWithComparer com r t arg
            |> Some
        | [ IEqualityComparer ], [ eqComp ]
        | [ Number _; IEqualityComparer ], [ _; eqComp ] ->
            makeComparerFromEqualityComparer eqComp
            |> makeDictionaryWithComparer com r t (makeArray Any [])
            |> Some
        | _ -> None
    | "get_IsReadOnly", _ -> makeBoolConst false |> Some
    | "get_Count", _ ->
        Helper.GlobalCall("len", t, [ thisArg.Value ], [ t ], ?loc = r) |> Some
    | "GetEnumerator", Some callee -> getEnumerator com r t callee |> Some
    | "ContainsValue", _ ->
        match thisArg, args with
        | Some c, [ arg ] ->
            Helper.LibCall(
                com,
                "map_util",
                "contains_value",
                t,
                [
                    arg
                    c
                ],
                ?loc = r
            )
            |> Some
        | _ -> None
    | "TryGetValue", _ ->
        Helper.LibCall(
            com,
            "map_util",
            "tryGetValue",
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | "Add", _ ->
        Helper.LibCall(
            com,
            "map_util",
            "add_to_dict",
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | "Remove", _ ->
        Helper.LibCall(
            com,
            "map_util",
            "remove_from_dict",
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | "get_Item", _ ->
        Helper.LibCall(
            com,
            "map_util",
            "getItemFromDict",
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | ReplaceName [ "set_Item", "set"
                    "get_Keys", "keys"
                    "get_Values", "values"
                    "ContainsKey", "has"
                    "Clear", "clear" ] methName,
      Some c ->
        Helper.InstanceCall(c, methName, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | _ -> None

let hashSets
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ ->
        match i.SignatureArgTypes, args with
        | [], _ -> makeHashSet com ctx r t (makeArray Any []) |> Some
        | [ IEnumerable ], [ arg ] -> makeHashSet com ctx r t arg |> Some
        | [ IEnumerable; IEqualityComparer ], [ arg; eqComp ] ->
            makeComparerFromEqualityComparer eqComp
            |> makeHashSetWithComparer com r t arg
            |> Some
        | [ IEqualityComparer ], [ eqComp ] ->
            makeComparerFromEqualityComparer eqComp
            |> makeHashSetWithComparer com r t (makeArray Any [])
            |> Some
        | _ -> None
    | "get_Count", _, _ -> getFieldWith r t thisArg.Value "size" |> Some
    | "get_IsReadOnly", _, _ -> BoolConstant false |> makeValue r |> Some
    | ReplaceName [ "Clear", "clear"; "Contains", "has"; "Remove", "delete" ] methName,
      Some c,
      args ->
        Helper.InstanceCall(c, methName, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | "GetEnumerator", Some c, _ -> getEnumerator com r t c |> Some
    | "Add", Some c, [ arg ] ->
        Helper.LibCall(
            com,
            "map_util",
            "addToSet",
            t,
            [
                arg
                c
            ],
            ?loc = r
        )
        |> Some
    | ("IsProperSubsetOf" | "IsProperSupersetOf" | "UnionWith" | "IntersectWith" | "ExceptWith" | "IsSubsetOf" | "IsSupersetOf" as meth),
      Some c,
      args ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Set" meth i.GenericArgs args

        Helper.LibCall(com, "Set", meth, t, c :: args, ?loc = r) |> Some
    // | "CopyTo" // TODO!!!
    // | "SetEquals"
    // | "Overlaps"
    // | "SymmetricExceptWith"
    | _ -> None

let exceptions
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        Helper.ConstructorCall(makeIdentExpr "Exception", t, args, ?loc = r)
        |> Some
    | "get_Message", Some e ->
        Helper.GlobalCall("str", t, [ thisArg.Value ], ?loc = r) |> Some
    | "get_StackTrace", Some e -> getFieldWith r t e "stack" |> Some
    | _ -> None

let objects
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> typedObjExpr t [] |> Some
    | "ToString", Some arg, _ -> toString com ctx r [ arg ] |> Some
    | "ReferenceEquals", _, [ left; right ] ->
        makeEqOpStrict r left right BinaryEqual |> Some
    | "Equals", Some arg1, [ arg2 ]
    | "Equals", None, [ arg1; arg2 ] -> equals com ctx r true arg1 arg2 |> Some
    | "GetHashCode", Some arg, _ -> identityHash com r arg |> Some
    | "GetType", Some arg, _ ->
        if arg.Type = Any then
            "Types can only be resolved at compile time. At runtime this will be same as `typeof<obj>`"
            |> addWarning com ctx.InlinePath r

        makeTypeInfo r arg.Type |> Some
    | _ -> None

let valueTypes
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> typedObjExpr t [] |> Some
    | "ToString", Some arg, _ -> toString com ctx r [ arg ] |> Some
    | "Equals", Some arg1, [ arg2 ]
    | "Equals", None, [ arg1; arg2 ] -> equals com ctx r true arg1 arg2 |> Some
    | "GetHashCode", Some arg, _ -> structuralHash com r arg |> Some
    | "CompareTo", Some arg1, [ arg2 ] -> compare com ctx r arg1 arg2 |> Some
    | _ -> None

let unchecked
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, args with
    | "DefaultOf", _ ->
        (genArg com ctx r 0 i.GenericArgs) |> defaultof com ctx r |> Some
    | "Hash", [ arg ] -> structuralHash com r arg |> Some
    | "Equals", [ arg1; arg2 ] -> equals com ctx r true arg1 arg2 |> Some
    | "Compare", [ arg1; arg2 ] -> compare com ctx r arg1 arg2 |> Some
    | _ -> None

let enums
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match thisArg, i.CompiledName, args with
    | Some this, "HasFlag", [ arg ] ->
        // x.HasFlags(y) => (int x) &&& (int y) <> 0
        makeBinOp r (Int32.Number) this arg BinaryAndBitwise
        |> fun bitwise -> makeEqOp r bitwise (makeIntConst 0) BinaryUnequal
        |> Some
    | None,
      Patterns.DicContains (dict [ "Parse", "parseEnum"
                                   "TryParse", "tryParseEnum"
                                   "IsDefined", "isEnumDefined"
                                   "GetName", "getEnumName"
                                   "GetNames", "getEnumNames"
                                   "GetValues", "getEnumValues"
                                   "GetUnderlyingType", "getEnumUnderlyingType" ]) meth,
      args ->
        let args =
            match meth, args with
            // TODO: Parse at compile time if we know the type
            | "parseEnum", [ value ] ->
                [
                    makeTypeInfo None t
                    value
                ]
            | "tryParseEnum", [ value; refValue ] ->
                [
                    genArg com ctx r 0 i.GenericArgs |> makeTypeInfo None
                    value
                    refValue
                ]
            | _ -> args

        Helper.LibCall(com, "Reflection", meth, t, args, ?loc = r) |> Some
    | _ -> None

let log (com: ICompiler) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    let args =
        match args with
        | [] -> []
        | [ v ] -> [ v ]
        | (StringConst _) :: _ ->
            [
                Helper.LibCall(
                    com,
                    "String",
                    "format",
                    t,
                    args,
                    i.SignatureArgTypes
                )
            ]
        | _ -> [ args.Head ]

    match com.Options.Language with
    | Python -> Helper.GlobalCall("print", t, args, ?loc = r)
    | _ -> Helper.GlobalCall("console", t, args, memb = "log", ?loc = r)

let bitConvert
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "GetBytes" ->
        let memberName =
            match args.Head.Type with
            | Boolean -> "getBytesBoolean"
            | Char
            | String -> "getBytesChar"
            | Number(Int16, _) -> "getBytesInt16"
            | Number(Int32, _) -> "getBytesInt32"
            | Number(UInt16, _) -> "getBytesUInt16"
            | Number(UInt32, _) -> "getBytesUInt32"
            | Number(Float32, _) -> "getBytesSingle"
            | Number(Float64, _) -> "getBytesDouble"
            | Number(Int64, _) -> "getBytesInt64"
            | Number(UInt64, _) -> "getBytesUInt64"
            | x ->
                FableError $"Unsupported type in BitConverter.GetBytes(): %A{x}"
                |> raise

        let expr =
            Helper.LibCall(
                com,
                "BitConverter",
                memberName,
                Boolean,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )

        if com.Options.TypedArrays then
            expr |> Some
        else
            toArray r t expr |> Some // convert to dynamic array
    | _ ->
        let memberName = Naming.lowerFirst i.CompiledName

        Helper.LibCall(
            com,
            "BitConverter",
            memberName,
            Boolean,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let convert
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "ToSByte"
    | "ToByte"
    | "ToInt16"
    | "ToUInt16"
    | "ToInt32"
    | "ToUInt32" -> round com args |> toInt com ctx r t |> Some
    | "ToInt64" -> round com args |> toLong com ctx r false t |> Some
    | "ToUInt64" -> round com args |> toLong com ctx r true t |> Some
    | "ToSingle"
    | "ToDouble" -> toFloat com ctx r t args |> Some
    | "ToDecimal" -> toDecimal com ctx r t args |> Some
    | "ToChar" -> toChar args.Head |> Some
    | "ToString" -> toString com ctx r args |> Some
    | "ToBase64String"
    | "FromBase64String" ->
        if not (List.isSingle args) then
            $"Convert.%s{Naming.upperFirst i.CompiledName} only accepts one single argument"
            |> addWarning com ctx.InlinePath r

        Helper.LibCall(
            com,
            "String",
            (Naming.lowerFirst i.CompiledName),
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let console
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "get_Out" -> typedObjExpr t [] |> Some // empty object
    | "Write" ->
        addWarning com ctx.InlinePath r "Write will behave as WriteLine"
        log com r t i thisArg args |> Some
    | "ReadLine" -> Helper.GlobalCall("input", t, args, ?loc = r) |> Some
    | "WriteLine" -> log com r t i thisArg args |> Some
    | _ -> None

let stopwatch
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        Helper.LibCall(
            com,
            "diagnostics",
            "StopWatch",
            t,
            args,
            i.SignatureArgTypes,
            isConstructor = true,
            ?loc = r
        )
        |> Some
    | "get_ElapsedMilliseconds", Some x ->
        Helper.InstanceCall(x, "elapsed_milliseconds", t, []) |> Some
    | "get_ElapsedTicks", Some x ->
        Helper.InstanceCall(x, "elapsed_ticks", t, []) |> Some
    | "Start", Some x
    | "Stop", Some x ->
        Helper.InstanceCall(x, i.CompiledName.ToLower(), t, []) |> Some
    | _ ->
        let memberName = Naming.lowerFirst i.CompiledName

        Helper.LibCall(
            com,
            "diagnostics",
            memberName,
            Boolean,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some


let debug
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "Write" ->
        addWarning com ctx.InlinePath r "Write will behave as WriteLine"
        log com r t i thisArg args |> Some
    | "WriteLine" -> log com r t i thisArg args |> Some
    | "Break" -> makeDebugger r |> Some
    | "Assert" ->
        let unit = Value(Null Unit, None)

        match args with
        | []
        | [ Value(BoolConstant true, _) ] -> Some unit
        | [ Value(BoolConstant false, _) ] -> makeDebugger r |> Some
        | arg :: _ ->
            // emit i "if (!$0) { debugger; }" i.args |> Some
            let cond = Operation(Unary(UnaryNot, arg), Tags.empty, Boolean, r)
            IfThenElse(cond, makeDebugger r, unit, r) |> Some
    | _ -> None

let dates
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let getTime (e: Expr) =
        Helper.InstanceCall(e, "getTime", t, [])

    let moduleName =
        if i.DeclaringEntityFullName = Types.datetime then
            "Date"
        else
            "DateOffset"

    match i.CompiledName with
    | ".ctor" ->
        match args with
        | [] ->
            Helper.LibCall(com, moduleName, "minValue", t, [], [], ?loc = r)
            |> Some
        | ExprType(Number(Int64, _)) :: _ ->
            Helper.LibCall(
                com,
                moduleName,
                "fromTicks",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | ExprType(DeclaredType(e, [])) :: _ when e.FullName = Types.datetime ->
            Helper.LibCall(
                com,
                "DateOffset",
                "fromDate",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ ->
            let last = List.last args

            match args.Length, last.Type with
            | 7, Number(_, NumberInfo.IsEnum ent) when
                ent.FullName = "System.DateTimeKind"
                ->
                let args =
                    (List.take 6 args)
                    @ [
                        makeIntConst 0
                        last
                    ]

                let argTypes =
                    (List.take 6 i.SignatureArgTypes)
                    @ [
                        Int32.Number
                        last.Type
                    ]

                Helper.LibCall(
                    com,
                    "Date",
                    "create",
                    t,
                    args,
                    argTypes,
                    ?loc = r
                )
                |> Some
            | _ ->
                Helper.LibCall(
                    com,
                    moduleName,
                    "create",
                    t,
                    args,
                    i.SignatureArgTypes,
                    ?loc = r
                )
                |> Some
    | "ToString" ->
        Helper.LibCall(
            com,
            "Date",
            "toString",
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | "get_Kind"
    | "get_Offset" ->
        Naming.removeGetSetPrefix i.CompiledName
        |> Naming.lowerFirst
        |> getFieldWith r t thisArg.Value
        |> Some
    // DateTimeOffset
    | "get_LocalDateTime" ->
        Helper.LibCall(
            com,
            "DateOffset",
            "toLocalTime",
            t,
            [ thisArg.Value ],
            [ thisArg.Value.Type ],
            ?loc = r
        )
        |> Some
    | "get_UtcDateTime" ->
        Helper.LibCall(
            com,
            "DateOffset",
            "toUniversalTime",
            t,
            [ thisArg.Value ],
            [ thisArg.Value.Type ],
            ?loc = r
        )
        |> Some
    | "get_DateTime" ->
        let kind = System.DateTimeKind.Unspecified |> int |> makeIntConst

        Helper.LibCall(
            com,
            "Date",
            "fromDateTimeOffset",
            t,
            [
                thisArg.Value
                kind
            ],
            [
                thisArg.Value.Type
                kind.Type
            ],
            ?loc = r
        )
        |> Some
    | "FromUnixTimeSeconds"
    | "FromUnixTimeMilliseconds" ->
        let value =
            Helper.LibCall(
                com,
                "Long",
                "toNumber",
                Float64.Number,
                args,
                i.SignatureArgTypes
            )

        let value =
            if i.CompiledName = "FromUnixTimeSeconds" then
                makeBinOp r t value (makeIntConst 1000) BinaryMultiply
            else
                value

        Helper.LibCall(
            com,
            "DateOffset",
            "datetime.fromtimestamp",
            t,
            [
                value
                makeIntConst 0
            ],
            [
                value.Type
                Int32.Number
            ],
            ?loc = r
        )
        |> Some
    | "ToUnixTimeSeconds"
    | "ToUnixTimeMilliseconds" ->
        let ms = getTime thisArg.Value

        let args =
            if i.CompiledName = "ToUnixTimeSeconds" then
                [ makeBinOp r t ms (makeIntConst 1000) BinaryDivide ]
            else
                [ ms ]

        Helper.LibCall(com, "Long", "fromNumber", t, args, ?loc = r) |> Some
    | "get_Ticks" ->
        Helper.LibCall(
            com,
            "Date",
            "getTicks",
            t,
            [ thisArg.Value ],
            [ thisArg.Value.Type ],
            ?loc = r
        )
        |> Some
    | "get_UtcTicks" ->
        Helper.LibCall(
            com,
            "DateOffset",
            "getUtcTicks",
            t,
            [ thisArg.Value ],
            [ thisArg.Value.Type ],
            ?loc = r
        )
        |> Some
    | "AddTicks" ->
        match thisArg, args with
        | Some c, [ ticks ] ->
            let ms =
                Helper.LibCall(
                    com,
                    "long",
                    "op_Division",
                    i.SignatureArgTypes.Head,
                    [
                        ticks
                        makeIntConst 10000
                    ],
                    [
                        ticks.Type
                        Int32.Number
                    ]
                )

            let ms =
                Helper.LibCall(
                    com,
                    "long",
                    "toNumber",
                    Float64.Number,
                    [ ms ],
                    [ ms.Type ]
                )

            Helper.LibCall(
                com,
                moduleName,
                "addMilliseconds",
                Float64.Number,
                [
                    c
                    ms
                ],
                [
                    c.Type
                    ms.Type
                ],
                ?loc = r
            )
            |> Some
        | _ -> None
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst

        Helper.LibCall(
            com,
            moduleName,
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some

let timeSpans
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    // let callee = match i.callee with Some c -> c | None -> i.args.Head

    match i.CompiledName with
    | ".ctor" ->
        Helper.LibCall(
            com,
            "time_span",
            "create",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "ToString" when (args.Length = 1) ->
        "TimeSpan.ToString with one argument is not supported, because it depends of local culture, please add CultureInfo.InvariantCulture"
        |> addError com ctx.InlinePath r

        None
    | "ToString" when (args.Length = 2) ->
        match args.Head with
        | StringConst "c"
        | StringConst "g"
        | StringConst "G" ->
            Helper.LibCall(
                com,
                "time_span",
                "toString",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ ->
            "TimeSpan.ToString don't support custom format. It only handles \"c\", \"g\" and \"G\" format, with CultureInfo.InvariantCulture."
            |> addError com ctx.InlinePath r

            None
    | "get_Nanoseconds"
    | "get_TotalNanoseconds" -> None
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst

        Helper.LibCall(
            com,
            "time_span",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some

let timers
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ ->
        Helper.LibCall(
            com,
            "timer",
            "Timer",
            t,
            args,
            i.SignatureArgTypes,
            isConstructor = true,
            ?loc = r
        )
        |> Some
    | Naming.StartsWith "get_" meth, Some x, _ ->
        getFieldWith r t x meth |> Some
    | Naming.StartsWith "set_" meth, Some x, [ value ] ->
        setExpr r x (makeStrConst meth) value |> Some
    | meth, Some x, args ->
        Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | _ -> None

let systemEnv
    (com: ICompiler)
    (ctx: Context)
    (_: SourceLocation option)
    (_: Type)
    (i: CallInfo)
    (_: Expr option)
    (_: Expr list)
    =
    match i.CompiledName with
    | "get_NewLine" -> Some(makeStrConst "\n")
    | _ -> None

// Initial support, making at least InvariantCulture compile-able
// to be used System.Double.Parse and System.Single.Parse
// see https://github.com/fable-compiler/Fable/pull/1197#issuecomment-348034660
let globalization
    (com: ICompiler)
    (ctx: Context)
    (_: SourceLocation option)
    t
    (i: CallInfo)
    (_: Expr option)
    (_: Expr list)
    =
    match i.CompiledName with
    | "get_InvariantCulture" ->
        // System.Globalization namespace is not supported by Fable. The value InvariantCulture will be compiled to an empty object literal
        ObjectExpr([], t, None) |> Some
    | _ -> None

let random
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | ".ctor" -> ObjectExpr([], t, None) |> Some
    | "Next" ->
        let min, max =
            match args with
            | [] -> makeIntConst 0, makeIntConst System.Int32.MaxValue
            | [ max ] -> makeIntConst 0, max
            | [ min; max ] -> min, max
            | _ -> FableError "Unexpected arg count for Random.Next" |> raise

        Helper.LibCall(
            com,
            "util",
            "randint",
            t,
            [
                min
                max
            ],
            [
                min.Type
                max.Type
            ],
            ?loc = r
        )
        |> Some
    | "NextDouble" -> Helper.ImportedCall("random", "random", t, [], []) |> Some
    | "NextBytes" ->
        let byteArray =
            match args with
            | [ b ] -> b
            | _ ->
                FableError "Unexpected arg count for Random.NextBytes" |> raise

        Helper.LibCall(
            com,
            "util",
            "randomBytes",
            t,
            [ byteArray ],
            [ byteArray.Type ],
            ?loc = r
        )
        |> Some
    | _ -> None

let cancels
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "get_None" // TODO: implement as non-cancellable token
    | ".ctor" ->
        Helper.LibCall(
            com,
            "async_",
            "createCancellationToken",
            t,
            args,
            i.SignatureArgTypes
        )
        |> Some
    | "get_Token" -> thisArg
    | "Cancel"
    | "CancelAfter"
    | "get_IsCancellationRequested"
    | "ThrowIfCancellationRequested" ->
        let args, argTypes =
            match thisArg with
            | Some c -> c :: args, c.Type :: i.SignatureArgTypes
            | None -> args, i.SignatureArgTypes

        Helper.LibCall(
            com,
            "async_",
            Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst,
            t,
            args,
            argTypes,
            ?loc = r
        )
        |> Some
    // TODO: Add check so CancellationTokenSource cannot be cancelled after disposed?
    | "Dispose" -> Null Type.Unit |> makeValue r |> Some
    | "Register" ->
        Helper.InstanceCall(
            thisArg.Value,
            "register",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let monitor
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "Enter"
    | "Exit" -> Null Type.Unit |> makeValue r |> Some
    | _ -> None

let thread
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "Sleep" ->
        Helper.LibCall(
            com,
            "thread",
            "sleep",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let activator
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | "CreateInstance", None, ([ _type ] | [ _type; (ExprType(Array(Any, _))) ]) ->
        Helper.LibCall(com, "Reflection", "createInstance", t, args, ?loc = r)
        |> Some
    | _ -> None

let regex
    com
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let propInt p callee = getExpr r t callee (makeIntConst p)
    let propStr p callee = getExpr r t callee (makeStrConst p)

    let isGroup =
        match thisArg with
        | Some(ExprType(DeclaredTypeFullName Types.regexGroup)) -> true
        | _ -> false

    let createRegex r t args =
        Helper.LibCall(com, "RegExp", "create", t, args, ?loc = r)

    match i.CompiledName with
    // TODO: Use RegexConst if no options have been passed?
    | ".ctor" ->
        Helper.LibCall(
            com,
            "RegExp",
            "create",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "get_Options" ->
        Helper.LibCall(
            com,
            "RegExp",
            "options",
            t,
            [ thisArg.Value ],
            [ thisArg.Value.Type ],
            ?loc = r
        )
        |> Some
    // Capture
    | "get_Index" ->
        if not isGroup then
            Helper.InstanceCall(
                thisArg.Value,
                "start",
                t,
                [],
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        else
            "Accessing index of Regex groups is not supported"
            |> addErrorAndReturnNull com ctx.InlinePath r
            |> Some
    | "get_Value" ->
        if
            isGroup
        // In JS Regex group values can be undefined, ensure they're empty strings #838
        then
            Operation(
                Logical(LogicalOr, thisArg.Value, makeStrConst ""),
                Tags.empty,
                t,
                r
            )
            |> Some
        else
            propInt 0 thisArg.Value |> Some
    | "get_Length" ->
        if isGroup then
            Helper.GlobalCall("len", t, [ thisArg.Value ], [ t ], ?loc = r)
            |> Some
        else
            let prop = propInt 0 thisArg.Value
            Helper.GlobalCall("len", t, [ prop ], [ t ], ?loc = r) |> Some
    // Group
    | "get_Success" -> nullCheck r false thisArg.Value |> Some
    // MatchCollection & GroupCollection
    | "get_Item" when i.DeclaringEntityFullName = Types.regexGroupCollection ->
        Helper.LibCall(
            com,
            "RegExp",
            "get_item",
            t,
            [
                thisArg.Value
                args.Head
            ],
            [ thisArg.Value.Type ],
            ?loc = r
        )
        |> Some
    | "get_Item" -> getExpr r t thisArg.Value args.Head |> Some
    | "get_Count" ->
        Helper.GlobalCall("len", t, [ thisArg.Value ], [ t ], ?loc = r) |> Some
    | "GetEnumerator" -> getEnumerator com r t thisArg.Value |> Some
    | "IsMatch"
    | "Match"
    | "Matches" as meth ->
        match thisArg, args with
        | Some thisArg, args ->
            if args.Length > 2 then
                $"Regex.{meth} doesn't support more than 2 arguments"
                |> addError com ctx.InlinePath r

            thisArg :: args |> Some
        | None, input :: pattern :: args ->
            let reg = createRegex None Any (pattern :: args)

            [
                reg
                input
            ]
            |> Some
        | _ -> None
        |> Option.map (fun args ->
            Helper.LibCall(
                com,
                "RegExp",
                Naming.lowerFirst meth,
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
        )
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst

        Helper.LibCall(
            com,
            "reg_exp",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some

let encoding
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args.Length with
    | ("get_Unicode" | "get_UTF8"), _, _ ->
        Helper.LibCall(
            com,
            "Encoding",
            i.CompiledName,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "GetBytes", Some callee, (1 | 3) ->
        let meth = Naming.lowerFirst i.CompiledName

        let expr =
            Helper.InstanceCall(
                callee,
                meth,
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )

        if com.Options.TypedArrays then
            expr |> Some
        else
            toArray r t expr |> Some // convert to dynamic array
    | "GetString", Some callee, (1 | 3) ->
        let meth = Naming.lowerFirst i.CompiledName

        Helper.InstanceCall(
            callee,
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let enumerators
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match thisArg with
    | Some callee ->
        // Enumerators are mangled, use the fully qualified name
        let isGenericCurrent =
            i.CompiledName = "get_Current"
            && i.DeclaringEntityFullName <> Types.ienumerator

        let entityName =
            if isGenericCurrent then
                Types.ienumeratorGeneric
            else
                Types.ienumerator

        let methName = entityName + "." + i.CompiledName

        Helper.InstanceCall(callee, methName, t, args, ?loc = r) |> Some
    | _ -> None

let enumerables
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (_: Expr list)
    =
    match thisArg, i.CompiledName with
    // This property only belongs to Key and Value Collections
    | Some callee, "get_Count" ->
        Helper.LibCall(com, "Seq", "length", t, [ callee ], ?loc = r) |> Some
    | Some callee, "GetEnumerator" -> getEnumerator com r t callee |> Some
    | _ -> None

let events
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        Helper.LibCall(
            com,
            "event",
            "Event",
            t,
            args,
            i.SignatureArgTypes,
            isConstructor = true,
            ?loc = r
        )
        |> Some
    | "get_Publish", Some x -> getFieldWith r t x "Publish" |> Some
    | meth, Some x ->
        Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | meth, None ->
        Helper.LibCall(
            com,
            "event",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let observable
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    Helper.LibCall(
        com,
        "Observable",
        Naming.lowerFirst i.CompiledName,
        t,
        args,
        i.SignatureArgTypes,
        ?loc = r
    )
    |> Some

let mailbox
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match thisArg with
    | None ->
        match i.CompiledName with
        | ".ctor" ->
            Helper.LibCall(
                com,
                "mailbox_processor",
                "MailboxProcessor",
                t,
                args,
                i.SignatureArgTypes,
                isConstructor = true,
                ?loc = r
            )
            |> Some
        | "Start" ->
            Helper.LibCall(
                com,
                "mailbox_processor",
                "start",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ -> None
    | Some callee ->
        match i.CompiledName with
        // `reply` belongs to AsyncReplyChannel
        | "Start"
        | "Receive"
        | "PostAndAsyncReply"
        | "Post" ->
            let memb =
                if i.CompiledName = "Start" then
                    "startInstance"
                else
                    Naming.lowerFirst i.CompiledName

            Helper.LibCall(
                com,
                "mailbox_processor",
                memb,
                t,
                args,
                i.SignatureArgTypes,
                thisArg = callee,
                ?loc = r
            )
            |> Some
        | "Reply" ->
            Helper.InstanceCall(
                callee,
                "reply",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ -> None

let asyncBuilder
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match thisArg, i.CompiledName, args with
    | _, "Singleton", _ ->
        makeImportLib com t "singleton" "async_builder" |> Some
    // For Using we need to cast the argument to IDisposable
    | Some x, "Using", [ arg; f ] ->
        Helper.InstanceCall(
            x,
            "Using",
            t,
            [
                arg
                f
            ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, meth, _ ->
        Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | None, meth, _ ->
        Helper.LibCall(
            com,
            "async_builder",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let asyncs
    com
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "Start" ->
        Helper.LibCall(
            com,
            "async_",
            "start",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    // Make sure cancellationToken is called as a function and not a getter
    | "get_CancellationToken" ->
        Helper.LibCall(com, "async_", "cancellationToken", t, [], ?loc = r)
        |> Some
    // `catch` cannot be used as a function name in JS
    | "Catch" ->
        Helper.LibCall(
            com,
            "async_",
            "catchAsync",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    // Fable.Core extensions
    | meth ->
        Helper.LibCall(
            com,
            "async_",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some


let paths
    com
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "GetDirectoryName"
    | "GetExtension"
    | "GetFileName"
    | "GetFileNameWithoutExtension"
    | "GetFullPath"
    | "GetRandomFileName"
    | "GetTempFileName"
    | "GetTempPath"
    | "HasExtension" as meth ->
        Helper.LibCall(
            com,
            "path",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let files
    com
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "Copy"
    | "Delete"
    | "Exists"
    | "Move"
    | "ReadAllBytes"
    | "ReadAllLines"
    | "ReadAllText"
    | "WriteAllBytes"
    | "WriteAllLines"
    | "WriteAllText" as meth ->
        Helper.LibCall(
            com,
            "file",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | _ -> None

let tasks
    com
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match thisArg, i.CompiledName with
    | Some x, "GetAwaiter" ->
        Helper.LibCall(
            com,
            "task",
            "get_awaiter",
            t,
            [ x ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, "GetResult" ->
        Helper.LibCall(
            com,
            "task",
            "get_result",
            t,
            [ x ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, "get_Result" ->
        Helper.LibCall(
            com,
            "task",
            "get_result",
            t,
            [ x ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, "RunSynchronously" ->
        Helper.LibCall(
            com,
            "task",
            "run_synchronously",
            t,
            [ x ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, "Start" ->
        Helper.LibCall(
            com,
            "task",
            "start",
            t,
            [ x ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, meth ->
        Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | None, ".ctor" ->
        Helper.LibCall(
            com,
            "task",
            "TaskCompletionSource",
            t,
            [],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | None, meth ->
        Helper.LibCall(
            com,
            "task",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let taskBuilder
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match thisArg, i.CompiledName, args with
    | _, "Singleton", _ ->
        makeImportLib com t "singleton" "task_builder" |> Some
    // For Using we need to cast the argument to IDisposable
    | Some x, "Using", [ arg; f ]
    | Some x, "TaskBuilderBase.Using", [ arg; f ] ->
        Helper.InstanceCall(
            x,
            "Using",
            t,
            [
                arg
                f
            ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, "TaskBuilderBase.Bind", [ arg; f ] ->
        Helper.InstanceCall(
            x,
            "Bind",
            t,
            [
                arg
                f
            ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, "TaskBuilderBase.ReturnFrom", [ arg ] ->
        Helper.InstanceCall(
            x,
            "ReturnFrom",
            t,
            [ arg ],
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | Some x, meth, _ ->
        Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | None, meth, _ ->
        Helper.LibCall(
            com,
            "task_builder",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let guids
    (com: ICompiler)
    (ctx: Context)
    (r: SourceLocation option)
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "NewGuid" -> Helper.LibCall(com, "Guid", "new_guid", t, []) |> Some
    | "Parse" ->
        Helper.LibCall(com, "Guid", "parse", t, args, i.SignatureArgTypes)
        |> Some
    | "TryParse" ->
        Helper.LibCall(com, "Guid", "tryParse", t, args, i.SignatureArgTypes)
        |> Some
    | "ToByteArray" ->
        Helper.LibCall(
            com,
            "Guid",
            "guidToArray",
            t,
            [ thisArg.Value ],
            [ thisArg.Value.Type ]
        )
        |> Some
    | "ToString" when (args.Length = 0) ->
        Helper.GlobalCall("str", t, [ thisArg.Value ], ?loc = r) |> Some
    | "ToString" when (args.Length = 1) ->
        match args with
        | [ StringConst literalFormat ] ->
            match literalFormat with
            | "N"
            | "D"
            | "B"
            | "P"
            | "X" ->
                Helper.LibCall(
                    com,
                    "Guid",
                    "toString",
                    t,
                    args,
                    i.SignatureArgTypes,
                    ?thisArg = thisArg,
                    ?loc = r
                )
                |> Some
            | _ ->
                "Guid.ToString doesn't support a custom format. It only handles \"N\", \"D\", \"B\", \"P\" and \"X\" format."
                |> addError com ctx.InlinePath r

                None
        | _ ->
            Helper.LibCall(
                com,
                "Guid",
                "toString",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
    | ".ctor" ->
        match args with
        | [] -> emptyGuid () |> Some
        | [ ExprType(Array _) ] ->
            Helper.LibCall(
                com,
                "Guid",
                "arrayToGuid",
                t,
                args,
                i.SignatureArgTypes
            )
            |> Some
        | [ StringConst _ ] ->
            Helper.LibCall(com, "Guid", "parse", t, args, i.SignatureArgTypes)
            |> Some
        | [ ExprType String ] ->
            Helper.LibCall(com, "Guid", "parse", t, args, i.SignatureArgTypes)
            |> Some
        | _ -> None
    | _ -> None

let uris
    (com: ICompiler)
    (ctx: Context)
    (r: SourceLocation option)
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | ".ctor" ->
        Helper.LibCall(
            com,
            "Uri",
            "Uri.create",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "TryCreate" ->
        Helper.LibCall(
            com,
            "Uri",
            "Uri.try_create",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "UnescapeDataString" ->
        Helper.LibCall(
            com,
            "Util",
            "unescapeDataString",
            t,
            args,
            i.SignatureArgTypes
        )
        |> Some
    | "EscapeDataString" ->
        Helper.LibCall(
            com,
            "Util",
            "escapeDataString",
            t,
            args,
            i.SignatureArgTypes
        )
        |> Some
    | "EscapeUriString" ->
        Helper.LibCall(
            com,
            "Util",
            "escapeUriString",
            t,
            args,
            i.SignatureArgTypes
        )
        |> Some
    | "get_IsAbsoluteUri"
    | "get_Scheme"
    | "get_Host"
    | "get_AbsolutePath"
    | "get_AbsoluteUri"
    | "get_PathAndQuery"
    | "get_Query"
    | "get_Fragment"
    | "get_OriginalString" ->
        Naming.removeGetSetPrefix i.CompiledName
        |> Naming.lowerFirst
        |> getFieldWith r t thisArg.Value
        |> Some
    | _ -> None

let laziness
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | (".ctor" | "Create"), _, _ ->
        Helper.LibCall(
            com,
            "Util",
            "Lazy",
            t,
            args,
            i.SignatureArgTypes,
            isConstructor = true,
            ?loc = r
        )
        |> Some
    | "CreateFromValue", _, _ ->
        Helper.LibCall(
            com,
            "Util",
            "lazyFromValue",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "Force", Some callee, _ -> getFieldWith r t callee "Value" |> Some
    | ("get_Value" | "get_IsValueCreated"), Some callee, _ ->
        Naming.removeGetSetPrefix i.CompiledName
        |> getFieldWith r t callee
        |> Some
    | _ -> None

let controlExtensions
    (com: ICompiler)
    (ctx: Context)
    (_: SourceLocation option)
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "AddToObservable" -> Some "add"
    | "SubscribeToObservable" -> Some "subscribe"
    | _ -> None
    |> Option.map (fun meth ->
        let args, argTypes =
            thisArg
            |> Option.map (fun thisArg ->
                thisArg :: args, thisArg.Type :: i.SignatureArgTypes
            )
            |> Option.defaultValue (args, i.SignatureArgTypes)
            |> fun (args, argTypes) -> List.rev args, List.rev argTypes

        Helper.LibCall(com, "Observable", meth, t, args, argTypes)
    )

let types
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let returnString r x = StringConstant x |> makeValue r |> Some

    let resolved =
        // Some optimizations when the type is known at compile time
        match thisArg with
        | Some(Value(TypeInfo(exprType, _), exprRange) as thisArg) ->
            match exprType with
            | GenericParam(name = name) ->
                genericTypeInfoError name
                |> addError com ctx.InlinePath exprRange
            | _ -> ()

            match i.CompiledName with
            | "GetInterface" ->
                match exprType, args with
                | DeclaredType(e, genArgs), [ StringConst name ] ->
                    Some(e, genArgs, name, false)
                | DeclaredType(e, genArgs),
                  [ StringConst name; BoolConst ignoreCase ] ->
                    Some(e, genArgs, name, ignoreCase)
                | _ -> None
                |> Option.map (fun (e, genArgs, name, ignoreCase) ->
                    let e = com.GetEntity(e)

                    let genMap =
                        List.zip
                            (e.GenericParameters |> List.map (fun p -> p.Name))
                            genArgs
                        |> Map

                    let comp =
                        if ignoreCase then
                            System.StringComparison.OrdinalIgnoreCase
                        else
                            System.StringComparison.Ordinal

                    e.AllInterfaces
                    |> Seq.tryPick (fun ifc ->
                        let ifcName =
                            getTypeNameFromFullName ifc.Entity.FullName

                        if ifcName.Equals(name, comp) then
                            let genArgs =
                                ifc.GenericArgs
                                |> List.map (
                                    function
                                    | GenericParam(name = name) as gen ->
                                        Map.tryFind name genMap
                                        |> Option.defaultValue gen
                                    | gen -> gen
                                )

                            Some(ifc.Entity, genArgs)
                        else
                            None
                    )
                    |> function
                        | Some(ifcEnt, genArgs) ->
                            DeclaredType(ifcEnt, genArgs) |> makeTypeInfo r
                        | None -> Value(Null t, r)
                )
            | "get_FullName" -> getTypeFullName false exprType |> returnString r
            | "get_Namespace" ->
                let fullname = getTypeFullName false exprType

                match fullname.LastIndexOf(".") with
                | -1 -> "" |> returnString r
                | i -> fullname.Substring(0, i) |> returnString r
            | "get_IsArray" ->
                match exprType with
                | Array _ -> true
                | _ -> false
                |> BoolConstant
                |> makeValue r
                |> Some
            | "get_IsEnum" ->
                match exprType with
                | Number(_, NumberInfo.IsEnum _) -> true
                | _ -> false
                |> BoolConstant
                |> makeValue r
                |> Some
            | "GetElementType" ->
                match exprType with
                | Array(t, _) -> makeTypeInfo r t |> Some
                | _ -> Null t |> makeValue r |> Some
            | "get_IsGenericType" ->
                List.isEmpty exprType.Generics
                |> not
                |> BoolConstant
                |> makeValue r
                |> Some
            | "get_GenericTypeArguments"
            | "GetGenericArguments" ->
                let arVals = exprType.Generics |> List.map (makeTypeInfo r)

                NewArray(ArrayValues arVals, Any, MutableArray)
                |> makeValue r
                |> Some
            | "GetGenericTypeDefinition" ->
                let newGen = exprType.Generics |> List.map (fun _ -> Any)

                let exprType =
                    match exprType with
                    | Option(_, isStruct) -> Option(newGen.Head, isStruct)
                    | Array(_, kind) -> Array(newGen.Head, kind)
                    | List _ -> List newGen.Head
                    | LambdaType _ ->
                        let argTypes, returnType = List.splitLast newGen
                        LambdaType(argTypes.Head, returnType)
                    | DelegateType _ ->
                        let argTypes, returnType = List.splitLast newGen
                        DelegateType(argTypes, returnType)
                    | Tuple(_, isStruct) -> Tuple(newGen, isStruct)
                    | DeclaredType(ent, _) -> DeclaredType(ent, newGen)
                    | t -> t

                makeTypeInfo exprRange exprType |> Some
            | _ -> None
        | _ -> None

    match resolved, thisArg with
    | Some _, _ -> resolved
    | None, Some thisArg ->
        match i.CompiledName with
        | "GetTypeInfo" -> Some thisArg
        | "get_GenericTypeArguments"
        | "GetGenericArguments" ->
            Helper.LibCall(
                com,
                "Reflection",
                "getGenerics",
                t,
                [ thisArg ],
                ?loc = r
            )
            |> Some
        | "MakeGenericType" ->
            Helper.LibCall(
                com,
                "Reflection",
                "makeGenericType",
                t,
                thisArg :: args,
                ?loc = r
            )
            |> Some
        | "get_FullName"
        | "get_Namespace"
        | "get_IsArray"
        | "GetElementType"
        | "get_IsGenericType"
        | "GetGenericTypeDefinition"
        | "get_IsEnum"
        | "GetEnumUnderlyingType"
        | "GetEnumValues"
        | "GetEnumNames"
        | "IsSubclassOf"
        | "IsInstanceOfType" ->
            let meth =
                Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst

            Helper.LibCall(
                com,
                "Reflection",
                meth,
                t,
                thisArg :: args,
                ?loc = r
            )
            |> Some
        | _ -> None
    | None, None -> None

let fsharpType
    com
    methName
    (r: SourceLocation option)
    t
    (i: CallInfo)
    (args: Expr list)
    =
    match methName with
    | "MakeTupleType" ->
        Helper.LibCall(
            com,
            "Reflection",
            "tuple_type",
            t,
            args,
            i.SignatureArgTypes,
            hasSpread = true,
            ?loc = r
        )
        |> Some
    // Prevent name clash with FSharpValue.GetRecordFields
    | "GetRecordFields" ->
        Helper.LibCall(
            com,
            "Reflection",
            "getRecordElements",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "GetUnionCases"
    | "GetTupleElements"
    | "GetFunctionElements"
    | "IsUnion"
    | "IsRecord"
    | "IsTuple"
    | "IsFunction" ->
        Helper.LibCall(
            com,
            "Reflection",
            Naming.lowerFirst methName,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "IsExceptionRepresentation"
    | "GetExceptionFields" -> None // TODO!!!
    | _ -> None

let fsharpValue
    com
    methName
    (r: SourceLocation option)
    t
    (i: CallInfo)
    (args: Expr list)
    =
    match methName with
    | "GetUnionFields"
    | "GetRecordFields"
    | "GetRecordField"
    | "GetTupleFields"
    | "GetTupleField"
    | "MakeUnion"
    | "MakeRecord"
    | "MakeTuple" ->
        Helper.LibCall(
            com,
            "Reflection",
            Naming.lowerFirst methName,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "GetExceptionFields" -> None // TODO!!!
    | _ -> None

let tryField com returnTyp ownerTyp fieldName =
    // printfn "tryField %A %A %A" returnTyp ownerTyp fieldName
    match ownerTyp, fieldName with
    | Number(Decimal, _), _ ->
        Helper.LibValue(com, "decimal", "get_" + fieldName, returnTyp) |> Some
    | String, "Empty" -> makeStrConst "" |> Some
    | Builtin BclGuid, "Empty" -> emptyGuid () |> Some
    | Builtin BclTimeSpan, "Zero" ->
        Helper.LibCall(
            com,
            "time_span",
            "create",
            returnTyp,
            [ makeIntConst 0 ]
        )
        |> Some
    | Builtin BclDateTime, ("MaxValue" | "MinValue") ->
        Helper.LibCall(
            com,
            coreModFor BclDateTime,
            Naming.lowerFirst fieldName,
            returnTyp,
            []
        )
        |> Some
    | Builtin BclDateTimeOffset, ("MaxValue" | "MinValue") ->
        Helper.LibCall(
            com,
            coreModFor BclDateTimeOffset,
            Naming.lowerFirst fieldName,
            returnTyp,
            []
        )
        |> Some
    | DeclaredType(ent, genArgs), fieldName ->
        match ent.FullName with
        | "System.BitConverter" ->
            Helper.LibCall(
                com,
                "bit_converter",
                Naming.lowerFirst fieldName,
                returnTyp,
                []
            )
            |> Some
        | "System.Diagnostics.Stopwatch" ->
            Helper.LibCall(
                com,
                "diagnostics",
                Naming.lowerFirst fieldName,
                returnTyp,
                []
            )
            |> Some
        | _ -> None
    | _ -> None

let private replacedModules =
    dict
        [
            "System.Math", operators
            "System.MathF", operators
            "Microsoft.FSharp.Core.Operators", operators
            "Microsoft.FSharp.Core.Operators.Checked", operators
            "Microsoft.FSharp.Core.Operators.Unchecked", unchecked
            "Microsoft.FSharp.Core.Operators.OperatorIntrinsics",
            intrinsicFunctions
            "Microsoft.FSharp.Core.ExtraTopLevelOperators", operators
            "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions",
            intrinsicFunctions
            "Microsoft.FSharp.Core.LanguagePrimitives", languagePrimitives
            "Microsoft.FSharp.Core.LanguagePrimitives.HashCompare",
            languagePrimitives
            "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators",
            operators
            "System.Runtime.CompilerServices.RuntimeHelpers", runtimeHelpers
            "System.Runtime.ExceptionServices.ExceptionDispatchInfo",
            exceptionDispatchInfo
            Types.char, chars
            Types.string, strings
            "Microsoft.FSharp.Core.StringModule", stringModule
            "System.FormattableString", formattableString
            "System.Runtime.CompilerServices.FormattableStringFactory",
            formattableString
            "System.Text.StringBuilder", bclType
            Types.array, arrays
            Types.list, lists
            "Microsoft.FSharp.Collections.ArrayModule", arrayModule
            "Microsoft.FSharp.Collections.ListModule", listModule
            "Microsoft.FSharp.Collections.HashIdentity", fsharpModule
            "Microsoft.FSharp.Collections.ComparisonIdentity", fsharpModule
            "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers", seqModule
            "Microsoft.FSharp.Collections.SeqModule", seqModule
            Types.keyValuePair, keyValuePairs
            "System.Collections.Generic.Comparer`1", bclType
            "System.Collections.Generic.EqualityComparer`1", bclType
            Types.dictionary, dictionaries
            Types.idictionary, dictionaries
            Types.ireadonlydictionary, dictionaries
            Types.ienumerableGeneric, enumerables
            Types.ienumerable, enumerables
            Types.valueCollection, enumerables
            Types.keyCollection, enumerables
            "System.Collections.Generic.Dictionary`2.Enumerator", enumerators
            "System.Collections.Generic.Dictionary`2.ValueCollection.Enumerator",
            enumerators
            "System.Collections.Generic.Dictionary`2.KeyCollection.Enumerator",
            enumerators
            "System.Collections.Generic.List`1.Enumerator", enumerators
            "System.Collections.Generic.HashSet`1.Enumerator", enumerators
            "System.CharEnumerator", enumerators
            Types.resizeArray, resizeArrays
            "System.Collections.Generic.IList`1", resizeArrays
            "System.Collections.IList", resizeArrays
            Types.icollectionGeneric, resizeArrays
            Types.icollection, resizeArrays
            "System.Collections.Generic.CollectionExtensions",
            collectionExtensions
            "System.ReadOnlySpan`1", readOnlySpans
            Types.hashset, hashSets
            Types.stack, bclType
            Types.queue, bclType
            Types.iset, hashSets
            Types.option, options false
            Types.valueOption, options true
            Types.nullable, nullables
            "Microsoft.FSharp.Core.OptionModule", optionModule false
            "Microsoft.FSharp.Core.ValueOption", optionModule true
            "Microsoft.FSharp.Core.ResultModule", results
            Types.bigint, bigints
            "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI", bigints
            Types.refCell, refCells
            Types.object, objects
            Types.valueType, valueTypes
            Types.enum_, enums
            "System.BitConverter", bitConvert
            Types.bool, parseBool
            Types.int8, parseNum
            Types.uint8, parseNum
            Types.int16, parseNum
            Types.uint16, parseNum
            Types.int32, parseNum
            Types.uint32, parseNum
            Types.int64, parseNum
            Types.uint64, parseNum
            Types.int128, parseNum
            Types.uint128, parseNum
            Types.float16, parseNum
            Types.float32, parseNum
            Types.float64, parseNum
            Types.decimal, decimals
            "System.Convert", convert
            "System.Console", console
            "System.Diagnostics.Debug", debug
            "System.Diagnostics.Debugger", debug
            "System.Diagnostics.Stopwatch", stopwatch
            Types.datetime, dates
            Types.datetimeOffset, dates
            Types.timespan, timeSpans
            "System.Timers.Timer", timers
            "System.Environment", systemEnv
            "System.Globalization.CultureInfo", globalization
            "System.IO.File", files
            "System.IO.Path", paths
            "System.Random", random
            "System.Threading.CancellationToken", cancels
            "System.Threading.CancellationTokenSource", cancels
            "System.Threading.Monitor", monitor
            "System.Threading.Thread", thread
            Types.task, tasks
            Types.taskGeneric, tasks
            "System.Threading.Tasks.TaskCompletionSource`1", tasks
            "System.Runtime.CompilerServices.TaskAwaiter`1", tasks
            "System.Activator", activator
            "System.Text.Encoding", encoding
            "System.Text.UnicodeEncoding", encoding
            "System.Text.UTF8Encoding", encoding
            Types.regexCapture, regex
            Types.regexMatch, regex
            Types.regexGroup, regex
            Types.regexMatchCollection, regex
            Types.regexGroupCollection, regex
            Types.regex, regex
            Types.fsharpSet, sets
            "Microsoft.FSharp.Collections.SetModule", setModule
            Types.fsharpMap, maps
            "Microsoft.FSharp.Collections.MapModule", mapModule
            "Microsoft.FSharp.Control.FSharpMailboxProcessor`1", mailbox
            "Microsoft.FSharp.Control.FSharpAsyncReplyChannel`1", mailbox
            "Microsoft.FSharp.Control.FSharpAsyncBuilder", asyncBuilder
            "Microsoft.FSharp.Control.AsyncActivation`1", asyncBuilder
            "Microsoft.FSharp.Control.FSharpAsync", asyncs
            "Microsoft.FSharp.Control.AsyncPrimitives", asyncs
            "Microsoft.FSharp.Control.TaskBuilder", tasks
            "Microsoft.FSharp.Control.TaskBuilderBase", taskBuilder
            "Microsoft.FSharp.Control.TaskBuilderModule", taskBuilder
            "Microsoft.FSharp.Control.TaskBuilderExtensions.HighPriority",
            taskBuilder
            "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority",
            taskBuilder
            Types.guid, guids
            "System.Uri", uris
            "System.Lazy`1", laziness
            "Microsoft.FSharp.Control.Lazy", laziness
            "Microsoft.FSharp.Control.LazyExtensions", laziness
            "Microsoft.FSharp.Control.CommonExtensions", controlExtensions
            "Microsoft.FSharp.Control.FSharpEvent`1", events
            "Microsoft.FSharp.Control.FSharpEvent`2", events
            "Microsoft.FSharp.Control.EventModule", events
            "Microsoft.FSharp.Control.ObservableModule", observable
            Types.type_, types
            "System.Reflection.TypeInfo", types
        ]

let tryCall
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    // printfn "Module: %A"  info.DeclaringEntityFullName
    match info.DeclaringEntityFullName with
    | Patterns.DicContains replacedModules replacement ->
        replacement com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings" ->
        errorStrings info.CompiledName
    | Types.printfModule
    | Naming.StartsWith Types.printfFormat _ ->
        fsFormat com ctx r t info thisArg args
    | Naming.StartsWith "Fable.Core." _ ->
        fableCoreLib com ctx r t info thisArg args
    | Naming.EndsWith "Exception" _ -> exceptions com ctx r t info thisArg args
    | "System.Timers.ElapsedEventArgs" -> thisArg // only signalTime is available here
    | Naming.StartsWith "System.Tuple" _
    | Naming.StartsWith "System.ValueTuple" _ ->
        tuples com ctx r t info thisArg args
    | Naming.StartsWith "System.Action" _
    | Naming.StartsWith "System.Func" _
    | Naming.StartsWith "Microsoft.FSharp.Core.FSharpFunc" _
    | Naming.StartsWith "Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc" _ ->
        funcs com ctx r t info thisArg args
    | "Microsoft.FSharp.Reflection.FSharpType" ->
        fsharpType com info.CompiledName r t info args
    | "Microsoft.FSharp.Reflection.FSharpValue" ->
        fsharpValue com info.CompiledName r t info args
    | "Microsoft.FSharp.Reflection.FSharpReflectionExtensions" ->
        // In netcore F# Reflection methods become extensions
        // with names like `FSharpType.GetExceptionFields.Static`
        let isFSharpType =
            info.CompiledName.StartsWith("FSharpType", StringComparison.Ordinal)

        let methName = info.CompiledName |> Naming.extensionMethodName

        if isFSharpType then
            fsharpType com methName r t info args
        else
            fsharpValue com methName r t info args
    | "Microsoft.FSharp.Reflection.UnionCaseInfo"
    | "System.Reflection.PropertyInfo"
    | "System.Reflection.ParameterInfo"
    | "System.Reflection.MethodBase"
    | "System.Reflection.MethodInfo"
    | "System.Reflection.MemberInfo" ->
        match thisArg, info.CompiledName with
        | Some c, "get_Tag" -> makeStrConst "tag" |> getExpr r t c |> Some
        | Some c, "get_ReturnType" ->
            makeStrConst "returnType" |> getExpr r t c |> Some
        | Some c, "GetParameters" ->
            makeStrConst "parameters" |> getExpr r t c |> Some
        | Some c, ("get_PropertyType" | "get_ParameterType") ->
            makeIntConst 1 |> getExpr r t c |> Some
        | Some c, "GetFields" ->
            Helper.LibCall(
                com,
                "Reflection",
                "getUnionCaseFields",
                t,
                [ c ],
                ?loc = r
            )
            |> Some
        | Some c, "GetValue" ->
            Helper.LibCall(
                com,
                "Reflection",
                "getValue",
                t,
                c :: args,
                ?loc = r
            )
            |> Some
        | Some c, "get_Name" ->
            match c with
            | Value(TypeInfo(exprType, _), loc) ->
                getTypeName com ctx loc exprType
                |> StringConstant
                |> makeValue r
                |> Some
            | c ->
                Helper.LibCall(com, "Reflection", "name", t, [ c ], ?loc = r)
                |> Some
        | _ -> None
    | _ -> None

let tryBaseConstructor
    com
    ctx
    (ent: EntityRef)
    (argTypes: Lazy<Type list>)
    genArgs
    args
    =
    match ent.FullName with
    | Types.exception_ -> Some(makeIdentExpr ("Exception"), args)
    | Types.attribute -> Some(makeImportLib com Any "Attribute" "Types", args)
    | Types.dictionary ->
        let args =
            match argTypes.Value, args with
            | ([] | [ Number _ ]), _ ->
                [
                    makeArray Any []
                    makeEqualityComparer com ctx (Seq.head genArgs)
                ]
            | [ IDictionary ], [ arg ] ->
                [
                    arg
                    makeEqualityComparer com ctx (Seq.head genArgs)
                ]
            | [ IDictionary; IEqualityComparer ], [ arg; eqComp ] ->
                [
                    arg
                    makeComparerFromEqualityComparer eqComp
                ]
            | [ IEqualityComparer ], [ eqComp ]
            | [ Number _; IEqualityComparer ], [ _; eqComp ] ->
                [
                    makeArray Any []
                    makeComparerFromEqualityComparer eqComp
                ]
            | _ -> FableError "Unexpected dictionary constructor" |> raise

        let entityName = Naming.cleanNameAsPyIdentifier "Dictionary"
        Some(makeImportLib com Any entityName "MutableMap", args)
    | Types.hashset ->
        let args =
            match argTypes.Value, args with
            | [], _ ->
                [
                    makeArray Any []
                    makeEqualityComparer com ctx (Seq.head genArgs)
                ]
            | [ IEnumerable ], [ arg ] ->
                [
                    arg
                    makeEqualityComparer com ctx (Seq.head genArgs)
                ]
            | [ IEnumerable; IEqualityComparer ], [ arg; eqComp ] ->
                [
                    arg
                    makeComparerFromEqualityComparer eqComp
                ]
            | [ IEqualityComparer ], [ eqComp ] ->
                [
                    makeArray Any []
                    makeComparerFromEqualityComparer eqComp
                ]
            | _ -> FableError "Unexpected hashset constructor" |> raise

        let entityName = Naming.cleanNameAsPyIdentifier "HashSet"
        Some(makeImportLib com Any entityName "MutableSet", args)
    | _ -> None

let tryType =
    function
    | Boolean -> Some(Types.bool, parseBool, [])
    | Number(kind, info) ->
        let f =
            match kind with
            | Decimal -> decimals
            | BigInt -> bigints
            | _ -> parseNum

        Some(getNumberFullName false kind info, f, [])
    | String -> Some(Types.string, strings, [])
    | Tuple(genArgs, _) as t -> Some(getTypeFullName false t, tuples, genArgs)
    | Option(genArg, isStruct) ->
        if isStruct then
            Some(Types.valueOption, options true, [ genArg ])
        else
            Some(Types.option, options false, [ genArg ])
    | Array(genArg, _) -> Some(Types.array, arrays, [ genArg ])
    | List genArg -> Some(Types.list, lists, [ genArg ])
    | Builtin kind ->
        match kind with
        | BclGuid -> Some(Types.guid, guids, [])
        | BclTimeSpan -> Some(Types.timespan, timeSpans, [])
        | BclDateTime -> Some(Types.datetime, dates, [])
        | BclDateTimeOffset -> Some(Types.datetimeOffset, dates, [])
        | BclTimer -> Some("System.Timers.Timer", timers, [])
        | BclHashSet genArg -> Some(Types.hashset, hashSets, [ genArg ])
        | BclDictionary(key, value) ->
            Some(
                Types.dictionary,
                dictionaries,
                [
                    key
                    value
                ]
            )
        | BclKeyValuePair(key, value) ->
            Some(
                Types.keyValuePair,
                keyValuePairs,
                [
                    key
                    value
                ]
            )
        | FSharpMap(key, value) ->
            Some(
                Types.fsharpMap,
                maps,
                [
                    key
                    value
                ]
            )
        | FSharpSet genArg -> Some(Types.fsharpSet, sets, [ genArg ])
        | FSharpResult(genArg1, genArg2) ->
            Some(
                Types.result,
                results,
                [
                    genArg1
                    genArg2
                ]
            )
        | FSharpChoice genArgs ->
            Some(
                $"{Types.choiceNonGeneric}`{List.length genArgs}",
                results,
                genArgs
            )
        | FSharpReference genArg -> Some(Types.refCell, refCells, [ genArg ])
        | BclDateOnly
        | BclTimeOnly -> None
    | _ -> None
