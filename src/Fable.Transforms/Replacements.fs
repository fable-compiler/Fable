module Fable.Transforms.JS.Replacements

#nowarn "1182"

open System
open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

let (|Floats|_|) =
    function
    | Float16
    | Float32
    | Float64 as kind -> Some kind
    | _ -> None

let (|Integers|_|) =
    function
    | Int8
    | UInt8
    | Int16
    | UInt16
    | Int32
    | UInt32 as kind -> Some kind
    | _ -> None

let (|BigIntegers|_|) =
    function
    | Int64
    | UInt64
    | Int128
    | UInt128
    | NativeInt
    | UNativeInt
    | BigInt as kind -> Some kind
    | _ -> None

let (|Numbers|_|) =
    function
    | Integers kind -> Some kind
    | Floats kind -> Some kind
    | _ -> None

let (|TypedArrayCompatible|_|) (com: Compiler) (arrayKind: ArrayKind) t =
    match arrayKind, t with
    | ResizeArray, _ -> None
    | _, Number(kind, _) when com.Options.TypedArrays ->
        match kind with
        | Int8 -> Some "Int8Array"
        | UInt8 when com.Options.ClampByteArrays -> Some "Uint8ClampedArray"
        | UInt8 -> Some "Uint8Array"
        | Int16 -> Some "Int16Array"
        | UInt16 -> Some "Uint16Array"
        | Int32 -> Some "Int32Array"
        | UInt32 -> Some "Uint32Array"
        | Int64 -> Some "BigInt64Array"
        | UInt64 -> Some "BigUint64Array"
        | Float32 -> Some "Float32Array"
        | Float64 -> Some "Float64Array"

        | Float16
        | Int128
        | UInt128
        | NativeInt
        | UNativeInt
        | Decimal
        | BigInt -> None
    | _ -> None

let error msg =
    Helper.ConstructorCall(makeIdentExpr "Error", Any, [ msg ])

let coreModFor =
    function
    | BclGuid -> "Guid"
    | BclDateTime -> "Date"
    | BclDateTimeOffset -> "DateOffset"
    | BclDateOnly -> "DateOnly"
    | BclTimeOnly -> "TimeOnly"
    | BclTimer -> "Timer"
    | BclTimeSpan -> "TimeSpan"
    | FSharpSet _ -> "Set"
    | FSharpMap _ -> "Map"
    | FSharpResult _ -> "Choice"
    | FSharpChoice _ -> "Choice"
    | FSharpReference _ -> "Types"
    | BclHashSet _ -> "MutableSet"
    | BclDictionary _ -> "MutableMap"
    | BclKeyValuePair _ -> FableError "Cannot decide core module" |> raise

let makeDecimal com r t (x: decimal) =
    let str = x.ToString(System.Globalization.CultureInfo.InvariantCulture)

    Helper.LibCall(
        com,
        "Decimal",
        "default",
        t,
        [ makeStrConst str ],
        isConstructor = true,
        ?loc = r
    )

let makeDecimalFromExpr com r t (e: Expr) =
    Helper.LibCall(
        com,
        "Decimal",
        "default",
        t,
        [ e ],
        isConstructor = true,
        ?loc = r
    )

let createAtom com (value: Expr) =
    let typ = value.Type

    Helper.LibCall(
        com,
        "Util",
        "createAtom",
        typ,
        [ value ],
        [ typ ],
        genArgs = [ typ ]
    )

let getRefCell com r typ (expr: Expr) = getFieldWith r typ expr "contents"

let setRefCell com r (expr: Expr) (value: Expr) =
    setExpr r expr (makeStrConst "contents") value

let makeRefCell com r genArg args =
    let typ = makeFSharpCoreType [ genArg ] Types.refCell

    Helper.LibCall(
        com,
        "Types",
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

let toChar (arg: Expr) =
    match arg.Type with
    | Char -> arg
    | String -> TypeCast(arg, Char)
    | _ -> Helper.GlobalCall("String", Char, [ arg ], memb = "fromCharCode")

let toString com (ctx: Context) r (args: Expr list) =
    match args with
    | [] ->
        "toString is called with empty args"
        |> addErrorAndReturnNull com ctx.InlinePath r
    | head :: tail ->
        match head.Type with
        | String -> head
        | Char -> TypeCast(head, String)
        | Builtin BclGuid when tail.IsEmpty -> head
        | Builtin(BclGuid | BclTimeSpan | BclTimeOnly | BclDateOnly as bt) ->
            Helper.LibCall(com, coreModFor bt, "toString", String, args)
        | Number(Int16, _) ->
            Helper.LibCall(com, "Util", "int16ToString", String, args)
        | Number(Int32, _) ->
            Helper.LibCall(com, "Util", "int32ToString", String, args)
        | Number(Int64, _) ->
            Helper.LibCall(com, "Util", "int64ToString", String, args)
        | Number(NativeInt, _) ->
            Helper.LibCall(com, "Util", "int64ToString", String, args)
        | Number(BigInt, _) ->
            Helper.LibCall(com, "BigInt", "toString", String, args)
        | Number(Decimal, _) ->
            Helper.LibCall(com, "Decimal", "toString", String, args)
        | Number _ -> Helper.InstanceCall(head, "toString", String, tail)
        | Array _
        | List _ ->
            Helper.LibCall(
                com,
                "Types",
                "seqToString",
                String,
                [ head ],
                ?loc = r
            )
        // | DeclaredType(ent, _) when ent.IsFSharpUnion || ent.IsFSharpRecord || ent.IsValueType ->
        //     Helper.InstanceCall(head, "toString", String, [], ?loc=r)
        // | DeclaredType(ent, _) ->
        | _ ->
            Helper.LibCall(com, "Types", "toString", String, [ head ], ?loc = r)

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
        Helper.InstanceCall(
            args.Head,
            "charCodeAt",
            Int32.Number,
            [ makeIntConst 0 ]
        )
    | String -> Helper.LibCall(com, "Double", "parse", targetType, args)
    | Number(kind, _) ->
        match kind with
        | Decimal ->
            Helper.LibCall(com, "Decimal", "toNumber", targetType, args)
        | BigIntegers _ ->
            Helper.LibCall(com, "BigInt", "toFloat64", targetType, args)
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
        Helper.InstanceCall(
            args.Head,
            "charCodeAt",
            Int32.Number,
            [ makeIntConst 0 ]
        )
        |> makeDecimalFromExpr com r targetType
    | String -> makeDecimalFromExpr com r targetType args.Head
    | Number(kind, _) ->
        match kind with
        | Decimal -> args.Head
        | BigIntegers _ ->
            Helper.LibCall(com, "BigInt", "toDecimal", Float64.Number, args)
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

let wrapLong com (ctx: Context) r t (arg: Expr) : Expr =
    match t with
    | Number(kind, _) ->
        let toMeth = "to" + kind.ToString()
        Helper.LibCall(com, "BigInt", toMeth, t, [ arg ])
    | _ ->
        addWarning com ctx.InlinePath r "Unexpected conversion to long"
        TypeCast(arg, t)

let toLong com (ctx: Context) r targetType (args: Expr list) : Expr =
    let sourceType = args.Head.Type

    match sourceType, targetType with
    | Char, _ ->
        Helper.LibCall(com, "BigInt", "fromChar", targetType, args, ?loc = r)
        |> wrapLong com ctx r targetType
    | String, _ ->
        stringToInt com ctx r targetType args |> wrapLong com ctx r targetType
    | Number(fromKind, _), Number(toKind, _) ->
        let fromMeth = "from" + fromKind.ToString()

        Helper.LibCall(com, "BigInt", fromMeth, BigInt.Number, args, ?loc = r)
        |> wrapLong com ctx r targetType
    | _ ->
        addWarning
            com
            ctx.InlinePath
            r
            "Cannot make conversion because source type is unknown"

        TypeCast(args.Head, targetType)

let emitIntCast toKind arg =
    match toKind with
    | Int8 -> emitExpr None Int8.Number [ arg ] "($0 + 0x80 & 0xFF) - 0x80"
    | Int16 ->
        emitExpr None Int16.Number [ arg ] "($0 + 0x8000 & 0xFFFF) - 0x8000"
    | Int32 -> fastIntFloor arg
    | UInt8 -> emitExpr None UInt8.Number [ arg ] "$0 & 0xFF"
    | UInt16 -> emitExpr None UInt16.Number [ arg ] "$0 & 0xFFFF"
    | UInt32 -> emitExpr None UInt32.Number [ arg ] "$0 >>> 0"
    | _ -> FableError $"Unexpected non-integer type %A{toKind}" |> raise

/// Conversion to integers (excluding longs and bigints)
let toInt com (ctx: Context) r targetType (args: Expr list) =
    let sourceType = args.Head.Type

    match sourceType, targetType with
    | Char, _ ->
        match targetType, args with
        | Number(kind, _), Value(CharConstant c, r) :: _ ->
            Value(NumberConstant(c, kind, NumberInfo.Empty), r)
        | _ ->
            Helper.InstanceCall(
                args.Head,
                "charCodeAt",
                targetType,
                [ makeIntConst 0 ]
            )
    | String, _ -> stringToInt com ctx r targetType args
    | Number(fromKind, _), Number(toKind, _) ->
        if needToCast fromKind toKind then
            match fromKind with
            | BigIntegers _ ->
                let meth = "to" + toKind.ToString()
                Helper.LibCall(com, "BigInt", meth, targetType, args)
            | Decimal ->
                Helper.LibCall(com, "Decimal", "toNumber", targetType, args)
            | _ -> args.Head
            |> emitIntCast toKind
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
                "Decimal",
                "toNumber",
                Float64.Number,
                [ args.Head ]
            )

        let rounded =
            Helper.LibCall(com, "Util", "round", Float64.Number, [ n ])

        rounded :: args.Tail
    | Number(Floats _, _) ->
        let rounded =
            Helper.LibCall(com, "Util", "round", Float64.Number, [ args.Head ])

        rounded :: args.Tail
    | _ -> args

let toList com returnType expr =
    Helper.LibCall(com, "List", "ofSeq", returnType, [ expr ])

let stringToCharArray e =
    Helper.InstanceCall(
        e,
        "split",
        Array(Char, MutableArray),
        [ makeStrConst "" ]
    )

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
            | Number(Integers _, _) :: _ ->
                binOp BinaryDivide left right |> fastIntFloor
            | _ -> binOp BinaryDivide left right
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
                    "Int32",
                    "op_UnaryNegation_Int8",
                    t,
                    args,
                    ?loc = r
                )
            | Number(Int16, _) :: _ ->
                Helper.LibCall(
                    com,
                    "Int32",
                    "op_UnaryNegation_Int16",
                    t,
                    args,
                    ?loc = r
                )
            | Number(Int32, _) :: _ ->
                Helper.LibCall(
                    com,
                    "Int32",
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
    | Number(Decimal, _) :: _ ->
        let opName =
            if opName = Operators.divideByInt then
                Operators.division
            else
                opName

        Helper.LibCall(com, "Decimal", opName, t, args, argTypes, ?loc = r)
    | Number(BigIntegers kind, _) :: _ ->
        let op =
            Helper.LibCall(com, "BigInt", opName, t, args, argTypes, ?loc = r)

        if kind = BigInt then
            op
        else
            wrapLong com ctx r t op
    | Builtin(BclDateTime | BclDateTimeOffset | BclDateOnly as bt) :: _ ->
        Helper.LibCall(com, coreModFor bt, opName, t, args, argTypes, ?loc = r)
    | Builtin(FSharpSet _) :: _ ->
        let mangledName =
            Naming.buildNameWithoutSanitationFrom "FSharpSet" true opName ""

        Helper.LibCall(com, "Set", mangledName, t, args, argTypes, ?loc = r)
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
    | Number(Numbers _, _) -> true
    // TODO: Non-record/union declared types without custom equality
    // should be compatible with JS comparison
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
        | Number(Decimal, _) -> "safeHash"
        | Number(BigIntegers _, _) -> "bigintHash"
        | Number(Numbers _, _) -> "numberHash"
        | Builtin BclTimeSpan
        | Builtin BclTimeOnly -> "numberHash"
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
        | Number(Decimal, _) -> "fastStructuralHash"
        | Number(BigIntegers _, _) -> "bigintHash"
        | Number(Numbers _, _) -> "numberHash"
        | Builtin BclTimeSpan
        | Builtin BclTimeOnly -> "numberHash"
        | List _ -> "safeHash"
        // TODO: Get hash functions of the generic arguments
        // for better performance when using tuples as map keys
        | Tuple _
        | Array _ -> "arrayHash"
        | Builtin(BclDateTime | BclDateTimeOffset | BclDateOnly) -> "dateHash"
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
            "Decimal",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | Number(BigIntegers _, _) ->
        Helper.LibCall(
            com,
            "BigInt",
            "equals",
            Boolean,
            [
                left
                right
            ],
            ?loc = r
        )
        |> is equal
    | Builtin(BclGuid | BclTimeSpan | BclTimeOnly)
    | Boolean
    | Char
    | String
    | Number _
    | MetaType ->
        let op =
            if equal then
                BinaryEqual
            else
                BinaryUnequal

        makeBinOp r Boolean left right op
    // Use BinaryEquals for MetaType to have a change of optimization in FableTransforms.operationReduction
    // We will call Reflection.equals in the Fable2Babel step
    //| MetaType -> Helper.LibCall(com, "Reflection", "equals", Boolean, [left; right], ?loc=r) |> is equal
    | Builtin(BclDateTime | BclDateTimeOffset | BclDateOnly) ->
        Helper.LibCall(
            com,
            "Date",
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
            "Util",
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
            "Array",
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
            "Util",
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
            "Util",
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
            "Util",
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
            "Decimal",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Number(BigIntegers _, _) ->
        Helper.LibCall(
            com,
            "BigInt",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Builtin(BclGuid | BclTimeSpan | BclTimeOnly)
    | Boolean
    | Char
    | String
    | Number _ ->
        Helper.LibCall(
            com,
            "Util",
            "comparePrimitives",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Builtin(BclDateTime | BclDateTimeOffset | BclDateOnly) ->
        Helper.LibCall(
            com,
            "Date",
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
            "Util",
            "compare",
            t,
            [
                left
                right
            ],
            ?loc = r
        )
    | Array(t, _) ->
        let f = makeComparerFunction com ctx t
        // Note Array.compareWith doesn't check the length first, see #2961
        Helper.LibCall(
            com,
            "Array",
            "compareTo",
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
            "Util",
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
            "Util",
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
            "Util",
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
    Helper.LibCall(com, "Set", Naming.lowerFirst methName, t, args, ?loc = r)

/// Adds comparer as last argument for map creator methods
let makeMap (com: ICompiler) ctx r t methName args genArg =
    let args = args @ [ makeComparer com ctx genArg ]
    Helper.LibCall(com, "Map", Naming.lowerFirst methName, t, args, ?loc = r)

let makeDictionaryWithComparer com r t sourceSeq comparer =
    Helper.LibCall(
        com,
        "MutableMap",
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
            "Map",
            t,
            [ sourceSeq ],
            isConstructor = true,
            ?loc = r
        )

let makeHashSetWithComparer com r t sourceSeq comparer =
    Helper.LibCall(
        com,
        "MutableSet",
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
            "Set",
            t,
            [ sourceSeq ],
            isConstructor = true,
            ?loc = r
        )

let rec getZero (com: ICompiler) (ctx: Context) (t: Type) =
    match t with
    | Boolean -> makeBoolConst false
    | Char
    | String -> makeStrConst "" // TODO: Use null for string?
    | Number(kind, uom) ->
        NumberConstant(getBoxedZero kind, kind, uom) |> makeValue None
    | Builtin(BclTimeSpan | BclTimeOnly) -> makeIntConst 0 // TODO: Type cast
    | Builtin BclDateTime as t -> Helper.LibCall(com, "Date", "minValue", t, [])
    | Builtin BclDateTimeOffset as t ->
        Helper.LibCall(com, "DateOffset", "minValue", t, [])
    | Builtin BclDateOnly as t ->
        Helper.LibCall(com, "DateOnly", "minValue", t, [])
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

let getOne (com: ICompiler) (ctx: Context) (t: Type) =
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

let makePojoFromLambda com arg =
    let rec flattenSequential =
        function
        | Sequential statements -> List.collect flattenSequential statements
        | e -> [ e ]

    match arg with
    | Lambda(_, lambdaBody, _) ->
        (flattenSequential lambdaBody, Some [])
        ||> List.foldBack (fun statement acc ->
            match acc, statement with
            | Some acc, Set(_, FieldSet(fieldName), _, value, _) ->
                objValue (fieldName, value) :: acc |> Some
            | _ -> None
        )
    | _ -> None
    |> Option.map (fun members -> ObjectExpr(members, Any, None))
    |> Option.defaultWith (fun () ->
        Helper.LibCall(com, "Util", "jsOptions", Any, [ arg ])
    )

let makePojo (com: Compiler) caseRule keyValueList =
    let makeObjMember caseRule name values =
        let value =
            match values with
            | [] -> makeBoolConst true
            | [ value ] -> value
            | values ->
                Value(NewArray(ArrayValues values, Any, MutableArray), None)

        objValue (Naming.applyCaseRule caseRule name, value)

    // let rec findKeyValueList scope identName =
    //     match scope with
    //     | [] -> None
    //     | (_,ident2,expr)::prevScope ->
    //         if identName = ident2.Name then
    //             match expr with
    //             | Some(ArrayOrListLiteral(kvs,_)) -> Some kvs
    //             | Some(MaybeCasted(IdentExpr ident)) -> findKeyValueList prevScope ident.Name
    //             | _ -> None
    //         else findKeyValueList prevScope identName

    let caseRule =
        match caseRule with
        | Some(NumberConst(:? int as rule, _, _)) -> Some rule
        | _ -> None
        |> Option.map enum
        |> Option.defaultValue Fable.Core.CaseRules.None

    match keyValueList with
    | ArrayOrListLiteral(kvs, _) -> Some kvs
    // | MaybeCasted(IdentExpr ident) -> findKeyValueList ctx.Scope ident.Name
    | _ -> None
    |> Option.bind (fun kvs ->
        (kvs, Some [])
        ||> List.foldBack (fun m acc ->
            match acc, m with
            // Try to get the member key and value at compile time for unions and tuples
            | Some acc, MaybeCasted(Value(NewUnion(values, uci, ent, _), _)) ->
                let uci = com.GetEntity(ent).UnionCases |> List.item uci
                let name = defaultArg uci.CompiledName uci.Name
                makeObjMember caseRule name values :: acc |> Some
            | Some acc,
              MaybeCasted(Value(NewTuple((StringConst name) :: values, _), _)) ->
                match values with
                | [ MaybeCasted(Value(NewOption(None, _, _), _)) ] -> Some acc
                | values ->
                    // Don't change the case for tuples in disguise
                    makeObjMember Core.CaseRules.None name values :: acc
                    |> Some
            | _ -> None
        )
    )
    |> Option.map (fun members -> ObjectExpr(members, Any, None))

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
                    args @ [ makeIdentExpr consName ]
                | _ -> args
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
        makeImportLib com Any "FSharpResult$2" "Choice" |> Some
    | BuiltinDefinition(FSharpChoice genArgs) ->
        let membName = $"FSharpChoice${List.length genArgs}"
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
    | Types.exception_ -> makeIdentExpr "Error" |> Some
    | Types.systemException ->
        makeImportLib com Any "SystemException" "SystemException" |> Some
    | Types.timeoutException ->
        makeImportLib com Any "TimeoutException" "SystemException" |> Some
    | Types.attribute -> makeImportLib com Any "Attribute" "Types" |> Some
    | "System.Uri" -> makeImportLib com Any "Uri" "Uri" |> Some
    | "Microsoft.FSharp.Control.FSharpAsyncReplyChannel`1" ->
        makeImportLib com Any "AsyncReplyChannel" "AsyncBuilder" |> Some
    | "Microsoft.FSharp.Control.FSharpEvent`1" ->
        makeImportLib com Any "Event" "Event" |> Some
    | "Microsoft.FSharp.Control.FSharpEvent`2" ->
        makeImportLib com Any "Event$2" "Event" |> Some
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
    Helper.LibCall(com, "Option", "tryOp", t, op :: args, ?loc = r)

let tryCoreOp com r t coreModule coreMember args =
    let op = Helper.LibValue(com, coreModule, coreMember, Any)
    tryOp com r t op args

let emptyGuid () =
    makeStrConst "00000000-0000-0000-0000-000000000000"

let rec defaultof (com: ICompiler) (ctx: Context) r t =
    match t with
    // Non-struct tuples default to null
    | Tuple(args, true) ->
        NewTuple(args |> List.map (defaultof com ctx r), true) |> makeValue None
    | Boolean
    | Number _
    | Builtin BclTimeSpan
    | Builtin BclDateTime
    | Builtin BclDateTimeOffset
    | Builtin BclDateOnly
    | Builtin BclTimeOnly -> getZero com ctx t
    | Builtin BclGuid -> emptyGuid ()
    | DeclaredType(entRef, _) ->
        let ent = com.GetEntity(entRef)
        // TODO: For BCL types we cannot access the constructor, raise error or warning?
        if ent.IsValueType then
            tryConstructor com ent
        else
            None
        |> Option.map (fun e -> Helper.ConstructorCall(e, t, []))
        |> Option.defaultWith (fun () ->
            // Null t |> makeValue None
            Helper.LibCall(com, "Util", "defaultOf", t, [], ?loc = r)
        )
    // TODO: Fail (or raise warning) if this is an unresolved generic parameter?
    | _ ->
        // Null t |> makeValue None
        Helper.LibCall(com, "Util", "defaultOf", t, [], ?loc = r)

let fableCoreLib
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let fixDynamicImportPath =
        function
        | Value(StringConstant path, r) when
            path.EndsWith(".fs", StringComparison.Ordinal)
            ->
            // In imports *.ts extensions have to be converted to *.js extensions instead
            let fileExt = com.Options.FileExtension

            let fileExt =
                if fileExt.EndsWith(".ts", StringComparison.Ordinal) then
                    Path.ChangeExtension(fileExt, ".js")
                else
                    fileExt

            Value(StringConstant(Path.ChangeExtension(path, fileExt)), r)
        | path -> path

    match i.DeclaringEntityFullName, i.CompiledName with
    | _, UniversalFableCoreHelpers com ctx r t i args error expr -> Some expr

    // Extensions
    | _, "Async.AwaitPromise.Static" ->
        Helper.LibCall(com, "Async", "awaitPromise", t, args, ?loc = r) |> Some
    | _, "Async.StartAsPromise.Static" ->
        Helper.LibCall(com, "Async", "startAsPromise", t, args, ?loc = r)
        |> Some
    | _, "FormattableString.GetStrings" ->
        getFieldWith r t thisArg.Value "strs" |> Some

    | "Fable.Core.Testing.Assert", _ ->
        match i.CompiledName with
        | "AreEqual" ->
            Helper.LibCall(com, "Util", "assertEqual", t, args, ?loc = r)
            |> Some
        | "NotEqual" ->
            Helper.LibCall(com, "Util", "assertNotEqual", t, args, ?loc = r)
            |> Some
        | _ -> None
    | "Fable.Core.Reflection", meth ->
        Helper.LibCall(com, "Reflection", meth, t, args, ?loc = r) |> Some
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
        | "triggeredByDependency" ->
            makeBoolConst com.Options.TriggeredByDependency |> Some
        | _ -> None
    | "Fable.Core.JS", ("js" | "expr_js" as meth) ->
        let isStatement = meth <> "expr_js"

        match args with
        | RequireStringConstOrTemplate com ctx r template :: _ ->
            emitTemplate r t [] isStatement template |> Some
        | _ -> None
    | "Fable.Core.JsInterop", meth ->
        match meth, args with
        | "importDynamic", [ path ] ->
            let path = fixDynamicImportPath path
            Helper.GlobalCall("import", t, [ path ], ?loc = r) |> Some
        | "importValueDynamic", [ MaybeInScope ctx arg ] ->
            let dynamicImport selector path apply =
                let path = fixDynamicImportPath path
                let import = Helper.GlobalCall("import", t, [ path ], ?loc = r)

                match selector with
                | StringConst "*" -> import
                | selector ->
                    let selector =
                        let m = makeIdent "m"

                        Delegate(
                            [ m ],
                            Get(IdentExpr m, ExprGet selector, Any, None)
                            |> apply,
                            None,
                            Tags.empty
                        )

                    Helper.InstanceCall(import, "then", t, [ selector ])

            match arg with
            // TODO: Check this is not a fable-library import?
            | Import(info, _, _) ->
                dynamicImport
                    (makeStrConst info.Selector)
                    (makeStrConst info.Path)
                    id
                |> Some
            | NestedLambda(args,
                           Call(Import(importInfo, _, _), callInfo, _, _),
                           None) when argEquals args callInfo.Args ->
                dynamicImport
                    (makeStrConst importInfo.Selector)
                    (makeStrConst importInfo.Path)
                    id
                |> Some
            | Call(Import(importInfo, _, _), callInfo, t, r) ->
                dynamicImport
                    (makeStrConst importInfo.Selector)
                    (makeStrConst importInfo.Path)
                    (makeCall r t callInfo)
                |> Some
            | _ ->
                "The imported value is not coming from a different file"
                |> addErrorAndReturnNull com ctx.InlinePath r
                |> Some
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
        | Naming.StartsWith "emitJs" rest, [ args; macro ] ->
            match macro with
            | RequireStringConstOrTemplate com ctx r template ->
                let args = destructureTupleArgs [ args ]
                let isStatement = rest = "Statement"
                emitTemplate r t args isStatement template |> Some
        | "op_EqualsEqualsGreater",
          [ name; MaybeLambdaUncurriedAtCompileTime value ] ->
            makeTuple
                r
                true
                [
                    name
                    value
                ]
            |> Some
        | "createObj", _ ->
            Helper.LibCall(com, "Util", "createObj", Any, args)
            |> withTag "pojo"
            |> Some
        | "keyValueList", [ caseRule; keyValueList ] ->
            // makePojo com ctx caseRule keyValueList
            let args =
                [
                    keyValueList
                    caseRule
                ]

            Helper.LibCall(com, "MapUtil", "keyValueList", Any, args)
            |> withTag "pojo"
            |> Some
        | "toPlainJsObj", _ ->
            let emptyObj = ObjectExpr([], t, None)

            Helper.GlobalCall(
                "Object",
                Any,
                emptyObj :: args,
                memb = "assign",
                ?loc = r
            )
            |> Some
        | "jsOptions", [ arg ] -> makePojoFromLambda com arg |> Some
        | "jsThis", _ -> emitExpr r t [] "this" |> Some
        | "jsConstructor", _ ->
            match (genArg com ctx r 0 i.GenericArgs) with
            | DeclaredType(ent, _) ->
                com.GetEntity(ent) |> constructor com |> Some
            | _ ->
                "Only declared types define a function constructor in JS"
                |> addError com ctx.InlinePath r

                None
        | "createEmpty", _ -> typedObjExpr t [] |> Some
        // Deprecated methods
        | "ofJson", _ ->
            Helper.GlobalCall("JSON", t, args, memb = "parse", ?loc = r) |> Some
        | "toJson", _ ->
            Helper.GlobalCall("JSON", t, args, memb = "stringify", ?loc = r)
            |> Some
        | ("inflate" | "deflate"), _ -> List.tryHead args
        | _ -> None
    | "Fable.Core.JSX", meth ->
        match meth with
        | "create" ->
            Helper.LibCall(com, "JSX", "create", t, args, ?loc = r)
            |> withTag "jsx"
            |> Some
        | "html"
        | "jsx" ->
            Helper.LibCall(com, "JSX", "html", t, args, ?loc = r)
            |> withTag "jsx-template"
            |> Some
        | "text" -> TypeCast(args.Head, t) |> Some
        | "nothing" -> makeNullTyped t |> Some
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
        |> FSharp2Fable.Helpers.cleanNameAsJsIdentifier

    let memberName =
        i.CompiledName |> FSharp2Fable.Helpers.cleanNameAsJsIdentifier

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
        genArgs = i.GenericArgs,
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
        genArgs = i.GenericArgs,
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
                "String",
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
                "String",
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
            "String",
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
            "String",
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
            "String",
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
            "String",
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
            "String",
            "toFail",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("PrintFormatToStringBuilder" | "PrintFormatToStringBuilderThen"), // Printf.kbprintf
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
                "String",
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
            "String",
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
        Helper.GlobalCall("Math", t, args, argTypes, memb = meth, ?loc = r)

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
                "Option",
                "defaultArg",
                t,
                args,
                i.SignatureArgTypes,
                genArgs = i.GenericArgs,
                ?loc = r
            )
            |> Some
    | "DefaultAsyncBuilder", _ ->
        makeImportLib com t "singleton" "AsyncBuilder" |> Some
    // Erased operators.
    // KeyValuePair is already compiled as a tuple
    | ("KeyValuePattern" | "Identity" | "Box" | "Unbox" | "ToEnum"), [ arg ] ->
        TypeCast(arg, t) |> Some
    // Cast to unit to make sure nothing is returned when wrapped in a lambda, see #1360
    | "Ignore", _ -> TypeCast(args.Head, Unit) |> Some
    // Number and String conversions
    | ("ToSByte" | "ToByte" | "ToInt8" | "ToUInt8" | "ToInt16" | "ToUInt16" | "ToInt" | "ToUInt" | "ToInt32" | "ToUInt32"),
      _ -> toInt com ctx r t args |> Some
    | ("ToInt64" | "ToUInt64" | "ToIntPtr" | "ToUIntPtr"), _ ->
        toLong com ctx r t args |> Some
    | ("ToSingle" | "ToDouble"), _ -> toFloat com ctx r t args |> Some
    | "ToDecimal", _ -> toDecimal com ctx r t args |> Some
    | "ToChar", _ -> toChar args.Head |> Some
    | "ToString", _ -> toString com ctx r args |> Some
    | "CreateSequence", [ xs ] -> TypeCast(xs, t) |> Some
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
            | Number(Int64, _) -> "Range", "rangeInt64", addStep args
            | Number(UInt64, _) -> "Range", "rangeUInt64", addStep args
            | Number(Decimal, _) -> "Range", "rangeDecimal", addStep args
            | Number(Numbers _, _) -> "Range", "rangeDouble", addStep args
            | Number(BigIntegers _, _) -> "Range", "rangeBigInt", addStep args
            | x -> FableError "Unsupported range type: %A{x}" |> raise

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
    | ("PrintFormatToString" | "PrintFormatToStringThen" | "PrintFormat" | "PrintFormatLine" | "PrintFormatToError" | "PrintFormatLineToError" | "PrintFormatThen" | "PrintFormatToStringThenFail" | "PrintFormatToStringBuilder" | "PrintFormatToStringBuilderThen"), // Printf.kbprintf
      _ -> fsFormat com ctx r t i thisArg args
    | ("Failure" | "FailurePattern" | "LazyPattern" | "Lock" | "NullArg" | "Using"), // using
      _ -> fsharpModule com ctx r t i thisArg args
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
                "Decimal",
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
                "Decimal",
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
    | "Abs", _ ->
        match args with
        | ExprType(Number(Decimal, _)) :: _ ->
            Helper.LibCall(
                com,
                "Decimal",
                "abs",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | ExprType(Number(BigIntegers _, _)) :: _ ->
            Helper.LibCall(
                com,
                "BigInt",
                "abs",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    | ("Acos" | "Asin" | "Atan" | "Atan2" | "Cos" | "Cosh" | "Exp" | "Log" | "Log2" | "Log10" | "Sin" | "Sinh" | "Sqrt" | "Tan" | "Tanh"),
      _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    | "Round", _ ->
        match args with
        | ExprType(Number(Decimal, _)) :: _ ->
            Helper.LibCall(
                com,
                "Decimal",
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
                "Util",
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
                "Decimal",
                "truncate",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ ->
            Helper.GlobalCall(
                "Math",
                t,
                args,
                i.SignatureArgTypes,
                memb = "trunc",
                ?loc = r
            )
            |> Some
    | "Sign", _ ->
        let args = toFloat com ctx r t args |> List.singleton

        Helper.LibCall(
            com,
            "Util",
            "sign",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "DivRem", _ ->
        match i.SignatureArgTypes with
        | Number(BigIntegers _, _) :: _ ->
            Helper.LibCall(
                com,
                "BigInt",
                "divRem",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ ->
            Helper.LibCall(
                com,
                "Int32",
                "divRem",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
    // Numbers
    | ("Infinity" | "InfinitySingle"), _ ->
        Helper.GlobalIdent("Number", "POSITIVE_INFINITY", t, ?loc = r) |> Some
    | ("NaN" | "NaNSingle"), _ ->
        Helper.GlobalIdent("Number", "NaN", t, ?loc = r) |> Some
    | "Fst", [ tup ] -> Get(tup, TupleIndex 0, t, r) |> Some
    | "Snd", [ tup ] -> Get(tup, TupleIndex 1, t, r) |> Some
    // Reference
    | "op_Dereference", [ arg ] -> getRefCell com r t arg |> Some
    | "op_ColonEquals", [ o; v ] -> setRefCell com r o v |> Some
    | "Ref", [ arg ] -> makeRefCellFromValue com r arg |> Some
    | ("Increment" | "Decrement"), _ ->
        if i.CompiledName = "Increment" then
            "void($0.contents++)"
        else
            "void($0.contents--)"
        |> emitExpr r t args
        |> Some
    // Concatenates two lists
    | "op_Append", _ ->
        Helper.LibCall(
            com,
            "List",
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
    | ("Min" | "Max" | "MinMagnitude" | "MaxMagnitude" | "Clamp" as meth), _ ->
        let meth = Naming.lowerFirst meth

        match args with
        | ExprType(Number(Decimal, _)) :: _ ->
            Helper.LibCall(
                com,
                "Decimal",
                meth,
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | ExprType(Number(BigIntegers _, _)) :: _ ->
            Helper.LibCall(
                com,
                "BigInt",
                meth,
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | ExprType(Number _) :: _ ->
            Helper.LibCall(
                com,
                "Double",
                meth,
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ ->
            let f = makeComparerFunction com ctx t

            Helper.LibCall(
                com,
                "Util",
                meth,
                t,
                f :: args,
                i.SignatureArgTypes,
                genArgs = i.GenericArgs,
                ?loc = r
            )
            |> Some
    | "Not", [ operand ] -> // TODO: Check custom operator?
        makeUnOp r t operand UnaryNot |> Some
    | Patterns.SetContains Operators.standardSet, _ ->
        applyOp com ctx r t i.CompiledName args |> Some
    // Type info
    | "TypeOf", _ ->
        (genArg com ctx r 0 i.GenericArgs)
        |> makeTypeInfo (changeRangeToCallSite ctx.InlinePath r)
        |> Some
    | "TypeDefOf", _ ->
        (genArg com ctx r 0 i.GenericArgs)
        |> makeTypeDefinitionInfo (changeRangeToCallSite ctx.InlinePath r)
        |> Some
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
    | "ToUpper" -> icall r t args i.SignatureArgTypes "toLocaleUpperCase"
    | "ToUpperInvariant" -> icall r t args i.SignatureArgTypes "toUpperCase"
    | "ToLower" -> icall r t args i.SignatureArgTypes "toLocaleLowerCase"
    | "ToLowerInvariant" -> icall r t args i.SignatureArgTypes "toLowerCase"
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

        let methName =
            if List.length args > 1 then
                methName + "2"
            else
                methName

        Helper.LibCall(
            com,
            "Char",
            methName,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | "IsSurrogatePair"
    | "Parse" ->
        let methName = Naming.lowerFirst i.CompiledName

        Helper.LibCall(
            com,
            "Char",
            methName,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
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
    Helper.LibCall(com, "Util", "getEnumerator", t, [ expr ], ?loc = r)

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
            | [ _; _ ] -> emitExpr r t args "Array($1 + 1).join($0)" |> Some // String(char, int)
            | _ ->
                "Unexpected arguments in System.String constructor."
                |> addErrorAndReturnNull com ctx.InlinePath r
                |> Some
        | Array _ ->
            match args with
            | [ _ ] -> emitExpr r t args "$0.join('')" |> Some // String(char[])
            | [ _; _; _ ] ->
                emitExpr r t args "$0.join('').substr($1, $2)" |> Some // String(char[], int, int)
            | _ ->
                "Unexpected arguments in System.String constructor."
                |> addErrorAndReturnNull com ctx.InlinePath r
                |> Some
        | _ -> fsFormat com ctx r t i thisArg args
    | "get_Length", Some c, _ -> getFieldWith r t c "length" |> Some
    | "get_Chars", Some c, _ ->
        Helper.LibCall(
            com,
            "String",
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
                "String",
                "compare",
                Int32.Number,
                [
                    x
                    y
                    kind
                ]
            )

        makeEqOp r left (makeIntConst 0) BinaryEqual |> Some
    | "GetEnumerator", Some c, _ ->
        stringToCharArray c |> getEnumerator com r t |> Some
    | "Contains", Some c, arg :: _ ->
        if (List.length args) > 1 then
            addWarning
                com
                ctx.InlinePath
                r
                "String.Contains: second argument is ignored"

        let left = Helper.InstanceCall(c, "indexOf", Int32.Number, [ arg ])
        makeEqOp r left (makeIntConst 0) BinaryGreaterOrEqual |> Some
    | "StartsWith", Some c, [ _str ] ->
        let left = Helper.InstanceCall(c, "indexOf", Int32.Number, args)
        makeEqOp r left (makeIntConst 0) BinaryEqual |> Some
    | "StartsWith", Some c, [ _str; _comp ] ->
        Helper.LibCall(
            com,
            "String",
            "startsWith",
            t,
            args,
            i.SignatureArgTypes,
            thisArg = c,
            ?loc = r
        )
        |> Some
    | ReplaceName [ "ToUpper", "toLocaleUpperCase"
                    "ToUpperInvariant", "toUpperCase"
                    "ToLower", "toLocaleLowerCase"
                    "ToLowerInvariant", "toLowerCase" ] methName,
      Some c,
      args ->
        Helper.InstanceCall(
            c,
            methName,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | ("IndexOf" | "LastIndexOf"), Some c, _ ->
        match args with
        | [ ExprType Char ]
        | [ ExprType String ]
        | [ ExprType Char; ExprType(Number(Int32, NumberInfo.Empty)) ]
        | [ ExprType String; ExprType(Number(Int32, NumberInfo.Empty)) ]
        | [ ExprType String; StringComparisonEnumValue ]
        | [ ExprType String
            ExprType(Number(Int32, NumberInfo.Empty))
            StringComparisonEnumValue ] ->
            let args =
                args
                |> List.filter (
                    function
                    | StringComparisonEnumValue -> false
                    | _ -> true
                )

            Helper.InstanceCall(
                c,
                Naming.lowerFirst i.CompiledName,
                t,
                args,
                i.SignatureArgTypes,
                genArgs = i.GenericArgs,
                ?loc = r
            )
            |> Some
        | _ ->
            "The only extra argument accepted for String.IndexOf/LastIndexOf is startIndex."
            |> addErrorAndReturnNull com ctx.InlinePath r
            |> Some
    | ("Trim" | "TrimStart" | "TrimEnd"), Some c, _ ->
        let methName = Naming.lowerFirst i.CompiledName

        match args with
        | [] ->
            Helper.InstanceCall(
                c,
                methName,
                t,
                [],
                i.SignatureArgTypes,
                genArgs = i.GenericArgs,
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
                "String",
                methName,
                t,
                c :: args,
                hasSpread = spread,
                ?loc = r
            )
            |> Some
    | "ToCharArray", Some c, _ -> stringToCharArray c |> Some
    | "Split", Some c, _ ->
        match args with
        // Optimization
        | [] -> Helper.InstanceCall(c, "split", t, [ makeStrConst " " ]) |> Some
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
                    makeNone (Int32.Number)
                    arg2
                ]

            Helper.LibCall(com, "String", "split", t, c :: args, ?loc = r)
            |> Some
        | arg1 :: args ->
            let arg1 =
                match arg1.Type with
                | Array _ -> arg1
                | _ ->
                    Value(
                        NewArray(ArrayValues [ arg1 ], String, MutableArray),
                        None
                    )

            Helper.LibCall(
                com,
                "String",
                "split",
                t,
                arg1 :: args,
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

        Helper.LibCall(com, "String", methName, t, args, ?loc = r) |> Some
    | "Concat", None, _ ->
        match i.SignatureArgTypes with
        | [ Array _ | IEnumerable ] ->
            Helper.LibCall(
                com,
                "String",
                "join",
                t,
                ((makeStrConst "") :: args),
                ?loc = r
            )
            |> Some
        | _ ->
            Helper.LibCall(
                com,
                "String",
                "concat",
                t,
                args,
                hasSpread = true,
                ?loc = r
            )
            |> Some
    | "CompareOrdinal", None, _ ->
        Helper.LibCall(com, "String", "compareOrdinal", t, args, ?loc = r)
        |> Some
    | Patterns.SetContains implementedStringFunctions, thisArg, args ->
        Helper.LibCall(
            com,
            "String",
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
    | "Length", [ arg ] -> getFieldWith r t arg "length" |> Some
    | ("Iterate" | "IterateIndexed" | "ForAll" | "Exists"), _ ->
        // Cast the string to char[], see #1279
        let args = args |> List.replaceLast stringToCharArray

        Helper.LibCall(
            com,
            "Seq",
            Naming.lowerFirst i.CompiledName,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | ("Map" | "MapIndexed" | "Collect"), _ ->
        // Cast the string to char[], see #1279
        let args = args |> List.replaceLast stringToCharArray
        let name = Naming.lowerFirst i.CompiledName

        emitExpr
            r
            t
            [ Helper.LibCall(com, "Seq", name, Any, args, i.SignatureArgTypes) ]
            "Array.from($0).join('')"
        |> Some
    | "Concat", _ ->
        Helper.LibCall(com, "String", "join", t, args, ?loc = r) |> Some
    // Rest of StringModule methods
    | meth, args ->
        Helper.LibCall(
            com,
            "String",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some

let formattableString
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    // Even if we're going to wrap it again to make it compatible with FormattableString API, we use a JS template string
    // because the strings array will always have the same reference so it can be used as a key in a WeakMap
    // Attention, if we change the shape of the object ({ strs, args }) we need to change the resolution
    // of the FormattableString.GetStrings extension in Fable.Core too
    | "Create",
      None,
      [ StringConst str; Value(NewArray(ArrayValues args, _, _), _) ] ->
        let matches =
            Regex.Matches(str, @"\{\d+(.*?)\}")
            |> Seq.cast<Match>
            |> Seq.toArray

        let hasFormat =
            matches |> Array.exists (fun m -> m.Groups[1].Value.Length > 0)

        let tag =
            if not hasFormat then
                Helper.LibValue(com, "String", "fmt", Any) |> Some
            else
                let fmtArg =
                    matches
                    |> Array.map (fun m -> makeStrConst m.Groups[1].Value)
                    |> Array.toList
                    |> makeArray String

                Helper.LibCall(com, "String", "fmtWith", Any, [ fmtArg ])
                |> Some

        let holes =
            matches
            |> Array.map (fun m ->
                {|
                    Index = m.Index
                    Length = m.Length
                |}
            )

        let template = makeStringTemplate tag str holes args |> makeValue r
        // Use a type cast to keep the FormattableString type
        TypeCast(template, t) |> Some
    | "get_Format", Some x, _ ->
        Helper.LibCall(com, "String", "getFormat", t, [ x ], ?loc = r) |> Some
    | "get_ArgumentCount", Some x, _ ->
        getFieldWith r t (getField x "args") "length" |> Some
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
    | "Cast", _ ->
        Helper.LibCall(com, "Util", "downcast", t, args)
        |> withTag "downcast"
        |> Some
    | "CreateEvent", [ addHandler; removeHandler; _createHandler ] ->
        Helper.LibCall(
            com,
            "Event",
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
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth),
      args ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq2" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "Seq2",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "Seq",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
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
        Helper.GlobalCall("Array", t, args, memb = "from", ?loc = r)
        |> withTag "array"
        |> Some
    | "get_Item", Some ar, [ idx ] -> getExpr r t ar idx |> Some
    | "set_Item", Some ar, [ idx; value ] -> setExpr r ar idx value |> Some
    | "Add", Some ar, [ arg ] ->
        "void ($0)"
        |> emitExpr r t [ Helper.InstanceCall(ar, "push", t, [ arg ]) ]
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

        Helper.LibCall(com, "Array", "removeInPlace", t, args, ?loc = r) |> Some
    | "RemoveAll", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "Array",
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
        Helper.InstanceCall(ar, "findIndex", t, [ arg ], ?loc = r) |> Some
    | "FindLastIndex", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "Array",
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
        Helper.InstanceCall(ar, "forEach", t, [ arg ], ?loc = r) |> Some
    | "GetEnumerator", Some ar, _ -> getEnumerator com r t ar |> Some
    // ICollection members, implemented in dictionaries and sets too. We need runtime checks (see #1120)
    | "get_Count", Some(MaybeCasted(ar)), _ ->
        match ar.Type with
        // Fable translates System.Collections.Generic.List as Array
        // TODO: Check also IList?
        | Array _ -> getFieldWith r t ar "length" |> Some
        | _ -> Helper.LibCall(com, "Util", "count", t, [ ar ], ?loc = r) |> Some
    | "Clear", Some ar, _ ->
        Helper.LibCall(com, "Util", "clear", t, [ ar ], ?loc = r) |> Some
    | "ConvertAll", Some ar, [ arg ] ->
        Helper.LibCall(
            com,
            "Array",
            "map",
            t,
            [
                arg
                ar
            ],
            ?loc = r
        )
        |> Some
    | "Find", Some ar, [ arg ] ->
        let opt =
            Helper.LibCall(
                com,
                "Array",
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
        let left =
            Helper.InstanceCall(
                ar,
                "findIndex",
                Int32.Number,
                [ arg ],
                ?loc = r
            )

        makeEqOp r left (makeIntConst -1) BinaryGreater |> Some
    | "FindLast", Some ar, [ arg ] ->
        let opt =
            Helper.LibCall(
                com,
                "Array",
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
            | Array _ -> "Array"
            | _ -> "Seq"

        Helper.LibCall(com, moduleName, "contains", t, args, ?loc = r) |> Some
    | "IndexOf", Some ar, args ->
        let args = injectIndexOfArgs com ctx r i.GenericArgs (ar :: args)
        Helper.LibCall(com, "Array", "indexOf", t, args, ?loc = r) |> Some
    | "Insert", Some ar, [ idx; arg ] ->
        Helper.InstanceCall(
            ar,
            "splice",
            t,
            [
                idx
                makeIntConst 0
                arg
            ],
            ?loc = r
        )
        |> Some
    | "InsertRange", Some ar, [ idx; arg ] ->
        Helper.LibCall(
            com,
            "Array",
            "insertRangeInPlace",
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
        Helper.InstanceCall(ar, "splice", t, args, ?loc = r) |> Some
    | "RemoveAt", Some ar, [ idx ] ->
        Helper.InstanceCall(
            ar,
            "splice",
            t,
            [
                idx
                makeIntConst 1
            ],
            ?loc = r
        )
        |> Some
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
            "Array",
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
        Helper.InstanceCall(ar, "slice", t, args, ?loc = r) |> Some
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
            "Array",
            "insertRangeInPlace",
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
        [|
            "Exists", "some"
            "Filter", "filter"
            "ForAll", "every"
            "Iterate", "forEach"
            "Reduce", "reduce"
            "ReduceBack", "reduceRight"
            "SortInPlaceWith", "sort"
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
        | (ExprType(Tuple(genArgs, _)) as e) :: _ ->
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
        genArgs = i.GenericArgs,
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
    | "get_Length", Some arg, _ -> getFieldWith r t arg "length" |> Some
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
    | "ConvertAll", None, [ source; mapping ] ->
        Helper.LibCall(
            com,
            "Array",
            "map",
            t,
            [
                mapping
                source
            ],
            ?loc = r
        )
        |> Some
    | "IndexOf", None, args ->
        let args = injectIndexOfArgs com ctx r i.GenericArgs args

        Helper.LibCall(
            com,
            "Array",
            "indexOf",
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
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
    let newArrayAlloc size t =
        Value(NewArray(ArrayAlloc size, t, MutableArray), None)

    let createArray size value =
        match t, value with
        | Array(Number _ as t2, _), None when com.Options.TypedArrays ->
            newArrayAlloc size t2
        | Array(t2, _), value ->
            let value =
                value |> Option.defaultWith (fun () -> getZero com ctx t2)
            // If we don't fill the array some operations may behave unexpectedly, like Array.prototype.reduce
            Helper.LibCall(
                com,
                "Array",
                "fill",
                t,
                [
                    newArrayAlloc size t2
                    makeIntConst 0
                    size
                    value
                ]
            )
        | _ ->
            $"Expecting an array type but got %A{t}"
            |> addErrorAndReturnNull com ctx.InlinePath r

    match i.CompiledName, args with
    | "ToSeq", [ arg ] -> Some arg
    | "OfSeq", [ arg ] -> toArray r t arg |> Some
    | "OfList", args ->
        Helper.LibCall(
            com,
            "List",
            "toArray",
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> withTag "array"
        |> Some
    | "ToList", args ->
        Helper.LibCall(
            com,
            "List",
            "ofArray",
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | ("Length" | "Count"), [ arg ] -> getFieldWith r t arg "length" |> Some
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

        newArrayAlloc (makeIntConst 0) t |> Some
    | "IsEmpty", [ ar ] ->
        eq (getFieldWith r (Int32.Number) ar "length") (makeIntConst 0) |> Some
    | Patterns.DicContains nativeArrayFunctions meth, _ ->
        let args, thisArg = List.splitLast args
        let argTypes = List.take (List.length args) i.SignatureArgTypes
        Helper.InstanceCall(thisArg, meth, t, args, argTypes, ?loc = r) |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth),
      args ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq2" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "Seq2",
            "Array_" + meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Array" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "Array",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
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
            "List",
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
            genArgs = i.GenericArgs,
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
    | "ToSeq", [ x ] -> TypeCast(x, t) |> Some
    | "ToArray", args ->
        Helper.LibCall(
            com,
            "List",
            "toArray",
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> withTag "array"
        |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth),
      args ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq2" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "Seq2",
            "List_" + meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "List" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "List",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
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
            "Set",
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

    Helper.LibCall(
        com,
        "Set",
        meth,
        t,
        args,
        i.SignatureArgTypes,
        genArgs = i.GenericArgs,
        ?loc = r
    )
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
            "Map",
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

    Helper.LibCall(
        com,
        "Map",
        meth,
        t,
        args,
        i.SignatureArgTypes,
        genArgs = i.GenericArgs,
        ?loc = r
    )
    |> Some

let disposables
    (com: ICompiler)
    (_: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    // `use` will call Dispose without a null check so use a safe version just in case, see #2719
    | "Dispose", Some c ->
        Helper.LibCall(com, "Util", "disposeSafe", t, [ c ], ?loc = r) |> Some
    | _ -> None

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
            "Choice",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
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
        Helper.LibCall(com, "Option", "value", t, [ c ], ?loc = r) |> Some
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
        Helper.LibCall(com, "Option", "value", t, [ c ], ?loc = r) |> Some
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
        let genArgs = List.truncate 1 i.GenericArgs

        Helper.LibCall(
            com,
            "Option",
            "toArray",
            Array(t, MutableArray),
            [ arg ],
            genArgs = genArgs,
            ?loc = r
        )

    match i.CompiledName, args with
    | "None", _ -> NewOption(None, t, isStruct) |> makeValue r |> Some
    | "GetValue", [ c ] ->
        Helper.LibCall(com, "Option", "value", t, args, ?loc = r) |> Some
    | ("OfObj" | "OfNullable"), _ ->
        Helper.LibCall(
            com,
            "Option",
            "ofNullable",
            t,
            args,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | ("ToObj" | "ToNullable"), _ ->
        Helper.LibCall(
            com,
            "Option",
            "toNullable",
            t,
            args,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | "IsSome", [ c ] -> Test(c, OptionTest true, r) |> Some
    | "IsNone", [ c ] -> Test(c, OptionTest false, r) |> Some
    | ("Filter" | "Flatten" | "Map" | "Map2" | "Map3" | "Bind" as meth), args ->
        Helper.LibCall(
            com,
            "Option",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | "ToArray", [ arg ] -> toArray r t arg |> Some
    | "ToList", [ arg ] ->
        let args = args |> List.replaceLast (toArray None t)
        Helper.LibCall(com, "List", "ofArray", t, args, ?loc = r) |> Some
    | "FoldBack", [ folder; opt; state ] ->
        Helper.LibCall(
            com,
            "Seq",
            "foldBack",
            t,
            [
                folder
                toArray None t opt
                state
            ],
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | "DefaultValue", _ ->
        Helper.LibCall(com, "Option", "defaultArg", t, List.rev args, ?loc = r)
        |> Some
    | "DefaultWith", _ ->
        Helper.LibCall(
            com,
            "Option",
            "defaultArgWith",
            t,
            List.rev args,
            List.rev i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | "OrElse", _ ->
        Helper.LibCall(com, "Option", "orElse", t, List.rev args, ?loc = r)
        |> Some
    | "OrElseWith", _ ->
        Helper.LibCall(
            com,
            "Option",
            "orElseWith",
            t,
            List.rev args,
            List.rev i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | ("Count" | "Contains" | "Exists" | "Fold" | "ForAll" | "Iterate" as meth),
      _ ->
        let meth = Naming.lowerFirst meth
        let args = args |> List.replaceLast (toArray None t)
        let args = injectArg com ctx r "Seq" meth i.GenericArgs args

        Helper.LibCall(
            com,
            "Seq",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
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
            "Boolean",
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
            | x -> failwithf $"Unexpected type in parse: %A{x}"

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

        let callExpr =
            Helper.LibCall(
                com,
                numberModule,
                Naming.lowerFirst meth,
                t,
                args,
                ?loc = r
            )

        match t with
        | Number(BigIntegers _, _) -> wrapLong com ctx r t callExpr
        | _ -> callExpr

    let isFloat =
        match i.SignatureArgTypes with
        | Number(Floats _, _) :: _ -> true
        | _ -> false

    match i.CompiledName, args with
    | "IsNaN", [ _ ] when isFloat ->
        Helper.GlobalCall("Number", t, args, memb = "isNaN", ?loc = r) |> Some
    | "Log2", _ ->
        match i.SignatureArgTypes with
        | Number(BigIntegers _, _) :: _ ->
            Helper.LibCall(
                com,
                "BigInt",
                "ilog2",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ ->
            let log =
                Helper.GlobalCall("Math", t, args, memb = "log2", ?loc = r)

            if isFloat then
                log |> Some
            else
                toInt com ctx r t [ log ] |> Some
    | "IsPositiveInfinity", [ _ ] when isFloat ->
        Helper.LibCall(
            com,
            "Double",
            "isPositiveInfinity",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "IsNegativeInfinity", [ _ ] when isFloat ->
        Helper.LibCall(
            com,
            "Double",
            "isNegativeInfinity",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "IsInfinity", [ _ ] when isFloat ->
        Helper.LibCall(
            com,
            "Double",
            "isInfinity",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "IsInfinity", [ _ ] when isFloat ->
        Helper.LibCall(
            com,
            "Double",
            "isInfinity",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | ("Min" | "Max" | "MinMagnitude" | "MaxMagnitude" | "Clamp"), _ ->
        operators com ctx r t i thisArg args
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

        parseCall meth str args style |> Some
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
        parseCall meth str args style |> Some
    | "Pow", _ ->
        Helper.GlobalCall(
            "Math",
            t,
            args,
            i.SignatureArgTypes,
            memb = "pow",
            ?loc = r
        )
        |> Some
    | "ToString", [ ExprTypeAs(String, format) ] ->
        let format = emitExpr r String [ format ] "'{0:' + $0 + '}'"

        Helper.LibCall(
            com,
            "String",
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
        Helper.GlobalCall("String", String, [ thisArg.Value ], ?loc = r) |> Some
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
            "Decimal",
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
            "Decimal",
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
                "Decimal",
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
            "Decimal",
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
            | BigIntegers _ -> toLong com ctx r t args |> Some
            | Integers _ -> toInt com ctx r t args |> Some
            | Floats _ -> toFloat com ctx r t args |> Some
            | Decimal -> toDecimal com ctx r t args |> Some
            | _ -> None
        | _ -> None
    | ("Ceiling" | "Floor" | "Round" | "Truncate" | "Min" | "Max" | "MinMagnitude" | "MaxMagnitude" | "Clamp" | "Add" | "Subtract" | "Multiply" | "Divide" | "Remainder" | "Negate" as meth),
      _ ->
        let meth = Naming.lowerFirst meth

        Helper.LibCall(
            com,
            "Decimal",
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
            "String",
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
        Helper.InstanceCall(thisArg.Value, "toString", String, [], ?loc = r)
        |> Some
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
    match i.CompiledName, thisArg with
    | ".ctor", None ->
        match i.SignatureArgTypes with
        | [ Array _ ] ->
            Helper.LibCall(
                com,
                "BigInt",
                "fromByteArray",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | [ Number(kind, _) ] ->
            let meth = "from" + kind.ToString()

            Helper.LibCall(
                com,
                "BigInt",
                meth,
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | _ -> None
    | "op_Explicit", None ->
        match t with
        | Number(kind, _) ->
            match kind with
            | BigIntegers _ -> toLong com ctx r t args |> Some
            | Integers _ -> toInt com ctx r t args |> Some
            | Floats _ -> toFloat com ctx r t args |> Some
            | Decimal -> toDecimal com ctx r t args |> Some
            | _ -> None
        | _ -> None
    | "Log", None ->
        match args with
        | [ arg1; arg2 ] ->
            Helper.LibCall(
                com,
                "BigInt",
                "log",
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
                "BigInt",
                "ln",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
    | "Log2", None ->
        Helper.LibCall(
            com,
            "BigInt",
            "ilog2",
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | meth, callee ->
        let args =
            match callee, meth with
            | None, _ -> args
            | Some c, _ -> c :: args

        Helper.LibCall(
            com,
            "BigInt",
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
            "Util",
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
    | "ParseInt64", [ arg ] -> toLong com ctx r t [ arg ] |> Some
    | "ParseUInt64", [ arg ] -> toLong com ctx r t [ arg ] |> Some
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
    | "UnboxFast", _, [ arg ] -> TypeCast(arg, t) |> Some
    | "UnboxGeneric", _, [ arg ] ->
        Helper.LibCall(com, "Util", "downcast", t, [ arg ])
        |> withTag "downcast"
        |> Some
    | "MakeDecimal", _, _ -> decimals com ctx r t i thisArg args
    | "GetString", _, [ ar; idx ]
    | "GetArray", _, [ ar; idx ] -> getExpr r t ar idx |> Some
    | "SetArray", _, [ ar; idx; value ] -> setExpr r ar idx value |> Some
    | ("GetArraySlice" | "GetStringSlice"), None, [ ar; lower; upper ] ->
        let upper =
            match upper with
            | Value(NewOption(None, _, _), _) ->
                getExpr None (Int32.Number) ar (makeStrConst "length")
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
            "Array",
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
        Helper.GlobalCall(
            "Math",
            t,
            args,
            i.SignatureArgTypes,
            memb = "pow",
            ?loc = r
        )
        |> Some
    | "PowDecimal", None, _ ->
        Helper.LibCall(
            com,
            "Decimal",
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
            "Range",
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
            "Range",
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
            "Range",
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
            "Range",
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
    // Use emit so auto-uncurrying is applied
    | "DynamicInvoke", Some callee ->
        emitExpr r t (callee :: args) "$0(...$1)" |> Some
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
    | ".ctor", _ -> makeTuple r true args |> Some
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
    | "get_Count", _ -> getFieldWith r t thisArg.Value "size" |> Some
    | "GetEnumerator", Some callee -> getEnumerator com r t callee |> Some
    | "ContainsValue", _ ->
        match thisArg, args with
        | Some c, [ arg ] ->
            Helper.LibCall(
                com,
                "MapUtil",
                "containsValue",
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
            "MapUtil",
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
            "MapUtil",
            "addToDict",
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
            "MapUtil",
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
                    "Clear", "clear"
                    "Remove", "delete" ] methName,
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
            "MapUtil",
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
        Helper.ConstructorCall(makeIdentExpr "Error", t, args, ?loc = r) |> Some
    | "get_Message", Some e -> getFieldWith r t e "message" |> Some
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
        makeEqOp r left right BinaryEqual |> Some
    | "Equals", Some arg1, [ arg2 ]
    | "Equals", None, [ arg1; arg2 ] -> equals com ctx r true arg1 arg2 |> Some
    | "GetHashCode", Some arg, _ -> identityHash com r arg |> Some
    | "GetType", Some arg, _ ->
        if arg.Type = Any then
            "Types can only be resolved at compile time. At runtime this will be same as `typeof<obj>`"
            |> addWarning com ctx.InlinePath r

        makeTypeInfo (changeRangeToCallSite ctx.InlinePath r) arg.Type |> Some
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

    Helper.GlobalCall("console", t, args, memb = "log", ?loc = r)

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
    | "ToInt64" -> round com args |> toLong com ctx r t |> Some
    | "ToUInt64" -> round com args |> toLong com ctx r t |> Some
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
    | "WriteLine" -> log com r t i thisArg args |> Some
    | _ -> None

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

let ignoreFormatProvider meth args =
    match meth, args with
    // Ignore IFormatProvider
    | "Parse", arg :: _ -> [ arg ]
    | "TryParse", input :: _culture :: _styles :: defVal :: _ ->
        [
            input
            defVal
        ]
    | "TryParse", input :: _culture :: defVal :: _ ->
        [
            input
            defVal
        ]
    | _ -> args

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
    | "get_Offset" as meth ->
        let moduleName =
            if meth = "get_Kind" then
                "Date"
            else
                "DateOffset"

        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst

        Helper.LibCall(
            com,
            moduleName,
            meth,
            t,
            [ thisArg.Value ],
            [ thisArg.Value.Type ],
            ?loc = r
        )
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
    | meth ->
        let args = ignoreFormatProvider meth args
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

let dateOnly
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | ".ctor" when args.Length = 4 ->
        "DateOnly constructor with the calendar parameter is not supported."
        |> addError com ctx.InlinePath r

        None
    | ".ctor" ->
        Helper.LibCall(
            com,
            "DateOnly",
            "create",
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "ToString" ->
        match args with
        | [ ExprType String ]
        | [ StringConst _ ] ->
            "DateOnly.ToString without CultureInfo is not supported, please add CultureInfo.InvariantCulture"
            |> addError com ctx.InlinePath r

            None
        | [ StringConst("d" | "o" | "O"); _ ] ->
            Helper.LibCall(
                com,
                "DateOnly",
                "toString",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | [ StringConst _; _ ] ->
            "DateOnly.ToString doesn't support custom format. It only handles \"d\", \"o\", \"O\" format, with CultureInfo.InvariantCulture."
            |> addError com ctx.InlinePath r

            None
        | [ _ ] ->
            Helper.LibCall(
                com,
                "DateOnly",
                "toString",
                t,
                makeStrConst "d" :: args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ -> None
    | "AddDays"
    | "AddMonths"
    | "AddYears" ->
        let meth = Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst

        Helper.LibCall(
            com,
            "Date",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | meth ->
        let args = ignoreFormatProvider meth args
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst

        Helper.LibCall(
            com,
            "DateOnly",
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
        let meth =
            match args with
            | [ ticks ] -> "fromTicks"
            | _ -> "create"

        Helper.LibCall(
            com,
            "TimeSpan",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | "FromMilliseconds" -> TypeCast(args.Head, t) |> Some
    | "get_TotalMilliseconds" -> TypeCast(thisArg.Value, t) |> Some
    | "ToString" when (args.Length = 1) ->
        "TimeSpan.ToString with one argument is not supported, because it depends on local culture, please add CultureInfo.InvariantCulture"
        |> addError com ctx.InlinePath r

        None
    | "ToString" when (args.Length = 2) ->
        match args.Head with
        | StringConst "c"
        | StringConst "g"
        | StringConst "G" ->
            Helper.LibCall(
                com,
                "TimeSpan",
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
    | meth ->
        let args = ignoreFormatProvider meth args
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst

        Helper.LibCall(
            com,
            "TimeSpan",
            meth,
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some

let timeOnly
    (com: ICompiler)
    (ctx: Context)
    r
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | ".ctor" ->
        match args with
        | [ ExprType(Number(Int64, _)) ] ->
            Helper.LibCall(
                com,
                "TimeOnly",
                "fromTicks",
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
                "TimeOnly",
                "create",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
    | "get_MinValue" -> makeIntConst 0 |> Some
    | "ToTimeSpan" ->
        // The representation is identical
        thisArg
    | "get_Hour"
    | "get_Minute"
    | "get_Second"
    | "get_Millisecond" ->
        // Translate TimeOnly properties with a name in singular to the equivalent properties on TimeSpan
        timeSpans
            com
            ctx
            r
            t
            { i with CompiledName = i.CompiledName + "s" }
            thisArg
            args
    | "get_Ticks" ->
        Helper.LibCall(
            com,
            "TimeSpan",
            "ticks",
            t,
            args,
            i.SignatureArgTypes,
            ?thisArg = thisArg,
            ?loc = r
        )
        |> Some
    | "ToString" ->
        match args with
        | [ ExprType String ]
        | [ StringConst _ ] ->
            "TimeOnly.ToString without CultureInfo is not supported, please add CultureInfo.InvariantCulture"
            |> addError com ctx.InlinePath r

            None
        | [ StringConst("r" | "R" | "o" | "O" | "t" | "T"); _ ] ->
            Helper.LibCall(
                com,
                "TimeOnly",
                "toString",
                t,
                args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | [ StringConst _; _ ] ->
            "TimeOnly.ToString doesn't support custom format. It only handles \"r\", \"R\", \"o\", \"O\", \"t\", \"T\" format, with CultureInfo.InvariantCulture."
            |> addError com ctx.InlinePath r

            None
        | [ _ ] ->
            Helper.LibCall(
                com,
                "TimeOnly",
                "toString",
                t,
                makeStrConst "t" :: args,
                i.SignatureArgTypes,
                ?thisArg = thisArg,
                ?loc = r
            )
            |> Some
        | _ -> None
    | meth ->
        let args = ignoreFormatProvider meth args
        let meth = Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst

        Helper.LibCall(
            com,
            "TimeOnly",
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
            "Timer",
            "default",
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
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match args with
        | [] ->
            Helper.LibCall(com, "Random", "nonSeeded", t, [], [], ?loc = r)
            |> Some
        | args ->
            Helper.LibCall(
                com,
                "Random",
                "seeded",
                t,
                args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
    // Not yet supported
    | ("NextInt64" | "NextSingle"), _ -> None
    | meth, Some thisArg ->
        let meth =
            if meth = "Next" then
                $"Next{List.length args}"
            else
                meth

        Helper.InstanceCall(
            thisArg,
            meth,
            t,
            args,
            i.SignatureArgTypes,
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
            "Async",
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
            "Async",
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
        match args with
        | [ StringConst pattern ] -> makeRegexConst r pattern []
        | StringConst pattern :: (RegexFlags flags) :: _ ->
            makeRegexConst r pattern flags
        | _ -> Helper.LibCall(com, "RegExp", "create", t, args, ?loc = r)

    match i.CompiledName with
    | ".ctor" -> createRegex r t args |> Some
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
            propStr "index" thisArg.Value |> Some
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
            propStr "length" thisArg.Value |> Some
        else
            propInt 0 thisArg.Value |> propStr "length" |> Some
    // Group
    | "get_Success" -> nullCheck r false thisArg.Value |> Some
    // Match
    | "get_Groups" -> thisArg.Value |> Some
    // MatchCollection & GroupCollection
    | "get_Item" when i.DeclaringEntityFullName = Types.regexGroupCollection ->
        // can be group index or group name
        //        `m.Groups.[0]` `m.Groups.["name"]`
        match (args |> List.head).Type with
        | String ->
            // name
            (* `groups` might not exist -> check first:
                (`m`: `thisArg.Value`; `name`: `args.Head`)
                  ```ts
                  m.groups?.[name]
                  ```
                or here
                  ```ts
                  m.groups && m.groups[name]
                  ```
            *)
            let groups = propStr "groups" thisArg.Value
            let getItem = getExpr r t groups args.Head

            Operation(Logical(LogicalAnd, groups, getItem), Tags.empty, t, None)
            |> Some
        | _ ->
            // index
            getExpr r t thisArg.Value args.Head |> Some
    | "get_Item" -> getExpr r t thisArg.Value args.Head |> Some
    | "get_Count" -> propStr "length" thisArg.Value |> Some
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
            "RegExp",
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
    (_ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        let className =
            match i.GenericArgs with
            | [ _ ] -> "Event"
            | _ -> "Event$2"

        Helper.LibCall(
            com,
            "Event",
            className,
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
            "Event",
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
        genArgs = i.GenericArgs,
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
                "MailboxProcessor",
                "default",
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
                "MailboxProcessor",
                "start",
                t,
                args,
                i.SignatureArgTypes,
                genArgs = i.GenericArgs,
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
                "MailboxProcessor",
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
        makeImportLib com t "singleton" "AsyncBuilder" |> Some
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
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | Some x, meth, _ ->
        Helper.InstanceCall(
            x,
            meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    | None, meth, _ ->
        Helper.LibCall(
            com,
            "AsyncBuilder",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
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
    // TODO: Throw error for RunSynchronously
    | "Start" ->
        "Async.Start will behave as StartImmediate"
        |> addWarning com ctx.InlinePath r

        Helper.LibCall(
            com,
            "Async",
            "start",
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    // Make sure cancellationToken is called as a function and not a getter
    | "get_CancellationToken" ->
        Helper.LibCall(com, "Async", "cancellationToken", t, [], ?loc = r)
        |> Some
    // `catch` cannot be used as a function name in JS
    | "Catch" ->
        Helper.LibCall(
            com,
            "Async",
            "catchAsync",
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
            ?loc = r
        )
        |> Some
    // Fable.Core extensions
    | meth ->
        Helper.LibCall(
            com,
            "Async",
            Naming.lowerFirst meth,
            t,
            args,
            i.SignatureArgTypes,
            genArgs = i.GenericArgs,
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
    let parseGuid (literalGuid: string) =
        try
            System.Guid.Parse(literalGuid) |> string<Guid> |> makeStrConst
        with e ->
            e.Message |> addErrorAndReturnNull com ctx.InlinePath r
        |> Some

    match i.CompiledName with
    | "NewGuid" -> Helper.LibCall(com, "Guid", "newGuid", t, []) |> Some
    | "Parse" ->
        match args with
        | [ StringConst literalGuid ] -> parseGuid literalGuid
        | _ ->
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
    | "ToString" when (args.Length = 0) -> thisArg.Value |> Some
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
        | [ StringConst literalGuid ] -> parseGuid literalGuid
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
            "Uri.tryCreate",
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
            genArgs = i.GenericArgs,
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
                        let ifcName = splitFullName ifc.Entity.FullName |> snd

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
                            DeclaredType(ifcEnt, genArgs)
                            |> makeTypeInfo (
                                changeRangeToCallSite ctx.InlinePath r
                            )
                        | None -> Value(Null t, r)
                )
            | "get_FullName" -> getTypeFullName false exprType |> returnString r
            | "get_Namespace" ->
                getTypeFullName false exprType
                |> splitFullName
                |> fst
                |> returnString r
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
                | Array(t, _) ->
                    makeTypeInfo (changeRangeToCallSite ctx.InlinePath r) t
                    |> Some
                | _ -> Null t |> makeValue r |> Some
            | "get_IsGenericType" ->
                List.isEmpty exprType.Generics
                |> not
                |> BoolConstant
                |> makeValue r
                |> Some
            | "get_GenericTypeArguments"
            | "GetGenericArguments" ->
                let arVals =
                    exprType.Generics
                    |> List.map (
                        makeTypeInfo (changeRangeToCallSite ctx.InlinePath r)
                    )

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

                makeTypeInfo
                    (changeRangeToCallSite ctx.InlinePath exprRange)
                    exprType
                |> Some
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

let makeGenericTypeInfo r t =
    TypeInfo(t, [ "allow-generics" ]) |> makeValue r

let makeMethodInfo
    com
    r
    (name: string)
    (parameters: (string * Type) list)
    (returnType: Type)
    =
    let t = Any // TODO: Proper type

    let args =
        [
            makeStrConst name
            parameters
            |> List.map (fun (name, t) ->
                makeTuple
                    None
                    false
                    [
                        makeStrConst name
                        makeGenericTypeInfo None t
                    ]
            )
            |> makeArray Any
            makeGenericTypeInfo None returnType
        ]

    Helper.LibCall(
        com,
        "Reflection",
        "MethodInfo",
        t,
        args,
        isConstructor = true,
        ?loc = r
    )

let tryField com returnTyp ownerTyp fieldName =
    match ownerTyp, fieldName with
    | Number(Decimal, _), _ ->
        Helper.LibValue(com, "Decimal", "get_" + fieldName, returnTyp) |> Some
    | String, "Empty" -> makeStrConst "" |> Some
    | Builtin BclGuid, "Empty" -> emptyGuid () |> Some
    | Builtin BclTimeSpan, "Zero" -> makeIntConst 0 |> Some
    | Builtin(BclDateTime | BclDateTimeOffset | BclTimeOnly | BclDateOnly as t),
      ("MaxValue" | "MinValue") ->
        Helper.LibCall(
            com,
            coreModFor t,
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
                "BitConverter",
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
            // JS cannot parallelize synchronous actions so for now redirect to "standard" array module
            // TODO: Other languages may want to implement it
            "Microsoft.FSharp.Collections.ArrayModule.Parallel", arrayModule
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
            Types.idisposable, disposables
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
            Types.datetime, dates
            Types.datetimeOffset, dates
            Types.dateOnly, dateOnly
            Types.timeOnly, timeOnly
            Types.timespan, timeSpans
            "System.Timers.Timer", timers
            "System.Environment", systemEnv
            "System.Globalization.CultureInfo", globalization
            "System.Random", random
            "System.Threading.CancellationToken", cancels
            "System.Threading.CancellationTokenSource", cancels
            "System.Threading.Monitor", monitor
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
    | "System.Delegate"
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
    | Types.exception_ -> Some(makeImportLib com Any "Exception" "Types", args)
    | Types.attribute -> Some(makeImportLib com Any "Attribute" "Types", args)
    | fullName when
        fullName.StartsWith("Fable.Core.", StringComparison.Ordinal)
        && fullName.EndsWith("Attribute", StringComparison.Ordinal)
        ->
        Some(makeImportLib com Any "Attribute" "Types", args)
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

        let entityName =
            FSharp2Fable.Helpers.cleanNameAsJsIdentifier "Dictionary"

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

        let entityName = FSharp2Fable.Helpers.cleanNameAsJsIdentifier "HashSet"
        Some(makeImportLib com Any entityName "MutableSet", args)
    | Types.stack ->
        match argTypes.Value, args with
        | [], _ ->
            let args = []

            let entityName =
                FSharp2Fable.Helpers.cleanNameAsJsIdentifier "Stack"

            Some(makeImportLib com Any entityName "Stack", args)
        | _ -> None
    | Types.queue ->
        match argTypes.Value, args with
        | [], _ ->
            let args = []

            let entityName =
                FSharp2Fable.Helpers.cleanNameAsJsIdentifier "Queue"

            Some(makeImportLib com Any entityName "Queue", args)
        | _ -> None
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
        | BclDateTime -> Some(Types.datetime, dates, [])
        | BclDateTimeOffset -> Some(Types.datetimeOffset, dates, [])
        | BclDateOnly -> Some(Types.dateOnly, dateOnly, [])
        | BclTimeOnly -> Some(Types.timeOnly, timeOnly, [])
        | BclTimer -> Some("System.Timers.Timer", timers, [])
        | BclTimeSpan -> Some(Types.timespan, timeSpans, [])
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
    | _ -> None
