[<RequireQualifiedAccess>]
module Fable.Transforms.Rust.Replacements

#nowarn "1182"

open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

type Context = FSharp2Fable.Context
type ICompiler = FSharp2Fable.IFableCompiler
type CallInfo = ReplaceCallInfo

let error (msg: Expr) = msg

let coreModFor = function
    | BclGuid -> "Guid"
    | BclDateTime -> "DateTime"
    | BclDateTimeOffset -> "DateOffset"
    | BclTimer -> "Timer"
    | BclTimeSpan -> "TimeSpan"
    | FSharpSet _ -> "Set"
    | FSharpMap _ -> "Map"
    | FSharpResult _ -> "Result"
    | FSharpChoice _ -> "Choice"
    | FSharpReference _ -> "Native"
    | BclHashSet _ -> "MutableSet"
    | BclDictionary _ -> "MutableMap"
    | BclKeyValuePair _
    | BclDateOnly
    | BclTimeOnly -> FableError "Cannot decide core module" |> raise

let nativeCall expr =
    expr |> withTag "native"

let makeInstanceCall r t (i: CallInfo) callee memberName args =
    Helper.InstanceCall(callee, memberName, t, args, i.SignatureArgTypes, ?loc=r)
    |> nativeCall

let makeStaticLibCall (com: ICompiler) r t (i: CallInfo) moduleName memberName args =
    Helper.LibCall(com, moduleName, memberName, t, args, i.SignatureArgTypes, isModuleMember=false, ?loc=r)

let makeLibCall (com: ICompiler) r t (i: CallInfo) moduleName memberName args =
    Helper.LibCall(com, moduleName, memberName, t, args, i.SignatureArgTypes, ?loc=r)

let makeGlobalIdent (ident: string, memb: string, typ: Type) =
    makeTypedIdentExpr typ (ident + "::" + memb)

let makeUniqueIdent ctx t name =
    FSharp2Fable.Helpers.getIdentUniqueName ctx name
    |> makeTypedIdent t

let makeDecimal com r t (x: decimal) =
    let str = x.ToString(System.Globalization.CultureInfo.InvariantCulture)
    Helper.LibCall(com, "Decimal", "fromString", t, [makeStrConst str], isConstructor=true, ?loc=r)

let makeRef (value: Expr) =
    Operation(Unary(UnaryAddressOf, value), value.Type, None)

let getRefCell com r t (expr: Expr) =
    Helper.InstanceCall(expr, "get", t, [], ?loc=r)
    |> nativeCall

let setRefCell com r (expr: Expr) (value: Expr) =
    Fable.Set(expr, Fable.ValueSet, value.Type, value, r)

let makeRefCell com r typ (value: Expr) =
    Helper.LibCall(com, "Native", "refCell", typ, [value], ?loc=r)

let makeRefFromMutableValue com ctx r t (value: Expr) =
    Operation(Unary(UnaryAddressOf, value), t, r)

let makeRefFromMutableField com ctx r t callee key =
    let value = Get(callee, FieldInfo.Create(key), t, r)
    Operation(Unary(UnaryAddressOf, value), t, r)

// Mutable and public module values are compiled as functions
let makeRefFromMutableFunc com ctx r t (value: Expr) =
    value

let toNativeIndex expr =
    TypeCast(expr, Number(UNativeInt, NumberInfo.Empty))

let toChar com (arg: Expr) =
    match arg.Type with
    | Char -> arg
    | String ->
        Helper.LibCall(com, "String", "getCharAt", Char, [arg; makeIntConst 0])
    | _ ->
        let code = TypeCast(arg, Number(UInt32, NumberInfo.Empty))
        Helper.LibCall(com, "String", "fromCharCode", Char, [code])

let toString com (ctx: Context) r (args: Expr list) =
    match args with
    | [] ->
        "toString is called with empty args"
        |> addErrorAndReturnNull com ctx.InlinePath r
    | head::tail ->
        match head.Type with
        | String -> head
        | Char -> Helper.LibCall(com, "String", "ofChar", String, [head])
        // | Number(BigInt,_) -> Helper.LibCall(com, "BigInt", "toString", String, args)
        // | Number(Decimal, _) -> Helper.LibCall(com, "Decimal", "toString", String, args)
        // | Builtin BclGuid when tail.IsEmpty -> head
        // | Builtin (BclGuid|BclTimeSpan as bt) ->
        //     Helper.LibCall(com, coreModFor bt, "toString", String, args)
        // | Array _ | List _ ->
        //     Helper.LibCall(com, "Types", "seqToString", String, [head], ?loc=r)
        // | DeclaredType(ent, _) when ent.IsFSharpUnion || ent.IsFSharpRecord || ent.IsValueType ->
        //     Helper.InstanceCall(head, "toString", String, [], ?loc=r)
        // | DeclaredType(ent, _) ->
        | _ ->
            Helper.LibCall(com, "String", "toString", String, [makeRef head])

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
    | Number(kind,_) ->
        match kind with
        | Int8 -> "toSByte"
        | UInt8 -> "toByte"
        | Int16 -> "toInt16"
        | UInt16 -> "toUInt16"
        | Int32 -> "toInt32"
        | UInt32 -> "toUInt32"
        | Int64 -> "toInt64"
        | UInt64 -> "toUInt64"
        | Float32 -> "toSingle"
        | Float64 -> "toDouble"
        | Decimal -> "toDecimal"
        | BigInt | NativeInt | UNativeInt ->
            FableError $"Unexpected BigInt/%A{kind} conversion" |> raise
    | _ -> FableError $"Unexpected non-number type %A{typeTo}" |> raise

let kindIndex t =           //         0   1   2   3   4   5   6   7   8   9  10  11
    match t with            //         i8 i16 i32 i64  u8 u16 u32 u64 f32 f64 dec big
    | Int8 -> 0    //  0 i8   -   -   -   -   +   +   +   +   -   -   -   +
    | Int16 -> 1   //  1 i16  +   -   -   -   +   +   +   +   -   -   -   +
    | Int32 -> 2   //  2 i32  +   +   -   -   +   +   +   +   -   -   -   +
    | Int64 -> 3   //  3 i64  +   +   +   -   +   +   +   +   -   -   -   +
    | UInt8 -> 4   //  4 u8   +   +   +   +   -   -   -   -   -   -   -   +
    | UInt16 -> 5  //  5 u16  +   +   +   +   +   -   -   -   -   -   -   +
    | UInt32 -> 6  //  6 u32  +   +   +   +   +   +   -   -   -   -   -   +
    | UInt64 -> 7  //  7 u64  +   +   +   +   +   +   +   -   -   -   -   +
    | Float32 -> 8 //  8 f32  +   +   +   +   +   +   +   +   -   -   -   +
    | Float64 -> 9 //  9 f64  +   +   +   +   +   +   +   +   -   -   -   +
    | Decimal -> 10         // 10 dec  +   +   +   +   +   +   +   +   -   -   -   +
    | BigInt -> 11          // 11 big  +   +   +   +   +   +   +   +   +   +   +   -
    | NativeInt | UNativeInt -> FableError "Casting to/from (u)nativeint is unsupported" |> raise

let needToCast typeFrom typeTo =
    let v = kindIndex typeFrom // argument type (vertical)
    let h = kindIndex typeTo   // return type (horizontal)
    ((v > h) || (v < 4 && h > 3)) && (h < 8) || (h <> v && (h = 11 || v = 11))

/// Conversions to floating point
let toFloat com (ctx: Context) r targetType (args: Expr list): Expr =
    let meth =
        match targetType with
        | Number(kind, _) -> "to" + kind.ToString()
        | x -> FableError $"Unexpected type in toFloat: %A{x}" |> raise
    match args.Head.Type with
    | Char ->
        let code = TypeCast(args.Head, Number(UInt32, NumberInfo.Empty))
        TypeCast(code, targetType)
    | String -> Helper.LibCall(com, "String", meth, targetType, args)
    | Number(kind, _) ->
        match kind with
        | BigInt -> Helper.LibCall(com, "BigInt", castBigIntMethod targetType, targetType, args)
        | Decimal -> Helper.LibCall(com, "Decimal", meth, targetType, args)
        | _ -> TypeCast(args.Head, targetType)
    | _ ->
        addWarning com ctx.InlinePath r "Cannot make conversion because source type is unknown"
        TypeCast(args.Head, targetType)

let toDecimal com (ctx: Context) r targetType (args: Expr list): Expr =
    match args.Head.Type with
    // | Char ->
    //     let code = TypeCast(args.Head, Number(UInt32, NumberInfo.Empty))
    //     Helper.LibCall(com, "Decimal", "fromChar", targetType, args)
    | Array(Number(Int32, NumberInfo.Empty), _) ->
        Helper.LibCall(com, "Decimal", "fromIntArray", targetType, args)
    | String ->
        Helper.LibCall(com, "Decimal", "fromString", targetType, args)
    | Number(kind, _) ->
        match kind with
        | Decimal -> args.Head
        | BigInt ->
            Helper.LibCall(com, "BigInt", castBigIntMethod targetType, targetType, args)
        | kind ->
            let meth = "from" + kind.ToString()
            Helper.LibCall(com, "Decimal", meth, targetType, args)
    | _ ->
        addWarning com ctx.InlinePath r "Cannot make conversion because source type is unknown"
        TypeCast(args.Head, targetType)

// // Apparently ~~ is faster than Math.floor (see https://coderwall.com/p/9b6ksa/is-faster-than-math-floor)
// let fastIntFloor expr =
//     let inner = makeUnOp None Any expr UnaryNotBitwise
//     makeUnOp None (Number(Int32, NumberInfo.Empty)) inner UnaryNotBitwise

let stringToInt com (ctx: Context) r targetType (args: Expr list): Expr =
    let kind =
        match targetType with
        | Number(kind, _) -> kind
        | x -> FableError $"Unexpected type in stringToInt: %A{x}" |> raise
    let style = int System.Globalization.NumberStyles.Any
    // let _isFloatOrDecimal, numberModule, unsigned, bitsize = getParseParams kind
    // let parseArgs = [makeIntConst style; makeBoolConst unsigned; makeIntConst bitsize]
    // TODO: NumberStyles
    let meth = "to" + kind.ToString()
    Helper.LibCall(com, "String", meth, targetType, [args.Head], ?loc=r)

// let toLong com (ctx: Context) r (unsigned: bool) targetType (args: Expr list): Expr =
//     let sourceType = args.Head.Type
//     match sourceType with
//     | Char ->
//         let code = TypeCast(args.Head, Number(UInt32, NumberInfo.Empty))
//         TypeCast(code, targetType)
//     | String -> stringToInt com ctx r targetType args
//     | Number(kind,_) ->
//         match kind with
//         | Decimal ->
//             let n = Helper.LibCall(com, "Decimal", "toNumber", Number(Float64, NumberInfo.Empty), args)
//             Helper.LibCall(com, "Long", "fromNumber", targetType, [n; makeBoolConst unsigned])
//         | BigInt -> Helper.LibCall(com, "BigInt", castBigIntMethod targetType, targetType, args)
//         | _ -> TypeCast(args.Head, targetType)
//     | _ ->
//         addWarning com ctx.InlinePath r "Cannot make conversion because source type is unknown"
//         TypeCast(args.Head, targetType)

// let round com (args: Expr list) =
//     match args.Head.Type with
//     | Number(Decimal, _) ->
//         let n = Helper.LibCall(com, "Decimal", "round", Number(Float64, NumberInfo.Empty), [args.Head])
//         let rounded = Helper.LibCall(com, "Util", "round", Number(Float64, NumberInfo.Empty), [n])
//         rounded::args.Tail
//     | Number((Float32|Float64), _) ->
//         let rounded = Helper.InstanceCall(args.Head, "round", Number(Float64, NumberInfo.Empty), [])
//         rounded::args.Tail
//     | _ -> args

/// Conversion to integers (excluding bigints)
let toInt com (ctx: Context) r targetType (args: Expr list) =
    let sourceType = args.Head.Type
    match sourceType, targetType with
    | Char, _ ->
        let code = TypeCast(args.Head, Number(UInt32, NumberInfo.Empty))
        TypeCast(code, targetType)
    | String, _ -> stringToInt com ctx r targetType args
    | Number(BigInt, _), _ ->
        Helper.LibCall(com, "BigInt", castBigIntMethod targetType, targetType, args)
    | Number(Decimal, _), Number(kind, _) ->
        let meth = "to" + kind.ToString()
        Helper.LibCall(com, "Decimal", meth, targetType, [args.Head])
    | Number _, Number _ -> TypeCast(args.Head, targetType)
    | _ ->
        addWarning com ctx.InlinePath r "Cannot make conversion because source type is unknown"
        TypeCast(args.Head, targetType)

let toRoundInt com (ctx: Context) r targetType i (args: Expr list) =
    let sourceType = args.Head.Type
    let args =
        match sourceType, targetType with
        | Number((Float32|Float64|Decimal), _), _ ->
            let rounded = makeInstanceCall r sourceType i args.Head "round" []
            rounded :: args.Tail
        | _ -> args
    toInt com ctx r targetType args

let toArray com t (expr: Expr) =
    match expr.Type with
    | Array _ -> expr
    | List _ -> Helper.LibCall(com, "List", "toArray", t, [expr])
    | String -> Helper.LibCall(com, "String", "toCharArray", t, [expr])
    | IEnumerable -> Helper.LibCall(com, "Seq", "toArray", t, [expr])
    | _ -> TypeCast(expr, t)

let toList com t (expr: Expr) =
    match expr.Type with
    | List _ -> expr
    | Array _ -> Helper.LibCall(com, "List", "ofArray", t, [expr])
    | String ->
        let a = Helper.LibCall(com, "String", "toCharArray", t, [expr])
        Helper.LibCall(com, "List", "ofArray", t, [a])
    | IEnumerable -> Helper.LibCall(com, "List", "ofSeq", t, [expr])
    | _ -> TypeCast(expr, t)

let toSeq com t (expr: Expr) =
    match expr.Type with
    | IEnumerable -> expr
    | List _ -> Helper.LibCall(com, "Seq", "ofList", t, [expr])
    | Array _ -> Helper.LibCall(com, "Seq", "ofArray", t, [expr])
    | String ->
        let a = Helper.LibCall(com, "String", "toCharArray", t, [expr])
        Helper.LibCall(com, "Seq", "ofArray", t, [a])
    | _ -> TypeCast(expr, t)

let emitFormat (com: ICompiler) r t (args: Expr list) macro =
    let args =
        match args with
        | [] -> [makeStrConst ""]
        | [arg] -> [makeStrConst "{0}"; arg]
        | [str; Value(NewArray(ArrayValues [], _, _), _)] -> [str]
        | [str; Value(NewArray(ArrayValues args, _, _), _)] -> str::args
        | (ExprType String)::restArgs -> args
        | _ -> (makeStrConst "{0}") :: args
    macro |> emitExpr r t args

let getLength com r t i (expr: Expr) =
    let typ = Number(UNativeInt, NumberInfo.Empty)
    let len = makeInstanceCall r typ i expr "len" []
    TypeCast(len, t)

let getMut expr =
    Helper.InstanceCall(expr, "get_mut", expr.Type, [])
    |> nativeCall

let applyOp (com: ICompiler) (ctx: Context) r t opName (args: Expr list) =
    let unOp operator operand =
        Operation(Unary(operator, operand), t, r)
    let binOp op left right =
        Operation(Binary(op, left, right), t, r)
    let truncateUnsigned operation = // see #1550
        match t with
        // | Number(UInt32,_) ->
        //     Operation(Binary(BinaryShiftRightZeroFill,operation,makeIntConst 0), t, r)
        | _ -> operation
    let logicOp op left right =
        Operation(Logical(op, left, right), Boolean, r)
    let nativeOp opName argTypes args =
        match opName, args with
        | Operators.addition, [left; right] -> binOp BinaryPlus left right
        | Operators.subtraction, [left; right] -> binOp BinaryMinus left right
        | Operators.multiply, [left; right] -> binOp BinaryMultiply left right
        | Operators.division, [left; right] -> binOp BinaryDivide left right
        | Operators.divideByInt, [left; right] ->
            binOp BinaryDivide left (TypeCast(right, t))
        | Operators.modulus, [left; right] -> binOp BinaryModulus left right
        | Operators.leftShift, [left; right] -> binOp BinaryShiftLeft left right |> truncateUnsigned // See #1530
        | Operators.rightShift, [left; right] ->
            match argTypes with
            // | Number(UInt32,_)::_ -> binOp BinaryShiftRightZeroFill left right // See #646
            | _ -> binOp BinaryShiftRightSignPropagating left right
        | Operators.bitwiseAnd, [left; right] -> binOp BinaryAndBitwise left right |> truncateUnsigned
        | Operators.bitwiseOr, [left; right] -> binOp BinaryOrBitwise left right |> truncateUnsigned
        | Operators.exclusiveOr, [left; right] -> binOp BinaryXorBitwise left right |> truncateUnsigned
        | Operators.booleanAnd, [left; right] -> logicOp LogicalAnd left right
        | Operators.booleanOr, [left; right] -> logicOp LogicalOr left right
        | Operators.logicalNot, [operand] -> unOp UnaryNotBitwise operand |> truncateUnsigned
        | Operators.unaryNegation, [operand] -> unOp UnaryMinus operand
        | _ ->
            $"Operator %s{opName} not found in %A{argTypes}"
            |> addErrorAndReturnNull com ctx.InlinePath r
    let argTypes = args |> List.map (fun a -> a.Type)
    match argTypes with
    | Number(BigInt as kind,_)::_ ->
        Helper.LibCall(com, "BigInt", opName, t, args, argTypes, ?loc=r)
    | Builtin (BclDateTime|BclDateTimeOffset as bt)::_ ->
        Helper.LibCall(com, coreModFor bt, opName, t, args, argTypes, ?loc=r)
    | Builtin (FSharpSet _)::_ ->
        let methName =
            match opName with
            | Operators.addition -> "union"
            | Operators.subtraction -> "difference"
            | _ -> opName
        Helper.LibCall(com, "Set", methName, t, args, argTypes, ?loc=r)
    // | Builtin (FSharpMap _)::_ ->
    //     let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpMap" true opName overloadSuffix.Value
    //     Helper.LibCall(com, "Map", mangledName, t, args, argTypes, ?loc=r)
    | Builtin BclTimeSpan::_ ->
        nativeOp opName argTypes args
    | CustomOp com ctx r t opName args e -> e
    | _ -> nativeOp opName argTypes args

let isCompatibleWithNativeComparison = function
    | Number((Int8|Int16|Int32|UInt8|UInt16|UInt32|Int64|UInt64|Float32|Float64),_)
    | Boolean | Char | String
    | GenericParam _ | Array _ | List _
    | Builtin (BclGuid|BclTimeSpan) -> true
    | _ -> false

// Overview of hash rules:
// * `hash`, `Unchecked.hash` first check if GetHashCode is implemented and then default to structural hash.
// * `.GetHashCode` called directly defaults to identity hash (for reference types except string) if not implemented.
// * `LanguagePrimitive.PhysicalHash` creates an identity hash no matter whether GetHashCode is implemented or not.

let identityHash com r (arg: Expr) =
    let methodName =
        match arg.Type with
        // These are the same for identity/structural hashing
        | Char | String | Builtin BclGuid -> "stringHash"
        | Number _ | Builtin BclTimeSpan -> "numberHash"
        | List _ -> "safeHash"
        | Tuple _ -> "arrayHash" // F# tuples must use structural hashing
        // These are only used for structural hashing
        // | Array _ -> "arrayHash"
        // | Builtin (BclDateTime|BclDateTimeOffset) -> "dateHash"
        // | Builtin (BclDecimal) -> "fastStructuralHash"
        | DeclaredType _ -> "safeHash"
        | _ -> "identityHash"
    Helper.LibCall(com, "Util", methodName, Number(Int32, NumberInfo.Empty), [arg], ?loc=r)

let structuralHash (com: ICompiler) r (arg: Expr) =
    let methodName =
        match arg.Type with
        | Char | String | Builtin BclGuid -> "stringHash"
        | Number _ | Builtin BclTimeSpan -> "numberHash"
        | List _ -> "safeHash"
        // TODO: Get hash functions of the generic arguments
        // for better performance when using tuples as map keys
        | Tuple _
        | Array _ -> "arrayHash"
        | Builtin (BclDateTime|BclDateTimeOffset) -> "dateHash"
        | DeclaredType(ent, _) ->
            let ent = com.GetEntity(ent)
            if not ent.IsInterface then "safeHash"
            else "structuralHash"
        | _ -> "structuralHash"
    Helper.LibCall(com, "Util", methodName, Number(Int32, NumberInfo.Empty), [arg], ?loc=r)

let equals (com: ICompiler) ctx r (left: Expr) (right: Expr) =
    match left.Type with
    // | Builtin (BclDecimal|BclBigInt as bt) ->
    //     Helper.LibCall(com, coreModFor bt, "equals", Boolean, [left; right], ?loc=r)
    // | Builtin (BclGuid|BclTimeSpan)
    // | Boolean | Char | String | Number _ ->
    //     makeEqOp r left right BinaryEqual
    | Builtin (BclDateTime|BclDateTimeOffset) ->
        Helper.LibCall(com, "DateTime", "equals", Boolean, [left; right], ?loc=r)
    | Array _ ->
        Helper.LibCall(com, "Array", "equals", Boolean, [left; right], ?loc=r)
    | List _ ->
        Helper.LibCall(com, "List", "equals", Boolean, [left; right], ?loc=r)
    | IEnumerable ->
        Helper.LibCall(com, "Seq", "equals", Boolean, [left; right], ?loc=r)
    | Builtin (FSharpSet _) ->
        Helper.LibCall(com, "Set", "equals", Boolean, [left; right], ?loc=r)
    | Builtin (FSharpMap _) ->
        Helper.LibCall(com, "Map", "equals", Boolean, [left; right], ?loc=r)
    // | DeclaredType _ ->
    //     Helper.LibCall(com, "Util", "equals", Boolean, [left; right], ?loc=r)
    // | MetaType ->
    //     Helper.LibCall(com, "Reflection", "equals", Boolean, [left; right], ?loc=r)
    // | Tuple _ ->
    //     Helper.LibCall(com, "Util", "equalArrays", Boolean, [left; right], ?loc=r)
    | _ ->
        // Helper.LibCall(com, "Util", "equals", Boolean, [left; right], ?loc=r)
        makeEqOp r left right BinaryEqual

let objectEquals (com: ICompiler) ctx r (left: Expr) (right: Expr) =
    // reference equality actually happens in Fable2Rust.transformOperation
    TypeCast(right, left.Type) |> equals com ctx r left

let structEquals (com: ICompiler) ctx r (left: Expr) (right: Expr) =
    TypeCast(right, left.Type) |> equals com ctx r left

/// Compare function that will call Util.compare or instance `CompareTo` as appropriate
let compare (com: ICompiler) ctx r (left: Expr) (right: Expr) =
    match left.Type with
    | Number (Decimal,_) ->
        Helper.LibCall(com, "Decimal", "compare", Number(Int32, NumberInfo.Empty), [ left; right ], ?loc = r)
    | Number (BigInt,_) ->
        Helper.LibCall(com, "BigInt", "compare", Number(Int32, NumberInfo.Empty), [ left; right ], ?loc = r)
    | Builtin (BclGuid|BclTimeSpan)
    | Boolean | Char | String | Number _ ->
        Helper.LibCall(com, "Util", "compare", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    | Builtin (BclDateTime|BclDateTimeOffset) ->
        Helper.LibCall(com, "DateTime", "compare", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    | Array _ ->
        Helper.LibCall(com, "Array", "compareTo", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    | List _ ->
        Helper.LibCall(com, "List", "compareTo", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    | IEnumerable ->
        Helper.LibCall(com, "Seq", "compareTo", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    | Builtin (FSharpSet _) ->
        Helper.LibCall(com, "Set", "compareTo", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    | Builtin (FSharpMap _) ->
        Helper.LibCall(com, "Map", "compareTo", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    | DeclaredType _ ->
        Helper.LibCall(com, "Util", "compare", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    // | Tuple _ ->
    //     Helper.LibCall(com, "Util", "compareArrays", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)
    | _ ->
        Helper.LibCall(com, "Util", "compare", Number(Int32, NumberInfo.Empty), [left; right], ?loc=r)

/// Boolean comparison operators like <, >, <=, >=
let booleanCompare (com: ICompiler) ctx r (left: Expr) (right: Expr) op =
    if isCompatibleWithNativeComparison left.Type then
        makeEqOp r left right op
    else
        let comparison = compare com ctx r left right
        makeEqOp r comparison (makeIntConst 0) op

let applyCompareOp (com: ICompiler) (ctx: Context) r t opName (left: Expr) (right: Expr) =
    let op =
        match opName with
        | Operators.equality | "Eq" -> BinaryEqual
        | Operators.inequality | "Neq" -> BinaryUnequal
        | Operators.lessThan | "Lt" -> BinaryLess
        | Operators.lessThanOrEqual | "Lte" -> BinaryLessOrEqual
        | Operators.greaterThan | "Gt" ->  BinaryGreater
        | Operators.greaterThanOrEqual | "Gte" -> BinaryGreaterOrEqual
        | _ -> FableError $"Unexpected operator %s{opName}" |> raise
    match op with
    | BinaryEqual ->
        equals com ctx r left right
    | BinaryUnequal ->
        let expr = equals com ctx r left right
        makeUnOp None Boolean expr UnaryNot
    | _ ->
        booleanCompare com ctx r left right op

let makeComparerFunction (com: ICompiler) ctx typArg =
    let x = makeUniqueIdent ctx typArg "x"
    let y = makeUniqueIdent ctx typArg "y"
    let body = compare com ctx None (IdentExpr x) (IdentExpr y)
    Delegate([x; y], body, None, Tags.empty)

let makeComparer (com: ICompiler) ctx typArg =
    objExpr ["Compare", makeComparerFunction com ctx typArg]

// let makeEqualityFunction (com: ICompiler) ctx typArg =
//     let x = makeUniqueIdent ctx typArg "x"
//     let y = makeUniqueIdent ctx typArg "y"
//     let body = equals com ctx None (IdentExpr x) (IdentExpr y)
//     Delegate([x; y], body, None, Tags.empty)

let makeEqualityComparer (com: ICompiler) ctx typArg =
    let x = makeUniqueIdent ctx typArg "x"
    let y = makeUniqueIdent ctx typArg "y"
    objExpr ["Equals",  Delegate([x; y], equals com ctx None (IdentExpr x) (IdentExpr y), None, Tags.empty)
             "GetHashCode", Delegate([x], structuralHash com None (IdentExpr x), None, Tags.empty)]

// TODO: Try to detect at compile-time if the object already implements `Compare`?
let inline makeComparerFromEqualityComparer e =
    e // leave it as is, if implementation supports it
    // Helper.LibCall(com, "Util", "comparerFromEqualityComparer", Any, [e])

/// Adds comparer as last argument for set creator methods
let makeSet (com: ICompiler) ctx r t args genArg =
    // let args = args @ [makeComparer com ctx genArg]
    let meth =
        match args with
        | [] -> "empty"
        | [ExprType(List _)] -> "ofList"
        | [ExprType(Array _)] -> "ofArray"
        | _ -> "ofSeq"
    Helper.LibCall(com, "Set", meth, t, args, ?loc=r)

/// Adds comparer as last argument for map creator methods
let makeMap (com: ICompiler) ctx r t args genArg =
    // let args = args @ [makeComparer com ctx genArg]
    let meth =
        match args with
        | [] -> "empty"
        | [ExprType(List _)] -> "ofList"
        | [ExprType(Array _)] -> "ofArray"
        | _ -> "ofSeq"
    Helper.LibCall(com, "Map", Naming.lowerFirst meth, t, args, ?loc=r)

// let makeDictionaryWithComparer com r t sourceSeq comparer =
//     Helper.LibCall(com, "MutableMap", "Dictionary", t, [sourceSeq; comparer], isConstructor=true, ?loc=r)

// let makeDictionary (com: ICompiler) ctx r t sourceSeq =
//     Helper.LibCall(com, "Dict", "ofSeq", t, [sourceSeq], ?loc=r)

// let makeHashSetWithComparer com r t sourceSeq comparer =
//     Helper.LibCall(com, "MutableSet", "HashSet", t, [sourceSeq; comparer], isConstructor=true, ?loc=r)

// let makeHashSet (com: ICompiler) ctx r t sourceSeq =
//     match t with
//     | DeclaredType(_,[key]) when not(isCompatibleWithNativeComparison key) ->
//         // makeComparer com ctx key
//         makeEqualityComparer com ctx key
//         |> makeHashSetWithComparer com r t sourceSeq
//     | _ -> Helper.GlobalCall("Set", t, [sourceSeq], isConstructor=true, ?loc=r)

let rec getZero (com: ICompiler) (ctx: Context) (t: Type) =
    match t with
    | Boolean -> makeBoolConst false
    | Number (BigInt,_) as t -> Helper.LibCall(com, "BigInt", "fromInt32", t, [makeIntConst 0])
    | Number (Decimal,_) as t -> Helper.LibValue(com, "Decimal", "Zero", t)
    | Number (kind, uom) -> NumberConstant (getBoxedZero kind, kind, uom) |> makeValue None
    | Char -> CharConstant '\u0000' |> makeValue None
    | String -> makeStrConst "" // TODO: Use null for string?
    | Array(typ,_) -> makeArray typ []
    | Builtin BclTimeSpan -> makeIntConst 0
    | Builtin BclDateTime -> Helper.LibCall(com, "DateTime", "min_value", t, [])
    | Builtin BclDateTimeOffset -> Helper.LibCall(com, "DateOffset", "min_value", t, [])
    | Builtin (FSharpSet genArg) -> makeSet com ctx None t [] genArg
    | Builtin BclGuid -> Helper.LibCall(com, "Guid", "empty", t, [])
    | Builtin (BclKeyValuePair(k,v)) ->
        makeTuple None true [getZero com ctx k; getZero com ctx v]
    | ListSingleton(CustomOp com ctx None t "get_Zero" [] e) -> e
    | _ ->
        Helper.LibCall(com, "Native", "defaultOf", t, [])

let getOne (com: ICompiler) (ctx: Context) (t: Type) =
    match t with
    | Boolean -> makeBoolConst true
    | Number (BigInt,_) -> Helper.LibCall(com, "BigInt", "fromInt32", t, [makeIntConst 1])
    | Number (Decimal,_) -> Helper.LibValue(com, "Decimal", "One", t)
    | Number (kind, uom) -> NumberConstant (getBoxedOne kind, kind, uom) |> makeValue None
    | ListSingleton(CustomOp com ctx None t "get_One" [] e) -> e
    | _ -> makeIntConst 1

let makeAddFunction (com: ICompiler) ctx t =
    let x = makeUniqueIdent ctx t "x"
    let y = makeUniqueIdent ctx t "y"
    let body = applyOp com ctx None t Operators.addition [IdentExpr x; IdentExpr y]
    Delegate([x; y], body, None, Tags.empty)

let makeGenericAdder (com: ICompiler) ctx t =
    objExpr [
        "GetZero", getZero com ctx t |> makeDelegate []
        "Add", makeAddFunction com ctx t
    ]

let makeGenericAverager (com: ICompiler) ctx t =
    let divideFn =
        let x = makeUniqueIdent ctx t "x"
        let i = makeUniqueIdent ctx (Number(Int32, NumberInfo.Empty)) "i"
        let body = applyOp com ctx None t Operators.divideByInt [IdentExpr x; IdentExpr i]
        Delegate([x; i], body, None, Tags.empty)
    objExpr [
        "GetZero", getZero com ctx t |> makeDelegate []
        "Add", makeAddFunction com ctx t
        "DivideByInt", divideFn
    ]

// let injectArg (com: ICompiler) (ctx: Context) r moduleName methName (genArgs: (string * Type) list) args =
//     let injectArgInner args (injectType, injectGenArgIndex) =
//         let fail () =
//             $"Cannot inject arg to %s{moduleName}.%s{methName} (genArgs %A{List.map fst genArgs} - expected index %i{injectGenArgIndex})"
//             |> addError com ctx.InlinePath r
//             args

//         match List.tryItem injectGenArgIndex genArgs with
//         | None -> fail()
//         | Some(_,genArg) ->
//             match injectType with
//             | Types.comparer ->
//                 args @ [makeComparer com ctx genArg]
//             | Types.equalityComparer ->
//                 args @ [makeEqualityComparer com ctx genArg]
//             | Types.arrayCons ->
//                 match genArg with
//                 | Number(numberKind,_) when com.Options.TypedArrays ->
//                     args @ [getTypedArrayName com numberKind |> makeIdentExpr]
//                 // Python will complain if we miss an argument
//                 | _ when com.Options.Language = Python ->
//                     args @ [ Expr.Value(ValueKind.NewOption(None, genArg, false), None) ]
//                 | _ -> args
//             | Types.adder ->
//                 args @ [makeGenericAdder com ctx genArg]
//             | Types.averager ->
//                 args @ [makeGenericAverager com ctx genArg]
//             | _ -> fail()

//     Map.tryFind moduleName ReplacementsInject.fableReplacementsModules
//     |> Option.bind (Map.tryFind methName)
//     |> function
//         | None -> args
//         | Some injectInfo -> injectArgInner args injectInfo

let tryOp com r t op args =
    Helper.LibCall(com, "Option", "tryOp", t, op::args, ?loc=r)

let tryCoreOp com r t coreModule coreMember args =
    let op = Helper.LibValue(com, coreModule, coreMember, Any)
    tryOp com r t op args

let fableCoreLib (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let fixDynamicImportPath = function
        | Value(StringConstant path, r) when path.EndsWith(".fs") ->
            // In imports *.ts extensions have to be converted to *.js extensions instead
            let fileExt = com.Options.FileExtension
            let fileExt = if fileExt.EndsWith(".ts") then Path.ChangeExtension(".js", fileExt) else fileExt
            Value(StringConstant(Path.ChangeExtension(path, fileExt)), r)
        | path -> path

    match i.DeclaringEntityFullName, i.CompiledName with
    | _, "op_ErasedCast" -> List.tryHead args
    | _, ".ctor" -> typedObjExpr t [] |> Some
    | _, ("jsNative"|"nativeOnly") ->
        // TODO: Fail at compile time?
        addWarning com ctx.InlinePath r $"{i.CompiledName} is being compiled without replacement, this will fail at runtime."
        let runtimeMsg =
            "A function supposed to be replaced by JS native code has been called, please check."
            |> StringConstant |> makeValue None
        makeThrow r t (error runtimeMsg) |> Some
    | _, ("nameof"|"nameof2" as meth) ->
        match args with
        | [Nameof com ctx name as arg] ->
            if meth = "nameof2"
            then makeTuple r true [makeStrConst name; arg] |> Some
            else makeStrConst name |> Some
        | _ -> "Cannot infer name of expression"
               |> addError com ctx.InlinePath r
               makeStrConst Naming.unknown |> Some
    | _, ("nameofLambda"|"namesofLambda" as meth) ->
        match args with
        | [Lambda(_, (Namesof com ctx names), _)] -> Some names
        | [MaybeCasted(IdentExpr ident)] ->
            match tryFindInScope ctx ident.Name with
            | Some(Lambda(_, (Namesof com ctx names), _)) -> Some names
            | _ -> None
        | _ -> None
        |> Option.defaultWith (fun () ->
            "Cannot infer name of expression"
            |> addError com ctx.InlinePath r
            [Naming.unknown])
        |> fun names ->
            if meth = "namesofLambda" then List.map makeStrConst names |> makeArray String |> Some
            else List.tryHead names |> Option.map makeStrConst

    | _, ("casenameWithFieldCount"|"casenameWithFieldIndex" as meth) ->
        let rec inferCasename = function
            | Lambda(arg, IfThenElse(Test(IdentExpr arg2, UnionCaseTest tag,_),thenExpr,_,_),_) when arg.Name = arg2.Name ->
                match arg.Type with
                | DeclaredType(e,_) ->
                    let e = com.GetEntity(e)
                    if e.IsFSharpUnion then
                        let c = e.UnionCases[tag]
                        let caseName = defaultArg c.CompiledName c.Name
                        if meth = "casenameWithFieldCount" then
                            Some(caseName, c.UnionCaseFields.Length)
                        else
                            match thenExpr with
                            | NestedRevLets(bindings, IdentExpr i) ->
                                bindings |> List.tryPick (fun (i2, v) ->
                                    match v with
                                    | Get(_, UnionField unionInfo,_,_) when i.Name = i2.Name -> Some unionInfo.FieldIndex
                                    | _ -> None)
                                |> Option.map (fun fieldIdx -> caseName, fieldIdx)
                            | _ -> None
                    else None
                | _ -> None
            | _ -> None

        match args with
        | [MaybeCasted(IdentExpr ident)] -> tryFindInScope ctx ident.Name |> Option.bind inferCasename
        | [e] -> inferCasename e
        | _ -> None
        |> Option.orElseWith (fun () ->
            "Cannot infer case name of expression"
            |> addError com ctx.InlinePath r
            Some(Naming.unknown, -1))
        |> Option.map (fun (s, i) ->
            makeTuple r true [makeStrConst s; makeIntConst i])

    // Extensions
    | _, "Async.AwaitPromise.Static" -> Helper.LibCall(com, "Async", "awaitPromise", t, args, ?loc=r) |> Some
    | _, "Async.StartAsPromise.Static" -> Helper.LibCall(com, "Async", "startAsPromise", t, args, ?loc=r) |> Some
    | _, "FormattableString.GetStrings" -> getFieldWith r t thisArg.Value "strs" |> Some

    | "Fable.Core.Testing.Assert", _ ->
        match i.CompiledName with
        | "AreEqual" -> Helper.LibCall(com, "Util", "assertEqual", t, args, ?loc=r) |> Some
        | "NotEqual" -> Helper.LibCall(com, "Util", "assertNotEqual", t, args, ?loc=r) |> Some
        | _ -> None
    | "Fable.Core.Reflection", meth ->
        Helper.LibCall(com, "Reflection", meth, t, args, ?loc=r) |> Some
    | "Fable.Core.Compiler", meth ->
        match meth with
        | "version" -> makeStrConst Literals.VERSION |> Some
        | "majorMinorVersion" ->
            try
                let m = Regex.Match(Literals.VERSION, @"^\d+\.\d+")
                float m.Value |> makeFloatConst |> Some
            with _ ->
                "Cannot parse compiler version"
                |> addErrorAndReturnNull com ctx.InlinePath r |> Some
        | "debugMode" -> makeBoolConst com.Options.DebugMode |> Some
        | "typedArrays" -> makeBoolConst com.Options.TypedArrays |> Some
        | "extension" -> makeStrConst com.Options.FileExtension |> Some
        | _ -> None
    | "Fable.Core.JsInterop", _ ->
        match i.CompiledName, args with
        | "importDynamic", [path] ->
            let path = fixDynamicImportPath path
            Helper.GlobalCall("import", t, [path], ?loc=r) |> Some
        | "importValueDynamic", [arg] ->
            let dynamicImport selector path =
                let path = fixDynamicImportPath path
                let import = Helper.GlobalCall("import", t, [path], ?loc=r)
                match selector with
                | StringConst "*" -> import
                | selector ->
                    let selector =
                        let m = makeIdent "m"
                        Delegate([m], Get(IdentExpr m, ExprGet selector, Any, None), None, Tags.empty)
                    makeInstanceCall r t i import "then" [selector]
            let arg =
                match arg with
                | IdentExpr ident ->
                    tryFindInScope ctx ident.Name
                    |> Option.defaultValue arg
                | arg -> arg
            match arg with
            // TODO: Check this is not a fable-library import?
            | Import(info,_,_) ->
                dynamicImport (makeStrConst info.Selector) (makeStrConst info.Path) |> Some
            | NestedLambda(args, Call(Import(importInfo,_,_),callInfo,_,_), None)
                when argEquals args callInfo.Args ->
                dynamicImport (makeStrConst importInfo.Selector) (makeStrConst importInfo.Path) |> Some
            | _ ->
                "The imported value is not coming from a different file"
                |> addErrorAndReturnNull com ctx.InlinePath r |> Some
        | Naming.StartsWith "import" suffix, _ ->
            match suffix, args with
            | "Member", [RequireStringConst com ctx r path]      -> makeImportUserGenerated r t Naming.placeholder path |> Some
            | "Default", [RequireStringConst com ctx r path]     -> makeImportUserGenerated r t "default" path |> Some
            | "SideEffects", [RequireStringConst com ctx r path] -> makeImportUserGenerated r t "" path |> Some
            | "All", [RequireStringConst com ctx r path]         -> makeImportUserGenerated r t "*" path |> Some
            | _, [RequireStringConst com ctx r selector; RequireStringConst com ctx r path] -> makeImportUserGenerated r t selector path |> Some
            | _ -> None
        // Dynamic casting, erase
        | "op_BangHat", [arg] -> Some arg
        | "op_BangBang", [arg] ->
            match arg, i.GenericArgs with
            | IsNewAnonymousRecord(_, exprs, fieldNames, _, _, _),
              [_; DeclaredType(ent, [])] ->
                let ent = com.GetEntity(ent)
                if ent.IsInterface then
                    FSharp2Fable.TypeHelpers.fitsAnonRecordInInterface com r exprs fieldNames ent
                    |> function
                       | Error errors ->
                            errors
                            |> List.iter (fun (range, error) -> addWarning com ctx.InlinePath range error)
                            Some arg
                       | Ok () ->
                            Some arg
                else Some arg
            | _ -> Some arg
        | _ -> None
    | "Fable.Core.Rust", _ ->
        match i.CompiledName, args with
        | "import", [RequireStringConst com ctx r selector; RequireStringConst com ctx r path] ->
            makeImportUserGenerated r t selector path |> Some
        | "importAll", [RequireStringConst com ctx r path] ->
            makeImportUserGenerated r t "*" path |> Some
        | "emitExpr", [args; RequireStringConst com ctx r macro] ->
            let args = destructureTupleArgs [args]
            emitExpr r t args macro |> Some
        | _ -> None
    | _ -> None

let refCells (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "get_Value", Some callee, _ -> getRefCell com r t callee |> Some
    | "set_Value", Some callee, [value] -> setRefCell com r callee value |> Some
    | _ -> None

let getMemberName isStatic (i: CallInfo) =
    let memberName = i.CompiledName |> FSharp2Fable.Helpers.cleanNameAsRustIdentifier
    if i.OverloadSuffix = ""
    then memberName
    elif isStatic
    then memberName + "__" + i.OverloadSuffix
    else memberName + "_" + i.OverloadSuffix

let getModuleAndMemberName (i: CallInfo) (thisArg: Expr option) =
    let isStatic = Option.isNone thisArg
    let entFullName = i.DeclaringEntityFullName.Replace("Microsoft.", "")
    let pos = entFullName.LastIndexOf('.')
    let moduleName = entFullName.Substring(0, pos)
    let entityName = entFullName.Substring(pos + 1) |> FSharp2Fable.Helpers.cleanNameAsRustIdentifier
    let memberName =
        if isStatic
        then entityName + "::" + (getMemberName isStatic i)
        else getMemberName isStatic i
    moduleName, memberName

let bclType (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg with
    | Some callee ->
        let memberName = getMemberName false i
        makeInstanceCall r t i callee memberName args |> Some
    | None ->
        let moduleName, memberName = getModuleAndMemberName i thisArg
        makeStaticLibCall com r t i moduleName memberName args |> Some

let fsharpModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let moduleName, memberName = getModuleAndMemberName i thisArg
    Helper.LibCall(com, moduleName, memberName, t, args, i.SignatureArgTypes, ?loc=r) |> Some

// // TODO: This is likely broken
// let getPrecompiledLibMangledName entityName memberName overloadSuffix isStatic =
//     let memberName = Naming.sanitizeIdentForbiddenChars memberName
//     let entityName = Naming.sanitizeIdentForbiddenChars entityName
//     let name, memberPart =
//         match entityName, isStatic with
//         | "", _ -> memberName, Naming.NoMemberPart
//         | _, true -> entityName, Naming.StaticMemberPart(memberName, overloadSuffix)
//         | _, false -> entityName, Naming.InstanceMemberPart(memberName, overloadSuffix)
//     Naming.buildNameWithoutSanitation name memberPart |> Naming.checkJsKeywords

let fsFormat (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "get_Value", Some callee, _ ->
        getFieldWith None t callee "input" |> Some
    | "PrintFormatToStringThen", _, _ ->
        match args with
        | [_] -> Helper.LibCall(com, "String", "toText", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [cont; fmt] -> makeInstanceCall r t i fmt "cont" [cont] |> Some
        | _ -> None
    | "PrintFormatToString", _, _ ->
        match args with
        | [template] -> Some template
        | _ -> Helper.LibCall(com, "String", "toText", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "PrintFormatLine", _, _ ->
        Helper.LibCall(com, "String", "toConsole", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ("PrintFormatToError"|"PrintFormatLineToError"), _, _ ->
        // addWarning com ctx.FileName r "eprintf will behave as eprintfn"
        Helper.LibCall(com, "String", "toConsoleError", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ("PrintFormatToTextWriter"|"PrintFormatLineToTextWriter"), _, _::args ->
        // addWarning com ctx.FileName r "fprintfn will behave as printfn"
        Helper.LibCall(com, "String", "toConsole", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "PrintFormat", _, _ ->
        // addWarning com ctx.FileName r "Printf will behave as printfn"
        Helper.LibCall(com, "String", "toConsole", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "PrintFormatThen", _, arg::callee::_ ->
        makeInstanceCall r t i callee "cont" [arg] |> Some
    | "PrintFormatToStringThenFail", _, _ ->
        Helper.LibCall(com, "String", "toFail", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ("PrintFormatToStringBuilder"      // bprintf
    |  "PrintFormatToStringBuilderThen"  // Printf.kbprintf
       ), _, _ -> fsharpModule com ctx r t i thisArg args
    | ".ctor", _, str::(Value(NewArray(ArrayValues templateArgs, _, _), _) as values)::_ ->
        let simpleFormats = [|"%b";"%c";"%d";"%f";"%g";"%i";"%s";"%u"|]
        match makeStringTemplateFrom simpleFormats templateArgs str with
        | Some stringTemplate -> makeValue r stringTemplate |> Some // Value(StringTemplate)
        | None -> Helper.LibCall(com, "String", "interpolate", t, [str; values], i.SignatureArgTypes, ?loc=r) |> Some
    | ".ctor", _, arg::_ ->
        Helper.LibCall(com, "String", "printf", t, [arg], i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let operators (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let math r t (args: Expr list) argTypes methName =
        let meth = Naming.lowerFirst methName
        match args with
        | thisArg::restArgs -> makeInstanceCall r t i thisArg meth restArgs
        | _ -> "Missing argument." |> addErrorAndReturnNull com ctx.InlinePath r

    match i.CompiledName, args with
    | ("DefaultArg" | "DefaultValueArg"), [opt; defValue] ->
        match opt with
        | MaybeInScope ctx (Value(NewOption(opt, _, _),_)) ->
            match opt with
            | Some value -> Some value
            | None -> Some defValue
        | _ -> Helper.LibCall(com, "Option", "defaultArg", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "DefaultAsyncBuilder", _ ->
        makeImportLib com t "singleton" "AsyncBuilder" |> Some
    // Erased operators.
    // KeyValuePair is already compiled as a tuple
    | ("KeyValuePattern"|"Identity"|"Box"|"Unbox"|"ToEnum"), [arg] -> TypeCast(arg, t) |> Some
    // Cast to unit to make sure nothing is returned when wrapped in a lambda, see #1360
    | "Ignore", _ ->
        Helper.LibCall(com, "Util", "ignore", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // Number and String conversions
    | ("ToSByte"|"ToByte"|"ToInt8"|"ToUInt8"|"ToInt16"|"ToUInt16"|"ToInt"|"ToUInt"|"ToInt32"|"ToUInt32"), _ ->
        toInt com ctx r t args |> Some
    | "ToInt64", _ -> toInt com ctx r t args |> Some
    | "ToUInt64", _ -> toInt com ctx r t args |> Some
    | "ToIntPtr", _ -> toInt com ctx r t args |> Some
    | "ToUIntPtr", _ -> toInt com ctx r t args |> Some
    | ("ToSingle"|"ToDouble"), _ -> toFloat com ctx r t args |> Some
    | "ToDecimal", _ -> toDecimal com ctx r t args |> Some
    | "ToChar", _ -> toChar com args.Head |> Some
    | "ToString", _ -> toString com ctx r args |> Some
    | "CreateSequence", [xs] -> toSeq com t xs |> Some
    | "CreateDictionary", [arg] ->
        Helper.LibCall(com, "HashMap", "fromArray", t, [toArray com t arg]) |> Some
    | "CreateSet", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeSet com ctx r t args |> Some
    // Ranges
    | ("op_Range"|"op_RangeStep"), _ ->
        let genArg = genArg com ctx r 0 i.GenericArgs
        let addStep args =
            match args with
            | [first; last] -> [first; getOne com ctx genArg; last]
            | _ -> args
        let meth, args =
            match genArg with
            | Char -> "rangeChar", args
            | _ -> "rangeNumeric", addStep args
        Helper.LibCall(com, "Range", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // Pipes and composition
    | "op_PipeRight", [x; f]
    | "op_PipeLeft", [f; x] -> curriedApply r t f [x] |> Some
    | "op_PipeRight2", [x; y; f]
    | "op_PipeLeft2", [f; x; y] -> curriedApply r t f [x; y] |> Some
    | "op_PipeRight3", [x; y; z; f]
    | "op_PipeLeft3", [f; x; y; z] -> curriedApply r t f [x; y; z] |> Some
    | "op_ComposeRight", [f1; f2] -> compose com ctx r t f1 f2 |> Some
    | "op_ComposeLeft", [f2; f1] -> compose com ctx r t f1 f2 |> Some
    // Strings
    | ("PrintFormatToString"             // sprintf
    |  "PrintFormatToStringThen"         // Printf.ksprintf
    |  "PrintFormat" | "PrintFormatLine" // printf / printfn
    |  "PrintFormatToError"              // eprintf
    |  "PrintFormatLineToError"          // eprintfn
    |  "PrintFormatThen"                 // Printf.kprintf
    |  "PrintFormatToStringThenFail"     // Printf.failwithf
    |  "PrintFormatToStringBuilder"      // bprintf
    |  "PrintFormatToStringBuilderThen"  // Printf.kbprintf
        ), _ -> fsFormat com ctx r t i thisArg args
    | ("Failure"
    |  "FailurePattern"  // (|Failure|_|)
    |  "LazyPattern"     // (|Lazy|_|)
    |  "NullArg"         // nullArg
    |  "Using"           // using
       ), _ -> fsharpModule com ctx r t i thisArg args
    |  "Lock", _ ->
        Helper.LibCall(com, "Monitor", "lock", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    // Exceptions
    | "FailWith", [msg] | "InvalidOp", [msg] ->
        makeThrow r t (error msg) |> Some
    | "InvalidArg", [argName; msg] ->
        let msg = add (add msg (str "\\nParameter name: ")) argName
        makeThrow r t (error msg) |> Some
    | "Raise", [arg] -> makeThrow r t arg |> Some
    | "Reraise", _ ->
        match ctx.CaughtException with
        | Some ex -> makeThrow r t (IdentExpr ex) |> Some
        | None ->
            "`reraise` used in context where caught exception is not available, please report"
            |> addError com ctx.InlinePath r
            makeThrow r t (error (str "")) |> Some
    // Math functions
    // TODO: optimize square pow: x * x
    | ("Pow" | "PowInteger" | "op_Exponentiation"), _ ->
        let argTypes = args |> List.map (fun a -> a.Type)
        match argTypes with
        | Number(Decimal,_)::_ ->
            Helper.LibCall(com, "Decimal", "pown", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
        | Number((Float32 | Float64), _)::_ ->
            let meth = if i.CompiledName = "PowInteger" then "powi" else "powf"
            math r t args i.SignatureArgTypes meth |> Some
        | CustomOp com ctx r t "Pow" args e -> Some e
        | _ ->
            math r t args i.SignatureArgTypes "pow" |> Some
    | ("Ceiling" | "Floor" as meth), _ ->
        let meth = Naming.lowerFirst meth
        match args with
        | ExprType(Number(Decimal,_))::_ ->
            Helper.LibCall(com, "Decimal", meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
        | _ ->
            let meth = if meth = "ceiling" then "ceil" else meth
            math r t args i.SignatureArgTypes meth |> Some
    | "Log", [arg1; arg2] ->
        math r t args i.SignatureArgTypes "log" |> Some
    | "Log", [arg] ->
        math r t args i.SignatureArgTypes "ln" |> Some
    | "Abs", _ ->
        match args with
        | ExprType(Number(Decimal,_))::_ ->
            Helper.LibCall(com, "Decimal", "abs", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
        | _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    | ("Acos" | "Asin" | "Atan" | "Atan2" | "Cos" | "Cosh" | "Exp"), _
    | ("Log2" | "Log10"| "Sin" | "Sinh" | "Sqrt" | "Tan" | "Tanh"), _ ->
        math r t args i.SignatureArgTypes i.CompiledName |> Some
    | "Round", _ ->
        match args with
        | [ExprType(Number(Decimal,_))] ->
            Helper.LibCall(com, "Decimal", "round", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
        | [ExprType(Number(Decimal,_)); ExprType(Number(Int32,_))] ->
            Helper.LibCall(com, "Decimal", "roundTo", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
        | [ExprType(Number(Decimal,_)); mode] ->
            Helper.LibCall(com, "Decimal", "roundMode", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [ExprType(Number(Decimal,_)); dp; mode] ->
            Helper.LibCall(com, "Decimal", "roundToMode", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [ExprTypeAs(Number(Float64,_), arg)] ->
            //TODO: other midpoint modes for Double
            makeInstanceCall r t i arg "round" [] |> Some
        | _ -> None
    | "Truncate", [arg] ->
        match args with
        | ExprType(Number(Decimal,_))::_ ->
            Helper.LibCall(com, "Decimal", "truncate", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
        | _ ->
            makeInstanceCall r t i arg "trunc" [] |> Some
    | "Sign", [arg] ->
        makeInstanceCall r t i arg "signum" [] |> Some
    | "DivRem", _ ->
        match args with
        | [x; y] ->
            Helper.LibCall(com, "Util", "divRem", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [x; y; rem] ->
            Helper.LibCall(com, "Util", "divRemOut", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ -> None
    // Numbers
    | "Infinity", _ ->
        makeGlobalIdent("f64", "INFINITY", t) |> Some
    | "InfinitySingle", _ ->
        makeGlobalIdent("f32", "INFINITY", t) |> Some
    | "NaN", _ ->
        makeGlobalIdent("f64", "NAN", t) |> Some
    | "NaNSingle", _ ->
        makeGlobalIdent("f32", "NAN", t) |> Some
    | "Fst", [tup] -> Get(tup, TupleIndex 0, t, r) |> Some
    | "Snd", [tup] -> Get(tup, TupleIndex 1, t, r) |> Some
    // Reference
    | "op_Dereference", [arg] -> getRefCell com r t arg  |> Some
    | "op_ColonEquals", [o; v] -> setRefCell com r o v |> Some
    | "Ref", [arg] -> makeRefCell com r t arg |> Some
    | "Increment", [arg] ->
        let v = add (getRefCell com r t arg) (getOne com ctx t)
        setRefCell com r arg v |> Some
    | "Decrement", [arg] ->
        let v = sub (getRefCell com r t arg) (getOne com ctx t)
        setRefCell com r arg v |> Some
    // Concatenates two lists
    | "op_Append", _ -> Helper.LibCall(com, "List", "append", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | "IsNull", [arg] -> nullCheck r true arg |> Some
    | "Hash", [arg] -> structuralHash com r arg |> Some
    // Comparison
    | Patterns.SetContains Operators.compareSet, [left; right] ->
        applyCompareOp com ctx r t i.CompiledName left right |> Some
    | "Compare", [left; right] -> compare com ctx r left right |> Some
    | ("Min"|"Max"|"Clamp" as meth), _ ->
        math r t args i.SignatureArgTypes i.CompiledName |> Some
    | "Not", [operand] -> // TODO: Check custom operator?
        makeUnOp r t operand UnaryNot |> Some
    | Patterns.SetContains Operators.standardSet, _ ->
        applyOp com ctx r t i.CompiledName args |> Some
    // Type info
    | "TypeOf", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeTypeInfo r |> Some
    | "TypeDefOf", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeTypeDefinitionInfo r |> Some
    | _ -> None

let chars (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ReplaceName [ "ToUpper",          "toUpperChar"
                    "ToUpperInvariant", "toUpperChar"
                    "ToLower",          "toLowerChar"
                    "ToLowerInvariant", "toLowerChar" ] methName, None, [c] ->
        Helper.LibCall(com, "String", methName, Char, args) |> Some
    | "ToString", None, [ExprType(Char)] -> toString com ctx r args |> Some
    | "ToString", Some c, [] -> toString com ctx r [c] |> Some
    | ReplaceName [ "IsControl",        "is_control"
                    "IsDigit",          "is_ascii_digit"
                    "IsLetter",         "is_alphabetic"
                    "IsLetterOrDigit",  "is_alphanumeric"       // imprecise, TODO:
                    "IsUpper",          "is_uppercase"
                    "IsLower",          "is_lowercase"
                    "IsNumber",         "is_numeric"
                    "IsPunctuation",    "is_ascii_punctuation"  // imprecise, TODO:
                    "IsSeparator",      "is_ascii_whitespace"   // imprecise, TODO:
                    "IsSymbol",         "is_ascii_punctuation"  // imprecise, TODO:
                    "IsWhiteSpace",     "is_whitespace" ] methName, None, args ->
        match args with
        | [c] -> makeInstanceCall r t i c methName [] |> Some
        | [str; idx] ->
            let c = Helper.LibCall(com, "String", "getCharAt", Char, args)
            makeInstanceCall r t i c methName [] |> Some
        | _ -> None

    // | "GetUnicodeCategory" , None, args -> //TODO:
    // | "IsHighSurrogate" | "IsLowSurrogate" | "IsSurrogate" ->
    //     let methName = Naming.lowerFirst i.CompiledName
    //     let methName = if List.length args > 1 then methName + "2" else methName
    //     Helper.LibCall(com, "Char", methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // | "IsSurrogatePair" | "Parse" ->
    //     let methName = Naming.lowerFirst i.CompiledName
    //     Helper.LibCall(com, "Char", methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

// let implementedStringFunctions =
//     set [| "Compare"
//            "CompareTo"
//            "EndsWith"
//            "Format"
//            "IndexOfAny"
//            "Insert"
//            "IsNullOrEmpty"
//            "IsNullOrWhiteSpace"
//            "PadLeft"
//            "PadRight"
//            "Remove"
//            "Replace"
//            "Substring"
//         |]

let getEnumerator com r t i (expr: Expr) =
    match expr.Type with
    | IsEntity (Types.keyCollection) _
    | IsEntity (Types.valueCollection) _
    | IsEntity (Types.icollectionGeneric) _
    | Array _ ->
        Helper.LibCall(com, "Seq", "Enumerable::ofArray", t, [expr], ?loc=r)
    | List _ ->
        Helper.LibCall(com, "Seq", "Enumerable::ofList", t, [expr], ?loc=r)
    | IsEntity (Types.hashset) _
    | IsEntity (Types.iset) _ ->
        let ar = Helper.LibCall(com, "HashSet", "entries", t, [expr])
        Helper.LibCall(com, "Seq", "Enumerable::ofArray", t, [ar], ?loc=r)
    | IsEntity (Types.dictionary) _
    | IsEntity (Types.idictionary) _
    | IsEntity (Types.ireadonlydictionary) _ ->
        let ar = Helper.LibCall(com, "HashMap", "entries", t, [expr], [expr.Type])
        Helper.LibCall(com, "Seq", "Enumerable::ofArray", t, [ar], ?loc=r)
    | _ ->
        // Helper.LibCall(com, "Util", "getEnumerator", t, [toSeq com Any expr], ?loc=r)
        makeInstanceCall r t i expr "GetEnumerator" []

let strings (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ ->
        match i.SignatureArgTypes with
        | [Char; Number(Int32, _)] ->
            Helper.LibCall(com, "String", "fromChar", t, args, ?loc=r) |> Some
        | [Array(Char,_)] ->
            Helper.LibCall(com, "String", "fromChars", t, args, ?loc=r) |> Some
        | [Array(Char,_); Number(Int32, _); Number(Int32, _)] ->
            Helper.LibCall(com, "String", "fromChars2", t, args, ?loc=r) |> Some
        | _ -> None
    | "get_Length", Some c, _ ->
        Helper.LibCall(com, "String", "length", t, c::args, ?loc=r) |> Some
    | "get_Chars", Some c, _ ->
        Helper.LibCall(com, "String", "getCharAt", t, c::args, ?loc=r) |> Some
    | "Equals", Some x, [y] | "Equals", None, [x; y] ->
        makeEqOp r x y BinaryEqual |> Some
    // | "Equals", Some x, [y; kind] | "Equals", None, [x; y; kind] ->
    //     let left = Helper.LibCall(com, "String", "compare", Number(Int32, NumberInfo.Empty), [x; y; kind])
    //     makeEqOp r left (makeIntConst 0) BinaryEqual |> Some
    | "GetEnumerator", Some c, _ -> getEnumerator com r t i c |> Some
    | "IsNullOrEmpty", None, _ ->
        Helper.LibCall(com, "String", "isEmpty", t, args, ?loc=r) |> Some
    | "IsNullOrWhiteSpace", None, _ ->
        Helper.LibCall(com, "String", "isWhitespace", t, args, ?loc=r) |> Some
    | "Contains", Some c, _ ->
        match args with
        | [ExprType Char] ->
            Helper.LibCall(com, "String", "containsChar", t, c::args, ?loc=r) |> Some
        | [ExprType String] ->
            Helper.LibCall(com, "String", "contains", t, c::args, ?loc=r) |> Some
        | _ -> None
    | "Replace", Some c, _ ->
        match args with
        | [ExprType String; ExprType String] ->
            Helper.LibCall(com, "String", "replace", t, c::args, ?loc=r) |> Some
        | _ -> None
    | "StartsWith", Some c, _ ->
        match args with
        | [ExprType Char] ->
            Helper.LibCall(com, "String", "startsWithChar", t, c::args, ?loc=r) |> Some
        | [ExprType String] ->
            Helper.LibCall(com, "String", "startsWith", t, c::args, ?loc=r) |> Some
        | _ -> None
    | "EndsWith", Some c, _ ->
        match args with
        | [ExprType Char] ->
            Helper.LibCall(com, "String", "endsWithChar", t, c::args, ?loc=r) |> Some
        | [ExprType String] ->
            Helper.LibCall(com, "String", "endsWith", t, c::args, ?loc=r) |> Some
        | _ -> None
    | ReplaceName [ "ToUpper",          "toUpper"
                    "ToUpperInvariant", "toUpper"
                    "ToLower",          "toLower"
                    "ToLowerInvariant", "toLower" ] methName, Some c, args ->
        Helper.LibCall(com, "String", methName, t, c::args, ?loc=r) |> Some
    | ("IndexOf" | "LastIndexOf" | "IndexOfAny" | "LastIndexOfAny"), Some c, _ ->
        let suffixOpt =
            match args with
            | [ExprType String] -> Some ""
            | [ExprType String; ExprType(Number(Int32, _))] -> Some "2"
            | [ExprType String; ExprType(Number(Int32, _)); ExprType(Number(Int32, _))] -> Some "3"
            | [ExprType Char] -> Some "Char"
            | [ExprType Char; ExprType(Number(Int32, _))] -> Some "Char2"
            | [ExprType Char; ExprType(Number(Int32, _)); ExprType(Number(Int32, _))] -> Some "Char3"
            | [ExprType(Array(Char,_))] -> Some ""
            | [ExprType(Array(Char,_)); ExprType(Number(Int32, _))] -> Some "2"
            | [ExprType(Array(Char,_)); ExprType(Number(Int32, _)); ExprType(Number(Int32, _))] -> Some "3"
            | _ -> None
        match suffixOpt with
        | Some suffix ->
            let methName = (Naming.lowerFirst i.CompiledName) + suffix
            Helper.LibCall(com, "String", methName, t, c::args, ?loc=r) |> Some
        | _ -> None
    | ("PadLeft" | "PadRight"), Some c, _ ->
        let methName = Naming.lowerFirst i.CompiledName
        match args with
        | [ExprTypeAs(Number(Int32, _), arg)] ->
            let ch = makeTypeConst None Char ' '
            Helper.LibCall(com, "String", methName, t, [c; arg; ch], ?loc=r) |> Some
        | [ExprType(Number(Int32, _)); ExprType Char] ->
            Helper.LibCall(com, "String", methName, t, c::args, ?loc=r) |> Some
        | _ -> None
    | ("Trim" | "TrimStart" | "TrimEnd"), Some c, _ ->
        let methName = Naming.lowerFirst i.CompiledName
        match args with
        | [] ->
            Helper.LibCall(com, "String", methName, t, c::args, ?loc=r) |> Some
        | [ExprType Char] ->
            Helper.LibCall(com, "String", methName + "Char", t, c::args, ?loc=r) |> Some
        | [ExprType(Array(Char,_))] ->
            Helper.LibCall(com, "String", methName + "Chars", t, c::args, ?loc=r) |> Some
        | _ -> None
    | "ToCharArray", Some c, _ ->
        match args with
        | [] ->
            Helper.LibCall(com, "String", "toCharArray", t, c::args, ?loc=r) |> Some
        | [ExprType(Number(Int32, _)); ExprType(Number(Int32, _))] ->
            Helper.LibCall(com, "String", "toCharArray2", t, c::args, ?loc=r) |> Some
        | _ -> None
    | "Split", Some c, _ ->
        match args with
        | [] ->
            Helper.LibCall(com, "String", "split", t, [c; makeStrConst ""; makeIntConst -1; makeIntConst 0], ?loc=r) |> Some

        | [ExprTypeAs(String, arg1)] ->
            Helper.LibCall(com, "String", "split", t, [c; arg1; makeIntConst -1; makeIntConst 0], ?loc=r) |> Some
        | [ExprTypeAs(String, arg1); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2)] ->
            Helper.LibCall(com, "String", "split", t, [c; arg1; makeIntConst -1; arg2], ?loc=r) |> Some
        | [ExprTypeAs(String, arg1); ExprTypeAs(Number(Int32, _), arg2); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg3)] ->
            Helper.LibCall(com, "String", "split", t, [c; arg1; arg2; arg3], ?loc=r) |> Some

        | [Value(NewArray(ArrayValues [arg1], String, _), _)] ->
            Helper.LibCall(com, "String", "split", t, [c; arg1; makeIntConst -1; makeIntConst 0], ?loc=r) |> Some
        | [Value(NewArray(ArrayValues [arg1], String, _), _); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2)] ->
            Helper.LibCall(com, "String", "split", t, [c; arg1; makeIntConst -1; arg2], ?loc=r) |> Some
        | [Value(NewArray(ArrayValues [arg1], String, _), _); ExprTypeAs(Number(Int32, _), arg2); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg3)] ->
            Helper.LibCall(com, "String", "split", t, [c; arg1; arg2; arg3], ?loc=r) |> Some

        | [ExprTypeAs(Char, arg1)] ->
            Helper.LibCall(com, "String", "splitChars", t, [c; makeArray Char [arg1]; makeIntConst -1; makeIntConst 0], ?loc=r) |> Some
        | [ExprTypeAs(Char, arg1); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2)] ->
            Helper.LibCall(com, "String", "splitChars", t, [c; makeArray Char [arg1]; makeIntConst -1; arg2], ?loc=r) |> Some
        | [ExprTypeAs(Char, arg1); ExprTypeAs(Number(Int32, _), arg2); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg3)] ->
            Helper.LibCall(com, "String", "splitChars", t, [c; makeArray Char [arg1]; arg2; arg3], ?loc=r) |> Some

        | [ExprTypeAs(Array(Char,_), arg1)] ->
            Helper.LibCall(com, "String", "splitChars", t, [c; arg1; makeIntConst -1; makeIntConst 0], ?loc=r) |> Some
        | [ExprTypeAs(Array(Char,_), arg1); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2)] ->
            Helper.LibCall(com, "String", "splitChars", t, [c; arg1; makeIntConst -1; arg2], ?loc=r) |> Some
        | [ExprTypeAs(Array(Char,_), arg1); ExprTypeAs(Number(Int32, _), arg2)] ->
            Helper.LibCall(com, "String", "splitChars", t, [c; arg1; arg2; makeIntConst 0], ?loc=r) |> Some
        | [ExprTypeAs(Array(Char,_), arg1); ExprTypeAs(Number(Int32, _), arg2); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg3)] ->
            Helper.LibCall(com, "String", "splitChars", t, [c; arg1; arg2; arg3], ?loc=r) |> Some

        // TODO: handle arrays of string separators with more than one element
        | _ -> None
    | "Insert", Some c, _ ->
        Helper.LibCall(com, "String", "insert", t, c::args, ?loc=r) |> Some
    | "Remove", Some c, _ ->
        match args with
        | [ExprType(Number(Int32, _))] ->
            Helper.LibCall(com, "String", "remove", t, c::args, ?loc=r) |> Some
        | [ExprType(Number(Int32, _)); ExprType(Number(Int32, _))] ->
            Helper.LibCall(com, "String", "remove2", t, c::args, ?loc=r) |> Some
        | _ -> None
    | "Substring", Some c, _ ->
        match args with
        | [ExprType(Number(Int32, _))] ->
            Helper.LibCall(com, "String", "substring", t, c::args, ?loc=r) |> Some
        | [ExprType(Number(Int32, _)); ExprType(Number(Int32, _))] ->
            Helper.LibCall(com, "String", "substring2", t, c::args, ?loc=r) |> Some
        | _ -> None
    | "Join", None, _ ->
        let args =
            match args with
            | [ ExprTypeAs(String, sep);
                ExprTypeAs(IEnumerable, arg) ] ->
                [sep; toArray com t arg]
            | [ ExprTypeAs(String, sep);
                ExprTypeAs(Array(String,_), arg) ] ->
                [sep; arg]
            | [ ExprTypeAs(Char, sep);
                ExprTypeAs(Array(String,_), arg) ] ->
                let sep = Helper.LibCall(com, "String", "ofChar", String, [sep])
                [sep; arg]
            | [ ExprTypeAs(String, sep);
                ExprTypeAs(Array(String,_), arg);
                ExprTypeAs(Number(Int32, _), idx);
                ExprTypeAs(Number(Int32, _), cnt) ] ->
                let arg = Helper.LibCall(com, "Array", "getSubArray", Array(String, MutableArray), [arg; idx; cnt])
                [sep; arg]
            | [ ExprTypeAs(Char, sep);
                ExprTypeAs(Array(String,_), arg);
                ExprTypeAs(Number(Int32, _), idx);
                ExprTypeAs(Number(Int32, _), cnt) ] ->
                let sep = Helper.LibCall(com, "String", "ofChar", String, [sep])
                let arg = Helper.LibCall(com, "Array", "getSubArray", Array(String, MutableArray), [arg; idx; cnt])
                [sep; arg]
            | _ -> []
        if not (List.isEmpty args) then
            Helper.LibCall(com, "String", "join", t, args, ?loc=r) |> Some
        else None
    | "Concat", None, _ ->
        match args with
        | [ExprTypeAs(IEnumerable, arg)] ->
            Helper.LibCall(com, "String", "concat", t, [toArray com t arg], ?loc=r) |> Some
        | [ExprType String; ExprType String]
        | [ExprType String; ExprType String; ExprType String]
        | [ExprType String; ExprType String; ExprType String; ExprType String] ->
            Helper.LibCall(com, "String", "concat", t, [makeArray String args], ?loc=r) |> Some
        | [ExprType(Array(String,_))] ->
            Helper.LibCall(com, "String", "concat", t, args, ?loc=r) |> Some
        | _ -> None
    | "CompareTo", Some c, [ExprType String] ->
        Helper.LibCall(com, "Util", "compare", t, c::args, ?loc=r) |> Some
    | "Compare", None, [ExprType String; ExprType String] ->
        Helper.LibCall(com, "Util", "compare", t, args, ?loc=r) |> Some
    | "CompareOrdinal", None, [ExprType String; ExprType String] ->
        Helper.LibCall(com, "Util", "compare", t, args, ?loc=r) |> Some
    // TODO: more compare overloads
    | "Format", None, _ ->
        "format!" |> emitFormat com r t args |> Some
    // | Patterns.SetContains implementedStringFunctions, thisArg, args ->
    //     Helper.LibCall(com, "String", Naming.lowerFirst i.CompiledName, t, args, i.SignatureArgTypes,
    //                     hasSpread=i.HasSpread, ?thisArg=thisArg, ?loc=r) |> Some
    | _ -> None

let stringModule (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "Concat", [sep; arg] ->
        Helper.LibCall(com, "String", "join", t, [sep; toArray com t arg], ?loc=r) |> Some
    | ReplaceName [ "Initialize",     "init"
                    "Iterate",        "iter"
                    "IterateIndexed", "iteri"
                    "MapIndexed",     "mapi" ] methName, args ->
        Helper.LibCall(com, "String", methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, args ->
        Helper.LibCall(com, "String", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let formattableString (com: ICompiler) (_ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    // Even if we're going to wrap it again to make it compatible with FormattableString API, we use a JS template string
    // because the strings array will always have the same reference so it can be used as a key in a WeakMap
    // Attention, if we change the shape of the object ({ strs, args }) we need to change the resolution
    // of the FormattableString.GetStrings extension in Fable.Core too
    | "Create", None, [StringConst str; Value(NewArray(ArrayValues args, _, _),_)] ->
        let matches = Regex.Matches(str, @"\{\d+(.*?)\}") |> Seq.cast<Match> |> Seq.toArray
        let hasFormat = matches |> Array.exists (fun m -> m.Groups[1].Value.Length > 0)
        let tag =
            if not hasFormat then
                Helper.LibValue(com, "String", "fmt", Any) |> Some
            else
                let fmtArg =
                    matches
                    |> Array.map (fun m -> makeStrConst m.Groups[1].Value)
                    |> Array.toList
                    |> makeArray String
                Helper.LibCall(com, "String", "fmtWith", Any, [fmtArg]) |> Some
        let holes = matches |> Array.map (fun m -> {| Index = m.Index; Length = m.Length |})
        let template = makeStringTemplate tag str holes args |> makeValue r
        // Use a type cast to keep the FormattableString type
        TypeCast(template, t) |> Some
    | "get_Format", Some x, _ -> Helper.LibCall(com, "String", "getFormat", t, [x], ?loc=r) |> Some
    | "get_ArgumentCount", Some x, _ -> getFieldWith r t (getField x "args") "length" |> Some
    | "GetArgument", Some x, [idx] -> getExpr r t (getField x "args") idx |> Some
    | "GetArguments", Some x, [] -> getFieldWith r t x "args" |> Some
    | _ -> None

let seqModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "Cast", [arg] -> Some arg // Erase
    // | "ToArray", [arg] ->
    //     Helper.LibCall(com, "Array", "ofSeq", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "ToList", [arg] ->
        Helper.LibCall(com, "List", "ofSeq", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "CreateEvent", [addHandler; removeHandler; createHandler] ->
        Helper.LibCall(com, "Event", "createEvent", t, [addHandler; removeHandler], i.SignatureArgTypes, ?loc=r) |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth), args ->
        let meth = Naming.lowerFirst meth
        Helper.LibCall(com, "Seq", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        Helper.LibCall(com, "Seq", meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let resizeArrays (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, [] ->
        // makeArray (getElementType t) [] |> Some
        Helper.LibCall(com, "Native", "arrayEmpty", t, [], ?loc=r) |> Some
    | ".ctor", _, [ExprTypeAs(Number(Int32, _), idx)] ->
        Helper.LibCall(com, "Native", "arrayWithCapacity", t, [idx], ?loc=r) |> Some
    // Optimize expressions like `ResizeArray [|1|]` or `ResizeArray [1]`
    | ".ctor", _, [ArrayOrListLiteral(vals, typ)] ->
        makeArray typ vals |> Some
    | ".ctor", _, [arg] -> toArray com t arg |> Some
    | "get_Item", Some ar, [idx] -> getExpr r t ar idx |> Some
    | "set_Item", Some ar, [idx; value] -> setExpr r ar idx value |> Some
    | "Add", Some(MaybeCasted(ar)), [arg] ->
        makeInstanceCall r t i (getMut ar) "push" [arg] |> Some
    | "Remove", Some(MaybeCasted(ar)), [arg] ->
        Helper.LibCall(com, "Array", "removeInPlace", t, [arg; ar], ?loc=r) |> Some
    | "RemoveAll", Some ar, [arg] ->
        Helper.LibCall(com, "Array", "removeAllInPlace", t, [arg; ar], ?loc=r) |> Some
    | "FindIndex", Some ar, [arg] ->
        makeInstanceCall r t i ar "findIndex" [arg] |> Some
    | "FindLastIndex", Some ar, [arg] ->
        Helper.LibCall(com, "Array", "findLastIndex", t, [arg; ar], ?loc=r) |> Some
    | "ForEach", Some ar, [arg] ->
        makeInstanceCall r t i ar "forEach" [arg] |> Some
    | "GetEnumerator", Some(MaybeCasted(ar)), _ ->
        Helper.LibCall(com, "Seq", "Enumerable::ofArray", t, [ar], ?loc=r) |> Some
    | "get_Count", Some(MaybeCasted(ar)), _ ->
        getLength com r t i ar |> Some
    | "Clear", Some(MaybeCasted(ar)), [] ->
        makeInstanceCall r t i (getMut ar) "clear" [] |> Some
    | "ConvertAll", Some ar, [arg] ->
        Helper.LibCall(com, "Array", "map", t, [arg; ar], ?loc=r) |> Some
    | "Find", Some ar, [arg] ->
        let opt = Helper.LibCall(com, "Array", "tryFind", t, [arg; ar], ?loc=r)
        Helper.LibCall(com, "Option", "defaultArg", t, [opt; getZero com ctx t], ?loc=r) |> Some
    | "Exists", Some ar, [arg] ->
        Helper.LibCall(com, "Array", "exists", t, [arg; ar], i.SignatureArgTypes, ?loc=r) |> Some
    | "FindLast", Some ar, [arg] ->
        let opt = Helper.LibCall(com, "Array", "tryFindBack", t, [arg; ar], ?loc=r)
        Helper.LibCall(com, "Option", "defaultArg", t, [opt; getZero com ctx t], ?loc=r) |> Some
    | "FindAll", Some ar, [arg] ->
        Helper.LibCall(com, "Array", "filter", t, [arg; ar], ?loc=r) |> Some
    | "AddRange", Some ar, [arg] ->
        Helper.LibCall(com, "Array", "addRangeInPlace", t, [arg; ar], ?loc=r) |> Some
    | "GetRange", Some ar, [idx; cnt] ->
        Helper.LibCall(com, "Array", "getSubArray", t, [ar; idx; cnt], ?loc=r) |> Some
    | "Contains", Some(MaybeCasted(ar)), [arg] ->
        Helper.LibCall(com, "Array", "contains", t, [arg; ar], i.SignatureArgTypes, ?loc=r) |> Some
    | "IndexOf", Some ar, [arg] ->
        Helper.LibCall(com, "Array", "indexOf", t, [ar; arg], i.SignatureArgTypes, ?loc=r) |> Some
    | "Insert", Some ar, [idx; arg] ->
        makeInstanceCall r t i (getMut ar) "insert" [toNativeIndex idx; arg] |> Some
    | "InsertRange", Some ar, [idx; arg] ->
        Helper.LibCall(com, "Array", "insertRangeInPlace", t, [idx; arg; ar], ?loc=r) |> Some
    | "RemoveRange", Some ar, args ->
        makeInstanceCall r t i ar "splice" args |> Some
    | "RemoveAt", Some ar, [idx] ->
        makeInstanceCall r t i (getMut ar) "remove" [toNativeIndex idx] |> Some
    | "Reverse", Some ar, [] ->
        makeInstanceCall r t i (getMut ar) "reverse" args |> Some
    | "Sort", Some ar, [] ->
        // can't use .sort() as it needs T: Ord
        Helper.LibCall(com, "Array", "sortInPlace", t, [ar], i.SignatureArgTypes, ?loc=r) |> Some
    | "Sort", Some ar, [ExprType(DelegateType _) as comparer] ->
        let cmp = Helper.LibCall(com, "Native", "comparer", t, [comparer], ?loc=r)
        makeInstanceCall r t i (getMut ar) "sort_by" [cmp] |> Some
    // | "Sort", Some ar, [arg] ->
    //     Helper.LibCall(com, "Array", "sortInPlaceWithComparer", t, [ar; arg], i.SignatureArgTypes, ?loc=r) |> Some
    | "ToArray", Some ar, [] ->
        Helper.LibCall(com, "Native", "arrayCopy", t, [ar], ?loc=r) |> Some
    | _ -> None

let tuples (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let changeKind isStruct = function
        | Value(NewTuple(args, _), r)::_ -> Value(NewTuple(args, isStruct), r) |> Some
        | (ExprType(Tuple(genArgs, _)) as e)::_ -> TypeCast(e, Tuple(genArgs, isStruct)) |> Some
        | _ -> None
    match i.CompiledName, thisArg with
    | (".ctor"|"Create"), _ ->
        let isStruct = i.DeclaringEntityFullName.StartsWith("System.ValueTuple")
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

let createArray (com: ICompiler) ctx r t size value =
    match t, value with
    | Array(typ,_), None ->
        let value = getZero com ctx typ
        Value(NewArray(makeTuple None true [value; size] |> ArrayFrom, typ, MutableArray), r)
    | Array(typ,_), Some value ->
        Value(NewArray(makeTuple None true [value; size] |> ArrayFrom, typ, MutableArray), r)
    | _ ->
        $"Expecting an array type but got %A{t}"
        |> addErrorAndReturnNull com ctx.InlinePath r

let copyToArray (com: ICompiler) r t (i: CallInfo) args =
    Helper.LibCall(com, "Array", "copyTo", t, args, i.SignatureArgTypes, ?loc=r) |> Some

let arrays (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "get_Length", Some arg, _ -> getLength com r t i arg |> Some
    | "get_Item", Some arg, [idx] -> getExpr r t arg idx |> Some
    | "set_Item", Some arg, [idx; value] -> setExpr r arg idx value |> Some
    | "Clone", Some callee, _ ->
        Helper.LibCall(com, "Native", "arrayCopy", t, [callee], ?loc=r) |> Some
    | "Copy", None, [_source; _sourceIndex; _target; _targetIndex; _count] -> copyToArray com r t i args
    | "Copy", None, [source; target; count] -> copyToArray com r t i [source; makeIntConst 0; target; makeIntConst 0; count]
    | "ConvertAll", None, [source; mapping] ->
        Helper.LibCall(com, "Array", "map", t, [mapping; source], ?loc=r) |> Some
    | "IndexOf", None, [ar; arg] ->
        Helper.LibCall(com, "Array", "indexOf", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "GetEnumerator", Some ar, _ ->
        Helper.LibCall(com, "Seq", "Enumerable::ofArray", t, [ar], ?loc=r) |> Some
    | "Reverse", None, [ar] ->
        makeInstanceCall r t i (getMut ar) "reverse" [] |> Some
    | "Sort", None, [ar] ->
        // can't use .sort() as it needs T: Ord
        Helper.LibCall(com, "Array", "sortInPlace", t, [ar], i.SignatureArgTypes, ?loc=r) |> Some
    | "Sort", None, [ar; ExprType(DelegateType _) as comparer] ->
        let cmp = Helper.LibCall(com, "Native", "comparer", t, [comparer], ?loc=r)
        makeInstanceCall r t i (getMut ar) "sort_by" [cmp] |> Some
    // | "Sort", None, [ar; arg] ->
    //     Helper.LibCall(com, "Array", "sortInPlaceWithComparer", t, [ar; arg], i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let arrayModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "ToSeq", [arg] ->
        Helper.LibCall(com, "Seq", "ofArray", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "OfSeq", [arg] ->
        Helper.LibCall(com, "Seq", "toArray", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "OfList", [arg] ->
        Helper.LibCall(com, "List", "toArray", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "ToList", args ->
        Helper.LibCall(com, "List", "ofArray", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ("Length" | "Count"), [arg] -> getLength com r t i arg |> Some
    | "Item", [idx; ar] -> getExpr r t ar idx |> Some
    | "Get", [ar; idx] -> getExpr r t ar idx |> Some
    | "Set", [ar; idx; value] -> setExpr r ar idx value |> Some
    | "ZeroCreate", [count] -> createArray com ctx r t count None |> Some
    | "Create", [count; value] -> createArray com ctx r t count (Some value) |> Some
    | "Empty", [] -> createArray com ctx r t (makeIntConst 0) None |> Some
    | "Singleton", [value] -> createArray com ctx r t (makeIntConst 1) (Some value) |> Some
    | "IsEmpty", [ar] ->
        makeInstanceCall r t i ar "is_empty" [] |> Some
    | "Copy", [ar] ->
        Helper.LibCall(com, "Native", "arrayCopy", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "CopyTo", args ->
        copyToArray com r t i args
    | ("Concat" | "Transpose" as meth), [arg] ->
        Helper.LibCall(com, "Array", Naming.lowerFirst meth, t, [toArray com t arg], i.SignatureArgTypes, ?loc=r) |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth), args ->
        let meth = Naming.lowerFirst meth
        Helper.LibCall(com, "Array", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        Helper.LibCall(com, "Array", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let lists (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    // Use methods for Head and Tail (instead of Get(ListHead) for example) to check for empty lists
    | ReplaceName
      [ "get_Head",   "head"
        "get_Tail",   "tail"
        "get_Item",   "item"
        "get_Length", "length"
        "GetSlice",   "getSlice" ] methName, Some x, _ ->
            let args = match args with [ExprType Unit] -> [x] | args -> args @ [x]
            Helper.LibCall(com, "List", methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "get_IsEmpty", Some c, _ -> Test(c, ListTest false, r) |> Some
    | "get_Empty", None, _ -> NewList(None, (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    | "Cons", None, [h;t] -> NewList(Some(h,t), (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    | ("GetHashCode" | "Equals" | "CompareTo"), Some c, _ ->
        makeInstanceCall r t i c i.CompiledName args |> Some
    | "GetEnumerator", Some c, _ ->
        Helper.LibCall(com, "Seq", "Enumerable::ofList", t, [c], ?loc=r) |> Some
    | _ -> None

let listModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "IsEmpty", [arg] -> Test(arg, ListTest false, r) |> Some
    | "Empty", _ -> NewList(None, (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    | "Singleton", [arg] ->
        NewList(Some(arg, Value(NewList(None, t), None)), (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    // Use a cast to give it better chances of optimization (e.g. converting list
    // literals to arrays) after the beta reduction pass
    | "ToSeq", [arg] ->
        Helper.LibCall(com, "Seq", "ofList", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "OfSeq", [arg] ->
        Helper.LibCall(com, "List", "ofSeq", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ("Concat" | "Transpose" as meth), [arg] ->
        Helper.LibCall(com, "List", Naming.lowerFirst meth, t, [toList com t arg], i.SignatureArgTypes, ?loc=r) |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy" as meth), args ->
        let meth = Naming.lowerFirst meth
        Helper.LibCall(com, "List", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        Helper.LibCall(com, "List", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let discardUnitArgs args =
    match args with
    | Value(UnitConstant, _) :: rest -> rest
    | _ -> args

let sets (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let args = discardUnitArgs args
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        (genArg com ctx r 0 i.GenericArgs) |> makeSet com ctx r t args |> Some
    | ReplaceName [
        "get_MinimumElement", "minElement"
        "get_MaximumElement", "maxElement"
        "IsSubsetOf", "isSubset"
        "IsSupersetOf", "isSuperset"
        "IsProperSubsetOf", "isProperSubset"
        "IsProperSupersetOf", "isProperSuperset"
        "CopyTo", "copyToArray"] meth, Some callee ->
        Helper.LibCall(com, "Set", meth, t, callee::args, ?loc=r) |> Some
    | meth, _ ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        let args = match thisArg with Some callee -> args @ [callee] | _ -> args
        Helper.LibCall(com, "Set", meth, t, args, ?loc=r) |> Some

let setModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let meth = Naming.lowerFirst i.CompiledName
    Helper.LibCall(com, "Set", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let maps (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let args = discardUnitArgs args
    match i.CompiledName, thisArg with
    | ".ctor", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeMap com ctx r t args |> Some
    | ReplaceName [
        "CopyTo", "copyToArray" ] meth, Some callee ->
        Helper.LibCall(com, "Map", meth, t, callee::args, ?loc=r) |> Some
    | meth, _ ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        let args = match thisArg with Some callee -> args @ [callee] | _ -> args
        Helper.LibCall(com, "Map", meth, t, args, ?loc=r) |> Some

let mapModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let meth = Naming.lowerFirst i.CompiledName
    Helper.LibCall(com, "Map", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let results (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | ("Bind" | "Map" | "MapError") as meth ->
        Helper.LibCall(com, "Result", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let nullables (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", None -> List.tryHead args
    // | "get_Value", Some c -> Get(c, OptionValue, t, r) |> Some // Get(OptionValueOptionValue) doesn't do a null check
    | "get_Value", Some c -> Helper.LibCall(com, "Option", "value", t, [c], ?loc=r) |> Some
    | "get_HasValue", Some c -> Test(c, OptionTest true, r) |> Some
    | _ -> None

// See fable-library/Option.ts for more info on how options behave in Fable runtime
let options (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | "get_Value", Some c -> Get(c, OptionValue, t, r) |> Some
    | "get_IsSome", Some c -> Test(c, OptionTest true, r) |> Some
    | "get_IsNone", Some c -> Test(c, OptionTest false, r) |> Some
    | _ -> None

let optionModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "None", _ -> NewOption(None, t, false) |> makeValue r |> Some
    | "GetValue", [c] -> Get(c, OptionValue, t, r) |> Some
    | ("OfObj" | "OfNullable"), _ -> None // TODO:
    | ("ToObj" | "ToNullable"), _ -> None // TODO:
    | "IsSome", [c] -> Test(c, OptionTest true, r) |> Some
    | "IsNone", [c] -> Test(c, OptionTest false, r) |> Some
    | "ToArray", [arg] ->
        Helper.LibCall(com, "Array", "ofOption", t, args, ?loc=r) |> Some
    | "ToList", [arg] ->
        Helper.LibCall(com, "List", "ofOption", t, args, ?loc=r) |> Some
    | meth, args ->
        Helper.LibCall(com, "Option", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let parseBool (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | ("Parse" | "TryParse" as method), args ->
        let func = Naming.lowerFirst method
        Helper.LibCall(com, "Boolean", func, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let parseNum (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let parseCall meth str args style =
        let kind =
            match i.DeclaringEntityFullName with
            | Patterns.DicContains FSharp2Fable.TypeHelpers.numberTypes kind -> kind
            | x -> FableError $"Unexpected type in parse: %A{x}" |> raise
        let isFloatOrDecimal, numberModule, unsigned, bitsize =
            getParseParams kind
        let outValue =
            if meth = "TryParse" then [List.last args] else []
        let args =
            if isFloatOrDecimal then [str] @ outValue
            else [str; makeIntConst style; makeBoolConst unsigned; makeIntConst bitsize] @ outValue
        Helper.LibCall(com, numberModule, Naming.lowerFirst meth, t, args, ?loc=r) |> Some

    let isFloat =
        match i.SignatureArgTypes.Head with
        | Number((Float32 | Float64),_) -> true
        | _ -> false

    match i.CompiledName, args with
    | "IsNaN", [arg] when isFloat ->
        makeInstanceCall r t i arg "is_nan" [] |> Some
    | "IsPositiveInfinity", [arg] when isFloat ->
        let op1 = makeInstanceCall r t i arg "is_sign_positive" []
        let op2 = makeInstanceCall r t i arg "is_infinite" []
        Operation(Logical(LogicalAnd, op1, op2), t, None) |> Some
    | "IsNegativeInfinity", [arg] when isFloat ->
        let op1 = makeInstanceCall r t i arg "is_sign_negative" []
        let op2 = makeInstanceCall r t i arg "is_infinite" []
        Operation(Logical(LogicalAnd, op1, op2), t, None) |> Some
    | "IsInfinity", [arg] when isFloat ->
        makeInstanceCall r t i arg "is_infinite" [] |> Some
    | ("Parse" | "TryParse") as meth,
            str::NumberConst(:? int as style,_)::_ ->
        let hexConst = int System.Globalization.NumberStyles.HexNumber
        let intConst = int System.Globalization.NumberStyles.Integer
        if style <> hexConst && style <> intConst then
            $"%s{i.DeclaringEntityFullName}.%s{meth}(): NumberStyle %d{style} is ignored"
            |> addWarning com ctx.InlinePath r
        let acceptedArgs = if meth = "Parse" then 2 else 3
        if List.length args > acceptedArgs then
            // e.g. Double.Parse(string, style, IFormatProvider) etc.
            $"%s{i.DeclaringEntityFullName}.%s{meth}(): provider argument is ignored"
            |> addWarning com ctx.InlinePath r
        parseCall meth str args style
    | ("Parse" | "TryParse") as meth, str::_ ->
        let acceptedArgs = if meth = "Parse" then 1 else 2
        if List.length args > acceptedArgs then
            // e.g. Double.Parse(string, IFormatProvider) etc.
            $"%s{i.DeclaringEntityFullName}.%s{meth}(): provider argument is ignored"
            |> addWarning com ctx.InlinePath r
        let style = int System.Globalization.NumberStyles.Any
        parseCall meth str args style
    | "ToString", [ExprTypeAs(String, format)] ->
        let format = emitExpr r String [format] "'{0:' + $0 + '}'"
        Helper.LibCall(com, "String", "format", t, [format; thisArg.Value], [format.Type; thisArg.Value.Type], ?loc=r) |> Some
    | "ToString", _ ->
        Helper.GlobalCall("String", String, [thisArg.Value], ?loc=r) |> Some
    | _ ->
        None

let decimals (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | (".ctor" | "MakeDecimal"), ([low; mid; high; isNegative; scale] as args) ->
        Helper.LibCall(com, "Decimal", "fromParts", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ".ctor", [Value(NewArray(ArrayValues ([low; mid; high; signExp] as args),_,_),_)] ->
        Helper.LibCall(com, "Decimal", "fromInts", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ".ctor", [arg] -> toDecimal com ctx r t args |> Some
    | "GetBits", _ ->
        Helper.LibCall(com, "Decimal", "getBits", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Parse", _ ->
        Helper.LibCall(com, "Decimal", "parse", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "TryParse", _ ->
        Helper.LibCall(com, "Decimal", "tryParse", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | Operators.lessThan, [left; right] -> booleanCompare com ctx r left right BinaryLess |> Some
    | Operators.lessThanOrEqual, [left; right] -> booleanCompare com ctx r left right BinaryLessOrEqual |> Some
    | Operators.greaterThan, [left; right] -> booleanCompare com ctx r left right BinaryGreater |> Some
    | Operators.greaterThanOrEqual, [left; right] -> booleanCompare com ctx r left right BinaryGreaterOrEqual |> Some
    |(Operators.addition
    | Operators.subtraction
    | Operators.multiply
    | Operators.division
    | Operators.divideByInt
    | Operators.modulus
    | Operators.unaryNegation), _ ->
        applyOp com ctx r t i.CompiledName args |> Some
    | "op_Explicit", _ -> toDecimal com ctx r t args |> Some
    | ("Ceiling" | "Floor" | "Truncate" |
        "Add" | "Subtract" | "Multiply" | "Divide" | "Remainder" | "Negate" as meth), _ ->
        let meth = Naming.lowerFirst meth
        Helper.LibCall(com, "Decimal", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ("get_Zero" | "get_One" | "get_MinusOne" | "get_MinValue" | "get_MaxValue"), _ ->
        Helper.LibValue(com, "Decimal", Naming.removeGetSetPrefix i.CompiledName, t) |> Some
    | "get_Scale", [] ->
        match thisArg with
        | Some c ->
            Helper.LibCall(com, "Decimal", "scale", t, [c], i.SignatureArgTypes, ?loc=r) |> Some
        | None -> None
    | "Round", _ ->
        match args with
        | [x] ->
            Helper.LibCall(com, "Decimal", "round", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [x; ExprTypeAs(Number(Int32,_), dp)] ->
            Helper.LibCall(com, "Decimal", "roundTo", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [x; mode] ->
            Helper.LibCall(com, "Decimal", "roundMode", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [x; dp; mode] ->
            Helper.LibCall(com, "Decimal", "roundToMode", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ -> None
    // | "ToString", [ExprTypeAs(String, format)] ->
    //     let format = emitExpr r String [format] "'{0:' + $0 + '}'"
    //     Helper.LibCall(com, "String", "format", t, [format; thisArg.Value], [format.Type; thisArg.Value.Type], ?loc=r) |> Some
    | "ToString", _ ->
        Helper.LibCall(com, "Decimal", "toString", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let bigints (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", None ->
        match i.SignatureArgTypes with
        | [Array _] ->
            Helper.LibCall(com, "BigInt", "fromByteArray", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [Number ((Int64|UInt64),_)] ->
            Helper.LibCall(com, "BigInt", "fromInt64", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ ->
            Helper.LibCall(com, "BigInt", "fromInt32", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "op_Explicit", None ->
        match t with
        | Number(kind,_) ->
            match kind with
            | Int8 | Int16 | Int32 | Int64
            | UInt8 | UInt16 | UInt32 | UInt64 ->
                toInt com ctx r t args |> Some
            | Float32 | Float64 ->
                toFloat com ctx r t args |> Some
            | Decimal ->
                toDecimal com ctx r t args |> Some
            | BigInt | NativeInt | UNativeInt -> None
        | _ -> None
    | "DivRem", None ->
        Helper.LibCall(com, "BigInt", "divRem", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, None when meth.StartsWith("get_") ->
        Helper.LibValue(com, "BigInt", meth, t) |> Some
    | meth, None ->
        Helper.LibCall(com, "BigInt", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, Some c ->
        Helper.LibCall(com, "BigInt", Naming.lowerFirst meth, t, c::args, i.SignatureArgTypes, ?loc=r) |> Some

// Compile static strings to their constant values
// reference: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/languageprimitives.errorstrings-module-%5bfsharp%5d
let errorStrings = function
    | "InputArrayEmptyString" -> str "The input array was empty" |> Some
    | "InputSequenceEmptyString" -> str "The input sequence was empty" |> Some
    | "InputMustBeNonNegativeString" -> str "The input must be non-negative" |> Some
    | _ -> None

let languagePrimitives (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | Naming.EndsWith "Dynamic" operation, arg::_ ->
        let operation = if operation = Operators.divideByInt then operation else "op_" + operation
        if operation = "op_Explicit" then Some arg // TODO
        else applyOp com ctx r t operation args |> Some
    | "DivideByInt", _ -> applyOp com ctx r t i.CompiledName args |> Some
    | "GenericZero", _ -> Helper.LibCall(com, "Native", "getZero", t, []) |> Some
    | "GenericOne", _ -> getOne com ctx t |> Some
    | ("SByteWithMeasure"
    | "Int16WithMeasure"
    | "Int32WithMeasure"
    | "Int64WithMeasure"
    | "Float32WithMeasure"
    | "FloatWithMeasure"
    | "DecimalWithMeasure"), [arg] -> arg |> Some
    | "EnumOfValue", [arg] -> TypeCast(arg, t) |> Some
    | "EnumToValue", [arg] -> TypeCast(arg, t) |> Some
    | ("GenericHash" | "GenericHashIntrinsic"), [arg] ->
        structuralHash com r arg |> Some
    | ("FastHashTuple2" | "FastHashTuple3" | "FastHashTuple4" | "FastHashTuple5"
    | "GenericHashWithComparer" | "GenericHashWithComparerIntrinsic"), [comp; arg] ->
        makeInstanceCall r t i comp "GetHashCode" [arg] |> Some
    | ("GenericComparison" | "GenericComparisonIntrinsic"), [left; right] ->
        compare com ctx r left right |> Some
    | ("FastCompareTuple2" | "FastCompareTuple3" | "FastCompareTuple4" | "FastCompareTuple5"
    | "GenericComparisonWithComparer" | "GenericComparisonWithComparerIntrinsic"), [comp; left; right] ->
        makeInstanceCall r t i comp "Compare" [left; right] |> Some
    | ("GenericLessThan" | "GenericLessThanIntrinsic"), [left; right] ->
        booleanCompare com ctx r left right BinaryLess |> Some
    | ("GenericLessOrEqual" | "GenericLessOrEqualIntrinsic"), [left; right] ->
        booleanCompare com ctx r left right BinaryLessOrEqual |> Some
    | ("GenericGreaterThan" | "GenericGreaterThanIntrinsic"), [left; right] ->
        booleanCompare com ctx r left right BinaryGreater |> Some
    | ("GenericGreaterOrEqual" | "GenericGreaterOrEqualIntrinsic"), [left; right] ->
        booleanCompare com ctx r left right BinaryGreaterOrEqual |> Some
    | ("GenericEquality" | "GenericEqualityIntrinsic"), [left; right] ->
        equals com ctx r left right |> Some
    | ("GenericEqualityER" | "GenericEqualityERIntrinsic"), [left; right] ->
        // TODO: In ER mode, equality on two NaNs returns "true".
        equals com ctx r left right |> Some
    | ("FastEqualsTuple2" | "FastEqualsTuple3" | "FastEqualsTuple4" | "FastEqualsTuple5"
    | "GenericEqualityWithComparer" | "GenericEqualityWithComparerIntrinsic"), [comp; left; right] ->
        makeInstanceCall r t i comp "Equals" [left; right] |> Some
    | ("PhysicalEquality" | "PhysicalEqualityIntrinsic"), [left; right] ->
        makeEqOp r left right BinaryEqual |> Some
    | ("PhysicalHash" | "PhysicalHashIntrinsic"), [arg] ->
        Helper.LibCall(com, "Util", "physicalHash", Number(Int32, NumberInfo.Empty), [arg], ?loc=r) |> Some
    | ("GenericEqualityComparer"
    |  "GenericEqualityERComparer"
    |  "FastGenericComparer"
    |  "FastGenericComparerFromTable"
    |  "FastGenericEqualityComparer"
    |  "FastGenericEqualityComparerFromTable"
        ), _ -> fsharpModule com ctx r t i thisArg args
    | ("ParseInt32"|"ParseUInt32"), [arg] -> toInt com ctx r t [arg] |> Some
    | "ParseInt64", [arg] -> toInt com ctx r t [arg] |> Some
    | "ParseUInt64", [arg] -> toInt com ctx r t [arg] |> Some
    | _ -> None

let intrinsicFunctions (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    // Erased operators
    | "CheckThis", _, [arg]
    | "UnboxFast", _, [arg]
    | "UnboxGeneric", _, [arg] -> Some arg
    | "MakeDecimal", _, _ -> decimals com ctx r t i thisArg args
    | "GetString", _, [ar; idx] ->
        Helper.LibCall(com, "String", "getCharAt", t, args, ?loc=r) |> Some
    | "GetStringSlice", None, [ar; lower; upper] ->
        Helper.LibCall(com, "String", "getSlice", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "GetArray", _, [ar; idx] -> getExpr r t ar idx |> Some
    | "SetArray", _, [ar; idx; value] -> setExpr r ar idx value |> Some
    | "GetArraySlice", None, [ar; lower; upper] ->
        Helper.LibCall(com, "Array", "getSlice", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "SetArraySlice", None, args ->
        Helper.LibCall(com, "Array", "setSlice", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ("TypeTestGeneric" | "TypeTestFast"), None, [expr] ->
        Test(expr, TypeTest((genArg com ctx r 0 i.GenericArgs)), r) |> Some
    // | "CreateInstance", None, _ ->
    //     match genArg com ctx r 0 i.GenericArgs with
    //     | DeclaredType(ent, _) ->
    //         let ent = com.GetEntity(ent)
    //         Helper.ConstructorCall(constructor com ent, t, [], ?loc=r) |> Some
    //     | t -> $"Cannot create instance of type unresolved at compile time: %A{t}"
    //            |> addErrorAndReturnNull com ctx.InlinePath r |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.powdouble-function-%5bfsharp%5d
    // Type: PowDouble : float -> int -> float
    // Usage: PowDouble x n
    | "PowDouble", None, (thisArg::restArgs) ->
        makeInstanceCall r t i thisArg "powf" restArgs |> Some
    | "PowDecimal", None, _ ->
        Helper.LibCall(com, "Decimal", "pown", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangechar-function-%5bfsharp%5d
    // Type: RangeChar : char -> char -> seq<char>
    // Usage: RangeChar start stop
    | "RangeChar", None, _ ->
        Helper.LibCall(com, "Range", "rangeChar", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangedouble-function-%5bfsharp%5d
    // Type: RangeDouble: float -> float -> float -> seq<float>
    // Usage: RangeDouble start step stop
    | ("RangeSByte" | "RangeByte"
    | "RangeInt16"  | "RangeUInt16"
    | "RangeInt32"  | "RangeUInt32"
    | "RangeInt64"  | "RangeUInt64"
    | "RangeSingle" | "RangeDouble"), None, args ->
        Helper.LibCall(com, "Range", "rangeNumeric", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let runtimeHelpers (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, args with
    | "GetHashCode", [arg] -> identityHash com r arg |> Some
    | _ -> None

// ExceptionDispatchInfo is used to raise exceptions through different threads in async workflows
// We don't need to do anything in JS, see #2396
let exceptionDispatchInfo (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg, args with
    | "Capture", _, [arg] -> Some arg
    | "Throw", Some arg, _ -> makeThrow r t arg |> Some
    | _ -> None

let funcs (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg with
    // Just use Emit to change the type of the arg, Fable will automatically uncurry the function
    | "Adapt", _ -> emitExpr r t args "$0" |> Some
    | "Invoke", Some callee ->
        Helper.Application(callee, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let keyValuePairs (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> makeTuple r true args |> Some
    | "get_Key", Some c -> Get(c, TupleIndex 0, t, r) |> Some
    | "get_Value", Some c -> Get(c, TupleIndex 1, t, r) |> Some
    | _ -> None

let dictionaries (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, [] ->
        Helper.LibCall(com, "HashMap", "empty", t, args) |> Some
    | ".ctor", _, [ExprType(Number _)] ->
        Helper.LibCall(com, "HashMap", "withCapacity", t, args) |> Some
    | ".ctor", _, [ExprType(IEnumerable)] ->
        let a = Helper.LibCall(com, "Seq", "toArray", t, args)
        Helper.LibCall(com, "HashMap", "fromArray", t, [a]) |> Some
        // match i.SignatureArgTypes, args with
        // | ([]|[Number _]), _ ->
        //     makeDictionary com ctx r t (makeArray Any []) |> Some
        // | [IDictionary], [arg] ->
        //     makeDictionary com ctx r t arg |> Some
        // | [IDictionary; IEqualityComparer], [arg; eqComp] ->
        //     makeComparerFromEqualityComparer eqComp
        //     |> makeDictionaryWithComparer com r t arg |> Some
        // | [IEqualityComparer], [eqComp]
        // | [Number _; IEqualityComparer], [_; eqComp] ->
        //     makeComparerFromEqualityComparer eqComp
        //     |> makeDictionaryWithComparer com r t (makeArray Any []) |> Some
        // | _ -> None
    | "get_IsReadOnly", _, _ -> makeBoolConst false |> Some
    | "get_Count", Some c, _ -> getLength com r t i c |> Some
    | "GetEnumerator", Some c, _ ->
        let ar = Helper.LibCall(com, "HashMap", "entries", t, [c], [c.Type])
        Helper.LibCall(com, "Seq", "Enumerable::ofArray", t, [ar], ?loc=r) |> Some
    | "ContainsValue", Some c, [arg] ->
        let vs = Helper.LibCall(com, "HashMap", "values", t, [c])
        Helper.LibCall(com, "Array", "contains", t, [arg; vs], ?loc=r) |> Some
    | "ContainsKey", Some c, _ ->
        let args = args |> List.map makeRef
        makeInstanceCall r t i c "contains_key" args |> Some
    | "TryGetValue", Some c, _ ->
        Helper.LibCall(com, "HashMap", "tryGetValue", t, c::args, c.Type::i.SignatureArgTypes, ?loc=r) |> Some
    | "TryAdd", Some c, _ ->
        Helper.LibCall(com, "HashMap", "tryAdd", t, c::args, c.Type::i.SignatureArgTypes, ?loc=r) |> Some
    | "Add", Some c, _ ->
        Helper.LibCall(com, "HashMap", "add", t, c::args, c.Type::i.SignatureArgTypes, ?loc=r) |> Some
    | "Remove", Some c, _ ->
        let args = args |> List.map makeRef
        let v = makeInstanceCall r t i (getMut c) "remove" args
        makeInstanceCall r t i v "is_some" [] |> Some
    | "Clear", Some c, _ ->
        makeInstanceCall r t i (getMut c) "clear" args |> Some
    | "get_Item", Some c, _ ->
        Helper.LibCall(com, "HashMap", "get", t, c::args, c.Type::i.SignatureArgTypes, ?loc=r) |> Some
    | "set_Item", Some c, _ ->
        Helper.LibCall(com, "HashMap", "set", t, c::args, c.Type::i.SignatureArgTypes, ?loc=r) |> Some
    | "get_Keys", Some c, _ ->
        Helper.LibCall(com, "HashMap", "keys", t, c::args, ?loc=r) |> Some
    | "get_Values", Some c, _ ->
        Helper.LibCall(com, "HashMap", "values", t, c::args, ?loc=r) |> Some
    | _ -> None

let hashSets (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, [] ->
        Helper.LibCall(com, "HashSet", "empty", t, args) |> Some
    | ".ctor", _, [ExprType(Number _)] ->
        Helper.LibCall(com, "HashSet", "withCapacity", t, args) |> Some
    | ".ctor", _, [ExprTypeAs(IEnumerable, arg)] ->
        Helper.LibCall(com, "HashSet", "fromArray", t, [toArray com t arg]) |> Some
        // match i.SignatureArgTypes, args with
        // | [], _ ->
        //     makeHashSet com ctx r t (makeArray Any []) |> Some
        // | [IEnumerable], [arg] ->
        //     makeHashSet com ctx r t arg |> Some
        // | [IEnumerable; IEqualityComparer], [arg; eqComp] ->
        //     makeComparerFromEqualityComparer eqComp
        //     |> makeHashSetWithComparer com r t arg |> Some
        // | [IEqualityComparer], [eqComp] ->
        //     makeComparerFromEqualityComparer eqComp
        //     |> makeHashSetWithComparer com r t (makeArray Any []) |> Some
        // | _ -> None
    | "get_Count", Some c, _ -> getLength com r t i c |> Some
    | "get_IsReadOnly", _, _ -> BoolConstant false |> makeValue r |> Some
    | "Contains", Some c, args ->
        let args = args |> List.map makeRef
        makeInstanceCall r t i c "contains" args |> Some
    | "GetEnumerator", Some c, _ ->
        let ar = Helper.LibCall(com, "HashSet", "entries", t, [c])
        Helper.LibCall(com, "Seq", "Enumerable::ofArray", t, [ar], ?loc=r) |> Some
    | "Add", Some c, [arg] ->
        makeInstanceCall r t i (getMut c) "insert" args |> Some
    | "Remove" as meth, Some c, [arg] ->
        let args = args |> List.map makeRef
        makeInstanceCall r t i (getMut c) "remove" args |> Some
    | "Clear", Some c, _ ->
        makeInstanceCall r t i (getMut c) "clear" args |> Some
    | ("IsProperSubsetOf" | "IsProperSupersetOf" | "UnionWith" | "IntersectWith" |
        "ExceptWith" | "IsSubsetOf" | "IsSupersetOf" as meth), Some c, args ->
        let meth = Naming.lowerFirst meth
        // let args = injectArg com ctx r "Set" meth i.GenericArgs args
        Helper.LibCall(com, "Set", meth, t, c::args, ?loc=r) |> Some
    // | "CopyTo" // TODO!!!
    // | "SetEquals"
    // | "Overlaps"
    // | "SymmetricExceptWith"
    | _ -> None

let exceptions (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> Helper.ConstructorCall(makeIdentExpr "Error", t, args, ?loc=r) |> Some
    | "get_Message", Some e -> getFieldWith r t e "message" |> Some
    | "get_StackTrace", Some e -> getFieldWith r t e "stack" |> Some
    | _ -> None

let objects (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> typedObjExpr t [] |> Some
    | "ToString", Some arg, _ -> toString com ctx r [arg] |> Some
    | "ReferenceEquals", None, [arg1; arg2]
    | "Equals", Some arg1, [arg2]
    | "Equals", None, [arg1; arg2] -> objectEquals com ctx r arg1 arg2 |> Some
    | "GetHashCode", Some arg, _ -> identityHash com r arg |> Some
    | "GetType", Some arg, _ ->
        if arg.Type = Any then
            "Types can only be resolved at compile time. At runtime this will be same as `typeof<obj>`"
            |> addWarning com ctx.InlinePath r
        makeTypeInfo r arg.Type |> Some
    | _ -> None

let valueTypes (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> typedObjExpr t [] |> Some
    | "ToString", Some arg, _ -> toString com ctx r [arg] |> Some
    | "Equals", Some arg1, [arg2]
    | "Equals", None, [arg1; arg2] -> structEquals com ctx r arg1 arg2 |> Some
    | "GetHashCode", Some arg, _ -> structuralHash com r arg |> Some
    | "CompareTo", Some arg1, [arg2] -> compare com ctx r arg1 arg2 |> Some
    | _ -> None

let unchecked (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "DefaultOf", _ -> (genArg com ctx r 0 i.GenericArgs) |> getZero com ctx |> Some
    | "Hash", [arg] -> structuralHash com r arg |> Some
    | "Equals", [arg1; arg2] -> equals com ctx r arg1 arg2 |> Some
    | "Compare", [arg1; arg2] -> compare com ctx r arg1 arg2 |> Some
    | _ -> None

let enums (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "HasFlag", Some this, [arg] ->
        // x.HasFlags(y) => (int x) &&& (int y) <> 0
        makeBinOp r (Number(Int32, NumberInfo.Empty)) this arg BinaryAndBitwise
        |> fun bitwise -> makeEqOp r bitwise (makeIntConst 0) BinaryUnequal
        |> Some
    | Patterns.DicContains(dict [
            "Parse", "parseEnum"
            "TryParse", "tryParseEnum"
            "IsDefined", "isEnumDefined"
            "GetName", "getEnumName"
            "GetNames", "getEnumNames"
            "GetValues", "getEnumValues"
            "GetUnderlyingType", "getEnumUnderlyingType"]) meth, None, args ->
        let args =
            match meth, args with
            // TODO: Parse at compile time if we know the type
            | "parseEnum", [value] -> [makeTypeInfo None t; value]
            | "tryParseEnum", [value; refValue] -> [genArg com ctx r 0 i.GenericArgs |> makeTypeInfo None; value; refValue]
            | _ -> args
        Helper.LibCall(com, "Reflection", meth, t, args, ?loc=r) |> Some
    | _ -> None

let bitConvert (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "GetBytes" ->
        let memberName =
            match args.Head.Type with
            | Boolean -> "getBytesBoolean"
            | Char | String -> "getBytesChar"
            | Number(Int16,_) -> "getBytesInt16"
            | Number(Int32,_) -> "getBytesInt32"
            | Number(UInt16,_) -> "getBytesUInt16"
            | Number(UInt32,_) -> "getBytesUInt32"
            | Number(Float32,_) -> "getBytesSingle"
            | Number(Float64,_) -> "getBytesDouble"
            | Number(Int64,_) -> "getBytesInt64"
            | Number(UInt64,_) -> "getBytesUInt64"
            | x -> FableError $"Unsupported type in BitConverter.GetBytes(): %A{x}" |> raise
        let expr = Helper.LibCall(com, "BitConverter", memberName, Boolean, args, i.SignatureArgTypes, ?loc=r)
        if com.Options.TypedArrays then expr |> Some
        else toArray com t expr |> Some // convert to dynamic array
    | _ ->
        let memberName = Naming.lowerFirst i.CompiledName
        Helper.LibCall(com, "BitConverter", memberName, Boolean, args, i.SignatureArgTypes, ?loc=r) |> Some

let convert (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "ToSByte" | "ToByte"
    | "ToInt16" | "ToUInt16"
    | "ToInt32" | "ToUInt32"
    | "ToInt64" | "ToUInt64" -> toRoundInt com ctx r t i args |> Some
    | "ToSingle" | "ToDouble" -> toFloat com ctx r t args |> Some
    | "ToDecimal" -> toDecimal com ctx r t args |> Some
    | "ToChar" -> toChar com args.Head |> Some
    | "ToString" -> toString com ctx r args |> Some
    | "ToBase64String" | "FromBase64String" ->
        if not(List.isSingle args) then
            $"Convert.%s{Naming.upperFirst i.CompiledName} only accepts one single argument"
            |> addWarning com ctx.InlinePath r
        Helper.LibCall(com, "String", (Naming.lowerFirst i.CompiledName), t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let console (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "get_Out" -> typedObjExpr t [] |> Some // empty object
    | "Write" -> "print!" |> emitFormat com r t args |> Some
    | "WriteLine" -> "println!" |> emitFormat com r t args |> Some
    | _ -> None

let debug (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Write" -> "print!" |> emitFormat com r t args |> Some
    | "WriteLine" -> "println!" |> emitFormat com r t args |> Some
    | "Break" -> makeDebugger r |> Some
    | "Assert" ->
        let unit = Value(Null Unit, None)
        match args with
        | [] | [Value(BoolConstant true,_)] -> Some unit
        | [Value(BoolConstant false,_)] -> makeDebugger r |> Some
        | arg::_ ->
            // emit i "if (!$0) { debugger; }" i.args |> Some
            let cond = Operation(Unary(UnaryNot, arg), Boolean, r)
            IfThenElse(cond, makeDebugger r, unit, r) |> Some
    | _ -> None

let dates (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let getTime (e: Expr) =
        makeInstanceCall r t i e "get_time" []
    let moduleName =
        if i.DeclaringEntityFullName = Types.datetime
        then "DateTime" else "DateTimeOffset"
    match i.CompiledName with
    | ".ctor" ->
        match args with
        | [] -> Helper.LibCall(com, moduleName, "min_value", t, [], [], ?loc=r) |> Some
        | ExprType(Number(Int64,_))::_ ->
            Helper.LibCall(com, moduleName, "from_ticks", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | ExprType(DeclaredType(e,[]))::_ when e.FullName = Types.datetime ->
            Helper.LibCall(com, "DateTimeOffset", "from_date", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::[] ->
            Helper.LibCall(com, "DateTime", "new_ymd", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::[] ->
            Helper.LibCall(com, "DateTime", "new_ymdhms", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))
            ::ExprType(Number(_, NumberInfo.IsEnum ent))::[]  when ent.FullName = "System.DateTimeKind"->
            Helper.LibCall(com, "DateTime", "new_ymdhms_withkind", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::ExprType(Number(Int32,_))::[] ->
            Helper.LibCall(com, "DateTime", "new_ymdhmsms", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ ->
            let last = List.last args
            match args.Length, last.Type with
            | 7, Number(_, NumberInfo.IsEnum ent) when ent.FullName = "System.DateTimeKind" ->
                let args = (List.take 6 args) @ [makeIntConst 0; last]
                let argTypes = (List.take 6 i.SignatureArgTypes) @ [Number(Int32, NumberInfo.Empty); last.Type]
                Helper.LibCall(com, "DateTime", "new", t, args, argTypes, ?loc=r) |> Some
            | _ ->
                Helper.LibCall(com, moduleName, "new", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "ToString" ->
        match thisArg with
        | Some thisArg -> makeInstanceCall r t i thisArg "to_string" args |> Some
        | None -> None
    | "get_Offset" ->
        Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst |> getFieldWith r t thisArg.Value |> Some
    // DateTimeOffset
    | "get_LocalDateTime" ->
        Helper.LibCall(com, "DateTimeOffset", "to_local_time", t, [thisArg.Value], [thisArg.Value.Type], ?loc=r) |> Some
    | "get_UtcDateTime" ->
        Helper.LibCall(com, "DateTimeOffset", "to_universal_time", t, [thisArg.Value], [thisArg.Value.Type], ?loc=r) |> Some
    | "get_DateTime" ->
        let kind = System.DateTimeKind.Unspecified |> int |> makeIntConst
        Helper.LibCall(com, "DateTime", "from_date_time_offset", t, [thisArg.Value; kind], [thisArg.Value.Type; kind.Type], ?loc=r) |> Some
    | "FromUnixTimeSeconds"
    | "FromUnixTimeMilliseconds" ->
        let value = Helper.LibCall(com, "Long", "to_number", Number(Float64, NumberInfo.Empty), args, i.SignatureArgTypes)
        let value =
            if i.CompiledName = "FromUnixTimeSeconds"
            then makeBinOp r t value (makeIntConst 1000) BinaryMultiply
            else value
        Helper.LibCall(com, "DateTimeOffset", "default", t, [value; makeIntConst 0], [value.Type; Number(Int32, NumberInfo.Empty)], ?loc=r) |> Some
    | "ToLocalTime" ->
        makeInstanceCall r t i thisArg.Value "to_local_time" args |> Some
    | "ToUnixTimeSeconds"
    | "ToUnixTimeMilliseconds" ->
        let ms = getTime thisArg.Value
        let args =
            if i.CompiledName = "ToUnixTimeSeconds"
            then [makeBinOp r t ms (makeIntConst 1000) BinaryDivide]
            else [ms]
        Helper.LibCall(com, "Long", "from_number", t, args, ?loc=r) |> Some
    | "get_Ticks" ->
        Helper.LibCall(com, "DateTime", "get_ticks", t, [thisArg.Value], [thisArg.Value.Type], ?loc=r) |> Some
    | "get_UtcTicks" ->
        Helper.LibCall(com, "DateTimeOffset", "get_utc_ticks", t, [thisArg.Value], [thisArg.Value.Type], ?loc=r) |> Some
    | "AddTicks" ->
        match thisArg, args with
        | Some c, [ticks] ->
            let ms = Helper.LibCall(com, "Long", "op_division", i.SignatureArgTypes.Head, [ticks; makeIntConst 10000], [ticks.Type; Number(Int32, NumberInfo.Empty)])
            let ms = Helper.LibCall(com, "Long", "to_number", Number(Float64, NumberInfo.Empty), [ms], [ms.Type])
            Helper.LibCall(com, moduleName, "add_milliseconds", Number(Float64, NumberInfo.Empty), [c; ms], [c.Type; ms.Type], ?loc=r) |> Some
        | _ -> None
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        match thisArg with
        | Some thisArg -> makeInstanceCall r t i thisArg meth args |> Some
        | None -> Helper.LibCall(com, moduleName, meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let timeSpans (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    // let callee = match i.callee with Some c -> c | None -> i.args.Head
    match i.CompiledName with
    | ".ctor" ->
        let meth = match args with [ticks] -> "from_ticks" | _ -> "create"
        Helper.LibCall(com, "TimeSpan", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "FromMilliseconds" -> TypeCast(args.Head, t) |> Some
    | "get_TotalMilliseconds" -> TypeCast(thisArg.Value, t) |> Some
    | "ToString" when (args.Length = 1) ->
        "TimeSpan.ToString with one argument is not supported, because it depends of local culture, please add CultureInfo.InvariantCulture"
        |> addError com ctx.InlinePath r
        None
    | "ToString" when (args.Length = 2) ->
        match args.Head with
        | StringConst "c"
        | StringConst "g"
        | StringConst "G" ->
            Helper.LibCall(com, "TimeSpan", "to_string", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
        | _ ->
            "TimeSpan.ToString don't support custom format. It only handles \"c\", \"g\" and \"G\" format, with CultureInfo.InvariantCulture."
            |> addError com ctx.InlinePath r
            None
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        Helper.LibCall(com, "TimeSpan", meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let timers (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> Helper.LibCall(com, "Timer", "default", t, args, i.SignatureArgTypes, isConstructor=true, ?loc=r) |> Some
    | Naming.StartsWith "get_" meth, Some x, _ -> getFieldWith r t x meth |> Some
    | Naming.StartsWith "set_" meth, Some x, [value] -> setExpr r x (makeStrConst meth) value |> Some
    | meth, Some x, args -> makeInstanceCall r t i x meth args |> Some
    | _ -> None

let systemEnv (com: ICompiler) (ctx: Context) (_: SourceLocation option) (_: Type) (i: CallInfo) (_: Expr option) (_: Expr list) =
    match i.CompiledName with
    | "get_NewLine" -> Some(makeStrConst "\n")
    | _ -> None

// Initial support, making at least InvariantCulture compile-able
// to be used System.Double.Parse and System.Single.Parse
// see https://github.com/fable-compiler/Fable/pull/1197#issuecomment-348034660
let globalization (com: ICompiler) (ctx: Context) (_: SourceLocation option) t (i: CallInfo) (_: Expr option) (_: Expr list) =
    match i.CompiledName with
    | "get_InvariantCulture" ->
        // System.Globalization namespace is not supported by Fable. The value InvariantCulture will be compiled to an empty object literal
        ObjectExpr([], t, None) |> Some
    | _ -> None

let random (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | ".ctor" -> ObjectExpr ([], t, None) |> Some
    | "Next" ->
        let min, max =
            match args with
            | [] -> makeIntConst 0, makeIntConst System.Int32.MaxValue
            | [max] -> makeIntConst 0, max
            | [min; max] -> min, max
            | _ -> FableError "Unexpected arg count for Random.Next" |> raise
        Helper.LibCall(com, "Util", "randomNext", t, [min; max], [min.Type; max.Type], ?loc=r) |> Some
    | "NextDouble" ->
        Helper.GlobalCall("Math", t, [], memb="random") |> Some
    | "NextBytes" ->
        let byteArray =
            match args with
            | [b] -> b
            | _ -> FableError "Unexpected arg count for Random.NextBytes" |> raise
        Helper.LibCall(com, "Util", "randomBytes", t, [byteArray], [byteArray.Type], ?loc=r) |> Some
    | _ -> None

let cancels (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "get_None" // TODO: implement as non-cancellable token
    | ".ctor" -> Helper.LibCall(com, "Async", "createCancellationToken", t, args, i.SignatureArgTypes) |> Some
    | "get_Token" -> thisArg
    | "Cancel" | "CancelAfter" | "get_IsCancellationRequested" | "ThrowIfCancellationRequested" ->
        let args, argTypes = match thisArg with Some c -> c::args, c.Type::i.SignatureArgTypes | None -> args, i.SignatureArgTypes
        Helper.LibCall(com, "Async", Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst, t, args, argTypes, ?loc=r) |> Some
    // TODO: Add check so CancellationTokenSource cannot be cancelled after disposed?
    | "Dispose" -> Null Type.Unit |> makeValue r |> Some
    | "Register" -> makeInstanceCall r t i thisArg.Value "register" args |> Some
    | _ -> None

let monitor (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Enter" -> Helper.LibCall(com, "Monitor", "enter", t, args, ?loc=r) |> Some
    | "Exit" -> Helper.LibCall(com, "Monitor", "exit", t, args, ?loc=r) |> Some
    | _ -> None

let tasks com (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, i.GenericArgs with
    | ".ctor", None, [tType] ->
        Helper.LibCall(com, "Task", "new", tType, args, ?loc=r) |> Some
    | "FromResult", None, [tType] ->
        Helper.LibCall(com, "Task", "from_result", tType, args, ?loc=r) |> Some
    | "get_Result", Some x, _ ->
        makeInstanceCall r t i x "get_result" args |> Some
    | _ -> None

let threads com (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, i.GenericArgs, args with
    | ".ctor", None, [], _ ->
        Helper.LibCall(com, "Thread", "new", t, args, ?loc=r) |> Some
    | "Sleep", None, _, [ExprType(Number(Int32,_))] ->
        Helper.LibCall(com, "Thread", "sleep", t, args, ?loc=r) |> Some
    | "Start", Some x, [], [] ->
        makeInstanceCall r t i x "start" args |> Some
    | "Join", Some x, [], [] ->
        makeInstanceCall r t i x "join" args |> Some
    | _ -> None

let activator (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "CreateInstance", None, ([_type] | [_type; (ExprType(Array(Any,_)))]) ->
        Helper.LibCall(com, "Reflection", "createInstance", t, args, ?loc=r) |> Some
    | _ -> None

let regex com (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let propInt p callee = getExpr r t callee (makeIntConst p)
    let propStr p callee = getExpr r t callee (makeStrConst p)
    let isGroup =
        match thisArg with
        | Some(ExprType(EntFullName "System.Text.RegularExpressions.Group")) -> true
        | _ -> false

    match i.CompiledName with
    | ".ctor" ->
        match args with
        | [StringConst pattern] -> makeRegexConst r pattern [] |> Some
        | StringConst pattern::(RegexFlags flags)::_ -> makeRegexConst r pattern flags |> Some
        | _ -> Helper.LibCall(com, "RegExp", "create", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "get_Options" -> Helper.LibCall(com, "RegExp", "options", t, [thisArg.Value], [thisArg.Value.Type], ?loc=r) |> Some
    // Capture
    | "get_Index" ->
        if not isGroup
        then propStr "index" thisArg.Value |> Some
        else "Accessing index of Regex groups is not supported"
             |> addErrorAndReturnNull com ctx.InlinePath r |> Some
    | "get_Value" ->
        if isGroup
        // In JS Regex group values can be undefined, ensure they're empty strings #838
        then Operation(Logical(LogicalOr, thisArg.Value, makeStrConst ""), t, r) |> Some
        else propInt 0 thisArg.Value |> Some
    | "get_Length" ->
        if isGroup
        then propStr "length" thisArg.Value |> Some
        else propInt 0 thisArg.Value |> propStr "length" |> Some
    // Group
    | "get_Success" -> nullCheck r false thisArg.Value |> Some
    // Match
    | "get_Groups" -> thisArg.Value |> Some
    // MatchCollection & GroupCollection
    | "get_Item" when i.DeclaringEntityFullName = "System.Text.RegularExpressions.GroupCollection" ->
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

            Operation(Logical(LogicalAnd, groups, getItem), t, None)
            |> Some
        | _ ->
            // index
            getExpr r t thisArg.Value args.Head |> Some
    | "get_Item" -> getExpr r t thisArg.Value args.Head |> Some
    | "get_Count" -> propStr "length" thisArg.Value |> Some
    | "GetEnumerator" -> getEnumerator com r t i thisArg.Value |> Some
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        Helper.LibCall(com, "RegExp", meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let encoding (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args.Length with
    | ("get_Unicode" | "get_UTF8"), _, _ ->
        Helper.LibCall(com, "Encoding", i.CompiledName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "GetBytes", Some callee, (1 | 3) ->
        let meth = Naming.lowerFirst i.CompiledName
        let expr = makeInstanceCall r t i callee meth args
        if com.Options.TypedArrays then expr |> Some
        else toArray com t expr |> Some // convert to dynamic array
    | "GetString", Some callee, (1 | 3) ->
        let meth = Naming.lowerFirst i.CompiledName
        makeInstanceCall r t i callee meth args |> Some
    | _ -> None

let enumerators (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | meth, Some callee ->
        // // Enumerators are mangled, use the fully qualified name
        // let isGenericCurrent = i.CompiledName = "get_Current" && i.DeclaringEntityFullName <> Types.ienumerator
        // let entityName = if isGenericCurrent then Types.ienumeratorGeneric else Types.ienumerator
        // let methName = entityName + "." + i.CompiledName
        makeInstanceCall r t i callee (Naming.removeGetSetPrefix meth) args |> Some
    | _ -> None

let enumerables (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (_: Expr list) =
    match i.CompiledName, thisArg with
    // This property only belongs to Key and Value Collections
    | "get_Count", Some callee -> Helper.LibCall(com, "Seq", "length", t, [callee], ?loc=r) |> Some
    | "GetEnumerator", Some callee -> getEnumerator com r t i callee |> Some
    | _ -> None

let events (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> Helper.LibCall(com, "Event", "default", t, args, i.SignatureArgTypes, isConstructor=true, ?loc=r) |> Some
    | "get_Publish", Some x -> getFieldWith r t x "Publish" |> Some
    | meth, Some x -> makeInstanceCall r t i x meth args |> Some
    | meth, None -> Helper.LibCall(com, "Event", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let observable (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    Helper.LibCall(com, "Observable", Naming.lowerFirst i.CompiledName, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let mailbox (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg with
    | None ->
        match i.CompiledName with
        | ".ctor" -> Helper.LibCall(com, "MailboxProcessor", "default", t, args, i.SignatureArgTypes, isConstructor=true, ?loc=r) |> Some
        | "Start" -> Helper.LibCall(com, "MailboxProcessor", "start", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ -> None
    | Some callee ->
        match i.CompiledName with
        // `reply` belongs to AsyncReplyChannel
        | "Start" | "Receive" | "PostAndAsyncReply" | "Post" ->
            let memb =
                if i.CompiledName = "Start"
                then "startInstance"
                else Naming.lowerFirst i.CompiledName
            Helper.LibCall(com, "MailboxProcessor", memb, t, args, i.SignatureArgTypes, thisArg=callee, ?loc=r) |> Some
        | "Reply" -> makeInstanceCall r t i callee "reply" args |> Some
        | _ -> None

let asyncBuilder (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "Singleton", _, _ ->
        Some (Value (UnitConstant, r))
        //makeImportLib com t "singleton" "AsyncBuilder" |> Some
    // For Using we need to cast the argument to IDisposable
    | "Using", Some x, [arg; f] ->
        makeInstanceCall r t i x "Using" [arg; f] |> Some
    | "Delay", _, _ -> Helper.LibCall(com, "AsyncBuilder", "delay", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Bind", _, _ -> Helper.LibCall(com, "AsyncBuilder", "bind", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Return", _, _ -> Helper.LibCall(com, "AsyncBuilder", "r_return", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Zero", _, _ -> Helper.LibCall(com, "AsyncBuilder", "zero", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, Some x, _ -> makeInstanceCall r t i x meth args |> Some
    | meth, None, _ -> Helper.LibCall(com, "AsyncBuilder", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let asyncs com (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    // TODO: Throw error for RunSynchronously
    | "Start" ->
        "Async.Start will behave as StartImmediate" |> addWarning com ctx.InlinePath r
        Helper.LibCall(com, "Async", "start", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // Make sure cancellationToken is called as a function and not a getter
    | "get_CancellationToken" -> Helper.LibCall(com, "Async", "cancellationToken", t, [], ?loc=r) |> Some
    // `catch` cannot be used as a function name in JS
    | "Catch" -> Helper.LibCall(com, "Async", "catchAsync", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // Fable.Core extensions
    | meth -> Helper.LibCall(com, "Async", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let taskBuilder (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    // | "Run", _, _ -> Helper.LibCall(com, "Task", "run", t, args, ?loc=r) |> Some
    // | "Singleton", _, _ -> makeImportLib com t "singleton" "TaskBuilder" |> Some
    | ".ctor", None, _ ->
        makeImportLib com t "new" "TaskBuilder" |> Some
    | "Run", Some x, _ ->
        makeInstanceCall r t i x "run" args |> Some
    | meth, Some x, _ -> makeInstanceCall r t i x meth args |> Some
    | meth, None, _ -> Helper.LibCall(com, "TaskBuilder", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let taskBuilderB (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "Bind", _, _ -> Helper.LibCall(com, "Task", "bind", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Return", _, _ -> Helper.LibCall(com, "Task", "r_return", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Delay", _, _ -> Helper.LibCall(com, "Task", "delay", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Zero", _, _ -> Helper.LibCall(com, "Task", "zero", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let taskBuilderHP (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "TaskBuilderBase.Bind", _, _ -> Helper.LibCall(com, "Task", "bind", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "TaskBuilderBase.Zero", _, _ -> Helper.LibCall(com, "Task", "zero", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, Some x, _ -> makeInstanceCall r t i x meth args |> Some
    | meth, None, _ -> Helper.LibCall(com, "TaskBuilder", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let taskBuilderM (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "task", _, _ -> Helper.LibCall(com, "TaskBuilder", "new", t, [], ?loc=r) |> Some
    | meth, Some x, _ -> makeInstanceCall r t i x meth args |> Some
    | meth, None, _ -> Helper.LibCall(com, "TaskBuilder", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let guids (com: ICompiler) (ctx: Context) (r: SourceLocation option) t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", None, _ ->
        match args with
        | [] -> Helper.LibCall(com, "Guid", "empty", t, [], ?loc=r) |> Some
        | [ExprType String] -> Helper.LibCall(com, "Guid", "parse", t, args, ?loc=r) |> Some
        // TODO: other constructor overrides
        | _ -> None
    // | "Empty", None, [] -> // it's a static field, see tryField
    | "NewGuid", None, [] -> Helper.LibCall(com, "Guid", "new_guid", t, args, ?loc=r) |> Some
    | "Parse", None, [ExprType String] -> Helper.LibCall(com, "Guid", "parse", t, args, ?loc=r) |> Some
    // | None, "TryParse", _ -> Helper.LibCall(com, "Guid", "try_parse", t, args, ?loc=r) |> Some
    // | Some x, "ToByteArray", [] -> Helper.LibCall(com, "Guid", "to_byte_array", t, args, ?loc=r) |> Some
    | "ToString", Some x, [] -> toString com ctx r [x] |> Some
    // TODO: other methods and overrides
    | _ -> None

let uris (com: ICompiler) (ctx: Context) (r: SourceLocation option) t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | ".ctor" -> Helper.LibCall(com, "Uri", "Uri.create", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "TryCreate" -> Helper.LibCall(com, "Uri", "Uri.tryCreate", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "UnescapeDataString" -> Helper.LibCall(com, "Util", "unescapeDataString", t, args, i.SignatureArgTypes) |> Some
    | "EscapeDataString"   -> Helper.LibCall(com, "Util", "escapeDataString", t, args, i.SignatureArgTypes) |> Some
    | "EscapeUriString"    -> Helper.LibCall(com, "Util", "escapeUriString", t, args, i.SignatureArgTypes) |> Some
    | "get_IsAbsoluteUri"
    | "get_Scheme"
    | "get_Host"
    | "get_AbsolutePath"
    | "get_AbsoluteUri"
    | "get_PathAndQuery"
    | "get_Query"
    | "get_Fragment"
    | "get_OriginalString" ->
        Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst |> getFieldWith r t thisArg.Value |> Some
    | _ -> None

let laziness (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | (".ctor"|"Create"),_,_ -> Helper.LibCall(com, "Util", "Lazy", t, args, i.SignatureArgTypes, isConstructor=true, ?loc=r) |> Some
    | "CreateFromValue",_,_ -> Helper.LibCall(com, "Util", "lazyFromValue", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Force", Some callee, _ -> getFieldWith r t callee "Value" |> Some
    | ("get_Value"|"get_IsValueCreated"), Some callee, _ ->
        Naming.removeGetSetPrefix i.CompiledName |> getFieldWith r t callee |> Some
    | _ -> None

let controlExtensions (com: ICompiler) (ctx: Context) (_: SourceLocation option) t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "AddToObservable" -> Some "add"
    | "SubscribeToObservable" -> Some "subscribe"
    | _ -> None
    |> Option.map (fun meth ->
        let args, argTypes =
            thisArg
            |> Option.map (fun thisArg -> thisArg::args, thisArg.Type::i.SignatureArgTypes)
            |> Option.defaultValue (args, i.SignatureArgTypes)
            |> fun (args, argTypes) -> List.rev args, List.rev argTypes
        Helper.LibCall(com, "Observable", meth, t, args, argTypes))

let types (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let returnString r x = StringConstant x |> makeValue r |> Some
    let resolved =
        // Some optimizations when the type is known at compile time
        match thisArg with
        | Some(Value(TypeInfo(exprType, _), exprRange) as thisArg) ->
            match exprType with
            | GenericParam(name=name) -> genericTypeInfoError name |> addError com ctx.InlinePath exprRange
            | _ -> ()
            match i.CompiledName with
            | "GetInterface" ->
                match exprType, args with
                | DeclaredType(e, genArgs), [StringConst name] -> Some(e, genArgs, name, false)
                | DeclaredType(e, genArgs), [StringConst name; BoolConst ignoreCase] -> Some(e, genArgs, name, ignoreCase)
                | _ -> None
                |> Option.map (fun (e, genArgs, name, ignoreCase) ->
                    let e = com.GetEntity(e)
                    let genMap = List.zip (e.GenericParameters |> List.map (fun p -> p.Name)) genArgs |> Map
                    let comp = if ignoreCase then System.StringComparison.OrdinalIgnoreCase else System.StringComparison.Ordinal
                    e.AllInterfaces |> Seq.tryPick (fun ifc ->
                        let ifcName = splitFullName ifc.Entity.FullName |> snd
                        if ifcName.Equals(name, comp) then
                            let genArgs = ifc.GenericArgs |> List.map (function
                                | GenericParam(name=name) as gen -> Map.tryFind name genMap |> Option.defaultValue gen
                                | gen -> gen)
                            Some(ifc.Entity, genArgs)
                        else None)
                    |> function
                        | Some(ifcEnt, genArgs) -> DeclaredType(ifcEnt, genArgs) |> makeTypeInfo r
                        | None -> Value(Null t, r))
            | "get_FullName" -> getTypeFullName false exprType |> returnString r
            | "get_Namespace" -> getTypeFullName false exprType |> splitFullName |> fst |> returnString r
            | "get_IsArray" ->
                match exprType with Array _ -> true | _ -> false
                |> BoolConstant |> makeValue r |> Some
            | "get_IsEnum" ->
                match exprType with
                | Number(_, NumberInfo.IsEnum _) -> true | _ -> false
                |> BoolConstant |> makeValue r |> Some
            | "GetElementType" ->
                match exprType with
                | Array(t,_) -> makeTypeInfo r t |> Some
                | _ -> Null t |> makeValue r |> Some
            | "get_IsGenericType" ->
                List.isEmpty exprType.Generics |> not |> BoolConstant |> makeValue r |> Some
            | "get_GenericTypeArguments" | "GetGenericArguments" ->
                let arVals = exprType.Generics |> List.map (makeTypeInfo r)
                NewArray(ArrayValues arVals, Any, MutableArray) |> makeValue r |> Some
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
                    | Tuple (_, isStruct) -> Tuple(newGen, isStruct)
                    | DeclaredType (ent, _) -> DeclaredType(ent, newGen)
                    | t -> t
                makeTypeInfo exprRange exprType |> Some
            | _ -> None
        |  _ -> None
    match resolved, thisArg with
    | Some _, _ -> resolved
    | None, Some thisArg ->
        match i.CompiledName with
        | "GetTypeInfo" -> Some thisArg
        | "get_GenericTypeArguments" | "GetGenericArguments" ->
            Helper.LibCall(com, "Reflection", "getGenerics", t, [thisArg], ?loc=r) |> Some
        | "MakeGenericType" ->
            Helper.LibCall(com, "Reflection", "makeGenericType", t, thisArg::args, ?loc=r) |> Some
        | "get_FullName" | "get_Namespace"
        | "get_IsArray" | "GetElementType"
        | "get_IsGenericType" | "GetGenericTypeDefinition"
        | "get_IsEnum" | "GetEnumUnderlyingType" | "GetEnumValues" | "GetEnumNames" | "IsSubclassOf" | "IsInstanceOfType" ->
            let meth = Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst
            Helper.LibCall(com, "Reflection", meth, t, thisArg::args, ?loc=r) |> Some
        | _ -> None
    | None, None -> None

let fsharpType com methName (r: SourceLocation option) t (i: CallInfo) (args: Expr list) =
    match methName with
    | "MakeTupleType" ->
        Helper.LibCall(com, "Reflection", "tuple_type", t, args, i.SignatureArgTypes, hasSpread=true, ?loc=r) |> Some
    // Prevent name clash with FSharpValue.GetRecordFields
    | "GetRecordFields" ->
        Helper.LibCall(com, "Reflection", "getRecordElements", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "GetUnionCases" | "GetTupleElements" | "GetFunctionElements"
    | "IsUnion" | "IsRecord" | "IsTuple" | "IsFunction" ->
        Helper.LibCall(com, "Reflection", Naming.lowerFirst methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "IsExceptionRepresentation" | "GetExceptionFields" -> None // TODO!!!
    | _ -> None

let fsharpValue com methName (r: SourceLocation option) t (i: CallInfo) (args: Expr list) =
    match methName with
    | "GetUnionFields" | "GetRecordFields" | "GetRecordField" | "GetTupleFields" | "GetTupleField"
    | "MakeUnion" | "MakeRecord" | "MakeTuple" ->
        Helper.LibCall(com, "Reflection", Naming.lowerFirst methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "GetExceptionFields" -> None // TODO!!!
    | _ -> None

let tryField com returnTyp ownerTyp fieldName =
    match ownerTyp, fieldName with
    | Number(Decimal,_), _ ->
        Helper.LibValue(com, "Decimal", fieldName, returnTyp) |> Some
    | String, "Empty" -> makeStrConst "" |> Some
    | Builtin BclGuid, "Empty" ->
        Helper.LibCall(com, "Guid", "empty", returnTyp, []) |> Some
    | Builtin BclTimeSpan, "Zero" -> makeIntConst 0 |> Some
    | Builtin BclDateTime, ("MaxValue" | "MinValue") ->
        Helper.LibCall(com, coreModFor BclDateTime, Naming.lowerFirst fieldName, returnTyp, []) |> Some
    | Builtin BclDateTimeOffset, ("MaxValue" | "MinValue") ->
        Helper.LibCall(com, coreModFor BclDateTimeOffset, Naming.lowerFirst fieldName, returnTyp, []) |> Some
    | DeclaredType(ent, genArgs), fieldName ->
        match ent.FullName with
        | "System.BitConverter" ->
            Helper.LibCall(com, "BitConverter", Naming.lowerFirst fieldName, returnTyp, []) |> Some
        | _ -> None
    | _ -> None

let private replacedModules =
  dict [
    "System.Math", operators
    "Microsoft.FSharp.Core.Operators", operators
    "Microsoft.FSharp.Core.Operators.Checked", operators
    "Microsoft.FSharp.Core.Operators.Unchecked", unchecked
    "Microsoft.FSharp.Core.Operators.OperatorIntrinsics", intrinsicFunctions
    "Microsoft.FSharp.Core.ExtraTopLevelOperators", operators
    "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions", intrinsicFunctions
    "Microsoft.FSharp.Core.LanguagePrimitives", languagePrimitives
    "Microsoft.FSharp.Core.LanguagePrimitives.HashCompare", languagePrimitives
    "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators", operators
    "System.Runtime.CompilerServices.RuntimeHelpers", runtimeHelpers
    "System.Runtime.ExceptionServices.ExceptionDispatchInfo", exceptionDispatchInfo
    Types.char, chars
    Types.string, strings
    "Microsoft.FSharp.Core.StringModule", stringModule
    "System.FormattableString", formattableString
    "System.Runtime.CompilerServices.FormattableStringFactory", formattableString
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
    Types.ienumeratorGeneric, enumerators
    Types.ienumerator, enumerators
    Types.valueCollection, resizeArrays
    Types.keyCollection, resizeArrays
    "System.Collections.Generic.Dictionary`2.Enumerator", enumerators
    "System.Collections.Generic.Dictionary`2.ValueCollection.Enumerator", enumerators
    "System.Collections.Generic.Dictionary`2.KeyCollection.Enumerator", enumerators
    "System.Collections.Generic.List`1.Enumerator", enumerators
    "System.Collections.Generic.HashSet`1.Enumerator", enumerators
    "System.CharEnumerator", enumerators
    Types.resizeArray, resizeArrays
    "System.Collections.Generic.IList`1", resizeArrays
    "System.Collections.IList", resizeArrays
    Types.icollectionGeneric, resizeArrays
    Types.icollection, resizeArrays
    Types.hashset, hashSets
    Types.stack, bclType
    Types.queue, bclType
    Types.iset, hashSets
    Types.option, options
    Types.valueOption, options
    "System.Nullable`1", nullables
    "Microsoft.FSharp.Core.OptionModule", optionModule
    "Microsoft.FSharp.Core.ValueOptionModule", optionModule
    "Microsoft.FSharp.Core.ResultModule", results
    Types.bigint, bigints
    "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI", bigints
    Types.refCell, refCells
    Types.object, objects
    Types.valueType, valueTypes
    "System.Enum", enums
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
    Types.float32, parseNum
    Types.float64, parseNum
    Types.decimal, decimals
    "System.Convert", convert
    "System.Console", console
    "System.Diagnostics.Debug", debug
    "System.Diagnostics.Debugger", debug
    Types.datetime, dates
    Types.datetimeOffset, dates
    Types.timespan, timeSpans
    "System.Timers.Timer", timers
    "System.Environment", systemEnv
    "System.Globalization.CultureInfo", globalization
    "System.Random", random
    "System.Threading.CancellationToken", cancels
    "System.Threading.CancellationTokenSource", cancels
    "System.Threading.Monitor", monitor
    Types.task, tasks
    Types.taskGeneric, tasks
    Types.thread, threads
    "System.Threading.Tasks.TaskCompletionSource`1", tasks
    "System.Runtime.CompilerServices.TaskAwaiter`1", tasks
    "System.Activator", activator
    "System.Text.Encoding", encoding
    "System.Text.UnicodeEncoding", encoding
    "System.Text.UTF8Encoding", encoding
    "System.Text.RegularExpressions.Capture", regex
    "System.Text.RegularExpressions.Match", regex
    "System.Text.RegularExpressions.Group", regex
    "System.Text.RegularExpressions.MatchCollection", regex
    "System.Text.RegularExpressions.GroupCollection", regex
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
    "Microsoft.FSharp.Control.TaskBuilderModule", taskBuilderM
    "Microsoft.FSharp.Control.TaskBuilder", taskBuilder
    "Microsoft.FSharp.Control.TaskBuilderBase", taskBuilderB
    "Microsoft.FSharp.Control.TaskBuilderExtensions.HighPriority", taskBuilderHP
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

let tryCall (com: ICompiler) (ctx: Context) r t (info: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match info.DeclaringEntityFullName with
    | Patterns.DicContains replacedModules replacement -> replacement com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings" -> errorStrings info.CompiledName
    | Types.printfModule
    | Naming.StartsWith Types.printfFormat _ -> fsFormat com ctx r t info thisArg args
    | Naming.StartsWith "Fable.Core." _ -> fableCoreLib com ctx r t info thisArg args
    | Naming.EndsWith "Exception" _ -> exceptions com ctx r t info thisArg args
    | "System.Timers.ElapsedEventArgs" -> thisArg // only signalTime is available here
    | Naming.StartsWith "System.Tuple" _
    | Naming.StartsWith "System.ValueTuple" _ -> tuples com ctx r t info thisArg args
    | Naming.StartsWith "System.Action" _
    | Naming.StartsWith "System.Func" _
    | Naming.StartsWith "Microsoft.FSharp.Core.FSharpFunc" _
    | Naming.StartsWith "Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc" _ -> funcs com ctx r t info thisArg args
    | "Microsoft.FSharp.Reflection.FSharpType" -> fsharpType com info.CompiledName r t info args
    | "Microsoft.FSharp.Reflection.FSharpValue" -> fsharpValue com info.CompiledName r t info args
    | "Microsoft.FSharp.Reflection.FSharpReflectionExtensions" ->
        // In netcore F# Reflection methods become extensions
        // with names like `FSharpType.GetExceptionFields.Static`
        let isFSharpType = info.CompiledName.StartsWith("FSharpType")
        let methName = info.CompiledName |> Naming.extensionMethodName
        if isFSharpType
        then fsharpType com methName r t info args
        else fsharpValue com methName r t info args
    | "Microsoft.FSharp.Reflection.UnionCaseInfo"
    | "System.Reflection.PropertyInfo"
    | "System.Reflection.ParameterInfo"
    | "System.Reflection.MethodBase"
    | "System.Reflection.MethodInfo"
    | "System.Reflection.MemberInfo" ->
        match thisArg, info.CompiledName with
        | Some c, "get_Tag" -> makeStrConst "tag" |> getExpr r t c |> Some
        | Some c, "get_ReturnType" -> makeStrConst "returnType" |> getExpr r t c |> Some
        | Some c, "GetParameters" -> makeStrConst "parameters" |> getExpr r t c |> Some
        | Some c, ("get_PropertyType"|"get_ParameterType") -> makeIntConst 1 |> getExpr r t c |> Some
        | Some c, "GetFields" -> Helper.LibCall(com, "Reflection", "getUnionCaseFields", t, [c], ?loc=r) |> Some
        | Some c, "GetValue" -> Helper.LibCall(com, "Reflection", "getValue", t, c::args, ?loc=r) |> Some
        | Some c, "get_Name" ->
            match c with
            | Value(TypeInfo(exprType, _), loc) ->
                getTypeName com ctx loc exprType
                |> StringConstant |> makeValue r |> Some
            | c ->
                Helper.LibCall(com, "Reflection", "name", t, [c], ?loc=r) |> Some
        | _ -> None
    | _ -> None

let tryType typ =
    match typ with
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
        if isStruct
        then Some(Types.valueOption, options, [genArg])
        else Some(Types.option, options, [genArg])
    | Array(genArg,_) -> Some(Types.array, arrays, [genArg])
    | List genArg -> Some(Types.list, lists, [genArg])
    | Builtin kind ->
        match kind with
        | BclGuid -> Some(Types.guid, guids, [])
        | BclTimeSpan -> Some(Types.timespan, timeSpans, [])
        | BclDateTime -> Some(Types.datetime, dates, [])
        | BclDateTimeOffset -> Some(Types.datetimeOffset, dates, [])
        | BclTimer -> Some("System.Timers.Timer", timers, [])
        | BclHashSet genArg -> Some(Types.hashset, hashSets, [genArg])
        | BclDictionary(key, value) -> Some(Types.dictionary, dictionaries, [key; value])
        | BclKeyValuePair(key, value) -> Some(Types.keyValuePair, keyValuePairs, [key; value])
        | FSharpMap(key, value) -> Some(Types.fsharpMap, maps, [key; value])
        | FSharpSet genArg -> Some(Types.fsharpSet, sets, [genArg])
        | FSharpResult(genArg1, genArg2) -> Some(Types.result, results, [genArg1; genArg2])
        | FSharpChoice genArgs -> Some($"{Types.choiceNonGeneric}`{List.length genArgs}", results, genArgs)
        | FSharpReference genArg -> Some(Types.refCell, refCells, [genArg])
        | BclDateOnly -> None
        | BclTimeOnly -> None
    | _ -> None
