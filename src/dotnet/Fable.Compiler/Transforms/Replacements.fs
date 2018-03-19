module Fable.Transforms.Replacements

open Fable
open Fable.AST
open Fable.AST.Fable
open Microsoft.FSharp.Compiler.SourceCodeServices
open Patterns

type Context = Fable.Transforms.FSharp2Fable.Context
type ICompiler = Fable.Transforms.FSharp2Fable.IFableCompiler

module private Helpers =
    let resolveArgTypes argTypes genArgs =
        argTypes |> List.map (function
            | GenericParam name as t ->
                genArgs |> List.tryPick (fun (name2,t) ->
                    if name = name2 then Some t else None)
                |> Option.defaultValue t
            | t -> t)

    let coreCall r t coreModule coreMember thisArg args argTypes =
        let info = argInfo thisArg args (Some argTypes)
        let funcExpr = Import(coreMember, coreModule, CoreLib, Any)
        Operation(Call(StaticCall funcExpr, info), t, r)

    let coreCall_ t coreModule coreMember args =
        let funcExpr = Import(coreMember, coreModule, CoreLib, Any)
        Operation(Call(StaticCall funcExpr, argInfo None args None), t, None)

    let globalCall r t ident memb thisArg args argTypes =
        let funcExpr =
            match memb with
            | Some m -> get None Any (makeIdentExpr ident) m
            | None -> makeIdentExpr ident
        let info = argInfo thisArg args (Some argTypes)
        Operation(Call(StaticCall funcExpr, info), t, r)

    let globalCall_ t ident memb args =
        let funcExpr =
            match memb with
            | Some m -> get None Any (makeIdentExpr ident) m
            | None -> makeIdentExpr ident
        Operation(Call(StaticCall funcExpr, argInfo None args None), t, None)

    let globalIdent ident memb =
        get None Any (makeIdentExpr ident) memb

    let constructorCall_ t consExpr args =
        let info = argInfo None args None
        Fable.Operation(Fable.Call(Fable.ConstructorCall consExpr, info), t, None)

    let instanceCall_ t callee memb args =
        let kind = makeStrConst memb |> Some |> InstanceCall
        Operation(Call(kind, argInfo (Some callee) args None), t, None)

    let icall r t (i: CallInfo) thisArg args meth =
        let info = argInfo thisArg args (Some i.ArgTypes)
        instanceCall r t info (makeStrConst meth |> Some)

    let emitJs r t args macro =
        let info = argInfo None args None
        Operation(Emit(macro, Some info), t, r)

    let objExpr t kvs =
        let kvs = List.map (fun (k,v) -> k, v, ObjectValue) kvs
        ObjectExpr(kvs, t, None)

    let add left right =
        Operation(BinaryOperation(BinaryPlus, left, right), left.Type, None)

    let eq left right =
        Operation(BinaryOperation(BinaryEqualStrict, left, right), Boolean, None)

    let neq left right =
        Operation(BinaryOperation(BinaryUnequalStrict, left, right), Boolean, None)

    let isNull expr =
        Operation(BinaryOperation(BinaryEqual, expr, Value(Null Any)), Boolean, None)

    let error msg =
        constructorCall_ Any (makeIdentExpr "Error") [msg]

    let s txt = Value(StringConstant txt)

    let genArg (com: ICompiler) r (name: string) (genArgs: Map<string,Type>) =
        match Map.tryFind name genArgs with
        | Some t -> t
        | None ->
            "Couldn't find generic " + name |> addError com r
            Any

    let firstGenArg (com: ICompiler) r (genArgs: (string * Type) list) =
        List.tryHead genArgs
        |> Option.map snd
        |> Option.defaultWith (fun () ->
            "Couldn't find any generic argument" |> addError com r
            Any)

    let defaultof (t: Type) =
        match t with
        | Number _ -> makeIntConst 0
        | Boolean -> makeBoolConst false
        | _ -> Null t |> Value

open Helpers

type BuiltinType =
    | BclGuid
    | BclTimeSpan
    | BclDateTime
    | BclDateTimeOffset
    | BclInt64
    | BclUInt64
    | BclBigInt
    | FSharpSet
    | FSharpMap

let (|Builtin|_|) = function
    | DeclaredType(ent,_) ->
        match ent.TryFullName with
        | Some Types.guid -> Some BclGuid
        | Some Types.timespan -> Some BclTimeSpan
        | Some Types.datetime -> Some BclDateTime
        | Some Types.datetimeOffset -> Some BclDateTimeOffset
        | Some "System.Int64" -> Some BclInt64
        | Some "System.UInt64" -> Some BclUInt64
        | Some (Naming.StartsWith "Microsoft.FSharp.Core.int64" _) -> Some BclInt64
        | Some "System.Numerics.BigInteger" -> Some BclBigInt
        | Some "Microsoft.FSharp.Collections.FSharpSet" -> Some FSharpSet
        | Some "Microsoft.FSharp.Collections.FSharpMap" -> Some FSharpMap
        | _ -> None
    | _ -> None

let (|Integer|Float|) = function
    | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 -> Integer
    | Float32 | Float64 | Decimal -> Float

let (|Nameof|_|) = function
    | IdentExpr ident -> Some ident.Name
    | Get(_, ExprGet(Value(StringConstant prop)), _, _) -> Some prop
    | _ -> None

let inline (|ExprType|) (e: Expr) = e.Type

let coreModFor = function
    | BclGuid -> "String"
    | BclDateTime -> "Date"
    | BclDateTimeOffset -> "DateOffset"
    | BclInt64 -> "Long"
    | BclUInt64 -> "Long"
    | BclBigInt -> "BigInt"
    | BclTimeSpan -> "Int32"
    | FSharpSet -> "Set"
    | FSharpMap -> "Map"

let makeLongInt t signed (x: uint64) =
    let lowBits = NumberConstant (float (uint32 x), Float64)
    let highBits = NumberConstant (float (x >>> 32), Float64)
    let unsigned = BoolConstant (not signed)
    let args = [Value lowBits; Value highBits; Value unsigned]
    coreCall_ t "Long" "fromBits" args

let makeFloat32 (x: float32) =
    [NumberConstant (float x, Float32) |> Value]
    |> globalCall_ (Number Float32) "Math" (Some "fround")

let makeTypeConst (typ: Type) (value: obj) =
    match typ, value with
    // Long Integer types
    | Builtin BclInt64, (:? int64 as x) -> makeLongInt typ true (uint64 x)
    | Builtin BclUInt64, (:? uint64 as x) -> makeLongInt typ false x
    // Decimal type
    | Number Decimal, (:? decimal as x) -> makeDecConst x
    // Short Float type
    | Number Float32, (:? float32 as x) -> makeFloat32 x
    | Boolean, (:? bool as x) -> BoolConstant x |> Value
    | String, (:? string as x) -> StringConstant x |> Value
    | Char, (:? char as x) -> StringConstant (string x) |> Value
    // Integer types
    | Number UInt8, (:? byte as x) -> NumberConstant (float x, UInt8) |> Value
    | Number Int8, (:? sbyte as x) -> NumberConstant (float x, Int8) |> Value
    | Number Int16, (:? int16 as x) -> NumberConstant (float x, Int16) |> Value
    | Number UInt16, (:? uint16 as x) -> NumberConstant (float x, UInt16) |> Value
    | Number Int32, (:? int as x) -> NumberConstant (float x, Int32) |> Value
    | Number UInt32, (:? uint32 as x) -> NumberConstant (float x, UInt32) |> Value
    // Float types
    | Number Float64, (:? float as x) -> NumberConstant (float x, Float64) |> Value
    // Enums
    | EnumType _, (:? int64)
    | EnumType _, (:? uint64) -> failwith "int64 enums are not supported"
    | EnumType(_, name), (:? byte as x) -> Enum(NumberEnum(int x), name) |> Value
    | EnumType(_, name), (:? sbyte as x) -> Enum(NumberEnum(int x), name) |> Value
    | EnumType(_, name), (:? int16 as x) -> Enum(NumberEnum(int x), name) |> Value
    | EnumType(_, name), (:? uint16 as x) -> Enum(NumberEnum(int x), name) |> Value
    | EnumType(_, name), (:? int as x) -> Enum(NumberEnum(int x), name) |> Value
    | EnumType(_, name), (:? uint32 as x) -> Enum(NumberEnum(int x), name) |> Value
    // TODO: Regex
    | Unit, _ -> UnitConstant |> Value
    // Arrays with small data type (ushort, byte) are represented
    // in F# AST as BasicPatterns.Const
    | Array (Number kind), (:? (byte[]) as arr) ->
        let values = arr |> Array.map (fun x -> NumberConstant (float x, kind) |> Value) |> Seq.toList
        NewArray (ArrayValues values, Number kind) |> Value
    | Array (Number kind), (:? (uint16[]) as arr) ->
        let values = arr |> Array.map (fun x -> NumberConstant (float x, kind) |> Value) |> Seq.toList
        NewArray (ArrayValues values, Number kind) |> Value
    | _ -> failwithf "Unexpected type %A, literal %O" typ value

let rec makeTypeTest com range expr (typ: Type) =
    let jsTypeof (primitiveType: string) expr =
        let typof = makeUnOp None String expr UnaryTypeof
        makeEqOp range typof (makeStrConst primitiveType) BinaryEqualStrict
    let jsInstanceof (cons: Expr) expr =
        makeBinOp range Boolean expr cons BinaryInstanceOf
    match typ with
    | Any -> makeBoolConst true
    | Unit -> makeEqOp range expr (Value <| Null Any) BinaryEqual
    | Boolean -> jsTypeof "boolean" expr
    | Char | String _ -> jsTypeof "string" expr
    | FunctionType _ -> jsTypeof "function" expr
    | Number _ | EnumType _ -> jsTypeof "number" expr
    | Regex ->
        jsInstanceof (makeIdentExpr "RegExp") expr
    | Builtin (BclInt64 | BclUInt64) ->
        jsInstanceof (makeCoreRef Any "Long" "default") expr
    | Builtin BclBigInt ->
        jsInstanceof (makeCoreRef Any "BigInt" "default") expr
    | Array _ | Tuple _ | List _ ->
        coreCall_ Boolean "Util" "isArray" [expr]
    | DeclaredType (ent, _) ->
        match ent.TryFullName with
        | Some "System.IDisposable" ->
            coreCall_ Boolean "Util" "isDisposable" [expr]
        | _ ->
            if ent.IsClass
            then jsInstanceof (FSharp2Fable.Util.entityRef com ent) expr
            else "Cannot type test interfaces, records or unions"
                 |> addErrorAndReturnNull com range
    | Option _ | GenericParam _ | ErasedUnion _ ->
        "Cannot type test options, generic parameters or erased unions"
        |> addErrorAndReturnNull com range

let toChar (sourceType: Type) (args: Expr list) =
    match sourceType with
    | Char
    | String -> args.Head
    | _ -> globalCall_ Char "String" (Some "fromCharCode") args

let toString (sourceType: Type) (args: Expr list) =
    match sourceType with
    | Char | String -> args.Head
    | Unit | Boolean | Array _ | Tuple _ | FunctionType _ | EnumType _ ->
        globalCall_ String "String" None args
    | Builtin (BclInt64 | BclUInt64) -> coreCall_ String "Long" "toString" args
    | Number Int16 -> coreCall_ String "Util" "int16ToString" args
    | Number Int32 -> coreCall_ String "Util" "int32ToString" args
    | Number _ -> instanceCall_ String args.Head "toString" args.Tail
    | _ -> coreCall_ String "Util" "toString" args

let toFloat (sourceType: Type) targetType (args: Expr list) =
    match sourceType with
    | String -> coreCall_ (Number Float64) "Double" "parse" args
    | Builtin (BclInt64 | BclUInt64) ->
        coreCall_ (Number Float64) "Long" "toNumber" args
    | Builtin BclBigInt ->
        let meth = match targetType with
                    | Number Float32 -> "toSingle"
                    | Number Float64 -> "toDouble"
                    | Number Decimal -> "toDecimal"
                    | _ -> failwith "Unexpected BigInt conversion"
        coreCall_ (Number Float64) "BigInt" meth args
    | _ -> args.Head

// Apparently ~~ is faster than Math.floor (see https://coderwall.com/p/9b6ksa/is-faster-than-math-floor)
let fastIntFloor expr =
    let inner = makeUnOp None Any expr UnaryNotBitwise
    makeUnOp None (Number Int32) inner UnaryNotBitwise

let toInt (round: bool) (sourceType: Type) targetType (args: Expr list) =
    let kindIndex t =             //         0   1   2   3   4   5   6   7   8   9  10  11
        match t with              //         i8 i16 i32 i64  u8 u16 u32 u64 f32 f64 dec big
        | Number Int8 -> 0        //  0 i8   -   -   -   -   +   +   +   +   -   -   -   +
        | Number Int16 -> 1       //  1 i16  +   -   -   -   +   +   +   +   -   -   -   +
        | Number Int32 -> 2       //  2 i32  +   +   -   -   +   +   +   +   -   -   -   +
        | Builtin BclInt64 -> 3   //  3 i64  +   +   +   -   +   +   +   +   -   -   -   +
        | Number UInt8 -> 4       //  4 u8   +   +   +   +   -   -   -   -   -   -   -   +
        | Number UInt16 -> 5      //  5 u16  +   +   +   +   +   -   -   -   -   -   -   +
        | Number UInt32 -> 6      //  6 u32  +   +   +   +   +   +   -   -   -   -   -   +
        | Builtin BclUInt64 -> 7  //  7 u64  +   +   +   +   +   +   +   -   -   -   -   +
        | Number Float32 -> 8     //  8 f32  +   +   +   +   +   +   +   +   -   -   -   +
        | Number Float64 -> 9     //  9 f64  +   +   +   +   +   +   +   +   -   -   -   +
        | Number Decimal -> 10    // 10 dec  +   +   +   +   +   +   +   +   -   -   -   +
        | Builtin BclBigInt -> 11 // 11 big  +   +   +   +   +   +   +   +   +   +   +   -
        | _ -> failwith "Unexpected non-number type"
    let needToCast typeFrom typeTo =
        let v = kindIndex typeFrom // argument type (vertical)
        let h = kindIndex typeTo   // return type (horizontal)
        ((v > h) || (v < 4 && h > 3)) && (h < 8) || (h <> v && (h = 11 || v = 11))
    let emitLong unsigned (args: Expr list) =
        match sourceType with
        | Builtin (BclInt64|BclUInt64) -> coreCall_ targetType "Long" "fromValue" args
        | _ -> coreCall_ targetType "Long" "fromNumber" (args@[makeBoolConst unsigned])
    let emitBigInt (args: Expr list) =
        match sourceType with
        | Builtin (BclInt64|BclUInt64) -> coreCall_ targetType "BigInt" "fromInt64" args
        | _ -> coreCall_ targetType "BigInt" "fromInt32" args
    let emitCast typeTo args =
        match typeTo with
        | Builtin BclBigInt -> emitBigInt args
        | Builtin BclUInt64 -> emitLong true args
        | Builtin BclInt64 -> emitLong false args
        | Number Int8 -> emitJs None targetType args "($0 + 0x80 & 0xFF) - 0x80"
        | Number Int16 -> emitJs None targetType args "($0 + 0x8000 & 0xFFFF) - 0x8000"
        | Number Int32 -> fastIntFloor args.Head
        | Number UInt8 -> emitJs None targetType args "$0 & 0xFF"
        | Number UInt16 -> emitJs None targetType args "$0 & 0xFFFF"
        | Number UInt32 -> emitJs None targetType args "$0 >>> 0"
        // TODO: Use Cast(args.Head, targetType) for these cases (more below)?
        | Number Float32 -> args.Head
        | Number Float64 -> args.Head
        | Number Decimal -> args.Head
        | _ -> failwith "Unexpected non-number type"
    let castBigIntMethod typeTo =
        match typeTo with
        | Builtin BclBigInt -> failwith "Unexpected conversion"
        | Number Int8 -> "toSByte"
        | Number Int16 -> "toInt16"
        | Number Int32 -> "toInt32"
        | Builtin BclInt64 -> "toInt64"
        | Number UInt8 -> "toByte"
        | Number UInt16 -> "toUInt16"
        | Number UInt32 -> "toUInt32"
        | Builtin BclUInt64 -> "toUInt64"
        | Number Float32 -> "toSingle"
        | Number Float64 -> "toDouble"
        | Number Decimal -> "toDecimal"
        | _ -> failwith "Unexpected non-number type"
    let sourceType =
        match sourceType with
        | EnumType(NumberEnumType, _) -> Number Int32
        | t -> t
    match sourceType with
    | Char -> instanceCall_ targetType args.Head "charCodeAt" [makeIntConst 0]
    | String ->
        match targetType with
        | Builtin (BclInt64|BclUInt64 as kind) ->
            let unsigned = kind = BclUInt64
            let args = [args.Head]@[makeBoolConst unsigned]@args.Tail
            coreCall_ targetType "Long" "fromString" args
        | _ -> coreCall_ targetType "Int32" "parse" args
    | Builtin BclBigInt ->
        let meth = castBigIntMethod targetType
        coreCall_ targetType "BigInt" meth args
    | Number _ | Builtin (BclInt64 | BclUInt64) as typeFrom ->
        match targetType with
        | typeTo when needToCast typeFrom typeTo ->
            match typeFrom, typeTo with
            | Builtin (BclUInt64|BclInt64), Number _ ->
                coreCall_ targetType "Long" "toNumber" args
            | Number (Decimal|Float), (Number Integer | Builtin(BclInt64|BclUInt64)) when round ->
                coreCall_ targetType "Util" "round" args
            | _, _ -> args.Head
            |> List.singleton
            |> emitCast typeTo
        | Builtin (BclUInt64|BclInt64 as kind) ->
            emitLong (kind = BclUInt64) [args.Head]
        | Number _ -> args.Head
        | _ -> args.Head
    | _ -> args.Head

// TODO: Should we pass the empty list representation here?
let toList returnType expr =
    coreCall_ returnType "Seq" "toList" [expr]

let toArray (com: ICompiler) returnType expr =
    match expr, returnType with
    // Typed arrays
    | _, Array(Number numberKind) when com.Options.typedArrays ->
        globalCall_ returnType (getTypedArrayName com numberKind) (Some "from") [expr]
    | _ -> globalCall_ returnType "Array" (Some "from") [expr]

let toSeq returnType (expr: Expr) =
    match expr.Type with
    | List _ -> coreCall_ returnType "List" "toSeq" [expr]
    | _ -> expr // TODO

let getZero = function
    | Char | String -> makeStrConst ""
    | Builtin BclTimeSpan -> makeIntConst 0
    | Builtin BclDateTime as t -> coreCall_ t "Date" "minValue" []
    | Builtin BclDateTimeOffset as t -> coreCall_ t "DateOffset" "minValue" []
    | Builtin FSharpSet as t -> coreCall_ t "Set" "create" []
    | Builtin (BclInt64|BclUInt64) as t -> coreCall_ t "Long" "fromInt" [makeIntConst 0]
    | Builtin BclBigInt as t -> coreCall_ t "BigInt" "fromInt32" [makeIntConst 0]
    // TODO: Calls to custom Zero implementation
    | _ -> makeIntConst 0

let applyOp (com: ICompiler) ctx r t opName (args: Expr list) argTypes genArgs =
    let (|CustomOp|_|) com opName argTypes =
        let tryFindMember com (ent: FSharpEntity) opName argTypes =
            FSharp2Fable.TypeHelpers.tryFindMember com ent opName false argTypes
        argTypes |> List.tryPick (function
            | DeclaredType(ent,_) -> tryFindMember com ent opName argTypes
            | _ -> None)
    let unOp operator operand =
        Operation(UnaryOperation(operator, operand), t, r)
    let binOp op left right =
        Operation(BinaryOperation(op, left, right), t, r)
    let logicOp op left right =
        Operation(LogicalOperation(op, left, right), Boolean, r)
    let nativeOp opName argTypes args =
        match opName, args with
        | Operators.addition, [left; right] -> binOp BinaryPlus left right
        | Operators.subtraction, [left; right] -> binOp BinaryMinus left right
        | Operators.multiply, [left; right] -> binOp BinaryMultiply left right
        | Operators.division, [left; right] ->
            match argTypes with
            // Floor result of integer divisions (see #172)
            | Number Integer::_ -> binOp BinaryDivide left right |> fastIntFloor
            | _ -> binOp BinaryDivide left right
        | Operators.modulus, [left; right] -> binOp BinaryModulus left right
        | Operators.leftShift, [left; right] -> binOp BinaryShiftLeft left right
        | Operators.rightShift, [left; right] ->
            match argTypes with
            | Number UInt32::_ -> binOp BinaryShiftRightZeroFill left right // See #646
            | _ -> binOp BinaryShiftRightSignPropagating left right
        | Operators.bitwiseAnd, [left; right] -> binOp BinaryAndBitwise left right
        | Operators.bitwiseOr, [left; right] -> binOp BinaryOrBitwise left right
        | Operators.exclusiveOr, [left; right] -> binOp BinaryXorBitwise left right
        | Operators.booleanAnd, [left; right] -> logicOp LogicalAnd left right
        | Operators.booleanOr, [left; right] -> logicOp LogicalOr left right
        | Operators.logicalNot, [operand] -> unOp UnaryNotBitwise operand
        | Operators.unaryNegation, [operand] -> unOp UnaryMinus operand
        | _ -> "Unknown operator: " + opName |> addErrorAndReturnNull com r
    let argTypes = resolveArgTypes argTypes genArgs
    match argTypes with
    | Builtin(BclInt64|BclUInt64|BclBigInt|BclDateTime|BclDateTimeOffset|FSharpSet as bt)::_ ->
        coreCall r t (coreModFor bt) opName None args argTypes
    | Builtin BclTimeSpan::_ ->
        nativeOp opName argTypes args
    | CustomOp com opName m ->
        let genArgs = genArgs |> Seq.map snd
        FSharp2Fable.Util.makeCallFrom com ctx r t genArgs None args m
    | _ -> nativeOp opName argTypes args

let equals r equal (args: Expr list) =
    let op equal =
        if equal then BinaryEqualStrict else BinaryUnequalStrict

    let is equal expr =
        if equal
        then expr
        else makeUnOp None Boolean expr UnaryNot

    match args with
    | [ExprType(Builtin(BclGuid|BclTimeSpan)) as left; right]
    | [ExprType(Boolean | Char | String | Number _ | EnumType _) as left; right] ->
        op equal |> makeBinOp r Boolean left right

    | ExprType(Builtin(BclDateTime|BclDateTimeOffset))::_ ->
        coreCall_ Boolean "Date" "equals" args |> is equal

    | ExprType(Builtin(FSharpSet|FSharpMap)) as callee::args ->
        instanceCall_ Boolean callee "Equals" args |> is equal

    | ExprType(Builtin(BclInt64|BclUInt64|BclBigInt as bt))::_ ->
        coreCall_ Boolean (coreModFor bt) "equals" args |> is equal

    | ExprType(Array _ | List _ | Tuple _)::_ ->
        coreCall_ Boolean "Util" "equalArrays" args |> is equal
    | ExprType(DeclaredType(ent,_))::_ when ent.IsFSharpUnion ->
        coreCall_ Boolean "Util" "equalArrays" args |> is equal

    | ExprType(DeclaredType(ent,_))::_ when ent.IsFSharpRecord ->
        coreCall_ Boolean "Util" "equalObjects" args |> is equal

    | _ -> coreCall_ Boolean "Util" "equals" args |> is equal

/// Compare function that will call Util.compare or instance `CompareTo` as appropriate
/// If passed an optional binary operator, it will wrap the comparison like `comparison < 0`
let compare r (args: Expr list) op =
    let wrapWith op comparison =
        match op with
        | None -> comparison
        | Some op -> makeEqOp r comparison (makeIntConst 0) op

    match args with
    | [ExprType(Builtin(BclGuid|BclTimeSpan)) as left; right]
    | [ExprType(Boolean | Char | String | Number _ | EnumType _) as left; right] ->
        match op with
        | Some op -> makeEqOp r left right op
        | None -> coreCall_ (Number Int32) "Util" "comparePrimitives" args

    | ExprType(Builtin(BclDateTime|BclDateTimeOffset))::_ ->
        coreCall_ (Number Int32) "Date" "compare" args |> wrapWith op

    | ExprType(Builtin(BclInt64|BclUInt64|BclBigInt as bt))::_ ->
        coreCall_ Boolean (coreModFor bt) "compare" args |> wrapWith op

    | ExprType(Array _ | List _ | Tuple _)::_ ->
        coreCall_ Boolean "Util" "compareArrays" args |> wrapWith op
    | ExprType(DeclaredType(ent,_))::_ when ent.IsFSharpUnion ->
        coreCall_ Boolean "Util" "compareArrays" args |> wrapWith op

    | ExprType(DeclaredType(ent,_))::_ when ent.IsFSharpRecord ->
        coreCall_ Boolean "Util" "compareObjects" args |> wrapWith op

    | _ -> coreCall_ (Number Int32) "Util" "compare" args |> wrapWith op

let makeComparerFunction typArg =
    let t = FunctionType(DelegateType [typArg; typArg], Number Int32)
    match typArg with
    | Builtin(BclGuid|BclTimeSpan)
    | Boolean | Char | String | Number _ | EnumType _ ->
        makeCoreRef t "Util" "comparePrimitives"
    | Builtin(BclDateTime|BclDateTimeOffset) ->
        makeCoreRef t "Date" "compare"
    | Builtin(BclInt64|BclUInt64|BclBigInt as bt) ->
       makeCoreRef t (coreModFor bt) "compare"
    | Array _ | List _ | Tuple _ ->
        makeCoreRef t "Util" "compareArrays"
    | DeclaredType(ent,_) when ent.IsFSharpUnion ->
        makeCoreRef t "Util" "compareArrays"
    | DeclaredType(ent,_) when ent.IsFSharpRecord ->
        makeCoreRef t "Util" "compareObjects"
    | _ ->
        makeCoreRef t "Util" "compare"

// TODO!!! This and the following method must be adjusted to new fable-core Map and Set modules
let makeMapOrSetCons com r t (i: CallInfo) modName args =
    let typArg = firstGenArg com r i.GenericArgs
    let comparer =
        let fn = makeComparerFunction typArg
        coreCall_ Any "Util" "Comparer" [fn]
    let args =
        match args with
        | [] -> [Value (Null typArg); comparer]
        | args -> args @ [comparer]
    coreCall r t modName "create" None args i.ArgTypes

let makeDictionaryOrHashSet com r t (i: CallInfo) modName forceFSharp args =
    let makeFSharp typArg args =
        let args =
            match args with
            | [iterable] -> [iterable; coreCall_ Any "Util" "Comparer" [makeComparerFunction typArg]]
            | args -> args
        coreCall r t modName "create" None args i.ArgTypes
    let typArg = firstGenArg com r i.GenericArgs
    if forceFSharp
    then makeFSharp typArg args
    else
        match typArg with
        | Builtin(BclInt64|BclUInt64|BclBigInt)
        | Array _ | List _ | Tuple _ ->
            makeFSharp typArg args
        | Builtin(BclGuid|BclTimeSpan) ->
            globalCall r t modName None None args i.ArgTypes
        | DeclaredType _ ->
            makeFSharp typArg args
        | _ ->
            globalCall r t modName None None args i.ArgTypes

// TODO: Try to compile as literal object
let makeJsLiteralFromLambda arg =
    // match arg with
    // | Value(Lambda(args, lambdaBody, _)) ->
    //     (Some [], flattenSequential lambdaBody)
    //     ||> List.fold (fun acc statement ->
    //         match acc, statement with
    //         // Set of callee: Expr * property: Expr option * value: Expr * range: SourceLocation option
    //         | Some acc, Set(_,Some(Value(StringConst prop)),value,_) ->
    //             (prop, value)::acc |> Some
    //         | _ -> None)
    // | _ -> None
    // |> Option.map (makeJsObject r)
    // |> Option.defaultWith (fun () ->
        coreCall_ Any "Util" "jsOptions" [arg]
    // )

// TODO: Try to compile as literal object
let makeJsLiteral caseRule keyValueList =
    // let rec (|Fields|_|) caseRule = function
    //     | Value(ArrayConst(ArrayValues exprs, _)) ->
    //         (Some [], exprs) ||> List.fold (fun acc e ->
    //             acc |> Option.bind (fun acc ->
    //                 match e with
    //                 | Value(TupleConst [Value(StringConst key); value]) ->
    //                     (key, value)::acc |> Some
    //                 | UnionCons(tag, fields, cases) ->
    //                     let key =
    //                         let key = cases |> List.item tag |> fst
    //                         match caseRule with
    //                         | CaseRules.LowerFirst -> Naming.lowerFirst key
    //                         | _ -> key
    //                     let value =
    //                         match fields with
    //                         | [] -> Value(BoolConst true) |> Some
    //                         | [CoreCons "List" "default" []] -> makeJsObject r [] |> Some
    //                         | [CoreMeth "List" "ofArray" [Fields caseRule fields]] -> makeJsObject r fields |> Some
    //                         | [expr] ->
    //                             match expr.Type with
    //                             // Lists references must be converted to objects at runtime
    //                             | DeclaredType(fullName,_) when fullName = "Microsoft.FSharp.Collections.FSharpList" -> None
    //                             | _ -> Some expr
    //                         | exprs -> Value(ArrayConst(ArrayValues exprs, Any)) |> Some
    //                     value |> Option.map (fun value -> (key, value)::acc)
    //                 | _ -> None))
    //         |> Option.map List.rev
    //     | _ -> None
    // match keyValueList with
    // | CoreCons "List" "default" [] -> makeJsObject r []
    // | CoreMeth "List" "ofArray" [Fields caseRule fields] -> makeJsObject r fields
    // | _ ->
        [keyValueList; caseRule |> int |> makeIntConst]
        |> coreCall_ Any "Util" "createObj"

let fableCoreLib (com: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "importDynamic", _ ->
        globalCall r t "import" None thisArg args i.ArgTypes |> Some
    | Naming.StartsWith "import" suffix, _ ->
        let fail() =
            sprintf "Fable.Core.JsInterop.import%s only accepts literal strings" suffix
            |> addError com r
        let selector, args =
            match suffix with
            | "Member" -> Naming.placeholder, args
            | "Default" -> "default", args
            | "SideEffects" -> "", args
            | "All" -> "*", args
            | _ ->
                match args with
                | Value(StringConstant selector)::args -> selector, args
                | _ -> fail(); "*", [makeStrConst "unknown"]
        let path =
            match args with
            | [Value(StringConstant path)] -> path
            | _ -> fail(); "unknown"
        Import(selector, path, CustomImport, t) |> Some
    // Dynamic casting, erase
    | "op_BangBang", _ | "op_BangHat", _ -> List.tryHead args
    | "op_Dynamic", [left; memb] -> getExpr r t left memb |> Some
    | "op_DynamicAssignment", [callee; prop; value] ->
        Set(callee, ExprSet prop, value, r) |> Some
    | ("op_Dollar"|"createNew" as m), callee::args ->
        let argInfo = { argInfo None args None with Spread = TupleSpread }
        if m = "createNew"
        then constructorCall r t argInfo callee |> Some
        else staticCall r t argInfo callee |> Some
    | "op_EqualsEqualsGreater", _ ->
        NewTuple args |> Value |> Some
    | "createObj", [kvs] ->
        makeJsLiteral Fable.Core.CaseRules.None kvs |> Some
     | "keyValueList", _ ->
        match args with
        | [Value(Enum(NumberEnum rule, _)); keyValueList] ->
            let caseRule: Fable.Core.CaseRules = enum (int rule)
            makeJsLiteral caseRule keyValueList |> Some
        | [caseRule; keyValueList] ->
            coreCall r t "Util" "createObj" None [keyValueList; caseRule] i.ArgTypes |> Some
        | _ -> None
    | "jsOptions", [arg] ->
        makeJsLiteralFromLambda arg |> Some
    | "jsThis", _ ->
        This t |> Value |> Some
    | "createEmpty", _ ->
        objExpr t [] |> Some
    | "nameof", _ ->
        match args with
        | [Nameof name] -> name
        | _ -> "Cannot infer name of expression" |> addError com r; "unknown"
        |> makeStrConst |> Some
    | "nameofLambda", _ ->
        match args with
        | [Function(_, Nameof name, _)] -> name
        | _ -> "Cannot infer name of expression" |> addError com r; "unknown"
        |> makeStrConst |> Some
    | "AreEqual", _ ->
        coreCall r t "Util" "assertEqual" thisArg args i.ArgTypes |> Some
    | ("async.AwaitPromise.Static"|"async.StartAsPromise.Static" as m), _ ->
        let meth =
            if m = "async.AwaitPromise.Static"
            then "awaitPromise" else "startAsPromise"
        coreCall r t "Async" meth thisArg args i.ArgTypes |> Some
    | "jsNative", _ ->
        // TODO: Fail at compile time?
        addWarning com r "jsNative is being compiled without replacement, this will fail at runtime."
        let runtimeMsg =
            "A function supposed to be replaced by JS native code has been called, please check."
            |> StringConstant |> Value
        Throw(error runtimeMsg, t, r) |> Some
    | _ -> None

let references (_: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, [arg] ->
        objExpr t ["contents", arg] |> Some
    | "get_Value", Some callee, _ ->
        get r t callee "contents" |> Some
    | "set_Value", Some callee, [value] ->
        Set(callee, makeStrConst "contents" |> ExprSet, value, r) |> Some
    | _ -> None

let fsFormat (_: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "get_Value", Some callee, _ ->
        get None t callee "input" |> Some
    | "PrintFormatToStringThen", _, _ ->
        match args with
        | [_] -> coreCall r t "String" "toText" None args i.ArgTypes |> Some
        | [cont; fmt] -> instanceCall_ t fmt "cont" [cont] |> Some
        | _ -> None
    | "PrintFormatToString", _, _ ->
        coreCall r t "String" "toText" None args i.ArgTypes |> Some
    | "PrintFormatLine", _, _ ->
        coreCall r t "String" "toConsole" None args i.ArgTypes |> Some
    | ("PrintFormatToTextWriter"|"PrintFormatLineToTextWriter"), _, _::args ->
        // addWarning com r "fprintfn will behave as printfn"
        coreCall r t "String" "toConsole" None args i.ArgTypes |> Some
    | "PrintFormat", _, _ ->
        // addWarning com r "Printf will behave as printfn"
        coreCall r t "String" "toConsole" None args i.ArgTypes |> Some
    | "PrintFormatThen", _, arg::callee::_ ->
        instanceCall_ t callee "cont" [arg] |> Some
    | "PrintFormatToStringThenFail", _, _ ->
        coreCall r t "String" "toFail" None args i.ArgTypes |> Some
    | ".ctor", _, arg::_ ->
        coreCall r t "String" "printf" None [arg] i.ArgTypes |> Some
    | _ -> None

let operators (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let curriedApply r t applied args =
        Operation(CurriedApply(applied, args), t, r)

    let compose (com: ICompiler) r t f1 f2 =
        let argType, retType =
            match t with
            | FunctionType(LambdaType argType, retType) -> argType, retType
            | _ -> Any, Any
        let tempVar = com.GetUniqueVar("arg") |> makeTypedIdent argType
        let body =
            [IdentExpr tempVar]
            |> curriedApply None Any f1
            |> List.singleton
            |> curriedApply r retType f2
        Function(Lambda tempVar, body, None)

    let math r t (args: Expr list) argTypes methName =
        let methName = Naming.lowerFirst methName |> Some
        globalCall r t "Math" methName None args argTypes

    match i.CompiledName, args with
    | "DefaultArg", [arg1; arg2] ->
        if hasDoubleEvalRisk arg1
        then coreCall r t "Option" "defaultArg" None args i.ArgTypes |> Some
        else
            let cond = Test(arg1, OptionTest false, None)
            IfThenElse(cond, arg1, arg2) |> Some
    | "DefaultAsyncBuilder", _ ->
        makeCoreRef t "AsyncBuilder" "singleton" |> Some
    // Erased operators. TODO: Use Cast?
    // KeyValuePair is already compiled as a tuple
    | ("KeyValuePattern"|"Identity"|"Box"|"Unbox"|"ToEnum"), _ -> List.tryHead args
    // Cast to unit to make sure nothing is returned when wrapped in a lambda, see #1360
    | "Ignore", [arg]  -> Cast(arg, Unit) |> Some
    // TODO: Number and String conversions
    | ("ToSByte"|"ToByte"|"ToInt8"|"ToUInt8"|"ToInt16"|"ToUInt16"|"ToInt"|"ToUInt"|"ToInt32"|"ToUInt32"|"ToInt64"|"ToUInt64"), _ ->
        let sourceType = firstGenArg com r i.GenericArgs
        toInt false sourceType t args |> Some
    | ("ToSingle"|"ToDouble"|"ToDecimal"), _ ->
        let sourceType = firstGenArg com r i.GenericArgs
        toFloat sourceType t args |> Some
    | "ToChar", _ -> toChar (firstGenArg com r i.GenericArgs) args |> Some
    | "ToString", _ -> toString (firstGenArg com r i.GenericArgs) args |> Some
    // The cast will be resolved in a different step
    | "CreateSequence", [xs] -> Cast(xs, t) |> Some
    | "CreateDictionary", _ ->
        makeDictionaryOrHashSet com r t i "Map" false args |> Some
    | "CreateSet", _ ->
        makeMapOrSetCons com r t i "Set" args |> Some
    // Ranges
    | ("op_Range"|"op_RangeStep"), _ ->
        let meth =
            match firstGenArg com r i.GenericArgs with
            | Fable.Char -> "rangeChar"
            | _ -> if i.CompiledName = "op_Range" then "range" else "rangeStep"
        coreCall r t "Seq" meth None args i.ArgTypes |> Some
    // Pipes and composition
    | "op_PipeRight", [x; f]
    | "op_PipeLeft", [f; x] -> curriedApply r t f [x] |> Some
    | "op_PipeRight2", [x; y; f]
    | "op_PipeLeft2", [f; x; y] -> curriedApply r t f [x; y] |> Some
    | "op_PipeRight3", [x; y; z; f]
    | "op_PipeLeft3", [f; x; y; z] -> curriedApply r t f [x; y; z] |> Some
    | "op_ComposeRight", [f1; f2] -> compose com r t f1 f2 |> Some
    | "op_ComposeLeft", [f2; f1] -> compose com r t f1 f2 |> Some
    // Strings
    | ("PrintFormatToString"             // sprintf
    |  "PrintFormatToStringThen"         // Printf.sprintf
    |  "PrintFormat" | "PrintFormatLine" // printf / printfn
    |  "PrintFormatThen"                 // Printf.kprintf
    |  "PrintFormatToStringThenFail"), _ ->  // Printf.failwithf
        fsFormat com r t i thisArg args
    // Exceptions
    | "FailWith", [msg] | "InvalidOp", [msg] ->
        Throw(error msg, t, r) |> Some
    | "InvalidArg", [argName; msg] ->
        let msg = add (add msg (s "\\nParameter name: ")) argName
        Throw(error msg, t, r) |> Some
    | "Raise", [arg] -> Throw(arg, t, r) |> Some
    | "Reraise", _ ->
        match ctx.CaughtException with
        | Some ex -> Throw(IdentExpr ex, t, r) |> Some
        | None ->
            addError com r "`reraise` used in context where caught exception is not available, please report"
            Throw(error (s ""), t, r) |> Some
    // Math functions
    // TODO: optimize square pow: x * x
    | "Pow", _ | "PowInteger", _ | "op_Exponentiation", _ ->
        math r t args i.ArgTypes "pow" |> Some
    | "Ceil", _ | "Ceiling", _ ->
        math r t args i.ArgTypes "ceil" |> Some
    | "Log", [arg1; arg2] ->
        // "Math.log($0) / Math.log($1)"
        let dividend = math None t [arg1] (List.take 1 i.ArgTypes) "log"
        let divisor = math None t [arg2] (List.skip 1 i.ArgTypes) "log"
        makeBinOp r t dividend divisor BinaryDivide |> Some
    | "Abs", _ ->
        match resolveArgTypes i.ArgTypes i.GenericArgs with
        | Builtin(BclInt64 | BclBigInt as bt)::_  ->
            coreCall r t (coreModFor bt) "abs" thisArg args i.ArgTypes |> Some
        | _ -> math r t args i.ArgTypes i.CompiledName |> Some
    | "Acos", _ | "Asin", _ | "Atan", _ | "Atan2", _
    | "Cos", _ | "Exp", _ | "Floor", _ | "Log", _ | "Log10", _
    | "Sin", _ | "Sqrt", _ | "Tan", _ ->
        math r t args i.ArgTypes i.CompiledName |> Some
    | "Round", _ ->
        coreCall r t "Util" "round" thisArg args i.ArgTypes |> Some
    | "Sign", _ ->
        let args =
            match args with
            | ((ExprType(Builtin(BclInt64|BclBigInt) as t)) as arg)::_ ->
                toFloat t (Number Float64) [arg] |> List.singleton
            | _ -> args
        coreCall r t "Util" "sign" None args i.ArgTypes |> Some
    // Numbers
    | ("Infinity"|"InfinitySingle"), _ ->
        globalIdent "Number" "POSITIVE_INFINITY" |> Some
    | ("NaN"|"NaNSingle"), _ ->
        globalIdent "Number" "NaN" |> Some
    // TODO: Move these optimizations to special pass and check side effects
    // | "Fst", [Value(NewTuple(fst::_))] -> Some fst
    // | "Snd", [Value(NewTuple(_::snd::_))] -> Some snd
    | "Fst", [tup] -> Get(tup, TupleGet 0, t, r) |> Some
    | "Snd", [tup] -> Get(tup, TupleGet 1, t, r) |> Some
    // Reference
    | "op_Dereference", [arg] -> get r t arg "contents" |> Some
    | "op_ColonEquals", [o; v] -> Set(o, makeStrConst "contents" |> ExprSet, v, r) |> Some
    | "Ref", [arg] -> objExpr t ["contents", arg] |> Some
    | ("Increment"|"Decrement"), _ ->
        if i.CompiledName = "Increment" then "void($0.contents++)" else "void($0.contents--)"
        |> emitJs r t args |> Some
    // Concatenates two lists
    | "op_Append", _ -> coreCall r t "List" "append" thisArg args i.ArgTypes |> Some
    | ("op_Inequality"|"Neq"), _ -> equals r false args |> Some
    | ("op_Equality"|"Eq"), _ -> equals r true args |> Some
    | "IsNull", [arg] -> makeEqOp r arg (Null arg.Type |> Value) BinaryEqual |> Some
    | "Hash", _ -> coreCall r t "Util" "hash" thisArg args i.ArgTypes |> Some
    // Comparison
    | "Compare", _ -> compare r args None |> Some
    | ("op_LessThan"|"Lt"), _ -> compare r args (Some BinaryLess) |> Some
    | ("op_LessThanOrEqual"|"Lte"), _ -> compare r args (Some BinaryLessOrEqual) |> Some
    | ("op_GreaterThan"|"Gt"), _ -> compare r args (Some BinaryGreater) |> Some
    | ("op_GreaterThanOrEqual"|"Gte"), _ -> compare r args (Some BinaryGreaterOrEqual) |> Some
    | ("Min"|"Max"), [arg1; arg2] ->
        let op = if i.CompiledName = "Min" then BinaryLess else BinaryGreater
        let comparison = compare r args (Some op)
        IfThenElse(comparison, arg1, arg2) |> Some
    | "Not", [operand] -> // TODO: Check custom operator?
        makeUnOp r t operand UnaryNot |> Some
    | SetContains Operators.standardSet, _ ->
        applyOp com ctx r t i.CompiledName args i.ArgTypes i.GenericArgs |> Some
    // // Type ref
    // | "typeOf" | "typeDefOf" ->
    //     None // TODO
    //     // info.memberGenArgs.Head
    //     // |> resolveTypeRef com info (info.memberName = "typeOf")
    //     // |> Some
    | _ -> None

let chars (com: ICompiler) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    let icall r t args argTypes memb  =
        match args, argTypes with
        | thisArg::args, _::argTypes ->
            let info = argInfo (Some thisArg) args (Some argTypes)
            instanceCall r t info (makeStrConst memb |> Some) |> Some
        | _ -> None
    match i.CompiledName with
    | "ToUpper" -> icall r t args i.ArgTypes "toLocaleUpperCase"
    | "ToUpperInvariant" -> icall r t args i.ArgTypes "toUpperCase"
    | "ToLower" -> icall r t args i.ArgTypes "toLocaleLowerCase"
    | "ToLowerInvariant" -> icall r t args i.ArgTypes "toLowerCase"
    | "IsLetter" | "IsNumber" | "IsDigit"
    | "IsLetterOrDigit" | "IsWhiteSpace"
    | "IsUpper" | "IsLower"
    | "Parse" ->
        let methName =
            match i.CompiledName with
            | "IsNumber" ->
                addWarning com r "Char.IsNumber is compiled as Char.IsDigit"
                "isDigit"
            | methName -> Naming.lowerFirst methName
        let args, argTypes =
            match args with
            | [str; idx] -> [getExpr None Char str idx], [Char]
            | args -> args, i.ArgTypes
        coreCall r t "Char" methName None args argTypes |> Some
    | _ -> None

let strings (com: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, fstArg::_ ->
        match fstArg.Type with
        | String ->
            match args with
            | [_; _] -> emitJs r t args "Array($1 + 1).join($0)" |> Some // String(char, int)
            | _ -> addErrorAndReturnNull com r "Unexpected arguments in System.String constructor." |> Some
        | Array _ ->
            match args with
            | [_] -> emitJs r t args "$0.join('')" |> Some // String(char[])
            | [_; _; _] -> emitJs r t args "$0.join('').substr($1, $2)" |> Some // String(char[], int, int)
            | _ -> addErrorAndReturnNull com r "Unexpected arguments in System.String constructor." |> Some
        | _ ->
            fsFormat com r t i thisArg args
    | "get_Length", Some c, _ -> get r t c "length" |> Some
    | "Length", None, [arg] -> get r t arg "length" |> Some
    | "Equals", Some x, [y]
    | "Equals", None, [x; y] ->
        makeEqOp r x y BinaryEqualStrict |> Some
    | "Equals", Some x, [y; kind]
    | "Equals", None, [x; y; kind] ->
        makeEqOp r (coreCall_ (Number Int32) "String" "compare" [x; y; kind]) (makeIntConst 0) BinaryEqualStrict |> Some
    | "Contains", Some c, arg::_ ->
        if (List.length args) > 1 then
            addWarning com r "String.Contains: second argument is ignored"
        makeEqOp r (instanceCall_ (Number Int32) c "indexOf" [arg]) (makeIntConst 0) BinaryGreaterOrEqual |> Some
    | "StartsWith", Some c, [_str] ->
        makeEqOp r (instanceCall_ (Number Int32) c "indexOf" args) (makeIntConst 0) BinaryEqualStrict |> Some
    | "StartsWith", c, [_str; _comp] ->
        coreCall r t "String" "startsWith" c args i.ArgTypes|> Some
    | "Substring", _, _ -> icall r t i thisArg args "substr" |> Some
    | "ToUpper", _, _ -> icall r t i thisArg args "toLocaleUpperCase" |> Some
    | "ToUpperInvariant", _, _ -> icall r t i thisArg args "toUpperCase" |> Some
    | "ToLower", _, _ -> icall r t i thisArg args "toLocaleLowerCase" |> Some
    | "ToLowerInvariant", _, _ -> icall r t i thisArg args "toLowerCase" |> Some
    | "Chars", _, _ -> coreCall r t "String" "getCharAtIndex" thisArg args i.ArgTypes |> Some
    | ("IndexOf" | "LastIndexOf"), _, _ ->
        match args with
        | [ExprType Char]
        | [ExprType String]
        | [ExprType Char; ExprType(Number Int32)]
        | [ExprType String; ExprType(Number Int32)] ->
            Naming.lowerFirst i.CompiledName
            |> icall r t i thisArg args |> Some
        | _ -> "The only extra argument accepted for String.IndexOf/LastIndexOf is startIndex."
               |> addErrorAndReturnNull com r |> Some
    | ("Trim" | "TrimStart" | "TrimEnd"), Some c, _ ->
        let side =
            match i.CompiledName with
            | "TrimStart" -> "start"
            | "TrimEnd" -> "end"
            | _ -> "both"
        coreCall_ t "String" "trim" (c::(makeStrConst side)::args) |> Some
    | "ToCharArray", Some c, _ ->
        instanceCall_ t c "split" [makeStrConst ""] |> Some
    | ("Iterate" | "IterateIndexed" | "ForAll" | "Exists"), _, _ ->
        // TODO!!! Cast the string to seq<char>
        coreCall r t "Seq" (Naming.lowerFirst i.CompiledName) thisArg args i.ArgTypes |> Some
    | ("Map" | "MapIndexed" | "Collect"), _, _ ->
        // TODO!!! Cast the string to seq<char>
        let name = Naming.lowerFirst i.CompiledName
        emitJs r t [coreCall_ Any "Seq" name args] "Array.from($0).join('')" |> Some
    | "Concat", _, _ ->
        let args =
            if i.DeclaringEntityFullName = "System.String"
            then (makeStrConst "")::args else args
        coreCall_ t "String" "join" args |> Some
    | "Split", Some c, _ ->
        match args with
        // Optimization
        | [] -> instanceCall_ t c "split" [Value(StringConstant " ")] |> Some
        | [Value(StringConstant _) as separator]
        | [Value(NewArray(ArrayValues [separator],_))] ->
            instanceCall_ t c "split" [separator] |> Some
        | [arg1; ExprType(EnumType _) as arg2] ->
            let arg1 =
                match arg1.Type with
                | Array _ -> arg1
                | _ -> Value(NewArray(ArrayValues [arg1], String))
            let args = [arg1; Value(Null Any); arg2]
            coreCall_ t "String" "split" (c::args) |> Some
        | args ->
            coreCall r t "String" "split" thisArg args i.ArgTypes |> Some
    | "Filter", _, _ -> coreCall r t "String" "filter" thisArg args i.ArgTypes |> Some
    | _ -> None

let arrayCons (com: ICompiler) genArg =
    match genArg with
    | Number numberKind when com.Options.typedArrays ->
        getTypedArrayName com numberKind |> makeIdentExpr
    | _ -> makeIdentExpr "Array"

let seqs (com: ICompiler) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "ToArray", [arg] -> toArray com t arg |> Some
    // A export function 'length' method causes problems in JavaScript -- https://github.com/Microsoft/TypeScript/issues/442
    | "Length", _ ->
        coreCall r t "Seq" "count" thisArg args i.ArgTypes |> Some
    | "ChunkBySize" | "Permute" as meth, [arg1; arg2] ->
        let arg2 = toArray com (Array Any) arg2
        let args =
            match meth, t with
            | "Permute", DeclaredType(_seq, [genArg]) ->
                [arg1; arg2] @ [arrayCons com genArg]
            | _ -> [arg1; arg2]
        let result = coreCall_ Any "Array" (Naming.lowerFirst meth) args
        coreCall_ t "Seq" "ofArray" [result] |> Some
    | meth, _ -> coreCall r t "Seq" (Naming.lowerFirst meth) thisArg args i.ArgTypes |> Some
    // | _ -> None

let arrays (com: ICompiler) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, args, i.CompiledName with
    | None, [arg], "OfSeq" -> toArray com t arg |> Some
    | Some c, _, "get_Length" -> get r t c "length" |> Some
    | None, [arg], "Length" -> get r t arg "length" |> Some
    | None, [ar; idx], "Get" -> getExpr r t ar idx |> Some
    | None, [ar; idx; value], "Set" -> Set(ar, ExprSet idx, value, r) |> Some
    | _, _, "ZeroCreate" ->
        match firstGenArg com r i.GenericArgs, args with
        | Number _ as t, [len] -> NewArray(ArrayAlloc len, t) |> Value |> Some
        | Boolean, _ -> emitJs r t args "new Array($0).fill(false)" |> Some
        // If we don't fill the array with null values some operations
        // may behave unexpectedly, like Array.prototype.reduce
        | _ -> emitJs r t args "new Array($0).fill(null)" |> Some
    // dynamicSet r ar idx value |> Some
    | None, _, meth ->
        let args, meth =
            if meth = "Sort"
            then (firstGenArg com r i.GenericArgs |> makeComparerFunction)::args, "SortWith"
            else args, meth
        let args =
            match t with
            // TODO: Check if this covers all cases where the constructor is needed
            | Array genArg | Tuple([Array genArg; _]) -> args @ [arrayCons com genArg]
            | _ -> args
        coreCall r t "Array" (Naming.lowerFirst meth) thisArg args i.ArgTypes |> Some
    | _ -> None

let lists (com: ICompiler) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, args, i.CompiledName with
    | None, [x], "Head"
    | Some x, _, "get_Head" -> Get(x, ListHead, t, r) |> Some
    | None, [x], "Tail"
    | Some x, _, "get_Tail" -> Get(x, ListTail, t, r) |> Some
    | _, _, "get_Length" -> coreCall r t "List" "length" thisArg args i.ArgTypes |> Some
    | Some x, _, "get_Item" -> coreCall r t "List" "item" None (args@[x]) i.ArgTypes |> Some
    | Some x, _, "get_IsEmpty" -> Test(x, ListTest false, r) |> Some
    | None, _, ("get_Empty" | "Empty") ->
        NewList(None, firstGenArg com r i.GenericArgs) |> Value |> Some
    | None, [h;t], "Cons" -> NewList(Some(h,t), firstGenArg com r i.GenericArgs) |> Value |> Some
    | None, [x], "ToSeq" -> Cast(x, t) |> Some
    | None, _, "OfSeq" -> coreCall r t "Seq" "toList" None args i.ArgTypes |> Some
    | None, _, meth ->
        let meth = Naming.lowerFirst meth
        coreCall r t "List" meth None args i.ArgTypes |> Some
    | _ -> None

let options (_: ICompiler) r (t: Type) (i: CallInfo) (thisArg: Expr option) (_args: Expr list) =
    match thisArg, i.CompiledName with
    | Some c, "get_Value" -> Get(c, OptionValue, t, r) |> Some
    | _ -> None

let decimals (com: ICompiler) r (_: Type) (i: CallInfo) (_callee: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | ".ctor", [Value(NumberConstant(x, _))] ->
#if FABLE_COMPILER
        makeNumConst (float x) |> Some
#else
        makeDecConst (decimal(x)) |> Some
#endif
    | ".ctor", [Value(NewArray(ArrayValues arVals, _))] ->
        match arVals with
        | [ Value(NumberConstant(low, Int32))
            Value(NumberConstant(mid, Int32))
            Value(NumberConstant(high, Int32))
            Value(NumberConstant(scale, Int32)) ] ->
#if FABLE_COMPILER
                let x = (float ((uint64 (uint32 mid)) <<< 32 ||| (uint64 (uint32 low))))
                        / System.Math.Pow(10.0, float ((int scale) >>> 16 &&& 0xFF))
                makeNumConst (if scale < 0.0 then -x else x) |> Some
#else
                makeDecConst (new decimal([| int low; int mid; int high; int scale |])) |> Some
#endif
        | _ -> None
    | (".ctor" | "MakeDecimal"),
          [ Value(NumberConstant(low, Int32))
            Value(NumberConstant(mid, Int32))
            Value(NumberConstant(high, Int32))
            Value(BoolConstant isNegative)
            Value(NumberConstant(scale, UInt8)) ] ->
#if FABLE_COMPILER
                let x = (float ((uint64 (uint32 mid)) <<< 32 ||| (uint64 (uint32 low))))
                        / System.Math.Pow(10.0, float scale)
                makeNumConst (if isNegative then -x else x) |> Some
#else
                makeDecConst (new decimal(int low, int mid, int high, isNegative, byte scale)) |> Some
#endif
    | ".ctor", [IdentExpr _ as arg] ->
        addWarning com r "Decimals are implemented with floats."
        Some arg
    | ("Parse" | "TryParse"), _ ->
        None // TODO: parse com i true
    | _,_ -> None

// let bigint (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
//     let coreMod = coreModFor BclBigInt
//     match thisArg, i.CompiledName with
//     | None, ".ctor" ->
//         match i.ArgTypes with
//         | [Builtin(BclInt64|BclUInt64)] -> coreCall r t i coreMod "fromInt64" args
//         | [_] -> coreCall r t i coreMod "fromInt32" args
//         | _ -> coreCall r t i coreMod "default" args
//         |> Some
//     | None, ("Zero"|"One"|"Two" as memb) ->
//         makeCoreRef t coreMod (Naming.lowerFirst memb) |> Some
//     | None, ("FromZero"|"FromOne" as memb) ->
//         let memb = memb.Replace("From", "") |> Naming.lowerFirst
//         makeCoreRef t coreMod memb |> Some
//     | None, "FromString" ->
//         coreCall r t i coreMod "parse" args |> Some
//     | None, meth -> coreCall r t i coreMod meth args |> Some
//     | Some _callee, _ ->
//         // icall i meth |> Some
//         "TODO: BigInt instance methods" |> addErrorAndReturnNull com r |> Some

// Compile static strings to their constant values
// reference: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/languageprimitives.errorstrings-module-%5bfsharp%5d
let errorStrings = function
    | "InputArrayEmptyString" -> s "The input array was empty" |> Some
    | "InputSequenceEmptyString" -> s "The input sequence was empty" |> Some
    | "InputMustBeNonNegativeString" -> s "The input must be non-negative" |> Some
    | _ -> None

let languagePrimitives (com: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    // TODO: Check for types with custom zero/one (strings?)
    | "GenericZero" -> NumberConstant(0., Int32) |> Value |> Some
    | "GenericOne" -> NumberConstant(1., Int32) |> Value |> Some
    | _ -> None

let intrinsicFunctions (com: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    // Erased operators
    | "CheckThis", [arg]
    | "UnboxFast", [arg]
    | "UnboxGeneric", [arg] -> Some arg
    | "MakeDecimal", _ -> decimals com r t i thisArg args
    | "GetString", [ar; idx]
    | "GetArray", [ar; idx] -> getExpr r t ar idx |> Some
    | "SetArray", [ar; idx; value] -> Set(ar, ExprSet idx, value, r) |> Some
    | _ -> None
//         match i.memberName, (i.thisArg, args) with
//         | ("getArraySlice" | "getStringSlice"), ThreeArgs (ar, lower, upper) ->
//             let upper =
//                 let t = Number Int32
//                 match upper with
//                 | Null _ -> makeGet None t ar (makeStrConst "length")
//                 | _ -> Apply(Value(BinaryOp BinaryPlus),
//                                 [upper; makeIntConst 1], ApplyMeth, t, None)
//             InstanceCall (ar, "slice", [lower; upper])
//             |> makeCall r t |> Some
//         | "setArraySlice", (None, args) ->
//             CoreLibCall("Array", Some "setSlice", false, args)
//             |> makeCall r t |> Some
//         | "typeTestGeneric", (None, [expr]) ->
//             makeTypeTest com i.fileName r expr i.memberGenArgs.Head |> Some
//         | "createInstance", (None, _) ->
//             None // TODO
//             // let typRef, args = resolveTypeRef com i false i.memberGenArgs.Head, []
//             // Apply (typRef, args, ApplyCons, t, r) |> Some
//         | "rangeInt32", (None, args) ->
//             CoreLibCall("Seq", Some "rangeStep", false, args)
//             |> makeCall r t |> Some
//         // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.powdouble-function-%5bfsharp%5d
//         // Type: PowDouble : float -> int -> float
//         // Usage: PowDouble x n
//         | "powDouble", (None, _) ->
//             GlobalCall ("Math", Some "pow", false, args)
//             |> makeCall r t
//             |> Some
//         // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangechar-function-%5bfsharp%5d
//         // Type: RangeChar : char -> char -> seq<char>
//         // Usage: RangeChar start stop
//         | "rangeChar", (None, _) ->
//             CoreLibCall("Seq", Some "rangeChar", false, args)
//             |> makeCall r t |> Some
//         // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangedouble-function-%5bfsharp%5d
//         // Type: RangeDouble: float -> float -> float -> seq<float>
//         // Usage: RangeDouble start step stop
//         | "rangeDouble", (None, _) ->
//             CoreLibCall("Seq", Some "rangeStep", false, args)
//             |> makeCall r t |> Some
//         | _ -> None

let keyValuePairs (_: ICompiler) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> Value(NewTuple args) |> Some
    | "get_Key", Some c -> Get(c, TupleGet 0, t, r) |> Some
    | "get_Value", Some c -> Get(c, TupleGet 1, t, r) |> Some
    | _ -> None

let dictionaries (com: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let (|IDictionary|IEqualityComparer|Other|) = function
        | DeclaredType(ent,_) ->
            match ent.TryFullName with
            | Some "System.Collections.Generic.IDictionary`2" -> IDictionary
            | Some "System.Collections.Generic.IEqualityComparer`1" -> IEqualityComparer
            | _ -> Other
        | _ -> Other
    let inline makeComparer e =
        coreCall_ Any "Util" "comparerFromEqualityComparer" [e]
    let inline makeDic forceFSharpMap args =
        makeDictionaryOrHashSet com r t i "Map" forceFSharpMap args
    match i.CompiledName with
    | ".ctor" ->
        match i.ArgTypes with
        | [] | [IDictionary] ->
            makeDic false args |> Some
        | [IDictionary; IEqualityComparer] ->
            makeDic true [args.Head; makeComparer args.Tail.Head] |> Some
        | [IEqualityComparer] ->
            makeDic true [Value (Null Any); makeComparer args.Head] |> Some
        | [Number _] ->
            makeDic false [] |> Some
        | [Number _; IEqualityComparer] ->
            makeDic true [Value (Null Any); makeComparer args.Tail.Head] |> Some
        | _ -> None
    | "get_IsReadOnly" ->
        // TODO: Check for immutable maps with IDictionary interface
        makeBoolConst false |> Some
    | "get_Count" ->
        get r t thisArg.Value "size" |> Some
    // TODO: Check if the key allows for a JS Map (also "TryGetValue" below)
    | "ContainsValue" ->
        match thisArg, args with
        | Some c, [arg] -> coreCall_ t "Util" "containsValue" [arg; c] |> Some
        | _ -> None
    | "TryGetValue" ->
        coreCall r t "Util" "tryGetValue" thisArg args i.ArgTypes |> Some
    | "get_Item" -> icall r t i thisArg args "get" |> Some
    | "set_Item" -> icall r t i thisArg args "set" |> Some
    | "get_Keys" -> icall r t i thisArg args "keys" |> Some
    | "get_Values" -> icall r t i thisArg args "values" |> Some
    | "ContainsKey" -> icall r t i thisArg args "has" |> Some
    | "Clear" -> icall r t i thisArg args "clear" |> Some
    | "Add" -> icall r t i thisArg args "set" |> Some
    | "Remove" -> icall r t i thisArg args "delete" |> Some
    | _ -> None

let hashSets (com: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let (|IEnumerable|IEqualityComparer|Other|) = function
        | DeclaredType(ent,_) ->
            match ent.TryFullName with
            | Some "System.Collections.Generic.IEnumerable`1" -> IEnumerable
            | Some "System.Collections.Generic.IEqualityComparer`1" -> IEqualityComparer
            | _ -> Other
        | _ -> Other
    let inline makeComparer e =
        coreCall_ Any "Util" "comparerFromEqualityComparer" [e]
    let inline makeHashSet forceFSharpMap args =
        makeDictionaryOrHashSet com r t i "Set" forceFSharpMap args
    match i.CompiledName with
    | ".ctor" ->
        match i.ArgTypes with
        | [] | [IEnumerable] ->
            makeHashSet false args |> Some
        | [IEnumerable; IEqualityComparer] ->
            [args.Head; makeComparer args.Tail.Head]
            |> makeHashSet true |> Some
        | [IEqualityComparer] ->
            [Value (Null Any); makeComparer args.Head]
            |> makeHashSet true |> Some
        | _ -> None
    | "get_Count" -> get r t thisArg.Value "size" |> Some
    | "get_IsReadOnly" -> BoolConstant false |> Value |> Some
    | "Clear" -> icall r t i thisArg args "clear" |> Some
    | "Contains" -> icall r t i thisArg args "has" |> Some
    | "Remove" -> icall r t i thisArg args "delete" |> Some
    // TODO: Check if the value allows for a JS Set (also "TryGetValue" below)
    | "Add" ->
        match thisArg, args with
        | Some c, [arg] -> coreCall_ t "Util" "addToSet" [arg; c] |> Some
        | _ -> None
    // | "isProperSubsetOf" | "isProperSupersetOf"
    // | "unionWith" | "intersectWith" | "exceptWith"
    // | "isSubsetOf" | "isSupersetOf" | "copyTo" ->
    //     let meth =
    //         let m = match i.memberName with "exceptWith" -> "differenceWith" | m -> m
    //         m.Replace("With", "InPlace")
    //     CoreLibCall ("Set", Some meth, false, i.callee.Value::args)
    //     |> makeCall i.range i.returnType |> Some
    // TODO
    // | "setEquals"
    // | "overlaps"
    // | "symmetricExceptWith"
    | _ -> None

let exceptions (_: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> constructorCall_ t (makeIdentExpr "Error") args |> Some
    | "get_Message", Some e -> get r t e "message" |> Some
    | "get_StackTrace", Some e -> get r t e "stack" |> Some
    | _ -> None

let objects (_: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> objExpr t [] |> Some
    | "GetHashCode", Some arg, _ ->
        coreCall_ t "Util" "getHashCode" [arg] |> Some
    | "ToString", Some arg, _ ->
        coreCall_ t "Util" "toString" [arg] |> Some
    | "ReferenceEquals", _, [left; right] ->
        makeBinOp r Boolean left right BinaryEqualStrict |> Some
    | "Equals", Some arg1, [arg2]
    | "Equals", None, [arg1; arg2] ->
        coreCall_ t "Util" "equals" [arg1; arg2] |> Some
    | _ -> None

let unchecked (com: ICompiler) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "DefaultOf" -> firstGenArg com r i.GenericArgs |> defaultof |> Some
    | "Hash" -> coreCall r t "Util" "hash" None args i.ArgTypes |> Some
    | "Equals" -> coreCall r t "Util" "equals" None args i.ArgTypes |> Some
    | "Compare" -> coreCall r t "Util" "compare" None args i.ArgTypes |> Some
    | _ -> None

let uncurryExpr t arity (expr: Expr) =
    coreCall_ t "Util" "uncurry" [makeIntConst arity; expr]

let partialApply t arity (fn: Expr) (args: Expr list) =
    let args = NewArray(ArrayValues args, Any) |> Value
    coreCall_ t "Util" "partialApply" [makeIntConst arity; fn; args]

let tryField returnTyp ownerTyp fieldName =
    match ownerTyp, fieldName with
    | Number Decimal, "Zero" -> makeDecConst 0M |> Some
    | Number Decimal, "One" -> makeDecConst 1M |> Some
    | String, "Emtpy" -> makeStrConst "" |> Some
    | Builtin BclGuid, "Empty" -> makeStrConst "00000000-0000-0000-0000-000000000000" |> Some
    | Builtin BclTimeSpan, "Zero" -> makeIntConst 0 |> Some
    | Builtin BclDateTime, ("MaxValue" | "MinValue") ->
        coreCall_ returnTyp (coreModFor BclDateTime) (Naming.lowerFirst fieldName) [] |> Some
    | Builtin BclDateTimeOffset, ("MaxValue" | "MinValue") ->
        coreCall_ returnTyp (coreModFor BclDateTimeOffset) (Naming.lowerFirst fieldName) [] |> Some
    | _ -> None

let tryCall (com: ICompiler) ctx r t (info: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match info.DeclaringEntityFullName with
    | "System.Math"
    | "Microsoft.FSharp.Core.Operators"
    | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators"
    | "Microsoft.FSharp.Core.ExtraTopLevelOperators" -> operators com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions"
    | "Microsoft.FSharp.Core.Operators.OperatorIntrinsics" -> intrinsicFunctions com r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings" -> errorStrings info.CompiledName
    | "Microsoft.FSharp.Core.LanguagePrimitives" -> languagePrimitives com r t info thisArg args
    | "System.Char" -> chars com r t info thisArg args
    | "System.String"
    | "Microsoft.FSharp.Core.StringModule" -> strings com r t info thisArg args
    | "System.Array"
    | "Microsoft.FSharp.Collections.ArrayModule" -> arrays com r t info thisArg args
    | "Microsoft.FSharp.Collections.FSharpList`1"
    | "Microsoft.FSharp.Collections.ListModule" -> lists com r t info thisArg args
    | "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers"
    | "Microsoft.FSharp.Collections.SeqModule" -> seqs com r t info thisArg args
    | "System.Collections.Generic.KeyValuePair`2" -> keyValuePairs com r t info thisArg args
    | "System.Collections.Generic.Dictionary`2"
    | "System.Collections.Generic.IDictionary`2" -> dictionaries com r t info thisArg args
    | "System.Collections.Generic.HashSet`1"
    | "System.Collections.Generic.ISet`1" -> hashSets com r t info thisArg args
    | "Microsoft.FSharp.Core.FSharpOption`1"
    | "Microsoft.FSharp.Core.OptionModule" -> options com r t info thisArg args
    | "System.Decimal" -> decimals com r t info thisArg args
    // | "System.Numerics.BigInteger"
    // | "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI" -> bigint com r t info thisArg args
    | "Microsoft.FSharp.Core.FSharpRef" -> references com r t info thisArg args
    | "Microsoft.FSharp.Core.PrintfModule"
    | Naming.StartsWith "Microsoft.FSharp.Core.PrintfFormat" _ -> fsFormat com r t info thisArg args
    | "Microsoft.FSharp.Core.Operators.Unchecked" -> unchecked com r t info thisArg args
    | "System.Object" -> objects com r t info thisArg args
    | Naming.StartsWith "Fable.Core." _ -> fableCoreLib com r t info thisArg args
    | Naming.EndsWith "Exception" _ -> exceptions com r t info thisArg args
    | _ -> None
//         | "System.Timers.ElapsedEventArgs" -> info.thisArg // only signalTime is available here
//         | "System.Enum" -> enums com info
//         | "System.BitConverter" -> bitConvert com info
//         | "System.Int32" -> parse com info false
//         | "System.Single"
//         | "System.Double" -> parse com info true
//         | "System.Convert" -> convert com info
//         | "System.Console" -> console com info
//         | "System.Diagnostics.Debug"
//         | "System.Diagnostics.Debugger" -> debug com info
//         | "System.DateTime"
//         | "System.DateTimeOffset" -> dates com info
//         | "System.TimeSpan" -> timeSpans com info
//         | "System.Environment" -> systemEnv com info
//         | "System.Globalization.CultureInfo" -> globalization com info
//         | "System.Action" | "System.Func"
//         | "Microsoft.FSharp.Core.FSharpFunc"
//         | "Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc" -> funcs com info
//         | "System.Random" -> random com info
//         | "System.Threading.CancellationToken"
//         | "System.Threading.CancellationTokenSource" -> cancels com info
//         | "System.Activator" -> activator com info
//         | "System.Text.RegularExpressions.Capture"
//         | "System.Text.RegularExpressions.Match"
//         | "System.Text.RegularExpressions.Group"
//         | "System.Text.RegularExpressions.MatchCollection"
//         | "System.Text.RegularExpressions.GroupCollection"
//         | "System.Text.RegularExpressions.Regex" -> regex com info
//         | "System.Collections.Generic.IEnumerable"
//         | "System.Collections.IEnumerable" -> enumerable com info
//         | "System.Collections.Generic.Dictionary.KeyCollection"
//         | "System.Collections.Generic.Dictionary.ValueCollection"
//         | "System.Collections.Generic.ICollection" -> collectionsSecondPass com info Seq
//         | "System.Array"
//         | "System.Collections.Generic.List"
//         | "System.Collections.Generic.IList" -> collectionsSecondPass com info Array
//         | "Microsoft.FSharp.Collections.ArrayModule" -> collectionsFirstPass com info Array
//         | "Microsoft.FSharp.Collections.FSharpList"
//         | "Microsoft.FSharp.Collections.ListModule" -> collectionsFirstPass com info List
//         | "Microsoft.FSharp.Collections.FSharpMap"
//         | "Microsoft.FSharp.Collections.MapModule"
//         | "Microsoft.FSharp.Collections.FSharpSet"
//         | "Microsoft.FSharp.Collections.SetModule" -> mapAndSets com info
//         | "System.Type" -> types com info
//         | "Microsoft.FSharp.Control.FSharpMailboxProcessor"
//         | "Microsoft.FSharp.Control.FSharpAsyncReplyChannel" -> mailbox com info
//         | "Microsoft.FSharp.Control.FSharpAsync" -> asyncs com info
//         | "System.Guid" -> guids com info
//         | "System.Uri" -> uris com info
//         | "System.Lazy" | "Microsoft.FSharp.Control.Lazy"
//         | "Microsoft.FSharp.Control.LazyExtensions" -> laziness com info
//         | "Microsoft.FSharp.Control.CommonExtensions" -> controlExtensions com info
//         | "Microsoft.FSharp.Reflection.FSharpType" -> fsharpType com info info.memberName
//         | "Microsoft.FSharp.Reflection.FSharpValue" -> fsharpValue com info info.memberName
//         | "Microsoft.FSharp.Reflection.FSharpReflectionExtensions" ->
//             // In netcore F# Reflection methods become extensions
//             // with names like `FSharpType.GetExceptionFields.Static`
//             let isFSharpType = info.memberName.StartsWith("fSharpType")
//             let methName = info.memberName |> Naming.extensionMethodName |> Naming.lowerFirst
//             if isFSharpType
//             then fsharpType com info methName
//             else fsharpValue com info methName
//         | "Microsoft.FSharp.Reflection.UnionCaseInfo"
//         | "System.Reflection.PropertyInfo"
//         | "System.Reflection.MemberInfo" ->
//             match info.thisArg, info.memberName with
//             | _, "getFields" -> icall info "getUnionFields" |> Some
//             | Some c, "name" -> ccall info "Reflection" "getName" [c] |> Some
//             | Some c, ("tag" | "propertyType") ->
//                 let prop =
//                     if info.memberName = "tag" then "index" else info.memberName
//                     |> StringConstant |> Value
//                 makeGet info.range info.returnType c prop |> Some
//             | _ -> None
//         | _ -> None
