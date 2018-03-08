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
                Map.tryFind name genArgs |> Option.defaultValue t
            | t -> t)

    let coreCall r t (info: CallInfo) coreModule coreMember thisArg args =
        let info = argInfo thisArg args (Some info.ArgTypes)
        let funcExpr = Import(coreMember, coreModule, CoreLib, Any)
        Operation(Call(StaticCall funcExpr, info), t, r)

    let coreCall_ t coreModule coreMember args =
        let funcExpr = Import(coreMember, coreModule, CoreLib, Any)
        Operation(Call(StaticCall funcExpr, argInfo None args None), t, None)

    let globalCall r t info ident memb thisArg args =
        let funcExpr =
            match memb with
            | Some m -> get None Any (makeIdentExpr ident) m
            | None -> makeIdentExpr ident
        let info = argInfo thisArg args (Some info.ArgTypes)
        Operation(Call(StaticCall funcExpr, info), t, r)

    let globalCall_ t ident memb args =
        let funcExpr =
            match memb with
            | Some m -> get None Any (makeIdentExpr ident) m
            | None -> makeIdentExpr ident
        Operation(Call(StaticCall funcExpr, argInfo None args None), t, None)

    let instanceCall_ t callee memb args =
        let kind = makeStrConst memb |> Some |> InstanceCall
        Operation(Call(kind, argInfo (Some callee) args None), t, None)

    let emitJs_ t macro args =
        let info = argInfo None args None
        Operation(Emit(macro, Some info), t, None)

    let add left right =
        Operation(BinaryOperation(BinaryPlus, left, right), left.Type, None)

    let eq left right =
        Operation(BinaryOperation(BinaryEqualStrict, left, right), Boolean, None)

    let neq left right =
        Operation(BinaryOperation(BinaryUnequalStrict, left, right), Boolean, None)

    let isNull expr =
        Operation(BinaryOperation(BinaryEqual, expr, Value(Null Any)), Boolean, None)

    let error msg =
        constructorCall_ None Any (makeIdentExpr "Error") [msg]

    let s txt = Value(StringConstant txt)

    let genArg (com: ICompiler) r (name: string) (genArgs: Map<string,Type>) =
        match Map.tryFind name genArgs with
        | Some t -> t
        | None ->
            "Couldn't find generic " + name |> addError com r
            Any

    let singleGenArg (com: ICompiler) r (genArgs: Map<string,Type>) =
        Map.toSeq genArgs |> Seq.tryHead
        |> Option.map snd
        |> Option.defaultWith (fun () ->
            "Couldn't find any generic argument" |> addError com r
            Any)

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
        | Number Int8 -> emitJs_ targetType "($0 + 0x80 & 0xFF) - 0x80" args
        | Number Int16 -> emitJs_ targetType "($0 + 0x8000 & 0xFFFF) - 0x8000" args
        | Number Int32 -> fastIntFloor args.Head
        | Number UInt8 -> emitJs_ targetType "$0 & 0xFF" args
        | Number UInt16 -> emitJs_ targetType "$0 & 0xFFFF" args
        | Number UInt32 -> emitJs_ targetType "$0 >>> 0" args
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
let toList t expr =
    coreCall_ t "Seq" "toList" [expr]

let toArray (com: Fable.ICompiler) t expr =
    match expr, t with
    // Typed arrays
    | _, Fable.Array(Fable.Number numberKind) when com.Options.typedArrays ->
        globalCall_ t (getTypedArrayName com numberKind) (Some "from") [expr]
    | _ -> globalCall_ t "Array" (Some "from") [expr]

let getZero = function
    | Fable.Char | Fable.String -> makeStrConst ""
    | Builtin BclTimeSpan -> makeIntConst 0
    | Builtin BclDateTime as t -> coreCall_ t "Date" "minValue" []
    | Builtin BclDateTimeOffset as t -> coreCall_ t "DateOffset" "minValue" []
    | Builtin FSharpSet as t -> coreCall_ t "Set" "create" []
    | Builtin (BclInt64|BclUInt64) as t -> coreCall_ t "Long" "fromInt" [makeIntConst 0]
    | Builtin BclBigInt as t -> coreCall_ t "BigInt" "fromInt32" [makeIntConst 0]
    // TODO: Calls to custom Zero implementation
    | _ -> makeIntConst 0

let applyOp (com: ICompiler) ctx r t (i: CallInfo) (args: Expr list) =
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
    let argTypes = resolveArgTypes i.ArgTypes i.GenericArgs
    match argTypes with
    | Builtin(BclInt64|BclUInt64|BclBigInt|BclDateTime|BclDateTimeOffset|FSharpSet as bt)::_ ->
        coreCall r t i (coreModFor bt) i.CompiledName None args
    | Builtin BclTimeSpan::_ ->
        nativeOp i.CompiledName argTypes args
    | CustomOp com i.CompiledName m ->
        let genArgs = i.GenericArgs |> Seq.map (fun kv -> kv.Value)
        FSharp2Fable.Util.makeCallFrom com ctx r t genArgs None args m
    | _ -> nativeOp i.CompiledName argTypes args

let equality r opName left right =
    let op =
        match opName with
        | "op_Equality" -> BinaryEqualStrict
        | "op_Inequality" -> BinaryUnequalStrict
        | "op_LessThan" -> BinaryLess
        | "op_GreaterThan" -> BinaryGreater
        | "op_LessThanOrEqual" -> BinaryLessOrEqual
        | "op_GreaterThanOrEqual" -> BinaryGreaterOrEqual
        | _ -> failwith ("Unknown equality operator: " + opName)
    makeBinOp r Boolean left right op

//
let equals r equal (args: Expr list) =
    let op equal =
        if equal then BinaryEqualStrict else BinaryUnequalStrict
    let is equal expr =
        if equal
        then expr
        else makeUnOp None Boolean expr UnaryNot
    let icall (args: Expr list) equal =
        instanceCall_ Boolean args.Head "Equals" args.Tail |> is equal
    match args with
    | [ExprType(Builtin(BclGuid|BclTimeSpan)) as left; right]
    | [ExprType(Boolean | Char | String | Number _ | EnumType _) as left; right] ->
        op equal |> makeBinOp r Boolean left right

    | ExprType(Builtin(BclDateTime|BclDateTimeOffset))::_ ->
        coreCall_ Boolean "Date" "equals" args |> is equal

    | ExprType(Builtin(FSharpSet|FSharpMap)) as callee::args ->
        instanceCall_ Boolean callee "Equals" args |> is equal

    | ExprType(Builtin(BclInt64|BclUInt64|BclBigInt as bt))::_ ->
        coreCall_ Boolean (coreModFor bt) "equals" args

    | ExprType(Array _ | List _ | Tuple _)::_ ->
        coreCall_ Boolean "Util" "equalArrays" args
    | ExprType(DeclaredType(ent,_))::_ when ent.IsFSharpUnion ->
        coreCall_ Boolean "Util" "equalArrays" args

    | ExprType(DeclaredType(ent,_))::_ when ent.IsFSharpRecord ->
        coreCall_ Boolean "Util" "equalObjects" args

    | _ -> coreCall_ Boolean "Util" "equals" args

/// Compare function that will call Util.compare or instance `CompareTo` as appropriate
/// If passed an optional binary operator, it will wrap the comparison like `comparison < 0`
let compare com r (args: Expr list) op =
    let wrapWith op comparison =
        match op with
        | None -> comparison
        | Some op -> makeEqOp r comparison (makeIntConst 0) op
    let icall callee args op =
        instanceCall_ (Number Int32) callee "CompareTo" args |> wrapWith op
//     let compareReplacedEntities =
//         set ["System.Guid"; "System.TimeSpan"; "System.DateTime"; "System.DateTimeOffset"]

    match args with
    | [ExprType(Builtin(BclGuid|BclTimeSpan)) as left; right]
    | [ExprType(Boolean | Char | String | Number _ | EnumType _) as left; right] ->
        match op with
        | Some op -> makeEqOp r left right op
        | None -> coreCall_ (Number Int32) "Util" "comparePrimitives" args

    | ExprType(Builtin(BclDateTime|BclDateTimeOffset))::_ ->
        coreCall_ (Number Int32) "Date" "compare" args |> wrapWith op

    | ExprType(Builtin(BclInt64|BclUInt64|BclBigInt as bt))::_ ->
        coreCall_ Boolean (coreModFor bt) "compare" args

    | ExprType(Array _ | List _ | Tuple _)::_ ->
        coreCall_ Boolean "Util" "compareArrays" args
    | ExprType(DeclaredType(ent,_))::_ when ent.IsFSharpUnion ->
        coreCall_ Boolean "Util" "compareArrays" args

    | ExprType(DeclaredType(ent,_))::_ when ent.IsFSharpRecord ->
        coreCall_ Boolean "Util" "compareObjects" args

    | _ -> coreCall_ (Number Int32) "Util" "compare" args

//     let makeComparer (typArg: Fable.Type option) =
//         let f =
//             match typArg with
//             | Some(EntFullName "System.Guid")
//             | Some(EntFullName "System.TimeSpan")
//             | Some(Fable.Boolean | Fable.Char | Fable.String | Fable.Number _ | Fable.Enum _) ->
//                 makeCoreRef "Util" "comparePrimitives"
//             | Some(EntFullName ("System.DateTime" | "System.DateTimeOffset")) ->
//                 emitNoInfo "(x,y) => x = x.getTime(), y = y.getTime(), x === y ? 0 : (x < y ? -1 : 1)" []
//             | Some(Fable.ExtendedNumber (Int64|UInt64|BigInt)) ->
//                 emitNoInfo "(x,y) => x.CompareTo(y)" []
//             | Some(Fable.DeclaredType(ent, _))
//                 // TODO: when ent.HasInterface "System.IComparable"
//                 ->
//                 emitNoInfo "(x,y) => x.CompareTo(y)" []
//             | Some _ | None ->
//                 makeCoreRef "Util" "compare"
//         CoreLibCall("Comparer", None, true, [f])
//         |> makeCall None Fable.Any

let operators (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let curriedApply r t applied args =
        Operation(CurriedApply(applied, args), t, r)

    let compose (com: ICompiler) r t f1 f2 =
        let argType, retType =
            match t with
            | FunctionType(LambdaType argType, retType) -> argType, retType
            | _ -> Any, Any
        let tempVar = com.GetUniqueVar() |> makeTypedIdent argType
        let body =
            [IdentExpr tempVar]
            |> curriedApply None Any f1
            |> List.singleton
            |> curriedApply r retType f2
        Function(Lambda tempVar, body)

    let math r t (i: CallInfo) (args: Expr list) methName =
        let argTypes = resolveArgTypes i.ArgTypes i.GenericArgs
        match methName, List.head argTypes with
        | "Abs", Builtin(BclInt64 | BclBigInt as bt)  ->
            coreCall r t i (coreModFor bt) "abs" thisArg args |> Some
         | methName, _ ->
            let methName = Naming.lowerFirst methName |> Some
            globalCall r t i "Math" methName thisArg args |> Some

    match i.CompiledName, args with
    // Erased operators
    | ("CreateSequence"|"Identity"|"Box"|"Unbox"), _ -> List.tryHead args
    // Make sure `void 0` is returned in case `ignore` is wrapped in a lambda, see #1360
    | "Ignore", [arg]  -> [arg; Value UnitConstant] |> Sequential |> Some
    // TODO: Number and String conversions
    | ("ToSByte"|"ToByte"|"ToInt8"|"ToUInt8"|"ToInt16"|"ToUInt16"|"ToInt"|"ToUInt"|"ToInt32"|"ToUInt32"|"ToInt64"|"ToUInt64"), _ ->
        let sourceType = singleGenArg com r i.GenericArgs
        toInt false sourceType t args |> Some
    | ("ToSingle"|"ToDouble"|"ToDecimal"), _ ->
        let sourceType = singleGenArg com r i.GenericArgs
        toFloat sourceType t args |> Some
    | "ToChar", _ -> toChar (singleGenArg com r i.GenericArgs) args |> Some
    | "ToString", _ -> toString (singleGenArg com r i.GenericArgs) args |> Some
    | "ToEnum", _ -> args.Head |> Some

    // TODO

    // Pipes and composition
    | "op_PipeRight", [x; f]
    | "op_PipeLeft", [f; x] -> curriedApply r t f [x] |> Some
    | "op_PipeRight2", [x; y; f]
    | "op_PipeLeft2", [f; x; y] -> curriedApply r t f [x; y] |> Some
    | "op_PipeRight3", [x; y; z; f]
    | "op_PipeLeft3", [f; x; y; z] -> curriedApply r t f [x; y; z] |> Some
    | "op_ComposeRight", [f1; f2] -> compose com r t f1 f2 |> Some
    | "op_ComposeLeft", [f2; f1] -> compose com r t f1 f2 |> Some
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
    | "Pow", _ | "PowInteger", _ | "op_Exponentiation", _ -> math r t i args "pow"
    | "Ceil", _ | "Ceiling", _ -> math r t i args "ceil"
    | "Abs", _ | "Acos", _ | "Asin", _ | "Atan", _ | "Atan2", _
    | "Cos", _ | "Exp", _ | "Floor", _ | "Log", _ | "Log10", _
    | "Sin", _ | "Sqrt", _ | "Tan", _ -> math r t i args i.CompiledName
    | "Round", _ -> coreCall r t i "Util" "round" thisArg args |> Some
    // | "sign" ->
    //     let args =
    //         match args with
    //         | ((Type (ExtendedNumber _ as t)) as arg)::_ ->
    //             toFloat arg.Range t (Number Float64) [arg] |> List.singleton
    //         | _ -> args
    //     coreCall r t i "Util" "sign" args |> Some
    | "Fst", [Value(NewTuple(fst::_))] -> Some fst
    | "Fst", [tup] -> Get(tup, TupleGet 0, t, r) |> Some
    | "Snd", [Value(NewTuple(_::snd::_))] -> Some snd
    | "Snd", [tup] -> Get(tup, TupleGet 1, t, r) |> Some
    // Reference
    | "op_Dereference", [arg] -> get r t arg "contents" |> Some
    | "op_ColonEquals", [o; v] -> Set(o, makeStrConst "contents" |> ExprSet, v, r) |> Some
    | "Ref", [arg] -> ObjectExpr(["contents", arg, ObjectValue], t, None) |> Some
    | "Not", [operand] -> // TODO: Check custom operator?
        makeUnOp r t operand UnaryNot |> Some
    // Concatenates two lists
    | "op_Append", _ -> coreCall r t i "List" "Append" thisArg args |> Some
    // TODO: Structural equality
    | SetContains Operators.equalitySet as opName, [left; right] ->
        equality r opName left right |> Some
    | SetContains Operators.standardSet, _ ->
        applyOp com ctx r t i args |> Some
    | _ -> None

let seqs (_: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | meth -> coreCall r t i "Seq" (Naming.lowerFirst meth) thisArg args |> Some
    // | _ -> None

let arrays (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, args, i.CompiledName with
    | Some c, _, "get_Length" -> get r t c "length" |> Some
    | None, [arg], "Length" -> get r t arg "length" |> Some
    | None, [ar; idx], "Get" -> getExpr r t ar idx |> Some
    | None, [ar; idx; value], "Set" -> Set(ar, ExprSet idx, value, r) |> Some
    // dynamicSet r ar idx value |> Some
    | None, _, meth ->
        let arrayCons =
            match t with
            | Array(Number numberKind) when com.Options.typedArrays ->
                getTypedArrayName com numberKind |> makeIdentExpr
            | _ -> makeIdentExpr "Array"
        // TODO: Only append array constructor when needed
        coreCall r t i "Array" (Naming.lowerFirst meth) thisArg (args@[arrayCons]) |> Some
    | _ -> None

let lists (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, args, i.CompiledName with
    | None, [x], "Head"
    | Some x, _, "get_Head" -> Get(x, ListHead, t, r) |> Some
    | None, [x], "Tail"
    | Some x, _, "get_Tail" -> Get(x, ListTail, t, r) |> Some
    | _, _, "get_Length" -> coreCall r t i "List" "Length" thisArg args |> Some
    | Some x, _, "get_Item" -> coreCall r t i "List" "Item" None (args@[x]) |> Some
    | Some x, _, "get_IsEmpty" -> Test(x, ListTest false, r) |> Some
    | None, _, ("get_Empty" | "Empty") ->
        NewList(None, singleGenArg com r i.GenericArgs) |> Value |> Some
    | None, [h;t], "Cons" -> NewList(Some(h,t), singleGenArg com r i.GenericArgs) |> Value |> Some
    | None, _, "Map" -> coreCall r t i "List" "_Map" None args |> Some // "Map" method is mangled
    | None, _, meth -> coreCall r t i "List" meth None args |> Some
    | _ -> None

let options (_: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (_args: Expr list) =
    match thisArg, i.CompiledName with
    | Some c, "get_Value" -> Get(c, OptionValue, t, r) |> Some
    | _ -> None

let decimals (com: ICompiler) (_: Context) r (_: Type) (i: CallInfo) (_callee: Expr option) (args: Expr list) =
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

let languagePrimitives (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    // TODO: Check for types with custom zero/one (strings?)
    | "GenericZero" -> NumberConstant(0., Int32) |> Value |> Some
    | "GenericOne" -> NumberConstant(1., Int32) |> Value |> Some
    | _ -> None

let intrinsicFunctions (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    // Erased operators
    | "CheckThis", [arg]
    | "UnboxFast", [arg]
    | "UnboxGeneric", [arg] -> Some arg
    | "MakeDecimal", _ -> decimals com ctx r t i thisArg args
    | "GetString", [ar; idx]
    | "GetArray", [ar; idx] -> getExpr r t ar idx |> Some
    | "SetArray", [ar; idx; value] -> Set(ar, ExprSet idx, value, r) |> Some
    | _ -> None
//         match i.memberName, (i.thisArg, i.args) with
//         | ("getArraySlice" | "getStringSlice"), ThreeArgs (ar, lower, upper) ->
//             let upper =
//                 let t = Number Int32
//                 match upper with
//                 | Null _ -> makeGet None t ar (makeStrConst "length")
//                 | _ -> Apply(Value(BinaryOp BinaryPlus),
//                                 [upper; makeIntConst 1], ApplyMeth, t, None)
//             InstanceCall (ar, "slice", [lower; upper])
//             |> makeCall i.range i.returnType |> Some
//         | "setArraySlice", (None, args) ->
//             CoreLibCall("Array", Some "setSlice", false, args)
//             |> makeCall i.range i.returnType |> Some
//         | "typeTestGeneric", (None, [expr]) ->
//             makeTypeTest com i.fileName i.range expr i.memberGenArgs.Head |> Some
//         | "createInstance", (None, _) ->
//             None // TODO
//             // let typRef, args = resolveTypeRef com i false i.memberGenArgs.Head, []
//             // Apply (typRef, args, ApplyCons, i.returnType, i.range) |> Some
//         | "rangeInt32", (None, args) ->
//             CoreLibCall("Seq", Some "rangeStep", false, args)
//             |> makeCall i.range i.returnType |> Some
//         // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.powdouble-function-%5bfsharp%5d
//         // Type: PowDouble : float -> int -> float
//         // Usage: PowDouble x n
//         | "powDouble", (None, _) ->
//             GlobalCall ("Math", Some "pow", false, i.args)
//             |> makeCall i.range i.returnType
//             |> Some
//         // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangechar-function-%5bfsharp%5d
//         // Type: RangeChar : char -> char -> seq<char>
//         // Usage: RangeChar start stop
//         | "rangeChar", (None, _) ->
//             CoreLibCall("Seq", Some "rangeChar", false, i.args)
//             |> makeCall i.range i.returnType |> Some
//         // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangedouble-function-%5bfsharp%5d
//         // Type: RangeDouble: float -> float -> float -> seq<float>
//         // Usage: RangeDouble start step stop
//         | "rangeDouble", (None, _) ->
//             CoreLibCall("Seq", Some "rangeStep", false, i.args)
//             |> makeCall i.range i.returnType |> Some
//         | _ -> None

let fableCoreLib (_: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    // Dynamic casting, erase
    | "op_BangBang", _ | "op_BangHat", _ -> List.tryHead args
    | "op_Dynamic", [left; memb] -> getExpr r t left memb |> Some
    | "AreEqual", _ -> coreCall r t i "Util" "assertEqual" thisArg args |> Some
    | _ -> None

let exceptions (_: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> constructorCall_ r t (makeIdentExpr "Error") args |> Some
    | "get_Message", Some e -> get r t e "message" |> Some
    | "get_StackTrace", Some e -> get r t e "stack" |> Some
    | _ -> None

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
    | "Microsoft.FSharp.Core.Operators.OperatorIntrinsics" -> intrinsicFunctions com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings" -> errorStrings info.CompiledName
    | "Microsoft.FSharp.Core.LanguagePrimitives" -> languagePrimitives com ctx r t info thisArg args
    | "System.Array"
    | "Microsoft.FSharp.Collections.ArrayModule" -> arrays com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.FSharpList`1"
    | "Microsoft.FSharp.Collections.ListModule" -> lists com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.SeqModule" -> seqs com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.FSharpOption`1"
    | "Microsoft.FSharp.Core.OptionModule" -> options com ctx r t info thisArg args
    | "System.Decimal" -> decimals com ctx r t info thisArg args
    // | "System.Numerics.BigInteger"
    // | "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI" -> bigint com r t info thisArg args
    | Naming.StartsWith "Fable.Core." _ -> fableCoreLib com r t info thisArg args
    | Naming.EndsWith "Exception" _ -> exceptions com r t info thisArg args
    | _ -> None
//         | "System.Object" -> objects com info
//         | "System.Timers.ElapsedEventArgs" -> info.thisArg // only signalTime is available here
//         | "System.Char" -> chars com info
//         | "System.Enum" -> enums com info
//         | "System.String"
//         | "Microsoft.FSharp.Core.StringModule" -> strings com info
//         | "Microsoft.FSharp.Core.PrintfModule"
//         | "Microsoft.FSharp.Core.PrintfFormat" -> fsFormat com info
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
//         | "Microsoft.FSharp.Core.FSharpRef" -> references com info
//         | "System.Activator" -> activator com info
//         | "System.Text.RegularExpressions.Capture"
//         | "System.Text.RegularExpressions.Match"
//         | "System.Text.RegularExpressions.Group"
//         | "System.Text.RegularExpressions.MatchCollection"
//         | "System.Text.RegularExpressions.GroupCollection"
//         | "System.Text.RegularExpressions.Regex" -> regex com info
//         | "System.Collections.Generic.IEnumerable"
//         | "System.Collections.IEnumerable" -> enumerable com info
//         | "System.Collections.Generic.Dictionary"
//         | "System.Collections.Generic.IDictionary" -> dictionaries com info
//         | "System.Collections.Generic.HashSet"
//         | "System.Collections.Generic.ISet" -> hashSets com info
//         | "System.Collections.Generic.KeyValuePair" -> keyValuePairs com info
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
//         | "Microsoft.FSharp.Core.Operators.Unchecked" -> unchecked com info
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
//                     |> Fable.StringConst |> Fable.Value
//                 makeGet info.range info.returnType c prop |> Some
//             | _ -> None
//         | _ -> None
