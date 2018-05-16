module Fable.Transforms.Replacements

open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Core

type Context = Fable.Transforms.FSharp2Fable.Context
type ICompiler = Fable.Transforms.FSharp2Fable.IFableCompiler

type Helper =
    static member ConstructorCall(consExpr: Expr, returnType: Type, args: Expr list,
                                  ?argTypes: Type list, ?loc: SourceLocation) =
        Operation(Call(ConstructorCall consExpr, argInfo None args argTypes), returnType, loc)

    static member InstanceCall(callee: Expr, memb: string, returnType: Type, args: Expr list,
                               ?argTypes: Type list, ?loc: SourceLocation) =
        let kind = makeStrConst memb |> Some |> InstanceCall
        Operation(Call(kind, argInfo (Some callee) args argTypes), returnType, loc)

    static member Application(callee: Expr, returnType: Type, args: Expr list,
                               ?argTypes: Type list, ?loc: SourceLocation) =
        Operation(Call(InstanceCall None, argInfo (Some callee) args argTypes), returnType, loc)

    static member CoreCall(coreModule: string, coreMember: string, returnType: Type, args: Expr list,
                           ?argTypes: Type list, ?thisArg: Expr, ?isConstructor: bool,
                           ?hasSpread: bool, ?loc: SourceLocation) =
        let info =
            { ThisArg = thisArg
              Args = args
              SignatureArgTypes = argTypes
              Spread = match hasSpread with Some true -> SeqSpread | _ -> NoSpread
              IsSiblingConstructorCall = false }
        let funcExpr = Import(coreMember, coreModule, CoreLib, Any)
        match isConstructor with
        | Some true -> Operation(Call(ConstructorCall funcExpr, info), returnType, loc)
        | _ -> Operation(Call(StaticCall funcExpr, info), returnType, loc)

    static member GlobalCall(ident: string, returnType: Type, args: Expr list,
                             ?argTypes: Type list, ?memb: string, ?isConstructor: bool, ?loc: SourceLocation) =
        let funcExpr =
            match memb with
            | Some m -> get None Any (makeIdentExpr ident) m
            | None -> makeIdentExpr ident
        let op =
            match isConstructor with
            | Some true -> ConstructorCall funcExpr
            | _ -> StaticCall funcExpr
        let info = argInfo None args argTypes
        Operation(Call(op, info), returnType, loc)

    static member GlobalIdent(ident: string, memb: string, typ: Type, ?loc: SourceLocation) =
        get loc typ (makeIdentExpr ident) memb

module private Helpers =
    let resolveArgTypes argTypes genArgs =
        argTypes |> List.map (function
            | GenericParam name as t ->
                genArgs |> List.tryPick (fun (name2,t) ->
                    if name = name2 then Some t else None)
                |> Option.defaultValue t
            | t -> t)

    let emitJs r t args macro =
        let info = argInfo None args None
        Operation(Emit(macro, Some info), t, r)

    let objExpr t kvs =
        let kvs = List.map (fun (k,v) -> makeStrConst k, v, ObjectValue) kvs
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
        Helper.ConstructorCall(makeIdentExpr "Error", Any, [msg])

    let s txt = Value(StringConstant txt)

    let firstGenArg (com: ICompiler) r (genArgs: (string * Type) list) =
        List.tryHead genArgs
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
    | BclTimer
    | BclInt64
    | BclUInt64
    | BclBigInt
    | BclHashSet of Type
    | BclDictionary of key:Type * value:Type
    | FSharpSet of Type
    | FSharpMap of key:Type * value:Type

let (|Builtin|_|) = function
    | DeclaredType(ent, genArgs) ->
        // TODO: Convert this to dictionary
        match ent.TryFullName, genArgs with
        | Some Types.guid, _ -> Some BclGuid
        | Some Types.timespan, _ -> Some BclTimeSpan
        | Some Types.datetime, _ -> Some BclDateTime
        | Some Types.datetimeOffset, _ -> Some BclDateTimeOffset
        | Some "System.Timers.Timer", _ -> Some BclTimer
        | Some "System.Int64", _ -> Some BclInt64
        | Some "System.UInt64", _ -> Some BclUInt64
        | Some "Microsoft.FSharp.Core.int64`1", _ -> Some BclInt64
        | Some "System.Numerics.BigInteger", _ -> Some BclBigInt
        | Some Types.fsharpSet, [t] -> Some(FSharpSet(t))
        | Some Types.fsharpMap, [k;v] -> Some(FSharpMap(k,v))
        | Some Types.hashset, [t] -> Some(BclHashSet(t))
        | Some Types.dictionary, [k;v] -> Some(BclDictionary(k,v))
        | _ -> None
    | _ -> None

let (|Integer|Float|) = function
    | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 -> Integer
    | Float32 | Float64 | Decimal -> Float

let (|Nameof|_|) = function
    | IdentExpr ident -> Some ident.Name
    | Get(_, ExprGet(Value(StringConstant prop)), _, _) -> Some prop
    | Get(_, RecordGet(fi, _), _, _) -> Some fi.Name
    | _ -> None

let (|ReplaceName|_|) (namesAndReplacements: (string*string) list) name =
    namesAndReplacements |> List.tryPick (fun (name2, replacement) ->
        if name2 = name then Some replacement else None)

let inline (|ExprType|) (e: Expr) = e.Type

let (|OrDefault|) (def:'T) = function
    | Some v -> v
    | None -> def

let (|EntFullName|_|) (typ: Type) =
    match typ with
    | DeclaredType(ent, _) -> Some ent.FullName
    | _ -> None

let (|ListLiteral|_|) e =
    let rec untail t acc = function
        | Value(NewList(None, _)) -> Some(List.rev acc, t)
        | Value(NewList(Some(head, tail), _)) -> untail t (head::acc) tail
        | _ -> None
    match e with
    | Value(NewList(None, t)) -> Some([], t)
    | Value(NewList(Some(head, tail), t)) -> untail t [head] tail
    | _ -> None

let (|ArrayOrList|_|) = function
    | Array t -> Some("Array", t)
    | List t -> Some("List", t)
    | _ -> None

/// Try to uncurry lambdas at compile time in dynamic assignments
let (|MaybeLambdaUncurriedAtCompileTime|) = function
    | LambdaUncurriedAtCompileTime None lambda -> lambda
    | Cast(LambdaUncurriedAtCompileTime None lambda, _) -> lambda
    | e -> e

let coreModFor = function
    | BclGuid -> "String"
    | BclDateTime -> "Date"
    | BclDateTimeOffset -> "DateOffset"
    | BclTimer -> "Timer"
    | BclInt64 -> "Long"
    | BclUInt64 -> "Long"
    | BclBigInt -> "BigInt"
    | BclTimeSpan -> "Int32"
    | FSharpSet _ -> "Set"
    | FSharpMap _ -> "Map"
    | BclHashSet _
    | BclDictionary _ -> failwith "Cannot decide core module"

let defaultof (t: Type) =
    match t with
    | Number _ -> makeIntConst 0
    | Boolean -> makeBoolConst false
    | _ -> Null t |> Value

let makeLongInt t signed (x: uint64) =
    let lowBits = NumberConstant (float (uint32 x), Float64)
    let highBits = NumberConstant (float (x >>> 32), Float64)
    let unsigned = BoolConstant (not signed)
    let args = [Value lowBits; Value highBits; Value unsigned]
    Helper.CoreCall("Long", "fromBits", t, args)

let makeFloat32 (x: float32) =
    Helper.GlobalCall("Math", Number Float32, [NumberConstant (float x, Float32) |> Value], memb="fround")

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
    | EnumType(_, name), (:? byte as x) -> Enum(NumberEnum(makeIntConst(int x)), name) |> Value
    | EnumType(_, name), (:? sbyte as x) -> Enum(NumberEnum(makeIntConst(int x)), name) |> Value
    | EnumType(_, name), (:? int16 as x) -> Enum(NumberEnum(makeIntConst(int x)), name) |> Value
    | EnumType(_, name), (:? uint16 as x) -> Enum(NumberEnum(makeIntConst(int x)), name) |> Value
    | EnumType(_, name), (:? int as x) -> Enum(NumberEnum(makeIntConst x), name) |> Value
    | EnumType(_, name), (:? uint32 as x) -> Enum(NumberEnum(makeIntConst(int x)), name) |> Value
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
    | _ -> failwithf "Unexpected type %A for literal %O (%s)" typ value (value.GetType().FullName)

let createAtom (value: Expr) =
    let typ = value.Type
    Helper.CoreCall("Util", "createAtom", typ, [value], [typ])

let toChar (sourceType: Type) (args: Expr list) =
    match sourceType with
    | Char
    | String -> args.Head
    | _ -> Helper.GlobalCall("String", Char, args, memb="fromCharCode")

let toString (sourceType: Type) (args: Expr list) =
    match sourceType with
    | Char | String -> args.Head
    | Unit | Boolean | Array _ | Tuple _ | FunctionType _ | EnumType _ ->
        Helper.GlobalCall("String", String, args)
    | Builtin (BclInt64 | BclUInt64) -> Helper.CoreCall("Long", "toString", String, args)
    | Number Int16 -> Helper.CoreCall("Util", "int16ToString", String, args)
    | Number Int32 -> Helper.CoreCall("Util", "int32ToString", String, args)
    | Number _ -> Helper.InstanceCall(args.Head, "toString", String, args.Tail)
    | _ -> Helper.CoreCall("Util", "toString", String, args)

let toFloat (sourceType: Type) targetType (args: Expr list) =
    match sourceType with
    | String ->
        Helper.CoreCall("Double", "parse", Number Float64, args)
    | Builtin (BclInt64 | BclUInt64) ->
        Helper.CoreCall("Long", "toNumber", Number Float64, args)
    | Builtin BclBigInt ->
        let meth = match targetType with
                    | Number Float32 -> "toSingle"
                    | Number Float64 -> "toDouble"
                    | Number Decimal -> "toDecimal"
                    | _ -> failwith "Unexpected BigInt conversion"
        Helper.CoreCall("BigInt", meth, Number Float64, args)
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
        | Builtin (BclInt64|BclUInt64) -> Helper.CoreCall("Long", "fromValue", targetType, args)
        | _ -> Helper.CoreCall("Long", "fromNumber", targetType, args@[makeBoolConst unsigned])
    let emitBigInt (args: Expr list) =
        match sourceType with
        | Builtin (BclInt64|BclUInt64) -> Helper.CoreCall("BigInt", "fromInt64", targetType, args)
        | _ -> Helper.CoreCall("BigInt", "fromInt32", targetType, args)
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
    | Char -> Helper.InstanceCall(args.Head, "charCodeAt", targetType, [makeIntConst 0])
    | String ->
        match targetType with
        | Builtin (BclInt64|BclUInt64 as kind) ->
            let unsigned = kind = BclUInt64
            let args = [args.Head]@[makeBoolConst unsigned]@args.Tail
            Helper.CoreCall("Long", "fromString", targetType, args)
        | _ -> Helper.CoreCall("Int32", "parse", targetType, args)
    | Builtin BclBigInt ->
        let meth = castBigIntMethod targetType
        Helper.CoreCall("BigInt", meth, targetType, args)
    | Number _ | Builtin (BclInt64 | BclUInt64) as typeFrom ->
        match targetType with
        | typeTo when needToCast typeFrom typeTo ->
            match typeFrom, typeTo with
            | Builtin (BclUInt64|BclInt64), Number _ ->
                Helper.CoreCall("Long", "toNumber", targetType, args)
            | Number (Decimal|Float), (Number Integer | Builtin(BclInt64|BclUInt64)) when round ->
                Helper.CoreCall("Util", "round", targetType, args)
            | _, _ -> args.Head
            |> List.singleton
            |> emitCast typeTo
        | Builtin (BclUInt64|BclInt64 as kind) ->
            emitLong (kind = BclUInt64) [args.Head]
        | Number _ -> args.Head
        | _ -> args.Head
    | _ -> args.Head

let arrayCons (com: ICompiler) genArg =
    match genArg with
    | Number numberKind when com.Options.typedArrays ->
        getTypedArrayName com numberKind |> makeIdentExpr
    | _ -> makeIdentExpr "Array"

// TODO: Should we pass the empty list representation here?
let toList returnType expr =
    Helper.CoreCall("Seq", "toList", returnType, [expr])

let toArray (com: ICompiler) returnType expr =
    // match expr, returnType with
    // | _, Array(Number numberKind) when com.Options.typedArrays ->
    //     Helper.GlobalCall(getTypedArrayName com numberKind, returnType, [expr], memb="from")
    // | _ -> Helper.GlobalCall("Array", returnType, [expr], memb="from")

    // Calling the JS global methods (Array.from) directly creates problems with lambda optimization
    // because passing these functions as values in JS (e.g. `foo(Array.from)`) doesn't work
    let args =
        match returnType with
        | Array genArg
        // This is used also by Seq.cache, which returns `'T seq` instead of `'T array`
        | DeclaredType(_, [genArg]) -> [expr; arrayCons com genArg]
        | _ -> [expr]
    Helper.CoreCall("Array", "ofSeq", returnType, args)

let listToArray com r t (li: Expr) =
    match li with
    | ListLiteral(exprs, t) -> NewArray(ArrayValues exprs, t) |> Value
    | _ ->
        let args = match t with Array genArg -> [li; arrayCons com genArg] | _ -> [li]
        Helper.CoreCall("Array", "ofList", t, args, ?loc=r)

let listToSeq (li: Expr) =
    match li with
    // TODO: Use Any for Array type so we don't create typed arrays?
    | ListLiteral(exprs, t) -> NewArray(ArrayValues exprs, t) |> Value
    | _ -> li // Helper.CoreCall("List", "toSeq", t, [li])

// TODO: Use custom implementation?
// See https://github.com/fable-compiler/Fable/issues/1279#issuecomment-350122284
let stringToCharArray t e =
    Helper.InstanceCall(e, "split", t, [makeStrConst ""])

let enumerator2iterator (e: Expr) =
    Helper.CoreCall("Seq", "toIterator", e.Type, [e])

let toSeq t (e: Expr) =
    match e.Type with
    | List _ -> listToSeq e
    // Convert to array to get 16-bit code units, see #1279
    | String -> stringToCharArray t e
    // TODO: Add a runtime check for strings in case of generics?
    | _ -> e

let iterate r ident body xs =
    let f = Function(Delegate [ident], body, None)
    Helper.CoreCall("Seq", "iterate", Unit, [f; xs], ?loc=r)

let applyOp (com: ICompiler) (ctx: Context) r t opName (args: Expr list) argTypes genArgs =
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
    | Builtin(BclInt64|BclUInt64|BclBigInt|BclDateTime|BclDateTimeOffset as bt)::_ ->
        Helper.CoreCall(coreModFor bt, opName, t, args, argTypes, ?loc=r)
    | Builtin(FSharpSet _)::_ ->
        let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpSet" true opName
        Helper.CoreCall("Set", mangledName, t, args, argTypes, ?loc=r)
    | Builtin (FSharpMap _)::_ ->
        let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpMap" true opName
        Helper.CoreCall("Map", mangledName, t, args, argTypes, ?loc=r)
    | Builtin BclTimeSpan::_ ->
        nativeOp opName argTypes args
    | CustomOp com opName m ->
        let genArgs = genArgs |> Seq.map snd
        FSharp2Fable.Util.makeCallFrom com ctx r t genArgs None args m
    | _ -> nativeOp opName argTypes args

let isCompatibleWithJsComparison = function
    | Builtin(BclInt64|BclUInt64|BclBigInt)
    | Array _ | List _ | Tuple _ | Option _ -> false
    | Builtin(BclGuid|BclTimeSpan) -> true
    // TODO: Non-record/union declared types without custom equality
    // should be compatible with JS comparison
    | DeclaredType _ -> false
    // TODO: Raise warning when building dictionary/hashset with generic params?
    | GenericParam _ -> true
    | Any | Unit | Boolean | Number _ | String | Char | Regex
    | EnumType _ | ErasedUnion _ | FunctionType _ | Pojo _ -> true

let rec equals r equal left right =
    let is equal expr =
        if equal
        then expr
        else makeUnOp None Boolean expr UnaryNot

    match left with
    | ExprType(Builtin(BclGuid|BclTimeSpan))
    | ExprType(Boolean | Char | String | Number _ | EnumType _) ->
        let op = if equal then BinaryEqualStrict else BinaryUnequalStrict
        makeBinOp r Boolean left right op

    | ExprType(Builtin(BclDateTime|BclDateTimeOffset)) ->
        Helper.CoreCall("Date", "equals", Boolean, [left; right], ?loc=r) |> is equal

    | ExprType(Builtin(FSharpSet _|FSharpMap _)) ->
        Helper.InstanceCall(left, "Equals", Boolean, [right]) |> is equal

    | ExprType(Builtin(BclInt64|BclUInt64|BclBigInt as bt)) ->
        Helper.CoreCall(coreModFor bt, "equals", Boolean, [left; right], ?loc=r) |> is equal

    | ExprType(ArrayOrList(modName, t)) ->
        let f = makeComparerFunction t
        Helper.CoreCall(modName, "equalsWith", Boolean, [f; left; right], ?loc=r) |> is equal

    | ExprType(Tuple _) ->
        Helper.CoreCall("Util", "equalArrays", Boolean, [left; right], ?loc=r) |> is equal
    | ExprType(DeclaredType(ent,_)) when ent.IsFSharpUnion ->
        Helper.CoreCall("Util", "equalArrays", Boolean, [left; right], ?loc=r) |> is equal

    | ExprType(DeclaredType(ent,_)) when ent.IsFSharpRecord ->
        Helper.CoreCall("Util", "equalObjects", Boolean, [left; right], ?loc=r) |> is equal

    | _ -> Helper.CoreCall("Util", "equals", Boolean, [left; right], ?loc=r) |> is equal

/// Compare function that will call Util.compare or instance `CompareTo` as appropriate
and compare r left right =
    match left with
    | ExprType(Builtin(BclGuid|BclTimeSpan))
    | ExprType(Boolean | Char | String | Number _ | EnumType _) ->
        Helper.CoreCall("Util", "comparePrimitives", Number Int32, [left; right], ?loc=r)

    | ExprType(Builtin(BclDateTime|BclDateTimeOffset)) ->
        Helper.CoreCall("Date", "compare", Number Int32, [left; right], ?loc=r)

    | ExprType(Builtin(BclInt64|BclUInt64|BclBigInt as bt)) ->
        Helper.CoreCall(coreModFor bt, "compare", Number Int32, [left; right], ?loc=r)

    | ExprType(ArrayOrList(modName, t)) ->
        let f = makeComparerFunction t
        Helper.CoreCall(modName, "compareWith", Number Int32, [f; left; right], ?loc=r)

    | ExprType(Tuple _) ->
        Helper.CoreCall("Util", "compareArrays", Number Int32, [left; right], ?loc=r)
    | ExprType(DeclaredType(ent,_)) when ent.IsFSharpUnion ->
        Helper.CoreCall("Util", "compareArrays", Number Int32, [left; right], ?loc=r)

    // TODO: Create an ad-hoc comparison for the records?
    | ExprType(DeclaredType(ent,_)) when ent.IsFSharpRecord ->
        Helper.CoreCall("Util", "compareObjects", Number Int32, [left; right], ?loc=r)

    | ExprType(DeclaredType(ent,_)) when FSharp2Fable.Util.hasInterface Types.comparable ent ->
        Helper.InstanceCall(left, "CompareTo", Number Int32, [right], ?loc=r)

    | _ -> Helper.CoreCall("Util", "compare", Number Int32, [left; right], ?loc=r)

/// Wraps comparison with the binary operator, like `comparison < 0`
and compareIf r left op right =
    match left with
    | ExprType(Builtin(BclGuid|BclTimeSpan))
    | ExprType(Boolean | Char | String | Number _ | EnumType _) ->
        makeEqOp r left right op
    | _ ->
        let comparison = compare r left right
        makeEqOp r comparison (makeIntConst 0) op

and makeComparerFunction typArg =
    let x = makeTypedIdent typArg "x"
    let y = makeTypedIdent typArg "y"
    let body = compare None (IdentExpr x) (IdentExpr y)
    Function(Delegate [x; y], body, None)

and makeComparer typArg =
    let f = makeComparerFunction typArg
    // TODO: Use proper IComparer<'T> type instead of Any
    ObjectExpr([makeStrConst "Compare", f, ObjectValue], Any, None)

let inline makeComparerFromEqualityComparer e =
    Helper.CoreCall("Util", "comparerFromEqualityComparer", Any, [e])

/// Adds comparer as last argument for set creator methods
let makeSet r t methName args genArg =
    let args = args @ [makeComparer genArg]
    Helper.CoreCall("Set", Naming.lowerFirst methName, t, args, ?loc=r)

/// Adds comparer as last argument for map creator methods
let makeMap r t methName args genArg =
    let args = args @ [makeComparer genArg]
    Helper.CoreCall("Map", Naming.lowerFirst methName, t, args, ?loc=r)

let makeDictionaryWithComparer r t sourceSeq comparer =
    Helper.CoreCall("Map", "createMutable", t, [sourceSeq; comparer], ?loc=r)

let makeDictionary r t sourceSeq =
    match t with
    | DeclaredType(_,[key;_]) when not(isCompatibleWithJsComparison key) ->
        makeComparer key |> makeDictionaryWithComparer r t sourceSeq
    | _ -> Helper.GlobalCall("Map", t, [sourceSeq], isConstructor=true, ?loc=r)

let makeHashSetWithComparer r t sourceSeq comparer =
    Helper.CoreCall("Set", "createMutable", t, [sourceSeq; comparer], ?loc=r)

let makeHashSet r t sourceSeq =
    match t with
    | DeclaredType(_,[key;_]) when not(isCompatibleWithJsComparison key) ->
        makeComparer key |> makeHashSetWithComparer r t sourceSeq
    | _ -> Helper.GlobalCall("Set", t, [sourceSeq], isConstructor=true, ?loc=r)

let getZero = function
    | Char | String -> makeStrConst ""
    | Builtin BclTimeSpan -> makeIntConst 0
    | Builtin BclDateTime as t -> Helper.CoreCall("Date", "minValue", t, [])
    | Builtin BclDateTimeOffset as t -> Helper.CoreCall("DateOffset", "minValue", t, [])
    | Builtin (FSharpSet genArg) as t -> makeSet None t "Empty" [] genArg
    | Builtin (BclInt64|BclUInt64) as t -> Helper.CoreCall("Long", "fromInt", t, [makeIntConst 0])
    | Builtin BclBigInt as t -> Helper.CoreCall("BigInt", "fromInt32", t, [makeIntConst 0])
    // TODO: Calls to custom Zero implementation
    | _ -> makeIntConst 0

let getOne (t: Type) =
    match t with
    // TODO: Calls to custom Zero implementation
    | _ -> makeIntConst 1

let makePojoFromLambda arg =
    let rec flattenSequential = function
        | Sequential statements ->
            List.collect flattenSequential statements
        | e -> [e]
    match arg with
    | Function(Lambda _, lambdaBody, _) ->
        (flattenSequential lambdaBody, Some []) ||> List.foldBack (fun statement acc ->
            match acc, statement with
            | Some acc, Set(_, RecordSet(fi, _), value, _) ->
                (makeStrConst fi.Name, value, ObjectValue)::acc |> Some
            | Some acc, Set(_, ExprSet prop, value, _) ->
                (prop, value, ObjectValue)::acc |> Some
            | _ -> None)
    | _ -> None
    |> Option.map (fun members -> ObjectExpr(members, Any, None))
    |> Option.defaultWith (fun () -> Helper.CoreCall("Util", "jsOptions", Any, [arg]))

let changeCase caseRule name =
    match caseRule with
    | CaseRules.LowerFirst -> Naming.lowerFirst name
    | CaseRules.None | _ -> name

let makePojo caseRule keyValueList =
    match keyValueList with
    // It should be an array because the list is casted to seq, but check also list just in case
    | Value(NewArray(ArrayValues ms, _))
    | ListLiteral(ms, _) ->
        (ms, Some []) ||> List.foldBack (fun m acc ->
            match acc, m, caseRule with
            // For tuple literals, we can the member key and value at compile time (try to uncurry lambda values)
            | Some acc, Value(NewTuple [Value(StringConstant name); MaybeLambdaUncurriedAtCompileTime value]), _ ->
                (changeCase caseRule name |> makeStrConst, value, ObjectValue)::acc |> Some
            // If it's not a string literal, try to optimize only if caseRule is None,
            // because in other case we have to calculate it at runtime
            | Some acc, Value(NewTuple [name; MaybeLambdaUncurriedAtCompileTime value]), CaseRules.None ->
                (name, value, ObjectValue)::acc |> Some
            | _ -> None)
    | _ -> None
    |> Option.map (fun members -> ObjectExpr(members, Any, None))
    // With key & value for all members, build the POJO at compile time. If not, build it at runtime
    |> Option.defaultWith (fun () ->
        Helper.CoreCall("Util", "createObj", Any, [keyValueList; caseRule |> int |> makeIntConst]))

let injectArg com r moduleName methName (genArgs: (string*Type) list) args =
    let (|GenericArg|_|) genArgs genArgIndex =
        List.tryItem genArgIndex genArgs
    Map.tryFind moduleName Fable.Transforms.Inject.fableCoreModules
    |> Option.bind (Map.tryFind methName)
    |> function
        | Some(Types.comparer, GenericArg genArgs (_,genArg)) ->
            args @ [makeComparer genArg]
        | Some(Types.arrayCons, GenericArg genArgs (_,genArg)) ->
            args @ [arrayCons com genArg]
        | Some(_, genArgIndex) ->
            sprintf "Cannot inject arg to %s.%s (genArgs %A - expected index %i)"
                moduleName methName (List.map fst genArgs) genArgIndex
            |> addWarning com r
            args
        | None -> args

let fableCoreLib (com: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | ("Async.AwaitPromise.Static"|"Async.StartAsPromise.Static" as m), _ ->
        let meth =
            if m = "Async.AwaitPromise.Static"
            then "awaitPromise" else "startAsPromise"
        Helper.CoreCall("Async", meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | "importDynamic", _ -> Helper.GlobalCall("import", t, args, ?loc=r) |> Some
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
    | "op_DynamicAssignment", [callee; prop; MaybeLambdaUncurriedAtCompileTime value] ->
        Set(callee, ExprSet prop, value, r) |> Some
    | ("op_Dollar"|"createNew" as m), callee::args ->
        let argInfo = { argInfo None args None with Spread = TupleSpread }
        if m = "createNew"
        then constructorCall r t argInfo callee |> Some
        else staticCall r t argInfo callee |> Some
    | "op_EqualsEqualsGreater", _ ->
        NewTuple args |> Value |> Some
    | "createObj", [kvs] ->
        Cast(kvs, Pojo CaseRules.None) |> Some
     | "keyValueList", _ ->
        match args with
        | [Value(Enum(NumberEnum(Value(NumberConstant(rule, _))), _)); keyValueList] ->
            let caseRule: Fable.Core.CaseRules = enum (int rule)
            Cast(keyValueList, Pojo caseRule) |> Some
        | [caseRule; keyValueList] ->
            Helper.CoreCall("Util", "createObj", t, [keyValueList; caseRule], ?loc=r) |> Some
        | _ -> None
    | "jsOptions", [arg] ->
        makePojoFromLambda arg |> Some
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
        Helper.CoreCall("Util", "assertEqual", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | "jsNative", _ ->
        // TODO: Fail at compile time?
        addWarning com r "jsNative is being compiled without replacement, this will fail at runtime."
        let runtimeMsg =
            "A function supposed to be replaced by JS native code has been called, please check."
            |> StringConstant |> Value
        Throw(error runtimeMsg, t, r) |> Some
    | _ -> None

let references (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, [arg] -> objExpr t ["contents", arg] |> Some
    | "get_Value", Some callee, _ -> get r t callee "contents" |> Some
    | "set_Value", Some callee, [value] ->
        Set(callee, makeStrConst "contents" |> ExprSet, value, r) |> Some
    | _ -> None

let fsFormat (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "get_Value", Some callee, _ ->
        get None t callee "input" |> Some
    | "PrintFormatToStringThen", _, _ ->
        match args with
        | [_] -> Helper.CoreCall("String", "toText", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | [cont; fmt] -> Helper.InstanceCall(fmt, "cont", t, [cont]) |> Some
        | _ -> None
    | "PrintFormatToString", _, _ ->
        Helper.CoreCall("String", "toText", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "PrintFormatLine", _, _ ->
        Helper.CoreCall("String", "toConsole", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ("PrintFormatToTextWriter"|"PrintFormatLineToTextWriter"), _, _::args ->
        // addWarning com r "fprintfn will behave as printfn"
        Helper.CoreCall("String", "toConsole", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "PrintFormat", _, _ ->
        // addWarning com r "Printf will behave as printfn"
        Helper.CoreCall("String", "toConsole", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "PrintFormatThen", _, arg::callee::_ ->
        Helper.InstanceCall(callee, "cont", t, [arg]) |> Some
    | "PrintFormatToStringThenFail", _, _ ->
        Helper.CoreCall("String", "toFail", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | ".ctor", _, arg::_ ->
        Helper.CoreCall("String", "printf", t, [arg], i.SignatureArgTypes, ?loc=r) |> Some
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
        Helper.GlobalCall("Math", t, args, argTypes, memb=Naming.lowerFirst methName, ?loc=r)

    match i.CompiledName, args with
    | "DefaultArg", _ ->
        Helper.CoreCall("Option", "defaultArg", t, args, i.SignatureArgTypes, ?loc=r) |> Some
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
    | "CreateDictionary", [arg] -> makeDictionary r t arg |> Some
    | "CreateSet", _ -> firstGenArg com r i.GenericArgs |> makeSet r t "OfSeq" args |> Some
    // Ranges
    | ("op_Range"|"op_RangeStep"), _ ->
        let meth =
            match firstGenArg com r i.GenericArgs with
            | Char -> "rangeChar"
            | _ -> if i.CompiledName = "op_Range" then "range" else "rangeStep"
        Helper.CoreCall("Seq", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
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
        fsFormat com ctx r t i thisArg args
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
        math r t args i.SignatureArgTypes "pow" |> Some
    | "Ceil", _ | "Ceiling", _ ->
        math r t args i.SignatureArgTypes "ceil" |> Some
    | "Log", [arg1; arg2] ->
        // "Math.log($0) / Math.log($1)"
        let dividend = math None t [arg1] (List.take 1 i.SignatureArgTypes) "log"
        let divisor = math None t [arg2] (List.skip 1 i.SignatureArgTypes) "log"
        makeBinOp r t dividend divisor BinaryDivide |> Some
    | "Abs", _ ->
        match resolveArgTypes i.SignatureArgTypes i.GenericArgs with
        | Builtin(BclInt64 | BclBigInt as bt)::_  ->
            Helper.CoreCall(coreModFor bt, "abs", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
        | _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    | "Acos", _ | "Asin", _ | "Atan", _ | "Atan2", _
    | "Cos", _ | "Exp", _ | "Floor", _ | "Log", _ | "Log10", _
    | "Sin", _ | "Sqrt", _ | "Tan", _ ->
        math r t args i.SignatureArgTypes i.CompiledName |> Some
    | "Round", _ ->
        Helper.CoreCall("Util", "round", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | "Sign", _ ->
        let args =
            match args with
            | ((ExprType(Builtin(BclInt64|BclBigInt) as t)) as arg)::_ ->
                toFloat t (Number Float64) [arg] |> List.singleton
            | _ -> args
        Helper.CoreCall("Util", "sign", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // Numbers
    | ("Infinity"|"InfinitySingle"), _ ->
        Helper.GlobalIdent("Number", "POSITIVE_INFINITY", t, ?loc=r) |> Some
    | ("NaN"|"NaNSingle"), _ ->
        Helper.GlobalIdent("Number", "NaN", t, ?loc=r) |> Some
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
    | "op_Append", _ -> Helper.CoreCall("List", "append", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | ("op_Inequality"|"Neq"), [left; right] -> equals r false left right |> Some
    | ("op_Equality"|"Eq"), [left; right] -> equals r true left right |> Some
    | "IsNull", [arg] -> makeEqOp r arg (Null arg.Type |> Value) BinaryEqual |> Some
    | "Hash", _ -> Helper.CoreCall("Util", "hash", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    // Comparison
    | "Compare", [left; right] -> compare r left right |> Some
    | ("op_LessThan"|"Lt"), [left; right] -> compareIf r left BinaryLess right |> Some
    | ("op_LessThanOrEqual"|"Lte"), [left; right] -> compareIf r left BinaryLessOrEqual right |> Some
    | ("op_GreaterThan"|"Gt"), [left; right] -> compareIf r left BinaryGreater right |> Some
    | ("op_GreaterThanOrEqual"|"Gte"), [left; right] -> compareIf r left BinaryGreaterOrEqual right |> Some
    | ("Min"|"Max" as meth), _ ->
        let f = makeComparerFunction t
        Helper.CoreCall("Util", Naming.lowerFirst meth, t, f::args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Not", [operand] -> // TODO: Check custom operator?
        makeUnOp r t operand UnaryNot |> Some
    | Patterns.SetContains Operators.standardSet, _ ->
        applyOp com ctx r t i.CompiledName args i.SignatureArgTypes i.GenericArgs |> Some
    // // Type ref
    // | "typeOf" | "typeDefOf" ->
    //     None // TODO
    //     // info.memberGenArgs.Head
    //     // |> resolveTypeRef com info (info.memberName = "typeOf")
    //     // |> Some
    | _ -> None

let chars (com: ICompiler) (_: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    let icall r t args argTypes memb  =
        match args, argTypes with
        | thisArg::args, _::argTypes ->
            let info = argInfo (Some thisArg) args (Some argTypes)
            instanceCall r t info (makeStrConst memb |> Some) |> Some
        | _ -> None
    match i.CompiledName with
    | "ToUpper" -> icall r t args i.SignatureArgTypes "toLocaleUpperCase"
    | "ToUpperInvariant" -> icall r t args i.SignatureArgTypes "toUpperCase"
    | "ToLower" -> icall r t args i.SignatureArgTypes "toLocaleLowerCase"
    | "ToLowerInvariant" -> icall r t args i.SignatureArgTypes "toLowerCase"
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
            | args -> args, i.SignatureArgTypes
        Helper.CoreCall("Char", methName, t, args, argTypes, ?loc=r) |> Some
    | _ -> None

let strings (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
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
            fsFormat com ctx r t i thisArg args
    | "get_Length", Some c, _ -> get r t c "length" |> Some
    | "Length", None, [arg] -> get r t arg "length" |> Some
    | "Equals", Some x, [y]
    | "Equals", None, [x; y] ->
        makeEqOp r x y BinaryEqualStrict |> Some
    | "Equals", Some x, [y; kind]
    | "Equals", None, [x; y; kind] ->
        let left = Helper.CoreCall("String", "compare", Number Int32, [x; y; kind])
        makeEqOp r left (makeIntConst 0) BinaryEqualStrict |> Some
    | "Contains", Some c, arg::_ ->
        if (List.length args) > 1 then
            addWarning com r "String.Contains: second argument is ignored"
        let left = Helper.InstanceCall(c, "indexOf", Number Int32, [arg])
        makeEqOp r left (makeIntConst 0) BinaryGreaterOrEqual |> Some
    | "StartsWith", Some c, [_str] ->
        let left = Helper.InstanceCall(c, "indexOf", Number Int32, args)
        makeEqOp r left (makeIntConst 0) BinaryEqualStrict |> Some
    | "StartsWith", c, [_str; _comp] ->
        Helper.CoreCall("String", "startsWith", t, args, i.SignatureArgTypes, ?thisArg=c, ?loc=r) |> Some
    | ReplaceName [ "Substring",        "substr"
                    "ToUpper",          "toLocaleUpperCase"
                    "ToUpperInvariant", "toUpperCase"
                    "ToLower",          "toLocaleLowerCase"
                    "ToLowerInvariant", "toLowerCase" ] methName, Some c, args ->
        Helper.InstanceCall(c, methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "get_Chars", _, _ ->
        Helper.CoreCall("String", "getCharAtIndex", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | ("IndexOf" | "LastIndexOf"), Some c, _ ->
        match args with
        | [ExprType Char]
        | [ExprType String]
        | [ExprType Char; ExprType(Number Int32)]
        | [ExprType String; ExprType(Number Int32)] ->
            Helper.InstanceCall(c, Naming.lowerFirst i.CompiledName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ -> "The only extra argument accepted for String.IndexOf/LastIndexOf is startIndex."
               |> addErrorAndReturnNull com r |> Some
    | ("Trim" | "TrimStart" | "TrimEnd"), Some c, _ ->
        let side =
            match i.CompiledName with
            | "TrimStart" -> "start"
            | "TrimEnd" -> "end"
            | _ -> "both"
        Helper.CoreCall("String", "trim", t, (c::(makeStrConst side)::args), ?loc=r) |> Some
    | "ToCharArray", Some c, _ ->
        Helper.InstanceCall(c, "split", t, [makeStrConst ""]) |> Some
    | ("Iterate" | "IterateIndexed" | "ForAll" | "Exists"), _, _ ->
        // Cast the string to seq<char>, see #1279
        let args = args |> List.replaceLast (fun e -> stringToCharArray e.Type e)
        Helper.CoreCall("Seq", Naming.lowerFirst i.CompiledName, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | ("Map" | "MapIndexed" | "Collect"), _, _ ->
        // Cast the string to seq<char>, see #1279
        let args = args |> List.replaceLast (fun e -> stringToCharArray e.Type e)
        let name = Naming.lowerFirst i.CompiledName
        emitJs r t [Helper.CoreCall("Seq", name, Any, args)] "Array.from($0).join('')" |> Some
    | "Concat", _, _ ->
        let args =
            if i.DeclaringEntityFullName = "System.String"
            then (makeStrConst "")::args else args
        Helper.CoreCall("String", "join", t, args, ?loc=r) |> Some
    | "Split", Some c, _ ->
        match args with
        // Optimization
        | [] -> Helper.InstanceCall(c, "split", t, [makeStrConst ""]) |> Some
        | [Value(StringConstant _) as separator]
        | [Value(NewArray(ArrayValues [separator],_))] ->
            Helper.InstanceCall(c, "split", t, [separator]) |> Some
        | [arg1; ExprType(EnumType _) as arg2] ->
            let arg1 =
                match arg1.Type with
                | Array _ -> arg1
                | _ -> Value(NewArray(ArrayValues [arg1], String))
            let args = [arg1; Value(Null Any); arg2]
            Helper.CoreCall("String", "split", t, c::args, ?loc=r) |> Some
        | args ->
            Helper.CoreCall("String", "split", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    // Rest of StringModule methods
    | meth, thisArg, args ->
        let hasSpread = match i.Spread with SeqSpread -> true | _ -> false
        Helper.CoreCall("String", Naming.lowerFirst meth, t, args, i.SignatureArgTypes,
                        hasSpread=hasSpread, ?thisArg=thisArg, ?loc=r) |> Some
    | _ -> None

let getEnumerator r t expr =
    Helper.CoreCall("Seq", "getEnumerator", t, [toSeq Any expr], ?loc=r)

let seqs (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "Cast", [arg] -> Some arg // Erase
    | ("Cache"|"ToArray"), [arg] -> toArray com t arg |> Some
    | "OfList", [arg] -> Cast(arg, t) |> Some
    | "ToList", _ -> Helper.CoreCall("List", "ofSeq", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "ChunkBySize" | "Permute" as meth, [arg1; arg2] ->
        let arg2 = toArray com (Array Any) arg2
        let args =
            match meth, t with
            | "Permute", DeclaredType(_seq, [genArg]) ->
                [arg1; arg2] @ [arrayCons com genArg]
            | _ -> [arg1; arg2]
        let result = Helper.CoreCall("Array", Naming.lowerFirst meth, Any, args)
        Helper.CoreCall("Seq", "ofArray", t, [result]) |> Some
    // For Using we need to cast the argument to IDisposable
    | "EnumerateUsing", [arg; f] ->
        let arg =
            match arg.Type with
            | DeclaredType(ent,_) -> FSharp2Fable.Util.callInterfaceCast com t ent Types.disposable arg
            | _ -> arg
        Helper.CoreCall("Seq", "enumerateUsing", t, [arg; f], i.SignatureArgTypes, ?loc=r) |> Some
    // TODO!!! Sort and max/min methods, pass IComparable
    | meth, _ ->
        Helper.CoreCall("Seq", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let resizeArrays (_: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    // Use Any to prevent creation of a typed array (not resizable)
    // TODO: Include a value in Fable AST to indicate the Array should always be dynamic?
    | ".ctor", _, [] -> makeArray Any [] |> Some
    | ".ctor", _, [ExprType(Number _) as arg] -> NewArray(ArrayAlloc arg, Any) |> Value |> Some
    | ".ctor", _, [Value(NewArray(ArrayValues arVals, _))] -> makeArray Any arVals |> Some
    | ".ctor", _, args -> Helper.GlobalCall("Array", t, args, memb="from", ?loc=r) |> Some
    | "get_Item", Some ar, [idx] -> getExpr r t ar idx |> Some
    | "set_Item", Some ar, [idx; value] -> Set(ar, ExprSet idx, value, r) |> Some
    | "Add", Some ar, args -> Helper.InstanceCall(ar, "push", t, args, ?loc=r) |> Some
    | "Remove", Some ar, args -> Helper.CoreCall("Array", "removeInPlace", t, args @ [ar], ?loc=r) |> Some
    | "GetEnumerator", Some ar, _ -> getEnumerator r t ar |> Some
    // ICollection members, implemented in dictionaries and sets too. We need runtime checks (see #1120)
    | "get_Count", Some ar, _ -> Helper.CoreCall("Util", "count", t, [ar], ?loc=r) |> Some
    | "Clear", Some ar, _ -> Helper.CoreCall("Util", "clear", t, [ar], ?loc=r) |> Some
    | _ -> None
    // | "find" when Option.isSome c ->
    //     let defaultValue = defaultof i.calleeTypeArgs.Head
    //     ccall "Option" "getValue" [
    //         ccall "Seq" "tryFind" [args.Head;c.Value;defaultValue]
    //         Fable.Expr.Value (Fable.ValueKind.BoolConst true) ]
    //     |> Some
    // | "findAll" when Option.isSome c ->
    //     ccall "Seq" "filter" [args.Head;c.Value] |> toArray com i |> Some
    // | "findLast" when Option.isSome c ->
    //     let defaultValue = defaultof i.calleeTypeArgs.Head
    //     ccall "Option" "getValue" [
    //         ccall "Seq" "tryFindBack"
    //             [args.Head;c.Value;defaultValue];
    //         Fable.Expr.Value (Fable.ValueKind.BoolConst true) ]
    //     |> Some
    // | "addRange" ->
    //     ccall "Array" "addRangeInPlace" [args.Head; c.Value] |> Some
    // | "contains" ->
    //     match c, args with
    //     | Some c, args ->
    //         emit i "$0.indexOf($1) > -1" (c::args) |> Some
    //     | None, [item;xs] ->
    //         let f =
    //             wrapInLambda [makeIdent (com.GetUniqueVar())] (fun exprs ->
    //                 CoreLibCall("Util", Some "equals", false, item::exprs)
    //                 |> makeCall None Fable.Boolean)
    //         ccall "Seq" "exists" [f;xs] |> Some
    //     | _ -> None
    // | "indexOf" ->
    //     icall "indexOf" (c.Value, args) |> Some
    // | "insert" ->
    //     icall "splice" (c.Value, [args.Head; makeIntConst 0; args.Tail.Head]) |> Some
    // | "removeRange" ->
    //     icall "splice" (c.Value, args) |> Some
    // | "removeAt" ->
    //     icall "splice" (c.Value, [args.Head; makeIntConst 1]) |> Some
    // | "reverse" when kind = Array ->
    //     match i.returnType with
    //     | Fable.Array _ ->
    //         // Arrays need to be copied before sorted in place.
    //         emit i "$0.slice().reverse()" i.args |> Some
    //     | _ ->
    //         // ResizeArray should be sorted in place without copying.
    //         icall "reverse" (instanceArgs c i.args) |> Some

let nativeArrayFunctions =
    dict [| "Exists", "some"
            "Filter", "filter"
            "Find", "find"
            "FindIndex", "findIndex"
            "ForAll", "every"
            "Iterate", "forEach"
            "Reduce", "reduce"
            "ReduceBack", "reduceRight"
            "SortInPlace", "sort"
            "SortInPlaceWith", "sort" |]

let arrays (_: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, args, i.CompiledName with
    | Some ar, _, "get_Length" -> get r t ar "length" |> Some
    | Some ar, [idx], "get_Item" -> getExpr r t ar idx |> Some
    | Some ar, [idx; value], "set_Item" -> Set(ar, ExprSet idx, value, r) |> Some
    | _ -> None

let arrayModule (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let inline newArray size t =
        Value(NewArray(ArrayAlloc size, t))
    let createArray size value =
        match t, value with
        | Array(Number _ as t2), None -> newArray size t2
        | Array(Number _ as t2), Some value
        | Array(Boolean as t2), OrDefault (makeBoolConst false) value
        | Array t2, OrDefault (Value(Null Any)) value ->
            // If we don't fill the array some operations may behave unexpectedly, like Array.prototype.reduce
            Helper.CoreCall("Array", "fill", t, [newArray size t2; makeIntConst 0; size; value])
        | _ -> sprintf "Expecting an array type but got %A" t |> addErrorAndReturnNull com r
    match args, i.CompiledName with
    | [arg], "ToSeq" -> Some arg
    | [arg], "OfSeq" -> toArray com t arg |> Some
    | [arg], "OfList" -> listToArray com r t arg |> Some
    | [arg], ("Length"|"Count") -> get r t arg "length" |> Some
    | [idx; ar], "Item" -> getExpr r t ar idx |> Some
    | [ar; idx], "Get" -> getExpr r t ar idx |> Some
    | [ar; idx; value], "Set" -> Set(ar, ExprSet idx, value, r) |> Some
    | [count; ar], ("Take"|"Truncate") -> Helper.InstanceCall(ar, "slice", t, [makeIntConst 0; count], ?loc=r) |> Some
    | [count; ar], "Skip" -> Helper.InstanceCall(ar, "slice", t, [count], ?loc=r) |> Some
    | [ar],        "Copy" -> Helper.InstanceCall(ar, "slice", t, args, ?loc=r) |> Some
    | [count], "ZeroCreate"    -> createArray count None |> Some
    | [count; value], "Create" -> createArray count (Some value) |> Some
    | _, "Empty" ->
        let t = match t with Array t -> t | _ -> Any
        newArray (makeIntConst 0) t |> Some
    | [ar], "IsEmpty" ->
        eq (get r (Number Int32) ar "length") (makeIntConst 0) |> Some
    | [ar], "Head" -> getExpr r t ar (makeIntConst 0) |> Some
    | [ar], "Tail" -> Helper.InstanceCall(ar, "slice", t, [makeIntConst 1], ?loc=r) |> Some
    | _, Patterns.DicContains nativeArrayFunctions meth ->
        let args, thisArg = List.splitLast args
        let argTypes = List.take (List.length args) i.SignatureArgTypes
        Helper.InstanceCall(thisArg, meth, t, args, argTypes, ?loc=r) |> Some
    | _, meth ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com r "Array" meth i.GenericArgs args
        Helper.CoreCall("Array", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let lists (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, args, i.CompiledName with
    // Use methods for Head and Tail (instead of Get(ListHead) for example) to check for empty lists
    | Some x, _, ReplaceName [ "get_Head",   "head"
                               "get_Tail",   "tail"
                               "get_Item",   "item"
                               "get_Length", "length"
                               "GetSlice",   "slice" ] methName ->
        let args = match args with [ExprType Unit] -> [x] | args -> args @ [x]
        Helper.CoreCall("List", methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | Some x, _, "get_IsEmpty"
    | None, [x], "IsEmpty"-> Test(x, ListTest false, r) |> Some
    | None, _, ("get_Empty" | "Empty") ->
        NewList(None, firstGenArg com r i.GenericArgs) |> Value |> Some
    | None, [h;t], "Cons" -> NewList(Some(h,t), firstGenArg com r i.GenericArgs) |> Value |> Some
    // Use a cast to give it better chances of optimization (e.g. converting list
    // literals to arrays) after the beta reduction pass
    | None, [x], "ToSeq" -> Cast(x, t) |> Some
    | None, [x], "ToArray" -> listToArray com r t x |> Some
    | None, _, meth ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com r "List" meth i.GenericArgs args
        Helper.CoreCall("List", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let sets (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | ".ctor" -> firstGenArg com r i.GenericArgs |> makeSet r t "OfSeq" args |> Some
    | _ ->
        let isStatic = Option.isNone thisArg
        let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpSet" isStatic i.CompiledName
        Helper.CoreCall("Set", mangledName, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let setModule (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let meth = Naming.lowerFirst i.CompiledName
    let args = injectArg com r "Set" meth i.GenericArgs args
    Helper.CoreCall("Set", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let maps (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | ".ctor" -> firstGenArg com r i.GenericArgs |> makeMap r t "OfSeq" args |> Some
    | _ ->
        let isStatic = Option.isNone thisArg
        let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpMap" isStatic i.CompiledName
        Helper.CoreCall("Map", mangledName, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let mapModule (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let meth = Naming.lowerFirst i.CompiledName
    let args = injectArg com r "Map" meth i.GenericArgs args
    Helper.CoreCall("Map", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let results (_: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let meth = Naming.lowerFirst i.CompiledName
    Helper.CoreCall("Result", meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

// See fable-core/Option.ts for more info on how
// options behave in Fable runtime
let options (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let toArray r t arg =
        let ident = makeIdent (com.GetUniqueVar())
        let f =
            [IdentExpr ident]
            |> makeArray Any
            |> makeDelegate [ident]
        // Prevent functions being run twice, see #198
        Helper.CoreCall("Option", "defaultArg", t, [arg; makeArray Any []; f], ?loc=r)
    match i.CompiledName, thisArg, args with
    | "None", None, _ -> NewOption(None, t) |> Value |> Some
    | "GetValue", None, [c]
    | "get_Value", Some c, _ -> Get(c, OptionValue, t, r) |> Some
    | ("OfObj" | "OfNullable"), None, [c] -> Some c // TODO: Call function to keep type?
    | ("ToObj" | "ToNullable" | "Flatten"), None, [c] ->
        Helper.CoreCall("Option", "value", t, [c; makeBoolConst true], ?loc=r) |> Some
    | "IsSome", _, [c] | "get_IsSome", Some c, _ -> Test(c, OptionTest true, r) |> Some
    | "IsNone", _, [c] | "get_IsNone", Some c, _ -> Test(c, OptionTest false, r) |> Some
    | ("Map" | "Bind"), None, [f; arg] ->
        let fType, argType =
            match i.SignatureArgTypes with
            | [fType; argType] -> fType, argType
            | _ -> f.Type, arg.Type // unexpected
        let args = [arg; Value(NewOption(None, argType)); f]
        Helper.CoreCall("Option", "defaultArg", t, args, [argType; Option argType; fType],  ?loc=r) |> Some
    | "Filter", _, _ ->
        Helper.CoreCall("Option", "filter", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "ToArray", _, [arg] ->
        toArray r t arg |> Some
    | "FoldBack", _, [folder; opt; state] ->
        Helper.CoreCall("Seq", "foldBack", t, [folder; toArray None Any opt; state], i.SignatureArgTypes, ?loc=r) |> Some
    | ("DefaultValue" | "OrElse"), _, _ ->
        Helper.CoreCall("Option", "defaultArg", t, List.rev args, ?loc=r) |> Some
    | ("DefaultWith" | "OrElseWith"), _, _ ->
        Helper.CoreCall("Option", "defaultArgWith", t, List.rev args, List.rev i.SignatureArgTypes, ?loc=r) |> Some
    | ("Count" | "Contains" | "Exists" | "Fold" | "ForAll" | "Iterate" | "ToList" as meth), _, _ ->
        let args = args |> List.replaceLast (toArray None Any)
        let moduleName, meth =
            if meth = "ToList"
            then "List", "ofArray"
            else "Seq", Naming.lowerFirst meth
        Helper.CoreCall(moduleName, meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // | "map2" | "map3" -> failwith "TODO"
    | _ -> None

type ParseTarget =
    | Parse2Int
    | Parse2Float

let parse target (com: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    // TODO what about Single (and decimal)?
    let isFloat, numberModule =
        match target with
        | Parse2Int -> false, "Int32"
        | Parse2Float -> true, "Double"
    match i.CompiledName with
    | "IsNaN" when isFloat ->
        match args with
        | [_someNumber] -> Helper.GlobalCall("Number", t, args, memb="isNaN", ?loc=r) |> Some
        | _ -> None
    // TODO verify that the number is within the Int32/Double/Single range
    | "Parse" | "TryParse" ->
        let hexConst = float System.Globalization.NumberStyles.HexNumber
        match i.CompiledName, args with
        | "Parse", [str] ->
            Helper.CoreCall(numberModule, "parse", t,
                [str; (if isFloat then makeFloatConst 10.0 else makeIntConst 10)],
                [i.SignatureArgTypes.Head; Number Float32], ?loc=r) |> Some
        | "Parse", [str; Value(Enum(NumberEnum(Value(NumberConstant(hexConst', _))), _))] ->
            if hexConst' = hexConst then
                Helper.CoreCall(numberModule, "parse", t,
                    [str; (if isFloat then makeFloatConst 16.0 else makeIntConst 16)],
                    [i.SignatureArgTypes.Head; Number Float32], ?loc=r) |> Some
            else None  (* Todo *)
        // System.Double.Parse(string, IFormatProvider)
        // just ignore the second args (IFormatProvider) and compile
        // to System.Double.Parse(string)
        | "Parse", [str; _ ] ->
            Helper.CoreCall(numberModule, "parse", t,
                [str; (if isFloat then makeFloatConst 10.0 else makeIntConst 10)],
                [i.SignatureArgTypes.Head; Number Float32])
            |> Some
        | "TryParse", [str; defValue] ->
            Helper.CoreCall(numberModule, "tryParse", t,
                [str; (if isFloat then makeFloatConst 10.0 else makeIntConst 10); defValue],
                [i.SignatureArgTypes.Head; Number Float32; i.SignatureArgTypes.Tail.Head], ?loc=r) |> Some
        | _ ->
            sprintf "%s.%s only accepts a single argument" i.DeclaringEntityFullName i.CompiledName
            |> addErrorAndReturnNull com r |> Some
    | "ToString" ->
        match args with
        | [Value (StringConstant _) as format] ->
            let format = emitJs r String [format] "'{0:' + $0 + '}'"
            Helper.CoreCall("String", "format", t, [format; thisArg.Value], [format.Type; thisArg.Value.Type], ?loc=r) |> Some
        | _ -> Helper.CoreCall("Util", "toString", t, [thisArg.Value], [thisArg.Value.Type], ?loc=r) |> Some
    | _ -> None

let decimals (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | ".ctor", [Value(NumberConstant(x, _))] ->
        makeDecConst (decimal(x)) |> Some
    | ".ctor", [Value(NewArray(ArrayValues arVals, _))] ->
        match arVals with
        | [ Value(NumberConstant(low, Int32))
            Value(NumberConstant(mid, Int32))
            Value(NumberConstant(high, Int32)) // TODO: Review
            Value(NumberConstant(scale, Int32)) ] ->
                let x = (float ((uint64 (uint32 mid)) <<< 32 ||| (uint64 (uint32 low))))
                        / System.Math.Pow(10.0, float ((int scale) >>> 16 &&& 0xFF))
                decimal(if scale < 0.0 then -x else x) |> makeDecConst |> Some
        | _ -> None
    | (".ctor" | "MakeDecimal"),
          [ Value(NumberConstant(low, Int32))
            Value(NumberConstant(mid, Int32))
            Value(NumberConstant(high, Int32)) // TODO: Review
            Value(BoolConstant isNegative)
            Value(NumberConstant(scale, UInt8)) ] ->
                let x = (float ((uint64 (uint32 mid)) <<< 32 ||| (uint64 (uint32 low))))
                        / System.Math.Pow(10.0, float scale)
                decimal (if isNegative then -x else x) |> makeDecConst |> Some
    | ".ctor", [IdentExpr _ as arg] ->
        // TODO: Add this warning in other constructors?
        addWarning com r "Decimals are implemented with floats."
        Some arg
    | ("Parse" | "TryParse"), _ ->
        parse Parse2Float com ctx r t i thisArg args
// let parse isFloat (com: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =

    | _,_ -> None

// let bigint (com: ICompiler) (_: Context) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
//     let coreMod = coreModFor BclBigInt
//     match thisArg, i.CompiledName with
//     | None, ".ctor" ->
//         match i.SignatureArgTypes with
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

let languagePrimitives (_: ICompiler) (_: Context) r t (i: CallInfo) (_thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args, t with
    | "GenericZero", _, _ -> getZero t |> Some
    | "GenericOne", _, _ -> getOne t |> Some
    | "EnumOfValue", [arg], EnumType(_, fullName) -> Enum(NumberEnum arg, fullName) |> Value |> Some
    // TODO!!!
    // | "genericHash"
    // | "genericHashIntrinsic" ->
    //     CoreLibCall("Util", Some "hash", false, i.args)
    //     |> makeCall i.range i.returnType |> Some
    // | "genericComparison"
    // | "genericComparisonIntrinsic" ->
    //     CoreLibCall("Util", Some "compare", false, i.args)
    //     |> makeCall i.range i.returnType |> Some
    // | "genericLessThan"
    // | "genericLessThanIntrinsic" ->
    //     CoreLibCall("Util", Some "lessThan", false, i.args)
    //     |> makeCall i.range i.returnType |> Some
    // | "genericLessOrEqual"
    // | "genericLessOrEqualIntrinsic" ->
    //     CoreLibCall("Util", Some "lessOrEqual", false, i.args)
    //     |> makeCall i.range i.returnType |> Some
    // | "genericGreaterThan"
    // | "genericGreaterThanIntrinsic" ->
    //     CoreLibCall("Util", Some "greaterThan", false, i.args)
    //     |> makeCall i.range i.returnType |> Some
    // | "genericGreaterOrEqual"
    // | "genericGreaterOrEqualIntrinsic" ->
    //     CoreLibCall("Util", Some "greaterOrEqual", false, i.args)
    //     |> makeCall i.range i.returnType |> Some
    // | "genericEquality"
    // | "genericEqualityIntrinsic" ->
    //     CoreLibCall("Util", Some "equals", false, i.args)
    //     |> makeCall i.range i.returnType |> Some
    // | "physicalEquality"
    // | "physicalEqualityIntrinsic" ->
    //     makeEqOp i.range i.args BinaryEqualStrict |> Some
    // | "physicalHash"
    // | "physicalHashIntrinsic" ->
    //     CoreLibCall("Util", Some "getHashCode", false, i.args)
    //     |> makeCall i.range i.returnType |> Some
    | _ -> None

let intrinsicFunctions (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    // Erased operators
    | "CheckThis", _, [arg]
    | "UnboxFast", _, [arg] -> Some arg
    // Cast (e.g. used to cast to IDisposable at the end of `use` scope)
    | "UnboxGeneric", _, [arg] -> Cast(arg, t) |> Some
    | "MakeDecimal", _, _ -> decimals com ctx r t i thisArg args
    | "GetString", _, [ar; idx]
    | "GetArray", _, [ar; idx] -> getExpr r t ar idx |> Some
    | "SetArray", _, [ar; idx; value] -> Set(ar, ExprSet idx, value, r) |> Some
    | ("GetArraySlice" | "GetStringSlice"), None, [ar; lower; upper] ->
        let upper =
            match upper with
            | Value(NewOption(None,_)) -> getExpr None (Number Int32) ar (makeStrConst "length")
            | _ -> add upper (makeIntConst 1)
        Helper.InstanceCall(ar, "slice", t, [lower; upper], ?loc=r) |> Some
    | "SetArraySlice", None, args ->
        Helper.CoreCall("Array", "setSlice", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "TypeTestGeneric", None, [expr] ->
        Test(expr, TypeTest(firstGenArg com r i.GenericArgs), r) |> Some
    | "CreateInstance", None, _ ->
        None // TODO
        // let typRef, args = resolveTypeRef com i false i.memberGenArgs.Head, []
        // Apply (typRef, args, ApplyCons, t, r) |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.powdouble-function-%5bfsharp%5d
    // Type: PowDouble : float -> int -> float
    // Usage: PowDouble x n
    | "PowDouble", None, _ ->
        Helper.GlobalCall("Math", t, args, i.SignatureArgTypes, memb="pow", ?loc=r) |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangechar-function-%5bfsharp%5d
    // Type: RangeChar : char -> char -> seq<char>
    // Usage: RangeChar start stop
    | "RangeChar", None, _ ->
        Helper.CoreCall("Seq", "rangeChar", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangedouble-function-%5bfsharp%5d
    // Type: RangeDouble: float -> float -> float -> seq<float>
    // Usage: RangeDouble start step stop
    | "RangeDouble", None, _ ->
        Helper.CoreCall("Seq", "rangeStep", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "RangeInt32", None, args ->
        Helper.CoreCall("Seq", "rangeStep", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let funcs (_: ICompiler) (_: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg with
    | "Adapt", _ -> List.tryHead args // TODO: What's this used for?
    | "Invoke", Some callee ->
        Helper.Application(callee, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let keyValuePairs (_: ICompiler) (_: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> Value(NewTuple args) |> Some
    | "get_Key", Some c -> Get(c, TupleGet 0, t, r) |> Some
    | "get_Value", Some c -> Get(c, TupleGet 1, t, r) |> Some
    | _ -> None

let dictionaries (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let (|IDictionary|IEqualityComparer|Other|) = function
        | DeclaredType(ent,_) ->
            match ent.TryFullName with
            | Some Types.idictionary -> IDictionary
            | Some "System.Collections.Generic.IEqualityComparer`1" -> IEqualityComparer
            | _ -> Other
        | _ -> Other
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match i.SignatureArgTypes, args with
        | ([]|[Number _]), _ ->
            makeDictionary r t (makeArray Any []) |> Some
        | [IDictionary], [arg] ->
            makeDictionary r t arg |> Some
        | [IDictionary; IEqualityComparer], [arg; eqComp] ->
            makeComparerFromEqualityComparer eqComp
            |> makeDictionaryWithComparer r t arg |> Some
        | [IEqualityComparer], [eqComp]
        | [Number _; IEqualityComparer], [_; eqComp] ->
            makeComparerFromEqualityComparer eqComp
            |> makeDictionaryWithComparer r t (makeArray Any []) |> Some
        | _ -> None
    | "get_IsReadOnly", _ -> makeBoolConst false |> Some
    | "get_Count", _ -> get r t thisArg.Value "size" |> Some
    | "GetEnumerator", Some callee -> getEnumerator r t callee |> Some
    | "ContainsValue", _ ->
        match thisArg, args with
        | Some c, [arg] -> Helper.CoreCall("Util", "containsValue", t, [arg; c], ?loc=r) |> Some
        | _ -> None
    | "TryGetValue", _ ->
        Helper.CoreCall("Util", "tryGetValue", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | ReplaceName ["get_Item",     "get"
                   "set_Item",     "set"
                   "get_Keys",     "keys"
                   "get_Values",   "values"
                   "ContainsKey",  "has"
                   "Clear",        "clear"
                   "Add",          "set"
                   "Remove",       "delete" ] methName, Some c ->
        Helper.InstanceCall(c, methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let hashSets (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let (|IEnumerable|IEqualityComparer|Other|) = function
        | DeclaredType(ent,_) ->
            match ent.TryFullName with
            | Some Types.enumerable -> IEnumerable
            | Some "System.Collections.Generic.IEqualityComparer`1" -> IEqualityComparer
            | _ -> Other
        | _ -> Other
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ ->
        match i.SignatureArgTypes, args with
        | [], _ ->
            makeHashSet r t (makeArray Any []) |> Some
        | [IEnumerable], [arg] ->
            makeHashSet r t arg |> Some
        | [IEnumerable; IEqualityComparer], [arg; eqComp] ->
            makeComparerFromEqualityComparer eqComp
            |> makeHashSetWithComparer r t arg |> Some
        | [IEqualityComparer], [eqComp] ->
            makeComparerFromEqualityComparer eqComp
            |> makeHashSetWithComparer r t (makeArray Any []) |> Some
        | _ -> None
    | "get_Count", _, _ -> get r t thisArg.Value "size" |> Some
    | "get_IsReadOnly", _, _ -> BoolConstant false |> Value |> Some
    | ReplaceName ["Clear",    "clear"
                   "Contains", "has"
                   "Remove",   "delete" ] methName, Some c, args ->
        Helper.InstanceCall(c, methName, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Add", Some c, [arg] ->
        Helper.CoreCall("Util", "addToSet", t, [arg; c], ?loc=r) |> Some
    | ("isProperSubsetOf" | "isProperSupersetOf"
    |  "unionWith" | "intersectWith" | "exceptWith"
    |  "isSubsetOf" | "isSupersetOf" | "copyTo" as methName), Some c, args ->
        let methName =
            let m = match methName with "exceptWith" -> "differenceWith" | m -> m
            m.Replace("With", "InPlace")
        Helper.CoreCall("Set", methName, t, c::args, ?loc=r) |> Some
    // TODO
    // | "setEquals"
    // | "overlaps"
    // | "symmetricExceptWith"
    | _ -> None

let exceptions (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> Helper.ConstructorCall(makeIdentExpr "Error", t, args, ?loc=r) |> Some
    | "get_Message", Some e -> get r t e "message" |> Some
    | "get_StackTrace", Some e -> get r t e "stack" |> Some
    | _ -> None

let objects (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> objExpr t [] |> Some
    | "GetHashCode", Some arg, _ ->
        Helper.CoreCall("Util", "getHashCode", t, [arg]) |> Some
    | "ToString", Some arg, _ ->
        Helper.CoreCall("Util", "toString", t, [arg]) |> Some
    | "ReferenceEquals", _, [left; right] ->
        makeBinOp r Boolean left right BinaryEqualStrict |> Some
    | "Equals", Some arg1, [arg2]
    | "Equals", None, [arg1; arg2] ->
        Helper.CoreCall("Util", "equals", t, [arg1; arg2], ?loc=r) |> Some
    | _ -> None

let unchecked (com: ICompiler) (_: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "DefaultOf" -> firstGenArg com r i.GenericArgs |> defaultof |> Some
    | "Hash" -> Helper.CoreCall("Util", "hash", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Equals" -> Helper.CoreCall("Util", "equals", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Compare" -> Helper.CoreCall("Util", "compare", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let enums (_: ICompiler) (_: Context) r _ (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, i.CompiledName, args with
    | Some this, "HasFlag", [arg] ->
        // x.HasFlags(y) => (int x) &&& (int y) <> 0
        makeBinOp r (Number Int32) this arg BinaryAndBitwise
        |> fun bitwise -> makeEqOp r bitwise (makeIntConst 0)BinaryUnequal
        |> Some
    | _ -> None

let log (_: ICompiler) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    let args =
        match args with
        | [] -> []
        | [v] -> [v]
        | (Value (StringConstant _))::_ -> [Helper.CoreCall("String", "format", t, args, i.SignatureArgTypes)]
        | _ -> [args.Head]
    Helper.GlobalCall("console", t, args, memb="log", ?loc=r)

let bitConvert (_: ICompiler) (_: Context) r (_: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let memberName =
        if i.CompiledName = "GetBytes" then
            match args.Head.Type with
            | Boolean -> "getBytesBoolean"
            | Char | String -> "getBytesChar"
            | Number Int16 -> "getBytesInt16"
            | Number Int32 -> "getBytesInt32"
            | Builtin BclInt64 -> "getBytesInt64"
            | Number UInt16 -> "getBytesUInt16"
            | Builtin BclUInt64 -> "getBytesUInt64"
            | Number UInt32 -> "getBytesUInt32"
            | Number Float32 -> "getBytesSingle"
            | Number Float64 -> "getBytesDouble"
            | x -> failwithf "Unsupported type in BitConverter.GetBytes(): %A" x
        else Naming.lowerFirst i.CompiledName
    Helper.CoreCall("BitConverter", memberName, Boolean, args, i.SignatureArgTypes, ?loc=r) |> Some

let convert (com: ICompiler) (_: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    let sourceType = List.head i.SignatureArgTypes
    match i.CompiledName with
    | "ToSByte" | "ToByte"
    | "ToInt16" | "ToUInt16"
    | "ToInt32" | "ToUInt32"
    | "ToInt64" | "ToUInt64"
        -> toInt true sourceType t args |> Some
    | "ToSingle" | "ToDouble" | "ToDecimal"
        -> toFloat sourceType t args |> Some
    | "ToChar" -> toChar sourceType args |> Some
    | "ToString" -> toString sourceType args |> Some
    | "ToBase64String" | "FromBase64String" ->
        if not(List.isSingle args) then
            sprintf "Convert.%s only accepts one single argument" (Naming.upperFirst i.CompiledName)
            |> addWarning com r
        Helper.CoreCall("String", (Naming.lowerFirst i.CompiledName), t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let console (com: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Out" -> None
    | "Write" ->
        addWarning com r "Write will behave as WriteLine"
        log com r t i thisArg args |> Some
    | "WriteLine" -> log com r t i thisArg args |> Some
    | _ -> None

let debug (com: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Write" ->
        addWarning com r "Write will behave as WriteLine"
        log com r t i thisArg args |> Some
    | "WriteLine" -> log com r t i thisArg args |> Some
    | "Break" -> Debugger |> Some
    | "Assert" ->
        // emit i "if (!$0) { debugger; }" i.args |> Some
        let cond = Operation(UnaryOperation (UnaryNot, args.Head), Boolean, r)
        IfThenElse(cond, Debugger, Value (Null Unit)) |> Some
    | _ -> None

let dates (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let getTime (e: Expr) =
        Helper.InstanceCall(e, "getTime", t, [])
    let moduleName =
        if i.DeclaringEntityFullName = Types.datetime
        then "Date" else "DateOffset"
    match i.CompiledName with
    | ".ctor" ->
        match args with
        | [] -> Helper.CoreCall(moduleName, "minValue", t, [], [], ?loc=r) |> Some
        | (ExprType(Builtin BclInt64) as ticks)::rest ->
            let ms = Helper.CoreCall("Long","ticksToUnixEpochMilliseconds", t, [ticks], [i.SignatureArgTypes.Head])
            Helper.CoreCall(moduleName, "default", t, (ms::rest), t::i.SignatureArgTypes.Tail, ?loc=r) |> Some
        | ExprType(DeclaredType(e,[]))::_ when e.FullName = Types.datetime ->
            Helper.CoreCall("DateOffset", "fromDate", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ ->
            let last = List.last args
            match args.Length, last.Type with
            | 7, EnumType(_, "System.DateTimeKind") ->
                let args = (List.take 6 args) @ [makeIntConst 0; last]
                let argTypes = (List.take 6 i.SignatureArgTypes) @ [Number Int32; last.Type]
                Helper.CoreCall("Date", "create", t, args, argTypes, ?loc=r) |> Some
            | _ ->
                Helper.CoreCall(moduleName, "create", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "ToString" ->
        Helper.CoreCall("Date", "toString", t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some
    | "get_Kind" | "get_Offset" ->
        Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst |> get r t thisArg.Value |> Some
    // DateTimeOffset
    | "get_DateTime" | "get_LocalDateTime" | "get_UtcDateTime" as m ->
        let ms = getTime thisArg.Value
        let kind =
            if m = "LocalDateTime" then System.DateTimeKind.Local
            elif m = "UtcDateTime" then System.DateTimeKind.Utc
            else System.DateTimeKind.Unspecified
            |> int |> makeIntConst
        Helper.CoreCall("Date", "default", t, [ms; kind], [ms.Type; kind.Type], ?loc=r) |> Some
    | "FromUnixTimeSeconds"
    | "FromUnixTimeMilliseconds" ->
        let value = Helper.CoreCall("Long", "toNumber", Number Float64, args, i.SignatureArgTypes)
        let value =
            if i.CompiledName = "FromUnixTimeSeconds"
            then makeBinOp r t value (makeIntConst 1000) BinaryMultiply
            else value
        Helper.CoreCall("DateOffset", "default", t, [value; makeIntConst 0], [value.Type; Number Int32], ?loc=r) |> Some
    | "ToUnixTimeSeconds"
    | "ToUnixTimeMilliseconds" ->
        let ms = getTime thisArg.Value
        let args =
            if i.CompiledName = "ToUnixTimeSeconds"
            then [makeBinOp r t ms (makeIntConst 1000) BinaryDivide]
            else [ms]
        Helper.CoreCall("Long", "fromNumber", t, args, ?loc=r) |> Some
    // Ticks methods are moved to Long.ts so we don't have to import it from Date.ts
    | "get_Ticks" | "get_UtcTicks" | "ToBinary" ->
        let ms = getTime thisArg.Value
        let offset =
            if i.CompiledName = "get_UtcTicks"
            then makeIntConst 0
            else Helper.CoreCall("Date", "offset", Number Float64, [thisArg.Value], [thisArg.Value.Type])
        Helper.CoreCall("Long", "unixEpochMillisecondsToTicks",
                        Number Float64, [ms; offset], [ms.Type; offset.Type], ?loc=r) |> Some
    | "AddTicks" ->
        match thisArg, args with
        | Some c, [ticks] ->
            let ms = Helper.CoreCall("Long", "op_Division", i.SignatureArgTypes.Head, [ticks; makeIntConst 10000], [ticks.Type; Number Int32])
            let ms = Helper.CoreCall("Long", "toNumber", Number Float64, [ms], [ms.Type])
            Helper.CoreCall(moduleName, "addMilliseconds", Number Float64, [c; ms], [c.Type; ms.Type], ?loc=r) |> Some
        | _ -> None
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        Helper.CoreCall(moduleName, meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let timeSpans (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    // let callee = match i.callee with Some c -> c | None -> i.args.Head
    match i.CompiledName with
    | ".ctor" -> Helper.CoreCall("TimeSpan", "create", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "FromMilliseconds" -> args.Head |> Some
    | "get_TotalMilliseconds" -> thisArg.Value |> Some
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        Helper.CoreCall("TimeSpan", meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let timers (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> Helper.CoreCall("Timer", "default", t, args, i.SignatureArgTypes, isConstructor=true, ?loc=r) |> Some
    | Naming.StartsWith "get_" meth, Some x, _ -> get r t x meth |> Some
    | Naming.StartsWith "set_" meth, Some x, [value] -> Set(x, ExprSet(makeStrConst meth), value, r) |> Some
    | meth, Some x, args -> Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let systemEnv (_: ICompiler) (_: Context) (_: SourceLocation option) (_: Type) (i: CallInfo) (_: Expr option) (_: Expr list) =
    match i.CompiledName with
    | "get_NewLine" -> Some (makeStrConst "\n")
    | _ -> None

// Initial support, making at least InvariantCulture compile-able
// to be used System.Double.Parse and System.Single.Parse
// see https://github.com/fable-compiler/Fable/pull/1197#issuecomment-348034660
let globalization (_: ICompiler) (_: Context) (_: SourceLocation option) t (i: CallInfo) (_: Expr option) (_: Expr list) =
    match i.CompiledName with
    | "InvariantCulture" ->
        // System.Globalization namespace is not supported by Fable. The value InvariantCulture will be compiled to an empty object literal
        ObjectExpr([], t, None) |> Some
    | _ -> None

let random (_: ICompiler) (_: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | ".ctor" -> ObjectExpr ([], t, None) |> Some
    | "Next" ->
        let min, max =
            match args with
            | [] -> makeIntConst 0, makeIntConst System.Int32.MaxValue
            | [max] -> makeIntConst 0, max
            | [min; max] -> min, max
            | _ -> failwith "Unexpected arg count for Random.Next"
        Helper.CoreCall("Util", "randomNext", t, [min; max], [min.Type; max.Type], ?loc=r) |> Some
    | "NextDouble" ->
        Helper.GlobalCall ("Math", t, [], [], memb="random") |> Some
    | _ -> None

let cancels (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | ".ctor" -> Helper.CoreCall("Async", "createCancellationToken", t, args, i.SignatureArgTypes) |> Some
    | "get_Token" -> thisArg
    | "Cancel" | "CancelAfter" | "IsCancellationRequested" ->
        let args, argTypes = match thisArg with Some c -> c::args, c.Type::i.SignatureArgTypes | None -> args, i.SignatureArgTypes
        Helper.CoreCall("Async", Naming.lowerFirst i.CompiledName, t, args, argTypes, ?loc=r) |> Some
    // TODO: Add check so CancellationTokenSource cannot be cancelled after disposed?
    | "Dispose" -> Null Type.Unit |> Value |> Some
    | _ -> None

let activator (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "CreateInstance", None, typRef::args ->
        let info = argInfo None args (Some i.SignatureArgTypes.Tail)
        constructorCall r t info typRef |> Some
    | _ -> None

let regex com (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let propInt p callee = getExpr r t callee (makeIntConst p)
    let propStr p callee = getExpr r t callee (makeStrConst p)
    let isGroup =
        match thisArg with
        | Some (ExprType (EntFullName "System.Text.RegularExpressions.Group")) -> true
        | _ -> false
    match i.CompiledName with
    // TODO: Use RegexConst if no options have been passed?
    | ".ctor"   -> Helper.CoreCall("RegExp", "create", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "get_Options" -> Helper.CoreCall("RegExp", "options", t, [thisArg.Value], [thisArg.Value.Type], ?loc=r) |> Some
    // Capture
    | "get_Index" ->
        if not isGroup
        then propStr "index" thisArg.Value |> Some
        else addErrorAndReturnNull com r "Accessing index of Regex groups is not supported" |> Some
    | "get_Value" ->
        if isGroup
        // In JS Regex group values can be undefined, ensure they're empty strings #838
        then makeLogOp r thisArg.Value (makeStrConst "") LogicalOr |> Some
        else propInt 0 thisArg.Value |> Some
    | "get_Length" ->
        if isGroup
        then propStr "length" thisArg.Value |> Some
        else propInt 0 thisArg.Value |> propStr "length" |> Some
    // Group
    | "get_Success" -> makeEqOp r thisArg.Value (Value (Null thisArg.Value.Type)) BinaryUnequal |> Some
    // Match
    | "get_Groups" -> thisArg.Value |> Some
    // MatchCollection & GroupCollection
    | "get_Item" -> getExpr r t thisArg.Value args.Head |> Some
    | "get_Count" -> propStr "length" thisArg.Value |> Some
    | meth ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        Helper.CoreCall("RegExp", meth, t, args, i.SignatureArgTypes, ?thisArg=thisArg, ?loc=r) |> Some

let enumerables (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (_: Expr list) =
    match thisArg, i.CompiledName with
    // This property only belongs to Key and Value Collections
    | Some callee, "get_Count" -> Helper.CoreCall("Seq", "length", t, [callee], ?loc=r) |> Some
    | Some callee, "GetEnumerator" -> getEnumerator r t callee |> Some
    | _ -> None

let enumerators (_: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | "get_Current", Some x -> get r t x "Current" |> Some
    | meth, Some x -> Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | _ -> None

let events (_: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> Helper.CoreCall("Event", "default", t, args, i.SignatureArgTypes, isConstructor=true, ?loc=r) |> Some
    | "get_Publish", Some x -> get r t x "Publish" |> Some
    | meth, Some x -> Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | meth, None -> Helper.CoreCall("Event", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let observable (_: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    Helper.CoreCall("Observable", Naming.lowerFirst i.CompiledName, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let mailbox (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg with
    | None ->
        match i.CompiledName with
        | ".ctor" -> Helper.CoreCall("MailboxProcessor", "default", t, args, i.SignatureArgTypes, isConstructor=true, ?loc=r) |> Some
        | "Start" -> Helper.CoreCall("MailboxProcessor", "start", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ -> None
    | Some callee ->
        match i.CompiledName with
        // `reply` belongs to AsyncReplyChannel
        | "Start" | "Receive" | "PostAndAsyncReply" | "Post" ->
            let memb =
                if i.CompiledName = "Start"
                then "startInstance"
                else Naming.lowerFirst i.CompiledName
            Helper.CoreCall("MailboxProcessor", memb, t, args, i.SignatureArgTypes, thisArg=callee, ?loc=r) |> Some
        | "Reply" -> Helper.InstanceCall(callee, "reply", t, args, i.SignatureArgTypes, ?loc=r) |> Some
        | _ -> None

let asyncBuilder (com: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, i.CompiledName, args with
    | _, "Singleton", _ -> Import("singleton", "AsyncBuilder", CoreLib, t) |> Some
    // For Using we need to cast the argument to IDisposable
    | Some x, "Using", [arg; f] ->
        let arg =
            match arg.Type with
            | DeclaredType(ent,_) -> FSharp2Fable.Util.callInterfaceCast com t ent Types.disposable arg
            | _ -> arg
        Helper.InstanceCall(x, "Using", t, [arg; f], i.SignatureArgTypes, ?loc=r) |> Some
    | Some x, meth, _ -> Helper.InstanceCall(x, meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | None, meth, _ -> Helper.CoreCall("AsyncBuilder", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let asyncs com (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    // TODO: Throw error for RunSynchronously
    | "Start" ->
        "Async.Start will behave as StartImmediate" |> addWarning com r
        Helper.CoreCall("Async", "start", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // Make sure cancellationToken is called as a function and not a getter
    | "get_CancellationToken" -> Helper.CoreCall("Async", "cancellationToken", t, [], ?loc=r) |> Some
    // `catch` cannot be used as a function name in JS
    | "Catch" -> Helper.CoreCall("Async", "catchAsync", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    // Fable.Core extensions
    | meth -> Helper.CoreCall("Async", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc=r) |> Some

let guids (_: ICompiler) (_: Context) (_: SourceLocation option) t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "NewGuid"     -> Helper.CoreCall("String", "newGuid", t, []) |> Some
    | "Parse"       -> Helper.CoreCall("String", "validateGuid", t, args, i.SignatureArgTypes) |> Some
    | "TryParse"    -> Helper.CoreCall("String", "validateGuid", t, [args.Head; makeBoolConst true], [args.Head.Type; Boolean]) |> Some
    | "ToByteArray" -> Helper.CoreCall("String", "guidToArray", t, [thisArg.Value], [thisArg.Value.Type]) |> Some
    | ".ctor" ->
        match args with
        | [] -> makeStrConst "00000000-0000-0000-0000-000000000000" |> Some
        | [ExprType (Array _)] -> Helper.CoreCall("String", "arrayToGuid", t, args, i.SignatureArgTypes) |> Some
        | [ExprType String]    -> Helper.CoreCall("String", "validateGuid", t, args, i.SignatureArgTypes) |> Some
        | _ -> None
    | _ -> None

let uris (_: ICompiler) (_: Context) (_: SourceLocation option) t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "UnescapeDataString" -> Helper.CoreCall("Util", "unescapeDataString", t, args, i.SignatureArgTypes) |> Some
    | "EscapeDataString"   -> Helper.CoreCall("Util", "escapeDataString", t, args, i.SignatureArgTypes) |> Some
    | "EscapeUriString"    -> Helper.CoreCall("Util", "escapeUriString", t, args, i.SignatureArgTypes) |> Some
    | _ -> None

let laziness (_: ICompiler) (_: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | (".ctor"|"Create"),_,_ -> Helper.CoreCall("Util", "Lazy", t, args, i.SignatureArgTypes, isConstructor=true, ?loc=r) |> Some
    | "CreateFromValue",_,_ -> Helper.CoreCall("Util", "lazyFromValue", t, args, i.SignatureArgTypes, ?loc=r) |> Some
    | "Force", Some callee, _ -> get r t callee "Value" |> Some
    | ("get_Value"|"get_IsValueCreated"), Some callee, _ ->
        Naming.removeGetSetPrefix i.CompiledName |> get r t callee |> Some
    | _ -> None

let controlExtensions (_: ICompiler) (_: Context) (_: SourceLocation option) t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
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
        Helper.CoreCall("Observable", meth, t, args, argTypes))

let fsharpType methName (_: ICompiler) (_: SourceLocation option) t (i: CallInfo) (_: Expr option) (args: Expr list) =
    let hasInterface ifc (typRef: Expr) (typRefType) =
        let proto = Helper.CoreCall("Reflection", "getPrototypeOfType", Any, [typRef], [typRefType], ?loc=typRef.Range)
        Helper.CoreCall("Util", "hasInterface", t, [proto; makeStrConst ifc], [Any; String])
    match methName with
    | "GetRecordFields"
    | "GetExceptionFields" ->
        Helper.CoreCall("Reflection", "getProperties", t, args, i.SignatureArgTypes) |> Some
    | "GetUnionCases"
    | "GetTupleElements"
    | "GetFunctionElements" ->
        Helper.CoreCall("Reflection", Naming.lowerFirst methName, t, args, i.SignatureArgTypes) |> Some
    | "IsUnion" ->
        hasInterface "FSharpUnion" args.Head i.SignatureArgTypes.Head |> Some
    | "IsRecord" ->
        hasInterface "FSharpRecord" args.Head i.SignatureArgTypes.Head |> Some
    | "IsExceptionRepresentation" ->
        hasInterface "FSharpException" args.Head i.SignatureArgTypes.Head |> Some
    | "IsTuple" ->
        Helper.CoreCall("Reflection", "isTupleType", t, args, i.SignatureArgTypes) |> Some
    | "IsFunction" ->
        Helper.CoreCall("Reflection", "isFunctionType", t, args, i.SignatureArgTypes) |> Some
    | _ -> None

let fsharpValue methName (_: ICompiler) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match methName with
    | "GetUnionFields" ->
        Helper.CoreCall("Reflection", "getUnionFields", t, args, i.SignatureArgTypes) |> Some
    | "GetRecordFields"
    | "GetExceptionFields" ->
        Helper.CoreCall("Reflection", "getPropertyValues", t, args, i.SignatureArgTypes) |> Some
    | "GetTupleFields" -> // TODO: Check if it's an array first?
        Some args.Head
    | "GetTupleField" ->
        getExpr r t args.Head args.Tail.Head |> Some
    | "GetRecordField" ->
        match args with
        | [record; propInfo] ->
            let prop = get propInfo.Range String propInfo "name"
            getExpr r t record prop |> Some
        | _ -> None
    | "MakeUnion" ->
        Helper.CoreCall("Reflection", "makeUnion", t, args, i.SignatureArgTypes) |> Some
    | "MakeRecord" ->
        match args with
        | [typ; vals] ->
            let typ = Helper.CoreCall("Util", "getDefinition", Any, [typ], [i.SignatureArgTypes.Head], ?loc=typ.Range)
            let argInfo =
                { ThisArg = None
                  Args = [vals]
                  SignatureArgTypes = Some i.SignatureArgTypes.Tail
                  Spread = SeqSpread
                  IsSiblingConstructorCall = false }
            Operation(Call(ConstructorCall typ, argInfo), t, r) |> Some
        | _ -> None
    | "MakeTuple" ->
        Some args.Head
    | _ -> None

let curryExprAtRuntime t arity (expr: Expr) =
    Helper.CoreCall("Util", "curry", t, [makeIntConst arity; expr])

let uncurryExprAtRuntime t arity (expr: Expr) =
    Helper.CoreCall("Util", "uncurry", t, [makeIntConst arity; expr])

let partialApplyAtRuntime t arity (fn: Expr) (args: Expr list) =
    let args = NewArray(ArrayValues args, Any) |> Value
    Helper.CoreCall("Util", "partialApply", t, [makeIntConst arity; fn; args])

let tryReplaceInterface t (e: Expr) interfaceName =
    match interfaceName, e.Type with
    // CompareTo method is attached to prototype
    | Types.comparable, _ -> Some e
    | Types.enumerable, _ -> toSeq t e |> Some
    // These types in fable-core (or native JS) have methods attached to prototype
    | _, Builtin(BclTimeSpan | BclTimer | BclHashSet _ | BclDictionary _) -> Some e
    | _ -> None

let tryField returnTyp ownerTyp fieldName =
    match ownerTyp, fieldName with
    | Number Decimal, "Zero" -> makeDecConst 0M |> Some
    | Number Decimal, "One" -> makeDecConst 1M |> Some
    | String, "Empty" -> makeStrConst "" |> Some
    | Builtin BclGuid, "Empty" -> makeStrConst "00000000-0000-0000-0000-000000000000" |> Some
    | Builtin BclTimeSpan, "Zero" -> makeIntConst 0 |> Some
    | Builtin BclDateTime, ("MaxValue" | "MinValue") ->
        Helper.CoreCall(coreModFor BclDateTime, Naming.lowerFirst fieldName, returnTyp, []) |> Some
    | Builtin BclDateTimeOffset, ("MaxValue" | "MinValue") ->
        Helper.CoreCall(coreModFor BclDateTimeOffset, Naming.lowerFirst fieldName, returnTyp, []) |> Some
    | _ -> None

let private replacedModules =
  dict [
    "System.Math", operators
    "Microsoft.FSharp.Core.Operators", operators
    "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators", operators
    "Microsoft.FSharp.Core.ExtraTopLevelOperators", operators
    "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions", intrinsicFunctions
    "Microsoft.FSharp.Core.Operators.OperatorIntrinsics", intrinsicFunctions
    "Microsoft.FSharp.Core.LanguagePrimitives", languagePrimitives
    "System.Char", chars
    "System.String", strings
    "Microsoft.FSharp.Core.StringModule", strings
    "System.Array", arrays
    "Microsoft.FSharp.Collections.ArrayModule", arrayModule
    "Microsoft.FSharp.Collections.FSharpList`1", lists
    "Microsoft.FSharp.Collections.ListModule", lists
    "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers", seqs
    "Microsoft.FSharp.Collections.SeqModule", seqs
    "System.Collections.Generic.KeyValuePair`2", keyValuePairs
    Types.dictionary, dictionaries
    Types.idictionary, dictionaries
    Types.enumerable, enumerables
    "System.Collections.IEnumerable", enumerables
    "System.Collections.Generic.Dictionary`2.ValueCollection", enumerables
    "System.Collections.Generic.Dictionary`2.KeyCollection", enumerables
    "System.Collections.Generic.Dictionary`2.Enumerator", enumerators
    "System.Collections.Generic.Dictionary`2.ValueCollection.Enumerator", enumerators
    "System.Collections.Generic.Dictionary`2.KeyCollection.Enumerator", enumerators
    "System.Collections.Generic.List`1.Enumerator", enumerators
    "System.Collections.Generic.List`1", resizeArrays
    "System.Collections.Generic.IList`1", resizeArrays
    "System.Collections.Generic.ICollection`1", resizeArrays
    Types.hashset, hashSets
    Types.iset, hashSets
    "Microsoft.FSharp.Core.FSharpOption`1", options
    "Microsoft.FSharp.Core.OptionModule", options
    "Microsoft.FSharp.Core.ResultModule", results
    "System.Decimal", decimals
    // "System.Numerics.BigInteger", bigint
    // "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI", bigint
    Types.reference, references
    "Microsoft.FSharp.Core.Operators.Unchecked", unchecked
    "System.Object", objects
    "System.Enum", enums
    "System.BitConverter", bitConvert
    "System.Int32", parse Parse2Int
    "System.Single", parse Parse2Float
    "System.Double", parse Parse2Float
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
    "System.Activator", activator
    "System.Text.RegularExpressions.Capture", regex
    "System.Text.RegularExpressions.Match", regex
    "System.Text.RegularExpressions.Group", regex
    "System.Text.RegularExpressions.MatchCollection", regex
    "System.Text.RegularExpressions.GroupCollection", regex
    "System.Text.RegularExpressions.Regex", regex
    Types.fsharpSet, sets
    "Microsoft.FSharp.Collections.SetModule", setModule
    Types.fsharpMap, maps
    "Microsoft.FSharp.Collections.MapModule", mapModule
    "Microsoft.FSharp.Control.FSharpMailboxProcessor`1", mailbox
    "Microsoft.FSharp.Control.FSharpAsyncReplyChannel`1", mailbox
    "Microsoft.FSharp.Control.FSharpAsyncBuilder", asyncBuilder
    "Microsoft.FSharp.Control.FSharpAsync", asyncs
    "System.Guid", guids
    "System.Uri", uris
    "System.Lazy`1", laziness
    "Microsoft.FSharp.Control.Lazy", laziness
    "Microsoft.FSharp.Control.LazyExtensions", laziness
    "Microsoft.FSharp.Control.CommonExtensions", controlExtensions
    "Microsoft.FSharp.Control.FSharpEvent`1", events
    "Microsoft.FSharp.Control.EventModule", events
    "Microsoft.FSharp.Control.ObservableModule", observable
]

let tryCall (com: ICompiler) (ctx: Context) r t (info: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match info.DeclaringEntityFullName with
    | Patterns.DicContains replacedModules replacement -> replacement com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings" -> errorStrings info.CompiledName
    | "Microsoft.FSharp.Core.PrintfModule"
    | Naming.StartsWith "Microsoft.FSharp.Core.PrintfFormat" _ -> fsFormat com ctx r t info thisArg args
    | Naming.StartsWith "Fable.Core." _ -> fableCoreLib com ctx r t info thisArg args
    | Naming.EndsWith "Exception" _ -> exceptions com ctx r t info thisArg args
    | "System.Timers.ElapsedEventArgs" -> thisArg // only signalTime is available here
    | Naming.StartsWith "System.Action" _
    | Naming.StartsWith "System.Func" _
    | Naming.StartsWith "Microsoft.FSharp.Core.FSharpFunc" _
    | Naming.StartsWith "Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc" _ -> funcs com ctx r t info thisArg args
    // | "System.Type" -> types com info
    // | "Microsoft.FSharp.Reflection.FSharpType" -> fsharpType info.CompiledName com r t info thisArg args
    // | "Microsoft.FSharp.Reflection.FSharpValue" -> fsharpValue info.CompiledName com r t info thisArg args
    // | "Microsoft.FSharp.Reflection.FSharpReflectionExtensions" ->
    //     // In netcore F# Reflection methods become extensions
    //     // with names like `FSharpType.GetExceptionFields.Static`
    //     let isFSharpType = info.CompiledName.StartsWith("FSharpType")
    //     let methName = info.CompiledName |> Naming.extensionMethodName
    //     if isFSharpType
    //     then fsharpType methName com r t info thisArg args
    //     else fsharpValue methName com r t info thisArg args
    // | "Microsoft.FSharp.Reflection.UnionCaseInfo"
    // | "System.Reflection.PropertyInfo"
    // | "System.Reflection.MemberInfo" ->
    //     match thisArg, info.CompiledName with
    //     | _, "GetFields" -> Helper.InstanceCall(thisArg.Value, "getUnionFields", t, args) |> Some
    //     | Some c, "Name" -> Helper.CoreCall("Reflection", "getName", t, [c]) |> Some
    //     | Some c, ("Tag" | "PropertyType") ->
    //         let prop =
    //             if info.CompiledName = "Tag" then "Index" else info.CompiledName
    //             |> makeStrConst
    //         getExpr r t c prop |> Some
    //     | _ -> None
    | _ -> None
