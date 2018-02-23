module Fable.AST.Fable.Util

open Fable
open Fable.AST

// let (|CoreMeth|_|) coreMod meth expr =
//     match expr with
//     | Call(Callee(Import(meth',coreMod',CoreLib,_)),None,args,_,_,_)
//         when meth' = meth && coreMod' = coreMod ->
//         Some args
//     | _ -> None

let addWarning (com: ICompiler) (range: SourceLocation option) (warning: string) =
    com.AddLog(warning, Severity.Warning, ?range=range, fileName=com.CurrentFile)

let addError (com: ICompiler) (range: SourceLocation option) (warning: string) =
    com.AddLog(warning, Severity.Error, ?range=range, fileName=com.CurrentFile)

let addErrorAndReturnNull (com: ICompiler) (range: SourceLocation option) (error: string) =
    com.AddLog(error, Severity.Error, ?range=range, fileName=com.CurrentFile)
    Null Any |> Value

/// When referenced multiple times, is there a risk of double evaluation?
let hasDoubleEvalRisk = function
    | IdentExpr _
    // TODO: Add Union and List Getters here?
    | Value(This _ | Null _ | UnitConstant | NumberConstant _
                | StringConstant _ | BoolConstant _ | Enum _) -> false
    | Get(_,kind,_,_) ->
        match kind with
        // OptionValue has a runtime check
        | ListHead | ListTail | TupleGet _
        | UnionTag _ | UnionField _ -> false
        | _ -> true
    | _ -> true

let attachRange (range: SourceLocation option) msg =
    match range with
    | Some range -> msg + " " + (string range)
    | None -> msg

let attachRangeAndFile (range: SourceLocation option) (fileName: string) msg =
    match range with
    | Some range -> msg + " " + (string range) + " (" + fileName + ")"
    | None -> msg + " (" + fileName + ")"

let makeIdent name =
    { Name = name
      Type = Any
      IsMutable = false
      Range = None }

let makeTypedIdent typ name =
    { makeIdent name with Type = typ }

let makeLoop range loopKind = Loop (loopKind, range)

let makeCoreRef t modname prop =
    Import(prop, modname, CoreLib, t)

let makeImport t (selector: string) (path: string) =
    Import(selector.Trim(), path.Trim(), CustomImport, t)

let makeBinOp range typ left right op =
    Operation(BinaryOperation(op, left, right), typ, range)

let makeUnOp range typ arg op =
    Operation(UnaryOperation(op, arg), typ, range)

let makeLogOp range left right op =
    Operation(LogicalOperation(op, left, right), Boolean, range)

let makeEqOp range left right op =
    Operation(BinaryOperation(op, left, right), Boolean, range)

let makeIndexGet range typ callee idx =
    Get(callee, IndexGet idx, typ, range)

let makeFieldGet range typ callee field =
    Get(callee, FieldGet field, typ, range)

let makeUntypedFieldGet callee field =
    Get(callee, FieldGet field, Any, None)

let makeArray elementType arrExprs =
    NewArray(ArrayValues arrExprs, elementType) |> Value

let makeCall r t (applied: Expr) args =
    let callInfo =
        { ArgTypes =
            match applied.Type with
            | Fable.FunctionType(Fable.LambdaType arg, _) -> [arg]
            | Fable.FunctionType(Fable.DelegateType args, _) -> args
            | _ -> [Any]
          IsConstructor = false
          IsDynamic = false
          HasSpread = false
          HasThisArg = false }
    Operation(Call(applied, None, args, callInfo), t, r)

/// Dynamic calls will uncurry its function arguments with unknown arity
let makeDynamicCall r (applied: Expr) args =
    let callInfo =
        { ArgTypes = []
          IsConstructor = false
          IsDynamic = true
          HasSpread = false
          HasThisArg = false }
    Operation(Call(applied, None, args, callInfo), Fable.Any, r)


let makeLongInt (x: uint64) unsigned =
    let t = ExtendedNumber(if unsigned then UInt64 else Int64)
    let lowBits = NumberConstant (float (uint32 x), Float64)
    let highBits = NumberConstant (float (x >>> 32), Float64)
    let unsigned = BoolConstant (unsigned)
    let args = [Value lowBits; Value highBits; Value unsigned]
    makeCall None t (makeCoreRef Any "Long" "fromBits") args

let makeBoolConst (x: bool) = BoolConstant x |> Value
let makeStrConst (x: string) = StringConstant x |> Value
let makeIntConst (x: int) = NumberConstant (float x, Int32) |> Value
let makeNumConst (x: float) = NumberConstant (float x, Float64) |> Value
let makeDecConst (x: decimal) = NumberConstant (float x, Float64) |> Value

let makeFloat32 (x: float32) =
    let args = [NumberConstant (float x, Float32) |> Value]
    let callee = makeUntypedFieldGet (IdentExpr(makeIdent "Math")) "fround"
    makeCall None (Number Float32) callee args

let makeTypeConst (typ: Type) (value: obj) =
    match typ, value with
    // Long Integer types
    | ExtendedNumber Int64, (:? int64 as x) -> makeLongInt (uint64 x) false
    | ExtendedNumber UInt64, (:? uint64 as x) -> makeLongInt x true
    // Decimal type
    | ExtendedNumber Decimal, (:? decimal as x) -> makeDecConst x
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

let getTypedArrayName (com: ICompiler) numberKind =
    match numberKind with
    | Int8 -> "Int8Array"
    | UInt8 -> if com.Options.clampByteArrays then "Uint8ClampedArray" else "Uint8Array"
    | Int16 -> "Int16Array"
    | UInt16 -> "Uint16Array"
    | Int32 -> "Int32Array"
    | UInt32 -> "Uint32Array"
    | Float32 -> "Float32Array"
    | Float64 -> "Float64Array"

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
    | Regex -> jsInstanceof (IdentExpr(makeIdent "RegExp")) expr
    | Number _ | EnumType _ -> jsTypeof "number" expr
    | ExtendedNumber (Int64|UInt64) -> jsInstanceof(makeCoreRef Any "Long" "default") expr
    | ExtendedNumber Decimal -> jsTypeof "number" expr
    | ExtendedNumber BigInt -> jsInstanceof (makeCoreRef Any "BigInt" "default") expr
    | FunctionType _ -> jsTypeof "function" expr
    | Array _ | Tuple _ | List _ ->
        makeCall None Boolean (makeCoreRef Any "Util" "isArray") [expr]
    | DeclaredType (ent, _) ->
        failwith "TODO: DeclaredType type test"
        // if ent.IsClass
        // then jsInstanceof (TypeRef ent) expr
        // else "Cannot type test interfaces, records or unions"
        //      |> addErrorAndReturnNull com fileName range
    | Option _ | GenericParam _ | ErasedUnion _ ->
        "Cannot type test options, generic parameters or erased unions"
        |> addErrorAndReturnNull com range

// /// Helper when we need to compare the types of the arguments applied to a method
// /// (concrete) with the declared argument types for that method (may be generic)
// /// (e.g. when resolving a TraitCall)
// let compareDeclaredAndAppliedArgs declaredArgs appliedArgs =
//     // Curried functions returning a generic can match a function
//     // with higher arity (see #1262)
//     let rec funcTypesEqual eq types1 types2 =
//         match types1, types2 with
//         | [], [] -> true
//         | [GenericParam _], _ -> true
//         | head1::rest1, head2::rest2 ->
//             eq head1 head2 && funcTypesEqual eq rest1 rest2
//         | _ -> false
//     let listsEqual eq li1 li2 =
//         if not(List.sameLength li1 li2)
//         then false
//         else List.fold2 (fun b x y -> if b then eq x y else false) true li1 li2
//     let rec argEqual x y =
//         match x, y with
//         | Option genArg1, Option genArg2
//         | Array genArg1, Array genArg2 ->
//             argEqual genArg1 genArg2
//         | Tuple genArgs1, Tuple genArgs2 ->
//             listsEqual argEqual genArgs1 genArgs2
//         | LambdaType (genArgs1, returnType1), LambdaType (genArgs2, returnType2) ->
//             match genArgs1, genArgs2 with
//             | [], [] -> argEqual returnType1 returnType2
//             | head1::rest1, head2::rest2 ->
//                 if argEqual head1 head2 then
//                     let types1 = rest1@[returnType1]
//                     let types2 = rest2@[returnType2]
//                     funcTypesEqual argEqual types1 types2
//                 else false
//             | _ -> false
//         | DeclaredType(ent1, genArgs1), DeclaredType(ent2, genArgs2) ->
//             ent1 = ent2 && listsEqual argEqual genArgs1 genArgs2
//         | GenericParam _, _ ->
//             true
//         | x, y -> x = y
//     listsEqual argEqual declaredArgs appliedArgs
