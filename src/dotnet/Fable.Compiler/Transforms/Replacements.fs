module Fable.Transforms.Replacements

open Fable
open Fable.AST
open Fable.AST.Fable
open Microsoft.FSharp.Compiler.SourceCodeServices
open Patterns

type ICompiler = Fable.Transforms.FSharp2Fable.IFableCompiler

module private Helpers =
    let resolveArgTypes argTypes genArgs =
        argTypes |> List.map (function
            | GenericParam name as t ->
                Map.tryFind name genArgs |> Option.defaultValue t
            | t -> t)

    let coreCall r t (info: CallInfo) coreModule coreMember thisArg args =
        let argInfo = { ThisArg = thisArg; Args = args; ArgTypes = Some info.ArgTypes; Spread = NoSpread }
        let funcExpr = Import(coreMember, coreModule, CoreLib, Any)
        Operation(Call(StaticCall funcExpr, argInfo), t, r)

    let coreCall_ t coreModule coreMember args =
        let funcExpr = Import(coreMember, coreModule, CoreLib, Any)
        let argInfo = { ThisArg = None; Args = args; ArgTypes = None; Spread = NoSpread }
        Operation(Call(StaticCall funcExpr, argInfo), t, None)

    let globalCall r t info ident memb thisArg args =
        let argInfo = { ThisArg = thisArg; Args = args; ArgTypes = Some info.ArgTypes; Spread = NoSpread }
        let funcExpr =
            match memb with
            | Some m -> get None Any (makeIdentExpr ident) m
            | None -> makeIdentExpr ident
        Operation(Call(StaticCall funcExpr, argInfo), t, r)

    let globalCall_ t ident memb args =
        let argInfo = { ThisArg = None; Args = args; ArgTypes = None; Spread = NoSpread }
        let funcExpr =
            match memb with
            | Some m -> get None Any (makeIdentExpr ident) m
            | None -> makeIdentExpr ident
        Operation(Call(StaticCall funcExpr, argInfo), t, None)

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

    let tryGenArg (com: ICompiler) r (name: string) (genArgs: Map<string,Type>) =
        match Map.tryFind name genArgs with
        | Some t -> t
        | None ->
            "Couldn't find generic " + name |> addError com r
            Any

    let trySingleGenArg (com: ICompiler) r (genArgs: Map<string,Type>) =
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
        | _ -> None
        | _ -> None
    | _ -> None

let (|Integer|Float|) = function
    | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 -> Integer
    | Float32 | Float64 | Decimal -> Float

let coreModFor = function
    | BclGuid -> "String"
    | BclTimeSpan -> "Long"
    | BclDateTime -> "Date"
    | BclDateTimeOffset -> "DateOffset"
    | BclInt64 -> "Long"
    | BclUInt64 -> "Long"
    | BclBigInt -> "BigInt"

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
        failwith "TODO: DeclaredType type test"
        // if ent.IsClass
        // then jsInstanceof (TypeRef ent) expr
        // else "Cannot type test interfaces, records or unions"
        //      |> addErrorAndReturnNull com fileName range
    | Option _ | GenericParam _ | ErasedUnion _ ->
        "Cannot type test options, generic parameters or erased unions"
        |> addErrorAndReturnNull com range

let applyOp (com: ICompiler) r t (i: CallInfo) (args: Expr list) =
    let (|CustomOp|_|) com opName argTypes =
        let tryFindMember com (ent: FSharpEntity) opName argTypes =
            FSharp2Fable.TypeHelpers.tryFindMember com ent opName false argTypes
        match argTypes with
        | [DeclaredType(ent,_) as t] -> tryFindMember com ent opName [t]
        | [DeclaredType(ent,_) as t1; t2] ->
            match tryFindMember com ent opName [t1; t2], t2 with
            | Some m, _ -> Some m
            | None, DeclaredType(ent,_) -> tryFindMember com ent opName [t1; t2]
            | None, _ -> None
        | _ -> None
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
            let div = binOp BinaryDivide left right
            match argTypes with
            // Floor result of integer divisions (see #172)
            // Apparently ~~ is faster than Math.floor (see https://coderwall.com/p/9b6ksa/is-faster-than-math-floor)
            | Number Integer::_ -> unOp UnaryNotBitwise div |> unOp UnaryNotBitwise
            | _ -> div
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
    // TODO: Builtin types
    | Builtin(BclInt64|BclUInt64|BclBigInt|BclDateTime|BclDateTimeOffset as bt)::_ ->
        coreCall r t i (coreModFor bt) i.CompiledName None args
    // TODO: Check if the function is inlined
    | CustomOp com i.CompiledName m ->
        let argInfo = { ThisArg = None; Args = args; ArgTypes = Some i.ArgTypes; Spread = NoSpread }
        FSharp2Fable.Util.memberRef com m
        |> staticCall r t argInfo
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

let operators (com: ICompiler) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let math r t (i: CallInfo) (args: Fable.Expr list) methName =
        let argTypes = resolveArgTypes i.ArgTypes i.GenericArgs
        match Naming.lowerFirst methName, List.head argTypes with
        | "abs", Builtin(BclInt64 | BclBigInt as bt)  ->
            coreCall r t i (coreModFor bt) "abs" thisArg args |> Some
         | methName, _ -> globalCall r t i "Math" (Some methName) thisArg args |> Some
    match i.CompiledName, args with
    // Erased operators
    | "Box", _ | "Unbox", _ | "Identity", _ | "Ignore", _ -> List.tryHead args
    // TODO: Number conversions
    | "ToDouble", _ | "ToInt", _ -> List.tryHead args
    | "Raise", [arg] -> Throw(arg, t, r) |> Some
    | "FailWith", [msg] | "InvalidOp", [msg] ->
        Throw(error msg, t, r) |> Some
    | "InvalidArg", [argName; msg] ->
        let msg = add (add msg (s "\\nParameter name: ")) argName
        Throw(error msg, t, r) |> Some
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
    //         | ((Type (Fable.ExtendedNumber _ as t)) as arg)::_ ->
    //             toFloat arg.Range t (Fable.Number Float64) [arg] |> List.singleton
    //         | _ -> args
    //     coreCall r t i "Util" "sign" args |> Some
    | "Fst", [Value(NewTuple(fst::_))] -> Some fst
    | "Fst", [tup] -> Get(tup, TupleGet 0, t, r) |> Some
    | "Snd", [Value(NewTuple(_::snd::_))] -> Some snd
    | "Snd", [tup] -> Get(tup, TupleGet 1, t, r) |> Some
    // Reference
    | "op_Dereference", [arg] -> get r t arg "contents" |> Some
    | "op_ColonEquals", [o; v] -> Fable.Set(o, makeStrConst "contents" |> ExprSet, v, r) |> Some
    | "Ref", [arg] -> ObjectExpr(["contents", arg, ObjectValue false], t) |> Some
    | "Not", [operand] -> // TODO: Check custom operator?
        makeUnOp r t operand UnaryNot |> Some
    // Concatenates two lists
    | "op_Append", _ -> coreCall r t i "List" "Append" thisArg args |> Some
    // TODO: Structural equality
    | SetContains Operators.equalitySet as opName, [left; right] ->
        equality r opName left right |> Some
    | SetContains Operators.standardSet, _ ->
        applyOp com r t i args |> Some
    | _ -> None

let arrays (com: ICompiler) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, args, i.CompiledName with
    | Some c, _, "get_Length" -> get r t c "length" |> Some
    | None, [arg], "Length" -> get r t arg "length" |> Some
    | None, [ar; idx], "Get" -> getExpr r t ar idx |> Some
    | None, [ar; idx; value], "Set" -> Fable.Set(ar, ExprSet idx, value, r) |> Some
    // dynamicSet r ar idx value |> Some
    | None, _, meth ->
        let arrayCons =
            match t with
            | Fable.Array(Fable.Number numberKind) when com.Options.typedArrays ->
                getTypedArrayName com numberKind |> makeIdentExpr
            | _ -> makeIdentExpr "Array"
        // TODO: Only append array constructor when needed
        coreCall r t i "Array" (Naming.lowerFirst meth) thisArg (args@[arrayCons]) |> Some
    | _ -> None

let lists (com: ICompiler) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, args, i.CompiledName with
    | None, [x], "Head"
    | Some x, _, "get_Head" -> Get(x, ListHead, t, r) |> Some
    | None, [x], "Tail"
    | Some x, _, "get_Tail" -> Get(x, ListTail, t, r) |> Some
    | _, _, "get_Length" -> coreCall r t i "List" "Length" thisArg args |> Some
    | Some x, _, "get_Item" -> coreCall r t i "List" "Item" None (args@[x]) |> Some
    | Some x, _, "get_IsEmpty" -> Test(x, ListTest false, r) |> Some
    | None, _, ("get_Empty" | "Empty") ->
        NewList(None, trySingleGenArg com r i.GenericArgs) |> Value |> Some
    | None, [h;t], "Cons" -> NewList(Some(h,t), trySingleGenArg com r i.GenericArgs) |> Value |> Some
    | None, _, meth -> coreCall r t i "List" meth None args |> Some
    | _ -> None

let options (_com: ICompiler) r (t: Type) (i: CallInfo) (thisArg: Expr option) (_args: Expr list) =
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

// let bigint (com: ICompiler) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
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
    | "SetArray", [ar; idx; value] -> Fable.Set(ar, ExprSet idx, value, r) |> Some
    | _ -> None
//         match i.memberName, (i.thisArg, i.args) with
//         | ("getArraySlice" | "getStringSlice"), ThreeArgs (ar, lower, upper) ->
//             let upper =
//                 let t = Fable.Number Int32
//                 match upper with
//                 | Null _ -> makeGet None t ar (makeStrConst "length")
//                 | _ -> Fable.Apply(Fable.Value(Fable.BinaryOp BinaryPlus),
//                                 [upper; makeIntConst 1], Fable.ApplyMeth, t, None)
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
//             // Fable.Apply (typRef, args, Fable.ApplyCons, i.returnType, i.range) |> Some
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
    | "AreEqual", _ -> coreCall r t i "Assert" "equal" thisArg args |> Some
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

let tryCall (com: ICompiler) r t (info: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match info.DeclaringEntityFullName with
    | "System.Math"
    | "Microsoft.FSharp.Core.Operators"
    | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators"
    | "Microsoft.FSharp.Core.ExtraTopLevelOperators" -> operators com r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions"
    | "Microsoft.FSharp.Core.Operators.OperatorIntrinsics" -> intrinsicFunctions com r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.ErrorStrings" -> errorStrings info.CompiledName
    | "Microsoft.FSharp.Core.LanguagePrimitives" -> languagePrimitives com r t info thisArg args
    | "System.Array"
    | "Microsoft.FSharp.Collections.ArrayModule" -> arrays com r t info thisArg args
    | "Microsoft.FSharp.Collections.FSharpList`1"
    | "Microsoft.FSharp.Collections.ListModule" -> lists com r t info thisArg args
    | "Microsoft.FSharp.Core.FSharpOption`1"
    | "Microsoft.FSharp.Core.OptionModule" -> options com r t info thisArg args
    | "System.Decimal" -> decimals com r t info thisArg args
    // | "System.Numerics.BigInteger"
    // | "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI" -> bigint com r t info thisArg args
    | Naming.StartsWith "Fable.Core." _ -> fableCoreLib com r t info thisArg args
    | _ -> None
//         | Naming.EndsWith "Exception" _ -> exceptions com info
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
//         | "Microsoft.FSharp.Collections.SeqModule" -> collectionsSecondPass com info Seq
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
