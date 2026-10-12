[<RequireQualifiedAccess>]
module Fable.Transforms.Rust.Replacements

#nowarn "1182"

open System
open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

type Context = FSharp2Fable.Context
type ICompiler = FSharp2Fable.IFableCompiler
type CallInfo = ReplaceCallInfo

// let partialApplyAtRuntime (com: Compiler) t arity (expr: Expr) (partialArgs: Expr list) =
//     let rec makeNestedLambda body args =
//         match args with
//         | [] -> body
//         | arg::restArgs ->
//             let body = Fable.Lambda(arg, body, None)
//             makeNestedLambda body restArgs
//     let makeArgIdent i typ = makeTypedIdent typ $"a{i}"
//     let argTypes, returnType = uncurryLambdaType arity [] t
//     let argIdents = argTypes |> List.mapi makeArgIdent
//     let args = argIdents |> List.map Fable.IdentExpr
//     let body = Helper.Application(expr, returnType, partialArgs @ args)
//     makeNestedLambda body (List.rev argIdents)

// let curryExprAtRuntime (com: Compiler) arity (expr: Expr) =
//     partialApplyAtRuntime com expr.Type arity expr []

// let uncurryExprAtRuntime (com: Compiler) t arity (expr: Expr) =
//     let argTypes, returnType =
//         match t with
//         | Fable.LambdaType(argType, returnType) -> uncurryLambdaType arity [] t
//         | Fable.DelegateType(argTypes, returnType) -> argTypes, returnType
//         | _ -> [], expr.Type
//     let makeArgIdent i typ = makeTypedIdent typ $"b{i}$"
//     let argIdents = argTypes |> List.mapi makeArgIdent
//     let args = argIdents |> List.map Fable.IdentExpr
//     let body = curriedApply None returnType expr args
//     Fable.Delegate(argIdents, body, None, Fable.Tags.empty)

let error com (msg: Expr) = msg

let coreModFor =
    function
    | BclGuid -> "Guid"
    | BclDateTime -> "DateTime"
    | BclDateTimeOffset -> "DateTimeOffset"
    | BclDateOnly -> "DateOnly"
    | BclTimeOnly -> "TimeOnly"
    | BclRune -> "Rune"
    | BclTimer -> "Timer"
    | BclTimeSpan -> "TimeSpan"
    | FSharpSet _ -> "Set"
    | FSharpMap _ -> "Map"
    | FSharpResult _ -> "Result"
    | FSharpChoice _ -> "Choice"
    | FSharpReference _ -> "Native"
    | BclHashSet _ -> "HashSet"
    | BclDictionary _ -> "HashMap"
    | BclKeyValuePair _ -> "Native"

let makeInstanceCall r t (i: CallInfo) callee memberName args =
    Helper.InstanceCall(callee, memberName, t, args, i.SignatureArgTypes, i.GenericArgs, ?loc = r)

let makeStaticLibCall com r t (i: CallInfo) moduleName memberName args =
    let isConstructor = (i.CompiledName = ".ctor" || i.CompiledName = ".cctor")

    Helper.LibCall(
        com,
        moduleName,
        memberName,
        t,
        args,
        i.SignatureArgTypes,
        i.GenericArgs,
        isModuleMember = false,
        isConstructor = isConstructor,
        ?loc = r
    )

let makeStaticMemberCall com r t (i: CallInfo) moduleName memberName args =
    let fullName = i.DeclaringEntityFullName

    let entityName =
        fullName.Substring(fullName.LastIndexOf(".", StringComparison.Ordinal) + 1)

    let memberName = entityName + "::" + memberName
    makeStaticLibCall com r t i moduleName memberName args

let makeStaticFieldCall com r t moduleName entityName memberName =
    let memberName = entityName + "::" + memberName
    Helper.LibCall(com, moduleName, memberName, t, [], ?isModuleMember = Some(false), ?loc = r)

let makeLibModuleCall com r t (i: CallInfo) moduleName memberName (thisArg: Expr option) (args: Expr list) =
    let args, argTypes =
        match thisArg with
        | Some c -> c :: args, c.Type :: i.SignatureArgTypes
        | None -> args, i.SignatureArgTypes

    Helper.LibCall(com, moduleName, memberName, t, args, argTypes, i.GenericArgs, ?loc = r)

let makeLibCall com r t (i: CallInfo) moduleName memberName args =
    Helper.LibCall(com, moduleName, memberName, t, args, i.SignatureArgTypes, ?loc = r)

let libCallThis com r t (i: CallInfo) moduleName memberName thisArg args =
    Helper.LibCall(com, moduleName, memberName, t, args, i.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)

let libCallCons com r t (i: CallInfo) moduleName memberName args =
    Helper.LibCall(com, moduleName, memberName, t, args, i.SignatureArgTypes, isConstructor = true, ?loc = r)

let libCallTyped com r t moduleName memberName args argTypes =
    Helper.LibCall(com, moduleName, memberName, t, args, argTypes, ?loc = r)

let libCall com r t moduleName memberName args =
    Helper.LibCall(com, moduleName, memberName, t, args, ?loc = r)

let libValue com r t moduleName memberName args =
    Helper.LibValue(com, moduleName, memberName, t) // TODO: range

let makeGlobalIdent (ident: string, memb: string, typ: Type) =
    makeTypedIdentExpr typ (ident + "::" + memb)

let makeUniqueIdent com ctx t name =
    FSharp2Fable.Helpers.getIdentUniqueName com ctx name |> makeTypedIdent t

let makeDecimal com r t (x: decimal) =
    let str = x.ToString(System.Globalization.CultureInfo.InvariantCulture)
    Helper.LibCall(com, "Decimal", "fromString", t, [ makeStrConst str ], isConstructor = true, ?loc = r)

let makeRef (value: Expr) =
    Operation(Unary(UnaryAddressOf, value), Tags.empty, value.Type, None)

let makeClone com r t (expr: Expr) =
    Helper.InstanceCall(expr, "clone", t, [], ?loc = r)

let getRefCell com r t (expr: Expr) =
    Helper.InstanceCall(expr, "get", t, [], ?loc = r) |> makeClone com r t

let setRefCell com r (expr: Expr) (value: Expr) =
    Set(expr, ValueSet, value.Type, value, r)

let makeRefCell com r genArg args =
    let typ = makeFSharpCoreType [ genArg ] Types.refCell
    Helper.LibCall(com, "Native", "refCell", typ, args, isConstructor = true, ?loc = r)

let makeRefCellFromValue com r (value: Expr) = makeRefCell com r value.Type [ value ]

let makeRefFromMutableValue com ctx r t (value: Expr) =
    Operation(Unary(UnaryAddressOf, value), Tags.empty, t, r)

let makeRefFromMutableField com ctx r t callee key =
    let value = Get(callee, FieldInfo.Create(key), t, r)
    Operation(Unary(UnaryAddressOf, value), Tags.empty, t, r)

// Mutable and public module values are compiled as functions
let makeRefFromMutableFunc com ctx r t (value: Expr) = value

let toNativeIndex expr = TypeCast(expr, UNativeInt.Number)

let toLowerFirstWithArgsCountSuffix (args: Expr list) meth =
    let argCount = List.length args - 1 // don't count first arg
    let meth = Naming.lowerFirst meth

    if argCount > 1 then
        meth + (string<int> argCount)
    else
        meth

// let kindIndex kind = //         0   1   2   3   4   5   6   7   8   9  10  11
//     match kind with  //         i8 i16 i32 i64  u8 u16 u32 u64 f32 f64 dec big
//     | Int8 -> 0      //  0 i8   -   -   -   -   +   +   +   +   -   -   -   +
//     | Int16 -> 1     //  1 i16  +   -   -   -   +   +   +   +   -   -   -   +
//     | Int32 -> 2     //  2 i32  +   +   -   -   +   +   +   +   -   -   -   +
//     | Int64 -> 3     //  3 i64  +   +   +   -   +   +   +   +   -   -   -   +
//     | UInt8 -> 4     //  4 u8   +   +   +   +   -   -   -   -   -   -   -   +
//     | UInt16 -> 5    //  5 u16  +   +   +   +   +   -   -   -   -   -   -   +
//     | UInt32 -> 6    //  6 u32  +   +   +   +   +   +   -   -   -   -   -   +
//     | UInt64 -> 7    //  7 u64  +   +   +   +   +   +   +   -   -   -   -   +
//     | Float32 -> 8   //  8 f32  +   +   +   +   +   +   +   +   -   -   -   +
//     | Float64 -> 9   //  9 f64  +   +   +   +   +   +   +   +   -   -   -   +
//     | Decimal -> 10  // 10 dec  +   +   +   +   +   +   +   +   -   -   -   +
//     | BigInt -> 11   // 11 big  +   +   +   +   +   +   +   +   +   +   +   -
//     | Float16 -> FableError "Casting to/from float16 is unsupported" |> raise
//     | Int128 | UInt128 -> FableError "Casting to/from (u)int128 is unsupported" |> raise
//     | NativeInt | UNativeInt -> FableError "Casting to/from (u)nativeint is unsupported" |> raise

// let needToCast fromKind toKind =
//     let v = kindIndex fromKind // argument type (vertical)
//     let h = kindIndex toKind   // return type (horizontal)
//     ((v > h) || (v < 4 && h > 3)) && (h < 8) || (h <> v && (h = 11 || v = 11))

let convertTo com (ctx: Context) r t (args: Expr list) =
    let sourceType = args.Head.Type

    match t with
    | Boolean ->
        match sourceType with
        | Boolean -> args.Head
        | Number(Decimal, _) -> libCall com r t "Decimal" "toBoolean" args
        | Number(BigInt, _) -> libCall com r t "BigInt" "toBoolean" args
        | Number(_kind, _) -> libCall com r t "Convert" "toBoolean" args
        | Char -> libCall com r t "Convert" "toBoolean" args
        | String -> libCall com r t "Convert" "parseBoolean" args
        | _ ->
            addWarning com ctx.InlinePath r "Unsupported conversion"
            TypeCast(args.Head, t)

    | Char ->
        match sourceType with
        | Char -> args.Head
        | String -> libCall com r t "Convert" "parseChar" args
        | Number(Decimal, _) -> libCall com r t "Decimal" "toChar" args
        | Number(BigInt, _) -> libCall com r t "BigInt" "toChar" args
        | Number(_kind, _) ->
            let code = TypeCast(args.Head, UInt32.Number)
            libCall com r t "Char" "fromCharCode" [ code ]
        | _ ->
            addWarning com ctx.InlinePath r "Unsupported conversion"
            TypeCast(args.Head, t)

    | Number(Decimal, _) ->
        match sourceType with
        | Array(Number(Int32, _), _) -> libCall com r t "Decimal" "fromIntArray" args
        | Boolean -> libCall com r t "Decimal" "fromBoolean" args
        | Char -> libCall com r t "Decimal" "fromChar" args
        | String -> libCall com r t "Decimal" "fromString" args
        | Number(BigInt, _) -> libCall com r t "BigInt" "toDecimal" args
        | Number(kind, _) ->
            let meth = "from" + kind.ToString()
            libCall com r t "Decimal" meth args
        | _ ->
            addWarning com ctx.InlinePath r "Unsupported conversion"
            TypeCast(args.Head, t)

    | Number(BigInt, _) ->
        match sourceType with
        | Array(Number(UInt8, _), _) -> libCall com r t "BigInt" "fromByteArray" args
        | Boolean -> libCall com r t "BigInt" "fromBoolean" args
        | Char -> libCall com r t "BigInt" "fromChar" args
        | String -> libCall com r t "BigInt" "fromString" args
        | Number(kind, _) ->
            let meth = "from" + kind.ToString()
            libCall com r t "BigInt" meth args
        | _ ->
            addWarning com ctx.InlinePath r "Unsupported conversion"
            TypeCast(args.Head, t)

    | Number(kind, _) ->
        match sourceType with
        | Boolean ->
            // .NET Convert.ToXxx(bool) yields 0/1; route via i32 so float targets also work
            let code = TypeCast(args.Head, Int32.Number)
            TypeCast(code, t)
        | Char ->
            let code = TypeCast(args.Head, UInt32.Number)
            TypeCast(code, t)
        | String ->
            let meth = "to" + kind.ToString()
            libCall com r t "Convert" meth args
        | Number(Decimal, _) ->
            let meth = "to" + kind.ToString()
            libCall com r t "Decimal" meth args
        | Number(BigInt, _) ->
            let meth = "to" + kind.ToString()
            libCall com r t "BigInt" meth args
        | Number _ -> TypeCast(args.Head, t)
        | _ ->
            addWarning com ctx.InlinePath r "Unsupported conversion"
            TypeCast(args.Head, t)

    | _ ->
        addWarning com ctx.InlinePath r "Unsupported conversion"
        TypeCast(args.Head, t)

let rec toString com (ctx: Context) r (args: Expr list) =
    match args with
    | [] ->
        "toString is called with empty args"
        |> addErrorAndReturnNull com ctx.InlinePath r
    | head :: tail ->
        match head.Type with
        | String -> head
        | Char -> libCall com r String "String" "ofChar" [ head ]
        | Boolean -> libCall com r String "String" "ofBoolean" [ head ]
        // Rust has no Display for tuples, and the orphan rule rules out adding
        // one. Building the text here also matches .NET more closely than a
        // derived Debug would: each element goes through its own ToString, so a
        // string element is unquoted and a bool prints as True/False.
        | Tuple(genArgs, _) when not (List.isEmpty genArgs) ->
            let element i genArg =
                toString com ctx r [ Get(head, TupleIndex i, genArg, r) ]

            let separated =
                genArgs
                |> List.mapi element
                |> List.reduce (fun acc part -> add (add acc (makeStrConst ", ")) part)

            add (add (makeStrConst "(") separated) (makeStrConst ")")
        | Number(BigInt, _) -> libCall com r String "BigInt" "toString" args
        | Number(Decimal, _) -> libCall com r String "Decimal" "toString" args
        // | Array _ | List _ ->
        //     libCall com r String "Types" "seqToString" [ head ]
        // | DeclaredType(ent, _) when ent.IsFSharpUnion || ent.IsFSharpRecord || ent.IsValueType ->
        //     Helper.InstanceCall(head, "toString", String, [], ?loc=r)
        // | DeclaredType(ent, _) ->
        | _ -> libCall com r String "String" "toString" [ head ]

let toRoundInt com (ctx: Context) r t i (args: Expr list) =
    let sourceType = args.Head.Type

    let args =
        match sourceType with
        | Number((Float16 | Float32 | Float64 | Decimal), _) ->
            let rounded = makeInstanceCall r sourceType i args.Head "round" []
            rounded :: args.Tail
        | _ -> args

    convertTo com ctx r t args

let toRadixInt com (ctx: Context) r t i (args: Expr list) =
    match t with
    | Number(kind, _) ->
        let meth = "to" + kind.ToString() + "_radix"
        libCall com r t "Convert" meth args
    | _ -> FableError $"Unexpected conversion %s{i.CompiledName}" |> raise

let toArray com t (expr: Expr) =
    match expr.Type with
    | Array _ -> expr
    | List _ -> libCall com None t "List" "toArray" [ expr ]
    | String -> libCall com None t "String" "toCharArray" [ expr ]
    | IEnumerable -> libCall com None t "Seq" "toArray" [ expr ]
    | _ -> TypeCast(expr, t)

let toList com t (expr: Expr) =
    match expr.Type with
    | List _ -> expr
    | Array _ -> libCall com None t "List" "ofArray" [ expr ]
    | String ->
        let chars = libCall com None t "String" "toSeq" [ expr ]
        libCall com None t "List" "ofSeq" [ chars ]
    | IEnumerable -> libCall com None t "List" "ofSeq" [ expr ]
    | _ -> TypeCast(expr, t)

let toSeq com t (expr: Expr) =
    match expr.Type with
    | IEnumerable -> expr
    | List _ -> libCall com None t "Seq" "ofList" [ expr ]
    | Array _ -> libCall com None t "Seq" "ofArray" [ expr ]
    | Builtin(FSharpMap _) -> libCall com None t "Map" "toEnumerable" [ expr ]
    | Builtin(FSharpSet _) -> libCall com None t "Set" "toEnumerable" [ expr ]
    | String -> libCall com None t "String" "toSeq" [ expr ]
    | _ -> TypeCast(expr, t)

let emitRawString (s: string) = $"\"%s{s}\"" |> emitExpr None String []

let emitFormat (com: ICompiler) r t (args: Expr list) macro =
    let args =
        match args with
        | [] -> [ emitRawString "" ]
        | [ StringConst fmt; Value(NewArray(ArrayValues restArgs, _, _), _) ] -> (emitRawString fmt) :: restArgs
        | (StringConst fmt) :: restArgs -> (emitRawString fmt) :: restArgs
        | [ StringTempl(fmt, args); Value(NewArray(ArrayValues restArgs, _, _), _) ] ->
            (emitRawString fmt) :: args @ restArgs
        | (StringTempl(fmt, args)) :: restArgs -> (emitRawString fmt) :: args @ restArgs
        | [ ExprTypeAs(String, str); Value(NewArray(ArrayValues restArgs, _, _), _) ] ->
            (emitRawString "{0}") :: str :: restArgs
        | _ -> (emitRawString "{0}") :: args

    let unboxedArgs = args |> FSharp2Fable.Util.unboxBoxedArgs
    libCall com r t "String" macro unboxedArgs

let getMut expr =
    Helper.InstanceCall(expr, "get_mut", expr.Type, [])

let applyOp (com: ICompiler) (ctx: Context) r t opName (args: Expr list) =
    let unOp operator operand =
        Operation(Unary(operator, operand), Tags.empty, t, r)

    let binOp op left right =
        Operation(Binary(op, left, right), Tags.empty, t, r)

    let binOpChar op left right =
        let toUInt32 e =
            convertTo com ctx None UInt32.Number [ e ]

        Operation(Binary(op, toUInt32 left, toUInt32 right), Tags.empty, UInt32.Number, r)
        |> List.singleton
        |> convertTo com ctx r Char

    let truncateUnsigned operation = // see #1550
        match t with
        // | Number(UInt32,_) ->
        //     Operation(Binary(BinaryShiftRightZeroFill,operation,makeIntConst 0), t, r)
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
        | Operators.division, [ left; right ] -> binOp BinaryDivide left right
        | Operators.divideByInt, [ left; right ] -> libCallTyped com r t "Native" "divideByInt" [ left; right ] argTypes
        | Operators.modulus, [ left; right ] -> binOp BinaryModulus left right
        | Operators.leftShift, [ left; right ] -> binOp BinaryShiftLeft left right |> truncateUnsigned // See #1530
        | Operators.rightShift, [ left; right ] ->
            match argTypes with
            // | Number(UInt32,_)::_ -> binOp BinaryShiftRightZeroFill left right // See #646
            | _ -> binOp BinaryShiftRightSignPropagating left right
        | Operators.bitwiseAnd, [ left; right ] -> binOp BinaryAndBitwise left right |> truncateUnsigned
        | Operators.bitwiseOr, [ left; right ] -> binOp BinaryOrBitwise left right |> truncateUnsigned
        | Operators.exclusiveOr, [ left; right ] -> binOp BinaryXorBitwise left right |> truncateUnsigned
        | Operators.booleanAnd, [ left; right ] -> logicOp LogicalAnd left right
        | Operators.booleanOr, [ left; right ] -> logicOp LogicalOr left right
        | Operators.logicalNot, [ operand ] -> unOp UnaryNotBitwise operand |> truncateUnsigned
        | Operators.unaryNegation, [ operand ] -> unOp UnaryMinus operand
        | Operators.unaryPlus, [ operand ] -> unOp UnaryPlus operand
        | _ ->
            $"Operator %s{opName} not found in %A{argTypes}"
            |> addErrorAndReturnNull com ctx.InlinePath r

    let argTypes = args |> List.map (fun a -> a.Type)

    match argTypes with
    // | Number(BigInt as kind,_)::_ ->
    //     libCallTyped com r t "BigInt" opName [ left; right ] argTypes
    | Builtin(BclDateTime | BclDateTimeOffset | BclTimeOnly | BclTimeSpan | BclRune) :: _ ->
        nativeOp opName argTypes args
    | Builtin(FSharpSet _) :: _ ->
        let methName =
            match opName with
            | Operators.addition -> "union"
            | Operators.subtraction -> "difference"
            | _ -> opName

        libCallTyped com r t "Set" methName args argTypes
    // | Builtin (FSharpMap _)::_ ->
    //     let mangledName = Naming.buildNameWithoutSanitationFrom "FSharpMap" true opName overloadSuffix.Value
    //     libCallTyped com r t "Map" mangledName args argTypes
    | CustomOp com ctx r t opName args e -> e
    | _ -> nativeOp opName argTypes args

let isCompatibleWithNativeComparison =
    function
    | Boolean
    | Char
    | String
    | Number _
    | GenericParam _
    // | Array _
    // | List _
    | Builtin(BclGuid) -> true
    | Builtin(BclTimeSpan) -> true
    | Builtin(BclRune) -> true
    | _ -> false

// Overview of hash rules:
// * `hash`, `Unchecked.hash` first check if GetHashCode is implemented and then default to structural hash.
// * `.GetHashCode` called directly defaults to identity hash (for reference types except string) if not implemented.
// * `LanguagePrimitive.PhysicalHash` creates an identity hash no matter whether GetHashCode is implemented or not.

let referenceHash (com: ICompiler) ctx r (arg: Expr) =
    match arg.Type with
    | Boolean
    | Char
    | String
    | Number _ -> Helper.InstanceCall(arg, "getHashCode", Int32.Number, [], [], [], ?loc = r)
    | _ -> libCall com r Int32.Number "Native" "referenceHash" [ makeRef arg ]

let getHashCode (com: ICompiler) ctx r (arg: Expr) =
    match arg.Type with
    | HasReferenceEquality com _ -> referenceHash com ctx r arg
    | Builtin BclRune -> libCall com r Int32.Number "Rune" "getHashCode" [ arg ]
    | Char -> libCall com r Int32.Number "Char" "GetHashCode" [ arg ]
    | _ -> Helper.InstanceCall(arg, "getHashCode", Int32.Number, [], [], [], ?loc = r)

let objectHash (com: ICompiler) ctx r (arg: Expr) =
    match arg.Type with
    | Array _ -> referenceHash com ctx r arg
    | _ -> getHashCode com ctx r arg

let referenceEquals (com: ICompiler) ctx r (left: Expr) (right: Expr) =
    match left, right with
    | Value(Null _, _), o
    | o, Value(Null _, _) -> libCall com r Boolean "Native" "is_null" [ makeRef o ]
    | _ ->
        match left.Type with
        | Boolean
        | Char
        | String
        | Number _ -> makeEqOp r left right BinaryEqual
        | _ -> libCall com r Boolean "Native" "referenceEquals" [ makeRef left; makeRef right ]

let equals (com: ICompiler) ctx r (left: Expr) (right: Expr) =
    let t = Boolean

    match left.Type with
    | Boolean
    | Char
    | String
    | Number _
    | Builtin(FSharpChoice _ | FSharpResult _) -> makeEqOp r left right BinaryEqual
    | Builtin kind -> libCall com r t (coreModFor kind) "equals" [ left; right ]
    | Array(_, ResizeArray) -> referenceEquals com ctx r left right
    | Array _ -> libCall com r t "Array" "equals" [ left; right ]
    | List _ -> libCall com r t "List" "equals" [ left; right ]
    | IEnumerable -> libCall com r t "Seq" "equals" [ left; right ]
    // System.Type is erased to a boxed reflection object (dyn Any, no PartialEq),
    // so type-identity equality is compared on the carried TypeId at runtime.
    | MetaType -> libCall com r t "Reflection" "typeEquals" [ left; right ]
    | HasReferenceEquality com _ -> referenceEquals com ctx r left right
    | Nullable _ ->
        // transforms null checks into option tests
        match left, right with
        | expr, Value(NewOption(None, _, _), _) -> Test(expr, OptionTest false, r)
        | Value(NewOption(None, _, _), _), expr -> Test(expr, OptionTest false, r)
        | _ -> makeEqOp r left right BinaryEqual
    | _ ->
        // libCall com r t "Native" "equals" [ left; right ]
        makeEqOp r left right BinaryEqual

/// Compare function that will call Util.compare or instance `CompareTo` as appropriate
let compare (com: ICompiler) ctx r (left: Expr) (right: Expr) =
    let t = Int32.Number

    match left.Type with
    | Boolean
    | Char
    | String
    | Number _
    | Builtin(FSharpChoice _ | FSharpResult _) -> libCall com r t "Native" "compare" [ left; right ]
    | Builtin kind -> libCall com r t (coreModFor kind) "compareTo" [ left; right ]
    | Array _ -> libCall com r t "Array" "compareTo" [ left; right ]
    | List _ -> libCall com r t "List" "compareTo" [ left; right ]
    | IEnumerable -> libCall com r t "Seq" "compareTo" [ left; right ]
    | _ -> libCall com r t "Native" "compare" [ left; right ]

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
        | Operators.equality
        | "Eq" -> BinaryEqual
        | Operators.inequality
        | "Neq" -> BinaryUnequal
        | Operators.lessThan
        | "Lt" -> BinaryLess
        | Operators.lessThanOrEqual
        | "Lte" -> BinaryLessOrEqual
        | Operators.greaterThan
        | "Gt" -> BinaryGreater
        | Operators.greaterThanOrEqual
        | "Gte" -> BinaryGreaterOrEqual
        | _ -> FableError $"Unexpected operator %s{opName}" |> raise

    match op with
    | BinaryEqual -> equals com ctx r left right
    | BinaryUnequal ->
        match left.Type with
        | Boolean
        | Char
        | String
        | Number _ -> makeEqOp r left right BinaryUnequal
        | _ ->
            let expr = equals com ctx r left right
            makeUnOp None Boolean expr UnaryNot
    | _ -> booleanCompare com ctx r left right op

// let makeComparerFunction (com: ICompiler) ctx typArg =
//     let x = makeUniqueIdent com ctx typArg "x"
//     let y = makeUniqueIdent com ctx typArg "y"
//     let body = compare com ctx None (IdentExpr x) (IdentExpr y)
//     Delegate([x; y], body, None, Tags.empty)

// let makeComparer (com: ICompiler) ctx typArg =
//     objExpr ["Compare", makeComparerFunction com ctx typArg]

// let makeEqualityFunction (com: ICompiler) ctx typArg =
//     let x = makeUniqueIdent com ctx typArg "x"
//     let y = makeUniqueIdent com ctx typArg "y"
//     let body = equals com ctx None (IdentExpr x) (IdentExpr y)
//     Delegate([x; y], body, None, Tags.empty)

// let makeEqualityComparer (com: ICompiler) ctx typArg =
//     let x = makeUniqueIdent ctx typArg "x"
//     let y = makeUniqueIdent ctx typArg "y"
//     objExpr
//         [
//             "Equals", Delegate([ x; y ], equals com ctx None (IdentExpr x) (IdentExpr y), None, Tags.empty)
//             "GetHashCode", Delegate([ x ], getHashCode com ctx None (IdentExpr x), None, Tags.empty)
//         ]

// // TODO: Try to detect at compile-time if the object already implements `Compare`?
// let inline makeComparerFromEqualityComparer e =
//     e // leave it as is, if implementation supports it
//     // Helper.LibCall(com, "Util", "comparerFromEqualityComparer", Any, [e])

/// Adds comparer as last argument for set creator methods
let makeSet (com: ICompiler) ctx r t args genArg =
    // let args = args @ [makeComparer com ctx genArg]
    let meth =
        match args with
        | [] -> "empty"
        | [ ExprType(List _) ] -> "ofList"
        | [ ExprType(Array _) ] -> "ofArray"
        | _ -> "ofSeq"

    libCall com r t "Set" meth args

/// Adds comparer as last argument for map creator methods
let makeMap (com: ICompiler) ctx r t args genArg =
    // let args = args @ [makeComparer com ctx genArg]
    let meth =
        match args with
        | [] -> "empty"
        | [ ExprType(List _) ] -> "ofList"
        | [ ExprType(Array _) ] -> "ofArray"
        | _ -> "ofSeq"

    libCall com r t "Map" (Naming.lowerFirst meth) args

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
    | Nullable(genArg, true) -> NewOption(None, genArg, false) |> makeValue None
    | Nullable(genArg, false) -> Null t |> makeValue None
    | Boolean -> makeBoolConst false
    | Number(BigInt, _) -> libCall com None t "BigInt" "zero" []
    | Number(Decimal, _) -> libValue com None t "Decimal" "Zero" []
    | Number(kind, uom) -> NumberConstant(NumberValue.GetZero kind, uom) |> makeValue None
    | Char -> CharConstant '\u0000' |> makeValue None
    | String -> Null t |> makeValue None
    | Array(typ, _) -> makeArray typ []
    | List genArg -> NewList(None, genArg) |> makeValue None
    | Builtin BclDateTime -> libCall com None t "DateTime" "zero" []
    | Builtin BclDateTimeOffset -> libCall com None t "DateTimeOffset" "zero" []
    | Builtin BclDateOnly -> libCall com None t "DateOnly" "zero" []
    | Builtin BclTimeOnly -> libCall com None t "TimeOnly" "zero" []
    | Builtin BclRune -> libCall com None t "Rune" "zero" []
    | Builtin BclTimeSpan -> libValue com None t "TimeSpan" "zero" []
    | Builtin(FSharpSet genArg) -> makeSet com ctx None t [] genArg
    | Builtin BclGuid -> libValue com None t "Guid" "empty" []
    | Builtin(BclKeyValuePair(k, v)) -> makeTuple None true [ getZero com ctx k; getZero com ctx v ]
    // | ListSingleton(CustomOp com ctx None t "get_Zero" [] e) -> e
    | IsReferenceType com _ -> Null t |> makeValue None
    | _ -> libCall com None t "Native" "getZero" []

let getOne (com: ICompiler) (ctx: Context) (t: Type) =
    match t with
    | Boolean -> makeBoolConst true
    | Number(BigInt, _) -> libCall com None t "BigInt" "one" []
    | Number(Decimal, _) -> libValue com None t "Decimal" "One" []
    | Number(kind, uom) -> NumberConstant(NumberValue.GetOne kind, uom) |> makeValue None
    // | ListSingleton(CustomOp com ctx None t "get_One" [] e) -> e
    | _ -> makeIntConst 1

let makeAddFunction (com: ICompiler) ctx t =
    let x = makeUniqueIdent com ctx t "x"
    let y = makeUniqueIdent com ctx t "y"

    let body = applyOp com ctx None t Operators.addition [ IdentExpr x; IdentExpr y ]

    Delegate([ x; y ], body, None, Tags.empty)

// let makeGenericAdder (com: ICompiler) ctx t =
//     objExpr [
//         "GetZero", getZero com ctx t |> makeDelegate []
//         "Add", makeAddFunction com ctx t
//     ]

// let makeGenericAverager (com: ICompiler) ctx t =
//     let divideFn =
//         let x = makeUniqueIdent com ctx t "x"
//         let i = makeUniqueIdent com ctx (Int32.Number) "i"
//         let body = applyOp com ctx None t Operators.divideByInt [IdentExpr x; IdentExpr i]
//         Delegate([x; i], body, None, Tags.empty)
//     objExpr [
//         "GetZero", getZero com ctx t |> makeDelegate []
//         "Add", makeAddFunction com ctx t
//         "DivideByInt", divideFn
//     ]

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
//             | Types.icomparerGeneric ->
//                 args @ [makeComparer com ctx genArg]
//             | Types.iequalityComparer ->
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
    libCall com r t "Option" "tryOp" (op :: args)

let tryCoreOp com r t coreModule coreMember args =
    let op = libValue com r Any coreModule coreMember []
    tryOp com r t op args

let fableCoreLib (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.DeclaringEntityFullName, i.CompiledName with
    | _, UniversalFableCoreHelpers com ctx r t i args error expr -> Some expr
    | "Fable.Core.Reflection", meth -> libCall com r t "Reflection" meth args |> Some
    | "Fable.Core.Compiler", meth ->
        match meth with
        | "version" -> makeStrConst Literals.VERSION |> Some
        | "majorMinorVersion" ->
            try
                let m = Regex.Match(Literals.VERSION, @"^\d+\.\d+")
                float m.Value |> makeFloatConst |> Some
            with _ ->
                "Cannot parse compiler version"
                |> addErrorAndReturnNull com ctx.InlinePath r
                |> Some
        | "debugMode" -> makeBoolConst com.Options.DebugMode |> Some
        | "typedArrays" -> makeBoolConst com.Options.TypedArrays |> Some
        | "extension" -> makeStrConst com.Options.FileExtension |> Some
        | "isDotnet" -> makeBoolConst false |> Some
        | "isJavaScript" -> makeBoolConst (com.Options.Language = JavaScript) |> Some
        | "isTypeScript" -> makeBoolConst (com.Options.Language = TypeScript) |> Some
        | "isPython" -> makeBoolConst (com.Options.Language = Python) |> Some
        | "isDart" -> makeBoolConst (com.Options.Language = Dart) |> Some
        | "isRust" -> makeBoolConst (com.Options.Language = Rust) |> Some
        | "isPhp" -> makeBoolConst (com.Options.Language = Php) |> Some
        | "isBeam" -> makeBoolConst (com.Options.Language = Beam) |> Some
        | _ -> None
    | "Fable.Core.RustInterop", "op_BangHat" -> List.tryHead args
    | "Fable.Core.RustInterop", _ ->
        match i.CompiledName, args with
        | "emitRustExpr", [ args; RequireStringConstOrTemplate com ctx r template ] ->
            let args = destructureTupleArgs [ args ]
            emitTemplate r t args false template |> Some
        | _ -> None
    | "Fable.Core.Rust", _ ->
        match i.CompiledName, args with
        | "import", [ RequireStringConst com ctx r selector; RequireStringConst com ctx r path ] ->
            makeImportUserGenerated r t selector path |> Some
        | "importAll", [ RequireStringConst com ctx r path ] -> makeImportUserGenerated r t "*" path |> Some
        | _ -> None
    | _ -> None

let refCells (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "get_Value", Some c, _ -> getRefCell com r t c |> Some
    | "set_Value", Some c, [ value ] -> setRefCell com r c value |> Some
    | _ -> None

let getMemberName isStatic (i: CallInfo) =
    let memberName = i.CompiledName |> FSharp2Fable.Helpers.cleanNameAsRustIdentifier

    if String.IsNullOrEmpty(i.OverloadSuffix) then
        memberName
    else
        let sep =
            if isStatic then
                "__"
            else
                "_"

        memberName + sep + i.OverloadSuffix

let getModuleAndMemberName (i: CallInfo) (thisArg: Expr option) =
    let isStatic = Option.isNone thisArg
    let entFullName = i.DeclaringEntityFullName.Replace("Microsoft.", "")
    let pos = entFullName.LastIndexOf('.')
    let moduleName = entFullName.Substring(0, pos)

    let entityName =
        entFullName.Substring(pos + 1) |> FSharp2Fable.Helpers.cleanNameAsRustIdentifier

    let memberName =
        if isStatic then
            entityName + "::" + (getMemberName isStatic i)
        else
            getMemberName isStatic i

    moduleName, memberName

let bclType (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg with
    | Some c ->
        let memberName = getMemberName false i
        makeInstanceCall r t i c memberName args |> Some
    | None ->
        let moduleName, memberName = getModuleAndMemberName i thisArg
        makeStaticLibCall com r t i moduleName memberName args |> Some

let fsharpModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let moduleName, memberName = getModuleAndMemberName i thisArg

    makeLibCall com r t i moduleName memberName args |> Some

// Maps a .NET standard numeric format specifier (e.g. "X4", "D5", "F2") to a
// Rust format spec body { flags; width; precision; type } so it can be merged
// into a `format!` placeholder. Returns None for specifiers Rust's `format!`
// can't express (N, C, P, custom numeric patterns) so the value is emitted plain.
let private mapDotnetSpecToRust (spec: string) =
    match Regex.Match(spec, @"^([A-Za-z])(\d*)$") with
    | m when m.Success ->
        let typ = m.Groups[1].Value
        let n = m.Groups[2].Value
        // A width on hex/binary/octal/decimal means zero-pad to that width.
        let zeroPad =
            if n <> "" then
                "0"
            else
                ""

        match typ with
        | "X"
        | "x" -> Some(zeroPad, n, "", typ)
        | "B"
        | "b" -> Some(zeroPad, n, "", "b")
        | "O"
        | "o" -> Some(zeroPad, n, "", "o")
        | "D"
        | "d" -> Some(zeroPad, n, "", "")
        | "F"
        | "f" ->
            Some(
                "",
                "",
                "."
                + (if n = "" then
                       "2"
                   else
                       n),
                ""
            )
        | "E" ->
            Some(
                "",
                "",
                "."
                + (if n = "" then
                       "6"
                   else
                       n),
                "E"
            )
        | "e" ->
            Some(
                "",
                "",
                "."
                + (if n = "" then
                       "6"
                   else
                       n),
                "e"
            )
        | "G"
        | "g"
        | "R"
        | "r" -> Some("", "", "", "")
        | _ -> None
    | _ -> None

let makeRustFormatString interpolated (fmt: string) =
    let pattern1 =
        @"(?<pre>[^%]?)%(?<flags>[0+\- ]*)(?<width>\*|\d+)?(?<prec>\.\d+)?(?<type>\w)"

    let pattern2 =
        @"(?<pre>[^%]?)%(?<flags>[0+\- ]*)(?<width>\*|\d+)?(?<prec>\.\d+)?(?:P\((?<dotnet>[^)]*)\)|(?<type>\w)(?:%P\(\))?)"

    let pattern =
        if interpolated then
            pattern2
        else
            pattern1

    let input = fmt.Replace("{", "{{").Replace("}", "}}").Replace("%%", "%")

    let formatFlags (flags: string) =
        let sign =
            if flags.Contains("+") then
                "+"
            else
                ""

        if flags.Contains("-") then
            "<" + sign // left-align
        elif flags.Contains("0") then
            sign + "0" // zero padded
        else
            sign

    let mutable argCount = 0

    let rustFmt =
        Regex.Replace(
            input,
            pattern,
            fun m ->
                argCount <- argCount + 1
                let pre = m.Groups["pre"].Value
                let flags = m.Groups["flags"].Value |> formatFlags
                let width = m.Groups["width"].Value.Replace("*", "$") // width parameter
                let prec = m.Groups["prec"].Value

                let formatting =
                    if m.Groups["dotnet"].Success then
                        // .NET interpolation hole: %<align>P(<spec>) from e.g. $"{x,6:X4}"
                        let dotnet = m.Groups["dotnet"].Value

                        if dotnet = "" then
                            // Plain or alignment-only hole, e.g. %P() or %6P()
                            flags + width + prec
                        else
                            match mapDotnetSpecToRust dotnet with
                            | Some(specFlags, specWidth, specPrec, specType) ->
                                // An explicit alignment width takes the field width (space-padded);
                                // otherwise use the specifier's own (zero-padded) width.
                                let f =
                                    if width <> "" then
                                        flags
                                    else
                                        specFlags

                                let w =
                                    if width <> "" then
                                        width
                                    else
                                        specWidth

                                f + w + specPrec + specType
                            | None ->
                                // Unsupported specifier: keep any alignment, drop the rest.
                                flags + width + prec
                    else
                        let typ = m.Groups["type"].Value

                        let prec =
                            if String.IsNullOrEmpty(prec) && (typ = "f" || typ = "F") then
                                ".6"
                            else
                                prec

                        let typ =
                            match typ with
                            | "A" -> "?"
                            | "B" -> "b"
                            | ("b" | "c" | "d" | "i" | "s" | "u") -> ""
                            | ("o" | "x" | "X" | "e" | "E") as t -> t
                            | _ -> ""

                        flags + width + prec + typ

                if String.IsNullOrEmpty(formatting) then
                    pre + "{}"
                else
                    pre + "{:" + formatting + "}"
        )

    rustFmt, argCount

let makeRustFormatExpr com r t (fmt: string) args macro =
    let macroExpr = libValue com r Any "String" macro []
    let rustFmt, argCount = makeRustFormatString false fmt
    let argCount = argCount + 1 + (List.length args) // +1 is for fmt
    let applied = Extended(Curry(macroExpr, argCount), r)
    let unboxedArgs = args |> FSharp2Fable.Util.unboxBoxedArgs
    curriedApply r t applied (unboxedArgs @ [ emitRawString rustFmt ])

let fsFormat (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    // | "get_Value", Some c, _ ->
    //     c |> Some // TODO:
    | ("PrintFormatToString" | "PrintFormatToStringThen"), None, [ StringConst fmt ] ->
        "sprintf!" |> makeRustFormatExpr com r t fmt [] |> Some
    | ("PrintFormatToString" | "PrintFormatToStringThen"), None, [ MaybeCasted(template) ] -> template |> Some
    | ("PrintFormatThen" | "PrintFormatToStringThen"), None, [ cont; StringConst fmt ] ->
        "kprintf!" |> makeRustFormatExpr com r t fmt [ cont ] |> Some
    | ("PrintFormatThen" | "PrintFormatToStringThen"), None, [ cont; MaybeCasted(template) ] ->
        Helper.Application(cont, t, [ template ], ?loc = r) |> Some
    | "PrintFormatToError", None, [ StringConst fmt ] -> "eprintf!" |> makeRustFormatExpr com r t fmt [] |> Some
    | "PrintFormatToError", None, _ -> "eprintf!" |> emitFormat com r t args |> Some
    | "PrintFormatLineToError", None, [ StringConst fmt ] -> "eprintfn!" |> makeRustFormatExpr com r t fmt [] |> Some
    | "PrintFormatLineToError", None, _ -> "eprintfn!" |> emitFormat com r t args |> Some
    | "PrintFormat", None, [ StringConst fmt ] -> "printf!" |> makeRustFormatExpr com r t fmt [] |> Some
    | "PrintFormat", None, _ -> "printf!" |> emitFormat com r t args |> Some
    | "PrintFormatLine", None, [ StringConst fmt ] -> "printfn!" |> makeRustFormatExpr com r t fmt [] |> Some
    | "PrintFormatLine", None, _ -> "printfn!" |> emitFormat com r t args |> Some
    | "PrintFormatToTextWriter", None, [ StringConst fmt ] -> "printf!" |> makeRustFormatExpr com r t fmt [] |> Some
    | "PrintFormatToTextWriter", None, _ -> "printf!" |> emitFormat com r t args |> Some
    | "PrintFormatLineToTextWriter", None, [ StringConst fmt ] ->
        "printfn!" |> makeRustFormatExpr com r t fmt [] |> Some
    | "PrintFormatLineToTextWriter", None, _ -> "printfn!" |> emitFormat com r t args |> Some
    | "PrintFormatToStringThenFail", None, [ StringConst fmt ] ->
        "failwithf!" |> makeRustFormatExpr com r t fmt [] |> Some
    | "PrintFormatToStringThenFail", None, _ -> "failwithf!" |> emitFormat com r t args |> Some
    | "PrintFormatToStringBuilder", None, [ sb; StringConst fmt ] ->
        let cont = libCall com r t "Util" "bprintf" [ sb ]
        "kprintf!" |> makeRustFormatExpr com r t fmt [ cont ] |> Some
    | "PrintFormatToStringBuilder", None, [ sb; MaybeCasted(template) ] ->
        let cont = libCall com r t "Util" "bprintf" [ sb ]
        Helper.Application(cont, t, [ template ], ?loc = r) |> Some
    | "PrintFormatToStringBuilderThen", None, [ cont; sb; StringConst fmt ] ->
        let cont = libCall com r t "Util" "kbprintf" [ cont; sb ]
        "kprintf!" |> makeRustFormatExpr com r t fmt [ cont ] |> Some
    | "PrintFormatToStringBuilderThen", None, [ cont; sb; MaybeCasted(template) ] ->
        let cont = libCall com r t "Util" "kbprintf" [ cont; sb ]
        Helper.Application(cont, t, [ template ], ?loc = r) |> Some
    | ".ctor", _, (StringConst fmt) :: (Value(NewArray(ArrayValues templateArgs, _, _), _)) :: _ ->
        let rustFmt, _argCount = makeRustFormatString true fmt
        let unboxedArgs = templateArgs |> FSharp2Fable.Util.unboxBoxedArgs
        StringTemplate(None, [ rustFmt ], unboxedArgs) |> makeValue r |> Some
    | ".ctor", _, [ format ] -> format |> Some // just passing along the format
    | _ -> None

let operators (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let math r t (args: Expr list) argTypes methName =
        let meth = Naming.lowerFirst methName

        match args with
        | thisArg :: restArgs -> makeInstanceCall r t i thisArg meth restArgs
        | _ -> "Missing argument." |> addErrorAndReturnNull com ctx.InlinePath r

    match i.CompiledName, args with
    | ("DefaultArg" | "DefaultValueArg"), [ opt; defValue ] ->
        match opt with
        | MaybeInScope ctx (Value(NewOption(opt, _, _), _)) ->
            match opt with
            | Some value -> Some value
            | None -> Some defValue
        | _ -> makeLibCall com r t i "Option" "defaultArg" args |> Some
    | "DefaultAsyncBuilder", _ -> makeImportLib com t "singleton" "AsyncBuilder" |> Some
    // Erased operators.
    // Rust compiles KeyValuePair as a struct tuple, but the KeyValue active pattern expects a regular tuple.
    | "KeyValuePattern", [ arg ] ->
        match arg.Type with
        | Builtin(BclKeyValuePair(keyType, valueType)) ->
            makeTuple r false [ Get(arg, TupleIndex 0, keyType, r); Get(arg, TupleIndex 1, valueType, r) ]
            |> Some
        | _ -> TypeCast(arg, t) |> Some
    | ("Identity" | "Box" | "Unbox" | "ToEnum"), [ arg ] -> TypeCast(arg, t) |> Some
    // Cast to unit to make sure nothing is returned when wrapped in a lambda, see #1360
    | "Ignore", _ -> Value(UnitConstant, r) |> Some
    // Number and String conversions
    | ("ToSByte" | "ToByte" | "ToInt8" | "ToUInt8" | "ToInt16" | "ToUInt16" | "ToInt" | "ToUInt" | "ToInt32" | "ToUInt32" | "ToInt64" | "ToUInt64" | "ToIntPtr" | "ToUIntPtr"),
      [ arg ] -> convertTo com ctx r t args |> Some
    | ("ToSingle" | "ToDouble" | "ToDecimal"), [ arg ] -> convertTo com ctx r t args |> Some
    | "ToChar", [ arg ] -> convertTo com ctx r t args |> Some
    | "ToString", _ -> toString com ctx r args |> Some
    | "CreateSequence", [ xs ] -> toSeq com t xs |> Some
    | ("CreateDictionary" | "CreateReadOnlyDictionary"), [ arg ] ->
        libCall com r t "HashMap" "new_from_tuple_array" [ toArray com t arg ] |> Some
    | "CreateSet", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeSet com ctx r t args |> Some
    // Ranges
    | ("op_Range" | "op_RangeStep"), _ ->
        let genArg = genArg com ctx r 0 i.GenericArgs

        let addStep args =
            match args with
            | [ first; last ] -> [ first; getOne com ctx genArg; last ]
            | _ -> args

        let meth, args =
            match genArg with
            | Char -> "rangeChar", args
            | _ -> "rangeNumeric", addStep args

        makeLibCall com r t i "Range" meth args |> Some
    // Pipes and composition
    | "op_PipeRight", [ x; f ]
    | "op_PipeLeft", [ f; x ] -> curriedApply r t f [ x ] |> Some
    | "op_PipeRight2", [ x; y; f ]
    | "op_PipeLeft2", [ f; x; y ] -> curriedApply r t f [ x; y ] |> Some
    | "op_PipeRight3", [ x; y; z; f ]
    | "op_PipeLeft3", [ f; x; y; z ] -> curriedApply r t f [ x; y; z ] |> Some
    | "op_ComposeRight", [ f1; f2 ] -> compose com ctx r t f1 f2 |> Some
    | "op_ComposeLeft", [ f2; f1 ] -> compose com ctx r t f1 f2 |> Some
    // Strings
    | ("PrintFormatToString" | "PrintFormatToStringThen" | "PrintFormat" | "PrintFormatLine" | "PrintFormatToError" | "PrintFormatLineToError" | "PrintFormatThen" | "PrintFormatToStringThenFail" | "PrintFormatToStringBuilder" | "PrintFormatToStringBuilderThen"), // Printf.kbprintf
      _ -> fsFormat com ctx r t i thisArg args
    | ("Failure" | "FailurePattern" | "LazyPattern" | "NullArg" | "Using"), _ -> fsharpModule com ctx r t i thisArg args
    | "Lock", _ -> libCallThis com r t i "Monitor" "lock" thisArg args |> Some
    | ("IsNull" | "IsNotNull" | "IsNullV" | "NonNull" | "NonNullV" | "NullMatchPattern" | "NullValueMatchPattern" | "NonNullQuickPattern" | "NonNullQuickValuePattern" | "WithNull" | "WithNullV" | "NullV" | "NullArgCheck"),
      _ -> fsharpModule com ctx r t i thisArg args
    // Exceptions
    | ("FailWith" | "InvalidOp"), [ msg ] -> makeThrow r t (error com msg) |> Some
    | "InvalidArg", [ argName; msg ] ->
        let msg = add msg (add (add (str " (Parameter '") argName) (str "')"))
        makeThrow r t (error com msg) |> Some
    | "Raise", [ arg ] -> makeThrow r t arg |> Some
    | "Reraise", _ ->
        match ctx.CaughtException with
        | Some ex -> makeThrow r t (IdentExpr ex) |> Some
        | None ->
            "`reraise` used in context where caught exception is not available, please report"
            |> addError com ctx.InlinePath r

            makeThrow r t (error com (str "")) |> Some
    // Math functions
    // TODO: optimize square pow: x * x
    | ("Pow" | "PowInteger" | "op_Exponentiation"), _ ->
        let argTypes = args |> List.map (fun a -> a.Type)

        match argTypes with
        | Number(Decimal, _) :: _ -> libCallThis com r t i "Decimal" "pown" thisArg args |> Some
        | Number(BigInt, _) :: _ -> libCallThis com r t i "BigInt" "pow" thisArg args |> Some
        | Number((Float32 | Float64), _) :: _ ->
            let meth =
                if i.CompiledName = "PowInteger" then
                    "powi"
                else
                    "powf"

            math r t args i.SignatureArgTypes meth |> Some
        | CustomOp com ctx r t "Pow" args e -> Some e
        | _ -> math r t args i.SignatureArgTypes "pow" |> Some
    | ("Ceiling" | "Floor") as meth, _ ->
        let meth = Naming.lowerFirst meth

        match args with
        | ExprType(Number(Decimal, _)) :: _ -> libCallThis com r t i "Decimal" meth thisArg args |> Some
        | _ ->
            let meth =
                if meth = "ceiling" then
                    "ceil"
                else
                    meth

            math r t args i.SignatureArgTypes meth |> Some
    | "Log", [ arg ] -> math r t args i.SignatureArgTypes "ln" |> Some
    | "Abs", _ ->
        match args with
        | ExprType(Number(Decimal, _)) :: _ -> libCallThis com r t i "Decimal" "abs" thisArg args |> Some
        | ExprType(Number(BigInt, _)) :: _ -> libCallThis com r t i "BigInt" "abs" thisArg args |> Some
        | _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    | ("Acos" | "Asin" | "Atan" | "Atan2" | "Cos" | "Cosh" | "Exp" | "Log" | "Log2" | "Log10" | "Sin" | "Sinh" | "Sqrt" | "Tan" | "Tanh"),
      _ ->
        match args with
        | ExprType(Number(_, _)) :: _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
        | _ -> applyOp com ctx r t i.CompiledName args |> Some
    | "Round", _ ->
        match args with
        | [ ExprType(Number(Decimal, _)) ] -> libCallThis com r t i "Decimal" "round" thisArg args |> Some
        | [ ExprType(Number(Decimal, _)); ExprType(Number(Int32, _)) ] ->
            libCallThis com r t i "Decimal" "roundTo" thisArg args |> Some
        | [ ExprType(Number(Decimal, _)); mode ] -> makeLibCall com r t i "Decimal" "roundMode" args |> Some
        | [ ExprType(Number(Decimal, _)); dp; mode ] -> makeLibCall com r t i "Decimal" "roundToMode" args |> Some
        | [ ExprTypeAs(Number(Float64, _), arg) ] ->
            // TODO: other midpoint modes for Double
            makeInstanceCall r t i arg "round" [] |> Some
        | _ -> None
    | "Truncate", [ arg ] ->
        match args with
        | ExprType(Number(Decimal, _)) :: _ -> libCallThis com r t i "Decimal" "truncate" thisArg args |> Some
        | _ -> makeInstanceCall r t i arg "trunc" [] |> Some
    | "Sign", [ arg ] ->
        match args with
        | ExprType(Number(Decimal, _)) :: _ -> libCallThis com r t i "Decimal" "sign" thisArg args |> Some
        | ExprType(Number(BigInt, _)) :: _ -> libCallThis com r t i "BigInt" "sign" thisArg args |> Some
        | ExprType(Number((Float16 | Float32 | Float64), _)) :: _ ->
            compare com ctx r arg (getZero com ctx arg.Type) |> Some
        | _ ->
            let sign = makeInstanceCall r arg.Type i arg "signum" []
            TypeCast(sign, Int32.Number) |> Some
    | "DivRem", _ ->
        match args with
        | [ x; y ] -> makeLibCall com r t i "Util" "divRem" args |> Some
        | [ x; y; rem ] -> makeLibCall com r t i "Util" "divRemOut" args |> Some
        | _ -> None
    // Numbers
    | "Infinity", _ -> makeGlobalIdent ("f64", "INFINITY", t) |> Some
    | "InfinitySingle", _ -> makeGlobalIdent ("f32", "INFINITY", t) |> Some
    | "NaN", _ -> makeGlobalIdent ("f64", "NAN", t) |> Some
    | "NaNSingle", _ -> makeGlobalIdent ("f32", "NAN", t) |> Some
    | "Fst", [ tup ] -> Get(tup, TupleIndex 0, t, r) |> Some
    | "Snd", [ tup ] -> Get(tup, TupleIndex 1, t, r) |> Some
    // Reference
    | "op_Dereference", [ arg ] -> getRefCell com r t arg |> Some
    | "op_ColonEquals", [ o; v ] -> setRefCell com r o v |> Some
    | "Ref", [ arg ] -> makeRefCellFromValue com r arg |> Some
    | "Increment", [ arg ] ->
        let v = add (getRefCell com r t arg) (getOne com ctx t)
        setRefCell com r arg v |> Some
    | "Decrement", [ arg ] ->
        let v = sub (getRefCell com r t arg) (getOne com ctx t)
        setRefCell com r arg v |> Some
    // Concatenates two lists
    | "op_Append", _ -> libCallThis com r t i "List" "append" thisArg args |> Some
    | "IsNull", [ arg ] -> nullCheck r true arg |> Some
    | "Hash", [ arg ] -> getHashCode com ctx r arg |> Some
    // Comparison
    | Patterns.SetContains Operators.compareSet, [ left; right ] ->
        applyCompareOp com ctx r t i.CompiledName left right |> Some
    | "Compare", [ left; right ] -> compare com ctx r left right |> Some
    | "Clamp", _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
    | ("Min" | "Max") as meth, _ ->
        match args.Head.Type with
        | Boolean
        | Char
        | String
        | Number _ -> math r t args i.SignatureArgTypes i.CompiledName |> Some
        | _ -> libCall com r t "Native" (Naming.lowerFirst meth) args |> Some
    | ("MinMagnitude" | "MaxMagnitude") as meth, _ ->
        let meth = Naming.lowerFirst meth

        match args with
        | ExprType(Number(Decimal, _)) :: _ -> libCallThis com r t i "Decimal" meth thisArg args |> Some
        | ExprType(Number(BigInt, _)) :: _ -> libCallThis com r t i "BigInt" meth thisArg args |> Some
        | ExprType(Number _) :: _ -> libCallThis com r t i "Numeric" meth thisArg args |> Some
        | _ -> None
    | "Not", [ operand ] -> // TODO: Check custom operator?
        makeUnOp r t operand UnaryNot |> Some
    | Patterns.SetContains Operators.standardSet, _ -> applyOp com ctx r t i.CompiledName args |> Some
    // Type info
    | "TypeOf", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeTypeInfo r |> Some
    | "TypeDefOf", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeTypeDefinitionInfo r |> Some
    | _ -> None

let objects (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> typedObjExpr t [] |> Some
    | "ToString", Some arg, _ -> toString com ctx r [ arg ] |> Some
    | "ReferenceEquals", None, [ arg1; arg2 ] -> referenceEquals com ctx r arg1 arg2 |> Some
    | "Equals", Some(MaybeCasted arg1), [ MaybeCasted arg2 ]
    | "Equals", None, [ MaybeCasted arg1; MaybeCasted arg2 ] ->
        match arg1.Type, arg2.Type with
        | Array _, _
        | _, Array _ -> referenceEquals com ctx r arg1 arg2 |> Some
        | _ -> TypeCast(arg2, arg1.Type) |> equals com ctx r arg1 |> Some
    | "GetHashCode", Some arg, _ -> objectHash com ctx r arg |> Some
    | "GetType", Some arg, _ ->
        if arg.Type = Any then
            // Dynamic type of a boxed value: resolve via the runtime reflection
            // registry (populated by typeof<T>). Returns the concrete type's info
            // when registered, else the value as an opaque placeholder.
            libCall com r t "Reflection" "getTypeFromObj" [ arg ] |> Some
        else
            makeTypeInfo r arg.Type |> Some
    | _ -> None

let valueTypes (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> typedObjExpr t [] |> Some
    | "ToString", Some arg, _ -> toString com ctx r [ arg ] |> Some
    | "Equals", Some arg1, [ arg2 ]
    | "Equals", None, [ arg1; arg2 ] -> equals com ctx r arg1 arg2 |> Some
    | "GetHashCode", Some arg, _ -> getHashCode com ctx r arg |> Some
    | "CompareTo", Some arg1, [ arg2 ]
    | "Compare", None, [ arg1; arg2 ] -> compare com ctx r arg1 arg2 |> Some
    | _ -> None

let chars (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let getMethodName meth args =
        match args with
        | (ExprType String) :: _ -> meth + "_2"
        | _ -> meth

    match i.CompiledName, thisArg, args with
    | ("IsBetween") as meth, None, _ -> makeLibCall com r t i "Char" meth args |> Some
    | ("IsAscii" | "IsAsciiDigit" | "IsAsciiLetter" | "IsAsciiLetterLower" | "IsAsciiLetterUpper" | "IsAsciiLetterOrDigit" | "IsAsciiHexDigit" | "IsAsciiHexDigitLower" | "IsAsciiHexDigitUpper") as meth,
      None,
      [ c ] -> makeLibCall com r t i "Char" meth args |> Some
    | ("IsControl" | "IsDigit" | "IsLetter" | "IsLetterOrDigit" | "IsLower" | "IsUpper" | "IsNumber" | "IsPunctuation" | "IsSeparator" | "IsSymbol" | "IsWhiteSpace") as meth,
      None,
      _ ->
        let meth = getMethodName meth args

        makeLibCall com r t i "Char" meth args |> Some
    | ("GetNumericValue" | "GetUnicodeCategory" | "ConvertToUtf32") as meth, None, _ ->
        let meth = getMethodName meth args

        makeLibCall com r t i "Char" meth args |> Some
    | "ToString", None, [ ExprType(Char) ] -> toString com ctx r args |> Some
    | "ToString", Some c, [] -> toString com ctx r [ c ] |> Some
    | "ToString", Some c, [ _ ] -> toString com ctx r [ c ] |> Some
    | ("ToLower" | "ToUpper") as meth, None, [ c; ExprType(DeclaredType(ent, _)) ] when ent.FullName = Types.cultureInfo ->
        makeLibCall com r t i "Char" meth [ c ] |> Some
    | ("ConvertFromUtf32" | "ToLower" | "ToLowerInvariant" | "ToUpper" | "ToUpperInvariant") as meth, None, [ c ] ->
        makeLibCall com r t i "Char" meth args |> Some
    | "GetTypeCode", Some c, [] -> makeLibCall com r t i "Char" "GetTypeCode" [ c ] |> Some
    | ("TryParse" | "Parse") as meth, None, _ -> makeLibCall com r t i "Char" meth args |> Some
    | ("IsSurrogate" | "IsHighSurrogate" | "IsLowSurrogate" | "IsSurrogatePair") as meth, None, _ ->
        $"Rust chars are Unicode scalar values, so surrogate tests will be false."
        |> addWarning com ctx.InlinePath r

        let meth = getMethodName meth args

        makeLibCall com r t i "Char" meth args |> Some
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), _, _ -> valueTypes com ctx r t i thisArg args
    | _ -> None

let getEnumerator com r t i (expr: Expr) =
    match expr.Type with
    | IsEntity (Types.keyCollection) _
    | IsEntity (Types.valueCollection) _
    | IsEntity (Types.icollectionGeneric) _
    // | IsEntity (Types.regexMatchCollection) _
    // | IsEntity (Types.regexGroupCollection) _
    // | IsEntity (Types.regexCaptureCollection) _
    | Array _ -> libCall com r t "Seq" "Enumerable::ofArray" [ expr ]
    | List _ -> libCall com r t "Seq" "Enumerable::ofList" [ expr ]
    | String ->
        let en = toSeq com Any expr
        makeInstanceCall r t i en "GetEnumerator" []
    | IsEntity (Types.hashset) _
    | IsEntity (Types.iset) _ ->
        let ar = libCall com r t "HashSet" "entries" [ expr ]
        libCall com r t "Seq" "Enumerable::ofArray" [ ar ]
    | IsEntity (Types.dictionary) _
    | IsEntity (Types.idictionary) _
    | IsEntity (Types.ireadonlydictionary) _ ->
        let ar = libCallTyped com r t "HashMap" "entries" [ expr ] [ expr.Type ]
        libCall com r t "Seq" "Enumerable::ofArray" [ ar ]
    | _ -> makeInstanceCall r t i expr "GetEnumerator" []

let strings (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    // let isIgnoreCase args =
    //     match args with
    //     | [] -> false
    //     | [ BoolConst ignoreCase ] -> ignoreCase
    //     | [ BoolConst ignoreCase; _cultureInfo ] -> ignoreCase
    //     | [ NumberConst(NumberValue.Int32 kind, NumberInfo.IsEnum _) ] -> kind = 1 || kind = 3 || kind = 5
    //     | [ _cultureInfo; NumberConst(NumberValue.Int32 options, NumberInfo.IsEnum _) ] ->
    //         (options &&& 1 <> 0) || (options &&& 268435456 <> 0)
    //     | _ -> false

    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ ->
        match i.SignatureArgTypes with
        | [ Char; Number(Int32, _) ] -> libCall com r t "String" "fromChar" args |> Some
        | [ MaybeNullable(Array(Char, _)) ] -> libCall com r t "String" "fromChars" args |> Some
        | [ MaybeNullable(Array(Char, _)); Number(Int32, _); Number(Int32, _) ] ->
            libCall com r t "String" "fromChars2" args |> Some
        | _ -> None
    | "get_Length", Some c, _ -> libCall com r t "String" "length" (c :: args) |> Some
    | "get_Chars", Some c, _ -> libCall com r t "String" "getCharAt" (c :: args) |> Some
    | "EnumerateRunes", Some c, _ -> libCall com r t "String" "enumerateRunes" [ c ] |> Some
    | "CompareOrdinal", None, _ ->
        match args with
        | [ ExprType String; ExprType String ] -> libCall com r t "String" "compareOrdinal" args |> Some
        | [ ExprType String
            ExprType(Number(Int32, _))
            ExprType String
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> libCall com r t "String" "compareOrdinal2" args |> Some
        | _ -> None
    | "CompareTo", Some c, [ ExprTypeAs(String, arg) ] ->
        $"String.CompareTo will be compiled as String.CompareOrdinal"
        |> addWarning com ctx.InlinePath r

        libCall com r t "String" "compareOrdinal" [ c; arg ] |> Some
    | "Compare", None, _ ->
        $"String.Compare will be compiled as String.CompareOrdinal"
        |> addWarning com ctx.InlinePath r

        match args with
        | [ ExprType String; ExprType String ] -> libCall com r t "String" "compareOrdinal" args |> Some
        | ExprType String :: ExprType String :: ExprType Boolean :: restArgs ->
            libCall com r t "String" "compareCase" args |> Some
        | [ ExprType String; ExprType String; comparison ] -> libCall com r t "String" "compareWith" args |> Some
        | [ ExprType String
            ExprType(Number(Int32, _))
            ExprType String
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> libCall com r t "String" "compareOrdinal2" args |> Some
        | ExprType String :: ExprType(Number(Int32, _)) :: ExprType String :: ExprType(Number(Int32, _)) :: ExprType(Number(Int32,
                                                                                                                            _)) :: ExprType Boolean :: restArgs ->
            libCall com r t "String" "compareCase2" args |> Some
        | ExprType String :: ExprType(Number(Int32, _)) :: ExprType String :: ExprType(Number(Int32, _)) :: ExprType(Number(Int32,
                                                                                                                            _)) :: comparison :: restArgs ->
            libCall com r t "String" "compareWith2" args |> Some
        | _ -> None
    | "Concat", None, _ ->
        match args with
        | [ ExprTypeAs(IEnumerable, arg) ] -> libCall com r t "String" "concat" [ toArray com t arg ] |> Some
        | [ ExprType String; ExprType String ]
        | [ ExprType String; ExprType String; ExprType String ]
        | [ ExprType String; ExprType String; ExprType String; ExprType String ] ->
            libCall com r t "String" "concat" [ makeArray String args ] |> Some
        | [ ExprType(Array(String, _)) ] -> libCall com r t "String" "concat" args |> Some
        | _ -> None
    | "Contains", Some c, _ ->
        match args with
        | [ ExprType Char ] -> libCall com r t "String" "containsChar" (c :: args) |> Some
        | [ ExprType Char; _comparison ] -> libCall com r t "String" "containsChar2" (c :: args) |> Some
        | [ ExprType String ] -> libCall com r t "String" "contains" (c :: args) |> Some
        | [ ExprType String; _comparison ] -> libCall com r t "String" "contains2" (c :: args) |> Some
        | _ -> None
    | "EndsWith", Some c, _ ->
        match args with
        | [ ExprType Char ] -> libCall com r t "String" "endsWithChar" (c :: args) |> Some
        | [ ExprType String ] -> libCall com r t "String" "endsWith" (c :: args) |> Some
        | [ ExprType String; _comparison ] -> libCall com r t "String" "endsWith2" (c :: args) |> Some
        | [ pattern; ignoreCase; _culture ] -> libCall com r t "String" "endsWith3" [ c; pattern; ignoreCase ] |> Some
        | _ -> None
    | "Equals", _, _ ->
        match thisArg, args with
        | Some x, [ ExprTypeAs(String, y) ]
        | None, [ ExprTypeAs(String, x); ExprTypeAs(String, y) ] ->
            libCall com r t "String" "equalsOrdinal" [ x; y ] |> Some
        | Some x, [ ExprTypeAs(String, y); comparison ]
        | None, [ ExprTypeAs(String, x); ExprTypeAs(String, y); comparison ] ->
            libCall com r t "String" "equals2" [ x; y; comparison ] |> Some
        | _ -> None
    | "Format", None, _ ->
        match args with
        | (ExprType String :: _) -> "sprintf!" |> emitFormat com r t args |> Some
        | (cultureInfo :: restArgs) ->
            $"String.Format(): Format provider argument is ignored"
            |> addWarning com ctx.InlinePath r

            "sprintf!" |> emitFormat com r t restArgs |> Some
        | _ -> None
    | "GetEnumerator", Some c, _ -> getEnumerator com r t i c |> Some
    | ("IndexOf" | "LastIndexOf" | "IndexOfAny" | "LastIndexOfAny"), Some c, _ ->
        let suffixOpt =
            match args with
            | [ ExprType String ] -> Some ""
            | [ ExprType String; ExprType(Number(Int32, _)) ] -> Some "2"
            | [ ExprType String; ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] -> Some "3"
            | [ ExprType Char ] -> Some "Char"
            | [ ExprType Char; ExprType(Number(Int32, _)) ] -> Some "Char2"
            | [ ExprType Char; ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] -> Some "Char3"
            | [ ExprType(Array(Char, _)) ] -> Some ""
            | [ ExprType(Array(Char, _)); ExprType(Number(Int32, _)) ] -> Some "2"
            | [ ExprType(Array(Char, _)); ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] -> Some "3"
            | _ -> None

        match suffixOpt with
        | Some suffix ->
            let methName = (Naming.lowerFirst i.CompiledName) + suffix

            libCall com r t "String" methName (c :: args) |> Some
        | _ -> None
    | "Insert", Some c, _ -> libCall com r t "String" "insert" (c :: args) |> Some
    | "IsNullOrEmpty", None, _ -> libCall com r t "String" "isNullOrEmpty" args |> Some
    | "IsNullOrWhiteSpace", None, _ -> libCall com r t "String" "isNullOrWhitespace" args |> Some
    | "Join", None, _ ->
        let args =
            match args with
            | [ ExprTypeAs(String, sep); ExprTypeAs(IEnumerable, arg) ] -> [ sep; toArray com t arg ]
            | [ ExprTypeAs(String, sep); ExprTypeAs(Array(String, _), arg) ] -> [ sep; arg ]
            | [ ExprTypeAs(Char, sep); ExprTypeAs(Array(String, _), arg) ] ->
                let sep = libCall com r String "String" "ofChar" [ sep ]

                [ sep; arg ]
            | [ ExprTypeAs(String, sep)
                ExprTypeAs(Array(String, _), arg)
                ExprTypeAs(Number(Int32, _), idx)
                ExprTypeAs(Number(Int32, _), cnt) ] ->
                let arg =
                    libCall com r (Array(String, MutableArray)) "Array" "getSubArray" [ arg; idx; cnt ]

                [ sep; arg ]
            | [ ExprTypeAs(Char, sep)
                ExprTypeAs(Array(String, _), arg)
                ExprTypeAs(Number(Int32, _), idx)
                ExprTypeAs(Number(Int32, _), cnt) ] ->
                let sep = libCall com r String "String" "ofChar" [ sep ]

                let arg =
                    libCall com r (Array(String, MutableArray)) "Array" "getSubArray" [ arg; idx; cnt ]

                [ sep; arg ]
            | _ -> []

        if not (List.isEmpty args) then
            libCall com r t "String" "join" args |> Some
        else
            None
    | ("PadLeft" | "PadRight"), Some c, _ ->
        let methName = Naming.lowerFirst i.CompiledName

        match args with
        | [ ExprTypeAs(Number(Int32, _), arg) ] ->
            let ch = makeTypeConst None Char ' '

            libCall com r t "String" methName [ c; arg; ch ] |> Some
        | [ ExprType(Number(Int32, _)); ExprType Char ] -> libCall com r t "String" methName (c :: args) |> Some
        | _ -> None
    | "Remove", Some c, _ ->
        match args with
        | [ ExprType(Number(Int32, _)) ] -> makeLibCall com r t i "String" "remove" (c :: args) |> Some
        | [ ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] ->
            makeLibCall com r t i "String" "remove2" (c :: args) |> Some
        | _ -> None
    | "Replace", Some c, _ ->
        match args with
        | [ ExprType String; ExprType String ] -> libCall com r t "String" "replace" (c :: args) |> Some
        | [ ExprType Char; ExprType Char ] -> libCall com r t "String" "replaceChar" (c :: args) |> Some
        | _ -> None
    | "Split", Some c, _ ->
        match args with
        | [] ->
            libCall com r t "String" "split" [ c; makeStrConst ""; makeIntConst -1; makeIntConst 0 ]
            |> Some

        | [ ExprTypeAs(String, arg1) ] ->
            libCall com r t "String" "split" [ c; arg1; makeIntConst -1; makeIntConst 0 ]
            |> Some
        | [ ExprTypeAs(String, arg1); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2) ] ->
            libCall com r t "String" "split" [ c; arg1; makeIntConst -1; arg2 ] |> Some
        | [ ExprTypeAs(String, arg1)
            ExprTypeAs(Number(Int32, _), arg2)
            ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg3) ] ->
            libCall com r t "String" "split" [ c; arg1; arg2; arg3 ] |> Some

        | [ Value(NewArray(ArrayValues [ arg1 ], String, _), _) ] ->
            libCall com r t "String" "split" [ c; arg1; makeIntConst -1; makeIntConst 0 ]
            |> Some
        | [ Value(NewArray(ArrayValues [ arg1 ], String, _), _); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2) ] ->
            libCall com r t "String" "split" [ c; arg1; makeIntConst -1; arg2 ] |> Some
        | [ ExprTypeAs(Array(String, _), arg1); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2) ] ->
            libCall com r t "String" "splitStrings" [ c; arg1; arg2 ] |> Some
        | [ Value(NewArray(ArrayValues [ arg1 ], String, _), _)
            ExprTypeAs(Number(Int32, _), arg2)
            ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg3) ] ->
            libCall com r t "String" "split" [ c; arg1; arg2; arg3 ] |> Some

        | [ ExprTypeAs(Char, arg1) ] ->
            libCall com r t "String" "splitChars" [ c; makeArray Char [ arg1 ]; makeIntConst -1; makeIntConst 0 ]
            |> Some
        | [ ExprTypeAs(Char, arg1); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2) ] ->
            libCall com r t "String" "splitChars" [ c; makeArray Char [ arg1 ]; makeIntConst -1; arg2 ]
            |> Some
        | [ ExprTypeAs(Char, arg1); ExprTypeAs(Number(Int32, _), arg2); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg3) ] ->
            libCall com r t "String" "splitChars" [ c; makeArray Char [ arg1 ]; arg2; arg3 ]
            |> Some

        | [ ExprTypeAs(Array(Char, _), arg1) ] ->
            libCall com r t "String" "splitChars" [ c; arg1; makeIntConst -1; makeIntConst 0 ]
            |> Some
        | [ ExprTypeAs(Array(Char, _), arg1); ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg2) ] ->
            libCall com r t "String" "splitChars" [ c; arg1; makeIntConst -1; arg2 ] |> Some
        | [ ExprTypeAs(Array(Char, _), arg1); ExprTypeAs(Number(Int32, _), arg2) ] ->
            libCall com r t "String" "splitChars" [ c; arg1; arg2; makeIntConst 0 ] |> Some
        | [ ExprTypeAs(Array(Char, _), arg1)
            ExprTypeAs(Number(Int32, _), arg2)
            ExprTypeAs(Number(_, NumberInfo.IsEnum _), arg3) ] ->
            libCall com r t "String" "splitChars" [ c; arg1; arg2; arg3 ] |> Some

        // Remaining gap: the count-bearing string[] overload Split(string[], int, options)
        // for multi-element / non-literal arrays. splitStrings has no count parameter, and a
        // correct global left-to-right count across multiple separators is non-trivial.
        // The common Split(string[], options) form is handled above via splitStrings.
        | _ -> None
    | "StartsWith", Some c, _ ->
        match args with
        | [ ExprType Char ] -> libCall com r t "String" "startsWithChar" (c :: args) |> Some
        | [ ExprType String ] -> libCall com r t "String" "startsWith" (c :: args) |> Some
        | [ ExprType String; _comparison ] -> libCall com r t "String" "startsWith2" (c :: args) |> Some
        | [ pattern; ignoreCase; _culture ] -> libCall com r t "String" "startsWith3" [ c; pattern; ignoreCase ] |> Some
        | _ -> None
    | "Substring", Some c, _ ->
        match args with
        | [ ExprType(Number(Int32, _)) ] -> makeLibCall com r t i "String" "substring" (c :: args) |> Some
        | [ ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] ->
            makeLibCall com r t i "String" "substring2" (c :: args) |> Some
        | _ -> None
    | "ToCharArray", Some c, _ ->
        match args with
        | [] -> makeLibCall com r t i "String" "toCharArray" (c :: args) |> Some
        | [ ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] ->
            makeLibCall com r t i "String" "toCharArray2" (c :: args) |> Some
        | _ -> None
    | ("ToLower" | "ToLowerInvariant"), Some c, args -> libCall com r t "String" "toLower" (c :: args) |> Some
    | ("ToUpper" | "ToUpperInvariant"), Some c, args -> libCall com r t "String" "toUpper" (c :: args) |> Some
    | ("Trim" | "TrimStart" | "TrimEnd"), Some c, _ ->
        let methName = Naming.lowerFirst i.CompiledName

        match args with
        | [] -> libCall com r t "String" methName (c :: args) |> Some
        | [ ExprType Char ] -> libCall com r t "String" (methName + "Char") (c :: args) |> Some
        | [ ExprType(Array(Char, _)) ] -> libCall com r t "String" (methName + "Chars") (c :: args) |> Some
        | _ -> None
    | _ -> None

let stringModule (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "Concat", [ sep; arg ] -> libCall com r t "String" "join" [ sep; toArray com t arg ] |> Some
    | meth, args -> makeLibCall com r t i "String" (Naming.lowerFirst meth) args |> Some

let stringBuilder (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "AppendFormat", Some sb, _ ->
        match args with
        | (ExprType String :: _) ->
            let s = "sprintf!" |> emitFormat com None String args

            libCall com r t "Util" "sb_Append" [ sb; s ] |> Some
        | (cultureInfo :: restArgs) ->
            $"StringBuilder.AppendFormat(): Format provider argument is ignored"
            |> addWarning com ctx.InlinePath r

            let s = "sprintf!" |> emitFormat com None String restArgs

            libCall com r t "Util" "sb_Append" [ sb; s ] |> Some
        | _ -> None
    | _ -> bclType com ctx r t i thisArg args

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
    | "Create", None, [ StringConst str; Value(NewArray(ArrayValues args, _, _), _) ] ->
        let matches = Regex.Matches(str, @"\{\d+(.*?)\}") |> Seq.cast<Match> |> Seq.toArray

        let hasFormat = matches |> Array.exists (fun m -> m.Groups[1].Value.Length > 0)

        let tag =
            if not hasFormat then
                libValue com r Any "String" "fmt" [] |> Some
            else
                let fmtArg =
                    matches
                    |> Array.map (fun m -> makeStrConst m.Groups[1].Value)
                    |> Array.toList
                    |> makeArray String

                libCall com r Any "String" "fmtWith" [ fmtArg ] |> Some

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
    | "get_Format", Some x, _ -> libCall com r t "String" "getFormat" [ x ] |> Some
    | "get_ArgumentCount", Some x, _ -> getFieldWith r t (getField x "args") "length" |> Some
    | "GetArgument", Some x, [ idx ] -> getExpr r t (getField x "args") idx |> Some
    | "GetArguments", Some x, [] -> getFieldWith r t x "args" |> Some
    | _ -> None

let seqModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    // | "ToArray", [arg] ->
    //     makeLibCall com r t i "Array" "ofSeq" args |> Some
    | "ToList", [ arg ] -> makeLibCall com r t i "List" "ofSeq" args |> Some
    | "CreateEvent", [ addHandler; removeHandler; createHandler ] ->
        makeLibCall com r t i "Event" "createEvent" [ addHandler; removeHandler ]
        |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy") as meth, args ->
        let meth = Naming.lowerFirst meth

        makeLibCall com r t i "Seq" meth args |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth

        libCallThis com r t i "Seq" meth thisArg args |> Some

let resizeArrays (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, [] -> libCall com r t "NativeArray" "new_empty" args |> Some
    | ".ctor", _, [ ExprType(Number(Int32, _)) ] -> libCall com r t "NativeArray" "new_with_capacity" args |> Some
    | ".ctor", _, [ ExprType(IEnumerable) ] -> libCall com r t "NativeArray" "new_from_enumerable" args |> Some
    | ".ctor", _, [ arg ] -> toArray com t arg |> Some
    | "get_Capacity", Some ar, _ -> libCall com r t "NativeArray" "get_Capacity" [ ar ] |> Some
    | "get_Count", Some ar, _ -> libCall com r t "NativeArray" "get_Count" [ ar ] |> Some
    | "get_Item", Some ar, [ idx ] -> getExpr r t ar idx |> Some
    | "set_Item", Some ar, [ idx; value ] -> setExpr r ar idx value |> Some
    | "GetEnumerator", Some(MaybeCasted(ar)), _ -> libCall com r t "Seq" "Enumerable::ofArray" [ ar ] |> Some
    | ("Add" | "AddRange" | "Clear" | "Contains" | "ConvertAll" | "Exists" | "GetRange" | "Slice" | "ForEach" | "FindAll" | "Find" | "FindLast" | "FindIndex" | "FindLastIndex" | "Insert" | "InsertRange" | "Remove" | "RemoveAt" | "RemoveAll" | "RemoveRange" | "ToArray" | "TrimExcess" | "TrueForAll") as meth,
      Some ar,
      args ->
        // methods without overrides
        let meth = Naming.lowerFirst meth
        libCall com r t "NativeArray" meth (ar :: args) |> Some
    | ("BinarySearch" | "CopyTo" | "IndexOf" | "LastIndexOf" | "Reverse") as meth, Some ar, args ->
        // methods with some overrides
        let meth = meth |> toLowerFirstWithArgsCountSuffix (ar :: args)
        libCall com r t "NativeArray" meth (ar :: args) |> Some
    | "Sort", Some ar, [] -> libCall com r t "NativeArray" "sort" (ar :: args) |> Some
    | "Sort", Some ar, [ ExprType(DelegateType _) ] -> libCall com r t "NativeArray" "sortBy" (ar :: args) |> Some
    | "Sort", Some ar, [ comparer ] -> libCall com r t "NativeArray" "sortWith" (ar :: args) |> Some
    | "Sort", Some ar, [ index; count; comparer ] -> libCall com r t "NativeArray" "sortWith2" (ar :: args) |> Some
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
    | "AddRange", None, [ ar; arg ] -> libCall com r t "Array" "addRangeInPlace" [ arg; ar ] |> Some
    | "InsertRange", None, [ ar; idx; arg ] -> libCall com r t "Array" "insertRangeInPlace" [ idx; arg; ar ] |> Some
    | _ -> None

let readOnlySpans (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "op_Implicit", [ arg ] -> arg |> Some
    | _ -> None

let tuples (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let changeKind isStruct =
        function
        | Value(NewTuple(args, _), r) :: _ -> Value(NewTuple(args, isStruct), r) |> Some
        | (ExprType(Tuple(genArgs, _)) as e) :: _ -> TypeCast(e, Tuple(genArgs, isStruct)) |> Some
        | _ -> None

    match i.CompiledName, thisArg with
    | (".ctor" | "Create"), _ ->
        let isStruct =
            i.DeclaringEntityFullName.StartsWith("System.ValueTuple", StringComparison.Ordinal)

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

let createArray (com: ICompiler) ctx r t i count value =
    match t, value with
    | Array(typ, _), None ->
        let value = getZero com ctx typ

        Value(NewArray(makeTuple None true [ value; count ] |> ArrayFrom, typ, MutableArray), r)
    | Array(typ, _), Some value ->
        Value(NewArray(makeTuple None true [ value; count ] |> ArrayFrom, typ, MutableArray), r)
    | _ ->
        $"Expecting an array type but got %A{t}"
        |> addErrorAndReturnNull com ctx.InlinePath r

let copyToArray (com: ICompiler) r t (i: CallInfo) args =
    makeLibCall com r t i "Array" "copyTo" args |> Some

let arrays (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "get_Length", Some ar, _ -> libCall com r t "NativeArray" "get_Count" [ ar ] |> Some
    | "get_Item", Some ar, [ idx ] -> getExpr r t ar idx |> Some
    | "set_Item", Some ar, [ idx; value ] -> setExpr r ar idx value |> Some
    | "Clone", Some ar, _ -> libCall com r t "NativeArray" "toArray" [ ar ] |> Some
    | "Copy", None, [ _source; _sourceIndex; _target; _targetIndex; _count ] -> copyToArray com r t i args
    | "Copy", None, [ source; target; count ] ->
        copyToArray com r t i [ source; makeIntConst 0; target; makeIntConst 0; count ]
    | "GetEnumerator", Some ar, _ -> libCall com r t "Seq" "Enumerable::ofArray" [ ar ] |> Some
    | ("ConvertAll" | "Exists" | "GetRange" | "ForEach" | "FindAll" | "Find" | "FindLast" | "FindIndex" | "FindLastIndex" | "TrueForAll") as meth,
      None,
      args ->
        // methods without overrides
        let meth = Naming.lowerFirst meth
        libCall com r t "NativeArray" meth args |> Some
    | ("BinarySearch" | "CopyTo" | "IndexOf" | "LastIndexOf" | "Reverse") as meth, None, args ->
        // methods with some overrides
        let meth = meth |> toLowerFirstWithArgsCountSuffix args
        libCall com r t "NativeArray" meth args |> Some
    | "Sort", None, [ ar ] -> libCall com r t "NativeArray" "sort" args |> Some
    | "Sort", None, [ ar; ExprType(DelegateType _) as comparer ] -> libCall com r t "NativeArray" "sortBy" args |> Some
    | "Sort", None, [ ar; comparer ] -> libCall com r t "NativeArray" "sortWith" args |> Some
    | _ -> None

let arrayModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "ToSeq", [ ar ] -> makeLibCall com r t i "Seq" "ofArray" args |> Some
    | "OfSeq", [ ar ] -> makeLibCall com r t i "Seq" "toArray" args |> Some
    | "OfList", [ ar ] -> makeLibCall com r t i "List" "toArray" args |> Some
    | "ToList", args -> makeLibCall com r t i "List" "ofArray" args |> Some
    | ("Length" | "Count"), [ ar ] -> libCall com r t "NativeArray" "get_Count" [ ar ] |> Some
    | "Item", [ idx; ar ] -> getExpr r t ar idx |> Some
    | "Get", [ ar; idx ] -> getExpr r t ar idx |> Some
    | "Set", [ ar; idx; value ] -> setExpr r ar idx value |> Some
    | "ZeroCreate", [ count ] -> createArray com ctx r t i count None |> Some
    | "Create", [ count; value ] -> createArray com ctx r t i count (Some value) |> Some
    | "Empty", [] -> createArray com ctx r t i (makeIntConst 0) None |> Some
    | "Singleton", [ value ] -> createArray com ctx r t i (makeIntConst 1) (Some value) |> Some
    | "IsEmpty", [ ar ] -> makeInstanceCall r t i ar "is_empty" [] |> Some
    | "Copy", [ ar ] -> libCall com r t "NativeArray" "toArray" args |> Some
    | "CopyTo", args -> copyToArray com r t i args
    | ("Concat" | "Transpose") as meth, [ arg ] ->
        makeLibCall com r t i "Array" (Naming.lowerFirst meth) [ toArray com t arg ]
        |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy") as meth, args ->
        let meth = Naming.lowerFirst meth

        makeLibCall com r t i "Array" meth args |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth

        makeLibCall com r t i "Array" meth args |> Some

let lists (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
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

        makeLibCall com r t i "List" methName args |> Some
    | "get_IsEmpty", Some c, _ -> Test(c, ListTest false, r) |> Some
    | "get_Empty", None, _ -> NewList(None, (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    | "Cons", None, [ h; t ] -> NewList(Some(h, t), (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    // | ("GetHashCode" | "Equals" | "CompareTo"), Some c, _ ->
    //     makeInstanceCall r t i c i.CompiledName args |> Some
    | "GetEnumerator", Some c, _ -> libCall com r t "Seq" "Enumerable::ofList" [ c ] |> Some
    | _ -> None

let listModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "IsEmpty", [ arg ] -> Test(arg, ListTest false, r) |> Some
    | "Empty", _ -> NewList(None, (genArg com ctx r 0 i.GenericArgs)) |> makeValue r |> Some
    | "Singleton", [ arg ] ->
        NewList(Some(arg, Value(NewList(None, t), None)), (genArg com ctx r 0 i.GenericArgs))
        |> makeValue r
        |> Some
    // Use a cast to give it better chances of optimization (e.g. converting list
    // literals to arrays) after the beta reduction pass
    | "ToSeq", [ arg ] -> makeLibCall com r t i "Seq" "ofList" args |> Some
    | "OfSeq", [ arg ] -> makeLibCall com r t i "List" "ofSeq" args |> Some
    | ("Concat" | "Transpose") as meth, [ arg ] ->
        makeLibCall com r t i "List" (Naming.lowerFirst meth) [ toList com t arg ]
        |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy") as meth, args ->
        let meth = Naming.lowerFirst meth

        makeLibCall com r t i "List" meth args |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth

        makeLibCall com r t i "List" meth args |> Some

let discardUnitArgs args =
    match args with
    | Value(UnitConstant, _) :: rest -> rest
    | _ -> args

let sets (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let args = discardUnitArgs args

    match i.CompiledName, thisArg with
    | ".ctor", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeSet com ctx r t args |> Some
    | ReplaceName [ "get_MinimumElement", "minElement"
                    "get_MaximumElement", "maxElement"
                    "IsSubsetOf", "isSubset"
                    "IsSupersetOf", "isSuperset"
                    "IsProperSubsetOf", "isProperSubset"
                    "IsProperSupersetOf", "isProperSuperset"
                    "CopyTo", "copyToArray" ] meth,
      Some c -> libCall com r t "Set" meth (c :: args) |> Some
    | meth, Some c ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        libCall com r t "Set" meth (args @ [ c ]) |> Some
    | meth, None ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        libCall com r t "Set" meth args |> Some

let setModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let meth = Naming.lowerFirst i.CompiledName

    makeLibCall com r t i "Set" meth args |> Some

let maps (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let args = discardUnitArgs args

    match i.CompiledName, thisArg with
    | ".ctor", _ -> (genArg com ctx r 0 i.GenericArgs) |> makeMap com ctx r t args |> Some
    | ReplaceName [ "CopyTo", "copyToArray" ] meth, Some c -> libCall com r t "Map" meth (c :: args) |> Some
    | meth, Some c ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        libCall com r t "Map" meth (args @ [ c ]) |> Some
    | meth, None ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        libCall com r t "Map" meth args |> Some

let mapModule (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    let meth = Naming.lowerFirst i.CompiledName

    makeLibCall com r t i "Map" meth args |> Some

let results (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Bind"
    | "Map"
    | "MapError"
    | "IsOk"
    | "IsError"
    | "Contains"
    | "Count"
    | "DefaultValue"
    | "DefaultWith"
    | "Exists"
    | "Fold"
    | "FoldBack"
    | "ForAll"
    | "Iterate"
    | "ToArray"
    | "ToList"
    | "ToOption"
    | "ToValueOption" as meth -> makeLibCall com r t i "Result" (Naming.lowerFirst meth) args |> Some
    | _ -> None

let nullables (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", None -> NewOption(List.tryHead args, t.Generics.Head, false) |> makeValue r |> Some
    | "get_Value", Some c -> libCall com r t "Option" "getValue" [ c ] |> Some
    | "get_HasValue", Some c -> Test(c, OptionTest true, r) |> Some
    | _ -> None

// See fable-library-ts/Option.ts for more info on how options behave in Fable runtime
let options isStruct (com: ICompiler) (_: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | "Some", _ -> NewOption(List.tryHead args, t.Generics.Head, isStruct) |> makeValue r |> Some
    | "get_None", _ -> NewOption(None, t.Generics.Head, isStruct) |> makeValue r |> Some
    | "get_Value", Some c -> Get(c, OptionValue, t, r) |> Some
    | "get_IsSome", Some c -> Test(c, OptionTest true, r) |> Some
    | "get_IsNone", Some c -> Test(c, OptionTest false, r) |> Some
    | _ -> None

let optionModule isStruct (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "None", _ -> NewOption(None, t, isStruct) |> makeValue r |> Some
    | "GetValue", [ c ] -> Get(c, OptionValue, t, r) |> Some
    | "IsSome", [ c ] -> Test(c, OptionTest true, r) |> Some
    | "IsNone", [ c ] -> Test(c, OptionTest false, r) |> Some
    | "OfObj", [ arg ] -> libCall com r t "Native" "ofObj" args |> Some
    | "ToObj", [ arg ] -> libCall com r t "Native" "toObj" args |> Some
    // | "ToArray", [ arg ] -> libCall com r t "Array" "ofOption" args |> Some
    // | "ToList", [ arg ] -> libCall com r t "List" "ofOption" args |> Some
    | meth, args -> makeLibCall com r t i "Option" (Naming.lowerFirst meth) args |> Some

let parseBool (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | ("Parse" | "TryParse" as method), args ->
        let meth = Naming.lowerFirst method + "Boolean"

        makeLibCall com r t i "Convert" meth args |> Some
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), _ -> valueTypes com ctx r t i thisArg args
    | _ -> None

let parseNum (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let parseCall meth str args style =
        let moduleName, memberName, withStyleArg =
            match t with
            | Number(Decimal, _) -> "Decimal", Naming.lowerFirst meth, false
            | Number(BigInt, _) -> "BigInt", Naming.lowerFirst meth, false
            | Number(kind, _) when meth = "Parse" -> "Convert", Naming.lowerFirst meth + kind.ToString(), true
            | _ -> "Convert", Naming.lowerFirst meth, true

        let outValue =
            if meth = "TryParse" then
                [ List.last args ]
            else
                []

        let args =
            if not withStyleArg then
                [ str ] @ outValue
            else
                [ str; makeIntConst style ] @ outValue

        libCall com r t moduleName memberName args

    let isFloat =
        match i.SignatureArgTypes with
        | Number((Float16 | Float32 | Float64), _) :: _ -> true
        | _ -> false

    match i.CompiledName, thisArg, args with
    | "IsNaN", _, [ arg ] when isFloat -> makeInstanceCall r t i arg "is_nan" [] |> Some
    | "Log2", _, [ arg ] ->
        let log =
            if isFloat then
                makeInstanceCall r t i arg "log2" []
            else
                makeInstanceCall r UInt32.Number i arg "ilog2" []

        TypeCast(log, t) |> Some
    | "IsPositiveInfinity", _, [ arg ] when isFloat ->
        let op1 = makeInstanceCall r t i arg "is_sign_positive" []
        let op2 = makeInstanceCall r t i arg "is_infinite" []
        Operation(Logical(LogicalAnd, op1, op2), Tags.empty, t, None) |> Some
    | "IsNegativeInfinity", _, [ arg ] when isFloat ->
        let op1 = makeInstanceCall r t i arg "is_sign_negative" []
        let op2 = makeInstanceCall r t i arg "is_infinite" []
        Operation(Logical(LogicalAnd, op1, op2), Tags.empty, t, None) |> Some
    | "IsInfinity", _, [ arg ] when isFloat -> makeInstanceCall r t i arg "is_infinite" [] |> Some
    | ("Min" | "Max" | "MinMagnitude" | "MaxMagnitude" | "Clamp"), _, _ -> operators com ctx r t i thisArg args
    | ("Parse" | "TryParse") as meth, _, str :: NumberConst(NumberValue.Int32 style, _) :: _ ->
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
    | ("Parse" | "TryParse") as meth, _, str :: _ ->
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
    | "Pow", _, (arg :: restArgs) -> makeInstanceCall r t i arg "powf" restArgs |> Some
    // | "ToString", [StringConst fmt] ->
    //     let rustFmt = fmt // TODO: replace format specifiers with proper Rust format
    //     let format = makeStrConst ("{0:" + rustFmt + "}")
    //     "sprintf!" |> emitFormat com r t [format; thisArg] |> Some
    | "ToString", Some c, _ -> toString com ctx r [ c ] |> Some
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), thisArg, _ -> valueTypes com ctx r t i thisArg args
    | _ -> None

/// Drops culture and style arguments from a call.
///
/// Rust's formatting and parsing are invariant, which is exactly what
/// InvariantCulture asks for, so the overloads that take a provider are
/// equivalent to the ones that do not. Passing the argument on was not: the
/// runtime functions have no parameter for it, so `Decimal.Parse(s, style,
/// culture)` reached a one-argument function and `d.ToString(culture)` put a
/// provider where a format string belongs.
///
/// Filtering by argument type rather than by position leaves the arity of every
/// other overload alone.
let dropCultureArgs (args: Expr list) =
    let isCultureOrStyle (e: Expr) =
        match e.Type with
        | DeclaredType(ent, _) ->
            match ent.FullName with
            | Types.cultureInfo
            | "System.IFormatProvider" -> true
            | _ -> false
        | Number(_, NumberInfo.IsEnum ent) ->
            match ent.FullName with
            | "System.Globalization.NumberStyles"
            | "System.Globalization.DateTimeStyles" -> true
            | _ -> false
        | _ -> false

    args |> List.filter (isCultureOrStyle >> not)

let decimals (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let args = dropCultureArgs args

    match i.CompiledName, thisArg, args with
    | (".ctor" | "MakeDecimal"), _, ([ low; mid; high; isNegative; scale ] as args) ->
        makeLibCall com r t i "Decimal" "fromParts" args |> Some
    | ".ctor", _, [ Value(NewArray(ArrayValues([ low; mid; high; signExp ] as args), _, _), _) ] ->
        makeLibCall com r t i "Decimal" "fromInts" args |> Some
    | ".ctor", _, [ arg ] -> convertTo com ctx r t args |> Some
    | "GetBits", _, _ -> makeLibCall com r t i "Decimal" "getBits" args |> Some
    | "Parse", _, _ -> makeLibCall com r t i "Decimal" "parse" args |> Some
    | "TryParse", _, _ -> makeLibCall com r t i "Decimal" "tryParse" args |> Some
    | Patterns.SetContains Operators.compareSet, _, [ left; right ] ->
        applyCompareOp com ctx r t i.CompiledName left right |> Some
    | Patterns.SetContains Operators.standardSet, _, _ -> applyOp com ctx r t i.CompiledName args |> Some
    | "op_Explicit", _, _ -> convertTo com ctx r t args |> Some
    | ("Abs" | "Sign" | "Ceiling" | "Floor" | "Truncate" | "Min" | "Max" | "MinMagnitude" | "MaxMagnitude" | "Clamp" | "Add" | "Subtract" | "Multiply" | "Divide" | "Remainder" | "Negate") as meth,
      _,
      _ ->
        let meth = Naming.lowerFirst meth
        makeLibCall com r t i "Decimal" meth args |> Some
    | ("CopySign" | "FromOACurrency" | "GetTypeCode" | "ToOACurrency") as meth, _, _ ->
        makeLibCall com r t i "Decimal" (Naming.lowerFirst meth) args |> Some
    | ("get_Zero" | "get_One" | "get_MinusOne" | "get_MinValue" | "get_MaxValue"), _, _ ->
        libValue com r t "Decimal" (Naming.removeGetSetPrefix i.CompiledName) [] |> Some
    | ("IsInteger" | "IsEvenInteger" | "IsOddInteger" | "IsCanonical" | "IsNegative" | "IsPositive"), _, _ ->
        makeLibCall com r t i "Decimal" (Naming.lowerFirst i.CompiledName) args |> Some
    | "get_Scale", Some c, [] -> makeLibCall com r t i "Decimal" "scale" [ c ] |> Some
    | "get_Scale", None, [] -> None
    | "Round", _, _ ->
        match args with
        | [ x ] -> makeLibCall com r t i "Decimal" "round" args |> Some
        | [ x; ExprTypeAs(Number(Int32, _), dp) ] -> makeLibCall com r t i "Decimal" "roundTo" args |> Some
        | [ x; mode ] -> makeLibCall com r t i "Decimal" "roundMode" args |> Some
        | [ x; dp; mode ] -> makeLibCall com r t i "Decimal" "roundToMode" args |> Some
        | _ -> None
    // | "ToString", [StringConst fmt] ->
    //     let rustFmt = fmt // TODO: replace format specifiers with proper Rust format
    //     let format = makeStrConst ("{0:" + rustFmt + "}")
    //     "sprintf!" |> emitFormat com r t [format; thisArg] |> Some
    | "ToString", Some c, [] ->
        // For d.ToString(provider) the provider is the only argument, so dropping
        // it leaves nothing for toString's decimal parameter; the receiver takes
        // its place.
        let args = [ c ]

        makeLibCall com r t i "Decimal" "toString" args |> Some
    | "ToString", _, _ -> makeLibCall com r t i "Decimal" "toString" args |> Some
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), thisArg, _ -> valueTypes com ctx r t i thisArg args
    | _ -> None

let bigints (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", None, [ arg ] -> convertTo com ctx r t args |> Some
    | Patterns.SetContains Operators.compareSet, _, [ left; right ] ->
        applyCompareOp com ctx r t i.CompiledName left right |> Some
    | Patterns.SetContains Operators.standardSet, _, _ -> applyOp com ctx r t i.CompiledName args |> Some
    | "DivRem", None, [ x; y ] -> makeLibCall com r t i "BigInt" "divRem" args |> Some
    | "DivRem", None, [ x; y; rem ] -> makeLibCall com r t i "BigInt" "divRemOut" args |> Some
    | "op_Explicit", None, _ -> convertTo com ctx r t args |> Some
    | "Log", None, [ arg1; arg2 ] -> makeLibCall com r t i "BigInt" "log" args |> Some
    | "Log", None, [ arg ] -> makeLibCall com r t i "BigInt" "ln" args |> Some
    | "Log2", None, [ arg ] -> makeLibCall com r t i "BigInt" "ilog2" args |> Some
    | meth, None, _ when meth.StartsWith("get_", StringComparison.Ordinal) ->
        let meth = meth |> Naming.removeGetSetPrefix |> Naming.lowerFirst
        libCall com r t "BigInt" meth [] |> Some
    | meth, None, _ -> makeLibCall com r t i "BigInt" (Naming.lowerFirst meth) args |> Some
    | meth, Some c, _ -> makeLibCall com r t i "BigInt" (Naming.lowerFirst meth) (c :: args) |> Some

// Compile static strings to their constant values
// reference: https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/languageprimitives.errorstrings-module-%5bfsharp%5d
let errorStrings =
    function
    | "InputArrayEmptyString" -> str "The input array was empty" |> Some
    | "InputSequenceEmptyString" -> str "The input sequence was empty" |> Some
    | "InputMustBeNonNegativeString" -> str "The input must be non-negative" |> Some
    | _ -> None

let languagePrimitives (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
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
    | "GenericZero", _ -> libCall com r t "Native" "defaultOf" [] |> Some
    // | "GenericZero", _ -> getZero com ctx t |> Some
    | "GenericOne", _ -> getOne com ctx t |> Some
    | ("SByteWithMeasure" | "Int16WithMeasure" | "Int32WithMeasure" | "Int64WithMeasure" | "Float32WithMeasure" | "FloatWithMeasure" | "DecimalWithMeasure"),
      [ arg ] -> arg |> Some
    | "EnumOfValue", [ arg ] -> TypeCast(arg, t) |> Some
    | "EnumToValue", [ arg ] -> TypeCast(arg, t) |> Some
    | ("GenericHash" | "GenericHashIntrinsic"), [ arg ] -> getHashCode com ctx r arg |> Some
    | ("FastHashTuple2" | "FastHashTuple3" | "FastHashTuple4" | "FastHashTuple5" | "GenericHashWithComparer" | "GenericHashWithComparerIntrinsic"),
      [ comp; arg ] -> makeInstanceCall r t i comp "GetHashCode" [ arg ] |> Some
    | ("GenericComparison" | "GenericComparisonIntrinsic"), [ left; right ] -> compare com ctx r left right |> Some
    | ("FastCompareTuple2" | "FastCompareTuple3" | "FastCompareTuple4" | "FastCompareTuple5" | "GenericComparisonWithComparer" | "GenericComparisonWithComparerIntrinsic"),
      [ comp; left; right ] -> makeInstanceCall r t i comp "Compare" [ left; right ] |> Some
    | ("GenericLessThan" | "GenericLessThanIntrinsic"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryLess |> Some
    | ("GenericLessOrEqual" | "GenericLessOrEqualIntrinsic"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryLessOrEqual |> Some
    | ("GenericGreaterThan" | "GenericGreaterThanIntrinsic"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryGreater |> Some
    | ("GenericGreaterOrEqual" | "GenericGreaterOrEqualIntrinsic"), [ left; right ] ->
        booleanCompare com ctx r left right BinaryGreaterOrEqual |> Some
    | ("GenericEquality" | "GenericEqualityIntrinsic"), [ left; right ] -> equals com ctx r left right |> Some
    | ("GenericEqualityER" | "GenericEqualityERIntrinsic"), [ left; right ] ->
        // TODO: In ER mode, equality on two NaNs returns "true".
        equals com ctx r left right |> Some
    | ("FastEqualsTuple2" | "FastEqualsTuple3" | "FastEqualsTuple4" | "FastEqualsTuple5" | "GenericEqualityWithComparer" | "GenericEqualityWithComparerIntrinsic"),
      [ comp; left; right ] -> makeInstanceCall r t i comp "Equals" [ left; right ] |> Some
    | ("PhysicalEquality" | "PhysicalEqualityIntrinsic"), [ left; right ] ->
        referenceEquals com ctx r left right |> Some
    | ("PhysicalHash" | "PhysicalHashIntrinsic"), [ arg ] -> referenceHash com ctx r arg |> Some
    | ("GenericEqualityComparer" | "GenericEqualityERComparer" | "FastGenericComparer" | "FastGenericComparerFromTable" | "FastGenericEqualityComparer" | "FastGenericEqualityComparerFromTable"),
      _ -> fsharpModule com ctx r t i thisArg args
    | ("ParseInt32" | "ParseUInt32" | "ParseInt64" | "ParseUInt64"), [ arg ] -> convertTo com ctx r t args |> Some
    | _ -> None

let intrinsicFunctions (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    // Erased operators
    | "CheckThis", _, [ arg ] -> Some arg
    | "UnboxFast", _, [ arg ] -> Some arg
    | "UnboxGeneric", _, [ arg ] -> TypeCast(arg, t) |> Some
    | "MakeDecimal", _, _ -> decimals com ctx r t i thisArg args
    | "GetString", _, [ ar; idx ] -> libCall com r t "String" "getCharAt" args |> Some
    | "GetStringSlice", None, [ ar; lower; upper ] -> makeLibCall com r t i "String" "getSlice" args |> Some
    | "GetArray", _, [ ar; idx ] -> getExpr r t ar idx |> Some
    | "SetArray", _, [ ar; idx; value ] -> setExpr r ar idx value |> Some
    | "GetArraySlice", None, [ ar; lower; upper ] -> makeLibCall com r t i "Array" "getSlice" args |> Some
    | "SetArraySlice", None, args -> makeLibCall com r t i "Array" "setSlice" args |> Some
    | ("TypeTestGeneric" | "TypeTestFast"), None, [ expr ] ->
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
    | "PowDouble", None, (thisArg :: restArgs) -> makeInstanceCall r t i thisArg "powf" restArgs |> Some
    | "PowDecimal", None, _ -> makeLibCall com r t i "Decimal" "pown" args |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangechar-function-%5bfsharp%5d
    // Type: RangeChar : char -> char -> char seq
    // Usage: RangeChar start stop
    | "RangeChar", None, _ -> makeLibCall com r t i "Range" "rangeChar" args |> Some
    // reference: https://msdn.microsoft.com/visualfsharpdocs/conceptual/operatorintrinsics.rangedouble-function-%5bfsharp%5d
    // Type: RangeDouble: float -> float -> float -> float seq
    // Usage: RangeDouble start step stop
    | ("RangeSByte" | "RangeByte" | "RangeInt16" | "RangeUInt16" | "RangeInt32" | "RangeUInt32" | "RangeInt64" | "RangeUInt64" | "RangeSingle" | "RangeDouble"),
      None,
      args -> makeLibCall com r t i "Range" "rangeNumeric" args |> Some
    | _ -> None

let runtimeHelpers (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, args with
    | "GetHashCode", [ arg ] -> getHashCode com ctx r arg |> Some
    | _ -> None

// ExceptionDispatchInfo is used to raise exceptions through different threads in async workflows
// We don't need to do anything in JS, see #2396
let exceptionDispatchInfo (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg, args with
    | "Capture", _, [ arg ] -> Some arg
    | "Throw", Some arg, _ -> makeThrow r t arg |> Some
    | _ -> None

let funcs (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg with
    | "Adapt", _ ->
        match args, t with
        | [ arg ], DeclaredType(_, genArgs) -> uncurryExprAtRuntime com (List.length genArgs - 1) arg |> Some
        | _ -> emitExpr r t args "$0" |> Some
    | "Invoke", Some c -> Helper.Application(c, t, args, i.SignatureArgTypes, ?loc = r) |> Some
    | _ -> None

let keyValuePairs (com: ICompiler) (ctx: Context) r t (i: CallInfo) thisArg args =
    match i.CompiledName, thisArg with
    | ".ctor", _ -> makeTuple r true args |> Some
    | "get_Key", Some c -> Get(c, TupleIndex 0, t, r) |> Some
    | "get_Value", Some c -> Get(c, TupleIndex 1, t, r) |> Some
    | _ -> None

let dictionaries (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", None ->
        match i.SignatureArgTypes with
        | [] -> libCall com r t "HashMap" "new_empty" args |> Some
        | [ Number _ ] -> libCall com r t "HashMap" "new_with_capacity" args |> Some
        | [ IEqualityComparer ] -> libCall com r t "HashMap" "new_with_comparer" args |> Some
        | [ Number _; IEqualityComparer ] -> libCall com r t "HashMap" "new_with_capacity_comparer" args |> Some
        | [ IEnumerable ] -> libCall com r t "HashMap" "new_from_enumerable" args |> Some
        | [ IEnumerable; IEqualityComparer ] -> libCall com r t "HashMap" "new_from_enumerable_comparer" args |> Some
        | [ IDictionary ] -> libCall com r t "HashMap" "new_from_dictionary" args |> Some
        | [ IDictionary; IEqualityComparer ] -> libCall com r t "HashMap" "new_from_dictionary_comparer" args |> Some
        | _ -> None
    | "GetEnumerator", Some c ->
        let ar = libCall com r t "HashMap" "entries" [ c ]
        libCall com r t "Seq" "Enumerable::ofArray" [ ar ] |> Some
    | "get_Item", Some c -> makeLibModuleCall com r t i "HashMap" "get" (Some c) args |> Some
    | "set_Item", Some c -> makeLibModuleCall com r t i "HashMap" "set" (Some c) args |> Some
    | meth, _ ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        makeLibModuleCall com r t i "HashMap" meth thisArg args |> Some

let hashSets (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", None ->
        match i.SignatureArgTypes with
        | [] -> libCall com r t "HashSet" "new_empty" args |> Some
        | [ Number _ ] -> libCall com r t "HashSet" "new_with_capacity" args |> Some
        | [ IEqualityComparer ] -> libCall com r t "HashSet" "new_with_comparer" args |> Some
        | [ Number _; IEqualityComparer ] -> libCall com r t "HashSet" "new_with_capacity_comparer" args |> Some
        | [ IEnumerable ] -> libCall com r t "HashSet" "new_from_enumerable" args |> Some
        | [ IEnumerable; IEqualityComparer ] -> libCall com r t "HashSet" "new_from_enumerable_comparer" args |> Some
        | _ -> None
    | "GetEnumerator", Some c ->
        let ar = libCall com r t "HashSet" "entries" [ c ]
        libCall com r t "Seq" "Enumerable::ofArray" [ ar ] |> Some
    | "CopyTo", Some c ->
        let meth = i.CompiledName |> toLowerFirstWithArgsCountSuffix (c :: args)
        makeLibModuleCall com r t i "HashSet" meth thisArg args |> Some
    | ("UnionWith" | "IntersectWith" | "ExceptWith" | "SymmetricExceptWith" | "Overlaps" | "SetEquals"), Some _
    | ("IsProperSubsetOf" | "IsProperSupersetOf" | "IsSubsetOf" | "IsSupersetOf"), Some _ ->
        let meth = Naming.lowerFirst i.CompiledName
        makeLibModuleCall com r t i "HashSet" meth thisArg args |> Some
    | meth, _ ->
        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        makeLibModuleCall com r t i "HashSet" meth thisArg args |> Some

let collections (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg with
    | Some c ->
        match c.Type with
        | IsEntity (Types.keyCollection) _
        | IsEntity (Types.valueCollection) _
        | IsEntity (Types.icollectionGeneric) _
        | Array _ -> resizeArrays com ctx r t i thisArg args
        | List _ -> lists com ctx r t i thisArg args
        | IsEntity (Types.hashset) _
        | IsEntity (Types.iset) _ -> hashSets com ctx r t i thisArg args
        | IsEntity (Types.dictionary) _
        | IsEntity (Types.idictionary) _
        | IsEntity (Types.ireadonlydictionary) _ -> dictionaries com ctx r t i thisArg args
        | _ -> None
    | _ -> None

let exceptions (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", None -> bclType com ctx r t i thisArg args
    | "get_Message", Some ex -> makeInstanceCall r t i ex i.CompiledName args |> Some
    | "get_StackTrace", Some ex -> makeInstanceCall r t i ex i.CompiledName args |> Some
    | "get_InnerException", Some ex -> makeInstanceCall r t i ex i.CompiledName args |> Some
    | _ -> None

let unchecked (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | "DefaultOf", _ -> (genArg com ctx r 0 i.GenericArgs) |> getZero com ctx |> Some
    | "Hash", [ arg ] -> getHashCode com ctx r arg |> Some
    | "Equals", [ arg1; arg2 ] -> equals com ctx r arg1 arg2 |> Some
    | "Compare", [ arg1; arg2 ] -> compare com ctx r arg1 arg2 |> Some
    | "NonNull", [ arg ] -> arg |> Some
    | _ -> None

let enums (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "HasFlag", Some c, [ arg ] ->
        // x.HasFlags(y) => (int x) &&& (int y) <> 0
        makeBinOp r (Int32.Number) c arg BinaryAndBitwise
        |> fun bitwise -> makeEqOp r bitwise (makeIntConst 0) BinaryUnequal
        |> Some
    | Patterns.DicContains (dict [ "Parse", "parseEnum"
                                   "TryParse", "tryParseEnum"
                                   "IsDefined", "isEnumDefined"
                                   "GetName", "getEnumName"
                                   "GetNames", "getEnumNames"
                                   "GetValues", "getEnumValues"
                                   "GetUnderlyingType", "getEnumUnderlyingType" ]) meth,
      None,
      args ->
        let args =
            match meth, args with
            // TODO: Parse at compile time if we know the type
            | "parseEnum", [ value ] -> [ makeTypeInfo None t; value ]
            | "tryParseEnum", [ value; refValue ] ->
                [ genArg com ctx r 0 i.GenericArgs |> makeTypeInfo None; value; refValue ]
            | _ -> args

        libCall com r t "Reflection" meth args |> Some
    | _ -> None

let bitConvert (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "GetBytes" ->
        let memberName =
            match args.Head.Type with
            | Boolean -> "getBytesBoolean"
            | Char -> "getBytesChar"
            | Number(kind, _) -> "getBytes" + kind.ToString()
            | x -> FableError $"Unsupported type in BitConverter.GetBytes(): %A{x}" |> raise

        let expr = makeLibCall com r t i "BitConverter" memberName args

        if com.Options.TypedArrays then
            expr |> Some
        else
            toArray com t expr |> Some // convert to dynamic array
    | "ToString" ->
        let memberName = "toString" + args.Length.ToString()

        makeLibCall com r t i "BitConverter" memberName args |> Some
    | _ ->
        let memberName = Naming.lowerFirst i.CompiledName

        makeLibCall com r t i "BitConverter" memberName args |> Some

let convert (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName, args with
    | ("ToSByte" | "ToByte" | "ToInt16" | "ToUInt16" | "ToInt32" | "ToUInt32" | "ToInt64" | "ToUInt64") as meth,
      [ ExprType(String); ExprType(Number(Int32, _)) ] -> toRadixInt com ctx r t i args |> Some
    | ("ToSByte" | "ToByte" | "ToInt16" | "ToUInt16" | "ToInt32" | "ToUInt32" | "ToInt64" | "ToUInt64"), [ arg ] ->
        toRoundInt com ctx r t i args |> Some
    | ("ToSingle" | "ToDouble" | "ToDecimal"), [ arg ] -> convertTo com ctx r t args |> Some
    | "ToChar", [ arg ] -> convertTo com ctx r t args |> Some
    | "ToString", [ arg ] -> toString com ctx r args |> Some
    | "ToString", [ arg; ExprType(Number(Int32, _)) ] -> libCall com r t "Convert" "toStringRadix" args |> Some
    | ("ToHexString" | "ToHexStringLower" | "FromHexString" | "ToBase64String" | "FromBase64String"), [ arg ] ->
        libCall com r t "Convert" (Naming.lowerFirst i.CompiledName) args |> Some
    | _ -> None

let console (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "get_Out" -> typedObjExpr t [] |> Some // empty object
    | "Write" -> "printf!" |> emitFormat com r t args |> Some
    | "WriteLine" -> "printfn!" |> emitFormat com r t args |> Some
    | _ -> None

let debug (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Write" -> "printf!" |> emitFormat com r t args |> Some
    | "WriteLine" -> "printfn!" |> emitFormat com r t args |> Some
    | "Break" -> makeDebugger r |> Some
    | "Assert" ->
        match args with
        | [ condition ] -> "assert!" |> emitExpr r t args |> Some
        | _ -> None
    | _ -> None

let ignoreFormatProvider compiledName args =
    match compiledName, args with
    // Ignore IFormatProvider
    | "ToString", ExprTypeAs(String, arg) :: _ -> [ arg ]
    | "ToString", _ -> [ makeStrConst "" ] // default (no format string)
    | "Parse", arg :: _ -> [ arg ]
    | "TryParse", input :: _culture :: _styles :: defVal :: _ -> [ input; defVal ]
    | "TryParse", input :: _culture :: defVal :: _ -> [ input; defVal ]
    | _ -> args

let makeMemberCall com ctx r t i moduleName memberName (thisArg: Expr option) (args: Expr list) =
    let memberName = Naming.removeGetSetPrefix memberName |> Naming.lowerFirst
    let args = ignoreFormatProvider i.CompiledName args

    match thisArg with
    | Some c -> makeInstanceCall r t i c memberName args
    | None -> makeStaticMemberCall com r t i moduleName memberName args

let dateTimes (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match args with
        | [] -> "new_empty" |> Some
        | [ ExprType(Number(Int64, _)) ] -> "new_ticks" |> Some
        | [ ExprType(Number(Int64, _)); _kind ] -> "new_ticks_kind" |> Some
        | [ ExprType(DeclaredType(ent, [])); _timeOnly ] when ent.FullName = Types.dateOnly -> "new_date_time" |> Some
        | [ ExprType(DeclaredType(ent, [])); _timeOnly; _kind ] when ent.FullName = Types.dateOnly ->
            "new_date_time_kind" |> Some
        | [ ExprType(Number(Int32, _)); ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] -> "new_ymd" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(_, NumberInfo.IsEnum ent)) ] when ent.FullName = "System.DateTimeKind" ->
            "new_ymdhms_kind" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(_, NumberInfo.IsEnum ent)) ] when ent.FullName = "System.DateTimeKind" ->
            "new_ymdhms_milli_kind" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(_, NumberInfo.IsEnum ent)) ] when ent.FullName = "System.DateTimeKind" ->
            "new_ymdhms_micro_kind" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> "new_ymdhms" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> "new_ymdhms_milli" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> "new_ymdhms_micro" |> Some
        | _ -> None
        |> Option.map (fun meth -> makeStaticMemberCall com r t i "DateTime" meth args)
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), thisArg -> valueTypes com ctx r t i thisArg args
    | "Add", Some c ->
        Operation(Binary(BinaryOperator.BinaryPlus, c, args.Head), Tags.empty, t, r)
        |> Some
    | "Add", None -> None
    | "Subtract", Some c ->
        Operation(Binary(BinaryOperator.BinaryMinus, c, args.Head), Tags.empty, t, r)
        |> Some
    | "Subtract", None -> None
    | meth, thisArg -> makeMemberCall com ctx r t i "DateTime" meth thisArg args |> Some

let dateTimeOffsets (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match args with
        | [] -> "new_empty" |> Some
        | ExprType(Number(Int64, _)) :: _ -> "new_ticks" |> Some
        | [ ExprType(DeclaredType(ent, [])) ] when ent.FullName = Types.datetime -> "new_datetime" |> Some
        | ExprType(DeclaredType(ent, [])) :: _ when ent.FullName = Types.datetime -> "new_datetime2" |> Some
        | ExprType(DeclaredType(ent, [])) :: _ when ent.FullName = Types.dateOnly -> "new_date_time" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            _offset ] -> "new_ymdhms" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            _offset ] -> "new_ymdhms_milli" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            _offset ] -> "new_ymdhms_micro" |> Some
        | _ -> None
        |> Option.map (fun meth -> makeStaticMemberCall com r t i "DateTimeOffset" meth args)
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), thisArg -> valueTypes com ctx r t i thisArg args
    | "Add", Some c ->
        Operation(Binary(BinaryOperator.BinaryPlus, c, args.Head), Tags.empty, t, r)
        |> Some
    | "Add", None -> None
    | "Subtract", Some c ->
        Operation(Binary(BinaryOperator.BinaryMinus, c, args.Head), Tags.empty, t, r)
        |> Some
    | "Subtract", None -> None
    | meth, thisArg -> makeMemberCall com ctx r t i "DateTimeOffset" meth thisArg args |> Some

let dateOnly (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match args with
        | [ ExprType(Number(Int32, _)); ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] -> "new_ymd" |> Some
        | _ -> None
        |> Option.map (fun meth -> makeStaticMemberCall com r t i "DateOnly" meth args)
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), thisArg -> valueTypes com ctx r t i thisArg args
    | "ToDateTime", Some c when args.Length = 2 -> makeInstanceCall r t i c "toDateTime2" args |> Some
    | "ToDateTime", None when args.Length = 2 -> None
    | meth, thisArg -> makeMemberCall com ctx r t i "DateOnly" meth thisArg args |> Some

let timeOnly (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match args with
        | [ ExprType(Number(Int64, _)) ] -> "new_ticks" |> Some
        | [ ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] -> "new_hm" |> Some
        | [ ExprType(Number(Int32, _)); ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] -> "new_hms" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> "new_hms_milli" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> "new_hms_micro" |> Some
        | _ -> None
        |> Option.map (fun meth -> makeStaticMemberCall com r t i "TimeOnly" meth args)
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), thisArg -> valueTypes com ctx r t i thisArg args
    | "Add", Some c when args.Length = 2 -> makeInstanceCall r t i c "add2" args |> Some
    | "Add", None when args.Length = 2 -> None
    | meth, thisArg -> makeMemberCall com ctx r t i "TimeOnly" meth thisArg args |> Some

let runes (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", None, [ ExprType(Char) ] -> Some args.Head
    | ".ctor", None, [ ExprType(Number(Int32, _)) ] -> makeLibCall com r t i "Rune" "newInt" args |> Some
    | ".ctor", None, [ ExprType(Number(UInt32, _)) ] -> makeLibCall com r t i "Rune" "newUInt" args |> Some
    | ".ctor", None, [ ExprType(Char); ExprType(Char) ] -> makeLibCall com r t i "Rune" "newPair" args |> Some
    | ("ReplacementChar" | "get_ReplacementChar"), None, [] -> makeLibCall com r t i "Rune" "replacementChar" [] |> Some
    | "op_Explicit", None, [ ExprType(Char) ] -> Some args.Head
    | "op_Explicit", None, [ ExprType(Number(Int32, _)) ] -> makeLibCall com r t i "Rune" "newInt" args |> Some
    | "op_Explicit", None, [ ExprType(Number(UInt32, _)) ] -> makeLibCall com r t i "Rune" "newUInt" args |> Some
    | "TryCreate", None, [ ExprType(Char); _ ] -> makeLibCall com r t i "Rune" "tryCreateChar" args |> Some
    | "TryCreate", None, [ ExprType(Char); ExprType(Char); _ ] ->
        makeLibCall com r t i "Rune" "tryCreatePair" args |> Some
    | "TryCreate", None, [ ExprType(Number(Int32, _)); _ ] -> makeLibCall com r t i "Rune" "tryCreateInt" args |> Some
    | "TryCreate", None, [ ExprType(Number(UInt32, _)); _ ] -> makeLibCall com r t i "Rune" "tryCreateUInt" args |> Some
    | "get_Value", Some c, [] -> makeLibCall com r t i "Rune" "value" [ c ] |> Some
    | "get_Utf8SequenceLength", Some c, [] -> makeLibCall com r t i "Rune" "utf8SequenceLength" [ c ] |> Some
    | "get_Utf16SequenceLength", Some c, [] -> makeLibCall com r t i "Rune" "utf16SequenceLength" [ c ] |> Some
    | "get_IsAscii", Some c, [] -> makeLibCall com r t i "Rune" "isAscii" [ c ] |> Some
    | "get_IsBmp", Some c, [] -> makeLibCall com r t i "Rune" "isBmp" [ c ] |> Some
    | "get_Plane", Some c, [] -> makeLibCall com r t i "Rune" "plane" [ c ] |> Some
    | "GetNumericValue", None, [ arg ] -> makeLibCall com r t i "Rune" "getNumericValue" [ arg ] |> Some
    | "GetUnicodeCategory", None, [ arg ] -> makeLibCall com r t i "Rune" "getUnicodeCategory" [ arg ] |> Some
    | ("IsControl" | "IsDigit" | "IsLetter" | "IsLetterOrDigit" | "IsLower" | "IsNumber" | "IsPunctuation" | "IsSeparator" | "IsSymbol" | "IsUpper" | "IsWhiteSpace"),
      None,
      [ arg ] ->
        let meth = Naming.lowerFirst i.CompiledName
        makeLibCall com r t i "Rune" meth args |> Some
    | "IsValid", None, [ ExprType(Number(Int32, _)) ] -> makeLibCall com r t i "Rune" "isValidInt" args |> Some
    | "IsValid", None, [ ExprType(Number(UInt32, _)) ] -> makeLibCall com r t i "Rune" "isValidUInt" args |> Some
    | "ToLower", None, [ runeArg; ExprType(DeclaredType(ent, _)) ] when ent.FullName = Types.cultureInfo ->
        makeLibCall com r t i "Rune" "toLowerInvariant" [ runeArg ] |> Some
    | "ToLowerInvariant", None, [ arg ] -> makeLibCall com r t i "Rune" "toLowerInvariant" [ arg ] |> Some
    | "ToUpper", None, [ runeArg; ExprType(DeclaredType(ent, _)) ] when ent.FullName = Types.cultureInfo ->
        makeLibCall com r t i "Rune" "toUpperInvariant" [ runeArg ] |> Some
    | "ToUpperInvariant", None, [ arg ] -> makeLibCall com r t i "Rune" "toUpperInvariant" [ arg ] |> Some
    | "GetRuneAt", None, _ -> makeLibCall com r t i "Rune" "getRuneAt" args |> Some
    | "TryGetRuneAt", None, _ -> makeLibCall com r t i "Rune" "tryGetRuneAt" args |> Some
    | "Parse", None, [ arg ] -> makeLibCall com r t i "Rune" "parse" args |> Some
    | "TryParse", None, _ -> makeLibCall com r t i "Rune" "tryParse" args |> Some
    | "ToString", Some c, _ -> makeLibCall com r t i "Rune" "toString" [ c ] |> Some
    | "CompareTo", Some c, _ -> makeLibCall com r t i "Rune" "compareTo" (c :: args) |> Some
    | "Compare", None, _ -> makeLibCall com r t i "Rune" "compareTo" args |> Some
    | "Equals", Some c, _ -> makeLibCall com r t i "Rune" "equals" (c :: args) |> Some
    | "Equals", None, _ -> makeLibCall com r t i "Rune" "equals" args |> Some
    | "GetHashCode", Some c, _ -> makeLibCall com r t i "Rune" "getHashCode" [ c ] |> Some
    | _ -> None

let timeSpans (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    // let callee = match i.callee with Some c -> c | None -> i.args.Head
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match args with
        | [ ExprType(Number(Int64, _)) ] -> "new_ticks" |> Some
        | [ ExprType(Number(Int32, _)); ExprType(Number(Int32, _)); ExprType(Number(Int32, _)) ] -> "new_hms" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> "new_dhms" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> "new_dhms_milli" |> Some
        | [ ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _))
            ExprType(Number(Int32, _)) ] -> "new_dhms_micro" |> Some
        | _ -> None
        |> Option.map (fun meth -> makeStaticMemberCall com r t i "TimeSpan" meth args)
    | ("Compare" | "CompareTo" | "Equals" | "GetHashCode"), thisArg -> valueTypes com ctx r t i thisArg args
    // | "Zero" etc. -> // for static fields, see tryField
    | "Add", Some c ->
        Operation(Binary(BinaryOperator.BinaryPlus, c, args.Head), Tags.empty, t, r)
        |> Some
    | "Add", None -> None
    | "Subtract", Some c ->
        Operation(Binary(BinaryOperator.BinaryMinus, c, args.Head), Tags.empty, t, r)
        |> Some
    | "Subtract", None -> None
    | "Multiply", Some c ->
        Operation(Binary(BinaryOperator.BinaryMultiply, c, args.Head), Tags.empty, t, r)
        |> Some
    | "Multiply", None -> None
    | "Divide", Some c ->
        Operation(Binary(BinaryOperator.BinaryDivide, c, args.Head), Tags.empty, t, r)
        |> Some
    | ("FromDays" | "FromHours" | "FromMinutes" | "FromSeconds" | "FromMilliseconds" | "FromMicroseconds") as meth,
      thisArg ->
        match args with
        | [ ExprType(Number(Float64, _)) ] ->
            // overloads that take a float
            makeMemberCall com ctx r t i "TimeSpan" meth thisArg args |> Some
        | _ ->
            // overloads with variable argument counts
            let argCount = List.length args
            let meth = meth + (string<int> argCount)
            makeMemberCall com ctx r t i "TimeSpan" meth thisArg args |> Some
    | meth, thisArg -> makeMemberCall com ctx r t i "TimeSpan" meth thisArg args |> Some

let timers (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", _, _ -> libCallCons com r t i "Timer" "default" args |> Some
    | Naming.StartsWith "get_" meth, Some x, _ -> getFieldWith r t x meth |> Some
    | Naming.StartsWith "set_" meth, Some x, [ value ] -> setExpr r x (makeStrConst meth) value |> Some
    | meth, Some c, args -> makeInstanceCall r t i c meth args |> Some
    | _ -> None

let systemEnv
    (com: ICompiler)
    (ctx: Context)
    (r: SourceLocation option)
    (t: Type)
    (i: CallInfo)
    (_: Expr option)
    (args: Expr list)
    =
    match i.CompiledName with
    | "get_NewLine" -> Some(makeStrConst "\n")
    | "GetEnvironmentVariable" -> makeLibCall com r t i "Environment" "getEnvironmentVariable" args |> Some
    | "get_CurrentDirectory" -> libCall com r t "Environment" "getCurrentDirectory" [] |> Some
    | _ -> None

let paths (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Combine" ->
        match args with
        | [ _; _ ] -> makeLibCall com r t i "Path" "combine2" args |> Some
        | [ _; _; _ ] -> makeLibCall com r t i "Path" "combine3" args |> Some
        | _ -> None
    | "GetDirectoryName" -> makeLibCall com r t i "Path" "getDirectoryName" args |> Some
    | "GetFileName" -> makeLibCall com r t i "Path" "getFileName" args |> Some
    | "GetFileNameWithoutExtension" -> makeLibCall com r t i "Path" "getFileNameWithoutExtension" args |> Some
    | "GetExtension" -> makeLibCall com r t i "Path" "getExtension" args |> Some
    | "HasExtension" -> makeLibCall com r t i "Path" "hasExtension" args |> Some
    | "GetTempPath" -> libCall com r t "Path" "getTempPath" [] |> Some
    | "GetRandomFileName" -> libCall com r t "Path" "getRandomFileName" [] |> Some
    | _ -> None

let files (com: ICompiler) (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Exists" -> makeLibCall com r t i "File" "exists" args |> Some
    | "WriteAllText" -> makeLibCall com r t i "File" "writeAllText" args |> Some
    | "ReadAllText" -> makeLibCall com r t i "File" "readAllText" args |> Some
    | "Delete" -> makeLibCall com r t i "File" "delete" args |> Some
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

let random (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match args with
        | [] -> libCall com r t "Random" "new" [] |> Some
        | args -> makeLibCall com r t i "Random" "new_seeded" args |> Some
    | meth, Some c ->
        let meth =
            if meth = "Next" then
                $"next{List.length args}"
            else
                meth |> Naming.lowerFirst

        Helper.InstanceCall(c, meth, t, args, i.SignatureArgTypes, ?loc = r) |> Some
    | _ -> None

let cancels (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    // TODO: implement get_None as a non-cancellable token
    | ("get_None" | ".ctor"), _ -> makeLibCall com r t i "Async" "createCancellationToken" args |> Some
    | "get_Token", thisArg -> thisArg
    | ("Cancel" | "CancelAfter" | "get_IsCancellationRequested" | "ThrowIfCancellationRequested"), thisArg ->
        let meth = Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst
        makeLibModuleCall com r t i "Async" meth thisArg args |> Some
    // TODO: Add check so CancellationTokenSource cannot be cancelled after disposed?
    | "Dispose", _ -> Null Type.Unit |> makeValue r |> Some
    | "Register", Some c -> makeInstanceCall r t i c "register" args |> Some
    | _ -> None

let monitor (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Enter" -> libCall com r t "Monitor" "enter" args |> Some
    | "Exit" -> libCall com r t "Monitor" "exit" args |> Some
    | _ -> None

let tasks com (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, i.GenericArgs with
    | ".ctor", None, [ tType ] -> libCall com r tType "Task" "new" args |> Some
    | "FromResult", None, [ tType ] -> libCall com r tType "Task" "from_result" args |> Some
    | "get_Result", Some c, _ -> makeInstanceCall r t i c "get_result" args |> Some
    | _ -> None

let threads com (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, i.GenericArgs, args with
    | ".ctor", None, [], _ -> libCall com r t "Thread" "new" args |> Some
    | "Sleep", None, _, [ ExprType(Number(Int32, _)) ] -> libCall com r t "Thread" "sleep" args |> Some
    | "Start", Some c, [], [] -> makeInstanceCall r t i c "start" args |> Some
    | "Join", Some c, [], [] -> makeInstanceCall r t i c "join" args |> Some
    | _ -> None

let activator (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "CreateInstance", None, [ _type ]
    | "CreateInstance", None, [ _type; (ExprType(Array(Any, _))) ] ->
        libCall com r t "Reflection" "createInstance" args |> Some
    | _ -> None

// alternative member suffix for languages that don't support member overloads
let getArgsSuffix (thisArg: Expr option) (args: Expr list) =
    let rec typeSuffix =
        function
        | Nullable(t, _) -> typeSuffix t // suffix from the actual type
        | Measure _ -> '_'
        | MetaType -> '_'
        | Any -> '_'
        | Unit -> 'u'
        | Boolean -> 'b'
        | Char -> 'c'
        | String -> 's'
        | Regex -> 'r'
        | Number _ -> 'n'
        | Option _ -> 'o'
        | Tuple _ -> 't'
        | Array _ -> 'a'
        | List _ -> 'l'
        | LambdaType _ -> 'f'
        | DelegateType _ -> 'f'
        | GenericParam _ -> 'g'
        | DeclaredType _ -> '_'
        | AnonymousRecordType _ -> '_'

    let chars =
        [|
            if thisArg.IsNone then
                '_' // static methods have extra _
            if args.Length > 0 then
                '_'
            for arg in args do
                typeSuffix arg.Type
        |]

    System.String(chars)

let bclNativeImpl com ctx r t i moduleName memberName (thisArg: Expr option) (args: Expr list) =
    let suffix = getArgsSuffix thisArg args
    let memberName = memberName + suffix

    match thisArg with
    | Some c -> makeInstanceCall r t i c memberName args
    | None -> makeStaticMemberCall com r t i moduleName memberName args

let regex com (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    // | "GetEnumerator" -> getEnumerator com r t i thisArg.Value |> Some
    | meth ->
        let meth =
            if meth = ".ctor" then
                "new"
            else
                meth

        let meth = Naming.removeGetSetPrefix meth |> Naming.lowerFirst
        bclNativeImpl com ctx r t i "RegExp" meth thisArg args |> Some

let encoding (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ("get_Unicode" | "get_UTF8"), _, _ -> makeLibCall com r t i "Encoding" i.CompiledName args |> Some
    | ("GetBytes" | "GetByteCount"), Some c, ExprType(Array(Char, _)) :: _ ->
        let meth = Naming.lowerFirst i.CompiledName + "FromChars"

        let meth =
            if args.Length = 3 then
                meth + "2"
            else
                meth

        makeInstanceCall r t i c meth args |> Some
    | ("GetBytes" | "GetByteCount" | "GetChars" | "GetCharCount" | "GetMaxByteCount" | "GetMaxCharCount" | "GetString"),
      Some c,
      _ ->
        let meth = Naming.lowerFirst i.CompiledName

        let meth =
            if args.Length = 3 then
                meth + "2"
            else
                meth

        makeInstanceCall r t i c meth args |> Some
    | _ -> None

let enumerators (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | meth, Some c ->
        // // Enumerators are mangled, use the fully qualified name
        // let isGenericCurrent = i.CompiledName = "get_Current" && i.DeclaringEntityFullName <> Types.ienumerator
        // let entityName = if isGenericCurrent then Types.ienumeratorGeneric else Types.ienumerator
        // let methName = entityName + "." + i.CompiledName
        makeInstanceCall r t i c meth args |> Some
    | _ -> None

let enumerables (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (_: Expr list) =
    match i.CompiledName, thisArg with
    // This property only belongs to Key and Value Collections
    | "get_Count", Some c -> libCall com r t "Seq" "length" [ c ] |> Some
    | "GetEnumerator", Some c -> getEnumerator com r t i c |> Some
    | _ -> None

let events (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        let constructor =
            if i.DeclaringEntityFullName.EndsWith("`2", StringComparison.Ordinal) then
                "default2"
            else
                "default"

        libCallCons com r t i "Event" constructor args |> Some
    | "get_Publish", Some c -> getFieldWith r t c "Publish" |> Some
    | meth, Some c -> makeInstanceCall r t i c meth args |> Some
    | meth, None -> makeLibCall com r t i "Event" (Naming.lowerFirst meth) args |> Some

let observable (com: ICompiler) (ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    makeLibCall com r t i "Observable" (Naming.lowerFirst i.CompiledName) args
    |> Some

let mailbox (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg with
    | None ->
        match i.CompiledName with
        | ".ctor" -> libCallCons com r t i "MailboxProcessor" "default" args |> Some
        | "Start" -> makeLibCall com r t i "MailboxProcessor" "start" args |> Some
        | _ -> None
    | Some c ->
        match i.CompiledName with
        // `reply` belongs to AsyncReplyChannel
        | "Start" -> libCallThis com r t i "MailboxProcessor" "startInstance" thisArg args |> Some
        | ("Receive" | "PostAndAsyncReply" | "Post") as meth ->
            let meth = Naming.lowerFirst i.CompiledName
            libCallThis com r t i "MailboxProcessor" meth thisArg args |> Some
        | "Reply" -> makeInstanceCall r t i c "reply" args |> Some
        | _ -> None

let tryGetAsyncDelayBinder =
    function
    | MaybeCasted(Call(Import({ Selector = "AsyncBuilder_::delay" }, _, _), callInfo, _, _)) ->
        match callInfo.Args with
        | [ binder ] -> Some binder
        | _ -> None
    | _ -> None

let asyncBuilder (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | "Singleton", _, _ -> Value(UnitConstant, r) |> Some
    | "Bind", _, _ -> makeLibCall com r t i "AsyncBuilder" "bind" args |> Some
    | "Combine", _, _ -> makeLibCall com r t i "AsyncBuilder" "combine" args |> Some
    | "Delay", _, _ -> makeLibCall com r t i "AsyncBuilder" "delay" args |> Some
    | "For", _, _ -> makeLibCall com r t i "AsyncBuilder" "for_loop" args |> Some
    | "Return", _, _ -> makeLibCall com r t i "AsyncBuilder" "r_return" args |> Some
    | "ReturnFrom", _, _ -> makeLibCall com r t i "AsyncBuilder" "return_from" args |> Some
    | "TryFinally", _, _ -> makeLibCall com r t i "AsyncBuilder" "try_finally" args |> Some
    | "TryWith", _, _ -> makeLibCall com r t i "AsyncBuilder" "try_with" args |> Some
    | "Using", _, _ -> makeLibCall com r t i "AsyncBuilder" "using" args |> Some
    | "While", _, [ guard; body ] ->
        match tryGetAsyncDelayBinder body with
        | Some body -> makeLibCall com r t i "AsyncBuilder" "while_loop" [ guard; body ] |> Some
        | None ->
            "Async builder While expects a delayed body."
            |> addErrorAndReturnNull com ctx.InlinePath r
            |> Some
    | "Zero", _, _ -> makeLibCall com r t i "AsyncBuilder" "zero" args |> Some
    | meth, Some c, _ -> makeInstanceCall r t i c meth args |> Some
    | meth, None, _ -> makeLibCall com r t i "AsyncBuilder" (Naming.lowerFirst meth) args |> Some

let asyncs com (ctx: Context) r t (i: CallInfo) (_: Expr option) (args: Expr list) =
    match i.CompiledName with
    // TODO: Throw error for RunSynchronously
    | "Start" ->
        "Async.Start will behave as StartImmediate" |> addWarning com ctx.InlinePath r

        makeLibCall com r t i "Async" "start" args |> Some
    // Make sure cancellationToken is called as a function and not a getter
    | "get_CancellationToken" -> libCall com r t "Async" "cancellationToken" [] |> Some
    // `catch` cannot be used as a function name in JS
    | "Catch" -> makeLibCall com r t i "Async" "catchAsync" args |> Some
    // Fable.Core extensions
    | meth -> makeLibCall com r t i "Async" (Naming.lowerFirst meth) args |> Some

let tryGetTaskDelayBinder =
    function
    | MaybeCasted(Call(Import({ Selector = "TaskBuilder_::delay" }, _, _), callInfo, _, _)) ->
        match callInfo.Args with
        | [ binder ] -> Some binder
        | _ -> None
    | _ -> None

let taskBuilder (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | ".ctor", None, _ -> makeImportLib com t "new" "TaskBuilder" |> Some
    | "task", _, _ -> libCall com r t "TaskBuilder" "new" [] |> Some
    | "Run", Some c, _ -> makeInstanceCall r t i c "run" args |> Some
    | ("Bind" | "TaskBuilderBase.Bind"), _, _ -> makeLibCall com r t i "TaskBuilder" "bind" args |> Some
    | ("Combine" | "TaskBuilderBase.Combine"), _, _ -> makeLibCall com r t i "TaskBuilder" "combine" args |> Some
    | ("Delay" | "TaskBuilderBase.Delay"), _, _ -> makeLibCall com r t i "TaskBuilder" "delay" args |> Some
    | ("For" | "TaskBuilderBase.For"), _, _ -> makeLibCall com r t i "TaskBuilder" "for_loop" args |> Some
    | ("Return" | "TaskBuilderBase.Return"), _, _ -> makeLibCall com r t i "TaskBuilder" "r_return" args |> Some
    | ("ReturnFrom" | "TaskBuilderBase.ReturnFrom"), _, _ ->
        makeLibCall com r t i "TaskBuilder" "return_from" args |> Some
    | ("TryFinally" | "TaskBuilderBase.TryFinally"), _, _ ->
        makeLibCall com r t i "TaskBuilder" "try_finally" args |> Some
    | ("TryWith" | "TaskBuilderBase.TryWith"), _, _ -> makeLibCall com r t i "TaskBuilder" "try_with" args |> Some
    | ("Using" | "TaskBuilderBase.Using"), _, _ -> makeLibCall com r t i "TaskBuilder" "using" args |> Some
    | ("While" | "TaskBuilderBase.While"), _, [ guard; body ] ->
        match tryGetTaskDelayBinder body with
        | Some body -> makeLibCall com r t i "TaskBuilder" "while_loop" [ guard; body ] |> Some
        | None ->
            "Task builder While expects a delayed body."
            |> addErrorAndReturnNull com ctx.InlinePath r
            |> Some
    | ("Zero" | "TaskBuilderBase.Zero"), _, _ -> makeLibCall com r t i "TaskBuilder" "zero" args |> Some
    | meth, Some c, _ -> makeInstanceCall r t i c meth args |> Some
    | meth, None, _ -> makeLibCall com r t i "TaskBuilder" (Naming.lowerFirst meth) args |> Some

let guids
    (com: ICompiler)
    (ctx: Context)
    (r: SourceLocation option)
    t
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, thisArg, args with
    | ".ctor", None, _ ->
        match args with
        | [] -> libCall com r t "Guid" "empty" [] |> Some
        | [ ExprType String ] -> makeMemberCall com ctx r t i "Guid" "parse" None args |> Some
        | [ ExprType(Array(Number(UInt8, _), _)) ] ->
            makeMemberCall com ctx r t i "Guid" "new_from_array" thisArg args |> Some
        // TODO: other constructor overrides
        | _ -> None
    // | "Empty", None, [] -> // it's a static field, see tryField
    | "NewGuid", None, [] -> makeMemberCall com ctx r t i "Guid" "new_guid" thisArg args |> Some
    | "CreateVersion7", None, [] -> makeMemberCall com ctx r t i "Guid" "create_version7" thisArg args |> Some
    | "CreateVersion7", None, _ -> makeMemberCall com ctx r t i "Guid" "create_version7_with" thisArg args |> Some
    | "Parse", None, [ ExprType String ] -> makeMemberCall com ctx r t i "Guid" "parse" thisArg args |> Some
    | "TryParse", None, [ ExprType String; _ ] -> makeMemberCall com ctx r t i "Guid" "tryParse" thisArg args |> Some
    | "ToByteArray", Some x, [] -> makeMemberCall com ctx r t i "Guid" "toByteArray" thisArg [] |> Some
    | "ToString", Some x, [ ExprType String ] -> makeMemberCall com ctx r t i "Guid" "toString" thisArg args |> Some
    | "ToString", Some x, [] -> toString com ctx r [ x ] |> Some
    // TODO: other methods and overrides
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
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        match args with
        | [ ExprType String ] -> makeLibCall com r t i "Uri" "create" args |> Some
        | [ ExprType String; _ ] -> makeLibCall com r t i "Uri" "createWithKind" args |> Some
        | [ _; ExprType String ] -> makeLibCall com r t i "Uri" "createFromString" args |> Some
        | [ _; _ ] -> makeLibCall com r t i "Uri" "createFromUri" args |> Some
        | _ -> None
    | "TryCreate", _ ->
        match args with
        | (ExprType String) :: _ -> makeLibCall com r t i "Uri" "tryCreateWithKind" args |> Some
        | _ :: (ExprType String) :: _ -> makeLibCall com r t i "Uri" "tryCreateFromString" args |> Some
        | _ -> makeLibCall com r t i "Uri" "tryCreateFromUri" args |> Some
    | "ToString", Some c -> toString com ctx r [ c ] |> Some
    | "UnescapeDataString", _ -> makeLibCall com r t i "Uri" "unescapeDataString" args |> Some
    | "EscapeDataString", _ -> makeLibCall com r t i "Uri" "escapeDataString" args |> Some
    | "EscapeUriString", _ -> makeLibCall com r t i "Uri" "escapeUriString" args |> Some
    | ("get_IsAbsoluteUri" | "get_Scheme" | "get_Host" | "get_Port" | "get_IsDefaultPort" | "get_AbsolutePath" | "get_AbsoluteUri" | "get_PathAndQuery" | "get_Query" | "get_Fragment" | "get_OriginalString"),
      thisArg -> makeMemberCall com ctx r t i "Uri" i.CompiledName thisArg args |> Some
    | _ -> None

let laziness (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg, args with
    | _ -> bclType com ctx r t i thisArg args
// | (".ctor" | "Create"), _, _ ->
//     let memberName = "Lazy::" + (getMemberName isStatic i)
//     makeStaticLibCall com r t i "System" memberName args |> Some
// // | "CreateFromValue", _, _ ->
// //     Helper.LibCall(com, "Native", "lazyFromValue", t, args, i.SignatureArgTypes, ?loc = r)
// //     |> Some
// | ("Force" | "get_Value"), Some c, _ -> makeInstanceCall r t i c "force" [] |> Some
// // | "get_IsValueCreated", Some c, _ ->
// //     Naming.removeGetSetPrefix i.CompiledName |> getFieldWith r t c |> Some
// | _ -> None

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
            |> Option.map (fun thisArg -> thisArg :: args, thisArg.Type :: i.SignatureArgTypes)
            |> Option.defaultValue (args, i.SignatureArgTypes)
            |> fun (args, argTypes) -> List.rev args, List.rev argTypes

        libCallTyped com None t "Observable" meth args argTypes
    )

let types (com: ICompiler) (ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let returnString r x = StringConstant x |> makeValue r |> Some

    let resolved =
        // Some optimizations when the type is known at compile time
        match thisArg with
        | Some(Value(TypeInfo(exprType, _), exprRange) as thisArg) ->
            match exprType with
            | GenericParam(name = name) -> genericTypeInfoError name |> addError com ctx.InlinePath exprRange
            | _ -> ()

            match i.CompiledName with
            | "GetInterface" ->
                match exprType, args with
                | DeclaredType(e, genArgs), [ StringConst name ] -> Some(e, genArgs, name, false)
                | DeclaredType(e, genArgs), [ StringConst name; BoolConst ignoreCase ] ->
                    Some(e, genArgs, name, ignoreCase)
                | _ -> None
                |> Option.map (fun (e, genArgs, name, ignoreCase) ->
                    let e = com.GetEntity(e)

                    let genMap =
                        List.zip (e.GenericParameters |> List.map (fun p -> p.Name)) genArgs |> Map

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
                                        Map.tryFind name genMap |> Option.defaultValue gen
                                    | gen -> gen
                                )

                            Some(ifc.Entity, genArgs)
                        else
                            None
                    )
                    |> function
                        | Some(ifcEnt, genArgs) -> DeclaredType(ifcEnt, genArgs) |> makeTypeInfo r
                        | None -> Value(Null t, r)
                )
            | "get_FullName" -> getTypeFullName false exprType |> returnString r
            | "get_Namespace" -> getTypeFullName false exprType |> splitFullName |> fst |> returnString r
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
            | "get_IsGenericType" -> List.isEmpty exprType.Generics |> not |> BoolConstant |> makeValue r |> Some
            | "get_GenericTypeArguments"
            | "GetGenericArguments" ->
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
                    | Tuple(_, isStruct) -> Tuple(newGen, isStruct)
                    | DeclaredType(ent, _) -> DeclaredType(ent, newGen)
                    | t -> t

                makeTypeInfo exprRange exprType |> Some
            | _ -> None
        | _ -> None

    match resolved, thisArg with
    | Some _, _ -> resolved
    | None, Some c ->
        match i.CompiledName with
        | "GetTypeInfo" -> Some c
        | "get_GenericTypeArguments"
        | "GetGenericArguments" -> libCall com r t "Reflection" "getGenerics" [ c ] |> Some
        | "MakeGenericType" -> libCall com r t "Reflection" "makeGenericType" (c :: args) |> Some
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
            let meth = Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst

            libCall com r t "Reflection" meth (c :: args) |> Some
        | _ -> None
    | None, None -> None

let fsharpType com methName (r: SourceLocation option) t (i: CallInfo) (args: Expr list) =
    match methName with
    | "MakeTupleType" ->
        Helper.LibCall(com, "Reflection", "tuple_type", t, args, i.SignatureArgTypes, hasSpread = true, ?loc = r)
        |> Some
    // Prevent name clash with FSharpValue.GetRecordFields
    | "GetRecordFields" -> makeLibCall com r t i "Reflection" "getRecordElements" args |> Some
    | "GetUnionCases"
    | "GetTupleElements"
    | "GetFunctionElements"
    | "IsUnion"
    | "IsRecord"
    | "IsTuple"
    | "IsFunction" -> makeLibCall com r t i "Reflection" (Naming.lowerFirst methName) args |> Some
    | "IsExceptionRepresentation"
    | "GetExceptionFields" -> None // TODO!!!
    | _ -> None

let fsharpValue com methName (r: SourceLocation option) t (i: CallInfo) (args: Expr list) =
    match methName with
    | "GetRecordFields" ->
        let args, argTypes =
            match args with
            | record :: rest ->
                let rec getConcreteType (expr: Expr) =
                    match expr with
                    | TypeCast(inner, _) -> getConcreteType inner
                    | _ -> expr.Type

                let typeInfo = Fable.Value(Fable.TypeInfo(getConcreteType record, []), None)

                let argTypes =
                    match i.SignatureArgTypes with
                    | first :: restTypes -> first :: Fable.MetaType :: restTypes
                    | [] -> []

                record :: typeInfo :: rest, argTypes
            | _ -> args, i.SignatureArgTypes

        libCallTyped com r t "Reflection" "getRecordFields" args argTypes |> Some
    | "GetUnionFields"
    | "GetRecordField"
    | "GetTupleFields"
    | "GetTupleField"
    | "MakeUnion"
    | "MakeRecord"
    | "MakeTuple" -> makeLibCall com r t i "Reflection" (Naming.lowerFirst methName) args |> Some
    | "GetExceptionFields" -> None // TODO!!!
    | _ -> None

let tryField com t ownerTyp fieldName =
    match ownerTyp, fieldName with
    | Number(Decimal, _), _ -> libValue com None t "Decimal" fieldName [] |> Some
    | String, "Empty" -> makeStrConst "" |> Some
    | Builtin BclGuid, "Empty" -> libValue com None t "Guid" "empty" [] |> Some
    | Builtin BclTimeSpan, _ ->
        let meth = fieldName |> Naming.applyCaseRule Fable.Core.CaseRules.SnakeCase

        libValue com None t "TimeSpan" meth [] |> Some
    | Builtin BclRune, "ReplacementChar"
    | Builtin BclRune, "get_ReplacementChar" -> libValue com None t "Rune" "replacementChar" [] |> Some
    | Builtin BclDateTime, _ ->
        let meth = fieldName |> Naming.lowerFirst
        makeStaticFieldCall com None t "DateTime" "DateTime" meth |> Some
    | Builtin BclDateTimeOffset, _ ->
        let meth = fieldName |> Naming.lowerFirst

        makeStaticFieldCall com None t "DateTimeOffset" "DateTimeOffset" meth |> Some
    | DeclaredType(ent, genArgs), fieldName ->
        let meth = fieldName |> Naming.lowerFirst

        match ent.FullName with
        | "System.BitConverter" -> libCall com None t "BitConverter" meth [] |> Some
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
            "Microsoft.FSharp.Core.Operators.OperatorIntrinsics", intrinsicFunctions
            "Microsoft.FSharp.Core.ExtraTopLevelOperators", operators
            "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions", intrinsicFunctions
            "Microsoft.FSharp.Core.LanguagePrimitives", languagePrimitives
            "Microsoft.FSharp.Core.LanguagePrimitives.HashCompare", languagePrimitives
            "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators", operators
            "System.Runtime.CompilerServices.RuntimeHelpers", runtimeHelpers
            "System.Runtime.ExceptionServices.ExceptionDispatchInfo", exceptionDispatchInfo
            Types.attribute, bclType
            Types.char, chars
            Types.string, strings
            "Microsoft.FSharp.Core.StringModule", stringModule
            "System.FormattableString", formattableString
            "System.Runtime.CompilerServices.FormattableStringFactory", formattableString
            "System.Text.StringBuilder", stringBuilder
            Types.array, arrays
            Types.list, lists
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
            Types.iequatableGeneric, valueTypes
            Types.icomparableGeneric, valueTypes
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
            Types.icollectionGeneric, collections
            Types.icollection, collections
            "System.Collections.Generic.CollectionExtensions", collectionExtensions
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
            Types.datetime, dateTimes
            Types.datetimeOffset, dateTimeOffsets
            Types.dateOnly, dateOnly
            Types.timeOnly, timeOnly
            Types.rune, runes
            Types.timespan, timeSpans
            Types.timer, timers
            "System.Environment", systemEnv
            "System.IO.File", files
            "System.IO.Path", paths
            Types.cultureInfo, globalization
            Types.random, random
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
            Types.regex, regex
            Types.regexMatch, regex
            Types.regexGroup, regex
            Types.regexCapture, regex
            Types.regexMatchCollection, regex
            Types.regexGroupCollection, regex
            Types.regexCaptureCollection, regex
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
            "Microsoft.FSharp.Control.TaskBuilderModule", taskBuilder
            "Microsoft.FSharp.Control.TaskBuilder", taskBuilder
            "Microsoft.FSharp.Control.TaskBuilderBase", taskBuilder
            "Microsoft.FSharp.Control.TaskBuilderExtensions.HighPriority", taskBuilder
            "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority", taskBuilder
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
        let isFSharpType =
            info.CompiledName.StartsWith("FSharpType", StringComparison.Ordinal)

        let methName = info.CompiledName |> Naming.extensionMethodName

        if isFSharpType then
            fsharpType com methName r t info args
        else
            fsharpValue com methName r t info args
    | "Microsoft.FSharp.Reflection.UnionCaseInfo" ->
        // UnionCaseInfo from a NewUnionCase quotation deconstruction is a Rust-native
        // FSharpUnionCaseInfo carrier; route member access to the quotation runtime.
        match thisArg, info.CompiledName with
        | Some c, "get_Name" -> libCall com r t "quotation" "unionCaseName" [ c ] |> Some
        | Some c, "get_Tag" -> libCall com r t "quotation" "unionCaseTag" [ c ] |> Some
        | _ -> None
    | "System.Reflection.PropertyInfo"
    | "System.Reflection.ParameterInfo"
    | "System.Reflection.MethodBase"
    | "System.Reflection.MethodInfo"
    | "System.Reflection.MemberInfo" ->
        // Name/DeclaringType are inherited from MemberInfo, so `mi.Name` / `mi.DeclaringType`
        // on a Call quotation binding arrive here even though the binding's type is MethodInfo.
        // Detect that carrier via the thisArg's Fable type and route to the quotation runtime:
        // get_Name -> a dedicated accessor (not the generic reflection name<T>()); DeclaringType
        // -> the declaring-type fullname boxed as System.Type, so mi.DeclaringType.FullName works.
        let isMethodInfoCarrier (c: Expr) =
            match c.Type with
            | DeclaredType(e, _) -> e.FullName = "System.Reflection.MethodInfo"
            | _ -> false

        match thisArg, info.CompiledName with
        | Some c, "get_Name" when isMethodInfoCarrier c -> libCall com r t "quotation" "methodName" [ c ] |> Some
        | Some c, "get_DeclaringType" when isMethodInfoCarrier c ->
            libCall com r t "quotation" "methodDeclaringType" [ c ] |> Some
        | Some c, "get_Tag" -> makeStrConst "tag" |> getExpr r t c |> Some
        | Some c, "get_ReturnType" -> makeStrConst "returnType" |> getExpr r t c |> Some
        | Some c, "GetParameters" -> makeStrConst "parameters" |> getExpr r t c |> Some
        | Some c, ("get_PropertyType" | "get_ParameterType") -> makeIntConst 1 |> getExpr r t c |> Some
        | Some c, "GetFields" -> libCall com r t "Reflection" "getUnionCaseFields" [ c ] |> Some
        | Some c, "GetValue" -> libCall com r t "Reflection" "getValue" (c :: args) |> Some
        | Some c, "get_Name" ->
            match c with
            | Value(TypeInfo(exprType, _), loc) ->
                getTypeName com ctx loc exprType |> StringConstant |> makeValue r |> Some
            // Runtime PropertyInfo (e.g. a record field info): read its carried name.
            // Distinct from the generic type-name helper `name<T>()` to avoid a clash.
            | c -> libCall com r t "Reflection" "propertyName" [ c ] |> Some
        | _ -> None
    // F# Quotations
    | typeName -> Quotations.tryQuotationCall "quotation" com ctx r t info thisArg args typeName

let tryBaseConstructor com ctx (ent: EntityRef) (argTypes: Lazy<Type list>) genArgs args =
    match ent.FullName with
    // | Types.exception_ -> Some(makeImportLib com Any "Exception" "Types", args)
    // | Types.attribute -> Some(makeImportLib com Any "Attribute" "Types", args)
    // | fullName when
    //     fullName.StartsWith("Fable.Core.", StringComparison.Ordinal)
    //     && fullName.EndsWith("Attribute", StringComparison.Ordinal)
    //     ->
    //     Some(makeImportLib com Any "Attribute" "Types", args)
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
        if isStruct then
            Some(Types.valueOption, options true, [ genArg ])
        else
            Some(Types.option, options false, [ genArg ])
    | Array(genArg, _) -> Some(Types.array, arrays, [ genArg ])
    | List genArg -> Some(Types.list, lists, [ genArg ])
    | Builtin kind ->
        match kind with
        | BclGuid -> Some(Types.guid, guids, [])
        | BclDateTime -> Some(Types.datetime, dateTimes, [])
        | BclDateTimeOffset -> Some(Types.datetimeOffset, dateTimeOffsets, [])
        | BclDateOnly -> Some(Types.dateOnly, dateOnly, [])
        | BclTimeOnly -> Some(Types.timeOnly, timeOnly, [])
        | BclRune -> Some(Types.rune, runes, [])
        | BclTimer -> Some(Types.timer, timers, [])
        | BclTimeSpan -> Some(Types.timespan, timeSpans, [])
        | BclHashSet genArg -> Some(Types.hashset, hashSets, [ genArg ])
        | BclDictionary(key, value) -> Some(Types.dictionary, dictionaries, [ key; value ])
        | BclKeyValuePair(key, value) -> Some(Types.keyValuePair, keyValuePairs, [ key; value ])
        | FSharpMap(key, value) -> Some(Types.fsharpMap, maps, [ key; value ])
        | FSharpSet genArg -> Some(Types.fsharpSet, sets, [ genArg ])
        | FSharpResult(genArg1, genArg2) -> Some(Types.result, results, [ genArg1; genArg2 ])
        | FSharpChoice genArgs -> Some($"%s{Types.choiceNonGeneric}`%d{List.length genArgs}", results, genArgs)
        | FSharpReference genArg -> Some(Types.refCell, refCells, [ genArg ])
    | _ -> None
