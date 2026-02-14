[<RequireQualifiedAccess>]
module Fable.Transforms.Beam.Replacements

open Fable
open Fable.AST
open Fable.AST.Fable
open Fable.Transforms
open Replacements.Util

type Context = FSharp2Fable.Context
type ICompiler = FSharp2Fable.IFableCompiler
type CallInfo = ReplaceCallInfo

// Erlang's =:= does deep structural comparison for all types
// (atoms, numbers, binaries, tuples, maps, lists), so we can
// use BinaryEqual/BinaryUnequal directly instead of library calls.
let private equals r equal (left: Expr) (right: Expr) =
    if equal then
        makeBinOp r Boolean left right BinaryEqual
    else
        makeBinOp r Boolean left right BinaryUnequal

let private compare (com: ICompiler) r (left: Expr) (right: Expr) =
    Helper.LibCall(com, "fable_comparison", "compare", Number(Int32, NumberInfo.Empty), [ left; right ], ?loc = r)

let private getOne (com: ICompiler) (ctx: Context) (t: Type) =
    match t with
    | Boolean -> makeBoolConst true
    | Number(kind, uom) -> NumberConstant(NumberValue.GetOne kind, uom) |> makeValue None
    | ListSingleton(CustomOp com ctx None t "get_One" [] e) -> e
    | _ -> makeIntConst 1

let private fsFormat
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match i.CompiledName, _thisArg, args with
    | "get_Value", Some callee, _ -> emitExpr r t [ callee ] "maps:get(input, $0)" |> Some
    | "PrintFormatToString", _, _ ->
        match args with
        | [ template ] when template.Type = String -> Some template
        | _ ->
            Helper.LibCall(com, "fable_string", "to_text", t, args, i.SignatureArgTypes, ?loc = r)
            |> Some
    | "PrintFormatLine", _, _ ->
        Helper.LibCall(com, "fable_string", "to_console", t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | "PrintFormat", _, _ ->
        Helper.LibCall(com, "fable_string", "to_console", t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | ("PrintFormatToError" | "PrintFormatLineToError"), _, _ ->
        Helper.LibCall(com, "fable_string", "to_console_error", t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | "PrintFormatToStringThenFail", _, _ ->
        Helper.LibCall(com, "fable_string", "to_fail", t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | "PrintFormatToStringThen", _, _ ->
        match args with
        | [ _ ] ->
            Helper.LibCall(com, "fable_string", "to_text", t, args, i.SignatureArgTypes, ?loc = r)
            |> Some
        | [ cont; fmt ] -> emitExpr r t [ fmt; cont ] "(maps:get(cont, $0))($1)" |> Some
        | _ -> None
    | "PrintFormatThen", _, arg :: callee :: _ -> emitExpr r t [ callee; arg ] "(maps:get(cont, $0))($1)" |> Some
    | ".ctor", _, str :: (Value(NewArray(ArrayValues templateArgs, _, _), _) as values) :: _ ->
        match makeStringTemplateFrom [| "%s"; "%i" |] templateArgs str with
        | Some v -> makeValue r v |> Some
        | None ->
            Helper.LibCall(com, "fable_string", "interpolate", t, [ str; values ], i.SignatureArgTypes, ?loc = r)
            |> Some
    | ".ctor", _, arg :: _ ->
        Helper.LibCall(com, "fable_string", "printf", t, [ arg ], i.SignatureArgTypes, ?loc = r)
        |> Some
    | _ -> None

let private operators
    (com: ICompiler)
    (ctx: Context)
    r
    (_t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | ("DefaultArg" | "DefaultValueArg"), [ opt; defValue ] ->
        Helper.LibCall(com, "fable_option", "default_value", _t, [ opt; defValue ])
        |> Some
    | "FailWith", [ msg ]
    | "InvalidOp", [ msg ] -> makeThrow r _t msg |> Some
    | "InvalidArg", [ argName; msg ] ->
        let msg = add (add msg (Value(StringConstant "\\nParameter name: ", None))) argName
        makeThrow r _t msg |> Some
    | "Raise", [ arg ] -> makeThrow r _t arg |> Some
    | "NullArg", [ arg ] ->
        let msg =
            add
                (Value(StringConstant "Value cannot be null.", None))
                (add (Value(StringConstant "\\nParameter name: ", None)) arg)

        makeThrow r _t msg |> Some
    | "IsNull", [ arg ] -> emitExpr r _t [ arg ] "($0 =:= undefined)" |> Some
    | "IsNotNull", [ arg ] -> emitExpr r _t [ arg ] "($0 =/= undefined)" |> Some
    // Nullable active patterns and helpers
    | "NullMatchPattern", [ arg ] ->
        // Returns {0, ok} (Some(())) if null, {1} (None) otherwise
        emitExpr r _t [ arg ] "(case $0 of undefined -> {0, ok}; _ -> {1} end)" |> Some
    | ("NonNull" | "NonNullV"), [ arg ] -> Some arg // Identity - just return the value
    | ("NonNullQuickPattern" | "NonNullQuickValuePattern"), [ arg ] ->
        // Returns {0, x} (Some(x)) if non-null, {1} (None) otherwise
        emitExpr r _t [ arg ] "(case $0 of undefined -> {1}; X___ -> {0, X___} end)"
        |> Some
    | "NullValueMatchPattern", [ arg ] ->
        emitExpr r _t [ arg ] "(case $0 of undefined -> {0, ok}; _ -> {1} end)" |> Some
    | ("WithNull" | "WithNullV"), [ arg ] -> Some arg // Identity
    | "NullV", [] -> Value(Null _t, r) |> Some
    | ("IsNullV"), [ arg ] -> emitExpr r _t [ arg ] "($0 =:= undefined)" |> Some
    | "NullArgCheck", [ argName; arg ] ->
        emitExpr
            r
            _t
            [ argName; arg ]
            "(case $1 of undefined -> erlang:error({badarg, <<\"Value cannot be null. Parameter name: \", $0/binary>>}); _ -> $1 end)"
        |> Some
    // Lock — no-op in Erlang (processes are isolated), just call the action
    | "Lock", [ _lockObj; action ] -> CurriedApply(action, [ Value(UnitConstant, None) ], _t, r) |> Some
    // Using — resource disposal
    | "Using", [ resource; action ] ->
        // try action(resource) finally resource.Dispose()
        Helper.LibCall(com, "fable_utils", "using", _t, [ resource; action ], ?loc = r)
        |> Some
    // Failure — create exception from message
    | "Failure", [ msg ] -> emitExpr r _t [ msg ] "#{message => $0}" |> Some
    | "FailurePattern", [ exn ] -> emitExpr r _t [ exn ] "maps:get(message, $0, $0)" |> Some
    // LazyPattern — force a lazy value
    | "LazyPattern", [ arg ] -> Helper.LibCall(com, "fable_utils", "force_lazy", _t, [ arg ], ?loc = r) |> Some
    | "Hash", [ arg ] -> emitExpr r _t [ arg ] "erlang:phash2($0)" |> Some
    | "Compare", [ left; right ] -> compare com r left right |> Some
    // Math operators
    | "Abs", [ arg ] -> emitExpr r _t [ arg ] "erlang:abs($0)" |> Some
    | "Acos", [ arg ] -> emitExpr r _t [ arg ] "math:acos($0)" |> Some
    | "Asin", [ arg ] -> emitExpr r _t [ arg ] "math:asin($0)" |> Some
    | "Atan", [ arg ] -> emitExpr r _t [ arg ] "math:atan($0)" |> Some
    | "Atan2", [ y; x ] -> emitExpr r _t [ y; x ] "math:atan2($0, $1)" |> Some
    | "Ceiling", [ arg ] -> emitExpr r _t [ arg ] "float(erlang:ceil($0))" |> Some
    | "Cos", [ arg ] -> emitExpr r _t [ arg ] "math:cos($0)" |> Some
    | "Exp", [ arg ] -> emitExpr r _t [ arg ] "math:exp($0)" |> Some
    | "Floor", [ arg ] -> emitExpr r _t [ arg ] "float(erlang:floor($0))" |> Some
    | "Log", [ arg ] -> emitExpr r _t [ arg ] "math:log($0)" |> Some
    | "Log10", [ arg ] -> emitExpr r _t [ arg ] "math:log10($0)" |> Some
    | "Log2", [ arg ] -> emitExpr r _t [ arg ] "math:log2($0)" |> Some
    | ("Pow" | "PowInteger" | "op_Exponentiation"), [ base_; exp_ ] ->
        emitExpr r _t [ base_; exp_ ] "math:pow($0, $1)" |> Some
    | "Round", [ arg ] -> emitExpr r _t [ arg ] "float(erlang:round($0))" |> Some
    | "Round", [ arg; digits ] ->
        emitExpr r _t [ arg; digits ] "(erlang:round($0 * math:pow(10, $1)) / math:pow(10, $1))"
        |> Some
    | "Sign", [ arg ] ->
        emitExpr r _t [ arg ] "case $0 > 0 of true -> 1; false -> case $0 < 0 of true -> -1; false -> 0 end end"
        |> Some
    | "Sin", [ arg ] -> emitExpr r _t [ arg ] "math:sin($0)" |> Some
    | "Sqrt", [ arg ] -> emitExpr r _t [ arg ] "math:sqrt($0)" |> Some
    | "Tan", [ arg ] -> emitExpr r _t [ arg ] "math:tan($0)" |> Some
    | "Cosh", [ arg ] -> emitExpr r _t [ arg ] "math:cosh($0)" |> Some
    | "Sinh", [ arg ] -> emitExpr r _t [ arg ] "math:sinh($0)" |> Some
    | "Tanh", [ arg ] -> emitExpr r _t [ arg ] "math:tanh($0)" |> Some
    | "Truncate", [ arg ] -> emitExpr r _t [ arg ] "float(trunc($0))" |> Some
    | ("Max" | "Max_"), [ a; b ] -> emitExpr r _t [ a; b ] "erlang:max($0, $1)" |> Some
    | ("Min" | "Min_"), [ a; b ] -> emitExpr r _t [ a; b ] "erlang:min($0, $1)" |> Some
    | "Clamp", [ value; min_; max_ ] -> emitExpr r _t [ value; min_; max_ ] "erlang:min(erlang:max($0, $1), $2)" |> Some
    | "Log", [ x; base_ ] -> emitExpr r _t [ x; base_ ] "(math:log($0) / math:log($1))" |> Some
    | (Operators.equality | "Eq"), [ left; right ] -> equals r true left right |> Some
    | (Operators.inequality | "Neq"), [ left; right ] -> equals r false left right |> Some
    | (Operators.lessThan | "Lt"), [ left; right ] -> makeBinOp r Boolean left right BinaryLess |> Some
    | (Operators.lessThanOrEqual | "Lte"), [ left; right ] -> makeBinOp r Boolean left right BinaryLessOrEqual |> Some
    | (Operators.greaterThan | "Gt"), [ left; right ] -> makeBinOp r Boolean left right BinaryGreater |> Some
    | (Operators.greaterThanOrEqual | "Gte"), [ left; right ] ->
        makeBinOp r Boolean left right BinaryGreaterOrEqual |> Some
    | Operators.unaryNegation, [ operand ] -> Operation(Unary(UnaryMinus, operand), Tags.empty, _t, r) |> Some
    // Type conversions: int(x), float(x), string(x), etc.
    | ("ToSByte" | "ToByte" | "ToInt8" | "ToUInt8" | "ToInt16" | "ToUInt16" | "ToInt" | "ToUInt" | "ToInt32" | "ToUInt32" | "ToInt64" | "ToUInt64" | "ToIntPtr" | "ToUIntPtr"),
      [ arg ] ->
        match arg.Type with
        | Type.String -> Helper.LibCall(com, "fable_convert", "to_int", _t, [ arg ], ?loc = r) |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Decimal -> Helper.LibCall(com, "fable_decimal", "to_int", _t, [ arg ], ?loc = r) |> Some
            | Float16
            | Float32
            | Float64 -> emitExpr r _t [ arg ] "trunc($0)" |> Some
            | _ -> Some arg
        | Type.Char -> Some arg
        | _ -> Some arg
    | ("ToSingle" | "ToDouble"), [ arg ] ->
        match arg.Type with
        | Type.String -> Helper.LibCall(com, "fable_convert", "to_float", _t, [ arg ]) |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Decimal -> Helper.LibCall(com, "fable_decimal", "to_number", _t, [ arg ], ?loc = r) |> Some
            | Float16
            | Float32
            | Float64 -> Some arg
            | _ -> emitExpr r _t [ arg ] "float($0)" |> Some
        | _ -> emitExpr r _t [ arg ] "float($0)" |> Some
    | "ToDecimal", [ arg ] ->
        match arg.Type with
        | Type.String -> Helper.LibCall(com, "fable_decimal", "parse", _t, [ arg ], ?loc = r) |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Decimal -> Some arg
            | Float16
            | Float32
            | Float64 ->
                // float → fixed-scale: trunc(x * 10^28)
                emitExpr r _t [ arg ] "trunc($0 * 10000000000000000000000000000)" |> Some
            | _ ->
                // int → fixed-scale: x * 10^28
                emitExpr r _t [ arg ] "($0 * 10000000000000000000000000000)" |> Some
        | _ -> emitExpr r _t [ arg ] "($0 * 10000000000000000000000000000)" |> Some
    | "ToString", [ arg ] ->
        match arg.Type with
        | Type.String -> Some arg
        | Type.Char -> emitExpr r _t [ arg ] "<<$0/utf8>>" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Decimal -> Helper.LibCall(com, "fable_decimal", "to_string", _t, [ arg ], ?loc = r) |> Some
            | Float16
            | Float32
            | Float64 -> emitExpr r _t [ arg ] "float_to_binary($0)" |> Some
            | _ -> emitExpr r _t [ arg ] "integer_to_binary($0)" |> Some
        | Type.Boolean -> emitExpr r _t [ arg ] "atom_to_binary($0)" |> Some
        | _ -> Helper.LibCall(com, "fable_convert", "to_string", _t, [ arg ], ?loc = r) |> Some
    | "ToChar", [ arg ] ->
        match arg.Type with
        | Type.String -> emitExpr r _t [ arg ] "binary:first($0)" |> Some
        | _ -> Some arg
    // CreateSet: the `set [1;2;3]` syntax
    | "CreateSet", [ arg ] -> emitExpr r _t [ arg ] "ordsets:from_list($0)" |> Some
    // CreateDictionary: the `dict [...]` syntax
    | ("CreateDictionary" | "CreateReadOnlyDictionary"), [ arg ] ->
        Helper.LibCall(com, "fable_dictionary", "create_from_list", _t, [ arg ], ?loc = r)
        |> Some
    // Range operators: [start..stop] and [start..step..stop]
    | ("op_Range" | "op_RangeStep"), _ ->
        let genArg = genArg com ctx r 0 info.GenericArgs

        let addStep args =
            match args with
            | [ first; last ] -> [ first; getOne com ctx genArg; last ]
            | _ -> args

        let modul, meth, args =
            match genArg with
            | Char -> "range", "range_char", args
            | Number(Decimal, _) -> "range", "range_decimal", addStep args
            | Number(Int32, _) -> "range", "range_int32", addStep args
            | Number(UInt32, _) -> "range", "range_u_int32", addStep args
            | Number(Int64, _) -> "range", "range_int64", addStep args
            | Number(UInt64, _) -> "range", "range_u_int64", addStep args
            | _ -> "range", "range_double", addStep args

        Helper.LibCall(com, modul, meth, _t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    // Erlang has native arbitrary-precision integers, so Int64/UInt64/BigInt
    // use direct binary ops instead of library calls (like Python's int)
    // Bitwise operators — Erlang has native bitwise support for all integer sizes
    | Operators.bitwiseAnd, [ left; right ] -> emitExpr r _t [ left; right ] "($0 band $1)" |> Some
    | Operators.bitwiseOr, [ left; right ] -> emitExpr r _t [ left; right ] "($0 bor $1)" |> Some
    | Operators.exclusiveOr, [ left; right ] -> emitExpr r _t [ left; right ] "($0 bxor $1)" |> Some
    | Operators.leftShift, [ left; right ] -> emitExpr r _t [ left; right ] "($0 bsl $1)" |> Some
    | Operators.rightShift, [ left; right ] -> emitExpr r _t [ left; right ] "($0 bsr $1)" |> Some
    | Operators.logicalNot, [ operand ] -> emitExpr r _t [ operand ] "(bnot $0)" |> Some
    // Erlang has native arbitrary-precision integers, so Int64/UInt64/BigInt
    // use direct binary ops instead of library calls
    // Ref cells: ref, !, :=, incr, decr
    | "op_Dereference", [ arg ] -> emitExpr r _t [ arg ] "get($0)" |> Some
    | "op_ColonEquals", [ o; v ] -> emitExpr r Unit [ o; v ] "put($0, $1)" |> Some
    | "Ref", [ arg ] -> Helper.LibCall(com, "fable_utils", "new_ref", Any, [ arg ], ?loc = r) |> Some
    | ("Increment" | "Decrement"), [ arg ] ->
        // incr x → put(x, get(x) + 1); decr x → put(x, get(x) - 1)
        let delta =
            if info.CompiledName = "Increment" then
                "1"
            else
                "-1"

        emitExpr r _t [ arg ] $"put($0, get($0) + %s{delta})" |> Some
    // Erased operators — Beam doesn't need special treatment
    | ("KeyValuePattern" | "Identity" | "Box" | "Unbox" | "ToEnum"), [ arg ] -> TypeCast(arg, _t) |> Some
    | "Ignore", _ -> Sequential [ args.Head; Value(UnitConstant, None) ] |> Some
    | "CreateSequence", [ xs ] -> TypeCast(xs, _t) |> Some
    // Pipes and composition
    | "op_PipeRight", [ x; f ]
    | "op_PipeLeft", [ f; x ] -> CurriedApply(f, [ x ], _t, r) |> Some
    | "op_PipeRight2", [ x; y; f ]
    | "op_PipeLeft2", [ f; x; y ] -> CurriedApply(f, [ x; y ], _t, r) |> Some
    | "op_PipeRight3", [ x; y; z; f ]
    | "op_PipeLeft3", [ f; x; y; z ] -> CurriedApply(f, [ x; y; z ], _t, r) |> Some
    | "op_ComposeRight", [ f1; f2 ] ->
        // fun x -> f2(f1(x))
        let ident = makeTypedIdent _t "x"
        let identExpr = IdentExpr ident
        let innerCall = CurriedApply(f1, [ identExpr ], _t, None)
        let outerCall = CurriedApply(f2, [ innerCall ], _t, None)
        Lambda(ident, outerCall, None) |> Some
    | "op_ComposeLeft", [ f2; f1 ] ->
        let ident = makeTypedIdent _t "x"
        let identExpr = IdentExpr ident
        let innerCall = CurriedApply(f1, [ identExpr ], _t, None)
        let outerCall = CurriedApply(f2, [ innerCall ], _t, None)
        Lambda(ident, outerCall, None) |> Some
    // Not (boolean negation)
    | "Not", [ operand ] -> makeUnOp r _t operand UnaryNot |> Some
    // Tuples
    | "Fst", [ tup ] -> Get(tup, TupleIndex 0, _t, r) |> Some
    | "Snd", [ tup ] -> Get(tup, TupleIndex 1, _t, r) |> Some
    // List append
    | "op_Append", [ left; right ] -> emitExpr r _t [ left; right ] "($0 ++ $1)" |> Some
    // TypeOf: typeof<T> → TypeInfo
    | "TypeOf", _ -> (genArg com ctx r 0 info.GenericArgs) |> makeTypeInfo r |> Some
    // Reraise
    | "Reraise", _ ->
        match ctx.CaughtException with
        | Some ex -> makeThrow r _t (IdentExpr ex) |> Some
        | None -> makeThrow r _t (Value(StringConstant "reraise", None)) |> Some
    // Pow (for Double, via math:pow)
    | "Pow", [ base_; exp_ ] -> emitExpr r _t [ base_; exp_ ] "math:pow($0, $1)" |> Some
    | Patterns.SetContains Operators.standardSet, _ ->
        let argTypes = args |> List.map (fun a -> a.Type)

        match argTypes with
        | Builtin(FSharpSet _) :: _ ->
            match info.CompiledName, args with
            | Operators.addition, [ left; right ] -> emitExpr r _t [ left; right ] "ordsets:union($0, $1)" |> Some
            | Operators.subtraction, [ left; right ] -> emitExpr r _t [ left; right ] "ordsets:subtract($0, $1)" |> Some
            | _ -> None
        | Number((Int64 | UInt64 | Int128 | UInt128 | NativeInt | UNativeInt | BigInt), _) :: _ ->
            match info.CompiledName, args with
            | Operators.addition, [ left; right ] -> makeBinOp r _t left right BinaryPlus |> Some
            | Operators.subtraction, [ left; right ] -> makeBinOp r _t left right BinaryMinus |> Some
            | Operators.multiply, [ left; right ] -> makeBinOp r _t left right BinaryMultiply |> Some
            | (Operators.division | Operators.divideByInt), [ left; right ] ->
                makeBinOp r _t left right BinaryDivide |> Some
            | Operators.modulus, [ left; right ] -> makeBinOp r _t left right BinaryModulus |> Some
            | _ -> None
        // String concatenation
        | String :: _ ->
            match info.CompiledName, args with
            | Operators.addition, [ left; right ] -> add left right |> Some
            | _ -> None
        // Decimal: fixed-scale integer — +, -, rem work natively; *, / need library calls
        | Number(Decimal, _) :: _ ->
            match info.CompiledName, args with
            | Operators.addition, [ left; right ] -> makeBinOp r _t left right BinaryPlus |> Some
            | Operators.subtraction, [ left; right ] -> makeBinOp r _t left right BinaryMinus |> Some
            | Operators.multiply, [ left; right ] ->
                Helper.LibCall(com, "fable_decimal", "multiply", _t, [ left; right ], ?loc = r)
                |> Some
            | Operators.division, [ left; right ] ->
                Helper.LibCall(com, "fable_decimal", "divide", _t, [ left; right ], ?loc = r)
                |> Some
            | Operators.divideByInt, [ left; right ] ->
                Helper.LibCall(com, "fable_decimal", "divide_by_int", _t, [ left; right ], ?loc = r)
                |> Some
            | Operators.modulus, [ left; right ] -> makeBinOp r _t left right BinaryModulus |> Some
            | _ -> None
        // DateTime arithmetic: + and - need runtime library calls
        | DeclaredType(ent, _) :: _ when ent.FullName = Types.datetime ->
            match info.CompiledName, args with
            | Operators.addition, [ left; right ] ->
                Helper.LibCall(com, "fable_date", "op_addition", _t, [ left; right ], ?loc = r)
                |> Some
            | Operators.subtraction, [ left; right ] ->
                Helper.LibCall(com, "fable_date", "op_subtraction", _t, [ left; right ], ?loc = r)
                |> Some
            | _ -> None
        // Default: check for custom operator on DeclaredType, then fall back to native ops
        | _ ->
            let opName = info.CompiledName

            match (|CustomOp|_|) com ctx r _t opName args argTypes with
            | Some _ as e -> e
            | None ->
                match opName, args with
                | Operators.addition, [ left; right ] -> makeBinOp r _t left right BinaryPlus |> Some
                | Operators.subtraction, [ left; right ] -> makeBinOp r _t left right BinaryMinus |> Some
                | Operators.multiply, [ left; right ] -> makeBinOp r _t left right BinaryMultiply |> Some
                | (Operators.division | Operators.divideByInt), [ left; right ] ->
                    makeBinOp r _t left right BinaryDivide |> Some
                | Operators.modulus, [ left; right ] -> makeBinOp r _t left right BinaryModulus |> Some
                | Operators.unaryPlus, [ arg ] -> arg |> Some
                | _ -> None
    | "DefaultAsyncBuilder", _ ->
        Helper.LibCall(com, "fable_async_builder", "singleton", _t, [], ?loc = r)
        |> Some
    | ("PrintFormatToString" | "PrintFormatToStringThen" | "PrintFormat" | "PrintFormatLine" | "PrintFormatToError" | "PrintFormatLineToError" | "PrintFormatThen" | "PrintFormatToStringThenFail"),
      _ -> fsFormat com ctx r _t info _thisArg args
    | _ -> None

let private languagePrimitives
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | ("GenericEquality" | "GenericEqualityIntrinsic"), [ left; right ] -> equals r true left right |> Some
    | ("GenericEqualityER" | "GenericEqualityERIntrinsic"), [ left; right ] -> equals r true left right |> Some
    | ("GenericComparison" | "GenericComparisonIntrinsic"), [ left; right ] -> compare com r left right |> Some
    | ("GenericLessThan" | "GenericLessThanIntrinsic"), [ left; right ] ->
        makeBinOp r Boolean left right BinaryLess |> Some
    | ("GenericLessOrEqual" | "GenericLessOrEqualIntrinsic"), [ left; right ] ->
        makeBinOp r Boolean left right BinaryLessOrEqual |> Some
    | ("GenericGreaterThan" | "GenericGreaterThanIntrinsic"), [ left; right ] ->
        makeBinOp r Boolean left right BinaryGreater |> Some
    | ("GenericGreaterOrEqual" | "GenericGreaterOrEqualIntrinsic"), [ left; right ] ->
        makeBinOp r Boolean left right BinaryGreaterOrEqual |> Some
    | ("PhysicalEquality" | "PhysicalEqualityIntrinsic"), [ left; right ] ->
        makeBinOp r Boolean left right BinaryEqual |> Some
    | ("GenericHash" | "GenericHashIntrinsic"), [ arg ] -> emitExpr r t [ arg ] "erlang:phash2($0)" |> Some
    | ("PhysicalHash" | "PhysicalHashIntrinsic"), [ arg ] -> emitExpr r t [ arg ] "erlang:phash2($0)" |> Some
    // GenericZero/GenericOne
    | "GenericZero", _ ->
        match t with
        | Number(Float64, _)
        | Number(Float32, _)
        | Number(Float16, _)
        | Number(Decimal, _) -> makeFloatConst 0.0 |> Some
        | _ -> makeIntConst 0 |> Some
    | "GenericOne", _ ->
        match t with
        | Number(Float64, _)
        | Number(Float32, _)
        | Number(Float16, _)
        | Number(Decimal, _) -> makeFloatConst 1.0 |> Some
        | _ -> makeIntConst 1 |> Some
    // Enum conversions (erased)
    | "EnumOfValue", [ arg ]
    | "EnumToValue", [ arg ] -> TypeCast(arg, t) |> Some
    // Measure annotations (erased)
    | ("SByteWithMeasure" | "Int16WithMeasure" | "Int32WithMeasure" | "Int64WithMeasure" | "Float32WithMeasure" | "FloatWithMeasure" | "DecimalWithMeasure"),
      [ arg ] -> arg |> Some
    // Dynamic operations — used by SRTP (statically resolved type parameters)
    | Naming.EndsWith "Dynamic" operation, arg :: _ ->
        let operation =
            if operation = Operators.divideByInt then
                operation
            else
                "op_" + operation

        if operation = "op_Explicit" then
            Some arg
        else
            let argTypes = args |> List.map (fun a -> a.Type)
            // Check for custom operator on DeclaredType before falling back to native ops
            match (|CustomOp|_|) com ctx r t operation args argTypes with
            | Some _ as e -> e
            | None ->
                // For Dynamic ops, map to standard binary operations
                match operation, args with
                | "op_Addition", [ left; right ] -> makeBinOp r t left right BinaryPlus |> Some
                | "op_Subtraction", [ left; right ] -> makeBinOp r t left right BinaryMinus |> Some
                | "op_Multiply", [ left; right ] -> makeBinOp r t left right BinaryMultiply |> Some
                | "op_Division", [ left; right ] -> makeBinOp r t left right BinaryDivide |> Some
                | "op_Modulus", [ left; right ] -> makeBinOp r t left right BinaryModulus |> Some
                | ("DivideByInt" | Operators.divideByInt), [ left; right ] ->
                    makeBinOp r t left right BinaryDivide |> Some
                | "op_UnaryNegation", [ operand ] -> Operation(Unary(UnaryMinus, operand), Tags.empty, t, r) |> Some
                | "op_BitwiseAnd", [ left; right ] -> emitExpr r t [ left; right ] "($0 band $1)" |> Some
                | "op_BitwiseOr", [ left; right ] -> emitExpr r t [ left; right ] "($0 bor $1)" |> Some
                | "op_ExclusiveOr", [ left; right ] -> emitExpr r t [ left; right ] "($0 bxor $1)" |> Some
                | "op_LeftShift", [ left; right ] -> emitExpr r t [ left; right ] "($0 bsl $1)" |> Some
                | "op_RightShift", [ left; right ] -> emitExpr r t [ left; right ] "($0 bsr $1)" |> Some
                | "op_Explicit", [ arg ] -> Some arg
                | _ -> None
    | "DivideByInt", [ left; right ] -> makeBinOp r t left right BinaryDivide |> Some
    // IntrinsicFunctions within LanguagePrimitives
    | "UnboxFast", [ arg ] -> TypeCast(arg, t) |> Some
    | ("ParseInt32" | "ParseUInt32"), [ arg ] ->
        Helper.LibCall(com, "fable_convert", "to_int", t, [ arg ], ?loc = r) |> Some
    | ("ParseInt64" | "ParseUInt64"), [ arg ] ->
        Helper.LibCall(com, "fable_convert", "to_int", t, [ arg ], ?loc = r) |> Some
    | _ -> None

let private unchecked
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "DefaultOf", _ ->
        let typ = genArg com _ctx r 0 info.GenericArgs

        match typ with
        | Boolean -> makeBoolConst false |> Some
        | Number(kind, uom) -> NumberConstant(NumberValue.GetZero kind, uom) |> makeValue None |> Some
        | Char -> CharConstant '\u0000' |> makeValue None |> Some
        | String -> makeStrConst "" |> Some
        | _ -> Value(Null typ, r) |> Some
    | "Hash", [ arg ] -> emitExpr r t [ arg ] "erlang:phash2($0)" |> Some
    | "Equals", [ left; right ] -> equals r true left right |> Some
    | "Compare", [ left; right ] -> compare com r left right |> Some
    | "NonNull", [ arg ] -> Some arg // Identity - unchecked non-null assertion
    | _ -> None

/// Beam-specific System.Object replacements.
let private objects
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    | ".ctor", _, _ -> emitExpr r t [] "ok" |> Some
    | "ReferenceEquals", None, [ left; right ] -> makeBinOp r Boolean left right BinaryEqual |> Some
    | "Equals", Some thisObj, [ arg ] -> equals r true thisObj arg |> Some
    | "Equals", None, [ left; right ] -> equals r true left right |> Some
    | "GetHashCode", Some thisObj, [] -> emitExpr r t [ thisObj ] "erlang:phash2($0)" |> Some
    | "GetType", Some arg, _ -> makeTypeInfo r arg.Type |> Some
    | "ToString", Some thisObj, [] ->
        match thisObj.Type with
        | Type.Char -> emitExpr r t [ thisObj ] "<<$0/utf8>>" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Decimal ->
                Helper.LibCall(_com, "fable_decimal", "to_string", t, [ thisObj ], ?loc = r)
                |> Some
            | Float16
            | Float32
            | Float64 -> emitExpr r t [ thisObj ] "float_to_binary($0)" |> Some
            | _ -> emitExpr r t [ thisObj ] "integer_to_binary($0)" |> Some
        | Type.Boolean -> emitExpr r t [ thisObj ] "atom_to_binary($0)" |> Some
        | Type.String -> Some thisObj
        | DeclaredType(ent, _) when ent.FullName = Types.timespan ->
            Helper.LibCall(_com, "fable_timespan", "to_string", t, [ thisObj ], ?loc = r)
            |> Some
        | DeclaredType(ent, _) when ent.FullName = Types.datetime ->
            Helper.LibCall(_com, "fable_date", "to_string", t, [ thisObj ], ?loc = r)
            |> Some
        | DeclaredType(ent, _) when ent.FullName = "System.Uri" ->
            Helper.LibCall(_com, "fable_uri", "to_string", t, [ thisObj ], ?loc = r) |> Some
        | DeclaredType(ent, _) when ent.FullName = "System.Text.StringBuilder" ->
            // StringBuilder.ToString() → iolist_to_binary(get(maps:get(buf, get(Sb))))
            emitExpr r t [ thisObj ] "iolist_to_binary(get(maps:get(buf, get($0))))" |> Some
        | _ ->
            Helper.LibCall(_com, "fable_convert", "to_string", t, [ thisObj ], ?loc = r)
            |> Some
    | _ -> None

/// Beam-specific System.ValueType replacements.
let private valueTypes
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (_args: Expr list)
    =
    match info.CompiledName, thisArg with
    | "Equals", Some thisObj ->
        match _args with
        | [ arg ] -> equals r true thisObj arg |> Some
        | _ -> None
    | "GetHashCode", Some thisObj -> emitExpr r t [ thisObj ] "erlang:phash2($0)" |> Some
    | "CompareTo", Some thisObj ->
        match _args with
        | [ arg ] -> compare com r thisObj arg |> Some
        | _ -> None
    | _ -> None

/// Beam-specific System.Char replacements.
/// Chars in Erlang are integers (Unicode codepoints).
let private chars
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "ToUpper", [ c ] -> Helper.LibCall(com, "fable_char", "to_upper", t, [ c ]) |> Some
    | "ToUpperInvariant", [ c ] -> Helper.LibCall(com, "fable_char", "to_upper", t, [ c ]) |> Some
    | "ToLower", [ c ] -> Helper.LibCall(com, "fable_char", "to_lower", t, [ c ]) |> Some
    | "ToLowerInvariant", [ c ] -> Helper.LibCall(com, "fable_char", "to_lower", t, [ c ]) |> Some
    | "ToString", [ c ] -> Helper.LibCall(com, "fable_char", "to_string", t, [ c ]) |> Some
    | "IsLetter", [ c ] -> Helper.LibCall(com, "fable_char", "is_letter", t, [ c ]) |> Some
    | "IsLetter", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_letter", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsDigit", [ c ] -> Helper.LibCall(com, "fable_char", "is_digit", t, [ c ]) |> Some
    | "IsDigit", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_digit", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsLetterOrDigit", [ c ] -> Helper.LibCall(com, "fable_char", "is_letter_or_digit", t, [ c ]) |> Some
    | "IsLetterOrDigit", [ str; idx ] ->
        Helper.LibCall(
            com,
            "fable_char",
            "is_letter_or_digit",
            t,
            [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ]
        )
        |> Some
    | "IsUpper", [ c ] -> Helper.LibCall(com, "fable_char", "is_upper", t, [ c ]) |> Some
    | "IsUpper", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_upper", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsLower", [ c ] -> Helper.LibCall(com, "fable_char", "is_lower", t, [ c ]) |> Some
    | "IsLower", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_lower", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsNumber", [ c ] -> Helper.LibCall(com, "fable_char", "is_number", t, [ c ]) |> Some
    | "IsNumber", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_number", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsWhiteSpace", [ c ] -> Helper.LibCall(com, "fable_char", "is_whitespace", t, [ c ]) |> Some
    | "IsWhiteSpace", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_whitespace", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsControl", [ c ] -> Helper.LibCall(com, "fable_char", "is_control", t, [ c ]) |> Some
    | "IsControl", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_control", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsPunctuation", [ c ] -> Helper.LibCall(com, "fable_char", "is_punctuation", t, [ c ]) |> Some
    | "IsPunctuation", [ str; idx ] ->
        Helper.LibCall(
            com,
            "fable_char",
            "is_punctuation",
            t,
            [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ]
        )
        |> Some
    | "IsSeparator", [ c ] -> Helper.LibCall(com, "fable_char", "is_separator", t, [ c ]) |> Some
    | "IsSeparator", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_separator", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsSymbol", [ c ] -> Helper.LibCall(com, "fable_char", "is_symbol", t, [ c ]) |> Some
    | "IsSymbol", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_symbol", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "GetUnicodeCategory", [ c ] -> Helper.LibCall(com, "fable_char", "get_unicode_category", t, [ c ]) |> Some
    | "GetUnicodeCategory", [ str; idx ] ->
        Helper.LibCall(
            com,
            "fable_char",
            "get_unicode_category",
            t,
            [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ]
        )
        |> Some
    | "Parse", [ str ] -> Helper.LibCall(com, "fable_char", "parse", t, [ str ]) |> Some
    | "TryParse", _ -> Helper.LibCall(com, "fable_char", "try_parse", t, args, ?loc = r) |> Some
    | "IsHighSurrogate", [ c ] -> Helper.LibCall(com, "fable_char", "is_high_surrogate", t, [ c ]) |> Some
    | "IsHighSurrogate", [ str; idx ] ->
        Helper.LibCall(
            com,
            "fable_char",
            "is_high_surrogate",
            t,
            [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ]
        )
        |> Some
    | "IsLowSurrogate", [ c ] -> Helper.LibCall(com, "fable_char", "is_low_surrogate", t, [ c ]) |> Some
    | "IsLowSurrogate", [ str; idx ] ->
        Helper.LibCall(
            com,
            "fable_char",
            "is_low_surrogate",
            t,
            [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ]
        )
        |> Some
    | "IsSurrogate", [ c ] -> Helper.LibCall(com, "fable_char", "is_surrogate", t, [ c ]) |> Some
    | "IsSurrogate", [ str; idx ] ->
        Helper.LibCall(com, "fable_char", "is_surrogate", t, [ emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)" ])
        |> Some
    | "IsSurrogatePair", [ hi; lo ] -> Helper.LibCall(com, "fable_char", "is_surrogate_pair", t, [ hi; lo ]) |> Some
    | "IsSurrogatePair", [ str; idx ] ->
        Helper.LibCall(
            com,
            "fable_char",
            "is_surrogate_pair",
            t,
            [
                emitExpr r Type.Char [ str; idx ] "binary:at($0, $1)"
                emitExpr r Type.Char [ str; idx ] "binary:at($0, $1 + 1)"
            ]
        )
        |> Some
    | _ -> None

/// Beam-specific string method replacements.
/// Erlang strings are UTF-8 binaries (<<>>), so we use binary module functions.
let private strings
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    // String constructors: String(char[]), String(char, count), String(char[], start, length)
    | ".ctor", _, fstArg :: _ ->
        match fstArg.Type with
        | Type.Char ->
            match args with
            | [ ch; count ] ->
                Helper.LibCall(com, "fable_string", "string_ctor_char_count", t, [ ch; count ])
                |> Some
            | _ -> None
        | Type.Array _ ->
            match args with
            | [ chars ] -> Helper.LibCall(com, "fable_string", "string_ctor_chars", t, [ chars ]) |> Some
            | [ chars; start; len ] ->
                Helper.LibCall(com, "fable_string", "string_ctor_chars_range", t, [ chars; start; len ])
                |> Some
            | _ -> None
        | _ -> None
    // str.Length → byte_size(Str) — works for ASCII; for full Unicode use string:length
    | "get_Length", Some c, _ -> emitExpr r t [ c ] "byte_size($0)" |> Some
    // str.ToUpper() → string:uppercase(Str)
    | ("ToUpper" | "ToUpperInvariant"), Some c, _ -> emitExpr r t [ c ] "string:uppercase($0)" |> Some
    // str.ToLower() → string:lowercase(Str)
    | ("ToLower" | "ToLowerInvariant"), Some c, _ -> emitExpr r t [ c ] "string:lowercase($0)" |> Some
    // str.Trim() → string:trim(Str)
    | "Trim", Some c, [] -> emitExpr r t [ c ] "string:trim($0)" |> Some
    | "Trim", Some c, [ chars ] -> Helper.LibCall(com, "fable_string", "trim_chars", t, [ c; chars ]) |> Some
    | "TrimStart", Some c, [] -> emitExpr r t [ c ] "string:trim($0, leading)" |> Some
    | "TrimStart", Some c, [ chars ] -> Helper.LibCall(com, "fable_string", "trim_start_chars", t, [ c; chars ]) |> Some
    | "TrimEnd", Some c, [] -> emitExpr r t [ c ] "string:trim($0, trailing)" |> Some
    | "TrimEnd", Some c, [ chars ] -> Helper.LibCall(com, "fable_string", "trim_end_chars", t, [ c; chars ]) |> Some
    // str.StartsWith(prefix)
    | "StartsWith", Some c, [ prefix ] ->
        match prefix.Type with
        | Type.Char -> emitExpr r t [ c; prefix ] "fable_string:starts_with($0, <<$1/utf8>>)" |> Some
        | _ -> Helper.LibCall(com, "fable_string", "starts_with", t, [ c; prefix ]) |> Some
    | "StartsWith", Some c, [ prefix; _compType ] ->
        emitExpr r t [ c; prefix ] "fable_string:starts_with(string:lowercase($0), string:lowercase($1))"
        |> Some
    | "StartsWith", Some c, [ prefix; _ignoreCase; _culture ] ->
        emitExpr r t [ c; prefix ] "fable_string:starts_with(string:lowercase($0), string:lowercase($1))"
        |> Some
    // str.EndsWith(suffix)
    | "EndsWith", Some c, [ suffix ] ->
        match suffix.Type with
        | Type.Char -> emitExpr r t [ c; suffix ] "fable_string:ends_with($0, <<$1/utf8>>)" |> Some
        | _ -> Helper.LibCall(com, "fable_string", "ends_with", t, [ c; suffix ]) |> Some
    | "EndsWith", Some c, [ suffix; _compType ] ->
        emitExpr r t [ c; suffix ] "fable_string:ends_with(string:lowercase($0), string:lowercase($1))"
        |> Some
    | "EndsWith", Some c, [ suffix; _ignoreCase; _culture ] ->
        emitExpr r t [ c; suffix ] "fable_string:ends_with(string:lowercase($0), string:lowercase($1))"
        |> Some
    // str.Substring(start) → binary:part(Str, Start, byte_size(Str) - Start)
    | "Substring", Some c, [ start ] -> emitExpr r t [ c; start ] "binary:part($0, $1, byte_size($0) - $1)" |> Some
    // str.Substring(start, length) → binary:part(Str, Start, Length)
    | "Substring", Some c, [ start; len ] -> emitExpr r t [ c; start; len ] "binary:part($0, $1, $2)" |> Some
    // str.Replace(old, new)
    | "Replace", Some c, [ oldVal; newVal ] ->
        Helper.LibCall(com, "fable_string", "replace", t, [ c; oldVal; newVal ]) |> Some
    // str.Split(sep) → binary:split(Str, Sep, [global])
    | "Split", Some c, [ sep ] -> Helper.LibCall(com, "fable_string", "split", t, [ c; sep ]) |> Some
    | "Split", Some c, [ sep; options ] -> Helper.LibCall(com, "fable_string", "split", t, [ c; sep; options ]) |> Some
    | "Split", Some c, [ sep; count; _options ] ->
        Helper.LibCall(com, "fable_string", "split_with_count", t, [ c; sep; count ])
        |> Some
    // String.Join(sep, items)
    | "Join", None, [ sep; items ] -> Helper.LibCall(com, "fable_string", "join", t, [ sep; items ]) |> Some
    // String.Concat(items) → iolist_to_binary(Items)
    | "Concat", None, args ->
        match args with
        | [ items ] -> emitExpr r t [ items ] "iolist_to_binary($0)" |> Some
        | [ a; b ] -> emitExpr r t [ a; b ] "iolist_to_binary([$0, $1])" |> Some
        | [ a; b; c ] -> emitExpr r t [ a; b; c ] "iolist_to_binary([$0, $1, $2])" |> Some
        | _ -> None
    // String.Compare
    | "Compare", None, [ a; b ] -> Helper.LibCall(com, "fable_string", "compare", t, [ a; b ]) |> Some
    | "Compare", None, [ a; b; compType ] ->
        // Dispatch on ignoreCase bool (true) or StringComparison enum (5=OrdinalIgnoreCase)
        emitExpr
            r
            t
            [ a; b; compType ]
            "(fun() -> case $2 of true -> fable_string:compare_ignore_case($0, $1); 5 -> fable_string:compare_ignore_case($0, $1); _ -> fable_string:compare($0, $1) end end)()"
        |> Some
    | "Compare", None, [ a; startA; b; startB; len ] ->
        // String.Compare(a, startA, b, startB, len) — substring comparison
        emitExpr
            r
            t
            [ a; startA; b; startB; len ]
            "fable_string:compare(binary:part($0, $1, $4), binary:part($2, $3, $4))"
        |> Some
    | "Compare", None, [ a; startA; b; startB; len; compType ] ->
        // String.Compare(a, startA, b, startB, len, compType) — substring comparison with comparison type
        emitExpr
            r
            t
            [ a; startA; b; startB; len; compType ]
            "(fun() -> case $5 of true -> fable_string:compare_ignore_case(binary:part($0, $1, $4), binary:part($2, $3, $4)); 5 -> fable_string:compare_ignore_case(binary:part($0, $1, $4), binary:part($2, $3, $4)); _ -> fable_string:compare(binary:part($0, $1, $4), binary:part($2, $3, $4)) end end)()"
        |> Some
    // String.IsNullOrEmpty / IsNullOrWhiteSpace (static on System.String)
    | "IsNullOrEmpty", None, [ str ] -> Helper.LibCall(com, "fable_string", "is_null_or_empty", t, [ str ]) |> Some
    | "IsNullOrWhiteSpace", None, [ str ] ->
        Helper.LibCall(com, "fable_string", "is_null_or_white_space", t, [ str ])
        |> Some
    // String.Equals static
    | "Equals", None, [ a; b ] -> equals r true a b |> Some
    | "Equals", None, [ a; b; compType ] ->
        // Dispatch on StringComparison: 5=OrdinalIgnoreCase → lowercase compare, else exact
        emitExpr
            r
            t
            [ a; b; compType ]
            "(fun() -> case $2 of 5 -> string:lowercase($0) =:= string:lowercase($1); _ -> $0 =:= $1 end end)()"
        |> Some
    // str.PadLeft(width)
    | "PadLeft", Some c, [ width ] -> Helper.LibCall(com, "fable_string", "pad_left", t, [ c; width ]) |> Some
    | "PadLeft", Some c, [ width; padChar ] ->
        Helper.LibCall(com, "fable_string", "pad_left", t, [ c; width; padChar ])
        |> Some
    // str.PadRight(width)
    | "PadRight", Some c, [ width ] -> Helper.LibCall(com, "fable_string", "pad_right", t, [ c; width ]) |> Some
    | "PadRight", Some c, [ width; padChar ] ->
        Helper.LibCall(com, "fable_string", "pad_right", t, [ c; width; padChar ])
        |> Some
    // str.Contains(sub) → fable_string:contains
    | "Contains", Some c, [ sub ] -> Helper.LibCall(com, "fable_string", "contains", t, [ c; sub ]) |> Some
    // str.IndexOf(sub) / str.IndexOf(sub, startIdx)
    | "IndexOf", Some c, [ sub ] ->
        match sub.Type with
        | Type.Char -> emitExpr r t [ c; sub ] "fable_string:index_of($0, <<$1/utf8>>)" |> Some
        | _ -> Helper.LibCall(com, "fable_string", "index_of", t, [ c; sub ]) |> Some
    | "IndexOf", Some c, [ sub; startIdx ] ->
        match sub.Type with
        | Type.Char ->
            emitExpr r t [ c; sub; startIdx ] "fable_string:index_of($0, <<$1/utf8>>, $2)"
            |> Some
        | _ -> Helper.LibCall(com, "fable_string", "index_of", t, [ c; sub; startIdx ]) |> Some
    // str.LastIndexOf(sub) / str.LastIndexOf(sub, maxIdx)
    | "LastIndexOf", Some c, [ sub ] ->
        match sub.Type with
        | Type.Char -> emitExpr r t [ c; sub ] "fable_string:last_index_of($0, <<$1/utf8>>)" |> Some
        | _ -> Helper.LibCall(com, "fable_string", "last_index_of", t, [ c; sub ]) |> Some
    | "LastIndexOf", Some c, [ sub; maxIdx ] ->
        match sub.Type with
        | Type.Char ->
            emitExpr r t [ c; sub; maxIdx ] "fable_string:last_index_of($0, <<$1/utf8>>, $2)"
            |> Some
        | _ ->
            Helper.LibCall(com, "fable_string", "last_index_of", t, [ c; sub; maxIdx ])
            |> Some
    // str.ToCharArray() → binary_to_list(Str)
    | "ToCharArray", Some c, [] -> emitExpr r t [ c ] "binary_to_list($0)" |> Some
    | "ToCharArray", Some c, [ start; len ] ->
        Helper.LibCall(com, "fable_string", "to_char_array", t, [ c; start; len ])
        |> Some
    // str.Chars(idx) or str.[idx] → binary:at(Str, Idx)
    | ("get_Chars" | "get_Item"), Some c, [ idx ] -> emitExpr r t [ c; idx ] "binary:at($0, $1)" |> Some
    // str.Insert(idx, value)
    | "Insert", Some c, [ idx; value ] -> Helper.LibCall(com, "fable_string", "insert", t, [ c; idx; value ]) |> Some
    // str.Remove(startIdx)
    | "Remove", Some c, [ startIdx ] -> Helper.LibCall(com, "fable_string", "remove", t, [ c; startIdx ]) |> Some
    | "Remove", Some c, [ startIdx; count ] ->
        Helper.LibCall(com, "fable_string", "remove", t, [ c; startIdx; count ]) |> Some
    // str.IndexOf(sub) — let it fall through to JS replacements which generates indexOf → handled in Fable2Beam
    // str.Contains(sub) — let it fall through to JS replacements which generates indexOf >= 0
    // Instance methods: Equals, CompareTo, GetHashCode
    | "Equals", Some c, [ arg ] -> equals r true c arg |> Some
    | "Equals", Some c, [ arg; compType ] ->
        // Dispatch on StringComparison: 5=OrdinalIgnoreCase → lowercase compare, else exact
        emitExpr
            r
            t
            [ c; arg; compType ]
            "(fun() -> case $2 of 5 -> string:lowercase($0) =:= string:lowercase($1); _ -> $0 =:= $1 end end)()"
        |> Some
    | "CompareTo", Some c, [ arg ] -> compare com r c arg |> Some
    | "GetHashCode", Some c, [] -> emitExpr r t [ c ] "erlang:phash2($0)" |> Some
    // str.GetEnumerator() → convert string to list of codepoints and create enumerator
    | "GetEnumerator", Some c, _ -> emitExpr r t [ c ] "fable_utils:get_enumerator(binary_to_list($0))" |> Some
    // String.Format("{0} {1}", arg0, arg1)
    | "Format", None, (fmtStr :: fmtArgs) ->
        let argsArray = Value(NewArray(ArrayValues fmtArgs, Any, MutableArray), None)

        Helper.LibCall(com, "fable_string", "format", t, [ fmtStr; argsArray ], ?loc = r)
        |> Some
    | _ -> None

/// Beam-specific Option module replacements.
/// Options in Erlang: None = undefined atom, Some(x) = x.
let private optionModule
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "None", _ -> NewOption(None, t, false) |> makeValue r |> Some
    | "GetValue", [ c ] -> Some c
    | ("OfObj" | "OfNullable"), [ arg ] -> Some arg
    | ("ToObj" | "ToNullable"), [ arg ] -> Some arg
    | "DefaultValue", [ defVal; opt ] ->
        Helper.LibCall(com, "fable_option", "default_value", t, [ opt; defVal ]) |> Some
    | "DefaultWith", [ defFn; opt ] -> Helper.LibCall(com, "fable_option", "default_with", t, [ opt; defFn ]) |> Some
    | "Map", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "map", t, [ fn; opt ]) |> Some
    | "Bind", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "bind", t, [ fn; opt ]) |> Some
    | "IsSome", [ c ] -> Test(c, OptionTest true, r) |> Some
    | "IsNone", [ c ] -> Test(c, OptionTest false, r) |> Some
    | "OrElse", [ ifNone; opt ] -> Helper.LibCall(com, "fable_option", "or_else", t, [ opt; ifNone ]) |> Some
    | "OrElseWith", [ ifNoneFn; opt ] ->
        Helper.LibCall(com, "fable_option", "or_else_with", t, [ opt; ifNoneFn ])
        |> Some
    | "Iterate", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "iter", t, [ fn; opt ]) |> Some
    | "Map2", [ fn; opt1; opt2 ] -> Helper.LibCall(com, "fable_option", "map2", t, [ fn; opt1; opt2 ]) |> Some
    | "Map3", [ fn; opt1; opt2; opt3 ] ->
        Helper.LibCall(com, "fable_option", "map3", t, [ fn; opt1; opt2; opt3 ]) |> Some
    | "Contains", [ value; opt ] -> Helper.LibCall(com, "fable_option", "contains", t, [ value; opt ]) |> Some
    | "Filter", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "filter", t, [ fn; opt ]) |> Some
    | "Fold", [ fn; state; opt ] -> Helper.LibCall(com, "fable_option", "fold", t, [ fn; state; opt ]) |> Some
    | "FoldBack", [ fn; opt; state ] -> Helper.LibCall(com, "fable_option", "fold_back", t, [ fn; opt; state ]) |> Some
    | "ToArray", [ opt ] -> Helper.LibCall(com, "fable_option", "to_array", t, [ opt ]) |> Some
    | "ToList", [ opt ] -> Helper.LibCall(com, "fable_option", "to_list", t, [ opt ]) |> Some
    | "Flatten", [ opt ] -> Helper.LibCall(com, "fable_option", "flatten", t, [ opt ]) |> Some
    | "Count", [ opt ] -> Helper.LibCall(com, "fable_option", "count", t, [ opt ]) |> Some
    | "ForAll", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "for_all", t, [ fn; opt ]) |> Some
    | "Exists", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "exists", t, [ fn; opt ]) |> Some
    | _ -> None

/// Beam-specific FSharpOption instance method replacements.
let private options
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (_args: Expr list)
    =
    match info.CompiledName, thisArg with
    | "Some", _ -> NewOption(List.tryHead _args, t.Generics.Head, false) |> makeValue r |> Some
    | "get_None", _ -> NewOption(None, t.Generics.Head, false) |> makeValue r |> Some
    | "get_Value", Some c -> Some c
    | "get_IsSome", Some c -> Test(c, OptionTest true, r) |> Some
    | "get_IsNone", Some c -> Test(c, OptionTest false, r) |> Some
    | _ -> None

/// Beam-specific Result module replacements.
/// Result in Erlang: Ok x = {0, X}, Error e = {1, E}.
let private resultModule
    (com: ICompiler)
    (_ctx: Context)
    _r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "Map", [ fn; result ] -> Helper.LibCall(com, "fable_result", "map", t, [ fn; result ]) |> Some
    | "MapError", [ fn; result ] -> Helper.LibCall(com, "fable_result", "map_error", t, [ fn; result ]) |> Some
    | "Bind", [ fn; result ] -> Helper.LibCall(com, "fable_result", "bind", t, [ fn; result ]) |> Some
    | "IsOk", [ result ] -> Helper.LibCall(com, "fable_result", "is_ok", t, [ result ]) |> Some
    | "IsError", [ result ] -> Helper.LibCall(com, "fable_result", "is_error", t, [ result ]) |> Some
    | "Contains", [ value; result ] -> Helper.LibCall(com, "fable_result", "contains", t, [ value; result ]) |> Some
    | "Count", [ result ] -> Helper.LibCall(com, "fable_result", "count", t, [ result ]) |> Some
    | "DefaultValue", [ defVal; result ] ->
        Helper.LibCall(com, "fable_result", "default_value", t, [ defVal; result ])
        |> Some
    | "DefaultWith", [ defFn; result ] ->
        Helper.LibCall(com, "fable_result", "default_with", t, [ defFn; result ])
        |> Some
    | "Exists", [ fn; result ] -> Helper.LibCall(com, "fable_result", "exists", t, [ fn; result ]) |> Some
    | "Fold", [ fn; state; result ] -> Helper.LibCall(com, "fable_result", "fold", t, [ fn; state; result ]) |> Some
    | "FoldBack", [ fn; result; state ] ->
        Helper.LibCall(com, "fable_result", "fold_back", t, [ fn; result; state ])
        |> Some
    | "ForAll", [ fn; result ] -> Helper.LibCall(com, "fable_result", "forall", t, [ fn; result ]) |> Some
    | "Iterate", [ fn; result ] -> Helper.LibCall(com, "fable_result", "iter", t, [ fn; result ]) |> Some
    | "ToArray", [ result ] -> Helper.LibCall(com, "fable_result", "to_array", t, [ result ]) |> Some
    | "ToList", [ result ] -> Helper.LibCall(com, "fable_result", "to_list", t, [ result ]) |> Some
    | "ToOption", [ result ] -> Helper.LibCall(com, "fable_result", "to_option", t, [ result ]) |> Some
    | _ -> None

/// Beam-specific type conversion replacements.
/// Handles int(), float(), string(), ToString, Parse, etc.
let private conversions
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    // int(x), byte(x), etc. → convert to integer
    | ("ToSByte" | "ToByte" | "ToInt8" | "ToUInt8" | "ToInt16" | "ToUInt16" | "ToInt" | "ToUInt" | "ToInt32" | "ToUInt32" | "ToInt64" | "ToUInt64" | "ToIntPtr" | "ToUIntPtr"),
      [ arg ] ->
        match arg.Type with
        | Type.String -> Helper.LibCall(com, "fable_convert", "to_int", t, [ arg ], ?loc = r) |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r t [ arg ] "trunc($0)" |> Some
            | _ -> Some arg // int-to-int: no conversion needed in Erlang
        | Type.Char -> Some arg // char is already an integer in Erlang
        | _ -> Some arg
    // float(x), double(x) → convert to float
    | ("ToSingle" | "ToDouble"), [ arg ] ->
        match arg.Type with
        | Type.String -> Helper.LibCall(com, "fable_convert", "to_float", t, [ arg ]) |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> Some arg // already float
            | _ -> emitExpr r t [ arg ] "float($0)" |> Some
        | _ -> emitExpr r t [ arg ] "float($0)" |> Some
    // string(x), ToString() → convert to string
    | "ToString", _ ->
        let arg =
            match _thisArg with
            | Some c -> c
            | None ->
                match args with
                | [ a ] -> a
                | _ -> Value(Null Type.Unit, None)

        match arg.Type with
        | Type.String -> Some arg
        | Type.Char -> emitExpr r t [ arg ] "<<$0/utf8>>" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r t [ arg ] "float_to_binary($0)" |> Some
            | _ -> emitExpr r t [ arg ] "integer_to_binary($0)" |> Some
        | Type.Boolean -> emitExpr r t [ arg ] "atom_to_binary($0)" |> Some
        | _ -> Helper.LibCall(com, "fable_convert", "to_string", t, [ arg ], ?loc = r) |> Some
    // char(x) → Erlang integers represent chars
    | "ToChar", [ arg ] ->
        match arg.Type with
        | Type.String -> emitExpr r t [ arg ] "binary:first($0)" |> Some
        | _ -> Some arg // numbers are already chars in Erlang
    | _ -> None

/// Beam-specific numeric type method replacements (Parse, ToString, Equals, CompareTo, GetHashCode).
let private numericTypes
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    | "Parse", None, [ arg ] ->
        match info.DeclaringEntityFullName with
        | "System.Double"
        | "System.Single" -> Helper.LibCall(com, "fable_convert", "to_float", t, [ arg ]) |> Some
        | "System.Decimal" -> Helper.LibCall(com, "fable_decimal", "parse", t, [ arg ], ?loc = r) |> Some
        | _ ->
            // Int32, Int64, Byte, etc. — all parse to integer
            emitExpr r t [ arg ] "binary_to_integer($0)" |> Some
    | "ToString", Some c, [] ->
        match c.Type with
        | Type.Number(kind, _) ->
            match kind with
            | Decimal -> Helper.LibCall(com, "fable_decimal", "to_string", t, [ c ], ?loc = r) |> Some
            | Float16
            | Float32
            | Float64 -> emitExpr r t [ c ] "float_to_binary($0)" |> Some
            | _ -> emitExpr r t [ c ] "integer_to_binary($0)" |> Some
        | _ -> None
    | "ToString", Some c, [ fmt ] ->
        Helper.LibCall(com, "fable_convert", "int_to_string_with_format", t, [ c; fmt ], ?loc = r)
        |> Some
    | "Equals", Some thisObj, [ arg ] -> equals r true thisObj arg |> Some
    | "CompareTo", Some thisObj, [ arg ] -> compare com r thisObj arg |> Some
    | "GetHashCode", Some thisObj, [] -> emitExpr r t [ thisObj ] "erlang:phash2($0)" |> Some
    // IEEE 754 special value checks (static methods on Double/Single)
    | "IsNaN", None, [ arg ] ->
        // NaN is the only value not equal to itself
        emitExpr r t [ arg ] "($0 =/= $0)" |> Some
    | "IsPositiveInfinity", None, [ arg ] -> emitExpr r t [ arg ] "($0 =:= fable_utils:pos_infinity())" |> Some
    | "IsNegativeInfinity", None, [ arg ] -> emitExpr r t [ arg ] "($0 =:= fable_utils:neg_infinity())" |> Some
    | "IsInfinity", None, [ arg ] ->
        emitExpr r t [ arg ] "(($0 =:= fable_utils:pos_infinity()) orelse ($0 =:= fable_utils:neg_infinity()))"
        |> Some
    // System.Double.Pow (static)
    | "Pow", None, [ base_; exp_ ] -> emitExpr r t [ base_; exp_ ] "math:pow($0, $1)" |> Some
    // TryParse: F# out-parameter pattern. Fable passes [str; addressOfOutRef].
    // Our function returns bool and sets the out-ref via put(Ref, Value).
    | "TryParse", None, str :: outRef :: _ ->
        match info.DeclaringEntityFullName with
        | Types.float16
        | Types.float32
        | Types.float64 ->
            Helper.LibCall(com, "fable_convert", "try_parse_float", t, [ str; outRef ], ?loc = r)
            |> Some
        | Types.decimal ->
            Helper.LibCall(com, "fable_decimal", "try_parse", t, [ str; outRef ], ?loc = r)
            |> Some
        | _ ->
            Helper.LibCall(com, "fable_convert", "try_parse_int", t, [ str; outRef ], ?loc = r)
            |> Some
    // Decimal — fixed-scale integer (value × 10^28).
    // +, -, rem, abs, sign, comparisons work natively; *, / need library calls.
    | ("op_Addition" | "Add"), None, [ left; right ] -> makeBinOp r t left right BinaryPlus |> Some
    | ("op_Subtraction" | "Subtract"), None, [ left; right ] -> makeBinOp r t left right BinaryMinus |> Some
    | ("op_Multiply" | "Multiply"), None, [ left; right ] ->
        Helper.LibCall(com, "fable_decimal", "multiply", t, [ left; right ], ?loc = r)
        |> Some
    | ("op_Division" | "Divide"), None, [ left; right ] ->
        Helper.LibCall(com, "fable_decimal", "divide", t, [ left; right ], ?loc = r)
        |> Some
    | ("op_Modulus" | "Remainder"), None, [ left; right ] -> makeBinOp r t left right BinaryModulus |> Some
    | ("op_UnaryNegation" | "Negate"), None, [ operand ] ->
        Operation(Unary(UnaryMinus, operand), Tags.empty, t, r) |> Some
    // Static properties
    | "get_Zero", None, _ -> makeIntConst 0 |> Some
    | "get_One", None, _ -> Helper.LibCall(com, "fable_decimal", "get_one", t, [], ?loc = r) |> Some
    | "get_MinusOne", None, _ -> Helper.LibCall(com, "fable_decimal", "get_minus_one", t, [], ?loc = r) |> Some
    // MaxValue / MinValue
    | ("get_MaxValue" | "get_MinValue"), None, _ -> None // Not yet supported
    | _ -> None

/// Beam-specific System.Convert replacements.
let private convert
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    let toInt (arg: Expr) =
        match arg.Type with
        | Type.String -> Helper.LibCall(com, "fable_convert", "to_int", t, [ arg ], ?loc = r) |> Some
        | Type.Char -> Some arg
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r t [ arg ] "trunc($0)" |> Some
            | _ -> Some arg
        | _ -> Some arg

    let toFloat (arg: Expr) =
        match arg.Type with
        | Type.String -> Helper.LibCall(com, "fable_convert", "to_float", t, [ arg ]) |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> Some arg
            | _ -> emitExpr r t [ arg ] "float($0)" |> Some
        | _ -> emitExpr r t [ arg ] "float($0)" |> Some

    match info.CompiledName, args with
    | ("ToSByte" | "ToByte" | "ToInt16" | "ToUInt16" | "ToInt32" | "ToUInt32" | "ToInt64" | "ToUInt64"), [ arg ] ->
        toInt arg
    | ("ToSByte" | "ToByte" | "ToInt16" | "ToUInt16" | "ToInt32" | "ToUInt32" | "ToInt64" | "ToUInt64"),
      [ arg; baseArg ] ->
        Helper.LibCall(com, "fable_convert", "to_int_with_base", t, [ arg; baseArg ], ?loc = r)
        |> Some
    | ("ToSingle" | "ToDouble"), [ arg ] -> toFloat arg
    | "ToChar", [ arg ] ->
        match arg.Type with
        | Type.String -> emitExpr r t [ arg ] "binary:first($0)" |> Some
        | _ -> Some arg
    | "ToString", [ arg ] ->
        match arg.Type with
        | Type.String -> Some arg
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r t [ arg ] "float_to_binary($0)" |> Some
            | _ -> emitExpr r t [ arg ] "integer_to_binary($0)" |> Some
        | _ -> Helper.LibCall(com, "fable_convert", "to_string", t, [ arg ], ?loc = r) |> Some
    | "ToString", [ arg; baseArg ] ->
        let bitWidth =
            match arg.Type with
            | Type.Number(Int8, _)
            | Type.Number(UInt8, _) -> 8
            | Type.Number(Int16, _)
            | Type.Number(UInt16, _) -> 16
            | Type.Number(Int64, _)
            | Type.Number(UInt64, _) -> 64
            | _ -> 32

        Helper.LibCall(
            com,
            "fable_convert",
            "to_string_with_base",
            t,
            [ arg; baseArg; makeIntConst bitWidth ],
            ?loc = r
        )
        |> Some
    | "ToBoolean", [ arg ] ->
        match arg.Type with
        | Type.String ->
            Helper.LibCall(com, "fable_convert", "boolean_parse", t, [ arg ], ?loc = r)
            |> Some
        | _ -> None
    | "ToBase64String", [ arg ] -> Helper.LibCall(com, "fable_convert", "to_base64", t, [ arg ], ?loc = r) |> Some
    | "FromBase64String", [ arg ] ->
        Helper.LibCall(com, "fable_convert", "from_base64", t, [ arg ], ?loc = r)
        |> Some
    | _ -> None

/// Beam-specific List module replacements.
/// Lists in Erlang are native linked lists, same as F#.
let private listModule
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "Head", [ list ] -> emitExpr r t [ list ] "erlang:hd($0)" |> Some
    | "Tail", [ list ] -> emitExpr r t [ list ] "erlang:tl($0)" |> Some
    | ("Length" | "Count"), [ list ] -> emitExpr r t [ list ] "erlang:length($0)" |> Some
    | "IsEmpty", [ list ] -> emitExpr r t [ list ] "($0 =:= [])" |> Some
    | "Empty", _ -> Value(NewList(None, t), None) |> Some
    | "Singleton", [ item ] -> emitExpr r t [ item ] "[$0]" |> Some
    | "Map", [ fn; list ] -> emitExpr r t [ fn; list ] "lists:map($0, $1)" |> Some
    | "MapIndexed", [ fn; list ] -> Helper.LibCall(com, "fable_list", "map_indexed", t, [ fn; list ]) |> Some
    | "Filter", [ fn; list ] -> emitExpr r t [ fn; list ] "lists:filter($0, $1)" |> Some
    | "Reverse", [ list ] -> emitExpr r t [ list ] "lists:reverse($0)" |> Some
    | "Append", [ l1; l2 ] -> emitExpr r t [ l1; l2 ] "lists:append($0, $1)" |> Some
    | "Concat", [ lists ] -> emitExpr r t [ lists ] "lists:append($0)" |> Some
    | "Sum", [ list ] -> emitExpr r t [ list ] "lists:sum($0)" |> Some
    | "SumBy", [ fn; list ] -> Helper.LibCall(com, "fable_list", "sum_by", t, [ fn; list ]) |> Some
    | "Fold", [ fn; state; list ] -> Helper.LibCall(com, "fable_list", "fold", t, [ fn; state; list ]) |> Some
    | "FoldBack", [ fn; list; state ] -> Helper.LibCall(com, "fable_list", "fold_back", t, [ fn; list; state ]) |> Some
    | "Reduce", [ fn; list ] -> Helper.LibCall(com, "fable_list", "reduce", t, [ fn; list ]) |> Some
    | "Sort", [ list ] -> emitExpr r t [ list ] "lists:sort($0)" |> Some
    | "SortBy", [ fn; list ] -> Helper.LibCall(com, "fable_list", "sort_by", t, [ fn; list ]) |> Some
    | "SortDescending", [ list ] -> emitExpr r t [ list ] "lists:reverse(lists:sort($0))" |> Some
    | "SortByDescending", [ fn; list ] ->
        Helper.LibCall(com, "fable_list", "sort_by_descending", t, [ fn; list ]) |> Some
    | "SortWith", [ fn; list ] -> Helper.LibCall(com, "fable_list", "sort_with", t, [ fn; list ]) |> Some
    | "Contains", [ item; list ] -> emitExpr r t [ item; list ] "lists:member($0, $1)" |> Some
    | "Exists", [ fn; list ] -> emitExpr r t [ fn; list ] "lists:any($0, $1)" |> Some
    | "ForAll", [ fn; list ] -> emitExpr r t [ fn; list ] "lists:all($0, $1)" |> Some
    | "Iterate", [ fn; list ] -> emitExpr r t [ fn; list ] "lists:foreach($0, $1)" |> Some
    | "Find", [ fn; list ] -> Helper.LibCall(com, "fable_list", "find", t, [ fn; list ]) |> Some
    | "TryFind", [ fn; list ] -> Helper.LibCall(com, "fable_list", "try_find", t, [ fn; list ]) |> Some
    | "Choose", [ fn; list ] -> Helper.LibCall(com, "fable_list", "choose", t, [ fn; list ]) |> Some
    | "Collect", [ fn; list ] -> Helper.LibCall(com, "fable_list", "collect", t, [ fn; list ]) |> Some
    | "Partition", [ fn; list ] -> emitExpr r t [ fn; list ] "lists:partition($0, $1)" |> Some
    | "Zip", [ l1; l2 ] -> Helper.LibCall(com, "fable_list", "zip", t, [ l1; l2 ]) |> Some
    | "Unzip", [ list ] -> emitExpr r t [ list ] "lists:unzip($0)" |> Some
    | "Min", [ list ] -> emitExpr r t [ list ] "lists:min($0)" |> Some
    | "Max", [ list ] -> emitExpr r t [ list ] "lists:max($0)" |> Some
    | "MinBy", [ fn; list ] -> Helper.LibCall(com, "fable_list", "min_by", t, [ fn; list ]) |> Some
    | "MaxBy", [ fn; list ] -> Helper.LibCall(com, "fable_list", "max_by", t, [ fn; list ]) |> Some
    | "Indexed", [ list ] -> Helper.LibCall(com, "fable_list", "indexed", t, [ list ]) |> Some
    | "ReduceBack", [ fn; list ] -> Helper.LibCall(com, "fable_list", "reduce_back", t, [ fn; list ]) |> Some
    | ("Init" | "Initialize"), [ count; fn ] -> Helper.LibCall(com, "fable_list", "init", t, [ count; fn ]) |> Some
    | "Replicate", [ count; value ] -> Helper.LibCall(com, "fable_list", "replicate", t, [ count; value ]) |> Some
    | "Item", [ idx; list ] -> emitExpr r t [ idx; list ] "lists:nth($0 + 1, $1)" |> Some
    | "Skip", [ count; list ] -> emitExpr r t [ count; list ] "lists:nthtail($0, $1)" |> Some
    | "Take", [ count; list ] -> emitExpr r t [ count; list ] "lists:sublist($1, $0)" |> Some
    | "SkipWhile", [ fn; list ] -> emitExpr r t [ fn; list ] "lists:dropwhile($0, $1)" |> Some
    | "TakeWhile", [ fn; list ] -> emitExpr r t [ fn; list ] "lists:takewhile($0, $1)" |> Some
    | "Truncate", [ count; list ] -> emitExpr r t [ count; list ] "lists:sublist($1, $0)" |> Some
    | "Last", [ list ] -> emitExpr r t [ list ] "lists:last($0)" |> Some
    | "FindIndex", [ fn; list ] -> Helper.LibCall(com, "fable_list", "find_index", t, [ fn; list ]) |> Some
    | "TryFindIndex", [ fn; list ] -> Helper.LibCall(com, "fable_list", "try_find_index", t, [ fn; list ]) |> Some
    | "FindBack", [ fn; list ] -> Helper.LibCall(com, "fable_list", "find_back", t, [ fn; list ]) |> Some
    | "TryFindBack", [ fn; list ] -> Helper.LibCall(com, "fable_list", "try_find_back", t, [ fn; list ]) |> Some
    | "TryHead", [ list ] -> Helper.LibCall(com, "fable_list", "try_head", t, [ list ]) |> Some
    | "TryLast", [ list ] -> Helper.LibCall(com, "fable_list", "try_last", t, [ list ]) |> Some
    | "TryItem", [ idx; list ] -> Helper.LibCall(com, "fable_list", "try_item", t, [ idx; list ]) |> Some
    | "ExactlyOne", [ list ] -> Helper.LibCall(com, "fable_list", "exactly_one", t, [ list ]) |> Some
    | "TryExactlyOne", [ list ] -> Helper.LibCall(com, "fable_list", "try_exactly_one", t, [ list ]) |> Some
    | "Distinct", [ list ] -> Helper.LibCall(com, "fable_list", "distinct", t, [ list ]) |> Some
    | "DistinctBy", [ fn; list ] -> Helper.LibCall(com, "fable_list", "distinct_by", t, [ fn; list ]) |> Some
    | "Pairwise", [ list ] -> Helper.LibCall(com, "fable_list", "pairwise", t, [ list ]) |> Some
    | "Exists2", [ fn; l1; l2 ] -> Helper.LibCall(com, "fable_list", "exists2", t, [ fn; l1; l2 ]) |> Some
    | "ForAll2", [ fn; l1; l2 ] -> Helper.LibCall(com, "fable_list", "forall2", t, [ fn; l1; l2 ]) |> Some
    | "Map2", [ fn; l1; l2 ] -> Helper.LibCall(com, "fable_list", "map2", t, [ fn; l1; l2 ]) |> Some
    | "Map3", [ fn; l1; l2; l3 ] -> Helper.LibCall(com, "fable_list", "map3", t, [ fn; l1; l2; l3 ]) |> Some
    | "MapIndexed2", [ fn; l1; l2 ] -> Helper.LibCall(com, "fable_list", "mapi2", t, [ fn; l1; l2 ]) |> Some
    | "Iterate2", [ fn; l1; l2 ] -> Helper.LibCall(com, "fable_list", "iter2", t, [ fn; l1; l2 ]) |> Some
    | "IterateIndexed", [ fn; list ] -> Helper.LibCall(com, "fable_list", "iteri", t, [ fn; list ]) |> Some
    | "IterateIndexed2", [ fn; l1; l2 ] -> Helper.LibCall(com, "fable_list", "iteri2", t, [ fn; l1; l2 ]) |> Some
    | "Scan", [ fn; state; list ] -> Helper.LibCall(com, "fable_list", "scan", t, [ fn; state; list ]) |> Some
    | "ScanBack", [ fn; list; state ] -> Helper.LibCall(com, "fable_list", "scan_back", t, [ fn; list; state ]) |> Some
    | "Average", [ list ] -> Helper.LibCall(com, "fable_list", "average", t, [ list ]) |> Some
    | "AverageBy", [ fn; list ] -> Helper.LibCall(com, "fable_list", "average_by", t, [ fn; list ]) |> Some
    | "Zip3", [ l1; l2; l3 ] -> Helper.LibCall(com, "fable_list", "zip3", t, [ l1; l2; l3 ]) |> Some
    | "Unzip3", [ list ] -> emitExpr r t [ list ] "lists:unzip3($0)" |> Some
    | "Pick", [ fn; list ] -> Helper.LibCall(com, "fable_list", "pick", t, [ fn; list ]) |> Some
    | "TryPick", [ fn; list ] -> Helper.LibCall(com, "fable_list", "try_pick", t, [ fn; list ]) |> Some
    | "MapFold", [ fn; state; list ] -> Helper.LibCall(com, "fable_list", "map_fold", t, [ fn; state; list ]) |> Some
    | "MapFoldBack", [ fn; list; state ] ->
        Helper.LibCall(com, "fable_list", "map_fold_back", t, [ fn; list; state ])
        |> Some
    | "Unfold", [ fn; state ] -> Helper.LibCall(com, "fable_list", "unfold", t, [ fn; state ]) |> Some
    | "SplitAt", [ idx; list ] -> Helper.LibCall(com, "fable_list", "split_at", t, [ idx; list ]) |> Some
    | "ChunkBySize", [ size; list ] -> Helper.LibCall(com, "fable_list", "chunk_by_size", t, [ size; list ]) |> Some
    | "Windowed", [ size; list ] -> Helper.LibCall(com, "fable_list", "windowed", t, [ size; list ]) |> Some
    | "Except", [ excl; list ] -> Helper.LibCall(com, "fable_list", "except", t, [ excl; list ]) |> Some
    | "AllPairs", [ l1; l2 ] -> Helper.LibCall(com, "fable_list", "all_pairs", t, [ l1; l2 ]) |> Some
    | "Permute", [ fn; list ] -> Helper.LibCall(com, "fable_list", "permute", t, [ fn; list ]) |> Some
    | "SplitInto", [ count; list ] -> Helper.LibCall(com, "fable_list", "split_into", t, [ count; list ]) |> Some
    | "CountBy", [ fn; list ] -> Helper.LibCall(com, "fable_list", "count_by", t, [ fn; list ]) |> Some
    | "GroupBy", [ fn; list ] -> Helper.LibCall(com, "fable_list", "group_by", t, [ fn; list ]) |> Some
    | "Fold2", [ fn; state; l1; l2 ] -> Helper.LibCall(com, "fable_list", "fold2", t, [ fn; state; l1; l2 ]) |> Some
    | "FoldBack2", [ fn; l1; l2; state ] ->
        Helper.LibCall(com, "fable_list", "fold_back2", t, [ fn; l1; l2; state ])
        |> Some
    | "FindIndexBack", [ fn; list ] -> Helper.LibCall(com, "fable_list", "find_index_back", t, [ fn; list ]) |> Some
    | "TryFindIndexBack", [ fn; list ] ->
        Helper.LibCall(com, "fable_list", "try_find_index_back", t, [ fn; list ])
        |> Some
    | "Transpose", [ lists ] -> Helper.LibCall(com, "fable_list", "transpose", t, [ lists ]) |> Some
    | "CompareWith", [ fn; l1; l2 ] -> Helper.LibCall(com, "fable_list", "compare_with", t, [ fn; l1; l2 ]) |> Some
    | "UpdateAt", [ idx; value; list ] ->
        Helper.LibCall(com, "fable_list", "update_at", t, [ idx; value; list ]) |> Some
    | "InsertAt", [ idx; value; list ] ->
        Helper.LibCall(com, "fable_list", "insert_at", t, [ idx; value; list ]) |> Some
    | "InsertManyAt", [ idx; values; list ] ->
        Helper.LibCall(com, "fable_list", "insert_many_at", t, [ idx; values; list ])
        |> Some
    | "RemoveAt", [ idx; list ] -> Helper.LibCall(com, "fable_list", "remove_at", t, [ idx; list ]) |> Some
    | "RemoveManyAt", [ idx; count; list ] ->
        Helper.LibCall(com, "fable_list", "remove_many_at", t, [ idx; count; list ])
        |> Some
    | "ToArray", [ list ] -> Some(List.head args)
    | "OfArray", [ arr ] -> Some(List.head args)
    | "OfSeq", [ seq ] -> Some(List.head args)
    | "ToSeq", [ list ] -> Some(List.head args)
    | _ -> None

/// Beam-specific FSharpList instance method replacements.
let private lists
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (_args: Expr list)
    =
    match info.CompiledName, thisArg with
    | "get_Head", Some c -> emitExpr r t [ c ] "erlang:hd($0)" |> Some
    | "get_Tail", Some c -> emitExpr r t [ c ] "erlang:tl($0)" |> Some
    | "get_Length", Some c -> emitExpr r t [ c ] "erlang:length($0)" |> Some
    | "get_IsEmpty", Some c -> emitExpr r t [ c ] "($0 =:= [])" |> Some
    | "get_Empty", _ -> Value(NewList(None, t), None) |> Some
    | "get_Item", Some c ->
        match _args with
        | [ idx ] -> emitExpr r t [ c; idx ] "lists:nth($1 + 1, $0)" |> Some
        | _ -> None
    | _ -> None

/// Beam-specific Map module replacements.
/// F# Maps are represented as Erlang native maps (#{}).
let private mapModule
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "Empty", _ -> emitExpr r t [] "#{}" |> Some
    | "OfList", [ pairs ] -> emitExpr r t [ pairs ] "maps:from_list($0)" |> Some
    | "OfArray", [ arr ] -> emitExpr r t [ arr ] "maps:from_list($0)" |> Some
    | "OfSeq", [ seq ] -> emitExpr r t [ seq ] "maps:from_list($0)" |> Some
    | "Add", [ key; value; map ] -> emitExpr r t [ key; value; map ] "maps:put($0, $1, $2)" |> Some
    | "Find", [ key; map ] -> emitExpr r t [ key; map ] "maps:get($0, $1)" |> Some
    | "TryFind", [ key; map ] -> Helper.LibCall(com, "fable_map", "try_find", t, [ key; map ]) |> Some
    | "ContainsKey", [ key; map ] -> emitExpr r t [ key; map ] "maps:is_key($0, $1)" |> Some
    | "Remove", [ key; map ] -> emitExpr r t [ key; map ] "maps:remove($0, $1)" |> Some
    | "IsEmpty", [ map ] -> emitExpr r t [ map ] "(maps:size($0) =:= 0)" |> Some
    | ("Count" | "Length"), [ map ] -> emitExpr r t [ map ] "maps:size($0)" |> Some
    | "ToList", [ map ] -> emitExpr r t [ map ] "maps:to_list($0)" |> Some
    | "ToArray", [ map ] -> emitExpr r t [ map ] "maps:to_list($0)" |> Some
    | "ToSeq", [ map ] -> emitExpr r t [ map ] "maps:to_list($0)" |> Some
    | "Keys", [ map ] -> emitExpr r t [ map ] "maps:keys($0)" |> Some
    | "Values", [ map ] -> emitExpr r t [ map ] "maps:values($0)" |> Some
    | "Map", [ fn; map ] -> Helper.LibCall(com, "fable_map", "map", t, [ fn; map ]) |> Some
    | "Filter", [ fn; map ] -> Helper.LibCall(com, "fable_map", "filter", t, [ fn; map ]) |> Some
    | "Fold", [ fn; state; map ] -> Helper.LibCall(com, "fable_map", "fold", t, [ fn; state; map ]) |> Some
    | "FoldBack", [ fn; map; state ] -> Helper.LibCall(com, "fable_map", "fold_back", t, [ fn; map; state ]) |> Some
    | "Exists", [ fn; map ] -> Helper.LibCall(com, "fable_map", "exists", t, [ fn; map ]) |> Some
    | "ForAll", [ fn; map ] -> Helper.LibCall(com, "fable_map", "forall", t, [ fn; map ]) |> Some
    | "Iterate", [ fn; map ] -> Helper.LibCall(com, "fable_map", "iterate", t, [ fn; map ]) |> Some
    | "FindKey", [ fn; map ] -> Helper.LibCall(com, "fable_map", "find_key", t, [ fn; map ]) |> Some
    | "TryFindKey", [ fn; map ] -> Helper.LibCall(com, "fable_map", "try_find_key", t, [ fn; map ]) |> Some
    | "Partition", [ fn; map ] -> Helper.LibCall(com, "fable_map", "partition", t, [ fn; map ]) |> Some
    | "Pick", [ fn; map ] -> Helper.LibCall(com, "fable_map", "pick", t, [ fn; map ]) |> Some
    | "TryPick", [ fn; map ] -> Helper.LibCall(com, "fable_map", "try_pick", t, [ fn; map ]) |> Some
    | "MinKeyValue", [ map ] -> Helper.LibCall(com, "fable_map", "min_key_value", t, [ map ]) |> Some
    | "MaxKeyValue", [ map ] -> Helper.LibCall(com, "fable_map", "max_key_value", t, [ map ]) |> Some
    | "Change", [ key; fn; map ] -> Helper.LibCall(com, "fable_map", "change", t, [ key; fn; map ]) |> Some
    | _ -> None

/// Beam-specific FSharpMap instance method replacements.
let private maps
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    | ".ctor", None, [ pairs ] -> emitExpr r t [ pairs ] "maps:from_list($0)" |> Some
    | "get_Count", Some c, _ -> emitExpr r t [ c ] "maps:size($0)" |> Some
    | "get_IsEmpty", Some c, _ -> emitExpr r t [ c ] "(maps:size($0) =:= 0)" |> Some
    | "get_Item", Some c, [ key ] -> emitExpr r t [ key; c ] "maps:get($0, $1)" |> Some
    | "ContainsKey", Some c, [ key ] -> emitExpr r t [ key; c ] "maps:is_key($0, $1)" |> Some
    | "Add", Some c, [ key; value ] -> emitExpr r t [ key; value; c ] "maps:put($0, $1, $2)" |> Some
    | "Remove", Some c, [ key ] -> emitExpr r t [ key; c ] "maps:remove($0, $1)" |> Some
    | "TryGetValue", Some c, key :: outRef :: _ ->
        Helper.LibCall(com, "fable_map", "try_get_value", t, [ key; c; outRef ], ?loc = r)
        |> Some
    | "TryGetValue", Some c, [ key ] -> Helper.LibCall(com, "fable_map", "try_get_value", t, [ key; c ]) |> Some
    | "TryFind", Some c, [ key ] -> Helper.LibCall(com, "fable_map", "try_find", t, [ key; c ]) |> Some
    | _ -> None

/// Beam-specific Set module replacements.
/// F# Sets are represented as Erlang ordsets (sorted lists).
let private setModule
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "Empty", _ -> emitExpr r t [] "[]" |> Some
    | "Singleton", [ x ] -> emitExpr r t [ x ] "[$0]" |> Some
    | ("OfList" | "OfArray" | "OfSeq"), [ xs ] -> emitExpr r t [ xs ] "ordsets:from_list($0)" |> Some
    | "Add", [ elem; set ] -> emitExpr r t [ elem; set ] "ordsets:add_element($0, $1)" |> Some
    | "Contains", [ elem; set ] -> emitExpr r t [ elem; set ] "ordsets:is_element($0, $1)" |> Some
    | "Remove", [ elem; set ] -> emitExpr r t [ elem; set ] "ordsets:del_element($0, $1)" |> Some
    | "IsEmpty", [ set ] -> emitExpr r t [ set ] "($0 =:= [])" |> Some
    | "Count", [ set ] -> emitExpr r t [ set ] "erlang:length($0)" |> Some
    | "Union", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:union($0, $1)" |> Some
    | "Intersect", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:intersection($0, $1)" |> Some
    | "Difference", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:subtract($0, $1)" |> Some
    | "IsSubset", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:is_subset($0, $1)" |> Some
    | "IsSuperset", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:is_subset($1, $0)" |> Some
    | "IsProperSubset", [ s1; s2 ] -> Helper.LibCall(com, "fable_set", "is_proper_subset", t, [ s1; s2 ]) |> Some
    | "IsProperSuperset", [ s1; s2 ] -> Helper.LibCall(com, "fable_set", "is_proper_superset", t, [ s1; s2 ]) |> Some
    | "MinElement", [ set ] -> emitExpr r t [ set ] "erlang:hd($0)" |> Some
    | "MaxElement", [ set ] -> emitExpr r t [ set ] "lists:last($0)" |> Some
    | ("ToList" | "ToArray" | "ToSeq"), [ set ] -> Some set
    | "Filter", [ fn; set ] -> Helper.LibCall(com, "fable_set", "filter", t, [ fn; set ]) |> Some
    | "Map", [ fn; set ] -> Helper.LibCall(com, "fable_set", "map", t, [ fn; set ]) |> Some
    | "Fold", [ fn; state; set ] -> Helper.LibCall(com, "fable_set", "fold", t, [ fn; state; set ]) |> Some
    | "FoldBack", [ fn; set; state ] -> Helper.LibCall(com, "fable_set", "fold_back", t, [ fn; set; state ]) |> Some
    | "Exists", [ fn; set ] -> Helper.LibCall(com, "fable_set", "exists", t, [ fn; set ]) |> Some
    | "ForAll", [ fn; set ] -> Helper.LibCall(com, "fable_set", "forall", t, [ fn; set ]) |> Some
    | "Iterate", [ fn; set ] -> Helper.LibCall(com, "fable_set", "iterate", t, [ fn; set ]) |> Some
    | "Partition", [ fn; set ] -> Helper.LibCall(com, "fable_set", "partition", t, [ fn; set ]) |> Some
    | "UnionMany", [ sets ] -> Helper.LibCall(com, "fable_set", "union_many", t, [ sets ]) |> Some
    | "IntersectMany", [ sets ] -> Helper.LibCall(com, "fable_set", "intersect_many", t, [ sets ]) |> Some
    | _ -> None

/// Beam-specific FSharpSet instance method replacements.
let private sets
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    | ".ctor", None, [ xs ] -> emitExpr r t [ xs ] "ordsets:from_list($0)" |> Some
    | "get_Count", Some c, _ -> emitExpr r t [ c ] "erlang:length($0)" |> Some
    | "get_IsEmpty", Some c, _ -> emitExpr r t [ c ] "($0 =:= [])" |> Some
    | "Contains", Some c, [ elem ] -> emitExpr r t [ elem; c ] "ordsets:is_element($0, $1)" |> Some
    | "Add", Some c, [ elem ] -> emitExpr r t [ elem; c ] "ordsets:add_element($0, $1)" |> Some
    | "Remove", Some c, [ elem ] -> emitExpr r t [ elem; c ] "ordsets:del_element($0, $1)" |> Some
    | "get_MinimumElement", Some c, _ -> emitExpr r t [ c ] "erlang:hd($0)" |> Some
    | "get_MaximumElement", Some c, _ -> emitExpr r t [ c ] "lists:last($0)" |> Some
    | "IsSubsetOf", Some c, [ other ] -> emitExpr r t [ c; other ] "ordsets:is_subset($0, $1)" |> Some
    | "IsSupersetOf", Some c, [ other ] -> emitExpr r t [ c; other ] "ordsets:is_subset($1, $0)" |> Some
    | "IsProperSubsetOf", Some c, [ other ] ->
        Helper.LibCall(com, "fable_set", "is_proper_subset", t, [ c; other ]) |> Some
    | "IsProperSupersetOf", Some c, [ other ] ->
        Helper.LibCall(com, "fable_set", "is_proper_superset", t, [ c; other ]) |> Some
    | "op_Addition", None, [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:union($0, $1)" |> Some
    | "op_Subtraction", None, [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:subtract($0, $1)" |> Some
    | _ -> None

/// Beam-specific StringModule replacements.
let private stringModule
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "Concat", [ sep; items ] -> Helper.LibCall(com, "fable_string", "join", t, [ sep; items ]) |> Some
    | "Length", [ str ] -> emitExpr r t [ str ] "byte_size($0)" |> Some
    | "IsNullOrEmpty", [ str ] -> Helper.LibCall(com, "fable_string", "is_null_or_empty", t, [ str ]) |> Some
    | "IsNullOrWhiteSpace", [ str ] ->
        Helper.LibCall(com, "fable_string", "is_null_or_white_space", t, [ str ])
        |> Some
    | "Replicate", [ count; str ] -> Helper.LibCall(com, "fable_string", "replicate", t, [ count; str ]) |> Some
    | "ForAll", [ fn; str ] -> Helper.LibCall(com, "fable_string", "forall", t, [ fn; str ]) |> Some
    | "Exists", [ fn; str ] -> Helper.LibCall(com, "fable_string", "exists", t, [ fn; str ]) |> Some
    | "Initialize", [ count; fn ] -> Helper.LibCall(com, "fable_string", "init", t, [ count; fn ]) |> Some
    | "Collect", [ fn; str ] -> Helper.LibCall(com, "fable_string", "collect", t, [ fn; str ]) |> Some
    | "Iterate", [ fn; str ] -> Helper.LibCall(com, "fable_string", "iter", t, [ fn; str ]) |> Some
    | "IterateIndexed", [ fn; str ] -> Helper.LibCall(com, "fable_string", "iteri", t, [ fn; str ]) |> Some
    | "Map", [ fn; str ] -> Helper.LibCall(com, "fable_string", "map", t, [ fn; str ]) |> Some
    | "MapIndexed", [ fn; str ] -> Helper.LibCall(com, "fable_string", "mapi", t, [ fn; str ]) |> Some
    | "Filter", [ fn; str ] -> Helper.LibCall(com, "fable_string", "filter", t, [ fn; str ]) |> Some
    | _ -> None

/// Unwrap a ResizeArray (process dictionary ref) to its underlying list.
/// Needed because Seq operations that use lists:* BIFs directly
/// can receive a ResizeArray ref instead of a plain list.
/// ResizeArray<T> is represented as Array(T, ResizeArray) in Fable AST.
/// Also looks through TypeCast wrappers (e.g., ResizeArray :> seq<_>).
let rec private unwrapSeqArg r (expr: Expr) =
    match expr with
    | TypeCast(innerExpr, _) -> unwrapSeqArg r innerExpr
    | _ ->
        match expr.Type with
        | Array(_, Fable.ResizeArray) -> emitExpr r (List Any) [ expr ] "get($0)"
        | String -> emitExpr r (List Any) [ expr ] "binary_to_list($0)"
        | _ -> expr

/// Beam-specific Seq module replacements.
/// Sequences in Erlang are represented as eager lists.
/// Scalar-returning operations (fold, find, exists, etc.) route through compiled
/// seq.erl via Helper.LibCall which provides SignatureArgTypes. This enables the
/// uncurrySendingArgs FableTransform to automatically convert curried callbacks
/// (fun(A) -> fun(B) -> end) to uncurried Delegates (fun(A, B) -> end).
/// Sequence-returning operations stay as eager list implementations (BIFs/fable_list/fable_seq)
/// since the rest of the codebase expects plain Erlang lists.
/// Operations not in seq.erl (Distinct, GroupBy, etc.) stay in fable_seq.erl.
let private seqModule
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    // Cast — erased at runtime (Erlang is dynamically typed)
    | "Cast", [ arg ] -> TypeCast(arg, t) |> Some
    // Identity conversions — seq is a list in Beam (unwrap ResizeArray refs)
    | "ToList", [ seq ] -> unwrapSeqArg r seq |> Some
    | "ToArray", [ seq ] -> unwrapSeqArg r seq |> Some
    | "OfList", [ list ] -> Some list
    | "OfArray", [ arr ] -> Some arr
    // Empty
    | "Empty", _ -> Value(NewList(None, t), None) |> Some
    // === Scalar-returning operations — use compiled seq.erl via Helper.LibCall ===
    // Helper.LibCall provides SignatureArgTypes which enables uncurrySendingArgs to
    // automatically convert curried callbacks to uncurried Delegates.
    // NOTE: seq:length/1 has infinite recursion (shadows erlang:length/1), use BIF directly.
    // NOTE: seq:head uses try_head which conflates empty/None, use hd BIF directly.
    // Element access (no callback)
    | "Head", [ seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "erlang:hd($0)" |> Some
    | "Last", [ seq ] -> Helper.LibCall(com, "seq", "last", t, [ seq ], info.SignatureArgTypes) |> Some
    | ("Length" | "Count"), [ seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "erlang:length($0)" |> Some
    | "IsEmpty", [ seq ] ->
        Helper.LibCall(com, "seq", "is_empty", t, [ seq ], info.SignatureArgTypes)
        |> Some
    | "Item", [ idx; seq ] ->
        Helper.LibCall(com, "seq", "item", t, [ idx; seq ], info.SignatureArgTypes)
        |> Some
    | "TryHead", [ seq ] ->
        Helper.LibCall(com, "seq", "try_head", t, [ seq ], info.SignatureArgTypes)
        |> Some
    | "TryItem", [ idx; seq ] ->
        Helper.LibCall(com, "seq", "try_item", t, [ idx; seq ], info.SignatureArgTypes)
        |> Some
    | "TryLast", [ seq ] ->
        Helper.LibCall(com, "seq", "try_last", t, [ seq ], info.SignatureArgTypes)
        |> Some
    | "ExactlyOne", [ seq ] ->
        Helper.LibCall(com, "seq", "exactly_one", t, [ seq ], info.SignatureArgTypes)
        |> Some
    | "TryExactlyOne", [ seq ] ->
        Helper.LibCall(com, "seq", "try_exactly_one", t, [ seq ], info.SignatureArgTypes)
        |> Some
    // Predicates (1-arg callback)
    | "Exists", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "exists", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "ForAll", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "for_all", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    // Iteration (1-arg callback)
    | "Iterate", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "iterate", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    // Search (1-arg callback)
    | "Find", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "find", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "TryFind", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "try_find", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "FindBack", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "find_back", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "TryFindBack", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "try_find_back", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "FindIndex", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "find_index", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "TryFindIndex", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "try_find_index", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "FindIndexBack", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "find_index_back", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "TryFindIndexBack", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "try_find_index_back", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "Pick", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "pick", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "TryPick", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "try_pick", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    // === Operations with 2+ arg callbacks — uncurrySendingArgs handles callback conversion ===
    | "Fold", [ fn; state; seq ] ->
        Helper.LibCall(com, "seq", "fold", t, [ fn; state; seq ], info.SignatureArgTypes)
        |> Some
    | "FoldBack", [ fn; seq; state ] ->
        Helper.LibCall(com, "seq", "fold_back", t, [ fn; seq; state ], info.SignatureArgTypes)
        |> Some
    | "Reduce", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "reduce", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "ReduceBack", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "reduce_back", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "Exists2", [ fn; s1; s2 ] ->
        Helper.LibCall(com, "seq", "exists2", t, [ fn; s1; s2 ], info.SignatureArgTypes)
        |> Some
    | "ForAll2", [ fn; s1; s2 ] ->
        Helper.LibCall(com, "seq", "for_all2", t, [ fn; s1; s2 ], info.SignatureArgTypes)
        |> Some
    | "IterateIndexed", [ fn; seq ] ->
        Helper.LibCall(com, "seq", "iterate_indexed", t, [ fn; seq ], info.SignatureArgTypes)
        |> Some
    | "Iterate2", [ fn; s1; s2 ] ->
        Helper.LibCall(com, "seq", "iterate2", t, [ fn; s1; s2 ], info.SignatureArgTypes)
        |> Some
    | "Fold2", [ fn; state; s1; s2 ] ->
        Helper.LibCall(com, "seq", "fold2", t, [ fn; state; s1; s2 ], info.SignatureArgTypes)
        |> Some
    | "FoldBack2", [ fn; s1; s2; state ] ->
        Helper.LibCall(com, "seq", "fold_back2", t, [ fn; s1; s2; state ], info.SignatureArgTypes)
        |> Some
    | "CompareWith", [ fn; s1; s2 ] ->
        Helper.LibCall(com, "seq", "compare_with", t, [ fn; s1; s2 ], info.SignatureArgTypes)
        |> Some
    // === Sequence-returning operations — stay as eager list implementations ===
    // Simple BIF mappings (unwrap ResizeArray refs)
    | "Map", [ fn; seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ fn; seq ] "lists:map($0, $1)" |> Some
    | "Filter", [ fn; seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ fn; seq ] "lists:filter($0, $1)" |> Some
    | "Reverse", [ seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "lists:reverse($0)" |> Some
    | "Append", [ s1; s2 ] ->
        let s1 = unwrapSeqArg r s1
        let s2 = unwrapSeqArg r s2
        emitExpr r t [ s1; s2 ] "lists:append($0, $1)" |> Some
    | "Concat", [ seqs ] -> emitExpr r t [ seqs ] "lists:append($0)" |> Some
    | "Where", [ fn; seq ] -> emitExpr r t [ fn; seq ] "lists:filter($0, $1)" |> Some
    | "Tail", [ seq ] -> emitExpr r t [ seq ] "erlang:tl($0)" |> Some
    | "Partition", [ fn; seq ] ->
        let seq = unwrapSeqArg r seq in emitExpr r t [ fn; seq ] "lists:partition($0, $1)" |> Some
    | "Unzip", [ seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "lists:unzip($0)" |> Some
    // Sorting (no inject — use simple BIF-based implementations)
    | "Sort", [ seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "lists:sort($0)" |> Some
    | "SortDescending", [ seq ] ->
        let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "lists:reverse(lists:sort($0))" |> Some
    | "SortBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "sort_by", t, [ fn; seq ]) |> Some
    | "SortByDescending", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "sort_by_descending", t, [ fn; seq ]) |> Some
    | "SortWith", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "sort_with", t, [ fn; seq ]) |> Some
    // Aggregation (no inject — use simple BIF-based implementations)
    | "Sum", [ seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "lists:sum($0)" |> Some
    | "SumBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "sum_by", t, [ fn; seq ]) |> Some
    | "Min", [ seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "lists:min($0)" |> Some
    | "Max", [ seq ] -> let seq = unwrapSeqArg r seq in emitExpr r t [ seq ] "lists:max($0)" |> Some
    | "MinBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "min_by", t, [ fn; seq ]) |> Some
    | "MaxBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "max_by", t, [ fn; seq ]) |> Some
    | "Average", [ seq ] -> Helper.LibCall(com, "fable_list", "average", t, [ seq ]) |> Some
    | "AverageBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "average_by", t, [ fn; seq ]) |> Some
    | "Contains", [ item; seq ] ->
        let seq = unwrapSeqArg r seq in emitExpr r t [ item; seq ] "lists:member($0, $1)" |> Some
    // Reuse fable_list.erl for list-returning operations
    | "MapIndexed", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "map_indexed", t, [ fn; seq ]) |> Some
    | "Indexed", [ seq ] -> Helper.LibCall(com, "fable_list", "indexed", t, [ seq ]) |> Some
    | "Collect", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "collect", t, [ fn; seq ]) |> Some
    | "Choose", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "choose", t, [ fn; seq ]) |> Some
    | "Zip", [ s1; s2 ] -> Helper.LibCall(com, "fable_list", "zip", t, [ s1; s2 ]) |> Some
    | "AllPairs", [ s1; s2 ] -> Helper.LibCall(com, "fable_list", "all_pairs", t, [ s1; s2 ]) |> Some
    | "SplitInto", [ count; seq ] -> Helper.LibCall(com, "fable_list", "split_into", t, [ count; seq ]) |> Some
    | "Transpose", [ seqs ] -> Helper.LibCall(com, "fable_list", "transpose", t, [ seqs ]) |> Some
    | "UpdateAt", [ idx; value; seq ] -> Helper.LibCall(com, "fable_list", "update_at", t, [ idx; value; seq ]) |> Some
    | "InsertAt", [ idx; value; seq ] -> Helper.LibCall(com, "fable_list", "insert_at", t, [ idx; value; seq ]) |> Some
    | "InsertManyAt", [ idx; values; seq ] ->
        Helper.LibCall(com, "fable_list", "insert_many_at", t, [ idx; values; seq ])
        |> Some
    | "RemoveAt", [ idx; seq ] -> Helper.LibCall(com, "fable_list", "remove_at", t, [ idx; seq ]) |> Some
    | "RemoveManyAt", [ idx; count; seq ] ->
        Helper.LibCall(com, "fable_list", "remove_many_at", t, [ idx; count; seq ])
        |> Some
    | "MapFold", [ fn; state; seq ] -> Helper.LibCall(com, "fable_list", "map_fold", t, [ fn; state; seq ]) |> Some
    | "MapFoldBack", [ fn; seq; state ] ->
        Helper.LibCall(com, "fable_list", "map_fold_back", t, [ fn; seq; state ])
        |> Some
    // fable_seq.erl operations (list-returning)
    | "Delay", [ fn ] -> Helper.LibCall(com, "fable_seq", "delay", t, [ fn ]) |> Some
    | "EnumerateUsing", [ fn ] -> Helper.LibCall(com, "fable_seq", "delay", t, [ fn ]) |> Some
    | "Singleton", [ item ] -> Helper.LibCall(com, "fable_seq", "singleton", t, [ item ]) |> Some
    | "Unfold", [ fn; state ] -> Helper.LibCall(com, "fable_seq", "unfold", t, [ fn; state ]) |> Some
    | "Initialize", [ count; fn ] -> Helper.LibCall(com, "fable_seq", "initialize", t, [ count; fn ]) |> Some
    | "Take", [ count; seq ] -> Helper.LibCall(com, "fable_seq", "take", t, [ count; seq ]) |> Some
    | "Skip", [ count; seq ] -> Helper.LibCall(com, "fable_seq", "skip", t, [ count; seq ]) |> Some
    | "Truncate", [ count; seq ] -> Helper.LibCall(com, "fable_seq", "truncate", t, [ count; seq ]) |> Some
    | "TakeWhile", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "take_while", t, [ fn; seq ]) |> Some
    | "SkipWhile", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "skip_while", t, [ fn; seq ]) |> Some
    | "Pairwise", [ seq ] -> Helper.LibCall(com, "fable_seq", "pairwise", t, [ seq ]) |> Some
    | "Windowed", [ size; seq ] -> Helper.LibCall(com, "fable_seq", "windowed", t, [ size; seq ]) |> Some
    | "ChunkBySize", [ size; seq ] -> Helper.LibCall(com, "fable_seq", "chunk_by_size", t, [ size; seq ]) |> Some
    | "Scan", [ fn; state; seq ] -> Helper.LibCall(com, "fable_seq", "scan", t, [ fn; state; seq ]) |> Some
    | "ScanBack", [ fn; seq; state ] -> Helper.LibCall(com, "fable_seq", "scan_back", t, [ fn; seq; state ]) |> Some
    | "Map2", [ fn; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "map2", t, [ fn; s1; s2 ]) |> Some
    | "MapIndexed2", [ fn; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "map_indexed2", t, [ fn; s1; s2 ]) |> Some
    | "Zip3", [ s1; s2; s3 ] -> Helper.LibCall(com, "fable_seq", "zip3", t, [ s1; s2; s3 ]) |> Some
    // Operations NOT in compiled seq.erl — stay in fable_seq.erl
    | "Distinct", [ seq ] -> Helper.LibCall(com, "fable_seq", "distinct", t, [ seq ]) |> Some
    | "DistinctBy", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "distinct_by", t, [ fn; seq ]) |> Some
    | "GroupBy", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "group_by", t, [ fn; seq ]) |> Some
    | "CountBy", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "count_by", t, [ fn; seq ]) |> Some
    | "Except", [ excl; seq ] -> Helper.LibCall(com, "fable_seq", "except", t, [ excl; seq ]) |> Some
    | ("Readonly" | "ReadOnly" | "Cache"), [ seq ] -> Some seq
    // RuntimeHelpers — seq computation expression desugaring
    | "EnumerateWhile", [ guard; body ] ->
        // Eager: repeatedly evaluate guard and body until guard returns false
        emitExpr
            r
            t
            [ guard; body ]
            "(fun Enum_while() -> case ($0)(ok) of true -> ($1)(ok) ++ Enum_while(); false -> [] end end)()"
        |> Some
    | "EnumerateThenFinally", [ body; finalizer ] ->
        emitExpr r t [ body; finalizer ] "(fun() -> Result_etf = ($0)(ok), ($1)(ok), Result_etf end)()"
        |> Some
    | "CreateEvent", _ -> None // Not applicable for Beam
    | _ -> None

/// Beam-specific Array instance method replacements.
/// Arrays in Erlang are represented as lists.
let private arrays
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let isByteArray =
        match thisArg with
        | Some c ->
            match c.Type with
            | Type.Array(Type.Number(UInt8, _), _) -> true
            | _ -> false
        | None -> false

    match info.CompiledName, thisArg, args with
    | "get_Length", Some c, _ when isByteArray -> emitExpr r t [ c ] "byte_size($0)" |> Some
    | "get_Length", Some c, _ -> emitExpr r t [ c ] "erlang:length($0)" |> Some
    | "get_Item", Some c, [ idx ] when isByteArray -> emitExpr r t [ c; idx ] "binary:at($0, $1)" |> Some
    | "get_Item", Some c, [ idx ] -> emitExpr r t [ c; idx ] "lists:nth($1 + 1, $0)" |> Some
    | "set_Item", Some c, [ idx; value ] -> setExpr r c idx value |> Some
    // System.Array.Copy(source, dest, length) — copy first N elements
    | "Copy", None, [ src; _dest; len ] -> emitExpr r t [ src; len ] "lists:sublist($0, $1)" |> Some
    // System.Array.IndexOf(arr, value) — find index of value
    | "IndexOf", None, [ arr; value ] -> Helper.LibCall(com, "fable_list", "index_of_value", t, [ value; arr ]) |> Some
    | _ -> None

/// Beam-specific Array module replacements.
let private arrayModule
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | ("Length" | "Count"), [ arr ] -> emitExpr r t [ arr ] "erlang:length($0)" |> Some
    | "Item", [ idx; arr ] -> emitExpr r t [ arr; idx ] "lists:nth($1 + 1, $0)" |> Some
    | "Get", [ arr; idx ] -> emitExpr r t [ arr; idx ] "lists:nth($1 + 1, $0)" |> Some
    | "Set", [ arr; idx; value ] -> setExpr r arr idx value |> Some
    | "Head", [ arr ] -> emitExpr r t [ arr ] "erlang:hd($0)" |> Some
    | "Last", [ arr ] -> emitExpr r t [ arr ] "lists:last($0)" |> Some
    | "Tail", [ arr ] -> emitExpr r t [ arr ] "erlang:tl($0)" |> Some
    | "IsEmpty", [ arr ] -> emitExpr r t [ arr ] "($0 =:= [])" |> Some
    | "Empty", _ -> Value(NewArray(ArrayValues [], t, MutableArray), None) |> Some
    | "Singleton", [ item ] -> emitExpr r t [ item ] "[$0]" |> Some
    | "Map", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:map($0, $1)" |> Some
    | "MapIndexed", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "map_indexed", t, [ fn; arr ]) |> Some
    | "Filter", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:filter($0, $1)" |> Some
    | "Exists", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:any($0, $1)" |> Some
    | "ForAll", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:all($0, $1)" |> Some
    | "Iterate", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:foreach($0, $1)" |> Some
    | "Fold", [ fn; state; arr ] -> Helper.LibCall(com, "fable_list", "fold", t, [ fn; state; arr ]) |> Some
    | "FoldBack", [ fn; arr; state ] -> Helper.LibCall(com, "fable_list", "fold_back", t, [ fn; arr; state ]) |> Some
    | "Reduce", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "reduce", t, [ fn; arr ]) |> Some
    | "Reverse", [ arr ] -> emitExpr r t [ arr ] "lists:reverse($0)" |> Some
    | "Append", [ arr1; arr2 ] -> emitExpr r t [ arr1; arr2 ] "lists:append($0, $1)" |> Some
    | "Concat", [ arrs ] -> emitExpr r t [ arrs ] "lists:append($0)" |> Some
    | "Sum", [ arr ] -> emitExpr r t [ arr ] "lists:sum($0)" |> Some
    | "SumBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "sum_by", t, [ fn; arr ]) |> Some
    | "Min", [ arr ] -> emitExpr r t [ arr ] "lists:min($0)" |> Some
    | "Max", [ arr ] -> emitExpr r t [ arr ] "lists:max($0)" |> Some
    | "MinBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "min_by", t, [ fn; arr ]) |> Some
    | "MaxBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "max_by", t, [ fn; arr ]) |> Some
    | "Contains", [ value; arr ] -> emitExpr r t [ value; arr ] "lists:member($0, $1)" |> Some
    | "Find", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "find", t, [ fn; arr ]) |> Some
    | "TryFind", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "try_find", t, [ fn; arr ]) |> Some
    | "Choose", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "choose", t, [ fn; arr ]) |> Some
    | "Collect", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "collect", t, [ fn; arr ]) |> Some
    | "ToList", [ arr ] -> Some(List.head args) // arrays and lists are the same in Erlang
    | "OfList", [ lst ] -> Some(List.head args) // arrays and lists are the same in Erlang
    | "OfSeq", [ seq ] -> Some(List.head args)
    | "ToSeq", [ arr ] -> Some(List.head args)
    | "ZeroCreate", [ count ] -> emitExpr r t [ count ] "lists:duplicate($0, 0)" |> Some
    | "Create", [ count; value ] -> emitExpr r t [ count; value ] "lists:duplicate($0, $1)" |> Some
    | "Sort", [ arr ] -> emitExpr r t [ arr ] "lists:sort($0)" |> Some
    | "SortDescending", [ arr ] -> emitExpr r t [ arr ] "lists:reverse(lists:sort($0))" |> Some
    | "SortBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "sort_by", t, [ fn; arr ]) |> Some
    | "SortWith", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "sort_with", t, [ fn; arr ]) |> Some
    | "Zip", [ arr1; arr2 ] -> Helper.LibCall(com, "fable_list", "zip", t, [ arr1; arr2 ]) |> Some
    | "Zip3", [ a1; a2; a3 ] -> Helper.LibCall(com, "fable_list", "zip3", t, [ a1; a2; a3 ]) |> Some
    | "Unzip", [ arr ] -> emitExpr r t [ arr ] "lists:unzip($0)" |> Some
    | "Initialize", [ count; fn ] -> Helper.LibCall(com, "fable_list", "init", t, [ count; fn ]) |> Some
    | "Copy", [ arr ] -> emitExpr r t [ arr ] "lists:append($0, [])" |> Some
    | "Scan", [ fn; state; arr ] -> Helper.LibCall(com, "fable_list", "scan", t, [ fn; state; arr ]) |> Some
    | "ScanBack", [ fn; arr; state ] -> Helper.LibCall(com, "fable_list", "scan_back", t, [ fn; arr; state ]) |> Some
    | "ReduceBack", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "reduce_back", t, [ fn; arr ]) |> Some
    | "FindIndex", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "find_index", t, [ fn; arr ]) |> Some
    | "TryFindIndex", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "try_find_index", t, [ fn; arr ]) |> Some
    | "FindBack", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "find_back", t, [ fn; arr ]) |> Some
    | "FindIndexBack", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "find_index_back", t, [ fn; arr ]) |> Some
    | "TryFindBack", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "try_find_back", t, [ fn; arr ]) |> Some
    | "TryFindIndexBack", [ fn; arr ] ->
        Helper.LibCall(com, "fable_list", "try_find_index_back", t, [ fn; arr ]) |> Some
    | "Pick", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "pick", t, [ fn; arr ]) |> Some
    | "TryPick", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "try_pick", t, [ fn; arr ]) |> Some
    | "Partition", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:partition($0, $1)" |> Some
    | "Permute", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "permute", t, [ fn; arr ]) |> Some
    | "Map2", [ fn; a1; a2 ] -> Helper.LibCall(com, "fable_list", "map2", t, [ fn; a1; a2 ]) |> Some
    | "Map3", [ fn; a1; a2; a3 ] -> Helper.LibCall(com, "fable_list", "map3", t, [ fn; a1; a2; a3 ]) |> Some
    | "MapIndexed2", [ fn; a1; a2 ] -> Helper.LibCall(com, "fable_list", "mapi2", t, [ fn; a1; a2 ]) |> Some
    | "MapFold", [ fn; state; arr ] -> Helper.LibCall(com, "fable_list", "map_fold", t, [ fn; state; arr ]) |> Some
    | "MapFoldBack", [ fn; arr; state ] ->
        Helper.LibCall(com, "fable_list", "map_fold_back", t, [ fn; arr; state ])
        |> Some
    | "Distinct", [ arr ] -> Helper.LibCall(com, "fable_list", "distinct", t, [ arr ]) |> Some
    | "DistinctBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "distinct_by", t, [ fn; arr ]) |> Some
    | "Pairwise", [ arr ] -> Helper.LibCall(com, "fable_list", "pairwise", t, [ arr ]) |> Some
    | "GroupBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "group_by", t, [ fn; arr ]) |> Some
    | "CountBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "count_by", t, [ fn; arr ]) |> Some
    | "Windowed", [ size; arr ] -> Helper.LibCall(com, "fable_list", "windowed", t, [ size; arr ]) |> Some
    | "SplitInto", [ count; arr ] -> Helper.LibCall(com, "fable_list", "split_into", t, [ count; arr ]) |> Some
    | "Transpose", [ arrs ] -> Helper.LibCall(com, "fable_list", "transpose", t, [ arrs ]) |> Some
    | "Skip", [ count; arr ] -> emitExpr r t [ arr; count ] "lists:nthtail($1, $0)" |> Some
    | "SkipWhile", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:dropwhile($0, $1)" |> Some
    | "Take", [ count; arr ] -> emitExpr r t [ arr; count ] "lists:sublist($0, $1)" |> Some
    | "TakeWhile", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:takewhile($0, $1)" |> Some
    | "Truncate", [ count; arr ] -> emitExpr r t [ arr; count ] "lists:sublist($0, $1)" |> Some
    | "TryHead", [ arr ] -> Helper.LibCall(com, "fable_list", "try_head", t, [ arr ]) |> Some
    | "TryLast", [ arr ] -> Helper.LibCall(com, "fable_list", "try_last", t, [ arr ]) |> Some
    | "TryItem", [ idx; arr ] -> Helper.LibCall(com, "fable_list", "try_item", t, [ idx; arr ]) |> Some
    | "ExactlyOne", [ arr ] -> Helper.LibCall(com, "fable_list", "exactly_one", t, [ arr ]) |> Some
    | "TryExactlyOne", [ arr ] -> Helper.LibCall(com, "fable_list", "try_exactly_one", t, [ arr ]) |> Some
    | "Average", [ arr ] -> Helper.LibCall(com, "fable_list", "average", t, [ arr ]) |> Some
    | "AverageBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "average_by", t, [ fn; arr ]) |> Some
    | "IterateIndexed", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "iteri", t, [ fn; arr ]) |> Some
    | "Indexed", [ arr ] -> Helper.LibCall(com, "fable_list", "indexed", t, [ arr ]) |> Some
    | "CompareWith", [ fn; a1; a2 ] -> Helper.LibCall(com, "fable_list", "compare_with", t, [ fn; a1; a2 ]) |> Some
    | "UpdateAt", [ idx; value; arr ] -> Helper.LibCall(com, "fable_list", "update_at", t, [ idx; value; arr ]) |> Some
    | "InsertAt", [ idx; value; arr ] -> Helper.LibCall(com, "fable_list", "insert_at", t, [ idx; value; arr ]) |> Some
    | "InsertManyAt", [ idx; values; arr ] ->
        Helper.LibCall(com, "fable_list", "insert_many_at", t, [ idx; values; arr ])
        |> Some
    | "RemoveAt", [ idx; arr ] -> Helper.LibCall(com, "fable_list", "remove_at", t, [ idx; arr ]) |> Some
    | "RemoveManyAt", [ idx; count; arr ] ->
        Helper.LibCall(com, "fable_list", "remove_many_at", t, [ idx; count; arr ])
        |> Some
    | "Exists2", [ fn; a1; a2 ] -> Helper.LibCall(com, "fable_list", "exists2", t, [ fn; a1; a2 ]) |> Some
    | "ForAll2", [ fn; a1; a2 ] -> Helper.LibCall(com, "fable_list", "forall2", t, [ fn; a1; a2 ]) |> Some
    | "Fold2", [ fn; state; a1; a2 ] -> Helper.LibCall(com, "fable_list", "fold2", t, [ fn; state; a1; a2 ]) |> Some
    | "FoldBack2", [ fn; a1; a2; state ] ->
        Helper.LibCall(com, "fable_list", "fold_back2", t, [ fn; a1; a2; state ])
        |> Some
    | "Iterate2", [ fn; a1; a2 ] -> Helper.LibCall(com, "fable_list", "iter2", t, [ fn; a1; a2 ]) |> Some
    | "IterateIndexed2", [ fn; a1; a2 ] -> Helper.LibCall(com, "fable_list", "iteri2", t, [ fn; a1; a2 ]) |> Some
    | "SortByDescending", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "sort_by_descending", t, [ fn; arr ]) |> Some
    | ("Sub" | "GetSubArray"), [ arr; start; count ] ->
        emitExpr r t [ arr; start; count ] "lists:sublist($0, $1 + 1, $2)" |> Some
    | "Except", [ excludeArr; arr ] -> Helper.LibCall(com, "fable_list", "except", t, [ excludeArr; arr ]) |> Some
    | "ChunkBySize", [ size; arr ] -> Helper.LibCall(com, "fable_list", "chunk_by_size", t, [ size; arr ]) |> Some
    | "SplitAt", [ idx; arr ] -> Helper.LibCall(com, "fable_list", "split_at", t, [ idx; arr ]) |> Some
    | "AllPairs", [ a1; a2 ] -> Helper.LibCall(com, "fable_list", "all_pairs", t, [ a1; a2 ]) |> Some
    | "Unzip3", [ arr ] -> emitExpr r t [ arr ] "lists:unzip3($0)" |> Some
    // In-place sort operations: since arrays are lists in Erlang, return sorted copy
    | "SortInPlace", [ arr ] -> emitExpr r t [ arr ] "lists:sort($0)" |> Some
    | "SortInPlaceBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "sort_by", t, [ fn; arr ]) |> Some
    | "SortInPlaceWith", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "sort_with", t, [ fn; arr ]) |> Some
    | _ -> None

/// Beam-specific OperatorIntrinsics replacements (ranges).
/// F# range expressions like [1..n] compile to RangeInt32(start, step, stop).
/// Routes through Range.fs library for correctness with floats/decimals.
let private intrinsicFunctions
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "RangeChar", _ ->
        Helper.LibCall(com, "range", "range_char", t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    | ("RangeSByte" | "RangeByte" | "RangeInt16" | "RangeUInt16" | "RangeSingle" | "RangeDouble"), _ ->
        Helper.LibCall(com, "range", "range_double", t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    | "RangeInt32", _ ->
        Helper.LibCall(com, "range", "range_int32", t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    | "RangeUInt32", _ ->
        Helper.LibCall(com, "range", "range_u_int32", t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    | "RangeInt64", _ ->
        Helper.LibCall(com, "range", "range_int64", t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    | "RangeUInt64", _ ->
        Helper.LibCall(com, "range", "range_u_int64", t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    | "GetStringSlice", [ ar; lower; upper ] ->
        let lower =
            match lower with
            | Value(NewOption(Some lower, _, _), _) -> lower
            | _ -> makeIntConst 0

        match upper with
        | Value(NewOption(None, _, _), _) ->
            // s.[start..] → binary:part(s, start, byte_size(s) - start)
            emitExpr r t [ ar; lower ] "binary:part($0, $1, byte_size($0) - $1)" |> Some
        | _ ->
            let upper =
                match upper with
                | Value(NewOption(Some upper, _, _), _) -> upper
                | _ -> makeIntConst 0

            // s.[start..end] → binary:part(s, start, end - start + 1)  (F# slicing is inclusive)
            emitExpr r t [ ar; lower; upper ] "binary:part($0, $1, $2 - $1 + 1)" |> Some
    | "GetArraySlice", [ ar; lower; upper ] ->
        let lower =
            match lower with
            | Value(NewOption(Some lower, _, _), _) -> lower
            | _ -> makeIntConst 0

        match upper with
        | Value(NewOption(None, _, _), _) ->
            // arr.[start..] → lists:nthtail(start, arr)
            emitExpr r t [ ar; lower ] "lists:nthtail($1, $0)" |> Some
        | _ ->
            let upper =
                match upper with
                | Value(NewOption(Some upper, _, _), _) -> upper
                | _ -> makeIntConst 0

            // arr.[start..end] → lists:sublist(arr, start+1, end-start+1)  (1-based)
            emitExpr r t [ ar; lower; upper ] "lists:sublist($0, $1 + 1, $2 - $1 + 1)"
            |> Some
    | _ -> None

let error (_com: ICompiler) (msg: Expr) = msg

let defaultof (_com: ICompiler) (_ctx: Context) (r: SourceLocation option) (typ: Type) =
    match typ with
    | Boolean -> makeBoolConst false
    | Number(kind, uom) -> NumberConstant(NumberValue.GetZero kind, uom) |> makeValue None
    | Char -> CharConstant '\u0000' |> makeValue None
    | String -> Value(Null typ, r)
    | DeclaredType(ent, _) when ent.FullName = Types.guid -> makeStrConst "00000000-0000-0000-0000-000000000000"
    | _ -> Value(Null typ, r)

let getRefCell (_com: ICompiler) (r: SourceLocation option) (_typ: Type) (expr: Expr) =
    emitExpr r _typ [ expr ] "get($0)"

let setRefCell (_com: ICompiler) (r: SourceLocation option) (expr: Expr) (value: Expr) =
    emitExpr r Unit [ expr; value ] "put($0, $1)"

let makeRefCellFromValue (com: ICompiler) (r: SourceLocation option) (value: Expr) =
    Helper.LibCall(com, "fable_utils", "new_ref", Any, [ value ], ?loc = r)

let makeRefFromMutableValue (_com: ICompiler) (_ctx: Context) (r: SourceLocation option) (t: Type) (value: Expr) =
    // Wrap in UnaryAddressOf so Fable2Beam can detect out-parameter usage
    // and emit the atom key (process dict key) instead of the dereferenced value.
    Operation(Unary(UnaryAddressOf, value), Tags.empty, t, r)

let makeRefFromMutableFunc (_com: ICompiler) (_ctx: Context) (_r: SourceLocation option) (_t: Type) (value: Expr) =
    value

let makeRefFromMutableField
    (_com: ICompiler)
    (_ctx: Context)
    (r: SourceLocation option)
    (t: Type)
    (callee: Expr)
    (key: string)
    =
    Get(callee, FieldInfo.Create(key, isMutable = true), t, r)

let asyncBuilder (com: ICompiler) (_ctx: Context) r t (i: CallInfo) (_thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Singleton" -> Helper.LibCall(com, "fable_async_builder", "singleton", t, [], ?loc = r) |> Some
    | meth ->
        Helper.LibCall(com, "fable_async_builder", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some

let taskBuilder (com: ICompiler) (_ctx: Context) r t (i: CallInfo) (_thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Singleton" -> Helper.LibCall(com, "fable_async_builder", "singleton", t, [], ?loc = r) |> Some
    | "TaskBuilderBase.Bind"
    | "TaskBuilderBase.ReturnFrom" ->
        let meth = i.CompiledName.Replace("TaskBuilderBase.", "")

        Helper.LibCall(com, "fable_async_builder", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | meth ->
        Helper.LibCall(com, "fable_async_builder", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some

let tasks (com: ICompiler) (_ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg, i.CompiledName with
    | Some x, ("GetAwaiter" | "GetResult" | "get_Result" | "Result") ->
        // Task.Result / GetAwaiter().GetResult() → run_synchronously
        Helper.LibCall(com, "fable_async", "run_synchronously", t, [ x ], i.SignatureArgTypes, ?loc = r)
        |> Some
    | None, "FromResult" ->
        Helper.LibCall(com, "fable_async_builder", "return", t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | _ -> None

let asyncs (com: ICompiler) (_ctx: Context) r t (i: CallInfo) (_thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "Start" ->
        Helper.LibCall(com, "fable_async", "start_immediate", t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | "get_CancellationToken" ->
        Helper.LibCall(com, "fable_async", "cancellation_token", t, [], [], ?loc = r)
        |> Some
    | "Catch" ->
        Helper.LibCall(com, "fable_async", "catch_async", t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | meth ->
        Helper.LibCall(com, "fable_async", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some

let cancels (com: ICompiler) (_ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName with
    | "get_None"
    | ".ctor" ->
        Helper.LibCall(com, "fable_cancellation", "create", t, args, i.SignatureArgTypes, ?loc = r)
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
            "fable_cancellation",
            Naming.removeGetSetPrefix i.CompiledName |> Naming.lowerFirst,
            t,
            args,
            argTypes,
            ?loc = r
        )
        |> Some
    | "Dispose" -> Null Type.Unit |> makeValue r |> Some
    | "Register" ->
        let args, argTypes =
            match thisArg with
            | Some c -> c :: args, c.Type :: i.SignatureArgTypes
            | None -> args, i.SignatureArgTypes

        Helper.LibCall(com, "fable_cancellation", "register", t, args, argTypes, ?loc = r)
        |> Some
    | _ -> None

let mailbox (com: ICompiler) (_ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match thisArg with
    | None ->
        match i.CompiledName with
        | ".ctor" ->
            Helper.LibCall(com, "fable_mailbox", "default", t, args, i.SignatureArgTypes, ?loc = r)
            |> Some
        | "Start" ->
            Helper.LibCall(com, "fable_mailbox", "start", t, args, i.SignatureArgTypes, ?loc = r)
            |> Some
        | _ -> None
    | Some callee ->
        match i.CompiledName with
        | "Start" ->
            Helper.LibCall(com, "fable_mailbox", "start_instance", t, [ callee ], ?loc = r)
            |> Some
        | "Receive" ->
            Helper.LibCall(com, "fable_mailbox", "receive_msg", t, [ callee ], ?loc = r)
            |> Some
        | "Post" ->
            Helper.LibCall(com, "fable_mailbox", "post", t, callee :: args, i.SignatureArgTypes, ?loc = r)
            |> Some
        | "PostAndAsyncReply" ->
            Helper.LibCall(
                com,
                "fable_mailbox",
                "post_and_async_reply",
                t,
                callee :: args,
                i.SignatureArgTypes,
                ?loc = r
            )
            |> Some
        | "Reply" -> emitExpr r t (callee :: args) "(maps:get(reply, $0))($1)" |> Some
        | _ -> None

let private regex
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    // Constructor
    | ".ctor", _, [ pattern ] -> Helper.LibCall(com, "fable_regex", "create", t, [ pattern ], ?loc = r) |> Some
    | ".ctor", _, [ pattern; options ] ->
        Helper.LibCall(com, "fable_regex", "create", t, [ pattern; options ], ?loc = r)
        |> Some
    // Instance IsMatch
    | "IsMatch", Some callee, [ input ] ->
        Helper.LibCall(com, "fable_regex", "is_match", t, [ callee; input ], ?loc = r)
        |> Some
    | "IsMatch", Some callee, [ input; offset ] ->
        Helper.LibCall(com, "fable_regex", "is_match", t, [ callee; input; offset ], ?loc = r)
        |> Some
    // Static IsMatch
    | "IsMatch", None, [ input; pattern ] ->
        Helper.LibCall(com, "fable_regex", "is_match", t, [ input; pattern ], ?loc = r)
        |> Some
    | "IsMatch", None, [ input; pattern; options ] ->
        Helper.LibCall(com, "fable_regex", "is_match", t, [ input; pattern; options ], ?loc = r)
        |> Some
    // Instance Match
    | "Match", Some callee, [ input ] ->
        Helper.LibCall(com, "fable_regex", "match", t, [ callee; input ], ?loc = r)
        |> Some
    | "Match", Some callee, [ input; offset ] ->
        Helper.LibCall(com, "fable_regex", "match", t, [ callee; input; offset ], ?loc = r)
        |> Some
    // Static Match
    | "Match", None, [ input; pattern ] ->
        Helper.LibCall(com, "fable_regex", "match", t, [ input; pattern ], ?loc = r)
        |> Some
    | "Match", None, [ input; pattern; options ] ->
        Helper.LibCall(com, "fable_regex", "match", t, [ input; pattern; options ], ?loc = r)
        |> Some
    // Instance Matches
    | "Matches", Some callee, [ input ] ->
        Helper.LibCall(com, "fable_regex", "matches", t, [ callee; input ], ?loc = r)
        |> Some
    | "Matches", Some callee, [ input; offset ] ->
        Helper.LibCall(com, "fable_regex", "matches", t, [ callee; input; offset ], ?loc = r)
        |> Some
    // Static Matches
    | "Matches", None, [ input; pattern ] ->
        Helper.LibCall(com, "fable_regex", "matches", t, [ input; pattern ], ?loc = r)
        |> Some
    | "Matches", None, [ input; pattern; options ] ->
        Helper.LibCall(com, "fable_regex", "matches", t, [ input; pattern; options ], ?loc = r)
        |> Some
    // Instance Replace (string replacement)
    | "Replace", Some callee, [ input; replacement ] when replacement.Type = String ->
        Helper.LibCall(com, "fable_regex", "replace", t, [ callee; input; replacement ], ?loc = r)
        |> Some
    | "Replace", Some callee, [ input; replacement; count ] when replacement.Type = String ->
        Helper.LibCall(com, "fable_regex", "replace", t, [ callee; input; replacement; count ], ?loc = r)
        |> Some
    | "Replace", Some callee, [ input; replacement; count; offset ] when replacement.Type = String ->
        Helper.LibCall(com, "fable_regex", "replace", t, [ callee; input; replacement; count; offset ], ?loc = r)
        |> Some
    // Static Replace (string replacement)
    | "Replace", None, [ input; pattern; replacement ] when replacement.Type = String ->
        Helper.LibCall(com, "fable_regex", "replace", t, [ input; pattern; replacement ], ?loc = r)
        |> Some
    | "Replace", None, [ input; pattern; replacement; options ] when replacement.Type = String ->
        Helper.LibCall(com, "fable_regex", "replace", t, [ input; pattern; replacement; options ], ?loc = r)
        |> Some
    // Instance Replace (evaluator)
    | "Replace", Some callee, [ input; evaluator ] ->
        Helper.LibCall(com, "fable_regex", "replace_evaluator", t, [ callee; input; evaluator ], ?loc = r)
        |> Some
    | "Replace", Some callee, [ input; evaluator; count ] ->
        Helper.LibCall(com, "fable_regex", "replace_evaluator", t, [ callee; input; evaluator; count ], ?loc = r)
        |> Some
    | "Replace", Some callee, [ input; evaluator; count; offset ] ->
        Helper.LibCall(
            com,
            "fable_regex",
            "replace_evaluator",
            t,
            [ callee; input; evaluator; count; offset ],
            ?loc = r
        )
        |> Some
    // Static Replace (evaluator)
    | "Replace", None, [ input; pattern; evaluator ] ->
        Helper.LibCall(com, "fable_regex", "replace_evaluator", t, [ input; pattern; evaluator ], ?loc = r)
        |> Some
    // Instance Split
    | "Split", Some callee, [ input ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ callee; input ], ?loc = r)
        |> Some
    | "Split", Some callee, [ input; count ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ callee; input; count ], ?loc = r)
        |> Some
    | "Split", Some callee, [ input; count; offset ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ callee; input; count; offset ], ?loc = r)
        |> Some
    // Static Split
    | "Split", None, [ input; pattern ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ input; pattern ], ?loc = r)
        |> Some
    | "Split", None, [ input; pattern; options ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ input; pattern; options ], ?loc = r)
        |> Some
    // Escape / Unescape
    | "Escape", None, [ str ] -> Helper.LibCall(com, "fable_regex", "escape", t, [ str ], ?loc = r) |> Some
    | "Unescape", None, [ str ] -> Helper.LibCall(com, "fable_regex", "unescape", t, [ str ], ?loc = r) |> Some
    // Match/Group/Capture property accessors
    | "get_Value", Some callee, _ -> Helper.LibCall(com, "fable_regex", "get_value", t, [ callee ], ?loc = r) |> Some
    | "get_Index", Some callee, _ -> Helper.LibCall(com, "fable_regex", "get_index", t, [ callee ], ?loc = r) |> Some
    | "get_Length", Some callee, _ ->
        Helper.LibCall(com, "fable_regex", "get_length", t, [ callee ], ?loc = r)
        |> Some
    | "get_Success", Some callee, _ ->
        Helper.LibCall(com, "fable_regex", "get_success", t, [ callee ], ?loc = r)
        |> Some
    // Groups
    | "get_Groups", Some callee, _ ->
        Helper.LibCall(com, "fable_regex", "get_groups", t, [ callee ], ?loc = r)
        |> Some
    // Collection indexer (MatchCollection, GroupCollection)
    | "get_Item", Some callee, [ idx ] ->
        Helper.LibCall(com, "fable_regex", "get_item", t, [ callee; idx ], ?loc = r)
        |> Some
    // Count
    | "get_Count", Some callee, _ -> Helper.LibCall(com, "fable_regex", "get_count", t, [ callee ], ?loc = r) |> Some
    // Options
    | "get_Options", Some callee, _ ->
        Helper.LibCall(com, "fable_regex", "get_options", t, [ callee ], ?loc = r)
        |> Some
    // GetEnumerator for MatchCollection/GroupCollection iteration
    | "GetEnumerator", Some callee, _ ->
        Helper.LibCall(com, "fable_utils", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let private resizeArrays
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    // Constructors - use process dictionary (same as Ref cells)
    | ".ctor", _, [] ->
        let emptyList = Value(NewArray(ArrayValues [], Any, MutableArray), None)

        Helper.LibCall(com, "fable_utils", "new_ref", t, [ emptyList ], ?loc = r)
        |> Some
    | ".ctor", _, [ ExprType(Number _) ] ->
        // Ignore size hint, just create empty
        let emptyList = Value(NewArray(ArrayValues [], Any, MutableArray), None)

        Helper.LibCall(com, "fable_utils", "new_ref", t, [ emptyList ], ?loc = r)
        |> Some
    | ".ctor", _, [ arg ] ->
        // From IEnumerable/sequence
        Helper.LibCall(com, "fable_utils", "new_ref", t, [ arg ], ?loc = r) |> Some
    // get_Item: lists:nth is 1-based
    | "get_Item", Some callee, [ idx ] -> emitExpr r t [ callee; idx ] "lists:nth($1 + 1, get($0))" |> Some
    // set_Item
    | "set_Item", Some callee, [ idx; value ] ->
        Helper.LibCall(
            com,
            "fable_resize_array",
            "set_item",
            t,
            [ emitExpr r (List Any) [ callee ] "get($0)"; idx; value ],
            ?loc = r
        )
        |> fun newList -> emitExpr r Unit [ callee; newList ] "put($0, $1)" |> Some
    // get_Count
    | "get_Count", Some callee, _ -> emitExpr r t [ callee ] "erlang:length(erlang:get($0))" |> Some
    // Add
    | "Add", Some callee, [ arg ] -> emitExpr r Unit [ callee; arg ] "put($0, get($0) ++ [$1])" |> Some
    // AddRange
    | "AddRange", Some callee, [ arg ] -> emitExpr r Unit [ callee; arg ] "put($0, get($0) ++ $1)" |> Some
    // Contains
    | "Contains", Some callee, [ arg ] -> emitExpr r t [ callee; arg ] "lists:member($1, get($0))" |> Some
    // IndexOf
    | "IndexOf", Some callee, [ arg ] ->
        Helper.LibCall(
            com,
            "fable_resize_array",
            "index_of",
            t,
            [ emitExpr r (List Any) [ callee ] "get($0)"; arg ],
            ?loc = r
        )
        |> Some
    // Remove
    | "Remove", Some callee, [ arg ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let call =
            Helper.LibCall(com, "fable_resize_array", "remove", t, [ listExpr; arg ], ?loc = r)

        emitExpr r Boolean [ callee; call ] "(fun() -> {Result, NewList} = $1, put($0, NewList), Result end)()"
        |> Some
    // RemoveAll
    | "RemoveAll", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let call =
            Helper.LibCall(com, "fable_resize_array", "remove_all", t, [ listExpr; pred ], ?loc = r)

        emitExpr r t [ callee; call ] "(fun() -> {Count, NewList} = $1, put($0, NewList), Count end)()"
        |> Some
    // RemoveAt
    | "RemoveAt", Some callee, [ idx ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let call =
            Helper.LibCall(com, "fable_resize_array", "remove_at", t, [ listExpr; idx ], ?loc = r)

        emitExpr r Unit [ callee; call ] "put($0, $1)" |> Some
    // Insert
    | "Insert", Some callee, [ idx; arg ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let call =
            Helper.LibCall(com, "fable_resize_array", "insert", t, [ listExpr; idx; arg ], ?loc = r)

        emitExpr r Unit [ callee; call ] "put($0, $1)" |> Some
    // InsertRange
    | "InsertRange", Some callee, [ idx; items ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let call =
            Helper.LibCall(com, "fable_resize_array", "insert_range", t, [ listExpr; idx; items ], ?loc = r)

        emitExpr r Unit [ callee; call ] "put($0, $1)" |> Some
    // GetRange — returns a new ResizeArray (ref)
    | "GetRange", Some callee, [ idx; count ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let sublist =
            Helper.LibCall(com, "fable_resize_array", "get_range", t, [ listExpr; idx; count ], ?loc = r)

        Helper.LibCall(com, "fable_utils", "new_ref", t, [ sublist ], ?loc = r) |> Some
    // Clear
    | "Clear", Some callee, _ -> emitExpr r Unit [ callee ] "put($0, [])" |> Some
    // Reverse
    | "Reverse", Some callee, [] -> emitExpr r Unit [ callee ] "put($0, lists:reverse(get($0)))" |> Some
    // Sort (no args)
    | "Sort", Some callee, [] -> emitExpr r Unit [ callee ] "put($0, lists:sort(get($0)))" |> Some
    // Sort with comparison
    | "Sort", Some callee, [ comparer ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let call =
            Helper.LibCall(com, "fable_resize_array", "sort_with", t, [ listExpr; comparer ], ?loc = r)

        emitExpr r Unit [ callee; call ] "put($0, $1)" |> Some
    // ToArray
    | "ToArray", Some callee, [] -> emitExpr r t [ callee ] "get($0)" |> Some
    // Find — returns default value when not found (matching .NET List<T>.Find)
    | "Find", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"
        let defVal = defaultof com _ctx r t

        Helper.LibCall(com, "fable_resize_array", "find", t, [ pred; listExpr; defVal ], ?loc = r)
        |> Some
    // FindLast
    | "FindLast", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "find_last", t, [ listExpr; pred ], ?loc = r)
        |> Some
    // FindAll — returns a new ResizeArray (ref)
    | "FindAll", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let filtered =
            Helper.LibCall(com, "fable_resize_array", "find_all", t, [ listExpr; pred ], ?loc = r)

        Helper.LibCall(com, "fable_utils", "new_ref", t, [ filtered ], ?loc = r) |> Some
    // FindIndex
    | "FindIndex", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "find_index", t, [ listExpr; pred ], ?loc = r)
        |> Some
    // FindLastIndex
    | "FindLastIndex", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "find_last_index", t, [ listExpr; pred ], ?loc = r)
        |> Some
    // Exists
    | "Exists", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "exists", t, [ listExpr; pred ], ?loc = r)
        |> Some
    // ForEach
    | "ForEach", Some callee, [ fn ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "for_each", t, [ listExpr; fn ], ?loc = r)
        |> Some
    // ConvertAll
    | "ConvertAll", Some callee, [ fn ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "convert_all", t, [ listExpr; fn ], ?loc = r)
        |> Some
    // GetEnumerator - create a proper enumerator for iteration
    | "GetEnumerator", Some callee, _ ->
        Helper.LibCall(com, "fable_utils", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let private keyValuePairs
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg with
    | ".ctor", _ -> makeTuple r true args |> Some
    | "get_Key", Some c -> Get(c, TupleIndex 0, t, r) |> Some
    | "get_Value", Some c -> Get(c, TupleIndex 1, t, r) |> Some
    | _ -> None

let private tuples
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (i: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    let isStruct = i.DeclaringEntityFullName.StartsWith("System.ValueTuple")

    match i.CompiledName, thisArg with
    | (".ctor" | "Create"), _ -> Value(NewTuple(args, isStruct), r) |> Some
    | "get_Item1", Some x -> Get(x, TupleIndex 0, t, r) |> Some
    | "get_Item2", Some x -> Get(x, TupleIndex 1, t, r) |> Some
    | "get_Item3", Some x -> Get(x, TupleIndex 2, t, r) |> Some
    | "get_Item4", Some x -> Get(x, TupleIndex 3, t, r) |> Some
    | "get_Item5", Some x -> Get(x, TupleIndex 4, t, r) |> Some
    | "get_Item6", Some x -> Get(x, TupleIndex 5, t, r) |> Some
    | "get_Item7", Some x -> Get(x, TupleIndex 6, t, r) |> Some
    | "get_Rest", Some x -> Get(x, TupleIndex 7, t, r) |> Some
    | _ -> None

let private dictionaries
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    // Constructors
    | ".ctor", _, [] -> Helper.LibCall(com, "fable_dictionary", "create_empty", t, [], ?loc = r) |> Some
    | ".ctor", _, [ ExprType(Number _) ] ->
        // Ignore capacity hint
        Helper.LibCall(com, "fable_dictionary", "create_empty", t, [], ?loc = r) |> Some
    | ".ctor", _, [ arg ] ->
        // From IDictionary or IEnumerable<KeyValuePair>
        Helper.LibCall(com, "fable_dictionary", "create_from_list", t, [ arg ], ?loc = r)
        |> Some
    | ".ctor", _, [ _arg; _eqComp ] ->
        // With IEqualityComparer: ignore comparer for now, Erlang =:= does structural comparison
        match info.SignatureArgTypes with
        | [ Number _; _ ] ->
            // (capacity, comparer)
            Helper.LibCall(com, "fable_dictionary", "create_empty", t, [], ?loc = r) |> Some
        | _ ->
            Helper.LibCall(com, "fable_dictionary", "create_from_list", t, [ _arg ], ?loc = r)
            |> Some
    // get_Count
    | "get_Count", Some callee, _ ->
        Helper.LibCall(com, "fable_dictionary", "get_count", t, [ callee ], ?loc = r)
        |> Some
    // get_IsReadOnly
    | "get_IsReadOnly", _, _ -> makeBoolConst false |> Some
    // Add
    | "Add", Some callee, [ key; value ] ->
        Helper.LibCall(com, "fable_dictionary", "add", t, [ callee; key; value ], ?loc = r)
        |> Some
    // get_Item (indexer get)
    | "get_Item", Some callee, [ key ] ->
        Helper.LibCall(com, "fable_dictionary", "get_item", t, [ callee; key ], ?loc = r)
        |> Some
    // set_Item (indexer set)
    | "set_Item", Some callee, [ key; value ] ->
        Helper.LibCall(com, "fable_dictionary", "set_item", t, [ callee; key; value ], ?loc = r)
        |> Some
    // ContainsKey
    | "ContainsKey", Some callee, [ key ] ->
        Helper.LibCall(com, "fable_dictionary", "contains_key", t, [ callee; key ], ?loc = r)
        |> Some
    // ContainsValue
    | "ContainsValue", Some callee, [ value ] ->
        Helper.LibCall(com, "fable_dictionary", "contains_value", t, [ callee; value ], ?loc = r)
        |> Some
    // Remove
    | "Remove", Some callee, [ key ] ->
        Helper.LibCall(com, "fable_dictionary", "remove", t, [ callee; key ], ?loc = r)
        |> Some
    // Clear
    | "Clear", Some callee, _ ->
        Helper.LibCall(com, "fable_dictionary", "clear", t, [ callee ], ?loc = r)
        |> Some
    // TryGetValue: F# desugars `let ok, v = dic.TryGetValue(k)` into
    // TryGetValue(key, &outRef) → bool, where outRef is a mutable variable.
    // The outRef expression may be an AddressOf (passes atom key via our fix)
    // or a regular get() expression. We pass it through to try_get_value/3.
    | "TryGetValue", Some callee, [ key; outRef ] ->
        // Check if outRef is AddressOf - if so, it will generate the atom key
        // Otherwise generate inline code that ignores the out-ref
        match outRef with
        | Operation(Unary(UnaryAddressOf, _), _, _, _) ->
            Helper.LibCall(com, "fable_dictionary", "try_get_value", t, [ callee; key; outRef ], ?loc = r)
            |> Some
        | _ ->
            // outRef is a value (get(atom)), not an addressOf expression.
            // We need the atom key, but it's not available.
            // Fall back to inline emitExpr that returns {Bool, Value} as a tuple,
            // then extract just the first element (bool) since the F# desugaring
            // wraps this in {result, get(out_arg)}
            emitExpr
                r
                t
                [ callee; key ]
                "(fun() -> case maps:find($1, get($0)) of {ok, _V_} -> _V_; error -> 0 end end)()"
            |> Some
    | "TryGetValue", Some callee, [ key ] ->
        Helper.LibCall(com, "fable_dictionary", "try_get_value", t, [ callee; key ], ?loc = r)
        |> Some
    // get_Keys
    | "get_Keys", Some callee, _ ->
        Helper.LibCall(com, "fable_dictionary", "get_keys", t, [ callee ], ?loc = r)
        |> Some
    // get_Values
    | "get_Values", Some callee, _ ->
        Helper.LibCall(com, "fable_dictionary", "get_values", t, [ callee ], ?loc = r)
        |> Some
    // GetEnumerator for Dictionary iteration
    | "GetEnumerator", Some callee, _ ->
        Helper.LibCall(com, "fable_dictionary", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let private hashSets
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    // Constructors
    | ".ctor", _, [] -> Helper.LibCall(com, "fable_hashset", "create_empty", t, [], ?loc = r) |> Some
    | ".ctor", _, [ ExprType(Number _) ] ->
        // Ignore capacity hint
        Helper.LibCall(com, "fable_hashset", "create_empty", t, [], ?loc = r) |> Some
    | ".ctor", _, [ arg ] ->
        // From IEnumerable
        Helper.LibCall(com, "fable_hashset", "create_from_list", t, [ arg ], ?loc = r)
        |> Some
    | ".ctor", _, [ arg; _eqComp ] ->
        // With IEqualityComparer: ignore comparer, Erlang =:= does structural comparison
        match info.SignatureArgTypes with
        | [ Number _; _ ] ->
            // (capacity, comparer)
            Helper.LibCall(com, "fable_hashset", "create_empty", t, [], ?loc = r) |> Some
        | _ ->
            Helper.LibCall(com, "fable_hashset", "create_from_list", t, [ arg ], ?loc = r)
            |> Some
    // get_Count
    | "get_Count", Some callee, _ ->
        Helper.LibCall(com, "fable_hashset", "get_count", t, [ callee ], ?loc = r)
        |> Some
    // get_IsReadOnly
    | "get_IsReadOnly", _, _ -> makeBoolConst false |> Some
    // Add: returns bool (true if newly added)
    | "Add", Some callee, [ item ] ->
        Helper.LibCall(com, "fable_hashset", "add", t, [ callee; item ], ?loc = r)
        |> Some
    // Remove
    | "Remove", Some callee, [ item ] ->
        Helper.LibCall(com, "fable_hashset", "remove", t, [ callee; item ], ?loc = r)
        |> Some
    // Contains
    | "Contains", Some callee, [ item ] ->
        Helper.LibCall(com, "fable_hashset", "contains", t, [ callee; item ], ?loc = r)
        |> Some
    // Clear
    | "Clear", Some callee, _ -> Helper.LibCall(com, "fable_hashset", "clear", t, [ callee ], ?loc = r) |> Some
    // UnionWith
    | "UnionWith", Some callee, [ other ] ->
        Helper.LibCall(com, "fable_hashset", "union_with", t, [ callee; other ], ?loc = r)
        |> Some
    // IntersectWith
    | "IntersectWith", Some callee, [ other ] ->
        Helper.LibCall(com, "fable_hashset", "intersect_with", t, [ callee; other ], ?loc = r)
        |> Some
    // ExceptWith
    | "ExceptWith", Some callee, [ other ] ->
        Helper.LibCall(com, "fable_hashset", "except_with", t, [ callee; other ], ?loc = r)
        |> Some
    // IsSubsetOf / IsSupersetOf / IsProperSubsetOf / IsProperSupersetOf
    | "IsSubsetOf", Some callee, [ other ] ->
        Helper.LibCall(com, "fable_hashset", "is_subset_of", t, [ callee; other ], ?loc = r)
        |> Some
    | "IsSupersetOf", Some callee, [ other ] ->
        Helper.LibCall(com, "fable_hashset", "is_superset_of", t, [ callee; other ], ?loc = r)
        |> Some
    | "IsProperSubsetOf", Some callee, [ other ] ->
        Helper.LibCall(com, "fable_hashset", "is_proper_subset_of", t, [ callee; other ], ?loc = r)
        |> Some
    | "IsProperSupersetOf", Some callee, [ other ] ->
        Helper.LibCall(com, "fable_hashset", "is_proper_superset_of", t, [ callee; other ], ?loc = r)
        |> Some
    // GetEnumerator
    | "GetEnumerator", Some callee, _ ->
        Helper.LibCall(com, "fable_hashset", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let private queues
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    | ".ctor", _, [] -> Helper.LibCall(com, "fable_queue", "create_empty", t, [], ?loc = r) |> Some
    | ".ctor", _, [ ExprType(Number _) ] ->
        // Ignore capacity hint
        Helper.LibCall(com, "fable_queue", "create_empty", t, [], ?loc = r) |> Some
    | ".ctor", _, [ arg ] ->
        // From IEnumerable
        Helper.LibCall(com, "fable_queue", "create_from_list", t, [ arg ], ?loc = r)
        |> Some
    | "get_Count", Some callee, _ -> Helper.LibCall(com, "fable_queue", "get_count", t, [ callee ], ?loc = r) |> Some
    | "Enqueue", Some callee, [ item ] ->
        Helper.LibCall(com, "fable_queue", "enqueue", t, [ callee; item ], ?loc = r)
        |> Some
    | "Dequeue", Some callee, _ -> Helper.LibCall(com, "fable_queue", "dequeue", t, [ callee ], ?loc = r) |> Some
    | "TryDequeue", Some callee, [ outRef ] ->
        match outRef with
        | Operation(Unary(UnaryAddressOf, _), _, _, _) ->
            Helper.LibCall(com, "fable_queue", "try_dequeue", t, [ callee; outRef ], ?loc = r)
            |> Some
        | _ ->
            Helper.LibCall(com, "fable_queue", "try_dequeue", t, [ callee ], ?loc = r)
            |> Some
    | "TryDequeue", Some callee, _ ->
        Helper.LibCall(com, "fable_queue", "try_dequeue", t, [ callee ], ?loc = r)
        |> Some
    | "Peek", Some callee, _ -> Helper.LibCall(com, "fable_queue", "peek", t, [ callee ], ?loc = r) |> Some
    | "TryPeek", Some callee, [ outRef ] ->
        match outRef with
        | Operation(Unary(UnaryAddressOf, _), _, _, _) ->
            Helper.LibCall(com, "fable_queue", "try_peek", t, [ callee; outRef ], ?loc = r)
            |> Some
        | _ -> Helper.LibCall(com, "fable_queue", "try_peek", t, [ callee ], ?loc = r) |> Some
    | "TryPeek", Some callee, _ -> Helper.LibCall(com, "fable_queue", "try_peek", t, [ callee ], ?loc = r) |> Some
    | "Contains", Some callee, [ item ] ->
        Helper.LibCall(com, "fable_queue", "contains", t, [ callee; item ], ?loc = r)
        |> Some
    | "Clear", Some callee, _ -> Helper.LibCall(com, "fable_queue", "clear", t, [ callee ], ?loc = r) |> Some
    | "ToArray", Some callee, _ -> Helper.LibCall(com, "fable_queue", "to_array", t, [ callee ], ?loc = r) |> Some
    | "TrimExcess", Some callee, _ ->
        Helper.LibCall(com, "fable_queue", "trim_excess", t, [ callee ], ?loc = r)
        |> Some
    | "GetEnumerator", Some callee, _ ->
        Helper.LibCall(com, "fable_queue", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let private stacks
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    | ".ctor", _, [] -> Helper.LibCall(com, "fable_stack", "create_empty", t, [], ?loc = r) |> Some
    | ".ctor", _, [ ExprType(Number _) ] ->
        // Ignore capacity hint
        Helper.LibCall(com, "fable_stack", "create_empty", t, [], ?loc = r) |> Some
    | ".ctor", _, [ arg ] ->
        // From IEnumerable
        Helper.LibCall(com, "fable_stack", "create_from_list", t, [ arg ], ?loc = r)
        |> Some
    | "get_Count", Some callee, _ -> Helper.LibCall(com, "fable_stack", "get_count", t, [ callee ], ?loc = r) |> Some
    | "Push", Some callee, [ item ] ->
        Helper.LibCall(com, "fable_stack", "push", t, [ callee; item ], ?loc = r)
        |> Some
    | "Pop", Some callee, _ -> Helper.LibCall(com, "fable_stack", "pop", t, [ callee ], ?loc = r) |> Some
    | "TryPop", Some callee, [ outRef ] ->
        match outRef with
        | Operation(Unary(UnaryAddressOf, _), _, _, _) ->
            Helper.LibCall(com, "fable_stack", "try_pop", t, [ callee; outRef ], ?loc = r)
            |> Some
        | _ -> Helper.LibCall(com, "fable_stack", "try_pop", t, [ callee ], ?loc = r) |> Some
    | "TryPop", Some callee, _ -> Helper.LibCall(com, "fable_stack", "try_pop", t, [ callee ], ?loc = r) |> Some
    | "Peek", Some callee, _ -> Helper.LibCall(com, "fable_stack", "peek", t, [ callee ], ?loc = r) |> Some
    | "TryPeek", Some callee, [ outRef ] ->
        match outRef with
        | Operation(Unary(UnaryAddressOf, _), _, _, _) ->
            Helper.LibCall(com, "fable_stack", "try_peek", t, [ callee; outRef ], ?loc = r)
            |> Some
        | _ -> Helper.LibCall(com, "fable_stack", "try_peek", t, [ callee ], ?loc = r) |> Some
    | "TryPeek", Some callee, _ -> Helper.LibCall(com, "fable_stack", "try_peek", t, [ callee ], ?loc = r) |> Some
    | "Contains", Some callee, [ item ] ->
        Helper.LibCall(com, "fable_stack", "contains", t, [ callee; item ], ?loc = r)
        |> Some
    | "Clear", Some callee, _ -> Helper.LibCall(com, "fable_stack", "clear", t, [ callee ], ?loc = r) |> Some
    | "ToArray", Some callee, _ -> Helper.LibCall(com, "fable_stack", "to_array", t, [ callee ], ?loc = r) |> Some
    | "GetEnumerator", Some callee, _ ->
        Helper.LibCall(com, "fable_stack", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let private collections
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg with
    | "get_Count", Some callee ->
        Helper.LibCall(com, "fable_dictionary", "get_count", t, [ callee ], ?loc = r)
        |> Some
    | "get_IsReadOnly", _ -> makeBoolConst false |> Some
    | "Add", Some callee ->
        match args with
        | [ key; value ] ->
            Helper.LibCall(com, "fable_dictionary", "add", t, [ callee; key; value ], ?loc = r)
            |> Some
        | _ -> None
    | "Remove", Some callee ->
        match args with
        | [ key ] ->
            Helper.LibCall(com, "fable_dictionary", "remove", t, [ callee; key ], ?loc = r)
            |> Some
        | _ -> None
    | "Clear", Some callee ->
        Helper.LibCall(com, "fable_dictionary", "clear", t, [ callee ], ?loc = r)
        |> Some
    | "Contains", Some callee ->
        match args with
        | [ key ] ->
            Helper.LibCall(com, "fable_dictionary", "contains_key", t, [ callee; key ], ?loc = r)
            |> Some
        | _ -> None
    | "GetEnumerator", Some callee ->
        Helper.LibCall(com, "fable_dictionary", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let private enumerables
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (_args: Expr list)
    =
    match info.CompiledName, thisArg with
    | "GetEnumerator", Some callee ->
        Helper.LibCall(com, "fable_utils", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let private enumerators
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (_args: Expr list)
    =
    match info.CompiledName, thisArg with
    | "MoveNext", Some callee -> Helper.LibCall(com, "fable_utils", "move_next", t, [ callee ], ?loc = r) |> Some
    | ("get_Current" | "System.Collections.Generic.IEnumerator`1.get_Current" | "System.Collections.IEnumerator.get_Current"),
      Some callee ->
        Helper.LibCall(com, "fable_utils", "get_current", t, [ callee ], ?loc = r)
        |> Some
    | _ -> None

let tryField (com: ICompiler) returnTyp ownerTyp fieldName : Expr option =
    match ownerTyp, fieldName with
    | Number(Decimal, _), "Zero" -> makeIntConst 0 |> Some
    | Number(Decimal, _), "One" ->
        Helper.LibCall(com, "fable_decimal", "get_one", returnTyp, [], ?loc = None)
        |> Some
    | Number(Decimal, _), "MinusOne" ->
        Helper.LibCall(com, "fable_decimal", "get_minus_one", returnTyp, [], ?loc = None)
        |> Some
    | Number(Decimal, _), _ -> None
    | String, "Empty" -> makeStrConst "" |> Some
    | DeclaredType(ent, _), fieldName ->
        match ent.FullName, fieldName with
        | "System.Diagnostics.Stopwatch", "Frequency" ->
            // 1_000_000 microseconds per second
            Value(NumberConstant(NumberValue.Int64 1_000_000L, NumberInfo.Empty), None)
            |> Some
        | "System.BitConverter", "IsLittleEndian" ->
            // Match .NET behavior: IsLittleEndian = true (our runtime uses little-endian)
            Value(BoolConstant true, None) |> Some
        | "System.Guid", "Empty" -> makeStrConst "00000000-0000-0000-0000-000000000000" |> Some
        | Types.timespan, "Zero" -> makeIntConst 0 |> Some
        | Types.datetime, "MaxValue" ->
            Helper.LibCall(com, "fable_date", "max_value", returnTyp, [], ?loc = None)
            |> Some
        | Types.datetime, "MinValue" ->
            Helper.LibCall(com, "fable_date", "min_value", returnTyp, [], ?loc = None)
            |> Some
        | _ -> None
    | _ -> None

/// Beam-specific System.BitConverter replacements.
/// BEAM uses big-endian by convention for binary construction/extraction.
let private bitConvert
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    // GetBytes: convert a numeric value to a byte list (big-endian)
    | "GetBytes", [ arg ] ->
        let bitSize =
            match arg.Type with
            | Number(Int16, _)
            | Number(UInt16, _) -> 16
            | Number(Int32, _)
            | Number(UInt32, _)
            | Number(Float32, _) -> 32
            | Number(Int64, _)
            | Number(UInt64, _)
            | Number(Float64, _) -> 64
            | _ -> 32

        Helper.LibCall(com, "fable_bit_converter", "get_bytes", t, [ arg; makeIntConst bitSize ], ?loc = r)
        |> Some
    // ToInt16, ToUInt16, ToInt32, ToUInt32, ToInt64, ToUInt64, ToSingle, ToDouble
    | "ToInt16", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_int", t, [ bytes; startIndex; makeIntConst 16 ], ?loc = r)
        |> Some
    | ("ToUInt16"), [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_uint", t, [ bytes; startIndex; makeIntConst 16 ], ?loc = r)
        |> Some
    | "ToInt32", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_int", t, [ bytes; startIndex; makeIntConst 32 ], ?loc = r)
        |> Some
    | "ToUInt32", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_uint", t, [ bytes; startIndex; makeIntConst 32 ], ?loc = r)
        |> Some
    | "ToInt64", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_int", t, [ bytes; startIndex; makeIntConst 64 ], ?loc = r)
        |> Some
    | "ToUInt64", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_uint", t, [ bytes; startIndex; makeIntConst 64 ], ?loc = r)
        |> Some
    | "ToSingle", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_float", t, [ bytes; startIndex; makeIntConst 32 ], ?loc = r)
        |> Some
    | "ToDouble", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_float", t, [ bytes; startIndex; makeIntConst 64 ], ?loc = r)
        |> Some
    | _ -> None

/// Beam-specific System.Text.Encoding replacements.
/// In Erlang, strings are binaries (UTF-8 by default), so encoding is mostly identity.
let private encoding
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    // Encoding.UTF8 / Encoding.Unicode — return an atom tag (not used, but needed as callee)
    | ("get_UTF8" | "get_Unicode"), _, _ -> emitExpr r t [] "utf8" |> Some
    // GetBytes(string) → string is already a binary in Erlang
    | "GetBytes", Some _callee, [ arg ] -> Some arg
    // GetBytes(string, index, count) → binary:part
    | "GetBytes", Some _callee, [ str; idx; count ] ->
        emitExpr r t [ str; idx; count ] "binary:part($0, $1, $2)" |> Some
    // GetString(bytes) → bytes is already a string/binary in Erlang
    | "GetString", Some _callee, [ arg ] -> Some arg
    // GetString(bytes, index, count) → binary:part
    | "GetString", Some _callee, [ bytes; idx; count ] ->
        emitExpr r t [ bytes; idx; count ] "binary:part($0, $1, $2)" |> Some
    | _ -> None

/// Beam-specific System.Diagnostics.Stopwatch replacements.
let private stopwatch
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (_args: Expr list)
    =
    match info.CompiledName with
    // Stopwatch.Frequency → 1_000_000 (microseconds per second)
    | "get_Frequency" -> makeIntConst 1_000_000 |> Some
    // Stopwatch.GetTimestamp() → erlang:monotonic_time(microsecond)
    | "GetTimestamp" -> emitExpr r t [] "erlang:monotonic_time(microsecond)" |> Some
    | _ -> None

/// Beam-specific Nullable<T> replacements.
/// Nullable is erased in Erlang — Nullable(x) → x, Nullable<T>() → undefined.
let private nullables
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg with
    // Nullable(value) → value
    | ".ctor", None -> List.tryHead args
    // .Value → identity (value is already there)
    | "get_Value", Some c -> Some c
    // .HasValue → check if not undefined
    | "get_HasValue", Some c -> Test(c, OptionTest true, r) |> Some
    | _ -> None

/// Beam-specific System.Guid replacements.
/// GUIDs are represented as binary strings in D format (with dashes, lowercase).
let private guids
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName with
    | ".ctor" ->
        match args with
        | [] -> makeStrConst "00000000-0000-0000-0000-000000000000" |> Some
        | [ StringConst literalGuid ] ->
            try
                System.Guid.Parse(literalGuid) |> string<System.Guid> |> makeStrConst |> Some
            with _ ->
                None
        | [ arg ] when arg.Type = Type.String ->
            Helper.LibCall(com, "fable_guid", "parse", t, [ arg ], ?loc = r) |> Some
        | [ arg ] ->
            // byte array constructor
            Helper.LibCall(com, "fable_guid", "from_bytes", t, [ arg ], ?loc = r) |> Some
        | _ -> None
    | "NewGuid" -> Helper.LibCall(com, "fable_guid", "new_guid", t, [], ?loc = r) |> Some
    | "Parse" ->
        match args with
        | [ StringConst literalGuid ] ->
            try
                System.Guid.Parse(literalGuid) |> string<System.Guid> |> makeStrConst |> Some
            with _ ->
                Helper.LibCall(com, "fable_guid", "parse", t, args, ?loc = r) |> Some
        | _ -> Helper.LibCall(com, "fable_guid", "parse", t, args, ?loc = r) |> Some
    | "TryParse" -> None // TODO: needs out-param support
    | "ToByteArray" ->
        match thisArg with
        | Some c -> Helper.LibCall(com, "fable_guid", "to_byte_array", t, [ c ], ?loc = r) |> Some
        | None -> None
    | "ToString" when args.Length = 0 -> thisArg
    | "ToString" when args.Length = 1 ->
        match thisArg with
        | Some c ->
            Helper.LibCall(com, "fable_guid", "to_string_format", t, [ c; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "CompareTo" ->
        match thisArg with
        | Some c -> compare com r c args.Head |> Some
        | None -> None
    | "Equals" ->
        match thisArg with
        | Some c -> equals r true c args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some c -> emitExpr r t [ c ] "erlang:phash2($0)" |> Some
        | None -> None
    | _ -> None

/// Beam-specific System.Random replacements.
/// Uses Erlang's rand module for random number generation.
let private randoms
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg with
    | ".ctor", _ -> emitExpr r t [] "ok" |> Some // Random is stateless in Erlang
    | "Next", Some _ ->
        match args with
        | [] -> emitExpr r t [] "rand:uniform(2147483647) - 1" |> Some
        | [ maxVal ] -> emitExpr r t [ maxVal ] "rand:uniform($0) - 1" |> Some
        | [ minVal; maxVal ] -> emitExpr r t [ minVal; maxVal ] "$0 + rand:uniform($1 - $0) - 1" |> Some
        | _ -> None
    | "NextDouble", Some _ -> emitExpr r t [] "rand:uniform()" |> Some
    | "NextBytes", Some _ ->
        match args with
        | [ arr ] ->
            emitExpr r t [ arr ] "binary_to_list(crypto:strong_rand_bytes(length(get($0))))"
            |> Some
        | _ -> None
    | _ -> None

/// Beam-specific System.DateTime replacements.
/// DateTime is represented as a 2-tuple {Ticks, Kind}.
let private dates
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName with
    | ".ctor" ->
        match args with
        | [] -> Helper.LibCall(com, "fable_date", "min_value", t, [], ?loc = r) |> Some
        | ExprType(Number(Int64, _)) :: _ -> Helper.LibCall(com, "fable_date", "from_ticks", t, args, ?loc = r) |> Some
        | _ ->
            let last = List.last args

            match args.Length, last.Type with
            // DateTime(y,m,d,h,min,s, DateTimeKind) → create(y,m,d,h,min,s,0,kind)
            | 7, Number(_, NumberInfo.IsEnum ent) when ent.FullName = "System.DateTimeKind" ->
                let args = (List.take 6 args) @ [ makeIntConst 0; last ]
                Helper.LibCall(com, "fable_date", "create", t, args, ?loc = r) |> Some
            // DateTime(y,m,d,h,min,s,ms, DateTimeKind) → create(y,m,d,h,min,s,ms,0,kind)
            | 8, Number(_, NumberInfo.IsEnum ent) when ent.FullName = "System.DateTimeKind" ->
                let args = (List.take 7 args) @ [ makeIntConst 0; last ]
                Helper.LibCall(com, "fable_date", "create", t, args, ?loc = r) |> Some
            // DateTime(y,m,d,h,min,s,ms,mc, DateTimeKind) → create(y,m,d,h,min,s,ms,mc,kind)
            | 9, Number(_, NumberInfo.IsEnum ent) when ent.FullName = "System.DateTimeKind" ->
                Helper.LibCall(com, "fable_date", "create", t, args, ?loc = r) |> Some
            // DateTime(y,m,d,h,min,s,ms,mc) → create(y,m,d,h,min,s,ms,mc,0)
            | 8, _ ->
                let args = args @ [ makeIntConst 0 ]
                Helper.LibCall(com, "fable_date", "create", t, args, ?loc = r) |> Some
            | _ -> Helper.LibCall(com, "fable_date", "create", t, args, ?loc = r) |> Some
    | "ToString" ->
        match thisArg with
        | Some callee ->
            match args with
            | [] -> Helper.LibCall(com, "fable_date", "to_string", t, [ callee ], ?loc = r) |> Some
            | [ fmt ] ->
                Helper.LibCall(com, "fable_date", "to_string", t, [ callee; fmt ], ?loc = r)
                |> Some
            | [ fmt; culture ] ->
                Helper.LibCall(com, "fable_date", "to_string", t, [ callee; fmt; culture ], ?loc = r)
                |> Some
            | _ -> None
        | None -> None
    | "get_Kind" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "kind", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Ticks" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "ticks", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Date" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "date", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Year" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "year", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Month" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "month", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Day" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "day", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Hour" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "hour", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Minute" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "minute", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Second" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date", "second", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Millisecond" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "millisecond", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Microsecond" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "microsecond", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_DayOfWeek" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "day_of_week", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_DayOfYear" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "day_of_year", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Now" -> Helper.LibCall(com, "fable_date", "now", t, [], ?loc = r) |> Some
    | "get_UtcNow" -> Helper.LibCall(com, "fable_date", "utc_now", t, [], ?loc = r) |> Some
    | "get_Today" -> Helper.LibCall(com, "fable_date", "today", t, [], ?loc = r) |> Some
    | "get_MaxValue" -> Helper.LibCall(com, "fable_date", "max_value", t, [], ?loc = r) |> Some
    | "get_MinValue" -> Helper.LibCall(com, "fable_date", "min_value", t, [], ?loc = r) |> Some
    | "IsLeapYear" -> Helper.LibCall(com, "fable_date", "is_leap_year", t, args, ?loc = r) |> Some
    | "DaysInMonth" -> Helper.LibCall(com, "fable_date", "days_in_month", t, args, ?loc = r) |> Some
    | "SpecifyKind" -> Helper.LibCall(com, "fable_date", "specify_kind", t, args, ?loc = r) |> Some
    | "ToLocalTime" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "to_local_time", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "ToUniversalTime" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "to_universal_time", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "ToShortDateString" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "to_short_date_string", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "ToLongDateString" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "to_long_date_string", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "ToShortTimeString" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "to_short_time_string", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "ToLongTimeString" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "to_long_time_string", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "Add" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "add", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "Subtract" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "subtract", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "AddYears" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "add_years", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "AddMonths" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "add_months", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "AddDays" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "add_days", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "AddHours" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "add_hours", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "AddMinutes" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "add_minutes", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "AddSeconds" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "add_seconds", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "AddMilliseconds" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date", "add_milliseconds", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "op_Addition" -> Helper.LibCall(com, "fable_date", "op_addition", t, args, ?loc = r) |> Some
    | "op_Subtraction" -> Helper.LibCall(com, "fable_date", "op_subtraction", t, args, ?loc = r) |> Some
    | "Parse" ->
        let args = args |> List.take 1
        Helper.LibCall(com, "fable_date", "parse", t, args, ?loc = r) |> Some
    | "TryParse" -> Helper.LibCall(com, "fable_date", "try_parse", t, args, ?loc = r) |> Some
    | "CompareTo" ->
        match thisArg with
        | Some callee -> compare com r callee args.Head |> Some
        | None -> None
    | "Equals" ->
        match thisArg with
        | Some callee -> equals r true callee args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some callee -> emitExpr r t [ callee ] "erlang:phash2($0)" |> Some
        | None -> None
    | _ -> None

/// Beam-specific System.DateTimeOffset replacements.
/// DateTimeOffset is represented as a 3-tuple {Ticks, Kind, OffsetTicks}.
let private dateTimeOffsets
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName with
    | ".ctor" ->
        match args with
        | [] -> Helper.LibCall(com, "fable_date_offset", "min_value", t, [], ?loc = r) |> Some
        | ExprType(Number(Int64, _)) :: _ ->
            Helper.LibCall(com, "fable_date_offset", "from_ticks", t, args, ?loc = r)
            |> Some
        | ExprType(DeclaredType(e, [])) :: _ when e.FullName = Types.datetime ->
            Helper.LibCall(com, "fable_date_offset", "from_date", t, args, ?loc = r) |> Some
        | _ -> Helper.LibCall(com, "fable_date_offset", "create", t, args, ?loc = r) |> Some
    | "ToString" ->
        match thisArg with
        | Some callee ->
            match args with
            | [] ->
                Helper.LibCall(com, "fable_date_offset", "to_string", t, [ callee ], ?loc = r)
                |> Some
            | [ fmt ] ->
                Helper.LibCall(com, "fable_date_offset", "to_string", t, [ callee; fmt ], ?loc = r)
                |> Some
            | [ fmt; culture ] ->
                Helper.LibCall(com, "fable_date_offset", "to_string", t, [ callee; fmt; culture ], ?loc = r)
                |> Some
            | _ -> None
        | None -> None
    | "get_Offset" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "offset", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_DateTime" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "date_time", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_LocalDateTime" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "to_local_time", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_UtcDateTime" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "to_universal_time", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Year" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "year", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Month" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "month", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Day" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_date_offset", "day", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Hour" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "hour", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Minute" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "minute", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Second" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "second", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Millisecond" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "millisecond", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Ticks" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_date_offset", "ticks", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Now" -> Helper.LibCall(com, "fable_date_offset", "now", t, [], ?loc = r) |> Some
    | "get_UtcNow" -> Helper.LibCall(com, "fable_date_offset", "utc_now", t, [], ?loc = r) |> Some
    | "get_MinValue" -> Helper.LibCall(com, "fable_date_offset", "min_value", t, [], ?loc = r) |> Some
    | "TryParse" -> Helper.LibCall(com, "fable_date_offset", "try_parse", t, args, ?loc = r) |> Some
    | "Equals" ->
        match thisArg with
        | Some callee -> equals r true callee args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some callee -> emitExpr r t [ callee ] "erlang:phash2($0)" |> Some
        | None -> None
    | _ -> None

/// Beam-specific System.Uri replacements.
/// Uri is represented as an Erlang map with parsed components.
let private uris
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName with
    | ".ctor" -> Helper.LibCall(com, "fable_uri", "create", t, args, ?loc = r) |> Some
    | "TryCreate" -> Helper.LibCall(com, "fable_uri", "try_create", t, args, ?loc = r) |> Some
    | "ToString" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_uri", "to_string", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_IsAbsoluteUri" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_uri", "is_absolute_uri", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Scheme" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_uri", "scheme", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Host" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_uri", "host", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_AbsolutePath" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_uri", "absolute_path", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_AbsoluteUri" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_uri", "absolute_uri", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_PathAndQuery" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_uri", "path_and_query", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Query" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_uri", "query", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Fragment" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_uri", "fragment", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_OriginalString" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_uri", "original_string", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Port" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_uri", "port", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "Equals" ->
        match thisArg with
        | Some callee -> equals r true callee args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some callee -> emitExpr r t [ callee ] "erlang:phash2($0)" |> Some
        | None -> None
    | _ -> None

/// Beam-specific System.TimeSpan replacements.
/// TimeSpan is represented as an integer of ticks (100-nanosecond units).
let private timeSpans
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName with
    | ".ctor" -> Helper.LibCall(com, "fable_timespan", "create", t, args, ?loc = r) |> Some
    | "ToString" ->
        match thisArg with
        | Some callee ->
            match args with
            | [] ->
                Helper.LibCall(com, "fable_timespan", "to_string", t, [ callee ], ?loc = r)
                |> Some
            | [ fmt; culture ] ->
                Helper.LibCall(com, "fable_timespan", "to_string", t, [ callee; fmt; culture ], ?loc = r)
                |> Some
            | _ -> None
        | None -> None
    | "get_Zero" -> makeIntConst 0 |> Some
    | "FromTicks" -> Helper.LibCall(com, "fable_timespan", "from_ticks", t, args, ?loc = r) |> Some
    | "FromDays" -> Helper.LibCall(com, "fable_timespan", "from_days", t, args, ?loc = r) |> Some
    | "FromHours" -> Helper.LibCall(com, "fable_timespan", "from_hours", t, args, ?loc = r) |> Some
    | "FromMinutes" -> Helper.LibCall(com, "fable_timespan", "from_minutes", t, args, ?loc = r) |> Some
    | "FromSeconds" -> Helper.LibCall(com, "fable_timespan", "from_seconds", t, args, ?loc = r) |> Some
    | "FromMilliseconds" ->
        Helper.LibCall(com, "fable_timespan", "from_milliseconds", t, args, ?loc = r)
        |> Some
    | "FromMicroseconds" ->
        Helper.LibCall(com, "fable_timespan", "from_microseconds", t, args, ?loc = r)
        |> Some
    | "Parse" -> Helper.LibCall(com, "fable_timespan", "parse", t, args, ?loc = r) |> Some
    | "TryParse" -> Helper.LibCall(com, "fable_timespan", "try_parse", t, args, ?loc = r) |> Some
    | "Add" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "add", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "Subtract" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "subtract", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "Negate" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_timespan", "negate", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "Duration" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "duration", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "Multiply" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "multiply", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "Divide" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "divide", t, [ callee; args.Head ], ?loc = r)
            |> Some
        | None -> None
    | "CompareTo" ->
        match thisArg with
        | Some callee -> compare com r callee args.Head |> Some
        | None -> None
    | "Equals" ->
        match thisArg with
        | Some callee -> equals r true callee args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some callee -> emitExpr r t [ callee ] "erlang:phash2($0)" |> Some
        | None -> None
    // Instance properties: get_Days, get_Hours, get_Minutes, get_Seconds, get_Milliseconds, etc.
    | "get_Days" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_timespan", "days", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Hours" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_timespan", "hours", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_Minutes" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "minutes", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Seconds" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "seconds", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Milliseconds" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "milliseconds", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Microseconds" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "microseconds", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_Ticks" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_timespan", "ticks", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "get_TotalDays" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "total_days", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_TotalHours" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "total_hours", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_TotalMinutes" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "total_minutes", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_TotalSeconds" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "total_seconds", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_TotalMilliseconds" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "total_milliseconds", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    | "get_TotalMicroseconds" ->
        match thisArg with
        | Some callee ->
            Helper.LibCall(com, "fable_timespan", "total_microseconds", t, [ callee ], ?loc = r)
            |> Some
        | None -> None
    // Static operators: TimeSpan + TimeSpan, TimeSpan - TimeSpan
    | "op_Addition" ->
        match args with
        | [ left; right ] -> makeBinOp r t left right BinaryPlus |> Some
        | _ -> None
    | "op_Subtraction" ->
        match args with
        | [ left; right ] -> makeBinOp r t left right BinaryMinus |> Some
        | _ -> None
    | "op_Equality" ->
        match args with
        | [ left; right ] -> equals r true left right |> Some
        | _ -> None
    | "op_Inequality" ->
        match args with
        | [ left; right ] -> equals r false left right |> Some
        | _ -> None
    | "op_LessThan" ->
        match args with
        | [ left; right ] -> makeBinOp r Boolean left right BinaryLess |> Some
        | _ -> None
    | "op_LessThanOrEqual" ->
        match args with
        | [ left; right ] -> makeBinOp r Boolean left right BinaryLessOrEqual |> Some
        | _ -> None
    | "op_GreaterThan" ->
        match args with
        | [ left; right ] -> makeBinOp r Boolean left right BinaryGreater |> Some
        | _ -> None
    | "op_GreaterThanOrEqual" ->
        match args with
        | [ left; right ] -> makeBinOp r Boolean left right BinaryGreaterOrEqual |> Some
        | _ -> None
    | "op_UnaryNegation" ->
        match args with
        | [ operand ] ->
            Helper.LibCall(com, "fable_timespan", "negate", t, [ operand ], ?loc = r)
            |> Some
        | _ -> None
    | _ -> None

let private getMangledNames (i: CallInfo) (thisArg: Expr option) =
    let isStatic = Option.isNone thisArg
    let pos = i.DeclaringEntityFullName.LastIndexOf('.')

    let moduleName =
        i.DeclaringEntityFullName.Substring(0, pos).Replace("Microsoft.", "")

    let entityName =
        i.DeclaringEntityFullName.Substring(pos + 1)
        |> FSharp2Fable.Helpers.cleanNameAsJsIdentifier

    let memberName = i.CompiledName |> FSharp2Fable.Helpers.cleanNameAsJsIdentifier

    let mangledName =
        Naming.buildNameWithoutSanitationFrom entityName isStatic memberName i.OverloadSuffix

    moduleName, mangledName

let private bclType (com: ICompiler) (_ctx: Context) r t (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    let moduleName, mangledName = getMangledNames i thisArg

    let args =
        match thisArg with
        | Some callee -> callee :: args
        | _ -> args

    Helper.LibCall(com, moduleName, mangledName, t, args, i.SignatureArgTypes, genArgs = i.GenericArgs, ?loc = r)
    |> Some

let tryType (_t: Type) : Expr option = None

let tryCall
    (com: ICompiler)
    (ctx: Context)
    (r: SourceLocation option)
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    : Expr option
    =
    match info.DeclaringEntityFullName with
    | "Microsoft.FSharp.Core.Operators"
    | "Microsoft.FSharp.Core.Operators.Checked"
    | "Microsoft.FSharp.Core.ExtraTopLevelOperators"
    | "System.Math"
    | "System.MathF" -> operators com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives"
    | "Microsoft.FSharp.Core.LanguagePrimitives.HashCompare" -> languagePrimitives com ctx r t info thisArg args
    | Types.string -> strings com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.StringModule" -> stringModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.FSharpOption`1"
    | "Microsoft.FSharp.Core.FSharpValueOption`1" -> options com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.OptionModule"
    | "Microsoft.FSharp.Core.ValueOptionModule" -> optionModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.ResultModule" -> resultModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.FSharpList`1" -> lists com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.ListModule" -> listModule com ctx r t info thisArg args
    | "System.Array" -> arrays com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.ArrayModule"
    | "Microsoft.FSharp.Collections.ArrayModule.Parallel" -> arrayModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.FSharpMap`2" -> maps com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.MapModule" -> mapModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.FSharpSet`1" -> sets com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.SetModule" -> setModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.SeqModule" -> seqModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers" -> seqModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.Operators.OperatorIntrinsics" -> intrinsicFunctions com ctx r t info thisArg args
    | "System.Object" ->
        match objects com ctx r t info thisArg args with
        | Some _ as res -> res
        | None -> conversions com ctx r t info thisArg args
    | "System.ValueType" ->
        match valueTypes com ctx r t info thisArg args with
        | Some _ as res -> res
        | None -> conversions com ctx r t info thisArg args
    | "System.Convert" -> convert com ctx r t info thisArg args
    | "System.Boolean" ->
        match info.CompiledName, thisArg, args with
        | "Parse", None, [ arg ] ->
            Helper.LibCall(com, "fable_convert", "boolean_parse", t, [ arg ], ?loc = r)
            |> Some
        | "TryParse", None, _ ->
            Helper.LibCall(com, "fable_convert", "boolean_try_parse", t, args, ?loc = r)
            |> Some
        | "Equals", Some thisObj, [ arg ] -> equals r true thisObj arg |> Some
        | "CompareTo", Some thisObj, [ arg ] -> compare com r thisObj arg |> Some
        | "GetHashCode", Some thisObj, [] -> emitExpr r t [ thisObj ] "erlang:phash2($0)" |> Some
        | _ -> None
    | "System.Char" ->
        match chars com ctx r t info thisArg args with
        | Some _ as res -> res
        | None ->
            match info.CompiledName, thisArg, args with
            | "Equals", Some thisObj, [ arg ] -> equals r true thisObj arg |> Some
            | "CompareTo", Some thisObj, [ arg ] -> compare com r thisObj arg |> Some
            | "GetHashCode", Some thisObj, [] -> emitExpr r t [ thisObj ] "erlang:phash2($0)" |> Some
            | _ -> conversions com ctx r t info thisArg args
    | "System.SByte"
    | "System.Byte"
    | "System.Int16"
    | "System.UInt16"
    | "System.Int32"
    | "System.UInt32"
    | "System.Int64"
    | "System.UInt64"
    | "System.Single"
    | "System.Double"
    | "System.Decimal" -> numericTypes com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicFunctions" ->
        match info.CompiledName, args with
        | "GetArray", [ ar; idx ] ->
            match ar.Type with
            | Type.Array(Type.Number(UInt8, _), _) -> emitExpr r t [ ar; idx ] "binary:at($0, $1)" |> Some
            | _ -> emitExpr r t [ ar; idx ] "lists:nth($1 + 1, $0)" |> Some
        | "GetString", [ ar; idx ] -> emitExpr r t [ ar; idx ] "binary:at($0, $1)" |> Some
        | "SetArray", [ ar; idx; value ] -> setExpr r ar idx value |> Some
        | ("UnboxFast" | "UnboxGeneric" | "CheckThis"), [ arg ] -> TypeCast(arg, t) |> Some
        | "MakeDecimal", [ low; mid; high; isNegative; scale ] ->
            Helper.LibCall(com, "fable_decimal", "from_parts", t, [ low; mid; high; isNegative; scale ], ?loc = r)
            |> Some
        | _ -> None
    | "System.Numerics.BigInteger"
    | "Microsoft.FSharp.Core.NumericLiterals.NumericLiteralI" ->
        // Erlang has native arbitrary-precision integers, so BigInt ops map directly
        match info.CompiledName, thisArg, args with
        | ".ctor", None, [ arg ] -> Some arg // bigint(x) = x in Erlang
        | "op_Addition", None, [ left; right ] -> makeBinOp r t left right BinaryPlus |> Some
        | "op_Subtraction", None, [ left; right ] -> makeBinOp r t left right BinaryMinus |> Some
        | "op_Multiply", None, [ left; right ] -> makeBinOp r t left right BinaryMultiply |> Some
        | "op_Division", None, [ left; right ] -> makeBinOp r t left right BinaryDivide |> Some
        | "op_Modulus", None, [ left; right ] -> makeBinOp r t left right BinaryModulus |> Some
        | "op_UnaryNegation", None, [ operand ] -> Operation(Unary(UnaryMinus, operand), Tags.empty, t, r) |> Some
        | "op_BitwiseAnd", None, [ left; right ] -> emitExpr r t [ left; right ] "($0 band $1)" |> Some
        | "op_BitwiseOr", None, [ left; right ] -> emitExpr r t [ left; right ] "($0 bor $1)" |> Some
        | "op_ExclusiveOr", None, [ left; right ] -> emitExpr r t [ left; right ] "($0 bxor $1)" |> Some
        | "op_LeftShift", None, [ value; shift ] -> emitExpr r t [ value; shift ] "($0 bsl $1)" |> Some
        | "op_RightShift", None, [ value; shift ] -> emitExpr r t [ value; shift ] "($0 bsr $1)" |> Some
        | "op_LessThan", None, [ left; right ] -> makeBinOp r Boolean left right BinaryLess |> Some
        | "op_LessThanOrEqual", None, [ left; right ] -> makeBinOp r Boolean left right BinaryLessOrEqual |> Some
        | "op_GreaterThan", None, [ left; right ] -> makeBinOp r Boolean left right BinaryGreater |> Some
        | "op_GreaterThanOrEqual", None, [ left; right ] -> makeBinOp r Boolean left right BinaryGreaterOrEqual |> Some
        | "op_Equality", None, [ left; right ] -> equals r true left right |> Some
        | "op_Inequality", None, [ left; right ] -> equals r false left right |> Some
        | ("get_Sign" | "Sign"), Some thisObj, _ ->
            emitExpr r t [ thisObj ] "case $0 > 0 of true -> 1; false -> case $0 < 0 of true -> -1; false -> 0 end end"
            |> Some
        | "Abs", None, [ arg ] -> emitExpr r t [ arg ] "erlang:abs($0)" |> Some
        | ("get_IsZero" | "IsZero"), Some thisObj, _ -> emitExpr r t [ thisObj ] "($0 =:= 0)" |> Some
        | ("get_IsOne" | "IsOne"), Some thisObj, _ -> emitExpr r t [ thisObj ] "($0 =:= 1)" |> Some
        | ("ToInt64" | "ToInt64Unchecked" | "ToInt32" | "ToInt16" | "ToByte" | "ToSByte" | "op_Explicit"), None, [ arg ] ->
            Some arg // identity in Erlang
        | "Parse", None, [ str ] -> emitExpr r t [ str ] "binary_to_integer($0)" |> Some
        | "Pow", None, [ base_; exp_ ] -> emitExpr r t [ base_; exp_ ] "math:pow($0, $1)" |> Some
        | "get_Zero", _, _ -> Value(NumberConstant(NumberValue.Int32 0, NumberInfo.Empty), r) |> Some
        | "get_One", _, _ -> Value(NumberConstant(NumberValue.Int32 1, NumberInfo.Empty), r) |> Some
        | "CompareTo", Some thisObj, [ arg ] -> compare com r thisObj arg |> Some
        | "Equals", Some thisObj, [ arg ] -> equals r true thisObj arg |> Some
        | "GetHashCode", Some thisObj, [] -> emitExpr r t [ thisObj ] "erlang:phash2($0)" |> Some
        // NumericLiteralI special values
        | "FromZero", None, _ -> Value(NumberConstant(NumberValue.Int32 0, NumberInfo.Empty), r) |> Some
        | "FromOne", None, _ -> Value(NumberConstant(NumberValue.Int32 1, NumberInfo.Empty), r) |> Some
        // Large BigInt literals: FromString("12345...") → binary_to_integer
        | "FromString", None, [ arg ] -> emitExpr r t [ arg ] "binary_to_integer($0)" |> Some
        // FromInt32/FromInt64/etc: identity (Erlang native integers)
        | name, None, [ arg ] when name.StartsWith("From", System.StringComparison.Ordinal) -> Some arg
        // ToInt/ToDouble/etc: identity
        | name, None, [ arg ] when name.StartsWith("To", System.StringComparison.Ordinal) -> Some arg
        | name, Some c, _ when name.StartsWith("To", System.StringComparison.Ordinal) -> Some c
        | _ -> None
    | "Microsoft.FSharp.Core.FSharpRef`1" ->
        // Ref cell .Value get/set: use process dictionary
        match info.CompiledName, thisArg, args with
        | "get_Value", Some callee, _ -> getRefCell com r t callee |> Some
        | "set_Value", Some callee, [ value ] -> setRefCell com r callee value |> Some
        | _ -> None
    | "Microsoft.FSharp.Control.FSharpAsyncBuilder"
    | "Microsoft.FSharp.Control.AsyncActivation`1" -> asyncBuilder com ctx r t info thisArg args
    | "Microsoft.FSharp.Control.FSharpAsync"
    | "Microsoft.FSharp.Control.AsyncPrimitives" -> asyncs com ctx r t info thisArg args
    | "Microsoft.FSharp.Control.TaskBuilderBase"
    | "Microsoft.FSharp.Control.TaskBuilderModule"
    | "Microsoft.FSharp.Control.TaskBuilderExtensions.HighPriority"
    | "Microsoft.FSharp.Control.TaskBuilderExtensions.LowPriority" -> taskBuilder com ctx r t info thisArg args
    | "Microsoft.FSharp.Control.TaskBuilder" -> taskBuilder com ctx r t info thisArg args
    | "System.Threading.Tasks.Task"
    | "System.Threading.Tasks.Task`1"
    | "System.Threading.Tasks.TaskCompletionSource`1"
    | "System.Runtime.CompilerServices.TaskAwaiter`1" -> tasks com ctx r t info thisArg args
    | Types.printfModule
    | Naming.StartsWith Types.printfFormat _ -> fsFormat com ctx r t info thisArg args
    | "System.Threading.CancellationToken"
    | "System.Threading.CancellationTokenSource" -> cancels com ctx r t info thisArg args
    | "Microsoft.FSharp.Control.FSharpMailboxProcessor`1"
    | "Microsoft.FSharp.Control.FSharpAsyncReplyChannel`1" -> mailbox com ctx r t info thisArg args
    | Types.regex
    | Types.regexCapture
    | Types.regexMatch
    | Types.regexGroup
    | Types.regexMatchCollection
    | Types.regexGroupCollection -> regex com ctx r t info thisArg args
    | Types.resizeArray -> resizeArrays com ctx r t info thisArg args
    | Types.dictionary
    | Types.idictionary -> dictionaries com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.Operators.Unchecked" -> unchecked com ctx r t info thisArg args
    | Types.hashset -> hashSets com ctx r t info thisArg args
    | Types.queue -> queues com ctx r t info thisArg args
    | Types.stack -> stacks com ctx r t info thisArg args
    | Types.keyValuePair -> keyValuePairs com ctx r t info thisArg args
    | Naming.StartsWith "System.Tuple" _
    | Naming.StartsWith "System.ValueTuple" _ -> tuples com ctx r t info thisArg args
    | Types.icollectionGeneric
    | Types.icollection
    | Types.ireadonlycollection
    | "System.Collections.Generic.Dictionary`2.KeyCollection"
    | "System.Collections.Generic.Dictionary`2.ValueCollection" -> collections com ctx r t info thisArg args
    | Types.ienumerableGeneric
    | Types.ienumerable -> enumerables com ctx r t info thisArg args
    | Types.ienumeratorGeneric
    | Types.ienumerator
    | "System.Collections.Generic.List`1.Enumerator"
    | "System.Collections.Generic.Dictionary`2.Enumerator"
    | "System.Collections.Generic.Dictionary`2.KeyCollection.Enumerator"
    | "System.Collections.Generic.Dictionary`2.ValueCollection.Enumerator"
    | "System.Collections.Generic.HashSet`1.Enumerator"
    | "System.Collections.Generic.Queue`1.Enumerator"
    | "System.CharEnumerator" -> enumerators com ctx r t info thisArg args
    | "System.BitConverter" -> bitConvert com ctx r t info thisArg args
    | "System.Text.Encoding"
    | "System.Text.UnicodeEncoding"
    | "System.Text.UTF8Encoding" -> encoding com ctx r t info thisArg args
    | "System.Diagnostics.Stopwatch" -> stopwatch com ctx r t info thisArg args
    | Types.nullable -> nullables com ctx r t info thisArg args
    | Types.guid -> guids com ctx r t info thisArg args
    | "System.Random" -> randoms com ctx r t info thisArg args
    | "System.Environment" ->
        match info.CompiledName with
        | "get_NewLine" -> makeStrConst "\n" |> Some
        | _ -> None
    | Types.datetime -> dates com ctx r t info thisArg args
    | "System.Uri" -> uris com ctx r t info thisArg args
    | Types.datetimeOffset -> dateTimeOffsets com ctx r t info thisArg args
    | Types.timespan -> timeSpans com ctx r t info thisArg args
    | "System.Globalization.CultureInfo" ->
        match info.CompiledName with
        | "get_InvariantCulture" -> emitExpr r t [] "undefined" |> Some
        | _ -> None
    | "Fable.Core.BeamInterop" ->
        match info.CompiledName, args with
        | Naming.StartsWith "import" suffix, _ ->
            match suffix, args with
            | "Member", [ RequireStringConst com ctx r path ] ->
                makeImportUserGenerated r t Naming.placeholder path |> Some
            | "All", [ RequireStringConst com ctx r path ] -> makeImportUserGenerated r t "*" path |> Some
            | _, [ RequireStringConst com ctx r selector; RequireStringConst com ctx r path ] ->
                makeImportUserGenerated r t selector path |> Some
            | _ -> None
        | Naming.StartsWith "emitErl" rest, [ args; macro ] ->
            match macro with
            | RequireStringConstOrTemplate com ctx r template ->
                let args = destructureTupleArgs [ args ]
                let isStatement = rest = "Statement"
                emitTemplate r t args isStatement template |> Some
        | "op_BangBang", [ arg ] -> Some arg
        | _ -> None
    // Testing assertions (used by our test framework)
    | "Fable.Core.Testing.Assert" ->
        match info.CompiledName, args with
        | "AreEqual", [ expected; actual ] -> equals r true expected actual |> Some
        | "NotEqual", [ expected; actual ] -> equals r false expected actual |> Some
        | _ -> None
    // IDisposable
    | "System.IDisposable" ->
        match info.CompiledName, thisArg with
        | "Dispose", Some c -> Helper.LibCall(com, "fable_utils", "safe_dispose", t, [ c ], ?loc = r) |> Some
        | _ -> None
    // Exception — caught exceptions are wrapped as #{message => Msg} by Fable2Beam
    | "System.Exception" ->
        match info.CompiledName, thisArg, args with
        | ".ctor", None, [ msg ] -> emitExpr r t [ msg ] "#{message => $0}" |> Some
        | ".ctor", None, [] ->
            emitExpr r t [] "#{message => <<\"Exception of type 'System.Exception' was thrown.\">>}"
            |> Some
        | "get_Message", Some c, _ -> emitExpr r t [ c ] "maps:get(message, $0, $0)" |> Some
        | _ -> None
    // Built-in .NET exception types — all become #{message => Msg} maps in Erlang
    | BuiltinSystemException _
    | "System.Collections.Generic.KeyNotFoundException" ->
        match info.CompiledName, thisArg, args with
        | ".ctor", None, [ msg ] -> emitExpr r t [ msg ] "#{message => $0}" |> Some
        | ".ctor", None, [] ->
            let typeName = info.DeclaringEntityFullName
            let msg = $"Exception of type '{typeName}' was thrown."
            emitExpr r t [] $"#{{message => <<\"{msg}\">>}}" |> Some
        | "get_Message", Some c, _ -> emitExpr r t [ c ] "maps:get(message, $0, $0)" |> Some
        | _ -> None
    // System.Type (reflection) — type info is a map #{fullname => ..., generics => [...]}
    | "System.Type" ->
        match info.CompiledName, thisArg with
        | "get_FullName", Some c -> Helper.LibCall(com, "fable_reflection", "full_name", t, [ c ], ?loc = r) |> Some
        | "get_Namespace", Some c -> Helper.LibCall(com, "fable_reflection", "namespace", t, [ c ], ?loc = r) |> Some
        | "get_IsGenericType", Some c ->
            Helper.LibCall(com, "fable_reflection", "is_generic_type", t, [ c ], ?loc = r)
            |> Some
        | "get_IsArray", Some c -> Helper.LibCall(com, "fable_reflection", "is_array", t, [ c ], ?loc = r) |> Some
        | _ -> None
    // System.Enum
    | "System.Enum" ->
        match info.CompiledName, thisArg, args with
        | "HasFlag", Some value, [ flag ] -> emitExpr r t [ value; flag ] "(($0 band $1) =:= $1)" |> Some
        | _ -> None
    // Lazy — lazy values use process dict for memoization
    | "System.Lazy`1"
    | "Microsoft.FSharp.Control.Lazy"
    | "Microsoft.FSharp.Control.LazyExtensions" ->
        match info.CompiledName, thisArg, args with
        | ("Force" | "get_Value"), Some callee, _ ->
            Helper.LibCall(com, "fable_utils", "force_lazy", t, [ callee ], ?loc = r)
            |> Some
        | "get_IsValueCreated", Some callee, _ ->
            Helper.LibCall(com, "fable_utils", "is_value_created", t, [ callee ], ?loc = r)
            |> Some
        | (".ctor" | "Create"), None, [ factory ] ->
            Helper.LibCall(com, "fable_utils", "new_lazy", t, [ factory ], ?loc = r) |> Some
        | "CreateFromValue", None, [ value ] ->
            Helper.LibCall(com, "fable_utils", "new_lazy_from_value", t, [ value ], ?loc = r)
            |> Some
        | _ -> None
    // Action/Func/Delegate — functions in Erlang
    | "System.Delegate"
    | Naming.StartsWith "System.Action" _
    | Naming.StartsWith "System.Func" _
    | Naming.StartsWith "Microsoft.FSharp.Core.FSharpFunc" _
    | Naming.StartsWith "Microsoft.FSharp.Core.OptimizedClosures.FSharpFunc" _ ->
        match info.CompiledName, thisArg with
        | "Invoke", Some callee -> CurriedApply(callee, args, t, r) |> Some
        | _ -> None
    // F# Reflection — minimal support
    | "Microsoft.FSharp.Reflection.FSharpType" ->
        match info.CompiledName, args with
        | "MakeTupleType", [ typesArr ] ->
            // Return a type info map for a tuple
            Helper.LibCall(com, "fable_reflection", "make_tuple_type", t, [ typesArr ], ?loc = r)
            |> Some
        | _ -> None
    | "Microsoft.FSharp.Reflection.FSharpValue" ->
        match info.CompiledName, args with
        | "MakeTuple", [ values; _ ] ->
            // MakeTuple(values, tupleType) — just return values as a tuple (list in Erlang)
            emitExpr r t [ values ] "list_to_tuple($0)" |> Some
        | _ -> None
    | "System.Text.StringBuilder" -> bclType com ctx r t info thisArg args
    | _ -> None

let tryBaseConstructor
    (_com: ICompiler)
    (_ctx: Context)
    (_ent: EntityRef)
    (_argTypes: Lazy<Type list>)
    (_genArgs: Type list)
    (_args: Expr list)
    : Expr option
    =
    None
