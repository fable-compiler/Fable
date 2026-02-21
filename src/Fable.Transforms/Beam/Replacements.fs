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

// Use fable_comparison:equals/2 for structural equality.
// This handles ref-wrapped arrays (process dict refs) by dereferencing
// before comparison, so structural equality works correctly.
let private equals (com: ICompiler) r equal (left: Expr) (right: Expr) =
    let eqCall =
        Helper.LibCall(com, "fable_comparison", "equals", Boolean, [ left; right ], ?loc = r)

    if equal then
        eqCall
    else
        Operation(Unary(UnaryNot, eqCall), Tags.empty, Boolean, r)

let private compare (com: ICompiler) r (left: Expr) (right: Expr) =
    Helper.LibCall(com, "fable_comparison", "compare", Number(Int32, NumberInfo.Empty), [ left; right ], ?loc = r)

/// Deref an array ref to its underlying list (for passing to list BIFs).
/// Byte arrays (UInt8) are atomics — convert to list via fable_utils:byte_array_to_list.
let private derefArr r (expr: Expr) =
    match expr.Type with
    | Array(Type.Number(UInt8, _), _) -> emitExpr r (List Any) [ expr ] "fable_utils:byte_array_to_list($0)"
    | Array _ -> emitExpr r (List Any) [ expr ] "erlang:get($0)"
    | _ -> expr

/// Wrap a plain list result as an array ref.
/// Byte arrays become atomics via fable_utils:new_byte_array. Non-array types pass through.
let private wrapArr (com: ICompiler) r (t: Type) (expr: Expr) =
    match t with
    | Array(Type.Number(UInt8, _), _) -> Helper.LibCall(com, "fable_utils", "new_byte_array", t, [ expr ], ?loc = r)
    | Array _ -> Helper.LibCall(com, "fable_utils", "new_ref", t, [ expr ], ?loc = r)
    | _ -> expr

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
    | "Hash", [ arg ] -> Helper.LibCall(com, "fable_comparison", "hash", _t, [ arg ], ?loc = r) |> Some
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
    | (Operators.equality | "Eq"), [ left; right ] -> equals com r true left right |> Some
    | (Operators.inequality | "Neq"), [ left; right ] -> equals com r false left right |> Some
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
        | Type.Char -> emitExpr r _t [ arg ] "<<($0)/utf8>>" |> Some
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
            | Number(BigInt, _) -> "range", "range_big_int", addStep args
            | _ -> "range", "range_double", addStep args

        Helper.LibCall(com, modul, meth, _t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    // Erlang has native arbitrary-precision integers, so Int64/UInt64/BigInt
    // use direct binary ops instead of library calls (like Python's int)
    // Bitwise operators — Erlang has native bitwise support for all integer sizes
    | Operators.booleanOr, [ left; right ] -> emitExpr r _t [ left; right ] "($0 orelse $1)" |> Some
    | Operators.booleanAnd, [ left; right ] -> emitExpr r _t [ left; right ] "($0 andalso $1)" |> Some
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
    // TypeDefOf: typedefof<T> → TypeInfo with generics replaced by obj
    | "TypeDefOf", _ -> (genArg com ctx r 0 info.GenericArgs) |> makeTypeDefinitionInfo r |> Some
    // Reraise
    | "Reraise", _ ->
        match ctx.CaughtException with
        | Some ex -> makeThrow r _t (IdentExpr ex) |> Some
        | None -> makeThrow r _t (Value(StringConstant "reraise", None)) |> Some
    // Infinity and NaN — Erlang BEAM VM doesn't support IEEE 754 special float values.
    // Use max finite float (1.7976931348623157e308) as practical stand-in for infinity.
    | ("Infinity" | "InfinitySingle"), _ ->
        com.WarnOnlyOnce(
            "Erlang BEAM VM does not support IEEE 754 Infinity. Using max finite float as approximation. Arithmetic with this value may cause 'badarith' errors.",
            ?range = r
        )

        emitExpr r _t [] "1.7976931348623157e308" |> Some
    | ("NaN" | "NaNSingle"), _ ->
        com.WarnOnlyOnce(
            "Erlang BEAM VM does not support IEEE 754 NaN. Using atom 'nan' as sentinel. Arithmetic with this value will fail.",
            ?range = r
        )

        emitExpr r _t [] "nan" |> Some
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
    | ("GenericEquality" | "GenericEqualityIntrinsic"), [ left; right ] -> equals com r true left right |> Some
    | ("GenericEqualityER" | "GenericEqualityERIntrinsic"), [ left; right ] -> equals com r true left right |> Some
    | ("GenericComparison" | "GenericComparisonIntrinsic"), [ left; right ] -> compare com r left right |> Some
    | ("GenericLessThan" | "GenericLessThanIntrinsic"), [ left; right ] ->
        let cmp = compare com r left right
        makeBinOp r Boolean cmp (makeIntConst 0) BinaryLess |> Some
    | ("GenericLessOrEqual" | "GenericLessOrEqualIntrinsic"), [ left; right ] ->
        let cmp = compare com r left right
        makeBinOp r Boolean cmp (makeIntConst 0) BinaryLessOrEqual |> Some
    | ("GenericGreaterThan" | "GenericGreaterThanIntrinsic"), [ left; right ] ->
        let cmp = compare com r left right
        makeBinOp r Boolean cmp (makeIntConst 0) BinaryGreater |> Some
    | ("GenericGreaterOrEqual" | "GenericGreaterOrEqualIntrinsic"), [ left; right ] ->
        let cmp = compare com r left right
        makeBinOp r Boolean cmp (makeIntConst 0) BinaryGreaterOrEqual |> Some
    | ("PhysicalEquality" | "PhysicalEqualityIntrinsic"), [ left; right ] ->
        makeBinOp r Boolean left right BinaryEqual |> Some
    | ("GenericHash" | "GenericHashIntrinsic"), [ arg ] ->
        Helper.LibCall(com, "fable_comparison", "hash", t, [ arg ], ?loc = r) |> Some
    | ("PhysicalHash" | "PhysicalHashIntrinsic"), [ arg ] ->
        Helper.LibCall(com, "fable_comparison", "hash", t, [ arg ], ?loc = r) |> Some
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
    | "Hash", [ arg ] -> Helper.LibCall(com, "fable_comparison", "hash", t, [ arg ], ?loc = r) |> Some
    | "Equals", [ left; right ] -> equals com r true left right |> Some
    | "Compare", [ left; right ] -> compare com r left right |> Some
    | "NonNull", [ arg ] -> Some arg // Identity - unchecked non-null assertion
    | _ -> None

/// Beam-specific System.Object replacements.
let private objects
    (com: ICompiler)
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
    | "Equals", Some thisObj, [ arg ] -> equals com r true thisObj arg |> Some
    | "Equals", None, [ left; right ] -> equals com r true left right |> Some
    | "GetHashCode", Some thisObj, [] ->
        Helper.LibCall(com, "fable_comparison", "hash", t, [ thisObj ], ?loc = r)
        |> Some
    | "GetType", Some arg, _ -> makeTypeInfo r arg.Type |> Some
    | "ToString", Some thisObj, [] ->
        match thisObj.Type with
        | Type.Char -> emitExpr r t [ thisObj ] "<<($0)/utf8>>" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Decimal ->
                Helper.LibCall(com, "fable_decimal", "to_string", t, [ thisObj ], ?loc = r)
                |> Some
            | Float16
            | Float32
            | Float64 -> emitExpr r t [ thisObj ] "float_to_binary($0)" |> Some
            | _ -> emitExpr r t [ thisObj ] "integer_to_binary($0)" |> Some
        | Type.Boolean -> emitExpr r t [ thisObj ] "atom_to_binary($0)" |> Some
        | Type.String -> Some thisObj
        | DeclaredType(ent, _) when ent.FullName = Types.timespan ->
            Helper.LibCall(com, "fable_timespan", "to_string", t, [ thisObj ], ?loc = r)
            |> Some
        | DeclaredType(ent, _) when ent.FullName = Types.datetime ->
            Helper.LibCall(com, "fable_date", "to_string", t, [ thisObj ], ?loc = r) |> Some
        | DeclaredType(ent, _) when ent.FullName = "System.Uri" ->
            Helper.LibCall(com, "fable_uri", "to_string", t, [ thisObj ], ?loc = r) |> Some
        | DeclaredType(ent, _) when ent.FullName = "System.Text.StringBuilder" ->
            // StringBuilder.ToString() → iolist_to_binary(get(maps:get(field_buf, get(Sb))))
            emitExpr r t [ thisObj ] "iolist_to_binary(get(maps:get(field_buf, get($0))))"
            |> Some
        | _ ->
            Helper.LibCall(com, "fable_convert", "to_string", t, [ thisObj ], ?loc = r)
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
        | [ arg ] -> equals com r true thisObj arg |> Some
        | _ -> None
    | "GetHashCode", Some thisObj ->
        Helper.LibCall(com, "fable_comparison", "hash", t, [ thisObj ], ?loc = r)
        |> Some
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
    | "IsSurrogatePair", [ hi; lo ] when hi.Type = Type.Char ->
        Helper.LibCall(com, "fable_char", "is_surrogate_pair", t, [ hi; lo ]) |> Some
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
            | [ chars ] ->
                let chars = derefArr r chars
                Helper.LibCall(com, "fable_string", "string_ctor_chars", t, [ chars ]) |> Some
            | [ chars; start; len ] ->
                let chars = derefArr r chars

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
    | "Trim", Some c, [ chars ] ->
        let chars = derefArr r chars
        Helper.LibCall(com, "fable_string", "trim_chars", t, [ c; chars ]) |> Some
    | "TrimStart", Some c, [] -> emitExpr r t [ c ] "string:trim($0, leading)" |> Some
    | "TrimStart", Some c, [ chars ] ->
        let chars = derefArr r chars
        Helper.LibCall(com, "fable_string", "trim_start_chars", t, [ c; chars ]) |> Some
    | "TrimEnd", Some c, [] -> emitExpr r t [ c ] "string:trim($0, trailing)" |> Some
    | "TrimEnd", Some c, [ chars ] ->
        let chars = derefArr r chars
        Helper.LibCall(com, "fable_string", "trim_end_chars", t, [ c; chars ]) |> Some
    // str.StartsWith(prefix)
    | "StartsWith", Some c, [ prefix ] ->
        match prefix.Type with
        | Type.Char -> emitExpr r t [ c; prefix ] "fable_string:starts_with($0, <<($1)/utf8>>)" |> Some
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
        | Type.Char -> emitExpr r t [ c; suffix ] "fable_string:ends_with($0, <<($1)/utf8>>)" |> Some
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
    | "Split", Some c, [ sep ] ->
        let sep = derefArr r sep

        Helper.LibCall(com, "fable_string", "split", t, [ c; sep ])
        |> wrapArr com r t
        |> Some
    | "Split", Some c, [ sep; countOrOptions ] ->
        let sep = derefArr r sep

        match countOrOptions.Type with
        | Number(_, NumberInfo.IsEnum _) ->
            // Split(char[], StringSplitOptions)
            Helper.LibCall(com, "fable_string", "split", t, [ c; sep; countOrOptions ])
            |> wrapArr com r t
            |> Some
        | Number _ ->
            // Split(char[], count) — split with count limit
            Helper.LibCall(com, "fable_string", "split_with_count", t, [ c; sep; countOrOptions ])
            |> wrapArr com r t
            |> Some
        | _ ->
            // Default: treat as options (enum value may come as int in some contexts)
            Helper.LibCall(com, "fable_string", "split", t, [ c; sep; countOrOptions ])
            |> wrapArr com r t
            |> Some
    | "Split", Some c, [ sep; count; _options ] ->
        let sep = derefArr r sep

        Helper.LibCall(com, "fable_string", "split_with_count", t, [ c; sep; count ])
        |> wrapArr com r t
        |> Some
    // String.Join(sep, items)
    | "Join", None, [ sep; items ] ->
        // Check element type BEFORE derefArr (which erases to List Any).
        // Chars need special handling: convert to binaries before joining.
        let rec getElemType (ty: Type) =
            match ty with
            | Type.Array(elemType, _)
            | Type.List elemType -> Some elemType
            | Type.DeclaredType(_, [ elemType ]) -> Some elemType
            | _ -> None

        let hasCharElements =
            match getElemType items.Type with
            | Some Type.Char -> true
            | _ -> false

        let items = derefArr r items

        if hasCharElements then
            // Map chars to binaries: [<<C/utf8>> || C <- Items]
            emitExpr r t [ sep; items ] "fable_string:join($0, [<<C/utf8>> || C <- $1])"
            |> Some
        else
            Helper.LibCall(com, "fable_string", "join_strings", t, [ sep; items ]) |> Some
    | "Join", None, [ sep; items; startIndex; count ] ->
        let items = derefArr r items

        Helper.LibCall(com, "fable_string", "join", t, [ sep; items; startIndex; count ])
        |> Some
    // String.Concat(items) → iolist_to_binary(Items)
    | "Concat", None, args ->
        match args with
        | [ items ] -> emitExpr r t [ items ] "fable_string:concat($0)" |> Some
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
    | "Equals", None, [ a; b ] -> equals com r true a b |> Some
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
        | Type.Char -> emitExpr r t [ c; sub ] "fable_string:index_of($0, <<($1)/utf8>>)" |> Some
        | _ -> Helper.LibCall(com, "fable_string", "index_of", t, [ c; sub ]) |> Some
    | "IndexOf", Some c, [ sub; startIdx ] ->
        match sub.Type with
        | Type.Char ->
            emitExpr r t [ c; sub; startIdx ] "fable_string:index_of($0, <<($1)/utf8>>, $2)"
            |> Some
        | _ -> Helper.LibCall(com, "fable_string", "index_of", t, [ c; sub; startIdx ]) |> Some
    // str.IndexOfAny(chars) / str.IndexOfAny(chars, startIdx)
    | "IndexOfAny", Some c, [ chars ] ->
        let chars = derefArr r chars
        Helper.LibCall(com, "fable_string", "index_of_any", t, [ c; chars ]) |> Some
    | "IndexOfAny", Some c, [ chars; startIdx ] ->
        let chars = derefArr r chars

        Helper.LibCall(com, "fable_string", "index_of_any", t, [ c; chars; startIdx ])
        |> Some
    // str.LastIndexOf(sub) / str.LastIndexOf(sub, maxIdx)
    | "LastIndexOf", Some c, [ sub ] ->
        match sub.Type with
        | Type.Char -> emitExpr r t [ c; sub ] "fable_string:last_index_of($0, <<($1)/utf8>>)" |> Some
        | _ -> Helper.LibCall(com, "fable_string", "last_index_of", t, [ c; sub ]) |> Some
    | "LastIndexOf", Some c, [ sub; maxIdx ] ->
        match sub.Type with
        | Type.Char ->
            emitExpr r t [ c; sub; maxIdx ] "fable_string:last_index_of($0, <<($1)/utf8>>, $2)"
            |> Some
        | _ ->
            Helper.LibCall(com, "fable_string", "last_index_of", t, [ c; sub; maxIdx ])
            |> Some
    // str.ToCharArray() → binary_to_list(Str), wrap as array ref
    | "ToCharArray", Some c, [] -> emitExpr r t [ c ] "binary_to_list($0)" |> wrapArr com r t |> Some
    | "ToCharArray", Some c, [ start; len ] ->
        Helper.LibCall(com, "fable_string", "to_char_array", t, [ c; start; len ])
        |> wrapArr com r t
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
    | "Equals", Some c, [ arg ] -> equals com r true c arg |> Some
    | "Equals", Some c, [ arg; compType ] ->
        // Dispatch on StringComparison: 5=OrdinalIgnoreCase → lowercase compare, else exact
        emitExpr
            r
            t
            [ c; arg; compType ]
            "(fun() -> case $2 of 5 -> string:lowercase($0) =:= string:lowercase($1); _ -> $0 =:= $1 end end)()"
        |> Some
    | "CompareTo", Some c, [ arg ] -> compare com r c arg |> Some
    | "GetHashCode", Some c, [] -> Helper.LibCall(com, "fable_comparison", "hash", t, [ c ], ?loc = r) |> Some
    // str.GetEnumerator() → convert string to list of codepoints and create enumerator
    | "GetEnumerator", Some c, _ -> emitExpr r t [ c ] "fable_utils:get_enumerator(binary_to_list($0))" |> Some
    // String.Format("{0} {1}", arg0, arg1)
    | "Format", None, (fmtStr :: fmtArgs) ->
        // When a single array argument is passed (e.g. from ParamArray),
        // pass it directly — format/2 already handles refs and lists.
        let argsList =
            match fmtArgs with
            | [ singleArg ] when
                (match singleArg.Type with
                 | Array _ -> true
                 | _ -> false)
                ->
                singleArg
            | _ ->
                List.foldBack
                    (fun arg acc -> Value(NewList(Some(arg, acc), Any), None))
                    fmtArgs
                    (Value(NewList(None, Any), None))

        Helper.LibCall(com, "fable_string", "format", t, [ fmtStr; argsList ], ?loc = r)
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
    | "ToArray", [ opt ] ->
        Helper.LibCall(com, "fable_option", "to_array", t, [ opt ])
        |> wrapArr com r t
        |> Some
    | "ToList", [ opt ] -> Helper.LibCall(com, "fable_option", "to_list", t, [ opt ]) |> Some
    | "Flatten", [ opt ] -> Helper.LibCall(com, "fable_option", "flatten", t, [ opt ]) |> Some
    | "Count", [ opt ] -> Helper.LibCall(com, "fable_option", "count", t, [ opt ]) |> Some
    | "ForAll", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "for_all", t, [ fn; opt ]) |> Some
    | "Exists", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "exists", t, [ fn; opt ]) |> Some
    | ("OfOption" | "ToOption" | "OfValueOption" | "ToValueOption"), [ arg ] -> Some arg
    | _ -> None

/// Beam-specific FSharpOption instance method replacements.
let private options
    (com: ICompiler)
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
    // Static methods on ValueOption type (Bind, Map, DefaultValue, etc.)
    // fall through to optionModule which handles them identically
    | _ -> optionModule com _ctx r t info thisArg _args

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
    | "ToArray", [ result ] ->
        Helper.LibCall(com, "fable_result", "to_array", t, [ result ])
        |> wrapArr com _r t
        |> Some
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
        | Type.Char -> emitExpr r t [ arg ] "<<($0)/utf8>>" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Decimal -> Helper.LibCall(com, "fable_decimal", "to_string", t, [ arg ], ?loc = r) |> Some
            | Float16
            | Float32
            | Float64 -> emitExpr r t [ arg ] "float_to_binary($0)" |> Some
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
    | "Parse", None, [ arg; _style ] ->
        // NumberStyles.HexNumber — parse hex string to integer
        emitExpr r t [ arg ] "binary_to_integer($0, 16)" |> Some
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
    | "Equals", Some thisObj, [ arg ] -> equals com r true thisObj arg |> Some
    | "CompareTo", Some thisObj, [ arg ] -> compare com r thisObj arg |> Some
    | "GetHashCode", Some thisObj, [] ->
        Helper.LibCall(com, "fable_comparison", "hash", t, [ thisObj ], ?loc = r)
        |> Some
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
            | Decimal -> Helper.LibCall(com, "fable_decimal", "to_string", t, [ arg ], ?loc = r) |> Some
            | Float16
            | Float32
            | Float64 -> Helper.LibCall(com, "fable_convert", "to_string", t, [ arg ], ?loc = r) |> Some
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
    | "Take", [ count; list ] -> Helper.LibCall(com, "fable_list", "take", t, [ count; list ], ?loc = r) |> Some
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
    | "ToArray", [ list ] -> Helper.LibCall(com, "fable_utils", "new_ref", t, [ list ], ?loc = r) |> Some
    | "OfArray", [ arr ] -> derefArr r arr |> Some
    | "OfSeq", [ seq ] -> emitExpr r t [ seq ] "fable_utils:to_list($0)" |> Some
    | "ToSeq", [ list ] -> Some(List.head args)
    | _ -> None

/// Beam-specific FSharpList instance method replacements.
let private lists
    (com: ICompiler)
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
    | "GetSlice", Some c ->
        match _args with
        | [ lower; upper ] -> Helper.LibCall(com, "fable_list", "get_slice", t, [ lower; upper; c ]) |> Some
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
    | "OfArray", [ arr ] -> emitExpr r t [ derefArr r arr ] "maps:from_list($0)" |> Some
    | "OfSeq", [ seq ] -> emitExpr r t [ seq ] "maps:from_list(fable_utils:to_list($0))" |> Some
    | "Add", [ key; value; map ] -> emitExpr r t [ key; value; map ] "maps:put($0, $1, $2)" |> Some
    | "Find", [ key; map ] -> emitExpr r t [ key; map ] "maps:get($0, $1)" |> Some
    | "TryFind", [ key; map ] -> Helper.LibCall(com, "fable_map", "try_find", t, [ key; map ]) |> Some
    | "ContainsKey", [ key; map ] -> emitExpr r t [ key; map ] "maps:is_key($0, $1)" |> Some
    | "Remove", [ key; map ] -> emitExpr r t [ key; map ] "maps:remove($0, $1)" |> Some
    | "IsEmpty", [ map ] -> emitExpr r t [ map ] "(maps:size($0) =:= 0)" |> Some
    | ("Count" | "Length"), [ map ] -> emitExpr r t [ map ] "maps:size($0)" |> Some
    | "ToList", [ map ] -> emitExpr r t [ map ] "maps:to_list($0)" |> Some
    | "ToArray", [ map ] -> emitExpr r t [ map ] "maps:to_list($0)" |> wrapArr com r t |> Some
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
    | "OfArray", [ xs ] -> emitExpr r t [ derefArr r xs ] "ordsets:from_list($0)" |> Some
    | "OfList", [ xs ] -> emitExpr r t [ xs ] "ordsets:from_list($0)" |> Some
    | "OfSeq", [ xs ] -> emitExpr r t [ xs ] "ordsets:from_list(fable_utils:to_list($0))" |> Some
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
    | ("ToList" | "ToSeq"), [ set ] -> Some set
    | "ToArray", [ set ] -> Helper.LibCall(com, "fable_utils", "new_ref", t, [ set ], ?loc = r) |> Some
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
    | "Concat", [ sep; items ] ->
        let items = derefArr r items
        Helper.LibCall(com, "fable_string", "join", t, [ sep; items ]) |> Some
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

let private makeComparerFunction (com: ICompiler) ctx typArg =
    let x = makeUniqueIdent com ctx typArg "x"
    let y = makeUniqueIdent com ctx typArg "y"
    let body = compare com None (IdentExpr x) (IdentExpr y)
    Delegate([ x; y ], body, None, Tags.empty)

let private makeComparer (com: ICompiler) ctx typArg =
    objExpr [ "Compare", makeComparerFunction com ctx typArg ]

let private makeEqualityFunction (com: ICompiler) ctx typArg =
    let x = makeUniqueIdent com ctx typArg "x"
    let y = makeUniqueIdent com ctx typArg "y"
    let body = equals com None true (IdentExpr x) (IdentExpr y)
    Delegate([ x; y ], body, None, Tags.empty)

let private makeEqualityComparer (com: ICompiler) ctx typArg =
    let x = makeUniqueIdent com ctx typArg "x"
    let y = makeUniqueIdent com ctx typArg "y"

    objExpr
        [
            "Equals", Delegate([ x; y ], equals com None true (IdentExpr x) (IdentExpr y), None, Tags.empty)
            "GetHashCode",
            Delegate([ x ], Helper.LibCall(com, "fable_comparison", "hash", Any, [ IdentExpr x ]), None, Tags.empty)
        ]

let rec private getZero (com: ICompiler) (ctx: Context) (t: Type) =
    match t with
    | Boolean -> makeBoolConst false
    | Number(kind, uom) -> NumberConstant(NumberValue.GetZero kind, uom) |> makeValue None
    | Char
    | String -> makeStrConst ""
    | ListSingleton(CustomOp com ctx None t "get_Zero" [] e) -> e
    | _ -> Value(Null Any, None)

let private makeAddFunction (com: ICompiler) (ctx: Context) t =
    let x = makeUniqueIdent com ctx t "x"
    let y = makeUniqueIdent com ctx t "y"
    let body = makeBinOp None t (IdentExpr x) (IdentExpr y) BinaryPlus
    Delegate([ x; y ], body, None, Tags.empty)

let private makeGenericAdder (com: ICompiler) ctx t =
    objExpr
        [
            "GetZero", getZero com ctx t |> makeDelegate []
            "Add", makeAddFunction com ctx t
        ]

let private makeGenericAverager (com: ICompiler) ctx t =
    let divideFn =
        let x = makeUniqueIdent com ctx t "x"
        let i = makeUniqueIdent com ctx (Int32.Number) "i"
        let body = makeBinOp None t (IdentExpr x) (IdentExpr i) BinaryDivide
        Delegate([ x; i ], body, None, Tags.empty)

    objExpr
        [
            "GetZero", getZero com ctx t |> makeDelegate []
            "Add", makeAddFunction com ctx t
            "DivideByInt", divideFn
        ]

let private injectArg (com: ICompiler) (ctx: Context) r moduleName methName (genArgs: Type list) args =
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
            | Types.iequalityComparerGeneric -> args @ [ makeEqualityComparer com ctx genArg ]
            | Types.arrayCons ->
                // Not needed for Beam seq — pass None
                let cons = [ Expr.Value(ValueKind.NewOption(None, genArg, false), None) ]
                args @ cons
            | Types.adder -> args @ [ makeGenericAdder com ctx genArg ]
            | Types.averager -> args @ [ makeGenericAverager com ctx genArg ]
            | _ -> fail ()

    Map.tryFind moduleName ReplacementsInject.fableReplacementsModules
    |> Option.bind (Map.tryFind methName)
    |> function
        | None -> args
        | Some injectInfo -> injectArgInner args injectInfo

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
        | Array(Type.Number(UInt8, _), _) -> expr // byte arrays are binaries, no ref
        | Array _ -> emitExpr r (List Any) [ expr ] "erlang:get($0)" // deref array ref
        | String -> emitExpr r (List Any) [ expr ] "binary_to_list($0)"
        // For generic seq/list types, use runtime check — value might be array ref at runtime
        // Dictionary ref: convert to list of {Key, Value} tuples for Seq iteration
        | DeclaredType(entRef, _) when entRef.FullName = Types.dictionary ->
            emitExpr r (List Any) [ expr ] "maps:to_list(erlang:get($0))"
        | DeclaredType(entRef, _) when
            entRef.FullName = Types.ienumerableGeneric
            || entRef.FullName = Types.ienumerable
            ->
            emitExpr r (List Any) [ expr ] "fable_utils:to_list($0)"
        | _ -> expr

/// Beam-specific Seq module replacements.
/// Routes to compiled seq.erl (lazy, IEnumerator-based) via catch-all pattern,
/// matching the JS/Python approach. Uses injectArg for comparers/adders/averagers.
/// Operations needing equality comparers (Distinct, GroupBy, etc.) route to seq2.erl.
let private seqModule
    (com: ICompiler)
    (ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "Cast", [ arg ] -> Some arg
    | "ToArray", [ arg ] ->
        // seq:to_array already returns a ref-wrapped array, don't double-wrap
        Helper.LibCall(com, "seq", "to_array", t, [ arg ], info.SignatureArgTypes, ?loc = r)
        |> Some
    | "OfArray", [ arg ] ->
        let derefed = derefArr r arg

        Helper.LibCall(com, "seq", "of_array", t, [ derefed ], info.SignatureArgTypes, ?loc = r)
        |> Some
    | ("Distinct" | "DistinctBy" | "Except" | "GroupBy" | "CountBy") as meth, args ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq2" meth info.GenericArgs args

        Helper.LibCall(com, "seq2", meth, t, args, info.SignatureArgTypes, ?loc = r)
        |> Some
    | "Head", [ seq ] ->
        // Use compiled seq:head — handles lazy evaluation correctly (only evaluates first element)
        // Note: conflates None/empty for seq<Option<T>> due to option representation (pre-existing limitation)
        Helper.LibCall(com, "seq", "head", t, [ seq ], info.SignatureArgTypes, ?loc = r)
        |> Some
    | ("Length" | "Count"), [ seq ] ->
        Helper.LibCall(com, "seq", "length", t, [ seq ], info.SignatureArgTypes, ?loc = r)
        |> Some
    | ("Readonly" | "ReadOnly" | "Cache"), [ seq ] -> Some seq
    | "CreateEvent", [ addHandler; removeHandler; _createHandler ] ->
        Helper.LibCall(
            com,
            "fable_event",
            "create_event",
            t,
            [ addHandler; removeHandler ],
            info.SignatureArgTypes,
            ?loc = r
        )
        |> Some
    | meth, _ ->
        let meth = Naming.lowerFirst meth
        let args = injectArg com ctx r "Seq" meth info.GenericArgs args
        // Unwrap Dictionary args so seq operations get {K,V} tuple lists
        let rec unwrapDictArg (expr: Expr) =
            match expr with
            | TypeCast(innerExpr, _) -> unwrapDictArg innerExpr
            | _ ->
                match expr.Type with
                | DeclaredType(entRef, _) when entRef.FullName = Types.dictionary ->
                    emitExpr r (List Any) [ expr ] "maps:to_list(erlang:get($0))"
                | _ -> expr

        let args = args |> List.map unwrapDictArg

        Helper.LibCall(com, "seq", meth, t, args, info.SignatureArgTypes, ?thisArg = thisArg, ?loc = r)
        |> Some

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
    | "get_Length", Some c, _ when isByteArray -> emitExpr r t [ c ] "fable_utils:byte_array_length($0)" |> Some
    | "get_Length", Some c, _ -> emitExpr r t [ c ] "erlang:length(erlang:get($0))" |> Some
    | "get_Item", Some c, [ idx ] when isByteArray ->
        emitExpr r t [ c; idx ] "fable_utils:byte_array_get($0, $1)" |> Some
    | "get_Item", Some c, [ idx ] -> emitExpr r t [ c; idx ] "lists:nth($1 + 1, erlang:get($0))" |> Some
    | "set_Item", Some c, [ idx; value ] -> setExpr r c idx value |> Some
    // System.Array.Copy(source, dest, length) — copy first N elements
    | "Copy", None, [ src; _dest; len ] ->
        let src = derefArr r src
        emitExpr r t [ src; len ] "lists:sublist($0, $1)" |> Some
    // System.Array.IndexOf(arr, value) — find index of value
    | "IndexOf", None, [ arr; value ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "index_of_value", t, [ value; arr ]) |> Some
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
    // === Scalar-returning ops: deref array arg(s) ===
    | ("Length" | "Count"), [ arr ] -> let arr = derefArr r arr in emitExpr r t [ arr ] "erlang:length($0)" |> Some
    | "Item", [ idx; arr ] -> let arr = derefArr r arr in emitExpr r t [ arr; idx ] "lists:nth($1 + 1, $0)" |> Some
    | "Get", [ arr; idx ] -> let arr = derefArr r arr in emitExpr r t [ arr; idx ] "lists:nth($1 + 1, $0)" |> Some
    | "Set", [ arr; idx; value ] -> setExpr r arr idx value |> Some
    | "Head", [ arr ] -> let arr = derefArr r arr in emitExpr r t [ arr ] "erlang:hd($0)" |> Some
    | "Last", [ arr ] -> let arr = derefArr r arr in emitExpr r t [ arr ] "lists:last($0)" |> Some
    | "Tail", [ arr ] -> let arr = derefArr r arr in emitExpr r t [ arr ] "erlang:tl($0)" |> wrapArr com r t |> Some
    | "IsEmpty", [ arr ] -> let arr = derefArr r arr in emitExpr r t [ arr ] "($0 =:= [])" |> Some
    | "Sum", [ arr ] -> let arr = derefArr r arr in emitExpr r t [ arr ] "lists:sum($0)" |> Some
    | "Min", [ arr ] -> let arr = derefArr r arr in emitExpr r t [ arr ] "lists:min($0)" |> Some
    | "Max", [ arr ] -> let arr = derefArr r arr in emitExpr r t [ arr ] "lists:max($0)" |> Some
    | "Contains", [ value; arr ] ->
        let arr = derefArr r arr in emitExpr r t [ value; arr ] "lists:member($0, $1)" |> Some
    | "Exists", [ fn; arr ] -> let arr = derefArr r arr in emitExpr r t [ fn; arr ] "lists:any($0, $1)" |> Some
    | "ForAll", [ fn; arr ] -> let arr = derefArr r arr in emitExpr r t [ fn; arr ] "lists:all($0, $1)" |> Some
    | "Iterate", [ fn; arr ] -> let arr = derefArr r arr in emitExpr r t [ fn; arr ] "lists:foreach($0, $1)" |> Some
    | "Fold", [ fn; state; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "fold", t, [ fn; state; arr ]) |> Some
    | "FoldBack", [ fn; arr; state ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "fold_back", t, [ fn; arr; state ]) |> Some
    | "Reduce", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "reduce", t, [ fn; arr ]) |> Some
    | "SumBy", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "sum_by", t, [ fn; arr ]) |> Some
    | "MinBy", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "min_by", t, [ fn; arr ]) |> Some
    | "MaxBy", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "max_by", t, [ fn; arr ]) |> Some
    | "Find", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "find", t, [ fn; arr ]) |> Some
    | "TryFind", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_find", t, [ fn; arr ]) |> Some
    | "FindIndex", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "find_index", t, [ fn; arr ]) |> Some
    | "TryFindIndex", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_find_index", t, [ fn; arr ]) |> Some
    | "FindBack", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "find_back", t, [ fn; arr ]) |> Some
    | "FindIndexBack", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "find_index_back", t, [ fn; arr ]) |> Some
    | "TryFindBack", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_find_back", t, [ fn; arr ]) |> Some
    | "TryFindIndexBack", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_find_index_back", t, [ fn; arr ]) |> Some
    | "Pick", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "pick", t, [ fn; arr ]) |> Some
    | "TryPick", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_pick", t, [ fn; arr ]) |> Some
    | "ReduceBack", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "reduce_back", t, [ fn; arr ]) |> Some
    | "TryHead", [ arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_head", t, [ arr ]) |> Some
    | "TryLast", [ arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_last", t, [ arr ]) |> Some
    | "TryItem", [ idx; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_item", t, [ idx; arr ]) |> Some
    | "ExactlyOne", [ arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "exactly_one", t, [ arr ]) |> Some
    | "TryExactlyOne", [ arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "try_exactly_one", t, [ arr ]) |> Some
    | "Average", [ arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "average", t, [ arr ]) |> Some
    | "AverageBy", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "average_by", t, [ fn; arr ]) |> Some
    | "IterateIndexed", [ fn; arr ] ->
        let arr = derefArr r arr
        Helper.LibCall(com, "fable_list", "iteri", t, [ fn; arr ]) |> Some
    | "CompareWith", [ fn; a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2
        Helper.LibCall(com, "fable_list", "compare_with", t, [ fn; a1; a2 ]) |> Some
    // === Transform ops: deref input(s) AND wrap result ===
    | "Map", [ fn; arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ fn; arr ] "lists:map($0, $1)" |> wrapArr com r t |> Some
    | "MapIndexed", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "map_indexed", t, [ fn; arr ])
        |> wrapArr com r t
        |> Some
    | "Filter", [ fn; arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ fn; arr ] "lists:filter($0, $1)" |> wrapArr com r t |> Some
    | "Choose", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "choose", t, [ fn; arr ])
        |> wrapArr com r t
        |> Some
    | "Collect", [ fn; arr ] ->
        let arr = derefArr r arr
        // fn returns array refs, need to deref each before concatenating
        emitExpr r t [ fn; arr ] "lists:append(lists:map(fun(X_collect) -> erlang:get($0(X_collect)) end, $1))"
        |> wrapArr com r t
        |> Some
    | "Reverse", [ arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ arr ] "lists:reverse($0)" |> wrapArr com r t |> Some
    | "Append", [ arr1; arr2 ] ->
        let arr1 = derefArr r arr1
        let arr2 = derefArr r arr2
        emitExpr r t [ arr1; arr2 ] "lists:append($0, $1)" |> wrapArr com r t |> Some
    | "Concat", [ arrs ] ->
        // arrs is a seq/array of arrays — materialize to list, then deref each inner array ref
        emitExpr r t [ arrs ] "lists:append(lists:map(fun erlang:get/1, fable_utils:to_list($0)))"
        |> wrapArr com r t
        |> Some
    | "Sort", [ arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ arr ] "lists:sort($0)" |> wrapArr com r t |> Some
    | "SortDescending", [ arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ arr ] "lists:reverse(lists:sort($0))" |> wrapArr com r t |> Some
    | "SortBy", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "sort_by", t, [ fn; arr ])
        |> wrapArr com r t
        |> Some
    | "SortWith", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "sort_with", t, [ fn; arr ])
        |> wrapArr com r t
        |> Some
    | "SortByDescending", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "sort_by_descending", t, [ fn; arr ])
        |> wrapArr com r t
        |> Some
    | "Zip", [ arr1; arr2 ] ->
        let arr1 = derefArr r arr1
        let arr2 = derefArr r arr2

        Helper.LibCall(com, "fable_list", "zip", t, [ arr1; arr2 ])
        |> wrapArr com r t
        |> Some
    | "Zip3", [ a1; a2; a3 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2
        let a3 = derefArr r a3

        Helper.LibCall(com, "fable_list", "zip3", t, [ a1; a2; a3 ])
        |> wrapArr com r t
        |> Some
    | "Unzip", [ arr ] ->
        let arr = derefArr r arr

        emitExpr
            r
            t
            [ arr ]
            "(fun() -> {Uz_a, Uz_b} = lists:unzip($0), {fable_utils:new_ref(Uz_a), fable_utils:new_ref(Uz_b)} end)()"
        |> Some
    | "Unzip3", [ arr ] ->
        let arr = derefArr r arr

        emitExpr
            r
            t
            [ arr ]
            "(fun() -> {Uz_a, Uz_b, Uz_c} = lists:unzip3($0), {fable_utils:new_ref(Uz_a), fable_utils:new_ref(Uz_b), fable_utils:new_ref(Uz_c)} end)()"
        |> Some
    | "Copy", [ arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ arr ] "lists:append($0, [])" |> wrapArr com r t |> Some
    | "Scan", [ fn; state; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "scan", t, [ fn; state; arr ])
        |> wrapArr com r t
        |> Some
    | "ScanBack", [ fn; arr; state ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "scan_back", t, [ fn; arr; state ])
        |> wrapArr com r t
        |> Some
    | "Partition", [ fn; arr ] ->
        let arr = derefArr r arr

        emitExpr
            r
            t
            [ fn; arr ]
            "(fun() -> {Pt_a, Pt_b} = lists:partition($0, $1), {fable_utils:new_ref(Pt_a), fable_utils:new_ref(Pt_b)} end)()"
        |> Some
    | "Permute", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "permute", t, [ fn; arr ])
        |> wrapArr com r t
        |> Some
    | "Map2", [ fn; a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2

        Helper.LibCall(com, "fable_list", "map2", t, [ fn; a1; a2 ])
        |> wrapArr com r t
        |> Some
    | "Map3", [ fn; a1; a2; a3 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2
        let a3 = derefArr r a3

        Helper.LibCall(com, "fable_list", "map3", t, [ fn; a1; a2; a3 ])
        |> wrapArr com r t
        |> Some
    | "MapIndexed2", [ fn; a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2

        Helper.LibCall(com, "fable_list", "mapi2", t, [ fn; a1; a2 ])
        |> wrapArr com r t
        |> Some
    | "MapFold", [ fn; state; arr ] ->
        let arr = derefArr r arr
        // Returns {List, State} tuple — wrap the list part
        emitExpr
            r
            t
            [ Helper.LibCall(com, "fable_list", "map_fold", t, [ fn; state; arr ]) ]
            "(fun() -> {Mf_list, Mf_state} = $0, {fable_utils:new_ref(Mf_list), Mf_state} end)()"
        |> Some
    | "MapFoldBack", [ fn; arr; state ] ->
        let arr = derefArr r arr

        emitExpr
            r
            t
            [ Helper.LibCall(com, "fable_list", "map_fold_back", t, [ fn; arr; state ]) ]
            "(fun() -> {Mf_list, Mf_state} = $0, {fable_utils:new_ref(Mf_list), Mf_state} end)()"
        |> Some
    | "Distinct", [ arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "distinct", t, [ arr ])
        |> wrapArr com r t
        |> Some
    | "DistinctBy", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "distinct_by", t, [ fn; arr ])
        |> wrapArr com r t
        |> Some
    | "Pairwise", [ arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "pairwise", t, [ arr ])
        |> wrapArr com r t
        |> Some
    | "GroupBy", [ fn; arr ] ->
        let arr = derefArr r arr
        // Returns list of {Key, List} tuples — inner lists need wrapping as array refs
        emitExpr
            r
            t
            [ Helper.LibCall(com, "fable_list", "group_by", t, [ fn; arr ]) ]
            "fable_utils:new_ref(lists:map(fun({Gb_k, Gb_v}) -> {Gb_k, fable_utils:new_ref(Gb_v)} end, $0))"
        |> Some
    | "CountBy", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "count_by", t, [ fn; arr ])
        |> wrapArr com r t
        |> Some
    | "Windowed", [ size; arr ] ->
        let arr = derefArr r arr
        let result = Helper.LibCall(com, "fable_list", "windowed", t, [ size; arr ])
        // Returns list of lists (windows) — wrap each inner as array ref, then wrap outer
        emitExpr r t [ result ] "fable_utils:new_ref(lists:map(fun(Wn_x) -> fable_utils:new_ref(Wn_x) end, $0))"
        |> Some
    | "SplitInto", [ count; arr ] ->
        let arr = derefArr r arr
        let result = Helper.LibCall(com, "fable_list", "split_into", t, [ count; arr ])

        emitExpr r t [ result ] "fable_utils:new_ref(lists:map(fun(Si_x) -> fable_utils:new_ref(Si_x) end, $0))"
        |> Some
    | "Transpose", [ arrs ] ->
        // arrs is an array/seq ref containing array refs — unwrap outer (handles TypeCast), deref each inner
        let arrs = unwrapSeqArg r arrs
        let innerDerefed = emitExpr r t [ arrs ] "lists:map(fun erlang:get/1, $0)"
        let result = Helper.LibCall(com, "fable_list", "transpose", t, [ innerDerefed ])
        // Wrap each inner result and the outer
        emitExpr r t [ result ] "fable_utils:new_ref(lists:map(fun(Tr_x) -> fable_utils:new_ref(Tr_x) end, $0))"
        |> Some
    | "Skip", [ count; arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ arr; count ] "lists:nthtail($1, $0)" |> wrapArr com r t |> Some
    | "SkipWhile", [ fn; arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ fn; arr ] "lists:dropwhile($0, $1)" |> wrapArr com r t |> Some
    | "Take", [ count; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "take", t, [ count; arr ], ?loc = r)
        |> wrapArr com r t
        |> Some
    | "TakeWhile", [ fn; arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ fn; arr ] "lists:takewhile($0, $1)" |> wrapArr com r t |> Some
    | "Truncate", [ count; arr ] ->
        let arr = derefArr r arr
        emitExpr r t [ arr; count ] "lists:sublist($0, $1)" |> wrapArr com r t |> Some
    | ("Sub" | "GetSubArray"), [ arr; start; count ] ->
        let arr = derefArr r arr

        emitExpr r t [ arr; start; count ] "lists:sublist($0, $1 + 1, $2)"
        |> wrapArr com r t
        |> Some
    | "Except", [ excludeArr; arr ] ->
        let excludeArr = unwrapSeqArg r excludeArr
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "except", t, [ excludeArr; arr ])
        |> wrapArr com r t
        |> Some
    | "ChunkBySize", [ size; arr ] ->
        let arr = derefArr r arr
        let result = Helper.LibCall(com, "fable_list", "chunk_by_size", t, [ size; arr ])

        emitExpr r t [ result ] "fable_utils:new_ref(lists:map(fun(Ck_x) -> fable_utils:new_ref(Ck_x) end, $0))"
        |> Some
    | "SplitAt", [ idx; arr ] ->
        let arr = derefArr r arr

        emitExpr
            r
            t
            [ Helper.LibCall(com, "fable_list", "split_at", t, [ idx; arr ]) ]
            "(fun() -> {Sa_a, Sa_b} = $0, {fable_utils:new_ref(Sa_a), fable_utils:new_ref(Sa_b)} end)()"
        |> Some
    | "AllPairs", [ a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2

        Helper.LibCall(com, "fable_list", "all_pairs", t, [ a1; a2 ])
        |> wrapArr com r t
        |> Some
    | "Indexed", [ arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "indexed", t, [ arr ])
        |> wrapArr com r t
        |> Some
    | "UpdateAt", [ idx; value; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "update_at", t, [ idx; value; arr ])
        |> wrapArr com r t
        |> Some
    | "InsertAt", [ idx; value; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "insert_at", t, [ idx; value; arr ])
        |> wrapArr com r t
        |> Some
    | "InsertManyAt", [ idx; values; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "insert_many_at", t, [ idx; values; arr ])
        |> wrapArr com r t
        |> Some
    | "RemoveAt", [ idx; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "remove_at", t, [ idx; arr ])
        |> wrapArr com r t
        |> Some
    | "RemoveManyAt", [ idx; count; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_list", "remove_many_at", t, [ idx; count; arr ])
        |> wrapArr com r t
        |> Some
    | "Exists2", [ fn; a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2
        Helper.LibCall(com, "fable_list", "exists2", t, [ fn; a1; a2 ]) |> Some
    | "ForAll2", [ fn; a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2
        Helper.LibCall(com, "fable_list", "forall2", t, [ fn; a1; a2 ]) |> Some
    | "Fold2", [ fn; state; a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2
        Helper.LibCall(com, "fable_list", "fold2", t, [ fn; state; a1; a2 ]) |> Some
    | "FoldBack2", [ fn; a1; a2; state ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2

        Helper.LibCall(com, "fable_list", "fold_back2", t, [ fn; a1; a2; state ])
        |> Some
    | "Iterate2", [ fn; a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2
        Helper.LibCall(com, "fable_list", "iter2", t, [ fn; a1; a2 ]) |> Some
    | "IterateIndexed2", [ fn; a1; a2 ] ->
        let a1 = derefArr r a1
        let a2 = derefArr r a2
        Helper.LibCall(com, "fable_list", "iteri2", t, [ fn; a1; a2 ]) |> Some
    // === Creation ops: wrap result only ===
    | "Empty", _ -> Value(NewArray(ArrayValues [], t, MutableArray), None) |> Some
    | "Singleton", [ item ] ->
        match t with
        | Array(Type.Number(UInt8, _), _) ->
            Helper.LibCall(com, "fable_utils", "new_byte_array", t, [ emitExpr r t [ item ] "[$0]" ], ?loc = r)
            |> Some
        | _ ->
            Helper.LibCall(com, "fable_utils", "new_ref", t, [ emitExpr r t [ item ] "[$0]" ], ?loc = r)
            |> Some
    | "ZeroCreate", [ count ] ->
        match t with
        | Array(Type.Number(UInt8, _), _) ->
            Helper.LibCall(com, "fable_utils", "new_byte_array_zeroed", t, [ count ], ?loc = r)
            |> Some
        | _ ->
            Helper.LibCall(
                com,
                "fable_utils",
                "new_ref",
                t,
                [ emitExpr r t [ count ] "lists:duplicate($0, 0)" ],
                ?loc = r
            )
            |> Some
    | "Create", [ count; value ] ->
        match t with
        | Array(Type.Number(UInt8, _), _) ->
            // Use new_byte_array_filled which avoids intermediate list
            // and short-circuits to atomics:new for value 0
            Helper.LibCall(com, "fable_utils", "new_byte_array_filled", t, [ count; value ], ?loc = r)
            |> Some
        | _ ->
            Helper.LibCall(
                com,
                "fable_utils",
                "new_ref",
                t,
                [ emitExpr r t [ count; value ] "lists:duplicate($0, $1)" ],
                ?loc = r
            )
            |> Some
    | "Initialize", [ count; fn ] ->
        Helper.LibCall(com, "fable_list", "init", t, [ count; fn ])
        |> wrapArr com r t
        |> Some
    // === Conversion ops ===
    | "ToList", [ arr ] -> derefArr r arr |> Some
    | "OfList", [ lst ] -> wrapArr com r t lst |> Some
    | "OfSeq", [ seq ] ->
        // Use fable_utils:to_list to handle lazy seqs, refs, plain lists
        emitExpr r t [ seq ] "fable_utils:new_ref(fable_utils:to_list($0))" |> Some
    | "ToSeq", [ arr ] -> derefArr r arr |> Some
    // === In-place mutation: deref for computation, put result back ===
    | "SortInPlace", [ arr ] ->
        let derefed = derefArr r arr
        let sorted = emitExpr r t [ derefed ] "lists:sort($0)"
        Fable.Set(arr, ValueSet, Type.Unit, sorted, None) |> Some
    | "SortInPlaceBy", [ fn; arr ] ->
        let derefed = derefArr r arr
        let sorted = Helper.LibCall(com, "fable_list", "sort_by", t, [ fn; derefed ])
        Fable.Set(arr, ValueSet, Type.Unit, sorted, None) |> Some
    | "SortInPlaceWith", [ fn; arr ] ->
        let derefed = derefArr r arr
        let sorted = Helper.LibCall(com, "fable_list", "sort_with", t, [ fn; derefed ])
        Fable.Set(arr, ValueSet, Type.Unit, sorted, None) |> Some
    | "Fill", [ arr; start; count; value ] ->
        let derefed = derefArr r arr

        let filled =
            Helper.LibCall(com, "fable_resize_array", "fill", t, [ derefed; start; count; value ])

        Fable.Set(arr, ValueSet, Type.Unit, filled, None) |> Some
    | "CopyTo", [ source; sourceIdx; target; targetIdx; count ] ->
        let derefSource = derefArr r source
        let derefTarget = derefArr r target

        let blitted =
            Helper.LibCall(
                com,
                "fable_resize_array",
                "blit",
                t,
                [ derefSource; sourceIdx; derefTarget; targetIdx; count ]
            )

        Fable.Set(target, ValueSet, Type.Unit, blitted, None) |> Some
    | _ -> None

/// Beam-specific Array.Parallel module replacements — dispatches to fable_parallel.
let private arrayParallelModule
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "Map", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_parallel", "parallel_map", List Any, [ fn; arr ], ?loc = r)
        |> wrapArr com r t
        |> Some
    | "MapIndexed", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_parallel", "parallel_mapi", List Any, [ fn; arr ], ?loc = r)
        |> wrapArr com r t
        |> Some
    | "Iterate", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_parallel", "parallel_iter", t, [ fn; arr ], ?loc = r)
        |> Some
    | "IterateIndexed", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_parallel", "parallel_iteri", t, [ fn; arr ], ?loc = r)
        |> Some
    | "Initialize", [ count; fn ] ->
        Helper.LibCall(com, "fable_parallel", "parallel_init", List Any, [ count; fn ], ?loc = r)
        |> wrapArr com r t
        |> Some
    | "Collect", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_parallel", "parallel_collect", List Any, [ fn; arr ], ?loc = r)
        |> wrapArr com r t
        |> Some
    | "Choose", [ fn; arr ] ->
        let arr = derefArr r arr

        Helper.LibCall(com, "fable_parallel", "parallel_choose", List Any, [ fn; arr ], ?loc = r)
        |> wrapArr com r t
        |> Some
    // Fall back to sequential for ops without parallel implementation
    | _ -> arrayModule com _ctx r t info _thisArg args

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
        let ar = derefArr r ar

        let lower =
            match lower with
            | Value(NewOption(Some lower, _, _), _) -> lower
            | _ -> makeIntConst 0

        match upper with
        | Value(NewOption(None, _, _), _) ->
            // arr.[start..] → lists:nthtail(start, arr)
            emitExpr r t [ ar; lower ] "lists:nthtail($1, $0)" |> wrapArr com r t |> Some
        | _ ->
            let upper =
                match upper with
                | Value(NewOption(Some upper, _, _), _) -> upper
                | _ -> makeIntConst 0

            // arr.[start..end] → lists:sublist(arr, start+1, end-start+1)  (1-based)
            emitExpr r t [ ar; lower; upper ] "lists:sublist($0, $1 + 1, $2 - $1 + 1)"
            |> wrapArr com r t
            |> Some
    | _ -> None

let error (_com: ICompiler) (msg: Expr) = msg

let rec defaultof (_com: ICompiler) (_ctx: Context) (r: SourceLocation option) (typ: Type) =
    match typ with
    | Nullable _ -> Value(Null typ, r)
    | Tuple(args, true) -> NewTuple(args |> List.map (defaultof _com _ctx r), true) |> makeValue None
    | Boolean -> makeBoolConst false
    | Number(kind, uom) -> NumberConstant(NumberValue.GetZero kind, uom) |> makeValue None
    | Char -> CharConstant '\u0000' |> makeValue None
    | String -> Value(Null typ, r)
    | Builtin BclTimeSpan -> makeIntConst 0
    | Builtin BclDateTime -> Value(NewTuple([ makeIntConst 0; makeIntConst 0 ], false), r)
    | Builtin BclDateTimeOffset -> Value(NewTuple([ makeIntConst 0; makeIntConst 0; makeIntConst 0 ], false), r)
    | Builtin BclGuid -> makeStrConst "00000000-0000-0000-0000-000000000000"
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

let events (com: ICompiler) (_ctx: Context) r (t: Type) (i: CallInfo) (thisArg: Expr option) (args: Expr list) =
    match i.CompiledName, thisArg with
    | ".ctor", _ ->
        Helper.LibCall(com, "fable_event", "new_event", t, [], i.SignatureArgTypes, ?loc = r)
        |> Some
    | "get_Publish", Some x ->
        Helper.LibCall(com, "fable_event", "publish", t, [ x ], i.SignatureArgTypes, ?loc = r)
        |> Some
    | "Trigger", Some x ->
        Helper.LibCall(com, "fable_event", "trigger", t, x :: args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | "Add", Some x ->
        Helper.LibCall(com, "fable_event", "add", t, args @ [ x ], i.SignatureArgTypes, ?loc = r)
        |> Some
    | "Subscribe", Some x ->
        Helper.LibCall(com, "fable_observable", "subscribe", t, args @ [ x ], i.SignatureArgTypes, ?loc = r)
        |> Some
    | "AddHandler", Some x -> emitExpr r t (x :: args) "(maps:get(add_handler, $0))($1)" |> Some
    | "RemoveHandler", Some x -> emitExpr r t (x :: args) "(maps:get(remove_handler, $0))($1)" |> Some
    | meth, None ->
        Helper.LibCall(com, "fable_event", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | _ -> None

let observable (com: ICompiler) (_ctx: Context) r (t: Type) (i: CallInfo) (_: Expr option) (args: Expr list) =
    Helper.LibCall(
        com,
        "fable_observable",
        Naming.lowerFirst i.CompiledName,
        t,
        args,
        i.SignatureArgTypes,
        genArgs = i.GenericArgs,
        ?loc = r
    )
    |> Some

let controlExtensions
    (com: ICompiler)
    (_ctx: Context)
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

        Helper.LibCall(com, "fable_observable", meth, t, args, argTypes)
    )

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
    // Instance Split — returns string[], wrap as array ref
    | "Split", Some callee, [ input ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ callee; input ], ?loc = r)
        |> wrapArr com r t
        |> Some
    | "Split", Some callee, [ input; count ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ callee; input; count ], ?loc = r)
        |> wrapArr com r t
        |> Some
    | "Split", Some callee, [ input; count; offset ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ callee; input; count; offset ], ?loc = r)
        |> wrapArr com r t
        |> Some
    // Static Split — returns string[], wrap as array ref
    | "Split", None, [ input; pattern ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ input; pattern ], ?loc = r)
        |> wrapArr com r t
        |> Some
    | "Split", None, [ input; pattern; options ] ->
        Helper.LibCall(com, "fable_regex", "split", t, [ input; pattern; options ], ?loc = r)
        |> wrapArr com r t
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
        let emptyList = Value(NewList(None, Any), None)

        Helper.LibCall(com, "fable_utils", "new_ref", t, [ emptyList ], ?loc = r)
        |> Some
    | ".ctor", _, [ ExprType(Number _) ] ->
        // Ignore size hint, just create empty
        let emptyList = Value(NewList(None, Any), None)

        Helper.LibCall(com, "fable_utils", "new_ref", t, [ emptyList ], ?loc = r)
        |> Some
    | ".ctor", _, [ arg ] ->
        // From IEnumerable/sequence — unwrap array refs (handles TypeCast coercion)
        let arg = unwrapSeqArg r arg
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
    | "AddRange", Some callee, [ arg ] ->
        let arg = derefArr r arg
        emitExpr r Unit [ callee; arg ] "put($0, get($0) ++ $1)" |> Some
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
    // RemoveRange
    | "RemoveRange", Some callee, [ idx; count ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        let call =
            Helper.LibCall(com, "fable_resize_array", "remove_range", t, [ listExpr; idx; count ], ?loc = r)

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
    | "ToArray", Some callee, [] ->
        Helper.LibCall(com, "fable_utils", "new_ref", t, [ emitExpr r t [ callee ] "get($0)" ], ?loc = r)
        |> Some
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
    let isStruct =
        i.DeclaringEntityFullName.StartsWith("System.ValueTuple", System.StringComparison.Ordinal)

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
        // Could be IDictionary/IEnumerable<KeyValuePair> or IEqualityComparer — check signature
        match info.SignatureArgTypes with
        | [ IEqualityComparer ] ->
            // IEqualityComparer: ignore, create empty (Erlang =:= does structural comparison)
            Helper.LibCall(com, "fable_dictionary", "create_empty", t, [], ?loc = r) |> Some
        | _ ->
            Helper.LibCall(com, "fable_dictionary", "create_from_list", t, [ arg ], ?loc = r)
            |> Some
    | ".ctor", _, [ _arg; _eqComp ] ->
        // With IEqualityComparer: ignore comparer for now, Erlang =:= does structural comparison
        match info.SignatureArgTypes with
        | [ Number _; _ ] ->
            // (capacity, comparer)
            Helper.LibCall(com, "fable_dictionary", "create_empty", t, [], ?loc = r) |> Some
        | [ IEnumerable; _ ]
        | [ _; IEqualityComparer ] ->
            // (items, comparer) — use items, ignore comparer
            Helper.LibCall(com, "fable_dictionary", "create_from_list", t, [ _arg ], ?loc = r)
            |> Some
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
        // Could be IEnumerable, IEqualityComparer, or capacity (int)
        match info.SignatureArgTypes with
        | [ IEqualityComparer ] ->
            // IEqualityComparer: ignore, create empty (Erlang =:= does structural comparison)
            Helper.LibCall(com, "fable_hashset", "create_empty", t, [], ?loc = r) |> Some
        | _ ->
            // From IEnumerable
            Helper.LibCall(com, "fable_hashset", "create_from_list", t, [ arg ], ?loc = r)
            |> Some
    | ".ctor", _, [ arg; _eqComp ] ->
        // With IEqualityComparer: ignore comparer, Erlang =:= does structural comparison
        match info.SignatureArgTypes with
        | [ Number _; _ ] ->
            // (capacity, comparer)
            Helper.LibCall(com, "fable_hashset", "create_empty", t, [], ?loc = r) |> Some
        | [ IEnumerable; _ ]
        | [ _; IEqualityComparer ] ->
            // (items, comparer) — use items, ignore comparer
            Helper.LibCall(com, "fable_hashset", "create_from_list", t, [ arg ], ?loc = r)
            |> Some
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
    // CopyTo
    | "CopyTo", Some callee, [ arr ] ->
        Helper.LibCall(com, "fable_hashset", "copy_to", t, [ callee; arr ], ?loc = r)
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
    | "ToArray", Some callee, _ ->
        Helper.LibCall(com, "fable_queue", "to_array", t, [ callee ], ?loc = r)
        |> wrapArr com r t
        |> Some
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
    | "ToArray", Some callee, _ ->
        Helper.LibCall(com, "fable_stack", "to_array", t, [ callee ], ?loc = r)
        |> wrapArr com r t
        |> Some
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
    let isKeyOrValueCollection =
        info.DeclaringEntityFullName.EndsWith("KeyCollection", System.StringComparison.Ordinal)
        || info.DeclaringEntityFullName.EndsWith("ValueCollection", System.StringComparison.Ordinal)

    match info.CompiledName, thisArg with
    | "get_Count", Some callee when isKeyOrValueCollection -> emitExpr r t [ callee ] "erlang:length($0)" |> Some
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
    | "GetEnumerator", Some callee when isKeyOrValueCollection ->
        // KeyCollection/ValueCollection are plain lists, use generic enumerator
        Helper.LibCall(com, "fable_utils", "get_enumerator", t, [ callee ], ?loc = r)
        |> Some
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
        match arg.Type with
        | Boolean ->
            Helper.LibCall(com, "fable_bit_converter", "get_bytes_bool", t, [ arg ], ?loc = r)
            |> Some
        | _ ->
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
    | "ToBoolean", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_boolean", t, [ bytes; startIndex ], ?loc = r)
        |> Some
    | "ToString", [ bytes ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_string", t, [ bytes ], ?loc = r)
        |> Some
    | "ToString", [ bytes; startIndex ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_string", t, [ bytes; startIndex ], ?loc = r)
        |> Some
    | "ToString", [ bytes; startIndex; count ] ->
        Helper.LibCall(com, "fable_bit_converter", "to_string", t, [ bytes; startIndex; count ], ?loc = r)
        |> Some
    | "Int64BitsToDouble", [ arg ] ->
        Helper.LibCall(com, "fable_bit_converter", "int64_bits_to_double", t, [ arg ], ?loc = r)
        |> Some
    | "DoubleToInt64Bits", [ arg ] ->
        Helper.LibCall(com, "fable_bit_converter", "double_to_int64_bits", t, [ arg ], ?loc = r)
        |> Some
    | _ -> None

/// Beam-specific System.Text.Encoding replacements.
/// In Erlang, strings are binaries (UTF-8 by default), so encoding is mostly identity.
let private encoding
    (com: ICompiler)
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
    // GetBytes(string) → convert string binary to atomics byte array
    | "GetBytes", Some _callee, [ arg ] -> emitExpr r t [ arg ] "fable_utils:new_byte_array(binary_to_list($0))" |> Some
    // GetBytes(string, index, count) → extract then convert to byte array
    | "GetBytes", Some _callee, [ str; idx; count ] ->
        emitExpr r t [ str; idx; count ] "fable_utils:new_byte_array(binary_to_list(binary:part($0, $1, $2)))"
        |> Some
    // GetString(bytes) → convert byte array to string binary
    | "GetString", Some _callee, [ arg ] ->
        emitExpr r t [ arg ] "list_to_binary(fable_utils:byte_array_to_list($0))"
        |> Some
    // GetString(bytes, index, count) → convert byte array subset to string
    | "GetString", Some _callee, [ bytes; idx; count ] ->
        emitExpr
            r
            t
            [ bytes; idx; count ]
            "list_to_binary(lists:sublist(fable_utils:byte_array_to_list($0), $1 + 1, $2))"
        |> Some
    | _ -> None

/// Beam-specific System.Diagnostics.Stopwatch replacements.
let private stopwatch
    (com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (_args: Expr list)
    =
    match info.CompiledName, thisArg with
    | ".ctor", _ -> Helper.LibCall(com, "fable_stopwatch", "create", t, [], ?loc = r) |> Some
    | "StartNew", _ -> Helper.LibCall(com, "fable_stopwatch", "start_new", t, [], ?loc = r) |> Some
    | "Start", Some x -> Helper.LibCall(com, "fable_stopwatch", "start", t, [ x ], ?loc = r) |> Some
    | "Stop", Some x -> Helper.LibCall(com, "fable_stopwatch", "stop", t, [ x ], ?loc = r) |> Some
    | "Reset", Some x -> Helper.LibCall(com, "fable_stopwatch", "reset", t, [ x ], ?loc = r) |> Some
    | "get_IsRunning", Some x -> Helper.LibCall(com, "fable_stopwatch", "is_running", t, [ x ], ?loc = r) |> Some
    | "get_ElapsedMilliseconds", Some x ->
        Helper.LibCall(com, "fable_stopwatch", "elapsed_milliseconds", t, [ x ], ?loc = r)
        |> Some
    | "get_ElapsedTicks", Some x ->
        Helper.LibCall(com, "fable_stopwatch", "elapsed_ticks", t, [ x ], ?loc = r)
        |> Some
    | "get_Elapsed", Some x -> Helper.LibCall(com, "fable_stopwatch", "elapsed", t, [ x ], ?loc = r) |> Some
    // Stopwatch.Frequency → 1_000_000 (microseconds per second)
    | "get_Frequency", _ -> makeIntConst 1_000_000 |> Some
    // Stopwatch.GetTimestamp() → erlang:monotonic_time(microsecond)
    | "GetTimestamp", _ -> emitExpr r t [] "erlang:monotonic_time(microsecond)" |> Some
    | _ -> None

/// Beam-specific Nullable<T> replacements.
/// Nullable is erased in Erlang — Nullable(x) → x, Nullable<T>() → undefined.
let private nullables
    (com: ICompiler)
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
    | "TryParse" -> Helper.LibCall(com, "fable_guid", "try_parse", t, args, ?loc = r) |> Some
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
        | Some c -> equals com r true c args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some c -> Helper.LibCall(com, "fable_comparison", "hash", t, [ c ], ?loc = r) |> Some
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
        | Some callee -> equals com r true callee args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_comparison", "hash", t, [ callee ], ?loc = r) |> Some
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
        | Some callee -> equals com r true callee args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_comparison", "hash", t, [ callee ], ?loc = r) |> Some
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
        | Some callee -> equals com r true callee args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_comparison", "hash", t, [ callee ], ?loc = r) |> Some
        | None -> None
    | "UnescapeDataString" ->
        Helper.LibCall(com, "fable_uri", "unescape_data_string", t, args, ?loc = r)
        |> Some
    | "EscapeDataString" ->
        Helper.LibCall(com, "fable_uri", "escape_data_string", t, args, ?loc = r)
        |> Some
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
        | Some callee -> equals com r true callee args.Head |> Some
        | None -> None
    | "GetHashCode" ->
        match thisArg with
        | Some callee -> Helper.LibCall(com, "fable_comparison", "hash", t, [ callee ], ?loc = r) |> Some
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
        | [ left; right ] -> equals com r true left right |> Some
        | _ -> None
    | "op_Inequality" ->
        match args with
        | [ left; right ] -> equals com r false left right |> Some
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

let tryType (_t: Type) = None

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
    | "Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators"
    | "System.Math"
    | "System.MathF" -> operators com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives"
    | "Microsoft.FSharp.Core.LanguagePrimitives.HashCompare" -> languagePrimitives com ctx r t info thisArg args
    | Types.string -> strings com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.StringModule" -> stringModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.FSharpOption`1"
    | "Microsoft.FSharp.Core.FSharpValueOption`1" -> options com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.OptionModule"
    | "Microsoft.FSharp.Core.ValueOptionModule"
    | "Microsoft.FSharp.Core.ValueOption" -> optionModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.ResultModule" -> resultModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.FSharpList`1" -> lists com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.ListModule" -> listModule com ctx r t info thisArg args
    | "System.Array" -> arrays com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.ArrayModule" -> arrayModule com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.ArrayModule.Parallel" -> arrayParallelModule com ctx r t info thisArg args
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
    | "System.Console" ->
        match info.CompiledName, thisArg, args with
        | "WriteLine", None, [] -> emitExpr r t [] "io:format(<<\"~n\">>)" |> Some
        | "WriteLine", None, [ arg ] ->
            Helper.LibCall(com, "fable_string", "console_writeline", t, [ arg ], ?loc = r)
            |> Some
        | "Write", None, [ arg ] ->
            Helper.LibCall(com, "fable_string", "console_write", t, [ arg ], ?loc = r)
            |> Some
        | _ -> None
    | "System.Boolean" ->
        match info.CompiledName, thisArg, args with
        | "Parse", None, [ arg ] ->
            Helper.LibCall(com, "fable_convert", "boolean_parse", t, [ arg ], ?loc = r)
            |> Some
        | "TryParse", None, _ ->
            Helper.LibCall(com, "fable_convert", "boolean_try_parse", t, args, ?loc = r)
            |> Some
        | "Equals", Some thisObj, [ arg ] -> equals com r true thisObj arg |> Some
        | "CompareTo", Some thisObj, [ arg ] -> compare com r thisObj arg |> Some
        | "GetHashCode", Some thisObj, [] ->
            Helper.LibCall(com, "fable_comparison", "hash", t, [ thisObj ], ?loc = r)
            |> Some
        | _ -> None
    | "System.Char" ->
        match chars com ctx r t info thisArg args with
        | Some _ as res -> res
        | None ->
            match info.CompiledName, thisArg, args with
            | "Equals", Some thisObj, [ arg ] -> equals com r true thisObj arg |> Some
            | "CompareTo", Some thisObj, [ arg ] -> compare com r thisObj arg |> Some
            | "GetHashCode", Some thisObj, [] ->
                Helper.LibCall(com, "fable_comparison", "hash", t, [ thisObj ], ?loc = r)
                |> Some
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
            | Type.Array(Type.Number(UInt8, _), _) ->
                emitExpr r t [ ar; idx ] "fable_utils:byte_array_get($0, $1)" |> Some
            | _ ->
                let ar = derefArr r ar
                emitExpr r t [ ar; idx ] "lists:nth($1 + 1, $0)" |> Some
        | "GetString", [ ar; idx ] -> emitExpr r t [ ar; idx ] "binary:at($0, $1)" |> Some
        | "SetArray", [ ar; idx; value ] -> setExpr r ar idx value |> Some
        | ("UnboxFast" | "UnboxGeneric" | "CheckThis"), [ arg ] -> TypeCast(arg, t) |> Some
        | ("TypeTestGeneric" | "TypeTestFast"), [ expr ] ->
            Test(expr, TypeTest((genArg com ctx r 0 info.GenericArgs)), r) |> Some
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
        | "op_Equality", None, [ left; right ] -> equals com r true left right |> Some
        | "op_Inequality", None, [ left; right ] -> equals com r false left right |> Some
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
        | "Equals", Some thisObj, [ arg ] -> equals com r true thisObj arg |> Some
        | "GetHashCode", Some thisObj, [] ->
            Helper.LibCall(com, "fable_comparison", "hash", t, [ thisObj ], ?loc = r)
            |> Some
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
    | "System.Threading.Tasks.Parallel" ->
        match info.CompiledName, args with
        | "For", [ fromInclusive; toExclusive; body ] ->
            Helper.LibCall(com, "fable_parallel", "parallel_for", t, [ fromInclusive; toExclusive; body ], ?loc = r)
            |> Some
        | _ -> None
    | Types.printfModule
    | Naming.StartsWith Types.printfFormat _ -> fsFormat com ctx r t info thisArg args
    | "System.Threading.CancellationToken"
    | "System.Threading.CancellationTokenSource" -> cancels com ctx r t info thisArg args
    | "Microsoft.FSharp.Control.CommonExtensions" -> controlExtensions com ctx r t info thisArg args
    | "Microsoft.FSharp.Control.FSharpEvent`1"
    | "Microsoft.FSharp.Control.FSharpEvent`2"
    | "Microsoft.FSharp.Control.EventModule" -> events com ctx r t info thisArg args
    | "Microsoft.FSharp.Control.ObservableModule" -> observable com ctx r t info thisArg args
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
    | "Fable.Core.BeamInterop.Erlang" ->
        match info.CompiledName, args with
        | "receive", [ timeoutArg ] -> emitExpr r t [ timeoutArg ] "__fable_beam_receive__($0)" |> Some
        | "receiveForever", _ -> emitExpr r t [] "__fable_beam_receive_forever__" |> Some
        | _ -> None
    // Testing assertions (used by our test framework)
    | "Fable.Core.Testing.Assert" ->
        match info.CompiledName, args with
        | "AreEqual", [ expected; actual ] -> equals com r true expected actual |> Some
        | "NotEqual", [ expected; actual ] -> equals com r false expected actual |> Some
        | _ -> None
    // IDisposable
    | "System.IDisposable" ->
        match info.CompiledName, thisArg with
        | "Dispose", Some c -> Helper.LibCall(com, "fable_utils", "safe_dispose", t, [ c ], ?loc = r) |> Some
        | _ -> None
    // Exception — caught exceptions are wrapped as #{message => Msg} by Fable2Beam
    // ExceptionDispatchInfo is used to raise exceptions through different threads in async workflows
    | "System.Runtime.ExceptionServices.ExceptionDispatchInfo" ->
        match info.CompiledName, thisArg, args with
        | "Capture", _, [ arg ] -> Some arg
        | "Throw", Some arg, _ -> makeThrow r t arg |> Some
        | _ -> None
    | "System.Exception" ->
        match info.CompiledName, thisArg, args with
        | ".ctor", None, [ msg ] -> emitExpr r t [ msg ] "#{message => $0}" |> Some
        | ".ctor", None, [] ->
            emitExpr r t [] "#{message => <<\"Exception of type 'System.Exception' was thrown.\">>}"
            |> Some
        | "get_Message", Some c, _ ->
            // Handle both map exceptions and reference-based class exceptions
            emitExpr
                r
                t
                [ c ]
                "case erlang:is_reference($0) of true -> maps:get(message, erlang:get($0), $0); false -> maps:get(message, $0, $0) end"
            |> Some
        | _ -> None
    // Built-in .NET exception types — all become #{message => Msg} maps in Erlang
    | BuiltinSystemException _
    | "System.Collections.Generic.KeyNotFoundException"
    | "System.OperationCanceledException" ->
        match info.CompiledName, thisArg, args with
        | ".ctor", None, [ msg ] -> emitExpr r t [ msg ] "#{message => $0}" |> Some
        | ".ctor", None, [] ->
            let typeName = info.DeclaringEntityFullName
            let msg = $"Exception of type '%s{typeName}' was thrown."
            emitExpr r t [] $"#{{message => <<\"%s{msg}\">>}}" |> Some
        | "get_Message", Some c, _ -> emitExpr r t [ c ] "maps:get(message, $0, $0)" |> Some
        | _ -> None
    // System.Type (reflection) — type info is a map #{fullname => ..., generics => [...]}
    | "System.Type" ->
        match info.CompiledName, thisArg with
        | "get_FullName", Some c -> Helper.LibCall(com, "fable_reflection", "full_name", t, [ c ], ?loc = r) |> Some
        | "get_Namespace", Some c -> Helper.LibCall(com, "fable_reflection", "namespace", t, [ c ], ?loc = r) |> Some
        | "get_Name", Some c -> Helper.LibCall(com, "fable_reflection", "name", t, [ c ], ?loc = r) |> Some
        | "get_IsGenericType", Some c ->
            Helper.LibCall(com, "fable_reflection", "is_generic_type", t, [ c ], ?loc = r)
            |> Some
        | "get_IsArray", Some c -> Helper.LibCall(com, "fable_reflection", "is_array", t, [ c ], ?loc = r) |> Some
        | "GetGenericTypeDefinition", Some c ->
            Helper.LibCall(com, "fable_reflection", "get_generic_type_definition", t, [ c ], ?loc = r)
            |> Some
        | "GetElementType", Some c ->
            Helper.LibCall(com, "fable_reflection", "get_element_type", t, [ c ], ?loc = r)
            |> Some
        | "get_GenericTypeArguments", Some c ->
            Helper.LibCall(com, "fable_reflection", "get_generics", t, [ c ], ?loc = r)
            |> wrapArr com r t
            |> Some
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
        | "Invoke", Some callee ->
            // Delegates are uncurried in Erlang — apply all args at once, not curried.
            match args with
            | [] ->
                // Zero-arg Invoke (e.g. Func<int>.Invoke(), Action.Invoke()).
                // The underlying fun may be arity 0 (unit stripped at definition) or arity 1
                // (unit kept in Delegate AST node). Erlang enforces arity, so check at runtime.
                emitExpr
                    r
                    t
                    [ callee ]
                    "case erlang:fun_info($0, arity) of {arity, 0} -> ($0)(); {arity, _} -> ($0)(ok) end"
                |> Some
            | _ ->
                let placeholders =
                    args |> List.mapi (fun i _ -> $"$%d{i + 1}") |> String.concat ", "

                emitExpr r t (callee :: args) $"($0)(%s{placeholders})" |> Some
        | _ -> None
    // F# Reflection
    | "Microsoft.FSharp.Reflection.FSharpType" ->
        match info.CompiledName, args with
        | "MakeTupleType", [ typesArr ] ->
            let derefed = derefArr r typesArr

            Helper.LibCall(com, "fable_reflection", "make_tuple_type", t, [ derefed ], ?loc = r)
            |> Some
        | "GetRecordFields", _ ->
            Helper.LibCall(com, "fable_reflection", "get_record_elements", t, args, ?loc = r)
            |> wrapArr com r t
            |> Some
        | "GetUnionCases", _ ->
            Helper.LibCall(com, "fable_reflection", "get_union_cases", t, args, ?loc = r)
            |> wrapArr com r t
            |> Some
        | "GetTupleElements", _ ->
            Helper.LibCall(com, "fable_reflection", "get_tuple_elements", t, args, ?loc = r)
            |> wrapArr com r t
            |> Some
        | "GetFunctionElements", _ ->
            Helper.LibCall(com, "fable_reflection", "get_function_elements", t, args, ?loc = r)
            |> Some
        | "IsRecord", _ -> Helper.LibCall(com, "fable_reflection", "is_record", t, args, ?loc = r) |> Some
        | "IsUnion", _ -> Helper.LibCall(com, "fable_reflection", "is_union", t, args, ?loc = r) |> Some
        | "IsTuple", _ ->
            Helper.LibCall(com, "fable_reflection", "is_tuple_type", t, args, ?loc = r)
            |> Some
        | "IsFunction", _ ->
            Helper.LibCall(com, "fable_reflection", "is_function_type", t, args, ?loc = r)
            |> Some
        | _ -> None
    | "Microsoft.FSharp.Reflection.FSharpValue" ->
        match info.CompiledName, args with
        | "MakeTuple", [ values; _ ] ->
            // MakeTuple(values, tupleType) — values is an array ref, convert to Erlang tuple
            emitExpr r t [ values ] "list_to_tuple(erlang:get($0))" |> Some
        | "GetTupleFields", [ tuple ] ->
            Helper.LibCall(com, "fable_reflection", "get_tuple_fields_value", t, [ tuple ], ?loc = r)
            |> wrapArr com r t
            |> Some
        | "GetTupleField", [ tuple; index ] ->
            Helper.LibCall(com, "fable_reflection", "get_tuple_field_value", t, [ tuple; index ], ?loc = r)
            |> Some
        | "GetRecordFields", [ record ] ->
            // Inject type info at compile time since Erlang maps don't preserve order
            // Look through TypeCast to get the concrete type (since GetRecordFields takes obj)
            let rec getConcreteType (expr: Expr) =
                match expr with
                | TypeCast(inner, _) -> getConcreteType inner
                | _ -> expr.Type

            let concreteType = getConcreteType record
            let typeInfo = Value(TypeInfo(concreteType, []), None)

            Helper.LibCall(com, "fable_reflection", "get_record_fields_value", t, [ record; typeInfo ], ?loc = r)
            |> wrapArr com r t
            |> Some
        | "GetRecordField", [ record; propInfo ] ->
            Helper.LibCall(com, "fable_reflection", "get_record_field_value", t, [ record; propInfo ], ?loc = r)
            |> Some
        | "MakeRecord", [ typ; values ] ->
            Helper.LibCall(com, "fable_reflection", "make_record_from_values", t, [ typ; values ], ?loc = r)
            |> Some
        | "GetUnionFields", [ union; typ ] ->
            Helper.LibCall(com, "fable_reflection", "get_union_fields_value", t, [ union; typ ], ?loc = r)
            |> Some
        | "MakeUnion", [ caseInfo; values ] ->
            Helper.LibCall(com, "fable_reflection", "make_union_value", t, [ caseInfo; values ], ?loc = r)
            |> Some
        | _ -> None
    // PropertyInfo and UnionCaseInfo
    | "Microsoft.FSharp.Reflection.UnionCaseInfo"
    | "System.Reflection.PropertyInfo"
    | "System.Reflection.MemberInfo" ->
        match thisArg, info.CompiledName with
        | Some c, "get_Tag" -> emitExpr r t [ c ] "maps:get(tag, $0)" |> Some
        | Some c, ("get_PropertyType" | "get_ParameterType") -> emitExpr r t [ c ] "maps:get(property_type, $0)" |> Some
        | Some c, "GetFields" ->
            Helper.LibCall(com, "fable_reflection", "get_union_case_fields", t, [ c ], ?loc = r)
            |> wrapArr com r t
            |> Some
        | Some c, "GetValue" ->
            Helper.LibCall(com, "fable_reflection", "get_value", t, c :: args, ?loc = r)
            |> Some
        | Some c, "get_Name" ->
            match c.Type with
            | MetaType -> Helper.LibCall(com, "fable_reflection", "name", t, [ c ], ?loc = r) |> Some
            | _ -> emitExpr r t [ c ] "maps:get(name, $0)" |> Some
        | _ -> None
    | "System.Text.StringBuilder" -> bclType com ctx r t info thisArg args
    | _ -> None

let tryBaseConstructor
    (_com: ICompiler)
    (_ctx: Context)
    (ent: EntityRef)
    (_argTypes: Lazy<Type list>)
    (_genArgs: Type list)
    (args: Expr list)
    =
    match ent.FullName with
    | Types.exception_ ->
        // For classes inheriting from exn, the base constructor provides the message.
        // transformClassDeclaration extracts the args to add a message field.
        let baseExpr = emitExpr None Any [] "fable_exn_base"
        Some(baseExpr, args)
    | Types.attribute ->
        // Attributes have no runtime representation in Beam
        let baseExpr = emitExpr None Any [] "ok"
        Some(baseExpr, [])
    | _ -> None
