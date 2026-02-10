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
    (_com: ICompiler)
    (_ctx: Context)
    r
    (_t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | ("DefaultArg" | "DefaultValueArg"), [ opt; defValue ] ->
        Helper.LibCall(_com, "fable_option", "default_value", _t, [ opt; defValue ])
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
    | "Hash", [ arg ] -> emitExpr r _t [ arg ] "erlang:phash2($0)" |> Some
    | "Compare", [ left; right ] -> compare _com r left right |> Some
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
    | ("Pow" | "op_Exponentiation"), [ base_; exp_ ] -> emitExpr r _t [ base_; exp_ ] "math:pow($0, $1)" |> Some
    | "Round", [ arg ] -> emitExpr r _t [ arg ] "float(erlang:round($0))" |> Some
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
        | Type.String -> emitExpr r _t [ arg ] "binary_to_integer($0)" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r _t [ arg ] "trunc($0)" |> Some
            | _ -> Some arg
        | Type.Char -> Some arg
        | _ -> Some arg
    | ("ToSingle" | "ToDouble"), [ arg ] ->
        match arg.Type with
        | Type.String -> Helper.LibCall(_com, "fable_convert", "to_float", _t, [ arg ]) |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> Some arg
            | _ -> emitExpr r _t [ arg ] "float($0)" |> Some
        | _ -> emitExpr r _t [ arg ] "float($0)" |> Some
    | "ToDecimal", [ arg ] ->
        match arg.Type with
        | Type.String -> Helper.LibCall(_com, "fable_convert", "to_float", _t, [ arg ]) |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> Some arg
            | _ -> emitExpr r _t [ arg ] "float($0)" |> Some
        | _ -> emitExpr r _t [ arg ] "float($0)" |> Some
    | "ToString", [ arg ] ->
        match arg.Type with
        | Type.String -> Some arg
        | Type.Char -> emitExpr r _t [ arg ] "<<$0/utf8>>" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r _t [ arg ] "float_to_binary($0)" |> Some
            | _ -> emitExpr r _t [ arg ] "integer_to_binary($0)" |> Some
        | Type.Boolean -> emitExpr r _t [ arg ] "atom_to_binary($0)" |> Some
        | _ ->
            emitExpr r _t [ arg ] "iolist_to_binary(io_lib:format(binary_to_list(<<\"~p\">>), [$0]))"
            |> Some
    | "ToChar", [ arg ] ->
        match arg.Type with
        | Type.String -> emitExpr r _t [ arg ] "binary:first($0)" |> Some
        | _ -> Some arg
    // CreateSet: the `set [1;2;3]` syntax
    | "CreateSet", [ arg ] -> emitExpr r _t [ arg ] "ordsets:from_list($0)" |> Some
    // CreateDictionary: the `dict [...]` syntax
    | ("CreateDictionary" | "CreateReadOnlyDictionary"), [ arg ] ->
        Helper.LibCall(_com, "fable_dictionary", "create_from_list", _t, [ arg ], ?loc = r)
        |> Some
    // Range operators: [start..stop] and [start..step..stop]
    | "op_Range", [ first; last ] -> emitExpr r _t [ first; last ] "lists:seq($0, $1)" |> Some
    | "op_RangeStep", [ first; step; last ] -> emitExpr r _t [ first; step; last ] "lists:seq($0, $2, $1)" |> Some
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
    | "Ref", [ arg ] ->
        emitExpr r Any [ arg ] "(fun() -> Ref = make_ref(), put(Ref, $0), Ref end)()"
        |> Some
    | ("Increment" | "Decrement"), [ arg ] ->
        // incr x → put(x, get(x) + 1); decr x → put(x, get(x) - 1)
        let delta =
            if info.CompiledName = "Increment" then
                "1"
            else
                "-1"

        emitExpr r _t [ arg ] $"put($0, get($0) + %s{delta})" |> Some
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
        | _ -> None
    | "DefaultAsyncBuilder", _ ->
        Helper.LibCall(_com, "fable_async_builder", "singleton", _t, [], ?loc = r)
        |> Some
    | ("PrintFormatToString" | "PrintFormatToStringThen" | "PrintFormat" | "PrintFormatLine" | "PrintFormatToError" | "PrintFormatLineToError" | "PrintFormatThen" | "PrintFormatToStringThenFail"),
      _ -> fsFormat _com _ctx r _t info _thisArg args
    | _ -> None

let private languagePrimitives
    (com: ICompiler)
    (_ctx: Context)
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
    | "ReferenceEquals", None, [ left; right ] -> makeBinOp r Boolean left right BinaryEqual |> Some
    | "Equals", Some thisObj, [ arg ] -> equals r true thisObj arg |> Some
    | "Equals", None, [ left; right ] -> equals r true left right |> Some
    | "GetHashCode", Some thisObj, [] -> emitExpr r t [ thisObj ] "erlang:phash2($0)" |> Some
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
    | "DefaultValue", [ defVal; opt ] ->
        Helper.LibCall(com, "fable_option", "default_value", t, [ opt; defVal ]) |> Some
    | "DefaultWith", [ defFn; opt ] -> Helper.LibCall(com, "fable_option", "default_with", t, [ opt; defFn ]) |> Some
    | "Map", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "map", t, [ fn; opt ]) |> Some
    | "Bind", [ fn; opt ] -> Helper.LibCall(com, "fable_option", "bind", t, [ fn; opt ]) |> Some
    | "IsSome", [ c ] -> Test(c, OptionTest true, r) |> Some
    | "IsNone", [ c ] -> Test(c, OptionTest false, r) |> Some
    | "GetValue", [ c ] ->
        // Option.get / Option.value — just return the value (will crash on undefined at runtime)
        Some c
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
        | Type.String -> emitExpr r t [ arg ] "binary_to_integer($0)" |> Some
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
        | _ ->
            // Fallback: use io_lib:format ~p for general toString
            emitExpr r t [ arg ] "iolist_to_binary(io_lib:format(binary_to_list(<<\"~p\">>), [$0]))"
            |> Some
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
        | "System.Single"
        | "System.Decimal" -> Helper.LibCall(com, "fable_convert", "to_float", t, [ arg ]) |> Some
        | _ ->
            // Int32, Int64, Byte, etc. — all parse to integer
            emitExpr r t [ arg ] "binary_to_integer($0)" |> Some
    | "ToString", Some c, [] ->
        match c.Type with
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r t [ c ] "float_to_binary($0)" |> Some
            | _ -> emitExpr r t [ c ] "integer_to_binary($0)" |> Some
        | _ -> None
    | "Equals", Some thisObj, [ arg ] -> equals r true thisObj arg |> Some
    | "CompareTo", Some thisObj, [ arg ] -> compare com r thisObj arg |> Some
    | "GetHashCode", Some thisObj, [] -> emitExpr r t [ thisObj ] "erlang:phash2($0)" |> Some
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
        | Type.String -> emitExpr r t [ arg ] "binary_to_integer($0)" |> Some
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
        | _ ->
            emitExpr r t [ arg ] "iolist_to_binary(io_lib:format(binary_to_list(<<\"~p\">>), [$0]))"
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
    | "Head", [ list ] -> emitExpr r t [ list ] "hd($0)" |> Some
    | "Tail", [ list ] -> emitExpr r t [ list ] "tl($0)" |> Some
    | ("Length" | "Count"), [ list ] -> emitExpr r t [ list ] "length($0)" |> Some
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
    | "get_Head", Some c -> emitExpr r t [ c ] "hd($0)" |> Some
    | "get_Tail", Some c -> emitExpr r t [ c ] "tl($0)" |> Some
    | "get_Length", Some c -> emitExpr r t [ c ] "length($0)" |> Some
    | "get_IsEmpty", Some c -> emitExpr r t [ c ] "($0 =:= [])" |> Some
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
    | "Count", [ set ] -> emitExpr r t [ set ] "length($0)" |> Some
    | "Union", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:union($0, $1)" |> Some
    | "Intersect", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:intersection($0, $1)" |> Some
    | "Difference", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:subtract($0, $1)" |> Some
    | "IsSubset", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:is_subset($0, $1)" |> Some
    | "IsSuperset", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "ordsets:is_subset($1, $0)" |> Some
    | "IsProperSubset", [ s1; s2 ] -> Helper.LibCall(com, "fable_set", "is_proper_subset", t, [ s1; s2 ]) |> Some
    | "IsProperSuperset", [ s1; s2 ] -> Helper.LibCall(com, "fable_set", "is_proper_superset", t, [ s1; s2 ]) |> Some
    | "MinElement", [ set ] -> emitExpr r t [ set ] "hd($0)" |> Some
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
    | "get_Count", Some c, _ -> emitExpr r t [ c ] "length($0)" |> Some
    | "get_IsEmpty", Some c, _ -> emitExpr r t [ c ] "($0 =:= [])" |> Some
    | "Contains", Some c, [ elem ] -> emitExpr r t [ elem; c ] "ordsets:is_element($0, $1)" |> Some
    | "Add", Some c, [ elem ] -> emitExpr r t [ elem; c ] "ordsets:add_element($0, $1)" |> Some
    | "Remove", Some c, [ elem ] -> emitExpr r t [ elem; c ] "ordsets:del_element($0, $1)" |> Some
    | "get_MinimumElement", Some c, _ -> emitExpr r t [ c ] "hd($0)" |> Some
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

/// Beam-specific Seq module replacements.
/// Sequences in Erlang are represented as eager lists.
/// Most operations reuse lists:* BIFs or fable_list.erl;
/// Seq-specific operations (delay, unfold, etc.) use fable_seq.erl.
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
    // Identity conversions — seq is a list in Beam
    | "ToList", [ seq ] -> Some seq
    | "ToArray", [ seq ] -> Some seq
    | "OfList", [ list ] -> Some list
    | "OfArray", [ arr ] -> Some arr
    // Empty
    | "Empty", _ -> Value(NewList(None, t), None) |> Some
    // Simple emitExpr (1:1 BIF mappings)
    | "Head", [ seq ] -> emitExpr r t [ seq ] "hd($0)" |> Some
    | "Last", [ seq ] -> emitExpr r t [ seq ] "lists:last($0)" |> Some
    | ("Length" | "Count"), [ seq ] -> emitExpr r t [ seq ] "length($0)" |> Some
    | "IsEmpty", [ seq ] -> emitExpr r t [ seq ] "($0 =:= [])" |> Some
    | "Map", [ fn; seq ] -> emitExpr r t [ fn; seq ] "lists:map($0, $1)" |> Some
    | "Filter", [ fn; seq ] -> emitExpr r t [ fn; seq ] "lists:filter($0, $1)" |> Some
    | "Reverse", [ seq ] -> emitExpr r t [ seq ] "lists:reverse($0)" |> Some
    | "Append", [ s1; s2 ] -> emitExpr r t [ s1; s2 ] "lists:append($0, $1)" |> Some
    | "Concat", [ seqs ] -> emitExpr r t [ seqs ] "lists:append($0)" |> Some
    | "Sum", [ seq ] -> emitExpr r t [ seq ] "lists:sum($0)" |> Some
    | "Contains", [ item; seq ] -> emitExpr r t [ item; seq ] "lists:member($0, $1)" |> Some
    | "Exists", [ fn; seq ] -> emitExpr r t [ fn; seq ] "lists:any($0, $1)" |> Some
    | "ForAll", [ fn; seq ] -> emitExpr r t [ fn; seq ] "lists:all($0, $1)" |> Some
    | "Iterate", [ fn; seq ] -> emitExpr r t [ fn; seq ] "lists:foreach($0, $1)" |> Some
    | "Sort", [ seq ] -> emitExpr r t [ seq ] "lists:sort($0)" |> Some
    | "SortDescending", [ seq ] -> emitExpr r t [ seq ] "lists:reverse(lists:sort($0))" |> Some
    | "Min", [ seq ] -> emitExpr r t [ seq ] "lists:min($0)" |> Some
    | "Max", [ seq ] -> emitExpr r t [ seq ] "lists:max($0)" |> Some
    | "Partition", [ fn; seq ] -> emitExpr r t [ fn; seq ] "lists:partition($0, $1)" |> Some
    | "Unzip", [ seq ] -> emitExpr r t [ seq ] "lists:unzip($0)" |> Some
    | "Item", [ idx; seq ] -> emitExpr r t [ seq; idx ] "lists:nth($1 + 1, $0)" |> Some
    // Reuse fable_list.erl
    | "Fold", [ fn; state; seq ] -> Helper.LibCall(com, "fable_list", "fold", t, [ fn; state; seq ]) |> Some
    | "FoldBack", [ fn; seq; state ] -> Helper.LibCall(com, "fable_list", "fold_back", t, [ fn; seq; state ]) |> Some
    | "Reduce", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "reduce", t, [ fn; seq ]) |> Some
    | "Collect", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "collect", t, [ fn; seq ]) |> Some
    | "Choose", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "choose", t, [ fn; seq ]) |> Some
    | "Find", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "find", t, [ fn; seq ]) |> Some
    | "TryFind", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "try_find", t, [ fn; seq ]) |> Some
    | "SumBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "sum_by", t, [ fn; seq ]) |> Some
    | "MinBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "min_by", t, [ fn; seq ]) |> Some
    | "MaxBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "max_by", t, [ fn; seq ]) |> Some
    | "MapIndexed", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "map_indexed", t, [ fn; seq ]) |> Some
    | "Indexed", [ seq ] -> Helper.LibCall(com, "fable_list", "indexed", t, [ seq ]) |> Some
    | "SortBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "sort_by", t, [ fn; seq ]) |> Some
    | "SortByDescending", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "sort_by_descending", t, [ fn; seq ]) |> Some
    | "SortWith", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "sort_with", t, [ fn; seq ]) |> Some
    | "Zip", [ s1; s2 ] -> Helper.LibCall(com, "fable_list", "zip", t, [ s1; s2 ]) |> Some
    // New fable_seq.erl operations
    | ("Delay" | "EnumerateUsing"), [ fn ] -> Helper.LibCall(com, "fable_seq", "delay", t, [ fn ]) |> Some
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
    | "Distinct", [ seq ] -> Helper.LibCall(com, "fable_seq", "distinct", t, [ seq ]) |> Some
    | "DistinctBy", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "distinct_by", t, [ fn; seq ]) |> Some
    | "GroupBy", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "group_by", t, [ fn; seq ]) |> Some
    | "CountBy", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "count_by", t, [ fn; seq ]) |> Some
    | "Except", [ excl; seq ] -> Helper.LibCall(com, "fable_seq", "except", t, [ excl; seq ]) |> Some
    | "FindIndex", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "find_index", t, [ fn; seq ]) |> Some
    | "TryFindIndex", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "try_find_index", t, [ fn; seq ]) |> Some
    | "Map2", [ fn; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "map2", t, [ fn; s1; s2 ]) |> Some
    | "MapIndexed2", [ fn; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "map_indexed2", t, [ fn; s1; s2 ]) |> Some
    | "IterateIndexed", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "iter_indexed", t, [ fn; seq ]) |> Some
    | "Iterate2", [ fn; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "iter2", t, [ fn; s1; s2 ]) |> Some
    | "Fold2", [ fn; state; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "fold2", t, [ fn; state; s1; s2 ]) |> Some
    | "FoldBack2", [ fn; s1; s2; state ] ->
        Helper.LibCall(com, "fable_seq", "fold_back2", t, [ fn; s1; s2; state ]) |> Some
    | "Scan", [ fn; state; seq ] -> Helper.LibCall(com, "fable_seq", "scan", t, [ fn; state; seq ]) |> Some
    | "ScanBack", [ fn; seq; state ] -> Helper.LibCall(com, "fable_seq", "scan_back", t, [ fn; seq; state ]) |> Some
    | "ReduceBack", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "reduce_back", t, [ fn; seq ]) |> Some
    | "ForAll2", [ fn; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "for_all2", t, [ fn; s1; s2 ]) |> Some
    | "Exists2", [ fn; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "exists2", t, [ fn; s1; s2 ]) |> Some
    | "CompareWith", [ fn; s1; s2 ] -> Helper.LibCall(com, "fable_seq", "compare_with", t, [ fn; s1; s2 ]) |> Some
    | "Zip3", [ s1; s2; s3 ] -> Helper.LibCall(com, "fable_seq", "zip3", t, [ s1; s2; s3 ]) |> Some
    | "Pick", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "pick", t, [ fn; seq ]) |> Some
    | "TryPick", [ fn; seq ] -> Helper.LibCall(com, "fable_seq", "try_pick", t, [ fn; seq ]) |> Some
    // Reuse fable_list.erl for operations already implemented there
    | "FindBack", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "find_back", t, [ fn; seq ]) |> Some
    | "TryFindBack", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "try_find_back", t, [ fn; seq ]) |> Some
    | "FindIndexBack", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "find_index_back", t, [ fn; seq ]) |> Some
    | "TryFindIndexBack", [ fn; seq ] ->
        Helper.LibCall(com, "fable_list", "try_find_index_back", t, [ fn; seq ]) |> Some
    | "TryHead", [ seq ] -> Helper.LibCall(com, "fable_list", "try_head", t, [ seq ]) |> Some
    | "Tail", [ seq ] -> emitExpr r t [ seq ] "tl($0)" |> Some
    | "TryItem", [ idx; seq ] -> Helper.LibCall(com, "fable_list", "try_item", t, [ idx; seq ]) |> Some
    | "TryLast", [ seq ] -> Helper.LibCall(com, "fable_list", "try_last", t, [ seq ]) |> Some
    | "ExactlyOne", [ seq ] -> Helper.LibCall(com, "fable_list", "exactly_one", t, [ seq ]) |> Some
    | "TryExactlyOne", [ seq ] -> Helper.LibCall(com, "fable_list", "try_exactly_one", t, [ seq ]) |> Some
    | "MapFold", [ fn; state; seq ] -> Helper.LibCall(com, "fable_list", "map_fold", t, [ fn; state; seq ]) |> Some
    | "MapFoldBack", [ fn; seq; state ] ->
        Helper.LibCall(com, "fable_list", "map_fold_back", t, [ fn; seq; state ])
        |> Some
    | "Average", [ seq ] -> Helper.LibCall(com, "fable_list", "average", t, [ seq ]) |> Some
    | "AverageBy", [ fn; seq ] -> Helper.LibCall(com, "fable_list", "average_by", t, [ fn; seq ]) |> Some
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
    | ("Readonly" | "ReadOnly" | "Cache"), [ seq ] -> Some seq
    | "Where", [ fn; seq ] -> emitExpr r t [ fn; seq ] "lists:filter($0, $1)" |> Some
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
    match info.CompiledName, thisArg, args with
    | "get_Length", Some c, _ -> emitExpr r t [ c ] "length($0)" |> Some
    | "get_Item", Some c, [ idx ] -> emitExpr r t [ c; idx ] "lists:nth($1 + 1, $0)" |> Some
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
    | ("Length" | "Count"), [ arr ] -> emitExpr r t [ arr ] "length($0)" |> Some
    | "Item", [ idx; arr ] -> emitExpr r t [ arr; idx ] "lists:nth($1 + 1, $0)" |> Some
    | "Get", [ arr; idx ] -> emitExpr r t [ arr; idx ] "lists:nth($1 + 1, $0)" |> Some
    | "Head", [ arr ] -> emitExpr r t [ arr ] "hd($0)" |> Some
    | "Last", [ arr ] -> emitExpr r t [ arr ] "lists:last($0)" |> Some
    | "Tail", [ arr ] -> emitExpr r t [ arr ] "tl($0)" |> Some
    | "IsEmpty", [ arr ] -> emitExpr r t [ arr ] "($0 =:= [])" |> Some
    | "Empty", _ -> Value(NewArray(ArrayValues [], t, MutableArray), None) |> Some
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
    // In-place sort operations: since arrays are lists in Erlang, return sorted copy
    | "SortInPlace", [ arr ] -> emitExpr r t [ arr ] "lists:sort($0)" |> Some
    | "SortInPlaceBy", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "sort_by", t, [ fn; arr ]) |> Some
    | "SortInPlaceWith", [ fn; arr ] -> Helper.LibCall(com, "fable_list", "sort_with", t, [ fn; arr ]) |> Some
    | _ -> None

/// Beam-specific OperatorIntrinsics replacements (ranges).
/// F# range expressions like [1..n] compile to RangeInt32(start, step, stop).
/// Erlang's lists:seq(From, To, Step) generates the equivalent list.
let private intrinsicFunctions
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "RangeChar", [ start; stop ] -> emitExpr r t [ start; stop ] "lists:seq($0, $1)" |> Some
    | ("RangeSByte" | "RangeByte" | "RangeInt16" | "RangeUInt16" | "RangeInt32" | "RangeUInt32" | "RangeInt64" | "RangeUInt64" | "RangeSingle" | "RangeDouble"),
      [ start; step; stop ] ->
        // lists:seq(From, To, Step) — args are (start, step, stop)
        emitExpr r t [ start; step; stop ] "lists:seq($0, $2, $1)" |> Some
    | _ -> None

let error (_com: ICompiler) (msg: Expr) = msg

let defaultof (_com: ICompiler) (_ctx: Context) (r: SourceLocation option) (typ: Type) =
    match typ with
    | Boolean -> makeBoolConst false
    | Number(kind, uom) -> NumberConstant(NumberValue.GetZero kind, uom) |> makeValue None
    | Char -> CharConstant '\u0000' |> makeValue None
    | String -> makeStrConst ""
    | _ -> Value(Null typ, r)

let getRefCell (_com: ICompiler) (r: SourceLocation option) (_typ: Type) (expr: Expr) =
    emitExpr r _typ [ expr ] "get($0)"

let setRefCell (_com: ICompiler) (r: SourceLocation option) (expr: Expr) (value: Expr) =
    emitExpr r Unit [ expr; value ] "put($0, $1)"

let makeRefCellFromValue (_com: ICompiler) (r: SourceLocation option) (value: Expr) =
    emitExpr r Any [ value ] "(fun() -> Ref = make_ref(), put(Ref, $0), Ref end)()"

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
    | "get_CancellationToken" -> emitExpr r t [] "undefined" |> Some
    | "Catch" ->
        Helper.LibCall(com, "fable_async", "catch_async", t, args, i.SignatureArgTypes, ?loc = r)
        |> Some
    | meth ->
        Helper.LibCall(com, "fable_async", Naming.lowerFirst meth, t, args, i.SignatureArgTypes, ?loc = r)
        |> Some

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
    // GetEnumerator for MatchCollection iteration
    | "GetEnumerator", Some callee, _ ->
        // Return the list itself; Beam iteration works on lists
        Some callee
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
    | ".ctor", _, [] -> emitExpr r t [] "(fun() -> Ref = make_ref(), put(Ref, []), Ref end)()" |> Some
    | ".ctor", _, [ ExprType(Number _) ] ->
        // Ignore size hint, just create empty
        emitExpr r t [] "(fun() -> Ref = make_ref(), put(Ref, []), Ref end)()" |> Some
    | ".ctor", _, [ arg ] ->
        // From IEnumerable/sequence
        emitExpr r t [ arg ] "(fun() -> Ref = make_ref(), put(Ref, $0), Ref end)()"
        |> Some
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
    | "get_Count", Some callee, _ -> emitExpr r t [ callee ] "length(get($0))" |> Some
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
    // GetRange
    | "GetRange", Some callee, [ idx; count ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "get_range", t, [ listExpr; idx; count ], ?loc = r)
        |> Some
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
    // Find
    | "Find", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "find", t, [ pred; listExpr ], ?loc = r)
        |> Some
    // FindLast
    | "FindLast", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "find_last", t, [ listExpr; pred ], ?loc = r)
        |> Some
    // FindAll
    | "FindAll", Some callee, [ pred ] ->
        let listExpr = emitExpr r (List Any) [ callee ] "get($0)"

        Helper.LibCall(com, "fable_resize_array", "find_all", t, [ listExpr; pred ], ?loc = r)
        |> Some
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

let tryField (_com: ICompiler) _returnTyp ownerTyp fieldName : Expr option =
    match ownerTyp, fieldName with
    | String, "Empty" -> makeStrConst "" |> Some
    | _ -> None

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
    | "System.Math" -> operators com ctx r t info thisArg args
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
        | "GetArray", [ ar; idx ] -> emitExpr r t [ ar; idx ] "lists:nth($1 + 1, $0)" |> Some
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
    | Types.hashset -> hashSets com ctx r t info thisArg args
    | Types.queue -> queues com ctx r t info thisArg args
    | Types.stack -> stacks com ctx r t info thisArg args
    | Types.keyValuePair -> keyValuePairs com ctx r t info thisArg args
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
    | "System.Collections.Generic.HashSet`1.Enumerator" -> enumerators com ctx r t info thisArg args
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
