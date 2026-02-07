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
    | "FailWith", [ msg ]
    | "InvalidOp", [ msg ] -> makeThrow r _t msg |> Some
    | "InvalidArg", [ argName; msg ] ->
        let msg = add (add msg (Value(StringConstant "\\nParameter name: ", None))) argName
        makeThrow r _t msg |> Some
    | "Raise", [ arg ] -> makeThrow r _t arg |> Some
    | (Operators.equality | "Eq"), [ left; right ] -> equals r true left right |> Some
    | (Operators.inequality | "Neq"), [ left; right ] -> equals r false left right |> Some
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
        | Type.String -> emitExpr r _t [ arg ] "binary_to_float($0)" |> Some
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
        | Type.String -> emitExpr r _t [ arg ] "binary_to_float($0)" |> Some
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
    // Erlang has native arbitrary-precision integers, so Int64/UInt64/BigInt
    // use direct binary ops instead of library calls (like Python's int)
    | Patterns.SetContains Operators.standardSet, _ ->
        let argTypes = args |> List.map (fun a -> a.Type)

        match argTypes with
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
    | _ -> None

let private languagePrimitives
    (_com: ICompiler)
    (_ctx: Context)
    r
    (_t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | ("GenericEquality" | "GenericEqualityIntrinsic"), [ left; right ] -> equals r true left right |> Some
    | ("GenericEqualityER" | "GenericEqualityERIntrinsic"), [ left; right ] -> equals r true left right |> Some
    | _ -> None

/// Beam-specific string method replacements.
/// Erlang strings are UTF-8 binaries (<<>>), so we use binary module functions.
let private strings
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, thisArg, args with
    // str.Length → byte_size(Str) — works for ASCII; for full Unicode use string:length
    | "get_Length", Some c, _ -> emitExpr r t [ c ] "byte_size($0)" |> Some
    // str.ToUpper() → string:uppercase(Str)
    | ("ToUpper" | "ToUpperInvariant"), Some c, _ -> emitExpr r t [ c ] "string:uppercase($0)" |> Some
    // str.ToLower() → string:lowercase(Str)
    | ("ToLower" | "ToLowerInvariant"), Some c, _ -> emitExpr r t [ c ] "string:lowercase($0)" |> Some
    // str.Trim() → string:trim(Str)
    | "Trim", Some c, [] -> emitExpr r t [ c ] "string:trim($0)" |> Some
    | "TrimStart", Some c, [] -> emitExpr r t [ c ] "string:trim($0, leading)" |> Some
    | "TrimEnd", Some c, [] -> emitExpr r t [ c ] "string:trim($0, trailing)" |> Some
    // str.StartsWith(prefix) → binary part check
    | "StartsWith", Some c, [ prefix ] ->
        emitExpr r t [ c; prefix ] "(binary:match($0, $1) =:= {0, byte_size($1)})"
        |> Some
    // str.EndsWith(suffix) → binary part check at end
    | "EndsWith", Some c, [ suffix ] ->
        emitExpr
            r
            t
            [ c; suffix ]
            "(binary:match($0, $1, [{scope, {byte_size($0) - byte_size($1), byte_size($1)}}]) =/= nomatch)"
        |> Some
    // str.Substring(start) → binary:part(Str, Start, byte_size(Str) - Start)
    | "Substring", Some c, [ start ] -> emitExpr r t [ c; start ] "binary:part($0, $1, byte_size($0) - $1)" |> Some
    // str.Substring(start, length) → binary:part(Str, Start, Length)
    | "Substring", Some c, [ start; len ] -> emitExpr r t [ c; start; len ] "binary:part($0, $1, $2)" |> Some
    // str.Replace(old, new) → binary replacement via string:replace
    | "Replace", Some c, [ oldVal; newVal ] ->
        emitExpr r t [ c; oldVal; newVal ] "iolist_to_binary(string:replace($0, $1, $2, all))"
        |> Some
    // str.IndexOf(sub) — let it fall through to JS replacements which generates indexOf → handled in Fable2Beam
    // str.Contains(sub) — let it fall through to JS replacements which generates indexOf >= 0
    | _ -> None

/// Beam-specific Option module replacements.
/// Options in Erlang: None = undefined atom, Some(x) = x.
let private optionModule
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "DefaultValue", [ defVal; opt ] ->
        // Option.defaultValue defVal opt → case Opt of undefined -> DefVal; _ -> Opt end
        // F# sends args as [defVal; opt] (defaultValue first, then option)
        emitExpr r t [ opt; defVal ] "case $0 of undefined -> $1; Opt_val_ -> Opt_val_ end"
        |> Some
    | "DefaultWith", [ defFn; opt ] ->
        // F# sends args as [defFn; opt]
        emitExpr r t [ opt; defFn ] "case $0 of undefined -> ($1)(ok); Opt_val_ -> Opt_val_ end"
        |> Some
    | "Map", [ fn; opt ] ->
        emitExpr r t [ fn; opt ] "case $1 of undefined -> undefined; Opt_val_ -> ($0)(Opt_val_) end"
        |> Some
    | "Bind", [ fn; opt ] ->
        emitExpr r t [ fn; opt ] "case $1 of undefined -> undefined; Opt_val_ -> ($0)(Opt_val_) end"
        |> Some
    | "IsSome", [ c ] -> Test(c, OptionTest true, r) |> Some
    | "IsNone", [ c ] -> Test(c, OptionTest false, r) |> Some
    | "GetValue", [ c ] ->
        // Option.get / Option.value — just return the value (will crash on undefined at runtime)
        Some c
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

/// Beam-specific type conversion replacements.
/// Handles int(), float(), string(), ToString, Parse, etc.
let private conversions
    (_com: ICompiler)
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
        | Type.String -> emitExpr r t [ arg ] "binary_to_float($0)" |> Some
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

/// Beam-specific numeric type method replacements (Parse, ToString on numeric types).
let private numericTypes
    (_com: ICompiler)
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
        | "System.Decimal" -> emitExpr r t [ arg ] "binary_to_float($0)" |> Some
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
    | _ -> None

/// Beam-specific System.Convert replacements.
let private convert
    (_com: ICompiler)
    (_ctx: Context)
    r
    (t: Type)
    (info: CallInfo)
    (_thisArg: Expr option)
    (args: Expr list)
    =
    match info.CompiledName, args with
    | "ToInt32", [ arg ] ->
        match arg.Type with
        | Type.String -> emitExpr r t [ arg ] "binary_to_integer($0)" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r t [ arg ] "trunc($0)" |> Some
            | _ -> Some arg
        | _ -> Some arg
    | "ToInt64", [ arg ] ->
        match arg.Type with
        | Type.String -> emitExpr r t [ arg ] "binary_to_integer($0)" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> emitExpr r t [ arg ] "trunc($0)" |> Some
            | _ -> Some arg
        | _ -> Some arg
    | "ToDouble", [ arg ] ->
        match arg.Type with
        | Type.String -> emitExpr r t [ arg ] "binary_to_float($0)" |> Some
        | Type.Number(kind, _) ->
            match kind with
            | Float16
            | Float32
            | Float64
            | Decimal -> Some arg
            | _ -> emitExpr r t [ arg ] "float($0)" |> Some
        | _ -> emitExpr r t [ arg ] "float($0)" |> Some
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

let tryField (_com: ICompiler) _returnTyp _ownerTyp _fieldName : Expr option = None

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
    | "Microsoft.FSharp.Core.ExtraTopLevelOperators" -> operators com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.LanguagePrimitives"
    | "Microsoft.FSharp.Core.LanguagePrimitives.HashCompare" -> languagePrimitives com ctx r t info thisArg args
    | Types.string -> strings com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.FSharpOption`1"
    | "Microsoft.FSharp.Core.FSharpValueOption`1" -> options com ctx r t info thisArg args
    | "Microsoft.FSharp.Core.OptionModule"
    | "Microsoft.FSharp.Core.ValueOptionModule" -> optionModule com ctx r t info thisArg args
    | "System.Object"
    | "System.ValueType" -> conversions com ctx r t info thisArg args
    | "System.Convert" -> convert com ctx r t info thisArg args
    | "System.Char" -> conversions com ctx r t info thisArg args
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
