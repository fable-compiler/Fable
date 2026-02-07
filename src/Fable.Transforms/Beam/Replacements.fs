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

/// Beam-specific Array instance method replacements.
/// Arrays in Erlang are represented as lists.
let private arrays
    (_com: ICompiler)
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
    | _ -> None

/// Beam-specific Array module replacements.
let private arrayModule
    (_com: ICompiler)
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
    | "Filter", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:filter($0, $1)" |> Some
    | "Exists", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:any($0, $1)" |> Some
    | "ForAll", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:all($0, $1)" |> Some
    | "Iterate", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:foreach($0, $1)" |> Some
    | "Fold", [ fn; state; arr ] ->
        // F# Array.fold: folder(state, item), Erlang foldl: fun(item, acc) — must swap
        emitExpr r t [ fn; state; arr ] "lists:foldl(fun(X_item_, X_acc_) -> ($0)(X_acc_, X_item_) end, $1, $2)"
        |> Some
    | "FoldBack", [ fn; arr; state ] ->
        emitExpr r t [ fn; arr; state ] "lists:foldr(fun(X_item_, X_acc_) -> ($0)(X_item_, X_acc_) end, $2, $1)"
        |> Some
    | "Reduce", [ fn; arr ] ->
        emitExpr r t [ fn; arr ] "lists:foldl(fun(X_item_, X_acc_) -> ($0)(X_acc_, X_item_) end, hd($1), tl($1))"
        |> Some
    | "Reverse", [ arr ] -> emitExpr r t [ arr ] "lists:reverse($0)" |> Some
    | "Append", [ arr1; arr2 ] -> emitExpr r t [ arr1; arr2 ] "lists:append($0, $1)" |> Some
    | "Concat", [ arrs ] -> emitExpr r t [ arrs ] "lists:append($0)" |> Some
    | "Sum", [ arr ] -> emitExpr r t [ arr ] "lists:sum($0)" |> Some
    | "SumBy", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:sum(lists:map($0, $1))" |> Some
    | "Min", [ arr ] -> emitExpr r t [ arr ] "lists:min($0)" |> Some
    | "Max", [ arr ] -> emitExpr r t [ arr ] "lists:max($0)" |> Some
    | "MinBy", [ fn; arr ] ->
        emitExpr r t [ fn; arr ] "element(2, lists:min(lists:map(fun(X_e_) -> {($0)(X_e_), X_e_} end, $1)))"
        |> Some
    | "MaxBy", [ fn; arr ] ->
        emitExpr r t [ fn; arr ] "element(2, lists:max(lists:map(fun(X_e_) -> {($0)(X_e_), X_e_} end, $1)))"
        |> Some
    | "Contains", [ value; arr ] -> emitExpr r t [ value; arr ] "lists:member($0, $1)" |> Some
    | "Find", [ fn; arr ] ->
        emitExpr
            r
            t
            [ fn; arr ]
            "case lists:dropwhile(fun(X_e_) -> not ($0)(X_e_) end, $1) of [H_|_] -> H_; [] -> erlang:error(<<\"key_not_found\">>) end"
        |> Some
    | "TryFind", [ fn; arr ] ->
        emitExpr
            r
            t
            [ fn; arr ]
            "case lists:dropwhile(fun(X_e_) -> not ($0)(X_e_) end, $1) of [H_|_] -> H_; [] -> undefined end"
        |> Some
    | "Choose", [ fn; arr ] ->
        emitExpr
            r
            t
            [ fn; arr ]
            "lists:filtermap(fun(X_e_) -> case ($0)(X_e_) of undefined -> false; V_ -> {true, V_} end end, $1)"
        |> Some
    | "Collect", [ fn; arr ] -> emitExpr r t [ fn; arr ] "lists:append(lists:map($0, $1))" |> Some
    | "ToList", [ arr ] -> Some(List.head args) // arrays and lists are the same in Erlang
    | "OfList", [ lst ] -> Some(List.head args) // arrays and lists are the same in Erlang
    | "OfSeq", [ seq ] -> Some(List.head args)
    | "ToSeq", [ arr ] -> Some(List.head args)
    | "ZeroCreate", [ count ] -> emitExpr r t [ count ] "lists:duplicate($0, 0)" |> Some
    | "Create", [ count; value ] -> emitExpr r t [ count; value ] "lists:duplicate($0, $1)" |> Some
    | "Sort", [ arr ] -> emitExpr r t [ arr ] "lists:sort($0)" |> Some
    | "SortDescending", [ arr ] -> emitExpr r t [ arr ] "lists:reverse(lists:sort($0))" |> Some
    | "SortBy", [ fn; arr ] ->
        emitExpr r t [ fn; arr ] "lists:sort(fun(A_, B_) -> ($0)(A_) =< ($0)(B_) end, $1)"
        |> Some
    | "SortWith", [ fn; arr ] ->
        emitExpr r t [ fn; arr ] "lists:sort(fun(A_, B_) -> ($0)(A_, B_) =< 0 end, $1)"
        |> Some
    | "Zip", [ arr1; arr2 ] ->
        emitExpr r t [ arr1; arr2 ] "lists:zipwith(fun(A_, B_) -> {A_, B_} end, $0, $1)"
        |> Some
    | "Unzip", [ arr ] -> emitExpr r t [ arr ] "lists:unzip($0)" |> Some
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
    | "System.Array" -> arrays com ctx r t info thisArg args
    | "Microsoft.FSharp.Collections.ArrayModule"
    | "Microsoft.FSharp.Collections.ArrayModule.Parallel" -> arrayModule com ctx r t info thisArg args
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
