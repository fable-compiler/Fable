namespace Fable.Beam

module Naming =
    open System.Text.RegularExpressions

    // https://www.erlang.org/doc/system/expressions#reserved-words
    let erlKeywords =
        System.Collections.Generic.HashSet
            [
                "after"
                "and"
                "andalso"
                "band"
                "begin"
                "bnot"
                "bor"
                "bsl"
                "bsr"
                "bxor"
                "case"
                "catch"
                "cond"
                "div"
                "end"
                "fun"
                "if"
                "let"
                "not"
                "of"
                "or"
                "orelse"
                "receive"
                "rem"
                "try"
                "when"
                "xor"
                "maybe"
                "else"
            ]

    /// Auto-imported BIFs that cause "ambiguous call" errors when used as
    /// module-level function names. These need no_auto_import in generated modules.
    let erlAutoImportedBifs =
        System.Collections.Generic.HashSet
            [ "apply"; "now"; "self"; "throw"; "exit"; "error"; "spawn"; "link"; "monitor" ]

    let checkErlKeywords name =
        if erlKeywords.Contains name then
            name + "_"
        else
            name

    let toSnakeCase (name: string) =
        let sb = System.Text.StringBuilder()

        for i = 0 to name.Length - 1 do
            let c = name.[i]

            if System.Char.IsUpper(c) then
                if i > 0 then
                    sb.Append('_') |> ignore

                sb.Append(System.Char.ToLowerInvariant(c)) |> ignore
            else
                sb.Append(c) |> ignore

        sb.ToString()

    let sanitizeErlangName (name: string) =
        // Decode $XXXX hex sequences from F# compiled names (e.g. $0020 -> space -> _)
        Regex.Replace(
            name,
            @"\$([0-9A-Fa-f]{4})",
            fun m ->
                let c = char (System.Convert.ToInt32(m.Groups.[1].Value, 16))

                if System.Char.IsLetterOrDigit(c) then
                    c.ToString()
                else
                    "_"
        )
        |> fun s ->
            s.Replace("'", "").Replace("$", "_").Replace("@", "").Replace(".", "_").Replace("`", "_").Replace("-", "_")
        |> toSnakeCase
        |> fun s -> Regex.Replace(s, "_+", "_")
        |> fun s -> s.Trim('_')
        |> checkErlKeywords

    let moduleNameFromFile (filePath: string) =
        Fable.Path.GetFileNameWithoutExtension(filePath)
        |> fun s -> s.Replace(".", "_").Replace("-", "_")
        |> Fable.Naming.applyCaseRule Fable.Core.CaseRules.SnakeCase

    let capitalizeFirst (s: string) =
        if s.Length = 0 then
            s
        else
            s.[0..0].ToUpperInvariant() + s.[1..]

    /// Convert a field name to a safe Erlang atom (snake_case + keyword escaping).
    /// camelCase names get a trailing '_' to avoid collision with PascalCase names
    /// (e.g., firstName -> first_name_, FirstName -> first_name).
    let sanitizeFieldName (name: string) =
        let snakeName = toSnakeCase name

        let disambiguated =
            if name.Length > 0 && System.Char.IsLower(name.[0]) then
                snakeName + "_"
            else
                snakeName

        checkErlKeywords disambiguated

    /// Quote an Erlang atom if it needs quoting (starts with uppercase, contains special chars, etc.)
    let quoteErlangAtom (name: string) =
        if name.Length = 0 then
            "''"
        elif
            System.Char.IsLower(name.[0])
            && name
               |> Seq.forall (fun c -> System.Char.IsLetterOrDigit(c) || c = '_' || c = '@')
        then
            name
        else
            $"'%s{name}'"

    let sanitizeErlangVar (name: string) =
        // Remove/replace characters invalid in Erlang variable names
        name.Replace("$", "_").Replace("@", "_")
        |> fun s -> Regex.Replace(s, "[^a-zA-Z0-9_]", "_")

/// Fixed-width integer semantics. Erlang integers are arbitrary precision and never
/// overflow, so operations that can leave the width of a .NET sized integer are routed
/// through the `fable_int` runtime module to truncate them back (two's complement).
module Integers =
    open Fable.AST // NumberKind
    open Fable.AST.Fable

    /// Bit width and signedness of a .NET sized integer type. `None` for types whose
    /// Erlang representation is legitimately unbounded (bigint, the fixed-scale decimal)
    /// or not an integer at all — those must never be wrapped.
    let sizedIntInfo (typ: Type) : (int * bool) option =
        match typ with
        // Chars are integers on Erlang, and .NET truncates conversions into them like a uint16
        | Type.Char -> Some(16, false)
        | Type.Number(kind, _) ->
            match kind with
            | Int8 -> Some(8, true)
            | UInt8 -> Some(8, false)
            | Int16 -> Some(16, true)
            | UInt16 -> Some(16, false)
            | Int32 -> Some(32, true)
            | UInt32 -> Some(32, false)
            | Int64 -> Some(64, true)
            | UInt64 -> Some(64, false)
            | NativeInt -> Some(64, true)
            | UNativeInt -> Some(64, false)
            | _ -> None
        | _ -> None

    /// Name of the `fable_int` function that wraps a value to the given width/signedness.
    let wrapFunctionName (bits: int, signed: bool) =
        let prefix =
            if signed then
                "i"
            else
                "u"

        $"wrap_%s{prefix}%d{bits}"

    /// Whether converting a value of type `source` to type `target` can leave `target`'s
    /// range, i.e. whether the conversion needs truncating.
    let conversionNeedsWrap (source: Type) (target: Type) =
        match sizedIntInfo target, sizedIntInfo source with
        | None, _ -> false
        | Some _, None -> true // unbounded or non-integer source (float, bigint, decimal)
        | Some(targetBits, targetSigned), Some(sourceBits, sourceSigned) ->
            if sourceSigned = targetSigned then
                sourceBits > targetBits
            elif targetSigned then
                sourceBits >= targetBits // unsigned source only fits in a strictly wider signed target
            else
                true // a signed source can be negative, which never fits an unsigned target
