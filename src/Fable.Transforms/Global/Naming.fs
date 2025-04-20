namespace Fable

open System

module Naming =
    open Fable.Core
    open System.Text.RegularExpressions

    let (|StartsWith|_|) (pattern: string) (txt: string) =
        if txt.StartsWith(pattern, StringComparison.Ordinal) then
            txt.Substring(pattern.Length) |> Some
        else
            None

    let (|EndsWith|_|) (pattern: string) (txt: string) =
        if txt.EndsWith(pattern, StringComparison.Ordinal) then
            txt.Substring(0, txt.Length - pattern.Length) |> Some
        else
            None

    let (|Regex|_|) (reg: Regex) (str: string) =
        let m = reg.Match(str)

        if m.Success then
            m.Groups |> Seq.cast<Group> |> Seq.map (fun g -> g.Value) |> Seq.toList |> Some
        else
            None

    [<Literal>]
    let fableCompilerConstant = "FABLE_COMPILER"

    [<Literal>]
    let placeholder = "__PLACE-HOLDER__"

    [<Literal>]
    let fableModules = "fable_modules"

    [<Literal>]
    let fableRegion = "FABLE_REGION"

    [<Literal>]
    let fablePrecompile = "Fable.Precompiled"

    [<Literal>]
    let fableProjExt = ".fableproj"

    [<Literal>]
    let unknown = "UNKNOWN"

    let isInFableModules (file: string) =
        file.Split([| '\\'; '/' |]) |> Array.exists ((=) fableModules)

    let isIdentChar index (c: char) =
        let code = int c

        c = '_'
        || c = '$'
        || (65 <= code && code <= 90) // a-z
        || (97 <= code && code <= 122) // A-Z
        // Digits are not allowed in first position, see #1397
        || (index > 0 && 48 <= code && code <= 57) // 0-9
        || match Compiler.Language with
           | Dart -> false
           | _ -> Char.IsLetter c

    let hasIdentForbiddenChars (ident: string) =
        let mutable found = false

        for i = 0 to ident.Length - 1 do
            found <- found || not (isIdentChar i ident.[i])

        found

    let sanitizeIdentForbiddenCharsWith replace (ident: string) =
        if hasIdentForbiddenChars ident then
            Seq.init
                ident.Length
                (fun i ->
                    let c = ident.[i]

                    if isIdentChar i c then
                        string<char> c
                    else
                        replace c
                )
            |> String.Concat
        else
            ident

    let sanitizeIdentForbiddenChars (ident: string) =
        ident
        |> sanitizeIdentForbiddenCharsWith (fun c -> "$" + String.Format("{0:X}", int c).PadLeft(4, '0'))

    let replaceRegex (pattern: string) (value: string) (input: string) = Regex.Replace(input, pattern, value)

    let replacePrefix (prefix: string) (value: string) (input: string) =
        if input.StartsWith(prefix, StringComparison.Ordinal) then
            value + (input.Substring(prefix.Length))
        else
            input

    let replaceSuffix (suffix: string) (value: string) (input: string) =
        if input.EndsWith(suffix, StringComparison.Ordinal) then
            (input.Substring(0, input.Length - suffix.Length)) + value
        else
            input

    let removeGetSetPrefix (s: string) =
        if
            s.StartsWith("get_", StringComparison.Ordinal)
            || s.StartsWith("set_", StringComparison.Ordinal)
        then
            s.Substring(4)
        else
            s

    let extensionMethodName (s: string) =
        let i1 = s.IndexOf(".", StringComparison.Ordinal)

        if i1 < 0 then
            s
        else
            let i2 = s.IndexOf(".", i1 + 1, StringComparison.Ordinal)

            if i2 < 0 then
                s
            else
                s.Substring(i1 + 1, i2 - i1 - 1)

    let splitFirstBy (sep: string) (s: string) =
        let i = s.IndexOf(sep, StringComparison.Ordinal)

        if i < 0 then
            s, ""
        else
            s.Substring(0, i), s.Substring(i + sep.Length)

    let splitLastBy (sep: string) (s: string) =
        let i = s.LastIndexOf(sep, StringComparison.Ordinal)

        if i < 0 then
            "", s
        else
            s.Substring(0, i), s.Substring(i + sep.Length)

    let splitLast (s: string) = splitLastBy "." s |> snd

    let lowerFirst (s: string) =
        s.Substring(0, 1).ToLowerInvariant() + s.Substring(1)

    let upperFirst (s: string) =
        s.Substring(0, 1).ToUpperInvariant() + s.Substring(1)

    let xmlDecode (text: string) =
#if !FABLE_COMPILER
        System.Web.HttpUtility.HtmlDecode(text)
#else
        text
            .Replace("&lt;", "<")
            .Replace("&gt;", ">")
            .Replace("&amp;", "&")
            .Replace("&apos;", "'")
            .Replace("&quot;", "\"")
#endif

    let private dashify (separator: string) (input: string) =
        Regex.Replace(
            input,
            "[a-z]?[A-Z]",
            fun m ->
                if m.Value.Length = 1 then
                    m.Value.ToLowerInvariant()
                else
                    m.Value.Substring(0, 1) + separator + m.Value.Substring(1, 1).ToLowerInvariant()
        )

    let applyCaseRule caseRule name =
        match caseRule with
        | CaseRules.LowerFirst -> lowerFirst name
        | CaseRules.SnakeCase -> dashify "_" name
        | CaseRules.SnakeCaseAllCaps -> (dashify "_" name).ToUpperInvariant()
        | CaseRules.KebabCase -> dashify "-" name
        | CaseRules.LowerAll -> name.ToLowerInvariant()
        | CaseRules.None
        | _ -> name

    // TODO: Reserved words for other languages
    // Dart: List, identical...

    let jsKeywords =
        System.Collections.Generic.HashSet
            [
                // Keywords: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
                "break"
                "case"
                "catch"
                "class"
                "const"
                "continue"
                "debugger"
                "default"
                "delete"
                "do"
                "else"
                "export"
                "extends"
                "finally"
                "for"
                "function"
                "if"
                "import"
                "in"
                "instanceof"
                "new"
                "return"
                "super"
                "switch"
                "this"
                "throw"
                "try"
                "typeof"
                "var"
                "void"
                "while"
                "with"
                "yield"

                "enum"

                "implements"
                "interface"
                "let"
                "package"
                "private"
                "protected"
                "public"
                "static"

                "await"

                "null"
                "true"
                "false"
                "arguments"
                "get"
                "set"

                // Standard built-in objects: https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects
                "Infinity"
                "NaN"
                "undefined"
                "globalThis"

                "eval"
                "uneval"
                "isFinite"
                "isNaN"
                "parseFloat"
                "parseInt"
                "decodeURI"
                "decodeURIComponent"
                "encodeURI"
                "encodeURIComponent"

                "Object"
                "Function"
                "Boolean"
                "Symbol"

                "Error"
                "AggregateError"
                "EvalError"
                "InternalError"
                "RangeError"
                "ReferenceError"
                "SyntaxError"
                "TypeError"
                "URIError"

                "Number"
                "BigInt"
                "Math"
                "Date"

                "String"
                "RegExp"

                "Array"
                "Int8Array"
                "Uint8Array"
                "Uint8ClampedArray"
                "Int16Array"
                "Uint16Array"
                "Int32Array"
                "Uint32Array"
                "Float32Array"
                "Float64Array"
                "BigInt64Array"
                "BigUint64Array"

                "Map"
                "Set"
                "WeakMap"
                "WeakSet"

                "ArrayBuffer"
                "SharedArrayBuffer"
                "Atomics"
                "DataView"
                "JSON"

                "Promise"
                "Generator"
                "GeneratorFunction"
                "AsyncFunction"

                "Reflect"
                "Proxy"

                "Intl"
                "WebAssembly"

                // DOM interfaces (omitting SVG): https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model
                "Attr"
                "CDATASection"
                "CharacterData"
                "ChildNode"
                "Comment"
                "CustomEvent"
                "Document"
                "DocumentFragment"
                "DocumentType"
                "DOMError"
                "DOMException"
                "DOMImplementation"
                "DOMString"
                "DOMTimeStamp"
                "DOMStringList"
                "DOMTokenList"
                "Element"
                "Event"
                "EventTarget"
                "HTMLCollection"
                "MutationObserver"
                "MutationRecord"
                "NamedNodeMap"
                "Node"
                "NodeFilter"
                "NodeIterator"
                "NodeList"
                "NonDocumentTypeChildNode"
                "ParentNode"
                "ProcessingInstruction"
                "Selection"
                "Range"
                "Text"
                "TextDecoder"
                "TextEncoder"
                "TimeRanges"
                "TreeWalker"
                "URL"
                "Window"
                "Worker"
                "XMLDocument"

                // Other JS global and special objects/functions. See #258, #1358
                "console"
                "window"
                "document"
                "global"
                "fetch"
            ]

    let preventConflicts conflicts originalName =
        let rec check originalName n =
            let name =
                if n > 0 then
                    originalName + "_" + (string<int> n)
                else
                    originalName

            if not (conflicts name) then
                name
            else
                check originalName (n + 1)

        check originalName 0

    // TODO: Move this to FSharp2Fable.Util
    type MemberPart =
        | InstanceMemberPart of memberCompiledName: string * overloadSuffix: string
        | StaticMemberPart of memberCompiledName: string * overloadSuffix: string
        | NoMemberPart

        member this.Replace(f: string -> string) =
            match this with
            | InstanceMemberPart(s, o) -> InstanceMemberPart(f s, o)
            | StaticMemberPart(s, o) -> StaticMemberPart(f s, o)
            | NoMemberPart -> this

        member this.OverloadSuffix =
            match this with
            | InstanceMemberPart(_, o)
            | StaticMemberPart(_, o) -> o
            | NoMemberPart -> ""

    let reflectionSuffix = "_$reflection"

    let private printPart sanitize separator part overloadSuffix =
        (if part = "" then
             ""
         else
             separator + (sanitize part))
        + (if overloadSuffix = "" then
               ""
           else
               "_" + overloadSuffix)

    let private buildName sanitize name part =
        (sanitize name)
        + (
            match part with
            | InstanceMemberPart(s, i) -> printPart sanitize "__" s i
            | StaticMemberPart(s, i) -> printPart sanitize "_" s i
            | NoMemberPart -> ""
        )

    let buildNameWithoutSanitation name part = buildName id name part

    /// This helper is intended for instance and static members in fable-library library compiled from F# (FSharpSet, FSharpMap...)
    let buildNameWithoutSanitationFrom (entityName: string) isStatic memberCompiledName overloadSuffix =
        (if isStatic then
             entityName, StaticMemberPart(memberCompiledName, overloadSuffix)
         else
             entityName, InstanceMemberPart(memberCompiledName, overloadSuffix))
        ||> buildName id

    let checkJsKeywords name =
        if jsKeywords.Contains name then
            name + "$"
        else
            name

    let sanitizeIdent conflicts name part =
        // Replace Forbidden Chars
        buildName sanitizeIdentForbiddenChars name part
        |> checkJsKeywords
        // Check if it already exists
        |> preventConflicts conflicts

    // Ported to F# from https://github.com/microsoft/referencesource/blob/master/System.Web/Util/HttpEncoder.cs#L391
    let escapeString charRequiresEncoding (value: string) : string =
        if (String.IsNullOrEmpty(value)) then
            String.Empty
        else
            let sb = System.Text.StringBuilder(value.Length)

            for i = 0 to value.Length - 1 do
                match value.[i] with
                | '\'' -> sb.Append("\\\'") |> ignore
                | '\"' -> sb.Append("\\\"") |> ignore
                | '\\' -> sb.Append("\\\\") |> ignore
                | '\r' -> sb.Append("\\r") |> ignore
                | '\t' -> sb.Append("\\t") |> ignore
                | '\n' -> sb.Append("\\n") |> ignore
                | '\b' -> sb.Append("\\b") |> ignore
                | '\f' -> sb.Append("\\f") |> ignore
                | c when
                    charRequiresEncoding c
                    || c < (char) 0x20 // other control chars
                    || c = '\u0085' // other newline chars
                    || c = '\u2028'
                    || c = '\u2029'
                    ->
                    let u = String.Format(@"\u{0:x4}", int c)
                    sb.Append(u) |> ignore
                | c -> sb.Append(c) |> ignore

            sb.ToString()
