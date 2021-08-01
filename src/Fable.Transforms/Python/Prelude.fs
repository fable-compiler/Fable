namespace Fable.Python

open Fable

module Naming =
    open Fable.Core
    open System.Text.RegularExpressions

    let lowerFirst (s: string) =
        s.Substring(0,1).ToLowerInvariant() + s.Substring(1)

    let upperFirst (s: string) =
        s.Substring(0,1).ToUpperInvariant() + s.Substring(1)

    let private dashify (separator: string) (input: string) =
        Regex.Replace(input, "[a-z]?[A-Z]", fun m ->
            if m.Value.Length = 1 then m.Value.ToLowerInvariant()
            else m.Value.Substring(0,1) + separator + m.Value.Substring(1,1).ToLowerInvariant())

    let applyCaseRule caseRule name =
        match caseRule with
        | CaseRules.LowerFirst -> lowerFirst name
        | CaseRules.SnakeCase -> dashify "_" name
        | CaseRules.SnakeCaseAllCaps -> (dashify "_" name).ToUpperInvariant()
        | CaseRules.KebabCase -> dashify "-" name
        | CaseRules.None | _ -> name

    let pyKeywords =
        // https://docs.python.org/3/reference/lexical_analysis.html#keywords

        System.Collections.Generic.HashSet [
            "False"
            "await"
            "else"
            "import"
            "pass"
            "None"
            "break"
            "except"
            "in"
            "raise"
            "True"
            "class"
            "finally"
            "is"
            "return"
            "and"
            "continue"
            "for"
            "lambda"
            "try"
            "as"
            "def"
            "from"
            "nonlocal"
            "while"
            "assert"
            "del"
            "global"
            "not"
            "with"
            "async"
            "elif"
            "if"
            "or"
            "yield"

            // Other globals we should stay away from https://docs.python.org/3/library/functions.html
            "len"
            "str"
            "int"
            "float"
            "set"
            "enumerate"
            "next"
            "super"
            "callable"
            "hash"
            "classmethod"
            "staticmethod"
            "list"
            "dict"
            "bool"
            "isinstance"
            "issubclass"
            "hasattr"
            "getattr"

            // Other names
            "self"
        ]

    let preventConflicts conflicts originalName =
        let rec check originalName n =
            let name = if n > 0 then originalName + "_" + (string n) else originalName
            if not (conflicts name) then name else check originalName (n+1)
        check originalName 0

    let isIdentChar index (c: char) =
        let code = int c
        c = '_'
        || (65 <= code && code <= 90)   // a-z
        || (97 <= code && code <= 122)  // A-Z
        // Digits are not allowed in first position, see #1397
        || (index > 0 && 48 <= code && code <= 57) // 0-9

    let hasIdentForbiddenChars (ident: string) =
        let mutable found = false
        for i = 0 to ident.Length - 1 do
            found <- found || not(isIdentChar i ident.[i])
        found

    let sanitizeIdentForbiddenChars (ident: string) =
        if hasIdentForbiddenChars ident then
            System.String.Concat(seq {
                for i = 0 to (ident.Length - 1) do
                    let c = ident.[i]
                    if isIdentChar i c
                    then string c
                    elif c = '$'
                    then "_"
                    else "_" + System.String.Format("{0:X}", int c).PadLeft(4, '0')
                })
        else ident

    let checkPyKeywords name =
        if pyKeywords.Contains name
        then name + "_"
        else name

    let private printPart sanitize separator part overloadSuffix =
        (if part = "" then "" else separator + (sanitize part)) +
            (if overloadSuffix = "" then "" else "_" + overloadSuffix)

    let private buildName sanitize name part =
        (sanitize name) +
            (match part with
                | Naming.InstanceMemberPart(s, i) -> printPart sanitize "__" s i
                | Naming.StaticMemberPart(s, i) -> printPart sanitize "_" s i
                | Naming.NoMemberPart -> "")

    let sanitizeIdent conflicts name part =
        let result =
            // Replace Forbidden Chars
            buildName sanitizeIdentForbiddenChars name part
            |> checkPyKeywords
            // Check if it already exists
            |> preventConflicts conflicts
        result