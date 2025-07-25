namespace Fable.Py

open Fable
open System

module Naming =
    open Fable.Core
    open System.Text.RegularExpressions

    [<Literal>]
    let fableLibPyPI = "fable-library"

    let lowerFirst (s: string) =
        if String.IsNullOrEmpty(s) then
            s
        else
            s.Substring(0, 1).ToLowerInvariant() + s.Substring(1)

    let upperFirst (s: string) =
        if String.IsNullOrEmpty(s) then
            s
        else
            s.Substring(0, 1).ToUpperInvariant() + s.Substring(1)

    let toCamelCase (name: string) =
        Naming.applyCaseRule CaseRules.LowerFirst name

    let toSnakeCase (name: string) =
        Naming.applyCaseRule CaseRules.SnakeCase name

    /// Convert F# record field name to snake_case with special handling for camelCase/PascalCase conflicts.
    /// - If the name is PascalCase, convert to snake_case without suffix.
    /// - If the name is camelCase, convert to snake_case and add '_' suffix to avoid conflict with PascalCase.
    let toRecordFieldSnakeCase (name: string) =
        let snakeCase = Naming.applyCaseRule CaseRules.SnakeCase name

        if name.Length > 0 && Char.IsLower(name.[0]) then
            snakeCase + "_"
        else
            snakeCase

    let toPascalCase (name: string) = upperFirst name

    /// Convert name to Python naming convention.
    /// - If the name starts with a lowercase letter, convert it to snake_case.
    /// - If the name starts with an uppercase letter, preserve case as is.
    let toPythonNaming (name: string) =
        if name.Length > 0 && Char.IsLower(name.[0]) then
            Naming.applyCaseRule CaseRules.SnakeCase name
        else
            name

    let cleanNameAsPyIdentifier (name: string) =
        name.Replace('.', '_').Replace('`', '_')

    let pyKeywords =
        // https://docs.python.org/3/reference/lexical_analysis.html#keywords

        System.Collections.Generic.HashSet
            [
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
            ]

    // Other global builtins we should avoid https://docs.python.org/3/library/functions.html
    let pyBuiltins =
        System.Collections.Generic.HashSet
            [
                "abs"
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

    let pyStdlib =
        System.Collections.Generic.HashSet
            [
                "abc"
                "asyncio"
                "array"
                "base64"
                "builtins"
                "collections"
                "dataclasses"
                "datetime"
                "decimal"
                "enum"
                "functools"
                "inspect"
                "itertools"
                "io"
                "locale"
                "math"
                "operator"
                "os"
                "pathlib"
                "platform"
                "queue"
                "random"
                "re"
                "readline"
                "posix"
                "string"
                "struct"
                "sys"
                "tempfile"
                "threading"
                "time"
                "typing"
                "unicodedata"
                "urllib"
                "uuid"
                "warnings"
            ]

    let reflectionSuffix = "_reflection"

    let mutable uniqueIndex = 0

    let getUniqueIndex () =
        let idx = uniqueIndex
        uniqueIndex <- uniqueIndex + 1
        idx

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

    let isIdentChar index (c: char) =
        // Digits are not allowed in first position, see #1397
        c = '_' || Char.IsLetter(c) || Char.IsDigit(c) && index > 0

    let hasIdentForbiddenChars (ident: string) =
        let mutable found = false

        for i = 0 to ident.Length - 1 do
            found <- found || not (isIdentChar i ident.[i])

        found

    let sanitizeIdentForbiddenChars (ident: string) =
        if hasIdentForbiddenChars ident then
            String.Concat(
                seq {
                    for i = 0 to (ident.Length - 1) do
                        let c = ident.[i]

                        if isIdentChar i c then
                            string<char> c
                        elif c = '$' || c = '_' || c = ' ' || c = '*' || c = '.' || c = '`' then
                            "_"
                        else
                            "_" + String.Format("{0:X}", int c).PadLeft(4, '0')
                }
            )
        else
            ident

    let checkPyKeywords name =
        if pyKeywords.Contains name then
            name + "_"
        else
            name

    let checkPyStdlib name =
        if pyStdlib.Contains name then
            name + "_"
        else
            name

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
            | Naming.InstanceMemberPart(s, i) -> printPart sanitize "__" s i
            | Naming.StaticMemberPart(s, i) -> printPart sanitize "_" s i
            | Naming.NoMemberPart -> ""
        )

    let sanitizeIdent conflicts (name: string) part =
        // Replace Forbidden Chars
        buildName sanitizeIdentForbiddenChars name part
        |> checkPyKeywords
        // Check if it already exists
        |> preventConflicts conflicts

    /// Convert name to Python naming convention.
    /// Removes @ suffixes and applies standard Python naming rules.
    let toPropertyNaming (name: string) =
        let pythonName = toPythonNaming name
        // Remove @ suffix
        let cleanName =
            if pythonName.EndsWith("@", StringComparison.Ordinal) then
                pythonName.Substring(0, pythonName.Length - 1)
            else
                pythonName
        // Apply standard sanitization
        sanitizeIdent pyBuiltins.Contains cleanName Naming.NoMemberPart

    let toPropertyBackingFieldNaming (name: string) =
        let propertyName = name |> toPropertyNaming
        $"_{Naming.applyCaseRule CaseRules.SnakeCase propertyName}"
