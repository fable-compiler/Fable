namespace Fable.Go

open Fable
open System

module Naming =
    open Fable.Core
    open System.Text.RegularExpressions

    let lowerFirst (s: string) =
        s.Substring(0, 1).ToLowerInvariant()
        + s.Substring(1)

    let upperFirst (s: string) =
        s.Substring(0, 1).ToUpperInvariant()
        + s.Substring(1)

    let private dashify (separator: string) (input: string) =
        Regex.Replace(
            input,
            "[a-z]?[A-Z]",
            fun m ->
                if m.Value.Length = 1 then
                    m.Value.ToLowerInvariant()
                else
                    m.Value.Substring(0, 1)
                    + separator
                    + m.Value.Substring(1, 1).ToLowerInvariant()
        )

    let applyCaseRule caseRule name =
        match caseRule with
        | CaseRules.LowerFirst -> lowerFirst name
        | CaseRules.SnakeCase -> dashify "_" name
        | CaseRules.SnakeCaseAllCaps -> (dashify "_" name).ToUpperInvariant()
        | CaseRules.KebabCase -> dashify "-" name
        | CaseRules.None
        | _ -> name

    let toSnakeCase (name: string) =
        if name.Length > 0 && Char.IsLower(name.[0]) then
            Naming.applyCaseRule CaseRules.SnakeCase name
        else
            name

    let cleanNameAsGoIdentifier (name: string) =
        if name = ".ctor" then
            "_ctor"
        else
            name.Replace('.', '_').Replace('`', '_')

    let goKeywords =
        // https://go.dev/ref/spec#Keywords
        System.Collections.Generic.HashSet [ "break"
                                             "default"
                                             "func"
                                             "interface"
                                             "select"
                                             "case"
                                             "defer"
                                             "go"
                                             "map"
                                             "struct"
                                             "chan"
                                             "else"
                                             "goto"
                                             "package"
                                             "switch"
                                             "const"
                                             "fallthrough"
                                             "if"
                                             "range"
                                             "type"
                                             "continue"
                                             "for"
                                             "import"
                                             "return"
                                             "var" ]



    // Other global builtins we should avoid https://pkg.go.dev/builtin
    let goBuiltins =
        System.Collections.Generic.HashSet [ "append"
                                             "cap"
                                             "const"
                                             "close"
                                             "complex"
                                             "copy"
                                             "delete"
                                             "iota"
                                             "imag"
                                             "len"
                                             "make"
                                             "new"
                                             "nil"
                                             "panic"
                                             "print"
                                             "println"
                                             "real"
                                             "recover"
                                             "string"
                                             "true"
                                             "false"
                                           ]

    let reflectionSuffix = "_reflection"


    let preventConflicts conflicts originalName =
        let rec check originalName n =
            let name =
                if n > 0 then
                    originalName + "_" + (string n)
                else
                    originalName

            if not (conflicts name) then
                name
            else
                check originalName (n + 1)

        check originalName 0

    let isIdentChar index (c: char) =
        // Digits are not allowed in first position, see #1397
        c = '_'
        || Char.IsLetter(c)
        || Char.IsDigit(c) && index > 0

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
                            string c
                        elif c = '$'
                             || c = '_'
                             || c = ' '
                             || c = '*'
                             || c = '.'
                             || c = '`' then
                            "_"
                        else
                            "_"
                            + String.Format("{0:X}", int c).PadLeft(4, '0')
                }
            )
        else
            ident

    let checkGoKeywords name =
        if goKeywords.Contains name then
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
        + (match part with
           | Naming.InstanceMemberPart (s, i) -> printPart sanitize "__" s i
           | Naming.StaticMemberPart (s, i) -> printPart sanitize "_" s i
           | Naming.NoMemberPart -> "")

    let sanitizeIdent conflicts (name: string) part =
        let name =
            if name.EndsWith("@") then
                $"_{name.Substring(0, name.Length - 1)}"
            else
                name
        // Replace Forbidden Chars
        buildName sanitizeIdentForbiddenChars name part
        |> checkGoKeywords
        // Check if it already exists
        |> preventConflicts conflicts
