module Fable.Transforms.DartPrinter

open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.AST.Dart
open Fable.Transforms.Printer

type ListPos =
    | IsFirst
    | IsMiddle
    | IsLast
    | IsSingle

module PrinterExtensions =
    type Printer with

        member this.AddError(msg, ?range) =
            this.AddLog(msg, Severity.Error, ?range = range)

        member this.AddWarning(msg, ?range) =
            this.AddLog(msg, Severity.Warning, ?range = range)

        member printer.PrintBlock
            (
                nodes: 'a list,
                printNode: Printer -> 'a -> unit,
                ?printSeparator: Printer -> unit,
                ?skipNewLineAtEnd
            )
            =
            let printSeparator = defaultArg printSeparator (fun _ -> ())
            let skipNewLineAtEnd = defaultArg skipNewLineAtEnd false
            printer.Print("{")
            printer.PrintNewLine()
            printer.PushIndentation()

            for node in nodes do
                printNode printer node
                printSeparator printer

            printer.PopIndentation()
            printer.Print("}")

            if not skipNewLineAtEnd then
                printer.PrintNewLine()

        member printer.PrintBlock(nodes: Statement list, ?skipNewLineAtEnd) =
            printer.PrintBlock(
                nodes,
                (fun p s -> p.PrintProductiveStatement(s)),
                (fun p -> p.PrintStatementSeparator()),
                ?skipNewLineAtEnd = skipNewLineAtEnd
            )

        member printer.PrintStatementSeparator() =
            if printer.Column > 0 then
                printer.Print(";")
                printer.PrintNewLine()

        member this.HasSideEffects(e: Expression) = // TODO
            match e with
            | _ -> true

        member this.IsProductiveStatement(s: Statement) =
            match s with
            | ExpressionStatement(expr) -> this.HasSideEffects(expr)
            | _ -> true

        member printer.PrintProductiveStatement(s: Statement, ?printSeparator) =
            if printer.IsProductiveStatement(s) then
                printer.Print(s)
                printSeparator |> Option.iter (fun f -> f printer)

        // TODO: Most of this code matches BabelPrinter.PrintEmitExpression, can we refactor it?
        member printer.PrintEmitExpression
            (
                value: string,
                args: Expression list
            )
            =
            let inline replace pattern (f: Match -> string) input =
                Regex.Replace(input, pattern, f)

            let printSegment
                (printer: Printer)
                (value: string)
                segmentStart
                segmentEnd
                =
                let segmentLength = segmentEnd - segmentStart

                if segmentLength > 0 then
                    let segment = value.Substring(segmentStart, segmentLength)
                    printer.Print(segment)

            // Macro transformations
            // https://fable.io/docs/communicate/js-from-fable.html#Emit-when-F-is-not-enough
            let value =
                value
                |> replace
                    @"\$(\d+)\.\.\."
                    (fun m ->
                        let rep = ResizeArray()
                        let i = int m.Groups[1].Value

                        for j = i to args.Length - 1 do
                            rep.Add("$" + string<int> j)

                        String.concat ", " rep
                    )

                |> replace
                    @"\{\{\s*\$(\d+)\s*\?(.*?):(.*?)\}\}"
                    (fun m ->
                        let i = int m.Groups[1].Value

                        match args[i] with
                        | Literal(BooleanLiteral(value = value)) when value ->
                            m.Groups[2].Value
                        | _ -> m.Groups[3].Value
                    )

                |> replace
                    @"\{\{([^\}]*\$(\d+).*?)\}\}"
                    (fun m ->
                        let i = int m.Groups[2].Value

                        match List.tryItem i args with
                        | Some _ -> m.Groups[1].Value
                        | None -> ""
                    )

                // If placeholder is followed by !, emit string literals as native code: "let $0! = $1"
                |> replace
                    @"\$(\d+)!"
                    (fun m ->
                        let i = int m.Groups[1].Value

                        match List.tryItem i args with
                        | Some(Literal(StringLiteral value)) -> value
                        | _ -> ""
                    )

            let matches = Regex.Matches(value, @"\$\d+")

            if matches.Count > 0 then
                for i = 0 to matches.Count - 1 do
                    let m = matches[i]

                    let isSurroundedWithParens =
                        m.Index > 0
                        && m.Index + m.Length < value.Length
                        && value[m.Index - 1] = '('
                        && value[m.Index + m.Length] = ')'

                    let segmentStart =
                        if i > 0 then
                            matches[i - 1].Index + matches[i - 1].Length
                        else
                            0

                    printSegment printer value segmentStart m.Index

                    let argIndex = int m.Value[1..]

                    match List.tryItem argIndex args with
                    | Some e when isSurroundedWithParens -> printer.Print(e)
                    | Some e -> printer.PrintWithParensIfComplex(e)
                    | None -> ()

                let lastMatch = matches[matches.Count - 1]

                printSegment
                    printer
                    value
                    (lastMatch.Index + lastMatch.Length)
                    value.Length
            else
                printSegment printer value 0 value.Length

        member printer.PrintList
            (
                left: string,
                right: string,
                items: 'a list,
                printItemAndSeparator: ListPos -> 'a -> unit,
                ?skipIfEmpty
            )
            =
            let skipIfEmpty = defaultArg skipIfEmpty false

            let rec printList isFirst =
                function
                | [] -> ()
                | [ item ] ->
                    let pos =
                        if isFirst then
                            IsSingle
                        else
                            IsLast

                    printItemAndSeparator pos item
                | item :: items ->
                    let pos =
                        if isFirst then
                            IsFirst
                        else
                            IsMiddle

                    printItemAndSeparator pos item
                    printList false items

            match skipIfEmpty, items with
            | true, [] -> ()
            | _, items ->
                printer.Print(left)
                printList true items
                printer.Print(right)

        member printer.PrintList
            (
                left: string,
                separator: string,
                right: string,
                items: 'a list,
                printItem: 'a -> unit,
                ?skipIfEmpty
            )
            =
            let printItem pos item =
                printItem item

                match pos with
                | IsSingle
                | IsLast -> ()
                | IsFirst
                | IsMiddle -> printer.Print(separator)

            printer.PrintList(
                left,
                right,
                items,
                printItem,
                ?skipIfEmpty = skipIfEmpty
            )

        member printer.PrintList
            (
                left,
                items: string list,
                right,
                ?skipIfEmpty
            )
            =
            printer.PrintList(
                left,
                ", ",
                right,
                items,
                (fun (x: string) -> printer.Print(x)),
                ?skipIfEmpty = skipIfEmpty
            )

        member printer.PrintIdentList
            (
                left,
                idents: Ident list,
                right,
                ?printType: bool
            )
            =
            printer.PrintList(
                left,
                ", ",
                right,
                idents,
                fun x -> printer.PrintIdent(x, ?printType = printType)
            )

        member printer.PrintExprList(left, items: Expression list, right) =
            printer.PrintList(
                left,
                ", ",
                right,
                items,
                fun (x: Expression) -> printer.Print(x)
            )

        member printer.PrintGenericParams(items: GenericParam list) =
            printer.PrintList(
                "<",
                ", ",
                ">",
                items,
                (fun (g: GenericParam) ->
                    printer.Print(g.Name)

                    match g.Extends with
                    | None -> ()
                    | Some e ->
                        printer.Print(" extends ")
                        printer.PrintType(e)
                ),
                skipIfEmpty = true
            )

        member printer.PrintCallArgAndSeparator
            (hasUnnamedArgs: bool)
            (pos: ListPos)
            ((name, expr): CallArg)
            =
            let isNamed =
                match name with
                | None -> false
                | Some name ->
                    match pos with
                    | IsFirst
                    | IsSingle when not hasUnnamedArgs ->
                        printer.PrintNewLine()
                        printer.PushIndentation()
                    | _ -> ()

                    printer.Print(name + ": ")
                    true

            printer.Print(expr)

            match pos with
            | IsSingle
            | IsLast ->
                if isNamed && not hasUnnamedArgs then
                    printer.PrintNewLine()
                    printer.PopIndentation()
                else
                    ()
            | IsFirst
            | IsMiddle ->
                if isNamed && not hasUnnamedArgs then
                    printer.Print(",")
                    printer.PrintNewLine()
                else
                    printer.Print(", ")

        member printer.PrintType(t: Type) =
            match t with
            | Void -> printer.Print("void")
            | MetaType -> printer.Print("Type")
            | Boolean -> printer.Print("bool")
            | String -> printer.Print("String")
            | Integer -> printer.Print("int")
            | Double -> printer.Print("double")
            | Object -> printer.Print("Object")
            | Dynamic -> printer.Print("dynamic")
            | List t ->
                printer.Print("List<")
                printer.PrintType(t)
                printer.Print(">")
            | Nullable t ->
                printer.PrintType(t)
                printer.Print("?")
            | Generic name -> printer.Print(name)
            | TypeReference(ref, gen, _info) ->
                printer.PrintIdent(ref)

                printer.PrintList(
                    "<",
                    ", ",
                    ">",
                    gen,
                    printer.PrintType,
                    skipIfEmpty = true
                )
            | Function(argTypes, returnType) ->
                printer.PrintType(returnType)
                printer.Print(" ")
                // Probably this won't work if we have multiple args
                let argTypes =
                    argTypes
                    |> List.filter (
                        function
                        | Void -> false
                        | _ -> true
                    )

                printer.PrintList(
                    "Function(",
                    ", ",
                    ")",
                    argTypes,
                    printer.PrintType
                )

        member printer.PrintWithParens(expr: Expression) =
            printer.Print("(")
            printer.Print(expr)
            printer.Print(")")

        member printer.PrintWithParensIfNotIdent(expr: Expression) =
            match expr with
            | IdentExpression _
            | PropertyAccess _ -> printer.Print(expr)
            | _ -> printer.PrintWithParens(expr)

        /// Should the expression be printed with parens when nested?
        member printer.IsComplex(expr: Expression) =
            match expr with
            | CommentedExpression(_, e) -> printer.IsComplex(e)

            | Literal(value) ->
                match value with
                | IntegerLiteral v -> v < 0
                | DoubleLiteral v -> v < 0.
                | _ -> false

            | ThisExpression _
            | SuperExpression _
            | InterpolationString _
            | TypeLiteral _
            | IdentExpression _
            | PropertyAccess _
            | IndexExpression _
            | InvocationExpression _
            | UpdateExpression _
            | UnaryExpression _
            | NotNullAssert _
            | RethrowExpression _ -> false

            | BinaryExpression _
            | LogicalExpression _
            | ThrowExpression _
            | AsExpression _
            | IsExpression _
            | ConditionalExpression _
            | AnonymousFunction _
            | AssignmentExpression _
            | EmitExpression _ -> true

        member printer.PrintWithParensIfComplex(expr: Expression) =
            if printer.IsComplex(expr) then
                printer.PrintWithParens(expr)
            else
                printer.Print(expr)

        member printer.PrintBinaryExpression
            (
                operator: BinaryOperator,
                left: Expression,
                right: Expression,
                typ
            )
            =
            printer.PrintWithParensIfComplex(left)
            // TODO: review
            match operator with
            | BinaryEqual -> printer.Print(" == ")
            | BinaryUnequal -> printer.Print(" != ")
            | BinaryLess -> printer.Print(" < ")
            | BinaryLessOrEqual -> printer.Print(" <= ")
            | BinaryGreater -> printer.Print(" > ")
            | BinaryGreaterOrEqual -> printer.Print(" >= ")
            | BinaryShiftLeft -> printer.Print(" << ")
            | BinaryShiftRightSignPropagating -> printer.Print(" >> ")
            | BinaryShiftRightZeroFill -> printer.Print(" >>> ")
            | BinaryMinus -> printer.Print(" - ")
            | BinaryPlus -> printer.Print(" + ")
            | BinaryMultiply -> printer.Print(" * ")
            | BinaryDivide ->
                printer.Print(
                    if typ = Integer then
                        " ~/ "
                    else
                        " / "
                )
            | BinaryModulus -> printer.Print(" % ")
            | BinaryExponent -> printer.Print(" ** ")
            | BinaryOrBitwise -> printer.Print(" | ")
            | BinaryXorBitwise -> printer.Print(" ^ ")
            | BinaryAndBitwise -> printer.Print(" & ")

            printer.PrintWithParensIfComplex(right)

        member printer.PrintLogicalExpression
            (
                operator: LogicalOperator,
                left: Expression,
                right: Expression
            )
            =
            printer.PrintWithParensIfComplex(left)

            match operator with
            | LogicalAnd -> printer.Print(" && ")
            | LogicalOr -> printer.Print(" || ")

            printer.PrintWithParensIfComplex(right)

        member printer.PrintLiteral(kind: Literal) =
            match kind with
            | NullLiteral _ -> printer.Print("null")
            | ListLiteral(values, typ, isConst) ->
                if isConst then
                    printer.Print("const ")

                match values with
                | [] ->
                    printer.Print("<")
                    printer.PrintType(typ)
                    printer.Print(">[]")
                | values -> printer.PrintExprList("[", values, "]")
            | BooleanLiteral v ->
                printer.Print(
                    if v then
                        "true"
                    else
                        "false"
                )
            | StringLiteral value ->
                let escape str =
                    (Naming.escapeString (fun _ -> false) str)
                        .Replace(@"$", @"\$")

                printer.Print("'")
                printer.Print(escape value)
                printer.Print("'")
            | IntegerLiteral value -> printer.Print(value.ToString())
            | DoubleLiteral value ->
                let value =
                    match
                        value.ToString(
                            System.Globalization.CultureInfo.InvariantCulture
                        )
                    with
                    | "∞" -> "double.infinity"
                    | "-∞" -> "-double.infinity"
                    | value when not (value.Contains(".")) -> value + ".0"
                    | value -> value

                printer.Print(value)

        member printer.PrintIdent(ident: Ident, ?printType) =
            let printType = defaultArg printType false

            if printType then
                printer.PrintType(ident.Type)
                printer.Print(" ")

            match ident.ImportModule with
            | None -> ()
            | Some p -> printer.Print(p + ".")

            printer.Print(ident.Name)

        member printer.PrintIfStatement
            (
                test: Expression,
                consequent,
                alternate
            )
            =
            printer.Print("if (")
            printer.Print(test)
            printer.Print(") ")
            printer.PrintBlock(consequent, skipNewLineAtEnd = true)

            match alternate with
            | [] -> ()
            | alternate ->
                match alternate with
                | [ IfStatement(test, consequent, alternate) ] ->
                    printer.Print(" else ")
                    printer.PrintIfStatement(test, consequent, alternate)
                | alternate ->
                    // Get productive statements and skip `else` if they're empty
                    alternate
                    |> List.filter printer.IsProductiveStatement
                    |> function
                        | [] -> ()
                        | statements ->
                            printer.Print(" else ")
                            printer.PrintBlock(statements)

            if printer.Column > 0 then
                printer.PrintNewLine()

        member printer.Print(statement: Statement) =
            match statement with
            | CommentedStatement(comment, statement) ->
                printer.Print("// " + comment)
                printer.PrintNewLine()
                printer.Print(statement)

            | IfStatement(test, consequent, alternate) ->
                printer.PrintIfStatement(test, consequent, alternate)

            | ForStatement(init, test, update, body) ->
                printer.Print("for (")

                match init with
                | None -> ()
                | Some(ident, value) ->
                    printer.Print("var " + ident.Name + " = ")
                    printer.Print(value)

                printer.Print("; ")

                match test with
                | None -> ()
                | Some test -> printer.Print(test)

                printer.Print("; ")

                match update with
                | None -> ()
                | Some update -> printer.Print(update)

                printer.Print(") ")
                printer.PrintBlock(body)

            | ForInStatement(param, iterable, body) ->
                printer.Print("for (final " + param.Name + " in ")
                printer.PrintWithParensIfComplex(iterable)
                printer.Print(") ")
                printer.PrintBlock(body)

            | WhileStatement(test, body) ->
                printer.Print("while (")
                printer.Print(test)
                printer.Print(") ")
                printer.PrintBlock(body)

            | TryStatement(body, handlers, finalizer) ->
                printer.Print("try ")
                printer.PrintBlock(body, skipNewLineAtEnd = true)

                for handler in handlers do
                    match handler.Test with
                    | None -> ()
                    | Some test ->
                        printer.Print(" on ")
                        printer.PrintType(test)

                    match handler.Param with
                    | None -> ()
                    | Some param -> printer.Print(" catch (" + param.Name + ")")

                    printer.Print(" ")
                    printer.PrintBlock(handler.Body, skipNewLineAtEnd = true)

                match finalizer with
                | [] -> ()
                | finalizer ->
                    printer.Print(" finally ")
                    printer.PrintBlock(finalizer, skipNewLineAtEnd = true)

                printer.PrintNewLine()

            | ReturnStatement e ->
                printer.Print("return ")
                printer.Print(e)

            | BreakStatement(label) ->
                match label with
                | None -> printer.Print("break")
                | Some label -> printer.Print("break " + label)

            | ContinueStatement label ->
                match label with
                | None -> printer.Print("continue")
                | Some label -> printer.Print("continue " + label)

            | LabeledStatement(label, body) ->
                printer.Print(label + ":")
                printer.PrintNewLine()
                printer.Print(body)

            | LocalFunctionDeclaration f ->
                printer.PrintFunctionDeclaration(
                    f.ReturnType,
                    f.Name,
                    f.GenericParams,
                    f.Args,
                    f.Body
                )

            | ExpressionStatement e -> printer.Print(e)

            | LocalVariableDeclaration(ident, kind, value) ->
                match kind, value with
                | Final,
                  Some(AnonymousFunction(args, body, genParams, returnType)) ->
                    let args = args |> List.map FunctionArg

                    let genParams =
                        genParams
                        |> List.map (fun g ->
                            {
                                Name = g
                                Extends = None
                            }
                        )

                    printer.PrintFunctionDeclaration(
                        returnType,
                        ident.Name,
                        genParams,
                        args,
                        body
                    )
                | _ ->
                    printer.PrintVariableDeclaration(
                        ident,
                        kind,
                        ?value = value
                    )

            | SwitchStatement(discriminant, cases, defaultCase) ->
                printer.Print("switch (")
                printer.Print(discriminant)
                printer.Print(") ")

                let cases =
                    [
                        yield! List.map Choice1Of2 cases
                        match defaultCase with
                        | Some c -> Choice2Of2 c
                        | None -> ()
                    ]

                printer.PrintBlock(
                    cases,
                    fun p c ->
                        match c with
                        | Choice1Of2 c ->
                            for g in c.Guards do
                                p.Print("case ")
                                p.Print(g)
                                p.Print(":")
                                p.PrintNewLine()

                            p.PushIndentation()

                            for s in c.Body do
                                p.Print(s)
                                p.PrintStatementSeparator()

                            let rec needsBreak statements =
                                match List.tryLast statements with
                                | Some(ContinueStatement _)
                                | Some(BreakStatement _)
                                | Some(ReturnStatement _) -> false
                                | Some(IfStatement(_, consequent, alternate)) ->
                                    needsBreak consequent
                                    || needsBreak alternate
                                | _ -> true

                            if needsBreak c.Body then
                                p.Print("break;")
                                p.PrintNewLine()

                            p.PopIndentation()

                        | Choice2Of2 def ->
                            p.Print("default:")
                            p.PrintNewLine()
                            p.PushIndentation()

                            for s in def do
                                p.Print(s)
                                p.Print(";")
                                p.PrintNewLine()

                            p.PopIndentation()
                )

        member printer.Print(expr: Expression) =
            match expr with
            | CommentedExpression(comment, expr) ->
                printer.Print("/* " + comment + " */ ")
                printer.Print(expr)

            | EmitExpression(value, args, _) ->
                printer.PrintEmitExpression(value, args)

            | ThrowExpression(e, _) ->
                printer.Print("throw ")
                printer.Print(e)

            | RethrowExpression _ -> printer.Print("rethrow")

            | SuperExpression _ -> printer.Print("super")

            | ThisExpression _ -> printer.Print("this")

            | Literal kind -> printer.PrintLiteral(kind)

            | InterpolationString(parts, values) ->
                let escape str =
                    Regex
                        .Replace(str, @"(?<!\\)\\", @"\\")
                        .Replace("'", @"\'")
                        .Replace("$", @"\$")

                let quotes =
                    if parts |> List.exists (fun p -> p.Contains("\n")) then
                        "'''"
                    else
                        "'"

                printer.Print(quotes)

                for i = 0 to parts.Length - 2 do
                    printer.Print(escape parts[i])

                    match values[i] with
                    | IdentExpression i ->
                        printer.Print("$")
                        printer.PrintIdent(i)
                        printer.Print("")
                    | v ->
                        printer.Print("${")
                        printer.Print(v)
                        printer.Print("}")

                printer.Print(List.last parts |> escape)
                printer.Print(quotes)

            | TypeLiteral t -> printer.PrintType(t)

            | IdentExpression i -> printer.PrintIdent(i)

            | ConditionalExpression(test, consequent, alternate) ->
                match test, consequent, alternate with
                | Literal(BooleanLiteral(value = value)), _, _ ->
                    if value then
                        printer.Print(consequent)
                    else
                        printer.Print(alternate)
                | test,
                  Literal(BooleanLiteral(true)),
                  Literal(BooleanLiteral(false)) -> printer.Print(test)
                | test,
                  Literal(BooleanLiteral(false)),
                  Literal(BooleanLiteral(true)) ->
                    printer.Print("!")
                    printer.PrintWithParensIfComplex(test)
                | test, Literal(BooleanLiteral(true)), alternate ->
                    printer.PrintWithParensIfComplex(test)
                    printer.Print(" || ")
                    printer.PrintWithParensIfComplex(alternate)
                | test, consequent, Literal(BooleanLiteral(false)) ->
                    printer.PrintWithParensIfComplex(test)
                    printer.Print(" && ")
                    printer.PrintWithParensIfComplex(consequent)
                | _ ->
                    printer.PrintWithParensIfComplex(test)
                    printer.Print(" ? ")
                    printer.PrintWithParensIfComplex(consequent)
                    printer.Print(" : ")
                    printer.PrintWithParensIfComplex(alternate)

            | NotNullAssert expr ->
                printer.PrintWithParensIfNotIdent(expr)
                printer.Print("!")

            | UpdateExpression(op, isPrefix, expr) ->
                let printOp =
                    function
                    | UpdateMinus -> printer.Print("--")
                    | UpdatePlus -> printer.Print("++")

                if isPrefix then
                    printOp op
                    printer.PrintWithParensIfComplex(expr)
                else
                    printer.PrintWithParensIfComplex(expr)
                    printOp op

            | UnaryExpression(op, expr) ->
                let printUnaryOp (op: string) (expr: Expression) =
                    printer.Print(op)
                    printer.PrintWithParensIfNotIdent(expr)

                match op with
                | UnaryMinus -> printUnaryOp "-" expr
                | UnaryNot ->
                    match expr with
                    | UnaryExpression(UnaryNot, expr) -> printer.Print(expr)
                    | _ -> printUnaryOp "!" expr
                | UnaryNotBitwise -> printUnaryOp "~" expr
                // TODO: I think Dart doesn't accept + prefix, check
                | UnaryPlus
                | UnaryAddressOf -> printer.Print(expr)

            | BinaryExpression(op, left, right, typ) ->
                printer.PrintBinaryExpression(op, left, right, typ)

            | LogicalExpression(op, left, right) ->
                printer.PrintLogicalExpression(op, left, right)

            | AssignmentExpression(target, kind, value) ->
                let op =
                    // TODO: Copied from Babel, review
                    match kind with
                    | AssignEqual -> " = "
                    | AssignMinus -> " -= "
                    | AssignPlus -> " += "
                    | AssignMultiply -> " *= "
                    | AssignDivide -> " /= "
                    | AssignModulus -> " %= "
                    | AssignShiftLeft -> " <<= "
                    | AssignShiftRightSignPropagating -> " >>= "
                    | AssignShiftRightZeroFill -> " >>>= "
                    | AssignOrBitwise -> " |= "
                    | AssignXorBitwise -> " ^= "
                    | AssignAndBitwise -> " &= "

                printer.Print(target)
                printer.Print(op)
                printer.Print(value)

            | PropertyAccess(expr, prop, _typ, _isConst) ->
                printer.PrintWithParensIfComplex(expr)
                printer.Print("." + prop)

            | IndexExpression(expr, index, _typ) ->
                printer.PrintWithParensIfComplex(expr)
                printer.Print("[")
                printer.Print(index)
                printer.Print("]")

            | AsExpression(expr, typ) ->
                printer.PrintWithParensIfComplex(expr)
                printer.Print(" as ")
                printer.PrintType(typ)

            | IsExpression(expr, typ, isNot) ->
                printer.PrintWithParensIfComplex(expr)

                if isNot then
                    printer.Print(" !is ")
                else
                    printer.Print(" is ")

                printer.PrintType(typ)

            // TODO: Detect if we're calling Map/Set and use collection literal if possible
            // https://dart-lang.github.io/linter/lints/prefer_collection_literals.html
            | InvocationExpression(caller, genArgs, args, _typ, isConst) ->
                let hasUnnamedArgs =
                    args
                    |> List.exists (
                        function
                        | (name, _) -> Option.isNone name
                    )

                if isConst then
                    printer.Print("const ")

                printer.PrintWithParensIfNotIdent(caller)

                printer.PrintList(
                    "<",
                    ", ",
                    ">",
                    genArgs,
                    printer.PrintType,
                    skipIfEmpty = true
                )

                printer.PrintList(
                    "(",
                    ")",
                    args,
                    printer.PrintCallArgAndSeparator hasUnnamedArgs
                )

            | AnonymousFunction(args, body, genArgs, _returnType) ->
                printer.PrintList("<", genArgs, ">", skipIfEmpty = true)
                printer.PrintIdentList("(", args, ")", printType = true)
                printer.PrintFunctionBody(body, isExpression = true)

        member printer.PrintClassDeclaration(decl: Class) =
            if decl.IsAbstract then
                printer.Print("abstract ")

            printer.Print("class " + decl.Name)
            printer.PrintGenericParams(decl.GenericParams)
            printer.Print(" ")

            let callSuper =
                match decl.Extends with
                | None -> false
                | Some t ->
                    printer.Print("extends ")
                    printer.PrintType(t)
                    printer.Print(" ")
                    true

            printer.PrintList(
                "implements ",
                ", ",
                " ",
                decl.Implements,
                printer.PrintType,
                skipIfEmpty = true
            )

            let members =
                [
                    yield! decl.InstanceVariables |> List.map Choice1Of3
                    match decl.Constructor with
                    | Some c -> Choice2Of3 c
                    | None -> ()
                    yield! decl.InstanceMethods |> List.map Choice3Of3
                ]

            printer.PrintBlock(
                members,
                (fun p m ->
                    match m with
                    | Choice1Of3 v ->
                        if v.IsOverride then
                            p.Print("@override")
                            p.PrintNewLine()

                        p.PrintVariableDeclaration(
                            v.Ident,
                            v.Kind,
                            ?value = v.Value,
                            isLate = v.IsLate
                        )

                        p.Print(";")

                    // Constructor
                    | Choice2Of3 c ->
                        if c.IsConst then
                            p.Print("const ")

                        if c.IsFactory then
                            p.Print("factory ")

                        p.Print(decl.Name)
                        printer.PrintFunctionArgs(c.Args)

                        if callSuper then
                            p.Print(": super")

                            let hasUnnamedArgs =
                                c.SuperArgs
                                |> List.exists (
                                    function
                                    | (name, _) -> Option.isNone name
                                )

                            printer.PrintList(
                                "(",
                                ")",
                                c.SuperArgs,
                                printer.PrintCallArgAndSeparator hasUnnamedArgs
                            )

                        match c.Body with
                        | [] -> p.Print(";")
                        | body ->
                            p.Print(" ")
                            p.PrintBlock(body)
                    | Choice3Of3 m ->
                        if m.IsOverride then
                            p.Print("@override")
                            p.PrintNewLine()

                        match m.Kind with
                        | IsGetter ->
                            p.PrintType(m.ReturnType)
                            p.Print(" get " + m.Name)

                            p.PrintFunctionBody(
                                ?body = m.Body,
                                isModuleOrClassMember = true
                            )
                        | IsSetter ->
                            p.PrintType(m.ReturnType)
                            p.Print(" set " + m.Name)

                            let argIdents =
                                m.Args |> List.map (fun a -> a.Ident)

                            printer.PrintIdentList(
                                "(",
                                argIdents,
                                ") ",
                                printType = true
                            )

                            p.PrintFunctionBody(
                                ?body = m.Body,
                                isModuleOrClassMember = true
                            )
                        | IsMethod ->
                            p.PrintFunctionDeclaration(
                                m.ReturnType,
                                m.Name,
                                m.GenericParams,
                                m.Args,
                                ?body = m.Body,
                                isModuleOrClassMember = true
                            )
                        | IsOperator ->
                            p.PrintFunctionDeclaration(
                                m.ReturnType,
                                "operator " + m.Name,
                                m.GenericParams,
                                m.Args,
                                ?body = m.Body,
                                isModuleOrClassMember = true
                            )
                ),
                fun p -> p.PrintNewLine()
            )

        member printer.PrintFunctionBody
            (
                ?body: Statement list,
                ?isModuleOrClassMember: bool,
                ?isExpression: bool
            )
            =
            let isModuleOrClassMember = defaultArg isModuleOrClassMember false
            let isExpression = defaultArg isExpression false

            match body with
            | None ->
                if isModuleOrClassMember then
                    printer.Print(";")
                else
                    printer.Print(" {}")
            | Some [ ReturnStatement expr ] ->
                printer.Print(" => ")
                printer.Print(expr)

                if isModuleOrClassMember then
                    printer.Print(";")
            | Some body ->
                printer.Print(" ")

                printer.PrintBlock(
                    body,
                    skipNewLineAtEnd = (isExpression || isModuleOrClassMember)
                )

        member printer.PrintFunctionArgs(args: FunctionArg list) =
            let mutable prevArg: FunctionArg option = None

            printer.PrintList(
                "(",
                ")",
                args,
                fun pos arg ->
                    if arg.IsNamed then
                        match prevArg with
                        | None -> printer.Print("{")
                        | Some a when not a.IsNamed -> printer.Print("{")
                        | Some _ -> ()
                    elif arg.IsOptional then
                        match prevArg with
                        | None -> printer.Print("[")
                        | Some a when not a.IsOptional -> printer.Print("[")
                        | Some _ -> ()
                    else
                        ()

                    if arg.IsConsThisArg then
                        printer.Print("this." + arg.Ident.Name)
                    else
                        printer.PrintIdent(arg.Ident, printType = true)

                    match arg.DefaultValue with
                    | None -> ()
                    | Some defValue ->
                        printer.Print(" = ")
                        printer.Print(defValue)

                    match pos with
                    | IsSingle
                    | IsLast ->
                        if arg.IsNamed then
                            printer.Print("}")
                        elif arg.IsOptional then
                            printer.Print("]")
                        else
                            ()
                    | IsFirst
                    | IsMiddle -> printer.Print(", ")

                    prevArg <- Some arg
            )

        member printer.PrintFunctionDeclaration
            (
                returnType: Type,
                name: string,
                genParams: GenericParam list,
                args: FunctionArg list,
                ?body: Statement list,
                ?isModuleOrClassMember
            )
            =
            printer.PrintType(returnType)
            printer.Print(" ")
            printer.Print(name)
            printer.PrintGenericParams(genParams)
            printer.PrintFunctionArgs(args)

            printer.PrintFunctionBody(
                ?body = body,
                ?isModuleOrClassMember = isModuleOrClassMember
            )

        member printer.PrintVariableDeclaration
            (
                ident: Ident,
                kind: VariableDeclarationKind,
                ?value: Expression,
                ?isLate
            )
            =
            let value =
                match value with
                | None -> None
                // Dart recommends not to explicitly initialize mutable variables to null
                | Some(Literal(NullLiteral _)) when kind = Var -> None
                | Some v -> Some v

            match value with
            | None ->
                match isLate, ident.Type with
                | Some false, _
                | None, Nullable _ -> ()
                | Some true, _
                // Declare as late so Dart compiler doesn't complain var is not assigned
                | None, _ -> printer.Print("late ")

                match kind with
                | Final -> printer.Print("final ")
                | _ -> ()

                printer.PrintType(ident.Type)
                printer.Print(" " + ident.Name)

            | Some value ->
                let printType =
                    // Nullable types and unions usually need to be typed explicitly
                    // Print type also if ident and expression types are different?
                    // (this usually happens when removing unnecessary casts)
                    match ident.Type with
                    | Nullable _ -> true
                    | TypeReference(_, _, info) -> info.IsUnion
                    | _ -> false

                match kind with
                | Const -> printer.Print("const ")
                | Final -> printer.Print("final ")
                | Var when not printType -> printer.Print("var ")
                | Var -> ()

                if printType then
                    printer.PrintType(ident.Type)
                    printer.Print(" ")

                printer.Print(ident.Name + " = ")
                printer.Print(value)

open PrinterExtensions

let isEmpty (file: File) : bool = List.isEmpty file.Declarations

let run (writer: Writer) (file: File) : Async<unit> =
    let printDeclWithExtraLine
        extraLine
        (printer: Printer)
        (decl: Declaration)
        =
        match decl with
        | ClassDeclaration decl -> printer.PrintClassDeclaration(decl)

        | FunctionDeclaration d ->
            printer.PrintFunctionDeclaration(
                d.ReturnType,
                d.Name,
                d.GenericParams,
                d.Args,
                d.Body,
                isModuleOrClassMember = true
            )

            printer.PrintNewLine()

        | VariableDeclaration(ident, kind, value) ->
            match kind, value with
            | Final, AnonymousFunction(args, body, genParams, returnType) ->
                let args = args |> List.map FunctionArg

                let genParams =
                    genParams
                    |> List.map (fun g ->
                        {
                            Name = g
                            Extends = None
                        }
                    )

                printer.PrintFunctionDeclaration(
                    returnType,
                    ident.Name,
                    genParams,
                    args,
                    body,
                    isModuleOrClassMember = true
                )
            | _ ->
                printer.PrintVariableDeclaration(ident, kind, value)
                printer.Print(";")

            printer.PrintNewLine()

        if extraLine then
            printer.PrintNewLine()

    async {
        use printerImpl = new PrinterImpl(writer)
        let printer = printerImpl :> Printer

        // If we manage to master null assertions maybe we can remove unnecessary_non_null_assertion
        printer.Print(
            "// ignore_for_file: camel_case_types, constant_identifier_names, non_constant_identifier_names, unnecessary_this"
        )

        printer.PrintNewLine()

        file.Imports
        |> List.sortBy (fun i -> i.Path)
        |> List.iter (fun i ->
            let path = printer.MakeImportPath(i.Path)

            match i.LocalIdent with
            | None -> printer.Print("import '" + path + "';")
            | Some localId ->
                printer.Print("import '" + path + "' as " + localId + ";")

            printer.PrintNewLine()
        )

        printer.PrintNewLine()
        do! printerImpl.Flush()

        for decl in file.Declarations do
            printDeclWithExtraLine true printer decl
            do! printerImpl.Flush()
    }
