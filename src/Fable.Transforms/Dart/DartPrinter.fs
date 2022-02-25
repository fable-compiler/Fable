module Fable.Transforms.DartPrinter

open Fable.AST
open Fable.AST.Dart
open Fable.Transforms.Printer

module PrinterExtensions =
    type Printer with
        member this.AddError(msg, ?range) =
            this.AddLog(msg, Fable.Severity.Error, ?range=range)

        member this.AddWarning(msg, ?range) =
            this.AddLog(msg, Fable.Severity.Warning , ?range=range)

        member printer.PrintBlock(nodes: 'a list, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit, ?skipNewLineAtEnd) =
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
            printer.PrintBlock(nodes,
                               (fun p s -> p.PrintProductiveStatement(s)),
                               (fun p -> p.PrintStatementSeparator()),
                               ?skipNewLineAtEnd=skipNewLineAtEnd)

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

        member printer.Print(t: Type) =
            match t with
            | Void -> printer.Print("void")
            | Boolean -> printer.Print("bool")
            | String -> printer.Print("String")
            | Integer -> printer.Print("int")
            | Double -> printer.Print("double")
            | Object -> printer.Print("Object")
            | Dynamic -> printer.Print("dynamic")
            | List t ->
                printer.Print("List<")
                printer.Print(t)
                printer.Print(">")
            | Nullable t ->
                printer.Print(t)
                printer.Print("?")
            | t -> printer.AddError($"TODO: Print type %A{t}")

        member printer.WithParens(expr: Expression) =
            printer.Print("(")
            printer.Print(expr)
            printer.Print(")")

        // TODO
        member printer.ComplexExpressionWithParens(expr: Expression) =
            match expr with
            | Literal(IntegerLiteral v) when v < 0L -> printer.WithParens(expr)
            | Literal(DoubleLiteral v) when v < 0. -> printer.WithParens(expr)
            | Literal _
            | IdentExpression _
            | PropertyAccess _
            | IndexExpression _
            | InvocationExpression _ -> printer.Print(expr)
            | _ -> printer.WithParens(expr)

        member printer.PrintBinaryExpression(operator: BinaryOperator, left: Expression, right: Expression, isInt) =
            printer.ComplexExpressionWithParens(left)
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
            | BinaryDivide -> printer.Print(if isInt then " ~/ " else " / ")
            | BinaryModulus -> printer.Print(" % ")
            | BinaryExponent -> printer.Print(" ** ")
            | BinaryOrBitwise -> printer.Print(" | ")
            | BinaryXorBitwise -> printer.Print(" ^ ")
            | BinaryAndBitwise -> printer.Print(" & ")
            printer.ComplexExpressionWithParens(right)

        member printer.PrintLiteral(kind: Literal) =
            match kind with
            | NullLiteral -> printer.Print(null)
            | ListLiteral(values, isConst) ->
                if isConst then
                    printer.Print("const ")
                printer.PrintList("[", values, "]")
            | BooleanLiteral v -> printer.Print(if v then "true" else "false")
            | StringLiteral value ->
                printer.Print("'")
                printer.Print(printer.EscapeStringLiteral(value))
                printer.Print("'")
            | IntegerLiteral value ->
                printer.Print(value.ToString())
            | DoubleLiteral value ->
                let value =
                    match value.ToString(System.Globalization.CultureInfo.InvariantCulture) with
                    | "∞" -> "double.infinity"
                    | "-∞" -> "-double.infinity"
                    | value -> value
                printer.Print(value)

        member printer.PrintIdent(ident: Ident) =
            match ident.Prefix with
            | None -> ()
            | Some p -> printer.Print(p + ".")
            printer.Print(ident.Name)

        member printer.Print(statement: Statement) =
            match statement with
            | ReturnStatement e ->
                printer.Print("return ")
                printer.Print(e)
            | BreakStatement label ->
                match label with
                | None -> printer.Print("break")
                | Some label -> printer.Print("break " + label)
            | ContinueStatement label ->
                match label with
                | None -> printer.Print("continue")
                | Some label -> printer.Print("continue " + label)
            | Label _label ->
                printer.AddError("TODO: label")
            | ExpressionStatement e ->
                printer.Print(e)
            | LocalVariableDeclaration(ident, kind, value) ->
                printer.PrintVariableDeclaration(ident, kind, ?value=value)
            | LocalFunctionDeclaration _ ->
                printer.AddError("TODO: local function declaration")
            | SwitchStatement(discriminant, cases, defaultCase) ->
                printer.Print("switch (")
                printer.Print(discriminant)
                printer.Print(") ")

                let cases = [
                    yield! List.map Choice1Of2 cases
                    match defaultCase with
                    | Some c -> Choice2Of2 c
                    | None -> ()
                ]

                printer.PrintBlock(cases, (fun p c ->
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
                            p.Print(";")
                            p.PrintNewLine()

                        match List.tryLast c.Body with
                        | Some(ContinueStatement _)
                        | Some(BreakStatement _)
                        | Some(ReturnStatement _) -> ()
                        | _ ->
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
                ), fun _ -> ())

        member printer.Print(expr: Expression) =
            match expr with
            | Literal kind -> printer.PrintLiteral(kind)

            | IdentExpression i -> printer.PrintIdent(i)

            | ConditionalExpression(test, consequent, alternate) ->
                match test, consequent, alternate with
                | Literal(BooleanLiteral(value=value)), _, _ ->
                    if value then printer.Print(consequent)
                    else printer.Print(alternate)
                | test, Literal(BooleanLiteral(true)), Literal(BooleanLiteral(false)) ->
                    printer.Print(test)
                | test, Literal(BooleanLiteral(false)), Literal(BooleanLiteral(true)) ->
                    printer.Print("!")
                    printer.ComplexExpressionWithParens(test)
                | test, _, Literal(BooleanLiteral(false)) ->
                    printer.ComplexExpressionWithParens(test)
                    printer.Print(" && ")
                    printer.ComplexExpressionWithParens(consequent)
                | _ ->
                    printer.ComplexExpressionWithParens(test)
                    printer.Print(" ? ")
                    printer.ComplexExpressionWithParens(consequent)
                    printer.Print(" : ")
                    printer.ComplexExpressionWithParens(alternate)

            | UnaryExpression(op, expr) ->
                let printUnaryOp (op: string) (expr: Expression) =
                    printer.Print(op)
                    printer.ComplexExpressionWithParens(expr)
                match op with
                | UnaryMinus -> printUnaryOp "-" expr
                | UnaryNot -> printUnaryOp "!" expr
                | UnaryNotBitwise -> printUnaryOp "~" expr
                // TODO: I think Dart doesn't accept + prefix, check
                | UnaryPlus
                | UnaryAddressOf -> printer.Print(expr)

            | BinaryExpression(op, left, right, isInt) ->
                printer.PrintBinaryExpression(op, left, right, isInt)

            | LogicalExpression(op, left, right) ->
                failwith "todo: print LogicalExpression"

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

            | PropertyAccess(expr, prop) ->
                printer.ComplexExpressionWithParens(expr)
                printer.Print("." + prop)

            | IndexExpression(expr, index) ->
                printer.ComplexExpressionWithParens(expr)
                printer.Print("[" + string index + "]")

            | AsExpression(expr, typ) ->
                printer.ComplexExpressionWithParens(expr)
                printer.Print(" as ")
                printer.Print(typ)

            | IsExpression(expr, typ, isNot) ->
                printer.ComplexExpressionWithParens(expr)
                if isNot then
                    printer.Print(" !is ")
                else
                    printer.Print(" is ")
                printer.Print(typ)

            | InvocationExpression(caller, _genArgs, args) -> // TODO: genArgs
                printer.Print(caller)
                printer.PrintList("(", args, ")")

            | AnonymousFunction(args, Choice1Of2 body, _genParams) -> // TODO: genArgs
                printer.PrintList("(", args, ") ", printType=true)
                printer.PrintBlock(body, skipNewLineAtEnd=true)

            | AnonymousFunction(args, Choice2Of2 body, _genParams) -> // TODO: genArgs
                printer.PrintList("(", args, ")", printType=true)
                printer.Print(" => ")
                printer.Print(body)

            // | e -> printer.AddError($"TODO: Print expression %A{e}")

        member printer.PrintList(left: string, separator: string, right: string, items: 'a list, printItem: 'a -> unit) =
            let rec printList = function
                | [] -> ()
                | [item] -> printItem item
                | item::items ->
                    printItem item
                    printer.Print(separator)
                    printList items
            printer.Print(left)
            printList items
            printer.Print(right)

        member printer.PrintList(left, idents: Ident list, right, ?printType: bool) =
            let printType = defaultArg printType false
            printer.PrintList(left, ", ", right, idents, fun x ->
                if printType then
                    printer.Print(x.Type)
                    printer.Print(" ")
                printer.Print(x.Name)
            )

        member printer.PrintList(left, items: string list, right) =
            printer.PrintList(left, ", ", right, items, fun (x: string) -> printer.Print(x))

        member printer.PrintList(left, items: Expression list, right) =
            printer.PrintList(left, ", ", right, items, fun (x: Expression) -> printer.Print(x))

        member printer.PrintClassDeclaration(decl: ClassDeclaration) =
            printer.Print("class " + decl.Name + " ")
            let callSuper =
                match decl.Extends with
                | None -> false
                | Some i ->
                    printer.Print("extends ")
                    printer.PrintIdent(i)
                    printer.Print(" ")
                    true

            let members = [
                match decl.Constructor with
                | Some c -> Choice1Of2 c
                | None -> ()
                yield! decl.Members |> List.map Choice2Of2
            ]

            printer.PrintBlock(members, (fun p m ->
                match m with
                | Choice1Of2 c ->
                    p.Print(decl.Name)
                    p.PrintList("(", c.Args, ")", printType=true)
                    if callSuper then
                        p.Print(": super")
                        p.PrintList("(", c.SuperArgs, ")")
                    match c.Body with
                    | [] -> p.Print(";")
                    | body ->
                        p.Print(" ")
                        p.PrintBlock(body)
                | Choice2Of2(m, kind) -> p.PrintFunctionDeclaration(m)
            ), fun p -> p.PrintNewLine())

        member printer.PrintFunctionDeclaration(decl: FunctionDeclaration) =
            printer.Print(decl.ReturnType)
            printer.Print(" ")
            printer.Print(decl.Name)
            match decl.GenericParams with
            | [] -> ()
            | genParams -> printer.PrintList("<", genParams, ">")
            printer.PrintList("(", decl.Args, ")", printType=true)
            printer.Print(" ")
            printer.PrintBlock(decl.Body, skipNewLineAtEnd=true)

        member printer.PrintVariableDeclaration(ident: Ident, kind: VariableDeclarationKind, ?value: Expression) =
            match value with
            | None ->
                printer.Print(ident.Type)
                printer.Print(" " + ident.Name)
            | Some value ->
                match kind with
                | Const -> printer.Print("const " + ident.Name + " = ")
                | Final -> printer.Print("final " + ident.Name + " = ")
                | VarConst -> printer.Print("var " + ident.Name + " = const ")
                | Var -> printer.Print("var " + ident.Name + " = ")
                printer.Print(value)

open PrinterExtensions

let run (writer: Writer) (file: File): Async<unit> =
    let printDeclWithExtraLine extraLine (printer: Printer) (decl: Declaration) =
        match decl with
        | ClassDeclaration decl ->
            printer.PrintClassDeclaration(decl)

        | FunctionDeclaration decl ->
            printer.PrintFunctionDeclaration(decl)
            printer.PrintNewLine()

        | VariableDeclaration(ident, kind, value) ->
            printer.PrintVariableDeclaration(ident, kind, value)
            printer.Print(";")
            printer.PrintNewLine()

        if extraLine then
            printer.PrintNewLine()

    async {
        use printerImpl = new PrinterImpl(writer)
        let printer = printerImpl :> Printer

        for i in file.Imports do
            let path = printer.MakeImportPath(i.Path)
            match i.LocalIdent with
            | None -> printer.Print("import '" + path + "';")
            | Some localId -> printer.Print("import '" + path + "' as " + localId + ";")
            printer.PrintNewLine()

        printer.PrintNewLine()
        do! printerImpl.Flush()

        for decl in file.Declarations do
            printDeclWithExtraLine true printer decl
            do! printerImpl.Flush()
    }
