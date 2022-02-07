module Fable.Transforms.DartPrinter

open System
open Fable.AST
open Fable.AST.Dart
open Fable.Transforms.Printer

module PrinterExtensions =
    type Printer with
        member this.AddError(msg, ?range) =
            this.AddLog(msg, Fable.Severity.Error, ?range=range)

        member this.AddWarning(msg, ?range) =
            this.AddLog(msg, Fable.Severity.Warning , ?range=range)

        member printer.PrintBlock(nodes: 'a array, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit, ?skipNewLineAtEnd) =
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
            printer.PrintBlock(List.toArray nodes,
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
            | InvocationExpression _ -> printer.Print(expr)
            | _ -> printer.WithParens(expr)

        member printer.PrintBinaryExpression(operator: BinaryOperator, left: Expression, right: Expression, isInt) =
            printer.ComplexExpressionWithParens(left)
            // TODO: review
            match operator with
            | BinaryEqual | BinaryEqualStrict -> printer.Print(" == ")
            | BinaryUnequal | BinaryUnequalStrict -> printer.Print(" != ")
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

        member printer.Print(statement: Statement) =
            match statement with
            | ReturnStatement e ->
                printer.Print("return ")
                printer.Print(e)
            | ExpressionStatement e ->
                printer.Print(e)
            | LocalVariableDeclaration(ident, kind, value) ->
                printer.PrintVariableDeclaration(ident, kind, ?value=value)
            | LocalFunctionDeclaration _ ->
                printer.AddError("TODO: local function declaration")
            // TODO: label
            | Break label ->
                printer.Print("break")
            | Label _label ->
                printer.AddError("TODO: label")

        member printer.Print(expr: Expression) =
            match expr with
            | Literal kind -> printer.PrintLiteral(kind)

            | IdentExpression i -> printer.Print(i.Name)

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
                | UnaryAddressOf
                | UnaryTypeof
                | UnaryVoid
                | UnaryDelete -> printer.Print(expr)

            | BinaryExpression(op, left, right, isInt) ->
                printer.PrintBinaryExpression(op, left, right, isInt)

            | LogicalExpression(op, left, right) ->
                failwith "todo: print LogicalExpression"

            | AssignmentExpression(target, kind, value) ->
                let op =
                    // TODO: Copied from Babel, review
                    match kind with
                    | AssignEqual -> "="
                    | AssignMinus -> "-="
                    | AssignPlus -> "+="
                    | AssignMultiply -> "*="
                    | AssignDivide -> "/="
                    | AssignModulus -> "%="
                    | AssignShiftLeft -> "<<="
                    | AssignShiftRightSignPropagating -> ">>="
                    | AssignShiftRightZeroFill -> ">>>="
                    | AssignOrBitwise -> "|="
                    | AssignXorBitwise -> "^="
                    | AssignAndBitwise -> "&="
                printer.Print(target)
                printer.Print(op)
                printer.Print(value)

            | PropertyAccess(expr, prop) ->
                printer.ComplexExpressionWithParens(expr)
                printer.Print("." + prop)

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

        member printer.PrintFunctionDeclaration(name: string, args: Ident list, body: Statement list, genParams: string list, returnType: Type) =
            printer.Print(returnType)
            printer.Print(" ")
            printer.Print(name)
            match genParams with
            | [] -> ()
            | genParams -> printer.PrintList("<", genParams, ">")
            printer.PrintList("(", args, ")", printType=true)
            printer.Print(" ")
            printer.PrintBlock(body, skipNewLineAtEnd=true)

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
        | ClassDeclaration -> () // TODO

        | FunctionDeclaration(name, args, body, genParams, returnType) ->
            printer.PrintFunctionDeclaration(name, args, body, genParams, returnType)
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
