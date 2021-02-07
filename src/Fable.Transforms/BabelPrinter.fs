// fsharplint:disable InterfaceNames
module Fable.Transforms.BabelPrinter

open System
open Fable
open Fable.AST
open Fable.AST.Babel

type SourceMapGenerator =
    abstract AddMapping:
        originalLine: int
        * originalColumn: int
        * generatedLine: int
        * generatedColumn: int
        * ?name: string
        -> unit

type Writer =
    inherit IDisposable
    abstract EscapeJsStringLiteral: string -> string
    abstract MakeImportPath: string -> string
    abstract Write: string -> Async<unit>

type Printer =
    abstract Line: int
    abstract Column: int
    abstract PushIndentation: unit -> unit
    abstract PopIndentation: unit -> unit
    abstract Print: string * ?loc: SourceLocation -> unit
    abstract PrintNewLine: unit -> unit
    abstract AddLocation: SourceLocation option -> unit
    abstract EscapeJsStringLiteral: string -> string
    abstract MakeImportPath: string -> string

type PrinterImpl(writer: Writer, map: SourceMapGenerator) =
    // TODO: We can make this configurable later
    let indentSpaces = "    "
    let builder = Text.StringBuilder()
    let mutable indent = 0
    let mutable line = 1
    let mutable column = 0

    let addLoc (loc: SourceLocation option) =
        match loc with
        | None -> ()
        | Some loc ->
            map.AddMapping(originalLine = loc.start.line,
                           originalColumn = loc.start.column,
                           generatedLine = line,
                           generatedColumn = column,
                           ?name = loc.identifierName)

    member _.Flush(): Async<unit> =
        async {
            do! writer.Write(builder.ToString())
            builder.Clear() |> ignore
        }

    interface IDisposable with
        member _.Dispose() = writer.Dispose()

    interface Printer with
        member _.Line = line
        member _.Column = column

        member _.PrintNewLine() =
            builder.AppendLine() |> ignore
            line <- line + 1
            column <- 0

        member _.PushIndentation() =
            indent <- indent + 1

        member _.PopIndentation() =
            if indent > 0 then indent <- indent - 1

        member _.AddLocation(loc) =
            addLoc loc

        member _.Print(str: string, ?loc) =
            addLoc loc

            if column = 0 then
                let indent = String.replicate indent indentSpaces
                builder.Append(indent) |> ignore
                column <- indent.Length

            builder.Append(str) |> ignore
            column <- column + str.Length

        member this.EscapeJsStringLiteral(str) =
            writer.EscapeJsStringLiteral(str)

        member this.MakeImportPath(path) =
            writer.MakeImportPath(path)


module PrinterExtensions =
    type Printer with
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

        member printer.PrintStatementSeparator() =
            if printer.Column > 0 then
                printer.Print(";")
                printer.PrintNewLine()

        member _.IsProductiveStatement(s: Statement) =
            let rec hasNoSideEffects (e: Expression) =
                match e with
                | Undefined(_)
                | Literal(NullLiteral(_))
                | Literal(Literal.StringLiteral(_))
                | Literal(BooleanLiteral(_))
                | Literal(NumericLiteral(_)) -> true
                // Constructors of classes deriving from System.Object add an empty object at the end
                | ObjectExpression(properties, loc) -> properties.Length = 0
                | UnaryExpression(expr) when expr.Operator = "void" -> hasNoSideEffects expr.Argument
                // Some identifiers may be stranded as the result of imports
                // intended only for side effects, see #2228
                | Expression.Identifier(_) -> true
                | _ -> false

            match s with
            | ExpressionStatement(expr) -> hasNoSideEffects expr |> not
            | _ -> true

        member printer.PrintProductiveStatement(s: Statement, ?printSeparator) =
            if printer.IsProductiveStatement(s) then
                printer.Print(s)
                printSeparator |> Option.iter (fun f -> f printer)

        member printer.PrintProductiveStatements(statements: Statement[]) =
            for s in statements do
                printer.PrintProductiveStatement(s, (fun p -> p.PrintStatementSeparator()))

        member printer.PrintBlock(nodes: Statement array, ?skipNewLineAtEnd) =
            printer.PrintBlock(nodes,
                               (fun p s -> p.PrintProductiveStatement(s)),
                               (fun p -> p.PrintStatementSeparator()),
                               ?skipNewLineAtEnd=skipNewLineAtEnd)

        member printer.PrintOptional(node: Node option, ?before: string) =
            match node with
            | None -> ()
            | Some node ->
                match before with
                | Some before ->
                    printer.Print(before)
                | _ -> ()
                printer.Print(node)

        member printer.PrintOptional(node: Expression option, ?before: string) =
            printer.PrintOptional(node |> Option.map Expression, ?before=before)
        member printer.PrintOptional(node: TypeParameterDeclaration option, ?before: string) =
            printer.PrintOptional(node |> Option.map TypeParameterDeclaration, ?before=before)
        member printer.PrintOptional(node: TypeAnnotation option, ?before: string) =
            printer.PrintOptional(node |> Option.map TypeAnnotation, ?before=before)
        member printer.PrintOptional(node: Identifier option, ?before: string) =
            printer.PrintOptional(node |> Option.map Expression.Identifier, ?before=before)
        member printer.PrintOptional(node: Literal option, ?before: string) =
            printer.PrintOptional(node |> Option.map Literal, ?before=before)
        member printer.PrintOptional(node: StringLiteral option, ?before: string) =
            printer.PrintOptional(node |> Option.map Literal.StringLiteral, ?before=before)
        member printer.PrintOptional(node: TypeParameterInstantiation option, ?before: string) =
            printer.PrintOptional(node |> Option.map TypeParameterInstantiation, ?before=before)
        member printer.PrintOptional(node: Statement option, ?before: string) =
            printer.PrintOptional(node |> Option.map Statement, ?before=before)
        member printer.PrintOptional(node: Declaration option, ?before: string) =
            printer.PrintOptional(node |> Option.map Declaration, ?before=before)
        member printer.PrintOptional(node: VariableDeclaration option, ?before: string) =
            printer.PrintOptional(node |> Option.map VariableDeclaration, ?before=before)
        member printer.PrintOptional(node: CatchClause option, ?before: string) =
            printer.PrintOptional(node |> Option.map CatchClause, ?before=before)
        member printer.PrintOptional(node: BlockStatement option, ?before: string) =
            printer.PrintOptional(node, ?before=before)

        member printer.PrintArray(nodes: 'a array, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes.[i]
                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedArray(nodes: Node array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))
        member printer.PrintCommaSeparatedArray(nodes: Pattern array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))
        member printer.PrintCommaSeparatedArray(nodes: ImportSpecifier array) =
            printer.PrintArray(nodes, (fun p x ->
                match x with
                | ImportMemberSpecifier(local, imported) -> p.PrintImportMemberSpecific(local, imported)
                | ImportDefaultSpecifier(local) -> printer.Print(local)
                | ImportNamespaceSpecifier(local) -> printer.PrintImportNamespaceSpecifier(local)
            ), (fun p -> p.Print(", ")))
        member printer.PrintCommaSeparatedArray(nodes: ExportSpecifier array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))
        member printer.PrintCommaSeparatedArray(nodes: FunctionTypeParam array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))
        member printer.PrintCommaSeparatedArray(nodes: TypeAnnotationInfo array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))
        member printer.PrintCommaSeparatedArray(nodes: TypeParameter array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedArray(nodes: Expression array) =
            printer.PrintArray(nodes, (fun p x -> p.SequenceExpressionWithParens(x)), (fun p -> p.Print(", ")))


        // TODO: (super) type parameters, implements
        member printer.PrintClass(id: Identifier option, superClass: Expression option,
                superTypeParameters: TypeParameterInstantiation option,
                typeParameters: TypeParameterDeclaration option,
                implements: ClassImplements array option, body: ClassBody, loc) =
            printer.Print("class", ?loc=loc)
            printer.PrintOptional(id, " ")
            printer.PrintOptional(typeParameters |> Option.map TypeParameterDeclaration)
            match superClass with
            | Some (Expression.Identifier(Identifier(typeAnnotation=typeAnnotation))) when typeAnnotation.IsSome ->
                printer.Print(" extends ");
                printer.Print(typeAnnotation.Value.TypeAnnotation)
            | _ -> printer.PrintOptional(superClass, " extends ")
            // printer.PrintOptional(superTypeParameters)
            match implements with
            | Some implements when not (Array.isEmpty implements) ->
                printer.Print(" implements ")
                printer.PrintArray(implements, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))
            | _ -> ()
            printer.Print(" ")
            printer.Print(body)

        member printer.PrintFunction(id: Identifier option, parameters: Pattern array, body: BlockStatement,
                typeParameters: TypeParameterDeclaration option, returnType: TypeAnnotation option, loc, ?isDeclaration, ?isArrow) =
            let areEqualPassedAndAppliedArgs (passedArgs: Pattern[]) (appliedAgs: Expression[]) =
                Array.zip passedArgs appliedAgs
                |> Array.forall (function
                    | RestElement(p), Expression.Identifier(Identifier(name=name)) -> p.Name = name
                    | _ -> false)

            let isDeclaration = defaultArg isDeclaration false
            let isArrow = defaultArg isArrow false

            printer.AddLocation(loc)

            // Check if we can remove the function
            let skipExpr =
                match body.Body with
                | [| ReturnStatement(argument, loc) |] when not isDeclaration ->
                    match argument with
                    | CallExpression(callee, arguments, loc) when parameters.Length = arguments.Length ->
                        // To be sure we're not running side effects when deleting the function,
                        // check the callee is an identifier (accept non-computed member expressions too?)
                        match callee with
                        | Expression.Identifier(_) when areEqualPassedAndAppliedArgs parameters arguments ->
                            Some callee
                        | _ -> None
                    | _ -> None
                | _ -> None

            match skipExpr with
            | Some e -> printer.Print(e)
            | None ->
                if isArrow then
                    // Remove parens if we only have one argument? (and no annotation)
                    printer.PrintOptional(typeParameters)
                    printer.Print("(")
                    printer.PrintCommaSeparatedArray(parameters)
                    printer.Print(")")
                    printer.PrintOptional(returnType)
                    printer.Print(" => ")
                    match body.Body with
                    | [| ReturnStatement(argument, loc) |] ->
                        match argument with
                        | ObjectExpression(_) -> printer.WithParens(argument)
                        | MemberExpression(name, object, property, computed, loc) ->
                            match object with
                            | ObjectExpression(_) -> printer.PrintMemberExpression(name, object, property, computed, loc, objectWithParens=true)
                            | _ -> printer.Print(argument)
                        | _ -> printer.ComplexExpressionWithParens(argument)
                    | _ -> printer.PrintBlock(body.Body, skipNewLineAtEnd=true)
                else
                    printer.Print("function ")
                    printer.PrintOptional(id)
                    printer.PrintOptional(typeParameters)
                    printer.Print("(")
                    printer.PrintCommaSeparatedArray(parameters)
                    printer.Print(")")
                    printer.PrintOptional(returnType)
                    printer.Print(" ")
                    printer.PrintBlock(body.Body, skipNewLineAtEnd=true)

        member printer.WithParens(expr: Expression) =
            printer.Print("(")
            printer.Print(expr)
            printer.Print(")")

        member printer.SequenceExpressionWithParens(expr: Expression) =
            match expr with
            | SequenceExpression(_) -> printer.WithParens(expr)
            | _ -> printer.Print(expr)

        /// Surround with parens anything that can potentially conflict with operator precedence
        member printer.ComplexExpressionWithParens(expr: Expression) =
            match expr with
            | Undefined(_)
            | Literal(NullLiteral(_))
            | Literal(Literal.StringLiteral(_))
            | Literal(BooleanLiteral(_))
            | Literal(NumericLiteral(_))
            | Expression.Identifier(_)
            | MemberExpression(_)
            | CallExpression(_)
            | ThisExpression(_)
            | Super(_)
            | SpreadElement(_)
            | ArrayExpression(_)
            | ObjectExpression(_) -> printer.Print(expr)
            | _ -> printer.WithParens(expr)

        member printer.PrintOperation(left, operator, right, loc) =
            printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(left)
            printer.Print(" " + operator + " ")
            printer.ComplexExpressionWithParens(right)

        member printer.Print(node: Node) =
            match node with
            | Pattern(n) -> printer.Print(n)
            | Statement(n) -> printer.Print(n)
            | ClassBody(n) -> printer.Print(n)
            | Expression(n) -> printer.Print(n)
            | Node.SwitchCase(n) -> printer.Print(n)
            | CatchClause(n) -> printer.Print(n)
            | ObjectMember(n) -> printer.Print(n)
            | TypeParameter(n) -> printer.Print(n)
            | TypeAnnotation(n) -> printer.Print(n)
            | ExportSpecifier(n) -> printer.Print(n)
            | InterfaceExtends(n) -> printer.Print(n)
            | ModuleDeclaration(n) -> printer.Print(n)
            | FunctionTypeParam(n) -> printer.Print(n)
            | ObjectTypeProperty(n) -> printer.Print(n)
            | Node.TypeAnnotationInfo(n) -> printer.Print(n)
            | TypeParameterDeclaration(n) -> printer.Print(n)
            | TypeParameterInstantiation(n) -> printer.Print(n)
            | Node.Program(_)
            | Directive(_)
            | ImportSpecifier(_)
            | ObjectTypeIndexer(_)
            | VariableDeclarator(_)
            | ObjectTypeCallProperty(_)
            | ObjectTypeInternalSlot(_) -> failwith "Not implemented"

        member printer.Print(expr: Expression) =
            match expr with
            | Super(loc) ->  printer.Print("super", ?loc = loc)
            | Literal(n) -> printer.Print(n)
            | Undefined(loc) -> printer.Print("undefined", ?loc=loc)
            | Expression.Identifier(n) -> printer.Print(n)
            | NewExpression(callee, arguments, typeArguments, loc) -> printer.PrintNewExpression(callee, arguments, typeArguments, loc)
            | SpreadElement(n) -> printer.Print(n)
            | ThisExpression(loc) -> printer.Print("this", ?loc = loc)
            | CallExpression(callee, arguments, loc) -> printer.PrintCallExpression(callee, arguments, loc)
            | EmitExpression(value, args, loc) -> printer.PrintEmitExpression(value, args, loc)
            | ArrayExpression(elements, loc) ->
                printer.Print("[", ?loc = loc)
                printer.PrintCommaSeparatedArray(elements)
                printer.Print("]")
            | ClassExpression(n) -> printer.Print(n)
            | ClassImplements(n) -> printer.Print(n)
            | UnaryExpression(n) -> printer.Print(n)
            | UpdateExpression(n) -> printer.Print(n)
            | ObjectExpression(properties, loc) -> printer.PrintObjectExpression(properties, loc)
            | BinaryExpression(n) -> printer.Print(n)
            | MemberExpression(name, object, property, computed, loc) -> printer.PrintMemberExpression(name, object, property, computed, loc)
            | LogicalExpression(left, operator, right, loc) -> printer.PrintOperation(left, operator, right, loc)
            | SequenceExpression(expressions, loc) ->
                // A comma-separated sequence of expressions.
                printer.AddLocation(loc)
                printer.PrintCommaSeparatedArray(expressions)
            | FunctionExpression(n) -> printer.Print(n)
            | AssignmentExpression(left, right, operator, loc) -> printer.PrintOperation(left, operator, right, loc)
            | ConditionalExpression(n) -> printer.Print(n)
            | ArrowFunctionExpression(``params``, body, returnType, typeParameters, loc) -> printer.PrintArrowFunctionExpression(``params``, body, returnType, typeParameters, loc)

        member printer.Print(pattern: Pattern) =
            match pattern with
            | IdentifierPattern(p) -> printer.Print(p)
            | RestElement(e) -> printer.Print(e)

        member printer.Print(literal: Literal) =
            match literal with
            | RegExp(pattern, flags, loc) -> printer.PrintRegExp(pattern, flags, loc)
            | NullLiteral(loc) -> printer.Print("null", ?loc=loc)
            | Literal.StringLiteral(l) -> printer.Print(l)
            | BooleanLiteral(value, loc) -> printer.Print((if value then "true" else "false"), ?loc=loc)
            | NumericLiteral(value, loc) -> printer.PrintNumeric(value, loc)
            | Literal.DirectiveLiteral(l) -> failwith "not implemented"

        member printer.Print(stmt: Statement) =
            match stmt with
            | Declaration(s) -> printer.Print(s)
            | IfStatement(test, consequent, alternate, loc) -> printer.PrintIfStatment(test, consequent, alternate, loc)
            | TryStatement(block, handler, finalizer, loc) -> printer.PrintTryStatement(block, handler, finalizer, loc)
            | ForStatement(body, init, test, update, loc) -> printer.PrintForStatement(body, init, test, update, loc)
            | BreakStatement(label, loc) -> printer.Print("break", ?loc = loc)
            | WhileStatement(test, body, loc) -> printer.PrintWhileStatment(test, body, loc)
            | ThrowStatement(argument, loc) ->
                printer.Print("throw ", ?loc = loc)
                printer.Print(argument)
            | Statement.BlockStatement(s) -> printer.Print(s)
            | ReturnStatement(argument, loc) ->
                printer.Print("return ", ?loc = loc)
                printer.Print(argument)
            | SwitchStatement(discriminant, cases, loc) -> printer.PrintSwitchStatement(discriminant, cases, loc)
            | LabeledStatement(body, label) -> printer.PrintLabeledStatement(body, label)
            | DebuggerStatement(loc) -> printer.Print("debugger", ?loc = loc)
            | ContinueStatement(label, loc) ->
                printer.Print("continue", ?loc=loc)
                printer.PrintOptional(label, " ")

            | ExpressionStatement(expr) -> printer.Print(expr)

        member printer.Print(decl: Declaration) =
            match decl with
            | ClassDeclaration(d) -> printer.Print(d)
            | VariableDeclaration(d) -> printer.Print(d)
            | FunctionDeclaration(d) -> printer.Print(d)
            | InterfaceDeclaration(d) -> printer.Print(d)

        member printer.Print(md: ModuleDeclaration) =
            match md with
            | ImportDeclaration(d) -> printer.Print(d)
            | ExportNamedReferences(d) -> printer.Print(d)
            | ExportNamedDeclaration(d) -> printer.Print(d)
            | ExportAllDeclaration(source, loc) ->
                printer.Print("export * from ", ?loc=loc)
                printer.Print(source)
            | PrivateModuleDeclaration(statement) ->
                if printer.IsProductiveStatement(statement) then
                    printer.Print(statement)

            | ExportDefaultDeclaration(d) -> printer.Print(d)

        member printer.PrintEmitExpression(value, args, loc) =
            printer.AddLocation(loc)

            let inline replace pattern (f: System.Text.RegularExpressions.Match -> string) input =
                System.Text.RegularExpressions.Regex.Replace(input, pattern, f)

            let printSegment (printer: Printer) (value: string) segmentStart segmentEnd =
                let segmentLength = segmentEnd - segmentStart
                if segmentLength > 0 then
                    let segment = value.Substring(segmentStart, segmentLength)
                    let subSegments = System.Text.RegularExpressions.Regex.Split(segment, @"\r?\n")
                    for i = 1 to subSegments.Length do
                        let subSegment =
                            // Remove whitespace in front of new lines,
                            // indent will be automatically applied
                            if printer.Column = 0 then subSegments.[i - 1].TrimStart()
                            else subSegments.[i - 1]
                        if subSegment.Length > 0 then
                            printer.Print(subSegment)
                            if i < subSegments.Length then
                                printer.PrintNewLine()

            // Macro transformations
            // https://fable.io/docs/communicate/js-from-fable.html#Emit-when-F-is-not-enough
            let value =
                value
                |> replace @"\$(\d+)\.\.\." (fun m ->
                    let rep = ResizeArray()
                    let i = int m.Groups.[1].Value
                    for j = i to args.Length - 1 do
                        rep.Add("$" + string j)
                    String.concat ", " rep)

                |> replace @"\{\{\s*\$(\d+)\s*\?(.*?)\:(.*?)\}\}" (fun m ->
                    let i = int m.Groups.[1].Value
                    match args.[i] with
                    | Literal(BooleanLiteral(value=value)) when value -> m.Groups.[2].Value
                    | _ -> m.Groups.[3].Value)

                |> replace @"\{\{([^\}]*\$(\d+).*?)\}\}" (fun m ->
                    let i = int m.Groups.[2].Value
                    match Array.tryItem i args with
                    | Some _ -> m.Groups.[1].Value
                    | None -> "")

                // This is to emit string literals as JS, I think it's no really
                // used and it shouldn't be necessary with the new emitJsExpr
    //            |> replace @"\$(\d+)!" (fun m ->
    //                let i = int m.Groups.[1].Value
    //                match Array.tryItem i args with
    //                | Some(:? StringLiteral as s) -> s.Value
    //                | _ -> "")

            let matches = System.Text.RegularExpressions.Regex.Matches(value, @"\$\d+")
            if matches.Count > 0 then
                for i = 0 to matches.Count - 1 do
                    let m = matches.[i]

                    let segmentStart =
                        if i > 0 then matches.[i-1].Index + matches.[i-1].Length
                        else 0

                    printSegment printer value segmentStart m.Index

                    let argIndex = int m.Value.[1..]
                    match Array.tryItem argIndex args with
                    | Some e -> printer.ComplexExpressionWithParens(e)
                    | None -> printer.Print("undefined")

                let lastMatch = matches.[matches.Count - 1]
                printSegment printer value (lastMatch.Index + lastMatch.Length) value.Length
            else
                printSegment printer value 0 value.Length

        member printer.Print(identifier: Identifier) =
            let (Identifier(name, optional, typeAnnotation, loc)) = identifier
            printer.Print(name, ?loc=loc)
            if optional = Some true then
                printer.Print("?")
            printer.PrintOptional(typeAnnotation)

        member printer.PrintRegExp(pattern, flags, loc) =
            printer.Print("/", ?loc=loc)
            printer.Print(pattern)
            printer.Print("/")
            printer.Print(flags)

        member printer.Print(node: StringLiteral) =
            let (StringLiteral(value, loc)) = node
            printer.Print("\"", ?loc=loc)
            printer.Print(printer.EscapeJsStringLiteral(value))
            printer.Print("\"")

        member printer.PrintNumeric(value, loc) =
            let value =
                match value.ToString(System.Globalization.CultureInfo.InvariantCulture) with
                | "∞" -> "Infinity"
                | "-∞" -> "-Infinity"
                | value -> value
            printer.Print(value, ?loc=loc)

        member printer.Print(node: BlockStatement) =
            printer.PrintBlock(node.Body)

        member printer.PrintLabeledStatement(body, label) =
            printer.Print(label)
            printer.Print(":")
            printer.PrintNewLine()
            // Don't push indent
            printer.Print(body)

// Control Flow
        member printer.PrintIfStatment(test, consequent, alternate, loc) =
            printer.AddLocation(loc)
            printer.Print("if (", ?loc=loc)
            printer.Print(test)
            printer.Print(") ")
            printer.Print(consequent)
            match alternate with
            | None -> ()
            | Some alternate ->
                if printer.Column > 0 then printer.Print(" ")
                match alternate with
                | IfStatement(test, consequent, alternate, loc) ->
                    printer.Print("else ")
                    printer.PrintIfStatment(test, consequent, alternate, loc)
                | alternate ->
                    let statements =
                        match alternate with
                        | Statement.BlockStatement(b) -> b.Body
                        | alternate -> [|alternate|]
                    // Get productive statements and skip `else` if they're empty
                    statements
                    |> Array.filter printer.IsProductiveStatement
                    |> function
                        | [||] -> ()
                        | statements ->
                            printer.Print("else ")
                            printer.PrintBlock(statements)
            if printer.Column > 0 then
                printer.PrintNewLine()

        /// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
        member printer.Print(node: SwitchCase) =
            let (SwitchCase(test, consequent, loc)) = node
            printer.AddLocation(loc)

            match test with
            | None -> printer.Print("default")
            | Some test ->
                printer.Print("case ")
                printer.Print(test)

            printer.Print(":")

            match consequent.Length with
            | 0 -> printer.PrintNewLine()
            | 1 ->
                printer.Print(" ")
                printer.Print(consequent.[0])
            | _ ->
                printer.Print(" ")
                printer.PrintBlock(consequent)

        member printer.PrintSwitchStatement(discriminant, cases, loc) =
            printer.Print("switch (", ?loc=loc)
            printer.Print(discriminant)
            printer.Print(") ")
            printer.PrintBlock(cases, (fun p x -> p.Print(x)), fun _ -> ())

// Exceptions
        member printer.Print(node: CatchClause) =
            // "catch" is being printed by TryStatement
            printer.Print("(", ?loc = node.Loc)
            printer.Print(node.Param)
            printer.Print(") ")
            printer.Print(node.Body)

        member printer.PrintTryStatement(block, handler, finalizer, loc) =
            printer.Print("try ", ?loc = loc)
            printer.Print(block)
            printer.PrintOptional(handler, "catch ")
            printer.PrintOptional(finalizer, "finally ")

// Declarations

        member printer.Print(node: VariableDeclaration) =
            printer.Print(node.Kind + " ", ?loc = node.Loc)
            let canConflict = node.Declarations.Length > 1

            for i = 0 to node.Declarations.Length - 1 do
                let decl = node.Declarations.[i]
                printer.Print(decl.Id)

                match decl.Init with
                | None -> ()
                | Some e ->
                    printer.Print(" = ")
                    if canConflict then printer.ComplexExpressionWithParens(e)
                    else printer.SequenceExpressionWithParens(e)
                if i < node.Declarations.Length - 1 then
                    printer.Print(", ")

        member printer.PrintWhileStatment(test, body, loc) =
            printer.Print("while (", ?loc = loc)
            printer.Print(test)
            printer.Print(") ")
            printer.Print(body)

        member printer.PrintForStatement(body, init, test, update, loc) =
            printer.Print("for (", ?loc = loc)
            printer.PrintOptional(init)
            printer.Print("; ")
            printer.PrintOptional(test)
            printer.Print("; ")
            printer.PrintOptional(update)
            printer.Print(") ")
            printer.Print(body)

        member printer.Print(node: FunctionDeclaration) =
            printer.PrintFunction(Some node.Id, node.Params, node.Body, node.TypeParameters, node.ReturnType, node.Loc, isDeclaration=true)
            printer.PrintNewLine()

        /// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
        member printer.PrintArrowFunctionExpression(``params``, body, returnType, typeParameters, loc) =
            printer.PrintFunction(
                None,
                ``params``,
                body,
                typeParameters,
                returnType,
                loc,
                isArrow = true
            )

        member printer.Print(node: FunctionExpression)=
            printer.PrintFunction(node.Id, node.Params, node.Body, node.TypeParameters, node.ReturnType, node.Loc)

        member printer.Print(node: SpreadElement) =
            printer.Print("...", ?loc = node.Loc)
            printer.ComplexExpressionWithParens(node.Argument)

        member printer.Print(node: ObjectMember) =
            match node with
            | ObjectProperty(key, value, computed) -> printer.PrintObjectProperty(key, value, computed)
            | ObjectMethod(op) -> printer.Print(op)

        member printer.PrintObjectProperty(key, value, computed) =
            if computed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)
            printer.Print(": ")
            printer.SequenceExpressionWithParens(value)

        member printer.Print(node: ObjectMethod)=
            printer.AddLocation(node.Loc)

            if node.Kind <> "method" then
                printer.Print(node.Kind + " ")

            if node.Computed then
                printer.Print("[")
                printer.Print(node.Key)
                printer.Print("]")
            else
               printer.Print(node.Key)

            printer.PrintOptional(node.TypeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(node.Params)
            printer.Print(")")
            printer.PrintOptional(node.ReturnType)
            printer.Print(" ")

            printer.PrintBlock(node.Body.Body, skipNewLineAtEnd=true)

        member printer.PrintMemberExpression(name, object, property, computed, loc, ?objectWithParens: bool) =
            printer.AddLocation(loc)
            match objectWithParens, object with
            | Some true, _ | _, Literal(NumericLiteral(_)) -> printer.WithParens(object)
            | _ -> printer.ComplexExpressionWithParens(object)
            if computed then
                printer.Print("[")
                printer.Print(property)
                printer.Print("]")
            else
                printer.Print(".")
                printer.Print(property)

        member printer.PrintObjectExpression(properties, loc) =
            let printSeparator(p: Printer) =
                p.Print(",")
                p.PrintNewLine()

            printer.AddLocation(loc)
            if Array.isEmpty properties then printer.Print("{}")
            else printer.PrintBlock(properties, (fun p x -> p.Print(x)), printSeparator, skipNewLineAtEnd=true)

        member printer.Print(node: ConditionalExpression) =
            printer.AddLocation(node.Loc)
            match node.Test with
            // TODO: Move node optimization to Fable2Babel as with IfStatement?
            | Literal(BooleanLiteral(value=value)) ->
                if value then printer.Print(node.Consequent)
                else printer.Print(node.Alternate)
            | _ ->
                printer.ComplexExpressionWithParens(node.Test)
                printer.Print(" ? ")
                printer.ComplexExpressionWithParens(node.Consequent)
                printer.Print(" : ")
                printer.ComplexExpressionWithParens(node.Alternate)

        member printer.PrintCallExpression(callee, arguments, loc) =
            printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(callee)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(arguments)
            printer.Print(")")

        member printer.PrintNewExpression(callee, arguments, typeArguments, loc) =
            printer.Print("new ", ?loc=loc)
            printer.ComplexExpressionWithParens(callee)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(arguments)
            printer.Print(")")

        member printer.Print(node: UnaryExpression) =
            printer.AddLocation(node.Loc)
            match node.Operator with
            | "-" | "+" | "!" | "~" -> printer.Print(node.Operator)
            | _ -> printer.Print(node.Operator + " ")
            printer.ComplexExpressionWithParens(node.Argument)

        member printer.Print(node: UpdateExpression) =
            printer.AddLocation(node.Loc)
            if node.Prefix then
                printer.Print(node.Operator)
                printer.ComplexExpressionWithParens(node.Argument)
            else
                printer.ComplexExpressionWithParens(node.Argument)
                printer.Print(node.Operator)

// Binary Operations

        member printer.Print(node: BinaryExpression) =
            printer.PrintOperation(node.Left, node.Operator, node.Right, node.Loc)

        member printer.Print(node: RestElement) =
            printer.Print("...", ?loc=node.Loc)
            printer.Print(node.Argument)
            printer.PrintOptional(node.TypeAnnotation)

        member printer.Print(node: ClassMember) =
            match node with
            | ClassMethod(cm) -> printer.Print(cm)
            | ClassProperty(cp) -> printer.Print(cp)

        member printer.Print(node: ClassMethod) =
            printer.AddLocation(node.Loc)

            let keywords = [
                if node.Static = Some true then yield "static"
                if node.Abstract = Some true then yield "abstract"
                if node.Kind = "get" || node.Kind = "set" then yield node.Kind
            ]

            if not (List.isEmpty keywords) then
                printer.Print((String.concat " " keywords) + " ")

            if node.Computed then
                printer.Print("[")
                printer.Print(node.Key)
                printer.Print("]")
            else
                printer.Print(node.Key)

            printer.PrintOptional(node.TypeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(node.Params)
            printer.Print(")")
            printer.PrintOptional(node.ReturnType)
            printer.Print(" ")

            printer.Print(node.Body)

        member printer.Print(node: ClassProperty) =
            printer.AddLocation(node.Loc)
            if node.Static then
                printer.Print("static ")
            if node.Computed then
                printer.Print("[")
                printer.Print(node.Key)
                printer.Print("]")
            else
                printer.Print(node.Key)
            if node.Optional then
                printer.Print("?")
            printer.PrintOptional(node.TypeAnnotation)
            printer.PrintOptional(node.Value, ": ")

        member printer.Print(node: ClassImplements) =
            printer.Print(node.Id)
            printer.PrintOptional(node.TypeParameters)

        member printer.Print(node: ClassBody) =
            printer.AddLocation(node.Loc)
            printer.PrintBlock(node.Body, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))

        member printer.Print(node: ClassDeclaration) =
            printer.PrintClass(node.Id, node.SuperClass, node.SuperTypeParameters, node.TypeParameters, node.Implements, node.Body, node.Loc)

        member printer.Print(node: ClassExpression) =
            printer.PrintClass(node.Id, node.SuperClass, node.SuperTypeParameters, node.TypeParameters, node.Implements, node.Body, node.Loc)

        member printer.PrintImportMemberSpecific(local, imported) =
            // Don't print the braces, node will be done in the import declaration
            printer.Print(imported)
            if imported.Name <> local.Name then
                printer.Print(" as ")
                printer.Print(local)


        member printer.PrintImportNamespaceSpecifier(local) =
            printer.Print("* as ")
            printer.Print(local)

        member printer.Print(node: ImportDeclaration) =
            let members = node.Specifiers |> Array.choose (function ImportMemberSpecifier(local, imported) -> Some (ImportMemberSpecifier(local, imported)) | _ -> None)
            let defaults = node.Specifiers|> Array.choose (function ImportDefaultSpecifier(local) -> Some (ImportDefaultSpecifier(local)) | _ -> None)
            let namespaces = node.Specifiers |> Array.choose (function ImportNamespaceSpecifier(local) -> Some (ImportNamespaceSpecifier(local)) | _ -> None)

            printer.Print("import ")

            if not(Array.isEmpty defaults) then
                printer.PrintCommaSeparatedArray(defaults)
                if not(Array.isEmpty namespaces && Array.isEmpty members) then
                    printer.Print(", ")

            if not(Array.isEmpty namespaces) then
                printer.PrintCommaSeparatedArray(namespaces)
                if not(Array.isEmpty members) then
                    printer.Print(", ")

            if not(Array.isEmpty members) then
                printer.Print("{ ")
                printer.PrintCommaSeparatedArray(members)
                printer.Print(" }")

            if not(Array.isEmpty defaults && Array.isEmpty namespaces && Array.isEmpty members) then
                printer.Print(" from ")

            printer.Print("\"")
            let (StringLiteral(value, _)) = node.Source
            printer.Print(printer.MakeImportPath(value))
            printer.Print("\"")

        member printer.Print(node: ExportSpecifier) =
            // Don't print the braces, node will be done in the export declaration
            printer.Print(node.Local)
            if node.Exported.Name <> node.Local.Name then
                printer.Print(" as ")
                printer.Print(node.Exported)

        member printer.Print(node: ExportNamedDeclaration) =
            printer.Print("export ")
            printer.Print(node.Declaration)

        member printer.Print(node: ExportNamedReferences) =
            printer.Print("export ")
            printer.Print("{ ")
            printer.PrintCommaSeparatedArray(node.Specifiers)
            printer.Print(" }")
            printer.PrintOptional(node.Source, " from ")

        member printer.Print(node: ExportDefaultDeclaration) =
            printer.Print("export default ")
            match node.Declaration with
            | Choice1Of2 x -> printer.Print(x)
            | Choice2Of2 x -> printer.Print(x)

        member printer.Print(node: TypeAnnotationInfo) =
            match node with
            | StringTypeAnnotation -> printer.Print("string")
            | NumberTypeAnnotation -> printer.Print("number")
            | TypeAnnotationInfo(an) -> printer.Print(an)
            | BooleanTypeAnnotation -> printer.Print("boolean")
            | AnyTypeAnnotation -> printer.Print("any")
            | VoidTypeAnnotation -> printer.Print("void")
            | TupleTypeAnnotation(types) ->
                printer.Print("[")
                printer.PrintCommaSeparatedArray(types)
                printer.Print("]")
            | UnionTypeAnnotation(types) ->
                printer.PrintArray(types, (fun p x -> p.Print(x)), (fun p -> p.Print(" | ")))
            | FunctionTypeAnnotation(an) -> printer.Print(an)
            | NullableTypeAnnotation(typeAnnotation) -> printer.Print(typeAnnotation)
            | GenericTypeAnnotation(id, typeParameters) ->
                printer.Print(id)
                printer.PrintOptional(typeParameters)
            | ObjectTypeAnnotation(an) -> printer.Print(an)

        member printer.Print(node: TypeAnnotation) =
            printer.Print(": ")
            printer.Print(node.TypeAnnotation)

        member printer.Print(node: TypeParameter) =
            printer.Print(node.Name)
            // printer.PrintOptional(bound)
            // printer.PrintOptional(``default``)

        member printer.Print(node: TypeParameterDeclaration) =
            printer.Print("<")
            printer.PrintCommaSeparatedArray(node.Params)
            printer.Print(">")

        member printer.Print(node: TypeParameterInstantiation) =
            printer.Print("<")
            printer.PrintCommaSeparatedArray(node.Params)
            printer.Print(">")

        member printer.Print(node: FunctionTypeParam) =
            printer.Print(node.Name)
            if node.Optional = Some true then
                printer.Print("?")
            printer.Print(": ")
            printer.Print(node.TypeAnnotation)

        member printer.Print(node: FunctionTypeAnnotation) =
            printer.PrintOptional(node.TypeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(node.Params)
            if Option.isSome node.Rest then
                printer.Print("...")
                printer.Print(node.Rest.Value)
            printer.Print(") => ")
            printer.Print(node.ReturnType)

        member printer.Print(node: ObjectTypeProperty) =
            if node.Static then
                printer.Print("static ")
            if Option.isSome node.Kind then
                printer.Print(node.Kind.Value + " ")
            if node.Computed then
                printer.Print("[")
                printer.Print(node.Key)
                printer.Print("]")
            else
                printer.Print(node.Key)
            if node.Optional then
                printer.Print("?")
            // TODO: proto, method
            printer.Print(": ")
            printer.Print(node.Value)

        member printer.Print(node: ObjectTypeAnnotation) =
            printer.Print("{")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintArray(node.Properties, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(node.Indexers, (fun p x -> p.Print(x |> ObjectTypeIndexer)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(node.CallProperties, (fun p x -> p.Print(x |> ObjectTypeCallProperty)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(node.InternalSlots, (fun p x -> p.Print(x |> ObjectTypeInternalSlot)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintNewLine()
            printer.PopIndentation()
            printer.Print("}")
            printer.PrintNewLine()

        member printer.Print(node: InterfaceExtends) =
            printer.Print(node.Id)
            printer.PrintOptional(node.TypeParameters)

        member printer.Print(node: InterfaceDeclaration) =
            printer.Print("interface ")
            printer.Print(node.Id)
            printer.PrintOptional(node.TypeParameters)

            if not (Array.isEmpty node.Extends) then
                printer.Print(" extends ")
                printer.PrintArray(node.Extends, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

            if not (Array.isEmpty node.Implements) then
                printer.Print(" implements ")
                printer.PrintArray(node.Implements, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

            printer.Print(" ")
            printer.Print(node.Body)

open PrinterExtensions

let run writer map (program: Program): Async<unit> =
    let printDeclWithExtraLine extraLine (printer: Printer) (decl: ModuleDeclaration) =
        printer.Print(decl)

        if printer.Column > 0 then
            printer.Print(";")
            printer.PrintNewLine()
        if extraLine then
            printer.PrintNewLine()

    async {
        use printer = new PrinterImpl(writer, map)

        let imports, restDecls =
            program.Body |> Array.splitWhile (function
                | ImportDeclaration(_) -> true
                | _ -> false)

        for decl in imports do
            printDeclWithExtraLine false printer decl

        (printer :> Printer).PrintNewLine()
        do! printer.Flush()

        for decl in restDecls do
            printDeclWithExtraLine true printer decl
            // TODO: Only flush every XXX lines?
            do! printer.Flush()
    }
