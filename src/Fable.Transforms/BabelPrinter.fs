module Fable.Transforms.BabelPrinter

open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.AST.Babel
open Fable.Transforms.Printer

module PrinterExtensions =
    let rec hasSideEffects (e: Expression) =
        match e with
        | Undefined(_)
        | Literal(NullLiteral(_))
        | Literal(Literal.StringLiteral(_))
        | Literal(BooleanLiteral(_))
        | Literal(NumericLiteral(_)) -> false
        // Constructors of classes deriving from System.Object add an empty object at the end
        | ObjectExpression(properties, _loc) -> properties.Length > 0
        | UnaryExpression(argument, "void", false, _loc) ->
            hasSideEffects (argument)
        // Some identifiers may be stranded as the result of imports
        // intended only for side effects, see #2228
        | Expression.Identifier(_) -> false
        // Sometimes empty IIFE remain in the AST
        | CallExpression(ArrowFunctionExpression(_,
                                                 (BlockStatement body),
                                                 _,
                                                 _,
                                                 _),
                         _,
                         _,
                         _) -> body |> Array.exists isProductiveStatement
        | CommentedExpression(_, e) -> hasSideEffects e
        | _ -> true

    and isProductiveStatement (s: Statement) =
        match s with
        | ExpressionStatement(expr) -> hasSideEffects (expr)
        | _ -> true

    let (|UndefinedOrVoid|_|) =
        function
        | Undefined _ -> Some()
        | UnaryExpression(argument, "void", false, _loc) when
            not (hasSideEffects (argument))
            ->
            Some()
        | _ -> None

    let (|NullOrUndefinedOrVoid|_|) =
        function
        | Literal(NullLiteral _)
        | UndefinedOrVoid _ -> Some()
        | _ -> None

    let (|StringConstant|_|) =
        function
        | Literal(Literal.StringLiteral(StringLiteral(value = value))) ->
            Some value
        | _ -> None

    type Printer with

        member printer.PrintBlock
            (
                nodes: 'a array,
                printNode: Printer -> 'a -> unit,
                printSeparator: Printer -> unit,
                ?skipNewLineAtEnd
            )
            =
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

        member printer.PrintProductiveStatement(s: Statement, ?printSeparator) =
            if isProductiveStatement (s) then
                printer.Print(s)
                printSeparator |> Option.iter (fun f -> f printer)

        member printer.PrintProductiveStatements(statements: Statement[]) =
            for s in statements do
                printer.PrintProductiveStatement(
                    s,
                    (fun p -> p.PrintStatementSeparator())
                )

        member printer.PrintBlock(nodes: Statement array, ?skipNewLineAtEnd) =
            printer.PrintBlock(
                nodes,
                (fun p s -> p.PrintProductiveStatement(s)),
                (fun p -> p.PrintStatementSeparator()),
                ?skipNewLineAtEnd = skipNewLineAtEnd
            )

        member printer.PrintOptional
            (
                item: 'T option,
                print: Printer -> 'T -> unit,
                ?before: string
            )
            =
            match item with
            | None -> ()
            | Some item ->
                match before with
                | Some before -> printer.Print(before)
                | _ -> ()

                print printer item

        member printer.PrintOptional(item: Expression option, ?before: string) =
            printer.PrintOptional(
                item,
                (fun p i -> p.Print(i)),
                ?before = before
            )

        member printer.PrintOptional
            (
                item: TypeAnnotation option,
                ?before: string
            )
            =
            printer.PrintOptional(
                item,
                (fun p i -> p.Print(i)),
                ?before = before
            )

        member printer.PrintOptional(item: Identifier option, ?before: string) =
            printer.PrintOptional(
                item,
                (fun p i -> p.PrintIdent(i)),
                ?before = before
            )

        member printer.PrintOptional(item: Literal option, ?before: string) =
            printer.PrintOptional(
                item,
                (fun p i -> p.PrintLiteral(i)),
                ?before = before
            )

        member printer.PrintOptional
            (
                item: StringLiteral option,
                ?before: string
            )
            =
            printer.PrintOptional(
                item,
                (fun p i -> p.Print(i)),
                ?before = before
            )

        member printer.PrintOptional(item: Statement option, ?before: string) =
            printer.PrintOptional(
                item,
                (fun p i -> p.Print(i)),
                ?before = before
            )

        member printer.PrintOptional
            (
                item: Declaration option,
                ?before: string
            )
            =
            printer.PrintOptional(
                item,
                (fun p i -> p.PrintDeclaration(i)),
                ?before = before
            )

        member printer.PrintOptional
            (
                item: VariableDeclaration option,
                ?before: string
            )
            =
            printer.PrintOptional(
                item,
                (fun p i -> p.Print(i)),
                ?before = before
            )

        member printer.PrintOptional
            (
                item: CatchClause option,
                ?before: string
            )
            =
            printer.PrintOptional(
                item,
                (fun p i -> p.Print(i)),
                ?before = before
            )

        member printer.PrintOptional
            (
                item: BlockStatement option,
                ?before: string
            )
            =
            printer.PrintOptional(
                item,
                (fun p i -> p.Print(i)),
                ?before = before
            )

        member printer.PrintArray
            (
                items: 'a array,
                print: Printer -> 'a -> unit,
                printSeparator: Printer -> unit
            )
            =
            for i = 0 to items.Length - 1 do
                print printer items[i]

                if i < items.Length - 1 then
                    printSeparator printer

        member printer.PrintParameters
            (
                items: Parameter array,
                ?accessModifers: AccessModifier[]
            )
            =
            let accessModifiers = defaultArg accessModifers [||]
            let len = items.Length
            let mutable i = 0
            let mutable foundNamed = false

            let namedParamAnnotations =
                ResizeArray<Parameter * TypeAnnotation>()

            let printParameter
                (printer: Printer)
                (Parameter.Parameter(name, annotation, flags) as p)
                =
                if flags.IsNamed && not foundNamed then
                    printer.Print("{ ")
                    foundNamed <- true
                elif flags.IsSpread then
                    printer.Print("...")

                Array.tryItem i accessModifiers |> printer.PrintAccessModifier

                printer.Print(name)

                let annotation =
                    if foundNamed then
                        annotation
                        |> Option.iter (fun ta ->
                            namedParamAnnotations.Add((p, ta))
                        )

                        None
                    else
                        annotation

                match flags.DefVal with
                | Some defVal ->
                    printer.PrintOptional(annotation, ": ")
                    printer.Print(" = ")
                    printer.Print(defVal)
                | None ->
                    if flags.IsOptional && not foundNamed then
                        printer.Print("?")

                    printer.PrintOptional(annotation, ": ")

                i <- i + 1

                if i = len && foundNamed then
                    printer.Print(" }")

                    if namedParamAnnotations.Count > 0 then
                        printer.Print(": {")

                        printer.PrintArray(
                            namedParamAnnotations.ToArray(),
                            (fun
                                printer
                                ((Parameter.Parameter(name, _, flags)),
                                 annotation) ->
                                printer.Print(name)

                                if flags.IsOptional then
                                    printer.Print("?")

                                printer.Print(": ")
                                printer.Print(annotation)
                            ),
                            (fun p -> p.Print(", "))
                        )

                        printer.Print(" }")

            printer.PrintArray(items, printParameter, (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedArray(items: ImportSpecifier array) =
            printer.PrintArray(
                items,
                (fun p x ->
                    match x with
                    | ImportMemberSpecifier(local, imported) ->
                        p.PrintImportMemberSpecific(local, imported)
                    | ImportDefaultSpecifier(local) ->
                        printer.PrintIdent(local)
                    | ImportNamespaceSpecifier(local) ->
                        printer.PrintImportNamespaceSpecifier(local)
                ),
                (fun p -> p.Print(", "))
            )

        member printer.PrintCommaSeparatedArray(items: ExportSpecifier array) =
            printer.PrintArray(
                items,
                (fun p x -> p.Print(x)),
                (fun p -> p.Print(", "))
            )

        member printer.PrintCommaSeparatedArray
            (items: FunctionTypeParam array)
            =
            printer.PrintArray(
                items,
                (fun p x -> p.Print(x)),
                (fun p -> p.Print(", "))
            )

        member printer.PrintCommaSeparatedArray(items: TypeAnnotation array) =
            printer.PrintArray(
                items,
                (fun p x -> p.Print(x)),
                (fun p -> p.Print(", "))
            )

        member printer.PrintCommaSeparatedArray(items: TypeParameter array) =
            printer.PrintArray(
                items,
                (fun p x -> p.Print(x)),
                (fun p -> p.Print(", "))
            )

        member printer.PrintCommaSeparatedArray(items: Expression array) =
            printer.PrintArray(
                items,
                (fun p x -> p.Print(x)),
                (fun p -> p.Print(", "))
            )

        member printer.PrintClass
            (
                id: Identifier option,
                superClass: SuperClass option,
                typeParameters: TypeParameter[],
                implements: TypeAnnotation array,
                members: ClassMember array,
                loc
            )
            =
            printer.Print("class", ?loc = loc)
            printer.PrintOptional(id, " ")
            printer.Print(typeParameters)

            printer.PrintOptional(
                superClass,
                (fun p s ->
                    match s with
                    | SuperType item -> p.Print(item)
                    | SuperExpression item -> p.Print(item)
                ),
                " extends "
            )
            // printer.PrintOptional(superTypeParameters)
            implements
            |> Array.filter (
                function
                | AliasTypeAnnotation _ -> true
                | _ -> false
            )
            |> function
                | [||] -> ()
                | implements ->
                    printer.Print(" implements ")

                    printer.PrintArray(
                        implements,
                        (fun p x -> p.Print(x)),
                        (fun p -> p.Print(", "))
                    )

            printer.Print(" ")

            printer.PrintBlock(
                members,
                (fun p x -> p.PrintClassMember(x)),
                (fun p -> p.PrintStatementSeparator())
            )

        member printer.PrintFunction
            (
                id: Identifier option,
                parameters: Parameter array,
                body: BlockStatement,
                typeParameters: TypeParameter[],
                returnType: TypeAnnotation option,
                loc,
                ?isDeclaration,
                ?isArrow
            )
            =

            let (|ImmediatelyApplied|_|) =
                function
                | CallExpression(callee, appliedArgs, _typeParameters, _) when
                    parameters.Length = appliedArgs.Length
                    ->
                    // To be sure we're not running side effects when deleting the function check the callee is an identifier
                    match callee with
                    | Expression.Identifier(_) ->
                        Array.zip parameters appliedArgs
                        |> Array.forall (
                            function
                            | Parameter.Parameter(name = name1),
                              Expression.Identifier(Identifier(name = name2)) ->
                                name1 = name2
                            | _ -> false
                        )
                        |> function
                            | true -> Some callee
                            | false -> None
                    | _ -> None
                | _ -> None

            let isDeclaration = defaultArg isDeclaration false
            let isArrow = defaultArg isArrow false
            printer.AddLocation(loc)

            match body.Body with
            // Check if we can remove the function
            | [| ReturnStatement(ImmediatelyApplied e, _) |] when
                not isDeclaration
                ->
                printer.Print(e)
            | _ when isArrow ->
                // Remove parens if we only have one argument? (and no annotation)
                printer.Print(typeParameters)
                printer.Print("(")
                printer.PrintParameters(parameters)
                printer.Print(")")
                printer.PrintOptional(returnType, ": ")
                printer.Print(" => ")

                match body.Body with
                | [| ReturnStatement(argument, _loc) |] ->
                    match argument with
                    | ObjectExpression(_) -> printer.WithParens(argument)
                    | MemberExpression(object, property, isComputed, loc) ->
                        match object with
                        | ObjectExpression(_) ->
                            printer.PrintMemberExpression(
                                object,
                                property,
                                isComputed,
                                loc,
                                objectWithParens = true
                            )
                        | _ -> printer.Print(argument)
                    | _ -> printer.ComplexExpressionWithParens(argument)
                | _ -> printer.PrintBlock(body.Body, skipNewLineAtEnd = true)
            | _ ->
                printer.Print("function ")
                printer.PrintOptional(id)
                printer.Print(typeParameters)
                printer.Print("(")
                printer.PrintParameters(parameters)
                printer.Print(")")
                printer.PrintOptional(returnType, ": ")
                printer.Print(" ")
                printer.PrintBlock(body.Body, skipNewLineAtEnd = true)

        member printer.WithParens(expr: Expression) =
            printer.Print("(")
            printer.Print(expr)
            printer.Print(")")

        /// Should the expression be printed with parens when nested?
        member printer.IsComplex(expr: Expression) =
            match expr with
            | CommentedExpression(_, e) -> printer.IsComplex(e)
            | Undefined _
            | Literal(NullLiteral _)
            | Literal(Literal.StringLiteral _)
            | Literal(BooleanLiteral _)
            | Literal(NumericLiteral _)
            | Literal(EnumCaseLiteral _)
            | Expression.Identifier _
            | MemberExpression _
            | CallExpression _
            | ThisExpression _
            | Super _
            | SpreadElement _
            | ArrayExpression _
            | ObjectExpression _
            | JsxTemplate _
            | JsxElement _
            | UnaryExpression _ -> false
            | _ -> true

        /// Surround with parens anything that can potentially conflict with operator precedence
        member printer.ComplexExpressionWithParens(expr: Expression) =
            if printer.IsComplex(expr) then
                printer.WithParens(expr)
            else
                printer.Print(expr)

        member printer.ComplexTypeWithParens(t: TypeAnnotation) =
            match t with
            // FunctionTypeAnnotation is printed with parens by default. Use parens only when needed?
            // | FunctionTypeAnnotation _
            | UnionTypeAnnotation _
            | IntersectionTypeAnnotation _
            | KeyofTypeAnnotation _
            | TypeofTypeAnnotation _ ->
                printer.Print("(")
                printer.Print(t)
                printer.Print(")")
            | _ -> printer.Print(t)

        member printer.PrintOperation(left, operator, right, loc) =
            printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(left)
            printer.Print(" " + operator + " ")
            printer.ComplexExpressionWithParens(right)

        member printer.PrintJsxTemplate(parts: string[], values: Expression[]) =
            // Do we need to escape backslashes here?
            let escape str = str //Regex.Replace(str, @"(?<!\\)\\", @"\\")

            if parts.Length = 1 then
                printer.Print(escape parts[0])
            else
                for i = 0 to parts.Length - 2 do
                    printer.Print(escape parts[i])

                    match values[i] with
                    | JsxElement(componentOrTag, props, children) ->
                        printer.PrintJsxElement(componentOrTag, props, children)
                    | JsxTemplate(parts, values) ->
                        printer.PrintJsxTemplate(parts, values)
                    | value ->
                        printer.Print("{")
                        printer.Print(value)
                        printer.Print("}")

                printer.Print(Array.last parts |> escape)

        member printer.PrintJsxElement
            (
                componentOrTag: Expression,
                props: (string * Expression) list,
                children: Expression list
            )
            =
            let printTag =
                function
                | StringConstant tag -> printer.Print(tag)
                | componentRef -> printer.Print(componentRef)

            printer.Print("<")
            printTag componentOrTag

            if not (List.isEmpty props) then
                printer.PushIndentation()

                let mutable isFirst = true

                let printProp print =
                    if not isFirst then
                        printer.PrintNewLine()
                    else
                        isFirst <- false
                        printer.Print(" ")

                    print ()

                props
                |> List.iter (
                    function
                    | _, NullOrUndefinedOrVoid -> ()
                    | key, StringConstant value ->
                        printProp (fun () ->
                            printer.Print($"{key}=\"{value}\"")
                        )
                    | key, value ->
                        printProp (fun () ->
                            printer.Print(key + "={")
                            printer.Print(value)
                            printer.Print("}")
                        )
                )

                printer.PopIndentation()

            printer.Print(">")

            if not (List.isEmpty children) then
                printer.PrintNewLine()
                printer.PushIndentation()

                children
                |> List.iter (
                    function
                    | NullOrUndefinedOrVoid -> ()
                    | StringConstant text ->
                        printer.Print(text)
                        printer.PrintNewLine()
                    | JsxElement(componentOrTag, props, children) ->
                        printer.PrintJsxElement(componentOrTag, props, children)
                        printer.PrintNewLine()
                    | JsxTemplate(parts, values) ->
                        printer.PrintJsxTemplate(parts, values)
                        printer.PrintNewLine()
                    | child ->
                        printer.Print("{")
                        printer.Print(child)
                        printer.Print("}")
                        printer.PrintNewLine()
                )

                printer.PopIndentation()

            printer.Print("</")
            printTag componentOrTag
            printer.Print(">")

        member printer.Print(expr: Expression) =
            match expr with
            | CommentedExpression(comment, expr) ->
                printer.Print("/* " + comment + " */ ")
                printer.Print(expr)
            | JsxElement(componentOrTag, props, children) ->
                printer.PrintJsxElement(componentOrTag, props, children)
            | JsxTemplate(parts, values) ->
                printer.PrintJsxTemplate(parts, values)
            | Super(loc) -> printer.Print("super", ?loc = loc)
            | Literal(n) -> printer.PrintLiteral(n)
            | Undefined(loc) -> printer.Print("undefined", ?loc = loc)
            | Expression.Identifier(n) -> printer.PrintIdent(n)
            | NewExpression(callee, args, typeArguments, loc) ->
                printer.PrintNewExpression(callee, args, typeArguments, loc)
            | SpreadElement(argument, loc) ->
                printer.Print("...", ?loc = loc)
                printer.ComplexExpressionWithParens(argument)
            | ThisExpression(loc) -> printer.Print("this", ?loc = loc)
            | CallExpression(callee, args, typeArguments, loc) ->
                printer.PrintCallExpression(callee, args, typeArguments, loc)
            | EmitExpression(value, args, loc) ->
                printer.PrintEmitExpression(value, args, loc)
            | ArrayExpression(elements, loc) ->
                printer.Print("[", ?loc = loc)
                printer.PrintCommaSeparatedArray(elements)
                printer.Print("]")
            | ClassExpression(body,
                              id,
                              superClass,
                              implements,
                              typeArguments,
                              loc) ->
                printer.PrintClass(
                    id,
                    superClass,
                    typeArguments,
                    implements,
                    body,
                    loc
                )
            | UnaryExpression(argument, operator, isSuffix, loc) ->
                printer.PrintUnaryExpression(argument, operator, isSuffix, loc)
            | UpdateExpression(prefix, argument, operator, loc) ->
                printer.PrintUpdateExpression(prefix, argument, operator, loc)
            | ObjectExpression(properties, loc) ->
                printer.PrintObjectExpression(properties, loc)
            | BinaryExpression(left, right, operator, loc) ->
                printer.PrintOperation(left, operator, right, loc)
            | MemberExpression(object, property, isComputed, loc) ->
                printer.PrintMemberExpression(object, property, isComputed, loc)
            | LogicalExpression(left, operator, right, loc) ->
                printer.PrintOperation(left, operator, right, loc)
            | SequenceExpression(expressions, loc) ->
                // A comma-separated sequence of expressions.
                printer.AddLocation(loc)
                let last = expressions.Length - 1

                let expressions =
                    expressions
                    |> Array.filteri (fun i e -> i = last || hasSideEffects e)

                if expressions.Length = 1 then
                    printer.Print(expressions[0])
                else
                    let last = expressions.Length - 1
                    printer.Print("(")

                    for i = 0 to last do
                        let e = expressions[i]
                        printer.Print(e)

                        if i < last then
                            printer.Print(", ")

                    printer.Print(")")
            | AssignmentExpression(left, right, operator, loc) ->
                printer.PrintOperation(left, operator, right, loc)
            | ConditionalExpression(test, consequent, alternate, loc) ->
                printer.PrintConditionalExpression(
                    test,
                    consequent,
                    alternate,
                    loc
                )
            | FunctionExpression(id,
                                 parameters,
                                 body,
                                 typeParameters,
                                 returnType,
                                 loc) ->
                printer.PrintFunction(
                    id,
                    parameters,
                    body,
                    returnType,
                    typeParameters,
                    loc
                )
            | ArrowFunctionExpression(parameters,
                                      body,
                                      returnType,
                                      typeParameters,
                                      loc) ->
                printer.PrintArrowFunctionExpression(
                    parameters,
                    body,
                    returnType,
                    typeParameters,
                    loc
                )
            | AsExpression(expression, typeAnnotation) ->
                printer.Print(expression)
                printer.Print(" as ")
                printer.Print(typeAnnotation)

        member printer.PrintLiteral(literal: Literal) =
            match literal with
            | RegExp(pattern, flags, loc) ->
                printer.PrintRegExp(pattern, flags, loc)
            | NullLiteral(loc) -> printer.Print("null", ?loc = loc)
            | Literal.StringLiteral(l) -> printer.Print(l)
            | BooleanLiteral(value, loc) ->
                printer.Print(
                    (if value then
                         "true"
                     else
                         "false"),
                    ?loc = loc
                )
            | BigIntLiteral(value, loc) -> printer.PrintBigInt(value, loc)
            | NumericLiteral(value, loc) -> printer.PrintNumeric(value, loc)
            | Literal.DirectiveLiteral(literal) -> printer.Print(literal)
            | StringTemplate(tag, parts, values, loc) ->
                let escape str =
                    Regex.Replace(str, @"(?<!\\)\\", @"\\").Replace("`", @"\`")

                printer.AddLocation(loc)
                printer.PrintOptional(tag)
                printer.Print("`")

                for i = 0 to parts.Length - 2 do
                    printer.Print(escape parts[i])
                    printer.Print("${")
                    printer.Print(values[i])
                    printer.Print("}")

                printer.Print(Array.last parts |> escape)
                printer.Print("`")
            | EnumCaseLiteral(id, case) ->
                printer.PrintIdent(id)
                printer.Print(".")
                printer.Print(case)

        member printer.Print(stmt: Statement) =
            match stmt with
            | Declaration(s) -> printer.PrintDeclaration(s)
            | IfStatement(test, consequent, alternate, loc) ->
                printer.PrintIfStatement(test, consequent, alternate, loc)
            | TryStatement(block, handler, finalizer, loc) ->
                printer.PrintTryStatement(block, handler, finalizer, loc)
            | ForStatement(body, init, test, update, loc) ->
                printer.PrintForStatement(body, init, test, update, loc)
            | BreakStatement(label, loc) ->
                printer.Print("break", ?loc = loc)
                printer.PrintOptional(label, " ")
            | WhileStatement(test, body, loc) ->
                printer.PrintWhileStatement(test, body, loc)
            | ThrowStatement(argument, loc) ->
                printer.Print("throw ", ?loc = loc)
                printer.Print(argument)
            | Statement.BlockStatement(s) -> printer.Print(s)
            | ReturnStatement(argument, loc) ->
                printer.Print("return ", ?loc = loc)
                // If a JSX template starts with a new line, surround it in parens to avoid
                // having only return in single line (this causes JS to ignore the rest of the code)
                match argument with
                | JsxTemplate(parts, _) when Regex.IsMatch(parts[0], @"^\s*\n") ->
                    printer.WithParens(argument)
                | _ -> printer.Print(argument)
            | SwitchStatement(discriminant, cases, loc) ->
                printer.PrintSwitchStatement(discriminant, cases, loc)
            | LabeledStatement(body, label) ->
                printer.PrintLabeledStatement(body, label)
            | DebuggerStatement(loc) -> printer.Print("debugger", ?loc = loc)
            | ContinueStatement(label, loc) ->
                printer.Print("continue", ?loc = loc)
                printer.PrintOptional(label, " ")

            | ExpressionStatement(expr) ->
                match expr with
                | UnaryExpression(argument, "void", false, _loc) ->
                    printer.Print(argument)
                | _ -> printer.Print(expr)

        member printer.PrintJsDoc(doc: string option) =
            match doc with
            | None -> ()
            | Some doc ->
                // TODO: Check docs with params, etc
                let regex =
                    Regex(
                        @"<summary>([\s\S]*?)</summary>",
                        RegexOptions.Compiled
                    )

                let m = regex.Match(doc)

                if m.Success then
                    let lines = m.Groups[1].Value.Trim().Split('\n')
                    printer.Print("/**")
                    printer.PrintNewLine()

                    for line in lines do
#if !FABLE_COMPILER
                        let line = System.Web.HttpUtility.HtmlDecode(line)
#endif
                        printer.Print(" * ")
                        printer.Print(line.Trim())
                        printer.PrintNewLine()

                    printer.Print(" */")
                    printer.PrintNewLine()

        member printer.PrintDeclaration(decl: Declaration) =
            match decl with
            | ClassDeclaration(body,
                               id,
                               superClass,
                               implements,
                               typeParameters,
                               loc,
                               _doc) ->
                printer.PrintClass(
                    id,
                    superClass,
                    typeParameters,
                    implements,
                    body,
                    loc
                )
            | Declaration.VariableDeclaration(d) -> printer.Print(d)
            | FunctionDeclaration(parameters,
                                  body,
                                  id,
                                  returnType,
                                  typeParameters,
                                  loc,
                                  _doc) ->
                printer.PrintFunction(
                    Some id,
                    parameters,
                    body,
                    typeParameters,
                    returnType,
                    loc,
                    isDeclaration = true
                )

                printer.PrintNewLine()
            | InterfaceDeclaration(id, body, extends, typeParameters) ->
                printer.PrintInterfaceDeclaration(
                    id,
                    body,
                    extends,
                    typeParameters
                )
            | EnumDeclaration(name, cases, isConst) ->
                if isConst then
                    printer.Print("const ")

                printer.Print("enum " + name + " {")
                printer.PrintNewLine()
                printer.PushIndentation()
                let last = cases.Length - 1

                cases
                |> Array.iteri (fun i (name, value) ->
                    printer.Print(name)
                    printer.Print(" = ")
                    printer.Print(value)

                    if i < last then
                        printer.Print(",")
                        printer.PrintNewLine()
                )

                printer.PrintNewLine()
                printer.PopIndentation()
                printer.Print("}")
                printer.PrintNewLine()
            | TypeAliasDeclaration(id, paramsDecl, typ) ->
                printer.Print("type ")
                printer.Print(id)
                printer.Print(paramsDecl)
                printer.Print(" = ")

                match typ with
                | UnionTypeAnnotation(types) ->
                    printer.PrintNewLine()
                    printer.PushIndentation()

                    for typ in types do
                        printer.Print("| ")
                        printer.Print(typ)
                        printer.PrintNewLine()

                    printer.PopIndentation()
                | ObjectTypeAnnotation(members) ->
                    printer.PrintAbstractMembers(members, singleLine = false)
                | typ -> printer.Print(typ)

        member printer.Print(md: ModuleDeclaration) =
            match md with
            | ImportDeclaration(specifiers, source) ->
                printer.PrintImportDeclaration(specifiers, source)
            | ExportNamedReferences(specifiers, source) ->
                printer.Print("export ")
                printer.Print("{ ")
                printer.PrintCommaSeparatedArray(specifiers)
                printer.Print(" }")
                printer.PrintOptional(source, " from ")
            | ExportNamedDeclaration(declaration) ->
                printer.PrintJsDoc(declaration.JsDoc)
                printer.Print("export ")
                printer.PrintDeclaration(declaration)
            | ExportAllDeclaration(source, loc) ->
                printer.Print("export * from ", ?loc = loc)
                printer.PrintLiteral(source)
            | PrivateModuleDeclaration(statement) ->
                if isProductiveStatement (statement) then
                    printer.Print(statement)
            | ExportDefaultDeclaration(declaration) ->
                match declaration with
                | Choice1Of2 d -> printer.PrintJsDoc(d.JsDoc)
                | Choice2Of2 _ -> ()

                printer.Print("export default ")

                match declaration with
                | Choice1Of2 x -> printer.PrintDeclaration(x)
                | Choice2Of2 x -> printer.Print(x)

        member printer.PrintEmitExpression(value, args, loc) =
            printer.AddLocation(loc)

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
                    @"\{\{\s*\$(\d+)\s*\?\s*(.*?)\s*:\s*(.*?)\s*\}\}"
                    (fun m ->
                        let i = int m.Groups[1].Value

                        match Array.tryItem i args with
                        | Some expr ->
                            match expr with
                            | Literal(BooleanLiteral(value = false))
                            | NullOrUndefinedOrVoid -> m.Groups[3].Value
                            | _ -> m.Groups[2].Value
                        | None -> m.Groups[3].Value
                    )

                |> replace
                    @"\{\{([^\}]*\$(\d+).*?)\}\}"
                    (fun m ->
                        let i = int m.Groups[2].Value

                        match Array.tryItem i args with
                        | Some _ -> m.Groups[1].Value
                        | None -> ""
                    )

                // If placeholder is followed by !, emit string literals as JS: "let $0! = $1"
                |> replace
                    @"\$(\d+)!"
                    (fun m ->
                        let i = int m.Groups[1].Value

                        match Array.tryItem i args with
                        | Some(StringConstant value) -> value
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

                    match Array.tryItem argIndex args with
                    | Some e when isSurroundedWithParens -> printer.Print(e)
                    | Some e -> printer.ComplexExpressionWithParens(e)
                    | None -> printer.Print("undefined")

                let lastMatch = matches[matches.Count - 1]

                printSegment
                    printer
                    value
                    (lastMatch.Index + lastMatch.Length)
                    value.Length
            else
                printSegment printer value 0 value.Length

        member printer.PrintIdent(identifier: Identifier) =
            let (Identifier(name, loc)) = identifier
            printer.Print(name, ?loc = loc)

        member printer.PrintRegExp(pattern, flags, loc) =
            printer.Print("/", ?loc = loc)
            // Note we cannot use Naming.escapeString because it will corrupt the regex pattern
            printer.Print(
                Regex
                    .Replace(pattern, @"(?<!\\)\/", @"\/")
                    .Replace("\r", @"\r")
                    .Replace("\n", @"\n")
            )

            printer.Print("/")
            printer.Print(flags)

        member printer.Print(node: StringLiteral) =
            let (StringLiteral(value, loc)) = node
            printer.Print("\"", ?loc = loc)
            printer.Print(Naming.escapeString (fun _ -> false) value)
            printer.Print("\"")

        member printer.PrintBigInt(value, loc) =
            printer.Print(value + "n", ?loc = loc)

        member printer.PrintNumeric(value, loc) =
            let value =
                match
                    value.ToString(
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | "∞" -> "Infinity"
                | "-∞" -> "-Infinity"
                | value -> value

            printer.Print(value, ?loc = loc)

        member printer.Print(node: BlockStatement) =
            printer.PrintBlock(node.Body)

        member printer.PrintLabeledStatement(body, label) =
            printer.PrintIdent(label)
            printer.Print(":")
            printer.PrintNewLine()
            // Don't push indent
            printer.Print(body)

        // Control Flow
        member printer.PrintIfStatement(test, consequent, alternate, loc) =
            printer.AddLocation(loc)
            printer.Print("if (", ?loc = loc)
            printer.Print(test)
            printer.Print(") ")
            printer.Print(consequent)

            match alternate with
            | None -> ()
            | Some alternate ->
                if printer.Column > 0 then
                    printer.Print(" ")

                match alternate with
                | IfStatement(test, consequent, alternate, loc) ->
                    printer.Print("else ")
                    printer.PrintIfStatement(test, consequent, alternate, loc)
                | alternate ->
                    let statements =
                        match alternate with
                        | Statement.BlockStatement(b) -> b.Body
                        | alternate -> [| alternate |]
                    // Get productive statements and skip `else` if they're empty
                    statements
                    |> Array.filter isProductiveStatement
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

            let consequent =
                match consequent with
                | [| Statement.BlockStatement block |] -> block.Body
                | _ -> consequent

            match consequent.Length with
            | 0 -> printer.PrintNewLine()
            | 1 ->
                printer.PrintNewLine()
                printer.PushIndentation()
                printer.Print(consequent[0])
                printer.PrintStatementSeparator()
                printer.PopIndentation()
            | _ ->
                printer.Print(" ")
                printer.PrintBlock(consequent)

        member printer.PrintSwitchStatement(discriminant, cases, loc) =
            printer.Print("switch (", ?loc = loc)
            printer.Print(discriminant)
            printer.Print(") ")
            printer.PrintBlock(cases, (fun p x -> p.Print(x)), (fun _ -> ()))

        // Exceptions
        member printer.Print(node: CatchClause) =
            let (CatchClause(param, annotation, body, loc)) = node
            // "catch" is being printed by TryStatement
            printer.Print("(", ?loc = loc)
            printer.Print(param)
            printer.PrintOptional(annotation, ": ")
            printer.Print(") ")
            printer.Print(body)

        member printer.PrintTryStatement(block, handler, finalizer, loc) =
            printer.Print("try ", ?loc = loc)
            printer.Print(block)
            printer.PrintOptional(handler, "catch ")
            printer.PrintOptional(finalizer, "finally ")

        // Declarations

        member printer.Print(VariableDeclaration(declarations, kind, loc)) =
            if declarations.Length > 0 then
                let kind =
                    match kind with
                    | Var -> "var"
                    | Let -> "let"
                    | Const -> "const"

                printer.Print(kind + " ", ?loc = loc)
                let canConflict = declarations.Length > 1

                for i = 0 to declarations.Length - 1 do
                    let (VariableDeclarator(name,
                                            annotation,
                                            typeParams,
                                            init,
                                            loc)) =
                        declarations[i]

                    printer.Print(name, ?loc = loc)
                    // In some situations when inlining functions it may happen that a unit argument is assigned a value
                    // (see "Unit expression arguments are not removed" in ApplicativeTests). To prevent the TypeScript
                    // compiler from complaining we replace `void` type with `any`.
                    let annotation =
                        annotation
                        |> Option.map (
                            function
                            | VoidTypeAnnotation -> AnyTypeAnnotation
                            | t -> t
                        )

                    printer.PrintOptional(
                        annotation,
                        (fun p a ->
                            match a with
                            | FunctionTypeAnnotation(parameters,
                                                     returnType,
                                                     spread) ->
                                p.PrintFunctionTypeAnnotation(
                                    parameters,
                                    returnType,
                                    typeParams,
                                    ?spread = spread
                                )
                            | _ ->
                                p.Print(typeParams)
                                p.Print(a)
                        ),
                        ": "
                    )

                    match init with
                    | None -> ()
                    | Some e ->
                        printer.Print(" = ")

                        if canConflict then
                            printer.ComplexExpressionWithParens(e)
                        else
                            printer.Print(e)

                    if i < declarations.Length - 1 then
                        printer.Print(", ")

        member printer.PrintWhileStatement(test, body, loc) =
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

        /// A fat arrow function expression, e.g., let foo = (bar) => { body }
        member printer.PrintArrowFunctionExpression
            (
                parameters,
                body,
                returnType,
                typeParameters,
                loc
            )
            =
            printer.PrintFunction(
                None,
                parameters,
                body,
                typeParameters,
                returnType,
                loc,
                isArrow = true
            )

        member printer.Print(node: ObjectMember) =
            match node with
            | ObjectProperty(key, value, isComputed, doc) ->
                match value with
                | UndefinedOrVoid -> ()
                | value ->
                    printer.PrintJsDoc(doc)
                    printer.PrintObjectProperty(key, value, isComputed)

            | ObjectMethod(kind,
                           key,
                           parameters,
                           body,
                           isComputed,
                           returnType,
                           typeParameters,
                           loc,
                           doc) ->
                printer.PrintJsDoc(doc)

                printer.PrintObjectMethod(
                    kind,
                    key,
                    parameters,
                    body,
                    isComputed,
                    returnType,
                    typeParameters,
                    loc
                )

        member printer.PrintObjectProperty(key, value, isComputed) =
            if isComputed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)

            printer.Print(": ")
            printer.Print(value)

        member printer.PrintObjectMethod
            (
                kind,
                key,
                parameters,
                body,
                isComputed,
                returnType,
                typeParameters,
                loc
            )
            =
            printer.AddLocation(loc)

            let isSetter =
                match kind with
                | ObjectGetter ->
                    printer.Print("get ")
                    false
                | ObjectSetter ->
                    printer.Print("set ")
                    true
                | ObjectMeth -> false

            if isComputed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)

            printer.Print(typeParameters)
            printer.Print("(")
            printer.PrintParameters(parameters)
            printer.Print(")")

            if not isSetter then
                printer.PrintOptional(returnType, ": ")

            printer.Print(" ")

            printer.PrintBlock(body.Body, skipNewLineAtEnd = true)

        member printer.Print(m: AbstractMember) =
            match m with
            | AbstractProperty(key, returnType, isComputed, isOptional, doc) ->
                printer.PrintJsDoc(doc)

                if isComputed then
                    printer.Print("[")
                    printer.Print(key)
                    printer.Print("]")
                else
                    printer.Print(key)

                if isOptional then
                    printer.Print("?")

                printer.Print(": ")
                printer.Print(returnType)

            | AbstractMethod(kind,
                             key,
                             parameters,
                             returnType,
                             typeParameters,
                             isComputed,
                             doc) ->
                let isSetter =
                    match kind with
                    | ObjectGetter ->
                        printer.Print("get ")
                        false
                    | ObjectSetter ->
                        printer.Print("set ")
                        true
                    | ObjectMeth -> false

                printer.PrintJsDoc(doc)

                if isComputed then
                    printer.Print("[")
                    printer.Print(key)
                    printer.Print("]")
                else
                    printer.Print(key)

                printer.Print(typeParameters)
                printer.Print("(")
                printer.PrintParameters(parameters)
                printer.Print(")")

                if not isSetter then
                    printer.Print(": ")
                    printer.Print(returnType)

        member printer.PrintAbstractMembers
            (
                members: AbstractMember[],
                ?singleLine: bool
            )
            =
            let singleLine = defaultArg singleLine false

            if singleLine then
                printer.Print("{ ")

                printer.PrintArray(
                    members,
                    (fun p x -> p.Print(x)),
                    (fun p -> p.Print(", "))
                )

                printer.Print(" }")
            else
                printer.Print("{")
                printer.PrintNewLine()
                printer.PushIndentation()

                printer.PrintArray(
                    members,
                    (fun p x -> p.Print(x)),
                    (fun p ->
                        p.Print(",")
                        p.PrintNewLine()
                    )
                )

                printer.PopIndentation()
                printer.PrintNewLine()
                printer.Print("}")
                printer.PrintNewLine()

        member printer.PrintMemberExpression
            (
                object,
                property,
                isComputed,
                loc,
                ?objectWithParens: bool
            )
            =
            printer.AddLocation(loc)

            match objectWithParens, object with
            | Some true, _
            | _, Literal(NumericLiteral(_)) -> printer.WithParens(object)
            | _ -> printer.ComplexExpressionWithParens(object)

            if isComputed then
                printer.Print("[")
                printer.Print(property)
                printer.Print("]")
            else
                printer.Print(".")
                printer.Print(property)

        member printer.PrintObjectExpression(properties, loc) =
            let printSeparator (p: Printer) =
                p.Print(",")
                p.PrintNewLine()

            printer.AddLocation(loc)

            properties
            |> Array.filter (
                function
                | ObjectProperty(_, UndefinedOrVoid, _, _) -> false
                | _ -> true
            )
            |> function
                | [||] -> printer.Print("{}")
                | properties ->
                    printer.PrintBlock(
                        properties,
                        (fun p x -> p.Print(x)),
                        printSeparator,
                        skipNewLineAtEnd = true
                    )

        member printer.PrintConditionalExpression
            (
                test,
                consequent,
                alternate,
                loc
            )
            =
            printer.AddLocation(loc)

            match test, consequent, alternate with
            // TODO: Move node optimization to Fable2Babel as with IfStatement?
            | Literal(BooleanLiteral(value = value)), _, _ ->
                if value then
                    printer.Print(consequent)
                else
                    printer.Print(alternate)
            | test,
              Literal(BooleanLiteral(true, _)),
              Literal(BooleanLiteral(false, _)) -> printer.Print(test)
            | test,
              Literal(BooleanLiteral(false, _)),
              Literal(BooleanLiteral(true, _)) ->
                printer.PrintUnaryExpression(test, "!", false, loc)
            | test, _, Literal(BooleanLiteral(false, _)) ->
                printer.PrintOperation(test, "&&", consequent, loc)
            | _ ->
                printer.ComplexExpressionWithParens(test)
                printer.Print(" ? ")
                printer.ComplexExpressionWithParens(consequent)
                printer.Print(" : ")
                printer.ComplexExpressionWithParens(alternate)

        member printer.PrintCallExpression(callee, args, typeArguments, loc) =
            printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(callee)
            printer.Print(typeArguments)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(args)
            printer.Print(")")

        member printer.PrintNewExpression(callee, args, typeArguments, loc) =
            printer.Print("new ", ?loc = loc)
            printer.ComplexExpressionWithParens(callee)
            printer.Print(typeArguments)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(args)
            printer.Print(")")

        member printer.PrintUnaryExpression(argument, operator, isSuffix, loc) =
            let printOp () =
                match operator with
                | "-"
                | "+"
                | "!"
                | "~" -> printer.Print(operator)
                | _ ->
                    printer.Print(
                        if isSuffix then
                            " " + operator
                        else
                            operator + " "
                    )

            printer.AddLocation(loc)

            if isSuffix then
                printer.ComplexExpressionWithParens(argument)
                printOp ()
            else
                printOp ()
                printer.ComplexExpressionWithParens(argument)

        member printer.PrintUpdateExpression(prefix, argument, operator, loc) =
            printer.AddLocation(loc)

            if prefix then
                printer.Print(operator)
                printer.ComplexExpressionWithParens(argument)
            else
                printer.ComplexExpressionWithParens(argument)
                printer.Print(operator)

        member printer.PrintClassMember(memb: ClassMember) =
            match memb with
            | ClassMethod(kind,
                          parameters,
                          body,
                          isStatic,
                          isAbstract,
                          returnType,
                          typeParameters,
                          loc,
                          doc) ->
                printer.PrintJsDoc(doc)

                printer.PrintClassMethod(
                    kind,
                    parameters,
                    body,
                    isStatic = isStatic,
                    isAbstract = isAbstract,
                    returnType = returnType,
                    typeParameters = typeParameters,
                    loc = loc
                )
            | ClassProperty(key,
                            value,
                            isComputed,
                            isStatic,
                            isOptional,
                            typeAnnotation,
                            accessModifier,
                            loc,
                            doc) ->
                printer.PrintJsDoc(doc)

                printer.PrintClassProperty(
                    key,
                    value,
                    isComputed,
                    isStatic = isStatic,
                    isOptional = isOptional,
                    typeAnnotation = typeAnnotation,
                    accessModifier = accessModifier,
                    loc = loc
                )

        member printer.PrintClassMethod
            (
                kind,
                parameters,
                body,
                isStatic,
                isAbstract,
                returnType,
                typeParameters,
                loc
            )
            =
            printer.AddLocation(loc)

            if isStatic then
                printer.Print("static ")

            if isAbstract then
                printer.Print("abstract ")

            let isSetter =
                match kind with
                | ClassGetter _ ->
                    printer.Print("get ")
                    false
                | ClassSetter _ ->
                    printer.Print("set ")
                    true
                | ClassPrimaryConstructor _
                | ClassFunction _ -> false

            let key, isComputed, accessModifiers =
                match kind with
                | ClassSetter(key, isComputed)
                | ClassGetter(key, isComputed)
                | ClassFunction(key, isComputed) -> key, isComputed, [||]
                | ClassPrimaryConstructor accessModifiers ->
                    Expression.identifier ("constructor"),
                    false,
                    accessModifiers

            if isComputed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)

            printer.Print(typeParameters)
            printer.Print("(")
            printer.PrintParameters(parameters, accessModifiers)
            printer.Print(")")

            if not isSetter then
                printer.PrintOptional(returnType, ": ")

            printer.Print(" ")

            printer.Print(body)

        member printer.PrintAccessModifier =
            function
            | None -> ()
            | Some Public -> printer.Print("public ")
            | Some Private -> printer.Print("private ")
            | Some Protected -> printer.Print("protected ")
            | Some Readonly -> printer.Print("readonly ")

        member printer.PrintClassProperty
            (
                key,
                value,
                isComputed,
                isStatic,
                isOptional,
                typeAnnotation,
                accessModifier,
                loc
            )
            =
            printer.AddLocation(loc)

            if isStatic then
                printer.Print("static ")

            printer.PrintAccessModifier(accessModifier)

            if isComputed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)

            if isOptional then
                printer.Print("?")

            printer.PrintOptional(typeAnnotation, ": ")
            printer.PrintOptional(value, " = ")

        member printer.PrintImportMemberSpecific(local, imported) =
            // Don't print the braces, node will be done in the import declaration
            printer.PrintIdent(imported)

            if imported.Name <> local.Name then
                printer.Print(" as ")
                printer.PrintIdent(local)

        member printer.PrintImportNamespaceSpecifier(local) =
            printer.Print("* as ")
            printer.PrintIdent(local)

        member printer.PrintImportDeclaration(specifiers, source) =
            let members =
                specifiers
                |> Array.choose (
                    function
                    | ImportMemberSpecifier(local, imported) ->
                        Some(ImportMemberSpecifier(local, imported))
                    | _ -> None
                )

            let defaults =
                specifiers
                |> Array.choose (
                    function
                    | ImportDefaultSpecifier(local) ->
                        Some(ImportDefaultSpecifier(local))
                    | _ -> None
                )

            let namespaces =
                specifiers
                |> Array.choose (
                    function
                    | ImportNamespaceSpecifier(local) ->
                        Some(ImportNamespaceSpecifier(local))
                    | _ -> None
                )

            printer.Print("import ")

            if not (Array.isEmpty defaults) then
                printer.PrintCommaSeparatedArray(defaults)

                if not (Array.isEmpty namespaces && Array.isEmpty members) then
                    printer.Print(", ")

            if not (Array.isEmpty namespaces) then
                printer.PrintCommaSeparatedArray(namespaces)

                if not (Array.isEmpty members) then
                    printer.Print(", ")

            if not (Array.isEmpty members) then
                printer.Print("{ ")
                printer.PrintCommaSeparatedArray(members)
                printer.Print(" }")

            if
                not (
                    Array.isEmpty defaults
                    && Array.isEmpty namespaces
                    && Array.isEmpty members
                )
            then
                printer.Print(" from ")

            printer.Print("\"")
            let (StringLiteral(value, _)) = source
            printer.Print(printer.MakeImportPath(value))
            printer.Print("\"")

        member printer.Print(node: ExportSpecifier) =
            let (ExportSpecifier(local, exported)) = node
            // Don't print the braces, node will be done in the export declaration
            printer.PrintIdent(local)

            if exported.Name <> local.Name then
                printer.Print(" as ")
                printer.PrintIdent(exported)

        member printer.Print(node: TypeAnnotation) =
            match node with
            | AliasTypeAnnotation(id, typeParams) ->
                printer.PrintIdent(id)
                printer.Print(typeParams)
            | StringTypeAnnotation -> printer.Print("string")
            | NumberTypeAnnotation -> printer.Print("number")
            | BooleanTypeAnnotation -> printer.Print("boolean")
            | AnyTypeAnnotation -> printer.Print("any")
            | VoidTypeAnnotation -> printer.Print("void")
            | UndefinedTypeAnnotation -> printer.Print("undefined")
            | ArrayTypeAnnotation(t) ->
                printer.ComplexTypeWithParens(t)
                printer.Print("[]")
            | TupleTypeAnnotation(types) ->
                printer.Print("[")
                printer.PrintCommaSeparatedArray(types)
                printer.Print("]")
            | UnionTypeAnnotation(types) ->
                printer.PrintArray(
                    types,
                    (fun p x -> p.ComplexTypeWithParens(x)),
                    (fun p -> p.Print(" | "))
                )
            | IntersectionTypeAnnotation(types) ->
                printer.PrintArray(
                    types,
                    (fun p x -> p.ComplexTypeWithParens(x)),
                    (fun p -> p.Print(" & "))
                )
            | FunctionTypeAnnotation(parameters, returnType, spread) ->
                printer.PrintFunctionTypeAnnotation(
                    parameters,
                    returnType,
                    [||],
                    ?spread = spread
                )
            | ObjectTypeAnnotation(members) ->
                printer.PrintAbstractMembers(members, singleLine = true)
            | KeyofTypeAnnotation typ ->
                printer.Print("keyof ")
                printer.ComplexTypeWithParens(typ)
            | TypeofTypeAnnotation exp ->
                printer.Print("typeof ")
                printer.ComplexExpressionWithParens(exp)
            | IndexedTypeAnnotation(typ, prop) ->
                printer.Print(typ)
                printer.Print("[")
                printer.Print(prop)
                printer.Print("]")
            | LiteralTypeAnnotation lit -> printer.PrintLiteral(lit)

        member printer.Print
            ((TypeParameter(name, bound, _default)): TypeParameter)
            =
            printer.Print(name)
            printer.PrintOptional(bound, " extends ")
        // printer.PrintOptional(``default``)

        member printer.Print(parameters: TypeParameter[]) =
            if parameters.Length > 0 then
                printer.Print("<")
                printer.PrintCommaSeparatedArray(parameters)
                printer.Print(">")

        member printer.Print(parameters: TypeAnnotation[]) =
            if parameters.Length > 0 then
                printer.Print("<")
                printer.PrintCommaSeparatedArray(parameters)
                printer.Print(">")

        member printer.Print(node: FunctionTypeParam) =
            let (FunctionTypeParam(name, typeAnnotation, isOptional)) = node
            printer.PrintIdent(name)

            if isOptional then
                printer.Print("?")

            printer.Print(": ")
            printer.Print(typeAnnotation)

        member printer.PrintFunctionTypeAnnotation
            (
                parameters,
                returnType,
                typeParameters,
                ?spread
            )
            =
            printer.Print("(")
            printer.Print(typeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(parameters)

            match spread with
            | Some spread ->
                printer.Print("...")
                printer.Print(spread)
            | None -> ()

            printer.Print(") => ")
            printer.Print(returnType)
            printer.Print(")")

        member printer.PrintInterfaceDeclaration
            (
                id,
                members,
                extends,
                typeParameters
            )
            =
            printer.Print("interface ")
            printer.PrintIdent(id)
            printer.Print(typeParameters)

            if not (Array.isEmpty extends) then
                printer.Print(" extends ")

                printer.PrintArray(
                    extends,
                    (fun p x -> p.Print(x)),
                    (fun p -> p.Print(", "))
                )

            printer.Print(" ")
            printer.PrintAbstractMembers(members)

open PrinterExtensions

let run writer (program: Program) : Async<unit> =
    let printDeclWithExtraLine
        extraLine
        (printer: Printer)
        (decl: ModuleDeclaration)
        =
        printer.Print(decl)

        if printer.Column > 0 then
            printer.Print(";")
            printer.PrintNewLine()

        if extraLine then
            printer.PrintNewLine()

    async {
        use printer = new PrinterImpl(writer)

        let imports, restDecls =
            program.Body
            |> Array.splitWhile (
                function
                | ImportDeclaration(_) -> true
                | _ -> false
            )

        for decl in imports do
            printDeclWithExtraLine false printer decl

        (printer :> Printer).PrintNewLine()
        do! printer.Flush()

        for decl in restDecls do
            printDeclWithExtraLine true printer decl
            // TODO: Only flush every XXX lines?
            do! printer.Flush()
    }
