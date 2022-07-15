// fsharplint:disable InterfaceNames
module Fable.Transforms.BabelPrinter

open System.Text.RegularExpressions
open Fable
open Fable.AST
open Fable.AST.Babel
open Fable.Transforms.Printer

module PrinterExtensions =
    let rec hasSideEffects(e: Expression) =
        match e with
        | Undefined(_)
        | Literal(NullLiteral(_))
        | Literal(Literal.StringLiteral(_))
        | Literal(BooleanLiteral(_))
        | Literal(NumericLiteral(_)) -> false
        // Constructors of classes deriving from System.Object add an empty object at the end
        | ObjectExpression(properties, _loc) -> properties.Length > 0
        | UnaryExpression(argument, operator, _loc) when operator = "void" -> hasSideEffects(argument)
        // Some identifiers may be stranded as the result of imports
        // intended only for side effects, see #2228
        | Expression.Identifier(_) -> false
        // Sometimes empty IIFE remain in the AST
        | CallExpression(ArrowFunctionExpression(_,(BlockStatement body),_,_,_),_,_) ->
            body |> Array.exists isProductiveStatement
        | _ -> true

    and isProductiveStatement(s: Statement) =
        match s with
        | ExpressionStatement(expr) -> hasSideEffects(expr)
        | _ -> true

    let (|NullOrUndefined|_|) = function
        | Literal(NullLiteral _)
        | Undefined _ -> Some ()
        | UnaryExpression(argument, operator, _loc)
            when operator = "void" && not(hasSideEffects(argument)) -> Some()
        | _-> None

    let (|StringConstant|_|) = function
        | Literal(Literal.StringLiteral(StringLiteral(value=value))) -> Some value
        | _ -> None

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

        member printer.PrintProductiveStatement(s: Statement, ?printSeparator) =
            if isProductiveStatement(s) then
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
            printer.PrintOptional(node |> Option.map Node.TypeParameterDeclaration, ?before=before)
        member printer.PrintOptional(node: TypeAnnotation option, ?before: string) =
            printer.PrintOptional(node |> Option.map Node.TypeAnnotation, ?before=before)
        member printer.PrintOptional(node: Identifier option, ?before: string) =
            printer.PrintOptional(node |> Option.map Expression.Identifier, ?before=before)
        member printer.PrintOptional(node: Literal option, ?before: string) =
            printer.PrintOptional(node |> Option.map Literal, ?before=before)
        member printer.PrintOptional(node: StringLiteral option, ?before: string) =
            printer.PrintOptional(node |> Option.map Literal.StringLiteral, ?before=before)
        member printer.PrintOptional(node: TypeParameterInstantiation option, ?before: string) =
            printer.PrintOptional(node |> Option.map Node.TypeParameterInstantiation, ?before=before)
        member printer.PrintOptional(node: Statement option, ?before: string) =
            printer.PrintOptional(node |> Option.map Statement, ?before=before)
        member printer.PrintOptional(node: Declaration option, ?before: string) =
            printer.PrintOptional(node |> Option.map Declaration, ?before=before)
        member printer.PrintOptional(node: VariableDeclaration option, ?before: string) =
            printer.PrintOptional(node |> Option.map Declaration.VariableDeclaration, ?before=before)
        member printer.PrintOptional(node: CatchClause option, ?before: string) =
            printer.PrintOptional(node |> Option.map Node.CatchClause, ?before=before)
        member printer.PrintOptional(node: BlockStatement option, ?before: string) =
            printer.PrintOptional(node |> Option.map Statement.BlockStatement, ?before=before)

        member printer.PrintArray(nodes: 'a array, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes.[i]
                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedArray(nodes: Node array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedArray(nodes: Pattern array) =
            let len = nodes.Length
            let mutable i = 0
            let mutable foundNamed = false

            let printPattern (p: Printer) (x: Pattern) =
                if x.IsNamed && not foundNamed then
                    p.Print("{ ")
                    foundNamed <- true
                p.Print(x)
                i <- i + 1
                if i = len && foundNamed then
                    p.Print(" }")

            printer.PrintArray(nodes, printPattern, fun p -> p.Print(", "))

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
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))


        // TODO: (super) type parameters, implements
        member printer.PrintClass(id: Identifier option, superClass: Expression option,
                superTypeParameters: TypeParameterInstantiation option,
                typeParameters: TypeParameterDeclaration option,
                implements: ClassImplements array option, body: ClassBody, loc) =
            printer.Print("class", ?loc=loc)
            printer.PrintOptional(id, " ")
            printer.PrintOptional(typeParameters)
            match superClass with
            | Some (Expression.Identifier(Identifier(typeAnnotation=Some(typeAnnotation)))) ->
                printer.Print(" extends ")
                printer.Print(typeAnnotation)
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

            let (|ImmediatelyApplied|_|) = function
                | CallExpression(callee, appliedArgs, _) when parameters.Length = appliedArgs.Length ->
                    // To be sure we're not running side effects when deleting the function check the callee is an identifier
                    match callee with
                    | Expression.Identifier(_) ->
                        Array.zip parameters appliedArgs
                        |> Array.forall (function
                            | Pattern.Identifier(Identifier(name=name1)), Expression.Identifier(Identifier(name=name2)) -> name1 = name2
                            | _ -> false)
                        |> function true -> Some callee | false -> None
                    | _ -> None
                | _ -> None

            let isDeclaration = defaultArg isDeclaration false
            let isArrow = defaultArg isArrow false
            printer.AddLocation(loc)

            match body.Body with
            // Check if we can remove the function
            | [| ReturnStatement(ImmediatelyApplied e, _) |] when not isDeclaration ->
                printer.Print(e)
            | _ when isArrow ->
                // Remove parens if we only have one argument? (and no annotation)
                printer.PrintOptional(typeParameters)
                printer.Print("(")
                printer.PrintCommaSeparatedArray(parameters)
                printer.Print(")")
                printer.PrintOptional(returnType)
                printer.Print(" => ")
                match body.Body with
                | [| ReturnStatement(argument, _loc) |] ->
                    match argument with
                    | ObjectExpression(_) -> printer.WithParens(argument)
                    | MemberExpression(name, object, property, computed, loc) ->
                        match object with
                        | ObjectExpression(_) -> printer.PrintMemberExpression(name, object, property, computed, loc, objectWithParens=true)
                        | _ -> printer.Print(argument)
                    | _ -> printer.ComplexExpressionWithParens(argument)
                | _ -> printer.PrintBlock(body.Body, skipNewLineAtEnd=true)
            | _ ->
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
            | ObjectExpression(_)
            | JsxTemplate(_)
            | JsxElement(_) -> printer.Print(expr)
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
            | Node.ClassBody(n) -> printer.Print(n)
            | Expression(n) -> printer.Print(n)
            | Node.SwitchCase(n) -> printer.Print(n)
            | Node.CatchClause(n) -> printer.Print(n)
            | ObjectMember(n) -> printer.Print(n)
            | Node.TypeParameter(n) -> printer.Print(n)
            | Node.TypeAnnotation(n) -> printer.Print(n)
            | Node.ExportSpecifier(n) -> printer.Print(n)
            | Node.InterfaceExtends(n) -> printer.Print(n)
            | ModuleDeclaration(n) -> printer.Print(n)
            | Node.FunctionTypeParam(n) -> printer.Print(n)
            | Node.ObjectTypeProperty(n) -> printer.Print(n)
            | Node.TypeAnnotationInfo(n) -> printer.Print(n)
            | Node.TypeParameterDeclaration(n) -> printer.Print(n)
            | Node.TypeParameterInstantiation(n) -> printer.Print(n)
            | Node.Program(_)
            | Directive(_)
            | ImportSpecifier(_)
            | Node.ObjectTypeIndexer(_)
            | Node.VariableDeclarator(_)
            | Node.ObjectTypeCallProperty(_)
            | Node.ObjectTypeInternalSlot(_) -> failwith "Not implemented"

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

        member printer.PrintJsxElement(componentOrTag: Expression, props: (string * Expression) list, children: Expression list) =
            let printTag = function
                | StringConstant tag -> printer.Print(tag)
                | componentRef -> printer.Print(componentRef)

            printer.Print("<")
            printTag componentOrTag

            if not(List.isEmpty props) then
                printer.PushIndentation()

                let mutable isFirst = true
                let printProp print =
                    if not isFirst then
                        printer.PrintNewLine()
                    else
                        isFirst <- false
                        printer.Print(" ")
                    print()

                props |> List.iter (function
                    | _, NullOrUndefined -> ()
                    | key, StringConstant value ->
                        printProp(fun () ->
                            printer.Print($"{key}=\"{value}\""))
                    | key, value ->
                        printProp(fun () ->
                            printer.Print(key + "={")
                            printer.Print(value)
                            printer.Print("}"))
                )
                printer.PopIndentation()

            printer.Print(">")

            if not(List.isEmpty children) then
                printer.PrintNewLine()
                printer.PushIndentation()
                children |> List.iter (function
                    | NullOrUndefined -> ()
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
            | JsxElement(componentOrTag, props, children) -> printer.PrintJsxElement(componentOrTag, props, children)
            | JsxTemplate(parts, values) -> printer.PrintJsxTemplate(parts, values)
            | Super(loc) ->  printer.Print("super", ?loc = loc)
            | Literal(n) -> printer.Print(n)
            | Undefined(loc) -> printer.Print("undefined", ?loc=loc)
            | Expression.Identifier(n) -> printer.Print(n)
            | NewExpression(callee, arguments, typeArguments, loc) -> printer.PrintNewExpression(callee, arguments, typeArguments, loc)
            | SpreadElement(argument, loc) ->
                printer.Print("...", ?loc = loc)
                printer.ComplexExpressionWithParens(argument)
            | ThisExpression(loc) -> printer.Print("this", ?loc = loc)
            | CallExpression(callee, arguments, loc) -> printer.PrintCallExpression(callee, arguments, loc)
            | EmitExpression(value, args, loc) -> printer.PrintEmitExpression(value, args, loc)
            | ArrayExpression(elements, loc) ->
                printer.Print("[", ?loc = loc)
                printer.PrintCommaSeparatedArray(elements)
                printer.Print("]")
            | ClassExpression(body, id, superClass, implements, superTypeParameters, typeParameters, loc) ->
                printer.PrintClass(id, superClass, superTypeParameters, typeParameters, implements, body, loc)
            | Expression.ClassImplements(n) -> printer.Print(n)
            | UnaryExpression(argument, operator, loc) -> printer.PrintUnaryExpression(argument, operator, loc)
            | UpdateExpression(prefix, argument, operator, loc) -> printer.PrintUpdateExpression(prefix, argument, operator, loc)
            | ObjectExpression(properties, loc) -> printer.PrintObjectExpression(properties, loc)
            | BinaryExpression(left, right, operator, loc) ->  printer.PrintOperation(left, operator, right, loc)
            | MemberExpression(name, object, property, computed, loc) -> printer.PrintMemberExpression(name, object, property, computed, loc)
            | LogicalExpression(left, operator, right, loc) -> printer.PrintOperation(left, operator, right, loc)
            | SequenceExpression(expressions, loc) ->
                // A comma-separated sequence of expressions.
                printer.AddLocation(loc)
                // TODO: Remove parens if we end up with only one expression
                // (when the ones before last don't have side effects)
                printer.Print("(")
                let last = expressions.Length - 1
                for i = 0 to last do
                    let e = expressions.[i]
                    if i = last then
                        printer.Print(e)
                    elif hasSideEffects(e) then
                        printer.Print(e)
                        printer.Print(", ")
                printer.Print(")")
            | FunctionExpression(id, ``params``, body, typeParameters, returnType, loc) ->
                printer.PrintFunction(id, ``params``, body, returnType, typeParameters, loc)
            | AssignmentExpression(left, right, operator, loc) -> printer.PrintOperation(left, operator, right, loc)
            | ConditionalExpression(test, consequent, alternate, loc) -> printer.PrintConditionalExpression(test, consequent, alternate, loc)
            | ArrowFunctionExpression(``params``, body, returnType, typeParameters, loc) ->
                printer.PrintArrowFunctionExpression(``params``, body, returnType, typeParameters, loc)

        member printer.Print(pattern: Pattern) =
            match pattern with
            | Pattern.Identifier(p) -> printer.Print(p)
            // TODO: Should we try to destructure an array literal here?
            | RestElement(argument) ->
                printer.Print("...")
                printer.Print(argument)

        member printer.Print(literal: Literal) =
            match literal with
            | RegExp(pattern, flags, loc) -> printer.PrintRegExp(pattern, flags, loc)
            | NullLiteral(loc) -> printer.Print("null", ?loc=loc)
            | Literal.StringLiteral(l) -> printer.Print(l)
            | BooleanLiteral(value, loc) -> printer.Print((if value then "true" else "false"), ?loc=loc)
            | NumericLiteral(value, loc) -> printer.PrintNumeric(value, loc)
            | Literal.DirectiveLiteral(l) -> failwith "Not implement: Directive literal"
            | StringTemplate(tag, parts, values, loc) ->
                let escape str = Regex.Replace(str, @"(?<!\\)\\", @"\\").Replace("`", @"\`")
                printer.AddLocation(loc)
                printer.PrintOptional(tag)
                printer.Print("`")
                for i = 0 to parts.Length - 2 do
                    printer.Print(escape parts.[i])
                    printer.Print("${")
                    printer.Print(values.[i])
                    printer.Print("}")
                printer.Print(Array.last parts |> escape)
                printer.Print("`")

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
                // If a JSX template starts with a new line, surround it in parens to avoid
                // having only return in single line (this causes JS to ignore the rest of the code)
                match argument with
                | JsxTemplate(parts,_) when Regex.IsMatch(parts[0], @"^\s*\n") ->
                    printer.WithParens(argument)
                | _ ->
                    printer.Print(argument)
            | SwitchStatement(discriminant, cases, loc) -> printer.PrintSwitchStatement(discriminant, cases, loc)
            | LabeledStatement(body, label) -> printer.PrintLabeledStatement(body, label)
            | DebuggerStatement(loc) -> printer.Print("debugger", ?loc = loc)
            | ContinueStatement(label, loc) ->
                printer.Print("continue", ?loc=loc)
                printer.PrintOptional(label, " ")

            | ExpressionStatement(expr) ->
                match expr with
                | UnaryExpression(argument, operator, _loc) when operator = "void" -> printer.Print(argument)
                | _ -> printer.Print(expr)

        member printer.Print(decl: Declaration) =
            match decl with
            | ClassDeclaration(body, id, superClass, implements, superTypeParameters, typeParameters, loc) ->
                printer.PrintClass(id, superClass, superTypeParameters, typeParameters, implements, body, loc)
            | Declaration.VariableDeclaration(d) -> printer.Print(d)
            | FunctionDeclaration(``params``, body, id, returnType, typeParameters, loc) ->
                printer.PrintFunction(Some id, ``params``, body, typeParameters, returnType, loc, isDeclaration=true)
                printer.PrintNewLine()
            | InterfaceDeclaration(id, body, extends, implements, typeParameters) ->
                printer.PrintInterfaceDeclaration(id, body, extends, implements, typeParameters)

        member printer.Print(md: ModuleDeclaration) =
            match md with
            | ImportDeclaration(specifiers, source) -> printer.PrintImportDeclaration(specifiers, source)
            | ExportNamedReferences(specifiers, source) ->
                printer.Print("export ")
                printer.Print("{ ")
                printer.PrintCommaSeparatedArray(specifiers)
                printer.Print(" }")
                printer.PrintOptional(source, " from ")
            | ExportNamedDeclaration(declaration) ->
                printer.Print("export ")
                printer.Print(declaration)
            | ExportAllDeclaration(source, loc) ->
                printer.Print("export * from ", ?loc=loc)
                printer.Print(source)
            | PrivateModuleDeclaration(statement) ->
                if isProductiveStatement(statement) then
                    printer.Print(statement)
            | ExportDefaultDeclaration(declaration) ->
                printer.Print("export default ")
                match declaration with
                | Choice1Of2 x -> printer.Print(x)
                | Choice2Of2 x -> printer.Print(x)

        member printer.PrintEmitExpression(value, args, loc) =
            printer.AddLocation(loc)

            let inline replace pattern (f: Match -> string) input =
                Regex.Replace(input, pattern, f)

            let printSegment (printer: Printer) (value: string) segmentStart segmentEnd =
                let segmentLength = segmentEnd - segmentStart
                if segmentLength > 0 then
                    let segment = value.Substring(segmentStart, segmentLength)

                    let subSegments = Regex.Split(segment, @"\r?\n")
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

                |> replace @"\{\{\s*\$(\d+)\s*\?\s*(.*?)\s*:\s*(.*?)\s*\}\}" (fun m ->
                    let i = int m.Groups.[1].Value
                    match Array.tryItem i args with
                    | Some expr ->
                        match expr with
                        | Literal(BooleanLiteral(value=false))
                        | NullOrUndefined -> m.Groups.[3].Value
                        | _ -> m.Groups.[2].Value
                    | None -> m.Groups.[3].Value
                )

                |> replace @"\{\{([^\}]*\$(\d+).*?)\}\}" (fun m ->
                    let i = int m.Groups.[2].Value
                    match Array.tryItem i args with
                    | Some _ -> m.Groups.[1].Value
                    | None -> "")

                // If placeholder is followed by !, emit string literals as JS: "let $0! = $1"
                |> replace @"\$(\d+)!" (fun m ->
                    let i = int m.Groups.[1].Value
                    match Array.tryItem i args with
                    | Some(StringConstant value) -> value
                    | _ -> "")

            let matches = Regex.Matches(value, @"\$\d+")
            if matches.Count > 0 then
                for i = 0 to matches.Count - 1 do
                    let m = matches.[i]
                    let isSurroundedWithParens =
                        m.Index > 0
                        && m.Index + m.Length < value.Length
                        && value.[m.Index - 1] = '('
                        && value.[m.Index + m.Length] = ')'

                    let segmentStart =
                        if i > 0 then matches.[i-1].Index + matches.[i-1].Length
                        else 0

                    printSegment printer value segmentStart m.Index

                    let argIndex = int m.Value.[1..]
                    match Array.tryItem argIndex args with
                    | Some e when isSurroundedWithParens -> printer.Print(e)
                    | Some e -> printer.ComplexExpressionWithParens(e)
                    | None -> printer.Print("undefined")

                let lastMatch = matches.[matches.Count - 1]
                printSegment printer value (lastMatch.Index + lastMatch.Length) value.Length
            else
                printSegment printer value 0 value.Length

        member printer.Print(identifier: Identifier) =
            let (Identifier(name, optional, _named, typeAnnotation, loc)) = identifier
            printer.Print(name, ?loc=loc)
            if optional then
                printer.Print("?")
            printer.PrintOptional(typeAnnotation)

        member printer.PrintRegExp(pattern, flags, loc) =
            printer.Print("/", ?loc=loc)
            // Note we cannot use Naming.escapeString because it will corrupt the regex pattern
            printer.Print(Regex.Replace(pattern, @"(?<!\\)\/", @"\/").Replace("\r", @"\r").Replace("\n", @"\n"))
            printer.Print("/")
            printer.Print(flags)

        member printer.Print(node: StringLiteral) =
            let (StringLiteral(value, loc)) = node
            printer.Print("\"", ?loc=loc)
            printer.Print(Naming.escapeString (fun _ -> false) value)
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
            let (CatchClause(param, body, loc)) = node
            // "catch" is being printed by TryStatement
            printer.Print("(", ?loc = loc)
            printer.Print(param)
            printer.Print(") ")
            printer.Print(body)

        member printer.PrintTryStatement(block, handler, finalizer, loc) =
            printer.Print("try ", ?loc = loc)
            printer.Print(block)
            printer.PrintOptional(handler, "catch ")
            printer.PrintOptional(finalizer, "finally ")

// Declarations

        member printer.Print(node: VariableDeclaration) =
            let (VariableDeclaration(declarations, kind, loc)) = node
            printer.Print(kind + " ", ?loc = loc)
            let canConflict = declarations.Length > 1

            for i = 0 to declarations.Length - 1 do
                let (VariableDeclarator(id, init)) = declarations.[i]
                printer.Print(id)

                match init with
                | None -> ()
                | Some e ->
                    printer.Print(" = ")
                    if canConflict then printer.ComplexExpressionWithParens(e)
                    else printer.Print(e)
                if i < declarations.Length - 1 then
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

        member printer.Print(node: ObjectMember) =
            match node with
            | ObjectProperty(key, value, computed) -> printer.PrintObjectProperty(key, value, computed)
            | ObjectMethod(kind, key, ``params``, body, computed, returnType, typeParameters, loc)  ->
                printer.PrintObjectMethod(kind, key, ``params``, body, computed, returnType, typeParameters, loc)

        member printer.PrintObjectProperty(key, value, computed) =
            if computed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)
            printer.Print(": ")
            printer.Print(value)

        member printer.PrintObjectMethod(kind, key, ``params``, body, computed, returnType, typeParameters, loc) =
            printer.AddLocation(loc)

            if kind <> "method" then
                printer.Print(kind + " ")

            if computed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
               printer.Print(key)

            printer.PrintOptional(typeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(")")
            printer.PrintOptional(returnType)
            printer.Print(" ")

            printer.PrintBlock(body.Body, skipNewLineAtEnd=true)

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

        member printer.PrintConditionalExpression(test, consequent, alternate, loc) =
            printer.AddLocation(loc)
            match test, consequent, alternate with
            // TODO: Move node optimization to Fable2Babel as with IfStatement?
            | Literal(BooleanLiteral(value=value)), _, _ ->
                if value then printer.Print(consequent)
                else printer.Print(alternate)
            | test, Literal(BooleanLiteral(true,_)), Literal(BooleanLiteral(false,_)) ->
                printer.Print(test)
            | test, Literal(BooleanLiteral(false,_)), Literal(BooleanLiteral(true,_)) ->
                printer.PrintUnaryExpression(test, "!", loc)
            | test, _, Literal(BooleanLiteral(false,_)) ->
                printer.PrintOperation(test, "&&", consequent, loc)
            | _ ->
                printer.ComplexExpressionWithParens(test)
                printer.Print(" ? ")
                printer.ComplexExpressionWithParens(consequent)
                printer.Print(" : ")
                printer.ComplexExpressionWithParens(alternate)

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

        member printer.PrintUnaryExpression(argument, operator, loc) =
            printer.AddLocation(loc)
            match operator with
            | "-" | "+" | "!" | "~" -> printer.Print(operator)
            | _ -> printer.Print(operator + " ")
            printer.ComplexExpressionWithParens(argument)

        member printer.PrintUpdateExpression(prefix, argument, operator, loc) =
            printer.AddLocation(loc)
            if prefix then
                printer.Print(operator)
                printer.ComplexExpressionWithParens(argument)
            else
                printer.ComplexExpressionWithParens(argument)
                printer.Print(operator)

// Binary Operations

        member printer.Print(node: ClassMember) =
            match node with
            | ClassMethod(kind, key, ``params``, body, computed, ``static``, ``abstract``, returnType, typeParameters, loc) ->
                printer.PrintClassMethod(kind, key, ``params``, body, computed, ``static``, ``abstract``, returnType, typeParameters, loc)
            | ClassProperty(key, value, computed, ``static``, optional, typeAnnotation, loc) -> printer.PrintClassProperty(key, value, computed, ``static``, optional, typeAnnotation, loc)

        member printer.PrintClassMethod(kind, key, ``params``, body, computed, ``static``, ``abstract``, returnType, typeParameters, loc) =
            printer.AddLocation(loc)

            let keywords = [
                if ``static`` = Some true then yield "static"
                if ``abstract`` = Some true then yield "abstract"
                if kind = "get" || kind = "set" then yield kind
            ]

            if not (List.isEmpty keywords) then
                printer.Print((String.concat " " keywords) + " ")

            if computed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)

            printer.PrintOptional(typeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(")")
            printer.PrintOptional(returnType)
            printer.Print(" ")

            printer.Print(body)

        member printer.PrintClassProperty(key, value, computed, ``static``, optional, typeAnnotation, loc) =
            printer.AddLocation(loc)
            if ``static`` then
                printer.Print("static ")
            if computed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)
            if optional then
                printer.Print("?")
            printer.PrintOptional(typeAnnotation)
            printer.PrintOptional(value, ": ")

        member printer.Print(node: ClassImplements) =
            let (ClassImplements(id, typeParameters)) = node
            printer.Print(id)
            printer.PrintOptional(typeParameters)

        member printer.Print(node: ClassBody) =
            let (ClassBody(body, loc)) = node
            printer.AddLocation(loc)
            printer.PrintBlock(body, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))

        member printer.PrintImportMemberSpecific(local, imported) =
            // Don't print the braces, node will be done in the import declaration
            printer.Print(imported)
            if imported.Name <> local.Name then
                printer.Print(" as ")
                printer.Print(local)

        member printer.PrintImportNamespaceSpecifier(local) =
            printer.Print("* as ")
            printer.Print(local)

        member printer.PrintImportDeclaration(specifiers, source) =
            let members = specifiers |> Array.choose (function ImportMemberSpecifier(local, imported) -> Some (ImportMemberSpecifier(local, imported)) | _ -> None)
            let defaults = specifiers|> Array.choose (function ImportDefaultSpecifier(local) -> Some (ImportDefaultSpecifier(local)) | _ -> None)
            let namespaces = specifiers |> Array.choose (function ImportNamespaceSpecifier(local) -> Some (ImportNamespaceSpecifier(local)) | _ -> None)

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
            let (StringLiteral(value, _)) = source
            printer.Print(printer.MakeImportPath(value))
            printer.Print("\"")

        member printer.Print(node: ExportSpecifier) =
            let (ExportSpecifier (local, exported)) = node
            // Don't print the braces, node will be done in the export declaration
            printer.Print(local)
            if exported.Name <> local.Name then
                printer.Print(" as ")
                printer.Print(exported)

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
            | FunctionTypeAnnotation(``params``, returnType, typeParameters, rest) -> printer.PrintFunctionTypeAnnotation(``params``, returnType, typeParameters, rest)
            | NullableTypeAnnotation(typeAnnotation) -> printer.Print(typeAnnotation)
            | GenericTypeAnnotation(id, typeParameters) ->
                printer.Print(id)
                printer.PrintOptional(typeParameters)
            | TypeAnnotationInfo.ObjectTypeAnnotation(an) -> printer.Print(an)

        member printer.Print((TypeAnnotation info): TypeAnnotation) =
            printer.Print(": ")
            printer.Print(info)

        member printer.Print((TypeParameter(name=name)): TypeParameter) =
            printer.Print(name)
            // printer.PrintOptional(bound)
            // printer.PrintOptional(``default``)

        member printer.Print((TypeParameterDeclaration ``params``): TypeParameterDeclaration) =
            printer.Print("<")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(">")

        member printer.Print((TypeParameterInstantiation ``params``) : TypeParameterInstantiation) =
            printer.Print("<")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(">")

        member printer.Print(node: FunctionTypeParam) =
            let (FunctionTypeParam(name, typeAnnotation, optional)) = node
            printer.Print(name)
            if optional = Some true then
                printer.Print("?")
            printer.Print(": ")
            printer.Print(typeAnnotation)

        member printer.PrintFunctionTypeAnnotation(``params``, returnType, typeParameters, rest) =
            printer.PrintOptional(typeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(``params``)
            if Option.isSome rest then
                printer.Print("...")
                printer.Print(rest.Value)
            printer.Print(") => ")
            printer.Print(returnType)

        member printer.Print(node: ObjectTypeProperty) =
            let (ObjectTypeProperty(key, value, kind, computed, ``static``, optional, proto, method)) = node

            if ``static`` then
                printer.Print("static ")
            if Option.isSome kind then
                printer.Print(kind.Value + " ")
            if computed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)
            if optional then
                printer.Print("?")
            // TODO: proto, method
            printer.Print(": ")
            printer.Print(value)

        member printer.Print(node: ObjectTypeAnnotation) =
            let (ObjectTypeAnnotation(properties, indexers, callProperties, internalSlots, exact)) = node
            printer.Print("{")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintArray(properties, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(indexers, (fun p x -> p.Print(x |> Node.ObjectTypeIndexer)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(callProperties, (fun p x -> p.Print(x |> Node.ObjectTypeCallProperty)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(internalSlots, (fun p x -> p.Print(x |> Node.ObjectTypeInternalSlot)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintNewLine()
            printer.PopIndentation()
            printer.Print("}")
            printer.PrintNewLine()

        member printer.Print(node: InterfaceExtends) =
            let (InterfaceExtends(id, typeParameters)) = node
            printer.Print(id)
            printer.PrintOptional(typeParameters)

        member printer.PrintInterfaceDeclaration(id, body, extends, implements, typeParameters) =
            printer.Print("interface ")
            printer.Print(id)
            printer.PrintOptional(typeParameters)

            if not (Array.isEmpty extends) then
                printer.Print(" extends ")
                printer.PrintArray(extends, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

            if not (Array.isEmpty implements) then
                printer.Print(" implements ")
                printer.PrintArray(implements, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

            printer.Print(" ")
            printer.Print(body)

open PrinterExtensions

let run writer (program: Program): Async<unit> =
    let printDeclWithExtraLine extraLine (printer: Printer) (decl: ModuleDeclaration) =
        printer.Print(decl)

        if printer.Column > 0 then
            printer.Print(";")
            printer.PrintNewLine()
        if extraLine then
            printer.PrintNewLine()

    async {
        use printer = new PrinterImpl(writer)

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
