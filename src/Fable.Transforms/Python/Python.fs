namespace rec Fable.AST.Python

open Fable.AST
open PrinterExtensions

type Printer =
    abstract Line: int
    abstract Column: int
    abstract PushIndentation: unit -> unit
    abstract PopIndentation: unit -> unit
    abstract Print: string * ?loc:SourceLocation -> unit
    abstract PrintNewLine: unit -> unit
    abstract AddLocation: SourceLocation option -> unit
    abstract MakeImportPath: string -> string

module PrinterExtensions =
    type Printer with
        member printer.Print(node: AST) = node.Print(printer)

        member printer.PrintBlock
            (
                nodes: 'a list,
                printNode: Printer -> 'a -> unit,
                printSeparator: Printer -> unit,
                ?skipNewLineAtEnd
            ) =
            let skipNewLineAtEnd = defaultArg skipNewLineAtEnd false
            printer.Print("")
            printer.PrintNewLine()
            printer.PushIndentation()

            for node in nodes do
                printNode printer node
                printSeparator printer

            printer.PopIndentation()
            printer.Print("")

            if not skipNewLineAtEnd then
                printer.PrintNewLine()

        member printer.PrintStatementSeparator() =
            if printer.Column > 0 then
                printer.Print("")
                printer.PrintNewLine()

        member _.IsProductiveStatement(stmt: Statement) =
            let rec hasNoSideEffects(e: Expression) =
                printfn $"hasNoSideEffects: {e}"

                match e with
                | :? Constant -> true
                | :? Dict as d -> d.Keys.IsEmpty
                | _ -> false

            match stmt with
            | :? Expr as expr -> hasNoSideEffects expr.Value |> not
            | _ -> true

        member printer.PrintStatement(stmt: Statement, ?printSeparator) =
            printfn "PrintStatement: %A" stmt

            stmt.Print(printer)
            printSeparator |> Option.iter (fun fn -> fn printer)

        member printer.PrintStatements(statements: Statement list) =

            for stmt in statements do
                printer.PrintStatement(stmt, (fun p -> p.PrintStatementSeparator()))

        member printer.PrintBlock(nodes: Statement list, ?skipNewLineAtEnd) =
            printer.PrintBlock(
                nodes,
                (fun p s -> p.PrintStatement(s)),
                (fun p -> p.PrintStatementSeparator()),
                ?skipNewLineAtEnd = skipNewLineAtEnd
            )

        member printer.PrintOptional(before: string, node: #AST option) =
            match node with
            | None -> ()
            | Some node ->
                printer.Print(before)
                node.Print(printer)

        member printer.PrintOptional(before: string, node: #AST option, after: string) =
            match node with
            | None -> ()
            | Some node ->
                printer.Print(before)
                node.Print(printer)
                printer.Print(after)

        member printer.PrintOptional(node: #AST option) =
            match node with
            | None -> ()
            | Some node -> node.Print(printer)

        member printer.PrintList(nodes: 'a list, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes.[i]

                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedList(nodes: AST list) =
            printer.PrintList(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedList(nodes: Expression list) =
            printer.PrintList(nodes, (fun p x -> p.SequenceExpressionWithParens(x)), (fun p -> p.Print(", ")))

        // member printer.PrintCommaSeparatedArray(nodes: #Node array) =
//     printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))


        member printer.PrintFunction
            (
                id: Identifier option,
                args: Arguments,
                body: Statement list,
                returnType,
                ?isDeclaration
            ) =
            printer.Print("def ")
            printer.PrintOptional(id)
            printer.Print("(")
            printer.PrintCommaSeparatedList(args.Args |> List.map (fun arg -> arg :> AST))
            printer.Print(")")
            printer.PrintOptional(returnType)
            printer.Print(":")
            printer.PrintBlock(body, skipNewLineAtEnd = true)

        //     let areEqualPassedAndAppliedArgs (passedArgs: Pattern[]) (appliedAgs: Expression[]) =
//         Array.zip passedArgs appliedAgs
//         |> Array.forall (function
//             | (:? Identifier as p), (:? Identifier as a) -> p.Name = a.Name
//             | _ -> false)

        //     let isDeclaration = defaultArg isDeclaration false
//     let isArrow = defaultArg isArrow false

        //     printer.AddLocation(loc)

        //     // Check if we can remove the function
//     let skipExpr =
//         match body.Body with
//         | [|:? ReturnStatement as r|] when not isDeclaration ->
//             match r.Argument with
//             | :? CallExpression as c when parameters.Length = c.Arguments.Length ->
//                 // To be sure we're not running side effects when deleting the function,
//                 // check the callee is an identifier (accept non-computed member expressions too?)
//                 match c.Callee with
//                 | :? Identifier when areEqualPassedAndAppliedArgs parameters c.Arguments ->
//                     Some c.Callee
//                 | _ -> None
//             | _ -> None
//         | _ -> None

        //     match skipExpr with
//     | Some e -> e.Print(printer)
//     | None ->
//         if false then //isArrow then
//             // Remove parens if we only have one argument? (and no annotation)
//             printer.PrintOptional(typeParameters)
//             printer.Print("lambda-inline ")
//             printer.PrintCommaSeparatedArray(parameters)
//             printer.PrintOptional(returnType)
//             printer.Print(": ")
//             match body.Body with
//             | [|:? ReturnStatement as r |] ->
//                 match r.Argument with
//                 | :? ObjectExpression as e -> printer.WithParens(e)
//                 | :? MemberExpression as e ->
//                     match e.Object with
//                     | :? ObjectExpression -> e.Print(printer, objectWithParens=true)
//                     | _ -> e.Print(printer)
//                 | _ -> printer.ComplexExpressionWithParens(r.Argument)
//             | _ -> printer.PrintBlock(body.Body, skipNewLineAtEnd=true)
//         else
//             printer.Print("def ")
//             printer.PrintOptional(id)
//             printer.PrintOptional(typeParameters)
//             printer.Print("(")
//             printer.PrintCommaSeparatedArray(parameters)
//             printer.Print(")")
//             printer.PrintOptional(returnType)
//             printer.Print(":")
//             printer.PrintBlock(body.Body, skipNewLineAtEnd=true)

        member printer.WithParens(expr: Expression) =
            printer.Print("(")
            expr.Print(printer)
            printer.Print(")")

        member printer.SequenceExpressionWithParens(expr: Expression) =
            match expr with
            //| :? SequenceExpression -> printer.WithParens(expr)
            | _ -> printer.Print(expr)

        /// Surround with parens anything that can potentially conflict with operator precedence
        member printer.ComplexExpressionWithParens(expr: Expression) =
            printfn "Expr: %A" expr

            match expr with
            // | :? Undefined
            // | :? NullLiteral
            | :? Constant -> expr.Print(printer)
            // | :? BooleanLiteral
            // | :? NumericLiteral
            | :? Name -> expr.Print(printer)
            // | :? MemberExpression
            // | :? CallExpression
            // | :? ThisExpression
            // | :? Super
            // | :? SpreadElement
            // | :? ArrayExpression
            // | :? ObjectExpression -> expr.Print(printer)
            | _ -> printer.WithParens(expr)

        member printer.PrintOperation(left, operator, right, ?loc) =
            printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(left)
            printer.Print(operator)
            printer.ComplexExpressionWithParens(right)

type AST =
    //int col
    abstract Print: Printer -> unit

[<AbstractClass>]
type Expression() =
    member val Lineno: int = 0 with get, set
    member val ColOffset: int = 0 with get, set
    member val EndLineno: int option = None with get, set
    member val EndColOffset: int option = None with get, set

    interface AST with
        member this.Print(printer) = this.Print printer

    abstract Print: Printer -> unit

    member x.AsExpr() = x


type Operator =
    inherit AST

type BoolOperator =
    inherit AST

type ComparisonOperator =
    inherit AST

type UnaryOperator =
    inherit AST

type ExpressionContext =
    inherit AST


type Identifier =
    | Identifier of string

    interface AST with
        member this.Print(printer: Printer) =
            let (Identifier id) = this
            printer.Print id

[<AbstractClass>]
type Statement() =
    member val Lineno: int = 0 with get, set
    member val ColOffset: int = 0 with get, set
    member val EndLineno: int option = None with get, set
    member val EndColOffset: int option = None with get, set

    interface AST with
        member this.Print(printer) = this.Print printer

    abstract Print: Printer -> unit

type Module(body) =
    member _.Body: List<Statement> = body

    interface AST with
        member this.Print(printer) = this.Print printer

    member _.Print(printer: Printer) = printer.PrintStatements(body)

/// Both parameters are raw strings of the names. asname can be None if the regular name is to be used.
///
/// ```py
/// >>> print(ast.dump(ast.parse('from ..foo.bar import a as b, c'), indent=4))
/// Module(
///     body=[
///         ImportFrom(
///             module='foo.bar',
///             names=[
///                 alias(name='a', asname='b'),
///                 alias(name='c')],
///             level=2)],
///     type_ignores=[])
/// ```
type Alias(name, asname) =
    member _.Name: Identifier = name
    member _.AsName: Identifier option = asname

    interface AST with
        member this.Print(printer) = this.Print printer

    member _.Print(printer: Printer) =
        printer.Print(name)

        match asname with
        | Some (Identifier alias) ->
            printer.Print(" as ")
            printer.Print(alias)
        | _ -> ()


/// A single argument in a list. arg is a raw string of the argument name, annotation is its annotation, such as a Str
/// or Name node.
///
/// - type_comment is an optional string with the type annotation as a comment
type Arg(arg, ?annotation, ?typeComment) =
    member val Lineno: int = 0 with get, set
    member val ColOffset: int = 0 with get, set
    member val EndLineno: int option = None with get, set
    member val EndColOffset: int option = None with get, set

    member _.Arg: Identifier = arg
    member _.Annotation: Expression option = annotation
    member _.TypeComment: string option = typeComment

    interface AST with
        member _.Print(printer) =
            let (Identifier name) = arg
            printer.Print(name)

type Keyword(arg, value) =
    member val Lineno: int = 0 with get, set
    member val ColOffset: int = 0 with get, set
    member val EndLineno: int option = None with get, set
    member val EndColOffset: int option = None with get, set

    member _.Arg: Identifier = arg
    member _.Value: Expression = value

    interface AST with
        member _.Print(printer) =
            let (Identifier name) = arg
            printer.Print(name)
            printer.Print(" = ")
            printer.Print(value)

/// The arguments for a function.
///
///  - posonlyargs, args and kwonlyargs are lists of arg nodes.
///  - vararg and kwarg are single arg nodes, referring to the *args, **kwargs parameters.
///  - kwDefaults is a list of default values for keyword-only arguments. If one is None, the corresponding argument is
///    required.
///  - defaults is a list of default values for arguments that can be passed positionally. If there are fewer defaults,
///    they correspond to the last n arguments.
type Arguments(?posonlyargs, ?args, ?vararg, ?kwonlyargs, ?kwDefaults, ?kwarg, ?defaults) =
    member _.PosOnlyArgs: Arg list = defaultArg posonlyargs []
    member _.Args: Arg list = defaultArg args []
    member _.VarArg: Arg option = vararg
    member _.KwOnlyArgs: Arg list = defaultArg kwonlyargs []
    member _.KwDefaults: Expression list = defaultArg kwDefaults []
    member _.KwArg: Arg option = kwarg
    member _.Defaults: Expression list = defaultArg defaults []

    interface AST with
        member _.Print(printer) = printer.Print("(Arguments)")

//#region Statements

/// An assignment. targets is a list of nodes, and value is a single node.
///
/// Multiple nodes in targets represents assigning the same value to each. Unpacking is represented by putting a Tuple
/// or List within targets.
///
/// type_comment is an optional string with the type annotation as a comment.
///
/// ```py
/// >>> print(ast.dump(ast.parse('a = b = 1'), indent=4)) # Multiple assignment
/// Module(
///     body=[
///         Assign(
///             targets=[
///                 Name(id='a', ctx=Store()),
///                 Name(id='b', ctx=Store())],
///             value=Constant(value=1))],
///     type_ignores=[])
///
/// >>> print(ast.dump(ast.parse('a,b = c'), indent=4)) # Unpacking
/// Module(
///     body=[
///         Assign(
///             targets=[
///                 Tuple(
///                     elts=[
///                         Name(id='a', ctx=Store()),
///                         Name(id='b', ctx=Store())],
///                     ctx=Store())],
///             value=Name(id='c', ctx=Load()))],
///     type_ignores=[])
/// ```
type Assign(targets, value, ?typeComment) =
    inherit Statement()

    member _.Targets: Expression list = targets
    member _.Value: Expression = value
    member _.TypeComment: string option = typeComment

    override _.Print(printer) =
        printfn "Assign: %A" (targets, value)
        //printer.PrintOperation(targets.[0], "=", value, None)

        for target in targets do
            printer.Print(target)
            printer.Print(" = ")

        printer.Print(value)

/// When an expression, such as a function call, appears as a statement by itself with its return value not used or
/// stored, it is wrapped in this container. value holds one of the other nodes in this section, a Constant, a Name, a
/// Lambda, a Yield or YieldFrom node.
///
/// ```py
/// >>> print(ast.dump(ast.parse('-a'), indent=4))
/// Module(
///     body=[
///         Expr(
///             value=UnaryOp(
///                 op=USub(),
///                 operand=Name(id='a', ctx=Load())))],
///     type_ignores=[])
///```
type Expr(value) =
    inherit Statement()

    member _.Value: Expression = value

    override _.Print(printer) = value.Print(printer)

/// A for loop. target holds the variable(s) the loop assigns to, as a single Name, Tuple or List node. iter holds the
/// item to be looped over, again as a single node. body and orelse contain lists of nodes to execute. Those in orelse
/// are executed if the loop finishes normally, rather than via a break statement.
///
/// type_comment is an optional string with the type annotation as a comment.
///
/// ```py
/// >>> print(ast.dump(ast.parse("""
/// ... for x in y:
/// ...     ...
/// ... else:
/// ...     ...
/// ... """), indent=4))
/// Module(
///     body=[
///         For(
///             target=Name(id='x', ctx=Store()),
///             iter=Name(id='y', ctx=Load()),
///             body=[
///                 Expr(
///                     value=Constant(value=Ellipsis))],
///             orelse=[
///                 Expr(
///                     value=Constant(value=Ellipsis))])],
///     type_ignores=[])
///```
type For(target, iter, body, orelse, ?typeComment) =
    inherit Statement()

    member _.Target: Expression = target
    member _.Iterator: Expression = iter
    member _.Body: Statement list = body
    member _.Else: Statement list = orelse
    member _.TypeComment: string option = typeComment

    override this.Print(printer) =
        printer.Print("for ")
        printer.Print(iter)
        printer.Print(":")
        printer.PrintNewLine()
        printer.PushIndentation()
        printer.PrintStatements(this.Body)
        printer.PopIndentation()

type AsyncFor(target, iter, body, orelse, ?typeComment) =
    inherit Statement()

    member _.Target: Expression = target
    member _.Iterator: Expression = iter
    member _.Body: Statement list = body
    member _.Else: Statement list = orelse
    member _.TypeComment: string option = typeComment

    override _.Print(printer) = printer.Print("(AsyncFor)")

/// A while loop. test holds the condition, such as a Compare node.
///
/// ```py
/// >> print(ast.dump(ast.parse("""
/// ... while x:
/// ...    ...
/// ... else:
/// ...    ...
/// ... """), indent=4))
/// Module(
///     body=[
///         While(
///             test=Name(id='x', ctx=Load()),
///             body=[
///                 Expr(
///                     value=Constant(value=Ellipsis))],
///             orelse=[
///                 Expr(
///                     value=Constant(value=Ellipsis))])],
///     type_ignores=[])
/// ```
type While(test, body, orelse) =
    inherit Statement()

    member _.Test: Expression = test
    member _.Body: Statement list = body
    member _.Else: Statement list = orelse

    override this.Print(printer) =
        printer.Print("while ")
        printer.Print(test)
        printer.Print(":")
        printer.PrintNewLine()
        printer.PushIndentation()
        printer.PrintStatements(this.Body)
        printer.PopIndentation()

/// A class definition.
///
/// - name is a raw string for the class name
/// - bases is a list of nodes for explicitly specified base classes.
/// - keywords is a list of keyword nodes, principally for ‘metaclass’. Other keywords will be passed to the metaclass,
///   as per PEP-3115.
/// - starargs and kwargs are each a single node, as in a function call. starargs will be expanded to join the list of
///   base classes, and kwargs will be passed to the metaclass.
/// - body is a list of nodes representing the code within the class definition.
/// - decorator_list is a list of nodes, as in FunctionDef.
///
/// ```py
/// >>> print(ast.dump(ast.parse("""\
/// ... @decorator1
/// ... @decorator2
/// ... class Foo(base1, base2, metaclass=meta):
/// ...     pass
/// ... """), indent=4))
/// Module(
///     body=[
///         ClassDef(
///             name='Foo',
///             bases=[
///                 Name(id='base1', ctx=Load()),
///                 Name(id='base2', ctx=Load())],
///             keywords=[
///                 keyword(
///                     arg='metaclass',
///                     value=Name(id='meta', ctx=Load()))],
///             body=[
///                 Pass()],
///             decorator_list=[
///                 Name(id='decorator1', ctx=Load()),
///                 Name(id='decorator2', ctx=Load())])],
///     type_ignores=[])
///```
type ClassDef(name, ?bases, ?keywords, ?body, ?decoratorList, ?loc) =
    inherit Statement()

    member _.Name: Identifier = name
    member _.Bases: Expression list = defaultArg bases []
    member _.Keyword: Keyword list = defaultArg keywords []
    member _.Body: Statement list = defaultArg body []
    member _.DecoratorList: Expression list = defaultArg decoratorList []

    override this.Print(printer) =
        let (Identifier name) = name
        printer.Print("class ", ?loc = loc)
        printer.Print(name)

        match this.Bases with
        | [] -> ()
        | xs ->
            printer.Print("(")
            printer.PrintCommaSeparatedList(this.Bases)
            printer.Print(")")

        printer.Print(":")
        printer.PrintNewLine()
        printer.PushIndentation()
        printer.PrintStatements(this.Body)
        printer.PopIndentation()

/// An if statement. test holds a single node, such as a Compare node. body and orelse each hold a list of nodes.
///
/// elif clauses don’t have a special representation in the AST, but rather appear as extra If nodes within the orelse
/// section of the previous one.
///
/// ```py
/// >>> print(ast.dump(ast.parse("""
/// ... if x:
/// ...    ...
/// ... elif y:
/// ...    ...
/// ... else:
/// ...    ...
/// ... """), indent=4))
/// Module(
///     body=[
///         If(
///             test=Name(id='x', ctx=Load()),
///             body=[
///                 Expr(
///                     value=Constant(value=Ellipsis))],
///             orelse=[
///                 If(
///                     test=Name(id='y', ctx=Load()),
///                     body=[
///                         Expr(
///                             value=Constant(value=Ellipsis))],
///                     orelse=[
///                         Expr(
///                             value=Constant(value=Ellipsis))])])],
///     type_ignores=[])
/// ```
type If(test, body, orelse) =
    inherit Statement()

    member _.Test: Expression = test
    member _.Body: Statement list = body
    member _.Else: Statement list = orelse

    override _.Print(printer) = printer.Print("(If)")

/// A raise statement. exc is the exception object to be raised, normally a Call or Name, or None for a standalone
/// raise. cause is the optional part for y in raise x from y.
///
/// ```py
/// >>> print(ast.dump(ast.parse('raise x from y'), indent=4))
/// Module(
///     body=[
///         Raise(
///             exc=Name(id='x', ctx=Load()),
///             cause=Name(id='y', ctx=Load()))],
///     type_ignores=[])
/// ```
type Raise(exc, ?cause) =
    inherit Statement()

    member _.Exception: Expression = exc
    member _.Cause: Expression option = cause

    override _.Print(printer) = printer.Print("(Raise)")

/// A function definition.
///
/// - name is a raw string of the function name.
/// - args is a arguments node.
/// - body is the list of nodes inside the function.
/// - decorator_list is the list of decorators to be applied, stored outermost first (i.e. the first in the list will be
///   applied last).
/// - returns is the return annotation.
/// - type_comment is an optional string with the type annotation as a comment.
type FunctionDef(name, args, body, ?decoratorList, ?returns, ?typeComment) =

    inherit Statement()

    member _.Name: Identifier = name
    member _.Args: Arguments = args
    member _.Body: Statement list = body
    member _.DecoratorList: Expression list = defaultArg decoratorList []
    member _.Returns: Expression option = returns
    member _.TypeComment: string option = typeComment

    override _.Print(printer) =
        printer.PrintFunction(Some name, args, body, returns, isDeclaration = true)
        printer.PrintNewLine()

/// global and nonlocal statements. names is a list of raw strings.
///
/// ```py
/// >>> print(ast.dump(ast.parse('global x,y,z'), indent=4))
/// Module(
///     body=[
///         Global(
///             names=[
///                 'x',
///                 'y',
///                 'z'])],
///     type_ignores=[])
///
/// ```
type Global(names) =
    inherit Statement()

    member _.Names: Identifier list = names

    override _.Print(printer) = printer.Print("(Global)")

/// global and nonlocal statements. names is a list of raw strings.
///
/// ```py
/// >>> print(ast.dump(ast.parse('nonlocal x,y,z'), indent=4))
/// Module(
///     body=[
///         Nonlocal(
///             names=[
///                 'x',
///                 'y',
///                 'z'])],
///     type_ignores=[])
/// `````
type NonLocal(names) =
    inherit Statement()

    member _.Names: Identifier list = names

    override _.Print(printer) = printer.Print("(NonLocal)")

/// A pass statement.
///
/// ```py
/// >>> print(ast.dump(ast.parse('pass'), indent=4))
/// Module(
///     body=[
///         Pass()],
///     type_ignores=[])
/// ```
type Pass() =
    inherit Statement()

    override _.Print(printer) = printer.Print("pass")

/// The break statement.
type Break() =
    inherit Statement()

    override _.Print(printer) = printer.Print("break")

/// The continue statement.
type Continue() =
    inherit Statement()

    override _.Print(printer) = printer.Print("continue")

/// An async function definition.
///
/// - name is a raw string of the function name.
/// - args is a arguments node.
/// - body is the list of nodes inside the function.
/// - decorator_list is the list of decorators to be applied, stored outermost first (i.e. the first in the list will be
///   applied last).
/// - returns is the return annotation.
/// - type_comment is an optional string with the type annotation as a comment.
type AsyncFunctionDef(name, args, body, decoratorList, ?returns, ?typeComment) =

    inherit Statement()

    member _.Name: Identifier = name
    member _.Args: Arguments = args
    member _.Body: Statement list = body
    member _.DecoratorList: Expression list = decoratorList
    member _.Returns: Expression option = returns
    member _.TypeComment: string option = typeComment

    override _.Print(printer) = printer.Print("(AsyncFunctionDef)")

/// An import statement. names is a list of alias nodes.
///
/// ```py
/// >>> print(ast.dump(ast.parse('import x,y,z'), indent=4))
/// Module(
///     body=[
///         Import(
///             names=[
///                 alias(name='x'),
///                 alias(name='y'),
///                 alias(name='z')])],
///     type_ignores=[])
/// `````
type Import(names) =
    inherit Statement()

    member _.Names: Alias list = names

    override _.Print(printer) = printer.Print("(Import)")

/// Represents from x import y. module is a raw string of the ‘from’ name, without any leading dots, or None for
/// statements such as from . import foo. level is an integer holding the level of the relative import (0 means absolute
/// import).
///
/// ```py
/// >>> print(ast.dump(ast.parse('from y import x,y,z'), indent=4))
/// Module(
///     body=[
///         ImportFrom(
///             module='y',
///             names=[
///                 alias(name='x'),
///                 alias(name='y'),
///                 alias(name='z')],
///             level=0)],
///     type_ignores=[])
/// ```
type ImportFrom(``module``, names, ?level) =
    inherit Statement()

    member _.Module: Identifier option = ``module``
    member _.Names: Alias list = names
    member _.Level: int option = level

    override this.Print(printer) =
        let (Identifier path) =
            this.Module
            |> Option.defaultValue (Identifier ".")

        printer.Print("from ")

        printer.Print(printer.MakeImportPath(path))

        printer.Print(" import ")

        if not (List.isEmpty names) then
            if List.length names > 1 then
                printer.Print("(")
            printer.PrintCommaSeparatedList(names |> List.map (fun x -> x :> AST))
            if List.length names > 1 then
                printer.Print(")")

/// A return statement.
///
/// ```py
/// >>> print(ast.dump(ast.parse('return 4'), indent=4))
/// Module(
///     body=[
///         Return(
///             value=Constant(value=4))],
///     type_ignores=[])
/// ```
type Return(?value) =
    inherit Statement()
    member _.Value: Expression option = value

    override this.Print(printer) =
        printer.Print("return ")
        printer.PrintOptional(this.Value)

//#endregion

//#region Expressions

/// Attribute access, e.g. d.keys. value is a node, typically a Name. attr is a bare string giving the name of the
/// attribute, and ctx is Load, Store or Del according to how the attribute is acted on.
///
/// ```py
/// >>> print(ast.dump(ast.parse('snake.colour', mode='eval'), indent=4))
/// Expression(
///     body=Attribute(
///         value=Name(id='snake', ctx=Load()),
///         attr='colour',
///         ctx=Load()))
/// ```
type Attribute(value, attr, ctx) =
    inherit Expression()

    member _.Value: Expression = value
    member _.Attr: Identifier = attr
    member _.Ctx: ExpressionContext = ctx

    override this.Print(printer) =
        printer.Print(this.Value)
        printer.Print(".")
        printer.Print(this.Attr)

/// A subscript, such as l[1]. value is the subscripted object (usually sequence or mapping). slice is an index, slice
/// or key. It can be a Tuple and contain a Slice. ctx is Load, Store or Del according to the action performed with the
/// subscript.
///
/// ```py
/// >>> print(ast.dump(ast.parse('l[1:2, 3]', mode='eval'), indent=4))
/// Expression(
///     body=Subscript(
///         value=Name(id='l', ctx=Load()),
///         slice=Tuple(
///             elts=[
///                 Slice(
///                     lower=Constant(value=1),
///                     upper=Constant(value=2)),
///                 Constant(value=3)],
///             ctx=Load()),
///         ctx=Load()))
/// ```
type Subscript(value, slice, ctx) =
    inherit Expression()

    member _.Value: Expression = value
    member _.Slice: Expression = slice
    member _.Ctx: ExpressionContext = ctx

    override this.Print(printer) =
        printer.Print(this.Value)
        printer.Print("[")
        printer.Print(this.Slice)
        printer.Print("]")

type BinOp(left, op, right) =
    inherit Expression()

    member _.Left: Expression = left
    member _.Right: Expression = right
    member _.Operator: Operator = op

    override this.Print(printer) = printer.PrintOperation(left, op, right)

type Compare(left, ops, comparators) =
    inherit Expression()

    member _.Left: Expression = left
    member _.Comparators: Expression list = comparators
    member _.Ops: ComparisonOperator list = ops

    override this.Print(printer) =
        //printer.AddLocation(loc)
        printer.ComplexExpressionWithParens(left)

        for op, comparator in List.zip this.Ops this.Comparators do
            printer.Print(op)
            printer.ComplexExpressionWithParens(comparator)

/// A unary operation. op is the operator, and operand any expression node.
type UnaryOp(op, operand, ?loc) =
    inherit Expression()

    member _.Op: UnaryOperator = op
    member _.Operand: Expression = operand

    override this.Print(printer) =
        printer.AddLocation(loc)

        match op with
        | :? USub
        | :? UAdd
        | :? Not
        | :? Invert -> printer.Print(op)
        | _ ->
            printer.Print(op)
            printer.Print(" ")

        printer.ComplexExpressionWithParens(operand)

/// A constant value. The value attribute of the Constant literal contains the Python object it represents. The values
/// represented can be simple types such as a number, string or None, but also immutable container types (tuples and
/// frozensets) if all of their elements are constant.
///
/// ```py
/// >>> print(ast.dump(ast.parse('123', mode='eval'), indent=4))
/// Expression(
///     body=Constant(value=123))
/// `````
type Constant(value: obj) =
    inherit Expression()

    member _.Value: obj = value

    override _.Print(printer) =
        match box value with
        | :? string as str ->
            printer.Print("\"")
            printer.Print(string value)
            printer.Print("\"")
        | _ -> printer.Print(string value)

/// Node representing a single formatting field in an f-string. If the string contains a single formatting field and
/// nothing else the node can be isolated otherwise it appears in JoinedStr.
///
/// - value is any expression node (such as a literal, a variable, or a function call).
/// - conversion is an integer:
///   - -1: no formatting
///   - 115: !s string formatting
///   -  114: !r repr formatting
///   -  97: !a ascii formatting
/// - format_spec is a JoinedStr node representing the formatting of the value, or None if no format was specified. Both
/// conversion and format_spec can be set at the same time.
type FormattedValue(value, ?conversion, ?formatSpec) =
    inherit Expression()

    member _.Value: Expression = value
    member _.Conversion: int option = conversion
    member _.FormatSpec: Expression option = formatSpec

    override _.Print(printer) = printer.Print("(FormattedValue)")

/// A function call. func is the function, which will often be a Name or Attribute object. Of the arguments:
///
///     args holds a list of the arguments passed by position.
///
///     keywords holds a list of keyword objects representing arguments passed by keyword.
///
/// When creating a Call node, args and keywords are required, but they can be empty lists. starargs and kwargs are optional.
///
/// ```py
/// >>> print(ast.dump(ast.parse('func(a, b=c, *d, **e)', mode='eval'), indent=4))
/// Expression(
///     body=Call(
///         func=Name(id='func', ctx=Load()),
///         args=[
///             Name(id='a', ctx=Load()),
///             Starred(
///                 value=Name(id='d', ctx=Load()),
///                 ctx=Load())],
///         keywords=[
///             keyword(
///                 arg='b',
///                 value=Name(id='c', ctx=Load())),
///             keyword(
///                 value=Name(id='e', ctx=Load()))]))
/// ```
type Call(func, ?args, ?kw) =
    inherit Expression()

    member _.Func: Expression = func
    member _.Args: Expression list = defaultArg args []
    member _.Keywords: Keyword list = defaultArg kw []

    override this.Print(printer) =
        printer.Print(func)
        printer.Print("(")
        printer.PrintCommaSeparatedList(this.Args)
        printer.PrintCommaSeparatedList(this.Keywords |> List.map (fun x -> x :> AST))
        printer.Print(")")

type Emit(value, ?args) =
    inherit Expression()

    member _.Value: string = value
    member _.Args: Expression list = defaultArg args []

    override this.Print(printer) =
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
                for j = i to this.Args.Length - 1 do
                    rep.Add("$" + string j)
                String.concat ", " rep)

            |> replace @"\{\{\s*\$(\d+)\s*\?(.*?)\:(.*?)\}\}" (fun m ->
                let i = int m.Groups.[1].Value
                match this.Args.[i] with
                | :? Constant as b -> m.Groups.[2].Value
                | _ -> m.Groups.[3].Value)

            |> replace @"\{\{([^\}]*\$(\d+).*?)\}\}" (fun m ->
                let i = int m.Groups.[2].Value
                match List.tryItem i this.Args with
                | Some _ -> m.Groups.[1].Value
                | None -> "")

        let matches = System.Text.RegularExpressions.Regex.Matches(value, @"\$\d+")
        if matches.Count > 0 then
            for i = 0 to matches.Count - 1 do
                let m = matches.[i]

                let segmentStart =
                    if i > 0 then matches.[i-1].Index + matches.[i-1].Length
                    else 0

                printSegment printer value segmentStart m.Index

                let argIndex = int m.Value.[1..]
                match List.tryItem argIndex this.Args with
                | Some e -> printer.ComplexExpressionWithParens(e)
                | None -> printer.Print("None")

            let lastMatch = matches.[matches.Count - 1]
            printSegment printer value (lastMatch.Index + lastMatch.Length) value.Length
        else
            printSegment printer value 0 value.Length


/// lambda is a minimal function definition that can be used inside an expression. Unlike FunctionDef, body holds a
/// single node.
///
/// ```py
/// >>> print(ast.dump(ast.parse('lambda x,y: ...'), indent=4))
/// Module(
///     body=[
///         Expr(
///             value=Lambda(
///                 args=arguments(
///                     posonlyargs=[],
///                     args=[
///                         arg(arg='x'),
///                         arg(arg='y')],
///                     kwonlyargs=[],
///                     kw_defaults=[],
///                     defaults=[]),
///                 body=Constant(value=Ellipsis)))],
///     type_ignores=[])
/// ```
type Lambda(args, body) =

    inherit Expression()

    member _.Args: Arguments = args
    member _.Body: Statement list = body

    override _.Print(printer) =
        printer.Print("lambda")

        if (List.isEmpty >> not) args.Args then
            printer.Print(" ")

        printer.PrintCommaSeparatedList(args.Args |> List.map (fun arg -> arg :> AST))
        printer.Print(": ")

        for stmt in body do
            printer.Print(stmt)

/// A variable name. id holds the name as a string, and ctx is one of the following types.
type Name(id, ctx) =
    inherit Expression()

    member _.Id: Identifier = id
    member _.Context: ExpressionContext = ctx

    override _.Print(printer) =
        let (Identifier name) = id
        printer.Print(name)

/// A tuple. elts holds a list of nodes representing the elements. ctx is Store if the container is an assignment target
/// (i.e. (x,y)=something), and Load otherwise.
///
/// ```py
/// >>> print(ast.dump(ast.parse('(1, 2, 3)', mode='eval'), indent=4))
/// Expression(
///     body=Tuple(
///         elts=[
///             Constant(value=1),
///             Constant(value=2),
///             Constant(value=3)],
///         ctx=Load()))
///```
type Tuple(elts, ?loc) =
    inherit Expression()

    member _.Elements: Expression list = elts

    override _.Print(printer) =
        printer.Print("(", ?loc = loc)
        printer.PrintCommaSeparatedList(elts)

        if elts.Length = 1 then
            printer.Print(",")

        printer.Print(")")

/// A list or tuple. elts holds a list of nodes representing the elements. ctx is Store if the container is an
/// assignment target (i.e. (x,y)=something), and Load otherwise.
///
/// ```python
/// >>> print(ast.dump(ast.parse('[1, 2, 3]', mode='eval'), indent=4))
/// Expression(
///     body=List(
///         elts=[
///             Constant(value=1),
///             Constant(value=2),
///             Constant(value=3)],
///         ctx=Load()))
///```
type List(elts) =
    inherit Expression()

    member _.Elements: Expression list = elts

    override _.Print(printer) = printer.Print("(List)")

/// A set. elts holds a list of nodes representing the set’s elements.
///
/// ```py
/// >>> print(ast.dump(ast.parse('{1, 2, 3}', mode='eval'), indent=4))
/// Expression(
///     body=Set(
///         elts=[
///             Constant(value=1),
///             Constant(value=2),
///             Constant(value=3)]))
/// ```
type Set(elts) =
    inherit Expression()

    member _.Elements: Expression list = elts

    override _.Print(printer) = printer.Print("(Set)")

/// A dictionary. keys and values hold lists of nodes representing the keys and the values respectively, in matching
/// order (what would be returned when calling dictionary.keys() and dictionary.values()).
///
/// When doing dictionary unpacking using dictionary literals the expression to be expanded goes in the values list,
/// with a None at the corresponding position in keys.
///
/// ```py
/// >>> print(ast.dump(ast.parse('{"a":1, **d}', mode='eval'), indent=4))
/// Expression(
///     body=Dict(
///         keys=[
///             Constant(value='a'),
///             None],
///         values=[
///             Constant(value=1),
///             Name(id='d', ctx=Load())]))
/// ```
type Dict(keys, values) =
    inherit Expression()

    member _.Keys: Expression list = keys
    member _.Values: Expression list = values

    override _.Print(printer) =
        printer.Print("{")
        printer.PrintNewLine()
        printer.PushIndentation()

        let nodes = List.zip keys values |> List.mapi (fun i n -> (i, n))
        for i, (key, value) in nodes do
            printer.Print("\"")
            printer.Print(key)
            printer.Print("\"")
            printer.Print(": ")
            printer.Print(value)
            if i < nodes.Length - 1 then
                printer.Print(",")
                printer.PrintNewLine()

        printer.PrintNewLine()
        printer.PopIndentation()
        printer.Print("}")

/// A yield expression. Because these are expressions, they must be wrapped in a Expr node if the value sent back is not
/// used.
///
/// ```py
/// >>> print(ast.dump(ast.parse('yield from x'), indent=4))
/// Module(
///     body=[
///         Expr(
///             value=YieldFrom(
///                 value=Name(id='x', ctx=Load())))],
///     type_ignores=[])
///```
type Yield(?value) =
    inherit Expression()
    member _.Value: Expression option = value

    override _.Print(printer) = printer.Print("(Yield)")

/// A yield from expression. Because these are expressions, they must be wrapped in a Expr node if the value sent back
/// is not used.
///
/// ```py
/// >>> print(ast.dump(ast.parse('yield from x'), indent=4))
/// Module(
///     body=[
///         Expr(
///             value=YieldFrom(
///                 value=Name(id='x', ctx=Load())))],
///     type_ignores=[])
///```
type YieldFrom(?value) =
    inherit Expression()
    member _.Value: Expression option = value

    override _.Print(printer) = printer.Print("(YieldFrom)")

//#endregion

//#region Operators

type Add() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" + ")

type Sub() =
    static member Pattern = "-"

    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" - ")

type Mult() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" * ")

type Div() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" / ")

type FloorDiv() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" // ")

type Mod() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" % ")

type Pow() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" ** ")

type LShift() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" << ")

type RShift() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" >> ")

type BitOr() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" | ")

type BitXor() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print(" ^ ")

type BitAnd() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print($" & ")

type MatMult() =
    interface Operator with
        member _.Print(printer: Printer) = printer.Print($" @ ")

//#endregion

//#region Comparison operator tokens.

type Eq() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" == ")

type NotEq() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" != ")

type Lt() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" < ")

type LtE() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" <= ")

type Gt() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" > ")

type GtE() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" >= ")

type Is() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" is ")

type IsNot() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" is not ")

type In() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" in ")

type NotIn() =
    interface ComparisonOperator with
        member _.Print(printer: Printer) = printer.Print($" not in ")

//#endregion

//#region  Bool Operators

type And =
    inherit BoolOperator

type Or =
    inherit BoolOperator

//#region

//#region Unary Operators

type Invert() =
    interface UnaryOperator with
        member this.Print(printer) = printer.Print "~"

type Not() =
    interface UnaryOperator with
        member this.Print(printer) = printer.Print "not "

type UAdd() =
    interface UnaryOperator with
        member this.Print(printer) = printer.Print "+"

type USub() =
    interface UnaryOperator with
        member this.Print(printer) = printer.Print "-"

//#endregion

//#region  Expression Context

type Load() =
    interface ExpressionContext

    interface AST with
        member this.Print(printer) = ()

type Del()=
    interface ExpressionContext

    interface AST with
        member this.Print(printer) = ()

type Store() =
    interface ExpressionContext

    interface AST with
        member this.Print(printer) = ()

//#endregion
