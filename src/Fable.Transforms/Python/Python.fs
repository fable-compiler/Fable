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
    abstract EscapeJsStringLiteral: string -> string
    abstract MakeImportPath: string -> string

module PrinterExtensions =
    type Printer with
        member printer.Print(node: AST) = node.Print(printer)

        member printer.PrintBlock
            (
                nodes: 'a array,
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

// member _.IsProductiveStatement(s: Statement) =
//     let rec hasNoSideEffects (e: Expression) =
//         match e with
//         | :? Undefined
//         | :? NullLiteral
//         | :? StringLiteral
//         | :? BooleanLiteral
//         | :? NumericLiteral -> true
//         // Constructors of classes deriving from System.Object add an empty object at the end
//         | :? ObjectExpression as o -> o.Properties.Length = 0
//         | :? UnaryExpression as e when e.Operator = "void2" -> hasNoSideEffects e.Argument
//         // Some identifiers may be stranded as the result of imports
//         // intended only for side effects, see #2228
//         | :? Identifier -> true
//         | _ -> false

//     match s with
//     | :? ExpressionStatement as e -> hasNoSideEffects e.Expression |> not
//     | _ -> true

// member printer.PrintProductiveStatement(s: Statement, ?printSeparator) =
//     if printer.IsProductiveStatement(s) then
//         s.Print(printer)
//         printSeparator |> Option.iter (fun f -> f printer)

// member printer.PrintProductiveStatements(statements: Statement[]) =
//     for s in statements do
//         printer.PrintProductiveStatement(s, (fun p -> p.PrintStatementSeparator()))

// member printer.PrintBlock(nodes: Statement array, ?skipNewLineAtEnd) =
//     printer.PrintBlock(nodes,
//                        (fun p s -> p.PrintProductiveStatement(s)),
//                        (fun p -> p.PrintStatementSeparator()),
//                        ?skipNewLineAtEnd=skipNewLineAtEnd)

// member printer.PrintOptional(before: string, node: #Node option) =
//     match node with
//     | None -> ()
//     | Some node ->
//         printer.Print(before)
//         node.Print(printer)

// member printer.PrintOptional(before: string, node: #Node option, after: string) =
//     match node with
//     | None -> ()
//     | Some node ->
//         printer.Print(before)
//         node.Print(printer)
//         printer.Print(after)

// member printer.PrintOptional(node: #Node option) =
//     match node with
//     | None -> ()
//     | Some node -> node.Print(printer)

// member printer.PrintArray(nodes: 'a array, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
//     for i = 0 to nodes.Length - 1 do
//         printNode printer nodes.[i]
//         if i < nodes.Length - 1 then
//             printSeparator printer

// member printer.PrintCommaSeparatedArray(nodes: Expression array) =
//     printer.PrintArray(nodes, (fun p x -> p.SequenceExpressionWithParens(x)), (fun p -> p.Print(", ")))

// member printer.PrintCommaSeparatedArray(nodes: #Node array) =
//     printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

// // TODO: (super) type parameters, implements
// member printer.PrintClass(id: Identifier option, superClass: Expression option,
//         superTypeParameters: TypeParameterInstantiation option,
//         typeParameters: TypeParameterDeclaration option,
//         implements: ClassImplements array option, body: ClassBody, loc) =
//     printer.Print("class", ?loc=loc)
//     printer.PrintOptional(" ", id)
//     printer.PrintOptional(typeParameters)
//     match superClass with
//     | Some (:? Identifier as id) when id.TypeAnnotation.IsSome ->
//         printer.Print(" extends ");
//         printer.Print(id.TypeAnnotation.Value.TypeAnnotation)
//     | _ -> printer.PrintOptional("(", superClass, ")")
//     // printer.PrintOptional(superTypeParameters)
//     match implements with
//     | Some implements when not (Array.isEmpty implements) ->
//         printer.Print(" implements ")
//         printer.PrintArray(implements, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))
//     | _ -> ()
//     printer.Print(":")
//     printer.Print(body)

// member printer.PrintFunction(id: Identifier option, parameters: Pattern array, body: BlockStatement,
//         typeParameters: TypeParameterDeclaration option, returnType: TypeAnnotation option, loc, ?isDeclaration, ?isArrow) =
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

// member printer.WithParens(expr: Expression) =
//     printer.Print("(")
//     expr.Print(printer)
//     printer.Print(")")

// member printer.SequenceExpressionWithParens(expr: Expression) =
//     match expr with
//     | :? SequenceExpression -> printer.WithParens(expr)
//     | _ -> printer.Print(expr)

// /// Surround with parens anything that can potentially conflict with operator precedence
// member printer.ComplexExpressionWithParens(expr: Expression) =
//     match expr with
//     | :? Undefined
//     | :? NullLiteral
//     | :? StringLiteral
//     | :? BooleanLiteral
//     | :? NumericLiteral
//     | :? Identifier
//     | :? MemberExpression
//     | :? CallExpression
//     | :? ThisExpression
//     | :? Super
//     | :? SpreadElement
//     | :? ArrayExpression
//     | :? ObjectExpression -> expr.Print(printer)
//     | _ -> printer.WithParens(expr)

// member printer.PrintOperation(left, operator, right, loc) =
//     printer.AddLocation(loc)
//     printer.ComplexExpressionWithParens(left)
//     printer.Print(" " + operator + " ")
//     printer.ComplexExpressionWithParens(right)

type AST =
    //int col
    abstract Print: Printer -> unit

[<AbstractClass>]
type Expression () =
    member val Lineno : int = 0 with get, set
    member val ColOffset : int = 0  with get, set
    member val EndLineno : int option = None  with get, set
    member val EndColOffset : int option = None  with get, set

    interface AST with
        member this.Print(printer) = this.Print printer

    abstract Print: Printer -> unit

type Operator =
    inherit AST

type BoolOperator =
    inherit AST

type ComparisonOperator =
    inherit AST

type UnaryOperator =
    inherit AST

type Identifier =
    abstract Name: string

    inherit AST

[<AbstractClass>]
type Statement () =
    member val Lineno : int = 0 with get, set
    member val ColOffset : int = 0  with get, set
    member val EndLineno : int option = None  with get, set
    member val EndColOffset : int option = None  with get, set

    interface AST with
        member this.Print(printer) = this.Print printer

    abstract Print: Printer -> unit

type Module(body) =
    member _.Body: List<Statement> = body


type Alias(name, asname) =
    member _.Name: Identifier = name
    member _.AsName: Identifier option = asname

    interface AST with
        member _.Print(printer) = ()

/// A single argument in a list. arg is a raw string of the argument name, annotation is its annotation, such as a Str
/// or Name node.
///
/// - type_comment is an optional string with the type annotation as a comment
type Arg(arg, ?annotation, ?typeComment) =
    member val Lineno : int = 0 with get, set
    member val ColOffset : int = 0  with get, set
    member val EndLineno : int option = None  with get, set
    member val EndColOffset : int option = None  with get, set

    member _.Arg: Identifier = arg
    member _.Annotation : Expression option = annotation
    member _.TypeComment : string option = typeComment

/// The arguments for a function.
///
///  - posonlyargs, args and kwonlyargs are lists of arg nodes.
///  - vararg and kwarg are single arg nodes, referring to the *args, **kwargs parameters.
///  - kwDefaults is a list of default values for keyword-only arguments. If one is None, the corresponding argument is
///    required.
///  - defaults is a list of default values for arguments that can be passed positionally. If there are fewer defaults,
///    they correspond to the last n arguments.
type Arguments(?posonlyargs, ?args, ?vararg, ?kwonlyargs, ?kwDefaults, ?kwarg, ?defaults) =
    member _.PosOnlyArgs : Arg list = defaultArg posonlyargs []
    member _.Args : Arg list = defaultArg args []
    member _.VarArg : Arg option = vararg
    member _.KwOnlyArgs : Arg list = defaultArg kwonlyargs []
    member _.KwDefaults : Expression list = defaultArg kwDefaults []
    member _.KwArg : Arg option = kwarg
    member _.Defaults : Expression list = defaultArg defaults []

    interface AST with
        member _.Print(printer) = ()

//#region Statements

/// When an expression, such as a function call, appears as a statement by itself with its return value not used or
/// stored, it is wrapped in this container. value holds one of the other nodes in this section, a Constant, a Name, a
/// Lambda, a Yield or YieldFrom node.
/// ```python
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
    inherit Statement ()

    member _.Value: Expression = value

    override _.Print(printer) = ()

/// A function definition.
///
/// - name is a raw string of the function name.
/// - args is a arguments node.
/// - body is the list of nodes inside the function.
/// - decorator_list is the list of decorators to be applied, stored outermost first (i.e. the first in the list will be
///   applied last).
/// - returns is the return annotation.
/// - type_comment is an optional string with the type annotation as a comment.
type FunctionDef(name, args, body, decoratorList, ?returns, ?typeComment) =

    inherit Statement ()

    member _.Name: Identifier = name
    member _.Args: Arguments = args
    member _.Body: Statement list = body
    member _.DecoratorList: Expression list = decoratorList
    member _.Returns: Expression option = returns
    member _.TypeComment: string option = typeComment

    override _.Print(printer) = ()

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

    inherit Statement ()

    member _.Name: Identifier = name
    member _.Args: Arguments = args
    member _.Body: Statement list = body
    member _.DecoratorList: Expression list = decoratorList
    member _.Returns: Expression option = returns
    member _.TypeComment: string option = typeComment

    override _.Print(printer) = ()

/// An import statement. names is a list of alias nodes.
/// ```python
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
    inherit Statement ()

    member _.Names : Alias list = names

    override _.Print(printer) = ()

/// Represents from x import y. module is a raw string of the ‘from’ name, without any leading dots, or None for
/// statements such as from . import foo. level is an integer holding the level of the relative import (0 means absolute
/// import).
///```python
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
type ImportFrom(``module``, names, level) =
    inherit Statement ()

    member _.Module : Identifier option = ``module``
    member _.Names : Alias list = names
    member _.Level : int option = level

    override _.Print(printer) = ()

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
type Return (?value) =
    inherit Statement ()
    member _.Value : Expression option = value

    override _.Print(printer) = ()

//#endregion

//#region Expressions

/// A constant value. The value attribute of the Constant literal contains the Python object it represents. The values
/// represented can be simple types such as a number, string or None, but also immutable container types (tuples and
/// frozensets) if all of their elements are constant.
/// ```python
/// >>> print(ast.dump(ast.parse('123', mode='eval'), indent=4))
/// Expression(
///     body=Constant(value=123))
/// `````
type Constant<'T>(value: 'T) =
    inherit Expression ()

    member _.Value : 'T = value

    override _.Print(printer) = ()

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
    inherit Expression ()

    member _.Value : Expression = value
    member _.Conversion : int option = conversion
    member _.FormatSpec : Expression option = formatSpec

    override _.Print(printer) = ()

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

    inherit Expression ()

    member _.Args: Arguments = args
    member _.Body: Statement list = body

    override _.Print(printer) = ()


/// A tuple. elts holds a list of nodes representing the elements. ctx is Store if the container is an assignment target
/// (i.e. (x,y)=something), and Load otherwise.
///
/// ```python
/// >>> print(ast.dump(ast.parse('(1, 2, 3)', mode='eval'), indent=4))
/// Expression(
///     body=Tuple(
///         elts=[
///             Constant(value=1),
///             Constant(value=2),
///             Constant(value=3)],
///         ctx=Load()))
///```
type Tuple(elts) =
    inherit Expression ()

    member _.Elements : Expression list = elts
    override _.Print(printer) = ()

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
    inherit Expression ()

    member _.Elements : Expression list = elts

    override _.Print(printer) = ()

/// A set. elts holds a list of nodes representing the set’s elements.
///
/// ```python
/// >>> print(ast.dump(ast.parse('{1, 2, 3}', mode='eval'), indent=4))
/// Expression(
///     body=Set(
///         elts=[
///             Constant(value=1),
///             Constant(value=2),
///             Constant(value=3)]))
/// `````
type Set(elts) =
    inherit Expression ()

    member _.Elements : Expression list = elts

    override _.Print(printer) = ()

/// A dictionary. keys and values hold lists of nodes representing the keys and the values respectively, in matching
/// order (what would be returned when calling dictionary.keys() and dictionary.values()).
///
/// When doing dictionary unpacking using dictionary literals the expression to be expanded goes in the values list,
/// with a None at the corresponding position in keys.
///
/// ```python
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
    inherit Expression ()

    member _.Keys : Expression list= keys
    member _.Values : Expression list= values

    override _.Print(printer) = ()

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
type Yield (?value) =
    inherit Expression ()
    member _.Value : Expression option = value

    override _.Print(printer) = ()

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
type YieldFrom (?value) =
    inherit Expression ()
    member _.Value : Expression option = value

    override _.Print(printer) = ()

//#endregion

//#region Operators

type Add =
    inherit Operator

type Sub =
    inherit Operator

type Mult =
    inherit Operator

type Div =
    inherit Operator

type FloorDiv =
    inherit Operator

type Mod =
    inherit Operator

type Pow =
    inherit Operator

type LShift =
    inherit Operator

type RShift =
    inherit Operator

type BitOr =
    inherit Operator

type BitXor =
    inherit Operator

type BitAnd =
    inherit Operator

type MatMult =
    inherit Operator

//#endregion

//#region Comparison operator tokens.

type Eq =
    inherit ComparisonOperator

type NotEq =
    inherit ComparisonOperator

type Lt =
    inherit ComparisonOperator

type LtE =
    inherit ComparisonOperator

type Gt =
    inherit ComparisonOperator

type GtE =
    inherit ComparisonOperator

type Is =
    inherit ComparisonOperator

type IsNot =
    inherit ComparisonOperator

type In =
    inherit ComparisonOperator

type NotIn =
    inherit ComparisonOperator

//#endregion

//#region  Bool Operators

type And =
    inherit BoolOperator

type Or =
    inherit BoolOperator

//#region

//#region Unary Operators

type Invert =
    inherit UnaryOperator

type Not =
    inherit UnaryOperator

type UAdd =
    inherit UnaryOperator

type USub =
    inherit UnaryOperator

//#endregion