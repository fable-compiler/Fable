// Python AST based on https://docs.python.org/3/library/ast.html. Currently uses records instead of tagged unions to
// better match with the Python AST docs.
namespace rec Fable.AST.Python

// fsharplint:disable MemberNames InterfaceNames

open Fable.AST
open Fable.AST.Python

/// https://docs.python.org/3/library/ast.html#expressions
type Expression =
    | Attribute of Attribute
    | Subscript of Subscript
    | BoolOp of BoolOp
    | BinOp of BinOp
    | Await of Expression
    /// A yield from expression. Because these are expressions, they must be wrapped in a Expr node if the value sent
    /// back is not used.
    | YieldFrom of Expression option
    /// A yield expression. Because these are expressions, they must be wrapped in a Expr node if the value sent back is
    /// not used.
    | Yield of Expression option
    | Emit of Emit
    | IfExp of IfExp
    | UnaryOp of UnaryOp
    | FormattedValue of FormattedValue
    /// A constant value. The value attribute of the Constant literal contains the Python object it represents. The
    /// values represented can be simple types such as a number, string or None, but also immutable container types
    /// (tuples and frozensets) if all of their elements are constant.
    | Constant of value: Literal * loc: SourceLocation option
    | Call of Call
    | Compare of Compare
    | Lambda of Lambda
    | NamedExpr of NamedExpr
    /// A variable name. id holds the name as a string, and ctx is one of the following types.
    | Name of Name
    | Dict of Dict
    | Tuple of Tuple
    | Starred of value: Expression * ctx: ExpressionContext
    | List of elts: Expression list * ctx: ExpressionContext
    | Slice of
        lower: Expression option *
        upper: Expression option *
        step: Expression option

type Literal =
    | FloatLiteral of float
    | IntLiteral of obj
    | BoolLiteral of bool
    | BytesLiteral of byte[]
    | StringLiteral of string
    | NoneLiteral
    | TupleLiteral of Literal list
    | FrozensetLiteral of Literal list

type Operator =
    | Add
    | Sub
    | Mult
    | Div
    | FloorDiv
    | Mod
    | Pow
    | LShift
    | RShift
    | BitOr
    | BitXor
    | BitAnd
    | MatMult

type BoolOperator =
    | And
    | Or

type ComparisonOperator =
    | Eq
    | NotEq
    | Lt
    | LtE
    | Gt
    | GtE
    | Is
    | IsNot
    | In
    | NotIn

type UnaryOperator =
    | Invert
    | Not
    | UAdd
    | USub

type ExpressionContext =
    | Load
    | Del
    | Store

type Identifier =
    | Identifier of name: string

    member this.Name =
        let (Identifier name) = this
        name

/// https://docs.python.org/3/library/ast.html#statements
type Statement =
    | Pass
    | Break
    | Continue
    | If of If
    | For of For
    | Try of Try
    | Expr of Expr
    | With of With
    | While of While
    | Raise of Raise
    | Import of Import
    | Assign of Assign
    | Return of Return
    | Global of Global
    | NonLocal of NonLocal
    | ClassDef of ClassDef
    | AsyncFor of AsyncFor
    | AnnAssign of AnnAssign
    | ImportFrom of ImportFrom
    | FunctionDef of FunctionDef
    | AsyncFunctionDef of AsyncFunctionDef

type Module = { Body: Statement list }

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
type Alias =
    {
        Name: Identifier
        AsName: Identifier option
    }

/// A single except clause. type is the exception type it will match, typically a Name node (or None for a catch-all
/// except: clause). name is a raw string for the name to hold the exception, or None if the clause doesn’t have as foo.
/// body is a list of nodes.
type ExceptHandler =
    {
        Type: Expression option
        Name: Identifier option
        Body: Statement list
        Loc: SourceLocation option
    }

/// try blocks. All attributes are list of nodes to execute, except for handlers, which is a list of ExceptHandler
/// nodes.
type Try =
    {
        Body: Statement list
        Handlers: ExceptHandler list
        OrElse: Statement list
        FinalBody: Statement list
        Loc: SourceLocation option
    }

/// A single context manager in a with block. context_expr is the context manager, often a Call node. optional_vars is a
/// Name, Tuple or List for the as foo part, or None if that isn’t used.
type WithItem =
    {
        ContextExpr: Expression
        OptionalVars: Expression option
    }

/// A with block. items is a list of withitem nodes representing the context managers, and body is the indented block
/// inside the context.
type With =
    {
        Items: WithItem list
        Body: Statement list
        TypeComment: string option
    }

/// A single argument in a list. arg is a raw string of the argument name, annotation is its annotation, such as a Str
/// or Name node.
///
/// - type_comment is an optional string with the type annotation as a comment
type Arg =
    {
        Lineno: int
        ColOffset: int
        EndLineno: int option
        EndColOffset: int option

        Arg: Identifier
        Annotation: Expression option
        TypeComment: string option
    }

type Keyword =
    {
        Lineno: int
        ColOffset: int
        EndLineno: int option
        EndColOffset: int option

        Arg: Identifier
        Value: Expression
    }

/// The arguments for a function.
///
///  - posonlyargs, args and kwonlyargs are lists of arg nodes.
///  - vararg and kwarg are single arg nodes, referring to the *args, **kwargs parameters.
///  - kwDefaults is a list of default values for keyword-only arguments. If one is None, the corresponding argument is
///    required.
///  - defaults is a list of default values for arguments that can be passed positionally. If there are fewer defaults,
///    they correspond to the last n arguments.
type Arguments =
    {
        PosOnlyArgs: Arg list // https://www.python.org/dev/peps/pep-0570/
        Args: Arg list
        VarArg: Arg option
        KwOnlyArgs: Arg list
        KwDefaults: Expression list
        KwArg: Arg option
        Defaults: Expression list
    }

//#region Statements

/// An assignment. targets is a list of nodes, and value is a single node.
///
/// Multiple nodes in targets represents assigning the same value to each. Unpacking is represented by putting a Tuple
/// or List within targets.
///
/// type_comment is an optional string with the type annotation as a comment.
/// https://docs.python.org/3/library/ast.html#ast.Assign
type Assign =
    {
        Targets: Expression list
        Value: Expression
        TypeComment: string option
    }

/// An assignment with a type annotation. target is a single node and can be a Name, a Attribute or a Subscript.
/// annotation is the annotation, such as a Constant or Name node. value is a single optional node. simple is a
/// boolean integer set to True for a Name node in target that do not appear in between parenthesis and are hence
/// pure names and not expressions.
/// https://docs.python.org/3/library/ast.html#ast.AnnAssign
type AnnAssign =
    {
        Target: Expression
        Value: Expression option
        Annotation: Expression
        Simple: bool
    }

/// When an expression, such as a function call, appears as a statement by itself with its return value not used or
/// stored, it is wrapped in this container. value holds one of the other nodes in this section, a Constant, a Name, a
/// Lambda, a Yield or YieldFrom node.
type Expr = { Value: Expression }

/// A for loop. target holds the variable(s) the loop assigns to, as a single Name, Tuple or List node. iter holds the
/// item to be looped over, again as a single node. body and orelse contain lists of nodes to execute. Those in orelse
/// are executed if the loop finishes normally, rather than via a break statement.
///
/// type_comment is an optional string with the type annotation as a comment.
type For =
    {
        Target: Expression
        Iterator: Expression
        Body: Statement list
        Else: Statement list
        TypeComment: string option
    }

type AsyncFor =
    {
        Target: Expression
        Iterator: Expression
        Body: Statement list
        Else: Statement list
        TypeComment: string option
    }

/// A while loop. test holds the condition, such as a Compare node.
type While =
    {
        Test: Expression
        Body: Statement list
        Else: Statement list
        Loc: SourceLocation option
    }

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
type ClassDef =
    {
        Name: Identifier
        Bases: Expression list
        Keyword: Keyword list
        Body: Statement list
        DecoratorList: Expression list
        Loc: SourceLocation option
    }

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
type If =
    {
        Test: Expression
        Body: Statement list
        Else: Statement list
        Loc: SourceLocation option
    }

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
type Raise =
    {
        Exception: Expression
        Cause: Expression option
    }

    static member Create(exc, ?cause) : Statement =
        {
            Exception = exc
            Cause = cause
        }
        |> Raise

/// A function definition.
///
/// - name is a raw string of the function name.
/// - args is a arguments node.
/// - body is the list of nodes inside the function.
/// - decorator_list is the list of decorators to be applied, stored outermost first (i.e. the first in the list will be
///   applied last).
/// - returns is the return annotation.
/// - type_comment is an optional string with the type annotation as a comment.
type FunctionDef =
    {
        Name: Identifier
        Args: Arguments
        Body: Statement list
        DecoratorList: Expression list
        Returns: Expression option
        TypeComment: string option
    }

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
type Global =
    {
        Names: Identifier list
    }

    static member Create(names) = { Names = names }

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
type NonLocal =
    {
        Names: Identifier list
    }

    static member Create(names) = { Names = names }

/// An async function definition.
///
/// - name is a raw string of the function name.
/// - args is a arguments node.
/// - body is the list of nodes inside the function.
/// - decorator_list is the list of decorators to be applied, stored outermost first (i.e. the first in the list will be
///   applied last).
/// - returns is the return annotation.
/// - type_comment is an optional string with the type annotation as a comment.
type AsyncFunctionDef =
    {
        Name: Identifier
        Args: Arguments
        Body: Statement list
        DecoratorList: Expression list
        Returns: Expression option
        TypeComment: string option
    }

    static member Create
        (
            name,
            args,
            body,
            decoratorList,
            ?returns,
            ?typeComment
        )
        =
        {
            Name = name
            Args = args
            Body = body
            DecoratorList = decoratorList
            Returns = returns
            TypeComment = typeComment
        }

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
type Import = { Names: Alias list }


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
type ImportFrom =
    {
        Module: Identifier option
        Names: Alias list
        Level: int option
    }

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
type Return = { Value: Expression option }

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
type Attribute =
    {
        Value: Expression
        Attr: Identifier
        Ctx: ExpressionContext
    }

type NamedExpr =
    {
        Target: Expression
        Value: Expression
        Loc: SourceLocation option
    }

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
type Subscript =
    {
        Value: Expression
        Slice: Expression
        Ctx: ExpressionContext
    }

type BinOp =
    {
        Left: Expression
        Right: Expression
        Operator: Operator
        Loc: SourceLocation option
    }

type BoolOp =
    {
        Values: Expression list
        Operator: BoolOperator
        Loc: SourceLocation option
    }

/// A comparison of two or more values. left is the first value in the comparison, ops the list of operators, and
/// comparators the list of values after the first element in the comparison.
///
/// ```py
/// >>> print(ast.dump(ast.parse('1 <= a < 10', mode='eval'), indent=4))
/// Expression(
///     body=Compare(
///         left=Constant(value=1),
///         ops=[
///             LtE(),
///             Lt()],
///         comparators=[
///             Name(id='a', ctx=Load()),
///             Constant(value=10)]))
/// `````
type Compare =
    {
        Left: Expression
        Comparators: Expression list
        Ops: ComparisonOperator list
        Loc: SourceLocation option
    }

/// A unary operation. op is the operator, and operand any expression node.
type UnaryOp =
    {
        Op: UnaryOperator
        Operand: Expression
        Loc: SourceLocation option
    }

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
type FormattedValue =
    {
        Value: Expression
        Conversion: int option
        FormatSpec: Expression option
    }

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
type Call =
    {
        Func: Expression
        Args: Expression list
        Keywords: Keyword list
        Loc: SourceLocation option
    }

type Emit =
    {
        Value: string
        Args: Expression list
        Loc: SourceLocation option
    }

/// An expression such as a if b else c. Each field holds a single node, so in the following example, all three are Name nodes.
///
/// ```py
/// >>> print(ast.dump(ast.parse('a if b else c', mode='eval'), indent=4))
/// Expression(
///     body=IfExp(
///         test=Name(id='b', ctx=Load()),
///         body=Name(id='a', ctx=Load()),
///         orelse=Name(id='c', ctx=Load())))
/// ```
type IfExp =
    {
        Test: Expression
        Body: Expression
        OrElse: Expression
        Loc: SourceLocation option
    }

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
type Lambda =
    {
        Args: Arguments
        Body: Expression
    }

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
type Tuple =
    {
        Elements: Expression list
        Loc: SourceLocation option
    }

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
type List = { Elements: Expression list }

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
type Set = { Elements: Expression list }

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
type Dict =
    {
        Keys: Expression list
        Values: Expression list
    }

/// A variable name. id holds the name as a string, and ctx is one of the following types.
type Name =
    {
        Id: Identifier
        Context: ExpressionContext
        Loc: SourceLocation option
    }

[<RequireQualifiedAccess>]
type AST =
    | Expression of Expression
    | Statement of Statement
    | Operator of Operator
    | BoolOperator of BoolOperator
    | ComparisonOperator of ComparisonOperator
    | UnaryOperator of UnaryOperator
    | ExpressionContext of ExpressionContext
    | Alias of Alias
    | Module of Module
    | Arguments of Arguments
    | Keyword of Keyword
    | Arg of Arg
    | Identifier of Identifier
    | WithItem of WithItem

[<AutoOpen>]
module PythonExtensions =
    [<Literal>]
    let Ellipsis = "..."

    type Statement with

        static member break'() : Statement = Break
        static member continue' ?loc : Statement = Continue
        static member import(names) : Statement = Import { Names = names }
        static member expr(value) : Statement = { Expr.Value = value } |> Expr
        static member ellipsis: Statement = Statement.expr (Expression.ellipsis)

        static member raise(value) : Statement =
            {
                Exception = value
                Cause = None
            }
            |> Raise

        static member try'
            (
                body,
                ?handlers,
                ?orElse,
                ?finalBody,
                ?loc
            )
            : Statement
            =
            Try.try' (
                body,
                ?handlers = handlers,
                ?orElse = orElse,
                ?finalBody = finalBody,
                ?loc = loc
            )
            |> Try

        static member with'(items, ?body, ?typeComment) : Statement =
            {
                Items = items
                Body = defaultArg body []
                TypeComment = typeComment
            }
            |> With

        static member classDef
            (
                name,
                ?bases,
                ?keywords,
                ?body,
                ?decoratorList,
                ?loc
            )
            : Statement
            =
            {
                Name = name
                Bases = defaultArg bases []
                Keyword = defaultArg keywords []
                Body = defaultArg body []
                DecoratorList = defaultArg decoratorList []
                Loc = loc
            }
            |> ClassDef

        static member functionDef
            (
                name,
                args,
                body,
                ?decoratorList,
                ?returns,
                ?typeComment
            )
            : Statement
            =
            {
                FunctionDef.Name = name
                Args = args
                Body = body
                DecoratorList = defaultArg decoratorList []
                Returns = returns
                TypeComment = typeComment
            }
            |> FunctionDef

        static member asyncFunctionDef
            (
                name,
                args,
                body,
                ?decoratorList,
                ?returns,
                ?typeComment
            )
            : Statement
            =
            {
                AsyncFunctionDef.Name = name
                Args = args
                Body = body
                DecoratorList = defaultArg decoratorList []
                Returns = returns
                TypeComment = typeComment
            }
            |> AsyncFunctionDef

        static member assign(targets, value, ?typeComment) : Statement =
            {
                Targets = targets
                Value = value
                TypeComment = typeComment
            }
            |> Assign

        static member assign(target, annotation, ?value, ?simple) : Statement =
            {
                Target = target
                Value = value
                Annotation = annotation
                Simple = defaultArg simple true
            }
            |> AnnAssign

        static member return'(?value) : Statement = Return { Value = value }

        static member for'
            (
                target,
                iter,
                ?body,
                ?orelse,
                ?typeComment
            )
            : Statement
            =
            For.for' (
                target,
                iter,
                ?body = body,
                ?orelse = orelse,
                ?typeComment = typeComment
            )
            |> For

        static member while'(test, body, ?orelse, ?loc) : Statement =
            {
                While.Test = test
                Body = body
                Else = defaultArg orelse []
                Loc = loc
            }
            |> While

        static member if'(test, body, ?orelse, ?loc) : Statement =
            {
                Test = test
                Body = body
                Else = defaultArg orelse []
                Loc = loc
            }
            |> If

        static member importFrom(``module``, names, ?level) =
            ImportFrom.importFrom (``module``, names, ?level = level)
            |> ImportFrom

        static member nonLocal(ids) =
            NonLocal.Create ids |> Statement.NonLocal

        static member global'(ids) = Global.Create ids |> Statement.Global

    type Expression with

        static member name(identifier, ?ctx, ?loc) : Expression =
            {
                Id = identifier
                Context = defaultArg ctx Load
                Loc = loc
            }
            |> Name

        static member name(name, ?ctx) : Expression =
            Expression.name (Identifier(name), ?ctx = ctx)

        static member identifier(name, ?ctx, ?loc) : Expression =
            Expression.name (Identifier(name), ?ctx = ctx, ?loc = loc)

        static member identifier(identifier, ?ctx, ?loc) : Expression =
            Expression.name (identifier, ?ctx = ctx, ?loc = loc)

        static member dict(keys, values) : Expression =
            {
                Keys = keys
                Values = values
            }
            |> Dict

        static member dict() : Expression = Expression.dict ([], [])

        static member tuple(elts, ?loc) : Expression =
            {
                Elements = elts
                Loc = loc
            }
            |> Tuple

        static member slice(?lower, ?upper, ?slice) : Expression =
            Slice(lower, upper, slice)

        static member ifExp(test, body, orElse, ?loc) : Expression =
            {
                Test = test
                Body = body
                OrElse = orElse
                Loc = loc
            }
            |> IfExp

        static member lambda(args, body) : Expression =
            {
                Args = args
                Body = body
            }
            |> Lambda

        static member emit(value, ?args, ?loc) : Expression =
            {
                Value = value
                Args = defaultArg args []
                Loc = loc
            }
            |> Emit

        static member call(func, ?args, ?kw, ?loc) : Expression =
            {
                Func = func
                Args = defaultArg args []
                Keywords = defaultArg kw []
                Loc = loc
            }
            |> Call

        static member compare(left, ops, comparators, ?loc) : Expression =
            {
                Left = left
                Comparators = comparators
                Ops = ops
                Loc = loc
            }
            |> Compare

        static member none = Expression.name (Identifier(name = "None"))
        static member any = Expression.name (Identifier(name = "Any"))
        static member ellipsis = Expression.name (Identifier(name = Ellipsis))

        static member attribute(value, attr, ?ctx) : Expression =
            {
                Value = value
                Attr = attr
                Ctx = defaultArg ctx Load
            }
            |> Attribute

        static member unaryOp(op, operand, ?loc) : Expression =
            let op =
                match op with
                | UnaryMinus -> USub
                | UnaryPlus -> UAdd
                | UnaryNot -> Not
                | UnaryNotBitwise -> Invert
                // | UnaryAddressOf -> "&"
                | _ -> failwith $"unaryOp: Operator {op} not supported"

            Expression.unaryOp (op, operand, ?loc = loc)

        static member unaryOp(op, operand, ?loc) : Expression =
            {
                Op = op
                Operand = operand
                Loc = loc
            }
            |> UnaryOp

        static member namedExpr(target, value, ?loc) =
            {
                Target = target
                Value = value
                Loc = loc
            }
            |> NamedExpr

        static member subscript(value, slice, ?ctx) : Expression =
            {
                Value = value
                Slice = slice
                Ctx = defaultArg ctx Load
            }
            |> Subscript

        static member binOp(left, op, right, ?loc) : Expression =
            {
                Left = left
                Right = right
                Operator = op
                Loc = loc
            }
            |> BinOp

        static member binOp(left, op, right, ?loc) : Expression =
            let op =
                match op with
                | BinaryPlus -> Add
                | BinaryMinus -> Sub
                | BinaryMultiply -> Mult
                | BinaryDivide -> Div
                | BinaryModulus -> Mod
                | BinaryOrBitwise -> BitOr
                | BinaryAndBitwise -> BitAnd
                | BinaryShiftLeft -> LShift
                | BinaryShiftRightZeroFill -> RShift
                | BinaryShiftRightSignPropagating -> RShift
                | BinaryXorBitwise -> BitXor
                | _ -> failwith $"binOp: Operator {op} not supported"

            Expression.binOp (left, op, right, ?loc = loc)

        static member boolOp(op: BoolOperator, values, ?loc) : Expression =
            {
                Values = values
                Operator = op
                Loc = loc
            }
            |> BoolOp

        static member boolOp(op: LogicalOperator, values, ?loc) : Expression =
            let op =
                match op with
                | LogicalAnd -> And
                | LogicalOr -> Or

            Expression.boolOp (op, values, ?loc = loc)

        static member boolConstant(value: bool, ?loc) : Expression =
            Constant(value = BoolLiteral value, loc = loc)

        static member intConstant(value: obj, ?loc) : Expression =
            Constant(value = IntLiteral value, loc = loc)

        static member floatConstant(value: float, ?loc) : Expression =
            Constant(value = FloatLiteral value, loc = loc)

        static member stringConstant(value: string, ?loc) : Expression =
            Constant(value = StringLiteral value, loc = loc)

        static member constant(value: Literal, ?loc) : Expression =
            Constant(value = value, loc = loc)

        static member starred
            (
                value: Expression,
                ?ctx: ExpressionContext
            )
            : Expression
            =
            Starred(value, ctx |> Option.defaultValue Load)

        static member list
            (
                elts: Expression list,
                ?ctx: ExpressionContext
            )
            : Expression
            =
            List(elts, ctx |> Option.defaultValue Load)

    type List with

        static member list(elts) = { Elements = elts }

    type ExceptHandler with

        static member exceptHandler(``type``, ?name, ?body, ?loc) =
            {
                Type = ``type``
                Name = name
                Body = defaultArg body []
                Loc = loc
            }

    type Alias with

        static member alias(name, ?asname) =
            {
                Name = name
                AsName = asname
            }

    type WithItem with

        static member withItem(contextExpr, ?optinalVars) =
            {
                ContextExpr = contextExpr
                OptionalVars = optinalVars
            }

    type Try with

        static member try'(body, ?handlers, ?orElse, ?finalBody, ?loc) =
            {
                Body = body
                Handlers = defaultArg handlers []
                OrElse = defaultArg orElse []
                FinalBody = defaultArg finalBody []
                Loc = loc
            }

    type FormattedValue with

        static member formattedValue(value, ?conversion, ?formatSpec) =
            {
                Value = value
                Conversion = conversion
                FormatSpec = formatSpec
            }

    type Module with

        static member module'(body) = { Body = body }

    type Arg with

        static member arg(arg, ?annotation, ?typeComment) =
            {
                Lineno = 0
                ColOffset = 0
                EndLineno = None
                EndColOffset = None

                Arg = arg
                Annotation = annotation
                TypeComment = typeComment
            }

        static member arg(arg, ?annotation, ?typeComment) =
            Arg.arg (
                Identifier(arg),
                ?annotation = annotation,
                ?typeComment = typeComment
            )

    type Keyword with

        static member keyword(arg, value) =
            {
                Lineno = 0
                ColOffset = 0
                EndLineno = None
                EndColOffset = None

                Arg = arg
                Value = value
            }

    type Arguments with

        static member arguments
            (
                ?args,
                ?posonlyargs,
                ?vararg,
                ?kwonlyargs,
                ?kwDefaults,
                ?kwarg,
                ?defaults
            )
            =
            {
                PosOnlyArgs = defaultArg posonlyargs []
                Args = defaultArg args []
                VarArg = vararg
                KwOnlyArgs = defaultArg kwonlyargs []
                KwDefaults = defaultArg kwDefaults []
                KwArg = kwarg
                Defaults = defaultArg defaults []
            }

        static member empty = Arguments.arguments ()

    type For with

        static member for'(target, iter, ?body, ?orelse, ?typeComment) =
            {
                Target = target
                Iterator = iter
                Body = defaultArg body []
                Else = defaultArg orelse []
                TypeComment = typeComment
            }

    type AsyncFor with

        static member asyncFor(target, iter, body, ?orelse, ?typeComment) =
            {
                Target = target
                Iterator = iter
                Body = body
                Else = defaultArg orelse []
                TypeComment = typeComment
            }

    type ImportFrom with

        static member importFrom(``module``, names, ?level) =
            {
                Module = ``module``
                Names = names
                Level = level
            }

    type Expr with

        static member expr(value) : Expr = { Value = value }
