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

        member printer.Print(node: IPrint) = node.Print(printer)

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
                | Constant (_) -> true
                | Dict { Keys = keys } -> keys.IsEmpty
                | _ -> false

            match stmt with
            | Expr (expr) -> hasNoSideEffects expr.Value |> not
            | _ -> true

        member printer.PrintStatement(stmt: Statement, ?printSeparator) =
            printer.Print(stmt)

            printSeparator
            |> Option.iter (fun fn -> fn printer)

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

        member printer.PrintOptional(before: string, node: IPrint option) =
            match node with
            | None -> ()
            | Some node ->
                printer.Print(before)
                node.Print(printer)

        member printer.PrintOptional(before: string, node: IPrint option, after: string) =
            match node with
            | None -> ()
            | Some node ->
                printer.Print(before)
                node.Print(printer)
                printer.Print(after)

        member printer.PrintOptional(node: IPrint option) =
            match node with
            | None -> ()
            | Some node -> node.Print(printer)

        member printer.PrintList(nodes: 'a list, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes.[i]

                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedList(nodes: IPrint list) =
            printer.PrintList(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedList(nodes: Expression list) =
            printer.PrintList(nodes, (fun p x -> p.SequenceExpressionWithParens(x)), (fun p -> p.Print(", ")))

        member printer.PrintFunction
            (
                id: Identifier option,
                args: Arguments,
                body: Statement list,
                returnType: Expression option,
                ?isDeclaration
            ) =
            printer.Print("def ")
            printer.PrintOptional(id |> Option.map (fun x -> x :> IPrint))
            printer.Print("(")
            printer.Print(args)
            printer.Print(")")
            printer.PrintOptional(returnType |> Option.map (fun x -> x :> IPrint))
            printer.Print(":")
            printer.PrintBlock(body, skipNewLineAtEnd = true)

        member printer.WithParens(expr: Expression) =
            printer.Print("(")
            printer.Print(expr)
            printer.Print(")")

        member printer.SequenceExpressionWithParens(expr: Expression) =
            match expr with
            //| :? SequenceExpression -> printer.WithParens(expr)
            | _ -> printer.Print(expr)

        /// Surround with parens anything that can potentially conflict with operator precedence
        member printer.ComplexExpressionWithParens(expr: Expression) =
            printfn "Expr: %A" expr

            match expr with
            | Constant (_) -> printer.Print(expr)
            | Name (_) -> printer.Print(expr)
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

type IPrint =
    abstract Print: Printer -> unit

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

    interface IPrint with
        member x.Print(printer: Printer) =
            match x with
            | Expression (ex) -> printer.Print(ex)
            | Operator (op) -> printer.Print(op)
            | BoolOperator (op) -> printer.Print(op)
            | ComparisonOperator (op) -> printer.Print(op)
            | UnaryOperator (op) -> printer.Print(op)
            | ExpressionContext (ctx) -> printer.Print(ctx)
            | Alias (al) -> printer.Print(al)
            | Module ``mod`` -> printer.Print(``mod``)
            | Arguments (arg) -> printer.Print(arg)
            | Keyword (kw) -> printer.Print(kw)
            | Arg (arg) -> printer.Print(arg)
            | Statement (st) -> printer.Print(st)

type Expression =
    | Attribute of Attribute
    | Subscript of Subscript
    | BoolOp of BoolOp
    | BinOp of BinOp
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
    | Constant of Constant
    | Call of Call
    | Compare of Compare
    | Lambda of Lambda
    | NamedExpr of NamedExpr
    /// A variable name. id holds the name as a string, and ctx is one of the following types.
    | Name of Name
    | Dict of Dict
    | Tuple of Tuple



    // member val Lineno: int = 0 with get, set
    // member val ColOffset: int = 0 with get, set
    // member val EndLineno: int option = None with get, set
    // member val EndColOffset: int option = None with get, set
    interface IPrint with
        member x.Print(printer: Printer) =
            match x with
            | Attribute (ex) -> printer.Print(ex)
            | Subscript (ex) -> printer.Print(ex)
            | BoolOp (ex) -> printer.Print(ex)
            | BinOp (ex) -> printer.Print(ex)
            | Emit (ex) -> printer.Print(ex)
            | UnaryOp (ex) -> printer.Print(ex)
            | FormattedValue (ex) -> printer.Print(ex)
            | Constant (ex) -> printer.Print(ex)
            | IfExp (ex) -> printer.Print(ex)
            | Call (ex) -> printer.Print(ex)
            | Lambda (ex) -> printer.Print(ex)
            | NamedExpr (ex) -> printer.Print(ex)
            | Name (ex) -> printer.Print(ex)
            | Yield (expr) -> printer.Print("(Yield)")
            | YieldFrom (expr) -> printer.Print("(Yield)")
            | Compare (cp) -> printer.Print(cp)
            | Dict (di) -> printer.Print(di)
            | Tuple (tu) -> printer.Print(tu)

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



    interface IPrint with
        member x.Print(printer: Printer) =
            let op =
                match x with
                | Add -> " + "
                | Sub -> " - "
                | Mult -> " * "
                | Div -> " / "
                | FloorDiv -> " // "
                | Mod -> " % "
                | Pow -> " ** "
                | LShift -> " << "
                | RShift -> " >> "
                | BitOr -> " | "
                | BitXor -> " ^ "
                | BitAnd -> $" & "
                | MatMult -> $" @ "

            printer.Print(op)

type BoolOperator =
    | And
    | Or



    interface IPrint with
        member x.Print(printer: Printer) =
            let op =
                match x with
                | And -> " and "
                | Or -> " or "

            printer.Print(op)


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



    interface IPrint with
        member x.Print(printer) =
            let op =
                match x with
                | Eq -> " == "
                | NotEq -> " != "
                | Lt -> " < "
                | LtE -> " <= "
                | Gt -> " > "
                | GtE -> " >= "
                | Is -> " is "
                | IsNot -> " is not "
                | In -> " in "
                | NotIn -> " not in "

            printer.Print(op)

type UnaryOperator =
    | Invert
    | Not
    | UAdd
    | USub



    interface IPrint with
        member this.Print(printer) =
            let op =
                match this with
                | Invert -> "~"
                | Not -> "not "
                | UAdd -> "+"
                | USub -> "-"

            printer.Print(op)

type ExpressionContext =
    | Load
    | Del
    | Store



    interface IPrint with
        member this.Print(printer) = ()

type Identifier =
    | Identifier of string



    interface IPrint with
        member this.Print(printer: Printer) =
            let (Identifier id) = this
            printer.Print(id)

type Statement =
    | Assign of Assign
    | Import of Import
    | ImportFrom of ImportFrom
    | Expr of Expr
    | For of For
    | AsyncFor of AsyncFor
    | If of If
    | ClassDef of ClassDef
    | Raise of Raise
    | Global of Global
    | NonLocal of NonLocal
    | AsyncFunctionDef of AsyncFunctionDef
    | FunctionDef of FunctionDef
    | Return of Return
    | While of While
    | Try of Try
    | Pass
    | Break
    | Continue



    // member val Lineno: int = 0 with get, set
    // member val ColOffset: int = 0 with get, set
    // member val EndLineno: int option = None with get, set
    // member val EndColOffset: int option = None with get, set

    interface IPrint with
        member x.Print(printer) =
            match x with
            | FunctionDef (def) -> printer.Print(def)
            | AsyncFunctionDef (def) -> printer.Print(def)
            | Assign (st) -> printer.Print(st)
            | Expr (st) -> printer.Print(st)
            | For (st) -> printer.Print(st)
            | AsyncFor (st) -> printer.Print(st)
            | If (st) -> printer.Print(st)
            | ClassDef (st) -> printer.Print(st)
            | Raise (st) -> printer.Print(st)
            | Global (st) -> printer.Print(st)
            | NonLocal (st) -> printer.Print(st)
            | Pass -> printer.Print("pass")
            | Break -> printer.Print("break")
            | Continue -> printer.Print("continue")
            | Return (rtn) -> printer.Print(rtn)
            | Import (im) -> printer.Print(im)
            | ImportFrom (im) -> printer.Print(im)
            | While (wh) -> printer.Print(wh)
            | Try (st) -> printer.Print(st)

type Module =
    {
        Body: Statement list
    }

    static member Create(body) = { Body = body }

    interface IPrint with
        member x.Print(printer: Printer) = printer.PrintStatements(x.Body)

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

    static member Create(name, asname) = { Name = name; AsName = asname }

    interface IPrint with
        member x.Print(printer) =
            printer.Print(x.Name)

            match x.AsName with
            | Some (Identifier alias) ->
                printer.Print(" as ")
                printer.Print(alias)
            | _ -> ()

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

    static member Create(``type``, ?name, ?body, ?loc) =
        {
            Type = ``type``
            Name = name
            Body = defaultArg body []
            Loc = loc
        }

    interface IPrint with
        member x.Print(printer) =
            printer.Print("except ", ?loc = x.Loc)
            printer.PrintOptional(x.Type |> Option.map (fun t -> t :> IPrint))
            printer.PrintOptional(" as ", x.Name |> Option.map (fun t -> t :> IPrint))
            printer.Print(":")

            match x.Body with
            | [] -> printer.PrintBlock([ Pass ])
            | _ -> printer.PrintBlock(x.Body)


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

    static member Create(body, ?handlers, ?orElse, ?finalBody, ?loc): Statement =
        {
            Body = body
            Handlers = defaultArg handlers []
            OrElse = defaultArg orElse []
            FinalBody = defaultArg finalBody []
            Loc = loc
        }
        |> Try

    interface IPrint with
        member x.Print(printer) =
            printer.Print("try: ", ?loc = x.Loc)
            printer.PrintBlock(x.Body)

            for handler in x.Handlers do
                printer.Print(handler)

            if x.OrElse.Length > 0 then
                printer.Print("else: ")
                printer.PrintBlock(x.OrElse)

            if x.FinalBody.Length > 0 then
                printer.Print("finally: ")
                printer.PrintBlock(x.FinalBody)

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

    static member Create(arg, ?annotation, ?typeComment) =
        {
            Lineno = 0
            ColOffset = 0
            EndLineno = None
            EndColOffset = None

            Arg = arg
            Annotation = annotation
            TypeComment = typeComment
        }

    interface IPrint with
        member x.Print(printer) =
            let (Identifier name) = x.Arg
            printer.Print(name)
            match x.Annotation with
            | Some ann ->
                printer.Print("=")
                printer.Print(ann)
            | _ -> ()

type Keyword =
    {
        Lineno: int
        ColOffset: int
        EndLineno: int option
        EndColOffset: int option

        Arg: Identifier
        Value: Expression
    }

    static member Create(arg, value) =
        {
            Lineno = 0
            ColOffset = 0
            EndLineno = None
            EndColOffset = None

            Arg = arg
            Value = value
        }

    interface IPrint with
        member x.Print(printer) =
            let (Identifier name) = x.Arg
            printer.Print(name)
            printer.Print(" = ")
            printer.Print(x.Value)

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
        PosOnlyArgs: Arg list
        Args: Arg list
        VarArg: Arg option
        KwOnlyArgs: Arg list
        KwDefaults: Expression list
        KwArg: Arg option
        Defaults: Expression list
    }

    static member Create(?posonlyargs, ?args, ?vararg, ?kwonlyargs, ?kwDefaults, ?kwarg, ?defaults) =
        {
            PosOnlyArgs = defaultArg posonlyargs []
            Args = defaultArg args []
            VarArg = vararg
            KwOnlyArgs = defaultArg kwonlyargs []
            KwDefaults = defaultArg kwDefaults []
            KwArg = kwarg
            Defaults = defaultArg defaults []
        }

    interface IPrint with
        member x.Print(printer) =
            match x.Args, x.VarArg with
            | [], Some vararg ->
                printer.Print("*")
                printer.Print(vararg)
            | args, Some vararg ->
                printer.PrintCommaSeparatedList(args |> List.map (fun arg -> arg :> IPrint))
                printer.Print(", *")
                printer.Print(vararg)
            | args, None ->
                printer.PrintCommaSeparatedList(args |> List.map (fun arg -> arg :> IPrint))

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
type Assign =
    {
        Targets: Expression list
        Value: Expression
        TypeComment: string option
    }

    static member Create(targets, value, ?typeComment): Statement =
        {
            Targets = targets
            Value = value
            TypeComment = typeComment
        }
        |> Assign

    interface IPrint with
        member x.Print(printer) =
            printfn "Assign: %A" (x.Targets, x.Value)
            //printer.PrintOperation(targets.[0], "=", value, None)

            for target in x.Targets do
                printer.Print(target)
                printer.Print(" = ")

            printer.Print(x.Value)

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
type Expr =
    {
        Value: Expression
    }

    static member Create(value): Statement = { Value = value } |> Expr

    interface IPrint with
        member x.Print(printer) = printer.Print(x.Value)

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
type For =
    {
        Target: Expression
        Iterator: Expression
        Body: Statement list
        Else: Statement list
        TypeComment: string option
    }

    static member Create(target, iter, ?body, ?orelse, ?typeComment) =
        {
            Target = target
            Iterator = iter
            Body = defaultArg body []
            Else = defaultArg orelse []
            TypeComment = typeComment
        }

    interface IPrint with
        member x.Print(printer: Printer) =
            printer.Print("for ")
            printer.Print(x.Iterator)
            printer.Print(":")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintStatements(x.Body)
            printer.PopIndentation()

type AsyncFor =
    {
        Target: Expression
        Iterator: Expression
        Body: Statement list
        Else: Statement list
        TypeComment: string option
    }

    static member Create(target, iter, body, ?orelse, ?typeComment) =
        {
            Target = target
            Iterator = iter
            Body = body
            Else = defaultArg orelse []
            TypeComment = typeComment
        }

    interface IPrint with
        member _.Print(printer) = printer.Print("(AsyncFor)")

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
type While =
    {
        Test: Expression
        Body: Statement list
        Else: Statement list
    }

    static member Create(test, body, ?orelse): Statement =
        {
            Test = test
            Body = body
            Else = defaultArg orelse []
        }
        |> While

    interface IPrint with
        member x.Print(printer: Printer) =
            printer.Print("while ")
            printer.Print(x.Test)
            printer.Print(":")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintStatements(x.Body)
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
type ClassDef =
    {
        Name: Identifier
        Bases: Expression list
        Keyword: Keyword list
        Body: Statement list
        DecoratorList: Expression list
        Loc: SourceLocation option
    }

    static member Create(name, ?bases, ?keywords, ?body, ?decoratorList, ?loc): Statement =
        {
            Name = name
            Bases = defaultArg bases []
            Keyword = defaultArg keywords []
            Body = defaultArg body []
            DecoratorList = defaultArg decoratorList []
            Loc = loc
        }
        |> ClassDef

    interface IPrint with
        member x.Print(printer) =
            let (Identifier name) = x.Name
            printer.Print("class ", ?loc = x.Loc)
            printer.Print(name)

            match x.Bases with
            | [] -> ()
            | xs ->
                printer.Print("(")
                printer.PrintCommaSeparatedList(x.Bases)
                printer.Print(")")

            printer.Print(":")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintStatements(x.Body)
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
type If =
    {
        Test: Expression
        Body: Statement list
        Else: Statement list
    }

    static member Create(test, body, ?orelse): Statement =
        {
            Test = test
            Body = body
            Else = defaultArg orelse []
        }
        |> If

    interface IPrint with
        member x.Print(printer) =
            let rec printElse stmts =
                match stmts with
                | []
                | [ Pass ] -> ()
                | [ If iff ] ->
                    printer.Print("elif ")
                    printer.Print(iff.Test)
                    printer.Print(":")
                    printer.PrintBlock(iff.Body)
                    printElse iff.Else
                | xs ->
                    printer.Print("else: ")
                    printer.PrintBlock(xs)


            printer.Print("if ")
            printer.Print(x.Test)
            printer.Print(":")
            printer.PrintBlock(x.Body)
            printElse x.Else


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

    static member Create(exc, ?cause): Statement = { Exception = exc; Cause = cause } |> Raise

    interface IPrint with
        member _.Print(printer) = printer.Print("(Raise)")

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

    static member Create(name, args, body, ?decoratorList, ?returns, ?typeComment): Statement =
        {
            Name = name
            Args = args
            Body = body
            DecoratorList = defaultArg decoratorList []
            Returns = returns
            TypeComment = typeComment
        }
        |> FunctionDef

    interface IPrint with
        member x.Print(printer: Printer) =
            printer.PrintFunction(Some x.Name, x.Args, x.Body, x.Returns, isDeclaration = true)
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
type Global =
    {
        Names: Identifier list
    }

    static member Create(names) = { Names = names }

    interface IPrint with
        member x.Print(printer) = printer.Print("(Global)")

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

    interface IPrint with
        member _.Print(printer: Printer) = printer.Print("(NonLocal)")


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

    static member Create(name, args, body, decoratorList, ?returns, ?typeComment) =
        {
            Name = name
            Args = args
            Body = body
            DecoratorList = decoratorList
            Returns = returns
            TypeComment = typeComment
        }

    interface IPrint with
        member _.Print(printer: Printer) = printer.Print("(AsyncFunctionDef)")

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
type Import =
    {
        Names: Alias list
    }

    static member Create(names): Statement = Import { Names = names }

    interface IPrint with
        member _.Print(printer) = printer.Print("(Import)")

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

    static member Create(``module``, names, ?level): Statement =
        {
            Module = ``module``
            Names = names
            Level = level
        }
        |> ImportFrom

    interface IPrint with
        member x.Print(printer: Printer) =
            let (Identifier path) = x.Module |> Option.defaultValue (Identifier ".")

            printer.Print("from ")
            printer.Print(printer.MakeImportPath(path))
            printer.Print(" import ")

            if not (List.isEmpty x.Names) then
                if List.length x.Names > 1 then
                    printer.Print("(")

                printer.PrintCommaSeparatedList(x.Names |> List.map (fun x -> x :> IPrint))

                if List.length x.Names > 1 then
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
type Return =
    {
        Value: Expression option
    }

    static member Create(?value): Statement = Return { Value = value }

    interface IPrint with
        member this.Print(printer) =
            printer.Print("return ")
            printer.PrintOptional(this.Value |> Option.map (fun x -> x :> IPrint))

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


    static member Create(value, attr, ctx): Expression =
        {
            Value = value
            Attr = attr
            Ctx = ctx
        }
        |> Attribute

    interface IPrint with
        member this.Print(printer) =
            printer.Print(this.Value)
            printer.Print(".")
            printer.Print(this.Attr)

type NamedExpr =
    {
        Target: Expression
        Value: Expression
    }
    static member Create(target, value) =
        {
            Target = target
            Value = value
        }
        |> NamedExpr

    interface IPrint with
        member this.Print(printer) =
            printer.Print(this.Target)
            printer.Print(" :=")
            printer.Print(this.Value)

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

    static member Create(value, slice, ctx): Expression =
        {
            Value = value
            Slice = slice
            Ctx = ctx
        }
        |> Subscript

    interface IPrint with
        member this.Print(printer: Printer) =
            printer.Print(this.Value)
            printer.Print("[")
            printer.Print(this.Slice)
            printer.Print("]")

type BinOp =
    {
        Left: Expression
        Right: Expression
        Operator: Operator
    }

    static member Create(left, op, right): Expression =
        {
            Left = left
            Right = right
            Operator = op
        }
        |> BinOp

    interface IPrint with
        member this.Print(printer) = printer.PrintOperation(this.Left, this.Operator, this.Right)


type BoolOp =
    {
        Values: Expression list
        Operator: BoolOperator
    }

    static member Create(op, values): Expression = { Values = values; Operator = op } |> BoolOp

    interface IPrint with

        member this.Print(printer) =
            for i, value in this.Values |> List.indexed do
                printer.ComplexExpressionWithParens(value)

                if i < this.Values.Length - 1 then
                    printer.Print(this.Operator)

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
    }

    static member Create(left, ops, comparators): Expression =
        {
            Left = left
            Comparators = comparators
            Ops = ops
        }
        |> Compare

    interface IPrint with
        member x.Print(printer) =
            //printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(x.Left)

            for op, comparator in List.zip x.Ops x.Comparators do
                printer.Print(op)
                printer.ComplexExpressionWithParens(comparator)

/// A unary operation. op is the operator, and operand any expression node.
type UnaryOp =
    {
        Op: UnaryOperator
        Operand: Expression
        Loc: SourceLocation option
    }

    static member Create(op, operand, ?loc): Expression =
        {
            Op = op
            Operand = operand
            Loc = loc
        }
        |> UnaryOp

    interface IPrint with
        override x.Print(printer) =
            printer.AddLocation(x.Loc)

            match x.Op with
            | USub
            | UAdd
            | Not
            | Invert -> printer.Print(x.Op)

            printer.ComplexExpressionWithParens(x.Operand)

/// A constant value. The value attribute of the Constant literal contains the Python object it represents. The values
/// represented can be simple types such as a number, string or None, but also immutable container types (tuples and
/// frozensets) if all of their elements are constant.
///
/// ```py
/// >>> print(ast.dump(ast.parse('123', mode='eval'), indent=4))
/// Expression(
///     body=Constant(value=123))
/// `````
type Constant =
    {
        Value: obj
    }

    static member Create(value: obj): Expression = { Value = value } |> Constant

    interface IPrint with
        member x.Print(printer) =
            match box x.Value with
            | :? string as str ->
                printer.Print("\"")
                printer.Print(string x.Value)
                printer.Print("\"")
            | _ -> printer.Print(string x.Value)

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

    static member Create(value, ?conversion, ?formatSpec) =
        {
            Value = value
            Conversion = conversion
            FormatSpec = formatSpec
        }

    interface IPrint with
        member _.Print(printer) = printer.Print("(FormattedValue)")

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
    }

    static member Create(func, ?args, ?kw): Expression =
        {
            Func = func
            Args = defaultArg args []
            Keywords = defaultArg kw []
        }
        |> Call

    interface IPrint with
        member x.Print(printer) =
            printer.Print(x.Func)
            printer.Print("(")
            printer.PrintCommaSeparatedList(x.Args)
            printer.PrintCommaSeparatedList(x.Keywords |> List.map (fun x -> x :> IPrint))
            printer.Print(")")

type Emit =
    {
        Value: string
        Args: Expression list
    }

    static member Create(value, ?args): Expression =
        {
            Value = value
            Args = defaultArg args []
        }
        |> Emit

    interface IPrint with
        member x.Print(printer) =
            let inline replace pattern (f: System.Text.RegularExpressions.Match -> string) input =
                System.Text.RegularExpressions.Regex.Replace(input, pattern, f)

            let printSegment (printer: Printer) (value: string) segmentStart segmentEnd =
                let segmentLength = segmentEnd - segmentStart

                if segmentLength > 0 then
                    let segment = value.Substring(segmentStart, segmentLength)

                    let subSegments =
                        System.Text.RegularExpressions.Regex.Split(segment, @"\r?\n")

                    for i = 1 to subSegments.Length do
                        let subSegment =
                            // Remove whitespace in front of new lines,
                            // indent will be automatically applied
                            if printer.Column = 0 then
                                subSegments.[i - 1].TrimStart()
                            else
                                subSegments.[i - 1]

                        if subSegment.Length > 0 then
                            printer.Print(subSegment)

                            if i < subSegments.Length then
                                printer.PrintNewLine()


            // Macro transformations
            // https://fable.io/docs/communicate/js-from-fable.html#Emit-when-F-is-not-enough
            let value =
                x.Value
                |> replace
                    @"\$(\d+)\.\.\."
                    (fun m ->
                        let rep = ResizeArray()
                        let i = int m.Groups.[1].Value

                        for j = i to x.Args.Length - 1 do
                            rep.Add("$" + string j)

                        String.concat ", " rep)

                |> replace
                    @"\{\{\s*\$(\d+)\s*\?(.*?)\:(.*?)\}\}"
                    (fun m ->
                        let i = int m.Groups.[1].Value

                        match x.Args.[i] with
                        | Constant (c) -> m.Groups.[2].Value
                        | _ -> m.Groups.[3].Value)

                |> replace
                    @"\{\{([^\}]*\$(\d+).*?)\}\}"
                    (fun m ->
                        let i = int m.Groups.[2].Value

                        match List.tryItem i x.Args with
                        | Some _ -> m.Groups.[1].Value
                        | None -> "")

            let matches =
                System.Text.RegularExpressions.Regex.Matches(value, @"\$\d+")

            if matches.Count > 0 then
                for i = 0 to matches.Count - 1 do
                    let m = matches.[i]

                    let segmentStart =
                        if i > 0 then
                            matches.[i - 1].Index + matches.[i - 1].Length
                        else
                            0

                    printSegment printer value segmentStart m.Index

                    let argIndex = int m.Value.[1..]

                    match List.tryItem argIndex x.Args with
                    | Some e -> printer.ComplexExpressionWithParens(e)
                    | None -> printer.Print("None")

                let lastMatch = matches.[matches.Count - 1]
                printSegment printer value (lastMatch.Index + lastMatch.Length) value.Length
            else
                printSegment printer value 0 value.Length

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
    }

    static member Create(test, body, orElse): Expression =
        {
            Test = test
            Body = body
            OrElse = orElse
        }
        |> IfExp

    interface IPrint with
        member x.Print(printer: Printer) =
            printer.Print(x.Body)
            printer.Print(" if ")
            printer.Print(x.Test)
            printer.Print(" else ")
            printer.Print(x.OrElse)

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

    static member Create(args, body): Expression = { Args = args; Body = body } |> Lambda

    interface IPrint with
        member x.Print(printer: Printer) =
            printer.Print("lambda")

            if (List.isEmpty >> not) x.Args.Args then
                printer.Print(" ")

            printer.PrintCommaSeparatedList(x.Args.Args |> List.map (fun arg -> arg :> IPrint))
            printer.Print(": ")

            printer.Print(x.Body)


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

    static member Create(elts, ?loc): Expression = { Elements = elts; Loc = loc } |> Tuple

    interface IPrint with
        member x.Print(printer: Printer) =
            printer.Print("(", ?loc = x.Loc)
            printer.PrintCommaSeparatedList(x.Elements)

            if x.Elements.Length = 1 then
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
type List =
    {
        Elements: Expression list
    }

    static member Create(elts) = { Elements = elts }

    interface IPrint with
        member _.Print(printer) = printer.Print("(List)")

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
type Set (elts) =
    member _.Elements: Expression list = elts

    interface IPrint with
        member _.Print(printer) = printer.Print("(Set)")

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

    static member Create(keys, values): Expression = { Keys = keys; Values = values } |> Dict

    interface IPrint with
        member x.Print(printer: Printer) =
            printer.Print("{")
            printer.PrintNewLine()
            printer.PushIndentation()

            let nodes =
                List.zip x.Keys x.Values
                |> List.mapi (fun i n -> (i, n))

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

/// A variable name. id holds the name as a string, and ctx is one of the following types.
type Name =
    {
        Id: Identifier
        Context: ExpressionContext
    }

    static member Create(id, ctx): Expression = { Id = id; Context = ctx } |> Name

    interface IPrint with
        override x.Print(printer) =
            let (Identifier name) = x.Id
            printer.Print(name)
