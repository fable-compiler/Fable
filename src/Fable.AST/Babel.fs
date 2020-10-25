namespace rec Fable.AST.Babel

open Fable.AST
open PrinterExtensions

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

module PrinterExtensions =
    type Printer with
        member printer.Print(node: Node) =
            node.Print(printer)

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
                | :? Undefined
                | :? NullLiteral
                | :? StringLiteral
                | :? BooleanLiteral
                | :? NumericLiteral -> true
                // Constructors of classes deriving from System.Object add an empty object at the end
                | :? ObjectExpression as o -> o.Properties.Length = 0
                | :? UnaryExpression as e when e.Operator = "void" -> hasNoSideEffects e.Argument
                // Some identifiers may be stranded as the result of imports
                // intended only for side effects, see #2228
                | :? Identifier -> true
                | _ -> false

            match s with
            | :? ExpressionStatement as e -> hasNoSideEffects e.Expression |> not
            | _ -> true

        member printer.PrintProductiveStatement(s: Statement, ?printSeparator) =
            if printer.IsProductiveStatement(s) then
                s.Print(printer)
                printSeparator |> Option.iter (fun f -> f printer)

        member printer.PrintProductiveStatements(statements: Statement[]) =
            for s in statements do
                printer.PrintProductiveStatement(s, (fun p -> p.PrintStatementSeparator()))

        member printer.PrintBlock(nodes: Statement array, ?skipNewLineAtEnd) =
            printer.PrintBlock(nodes,
                               (fun p s -> p.PrintProductiveStatement(s)),
                               (fun p -> p.PrintStatementSeparator()),
                               ?skipNewLineAtEnd=skipNewLineAtEnd)

        member printer.PrintOptional(before: string, node: #Node option) =
            match node with
            | None -> ()
            | Some node ->
                printer.Print(before)
                node.Print(printer)

        member printer.PrintOptional(node: #Node option) =
            match node with
            | None -> ()
            | Some node -> node.Print(printer)

        member printer.PrintArray(nodes: 'a array, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes.[i]
                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedArray(nodes: Expression array) =
            printer.PrintArray(nodes, (fun p x -> p.SequenceExpressionWithParens(x)), (fun p -> p.Print(", ")))

        member printer.PrintCommaSeparatedArray(nodes: #Node array) =
            printer.PrintArray(nodes, (fun p x -> p.Print(x)), (fun p -> p.Print(", ")))

        // TODO: (super) type parameters, implements
        member printer.PrintClass(id: Identifier option, superClass: Expression option,
                superTypeParameters: TypeParameterInstantiation option,
                typeParameters: TypeParameterDeclaration option,
                implements: ClassImplements array option, body: ClassBody, loc) =
            printer.Print("class", ?loc=loc)
            printer.PrintOptional(" ", id)
            printer.PrintOptional(typeParameters)
            match superClass with
            | Some (:? Identifier as id) when id.TypeAnnotation.IsSome ->
                printer.Print(" extends ");
                printer.Print(id.TypeAnnotation.Value.TypeAnnotation)
            | _ -> printer.PrintOptional(" extends ", superClass)
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
                    | (:? Identifier as p), (:? Identifier as a) -> p.Name = a.Name
                    | _ -> false)

            let isDeclaration = defaultArg isDeclaration false
            let isArrow = defaultArg isArrow false

            printer.AddLocation(loc)

            // Check if we can remove the function
            let skipExpr =
                match body.Body with
                | [|:? ReturnStatement as r|] when not isDeclaration ->
                    match r.Argument with
                    | :? CallExpression as c when parameters.Length = c.Arguments.Length ->
                        // To be sure we're not running side effects when deleting the function,
                        // check the callee is an identifier (accept non-computed member expressions too?)
                        match c.Callee with
                        | :? Identifier when areEqualPassedAndAppliedArgs parameters c.Arguments ->
                            Some c.Callee
                        | _ -> None
                    | _ -> None
                | _ -> None

            match skipExpr with
            | Some e -> e.Print(printer)
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
                    | [|:? ReturnStatement as r |] ->
                        printer.ComplexExpressionWithParens(r.Argument, objExpr=true)
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
            expr.Print(printer)
            printer.Print(")")

        member printer.SequenceExpressionWithParens(expr: Expression) =
            match expr with
            | :? SequenceExpression -> printer.WithParens(expr)
            | _ -> printer.Print(expr)

        /// Surround with parens anything that can potentially conflict with operator precedence
        member printer.ComplexExpressionWithParens(expr: Expression, ?objExpr) =
            match expr with
            | :? Undefined
            | :? NullLiteral
            | :? StringLiteral
            | :? BooleanLiteral
            | :? NumericLiteral
            | :? Identifier
            | :? MemberExpression
            | :? CallExpression
            | :? ThisExpression
            | :? Super
            | :? ArrayExpression -> expr.Print(printer)
            | :? ObjectExpression ->
                match objExpr with
                | Some true -> printer.WithParens(expr)
                | _ -> expr.Print(printer)
            | _ -> printer.WithParens(expr)

        member printer.PrintOperation(left, operator, right, loc) =
            printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(left)
            printer.Print(" " + operator + " ")
            printer.ComplexExpressionWithParens(right)

/// The type field is a string representing the AST variant type.
/// Each subtype of Node is documented below with the specific string of its type field.
/// You can use this field to determine which interface a node implements.
/// The loc field represents the source location information of the node.
/// If the node contains no information about the source location, the field is null;
/// otherwise it is an object consisting of a start position (the position of the first character of the parsed source region)
/// and an end position (the position of the first character after the parsed source region):
type Node =
    abstract Print: Printer -> unit

/// Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.
type Expression =
    inherit Node
type Pattern =
    inherit Node
    abstract Name: string
type PatternExpression =
    inherit Pattern
    inherit Expression
type Literal = inherit Expression
type Statement = inherit Node
/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
type Declaration = inherit Statement
/// A module import or export declaration.
type ModuleDeclaration = inherit Node

/// Not in Babel specs
type EmitExpression(value, args, ?loc) =
    member _.Value: string = value
    member _.Args: Expression array = args
    interface Expression with
        member _.Print(printer) =
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
                    | :? BooleanLiteral as b when b.Value -> m.Groups.[2].Value
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

// Template Literals
//type TemplateElement(value: string, tail, ?loc) =
//    inherit Node("TemplateElement", ?loc = loc)
//    member _.Tail: bool = tail
//    member _.Value = dict [ ("raw", value); ("cooked", value) ]
//
//type TemplateLiteral(quasis, expressions, ?loc) =
//    inherit Literal("TemplateLiteral", ?loc = loc)
//    member _.Quasis: TemplateElement array = quasis
//    member _.Expressions: Expression array = expressions
//
//type TaggedTemplateExpression(tag, quasi, ?loc) =
//    interface Expression with
//    member _.Tag: Expression = tag
//    member _.Quasi: TemplateLiteral = quasi

// Identifier
/// Note that an identifier may be an expression or a destructuring pattern.
type Identifier(name, ?optional, ?typeAnnotation, ?loc) =
    member _.Name: string = name
    member _.Optional: bool option = optional
    member _.TypeAnnotation: TypeAnnotation option = typeAnnotation
    interface PatternExpression with
        member _.Name = name
        member _.Print(printer) =
            printer.Print(name, ?loc=loc)
            if optional = Some true then
                printer.Print("?")
            printer.PrintOptional(typeAnnotation)

// Literals
type RegExpLiteral(pattern, flags_, ?loc) =
    let flags =
        flags_ |> Seq.map (function
            | RegexGlobal -> "g"
            | RegexIgnoreCase -> "i"
            | RegexMultiline -> "m"
            | RegexSticky -> "y") |> Seq.fold (+) ""
    member _.Pattern: string = pattern
    member _.Flags: string = flags
    interface Literal with
        member _.Print(printer) =
            printer.Print("/", ?loc=loc)
            printer.Print(pattern)
            printer.Print("/")
            printer.Print(flags)

type Undefined(?loc) =
    // TODO: Use `void 0` instead? Just remove this node?
    interface Expression with
        member _.Print(printer) =
            printer.Print("undefined", ?loc=loc)

type NullLiteral(?loc) =
    interface Literal with
        member _.Print(printer) =
            printer.Print("null", ?loc=loc)

type StringLiteral(value, ?loc) =
    member _.Value: string = value
    interface Literal with
        member _.Print(printer) =
            printer.Print("\"", ?loc=loc)
            printer.Print(printer.EscapeJsStringLiteral(value))
            printer.Print("\"")

type BooleanLiteral(value, ?loc) =
    member _.Value: bool = value
    interface Literal with
        member _.Print(printer) =
            printer.Print((if value then "true" else "false"), ?loc=loc)

type NumericLiteral(value, ?loc) =
    member _.Value: float = value
    interface Literal with
        member _.Print(printer) =
            let value =
                match value.ToString(System.Globalization.CultureInfo.InvariantCulture) with
                | "∞" -> "Infinity"
                | "-∞" -> "-Infinity"
                | value -> value
            printer.Print(value, ?loc=loc)

// Misc
//type Decorator(value, ?loc) =
//    inherit Node("Decorator", ?loc = loc)
//    member _.Value = value
//
type DirectiveLiteral(value) =
    member _.Value: string = value
    interface Literal with
        member _.Print(_) = failwith "not implemented"

/// e.g. "use strict";
type Directive(value) =
    new (str) = Directive(DirectiveLiteral str)
    member _.Value: DirectiveLiteral = value
    interface Node with
        member _.Print(_) = failwith "not implemented"

// Program

/// A complete program source tree.
/// Parsers must specify sourceType as "module" if the source has been parsed as an ES6 module.
/// Otherwise, sourceType must be "script".
type Program(body) = // ?directives_,
//    let sourceType = "module" // Don't use "script"
//    member _.Directives: Directive array = directives
//    member _.SourceType: string = sourceType
    member _.Body: ModuleDeclaration array = body

// Statements
/// An expression statement, i.e., a statement consisting of a single expression.
type ExpressionStatement(expression) =
    member _.Expression: Expression = expression
    interface Statement with
        member _.Print(printer) =
            expression.Print(printer)

/// A block statement, i.e., a sequence of statements surrounded by braces.
type BlockStatement(body) = // ?directives_,
//    let directives = [||] // defaultArg directives_ [||]
//    member _.Directives: Directive array = directives
    member _.Body: Statement array = body
    interface Statement with
        member _.Print(printer) =
            printer.PrintBlock(body)

/// An empty statement, i.e., a solitary semicolon.
//type EmptyStatement(?loc) =
//    inherit Statement("EmptyStatement", ?loc = loc)
//    member _.Print(_) = ()

type DebuggerStatement(?loc) =
    interface Statement with
        member _.Print(printer) =
            printer.Print("debugger", ?loc=loc)

/// Statement (typically loop) prefixed with a label (for continue and break)
type LabeledStatement(label, body) =
    member _.Body: Statement = body
    member _.Label: Identifier = label
    interface Statement with
        member _.Print(printer) =
            printer.Print(label)
            printer.Print(":")
            printer.PrintNewLine()
            // Don't push indent
            printer.Print(body)

/// Break can optionally take a label of a loop to break
type BreakStatement(?label, ?loc) =
    member _.Label: Identifier option = label
    interface Statement with
        member _.Print(printer) =
            printer.Print("break", ?loc=loc)

/// Continue can optionally take a label of a loop to continue
type ContinueStatement(?label, ?loc) =
    member _.Label: Identifier option = label
    interface Statement with
        member _.Print(printer) =
            printer.Print("continue", ?loc=loc)
            printer.PrintOptional(" ", label)

// type WithStatement

// Control Flow
type ReturnStatement(argument, ?loc) =
    member _.Argument: Expression = argument
    interface Statement with
        member _.Print(printer) =
            printer.Print("return ", ?loc=loc)
            argument.Print(printer)

type IfStatement(test, consequent, ?alternate, ?loc) =
    member _.Test: Expression = test
    member _.Consequent: BlockStatement = consequent
    member _.Alternate: Statement option = alternate
    interface Statement with
        member _.Print(printer) =
            printer.AddLocation(loc)
            printer.Print("if (", ?loc=loc)
            test.Print(printer)
            printer.Print(") ")
            printer.Print(consequent)
            match alternate with
            | None -> ()
            | Some alternate ->
                if printer.Column > 0 then printer.Print(" ")
                match alternate with
                | :? IfStatement ->
                    printer.Print("else ")
                    printer.Print(alternate)
                | alternate ->
                    let statements =
                        match alternate with
                        | :? BlockStatement as b -> b.Body
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
type SwitchCase(consequent, ?test, ?loc) =
    member _.Test: Expression option = test
    member _.Consequent: Statement array = consequent
    interface Node with
        member _.Print(printer) =
            printer.AddLocation(loc)
            match test with
            | None -> printer.Print("default")
            | Some test ->
                printer.Print("case ")
                test.Print(printer)
            printer.Print(":")
            match consequent.Length with
            | 0 -> printer.PrintNewLine()
            | 1 ->
                printer.Print(" ")
                consequent.[0].Print(printer)
            | _ ->
                printer.Print(" ")
                printer.PrintBlock(consequent)

type SwitchStatement(discriminant, cases, ?loc) =
    member _.Discriminant: Expression = discriminant
    member _.Cases: SwitchCase array = cases
    interface Statement with
        member _.Print(printer) =
            printer.Print("switch (", ?loc=loc)
            discriminant.Print(printer)
            printer.Print(") ")
            printer.PrintBlock(cases, (fun p x -> p.Print(x)), fun _ -> ())

// Exceptions
type ThrowStatement(argument, ?loc) =
    member _.Argument: Expression = argument
    interface Statement with
        member _.Print(printer) =
            printer.Print("throw ", ?loc=loc)
            argument.Print(printer)

/// A catch clause following a try block.
type CatchClause(param, body, ?loc) =
    member _.Param: Pattern = param
    member _.Body: BlockStatement = body
    interface Node with
        member _.Print(printer) =
            // "catch" is being printed by TryStatement
            printer.Print("(", ?loc=loc)
            param.Print(printer)
            printer.Print(") ")
            printer.Print(body)

/// If handler is null then finalizer must be a BlockStatement.
type TryStatement(block, ?handler, ?finalizer, ?loc) =
    member _.Block: BlockStatement = block
    member _.Handler: CatchClause option = handler
    member _.Finalizer: BlockStatement option = finalizer
    interface Statement with
        member _.Print(printer) =
            printer.Print("try ", ?loc=loc)
            printer.Print(block)
            printer.PrintOptional("catch ", handler)
            printer.PrintOptional("finally ", finalizer)

// Declarations
type VariableDeclarator(id, ?init) =
    member _.Id: Pattern = id
    member _.Init: Expression option = init

type VariableDeclarationKind = Var | Let | Const

type VariableDeclaration(kind_, declarations, ?loc) =
    let kind = match kind_ with Var -> "var" | Let -> "let" | Const -> "const"
    new (var, ?init, ?kind, ?loc) =
        VariableDeclaration(defaultArg kind Let, [|VariableDeclarator(var, ?init=init)|], ?loc=loc)
    member _.Declarations: VariableDeclarator array = declarations
    member _.Kind: string = kind
    interface Declaration with
        member _.Print(printer) =
            printer.Print(kind + " ", ?loc=loc)
            let canConflict = declarations.Length > 1
            for i = 0 to declarations.Length - 1 do
                let decl = declarations.[i]
                printer.Print(decl.Id)
                match decl.Init with
                | None -> ()
                | Some e ->
                    printer.Print(" = ")
                    if canConflict then printer.ComplexExpressionWithParens(e)
                    else printer.SequenceExpressionWithParens(e)
                if i < declarations.Length - 1 then
                    printer.Print(", ")

// Loops
type WhileStatement(test, body, ?loc) =
    member _.Test: Expression = test
    member _.Body: BlockStatement = body
    interface Statement with
        member _.Print(printer) =
            printer.Print("while (", ?loc=loc)
            test.Print(printer)
            printer.Print(") ")
            printer.Print(body)

//type DoWhileStatement(body, test, ?loc) =
//    inherit Statement("DoWhileStatement", ?loc = loc)
//    member _.Body: BlockStatement = body
//    member _.Test: Expression = test

type ForStatement(body, ?init, ?test, ?update, ?loc) =
    member _.Body: BlockStatement = body
    // In JS this can be an expression too
    member _.Init: VariableDeclaration option = init
    member _.Test: Expression option = test
    member _.Update: Expression option = update
    interface Statement with
        member _.Print(printer) =
            printer.Print("for (", ?loc=loc)
            printer.PrintOptional(init)
            printer.Print("; ")
            printer.PrintOptional(test)
            printer.Print("; ")
            printer.PrintOptional(update)
            printer.Print(") ")
            printer.Print(body)

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
//type ForInStatement(left, right, body, ?loc) =
//    inherit Statement("ForInStatement", ?loc = loc)
//    member _.Body: BlockStatement = body
//    member _.Left: Choice<VariableDeclaration, Expression> = left
//    member _.Right: Expression = right

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
//type ForOfStatement(left, right, body, ?loc) =
//    inherit Statement("ForOfStatement", ?loc = loc)
//    member _.Body: BlockStatement = body
//    member _.Left: Choice<VariableDeclaration, Expression> = left
//    member _.Right: Expression = right

/// A function declaration. Note that id cannot be null.
type FunctionDeclaration(``params``, body, id, ?returnType, ?typeParameters, ?loc) = // ?async_, ?generator_, ?declare,
//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator
//    member _.Declare: bool option = declare
    member _.Params: Pattern array = ``params``
    member _.Body: BlockStatement = body
    member _.Id: Identifier = id
    member _.ReturnType: TypeAnnotation option = returnType
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    interface Declaration with
        member _.Print(printer) =
            printer.PrintFunction(Some id, ``params``, body, typeParameters, returnType, loc, isDeclaration=true)
            printer.PrintNewLine()

// Expressions

/// A super pseudo-expression.
type Super(?loc) =
    interface Expression with
        member _.Print(printer) =
            printer.Print("super", ?loc=loc)

type ThisExpression(?loc) =
    interface Expression with
        member _.Print(printer) =
            printer.Print("this", ?loc=loc)

/// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
type ArrowFunctionExpression(``params``, body: BlockStatement, ?returnType, ?typeParameters, ?loc) = //?async_, ?generator_,
    new (``params``, body: Expression, ?returnType, ?typeParameters, ?loc) =
        let body = BlockStatement [|ReturnStatement body|]
        ArrowFunctionExpression(``params``, body, ?returnType=returnType, ?typeParameters=typeParameters, ?loc=loc)
//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator
    member _.Params: Pattern array = ``params``
    member _.Body: BlockStatement = body
    member _.ReturnType: TypeAnnotation option = returnType
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    interface Expression with
        member _.Print(printer) =
            printer.PrintFunction(None, ``params``, body, typeParameters, returnType, loc, isArrow=true)

type FunctionExpression(``params``, body, ?id, ?returnType, ?typeParameters, ?loc) = //?generator_, ?async_
//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator
    member _.Id: Identifier option = id
    member _.Params: Pattern array = ``params``
    member _.Body: BlockStatement = body
    member _.ReturnType: TypeAnnotation option = returnType
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    interface Expression with
        member _.Print(printer) =
            printer.PrintFunction(id, ``params``, body, typeParameters, returnType, loc)

///// e.g., x = do { var t = f(); t * t + 1 };
///// http://wiki.ecmascript.org/doku.php?id=strawman:do_expressions
///// Doesn't seem to work well with block-scoped variables (let, const)
//type DoExpression(body, ?loc) =
//    interface Expression with
//    member _.Body: BlockStatement = body

//type YieldExpression(argument, ``delegate``, ?loc) =
//    interface Expression with
//    member _.Argument: Expression option = argument
//    /// Delegates to another generator? (yield*)
//    member _.Delegate: bool = ``delegate``
//
//type AwaitExpression(argument, ?loc) =
//    interface Expression with
//    member _.Argument: Expression option = argument

//type RestProperty(argument, ?loc) =
//    inherit Node("RestProperty", ?loc = loc)
//    member _.Argument: Expression = argument

///// e.g., var z = { x: 1, ...y } // Copy all properties from y
//type SpreadProperty(argument, ?loc) =
//    inherit Node("SpreadProperty", ?loc = loc)
//    member _.Argument: Expression = argument

// Should derive from Node, but make it an expression for simplicity
type SpreadElement(argument, ?loc) =
    member _.Argument: Expression = argument
    interface Expression with
        member _.Print(printer) =
            printer.Print("...", ?loc=loc)
            argument.Print(printer)

type ArrayExpression(elements, ?loc) =
    // member _.Elements: Choice<Expression, SpreadElement> option array = elements
    member _.Elements: Expression array = elements
    interface Expression with
        member _.Print(printer) =
            printer.Print("[", ?loc=loc)
            printer.PrintCommaSeparatedArray(elements)
            printer.Print("]")

type ObjectMember = inherit Node

type ObjectProperty(key, value, ?computed_) = // ?shorthand_,
    let computed = defaultArg computed_ false
//    let shorthand = defaultArg shorthand_ false
//    member _.Shorthand: bool = shorthand
    member _.Key: Expression = key
    member _.Value: Expression = value
    member _.Computed: bool = computed
    interface ObjectMember with
        member _.Print(printer) =
            if computed then
                printer.Print("[")
                key.Print(printer)
                printer.Print("]")
            else
                printer.Print(key)
            printer.Print(": ")
            printer.SequenceExpressionWithParens(value)

type ObjectMethodKind = ObjectGetter | ObjectSetter | ObjectMeth

type ObjectMethod(kind_, key, ``params``, body, ?computed_, ?returnType, ?typeParameters, ?loc) = // ?async_, ?generator_,
    let kind =
        match kind_ with
        | ObjectGetter -> "get"
        | ObjectSetter -> "set"
        | ObjectMeth -> "method"
    let computed = defaultArg computed_ false
//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator
    member _.Kind: string = kind
    member _.Key: Expression = key
    member _.Params: Pattern array = ``params``
    member _.Body: BlockStatement = body
    member _.Computed: bool = computed
    member _.ReturnType: TypeAnnotation option = returnType
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    interface ObjectMember with
        member _.Print(printer) =
            printer.AddLocation(loc)

            if kind <> "method" then
                printer.Print(kind + " ")

            if computed then
                printer.Print("[")
                key.Print(printer)
                printer.Print("]")
            else
                key.Print(printer)

            printer.PrintOptional(typeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(")")
            printer.PrintOptional(returnType)
            printer.Print(" ")

            printer.PrintBlock(body.Body, skipNewLineAtEnd=true)

/// If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression.
/// If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
type MemberExpression(object, property, ?computed_, ?loc) =
    let computed = defaultArg computed_ false
    member _.Object: Expression = object
    member _.Property: Expression = property
    member _.Computed: bool = computed
    interface PatternExpression with
        member _.Name =
            match property with
            | :? Identifier as i -> i.Name
            | _ -> ""
        member _.Print(printer) =
            printer.AddLocation(loc)
            match object with
            | :? NumericLiteral -> printer.WithParens(object)
            | _ -> printer.ComplexExpressionWithParens(object)
            if computed then
                printer.Print("[")
                property.Print(printer)
                printer.Print("]")
            else
                printer.Print(".")
                property.Print(printer)

type ObjectExpression(properties, ?loc) =
    member _.Properties: ObjectMember array = properties
    interface Expression with
        member _.Print(printer) =
            let printSeparator (p: Printer) =
                p.Print(",")
                p.PrintNewLine()

            printer.AddLocation(loc)
            if Array.isEmpty properties then printer.Print("{}")
            else printer.PrintBlock(properties, (fun p x -> p.Print(x)), printSeparator, skipNewLineAtEnd=true)

/// A conditional expression, i.e., a ternary ?/: expression.
type ConditionalExpression(test, consequent, alternate, ?loc) =
    member _.Test: Expression = test
    member _.Consequent: Expression = consequent
    member _.Alternate: Expression = alternate
    interface Expression with
        member _.Print(printer) =
            printer.AddLocation(loc)
            match test with
            // TODO: Move this optimization to Fable2Babel as with IfStatement?
            | :? BooleanLiteral as b ->
                if b.Value then printer.Print(consequent)
                else printer.Print(alternate)
            | _ ->
                printer.ComplexExpressionWithParens(test)
                printer.Print(" ? ")
                printer.ComplexExpressionWithParens(consequent)
                printer.Print(" : ")
                printer.ComplexExpressionWithParens(alternate)

/// A function or method call expression.
type CallExpression(callee, arguments, ?loc) =
    member _.Callee: Expression = callee
    // member _.Arguments: Choice<Expression, SpreadElement> array = arguments
    member _.Arguments: Expression array = arguments
    interface Expression with
        member _.Print(printer) =
            printer.AddLocation(loc)
            printer.ComplexExpressionWithParens(callee)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(arguments)
            printer.Print(")")

type NewExpression(callee, arguments, ?typeArguments, ?loc) =
    member _.Callee: Expression = callee
    // member _.Arguments: Choice<Expression, SpreadElement> array = arguments
    member _.Arguments: Expression array = arguments
    member _.TypeArguments: TypeParameterInstantiation option = typeArguments
    interface Expression with
        member _.Print(printer) =
            printer.Print("new ", ?loc=loc)
            printer.ComplexExpressionWithParens(callee)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(arguments)
            printer.Print(")")

/// A comma-separated sequence of expressions.
type SequenceExpression(expressions, ?loc) =
    member _.Expressions: Expression array = expressions
    interface Expression with
        member _.Print(printer) =
            printer.AddLocation(loc)
            printer.PrintCommaSeparatedArray(expressions)

// Unary Operations
type UnaryExpression(operator_, argument, ?loc) =
    let prefix = true
    let operator =
        match operator_ with
        | UnaryMinus -> "-"
        | UnaryPlus -> "+"
        | UnaryNot -> "!"
        | UnaryNotBitwise -> "~"
        | UnaryTypeof -> "typeof"
        | UnaryVoid -> "void"
        | UnaryDelete -> "delete"
    member _.Prefix: bool = prefix
    member _.Argument: Expression = argument
    member _.Operator: string = operator
    interface Expression with
        member _.Print(printer) =
            printer.AddLocation(loc)
            match operator with
            | "-" | "+" | "!" | "~" -> printer.Print(operator)
            | _ -> printer.Print(operator + " ")
            printer.ComplexExpressionWithParens(argument)

type UpdateExpression(operator_, prefix, argument, ?loc) =
    let operator =
        match operator_ with
        | UpdateMinus -> "--"
        | UpdatePlus -> "++"
    member _.Prefix: bool = prefix
    member _.Argument: Expression = argument
    member _.Operator: string = operator
    interface Expression with
        member _.Print(printer) =
            printer.AddLocation(loc)
            if prefix then
                printer.Print(operator)
                printer.ComplexExpressionWithParens(argument)
            else
                printer.ComplexExpressionWithParens(argument)
                printer.Print(operator)

// Binary Operations
type BinaryExpression(operator_, left, right, ?loc) =
    let operator =
        match operator_ with
        | BinaryEqual -> "=="
        | BinaryUnequal -> "!="
        | BinaryEqualStrict -> "==="
        | BinaryUnequalStrict -> "!=="
        | BinaryLess -> "<"
        | BinaryLessOrEqual -> "<="
        | BinaryGreater -> ">"
        | BinaryGreaterOrEqual -> ">="
        | BinaryShiftLeft -> "<<"
        | BinaryShiftRightSignPropagating -> ">>"
        | BinaryShiftRightZeroFill -> ">>>"
        | BinaryMinus -> "-"
        | BinaryPlus -> "+"
        | BinaryMultiply -> "*"
        | BinaryDivide -> "/"
        | BinaryModulus -> "%"
        | BinaryExponent -> "**"
        | BinaryOrBitwise -> "|"
        | BinaryXorBitwise -> "^"
        | BinaryAndBitwise -> "&"
        | BinaryIn -> "in"
        | BinaryInstanceOf -> "instanceof"
    member _.Left: Expression = left
    member _.Right: Expression = right
    member _.Operator: string = operator
    interface Expression with
        member _.Print(printer) =
            printer.PrintOperation(left, operator, right, loc)

type AssignmentExpression(operator_, left, right, ?loc) =
    let operator =
        match operator_ with
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
    member _.Left: Expression = left
    member _.Right: Expression = right
    member _.Operator: string = operator
    interface Expression with
        member _.Print(printer) =
            printer.PrintOperation(left, operator, right, loc)

type LogicalExpression(operator_, left, right, ?loc) =
    let operator =
        match operator_ with
        | LogicalOr -> "||"
        | LogicalAnd-> "&&"
    member _.Left: Expression = left
    member _.Right: Expression = right
    member _.Operator: string = operator
    interface Expression with
        member _.Print(printer) =
            printer.PrintOperation(left, operator, right, loc)

// Patterns
// type AssignmentProperty(key, value, ?loc) =
//     inherit ObjectProperty("AssignmentProperty", ?loc = loc)
//     member _.Value: Pattern = value

// type ObjectPattern(properties, ?loc) =
//     inherit Node("ObjectPattern", ?loc = loc)
//     member _.Properties: Choice<AssignmentProperty, RestProperty> array = properties
//     interface Pattern

//type ArrayPattern(elements, ?typeAnnotation, ?loc) =
//    inherit Pattern("ArrayPattern", ?loc = loc)
//    member _.Elements: Pattern option array = elements
//    member _.TypeAnnotation: TypeAnnotation option = typeAnnotation

//type AssignmentPattern(left, right, ?typeAnnotation, ?loc) =
//    inherit Pattern("AssignmentPattern", ?loc = loc)
//    member _.Left: Pattern = left
//    member _.Right: Expression = right
//    member _.TypeAnnotation: TypeAnnotation option = typeAnnotation

type RestElement(argument, ?typeAnnotation, ?loc) =
    member _.Argument: Pattern = argument
    member _.TypeAnnotation: TypeAnnotation option = typeAnnotation
    interface Pattern with
        member _.Name = argument.Name
        member _.Print(printer) =
            printer.Print("...", ?loc=loc)
            argument.Print(printer)
            printer.PrintOptional(typeAnnotation)

// Classes
type ClassMember = inherit Node

type ClassMethodKind =
    | ClassImplicitConstructor | ClassFunction | ClassGetter | ClassSetter

type ClassMethod(kind_, key, ``params``, body, ?computed_, ?``static``, ?``abstract``, ?returnType, ?typeParameters, ?loc) =
    let kind =
        match kind_ with
        | ClassImplicitConstructor -> "constructor"
        | ClassGetter -> "get"
        | ClassSetter -> "set"
        | ClassFunction -> "method"
    let computed = defaultArg computed_ false
    member _.Kind = kind
    member _.Key: Expression = key
    member _.Params: Pattern array = ``params``
    member _.Body: BlockStatement = body
    member _.Computed: bool = computed
    member _.Static: bool option = ``static``
    member _.Abstract: bool option = ``abstract``
    member _.ReturnType: TypeAnnotation option = returnType
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    // This appears in astexplorer.net but it's not documented
    // member _.Expression: bool = false
    interface ClassMember with
        member _.Print(printer) =
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
                key.Print(printer)
                printer.Print("]")
            else
                key.Print(printer)

            printer.PrintOptional(typeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(")")
            printer.PrintOptional(returnType)
            printer.Print(" ")

            printer.Print(body)

/// ES Class Fields & Static Properties
/// https://github.com/jeffmo/es-class-fields-and-static-properties
/// e.g, class MyClass { static myStaticProp = 5; myProp /* = 10 */; }
type ClassProperty(key, ?value, ?computed_, ?``static``, ?optional, ?typeAnnotation, ?loc) =
    let computed = defaultArg computed_ false
    member _.Key: Expression = key
    member _.Value: Expression option = value
    member _.Computed = computed
    member _.Static = defaultArg ``static`` false
    member _.Optional = defaultArg optional false
    member _.TypeAnnotation: TypeAnnotation option = typeAnnotation
    interface ClassMember with
        member _.Print(printer) =
            printer.AddLocation(loc)
            if ``static`` = Some true then
                printer.Print("static ")
            if computed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)
            if optional = Some true then
                printer.Print("?")
            printer.PrintOptional(typeAnnotation)
            printer.PrintOptional(": ", value)

type ClassImplements(id, ?typeParameters) =
    member _.Id: Identifier = id
    member _.TypeParameters: TypeParameterInstantiation option = typeParameters
    interface Expression with
        member _.Print(printer) =
            printer.Print(id)
            printer.PrintOptional(typeParameters)

type ClassBody(body, ?loc) =
    member _.Body: ClassMember array = body
    interface Node with
        member _.Print(printer) =
            printer.AddLocation(loc)
            printer.PrintBlock(body, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))

type ClassDeclaration(body, ?id, ?superClass, ?superTypeParameters, ?typeParameters, ?implements, ?loc) =
    member _.Body: ClassBody = body
    member _.Id: Identifier option = id
    member _.SuperClass: Expression option = superClass
    member _.Implements: ClassImplements array option = implements
    member _.SuperTypeParameters: TypeParameterInstantiation option = superTypeParameters
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    interface Declaration with
        member _.Print(printer) =
            printer.PrintClass(id, superClass, superTypeParameters, typeParameters, implements, body, loc)

/// Anonymous class: e.g., var myClass = class { }
type ClassExpression(body, ?id, ?superClass, ?superTypeParameters, ?typeParameters, ?implements, ?loc) =
    member _.Body: ClassBody = body
    member _.Id: Identifier option = id
    member _.SuperClass: Expression option = superClass
    member _.Implements: ClassImplements array option = implements
    member _.SuperTypeParameters: TypeParameterInstantiation option = superTypeParameters
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    interface Expression with
        member _.Print(printer) =
            printer.PrintClass(id, superClass, superTypeParameters, typeParameters, implements, body, loc)

// type MetaProperty(meta, property, ?loc) =
//     interface Expression with
//     member _.Meta: Identifier = meta
//     member _.Property: Expression = property

// Modules
type PrivateModuleDeclaration(statement) =
    member _.Statement: Statement = statement
    interface ModuleDeclaration with
        member _.Print(printer) =
            if printer.IsProductiveStatement(statement) then
                printer.Print(statement)

type ImportSpecifier = inherit Node

/// An imported variable binding, e.g., {foo} in import {foo} from "mod" or {foo as bar} in import {foo as bar} from "mod".
/// The imported field refers to the name of the export imported from the module.
/// The local field refers to the binding imported into the local module scope.
/// If it is a basic named import, such as in import {foo} from "mod", both imported and local are equivalent Identifier nodes; in this case an Identifier node representing foo.
/// If it is an aliased import, such as in import {foo as bar} from "mod", the imported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ImportMemberSpecifier(local: Identifier, imported) =
    member _.Imported: Identifier = imported
    interface ImportSpecifier with
        member this.Print(printer) =
            // Don't print the braces, this will be done in the import declaration
            printer.Print(imported)
            if imported.Name <> local.Name then
                printer.Print(" as ")
                printer.Print(local)

/// A default import specifier, e.g., foo in import foo from "mod".
type ImportDefaultSpecifier(local) =
    interface ImportSpecifier with
        member _.Print(printer) =
            printer.Print(local)

/// A namespace import specifier, e.g., * as foo in import * as foo from "mod".
type ImportNamespaceSpecifier(local) =
    interface ImportSpecifier with
        member _.Print(printer) =
            printer.Print("* as ")
            printer.Print(local)

/// e.g., import foo from "mod";.
type ImportDeclaration(specifiers, source) =
    member _.Specifiers: ImportSpecifier array = specifiers
    member _.Source: StringLiteral = source
    interface ModuleDeclaration with
        member _.Print(printer) =
            let members = specifiers |> Array.choose (function :? ImportMemberSpecifier as x -> Some x | _ -> None)
            let defaults = specifiers|> Array.choose (function :? ImportDefaultSpecifier as x -> Some x | _ -> None)
            let namespaces = specifiers |> Array.choose (function :? ImportNamespaceSpecifier as x -> Some x | _ -> None)

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
            printer.Print(printer.MakeImportPath(source.Value))
            printer.Print("\"")

/// An exported variable binding, e.g., {foo} in export {foo} or {bar as foo} in export {bar as foo}.
/// The exported field refers to the name exported in the module.
/// The local field refers to the binding into the local module scope.
/// If it is a basic named export, such as in export {foo}, both exported and local are equivalent Identifier nodes;
/// in this case an Identifier node representing foo. If it is an aliased export, such as in export {bar as foo},
/// the exported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ExportSpecifier(local: Identifier, exported) =
    member _.Exported: Identifier = exported
    interface Node with
        member _.Print(printer) =
            // Don't print the braces, this will be done in the export declaration
            printer.Print(local)
            if exported.Name <> local.Name then
                printer.Print(" as ")
                printer.Print(exported)

/// An export named declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
/// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
type ExportNamedDeclaration(declaration) =
    member _.Declaration: Declaration = declaration
    interface ModuleDeclaration with
        member _.Print(printer) =
            printer.Print("export ")
            printer.Print(declaration)

type ExportNamedReferences(specifiers, ?source) =
    member _.Specifiers: ExportSpecifier array = specifiers
    member _.Source: StringLiteral option = source
    interface ModuleDeclaration with
        member _.Print(printer) =
            printer.Print("export ")
            printer.Print("{ ")
            printer.PrintCommaSeparatedArray(specifiers)
            printer.Print(" }")
            printer.PrintOptional(" from ", source)

/// An export default declaration, e.g., export default function () {}; or export default 1;.
type ExportDefaultDeclaration(declaration) =
    member _.Declaration: Choice<Declaration, Expression> = declaration
    interface ModuleDeclaration with
        member _.Print(printer) =
            printer.Print("export default ")
            match declaration with
            | Choice1Of2 x -> x.Print(printer)
            | Choice2Of2 x -> x.Print(printer)

/// An export batch declaration, e.g., export * from "mod";.
type ExportAllDeclaration(source, ?loc) =
    member _.Source: Literal = source
    interface ModuleDeclaration with
        member _.Print(printer) =
            printer.Print("export * from ", ?loc=loc)
            source.Print(printer)

// Type Annotations
type TypeAnnotationInfo = inherit Node

type TypeAnnotation(typeAnnotation) =
    member _.TypeAnnotation: TypeAnnotationInfo = typeAnnotation
    interface Node with
        member _.Print(printer) =
            printer.Print(": ")
            printer.Print(typeAnnotation)

type TypeParameter(name, ?bound, ?``default``) =
    member _.Name: string = name
    member _.Bound: TypeAnnotation option = bound
    member _.Default: TypeAnnotationInfo option = ``default``
    interface Node with
        member _.Print(printer) =
            printer.Print(name)
            // printer.PrintOptional(bound)
            // printer.PrintOptional(``default``)

type TypeParameterDeclaration(``params``) =
    member _.Params: TypeParameter array = ``params``
    interface Node with
        member _.Print(printer) =
            printer.Print("<")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(">")

type TypeParameterInstantiation(``params``) =
    member _.Params: TypeAnnotationInfo array = ``params``
    interface Node with
        member _.Print(printer) =
            printer.Print("<")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(">")

type StringTypeAnnotation() =
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print("string")

type NumberTypeAnnotation() =
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print("number")

type BooleanTypeAnnotation() =
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print("boolean")

type AnyTypeAnnotation() =
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print("any")

type VoidTypeAnnotation() =
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print("void")

type TupleTypeAnnotation(types) =
    member _.Types: TypeAnnotationInfo array = types
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print("[")
            printer.PrintCommaSeparatedArray(types)
            printer.Print("]")

type UnionTypeAnnotation(types) =
    member _.Types: TypeAnnotationInfo array = types
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.PrintArray(types, (fun p x -> p.Print(x)), (fun p -> p.Print(" | ")))

type FunctionTypeParam(name, typeInfo, ?optional) =
    member _.Name: Identifier = name
    member _.TypeAnnotation: TypeAnnotationInfo = typeInfo
    member _.Optional: bool option = optional
    interface Node with
        member _.Print(printer) =
            printer.Print(name)
            if optional = Some true then
                printer.Print("?")
            printer.Print(": ")
            printer.Print(typeInfo)

type FunctionTypeAnnotation(``params``, returnType, ?typeParameters, ?rest) =
    member _.Params: FunctionTypeParam array = ``params``
    member _.ReturnType: TypeAnnotationInfo = returnType
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    member _.Rest: FunctionTypeParam option = rest
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.PrintOptional(typeParameters)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(``params``)
            if Option.isSome rest then
                printer.Print("...")
                printer.Print(rest.Value)
            printer.Print(") => ")
            printer.Print(returnType)

type NullableTypeAnnotation(``type``) =
    member _.TypeAnnotation: TypeAnnotationInfo = ``type``
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print(``type``)

type GenericTypeAnnotation(id, ?typeParameters) =
    member _.Id: Identifier = id
    member _.TypeParameters: TypeParameterInstantiation option = typeParameters
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print(id)
            printer.PrintOptional(typeParameters)

type ObjectTypeProperty(key, value, ?computed_, ?kind, ?``static``, ?optional, ?proto, ?method) =
    let computed = defaultArg computed_ false
    member _.Key: Expression = key
    member _.Value: TypeAnnotationInfo = value
    member _.Kind: string option = kind
    member _.Computed = computed
    member _.Static: bool option = ``static``
    member _.Optional: bool option = optional
    member _.Proto: bool option = proto
    member _.Method: bool option = method
    interface Node with
        member _.Print(printer) =
            if ``static`` = Some true then
                printer.Print("static ")
            if Option.isSome kind then
                printer.Print(kind.Value + " ")
            if computed then
                printer.Print("[")
                printer.Print(key)
                printer.Print("]")
            else
                printer.Print(key)
            if optional = Some true then
                printer.Print("?")
            // TODO: proto, method
            printer.Print(": ")
            printer.Print(value)

type ObjectTypeIndexer(key, value, ?id, ?``static``) =
    member _.Id: Identifier option = id
    member _.Key: Identifier = key
    member _.Value: TypeAnnotationInfo = value
    member _.Static: bool option = ``static``
    interface Node with
        member _.Print(_) = failwith "not implemented"

type ObjectTypeCallProperty(value, ?``static``) =
    member _.Value: TypeAnnotationInfo = value
    member _.Static: bool option = ``static``
    interface Node with
        member _.Print(_) = failwith "not implemented"

type ObjectTypeInternalSlot(id, value, optional, ``static``, method) =
    member _.Id: Identifier = id
    member _.Value: TypeAnnotationInfo = value
    member _.Optional: bool = optional
    member _.Static: bool = ``static``
    member _.Method: bool = method
    interface Node with
        member _.Print(_) = failwith "not implemented"

type ObjectTypeAnnotation(properties, ?indexers_, ?callProperties_, ?internalSlots_, ?exact_) =
    let exact = defaultArg exact_ false
    let indexers = defaultArg indexers_ [||]
    let callProperties = defaultArg callProperties_ [||]
    let internalSlots = defaultArg internalSlots_ [||]
    member _.Properties: ObjectTypeProperty array = properties
    member _.Indexers: ObjectTypeIndexer array = indexers
    member _.CallProperties: ObjectTypeCallProperty array = callProperties
    member _.InternalSlots: ObjectTypeInternalSlot array = internalSlots
    member _.Exact: bool = exact
    interface TypeAnnotationInfo with
        member _.Print(printer) =
            printer.Print("{")
            printer.PrintNewLine()
            printer.PushIndentation()
            printer.PrintArray(properties, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(indexers, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(callProperties, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintArray(internalSlots, (fun p x -> p.Print(x)), (fun p -> p.PrintStatementSeparator()))
            printer.PrintNewLine()
            printer.PopIndentation()
            printer.Print("}")
            printer.PrintNewLine()

type InterfaceExtends(id, ?typeParameters) =
    member _.Id: Identifier = id
    member _.TypeParameters: TypeParameterInstantiation option = typeParameters
    interface Node with
        member _.Print(printer) =
            printer.Print(id)
            printer.PrintOptional(typeParameters)

type InterfaceDeclaration(id, body, ?extends_, ?typeParameters, ?implements_) = // ?mixins_,
    let extends = defaultArg extends_ [||]
    let implements = defaultArg implements_ [||]
//    let mixins = defaultArg mixins_ [||]
    member _.Id: Identifier = id
    member _.Body: ObjectTypeAnnotation = body
    member _.Extends: InterfaceExtends array = extends
    member _.Implements: ClassImplements array = implements
//    member _.Mixins: InterfaceExtends array = mixins
    member _.TypeParameters: TypeParameterDeclaration option = typeParameters
    interface Declaration with
        member _.Print(printer) =
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
