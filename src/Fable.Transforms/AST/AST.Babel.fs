namespace rec Fable.AST.Babel

open Fable
open Fable.Core
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

module PrinterExtensions =
    type Printer with
        member printer.PrintPattern(pattern: Pattern) =
            match pattern with
            | U2.Case1 p -> p.Print(printer)
            | U2.Case2 p -> p.Print(printer)

        member printer.PrintBlock(nodes: 'a array, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit, ?newLineAtEnd) =
            let newLineAtEnd = defaultArg newLineAtEnd true
            printer.Print("{")
            printer.PrintNewLine()
            printer.PushIndentation()
            for node in nodes do
                printNode printer node
                printSeparator printer
            printer.PopIndentation()
            printer.Print("}")
            if newLineAtEnd then
                printer.PrintNewLine()

        member printer.PrintBlock(nodes: Statement array) =
            let printStatement printer (s: Statement) =
                match s with
                | :? ExpressionStatement as e ->
                    match e.Expression with
                    | :? NullLiteral -> ()
                    // TODO: Check the argument has no side-effect?
                    | :? UnaryExpression as e when e.Operator = "void" -> ()
                    // Constructors of classes deriving from System.Object add an empty object at the end
                    | :? ObjectExpression as o when o.Properties.Length = 0 -> ()
                    | _ -> e.Print(printer)
                | _ -> s.Print(printer)

            printer.PrintBlock(nodes, printStatement, (fun p ->
                if printer.Column > 0 then
                    printer.Print(";")
                    printer.PrintNewLine()
            ))

        member printer.PrintOptional(before: string, node: #Node option) =
            match node with
            | None-> ()
            | Some node ->
                printer.Print(before)
                node.Print(printer)

        member printer.PrintOptional(node: #Node option) =
            match node with
            | None-> ()
            | Some node -> node.Print(printer)

        member printer.PrintArray(nodes: 'a array, printNode: Printer -> 'a -> unit, printSeparator: Printer -> unit) =
            for i = 0 to nodes.Length - 1 do
                printNode printer nodes.[i]
                if i < nodes.Length - 1 then
                    printSeparator printer

        member printer.PrintCommaSeparatedArray(nodes: #Node array) =
            printer.PrintArray(nodes, (fun p x -> x.Print(printer)), (fun p -> printer.Print(", ")))

        member printer.PrintCommaSeparatedArray(nodes: Pattern array) =
            printer.PrintArray(nodes, (fun p x -> printer.PrintPattern(x)), (fun p -> printer.Print(", ")))

        // TODO: (super) type parameters, implements
        member printer.PrintClass(id: Identifier option, superClass: Expression option, body: ClassBody, loc) =
            printer.Print("class", ?loc=loc)
            printer.PrintOptional(" ", id)
            printer.PrintOptional(" extends ", superClass)
            printer.Print(" ")
            body.Print(printer)

        // TODO: type annotations
        member printer.PrintFunction(id: Identifier option, ``params``: Pattern array, body: BlockStatement, loc) =
            printer.Print("function ", ?loc=loc)
            printer.PrintOptional(id)
            printer.Print("(")
            printer.PrintCommaSeparatedArray(``params``)
            printer.Print(") ")
            body.Print(printer)

        member printer.MaybeWithParens(expr: Expression) =
            match expr with
            | :? PatternExpression as e -> e.Print(printer)
            | :? Literal as e -> e.Print(printer)
            | :? CallExpression as e -> e.Print(printer)
            | :? Super as e -> e.Print(printer)
            | _ ->
                printer.Print("(")
                expr.Print(printer)
                printer.Print(")")

        member printer.PrintOperation(left, operator, right, loc) =
            printer.AddLocation(loc)
            printer.MaybeWithParens(left)
            printer.Print(" " + operator + " ")
            printer.MaybeWithParens(right)

/// The type field is a string representing the AST variant type.
/// Each subtype of Node is documented below with the specific string of its type field.
/// You can use this field to determine which interface a node implements.
/// The loc field represents the source location information of the node.
/// If the node contains no information about the source location, the field is null;
/// otherwise it is an object consisting of a start position (the position of the first character of the parsed source region)
/// and an end position (the position of the first character after the parsed source region):
[<AbstractClass>]
type Node(``type``, ?loc) =
    member __.Type: string = ``type``
    member __.Loc: SourceLocation option = loc
    abstract Print: Printer -> unit
//    // TODO: Temporary solution to make everything compile,
//    // remove before merging PR
//    default _.Print(printer) = printer.Print(sprintf "TODO: %s" ``type``)

/// Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.
[<AbstractClass>] type Expression(``type``, ?loc) = inherit Node(``type``, ?loc = loc)

[<AbstractClass>] type PatternNode(``type``, ?loc) = inherit Node(``type``, ?loc = loc)
[<AbstractClass>] type PatternExpression(``type``, ?loc) = inherit Expression(``type``, ?loc = loc)

type Pattern = U2<PatternNode, PatternExpression>

[<AbstractClass>] type Literal(``type``, ?loc) = inherit Expression(``type``, ?loc = loc)

[<AbstractClass>] type Statement(``type``, ?loc) = inherit Node(``type``, ?loc = loc)

/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
[<AbstractClass>] type Declaration(``type``, ?loc) = inherit Statement(``type``, ?loc = loc)

/// A module import or export declaration.
[<AbstractClass>] type ModuleDeclaration(``type``, ?loc) = inherit Node(``type``, ?loc = loc)

/// Not in Babel specs, disguised as StringLiteral
type MacroExpression(value, args, ?loc) =
    inherit Literal("StringLiteral", ?loc = loc)
    let macro = true
    member __.Value: string = value
    member __.Args: Expression array = args
    member __.Macro: bool = macro
    override _.Print(printer) =
        printer.AddLocation(loc)

        let inline replace pattern (f: System.Text.RegularExpressions.Match -> string) input =
            System.Text.RegularExpressions.Regex.Replace(input, pattern, f)

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

        let printSegment (printer: Printer) (value: string) segmentStart segmentEnd =
            let segmentLength = segmentEnd - segmentStart
            if segmentLength > 0 then
                let segment = value.Substring(segmentStart, segmentLength)
                let subSegments = System.Text.RegularExpressions.Regex.Split(segment, @"\r?\n")
                for i = 1 to subSegments.Length do
                    printer.Print(subSegments.[i - 1])
                    if i < subSegments.Length then
                        printer.PrintNewLine()

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
                    | Some e -> printer.MaybeWithParens(e)
                    | None -> printer.Print("(void \"missing\")")

                let lastMatch = matches.[matches.Count - 1]
                printSegment printer value (lastMatch.Index + lastMatch.Length) value.Length

        else
            printer.Print(value)

// Template Literals
//type TemplateElement(value: string, tail, ?loc) =
//    inherit Node("TemplateElement", ?loc = loc)
//    member __.Tail: bool = tail
//    member __.Value = dict [ ("raw", value); ("cooked", value) ]
//
//type TemplateLiteral(quasis, expressions, ?loc) =
//    inherit Literal("TemplateLiteral", ?loc = loc)
//    member __.Quasis: TemplateElement array = quasis
//    member __.Expressions: Expression array = expressions
//
//type TaggedTemplateExpression(tag, quasi, ?loc) =
//    inherit Expression("TaggedTemplateExpression", ?loc = loc)
//    member __.Tag: Expression = tag
//    member __.Quasi: TemplateLiteral = quasi

// Identifier
/// Note that an identifier may be an expression or a destructuring pattern.
type Identifier(name, ?optional, ?typeAnnotation, ?loc) =
    inherit PatternExpression("Identifier", ?loc = loc)
    member __.Name: string = name
    member __.Optional: bool option = optional
    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation
    override __.ToString() = __.Name
    override _.Print(printer) =
        // TODO: optional, type annotation
        printer.Print(name, ?loc=loc)

// Literals
type RegExpLiteral(pattern, flags_, ?loc) =
    inherit Literal("RegExpLiteral", ?loc = loc)
    let flags =
        flags_ |> Seq.map (function
            | RegexGlobal -> "g"
            | RegexIgnoreCase -> "i"
            | RegexMultiline -> "m"
            | RegexSticky -> "y") |> Seq.fold (+) ""
    member __.Pattern: string = pattern
    member __.Flags: string = flags
    override _.Print(printer) =
        printer.Print("/", ?loc=loc)
        printer.Print(pattern)
        printer.Print("/")
        printer.Print(flags)

type Undefined(?loc) =
    inherit Identifier("undefined", ?loc = loc)
    // TODO: Use `void 0` instead? Just remove this node?
    override _.Print(printer) =
        printer.Print("undefined", ?loc=loc)

type NullLiteral(?loc) =
    inherit Literal("NullLiteral", ?loc = loc)
    override _.Print(printer) =
        printer.Print("null", ?loc=loc)

type StringLiteral(value, ?loc) =
    inherit Literal("StringLiteral", ?loc = loc)
    member __.Value: string = value
    override _.Print(printer) =
        printer.Print("\"", ?loc=loc)
        printer.Print(System.Web.HttpUtility.JavaScriptStringEncode(value))
        printer.Print("\"")

type BooleanLiteral(value, ?loc) =
    inherit Literal("BooleanLiteral", ?loc = loc)
    member __.Value: bool = value
    override _.Print(printer) =
        printer.Print((if value then "true" else "false"), ?loc=loc)

type NumericLiteral(value, ?loc) =
    inherit Literal("NumericLiteral", ?loc = loc)
    member __.Value: float = value
    override _.Print(printer) =
        printer.Print(value.ToString(), ?loc=loc)

// Misc
//type Decorator(value, ?loc) =
//    inherit Node("Decorator", ?loc = loc)
//    member __.Value = value
//
type DirectiveLiteral(value, ?loc) =
    inherit Literal("DirectiveLiteral", ?loc = loc)
    member __.Value: string = value
    override _.Print(_) = failwith "not implemented"

/// e.g. "use strict";
type Directive(value, ?loc) =
    inherit Node("Directive", ?loc = loc)
    new (str, ?loc) = Directive(DirectiveLiteral str, ?loc = loc)
    member __.Value: DirectiveLiteral = value
    override _.Print(_) = failwith "not implemented"

// Program

/// A complete program source tree.
/// Parsers must specify sourceType as "module" if the source has been parsed as an ES6 module.
/// Otherwise, sourceType must be "script".
type Program(fileName, body, ?logs_, ?dependencies_, ?sourceFiles_) = // ?directives_,
    inherit Node("Program")
    let sourceType = "module" // Don't use "script"
    let logs = defaultArg logs_ Map.empty
    let sourceFiles = defaultArg sourceFiles_ [||]
    let dependencies = defaultArg dependencies_ [||]
    let directives = [||] // defaultArg directives_ [||]
    member __.Directives: Directive array = directives
    member __.SourceType: string = sourceType
    member __.Body: U2<Statement, ModuleDeclaration> array = body
    // Properties below don't belong to babel specs
    member __.FileName: string = fileName
    member __.Logs: Map<string, string array> = logs
    member __.Dependencies: string[] = dependencies
    member __.SourceFiles: string[] = sourceFiles
    override _.Print(_) =
        failwith "Program cannot be printed directly"

// Statements
/// An expression statement, i.e., a statement consisting of a single expression.
type ExpressionStatement(expression, ?loc) =
    inherit Statement("ExpressionStatement", ?loc = loc)
    member __.Expression: Expression = expression
    override _.Print(printer) =
        expression.Print(printer)

/// A block statement, i.e., a sequence of statements surrounded by braces.
type BlockStatement(body, ?loc) = // ?directives_,
    inherit Statement("BlockStatement", ?loc = loc)
    let directives = [||] // defaultArg directives_ [||]
    member __.Directives: Directive array = directives
    member __.Body: Statement array = body
    override _.Print(printer) =
        printer.PrintBlock(body)

/// An empty statement, i.e., a solitary semicolon.
//type EmptyStatement(?loc) =
//    inherit Statement("EmptyStatement", ?loc = loc)
//    override _.Print(_) = ()

type DebuggerStatement(?loc) =
    inherit Statement("DebuggerStatement", ?loc = loc)
    override _.Print(printer) =
        printer.Print("debugger", ?loc=loc)

/// Statement (typically loop) prefixed with a label (for continue and break)
type LabeledStatement(label, body, ?loc) =
    inherit Statement("LabeledStatement", ?loc = loc)
    member __.Body: Statement = body
    member __.Label: Identifier = label
    override _.Print(printer) =
        label.Print(printer)
        printer.Print(":")
        printer.PrintNewLine()
        // Don't push indent
        body.Print(printer)

/// Break can optionally take a label of a loop to break
type BreakStatement(?label, ?loc) =
    inherit Statement("BreakStatement", ?loc = loc)
    member __.Label: Identifier option = label
    override _.Print(printer) =
        printer.Print("break", ?loc=loc)

/// Continue can optionally take a label of a loop to continue
type ContinueStatement(?label, ?loc) =
    inherit Statement("ContinueStatement", ?loc = loc)
    member __.Label: Identifier option = label
    override _.Print(printer) =
        printer.Print("continue", ?loc=loc)
        printer.PrintOptional(" ", label)

// type WithStatement

// Control Flow
type ReturnStatement(argument, ?loc) =
    inherit Statement("ReturnStatement", ?loc = loc)
    member __.Argument: Expression = argument
    override _.Print(printer) =
        printer.Print("return ", ?loc=loc)
        argument.Print(printer)

type IfStatement(test, consequent, ?alternate, ?loc) =
    inherit Statement("IfStatement", ?loc = loc)
    member __.Test: Expression = test
    member __.Consequent: Statement = consequent
    member __.Alternate: Statement option = alternate
    override _.Print(printer) =
        printer.Print("if (", ?loc=loc)
        test.Print(printer)
        printer.Print(") ")
        consequent.Print(printer)
        // TODO: Remove else clauses if they become empty after removing null statements (see PrintBlock)
        printer.PrintOptional((if printer.Column > 0 then " else " else "else "), alternate)
        // If the consequent/alternate is a block
        // a new line should already be printed
        if printer.Column > 0 then
            printer.PrintNewLine()

/// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
type SwitchCase(consequent, ?test, ?loc) =
    inherit Node("SwitchCase", ?loc = loc)
    member __.Test: Expression option = test
    member __.Consequent: Statement array = consequent
    override _.Print(printer) =
        printer.AddLocation(loc)
        match test with
        | None -> printer.Print("default:")
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
    inherit Statement("SwitchStatement", ?loc = loc)
    member __.Discriminant: Expression = discriminant
    member __.Cases: SwitchCase array = cases
    override _.Print(printer) =
        printer.Print("switch (", ?loc=loc)
        discriminant.Print(printer)
        printer.Print(") ")
        printer.PrintBlock(cases, (fun p x -> x.Print(p)), fun _ -> ())

// Exceptions
type ThrowStatement(argument, ?loc) =
    inherit Statement("ThrowStatement", ?loc = loc)
    member __.Argument: Expression = argument
    override _.Print(printer) =
        printer.Print("throw ", ?loc=loc)
        argument.Print(printer)

/// A catch clause following a try block.
type CatchClause(param, body, ?loc) =
    inherit Node("CatchClause", ?loc = loc)
    member __.Param: Pattern = param
    member __.Body: BlockStatement = body
    override _.Print(printer) =
        // "catch" is being printed by TryStatement
        printer.Print("(", ?loc=loc)
        printer.PrintPattern(param)
        printer.Print(") ")
        body.Print(printer)

/// If handler is null then finalizer must be a BlockStatement.
type TryStatement(block, ?handler, ?finalizer, ?loc) =
    inherit Statement("TryStatement", ?loc = loc)
    member __.Block: BlockStatement = block
    member __.Handler: CatchClause option = handler
    member __.Finalizer: BlockStatement option = finalizer
    override _.Print(printer) =
        printer.Print("try ", ?loc=loc)
        block.Print(printer)
        printer.PrintOptional("catch ", handler)
        printer.PrintOptional("finally ", finalizer)

// Declarations
type VariableDeclarator(id, ?init, ?loc) =
    inherit Node("VariableDeclarator", ?loc = loc)
    member __.Id: Pattern = id
    member __.Init: Expression option = init
    override _.Print(printer) =
        printer.AddLocation(loc)
        printer.PrintPattern(id)
        printer.PrintOptional(" = ", init)

type VariableDeclarationKind = Var | Let | Const

type VariableDeclaration(kind_, declarations, ?loc) =
    inherit Declaration("VariableDeclaration", ?loc = loc)
    let kind = match kind_ with Var -> "var" | Let -> "let" | Const -> "const"
    new (var, ?init, ?kind, ?loc) =
        VariableDeclaration(defaultArg kind Let, [|VariableDeclarator(var, ?init=init, ?loc=loc)|], ?loc=loc)
    member __.Declarations: VariableDeclarator array = declarations
    member __.Kind: string = kind
    override _.Print(printer) =
        printer.Print(kind + " ", ?loc=loc)
        printer.PrintCommaSeparatedArray(declarations)

// Loops
type WhileStatement(test, body, ?loc) =
    inherit Statement("WhileStatement", ?loc = loc)
    member __.Test: Expression = test
    member __.Body: BlockStatement = body
    override _.Print(printer) =
        printer.Print("while (", ?loc=loc)
        test.Print(printer)
        printer.Print(") ")
        body.Print(printer)

//type DoWhileStatement(body, test, ?loc) =
//    inherit Statement("DoWhileStatement", ?loc = loc)
//    member __.Body: BlockStatement = body
//    member __.Test: Expression = test

type ForStatement(body, ?init, ?test, ?update, ?loc) =
    inherit Statement("ForStatement", ?loc = loc)
    member __.Body: BlockStatement = body
    member __.Init: U2<VariableDeclaration, Expression> option = init
    member __.Test: Expression option = test
    member __.Update: Expression option = update
    override _.Print(printer) =
        printer.Print("for (", ?loc=loc)
        match init with
        | None -> ()
        | Some(U2.Case1 x) -> x.Print(printer)
        | Some(U2.Case2 x) -> x.Print(printer)
        printer.Print("; ")
        printer.PrintOptional(test)
        printer.Print("; ")
        printer.PrintOptional(update)
        printer.Print(") ")
        body.Print(printer)

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
//type ForInStatement(left, right, body, ?loc) =
//    inherit Statement("ForInStatement", ?loc = loc)
//    member __.Body: BlockStatement = body
//    member __.Left: U2<VariableDeclaration, Expression> = left
//    member __.Right: Expression = right

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
//type ForOfStatement(left, right, body, ?loc) =
//    inherit Statement("ForOfStatement", ?loc = loc)
//    member __.Body: BlockStatement = body
//    member __.Left: U2<VariableDeclaration, Expression> = left
//    member __.Right: Expression = right

/// A function declaration. Note that id cannot be null.
type FunctionDeclaration(``params``, body, id, ?returnType, ?typeParameters, ?loc) = // ?async_, ?generator_, ?declare,
    inherit Declaration("FunctionDeclaration", ?loc = loc)
//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member __.Async: bool = async
//    member __.Generator: bool = generator
//    member __.Declare: bool option = declare
    member __.Params: Pattern array = ``params``
    member __.Body: BlockStatement = body
    member __.Id: Identifier = id
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    override _.Print(printer) =
        printer.PrintFunction(Some id, ``params``, body, loc)

// Expressions

/// A super pseudo-expression.
type Super(?loc) =
    inherit Expression("Super", ?loc = loc)
    override _.Print(printer) =
        printer.Print("super", ?loc=loc)

type ThisExpression(?loc) =
    inherit Expression("ThisExpression", ?loc = loc)
    override _.Print(printer) =
        printer.Print("this", ?loc=loc)

/// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
type ArrowFunctionExpression(``params``, body, ?returnType, ?typeParameters, ?loc) = //?async_, ?generator_,
    inherit Expression("ArrowFunctionExpression", ?loc = loc)
//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member __.Async: bool = async
//    member __.Generator: bool = generator
    let expression = Some (match body with U2.Case1 _ -> false | U2.Case2 _ -> true)
    member __.Params: Pattern array = ``params``
    member __.Body: U2<BlockStatement, Expression> = body
    member __.Expression: bool option = expression
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    override _.Print(printer) =
        // TODO: type annotations
        // TODO: Remove parens if we only have one argument (and no annotation)
        printer.Print("(", ?loc=loc)
        printer.PrintCommaSeparatedArray(``params``)
        printer.Print(") => ")
        match body with
        | U2.Case1 block -> block.Print(printer)
        | U2.Case2 expr -> expr.Print(printer)

type FunctionExpression(``params``, body, ?id, ?returnType, ?typeParameters, ?loc) = //?generator_, ?async_
    inherit Expression("FunctionExpression", ?loc = loc)
//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member __.Async: bool = async
//    member __.Generator: bool = generator
    member __.Id: Identifier option = id
    member __.Params: Pattern array = ``params``
    member __.Body: BlockStatement = body
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    override _.Print(printer) =
        printer.PrintFunction(id, ``params``, body, loc)

///// e.g., x = do { var t = f(); t * t + 1 };
///// http://wiki.ecmascript.org/doku.php?id=strawman:do_expressions
///// Doesn't seem to work well with block-scoped variables (let, const)
//type DoExpression(body, ?loc) =
//    inherit Expression("DoExpression", ?loc = loc)
//    member __.Body: BlockStatement = body

//type YieldExpression(argument, ``delegate``, ?loc) =
//    inherit Expression("YieldExpression", ?loc = loc)
//    member __.Argument: Expression option = argument
//    /// Delegates to another generator? (yield*)
//    member __.Delegate: bool = ``delegate``
//
//type AwaitExpression(argument, ?loc) =
//    inherit Expression("AwaitExpression", ?loc = loc)
//    member __.Argument: Expression option = argument

//type RestProperty(argument, ?loc) =
//    inherit Node("RestProperty", ?loc = loc)
//    member __.Argument: Expression = argument

///// e.g., var z = { x: 1, ...y } // Copy all properties from y
//type SpreadProperty(argument, ?loc) =
//    inherit Node("SpreadProperty", ?loc = loc)
//    member __.Argument: Expression = argument

// Should derive from Node, but make it an expression for simplicity
type SpreadElement(argument, ?loc) =
    inherit Expression("SpreadElement", ?loc = loc)
    member __.Argument: Expression = argument
    override _.Print(printer) =
        printer.Print("...", ?loc=loc)
        argument.Print(printer)

type ArrayExpression(elements, ?loc) =
    inherit Expression("ArrayExpression", ?loc = loc)
    // member __.Elements: U2<Expression, SpreadElement> option array = elements
    member __.Elements: Expression array = elements
    override _.Print(printer) =
        printer.Print("[", ?loc=loc)
        printer.PrintCommaSeparatedArray(elements)
        printer.Print("]")

type ObjectProperty(key, value, ?computed_, ?loc) = // ?shorthand_,
    inherit Node("ObjectProperty", ?loc = loc)
    let computed = defaultArg computed_ false
//    let shorthand = defaultArg shorthand_ false
//    member __.Shorthand: bool = shorthand
    member __.Key: Expression = key
    member __.Value: Expression = value
    member __.Computed: bool = computed
    override _.Print(printer) =
        if computed then
            printer.Print("[")
            key.Print(printer)
            printer.Print("]: ")
        else
            key.Print(printer)
            printer.Print(": ")
        value.Print(printer)

type ObjectMethodKind = ObjectGetter | ObjectSetter | ObjectMeth

type ObjectMethod(kind_, key, ``params``, body, ?computed_, ?returnType, ?typeParameters, ?loc) = // ?async_, ?generator_,
    inherit Node("ObjectMethod", ?loc = loc)
    let kind =
        match kind_ with
        | ObjectGetter -> "get"
        | ObjectSetter -> "set"
        | ObjectMeth -> "method"
    let computed = defaultArg computed_ false
//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member __.Async: bool = async
//    member __.Generator: bool = generator
    member __.Kind: string = kind
    member __.Key: Expression = key
    member __.Params: Pattern array = ``params``
    member __.Body: BlockStatement = body
    member __.Computed: bool = computed
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    override _.Print(printer) =
        printer.AddLocation(loc)

        if kind <> "method" then
            printer.Print(kind + " ")

        if computed then
            printer.Print("[")
            key.Print(printer)
            printer.Print("]")
        else
            key.Print(printer)

        // TODO: type annotations
        printer.Print("(")
        printer.PrintCommaSeparatedArray(``params``)
        printer.Print(") ")

        body.Print(printer)

/// If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression.
/// If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
type MemberExpression(object, property, ?computed_, ?loc) =
    inherit PatternExpression("MemberExpression", ?loc = loc)
    let computed = defaultArg computed_ false
    member __.Object: Expression = object
    member __.Property: Expression = property
    member __.Computed: bool = computed
    override _.Print(printer) =
        printer.AddLocation(loc)
        object.Print(printer)
        if computed then
            printer.Print("[", ?loc=loc)
            property.Print(printer)
            printer.Print("]")
        else
            printer.Print(".", ?loc=loc)
            property.Print(printer)

type ObjectExpression(properties, ?loc) =
    inherit Expression("ObjectExpression", ?loc = loc)
//    member __.Properties: U3<ObjectProperty, ObjectMethod, SpreadProperty> array = properties
    member __.Properties: U2<ObjectProperty, ObjectMethod> array = properties
    override _.Print(printer) =
        let printProp (printer: Printer) = function
            | U2.Case1 (x: ObjectProperty) -> x.Print(printer)
            | U2.Case2 (x: ObjectMethod) -> x.Print(printer)

        let printSeparator (p: Printer) =
            p.Print(",")
            p.PrintNewLine()

        printer.AddLocation(loc)
        printer.PrintBlock(properties, printProp, printSeparator, newLineAtEnd=false)

/// A conditional expression, i.e., a ternary ?/: expression.
type ConditionalExpression(test, consequent, alternate, ?loc) =
    inherit Expression("ConditionalExpression", ?loc = loc)
    member __.Test: Expression = test
    member __.Consequent: Expression = consequent
    member __.Alternate: Expression = alternate
    override _.Print(printer) =
        printer.AddLocation(loc)
        printer.MaybeWithParens(test)
        printer.Print(" ? ")
        printer.MaybeWithParens(consequent)
        printer.Print(" : ")
        printer.MaybeWithParens(alternate)

/// A function or method call expression.
type CallExpression(callee, arguments, ?loc) =
    inherit Expression("CallExpression", ?loc = loc)
    member __.Callee: Expression = callee
    // member __.Arguments: U2<Expression, SpreadElement> array = arguments
    member __.Arguments: Expression array = arguments
    override _.Print(printer) =
        printer.AddLocation(loc)
        printer.MaybeWithParens(callee)
        printer.Print("(")
        printer.PrintCommaSeparatedArray(arguments)
        printer.Print(")")

type NewExpression(callee, arguments, ?typeArguments, ?loc) =
    inherit Expression("NewExpression", ?loc = loc)
    member __.Callee: Expression = callee
    // member __.Arguments: U2<Expression, SpreadElement> array = arguments
    member __.Arguments: Expression array = arguments
    member __.TypeArguments: TypeParameterInstantiation option = typeArguments
    override _.Print(printer) =
        printer.Print("new ", ?loc=loc)
        printer.MaybeWithParens(callee)
        printer.Print("(")
        printer.PrintCommaSeparatedArray(arguments)
        printer.Print(")")

/// A comma-separated sequence of expressions.
type SequenceExpression(expressions, ?loc) =
    inherit Expression("SequenceExpression", ?loc = loc)
    member __.Expressions: Expression array = expressions
    override _.Print(printer) =
        printer.AddLocation(loc)
        printer.PrintCommaSeparatedArray(expressions)

// Unary Operations
type UnaryExpression(operator_, argument, ?loc) =
    inherit Expression("UnaryExpression", ?loc = loc)
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
    member __.Prefix: bool = prefix
    member __.Argument: Expression = argument
    member __.Operator: string = operator
    override _.Print(printer) =
        printer.AddLocation(loc)
        match operator with
        | "-" | "+" | "!" | "~" -> printer.Print(operator)
        | _ -> printer.Print(operator + " ")
        printer.MaybeWithParens(argument)

type UpdateExpression(operator_, prefix, argument, ?loc) =
    inherit Expression("UpdateExpression", ?loc = loc)
    let operator =
        match operator_ with
        | UpdateMinus -> "--"
        | UpdatePlus -> "++"
    member __.Prefix: bool = prefix
    member __.Argument: Expression = argument
    member __.Operator: string = operator
    override _.Print(printer) =
        printer.AddLocation(loc)
        if prefix then
            printer.Print(operator)
            printer.MaybeWithParens(argument)
        else
            printer.MaybeWithParens(argument)
            printer.Print(operator)

// Binary Operations
type BinaryExpression(operator_, left, right, ?loc) =
    inherit Expression("BinaryExpression", ?loc = loc)
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
    member __.Left: Expression = left
    member __.Right: Expression = right
    member __.Operator: string = operator
    override _.Print(printer) =
        printer.PrintOperation(left, operator, right, loc)

type AssignmentExpression(operator_, left, right, ?loc) =
    inherit Expression("AssignmentExpression", ?loc = loc)
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
    member __.Left: Expression = left
    member __.Right: Expression = right
    member __.Operator: string = operator
    override _.Print(printer) =
        printer.PrintOperation(left, operator, right, loc)

type LogicalExpression(operator_, left, right, ?loc) =
    inherit Expression("LogicalExpression", ?loc = loc)
    let operator =
        match operator_ with
        | LogicalOr -> "||"
        | LogicalAnd-> "&&"
    member __.Left: Expression = left
    member __.Right: Expression = right
    member __.Operator: string = operator
    override _.Print(printer) =
        printer.PrintOperation(left, operator, right, loc)

// Patterns
// type AssignmentProperty(key, value, ?loc) =
//     inherit ObjectProperty("AssignmentProperty", ?loc = loc)
//     member __.Value: Pattern = value

// type ObjectPattern(properties, ?loc) =
//     inherit Node("ObjectPattern", ?loc = loc)
//     member __.Properties: U2<AssignmentProperty, RestProperty> array = properties
//     interface Pattern

//type ArrayPattern(elements, ?typeAnnotation, ?loc) =
//    inherit PatternNode("ArrayPattern", ?loc = loc)
//    member __.Elements: Pattern option array = elements
//    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation

//type AssignmentPattern(left, right, ?typeAnnotation, ?loc) =
//    inherit PatternNode("AssignmentPattern", ?loc = loc)
//    member __.Left: Pattern = left
//    member __.Right: Expression = right
//    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation

type RestElement(argument, ?typeAnnotation, ?loc) =
    inherit PatternNode("RestElement", ?loc = loc)
    member __.Argument: Pattern = argument
    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation
    override _.Print(printer) =
        printer.Print("...", ?loc=loc)
        // TODO: Type annotation
        printer.PrintPattern(argument)

// Classes
type ClassMethodKind =
    | ClassImplicitConstructor | ClassFunction | ClassGetter | ClassSetter

type ClassMethod(kind_, key, ``params``, body, ?computed_, ?``static``, ?``abstract``, ?returnType, ?typeParameters, ?loc) =
    inherit Node("ClassMethod", ?loc = loc)
    let kind =
        match kind_ with
        | ClassImplicitConstructor -> "constructor"
        | ClassGetter -> "get"
        | ClassSetter -> "set"
        | ClassFunction -> "method"
    let computed = defaultArg computed_ false
    member __.Kind = kind
    member __.Key: Expression = key
    member __.Params: Pattern array = ``params``
    member __.Body: BlockStatement = body
    member __.Computed: bool = computed
    member __.Static: bool option = ``static``
    member __.Abstract: bool option = ``abstract``
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    // This appears in astexplorer.net but it's not documented
    // member __.Expression: bool = false
    override _.Print(printer) =
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

        // TODO: type annotations
        printer.Print("(")
        printer.PrintCommaSeparatedArray(``params``)
        printer.Print(") ")

        body.Print(printer)

/// ES Class Fields & Static Properties
/// https://github.com/jeffmo/es-class-fields-and-static-properties
/// e.g, class MyClass { static myStaticProp = 5; myProp /* = 10 */; }
type ClassProperty(key, ?value, ?``static``, ?optional, ?typeAnnotation, ?loc) =
    inherit Node("ClassProperty", ?loc = loc)
    member __.Key: U2<Identifier, StringLiteral> = key
    member __.Value: Expression option = value
    member __.Static: bool option = ``static``
    member __.Optional: bool option = optional
    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation
    override _.Print(printer) =
        printer.AddLocation(loc)
        if ``static`` = Some true then
            printer.Print("static ")
        match key with
        | U2.Case1 key -> key.Print(printer)
        | U2.Case2 key ->
            printer.Print("[")
            key.Print(printer)
            printer.Print("]")
        if optional = Some true then
            printer.Print("?")
        // TODO: Type annotation
        printer.PrintOptional(": ", value)

type ClassImplements(id, ?typeParameters, ?loc) =
    inherit Expression("ClassImplements", ?loc = loc)
    member __.Id: Identifier = id
    member __.TypeParameters: TypeParameterInstantiation option = typeParameters
    override _.Print(printer) =
        printer.Print(" implements ", ?loc=loc)
        // TODO: Type parameters
        id.Print(printer)

type ClassBody(body, ?loc) =
    inherit Node("ClassBody", ?loc = loc)
    member __.Body: U2<ClassMethod, ClassProperty> array = body
    override _.Print(printer) =
        let printMember printer (memb: U2<ClassMethod, ClassProperty>) =
            match memb with
            | U2.Case1 x -> x.Print(printer)
            | U2.Case2 x -> x.Print(printer)

        printer.AddLocation(loc)
        printer.PrintBlock(body, printMember, (fun p ->
            if printer.Column > 0 then
                printer.Print(";")
                printer.PrintNewLine()
        ))

type ClassDeclaration(body, ?id, ?superClass, ?superTypeParameters, ?typeParameters, ?implements, ?loc) =
    inherit Declaration("ClassDeclaration", ?loc = loc)
    member __.Body: ClassBody = body
    member __.Id: Identifier option = id
    member __.SuperClass: Expression option = superClass
    member __.Implements: ClassImplements array option = implements
    member __.SuperTypeParameters: TypeParameterInstantiation option = superTypeParameters
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    override _.Print(printer) =
        printer.PrintClass(id, superClass, body, loc)

/// Anonymous class: e.g., var myClass = class { }
type ClassExpression(body, ?id, ?superClass, ?superTypeParameters, ?typeParameters, ?implements, ?loc) =
    inherit Expression("ClassExpression", ?loc = loc)
    member __.Body: ClassBody = body
    member __.Id: Identifier option = id
    member __.SuperClass: Expression option = superClass
    member __.Implements: ClassImplements array option = implements
    member __.SuperTypeParameters: TypeParameterInstantiation option = superTypeParameters
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    override _.Print(printer) =
        printer.PrintClass(id, superClass, body, loc)

// type MetaProperty(meta, property, ?loc) =
//     inherit Expression("MetaProperty", ?loc = loc)
//     member __.Meta: Identifier = meta
//     member __.Property: Expression = property

// Modules
/// A specifier in an import or export declaration.
[<AbstractClass>]
type ModuleSpecifier(``type``, local, ?loc) =
    inherit Node(``type``, ?loc = loc)
    member __.Local: Identifier = local

/// An imported variable binding, e.g., {foo} in import {foo} from "mod" or {foo as bar} in import {foo as bar} from "mod".
/// The imported field refers to the name of the export imported from the module.
/// The local field refers to the binding imported into the local module scope.
/// If it is a basic named import, such as in import {foo} from "mod", both imported and local are equivalent Identifier nodes; in this case an Identifier node representing foo.
/// If it is an aliased import, such as in import {foo as bar} from "mod", the imported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ImportSpecifier(local, imported, ?loc) =
    inherit ModuleSpecifier("ImportSpecifier", local, ?loc = loc)
    member __.Imported: Identifier = imported
    override this.Print(printer) =
        // Don't print the braces, this will be done in the import declaration
        let local = this.Local
        imported.Print(printer)
        if imported.Name <> local.Name then
            printer.Print(" as ")
            local.Print(printer)

/// A default import specifier, e.g., foo in import foo from "mod".
type ImportDefaultSpecifier(local, ?loc) =
    inherit ModuleSpecifier("ImportDefaultSpecifier", local, ?loc = loc)
    override this.Print(printer) =
        this.Local.Print(printer)

/// A namespace import specifier, e.g., * as foo in import * as foo from "mod".
type ImportNamespaceSpecifier(local, ?loc) =
    inherit ModuleSpecifier("ImportNamespaceSpecifier", local, ?loc = loc)
    override this.Print(printer) =
        printer.Print("* as ")
        this.Local.Print(printer)

/// e.g., import foo from "mod";.
type ImportDeclaration(specifiers, source, ?loc) =
    inherit ModuleDeclaration("ImportDeclaration", ?loc = loc)
    member __.Specifiers: U3<ImportSpecifier, ImportDefaultSpecifier, ImportNamespaceSpecifier> array = specifiers
    member __.Source: Literal = source
    override _.Print(printer) =
        let members = specifiers |> Array.choose (function U3.Case1 x -> Some x | _ -> None)
        let defaults = specifiers|> Array.choose (function U3.Case2 x -> Some x | _ -> None)
        let namespaces = specifiers |> Array.choose (function U3.Case3 x -> Some x | _ -> None)

        printer.Print("import ", ?loc = loc)

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

        printer.Print(" from ")
        source.Print(printer)

/// An exported variable binding, e.g., {foo} in export {foo} or {bar as foo} in export {bar as foo}.
/// The exported field refers to the name exported in the module.
/// The local field refers to the binding into the local module scope.
/// If it is a basic named export, such as in export {foo}, both exported and local are equivalent Identifier nodes;
/// in this case an Identifier node representing foo. If it is an aliased export, such as in export {bar as foo},
/// the exported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ExportSpecifier(local, exported, ?loc) =
    inherit ModuleSpecifier("ExportSpecifier", local, ?loc = loc)
    member __.Exported: Identifier = exported
    override this.Print(printer) =
        // Don't print the braces, this will be done in the export declaration
        let local = this.Local
        local.Print(printer)
        if exported.Name <> local.Name then
            printer.Print(" as ")
            exported.Print(printer)

/// An export named declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
/// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
type ExportNamedDeclaration(?declaration, ?specifiers_, ?source, ?loc) =
    inherit ModuleDeclaration("ExportNamedDeclaration", ?loc = loc)
    let specifiers = defaultArg specifiers_ [||]
    member __.Declaration: Declaration option = declaration
    member __.Specifiers: ExportSpecifier array = specifiers
    member __.Source: Literal option = source
    override _.Print(printer) =
        printer.Print("export ", ?loc=loc)
        match declaration with
        | Some decl -> decl.Print(printer)
        | None ->
            printer.Print("{ ")
            printer.PrintCommaSeparatedArray(specifiers)
            printer.Print(" }")
            printer.PrintOptional(" from ", source)

/// An export default declaration, e.g., export default function () {}; or export default 1;.
type ExportDefaultDeclaration(declaration, ?loc) =
    inherit ModuleDeclaration("ExportDefaultDeclaration", ?loc = loc)
    member __.Declaration: U2<Declaration, Expression> = declaration
    override _.Print(printer) =
        printer.Print("export default ", ?loc=loc)
        match declaration with
        | U2.Case1 x -> x.Print(printer)
        | U2.Case2 x -> x.Print(printer)

/// An export batch declaration, e.g., export * from "mod";.
type ExportAllDeclaration(source, ?loc) =
    inherit ModuleDeclaration("ExportAllDeclaration", ?loc = loc)
    member __.Source: Literal = source
    override _.Print(printer) =
        printer.Print("export * from ", ?loc=loc)
        source.Print(printer)

// Type Annotations
[<AbstractClass>]
type TypeAnnotationInfo(``type``) =
    member __.Type: string = ``type``

type TypeAnnotation(typeAnnotation) =
    inherit Node("TypeAnnotation")
    member __.TypeAnnotation: TypeAnnotationInfo = typeAnnotation
    override _.Print(_) = failwith "not implemented"

type TypeParameter(name, ?bound, ?``default``) =
    inherit Node("TypeParameter")
    member __.Name: string = name
    member __.Bound: TypeAnnotation option = bound
    member __.Default: TypeAnnotationInfo option = ``default``
    override _.Print(_) = failwith "not implemented"

type TypeParameterDeclaration(``params``) =
    inherit Node("TypeParameterDeclaration")
    member __.Params: TypeParameter array = ``params``
    override _.Print(_) = failwith "not implemented"

type TypeParameterInstantiation(``params``) =
    inherit Node("TypeParameterInstantiation")
    member __.Params: TypeAnnotationInfo array = ``params``
    override _.Print(_) = failwith "not implemented"

type StringTypeAnnotation() =
    inherit TypeAnnotationInfo("StringTypeAnnotation")

type NumberTypeAnnotation() =
    inherit TypeAnnotationInfo("NumberTypeAnnotation")

type BooleanTypeAnnotation() =
    inherit TypeAnnotationInfo("BooleanTypeAnnotation")

type AnyTypeAnnotation() =
    inherit TypeAnnotationInfo("AnyTypeAnnotation")

type VoidTypeAnnotation() =
    inherit TypeAnnotationInfo("VoidTypeAnnotation")

type TupleTypeAnnotation(types) =
    inherit TypeAnnotationInfo("TupleTypeAnnotation")
    member __.Types: TypeAnnotationInfo array = types

type UnionTypeAnnotation(types) =
    inherit TypeAnnotationInfo("UnionTypeAnnotation")
    member __.Types: TypeAnnotationInfo array = types

type FunctionTypeParam(name, typeInfo, ?optional) =
    inherit Node("FunctionTypeParam")
    member __.Name: Identifier = name
    member __.TypeAnnotation: TypeAnnotationInfo = typeInfo
    member __.Optional: bool option = optional
    override _.Print(_) = failwith "not implemented"

type FunctionTypeAnnotation(``params``, returnType, ?typeParameters, ?rest) =
    inherit TypeAnnotationInfo("FunctionTypeAnnotation")
    member __.Params: FunctionTypeParam array = ``params``
    member __.ReturnType: TypeAnnotationInfo = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    member __.Rest: FunctionTypeParam option = rest

type NullableTypeAnnotation(``type``) =
    inherit TypeAnnotationInfo("NullableTypeAnnotation")
    member __.TypeAnnotation: TypeAnnotationInfo = ``type``

type GenericTypeAnnotation(id, ?typeParameters) =
    inherit TypeAnnotationInfo("GenericTypeAnnotation")
    member __.Id: Identifier = id
    member __.TypeParameters: TypeParameterInstantiation option = typeParameters

type ObjectTypeProperty(key, value, ?kind, ?``static``, ?optional, ?proto, ?method) =
    inherit Node("ObjectTypeProperty")
    member __.Key: U2<Identifier, StringLiteral> = key
    member __.Value: TypeAnnotationInfo = value
    member __.Kind: string option = kind
    member __.Static: bool option = ``static``
    member __.Optional: bool option = optional
    member __.Proto: bool option = proto
    member __.Method: bool option = method
    override _.Print(_) = failwith "not implemented"

type ObjectTypeIndexer(key, value, ?id, ?``static``) =
    inherit Node("ObjectTypeIndexer")
    member __.Id: Identifier option = id
    member __.Key: Identifier = key
    member __.Value: TypeAnnotationInfo = value
    member __.Static: bool option = ``static``
    override _.Print(_) = failwith "not implemented"

type ObjectTypeCallProperty(value, ?``static``) =
    inherit Node("ObjectTypeCallProperty")
    member __.Value: TypeAnnotationInfo = value
    member __.Static: bool option = ``static``
    override _.Print(_) = failwith "not implemented"

type ObjectTypeInternalSlot(id, value, optional, ``static``, method) =
    inherit Node("ObjectTypeInternalSlot")
    member __.Id: Identifier = id
    member __.Value: TypeAnnotationInfo = value
    member __.Optional: bool = optional
    member __.Static: bool = ``static``
    member __.Method: bool = method
    override _.Print(_) = failwith "not implemented"

type ObjectTypeAnnotation(properties, ?indexers_, ?callProperties_, ?internalSlots_, ?exact_) =
    inherit TypeAnnotationInfo("ObjectTypeAnnotation")
    let exact = defaultArg exact_ false
    let indexers = defaultArg indexers_ [||]
    let callProperties = defaultArg callProperties_ [||]
    let internalSlots = defaultArg internalSlots_ [||]
    member __.Properties: ObjectTypeProperty array = properties
    member __.Indexers: ObjectTypeIndexer array = indexers
    member __.CallProperties: ObjectTypeCallProperty array = callProperties
    member __.InternalSlots: ObjectTypeInternalSlot array = internalSlots
    member __.Exact: bool = exact

type InterfaceExtends(id, ?typeParameters) =
    inherit Node("InterfaceExtends")
    member __.Id: Identifier = id
    member __.TypeParameters: TypeParameterInstantiation option = typeParameters
    override _.Print(_) = failwith "not implemented"

type InterfaceDeclaration(id, body, ?extends_, ?typeParameters, ?implements_, ?loc) = // ?mixins_,
    inherit Declaration("InterfaceDeclaration", ?loc = loc)
    let extends = defaultArg extends_ [||]
    let implements = defaultArg implements_ [||]
//    let mixins = defaultArg mixins_ [||]
    member __.Id: Identifier = id
    member __.Body: ObjectTypeAnnotation = body
    member __.Extends: InterfaceExtends array = extends
    member __.Implements: ClassImplements array = implements
//    member __.Mixins: InterfaceExtends array = mixins
    member __.TypeParameters: TypeParameterDeclaration option = typeParameters
    override _.Print(_) = failwith "not implemented"
