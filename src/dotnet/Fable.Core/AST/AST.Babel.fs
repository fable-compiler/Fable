namespace Fable.AST.Babel
open Fable
open Fable.AST

/// The type field is a string representing the AST variant type.
/// Each subtype of Node is documented below with the specific string of its type field.
/// You can use this field to determine which interface a node implements.
/// The loc field represents the source location information of the node.
/// If the node contains no information about the source location, the field is null;
/// otherwise it is an object consisting of a start position (the position of the first character of the parsed source region)
/// and an end position (the position of the first character after the parsed source region):
[<AbstractClass>]
type Node(``type``, ?loc) =
    member __.``type``: string = ``type``
    member __.loc: SourceLocation option = loc

/// Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.
[<AbstractClass>] type Expression(typ, ?loc) = inherit Node(typ, ?loc = loc)

[<AbstractClass>] type Literal(typ, ?loc) = inherit Expression(typ, ?loc = loc)

[<AbstractClass>] type Statement(typ, ?loc) = inherit Node(typ, ?loc = loc)

/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
[<AbstractClass>] type Declaration(typ, ?loc) = inherit Statement(typ, ?loc = loc)

/// A module import or export declaration.
[<AbstractClass>] type ModuleDeclaration(typ, ?loc) = inherit Node(typ, ?loc = loc)

[<AbstractClass>]
type TypeAnnotationInfo(``type``) =
    member __.``type``: string = ``type``

type TypeAnnotation(typeInfo) =
    member __.``type`` = "TypeAnnotation"
    member __.typeAnnotation: TypeAnnotationInfo = typeInfo

// TODO: TypeParameter can also have `variance` and `bound` properties
type TypeParameter(name) =
    member __.``type`` = "TypeParameter"
    member __.name: string = name

type TypeParameterDeclaration(typeParams) =
    member __.``type`` = "TypeParameterDeclaration"
    member __.``params``: TypeParameter list = typeParams

type TypeParameterInstantiation(typeParams) =
    member __.``type`` = "TypeParameterInstantiation"
    member __.``params``: TypeAnnotationInfo list = typeParams

type Pattern = interface end

/// Placeholder, doesn't belong to Babel specs
type EmptyExpression() =
    inherit Expression("EmptyExpression")

/// Not in Babel specs, disguised as StringLiteral
type MacroExpression(value, args, ?loc) =
    inherit Literal("StringLiteral", ?loc = loc)
    member __.value: string = value
    member __.args: Node list = args
    member __.macro = true

(** ##Template Literals *)
type TemplateElement(value: string, tail, ?loc) =
    inherit Node("TemplateElement", ?loc = loc)
    member __.tail: bool = tail
    member __.value = dict [ ("raw", value); ("cooked", value) ]

type TemplateLiteral(quasis, expressions, ?loc) =
    inherit Literal("TemplateLiteral", ?loc = loc)
    member __.quasis: TemplateElement list = quasis
    member __.expressions: Expression list = expressions

type TaggedTemplateExpression(tag, quasi, ?loc) =
    inherit Expression("TaggedTemplateExpression", ?loc = loc)
    member __.tag: Expression = tag
    member __.quasi: TemplateLiteral = quasi

(** ##Identifier *)
/// Note that an identifier may be an expression or a destructuring pattern.
type Identifier(name, ?typeAnnotation, ?loc) =
    inherit Expression("Identifier", ?loc = loc)
    member __.name: string = name
    member __.typeAnnotation: TypeAnnotation option = typeAnnotation
    interface Pattern
    override __.ToString() = __.name

(** ##Literals *)
type RegExpLiteral(pattern, flags, ?loc) =
    inherit Literal("RegExpLiteral", ?loc = loc)
    member __.pattern: string = pattern
    member __.flags =
        flags |> Seq.map (function
            | RegexGlobal -> "g"
            | RegexIgnoreCase -> "i"
            | RegexMultiline -> "m"
            | RegexSticky -> "y") |> Seq.fold (+) ""

type NullLiteral(?loc) =
    inherit Literal("NullLiteral", ?loc = loc)

type StringLiteral(value, ?loc) =
    inherit Literal("StringLiteral", ?loc = loc)
    member __.value: string = value

type BooleanLiteral(value, ?loc) =
    inherit Literal("BooleanLiteral", ?loc = loc)
    member __.value: bool = value

type NumericLiteral(value, ?loc) =
    inherit Literal("NumericLiteral", ?loc = loc)
    member __.value: float = value

(** ##Misc *)
type Decorator(value, ?loc) =
    inherit Node("Decorator", ?loc = loc)
    member __.value = value

type DirectiveLiteral(value, ?loc) =
    inherit Literal("DirectiveLiteral", ?loc = loc)
    member __.value: string = value

/// e.g. "use strict";
type Directive(value, ?loc) =
    inherit Node("Directive", ?loc = loc)
    new (str, ?loc) = Directive(DirectiveLiteral str, ?loc = loc)
    member __.value: DirectiveLiteral = value

(** ##Program *)

/// A complete program source tree.
/// Parsers must specify sourceType as "module" if the source has been parsed as an ES6 module.
/// Otherwise, sourceType must be "script".
type Program(fileName, loc, body, ?directives, ?logs, ?dependencies) =
    inherit Node("Program", loc)
    member __.sourceType = "module" // Don't use "script"
    member __.body: Choice<Statement, ModuleDeclaration> list = body
    member __.directives: Directive list = defaultArg directives []
    // Properties below don't belong to babel specs
    member __.fileName: string = fileName
    member __.logs: Map<string, string list> = defaultArg logs Map.empty
    member __.dependencies: string[] = defaultArg dependencies [||]

(** ##Statements *)
/// An expression statement, i.e., a statement consisting of a single expression.
type ExpressionStatement(expression, ?loc) =
    inherit Statement("ExpressionStatement", ?loc = loc)
    member __.expression: Expression = expression

/// A block statement, i.e., a sequence of statements surrounded by braces.
type BlockStatement(body, ?directives, ?loc) =
    inherit Statement("BlockStatement", ?loc = loc)
    member __.body: Statement list = body
    member __.directives: Directive list = defaultArg directives []

/// An empty statement, i.e., a solitary semicolon.
type EmptyStatement(?loc) =
    inherit Statement("EmptyStatement", ?loc = loc)

type DebuggerStatement(?loc) =
    inherit Statement("DebuggerStatement", ?loc = loc)

/// Statement (typically loop) prefixed with a label (for continuue and break)
type LabeledStatement(label, body, ?loc) =
    inherit Statement("LabeledStatement", ?loc = loc)
    member __.body: Statement = body
    member __.label: Identifier = label

/// Break can optionally take a label of a loop to break
type BreakStatement(?label, ?loc) =
    inherit Statement("BreakStatement", ?loc = loc)
    member __.label: Identifier option = label

/// Contineu can optionally take a label of a loop to continuue
type ContinueStatement(?label, ?loc) =
    inherit Statement("ContinueStatement", ?loc = loc)
    member __.label: Identifier option = label

// type WithStatement

(** ##Control Flow *)
type ReturnStatement(argument, ?loc) =
    inherit Statement("ReturnStatement", ?loc = loc)
    member __.argument: Expression = argument

// type LabeledStatement
// type BreakStatement
// type ContinueStatement

(** ##Choice *)
type IfStatement(test, consequent, ?alternate, ?loc) =
    inherit Statement("IfStatement", ?loc = loc)
    member __.test: Expression = test
    member __.consequent: Statement = consequent
    member __.alternate: Statement option = alternate

/// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
type SwitchCase(consequent, ?test, ?loc) =
    inherit Node("SwitchCase", ?loc = loc)
    member __.test: Expression option = test
    member __.consequent: Statement list = consequent

type SwitchStatement(discriminant, cases, ?loc) =
    inherit Statement("SwitchStatement", ?loc = loc)
    member __.discriminant: Expression = discriminant
    member __.cases: SwitchCase list = cases

(** ##Exceptions *)
type ThrowStatement(argument, ?loc) =
    inherit Statement("ThrowStatement", ?loc = loc)
    member __.argument: Expression = argument

/// A catch clause following a try block.
type CatchClause(param, body, ?loc) =
    inherit Node("CatchClause", ?loc = loc)
    member __.param: Pattern = param
    member __.body: BlockStatement = body

/// If handler is null then finalizer must be a BlockStatement.
type TryStatement(block, ?handler, ?finalizer, ?loc) =
    inherit Statement("TryStatement", ?loc = loc)
    member __.block: BlockStatement = block
    member __.handler: CatchClause option = handler
    member __.finalizer: BlockStatement option = finalizer

(** ##Declarations *)
type VariableDeclarator(id, ?init, ?loc) =
    inherit Declaration("VariableDeclarator", ?loc = loc)
    member __.id: Pattern = id
    member __.init: Expression option = init

type VariableDeclarationKind = Var | Let | Const

type VariableDeclaration(kind, declarations, ?loc) =
    inherit Declaration("VariableDeclaration", ?loc = loc)
    new (var, ?init, ?kind, ?loc) =
        VariableDeclaration(defaultArg kind Let, [VariableDeclarator(var, ?init=init, ?loc=loc)], ?loc=loc)
    member __.declarations: VariableDeclarator list = declarations
    member __.kind =
        match kind with Var -> "var" | Let -> "let" | Const -> "const"

(** ##Loops *)
type WhileStatement(test, body, ?loc) =
    inherit Statement("WhileStatement", ?loc = loc)
    member __.test: Expression = test
    member __.body: BlockStatement = body

type DoWhileStatement(body, test, ?loc) =
    inherit Statement("DoWhileStatement", ?loc = loc)
    member __.body: BlockStatement = body
    member __.test: Expression = test

type ForStatement(body, ?init, ?test, ?update, ?loc) =
    inherit Statement("ForStatement", ?loc = loc)
    member __.body: BlockStatement = body
    member __.init: Choice<VariableDeclaration, Expression> option = init
    member __.test: Expression option = test
    member __.update: Expression option = update

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
type ForInStatement(left, right, body, ?loc) =
    inherit Statement("ForInStatement", ?loc = loc)
    member __.body: BlockStatement = body
    member __.left: Choice<VariableDeclaration, Expression> = left
    member __.right: Expression = right

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
type ForOfStatement(left, right, body, ?loc) =
    inherit Statement("ForOfStatement", ?loc = loc)
    member __.body: BlockStatement = body
    member __.left: Choice<VariableDeclaration, Expression> = left
    member __.right: Expression = right

/// A function declaration. Note that id cannot be null.
type FunctionDeclaration(id, ``params``, body, ?generator, ?async, ?loc) =
    inherit Declaration("FunctionDeclaration", ?loc = loc)
    member __.id: Identifier = id
    member __.``params``: Pattern list = ``params``
    member __.body: BlockStatement = body
    member __.generator = defaultArg generator false
    member __.async = defaultArg async false

(** ##Expressions *)

/// A super pseudo-expression.
type Super(?loc) =
    inherit Expression("Super", ?loc = loc)

type ThisExpression(?loc) =
    inherit Expression("ThisExpression", ?loc = loc)

/// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
type ArrowFunctionExpression(``params``, body, ?async, ?loc) =
    inherit Expression("ArrowFunctionExpression", ?loc = loc)
    member __.expression =
        match body with Choice1Of2 _ -> false | Choice2Of2 _ -> true
    member __.``params``: Pattern list = ``params``
    member __.body: Choice<BlockStatement, Expression> = body
    member __.async: bool = defaultArg async false

type FunctionExpression(``params``, body, ?generator, ?async, ?id, ?loc) =
    inherit Expression("FunctionExpression", ?loc = loc)
    member __.id: Identifier option = id
    member __.``params``: Pattern list = ``params``
    member __.body: BlockStatement = body
    member __.generator: bool = defaultArg generator false
    member __.async: bool = defaultArg async false

/// e.g., x = do { var t = f(); t * t + 1 };
/// http://wiki.ecmascript.org/doku.php?id=strawman:do_expressions
/// Doesn't seem to work well with block-scoped variables (let, const)
type DoExpression(body, ?loc) =
    inherit Expression("DoExpression", ?loc = loc)
    member __.body: BlockStatement = body

type YieldExpression(argument, ``delegate``, ?loc) =
    inherit Expression("YieldExpression", ?loc = loc)
    member __.argument: Expression option = argument
    /// Delegates to another generator? (yield*)
    member __.``delegate``: bool = ``delegate``

type AwaitExpression(argument, ?loc) =
    inherit Expression("AwaitExpression", ?loc = loc)
    member __.argument: Expression option = argument

type RestProperty(argument, ?loc) =
    inherit Node("RestProperty", ?loc = loc)
    member __.argument: Expression = argument

/// e.g., var z = { x: 1, ...y } // Copy all properties from y
type SpreadProperty(argument, ?loc) =
    inherit Node("SpreadProperty", ?loc = loc)
    member __.argument: Expression = argument

type SpreadElement(argument, ?loc) =
    inherit Node("SpreadElement", ?loc = loc)
    member __.argument: Expression = argument

type ArrayExpression(elements, ?loc) =
    inherit Expression("ArrayExpression", ?loc = loc)
    member __.elements: Choice<Expression, SpreadElement> option list = elements

[<AbstractClass>]
type ObjectMember(typ, key, ?value, ?computed, ?loc) =
    inherit Node(typ, ?loc = loc)
    member __.key: Expression = key
    member __.value: Expression option = value
    member __.computed: bool = defaultArg computed false
    // member __.decorators: Decorator list = defaultArg decorators []

type ObjectProperty(key, value, ?shorthand, ?computed, ?loc) =
    inherit ObjectMember("ObjectProperty", key, value, ?computed=computed, ?loc=loc)
    member __.shorthand: bool = defaultArg shorthand false

type ObjectMethodKind = ObjectGetter | ObjectSetter | ObjectMeth

type ObjectMethod(kind, key, ``params``, body, ?computed, ?generator, ?async, ?loc) =
    inherit ObjectMember("ObjectMethod", key, ?computed=computed, ?loc=loc)
    member __.kind =
        match kind with
        | ObjectGetter -> "get"
        | ObjectSetter -> "set"
        | ObjectMeth -> "method"
    member __.``params``: Pattern list = ``params``
    member __.body: BlockStatement = body
    member __.generator: bool = defaultArg generator false
    member __.async: bool = defaultArg async false

/// If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression.
/// If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
type MemberExpression(``object``, property, ?computed, ?loc) =
    inherit Expression("MemberExpression", ?loc = loc)
    member __.``object``: Expression = ``object``
    member __.property: Expression = property
    member __.computed: bool = defaultArg computed false
    interface Pattern

type ObjectExpression(properties, ?loc) =
    inherit Expression("ObjectExpression", ?loc = loc)
    member __.properties: Choice<ObjectProperty, ObjectMethod, SpreadProperty> list = properties

/// A conditional expression, i.e., a ternary ?/: expression.
type ConditionalExpression(test, consequent, alternate, ?loc) =
    inherit Expression("ConditionalExpression", ?loc = loc)
    member __.test: Expression = test
    member __.consequent: Expression = consequent
    member __.alternate: Expression = alternate

/// A function or method call expression.
type CallExpression(callee, arguments, ?loc) =
    inherit Expression("CallExpression", ?loc = loc)
    member __.callee: Expression = callee
    member __.arguments: Choice<Expression, SpreadElement> list = arguments

type NewExpression(callee, arguments, ?loc) =
    inherit Expression("NewExpression", ?loc = loc)
    member __.callee: Expression = callee
    member __.arguments: Choice<Expression, SpreadElement> list = arguments

/// A comma-separated sequence of expressions.
type SequenceExpression(expressions, ?loc) =
    inherit Expression("SequenceExpression", ?loc = loc)
    member __.expressions: Expression list = expressions

(** ##Unary Operations *)
type UnaryExpression(operator, argument, ?prefix, ?loc) =
    inherit Expression("UnaryExpression", ?loc = loc)
    member __.prefix: bool = defaultArg prefix true
    member __.argument: Expression = argument
    member __.operator =
        match operator with
        | UnaryMinus -> "-"
        | UnaryPlus -> "+"
        | UnaryNot -> "!"
        | UnaryNotBitwise -> "~"
        | UnaryTypeof -> "typeof"
        | UnaryVoid -> "void"
        | UnaryDelete -> "delete"

type UpdateExpression(operator, prefix, argument, ?loc) =
    inherit Expression("UpdateExpression", ?loc = loc)
    member __.prefix: bool = prefix
    member __.argument: Expression = argument
    member __.operator =
        match operator with
        | UpdateMinus -> "--"
        | UpdatePlus -> "++"

(** ##Binary Operations *)
type BinaryExpression(operator, left, right, ?loc) =
    inherit Expression("BinaryExpression", ?loc = loc)
    member __.left: Expression = left
    member __.right: Expression = right
    member __.operator =
        match operator with
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

type AssignmentExpression(operator, left, right, ?loc) =
    inherit Expression("AssignmentExpression", ?loc = loc)
    member __.left: Expression = left
    member __.right: Expression = right
    member __.operator =
        match operator with
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

type LogicalExpression(operator, left, right, ?loc) =
    inherit Expression("LogicalExpression", ?loc = loc)
    member __.left: Expression = left
    member __.right: Expression = right
    member __.operator =
        match operator with
        | LogicalOr -> "||"
        | LogicalAnd-> "&&"


(** ##Patterns *)
// type AssignmentProperty(key, value, ?loc) =
//     inherit ObjectProperty("AssignmentProperty", ?loc = loc)
//     member __.value: Pattern = value

// type ObjectPattern(properties, ?loc) =
//     inherit Node("ObjectPattern", ?loc = loc)
//     member __.properties: Choice<AssignmentProperty, RestProperty> list = properties
//     interface Pattern

type ArrayPattern(elements, ?loc) =
    inherit Node("ArrayPattern", ?loc = loc)
    member __.elements: Pattern option list = elements
    interface Pattern

type AssignmentPattern(left, right, ?loc) =
    inherit Node("AssignmentPattern", ?loc = loc)
    member __.left: Pattern = left
    member __.right: Expression = right
    interface Pattern

type RestElement(argument, ?loc) =
    inherit Node("RestElement", ?loc = loc)
    member __.argument: Pattern = argument
    interface Pattern

(** ##Classes *)
type ClassMethodKind =
    | ClassConstructor | ClassFunction | ClassGetter | ClassSetter

type ClassMethod(kind, key, ``params``, body, computed, ``static``, ?loc) =
    inherit Node("ClassMethod", ?loc = loc)
    member __.kind =
        match kind with
        | ClassConstructor -> "constructor"
        | ClassGetter -> "get"
        | ClassSetter -> "set"
        | ClassFunction -> "method"
    member __.key: Expression = key
    member __.``params``: Pattern list = ``params``
    member __.body: BlockStatement = body
    member __.computed: bool = computed
    member __.``static``: bool = ``static``
    // member __.decorators: Decorator list = defaultArg decorators []
    // This appears in astexplorer.net but it's not documented
    // member __.expression: bool = false

/// ES Class Fields & Static Properties
/// https://github.com/jeffmo/es-class-fields-and-static-properties
/// e.g, class MyClass { static myStaticProp = 5; myProp /* = 10 */; }
type ClassProperty(key, ?value, ?typeAnnotation, ?loc) =
    inherit Node("ClassProperty", ?loc = loc)
    member __.key: Identifier = key
    member __.value: Expression option = value
    member __.typeAnnotation: TypeAnnotation option = typeAnnotation

type ClassBody(body, ?loc) =
    inherit Node("ClassBody", ?loc = loc)
    member __.body: Choice<ClassMethod, ClassProperty> list = body

type ClassDeclaration(body, id, ?super, ?typeParams, ?loc) =
    inherit Declaration("ClassDeclaration", ?loc = loc)
    member __.body: ClassBody = body
    member __.id: Identifier = id
    member __.superClass: Expression option = super
    member __.typeParameters: TypeParameterDeclaration option = typeParams
    // member __.decorators: Decorator list = defaultArg decorators []

/// Anonymous class: e.g., var myClass = class { }
type ClassExpression(body, ?id, ?super, ?typeParams, ?loc) =
    inherit Expression("ClassExpression", ?loc = loc)
    member __.body: ClassBody = body
    member __.id: Identifier option = id
    member __.superClass: Expression option = super
    member __.typeParameters: TypeParameterDeclaration option = typeParams
    // member __.decorators: Decorator list = defaultArg decorators []

// type MetaProperty(meta, property, ?loc) =
//     inherit Expression("MetaProperty", ?loc = loc)
//     member __.meta: Identifier = meta
//     member __.property: Expression = property

(** ##Modules *)
/// A specifier in an import or export declaration.
[<AbstractClass>]
type ModuleSpecifier(typ, local, ?loc) =
    inherit Node(typ, ?loc = loc)
    member __.local: Identifier = local

/// An imported variable binding, e.g., {foo} in import {foo} from "mod" or {foo as bar} in import {foo as bar} from "mod".
/// The imported field refers to the name of the export imported from the module.
/// The local field refers to the binding imported into the local module scope.
/// If it is a basic named import, such as in import {foo} from "mod", both imported and local are equivalent Identifier nodes; in this case an Identifier node representing foo.
/// If it is an aliased import, such as in import {foo as bar} from "mod", the imported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ImportSpecifier(local, imported, ?loc) =
    inherit ModuleSpecifier("ImportSpecifier", local, ?loc = loc)
    member __.imported: Identifier = imported

/// A default import specifier, e.g., foo in import foo from "mod".
type ImportDefaultSpecifier(local, ?loc) =
    inherit ModuleSpecifier("ImportDefaultSpecifier", local, ?loc = loc)

/// A namespace import specifier, e.g., * as foo in import * as foo from "mod".
type ImportNamespaceSpecifier(local, ?loc) =
    inherit ModuleSpecifier("ImportNamespaceSpecifier", local, ?loc = loc)

/// e.g., import foo from "mod";.
type ImportDeclaration(specifiers, source, ?loc) =
    inherit ModuleDeclaration("ImportDeclaration", ?loc = loc)
    member __.specifiers: Choice<ImportSpecifier, ImportDefaultSpecifier, ImportNamespaceSpecifier> list = specifiers
    member __.source: Literal = source

/// An exported variable binding, e.g., {foo} in export {foo} or {bar as foo} in export {bar as foo}.
/// The exported field refers to the name exported in the module.
/// The local field refers to the binding into the local module scope.
/// If it is a basic named export, such as in export {foo}, both exported and local are equivalent Identifier nodes;
/// in this case an Identifier node representing foo. If it is an aliased export, such as in export {bar as foo},
/// the exported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ExportSpecifier(local, exported, ?loc) =
    inherit ModuleSpecifier("ExportSpecifier", local, ?loc = loc)
    member __.exported: Identifier = exported

/// An export named declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
/// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
type ExportNamedDeclaration(?declaration, ?specifiers, ?source, ?loc) =
    inherit ModuleDeclaration("ExportNamedDeclaration", ?loc = loc)
    member __.declaration: Declaration option = declaration
    member __.specifiers: ExportSpecifier list = defaultArg specifiers []
    member __.source: Literal option = source

/// An export default declaration, e.g., export default function () {}; or export default 1;.
type ExportDefaultDeclaration(declaration, ?loc) =
    inherit ModuleDeclaration("ExportDefaultDeclaration", ?loc = loc)
    member __.declaration: Choice<Declaration, Expression> = declaration

/// An export batch declaration, e.g., export * from "mod";.
type ExportAllDeclaration(source, ?loc) =
    inherit ModuleDeclaration("ExportAllDeclaration", ?loc = loc)
    member __.source: Literal = source
