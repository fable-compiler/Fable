namespace Fabel.AST.Babel
open Fabel.AST

/// The type field is a string representing the AST variant type. 
/// Each subtype of Node is documented below with the specific string of its type field. 
/// You can use this field to determine which interface a node implements.
/// The loc field represents the source location information of the node. 
/// If the node contains no information about the source location, the field is null; 
/// otherwise it is an object consisting of a start position (the position of the first character of the parsed source region) 
/// and an end position (the position of the first character after the parsed source region):
[<AbstractClass>]
type Node(typ, loc) =
    member x.``type``: string = typ
    member x.loc: SourceLocation = loc

/// Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.
[<AbstractClass>] type Expression(typ, loc) = inherit Node(typ, loc)

[<AbstractClass>] type Literal(typ, loc) = inherit Expression(typ, loc)

[<AbstractClass>] type Statement(typ, loc) = inherit Node(typ, loc)

/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
[<AbstractClass>] type Declaration(typ, loc) = inherit Statement(typ, loc)

/// A module import or export declaration.
[<AbstractClass>] type ModuleDeclaration(typ, loc) = inherit Node(typ, loc)

type Pattern = interface end

/// Placeholder, doesn't belong to Babel specs
type EmptyExpression(loc) =
    inherit Expression("EmptyExpression", loc)

(** ##Template Literals *)

// type TemplateLiteral
// type TaggedTemplateExpression
// type TemplateElement

(** ##Patterns *)

// type ObjectPattern
// type ArrayPattern
// type RestElement
// type AssignmentPattern

(** ##Identifier *)
/// Note that an identifier may be an expression or a destructuring pattern.
type Identifier(loc, name) =
    inherit Expression("Identifier", loc)
    member x.name: string = name
    interface Pattern

(** ##Literals *)
type RegExpLiteral(loc, pattern, flags) =
    inherit Literal("RegExpLiteral", loc)
    member x.pattern: string = pattern
    member x.flags =
        flags |> Seq.map (function
            | RegexGlobal -> "g"
            | RegexIgnoreCase -> "i"
            | RegexMultiline -> "m"
            | RegexSticky -> "y") |> Seq.fold (+) ""

type NullLiteral(loc) =
    inherit Literal("NullLiteral", loc)

type StringLiteral(loc, value) =
    inherit Literal("StringLiteral", loc)
    member x.value: string = value

type BooleanLiteral(loc, value) =
    inherit Literal("BooleanLiteral", loc)
    member x.value: bool = value

type NumericLiteral(loc, value) =
    inherit Literal("NumericLiteral", loc)
    member x.value: U2<int, float> = value    

(** ##Misc *)
type Decorator(loc, value) =
    inherit Node("Decorator", loc)
    member x.value = value
    
type DirectiveLiteral(loc, value) =
    inherit Literal("DirectiveLiteral", loc)
    member x.value: string = value

/// e.g. "use strict";
type Directive(loc, value) =
    inherit Node("Directive", loc)
    member x.value: DirectiveLiteral = value    

(** ##Program *)
/// A complete program source tree.
/// Parsers must specify sourceType as "module" if the source has been parsed as an ES6 module. 
/// Otherwise, sourceType must be "script".
type Program(loc, body, ?directives) =
    inherit Node("Program", loc)
    member x.sourceType = "module" // Don't use "script"
    member x.body: U2<Statement, ModuleDeclaration> list = body
    member x.directives: Directive list = defaultArg directives []

(** ##Statements *)
/// An expression statement, i.e., a statement consisting of a single expression.
type ExpressionStatement(loc, expression) =
    inherit Statement("ExpressionStatement", loc)
    member x.expression: Expression = expression

/// A block statement, i.e., a sequence of statements surrounded by braces.
type BlockStatement(loc, body, ?directives) =
    inherit Statement("BlockStatement", loc)
    member x.body: Statement list = body
    member x.directives: Directive list = defaultArg directives []

/// An empty statement, i.e., a solitary semicolon.
type EmptyStatement(loc) =
    inherit Statement("EmptyStatement", loc)

type DebuggerStatement(loc) =
    inherit Statement("DebuggerStatement", loc)
    
// type WithStatement

(** ##Control Flow *)
type ReturnStatement(loc, argument) =
    inherit Statement("ReturnStatement", loc)
    member x.argument: Expression = argument

// type LabeledStatement
// type BreakStatement
// type ContinueStatement

(** ##Choice *)
type IfStatement(loc, test, consequent, ?alternate) =
    inherit Statement("IfStatement", loc)
    member x.test: Expression = test
    member x.consequent: Statement = consequent
    member x.alternate: Statement option = alternate    

/// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
type SwitchCase(loc, consequent, ?test) =
    inherit Node("SwitchCase", loc)
    member x.test: Expression option = test
    member x.consequent: Statement list = consequent

type SwitchStatement(loc, discriminant, cases) =
    inherit Statement("SwitchStatement", loc)
    member x.discriminant: Expression = discriminant
    member x.cases: SwitchCase list = cases

(** ##Exceptions *)
type ThrowStatement(loc, argument) =
    inherit Statement("ThrowStatement", loc)
    member x.argument: Expression = argument

/// A catch clause following a try block.
type CatchClause(loc, param, body) =
    inherit Node("CatchClause", loc)
    member x.param: Pattern = param
    member x.body: BlockStatement = body

/// If handler is null then finalizer must be a BlockStatement.
type TryStatement(loc, block, ?handler, ?finalizer) =
    inherit Statement("TryStatement", loc)
    member x.block: BlockStatement = block
    member x.handler: CatchClause option = handler
    member x.finalizer: BlockStatement option = finalizer

(** ##Declarations *)
type VariableDeclarator(loc, id, ?init) =
    inherit Declaration("VariableDeclarator", loc)
    member x.id: Pattern = id
    member x.init: Expression option = init

type VariableDeclarationKind = Var | Let | Const

type VariableDeclaration(loc, kind, declarations) =
    inherit Declaration("VariableDeclaration", loc)
    new (loc, id, ?init) =
        VariableDeclaration(loc, Var, [VariableDeclarator(loc, id, ?init=init)])
    member x.declarations: VariableDeclarator list = declarations
    member x.kind =
        match kind with Var -> "var" | Let -> "let" | Const -> "const"

(** ##Loops *)
type WhileStatement(loc, test, body) =
    inherit Statement("WhileStatement", loc)
    member x.test: Expression = test
    member x.body: BlockStatement = body

type DoWhileStatement(loc, body, test) =
    inherit Statement("DoWhileStatement", loc)
    member x.body: BlockStatement = body
    member x.test: Expression = test

type ForStatement(loc, body, ?init, ?test, ?update) =
    inherit Statement("ForStatement", loc)
    member x.body: BlockStatement = body
    member x.init: U2<VariableDeclaration, Expression> option = init
    member x.test: Expression option = test
    member x.update: Expression option = update

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
type ForInStatement(loc, left, right, body) =
    inherit Statement("ForInStatement", loc)
    member x.body: BlockStatement = body
    member x.left: U2<VariableDeclaration, Expression> = left
    member x.right: Expression = right

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
type ForOfStatement(loc, left, right, body) =
    inherit Statement("ForOfStatement", loc)
    member x.body: BlockStatement = body
    member x.left: U2<VariableDeclaration, Expression> = left
    member x.right: Expression = right

/// A function declaration. Note that id cannot be null.
type FunctionDeclaration(loc, id, arguments, body, generator, async) =
    inherit Declaration("FunctionDeclaration", loc)
    member x.id: Identifier = id
    member x.``params``: Pattern list = arguments
    member x.body: BlockStatement = body
    member x.generator = generator
    member x.async = async

(** ##Expressions *)

/// A super pseudo-expression.
type Super(loc) =
    inherit Expression("Super", loc)

type ThisExpression(loc) =
    inherit Expression("ThisExpression", loc)

/// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
type ArrowFunctionExpression(loc, arguments, body, async) =
    inherit Expression("ArrowFunctionExpression", loc)
    member x.expression =
        match body with U2.Case1 _ -> false | U2.Case2 _ -> true
    member x.``params``: Pattern list = arguments
    member x.body: U2<BlockStatement, Expression> = body
    member x.async: bool = async        
        
type FunctionExpression(loc, arguments, body, ?generator, ?async, ?id) =
    inherit Expression("FunctionExpression", loc)
    member x.id: Identifier option = id
    member x.``params``: Pattern list = arguments
    member x.body: BlockStatement = body
    member x.generator: bool = defaultArg generator false
    member x.async: bool = defaultArg async false
    
/// e.g., x = do { var t = f(); t * t + 1 };
/// http://wiki.ecmascript.org/doku.php?id=strawman:do_expressions
/// Doesn't seem to work well with block-scoped variables (let, const)
type DoExpression(loc, body) =
    inherit Expression("DoExpression", loc)
    member x.body: BlockStatement = body
    
type YieldExpression(argument, ``delegate``, loc) =
    inherit Expression("YieldExpression", loc)
    member x.argument: Expression option = argument
    /// Delegates to another generator? (yield*)
    member x.``delegate``: bool = ``delegate``    

type AwaitExpression(loc, argument) =
    inherit Expression("AwaitExpression", loc)
    member x.argument: Expression option = argument

// type RestProperty(loc, argument) =
//     inherit Node("RestProperty", loc)
//     member x.argument: Expression = argument

/// e.g., var z = { x: 1, ...y } // Copy all properties from y 
type SpreadProperty(loc, argument) =
    inherit Node("SpreadProperty", loc)
    member x.argument: Expression = argument

type SpreadElement(loc, argument) =
    inherit Node("SpreadElement", loc)
    member x.argument: Expression = argument
    
type ArrayExpression(loc, elements) =
    inherit Expression("ArrayExpression", loc)
    member x.elements: U2<Expression, SpreadElement> option list = elements

[<AbstractClass>]
type ObjectMember(typ, loc, key, computed, decorators, ?value) =
    inherit Node(typ, loc)
    member x.key: Expression = key
    member x.computed: bool = computed
    member x.value: Expression option = value
    member x.decorators: Decorator list = decorators
    
type ObjectProperty(loc, shorthand, key, computed, decorators, value) =
    inherit ObjectMember("ObjectProperty", loc, key, computed, decorators, value)
    member x.shorthand: bool = shorthand

type ObjectMethodKind = ObjectGetter | ObjectSetter | ObjectMethod

type ObjectMethod(loc, kind, key, computed, decorators, arguments, body, generator, async) =
    inherit ObjectMember("ObjectMethod", loc, key, computed, decorators)
    member x.kind = match kind with ObjectGetter -> "get"
                                  | ObjectSetter -> "set"
                                  | ObjectMethod -> "method"
    member x.``params``: Pattern list = arguments
    member x.body: BlockStatement = body
    member x.generator = generator
    member x.async = async

/// If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression. 
/// If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
type MemberExpression(loc, ``object``, property, ?computed) =
    inherit Expression("MemberExpression", loc)
    member x.``object``: Expression = ``object``
    member x.property: Expression = property
    member x.computed: bool = defaultArg computed false
    interface Pattern

type ObjectExpression(loc, properties) =
    inherit Expression("ObjectExpression", loc)
    member x.properties: U3<ObjectProperty, ObjectMethod, SpreadProperty> list = properties

/// A conditional expression, i.e., a ternary ?/: expression.
type ConditionalExpression(loc, test, alternate, consequent) =
    inherit Expression("ConditionalExpression", loc)
    member x.test: Expression = test
    member x.alternate: Expression = alternate
    member x.consequent: Expression = consequent

/// A function or method call expression.  
type CallExpression(loc, callee, arguments) =
    inherit Expression("CallExpression", loc)
    member x.callee: Expression = callee
    member x.arguments: U2<Expression, SpreadElement> list = arguments

type NewExpression(loc, callee, arguments) =
    inherit Expression("NewExpression", loc)
    member x.callee: Expression = callee
    member x.arguments: U2<Expression, SpreadElement> list = arguments

/// A comma-separated sequence of expressions.
type SequenceExpression(loc, expressions) =
    inherit Expression("SequenceExpression", loc)
    member x.expressions: Expression list = expressions

(** ##Unary Operations *)
type UnaryExpression(loc, operator, argument, ?prefix) =
    inherit Expression("UnaryExpression", loc)
    member x.prefix: bool = defaultArg prefix true
    member x.argument: Expression = argument
    member x.operator =
        match operator with
        | UnaryMinus -> "-"
        | UnaryPlus -> "+"
        | UnaryNot -> "!"
        | UnaryNotBitwise -> "~"
        | UnaryTypeof -> "typeof"
        | UnaryVoid -> "void"
        | UnaryDelete -> "delete"           

type UpdateExpression(loc, operator, prefix, argument) =
    inherit Expression("UpdateExpression", loc)
    member x.prefix: bool = prefix
    member x.argument: Expression = argument
    member x.operator =
        match operator with
        | UpdateMinus -> "--"
        | UpdatePlus -> "++"
    
(** ##Binary Operations *)
type BinaryExpression(loc, operator, left, right) =
    inherit Expression("BinaryExpression", loc)
    member x.left: Expression = left
    member x.right: Expression = right
    member x.operator =
        match operator with
        | BinaryEqual -> "=="
        | BinaryUnequal -> "!="
        | BinaryEqualStrict -> "==="
        | BinaryUnequalStrict -> "!=="
        | BinaryLess -> "<"
        | BinaryLessOrEqual -> "<="
        | BinaryMore -> ">"
        | BinaryMoreOrEqual -> ">="
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

type AssignmentExpression(loc, operator, left, right) =
    inherit Expression("AssignmentExpression", loc)
    member x.left: Expression = left
    member x.right: Expression = right
    member x.operator =
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
    
type LogicalExpression(loc, operator, left, right) =
    inherit Expression("LogicalExpression", loc)
    member x.left: Expression = left
    member x.right: Expression = right
    member x.operator =
        match operator with
        | LogicalOr -> "||"
        | LogicalAnd-> "&&"

(** ##Classes *)
type ClassMethodKind =
    | ClassConstructor | ClassFunction | ClassGetter | ClassSetter

type ClassMethod(loc, kind, key, args, body, computed, ``static``, ?decorators) =
    inherit Node("ClassMethod", loc)
    member x.kind = match kind with ClassConstructor -> "constructor"
                                  | ClassGetter -> "get"
                                  | ClassSetter -> "set"
                                  | ClassFunction -> "method"
    member x.key: Expression = key
    member x.``params``: Pattern list = args
    member x.body: BlockStatement = body
    member x.computed: bool = computed
    member x.``static``: bool = ``static``
    member x.decorators: Decorator list = defaultArg decorators []
    // This appears in astexplorer.net but it's not documented
    // member x.expression: bool = false

/// ES Class Fields & Static Properties
/// https://github.com/jeffmo/es-class-fields-and-static-properties
/// e.g, class MyClass { static myStaticProp = 5; myProp /* = 10 */; }
type ClassProperty(loc, key, value) =
    inherit Node("ClassProperty", loc)
    member x.key: Identifier = key
    member x.value: Expression = value

type ClassBody(loc, body) =
    inherit Node("ClassBody", loc)
    member x.body: U2<ClassMethod, ClassProperty> list = body

type ClassDeclaration(loc, body, id, ?super, ?decorators) =
    inherit Declaration("ClassDeclaration", loc)
    member x.body: ClassBody = body
    member x.id: Identifier = id
    member x.superClass: Expression option = super
    member x.decorators: Decorator list = defaultArg decorators []

/// Anonymous class: e.g., var myClass = class { }
type ClassExpression(loc, body, ?id, ?super, ?decorators) =
    inherit Expression("ClassExpression", loc)
    member x.body: ClassBody = body
    member x.id: Identifier option = id    
    member x.superClass: Expression option = super
    member x.decorators: Decorator list = defaultArg decorators []

// type MetaProperty(loc, meta, property) =
//     inherit Expression("MetaProperty", loc)
//     member x.meta: Identifier = meta
//     member x.property: Expression = property

(** ##Modules *)
/// A specifier in an import or export declaration.
[<AbstractClass>]
type ModuleSpecifier(typ, loc, local) =
    inherit Node(typ, loc)
    member x.local: Identifier = local

/// An imported variable binding, e.g., {foo} in import {foo} from "mod" or {foo as bar} in import {foo as bar} from "mod". 
/// The imported field refers to the name of the export imported from the module. 
/// The local field refers to the binding imported into the local module scope. 
/// If it is a basic named import, such as in import {foo} from "mod", both imported and local are equivalent Identifier nodes; in this case an Identifier node representing foo. 
/// If it is an aliased import, such as in import {foo as bar} from "mod", the imported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ImportSpecifier(loc, local, imported) =
    inherit ModuleSpecifier("ImportSpecifier", loc, local)
    member x.imported: Identifier = imported

/// A default import specifier, e.g., foo in import foo from "mod.js".
type ImportDefaultSpecifier(loc, local) =
    inherit ModuleSpecifier("ImportDefaultSpecifier", loc, local)
    
/// A namespace import specifier, e.g., * as foo in import * as foo from "mod.js".
type ImportNamespaceSpecifier(loc, local) =
    inherit ModuleSpecifier("ImportNamespaceSpecifier", loc, local)

/// e.g., import foo from "mod";.
type ImportDeclaration(loc, specifiers, source) =
    inherit ModuleDeclaration("ImportDeclaration", loc)
    member x.specifiers: U3<ImportSpecifier, ImportDefaultSpecifier, ImportNamespaceSpecifier> list = specifiers
    member x.source: Literal = source

/// An exported variable binding, e.g., {foo} in export {foo} or {bar as foo} in export {bar as foo}. 
/// The exported field refers to the name exported in the module. 
/// The local field refers to the binding into the local module scope. 
/// If it is a basic named export, such as in export {foo}, both exported and local are equivalent Identifier nodes; 
/// in this case an Identifier node representing foo. If it is an aliased export, such as in export {bar as foo}, 
/// the exported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ExportSpecifier(loc, local, exported) =
    inherit ModuleSpecifier("ExportSpecifier", loc, local)
    member x.exported: Identifier = exported
    
/// An export named declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
/// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
type ExportNamedDeclaration(loc, ?declaration, ?specifiers, ?source) =
    inherit ModuleDeclaration("ExportNamedDeclaration", loc)
    member x.declaration: Declaration option = declaration
    member x.specifiers: ExportSpecifier list = defaultArg specifiers []
    member x.source: Literal option = source

/// An export default declaration, e.g., export default function () {}; or export default 1;. 
type ExportDefaultDeclaration(loc, declaration) =
    inherit ModuleDeclaration("ExportDefaultDeclaration", loc)
    member x.declaration: U2<Declaration, Expression> = declaration

/// An export batch declaration, e.g., export * from "mod";.
type ExportAllDeclaration(loc, source) =
    inherit ModuleDeclaration("ExportAllDeclaration", loc)
    member x.source: Literal = source
