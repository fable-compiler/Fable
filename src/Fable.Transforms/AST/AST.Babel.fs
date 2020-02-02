namespace Fable.AST.Babel

open Fable
open Fable.Core
open Fable.AST

/// The type field is a string representing the AST variant type.
/// Each subtype of Node is documented below with the specific string of its type field.
/// You can use this field to determine which interface a node implements.
/// The loc field represents the source location information of the node.
/// If the node contains no information about the source location, the field is null;
/// otherwise it is an object consisting of a start position (the position of the first character of the parsed source region)
/// and an end position (the position of the first character after the parsed source region):
[<AbstractClass>]
type Node(typ, ?loc) =
    member __.Type: string = typ
    member __.Loc: SourceLocation option = loc

/// Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.
[<AbstractClass>] type Expression(typ, ?loc) = inherit Node(typ, ?loc = loc)

[<AbstractClass>] type PatternNode(typ, ?loc) = inherit Node(typ, ?loc = loc)
[<AbstractClass>] type PatternExpression(typ, ?loc) = inherit Expression(typ, ?loc = loc)

type Pattern = U2<PatternNode, PatternExpression>

[<AbstractClass>] type Literal(typ, ?loc) = inherit Expression(typ, ?loc = loc)

[<AbstractClass>] type Statement(typ, ?loc) = inherit Node(typ, ?loc = loc)

/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
[<AbstractClass>] type Declaration(typ, ?loc) = inherit Statement(typ, ?loc = loc)

/// A module import or export declaration.
[<AbstractClass>] type ModuleDeclaration(typ, ?loc) = inherit Node(typ, ?loc = loc)

[<AbstractClass>]
type TypeAnnotationInfo(typ) =
    member __.Type: string = typ

type TypeAnnotation(typeAnnotation) =
    member __.Type = "TypeAnnotation"
    member __.TypeAnnotation: TypeAnnotationInfo = typeAnnotation

type TypeParameter(name, ?bound, ?default_) =
    member __.Type = "TypeParameter"
    member __.Name: string = name
    member __.Bound: TypeAnnotation option = bound
    member __.Default: TypeAnnotationInfo option = default_

type TypeParameterDeclaration(params_) =
    member __.Type = "TypeParameterDeclaration"
    member __.Params: TypeParameter array = params_

type TypeParameterInstantiation(params_) =
    member __.Type = "TypeParameterInstantiation"
    member __.Params: TypeAnnotationInfo array = params_

/// Not in Babel specs, disguised as StringLiteral
type MacroExpression(value, args, ?loc) =
    inherit Literal("StringLiteral", ?loc = loc)
    let macro = true
    member __.Value: string = value
    member __.Args: Expression array = args
    member __.Macro = macro

// Template Literals
type TemplateElement(value: string, tail, ?loc) =
    inherit Node("TemplateElement", ?loc = loc)
    member __.Tail: bool = tail
    member __.Value = dict [ ("raw", value); ("cooked", value) ]

type TemplateLiteral(quasis, expressions, ?loc) =
    inherit Literal("TemplateLiteral", ?loc = loc)
    member __.Quasis: TemplateElement array = quasis
    member __.Expressions: Expression array = expressions

type TaggedTemplateExpression(tag, quasi, ?loc) =
    inherit Expression("TaggedTemplateExpression", ?loc = loc)
    member __.Tag: Expression = tag
    member __.Quasi: TemplateLiteral = quasi

// Identifier
/// Note that an identifier may be an expression or a destructuring pattern.
type Identifier(name, ?typeAnnotation, ?loc) =
    inherit PatternExpression("Identifier", ?loc = loc)
    member __.Name: string = name
    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation
    override __.ToString() = __.Name

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
    member __.Flags = flags

type NullLiteral(?loc) =
    inherit Literal("NullLiteral", ?loc = loc)

type StringLiteral(value, ?loc) =
    inherit Literal("StringLiteral", ?loc = loc)
    member __.Value: string = value

type BooleanLiteral(value, ?loc) =
    inherit Literal("BooleanLiteral", ?loc = loc)
    member __.Value: bool = value

type NumericLiteral(value, ?loc) =
    inherit Literal("NumericLiteral", ?loc = loc)
    member __.Value: float = value

// Misc
type Decorator(value, ?loc) =
    inherit Node("Decorator", ?loc = loc)
    member __.Value = value

type DirectiveLiteral(value, ?loc) =
    inherit Literal("DirectiveLiteral", ?loc = loc)
    member __.Value: string = value

/// e.g. "use strict";
type Directive(value, ?loc) =
    inherit Node("Directive", ?loc = loc)
    new (str, ?loc) = Directive(DirectiveLiteral str, ?loc = loc)
    member __.Value: DirectiveLiteral = value

// Program

/// A complete program source tree.
/// Parsers must specify sourceType as "module" if the source has been parsed as an ES6 module.
/// Otherwise, sourceType must be "script".
type Program(fileName, body, ?directives, ?logs, ?dependencies, ?sourceFiles) =
    inherit Node("Program")
    let sourceType = "module" // Don't use "script"
    member __.SourceType = sourceType
    member __.Body: U2<Statement, ModuleDeclaration> array = body
    member __.Directives: Directive array = defaultArg directives [||]
    // Properties below don't belong to babel specs
    member __.FileName: string = fileName
    member __.Logs: Map<string, string array> = defaultArg logs Map.empty
    member __.Dependencies: string[] = defaultArg dependencies [||]
    member __.SourceFiles: string[] = defaultArg sourceFiles [||]

// Statements
/// An expression statement, i.e., a statement consisting of a single expression.
type ExpressionStatement(expression, ?loc) =
    inherit Statement("ExpressionStatement", ?loc = loc)
    member __.Expression: Expression = expression

/// A block statement, i.e., a sequence of statements surrounded by braces.
type BlockStatement(body, ?directives, ?loc) =
    inherit Statement("BlockStatement", ?loc = loc)
    member __.Body: Statement array = body
    member __.Directives: Directive array = defaultArg directives [||]

/// An empty statement, i.e., a solitary semicolon.
type EmptyStatement(?loc) =
    inherit Statement("EmptyStatement", ?loc = loc)

type DebuggerStatement(?loc) =
    inherit Statement("DebuggerStatement", ?loc = loc)

/// Statement (typically loop) prefixed with a label (for continuue and break)
type LabeledStatement(label, body, ?loc) =
    inherit Statement("LabeledStatement", ?loc = loc)
    member __.Body: Statement = body
    member __.Label: Identifier = label

/// Break can optionally take a label of a loop to break
type BreakStatement(?label, ?loc) =
    inherit Statement("BreakStatement", ?loc = loc)
    member __.Label: Identifier option = label

/// Contineu can optionally take a label of a loop to continuue
type ContinueStatement(?label, ?loc) =
    inherit Statement("ContinueStatement", ?loc = loc)
    member __.Label: Identifier option = label

// type WithStatement

// Control Flow
type ReturnStatement(argument, ?loc) =
    inherit Statement("ReturnStatement", ?loc = loc)
    member __.Argument: Expression = argument

// type LabeledStatement
// type BreakStatement
// type ContinueStatement

// U2
type IfStatement(test, consequent, ?alternate, ?loc) =
    inherit Statement("IfStatement", ?loc = loc)
    member __.Test: Expression = test
    member __.Consequent: Statement = consequent
    member __.Alternate: Statement option = alternate

/// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
type SwitchCase(consequent, ?test, ?loc) =
    inherit Node("SwitchCase", ?loc = loc)
    member __.Test: Expression option = test
    member __.Consequent: Statement array = consequent

type SwitchStatement(discriminant, cases, ?loc) =
    inherit Statement("SwitchStatement", ?loc = loc)
    member __.Discriminant: Expression = discriminant
    member __.Cases: SwitchCase array = cases

// Exceptions
type ThrowStatement(argument, ?loc) =
    inherit Statement("ThrowStatement", ?loc = loc)
    member __.Argument: Expression = argument

/// A catch clause following a try block.
type CatchClause(param, body, ?loc) =
    inherit Node("CatchClause", ?loc = loc)
    member __.Param: Pattern = param
    member __.Body: BlockStatement = body

/// If handler is null then finalizer must be a BlockStatement.
type TryStatement(block, ?handler, ?finalizer, ?loc) =
    inherit Statement("TryStatement", ?loc = loc)
    member __.Block: BlockStatement = block
    member __.Handler: CatchClause option = handler
    member __.Finalizer: BlockStatement option = finalizer

// Declarations
type VariableDeclarator(id, ?init, ?loc) =
    inherit Declaration("VariableDeclarator", ?loc = loc)
    member __.Id: Pattern = id
    member __.Init: Expression option = init

type VariableDeclarationKind = Var | Let | Const

type VariableDeclaration(kind_, declarations, ?loc) =
    inherit Declaration("VariableDeclaration", ?loc = loc)
    let kind = match kind_ with Var -> "var" | Let -> "let" | Const -> "const"
    new (var, ?init, ?kind, ?loc) =
        VariableDeclaration(defaultArg kind Let, [|VariableDeclarator(var, ?init=init, ?loc=loc)|], ?loc=loc)
    member __.Declarations: VariableDeclarator array = declarations
    member __.Kind = kind

// Loops
type WhileStatement(test, body, ?loc) =
    inherit Statement("WhileStatement", ?loc = loc)
    member __.Test: Expression = test
    member __.Body: BlockStatement = body

type DoWhileStatement(body, test, ?loc) =
    inherit Statement("DoWhileStatement", ?loc = loc)
    member __.Body: BlockStatement = body
    member __.Test: Expression = test

type ForStatement(body, ?init, ?test, ?update, ?loc) =
    inherit Statement("ForStatement", ?loc = loc)
    member __.Body: BlockStatement = body
    member __.Init: U2<VariableDeclaration, Expression> option = init
    member __.Test: Expression option = test
    member __.Update: Expression option = update

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
type ForInStatement(left, right, body, ?loc) =
    inherit Statement("ForInStatement", ?loc = loc)
    member __.Body: BlockStatement = body
    member __.Left: U2<VariableDeclaration, Expression> = left
    member __.Right: Expression = right

/// When passing a VariableDeclaration, the bound value must go through
/// the `right` parameter instead of `init` property in VariableDeclarator
type ForOfStatement(left, right, body, ?loc) =
    inherit Statement("ForOfStatement", ?loc = loc)
    member __.Body: BlockStatement = body
    member __.Left: U2<VariableDeclaration, Expression> = left
    member __.Right: Expression = right

/// A function declaration. Note that id cannot be null.
type FunctionDeclaration(id, params_, body, ?generator, ?async, ?returnType, ?typeParams, ?loc) =
    inherit Declaration("FunctionDeclaration", ?loc = loc)
    member __.Id: Identifier = id
    member __.Params: Pattern array = params_
    member __.Body: BlockStatement = body
    member __.Generator = defaultArg generator false
    member __.Async = defaultArg async false
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParams

// Expressions

/// A super pseudo-expression.
type Super(?loc) =
    inherit Expression("Super", ?loc = loc)

type ThisExpression(?loc) =
    inherit Expression("ThisExpression", ?loc = loc)

/// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
type ArrowFunctionExpression(params_, body, ?async, ?returnType, ?typeParams, ?loc) =
    inherit Expression("ArrowFunctionExpression", ?loc = loc)
    let expression = match body with U2.Case1 _ -> false | U2.Case2 _ -> true
    member __.Expression = expression
    member __.Params: Pattern array = params_
    member __.Body: U2<BlockStatement, Expression> = body
    member __.Async: bool = defaultArg async false
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParams

type FunctionExpression(params_, body, ?generator, ?async, ?id, ?returnType, ?typeParams, ?loc) =
    inherit Expression("FunctionExpression", ?loc = loc)
    member __.Id: Identifier option = id
    member __.Params: Pattern array = params_
    member __.Body: BlockStatement = body
    member __.Generator: bool = defaultArg generator false
    member __.Async: bool = defaultArg async false
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParams

/// e.g., x = do { var t = f(); t * t + 1 };
/// http://wiki.ecmascript.org/doku.php?id=strawman:do_expressions
/// Doesn't seem to work well with block-scoped variables (let, const)
type DoExpression(body, ?loc) =
    inherit Expression("DoExpression", ?loc = loc)
    member __.Body: BlockStatement = body

type YieldExpression(argument, delegate_, ?loc) =
    inherit Expression("YieldExpression", ?loc = loc)
    member __.Argument: Expression option = argument
    /// Delegates to another generator? (yield*)
    member __.Delegate: bool = delegate_

type AwaitExpression(argument, ?loc) =
    inherit Expression("AwaitExpression", ?loc = loc)
    member __.Argument: Expression option = argument

type RestProperty(argument, ?loc) =
    inherit Node("RestProperty", ?loc = loc)
    member __.Argument: Expression = argument

/// e.g., var z = { x: 1, ...y } // Copy all properties from y
type SpreadProperty(argument, ?loc) =
    inherit Node("SpreadProperty", ?loc = loc)
    member __.Argument: Expression = argument

/// Should derive from Node, but make it an expression for simplicity
type SpreadElement(argument, ?loc) =
    inherit Expression("SpreadElement", ?loc = loc)
    member __.Argument: Expression = argument

type ArrayExpression(elements, ?loc) =
    inherit Expression("ArrayExpression", ?loc = loc)
    // member __.elements: U2<Expression, SpreadElement> option array = elements
    member __.Elements: Expression array = elements

[<AbstractClass>]
type ObjectMember(typ, key, ?value, ?computed, ?loc) =
    inherit Node(typ, ?loc = loc)
    member __.Key: Expression = key
    member __.Value: Expression option = value
    member __.Computed: bool = defaultArg computed false
    // member __.Decorators: Decorator array = defaultArg decorators [||]

type ObjectProperty(key, value, ?shorthand, ?computed, ?loc) =
    inherit ObjectMember("ObjectProperty", key, value, ?computed=computed, ?loc=loc)
    member __.Shorthand: bool = defaultArg shorthand false

type ObjectMethodKind = ObjectGetter | ObjectSetter | ObjectMeth

type ObjectMethod(kind, key, params_, body, ?computed, ?generator, ?async, ?returnType, ?typeParams, ?loc) =
    inherit ObjectMember("ObjectMethod", key, ?computed=computed, ?loc=loc)
    let kind =
        match kind with
        | ObjectGetter -> "get"
        | ObjectSetter -> "set"
        | ObjectMeth -> "method"
    member __.Kind = kind
    member __.Params: Pattern array = params_
    member __.Body: BlockStatement = body
    member __.Generator: bool = defaultArg generator false
    member __.Async: bool = defaultArg async false
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParams

/// If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression.
/// If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
type MemberExpression(object, property, ?computed, ?loc) =
    inherit PatternExpression("MemberExpression", ?loc = loc)
    member __.Object: Expression = object
    member __.Property: Expression = property
    member __.Computed: bool = defaultArg computed false

type ObjectExpression(properties, ?loc) =
    inherit Expression("ObjectExpression", ?loc = loc)
    member __.Properties: U3<ObjectProperty, ObjectMethod, SpreadProperty> array = properties

/// A conditional expression, i.e., a ternary ?/: expression.
type ConditionalExpression(test, consequent, alternate, ?loc) =
    inherit Expression("ConditionalExpression", ?loc = loc)
    member __.Test: Expression = test
    member __.Consequent: Expression = consequent
    member __.Alternate: Expression = alternate

/// A function or method call expression.
type CallExpression(callee, arguments, ?loc) =
    inherit Expression("CallExpression", ?loc = loc)
    member __.Callee: Expression = callee
    // member __.Arguments: U2<Expression, SpreadElement> array = arguments
    member __.Arguments: Expression array = arguments

type NewExpression(callee, arguments, ?loc) =
    inherit Expression("NewExpression", ?loc = loc)
    member __.Callee: Expression = callee
    // member __.Arguments: U2<Expression, SpreadElement> array = arguments
    member __.Arguments: Expression array = arguments

/// A comma-separated sequence of expressions.
type SequenceExpression(expressions, ?loc) =
    inherit Expression("SequenceExpression", ?loc = loc)
    member __.Expressions: Expression array = expressions

// Unary Operations
type UnaryExpression(operator, argument, ?prefix, ?loc) =
    inherit Expression("UnaryExpression", ?loc = loc)
    let operator =
        match operator with
        | UnaryMinus -> "-"
        | UnaryPlus -> "+"
        | UnaryNot -> "!"
        | UnaryNotBitwise -> "~"
        | UnaryTypeof -> "typeof"
        | UnaryVoid -> "void"
        | UnaryDelete -> "delete"
    member __.Prefix: bool = defaultArg prefix true
    member __.Argument: Expression = argument
    member __.Operator = operator

type UpdateExpression(operator_, prefix, argument, ?loc) =
    inherit Expression("UpdateExpression", ?loc = loc)
    let operator =
        match operator_ with
        | UpdateMinus -> "--"
        | UpdatePlus -> "++"
    member __.Prefix: bool = prefix
    member __.Argument: Expression = argument
    member __.Operator = operator

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
    member __.Operator = operator

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
    member __.Operator = operator

type LogicalExpression(operator_, left, right, ?loc) =
    inherit Expression("LogicalExpression", ?loc = loc)
    let operator =
        match operator_ with
        | LogicalOr -> "||"
        | LogicalAnd-> "&&"
    member __.Left: Expression = left
    member __.Right: Expression = right
    member __.Operator = operator

// Patterns
// type AssignmentProperty(key, value, ?loc) =
//     inherit ObjectProperty("AssignmentProperty", ?loc = loc)
//     member __.Value: Pattern = value

// type ObjectPattern(properties, ?loc) =
//     inherit Node("ObjectPattern", ?loc = loc)
//     member __.Properties: U2<AssignmentProperty, RestProperty> array = properties
//     interface Pattern

type ArrayPattern(elements, ?typeAnnotation, ?loc) =
    inherit PatternNode("ArrayPattern", ?loc = loc)
    member __.Elements: Pattern option array = elements
    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation

type AssignmentPattern(left, right, ?typeAnnotation, ?loc) =
    inherit PatternNode("AssignmentPattern", ?loc = loc)
    member __.Left: Pattern = left
    member __.Right: Expression = right
    // member __.Decorators: Decorator array = defaultArg decorators [||]
    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation

type RestElement(argument, ?typeAnnotation, ?loc) =
    inherit PatternNode("RestElement", ?loc = loc)
    member __.Argument: Pattern = argument
    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation

// Classes
type ClassMethodKind =
    | ClassImplicitConstructor | ClassFunction | ClassGetter | ClassSetter

type ClassMethod(kind_, key, params_, body, computed, static_, ?returnType, ?typeParams, ?loc) =
    inherit Node("ClassMethod", ?loc = loc)
    let kind =
        match kind_ with
        | ClassImplicitConstructor -> "constructor"
        | ClassGetter -> "get"
        | ClassSetter -> "set"
        | ClassFunction -> "method"
    member __.Kind = kind
    member __.Key: Expression = key
    member __.Params: Pattern array = params_
    member __.Body: BlockStatement = body
    member __.Computed: bool = computed
    member __.Static: bool = static_
    member __.ReturnType: TypeAnnotation option = returnType
    member __.TypeParameters: TypeParameterDeclaration option = typeParams
    // member __.Decorators: Decorator array = defaultArg decorators [||]
    // This appears in astexplorer.net but it's not documented
    // member __.Expression: bool = false

/// ES Class Fields & Static Properties
/// https://github.com/jeffmo/es-class-fields-and-static-properties
/// e.g, class MyClass { static myStaticProp = 5; myProp /* = 10 */; }
type ClassProperty(key, ?value, ?typeAnnotation, ?loc) =
    inherit Node("ClassProperty", ?loc = loc)
    member __.Key: Identifier = key
    member __.Value: Expression option = value
    member __.TypeAnnotation: TypeAnnotation option = typeAnnotation

type ClassBody(body, ?loc) =
    inherit Node("ClassBody", ?loc = loc)
    member __.Body: U2<ClassMethod, ClassProperty> array = body

type ClassDeclaration(body, id, ?superClass, ?typeParams, ?loc) =
    inherit Declaration("ClassDeclaration", ?loc = loc)
    member __.Body: ClassBody = body
    member __.Id: Identifier = id
    member __.SuperClass: Expression option = superClass
    member __.TypeParameters: TypeParameterDeclaration option = typeParams
    // member __.Decorators: Decorator array = defaultArg decorators [||]

/// Anonymous class: e.g., var myClass = class { }
type ClassExpression(body, ?id, ?superClass, ?typeParams, ?loc) =
    inherit Expression("ClassExpression", ?loc = loc)
    member __.Body: ClassBody = body
    member __.Id: Identifier option = id
    member __.SuperClass: Expression option = superClass
    member __.TypeParameters: TypeParameterDeclaration option = typeParams
    // member __.Decorators: Decorator array = defaultArg decorators [||]

// type MetaProperty(meta, property, ?loc) =
//     inherit Expression("MetaProperty", ?loc = loc)
//     member __.Meta: Identifier = meta
//     member __.Property: Expression = property

// Modules
/// A specifier in an import or export declaration.
[<AbstractClass>]
type ModuleSpecifier(typ, local, ?loc) =
    inherit Node(typ, ?loc = loc)
    member __.Local: Identifier = local

/// An imported variable binding, e.g., {foo} in import {foo} from "mod" or {foo as bar} in import {foo as bar} from "mod".
/// The imported field refers to the name of the export imported from the module.
/// The local field refers to the binding imported into the local module scope.
/// If it is a basic named import, such as in import {foo} from "mod", both imported and local are equivalent Identifier nodes; in this case an Identifier node representing foo.
/// If it is an aliased import, such as in import {foo as bar} from "mod", the imported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ImportSpecifier(local, imported, ?loc) =
    inherit ModuleSpecifier("ImportSpecifier", local, ?loc = loc)
    member __.Imported: Identifier = imported

/// A default import specifier, e.g., foo in import foo from "mod".
type ImportDefaultSpecifier(local, ?loc) =
    inherit ModuleSpecifier("ImportDefaultSpecifier", local, ?loc = loc)

/// A namespace import specifier, e.g., * as foo in import * as foo from "mod".
type ImportNamespaceSpecifier(local, ?loc) =
    inherit ModuleSpecifier("ImportNamespaceSpecifier", local, ?loc = loc)

/// e.g., import foo from "mod";.
type ImportDeclaration(specifiers, source, ?loc) =
    inherit ModuleDeclaration("ImportDeclaration", ?loc = loc)
    member __.Specifiers: U3<ImportSpecifier, ImportDefaultSpecifier, ImportNamespaceSpecifier> array = specifiers
    member __.Source: Literal = source

/// An exported variable binding, e.g., {foo} in export {foo} or {bar as foo} in export {bar as foo}.
/// The exported field refers to the name exported in the module.
/// The local field refers to the binding into the local module scope.
/// If it is a basic named export, such as in export {foo}, both exported and local are equivalent Identifier nodes;
/// in this case an Identifier node representing foo. If it is an aliased export, such as in export {bar as foo},
/// the exported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ExportSpecifier(local, exported, ?loc) =
    inherit ModuleSpecifier("ExportSpecifier", local, ?loc = loc)
    member __.Exported: Identifier = exported

/// An export named declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
/// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
type ExportNamedDeclaration(?declaration, ?specifiers, ?source, ?loc) =
    inherit ModuleDeclaration("ExportNamedDeclaration", ?loc = loc)
    member __.Declaration: Declaration option = declaration
    member __.Specifiers: ExportSpecifier array = defaultArg specifiers [||]
    member __.Source: Literal option = source

/// An export default declaration, e.g., export default function () {}; or export default 1;.
type ExportDefaultDeclaration(declaration, ?loc) =
    inherit ModuleDeclaration("ExportDefaultDeclaration", ?loc = loc)
    member __.Declaration: U2<Declaration, Expression> = declaration

/// An export batch declaration, e.g., export * from "mod";.
type ExportAllDeclaration(source, ?loc) =
    inherit ModuleDeclaration("ExportAllDeclaration", ?loc = loc)
    member __.Source: Literal = source

// Type Annotations

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
    member __.Type = "FunctionTypeParam"
    member __.Name: Identifier = name
    member __.TypeAnnotation: TypeAnnotationInfo = typeInfo
    member __.Optional = defaultArg optional false

type FunctionTypeAnnotation(params_, returnType, ?rest) =
    inherit TypeAnnotationInfo("FunctionTypeAnnotation")
    member __.Params: FunctionTypeParam array = params_
    member __.Rest: FunctionTypeParam option = rest
    member __.ReturnType: TypeAnnotationInfo = returnType

type NullableTypeAnnotation(typ) =
    inherit TypeAnnotationInfo("NullableTypeAnnotation")
    member __.TypeAnnotation: TypeAnnotationInfo = typ

type GenericTypeAnnotation(id, ?typeParams) =
    inherit TypeAnnotationInfo("GenericTypeAnnotation")
    member __.Id: Identifier = id
    member __.TypeParameters: TypeParameterInstantiation option = typeParams

type ObjectTypeProperty(key, value, ?isStatic, ?isOptional) =
    inherit Node("ObjectTypeProperty")
    member __.Key: Identifier = key
    member __.Value: TypeAnnotationInfo = value
    member __.Static: bool = defaultArg isStatic false
    member __.Optional: bool = defaultArg isOptional false

type ObjectTypeAnnotation(properties) = //, ?indexers, ?callProperties, ?internalSlots, ?exact) =
    inherit TypeAnnotationInfo("ObjectTypeAnnotation")
    member __.Properties: ObjectTypeProperty array = properties
    // member __.Indexers: ObjectTypeIndexer array = defaultArg indexers [||]
    // member __.CallProperties: ObjectTypeCallProperty array = defaultArg callProperties [||]
    // member __.InternalSlots: ObjectTypeInternalSlot array = defaultArg internalSlots [||]
    // member __.Exact: bool = defaultArg exact false

type InterfaceExtends(id, ?typeParams) =
    member __.Type = "InterfaceExtends"
    member __.Id: Identifier = id
    member __.TypeParameters: TypeParameterInstantiation option = typeParams

type InterfaceDeclaration(id, body, ?extends, ?typeParams, ?loc) =
    inherit Declaration("InterfaceDeclaration", ?loc = loc)
    member __.Id: Identifier = id
    member __.Body: ObjectTypeAnnotation = body
    member __.Extends: InterfaceExtends array = defaultArg extends [||]
    member __.TypeParameters: TypeParameterDeclaration option = typeParams
