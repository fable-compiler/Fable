// fsharplint:disable MemberNames InterfaceNames
namespace rec Fable.AST.Babel

open Fable.AST

type UpdateOperator =
    | UpdateMinus
    | UpdatePlus

type AssignmentOperator =
    | AssignEqual
    | AssignMinus
    | AssignPlus
    | AssignMultiply
    | AssignDivide
    | AssignModulus
    | AssignShiftLeft
    | AssignShiftRightSignPropagating
    | AssignShiftRightZeroFill
    | AssignOrBitwise
    | AssignXorBitwise
    | AssignAndBitwise

/// Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.
type Expression =
    | JsxElement of componentOrTag: Expression * props: (string * Expression) list * children: Expression list
    | JsxTemplate of parts: string[] * values: Expression[]
    | Literal of Literal
    | Identifier of Identifier
    | ClassExpression of
        members: ClassMember array *
        id: Identifier option *
        superClass: Expression option *
        implements: ClassImplements array option *
        typeParameters: TypeParameterDeclaration option *
        loc: SourceLocation option
    | ClassImplements of ClassImplements
    | Super of loc: SourceLocation option
    | Undefined of Loc: SourceLocation option
    | ThisExpression of loc: SourceLocation option
    | SpreadElement of argument: Expression * loc: SourceLocation option
    | ArrayExpression of elements: Expression array * loc: SourceLocation option
    | ObjectExpression of properties: ObjectMember array * loc: SourceLocation option
    | SequenceExpression of expressions: Expression array * loc: SourceLocation option
    | EmitExpression of value: string * args: Expression array * loc: SourceLocation option
    | CallExpression of callee: Expression * args: Expression array * typeParameters: TypeParameterInstantiation option * loc: SourceLocation option
    | UnaryExpression of argument: Expression * operator: string * loc: SourceLocation option
    | UpdateExpression of prefix: bool * argument: Expression * operator: string * loc: SourceLocation option
    | BinaryExpression of left: Expression * right: Expression * operator: string * loc: SourceLocation option
    | LogicalExpression of left: Expression * operator: string * right: Expression * loc: SourceLocation option
    | AssignmentExpression of left: Expression * right: Expression * operator: string * loc: SourceLocation option
    | ConditionalExpression of test: Expression * consequent: Expression * alternate: Expression * loc: SourceLocation option
    | MemberExpression of object: Expression * property: Expression * computed: bool * loc: SourceLocation option
    | NewExpression of callee: Expression * args: Expression array * typeParameters: TypeParameterInstantiation option * loc: SourceLocation option
    | FunctionExpression of
        id: Identifier option *
        parameters: Pattern array *
        body: BlockStatement *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameterDeclaration option *
        loc: SourceLocation option
    | ArrowFunctionExpression of
        parameters: Pattern array *
        body: BlockStatement *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameterDeclaration option *
        loc: SourceLocation option

type Pattern =
    | RestElement of argument: Pattern
    | Identifier of name: Identifier * accessModifier: AccessModifier option

    member this.Name =
        match this with
        | RestElement(argument) -> argument.Name
        | Identifier(Identifier.Identifier(name=name), _) -> name

    member this.IsNamed =
        match this with
        | Identifier(Identifier.Identifier(named=named), _) -> named
        | RestElement _ -> false

    member this.AsNamed =
        match this with
        | Identifier(Identifier.Identifier(name, optional, _named, typeAnnotation, loc), accessModifier) ->
            Identifier(Identifier.Identifier(name, optional, true, typeAnnotation, loc), accessModifier)
        | RestElement argument -> argument.AsNamed // Named arguments cannot be spread

type Literal =
    | StringLiteral of StringLiteral
    | StringTemplate of tag: Expression option * parts: string array * values: Expression array * loc: SourceLocation option
    | DirectiveLiteral of value: string
    | NullLiteral of loc: SourceLocation option
    | BooleanLiteral of value: bool * loc: SourceLocation option
    | NumericLiteral of value: float * loc: SourceLocation option
    | RegExp of pattern: string * flags: string * loc: SourceLocation option
    | EnumCaseLiteral of id: Identifier * caseName: string

type Statement =
    | Declaration of Declaration
    | BlockStatement of BlockStatement
    | ExpressionStatement of expr: Expression /// An expression statement, i.e., a statement consisting of a single expression.
    | DebuggerStatement of loc: SourceLocation option
    | LabeledStatement of body: Statement * label: Identifier
    | ThrowStatement of argument: Expression * loc: SourceLocation option
    | ReturnStatement of argument: Expression * loc: SourceLocation option
    | BreakStatement of label: Identifier option * loc: SourceLocation option
    | ContinueStatement of label: Identifier option * loc: SourceLocation option
    | WhileStatement of test: Expression * body: BlockStatement * loc: SourceLocation option
    | SwitchStatement of discriminant: Expression * cases: SwitchCase array * loc: SourceLocation option
    | IfStatement of test: Expression * consequent: BlockStatement * alternate: Statement option * loc: SourceLocation option
    | TryStatement of block: BlockStatement * handler: CatchClause option * finalizer: BlockStatement option * loc: SourceLocation option
    | ForStatement of body: BlockStatement * init: VariableDeclaration option * test: Expression option * update: Expression option * loc: SourceLocation option

/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
type Declaration =
    | ClassDeclaration of
        members: ClassMember array *
        id: Identifier option *
        superClass: Expression option *
        implements: ClassImplements array option *
        typeParameters: TypeParameterDeclaration option *
        loc: SourceLocation option
    | VariableDeclaration of VariableDeclaration
    | FunctionDeclaration of
        parameters: Pattern array *
        body: BlockStatement *
        id: Identifier *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameterDeclaration option *
        loc: SourceLocation option
    | InterfaceDeclaration of
        id: Identifier *
        body: ObjectTypeAnnotation *
        extends: InterfaceExtends array *
        implements: ClassImplements array *
        typeParameters: TypeParameterDeclaration option
    | EnumDeclaration of
        name: string *
        cases: (string * Expression) array *
        isConst: bool
    | TypeAliasDeclaration of
        name: string *
        typeParameters: TypeParameterDeclaration *
        alias: TypeAnnotation

/// A module import or export declaration.
type ModuleDeclaration =
    | PrivateModuleDeclaration of statement: Statement
    /// An export named declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
    /// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
    | ExportNamedDeclaration of declaration: Declaration
    | ExportAllDeclaration of source: Literal * loc: SourceLocation option
    /// An export default declaration, e.g., export default function () {}; or export default 1;.
    | ExportDefaultDeclaration of declaration: Choice<Declaration, Expression>
    | ImportDeclaration of specifiers: ImportSpecifier array * source: StringLiteral
    | ExportNamedReferences of specifiers: ExportSpecifier array * source: StringLiteral option

//    /// An export batch declaration, e.g., export * from "mod";.
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
type Identifier =
    | Identifier of name: string * optional: bool * named: bool * typeAnnotation: TypeAnnotation option * loc: SourceLocation option
    member this.MapName(f) =
        match this with
            | Identifier(name, optional, named, typeAnnotation, loc) ->
                Identifier(f name, optional, named, typeAnnotation, loc)

type StringLiteral =
    | StringLiteral of value: string * loc: SourceLocation option

// Misc
//type Decorator(value, ?loc) =
//    inherit Node("Decorator", ?loc = loc)
//    member _.Value = value
//

// Program

/// A complete program source tree.
/// Parsers must specify sourceType as "module" if the source has been parsed as an ES6 module.
/// Otherwise, sourceType must be "script".
type Program =
    | Program of body: ModuleDeclaration array
    member this.IsEmpty =
        match this with
        | Program body -> Array.isEmpty body

//    let sourceType = "module" // Don't use "script"
//    member _.Directives: Directive array = directives
//    member _.SourceType: string = sourceType

// Statements

/// A block statement, i.e., a sequence of statements surrounded by braces.
type BlockStatement =
    | BlockStatement of body: Statement array

//    let directives = [||] // defaultArg directives_ [||]
//    member _.Directives: Directive array = directives

// /// An empty statement, i.e., a solitary semicolon.
//type EmptyStatement(?loc) =
//    inherit Statement("EmptyStatement", ?loc = loc)
//    member _.Print(_) = ()

// Control Flow

/// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
type SwitchCase =
    | SwitchCase of test: Expression option * consequent: Statement array * loc: SourceLocation option

/// A catch clause following a try block.
type CatchClause =
    | CatchClause of param: Pattern * body: BlockStatement * loc: SourceLocation option

// Declarations
type VariableDeclarator =
    | VariableDeclarator of id: Pattern * init: Expression option

type VariableDeclarationKind =
    | Var
    | Let
    | Const

type VariableDeclaration =
    | VariableDeclaration of declarations: VariableDeclarator array * kind: string * loc: SourceLocation option

// Loops

//type DoWhileStatement(body, test, ?loc) =
//    inherit Statement("DoWhileStatement", ?loc = loc)
//    member _.Body: BlockStatement = body
//    member _.Test: Expression = test

// /// When passing a VariableDeclaration, the bound value must go through
// /// the `right` parameter instead of `init` property in VariableDeclarator
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


    //    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator
//    member _.Declare: bool option = declare

// Expressions

//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator



//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator

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

type ObjectMember =
    | ObjectProperty of key: Expression * value: Expression * computed: bool
    | ObjectMethod of
        kind: string *
        key: Expression *
        parameters: Pattern array *
        body: BlockStatement *
        computed: bool *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameterDeclaration option *
        loc: SourceLocation option

//    let shorthand = defaultArg shorthand_ false
//    member _.Shorthand: bool = shorthand

type ObjectMethodKind = ObjectGetter | ObjectSetter | ObjectMeth

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


// Classes
type AccessModifier =
    | Public
    | Private
    | Protected
    | Readonly

type ClassMember =
    | ClassMethod of
        kind: string *
        key: Expression *
        parameters: Pattern array *
        body: BlockStatement *
        computed: bool *
        ``static``: bool option *
        ``abstract``: bool option *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameterDeclaration option *
        loc: SourceLocation option
    | ClassProperty of
        key: Expression *
        value: Expression option *
        computed: bool *
        ``static``: bool *
        optional: bool *
        typeAnnotation: TypeAnnotation option *
        accessModifier: AccessModifier option *
        loc: SourceLocation option

type ClassMethodKind =
    | ClassPrimaryConstructor | ClassFunction | ClassGetter | ClassSetter

    // This appears in astexplorer.net but it's not documented
    // member _.Expression: bool = false

/// ES Class Fields & Static Properties
/// https://github.com/jeffmo/es-class-fields-and-static-properties
/// e.g, class MyClass { static myStaticProp = 5; myProp /* = 10 */; }
type ClassImplements =
    | ClassImplements of id: Identifier * typeParameters: TypeParameterInstantiation option

// type MetaProperty(meta, property, ?loc) =
//     interface Expression with
//     member _.Meta: Identifier = meta
//     member _.Property: Expression = property

// Modules

/// An imported variable binding, e.g., {foo} in import {foo} from "mod" or {foo as bar} in import {foo as bar} from "mod".
/// The imported field refers to the name of the export imported from the module.
/// The local field refers to the binding imported into the local module scope.
/// If it is a basic named import, such as in import {foo} from "mod", both imported and local are equivalent Identifier nodes; in this case an Identifier node representing foo.
/// If it is an aliased import, such as in import {foo as bar} from "mod", the imported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ImportSpecifier =
    /// e.g., import foo from "mod";.
    | ImportMemberSpecifier of local: Identifier * imported: Identifier
    /// A default import specifier, e.g., foo in import foo from "mod".
    | ImportDefaultSpecifier of local: Identifier
    /// A namespace import specifier, e.g., * as foo in import * as foo from "mod".
    | ImportNamespaceSpecifier of local: Identifier

/// An exported variable binding, e.g., {foo} in export {foo} or {bar as foo} in export {bar as foo}.
/// The exported field refers to the name exported in the module.
/// The local field refers to the binding into the local module scope.
/// If it is a basic named export, such as in export {foo}, both exported and local are equivalent Identifier nodes;
/// in this case an Identifier node representing foo. If it is an aliased export, such as in export {bar as foo},
/// the exported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ExportSpecifier =
    | ExportSpecifier of local: Identifier * exported: Identifier

// Type Annotations
type TypeAnnotation =
    | AliasTypeAnnotation of id: Identifier * typeParameters: TypeParameterInstantiation
    | AnyTypeAnnotation
    | VoidTypeAnnotation
    | StringTypeAnnotation
    | NumberTypeAnnotation
    | BooleanTypeAnnotation
    | UnionTypeAnnotation of types: TypeAnnotation array
    | ObjectTypeAnnotation of ObjectTypeAnnotation
    | FunctionTypeAnnotation of
        parameters: FunctionTypeParam array *
        returnType: TypeAnnotation *
        typeParameters: TypeParameterDeclaration *
        rest: FunctionTypeParam option
    | NullableTypeAnnotation of typeAnnotation: TypeAnnotation
    | ArrayTypeAnnotation of TypeAnnotation
    | TupleTypeAnnotation of types: TypeAnnotation array
    | KeyofTypeAnnotation of TypeAnnotation
    | TypeofTypeAnnotation of Expression
    | IndexedTypeAnnotation of TypeAnnotation * prop: TypeAnnotation
    | LiteralTypeAnnotation of Literal

type TypeParameter =
    | TypeParameter of name: string * bound: TypeAnnotation option * ``default``: TypeAnnotation option

type TypeParameterDeclaration = TypeParameter array

type TypeParameterInstantiation = TypeAnnotation array

type FunctionTypeParam =
    | FunctionTypeParam of name: Identifier * typeAnnotation: TypeAnnotation * optional: bool option

type ObjectTypeProperty =
    | ObjectTypeProperty of
        key: Expression *
        value: TypeAnnotation *
        kind: string option *
        computed: bool *
        ``static``: bool *
        optional: bool *
        proto: bool *
        method: bool

type ObjectTypeIndexer =
    | ObjectTypeIndexer of id: Identifier option * key: Identifier * value: TypeAnnotation * ``static``: bool option

type ObjectTypeCallProperty =
    | ObjectTypeCallProperty of value: TypeAnnotation * ``static``: bool option

type ObjectTypeInternalSlot =
    | ObjectTypeInternalSlot of id: Identifier * value: TypeAnnotation * optional: bool * ``static``: bool * method: bool

type ObjectTypeAnnotation =
    | ObjectTypeAnnotation of
        properties: ObjectTypeProperty array *
        indexers: ObjectTypeIndexer array *
        callProperties: ObjectTypeCallProperty array *
        internalSlots: ObjectTypeInternalSlot array *
        exact: bool
type InterfaceExtends =
    | InterfaceExtends of id: Identifier * typeParameters: TypeParameterInstantiation option

//    let mixins = defaultArg mixins_ [||]
//    member _.Mixins: InterfaceExtends array = mixins

[<AutoOpen>]
module Helpers =
    type Expression with
        static member jsxElement(componentOrTag, props, children) = JsxElement(componentOrTag, props, children)
        static member jsxTemplate(parts, values) = JsxTemplate(parts, values)
        static member jsxTemplate(part) = JsxTemplate([|part|], [||])
        static member super(?loc) = Super loc
        static member emitExpression(value, args, ?loc) = EmitExpression(value, args, loc)
        static member nullLiteral(?loc) = NullLiteral loc |> Literal
        static member numericLiteral(value, ?loc) = NumericLiteral (value, loc) |> Literal
        static member booleanLiteral(value, ?loc) = BooleanLiteral (value, loc) |> Literal
        static member stringLiteral(value, ?loc) = Literal.stringLiteral (value, ?loc=loc) |> Literal
        static member arrayExpression(elements, ?loc) = ArrayExpression(elements, ?loc=loc)
        static member identifier(name, ?typeAnnotation, ?loc) =
            Identifier.identifier(name, ?typeAnnotation = typeAnnotation, ?loc = loc)
            |> Expression.Identifier
        static member regExpLiteral(pattern, flags_, ?loc) =
            Literal.regExpLiteral(pattern, flags_, ?loc=loc) |> Literal
        /// A function or method call expression.
        static member callExpression(callee, args, ?typeParameters, ?loc) =
            CallExpression(callee, args, typeParameters, loc)
        static member assignmentExpression(operator_, left, right, ?loc) =
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
            AssignmentExpression(left, right, operator, loc)
        /// A super pseudo-expression.
        static member thisExpression(?loc) = ThisExpression loc
        /// A comma-separated sequence of expressions.
        static member sequenceExpression(expressions, ?loc) = SequenceExpression(expressions, loc)
        static member logicalExpression(left, operator, right, ?loc) =
            let operator =
                match operator with
                | LogicalOr -> "||"
                | LogicalAnd -> "&&"

            LogicalExpression(left, operator, right, loc)
        static member objectExpression(properties, ?loc) = ObjectExpression(properties, loc)
        static member newExpression(callee, args, ?typeParameters, ?loc) = NewExpression(callee, args, typeParameters, loc)
        /// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
        static member arrowFunctionExpression(parameters, body: BlockStatement, ?returnType, ?typeParameters, ?loc) = //?async_, ?generator_,
            ArrowFunctionExpression(parameters, body, returnType, typeParameters, loc)
        static member arrowFunctionExpression(parameters, body: Expression, ?returnType, ?typeParameters, ?loc): Expression =
            let body = BlockStatement [| Statement.returnStatement(body) |]
            Expression.arrowFunctionExpression(parameters, body, ?returnType = returnType, ?typeParameters = typeParameters, ?loc = loc)
        /// If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression.
        /// If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
        static member memberExpression(object, property, ?computed_, ?loc) =
            let computed = defaultArg computed_ false
            MemberExpression(object, property, computed, loc)
        static member functionExpression(parameters, body, ?id, ?returnType, ?typeParameters, ?loc) = //?generator_, ?async_
            FunctionExpression(id, parameters, body, returnType, typeParameters, loc)
        static member classExpression(body, ?id, ?superClass, ?typeParameters, ?implements, ?loc) =
            ClassExpression(body, id, superClass, implements, typeParameters, loc)
        static member spreadElement(argument, ?loc) =
            SpreadElement(argument, ?loc=loc)
        static member conditionalExpression(test, consequent, alternate, ?loc): Expression =
            ConditionalExpression(test, consequent, alternate, loc)
        static member binaryExpression(operator_, left, right, ?loc) =
            let operator =
                match operator_ with
                | BinaryEqual -> "==="
                | BinaryUnequal -> "!=="
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
            BinaryExpression(left, right, operator, loc)
        static member unaryExpression(operator_, argument, ?loc) =
            let operator =
                match operator_ with
                | UnaryMinus -> "-"
                | UnaryPlus -> "+"
                | UnaryNot -> "!"
                | UnaryNotBitwise -> "~"
                | UnaryAddressOf -> "" //"&"
            UnaryExpression(argument, operator, loc)
        static member updateExpression(operator_, prefix, argument, ?loc) : Expression =
            let operator =
                match operator_ with
                | UpdateMinus -> "--"
                | UpdatePlus -> "++"
            UpdateExpression(prefix, argument, operator, loc)

    type Identifier with
        member this.Name =
            let (Identifier(name=name)) = this
            name
        static member identifier(name, ?typeAnnotation, ?loc) : Identifier =
            Identifier(name, optional=false, named=false, typeAnnotation=typeAnnotation, loc=loc)

    type Statement with
        static member blockStatement(body) = BlockStatement body |> Statement.BlockStatement
        static member returnStatement(argument, ?loc) : Statement = ReturnStatement(argument, loc)
        static member continueStatement(label, ?loc) = ContinueStatement(Some label, loc)
        static member tryStatement(block, ?handler, ?finalizer, ?loc) = TryStatement(block, handler, finalizer, loc)
        static member ifStatement(test, consequent, ?alternate, ?loc): Statement = IfStatement(test, consequent, alternate, loc)
        /// Break can optionally take a label of a loop to break
        static member breakStatement(?label, ?loc) = BreakStatement(label, loc)
        /// Statement (typically loop) prefixed with a label (for continue and break)
        static member labeledStatement(label, body): Statement = LabeledStatement (body, label)
        static member whileStatement(test, body, ?loc) = WhileStatement(test, body, loc)
        static member debuggerStatement(?loc) = DebuggerStatement loc
        static member switchStatement(discriminant, cases, ?loc) = SwitchStatement(discriminant, cases, loc)
        static member variableDeclaration(kind, declarations, ?loc): Statement =
            Declaration.variableDeclaration(kind, declarations, ?loc = loc)
            |> Declaration
        static member variableDeclaration(var, ?init, ?kind, ?loc): Statement =
            Declaration.variableDeclaration(var, ?init = init, ?kind = kind, ?loc = loc)
            |> Declaration
        static member forStatement(body, ?init, ?test, ?update, ?loc) = ForStatement(body, init, test, update, loc)
        static member throwStatement(argument, ?loc) = ThrowStatement(argument, loc)

    type BlockStatement with
        member this.Body =
            let (BlockStatement body) = this
            body

    type Program with
        member this.Body =
            let (Program body) = this
            body

    type CatchClause with
        static member catchClause(param, body, ?loc) =
            CatchClause(param, body, loc)

    type SwitchCase with
        static member switchCase(?consequent, ?test, ?loc) =
            SwitchCase(test, defaultArg consequent Array.empty, loc)

    type Pattern with
        static member identifier(name, ?optional, ?named, ?typeAnnotation, ?accessModifier, ?loc) =
            let ident = Identifier(name, optional = defaultArg optional false, named = defaultArg named false, ?typeAnnotation = typeAnnotation, ?loc = loc)
            Pattern.Identifier(ident, accessModifier)
        static member restElement(argument: Pattern) =
            RestElement(argument)

    type ClassImplements with
        static member classImplements(id, ?typeParameters) =
            ClassImplements(id, typeParameters)

    type Declaration with
        static member variableDeclaration(kind, declarations, ?loc) : Declaration =
            VariableDeclaration.variableDeclaration(kind, declarations, ?loc = loc) |> Declaration.VariableDeclaration
        static member variableDeclaration(var, ?init, ?kind, ?loc) =
            Declaration.variableDeclaration(
                defaultArg kind Let,
                [| VariableDeclarator(var, init) |],
                ?loc = loc
            )
        static member functionDeclaration(parameters, body, id, ?returnType, ?typeParameters, ?loc) =
            FunctionDeclaration(parameters, body, id, returnType, typeParameters, loc)
        static member classDeclaration(body, ?id, ?superClass, ?typeParameters, ?implements, ?loc) =
            ClassDeclaration(body, id, superClass, implements, typeParameters, loc)
        static member interfaceDeclaration(id, body, ?extends_, ?typeParameters, ?implements_): Declaration = // ?mixins_,
            let extends = defaultArg extends_ [||]
            let implements = defaultArg implements_ [||]
            InterfaceDeclaration(id, body, extends, implements, typeParameters)
        static member enumDeclaration(name, cases, ?isConst) =
            EnumDeclaration(name, cases, defaultArg isConst false)

    type VariableDeclaration with
        static member variableDeclaration(kind, declarations, ?loc) : VariableDeclaration =
            let kind =
                match kind with
                | Var -> "var"
                | Let -> "let"
                | Const -> "const"
            VariableDeclaration(declarations, kind, loc)

        static member variableDeclaration(var, ?init, ?kind, ?loc) =
            VariableDeclaration.variableDeclaration(defaultArg kind Let, [| VariableDeclarator(var, init) |], ?loc = loc)

    type VariableDeclarator with
        static member variableDeclarator(id, ?init) = VariableDeclarator(id, init)

    type ObjectTypeIndexer with
        static member objectTypeIndexer(key, value, ?id, ?``static``) =
            ObjectTypeIndexer(id, key, value, ``static``)

    type InterfaceExtends with
        static member interfaceExtends(id, ?typeParameters) =
            InterfaceExtends(id, typeParameters)

    type FunctionTypeParam with
        static member functionTypeParam(name, typeInfo, ?optional) =
            FunctionTypeParam(name, typeInfo, optional)

    type ClassMember with
        static member classMethod(kind_, key, parameters, body, ?computed_, ?``static``, ?``abstract``, ?returnType, ?typeParameters, ?loc) : ClassMember =
            let kind =
                match kind_ with
                | ClassPrimaryConstructor -> "constructor"
                | ClassGetter -> "get"
                | ClassSetter -> "set"
                | ClassFunction -> "method"
            let computed = defaultArg computed_ false
            ClassMethod(kind, key, parameters, body, computed, ``static``, ``abstract``, returnType, typeParameters, loc)
        static member classProperty(key, ?value, ?computed_, ?``static``, ?optional, ?typeAnnotation, ?accessModifier, ?loc): ClassMember =
            let computed = defaultArg computed_ false
            ClassProperty(key, value, computed, defaultArg ``static`` false, defaultArg optional false, typeAnnotation, accessModifier, loc)

    type Literal with
        static member nullLiteral(?loc) = NullLiteral loc
        static member numericLiteral(value, ?loc) = NumericLiteral (value, loc)
        static member booleanLiteral(value, ?loc) = BooleanLiteral (value, loc)
        static member stringLiteral(value, ?loc) = StringLiteral (value, loc) |> Literal.StringLiteral
        static member regExpLiteral(pattern, flags, ?loc) =
            let flags =
                flags |> Seq.map (function
                    | RegexGlobal -> "g"
                    | RegexUnicode -> "u"
                    | RegexIgnoreCase -> "i"
                    | RegexMultiline -> "m"
                    | RegexSingleline -> "s"
                    | RegexSticky -> "y") |> Seq.fold (+) ""
            RegExp(pattern, flags, loc)

    type StringLiteral with
        static member stringLiteral(value, ?loc) = StringLiteral(value, loc)

    type ObjectMember with
        static member objectProperty(key, value, ?computed_) = // ?shorthand_,
            let computed = defaultArg computed_ false
            ObjectProperty(key, value, computed)
        static member objectMethod(kind_, key, parameters, body, ?computed_, ?returnType, ?typeParameters, ?loc) =
            let kind =
                match kind_ with
                | ObjectGetter -> "get"
                | ObjectSetter -> "set"
                | ObjectMeth -> "method"
            let computed = defaultArg computed_ false
            ObjectMethod(kind, key, parameters, body, computed, returnType, typeParameters, loc)

    type ObjectTypeProperty with
        static member objectTypeProperty(key, value, ?computed_, ?kind, ?``static``, ?optional, ?proto, ?method) =
            let computed = defaultArg computed_ false
            let optional = defaultArg optional false
            let method = defaultArg method false
            let ``static`` = defaultArg ``static`` false
            let proto = defaultArg proto false
            ObjectTypeProperty(key, value, kind, computed, ``static``, optional, proto, method)

    type ModuleDeclaration with
        static member exportAllDeclaration(source, ?loc) = ExportAllDeclaration(source, loc)
        static member exportNamedReferences(specifiers, ?source): ModuleDeclaration =
            ExportNamedReferences(specifiers, source)

    type TypeAnnotation with
        static member aliasTypeAnnotation(id, ?typeParameters) =
            AliasTypeAnnotation(id, defaultArg typeParameters [||])
        static member functionTypeAnnotation(parameters, returnType, ?typeParameters, ?rest): TypeAnnotation =
            FunctionTypeAnnotation(parameters,returnType, defaultArg typeParameters [||], rest)

    type ObjectTypeAnnotation with
        static member objectTypeAnnotation(properties, ?indexers_, ?callProperties_, ?internalSlots_, ?exact_) =
            let exact = defaultArg exact_ false
            let indexers = defaultArg indexers_ [||]
            let callProperties = defaultArg callProperties_ [||]
            let internalSlots = defaultArg internalSlots_ [||]

            ObjectTypeAnnotation(properties, indexers, callProperties, internalSlots, exact)

    type TypeParameter with
        static member typeParameter(name, ?bound, ?``default``) =
            TypeParameter(name, bound, ``default``)
