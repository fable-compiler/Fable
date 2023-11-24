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
    | CommentedExpression of comment: string * expr: Expression
    | JsxElement of
        componentOrTag: Expression *
        props: (string * Expression) list *
        children: Expression list
    | JsxTemplate of parts: string[] * values: Expression[]
    | Literal of Literal
    | Identifier of Identifier
    | ClassExpression of
        members: ClassMember array *
        id: Identifier option *
        superClass: SuperClass option *
        implements: TypeAnnotation array *
        typeParameters: TypeParameter array *
        loc: SourceLocation option
    | Super of loc: SourceLocation option
    | Undefined of loc: SourceLocation option
    | ThisExpression of loc: SourceLocation option
    | SpreadElement of argument: Expression * loc: SourceLocation option
    | ArrayExpression of elements: Expression array * loc: SourceLocation option
    | ObjectExpression of
        properties: ObjectMember array *
        loc: SourceLocation option
    | SequenceExpression of
        expressions: Expression array *
        loc: SourceLocation option
    | EmitExpression of
        value: string *
        args: Expression array *
        loc: SourceLocation option
    | CallExpression of
        callee: Expression *
        args: Expression array *
        typeArguments: TypeAnnotation array *
        loc: SourceLocation option
    | UnaryExpression of
        argument: Expression *
        operator: string *
        isSuffix: bool *
        loc: SourceLocation option
    | UpdateExpression of
        prefix: bool *
        argument: Expression *
        operator: string *
        loc: SourceLocation option
    | BinaryExpression of
        left: Expression *
        right: Expression *
        operator: string *
        loc: SourceLocation option
    | LogicalExpression of
        left: Expression *
        operator: string *
        right: Expression *
        loc: SourceLocation option
    | AssignmentExpression of
        left: Expression *
        right: Expression *
        operator: string *
        loc: SourceLocation option
    | ConditionalExpression of
        test: Expression *
        consequent: Expression *
        alternate: Expression *
        loc: SourceLocation option
    | MemberExpression of
        object: Expression *
        property: Expression *
        isComputed: bool *
        loc: SourceLocation option
    | NewExpression of
        callee: Expression *
        args: Expression array *
        typeArguments: TypeAnnotation array *
        loc: SourceLocation option
    | FunctionExpression of
        id: Identifier option *
        parameters: Parameter array *
        body: BlockStatement *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameter array *
        loc: SourceLocation option
    | ArrowFunctionExpression of
        parameters: Parameter array *
        body: BlockStatement *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameter array *
        loc: SourceLocation option
    | AsExpression of expression: Expression * typeAnnotation: TypeAnnotation

    member this.Location =
        match this with
        | ClassExpression(loc = loc) -> loc
        | Super(loc = loc) -> loc
        | Undefined(loc = loc) -> loc
        | ThisExpression(loc = loc) -> loc
        | SpreadElement(loc = loc) -> loc
        | ArrayExpression(loc = loc) -> loc
        | ObjectExpression(loc = loc) -> loc
        | SequenceExpression(loc = loc) -> loc
        | EmitExpression(loc = loc) -> loc
        | CallExpression(loc = loc) -> loc
        | UnaryExpression(loc = loc) -> loc
        | UpdateExpression(loc = loc) -> loc
        | BinaryExpression(loc = loc) -> loc
        | LogicalExpression(loc = loc) -> loc
        | AssignmentExpression(loc = loc) -> loc
        | ConditionalExpression(loc = loc) -> loc
        | MemberExpression(loc = loc) -> loc
        | NewExpression(loc = loc) -> loc
        | FunctionExpression(loc = loc) -> loc
        | ArrowFunctionExpression(loc = loc) -> loc
        | _ -> None

type ParameterFlags(?defVal: Expression, ?isOptional, ?isSpread, ?isNamed) =
    member _.DefVal = defVal
    member _.IsOptional = defaultArg isOptional false
    member _.IsNamed = defaultArg isNamed false
    member _.IsSpread = defaultArg isSpread false

type Parameter =
    | Parameter of
        name: string *
        typeAnnotation: TypeAnnotation option *
        flags: ParameterFlags

    member this.Name =
        match this with
        | Parameter(name = name) -> name

    member this.WithFlags(flags) =
        match this with
        | Parameter(name, typ, _) -> Parameter(name, typ, flags)

type Literal =
    | StringLiteral of StringLiteral
    | StringTemplate of
        tag: Expression option *
        parts: string array *
        values: Expression array *
        loc: SourceLocation option
    | DirectiveLiteral of value: string
    | NullLiteral of loc: SourceLocation option
    | BooleanLiteral of value: bool * loc: SourceLocation option
    | BigIntLiteral of value: string * loc: SourceLocation option
    | NumericLiteral of value: float * loc: SourceLocation option
    | RegExp of pattern: string * flags: string * loc: SourceLocation option
    | EnumCaseLiteral of id: Identifier * caseName: string

type Statement =
    | Declaration of Declaration
    | BlockStatement of BlockStatement
    | ExpressionStatement of expr: Expression
    /// An expression statement, i.e., a statement consisting of a single expression.
    | DebuggerStatement of loc: SourceLocation option
    | LabeledStatement of body: Statement * label: Identifier
    | ThrowStatement of argument: Expression * loc: SourceLocation option
    | ReturnStatement of argument: Expression * loc: SourceLocation option
    | BreakStatement of label: Identifier option * loc: SourceLocation option
    | ContinueStatement of label: Identifier option * loc: SourceLocation option
    | WhileStatement of
        test: Expression *
        body: BlockStatement *
        loc: SourceLocation option
    | SwitchStatement of
        discriminant: Expression *
        cases: SwitchCase array *
        loc: SourceLocation option
    | IfStatement of
        test: Expression *
        consequent: BlockStatement *
        alternate: Statement option *
        loc: SourceLocation option
    | TryStatement of
        block: BlockStatement *
        handler: CatchClause option *
        finalizer: BlockStatement option *
        loc: SourceLocation option
    | ForStatement of
        body: BlockStatement *
        init: VariableDeclaration option *
        test: Expression option *
        update: Expression option *
        loc: SourceLocation option

/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
type Declaration =
    | ClassDeclaration of
        members: ClassMember array *
        id: Identifier option *
        superClass: SuperClass option *
        implements: TypeAnnotation array *
        typeParameters: TypeParameter array *
        loc: SourceLocation option *
        doc: string option
    | VariableDeclaration of var: VariableDeclaration
    | FunctionDeclaration of
        parameters: Parameter array *
        body: BlockStatement *
        id: Identifier *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameter array *
        loc: SourceLocation option *
        doc: string option
    | InterfaceDeclaration of
        id: Identifier *
        members: AbstractMember array *
        extends: TypeAnnotation array *
        typeParameters: TypeParameter array
    | EnumDeclaration of
        name: string *
        cases: (string * Expression) array *
        isConst: bool
    | TypeAliasDeclaration of
        name: string *
        typeParameters: TypeParameter array *
        alias: TypeAnnotation

    member this.JsDoc =
        match this with
        | ClassDeclaration(_, _, _, _, _, _, doc)
        | FunctionDeclaration(_, _, _, _, _, _, doc) -> doc
        | _ -> None

/// A module import or export declaration.
type ModuleDeclaration =
    | PrivateModuleDeclaration of statement: Statement
    /// An export isNamed declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
    /// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
    | ExportNamedDeclaration of declaration: Declaration
    | ExportAllDeclaration of source: Literal * loc: SourceLocation option
    /// An export default declaration, e.g., export default function () {}; or export default 1;.
    | ExportDefaultDeclaration of declaration: Choice<Declaration, Expression>
    | ImportDeclaration of
        specifiers: ImportSpecifier array *
        source: StringLiteral
    | ExportNamedReferences of
        specifiers: ExportSpecifier array *
        source: StringLiteral option

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
type Identifier = | Identifier of name: string * loc: SourceLocation option

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
type BlockStatement = | BlockStatement of body: Statement array

//    let directives = [||] // defaultArg directives_ [||]
//    member _.Directives: Directive array = directives

// /// An empty statement, i.e., a solitary semicolon.
//type EmptyStatement(?loc) =
//    inherit Statement("EmptyStatement", ?loc = loc)
//    member _.Print(_) = ()

// Control Flow

/// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
type SwitchCase =
    | SwitchCase of
        test: Expression option *
        consequent: Statement array *
        loc: SourceLocation option

/// A catch clause following a try block.
type CatchClause =
    | CatchClause of
        param: string *
        annotation: TypeAnnotation option *
        body: BlockStatement *
        loc: SourceLocation option

// Declarations
type VariableDeclarator =
    | VariableDeclarator of
        name: string *
        annotation: TypeAnnotation option *
        typeParameters: TypeParameter array *
        init: Expression option *
        loc: SourceLocation option

type VariableDeclarationKind =
    | Var
    | Let
    | Const

type VariableDeclaration =
    | VariableDeclaration of
        declarations: VariableDeclarator array *
        kind: VariableDeclarationKind *
        loc: SourceLocation option

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

type AbstractMember =
    | AbstractProperty of
        key: Expression *
        returnType: TypeAnnotation *
        isComputed: bool *
        isOptional: bool *
        doc: string option
    | AbstractMethod of
        kind: ObjectMethodKind *
        key: Expression *
        parameters: Parameter array *
        returnType: TypeAnnotation *
        typeParameters: TypeParameter array *
        isComputed: bool *
        doc: string option

type ObjectMember =
    | ObjectProperty of
        key: Expression *
        value: Expression *
        isComputed: bool *
        doc: string option
    | ObjectMethod of
        kind: ObjectMethodKind *
        key: Expression *
        parameters: Parameter array *
        body: BlockStatement *
        isComputed: bool *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameter array *
        loc: SourceLocation option *
        doc: string option

//    let shorthand = defaultArg shorthand_ false
//    member _.Shorthand: bool = shorthand

type ObjectMethodKind =
    | ObjectGetter
    | ObjectSetter
    | ObjectMeth

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

type SuperClass =
    | SuperType of TypeAnnotation
    | SuperExpression of Expression

type ClassMember =
    | ClassMethod of
        kind: ClassMethodKind *
        parameters: Parameter array *
        body: BlockStatement *
        isStatic: bool *
        isAbstract: bool *
        returnType: TypeAnnotation option *
        typeParameters: TypeParameter array *
        loc: SourceLocation option *
        doc: string option
    | ClassProperty of
        key: Expression *
        value: Expression option *
        isComputed: bool *
        isStatic: bool *
        isOptional: bool *
        typeAnnotation: TypeAnnotation option *
        accessModifier: AccessModifier option *
        loc: SourceLocation option *
        doc: string option


type ClassMethodKind =
    | ClassPrimaryConstructor of AccessModifier[]
    | ClassFunction of key: Expression * isComputed: bool
    | ClassGetter of key: Expression * isComputed: bool
    | ClassSetter of key: Expression * isComputed: bool

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
    | AliasTypeAnnotation of
        id: Identifier *
        typeArguments: TypeAnnotation array
    | AnyTypeAnnotation
    | VoidTypeAnnotation
    | UndefinedTypeAnnotation
    | StringTypeAnnotation
    | NumberTypeAnnotation
    | BooleanTypeAnnotation
    | UnionTypeAnnotation of types: TypeAnnotation array
    | IntersectionTypeAnnotation of types: TypeAnnotation array
    | ObjectTypeAnnotation of AbstractMember array
    | FunctionTypeAnnotation of
        parameters: FunctionTypeParam array *
        returnType: TypeAnnotation *
        spread: FunctionTypeParam option
    | ArrayTypeAnnotation of TypeAnnotation
    | TupleTypeAnnotation of types: TypeAnnotation array
    | KeyofTypeAnnotation of TypeAnnotation
    | TypeofTypeAnnotation of Expression
    | IndexedTypeAnnotation of TypeAnnotation * prop: TypeAnnotation
    | LiteralTypeAnnotation of Literal

type TypeParameter =
    | TypeParameter of
        name: string *
        bound: TypeAnnotation option *
        ``default``: TypeAnnotation option

type FunctionTypeParam =
    | FunctionTypeParam of
        name: Identifier *
        typeAnnotation: TypeAnnotation *
        isOptional: bool

[<AutoOpen>]
module Helpers =
    type Expression with

        static member jsxElement(componentOrTag, props, children) =
            JsxElement(componentOrTag, props, children)

        static member jsxTemplate(parts, values) = JsxTemplate(parts, values)
        static member jsxTemplate(part) = JsxTemplate([| part |], [||])
        static member super(?loc) = Super loc

        static member emitExpression(value, args, ?loc) =
            EmitExpression(value, args, loc)

        static member nullLiteral(?loc) = NullLiteral loc |> Literal

        static member bigintLiteral(value, ?loc) =
            BigIntLiteral(value, loc) |> Literal

        static member numericLiteral(value, ?loc) =
            NumericLiteral(value, loc) |> Literal

        static member booleanLiteral(value, ?loc) =
            BooleanLiteral(value, loc) |> Literal

        static member stringLiteral(value, ?loc) =
            Literal.stringLiteral (value, ?loc = loc) |> Literal

        static member arrayExpression(elements, ?loc) =
            ArrayExpression(elements, ?loc = loc)

        static member identifier(name, ?loc) =
            Identifier.identifier (name, ?loc = loc) |> Expression.Identifier

        static member regExpLiteral(pattern, flags_, ?loc) =
            Literal.regExpLiteral (pattern, flags_, ?loc = loc) |> Literal

        /// A function or method call expression.
        static member callExpression(callee, args, ?typeArguments, ?loc) =
            CallExpression(callee, args, defaultArg typeArguments [||], loc)

        static member assignmentExpression(operator, left, right, ?loc) =
            let operator =
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

            AssignmentExpression(left, right, operator, loc)

        /// A super pseudo-expression.
        static member thisExpression(?loc) = ThisExpression loc

        /// A comma-separated sequence of expressions.
        static member sequenceExpression(expressions, ?loc) =
            SequenceExpression(expressions, loc)

        static member logicalExpression(left, operator, right, ?loc) =
            let operator =
                match operator with
                | LogicalOr -> "||"
                | LogicalAnd -> "&&"

            LogicalExpression(left, operator, right, loc)

        static member objectExpression(properties, ?loc) =
            ObjectExpression(properties, loc)

        static member newExpression(callee, args, ?typeArguments, ?loc) =
            NewExpression(callee, args, defaultArg typeArguments [||], loc)

        /// A fat arrow function expression, e.g., let foo = (bar) => { body }
        static member arrowFunctionExpression
            (
                parameters,
                body: BlockStatement,
                ?returnType,
                ?typeParameters,
                ?loc
            )
            = //?async_, ?generator_,
            ArrowFunctionExpression(
                parameters,
                body,
                returnType,
                defaultArg typeParameters [||],
                loc
            )

        static member arrowFunctionExpression
            (
                parameters,
                body: Expression,
                ?returnType,
                ?typeParameters,
                ?loc
            )
            : Expression
            =
            let body = BlockStatement [| Statement.returnStatement (body) |]

            Expression.arrowFunctionExpression (
                parameters,
                body,
                ?returnType = returnType,
                ?typeParameters = typeParameters,
                ?loc = loc
            )

        /// If isComputed is true, the node corresponds to a isComputed (a[b]) member expression and property is an Expression.
        /// If isComputed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
        static member memberExpression(object, property, ?isComputed, ?loc) =
            let isComputed = defaultArg isComputed false
            MemberExpression(object, property, isComputed, loc)

        static member functionExpression
            (
                parameters,
                body,
                ?id,
                ?returnType,
                ?typeParameters,
                ?loc
            )
            = //?generator_, ?async_
            FunctionExpression(
                id,
                parameters,
                body,
                returnType,
                defaultArg typeParameters [||],
                loc
            )

        static member classExpression
            (
                body,
                ?id,
                ?superClass,
                ?typeParameters,
                ?implements,
                ?loc
            )
            =
            ClassExpression(
                body,
                id,
                superClass,
                defaultArg implements [||],
                defaultArg typeParameters [||],
                loc
            )

        static member spreadElement(argument, ?loc) =
            SpreadElement(argument, ?loc = loc)

        static member conditionalExpression
            (
                test,
                consequent,
                alternate,
                ?loc
            )
            : Expression
            =
            ConditionalExpression(test, consequent, alternate, loc)

        static member binaryExpression(operator, left, right, ?loc) =
            let operator =
                match operator with
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

        static member unaryExpression
            (
                operator: string,
                argument,
                ?isSuffix,
                ?loc
            )
            =
            UnaryExpression(argument, operator, defaultArg isSuffix false, loc)

        static member unaryExpression(operator, argument, ?isSuffix, ?loc) =
            let operator =
                match operator with
                | UnaryMinus -> "-"
                | UnaryPlus -> "+"
                | UnaryNot -> "!"
                | UnaryNotBitwise -> "~"
                | UnaryAddressOf -> "" //"&"

            UnaryExpression(argument, operator, defaultArg isSuffix false, loc)

        static member updateExpression
            (
                operator,
                prefix,
                argument,
                ?loc
            )
            : Expression
            =
            let operator =
                match operator with
                | UpdateMinus -> "--"
                | UpdatePlus -> "++"

            UpdateExpression(prefix, argument, operator, loc)

    type Identifier with

        member this.Name =
            let (Identifier(name = name)) = this
            name

        static member identifier(name, ?loc) : Identifier =
            Identifier(name, loc = loc)

    type Statement with

        static member blockStatement(body) =
            BlockStatement body |> Statement.BlockStatement

        static member returnStatement(argument, ?loc) : Statement =
            ReturnStatement(argument, loc)

        static member continueStatement(label, ?loc) =
            ContinueStatement(Some label, loc)

        static member tryStatement(block, ?handler, ?finalizer, ?loc) =
            TryStatement(block, handler, finalizer, loc)

        static member ifStatement
            (
                test,
                consequent,
                ?alternate,
                ?loc
            )
            : Statement
            =
            IfStatement(test, consequent, alternate, loc)

        /// Break can optional take a label of a loop to break
        static member breakStatement(?label, ?loc) = BreakStatement(label, loc)

        /// Statement (typically loop) prefixed with a label (for continue and break)
        static member labeledStatement(label, body) : Statement =
            LabeledStatement(body, label)

        static member whileStatement(test, body, ?loc) =
            WhileStatement(test, body, loc)

        static member debuggerStatement(?loc) = DebuggerStatement loc

        static member switchStatement(discriminant, cases, ?loc) =
            SwitchStatement(discriminant, cases, loc)

        static member variableDeclaration
            (
                kind,
                declarations,
                ?loc
            )
            : Statement
            =
            Declaration.variableDeclaration (kind, declarations, ?loc = loc)
            |> Declaration

        static member variableDeclaration
            (
                kind,
                var,
                ?annotation,
                ?typeParameters,
                ?init,
                ?loc
            )
            : Statement
            =
            Declaration.variableDeclaration (
                kind,
                var,
                ?annotation = annotation,
                ?typeParameters = typeParameters,
                ?init = init,
                ?loc = loc
            )
            |> Declaration

        static member forStatement(body, ?init, ?test, ?update, ?loc) =
            ForStatement(body, init, test, update, loc)

        static member throwStatement(argument, ?loc) =
            ThrowStatement(argument, loc)

    type BlockStatement with

        member this.Body =
            let (BlockStatement body) = this
            body

    type Program with

        member this.Body =
            let (Program body) = this
            body

    type CatchClause with

        static member catchClause(param, body, ?annotation, ?loc) =
            CatchClause(param, annotation, body, loc)

    type SwitchCase with

        static member switchCase(?test, ?body, ?loc) =
            SwitchCase(test, defaultArg body Array.empty, loc)

    type Parameter with

        static member parameter(name, ?typeAnnotation) =
            Parameter(
                name,
                typeAnnotation = typeAnnotation,
                flags = ParameterFlags()
            )

    type Declaration with

        static member variableDeclaration
            (
                kind,
                declarations,
                ?loc
            )
            : Declaration
            =
            VariableDeclaration.variableDeclaration (
                kind,
                declarations,
                ?loc = loc
            )
            |> Declaration.VariableDeclaration

        static member variableDeclaration
            (
                kind,
                var,
                ?annotation,
                ?typeParameters,
                ?init,
                ?loc
            )
            =
            VariableDeclaration.variableDeclaration (
                kind,
                var,
                ?annotation = annotation,
                ?typeParameters = typeParameters,
                ?init = init,
                ?loc = loc
            )
            |> Declaration.VariableDeclaration

        static member functionDeclaration
            (
                parameters,
                body,
                id,
                ?returnType,
                ?typeParameters,
                ?loc,
                ?doc
            )
            =
            FunctionDeclaration(
                parameters,
                body,
                id,
                returnType,
                defaultArg typeParameters [||],
                loc,
                doc
            )

        static member classDeclaration
            (
                body,
                ?id,
                ?superClass,
                ?typeParameters,
                ?implements,
                ?loc,
                ?doc
            )
            =
            ClassDeclaration(
                body,
                id,
                superClass,
                defaultArg implements [||],
                defaultArg typeParameters [||],
                loc,
                doc
            )

        static member interfaceDeclaration
            (
                id,
                body,
                ?extends,
                ?typeParameters
            )
            : Declaration
            = // ?mixins_,
            InterfaceDeclaration(
                id,
                body,
                defaultArg extends [||],
                defaultArg typeParameters [||]
            )

        static member enumDeclaration(name, cases, ?isConst) =
            EnumDeclaration(name, cases, defaultArg isConst false)

    type VariableDeclaration with

        static member variableDeclaration
            (
                kind,
                declarations,
                ?loc
            )
            : VariableDeclaration
            =
            VariableDeclaration(declarations, kind, loc)

        static member variableDeclaration
            (
                kind,
                var,
                ?annotation,
                ?typeParameters,
                ?init,
                ?loc
            )
            =
            VariableDeclaration.variableDeclaration (
                kind,
                [|
                    VariableDeclarator.variableDeclarator (
                        var,
                        ?annotation = annotation,
                        ?typeParameters = typeParameters,
                        ?init = init
                    )
                |],
                ?loc = loc
            )

    type VariableDeclarator with

        static member variableDeclarator
            (
                id,
                ?annotation,
                ?typeParameters,
                ?init,
                ?loc
            )
            =
            VariableDeclarator(
                id,
                annotation,
                defaultArg typeParameters [||],
                init,
                loc
            )

    type FunctionTypeParam with

        static member functionTypeParam(name, typeInfo, ?isOptional) =
            FunctionTypeParam(name, typeInfo, defaultArg isOptional false)

    type ClassMember with

        static member classMethod
            (
                kind,
                parameters,
                body,
                ?isStatic,
                ?isAbstract,
                ?returnType,
                ?typeParameters,
                ?loc,
                ?doc
            )
            : ClassMember
            =
            ClassMethod(
                kind,
                parameters,
                body,
                defaultArg isStatic false,
                defaultArg isAbstract false,
                returnType,
                defaultArg typeParameters [||],
                loc,
                doc
            )

        static member classProperty
            (
                key,
                ?value,
                ?isComputed,
                ?isStatic,
                ?isOptional,
                ?typeAnnotation,
                ?accessModifier,
                ?loc,
                ?doc
            )
            : ClassMember
            =
            let isComputed = defaultArg isComputed false

            ClassProperty(
                key,
                value,
                isComputed,
                defaultArg isStatic false,
                defaultArg isOptional false,
                typeAnnotation,
                accessModifier,
                loc,
                doc
            )

    type Literal with

        static member nullLiteral(?loc) = NullLiteral loc
        static member numericLiteral(value, ?loc) = NumericLiteral(value, loc)
        static member booleanLiteral(value, ?loc) = BooleanLiteral(value, loc)

        static member stringLiteral(value, ?loc) =
            StringLiteral(value, loc) |> Literal.StringLiteral

        static member regExpLiteral(pattern, flags, ?loc) =
            let flags =
                flags
                |> Seq.map (
                    function
                    | RegexGlobal -> "g"
                    | RegexUnicode -> "u"
                    | RegexIgnoreCase -> "i"
                    | RegexMultiline -> "m"
                    | RegexSingleline -> "s"
                    | RegexSticky -> "y"
                )
                |> Seq.fold (+) ""

            RegExp(pattern, flags, loc)

    type StringLiteral with

        static member stringLiteral(value, ?loc) = StringLiteral(value, loc)

    type ObjectMember with

        static member objectProperty(key, value, ?isComputed, ?doc) = // ?shorthand_,
            let isComputed = defaultArg isComputed false
            ObjectProperty(key, value, isComputed, doc)

        static member objectMethod
            (
                kind,
                key,
                parameters,
                body,
                ?isComputed,
                ?returnType,
                ?typeParameters,
                ?loc,
                ?doc
            )
            =
            let isComputed = defaultArg isComputed false

            ObjectMethod(
                kind,
                key,
                parameters,
                body,
                isComputed,
                returnType,
                defaultArg typeParameters [||],
                loc,
                doc
            )

    type AbstractMember with

        static member abstractProperty
            (
                key,
                typ,
                ?isComputed,
                ?isOptional,
                ?doc
            )
            =
            AbstractProperty(
                key,
                typ,
                defaultArg isComputed false,
                defaultArg isOptional false,
                doc
            )

        static member abstractMethod
            (
                kind,
                key,
                parameters,
                returnType,
                ?typeParameters,
                ?isComputed,
                ?doc
            )
            =
            AbstractMethod(
                kind,
                key,
                parameters,
                returnType,
                defaultArg typeParameters [||],
                defaultArg isComputed false,
                doc
            )

    type ModuleDeclaration with

        static member exportAllDeclaration(source, ?loc) =
            ExportAllDeclaration(source, loc)

        static member exportNamedReferences
            (
                specifiers,
                ?source
            )
            : ModuleDeclaration
            =
            ExportNamedReferences(specifiers, source)

    type TypeAnnotation with

        static member aliasTypeAnnotation(id, ?typeArguments) =
            AliasTypeAnnotation(id, defaultArg typeArguments [||])

        static member functionTypeAnnotation
            (
                parameters,
                returnType,
                ?spread
            )
            : TypeAnnotation
            =
            FunctionTypeAnnotation(parameters, returnType, spread)

    type TypeParameter with

        static member typeParameter(name, ?bound, ?``default``) =
            TypeParameter(name, bound, ``default``)
