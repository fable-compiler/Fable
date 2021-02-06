// fsharplint:disable MemberNames InterfaceNames
namespace rec Fable.AST.Babel

open Fable.AST

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

/// The type field is a string representing the AST variant type.
/// Each subtype of Node is documented below with the specific string of its type field.
/// You can use this field to determine which interface a node implements.
/// The loc field represents the source location information of the node.
/// If the node contains no information about the source location, the field is null;
/// otherwise it is an object consisting of a start position (the position of the first character of the parsed source region)
/// and an end position (the position of the first character after the parsed source region):
type Node =
    | Pattern of Pattern
    | Program of Program
    | Statement of Statement
    | Directive of Directive
    | ClassBody of ClassBody
    | Expression of Expression
    | SwitchCase of SwitchCase
    | CatchClause of CatchClause
    | ObjectMember of ObjectMember
    | TypeParameter of TypeParameter
    | TypeAnnotation of TypeAnnotation
    | ExportSpecifier of ExportSpecifier
    | ImportSpecifier of ImportSpecifier
    | InterfaceExtends of InterfaceExtends
    | ObjectTypeIndexer of ObjectTypeIndexer
    | FunctionTypeParam of FunctionTypeParam
    | ModuleDeclaration of ModuleDeclaration
    | VariableDeclarator of VariableDeclarator
    | TypeAnnotationInfo of TypeAnnotationInfo
    | ObjectTypeProperty of ObjectTypeProperty
    | ObjectTypeCallProperty of ObjectTypeCallProperty
    | ObjectTypeInternalSlot of ObjectTypeInternalSlot
    | TypeParameterDeclaration of TypeParameterDeclaration
    | TypeParameterInstantiation of TypeParameterInstantiation

/// Since the left-hand side of an assignment may be any expression in general, an expression can also be a pattern.
type Expression =
    | Literal of Literal
    | Identifier of Identifier
    | Super of loc: SourceLocation option
    | Undefined of Loc: SourceLocation option
    | NewExpression of NewExpression
    | SpreadElement of SpreadElement
    | ThisExpression of loc: SourceLocation option
    | CallExpression of callee: Expression * arguments: Expression array * loc: SourceLocation option
    | ArrayExpression of ArrayExpression
    | ClassExpression of ClassExpression
    | ClassImplements of ClassImplements
    | UnaryExpression of UnaryExpression
    | UpdateExpression of UpdateExpression
    | ObjectExpression of properties: ObjectMember array * loc: SourceLocation option
    | BinaryExpression of BinaryExpression
    | MemberExpression of MemberExpression
    | LogicalExpression of left: Expression * operator: string * right: Expression * loc: SourceLocation option
    | SequenceExpression of expressions: Expression array * loc: SourceLocation option
    | FunctionExpression of FunctionExpression
    | AssignmentExpression of left: Expression * right: Expression * operator: string * loc: SourceLocation option
    | ConditionalExpression of ConditionalExpression
    | ArrowFunctionExpression of ArrowFunctionExpression
    | EmitExpression of value: string * args: Expression array * loc: SourceLocation option

    static member super(?loc) = Super loc
    static member emitExpression(value, args, ?loc) = EmitExpression(value, args, loc)
    static member nullLiteral(?loc) = NullLiteral loc |> Literal
    static member numericLiteral(value, ?loc) = NumericLiteral (value, loc) |> Literal
    static member booleanLiteral(value, ?loc) = BooleanLiteral (value, loc) |> Literal
    static member stringLiteral(value, ?loc) = Literal.stringLiteral (value, ?loc=loc) |> Literal
    static member binaryExpression(operator_, left, right, ?loc) =
        BinaryExpression.Create(operator_, left, right, ?loc=loc)
        |> BinaryExpression
    static member arrayExpression(elements, ?loc) = ArrayExpression.Create(elements, ?loc=loc) |> ArrayExpression
    static member unaryExpression(operator_, argument, ?loc) =
        UnaryExpression.Create(operator_, argument, ?loc=loc)
        |> UnaryExpression
    static member identifier(name, ?optional, ?typeAnnotation, ?loc): Expression =
        Identifier.Create(name, ?optional = optional, ?typeAnnotation = typeAnnotation, ?loc = loc)
        |> Identifier
    static member regExpLiteral(pattern, flags_, ?loc) : Expression =
        Literal.regExpLiteral(pattern, flags_, ?loc=loc) |> Literal
    /// A function or method call expression.
    static member callExpression(callee, arguments, ?loc) = CallExpression(callee, arguments, loc)
    static member assignmentExpression(operator_, left, right, ?loc): Expression =
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

type Pattern =
    | IdentifierPattern of Identifier
    | RestElement of RestElement

    member this.Name =
        match this with
        | IdentifierPattern(id) -> id.Name
        | RestElement(el) -> el.Name

    static member identifier(name, ?optional, ?typeAnnotation, ?loc) =
        Identifier.Create(name, ?optional = optional, ?typeAnnotation = typeAnnotation, ?loc = loc)
        |> IdentifierPattern
    static member restElement(argument: Pattern, ?typeAnnotation, ?loc) =
        RestElement.Create(argument, ?typeAnnotation=typeAnnotation, ?loc=loc) |> RestElement

type Literal =
    | RegExp of pattern: string * flags: string * loc: SourceLocation option
    | NullLiteral of loc: SourceLocation option
    | StringLiteral of StringLiteral
    | BooleanLiteral of value: bool * loc: SourceLocation option
    | NumericLiteral of value: float * loc: SourceLocation option
    | DirectiveLiteral of DirectiveLiteral

    static member nullLiteral(?loc) = NullLiteral loc
    static member numericLiteral(value, ?loc) = NumericLiteral (value, loc)
    static member booleanLiteral(value, ?loc) = BooleanLiteral (value, loc)
    static member stringLiteral(value, ?loc) = StringLiteral.Create (value, ?loc=loc) |> StringLiteral
    static member regExpLiteral(pattern, flags, ?loc) =
        let flags =
            flags |> Seq.map (function
                | RegexGlobal -> "g"
                | RegexIgnoreCase -> "i"
                | RegexMultiline -> "m"
                | RegexSticky -> "y") |> Seq.fold (+) ""
        RegExp(pattern, flags, loc)

type Statement =
    | Declaration of Declaration
    | IfStatement of test: Expression * consequent: BlockStatement * alternate: Statement option * loc: SourceLocation option
    | TryStatement of block: BlockStatement * handler: CatchClause option * finalizer: BlockStatement option * loc: SourceLocation option
    | ForStatement of body: BlockStatement * init: VariableDeclaration option * test: Expression option * update: Expression option * loc: SourceLocation option
    | BreakStatement of label: Identifier option * loc: SourceLocation option
    | WhileStatement of test: Expression * body: BlockStatement * loc: SourceLocation option
    | ThrowStatement of argument: Expression * loc: SourceLocation option
    | BlockStatement of BlockStatement
    | ReturnStatement of argument: Expression * loc: SourceLocation option
    | SwitchStatement of discriminant: Expression * cases: SwitchCase array * loc: SourceLocation option
    | LabeledStatement of body: Statement * label: Identifier
    | DebuggerStatement of loc: SourceLocation option
    | ContinueStatement of label: Identifier option * loc: SourceLocation option
    | ExpressionStatement of expr: Expression /// An expression statement, i.e., a statement consisting of a single expression.

    static member returnStatement(argument, ?loc) = ReturnStatement(argument, loc)
    static member continueStatement(label, ?loc) = ContinueStatement(Some label, loc)
    static member tryStatement(block, ?handler, ?finalizer, ?loc) = TryStatement(block, handler, finalizer, loc)
    static member ifStatement(test, consequent, ?alternate, ?loc): Statement = IfStatement(test, consequent, alternate, loc)
    static member blockStatement(body) = BlockStatement.Create(body) |> BlockStatement
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

/// Note that declarations are considered statements; this is because declarations can appear in any statement context.
type Declaration =
    | ClassDeclaration of ClassDeclaration
    | VariableDeclaration of VariableDeclaration
    | FunctionDeclaration of FunctionDeclaration
    | InterfaceDeclaration of InterfaceDeclaration

    static member variableDeclaration(kind, declarations, ?loc): Declaration =
        VariableDeclaration.Create(kind, declarations, ?loc = loc)
        |> VariableDeclaration
    static member variableDeclaration(var, ?init, ?kind, ?loc): Declaration =
        Declaration.variableDeclaration(
            defaultArg kind Let,
            [| VariableDeclarator.Create(var, ?init = init) |],
            ?loc = loc
        )

/// A module import or export declaration.
type ModuleDeclaration =
    | ImportDeclaration of ImportDeclaration
    | ExportAllDeclaration of source: Literal * loc: SourceLocation option
    | ExportNamedReferences of ExportNamedReferences
    | ExportNamedDeclaration of ExportNamedDeclaration
    | PrivateModuleDeclaration of PrivateModuleDeclaration
    | ExportDefaultDeclaration of ExportDefaultDeclaration

    /// An export batch declaration, e.g., export * from "mod";.
    static member exportAllDeclaration(source, ?loc) = ExportAllDeclaration(source, loc)

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
    { Name: string
      Optional: bool option
      TypeAnnotation: TypeAnnotation option
      Loc: SourceLocation option }

    static member Create(name, ?optional, ?typeAnnotation, ?loc): Identifier =
        { Name = name
          Optional = optional
          TypeAnnotation = typeAnnotation
          Loc = loc }

// Literals

type StringLiteral =
    { Value: string
      Loc: SourceLocation option }

    static member Create(value, ?loc) =
        { Value = value
          Loc = loc }

// Misc
//type Decorator(value, ?loc) =
//    inherit Node("Decorator", ?loc = loc)
//    member _.Value = value
//
type DirectiveLiteral =
    { Value: string }

    static member Create(value) = { Value = value }

/// e.g. "use strict";
type Directive =
    { Value: DirectiveLiteral }

    static member Create(value) = { Value = value }

// Program

/// A complete program source tree.
/// Parsers must specify sourceType as "module" if the source has been parsed as an ES6 module.
/// Otherwise, sourceType must be "script".
type Program =
    { Body: ModuleDeclaration array }

    static member Create(body) = // ?directives_,
        { Body = body }

//    let sourceType = "module" // Don't use "script"
//    member _.Directives: Directive array = directives
//    member _.SourceType: string = sourceType

// Statements

/// A block statement, i.e., a sequence of statements surrounded by braces.
type BlockStatement =
    { Body: Statement array }

    static member Create(body) = // ?directives_,
        { Body = body }


//    let directives = [||] // defaultArg directives_ [||]
//    member _.Directives: Directive array = directives

/// An empty statement, i.e., a solitary semicolon.
//type EmptyStatement(?loc) =
//    inherit Statement("EmptyStatement", ?loc = loc)
//    member _.Print(_) = ()

// type WithStatement

// Control Flow

/// A case (if test is an Expression) or default (if test === null) clause in the body of a switch statement.
type SwitchCase =
    { Test: Expression option
      Consequent: Statement array
      Loc: SourceLocation option }

    static member Create(consequent, ?test, ?loc) =
        { Test = test
          Consequent = consequent
          Loc = loc }


// Exceptions

/// A catch clause following a try block.
type CatchClause =
    { Param: Pattern
      Body: BlockStatement
      Loc: SourceLocation option }

    static member Create(param, body, ?loc) =
        { Param = param
          Body = body
          Loc = loc }

// Declarations
type VariableDeclarator =
    { Id: Pattern
      Init: Expression option }

    static member Create(id, ?init) = { Id = id; Init = init }

type VariableDeclarationKind =
    | Var
    | Let
    | Const

type VariableDeclaration =
    { Declarations: VariableDeclarator array
      Kind: string
      Loc: SourceLocation option }

    static member Create(kind, declarations, ?loc) =
        let kind =
            match kind with
            | Var -> "var"
            | Let -> "let"
            | Const -> "const"

        { Declarations = declarations
          Kind = kind
          Loc = loc }

    static member Create(var, ?init, ?kind, ?loc) =
        VariableDeclaration.Create(defaultArg kind Let, [| VariableDeclarator.Create(var, ?init = init) |], ?loc = loc)


// Loops

//type DoWhileStatement(body, test, ?loc) =
//    inherit Statement("DoWhileStatement", ?loc = loc)
//    member _.Body: BlockStatement = body
//    member _.Test: Expression = test

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
type FunctionDeclaration =
    { Params: Pattern array
      Body: BlockStatement
      Id: Identifier
      ReturnType: TypeAnnotation option
      TypeParameters: TypeParameterDeclaration option
      Loc: SourceLocation option }

    static member AsDeclaration(``params``, body, id, ?returnType, ?typeParameters, ?loc): Declaration = // ?async_, ?generator_, ?declare,
        { Params = ``params``
          Body = body
          Id = id
          ReturnType = returnType
          TypeParameters = typeParameters
          Loc = loc }
        |> FunctionDeclaration
    //    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator
//    member _.Declare: bool option = declare

// Expressions


/// A fat arrow function expression, e.g., let foo = (bar) => { /* body */ }.
type ArrowFunctionExpression =
    { Params: Pattern array
      Body: BlockStatement
      ReturnType: TypeAnnotation option
      TypeParameters: TypeParameterDeclaration option
      Loc: SourceLocation option }

    static member AsExpr(``params``, body: BlockStatement, ?returnType, ?typeParameters, ?loc): Expression = //?async_, ?generator_,
        { Params = ``params``
          Body = body
          ReturnType = returnType
          TypeParameters = typeParameters
          Loc = loc }
        |> ArrowFunctionExpression

    static member AsExpr(``params``, body: Expression, ?returnType, ?typeParameters, ?loc): Expression =
        let body = { Body = [| Statement.returnStatement(body) |] }
        ArrowFunctionExpression.AsExpr(``params``, body, ?returnType = returnType, ?typeParameters = typeParameters, ?loc = loc)

//    let async = defaultArg async_ false
//    let generator = defaultArg generator_ false
//    member _.Async: bool = async
//    member _.Generator: bool = generator
type FunctionExpression =
    { Id: Identifier option
      Params: Pattern array
      Body: BlockStatement
      ReturnType: TypeAnnotation option
      TypeParameters: TypeParameterDeclaration option
      Loc: SourceLocation option }

    static member AsExpr(``params``, body, ?id, ?returnType, ?typeParameters, ?loc): Expression = //?generator_, ?async_
        { Id = id
          Params = ``params``
          Body = body
          ReturnType = returnType
          TypeParameters = typeParameters
          Loc = loc }
        |> FunctionExpression

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

// Should derive from Node, but make it an expression for simplicity
type SpreadElement =
    { Argument: Expression
      Loc: SourceLocation option }

    static member AsExpr(argument, ?loc): Expression =
        { Argument = argument; Loc = loc }
        |> SpreadElement

type ArrayExpression =
    { // Elements: Choice<Expression, SpreadElement> option array
      Elements: Expression array
      Loc: SourceLocation option }

    static member Create(elements, ?loc) =
        { // Elements: Choice<Expression, SpreadElement> option array
          Elements = elements
          Loc = loc }

type ObjectMember =
    | ObjectProperty of key: Expression * value: Expression * computed: bool
    | ObjectMethod of ObjectMethod

    static member objectProperty(key, value, ?computed_): ObjectMember = // ?shorthand_,
        let computed = defaultArg computed_ false
        ObjectProperty(key, value, computed)
//    let shorthand = defaultArg shorthand_ false
//    member _.Shorthand: bool = shorthand

type ObjectMethodKind = ObjectGetter | ObjectSetter | ObjectMeth

type ObjectMethod =
    { Kind: string
      Key: Expression
      Params: Pattern array
      Body: BlockStatement
      Computed: bool
      ReturnType: TypeAnnotation option
      TypeParameters: TypeParameterDeclaration option
      Loc: SourceLocation option }

    static member AsObjectMember(kind_, key, ``params``, body, ?computed_, ?returnType, ?typeParameters, ?loc) : ObjectMember = // ?async_, ?generator_,
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
        { Kind = kind
          Key = key
          Params = ``params``
          Body = body
          Computed = computed
          ReturnType = returnType
          TypeParameters = typeParameters
          Loc = loc }
        |> ObjectMethod

/// If computed is true, the node corresponds to a computed (a[b]) member expression and property is an Expression.
/// If computed is false, the node corresponds to a static (a.b) member expression and property is an Identifier.
type MemberExpression =
    { Name: string
      Object: Expression
      Property: Expression
      Computed: bool
      Loc: SourceLocation option }

    static member AsExpr(object, property, ?computed_, ?loc) : Expression =
        let computed = defaultArg computed_ false
        let name =
            match property with
            | Identifier(id) -> id.Name
            | _ -> ""

        { Name = name
          Object = object
          Property = property
          Computed = computed
          Loc = loc }
        |> MemberExpression



/// A conditional expression, i.e., a ternary ?/: expression.
type ConditionalExpression =
    { Test: Expression
      Consequent: Expression
      Alternate: Expression
      Loc: SourceLocation option }

    static member AsExpr(test, consequent, alternate, ?loc): Expression =
        { Test = test
          Consequent = consequent
          Alternate = alternate
          Loc = loc }
        |> ConditionalExpression


type NewExpression =
    { Callee: Expression
      // Arguments: Choice<Expression, SpreadElement> array = arguments
      Arguments: Expression array
      TypeArguments: TypeParameterInstantiation option
      Loc: SourceLocation option }

    static member AsExpr(callee, arguments, ?typeArguments, ?loc): Expression =
        { Callee = callee
          Arguments = arguments
          TypeArguments = typeArguments
          Loc = loc }
        |> NewExpression


// Unary Operations
type UnaryExpression =
    { Prefix: bool
      Argument: Expression
      Operator: string
      Loc: SourceLocation option }

    static member Create(operator_, argument, ?loc) =
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

        { Prefix = prefix
          Argument = argument
          Operator = operator
          Loc = loc }

type UpdateExpression =
    { Prefix: bool
      Argument: Expression
      Operator: string
      Loc: SourceLocation option }

    static member AsExpr(operator_, prefix, argument, ?loc) : Expression =
        let operator =
            match operator_ with
            | UpdateMinus -> "--"
            | UpdatePlus -> "++"

        { Prefix = prefix
          Argument = argument
          Operator = operator
          Loc = loc }
        |> UpdateExpression

// Binary Operations
type BinaryExpression =
    { Left: Expression
      Right: Expression
      Operator: string
      Loc: SourceLocation option }

    static member Create(operator_, left, right, ?loc) =
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

        { Left = left
          Right = right
          Operator = operator
          Loc = loc }
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

type RestElement =
    { Name: string
      Argument: Pattern
      TypeAnnotation: TypeAnnotation option
      Loc: SourceLocation option }

    static member Create(argument: Pattern, ?typeAnnotation, ?loc) =
        { Name = argument.Name
          Argument = argument
          TypeAnnotation = typeAnnotation
          Loc = loc }

// Classes
type ClassMember =
    | ClassMethod of ClassMethod
    | ClassProperty of ClassProperty

type ClassMethodKind =
    | ClassImplicitConstructor | ClassFunction | ClassGetter | ClassSetter

type ClassMethod =
    { Kind: string
      Key: Expression
      Params: Pattern array
      Body: BlockStatement
      Computed: bool
      Static: bool option
      Abstract: bool option
      ReturnType: TypeAnnotation option
      TypeParameters: TypeParameterDeclaration option
      Loc: SourceLocation option }

    static member AsClassMember(kind_, key, ``params``, body, ?computed_, ?``static``, ?``abstract``, ?returnType, ?typeParameters, ?loc) : ClassMember =
        let kind =
            match kind_ with
            | ClassImplicitConstructor -> "constructor"
            | ClassGetter -> "get"
            | ClassSetter -> "set"
            | ClassFunction -> "method"
        let computed = defaultArg computed_ false

        { Kind = kind
          Key = key
          Params = ``params``
          Body = body
          Computed = computed
          Static = ``static``
          Abstract = ``abstract``
          ReturnType = returnType
          TypeParameters = typeParameters
          Loc = loc }
        |> ClassMethod
    // This appears in astexplorer.net but it's not documented
    // member _.Expression: bool = false

/// ES Class Fields & Static Properties
/// https://github.com/jeffmo/es-class-fields-and-static-properties
/// e.g, class MyClass { static myStaticProp = 5; myProp /* = 10 */; }
type ClassProperty =
    { Key: Expression
      Value: Expression option
      Computed: bool
      Static: bool
      Optional: bool
      TypeAnnotation: TypeAnnotation option
      Loc: SourceLocation option }

    static member AsClassMember(key, ?value, ?computed_, ?``static``, ?optional, ?typeAnnotation, ?loc): ClassMember =
        let computed = defaultArg computed_ false

        { Key = key
          Value = value
          Computed = computed
          Static = defaultArg ``static`` false
          Optional = defaultArg optional false
          TypeAnnotation = typeAnnotation
          Loc = loc }
        |> ClassProperty

type ClassImplements =
    { Id: Identifier
      TypeParameters: TypeParameterInstantiation option }

    static member Create(id, ?typeParameters) =
        { Id = id
          TypeParameters = typeParameters }

type ClassBody =
    { Body: ClassMember array
      Loc: SourceLocation option }

    static member Create(body, ?loc) =
        { Body = body
          Loc = loc }

type ClassDeclaration =
    { Body: ClassBody
      Id: Identifier option
      SuperClass: Expression option
      Implements: ClassImplements array option
      SuperTypeParameters: TypeParameterInstantiation option
      TypeParameters: TypeParameterDeclaration option
      Loc: SourceLocation option }

    static member AsDeclaration(body, ?id, ?superClass, ?superTypeParameters, ?typeParameters, ?implements, ?loc) : Declaration =
        { Body = body
          Id = id
          SuperClass = superClass
          Implements = implements
          SuperTypeParameters = superTypeParameters
          TypeParameters = typeParameters
          Loc = loc }
        |> ClassDeclaration

/// Anonymous class: e.g., var myClass = class { }
type ClassExpression =
    { Body: ClassBody
      Id: Identifier option
      SuperClass: Expression option
      Implements: ClassImplements array option
      SuperTypeParameters: TypeParameterInstantiation option
      TypeParameters: TypeParameterDeclaration option
      Loc: SourceLocation option }

    static member AsExpr(body, ?id, ?superClass, ?superTypeParameters, ?typeParameters, ?implements, ?loc): Expression =
        { Body = body
          Id = id
          SuperClass = superClass
          Implements = implements
          SuperTypeParameters = superTypeParameters
          TypeParameters = typeParameters
          Loc = loc }
        |> ClassExpression

// type MetaProperty(meta, property, ?loc) =
//     interface Expression with
//     member _.Meta: Identifier = meta
//     member _.Property: Expression = property

// Modules
type PrivateModuleDeclaration =
    { Statement: Statement }

    static member AsModuleDeclaration(statement): ModuleDeclaration =
        { Statement = statement }
        |> PrivateModuleDeclaration

/// An imported variable binding, e.g., {foo} in import {foo} from "mod" or {foo as bar} in import {foo as bar} from "mod".
/// The imported field refers to the name of the export imported from the module.
/// The local field refers to the binding imported into the local module scope.
/// If it is a basic named import, such as in import {foo} from "mod", both imported and local are equivalent Identifier nodes; in this case an Identifier node representing foo.
/// If it is an aliased import, such as in import {foo as bar} from "mod", the imported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ImportSpecifier =
    | ImportMemberSpecifier of local: Identifier * imported: Identifier
    | ImportDefaultSpecifier of local: Identifier
    | ImportNamespaceSpecifier of ImportNamespaceSpecifier


/// A default import specifier, e.g., foo in import foo from "mod".

/// A namespace import specifier, e.g., * as foo in import * as foo from "mod".
type ImportNamespaceSpecifier =
    { Local: Identifier }

    static member AsImportSpecifier(local): ImportSpecifier = { Local = local } |> ImportNamespaceSpecifier

/// e.g., import foo from "mod";.
type ImportDeclaration =
    { Specifiers: ImportSpecifier array
      Source: StringLiteral }

    static member AsModuleDeclaration(specifiers, source): ModuleDeclaration =
        { Specifiers = specifiers
          Source = source }
        |> ImportDeclaration

/// An exported variable binding, e.g., {foo} in export {foo} or {bar as foo} in export {bar as foo}.
/// The exported field refers to the name exported in the module.
/// The local field refers to the binding into the local module scope.
/// If it is a basic named export, such as in export {foo}, both exported and local are equivalent Identifier nodes;
/// in this case an Identifier node representing foo. If it is an aliased export, such as in export {bar as foo},
/// the exported field is an Identifier node representing foo, and the local field is an Identifier node representing bar.
type ExportSpecifier =
    { Local: Identifier
      Exported: Identifier }

    static member Create(local, exported) = { Local = local; Exported = exported }

/// An export named declaration, e.g., export {foo, bar};, export {foo} from "mod"; or export var foo = 1;.
/// Note: Having declaration populated with non-empty specifiers or non-null source results in an invalid state.
type ExportNamedDeclaration =
    { Declaration: Declaration }

    static member AsModuleDeclaration(declaration): ModuleDeclaration =
        { Declaration = declaration }
        |> ExportNamedDeclaration

type ExportNamedReferences =
    { Specifiers: ExportSpecifier array
      Source: StringLiteral option }

    static member AsModuleDeclaration(specifiers, ?source): ModuleDeclaration =
        { Specifiers = specifiers
          Source = source }
        |> ExportNamedReferences

/// An export default declaration, e.g., export default function () {}; or export default 1;.
type ExportDefaultDeclaration =
    { Declaration: Choice<Declaration, Expression> }

    static member AsModuleDeclaration(declaration): ModuleDeclaration =
        { Declaration = declaration }
        |> ExportDefaultDeclaration


// Type Annotations
type TypeAnnotationInfo =
    | StringTypeAnnotation
    | NumberTypeAnnotation
    | TypeAnnotationInfo of TypeAnnotationInfo
    | BooleanTypeAnnotation
    | AnyTypeAnnotation
    | VoidTypeAnnotation
    | TupleTypeAnnotation of types: TypeAnnotationInfo array
    | UnionTypeAnnotation of UnionTypeAnnotation
    | FunctionTypeAnnotation of FunctionTypeAnnotation
    | NullableTypeAnnotation of NullableTypeAnnotation
    | GenericTypeAnnotation of GenericTypeAnnotation
    | ObjectTypeAnnotation of ObjectTypeAnnotation


type TypeAnnotation =
    { TypeAnnotation: TypeAnnotationInfo }

    static member Create(typeAnnotation) = { TypeAnnotation = typeAnnotation }

type TypeParameter =
    { Name: string
      Bound: TypeAnnotation option
      Default: TypeAnnotationInfo option }

    static member Create(name, ?bound, ?``default``) =
        { Name = name
          Bound = bound
          Default = ``default`` }

type TypeParameterDeclaration =
    { Params: TypeParameter array }

    static member Create(``params``) = { Params = ``params`` }


type TypeParameterInstantiation =
    { Params: TypeAnnotationInfo array }

    static member Create(``params``) = { Params = ``params`` }


type UnionTypeAnnotation =
    { Types: TypeAnnotationInfo array }

    static member AsTypeAnnotationInfo(types): TypeAnnotationInfo = { Types = types } |> UnionTypeAnnotation

type FunctionTypeParam =
    { Name: Identifier
      TypeAnnotation: TypeAnnotationInfo
      Optional: bool option }

    static member Create(name, typeInfo, ?optional) =
        { Name = name
          TypeAnnotation = typeInfo
          Optional = optional }

type FunctionTypeAnnotation =
    { Params: FunctionTypeParam array
      ReturnType: TypeAnnotationInfo
      TypeParameters: TypeParameterDeclaration option
      Rest: FunctionTypeParam option }

    static member AsTypeAnnotationInfo(``params``, returnType, ?typeParameters, ?rest): TypeAnnotationInfo =
        { Params = ``params``
          ReturnType = returnType
          TypeParameters = typeParameters
          Rest = rest }
        |> FunctionTypeAnnotation

type NullableTypeAnnotation =
    { TypeAnnotation: TypeAnnotationInfo }

    static member AsTypeAnnotationInfo(``type``): TypeAnnotationInfo =
        { TypeAnnotation = ``type`` }
        |> NullableTypeAnnotation

type GenericTypeAnnotation =
    { Id: Identifier
      TypeParameters: TypeParameterInstantiation option }

    static member AsTypeAnnotationInfo(id, ?typeParameters): TypeAnnotationInfo =
        { Id = id
          TypeParameters = typeParameters }
        |> GenericTypeAnnotation

type ObjectTypeProperty =
    { Key: Expression
      Value: TypeAnnotationInfo
      Kind: string option
      Computed: bool
      Static: bool
      Optional: bool
      Proto: bool
      Method: bool }

    static member Create(key, value, ?computed_, ?kind, ?``static``, ?optional, ?proto, ?method) =
        let computed = defaultArg computed_ false

        { Key = key
          Value = value
          Kind = kind
          Computed = computed
          Static = defaultArg ``static`` false
          Optional = defaultArg optional false
          Proto = defaultArg proto false
          Method = defaultArg method false }

type ObjectTypeIndexer =
    { Id: Identifier option
      Key: Identifier
      Value: TypeAnnotationInfo
      Static: bool option }

    static member Create(key, value, ?id, ?``static``): Node =
        { Id = id
          Key = key
          Value = value
          Static = ``static`` }
        |> ObjectTypeIndexer

type ObjectTypeCallProperty =
    { Value: TypeAnnotationInfo
      Static: bool option }

    static member Create(value, ?``static``) = { Value = value; Static = ``static`` }

type ObjectTypeInternalSlot =
    { Id: Identifier
      Value: TypeAnnotationInfo
      Optional: bool
      Static: bool
      Method: bool }

    static member Create(id, value, optional, ``static``, method) =
        { Id = id
          Value = value
          Optional = optional
          Static = ``static``
          Method = method }
type ObjectTypeAnnotation =
    { Properties: ObjectTypeProperty array
      Indexers: ObjectTypeIndexer array
      CallProperties: ObjectTypeCallProperty array
      InternalSlots: ObjectTypeInternalSlot array
      Exact: bool }

    static member Create(properties, ?indexers_, ?callProperties_, ?internalSlots_, ?exact_) =
        let exact = defaultArg exact_ false
        let indexers = defaultArg indexers_ [||]
        let callProperties = defaultArg callProperties_ [||]
        let internalSlots = defaultArg internalSlots_ [||]

        { Properties = properties
          Indexers = indexers
          CallProperties = callProperties
          InternalSlots = internalSlots
          Exact = exact }

type InterfaceExtends =
    { Id: Identifier
      TypeParameters: TypeParameterInstantiation option }

    static member Create(id, ?typeParameters) =
        { Id = id
          TypeParameters = typeParameters }

type InterfaceDeclaration =
    { Id: Identifier
      Body: ObjectTypeAnnotation
      Extends: InterfaceExtends array
      Implements: ClassImplements array
      TypeParameters: TypeParameterDeclaration option }

    static member Create(id, body, ?extends_, ?typeParameters, ?implements_): Declaration = // ?mixins_,
        let extends = defaultArg extends_ [||]
        let implements = defaultArg implements_ [||]

        { Id = id
          Body = body
          Extends = extends
          Implements = implements
          TypeParameters = typeParameters }
        |> InterfaceDeclaration

//    let mixins = defaultArg mixins_ [||]
//    member _.Mixins: InterfaceExtends array = mixins
