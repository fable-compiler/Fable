// Loosely based on https://pub.dev/documentation/analyzer/latest/dart_ast_ast/dart_ast_ast-library.html
module rec Fable.AST.Dart

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

type Type =
    // Built in
    | Integer
    | Double
    | Boolean
    | String

    | Object
    | Dynamic
    | Void

    | List of Type
    | Nullable of Type

    | Generic of name: string
    | TypeReference of ref: Expression

type Ident =
    { Prefix: string option // Namespace
      Name: string
      Type: Type }

type Literal =
    | IntegerLiteral of value: int64
    | DoubleLiteral of value: double
    | BooleanLiteral of value: bool
    | StringLiteral of value: string
    | NullLiteral
    | ListLiteral of values: Expression list * isConst: bool

type Expression =
    | Literal of value: Literal
    | IdentExpression of ident: Ident
    | PropertyAccess of expr: Expression * prop: string
    // Dart AST says the index can be an expression but I've only seen ints
    | IndexExpression of expr: Expression * index: int
    | AsExpression of expr: Expression * typ: Type
    | IsExpression of expr: Expression * typ: Type * isNot: bool
    | InvocationExpression of expr: Expression * genArgs: Type list * args: Expression list
    | UnaryExpression of operator: UnaryOperator * expr: Expression
    | BinaryExpression of operator: BinaryOperator * left: Expression * right: Expression * isInt: bool
    | LogicalExpression of operator: LogicalOperator * left: Expression * right: Expression
    | ConditionalExpression of test: Expression * consequent: Expression * alternate: Expression
    | AnonymousFunction of args: Ident list * body: Choice<Statement list, Expression> * genericParams: string list //* returnType: Type
    | AssignmentExpression of target: Expression * kind: AssignmentOperator * value: Expression

    static member listLiteral(values, ?isConst) = ListLiteral(values, defaultArg isConst false) |> Literal
    static member integerLiteral(value) = IntegerLiteral value |> Literal
    static member integerLiteral(value: int) = IntegerLiteral value |> Literal
    static member doubleLiteral(value) = DoubleLiteral value |> Literal
    static member booleanLiteral(value) = BooleanLiteral value |> Literal
    static member stringLiteral(value) = StringLiteral value |> Literal
    static member nullLiteral() = NullLiteral |> Literal
    static member identExpression(ident) = IdentExpression(ident)
    static member indexExpression(expr, index) = IndexExpression(expr, index)
    static member propertyAccess(expr, prop) = PropertyAccess(expr, prop)
    static member asExpression(expr, typ) = AsExpression(expr, typ)
    static member isExpression(expr, typ, ?isNot) = IsExpression(expr, typ, defaultArg isNot false)
    static member invocationExpression(expr, genArgs, args) = InvocationExpression(expr, genArgs, args)
    static member unaryExpression(operator, expr) = UnaryExpression(operator, expr)
    static member binaryExpression(operator, left, right, ?isInt) = BinaryExpression(operator, left, right, defaultArg isInt false)
    static member logicalExpression(operator, left, right) = LogicalExpression(operator, left, right)
    static member conditionalExpression(test, consequent, alternate) = ConditionalExpression(test, consequent, alternate)
    static member anonymousFunction(args, body, genericParams) = AnonymousFunction(args, body, genericParams)
    static member assignmentExpression(target, kind, value) = AssignmentExpression(target, kind, value)

type VariableDeclarationKind =
    | Final
    | Const
    /// Variable is mutable but value is constant (union, list, immutable record...)
    | VarConst
    | Var

type SwitchCase(guards: Expression list, body: Statement list) =
    member _.Guards = guards
    member _.Body = body

type Statement =
    | SwitchStatement of discriminant: Expression * cases: SwitchCase list * defaultCase: Statement list option
    | ReturnStatement of Expression
    | BreakStatement of label: string option
    | ContinueStatement of label: string option
    | ExpressionStatement of Expression
    | LocalVariableDeclaration of ident: Ident * kind: VariableDeclarationKind * value: Expression option
    | LocalFunctionDeclaration of FunctionDeclaration
    | Label of label: string
    static member variableDeclaration(ident, ?kind, ?value) =
        LocalVariableDeclaration(ident, defaultArg kind Final, value)
    static member functionDeclaration(name: string, args: Ident list, body: Statement list, returnType: Type, ?genParams: string list) =
        LocalFunctionDeclaration {
            Name = name
            Args = args
            Body = body
            ReturnType = returnType
            GenericParams = defaultArg genParams []
        }
    static member switchStatement(discriminant, cases, defaultCase) =
        SwitchStatement(discriminant, cases, defaultCase)

type FunctionDeclaration =
    {
        Name: string
        Args: Ident list
        Body: Statement list
        GenericParams: string list
        ReturnType: Type
    }

type ConstructorDeclaration =
    {
        Args: Ident list
        Body: Statement list
        // GenericParams: string list
        SuperArgs: Ident list
    }

type MemberKind =
    | IsMethod
    | IsGetter
    | IsSetter

type AbstractMemberDeclaration =
    {
        Name: string
        Args: Ident list
        GenericParams: string list
        ReturnType: Type
    }

type ClassDeclaration =
    {
        Name: string
        IsAbstract: bool
        Extends: Ident option
        Constructor: ConstructorDeclaration option
        Members: (FunctionDeclaration * MemberKind) list
        AbstractMembers: (AbstractMemberDeclaration * MemberKind) list
    }

type Declaration =
    | ClassDeclaration of ClassDeclaration
    | VariableDeclaration of ident: Ident * kind: VariableDeclarationKind * value: Expression
    | FunctionDeclaration of FunctionDeclaration

    static member variableDeclaration(ident, kind, value) =
        VariableDeclaration(ident, kind, value)

    static member functionDeclaration(name: string, args: Ident list, body: Statement list, returnType: Type, ?genParams: string list) =
        FunctionDeclaration {
            Name = name
            Args = args
            Body = body
            ReturnType = returnType
            GenericParams = defaultArg genParams []
        }

    static member constructorDeclaration(?args: Ident list, ?body: Statement list, ?superArgs: Ident list) =
        {
            Args = defaultArg args []
            Body = defaultArg body []
            SuperArgs = defaultArg superArgs []
        }

    static member abstractMemberDeclaration(name: string, args: Ident list, returnType: Type, ?genParams: string list) =
        {
            Name = name
            Args = args
            ReturnType = returnType
            GenericParams = defaultArg genParams []
        }

    static member classDeclaration(name, ?extends, ?isAbstract, ?constructor, ?members, ?abstractMembers) =
        ClassDeclaration {
            Name = name
            IsAbstract = defaultArg isAbstract false
            Extends = extends
            Constructor = constructor
            Members = defaultArg members []
            AbstractMembers = defaultArg abstractMembers []
        }

type Import =
  { LocalIdent: string option
    Path: string }

type File =
    { Imports: Import list
      Declarations: Declaration list }
