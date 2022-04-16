// Loosely based on https://pub.dev/documentation/analyzer/latest/dart_ast_ast/dart_ast_ast-library.html
module rec Fable.AST.Dart

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

type Type =
    // Built in
    | Integer
    | Double
    | Boolean
    | String

    | Object
    | Dynamic
    | Void
    | MetaType

    | List of Type
    | Nullable of Type

    | Generic of name: string
    | TypeReference of Ident * generics: Type list
    | Function of argTypes: Type list * returnType: Type

type Ident =
    { Prefix: string option // Namespace
      Name: string
      Type: Type }
    member this.Expr =
        IdentExpression this

type Literal =
    | IntegerLiteral of value: int64
    | DoubleLiteral of value: double
    | BooleanLiteral of value: bool
    | StringLiteral of value: string
    | NullLiteral
    | ListLiteral of values: Expression list * typ: Type * isConst: bool

type Annotation = Ident * Literal list

type CallArg = string option * Expression

type Expression =
    // Pseudo-expression so we can flatten nested sequence expressions before printing
    | SequenceExpression of seqExprFn: Ident * exprs: Expression list
    | SuperExpression
    | ThisExpression
    | Literal of value: Literal
    | InterpolationString of parts: string list * values: Expression list
    // Dart AST doesn't include TypeLiteral with the other literals
    | TypeLiteral of value: Type
    | IdentExpression of ident: Ident
    | PropertyAccess of expr: Expression * prop: string * isConst: bool
    | IndexExpression of expr: Expression * index: Expression
    | AsExpression of expr: Expression * typ: Type
    | IsExpression of expr: Expression * typ: Type * isNot: bool
    | InvocationExpression of expr: Expression * genArgs: Type list * args: CallArg list * isConst: bool
    | NotNullAssert of expr: Expression
    | UpdateExpression of operator: UpdateOperator * isPrefix: bool * expr: Expression
    | UnaryExpression of operator: UnaryOperator * expr: Expression
    | BinaryExpression of operator: BinaryOperator * left: Expression * right: Expression * isInt: bool
    | LogicalExpression of operator: LogicalOperator * left: Expression * right: Expression
    | ConditionalExpression of test: Expression * consequent: Expression * alternate: Expression
    | AnonymousFunction of args: Ident list * body: Statement list * genericParams: string list * returnType: Type
    | AssignmentExpression of target: Expression * kind: AssignmentOperator * value: Expression
    | EmitExpression of value: string * args: Expression list
    | ThrowExpression of value: Expression
    | RethrowExpression

    static member sequenceExpression(seqExprFn, exprs) =
        SequenceExpression(seqExprFn, exprs)
    static member listLiteral(values, typ, ?isConst) = ListLiteral(values, typ, isConst=defaultArg isConst false) |> Literal
    static member integerLiteral(value) = IntegerLiteral value |> Literal
    static member integerLiteral(value: int) = IntegerLiteral value |> Literal
    static member doubleLiteral(value) = DoubleLiteral value |> Literal
    static member booleanLiteral(value) = BooleanLiteral value |> Literal
    static member stringLiteral(value) = StringLiteral value |> Literal
    static member nullLiteral() = NullLiteral |> Literal
    static member interpolationString(parts, values) = InterpolationString(parts, values)
    static member identExpression(ident) = IdentExpression(ident)
    static member indexExpression(expr, index) = IndexExpression(expr, index)
    static member propertyAccess(expr, prop, ?isConst) = PropertyAccess(expr, prop, isConst=defaultArg isConst false)
    static member asExpression(expr, typ) = AsExpression(expr, typ)
    static member isExpression(expr, typ, ?isNot) = IsExpression(expr, typ, defaultArg isNot false)
    static member invocationExpression(expr: Expression, args: CallArg list, ?genArgs, ?isConst) =
        InvocationExpression(expr, defaultArg genArgs [], args, defaultArg isConst false)
    static member invocationExpression(expr: Expression, ?genArgs, ?isConst) =
        InvocationExpression(expr, defaultArg genArgs [], [], defaultArg isConst false)
    static member invocationExpression(expr: Expression, args: Expression list, ?genArgs, ?isConst) =
        InvocationExpression(expr, defaultArg genArgs [], args |> List.map (fun a -> None, a), defaultArg isConst false)
    static member invocationExpression(expr: Expression, prop: string, args: CallArg list, ?genArgs, ?isConst) =
        let expr = PropertyAccess(expr, prop, false)
        InvocationExpression(expr, defaultArg genArgs [], args, defaultArg isConst false)
    static member invocationExpression(expr: Expression, prop: string, args: Expression list, ?genArgs, ?isConst) =
        let expr = PropertyAccess(expr, prop, false)
        InvocationExpression(expr, defaultArg genArgs [], args |> List.map (fun a -> None, a), defaultArg isConst false)
    static member updateExpression(operator, expr, ?isPrefix) = UpdateExpression(operator, defaultArg isPrefix false, expr)
    static member unaryExpression(operator, expr) = UnaryExpression(operator, expr)
    static member binaryExpression(operator, left, right, ?isInt) = BinaryExpression(operator, left, right, defaultArg isInt false)
    static member logicalExpression(operator, left, right) = LogicalExpression(operator, left, right)
    static member conditionalExpression(test, consequent, alternate) = ConditionalExpression(test, consequent, alternate)
    static member anonymousFunction(args, body: Statement list, returnType, ?genArgs) =
        AnonymousFunction(args, body, defaultArg genArgs [], returnType)
    static member anonymousFunction(args, body: Expression, returnType, ?genArgs) =
        let body = [Statement.returnStatement body]
        AnonymousFunction(args, body, defaultArg genArgs [], returnType)
    static member assignmentExpression(target, value, ?kind) = AssignmentExpression(target, defaultArg kind AssignEqual, value)
    static member emitExpression(value, args) = EmitExpression(value, args)
    static member throwExpression(value) = ThrowExpression(value)
    static member rethrowExpression() = RethrowExpression

type VariableDeclarationKind =
    | Final
    | Const
    | Var

type SwitchCase(guards: Expression list, body: Statement list) =
    member _.Guards = guards
    member _.Body = body

type CatchClause(body, ?param, ?test) =
    member _.Param: Ident option = param
    member _.Test: Type option = test
    member _.Body: Statement list = body

type Statement =
    | IfStatement of test: Expression * consequent: Statement list * alternate: Statement list
    | ForStatement of init: (Ident * Expression) option * test: Expression option * update: Expression option * body: Statement list
    | ForInStatement of param: Ident * iterable: Expression * body: Statement list
    | WhileStatement of test: Expression * body: Statement list
//    | DoStatement of body: Statement list * test: Expression
    | TryStatement of body: Statement list * handlers: CatchClause list * finalizer: Statement list
    | SwitchStatement of discriminant: Expression * cases: SwitchCase list * defaultCase: Statement list option
    | ReturnStatement of Expression
    | BreakStatement of label: string option * ignoreDeadCode: bool
    | ContinueStatement of label: string option
    | ExpressionStatement of Expression
    | LocalVariableDeclaration of ident: Ident * kind: VariableDeclarationKind * value: Expression option
    | LocalFunctionDeclaration of FunctionDecl
    | LabeledStatement of label: string * body: Statement
    static member returnStatement(arg) =
        ReturnStatement(arg)
    static member labeledStatement(label, body) =
        LabeledStatement(label, body)
    static member ifStatement(test, consequent, ?alternate) =
        IfStatement(test, consequent, defaultArg alternate [])
    static member forStatement(body, ?init, ?test, ?update) =
        ForStatement(init, test, update, body)
    static member forInStatement(param, iterable, body) =
        ForInStatement(param, iterable, body)
    static member whileStatement(test, body) =
        WhileStatement(test, body)
    static member breakStatement(?label, ?ignoreDeadCode) =
        BreakStatement(label, defaultArg ignoreDeadCode false)
    static member continueStatement(?label) =
        ContinueStatement(label)
    static member tryStatement(body, ?handlers, ?finalizer) =
        TryStatement(body, defaultArg handlers [], defaultArg finalizer [])
    static member variableDeclaration(ident, ?kind, ?value) =
        LocalVariableDeclaration(ident, defaultArg kind Final, value)
    static member functionDeclaration(name: string, args: FunctionArg list, body: Statement list, returnType: Type, ?genArgs: string list) =
        LocalFunctionDeclaration {
            Name = name
            Args = args
            Body = body
            ReturnType = returnType
            GenericArgs = defaultArg genArgs []
        }
    static member switchStatement(discriminant, cases, defaultCase) =
        SwitchStatement(discriminant, cases, defaultCase)

type FunctionArg(ident: Ident, ?isOptional: bool, ?isNamed: bool) =
    member _.Ident = ident
    member _.IsOptional = defaultArg isOptional false
    member _.IsNamed = defaultArg isNamed false

type FunctionDecl =
    {
        Name: string
        Args: FunctionArg list
        Body: Statement list
        GenericArgs: string list
        ReturnType: Type
    }

type ConsArg =
    | ConsArg of Ident
    | ConsThisArg of name: string

type Constructor(?args, ?body, ?superArgs, ?isConst, ?isFactory) =
    member _.Args: ConsArg list = defaultArg args []
    member _.Body: Statement list = defaultArg body []
    member _.SuperArgs: CallArg list = defaultArg superArgs []
    member _.IsConst = defaultArg isConst false
    member _.IsFactory = defaultArg isFactory false

type InstanceVariable(ident, ?value, ?kind, ?isOverride, ?isLate) =
    member _.Ident: Ident = ident
    member _.Kind: VariableDeclarationKind = defaultArg kind Final
    member _.Value: Expression option = value
    member _.IsOverride = defaultArg isOverride false
    member _.IsLate = defaultArg isLate false

type MethodKind =
    | IsMethod
    | IsGetter
    | IsSetter
    | IsOperator

// TODO: generic constraints
type InstanceMethod(name, args, returnType, ?genArgs, ?body, ?kind, ?isOverride, ?isStatic) =
    member _.Name: string = name
    member _.Args: FunctionArg list = args
    member _.Body: Statement list option = body
    member _.GenericArgs: string list = defaultArg genArgs []
    member _.ReturnType: Type = returnType
    member _.Kind: MethodKind = defaultArg kind IsMethod
    member _.IsOverride = defaultArg isOverride false
    member _.IsStatic = defaultArg isStatic false

// TODO: generic constraints
type Class(name, ?genArgs, ?constructor, ?extends, ?implements, ?variables, ?methods, ?isAbstract, ?annotations) =
    member _.Name: string = name
    member _.GenericArgs: string list = defaultArg genArgs []
    member _.IsAbstract = defaultArg isAbstract false
    member _.Extends: Type option = extends
    member _.Implements: Type list = defaultArg implements []
    member _.Constructor: Constructor option = constructor
    member _.InstanceVariables: InstanceVariable list = defaultArg variables []
    member _.InstanceMethods: InstanceMethod list = defaultArg methods []
    member _.Annotations: Annotation list = defaultArg annotations []

type Declaration =
    | ClassDeclaration of Class
    | VariableDeclaration of ident: Ident * kind: VariableDeclarationKind * value: Expression
    | FunctionDeclaration of FunctionDecl

    static member variableDeclaration(ident, kind, value) =
        VariableDeclaration(ident, kind, value)

    static member functionDeclaration(name: string, args: FunctionArg list, body: Statement list, returnType: Type, ?genArgs: string list) =
        FunctionDeclaration {
            Name = name
            Args = args
            Body = body
            ReturnType = returnType
            GenericArgs = defaultArg genArgs []
        }

    static member classDeclaration(name, ?genArgs, ?isAbstract, ?constructor, ?extends, ?implements, ?variables, ?methods) =
        Class(name, ?genArgs=genArgs, ?isAbstract=isAbstract, ?constructor=constructor, ?extends=extends, ?implements=implements, ?variables=variables, ?methods=methods)
        |> ClassDeclaration

type Import =
  { LocalIdent: string option
    Path: string }

type File =
    { Imports: Import list
      Declarations: Declaration list }
