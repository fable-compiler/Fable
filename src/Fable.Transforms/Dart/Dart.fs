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

type TypeInfo =
    {
        IsRecord: bool
        IsUnion: bool
    }

type Type =
    // Built in
    | Object
    | Dynamic

    | Void
    | MetaType

    | Integer
    | Double
    | Boolean
    | String

    | List of Type
    | Nullable of Type

    | Generic of name: string
    | TypeReference of Ident * generics: Type list * info: TypeInfo
    | Function of argTypes: Type list * returnType: Type

    member this.Generics =
        match this with
        | TypeReference(_, gen, _) -> gen
        | Function(gen1, gen2) -> gen1 @ [ gen2 ]
        | _ -> []

    static member reference(ident, ?generics, ?isRecord, ?isUnion) =
        let info: TypeInfo =
            {
                IsRecord = defaultArg isRecord false
                IsUnion = defaultArg isUnion false
            }

        TypeReference(ident, defaultArg generics [], info)

    static member needsCast (source: Type) (target: Type) =
        match source, target with
        | _, Object
        | _, Dynamic -> false
        | Void, Void
        | MetaType, MetaType
        | Integer, Integer
        | Double, Double
        | Boolean, Boolean
        | String, String -> false

        | List source, List target -> Type.needsCast source target
        | Nullable source, Nullable target -> Type.needsCast source target
        | source, Nullable target -> Type.needsCast source target
        | Generic source, Generic target -> source <> target

        // We should be able to detect class hierarchy here too
        | TypeReference(sourceIdent, sourceGen, _),
          TypeReference(targetIdent, targetGen, _) ->
            not (
                sourceIdent.Name = targetIdent.Name
                && sourceIdent.ImportModule = targetIdent.ImportModule
                && sourceGen.Length = targetGen.Length
                && not (
                    List.zip sourceGen targetGen
                    |> List.exists (fun (s, t) -> Type.needsCast s t)
                )
            )

        | _ -> true

type Ident =
    {
        ImportModule: string option
        Name: string
        Type: Type
        IsMutable: bool
    }

    member this.Expr = IdentExpression this

type Literal =
    | IntegerLiteral of value: int64
    | DoubleLiteral of value: double
    | BooleanLiteral of value: bool
    | StringLiteral of value: string
    | NullLiteral of Type
    | ListLiteral of values: Expression list * typ: Type * isConst: bool

    member this.Type =
        match this with
        | IntegerLiteral _ -> Integer
        | DoubleLiteral _ -> Double
        | BooleanLiteral _ -> Boolean
        | StringLiteral _ -> String
        | NullLiteral t -> Nullable t
        | ListLiteral(_, t, _) -> List t

type Annotation = Ident * Literal list

type CallArg = string option * Expression

type Expression =
    | CommentedExpression of comment: string * expr: Expression
    | SuperExpression of typ: Type
    | ThisExpression of typ: Type
    | Literal of value: Literal
    | InterpolationString of parts: string list * values: Expression list
    // Dart AST doesn't include TypeLiteral with the other literals
    | TypeLiteral of value: Type
    | IdentExpression of ident: Ident
    | PropertyAccess of
        expr: Expression *
        prop: string *
        typ: Type *
        isConst: bool
    | IndexExpression of expr: Expression * index: Expression * typ: Type
    | AsExpression of expr: Expression * typ: Type
    | IsExpression of expr: Expression * typ: Type * isNot: bool
    | InvocationExpression of
        expr: Expression *
        genArgs: Type list *
        args: CallArg list *
        typ: Type *
        isConst: bool
    | NotNullAssert of expr: Expression
    | UpdateExpression of
        operator: UpdateOperator *
        isPrefix: bool *
        expr: Expression
    | UnaryExpression of operator: UnaryOperator * expr: Expression
    | BinaryExpression of
        operator: BinaryOperator *
        left: Expression *
        right: Expression *
        typ: Type
    | LogicalExpression of
        operator: LogicalOperator *
        left: Expression *
        right: Expression
    | ConditionalExpression of
        test: Expression *
        consequent: Expression *
        alternate: Expression
    | AnonymousFunction of
        args: Ident list *
        body: Statement list *
        genParams: string list *
        returnType: Type
    | AssignmentExpression of
        target: Expression *
        kind: AssignmentOperator *
        value: Expression
    | EmitExpression of value: string * args: Expression list * typ: Type
    | ThrowExpression of value: Expression * typ: Type
    | RethrowExpression of typ: Type

    member this.Type =
        match this with
        | CommentedExpression(_, e) -> e.Type
        | IsExpression _ -> Boolean
        | LogicalExpression _ -> Boolean
        | Literal value -> value.Type
        | InterpolationString _ -> String
        | TypeLiteral _ -> MetaType
        | IdentExpression i -> i.Type
        | NotNullAssert e ->
            match e.Type with
            | Nullable t -> t
            | t -> t // shouldn't happen
        | SuperExpression t
        | ThisExpression t
        | PropertyAccess(_, _, t, _)
        | IndexExpression(_, _, t)
        | AsExpression(_, t)
        | BinaryExpression(_, _, _, t)
        | InvocationExpression(_, _, _, t, _)
        | EmitExpression(_, _, t)
        | ThrowExpression(_, t)
        | RethrowExpression t -> t
        | UpdateExpression(_, _, e)
        | UnaryExpression(_, e)
        | ConditionalExpression(_, e, _) -> e.Type
        | AnonymousFunction(args, _, _, returnType) ->
            Function(args |> List.map (fun a -> a.Type), returnType)
        | AssignmentExpression _ -> Void

    static member commented comment expr = CommentedExpression(comment, expr)

    static member listLiteral(values, typ, ?isConst) =
        ListLiteral(values, typ, isConst = defaultArg isConst false) |> Literal

    static member integerLiteral(value) = IntegerLiteral value |> Literal
    static member integerLiteral(value: int) = IntegerLiteral value |> Literal
    static member doubleLiteral(value) = DoubleLiteral value |> Literal
    static member booleanLiteral(value) = BooleanLiteral value |> Literal
    static member stringLiteral(value) = StringLiteral value |> Literal
    static member nullLiteral(typ) = NullLiteral typ |> Literal

    static member interpolationString(parts, values) =
        InterpolationString(parts, values)

    static member identExpression(ident) = IdentExpression(ident)

    static member indexExpression(expr, index, typ) =
        IndexExpression(expr, index, typ)

    static member propertyAccess(expr, prop, typ, ?isConst) =
        PropertyAccess(expr, prop, typ, isConst = defaultArg isConst false)

    static member asExpression(expr, typ) = AsExpression(expr, typ)

    static member isExpression(expr, typ, ?isNot) =
        IsExpression(expr, typ, defaultArg isNot false)

    static member invocationExpression
        (
            expr: Expression,
            args: CallArg list,
            typ,
            ?genArgs,
            ?isConst
        )
        =
        InvocationExpression(
            expr,
            defaultArg genArgs [],
            args,
            typ,
            defaultArg isConst false
        )

    static member invocationExpression
        (
            expr: Expression,
            typ,
            ?genArgs,
            ?isConst
        )
        =
        InvocationExpression(
            expr,
            defaultArg genArgs [],
            [],
            typ,
            defaultArg isConst false
        )

    static member invocationExpression
        (
            expr: Expression,
            args: Expression list,
            typ,
            ?genArgs,
            ?isConst
        )
        =
        InvocationExpression(
            expr,
            defaultArg genArgs [],
            args |> List.map (fun a -> None, a),
            typ,
            defaultArg isConst false
        )

    static member invocationExpression
        (
            expr: Expression,
            prop: string,
            args: CallArg list,
            typ,
            ?genArgs,
            ?isConst
        )
        =
        let expr = PropertyAccess(expr, prop, Dynamic, false)

        InvocationExpression(
            expr,
            defaultArg genArgs [],
            args,
            typ,
            defaultArg isConst false
        )

    static member invocationExpression
        (
            expr: Expression,
            prop: string,
            args: Expression list,
            typ,
            ?genArgs,
            ?isConst
        )
        =
        let expr = PropertyAccess(expr, prop, Dynamic, false)

        InvocationExpression(
            expr,
            defaultArg genArgs [],
            args |> List.map (fun a -> None, a),
            typ,
            defaultArg isConst false
        )

    static member updateExpression(operator, expr, ?isPrefix) =
        UpdateExpression(operator, defaultArg isPrefix false, expr)

    static member unaryExpression(operator, expr) =
        UnaryExpression(operator, expr)

    static member binaryExpression(operator, left, right, typ) =
        BinaryExpression(operator, left, right, typ)

    static member logicalExpression(operator, left, right) =
        LogicalExpression(operator, left, right)

    static member conditionalExpression(test, consequent, alternate) =
        ConditionalExpression(test, consequent, alternate)

    static member anonymousFunction
        (
            args,
            body: Statement list,
            returnType,
            ?genParams
        )
        =
        AnonymousFunction(args, body, defaultArg genParams [], returnType)

    static member anonymousFunction
        (
            args,
            body: Expression,
            returnType,
            ?genParams
        )
        =
        let body = [ Statement.returnStatement body ]
        AnonymousFunction(args, body, defaultArg genParams [], returnType)

    static member assignmentExpression(target, value, ?kind) =
        AssignmentExpression(target, defaultArg kind AssignEqual, value)

    static member emitExpression(value, args, typ) =
        EmitExpression(value, args, typ)

    static member throwExpression(value, typ) = ThrowExpression(value, typ)
    static member rethrowExpression(typ) = RethrowExpression typ

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
    | CommentedStatement of comment: string * statement: Statement
    | IfStatement of
        test: Expression *
        consequent: Statement list *
        alternate: Statement list
    | ForStatement of
        init: (Ident * Expression) option *
        test: Expression option *
        update: Expression option *
        body: Statement list
    | ForInStatement of
        param: Ident *
        iterable: Expression *
        body: Statement list
    | WhileStatement of test: Expression * body: Statement list
    //    | DoStatement of body: Statement list * test: Expression
    | TryStatement of
        body: Statement list *
        handlers: CatchClause list *
        finalizer: Statement list
    | SwitchStatement of
        discriminant: Expression *
        cases: SwitchCase list *
        defaultCase: Statement list option
    | ReturnStatement of Expression
    | BreakStatement of label: string option
    | ContinueStatement of label: string option
    | ExpressionStatement of Expression
    | LocalVariableDeclaration of
        ident: Ident *
        kind: VariableDeclarationKind *
        value: Expression option
    | LocalFunctionDeclaration of FunctionDecl
    | LabeledStatement of label: string * body: Statement

    static member commented comment statement =
        CommentedStatement(comment, statement)

    static member returnStatement(arg) = ReturnStatement(arg)
    static member labeledStatement(label, body) = LabeledStatement(label, body)

    static member ifStatement(test, consequent, ?alternate) =
        IfStatement(test, consequent, defaultArg alternate [])

    static member forStatement(body, ?init, ?test, ?update) =
        ForStatement(init, test, update, body)

    static member forInStatement(param, iterable, body) =
        ForInStatement(param, iterable, body)

    static member whileStatement(test, body) = WhileStatement(test, body)
    static member breakStatement(?label) = BreakStatement(label)
    static member continueStatement(?label) = ContinueStatement(label)

    static member tryStatement(body, ?handlers, ?finalizer) =
        TryStatement(body, defaultArg handlers [], defaultArg finalizer [])

    static member variableDeclaration(ident: Ident, kind, addToScope, ?value) =
        addToScope ident.Name
        LocalVariableDeclaration(ident, kind, value)

    /// Variables that won't be added to scope
    static member tempVariableDeclaration(ident: Ident, ?isMutable, ?value) =
        let isMutable = defaultArg isMutable false

        LocalVariableDeclaration(
            ident,
            (if isMutable then
                 Var
             else
                 Final),
            value
        )

    static member functionDeclaration
        (
            name: string,
            args: FunctionArg list,
            body: Statement list,
            returnType: Type,
            ?genParams: GenericParam list
        )
        =
        LocalFunctionDeclaration
            {
                Name = name
                Args = args
                Body = body
                ReturnType = returnType
                GenericParams = defaultArg genParams []
            }

    static member switchStatement(discriminant, cases, ?defaultCase) =
        SwitchStatement(discriminant, cases, defaultCase)

type FunctionArg
    (
        ident: Ident,
        ?isOptional: bool,
        ?isNamed: bool,
        ?isConsThisArg: bool,
        ?defaultValue: Expression
    )
    =
    member _.Ident = ident
    member _.DefaultValue = defaultValue
    member _.IsOptional = defaultArg isOptional false
    member _.IsNamed = defaultArg isNamed false
    member _.IsConsThisArg = defaultArg isConsThisArg false

    member _.AsConsThisArg(name) =
        FunctionArg(
            { ident with Name = name },
            ?isOptional = isOptional,
            ?isNamed = isNamed,
            isConsThisArg = true
        )

type FunctionDecl =
    {
        Name: string
        Args: FunctionArg list
        Body: Statement list
        GenericParams: GenericParam list
        ReturnType: Type
    }

type Constructor(?args, ?body, ?superArgs, ?isConst, ?isFactory) =
    member _.Args: FunctionArg list = defaultArg args []
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

type GenericParam =
    {
        Name: string
        Extends: Type option
    }

type InstanceMethod
    (name, args, returnType, ?genParams, ?body, ?kind, ?isOverride, ?isStatic)
    =
    member _.Name: string = name
    member _.Args: FunctionArg list = args
    member _.Body: Statement list option = body
    member _.GenericParams: GenericParam list = defaultArg genParams []
    member _.ReturnType: Type = returnType
    member _.Kind: MethodKind = defaultArg kind IsMethod
    member _.IsOverride = defaultArg isOverride false
    member _.IsStatic = defaultArg isStatic false

type Class
    (
        name,
        ?genParams,
        ?constructor,
        ?extends,
        ?implements,
        ?variables,
        ?methods,
        ?isAbstract,
        ?annotations
    )
    =
    member _.Name: string = name
    member _.GenericParams: GenericParam list = defaultArg genParams []
    member _.IsAbstract = defaultArg isAbstract false
    member _.Extends: Type option = extends
    member _.Implements: Type list = defaultArg implements []
    member _.Constructor: Constructor option = constructor
    member _.InstanceVariables: InstanceVariable list = defaultArg variables []
    member _.InstanceMethods: InstanceMethod list = defaultArg methods []
    member _.Annotations: Annotation list = defaultArg annotations []

type Declaration =
    | ClassDeclaration of Class
    | VariableDeclaration of
        ident: Ident *
        kind: VariableDeclarationKind *
        value: Expression
    | FunctionDeclaration of FunctionDecl

    static member variableDeclaration(ident, kind, value) =
        VariableDeclaration(ident, kind, value)

    static member functionDeclaration
        (
            name: string,
            args: FunctionArg list,
            body: Statement list,
            returnType: Type,
            ?genParams: GenericParam list
        )
        =
        FunctionDeclaration
            {
                Name = name
                Args = args
                Body = body
                ReturnType = returnType
                GenericParams = defaultArg genParams []
            }

    static member classDeclaration
        (
            name,
            ?genParams,
            ?isAbstract,
            ?constructor,
            ?extends,
            ?implements,
            ?variables,
            ?methods
        )
        =
        Class(
            name,
            ?genParams = genParams,
            ?isAbstract = isAbstract,
            ?constructor = constructor,
            ?extends = extends,
            ?implements = implements,
            ?variables = variables,
            ?methods = methods
        )
        |> ClassDeclaration

type Import =
    {
        LocalIdent: string option
        Path: string
    }

type File =
    {
        Imports: Import list
        Declarations: Declaration list
    }
