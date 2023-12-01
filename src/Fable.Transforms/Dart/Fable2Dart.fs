module rec Fable.Transforms.Fable2Dart

open System.Collections.Generic
open Fable
open Fable.AST
open Fable.AST.Dart

type ReturnStrategy =
    | Return of isVoid: bool
    | Assign of Expression
    | Target of Ident
    | Ignore
    | Capture of binding: Ident option

type CapturedExpr = Expression option

type ArgsInfo =
    | CallInfo of Fable.CallInfo
    | NoCallInfo of args: Fable.Expr list

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
    {
        RootScope: HashSet<string>
        DeclarationScopes: HashSet<string>
        CurrentDeclarationScope: HashSet<string>
    }

type Context =
    {
        File: Fable.File
        UsedNames: UsedNames
        /// Types asserted in a condition branch
        AssertedTypes: Map<string, Type>
        CastedUnions: Dictionary<string, string>
        DecisionTargets: (Fable.Ident list * Fable.Expr) list
        TailCallOpportunity: ITailCallOpportunity option
        EntityAndMemberGenericParams: Fable.GenericParam list
        OptimizeTailCall: unit -> unit
        /// Vars declared in current function scope
        VarsDeclaredInScope: HashSet<string>
        ConstIdents: Set<string>
    }

    member this.AddToScope(name) =
        this.VarsDeclaredInScope.Add(name) |> ignore

    member this.AppendLocalGenParams(genParams: string list) =
        let genParams =
            genParams
            |> List.map (fun g ->
                { new Fable.GenericParam with
                    member _.Name = g
                    member _.IsMeasure = false
                    member _.Constraints = []
                }
            )

        { this with
            EntityAndMemberGenericParams =
                this.EntityAndMemberGenericParams @ genParams
        }

type MemberKind =
    | ClassConstructor
    | NonAttached of funcName: string
    | Attached of isStatic: bool

type IDartCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> Import list

    abstract GetImportIdent:
        Context *
        selector: string *
        path: string *
        typ: Fable.Type *
        ?range: SourceLocation ->
            Ident

    abstract TransformType: Context * Fable.Type -> Type

    abstract Transform:
        Context * ReturnStrategy * Fable.Expr -> Statement list * CapturedExpr

    abstract TransformFunction:
        Context * string option * Fable.Ident list * Fable.Expr ->
            Ident list * Statement list * Type

    abstract WarnOnlyOnce:
        string * ?values: obj[] * ?range: SourceLocation -> unit

    abstract ErrorOnlyOnce:
        string * ?values: obj[] * ?range: SourceLocation -> unit

module Util =

    let (|TransformType|) (com: IDartCompiler) ctx e = com.TransformType(ctx, e)

    let (|Function|_|) =
        function
        | Fable.Lambda(arg, body, _) -> Some([ arg ], body)
        | Fable.Delegate(args, body, _, []) -> Some(args, body)
        | _ -> None

    let (|Lets|_|) =
        function
        | Fable.Let(ident, value, body) -> Some([ ident, value ], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
        | _ -> None

    let makeTypeRefFromName typeName genArgs =
        let ident = makeImmutableIdent MetaType typeName
        Type.reference (ident, genArgs)

    let libValue (com: IDartCompiler) ctx t moduleName memberName =
        com.GetImportIdent(ctx, memberName, getLibPath com moduleName, t)

    let libTypeRef (com: IDartCompiler) ctx moduleName memberName genArgs =
        let ident = libValue com ctx Fable.MetaType moduleName memberName
        Type.reference (ident, genArgs)

    let libCallWithType
        (com: IDartCompiler)
        ctx
        t
        moduleName
        memberName
        (args: Expression list)
        =
        let fn =
            com.GetImportIdent(
                ctx,
                memberName,
                getLibPath com moduleName,
                Fable.Any
            )

        Expression.invocationExpression (fn.Expr, args, t)

    let libCall
        (com: IDartCompiler)
        ctx
        t
        moduleName
        memberName
        (args: Expression list)
        =
        let t = transformType com ctx t
        libCallWithType com ctx t moduleName memberName args

    let libGenCall
        (com: IDartCompiler)
        ctx
        t
        moduleName
        memberName
        (args: Expression list)
        genArgs
        =
        let genArgs = transformGenArgs com ctx genArgs

        let fn =
            com.GetImportIdent(
                ctx,
                memberName,
                getLibPath com moduleName,
                Fable.Any
            )

        Expression.invocationExpression (
            fn.Expr,
            args,
            transformType com ctx t,
            genArgs = genArgs
        )

    let extLibCall
        (com: IDartCompiler)
        ctx
        t
        modulePath
        memberName
        (args: Expression list)
        =
        let fn = com.GetImportIdent(ctx, memberName, modulePath, Fable.Any)
        Expression.invocationExpression (fn.Expr, args, transformType com ctx t)

    let addErrorAndReturnNull
        (com: Compiler)
        (range: SourceLocation option)
        (error: string)
        =
        addError com [] range error
        NullLiteral Dynamic |> Literal

    let numType kind =
        Fable.Number(kind, Fable.NumberInfo.Empty)

    let namedArg name expr : CallArg = Some name, expr

    let unnamedArg expr : CallArg = None, expr

    let unnamedArgs exprs : CallArg list = List.map unnamedArg exprs

    let makeIdent isMutable typ name =
        {
            Name = name
            Type = typ
            IsMutable = isMutable
            ImportModule = None
        }

    let makeImmutableIdent typ name = makeIdent false typ name

    let makeReturnBlock expr = [ Statement.returnStatement expr ]

    let makeImmutableListExpr com ctx typ values : Expression =
        let typ = transformType com ctx typ

        let isConst, values =
            if areConstTypes [ typ ] && areConstExprs ctx values then
                true, List.map removeConst values
            else
                false, values

        Expression.listLiteral (values, typ, isConst)

    let makeMutableListExpr com ctx typ values : Expression =
        let typ = transformType com ctx typ
        Expression.listLiteral (values, typ)

    let tryGetEntityIdent (com: IDartCompiler) ctx ent =
        Dart.Replacements.tryEntityIdent com ent
        |> Option.bind (fun entRef ->
            match transformAndCaptureExpr com ctx entRef with
            | [], IdentExpression ident -> Some ident
            | _ ->
                addError
                    com
                    []
                    None
                    $"Unexpected, entity ref for {ent.FullName} is not an identifier"

                None
        )

    let getEntityIdent (com: IDartCompiler) ctx (ent: Fable.Entity) =
        match tryGetEntityIdent com ctx ent with
        | Some ident -> ident
        | None ->
            addError com [] None $"Cannot find reference for {ent.FullName}"
            makeImmutableIdent MetaType ent.DisplayName

    let transformTupleType com ctx genArgs =
        let tup = List.length genArgs |> getTupleTypeIdent com ctx
        Type.reference (tup, genArgs)

    let transformOptionType com ctx genArg =
        let genArg = transformType com ctx genArg

        Type.reference (
            libValue com ctx Fable.MetaType "Types" "Some",
            [ genArg ]
        )
        |> Nullable

    let transformDeclaredType
        (com: IDartCompiler)
        ctx
        (entRef: Fable.EntityRef)
        genArgs
        =
        let genArgs = transformGenArgs com ctx genArgs

        let makeIterator genArg =
            Type.reference (makeImmutableIdent MetaType "Iterator", [ genArg ])

        let makeMapEntry key value =
            Type.reference (
                makeImmutableIdent MetaType "MapEntry",
                [
                    key
                    value
                ]
            )

        match entRef.FullName, genArgs with
        | Types.enum_, _ -> Integer
        // List without generics is same as List<dynamic>
        | Types.array, _ -> List Dynamic
        | "System.Tuple`1", _ -> transformTupleType com ctx genArgs
        | Types.valueType, _ -> Object
        | Types.nullable, [ genArg ]
        | "Fable.Core.Dart.DartNullable`1", [ genArg ] -> Nullable genArg
        | Types.regexGroup, _ -> Nullable String
        | Types.regexMatch, _ -> makeTypeRefFromName "Match" []
        // We use `dynamic` for now because there doesn't seem to be a type that catches all errors in Dart
        | Naming.EndsWith "Exception" _, _ -> Dynamic
        | "System.Collections.Generic.Dictionary`2.Enumerator", [ key; value ] ->
            makeMapEntry key value |> makeIterator
        | "System.Collections.Generic.Dictionary`2.KeyCollection.Enumerator",
          [ key; _ ] -> makeIterator key
        | "System.Collections.Generic.Dictionary`2.ValueCollection.Enumerator",
          [ _; value ] -> makeIterator value
        | _ ->
            let ent = com.GetEntity(entRef)

            let ident, genArgs =
                match getEntityIdent com ctx ent with
                // If Iterator has more than one genArg assume we need to use MapEntry
                | {
                      Name = "Iterator"
                      ImportModule = None
                  } as ident when List.isMultiple genArgs ->
                    ident,
                    [
                        Type.reference (
                            makeImmutableIdent MetaType "MapEntry",
                            genArgs
                        )
                    ]
                | ident -> ident, genArgs

            Type.reference (
                ident,
                genArgs,
                isRecord = ent.IsFSharpRecord,
                isUnion = ent.IsFSharpUnion
            )

    let get t left memberName =
        PropertyAccess(left, memberName, t, isConst = false)

    let getExpr t left expr = IndexExpression(left, expr, t)

    let getUnionCaseName (uci: Fable.UnionCase) =
        match uci.CompiledName with
        | Some cname -> cname
        | None -> uci.Name

    let getUnionCaseDeclarationName
        (unionDeclName: string)
        (uci: Fable.UnionCase)
        =
        unionDeclName + "_" + uci.Name

    let getUnionExprTag expr = get Integer expr "tag"

    let hasConstAttribute (atts: Fable.Attribute seq) =
        atts |> Seq.exists (fun att -> att.Entity.FullName = Atts.dartIsConst)

    /// Fable doesn't currently sanitize attached members/fields so we do a simple sanitation here.
    /// Should this be done in FSharp2Fable step?
    let sanitizeMember (name: string) =
        Naming.sanitizeIdentForbiddenCharsWith
            (function
            | '@' -> "$"
            | _ -> "_")
            name

    let getUniqueNameInRootScope (ctx: Context) name =
        let name =
            (name, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (fun name ->
                ctx.UsedNames.RootScope.Contains(name)
                || ctx.UsedNames.DeclarationScopes.Contains(name)
            )

        ctx.UsedNames.RootScope.Add(name) |> ignore
        name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name =
            (name, Naming.NoMemberPart)
            ||> Naming.sanitizeIdent (fun name ->
                ctx.UsedNames.RootScope.Contains(name)
                || ctx.UsedNames.CurrentDeclarationScope.Contains(name)
            )

        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore
        name

    type NamedTailCallOpportunity
        (_com: IDartCompiler, ctx, name, args: Fable.Ident list)
        =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        let argIds =
            args
            |> FSharp2Fable.Util.discardUnitArg
            |> List.map (fun arg ->
                getUniqueNameInDeclarationScope ctx (arg.Name + "_mut")
            )

        interface ITailCallOpportunity with
            member _.Label = name
            member _.Args = argIds

            member _.IsRecursiveRef(e) =
                match e with
                | Fable.IdentExpr id -> name = id.Name
                | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwithf $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let isInt64OrLess =
        function
        | Fable.Number(Dart.Replacements.DartInt, _) -> true
        | _ -> false

    let isImmutableIdent =
        function
        | IdentExpression ident -> not ident.IsMutable
        | _ -> false

    let isConstIdent (ctx: Context) (ident: Ident) =
        Option.isSome ident.ImportModule
        || Set.contains ident.Name ctx.ConstIdents

    // Binary operations should be const if the operands are, but if necessary let's fold constants binary ops in FableTransforms
    let isConstExpr (ctx: Context) =
        function
        | CommentedExpression(_, expr) -> isConstExpr ctx expr
        | IdentExpression ident -> isConstIdent ctx ident
        | PropertyAccess(_, _, _, isConst)
        | InvocationExpression(_, _, _, _, isConst) -> isConst
        | BinaryExpression(_, left, right, _) ->
            isConstExpr ctx left && isConstExpr ctx right
        | Literal value ->
            match value with
            | ListLiteral(_, _, isConst) -> isConst
            | IntegerLiteral _
            | DoubleLiteral _
            | BooleanLiteral _
            | StringLiteral _
            | NullLiteral _ -> true
        | _ -> false

    let areConstExprs ctx exprs = List.forall (isConstExpr ctx) exprs

    let areConstTypes types =
        types
        |> List.forall (
            function
            | Generic _ -> false
            | _ -> true
        )

    // Dart linter complaints if we have too many "const"
    let removeConst =
        function
        | InvocationExpression(e, g, a, t, _isConst) ->
            InvocationExpression(e, g, a, t, false)
        | Literal value as e ->
            match value with
            | ListLiteral(values, typ, _isConst) ->
                ListLiteral(values, typ, false) |> Literal
            | _ -> e
        | e -> e

    let getVarKind ctx isMutable value =
        if isMutable then
            Var, value
        elif isConstExpr ctx value then
            Const, removeConst value
        else
            Final, value

    let assign (_range: SourceLocation option) left right =
        AssignmentExpression(left, AssignEqual, right)

    /// Immediately Invoked Function Expression
    let iife (statements: Statement list) (expr: Expression) =
        match statements with
        | [] -> expr
        | statements ->
            let t = expr.Type
            let body = statements @ [ ReturnStatement expr ]
            let fn = Expression.anonymousFunction ([], body, t)
            Expression.invocationExpression (fn, t)

    let optimizeTailCall
        (com: IDartCompiler)
        (ctx: Context)
        _range
        (tc: ITailCallOpportunity)
        args
        =
        let rec checkCrossRefs tempVars allArgs =
            function
            | [] -> tempVars
            | (argId, arg: Fable.Expr) :: rest ->
                let found =
                    allArgs
                    |> List.exists (
                        deepExists (
                            function
                            | Fable.IdentExpr i -> argId = i.Name
                            | _ -> false
                        )
                    )

                let tempVars =
                    if found then
                        let tempVar =
                            getUniqueNameInDeclarationScope
                                ctx
                                (argId + "_tailcall")

                        let tempVar = makeTypedIdent arg.Type tempVar
                        Map.add argId tempVar tempVars
                    else
                        tempVars

                checkCrossRefs tempVars allArgs rest

        ctx.OptimizeTailCall()
        let zippedArgs = List.zip tc.Args args
        let tempVars = checkCrossRefs Map.empty args zippedArgs

        let tempVarReplacements =
            tempVars |> Map.map (fun _ v -> makeIdentExpr v.Name)

        // First declare temp variables
        let statements1 =
            tempVars
            |> Seq.mapToList (fun (KeyValue(argId, tempVar)) ->
                let tempVar = transformIdent com ctx tempVar

                let argId =
                    makeImmutableIdent tempVar.Type argId
                    |> Expression.identExpression

                Statement.tempVariableDeclaration (tempVar, value = argId)
            )

        // Then assign argument expressions to the original argument identifiers
        // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
        let statements2 =
            zippedArgs
            |> List.collect (fun (argId, arg) ->
                let arg = FableTransforms.replaceValues tempVarReplacements arg

                let argId =
                    transformIdentWith com ctx false arg.Type argId
                    |> Expression.identExpression

                let statements, arg = transformAndCaptureExpr com ctx arg
                statements @ [ assign None argId arg |> ExpressionStatement ]
            )

        statements1 @ statements2 @ [ Statement.continueStatement (tc.Label) ]

    let transformCallArgs (com: IDartCompiler) ctx (info: ArgsInfo) =

        let paramsInfo, thisArg, args =
            match info with
            | NoCallInfo args ->
                let args = FSharp2Fable.Util.dropUnitCallArg args []
                None, None, args
            | CallInfo callInfo ->
                let args =
                    FSharp2Fable.Util.dropUnitCallArg
                        callInfo.Args
                        callInfo.SignatureArgTypes

                let paramsInfo =
                    callInfo.MemberRef
                    |> Option.bind com.TryGetMember
                    |> Option.map getParamsInfo

                paramsInfo, callInfo.ThisArg, args

        let unnamedArgs, namedArgs =
            paramsInfo
            |> Option.map (splitNamedArgs args)
            |> function
                | None -> args, []
                | Some(args, []) -> args, []
                | Some(args, namedArgs) ->
                    args,
                    namedArgs
                    |> List.choose (fun (p, v) ->
                        match p.Name, v with
                        | _,
                          Fable.Value((Fable.Null _ | Fable.NewOption(None,
                                                                      _,
                                                                      _)),
                                      _) when p.IsOptional -> None
                        | Some k, v -> Some(k, v)
                        | None, _ -> None
                    )

        let unnamedArgs =
            match unnamedArgs, paramsInfo with
            | args, Some paramsInfo ->
                let argsLen = args.Length
                let parameters = paramsInfo.Parameters

                if parameters.Length >= argsLen then
                    ([],
                     List.zip args (List.take argsLen parameters) |> List.rev)
                    ||> List.fold (fun acc (arg, par) ->
                        if par.IsOptional then
                            match arg with
                            | Fable.Value((Fable.Null t | Fable.NewOption(None,
                                                                          t,
                                                                          _)),
                                          r) ->
                                match acc with
                                | [] -> []
                                | acc -> Fable.Value(Fable.Null t, r) :: acc
                            | arg -> arg :: acc
                        else
                            arg :: acc
                    )
                else
                    args
            | args, None -> args

        let unnamedArgs = (Option.toList thisArg) @ unnamedArgs

        let args =
            unnamedArgs @ (List.map snd namedArgs)
            |> List.map (transformAndCaptureExpr com ctx)

        let statements, args = combineStatementsAndExprs com ctx args

        let keys =
            (List.map (fun _ -> None) unnamedArgs)
            @ (List.map (fst >> Some) namedArgs)

        statements,
        List.zip keys args
        |> List.map (
            function
            | Some k, a -> namedArg k a
            | None, a -> unnamedArg a
        )

    let resolveExpr strategy expr : Statement list * CapturedExpr =
        match strategy with
        | Ignore
        | Return(isVoid = true) -> [ ExpressionStatement expr ], None
        | Return(isVoid = false) -> [ ReturnStatement expr ], None
        | Assign left -> [ assign None left expr |> ExpressionStatement ], None
        | Target left ->
            [ assign None (IdentExpression left) expr |> ExpressionStatement ],
            None
        | Capture _ -> [], Some expr

    let combineCapturedExprs
        _com
        ctx
        (capturedExprs: (Statement list * CapturedExpr) list)
        : Statement list * Expression list
        =
        let extractExpression
            mayHaveSideEffect
            (statements, capturedExpr: CapturedExpr)
            =
            match capturedExpr with
            | Some expr ->
                if
                    (not mayHaveSideEffect)
                    || isImmutableIdent expr
                    || isConstExpr ctx expr
                then
                    statements, expr
                else
                    let ident =
                        getUniqueNameInDeclarationScope ctx "tmp_combine"
                        |> makeImmutableIdent expr.Type

                    let varDecl =
                        Statement.tempVariableDeclaration (ident, value = expr)

                    statements @ [ varDecl ], ident.Expr
            | _ -> statements, Expression.nullLiteral Void

        let _, statements, exprs =
            ((false, [], []), List.rev capturedExprs)
            ||> List.fold (fun
                               (mayHaveSideEffect, accStatements, accExprs)
                               statements ->
                let mayHaveSideEffect =
                    mayHaveSideEffect || not (List.isEmpty accStatements)

                let statements, expr =
                    extractExpression mayHaveSideEffect statements

                mayHaveSideEffect, statements @ accStatements, expr :: accExprs
            )

        statements, exprs

    let combineStatementsAndExprs
        com
        ctx
        (statementsAndExpr: (Statement list * Expression) list)
        : Statement list * Expression list
        =
        statementsAndExpr
        |> List.map (fun (statements, expr) -> statements, Some expr)
        |> combineCapturedExprs com ctx

    let combineCalleeAndArgStatements
        _com
        ctx
        calleeStatements
        argStatements
        (callee: Expression)
        =
        if List.isEmpty argStatements then
            calleeStatements, callee
        elif isImmutableIdent callee || isConstExpr ctx callee then
            calleeStatements @ argStatements, callee
        else
            let ident =
                getUniqueNameInDeclarationScope ctx "tmp_arg"
                |> makeImmutableIdent callee.Type

            let varDecl =
                Statement.tempVariableDeclaration (ident, value = callee)

            calleeStatements @ [ varDecl ] @ argStatements, ident.Expr

    let transformExprsAndResolve com ctx returnStrategy exprs transformExprs =
        List.map (transform com ctx (Capture(binding = None))) exprs
        |> combineCapturedExprs com ctx
        |> fun (statements, exprs) ->
            let statements2, capturedExpr =
                transformExprs exprs |> resolveExpr returnStrategy

            statements @ statements2, capturedExpr

    let transformExprAndResolve com ctx returnStrategy expr transformExpr =
        let statements, expr = transformAndCaptureExpr com ctx expr

        let statements2, capturedExpr =
            transformExpr expr |> resolveExpr returnStrategy

        statements @ statements2, capturedExpr

    let transformExprsAndResolve2
        com
        ctx
        returnStrategy
        expr0
        expr1
        transformExprs
        =
        List.map
            (transform com ctx (Capture(binding = None)))
            [
                expr0
                expr1
            ]
        |> combineCapturedExprs com ctx
        |> fun (statements, exprs) ->
            let statements2, capturedExpr =
                transformExprs exprs[0] exprs[1] |> resolveExpr returnStrategy

            statements @ statements2, capturedExpr

    let getFSharpListTypeIdent com ctx =
        libValue com ctx Fable.MetaType "List" "FSharpList"

    let getTupleTypeIdent (com: IDartCompiler) ctx itemsLength =
        libValue com ctx Fable.MetaType "Types" $"Tuple%i{itemsLength}"

    //    let getExceptionTypeIdent (com: IDartCompiler) ctx: Ident =
    //        transformIdentWith com ctx false Fable.MetaType "Exception"

    /// Discards Measure generic arguments
    let transformGenArgs com ctx (genArgs: Fable.Type list) =
        genArgs
        |> List.choose (fun t ->
            if isUnitOfMeasure t then
                None
            else
                transformType com ctx t |> Some
        )

    let transformType (com: IDartCompiler) (ctx: Context) (t: Fable.Type) =
        match t with
        | Fable.Measure _
        | Fable.Any -> Dynamic // TODO: Object instead? Seems to create issues with Dart compiler sometimes.
        | Fable.Unit -> Void
        | Fable.MetaType -> MetaType
        | Fable.Boolean -> Boolean
        | Fable.String -> String
        | Fable.Char -> Integer
        | Fable.Number(kind, _) ->
            match kind with
            | Int8
            | UInt8
            | Int16
            | UInt16
            | Int32
            | UInt32
            | Int64
            | UInt64
            | Int128
            | UInt128 -> Integer
            | Float16
            | Float32
            | Float64 -> Double
            | Decimal
            | BigInt
            | NativeInt
            | UNativeInt -> Dynamic // TODO
        | Fable.Option(genArg, _isStruct) -> transformOptionType com ctx genArg
        | Fable.Array(TransformType com ctx genArg, _) -> List genArg
        | Fable.List(TransformType com ctx genArg) ->
            Type.reference (getFSharpListTypeIdent com ctx, [ genArg ])
        | Fable.Tuple(genArgs, _) ->
            transformGenArgs com ctx genArgs |> transformTupleType com ctx
        | Fable.AnonymousRecordType(_, genArgs, _) ->
            genArgs
            |> List.map FableTransforms.uncurryType
            |> transformGenArgs com ctx
            |> transformTupleType com ctx
        | Fable.LambdaType(TransformType com ctx argType,
                           TransformType com ctx returnType) ->
            Function([ argType ], returnType)
        | Fable.DelegateType(argTypes, TransformType com ctx returnType) ->
            let argTypes = argTypes |> List.map (transformType com ctx)
            Function(argTypes, returnType)
        | Fable.GenericParam(name, _isMeasure, _constraints) -> Generic name
        | Fable.DeclaredType(ref, genArgs) ->
            transformDeclaredType com ctx ref genArgs
        | Fable.Regex -> makeTypeRefFromName "RegExp" []

    let transformIdentWith
        (com: IDartCompiler)
        ctx
        (isMutable: bool)
        (typ: Fable.Type)
        name
        : Ident
        =
        let typ = transformType com ctx typ
        makeIdent isMutable typ name

    let transformIdent (com: IDartCompiler) ctx (id: Fable.Ident) : Ident =
        transformIdentWith com ctx id.IsMutable id.Type id.Name

    let transformIdentAsExpr (com: IDartCompiler) ctx (id: Fable.Ident) =
        transformIdent com ctx id |> Expression.identExpression

    let transformGenericParam
        (com: IDartCompiler)
        ctx
        (g: Fable.GenericParam)
        : GenericParam option
        =
        if g.IsMeasure then
            None
        else
            let extends =
                g.Constraints
                |> List.tryPick (
                    function
                    | Fable.Constraint.CoercesTo t ->
                        transformType com ctx t |> Some
                    | _ -> None
                )

            Some
                {
                    Name = g.Name
                    Extends = extends
                }

    let transformImport
        (com: IDartCompiler)
        ctx
        r
        t
        (selector: string)
        (path: string)
        =
        let rec getParts t (parts: string list) (expr: Expression) =
            match parts with
            | [] -> expr
            | [ part ] -> get (transformType com ctx t) expr part
            | m :: ms -> get Dynamic expr m |> getParts t ms

        let selector, parts =
            let parts = Array.toList (selector.Split('.'))
            parts.Head, parts.Tail

        com.GetImportIdent(
            ctx,
            selector,
            path,
            (match parts with
             | [] -> t
             | _ -> Fable.Any),
            ?range = r
        )
        |> Expression.identExpression
        |> getParts t parts

    let transformNumberLiteral com r kind (x: obj) =
        match kind, x with
        | Dart.Replacements.DartInt, (:? char as x) ->
            Expression.integerLiteral (int64 x)
        | Int8, (:? int8 as x) -> Expression.integerLiteral (int64 x)
        | UInt8, (:? uint8 as x) -> Expression.integerLiteral (int64 x)
        | Int16, (:? int16 as x) -> Expression.integerLiteral (int64 x)
        | UInt16, (:? uint16 as x) -> Expression.integerLiteral (int64 x)
        | Int32, (:? int32 as x) -> Expression.integerLiteral (x)
        | UInt32, (:? uint32 as x) -> Expression.integerLiteral (int64 x)
        | Int64, (:? int64 as x) -> Expression.integerLiteral (x)
        | UInt64, (:? uint64 as x) -> Expression.integerLiteral (int64 x)
        | Float32, (:? float32 as x) -> Expression.doubleLiteral (float x)
        | Float64, (:? float as x) -> Expression.doubleLiteral (x)
        | _ ->
            $"Expected literal of type %A{kind} but got {x.GetType().FullName}"
            |> addErrorAndReturnNull com r

    let transformTuple (com: IDartCompiler) ctx (args: Expression list) =
        let tup = List.length args |> getTupleTypeIdent com ctx
        let genArgs = args |> List.map (fun a -> a.Type)
        let t = Type.reference (tup, genArgs)

        let isConst, args =
            if areConstTypes genArgs && areConstExprs ctx args then
                true, List.map removeConst args
            else
                false, args
        // Generic arguments can be omitted from invocation expression
        Expression.invocationExpression (tup.Expr, args, t, isConst = isConst)

    let transformValue
        (com: IDartCompiler)
        (ctx: Context)
        (r: SourceLocation option)
        returnStrategy
        kind
        : Statement list * CapturedExpr
        =
        match kind with
        | Fable.UnitConstant -> [], None
        | Fable.ThisValue t ->
            transformType com ctx t
            |> ThisExpression
            |> resolveExpr returnStrategy
        | Fable.BaseValue(None, t) ->
            transformType com ctx t
            |> SuperExpression
            |> resolveExpr returnStrategy
        | Fable.BaseValue(Some boundIdent, _) ->
            transformIdentAsExpr com ctx boundIdent
            |> resolveExpr returnStrategy
        | Fable.TypeInfo(t, _d) ->
            transformType com ctx t |> TypeLiteral |> resolveExpr returnStrategy
        | Fable.Null t ->
            transformType com ctx t
            |> Expression.nullLiteral
            |> resolveExpr returnStrategy
        | Fable.BoolConstant v ->
            Expression.booleanLiteral v |> resolveExpr returnStrategy
        | Fable.CharConstant v ->
            Expression.integerLiteral (int v) |> resolveExpr returnStrategy
        | Fable.StringConstant v ->
            Expression.stringLiteral v |> resolveExpr returnStrategy
        | Fable.StringTemplate(_tag, parts, values) ->
            transformExprsAndResolve
                com
                ctx
                returnStrategy
                values
                (fun values -> Expression.InterpolationString(parts, values))

        // Dart enums are limited as we cannot set arbitrary values or combine them as flags
        // so for now we compile F# enums as integers
        | Fable.NumberConstant(x, kind, _) ->
            transformNumberLiteral com r kind x |> resolveExpr returnStrategy

        | Fable.RegexConstant(source, flags) ->
            let flagToArg =
                function
                | RegexIgnoreCase ->
                    Some(Some "caseSensitive", Expression.booleanLiteral false)
                | RegexMultiline ->
                    Some(Some "multiLine", Expression.booleanLiteral true)
                | RegexSingleline ->
                    Some(Some "dotAll", Expression.booleanLiteral true)
                | RegexUnicode ->
                    Some(Some "unicode", Expression.booleanLiteral true)
                | RegexGlobal
                | RegexSticky -> None

            let regexIdent = makeImmutableIdent MetaType "RegExp"

            let args =
                [
                    None, Expression.stringLiteral source
                    yield! flags |> List.choose flagToArg
                ]

            Expression.invocationExpression (
                regexIdent.Expr,
                args,
                Type.reference regexIdent
            )
            |> resolveExpr returnStrategy

        | Fable.NewOption(expr, genArg, _isStruct) ->
            let transformOption
                (com: IDartCompiler)
                ctx
                genArg
                (arg: Expression)
                =
                let cons = libValue com ctx Fable.MetaType "Types" "Some"
                let t = transformOptionType com ctx genArg

                let isConst, args =
                    if areConstTypes t.Generics && isConstExpr ctx arg then
                        true, [ removeConst arg ]
                    else
                        false, [ arg ]

                Expression.invocationExpression (
                    cons.Expr,
                    args,
                    t,
                    isConst = isConst
                )

            match expr with
            | Some expr ->
                transformExprAndResolve
                    com
                    ctx
                    returnStrategy
                    expr
                    (transformOption com ctx genArg)

            | None ->
                transformType com ctx genArg
                |> Expression.nullLiteral
                |> resolveExpr returnStrategy

        | Fable.NewTuple(exprs, _) ->
            transformExprsAndResolve
                com
                ctx
                returnStrategy
                exprs
                (transformTuple com ctx)

        | Fable.NewArray(Fable.ArrayValues exprs, typ, _) ->
            transformExprsAndResolve
                com
                ctx
                returnStrategy
                exprs
                (makeMutableListExpr com ctx typ)
        // We cannot allocate in Dart without filling the array to a non-null value
        | Fable.NewArray((Fable.ArrayFrom expr | Fable.ArrayAlloc expr), typ, _) ->
            transformExprsAndResolve
                com
                ctx
                returnStrategy
                [ expr ]
                (fun exprs ->
                    let listIdent = makeImmutableIdent MetaType "List"
                    let typ = transformType com ctx typ

                    Expression.invocationExpression (
                        listIdent.Expr,
                        "of",
                        exprs,
                        Type.reference (listIdent, [ typ ])
                    )
                )

        | Fable.NewRecord(values, ref, genArgs) ->
            transformExprsAndResolve
                com
                ctx
                returnStrategy
                values
                (fun args ->
                    let ent = com.GetEntity(ref)
                    let genArgs = transformGenArgs com ctx genArgs
                    let consRef = getEntityIdent com ctx ent
                    let typeRef = Type.reference (consRef, genArgs)

                    let isConst =
                        areConstTypes genArgs
                        && List.forall (isConstExpr ctx) args
                        && (ent.FSharpFields
                            |> List.forall (fun f -> not f.IsMutable))

                    let args =
                        if isConst then
                            List.map removeConst args
                        else
                            args

                    Expression.invocationExpression (
                        consRef.Expr,
                        args,
                        typeRef,
                        genArgs = genArgs,
                        isConst = isConst
                    )
                )
        | Fable.NewAnonymousRecord(exprs, _fieldNames, _genArgs, _isStruct) ->
            transformExprsAndResolve
                com
                ctx
                returnStrategy
                exprs
                (transformTuple com ctx)

        | Fable.NewUnion(values, tag, ref, genArgs) ->
            transformExprsAndResolve
                com
                ctx
                returnStrategy
                values
                (fun fields ->
                    let ent = com.GetEntity(ref)
                    let genArgs = transformGenArgs com ctx genArgs
                    let consRef = getEntityIdent com ctx ent
                    let uci = ent.UnionCases |> List.item tag

                    let consRef, args =
                        match fields with
                        | [] ->
                            let caseName = getUnionCaseName uci

                            let tag =
                                Expression.integerLiteral (tag)
                                |> Expression.commented caseName

                            consRef, [ tag ]
                        | fields ->
                            { consRef with
                                Name =
                                    getUnionCaseDeclarationName
                                        consRef.Name
                                        uci
                            },
                            fields

                    let isConst, args =
                        if areConstTypes genArgs && areConstExprs ctx args then
                            true, List.map removeConst args
                        else
                            false, args

                    let typeRef = Type.reference (consRef, genArgs)

                    Expression.invocationExpression (
                        consRef.Expr,
                        args,
                        typeRef,
                        genArgs = genArgs,
                        isConst = isConst
                    )
                )

        | Fable.NewList(headAndTail, typ) ->
            let rec getItems acc =
                function
                | None -> List.rev acc, None
                | Some(head, Fable.Value(Fable.NewList(tail, _), _)) ->
                    getItems (head :: acc) tail
                | Some(head, tail) -> List.rev (head :: acc), Some tail

            match getItems [] headAndTail with
            | [], None ->
                libGenCall com ctx (Fable.List typ) "List" "empty" [] [ typ ]
                |> resolveExpr returnStrategy

            | [ expr ], None ->
                transformExprsAndResolve
                    com
                    ctx
                    returnStrategy
                    [ expr ]
                    (fun exprs ->
                        libCall
                            com
                            ctx
                            (Fable.List typ)
                            "List"
                            "singleton"
                            exprs
                    )

            | exprs, None ->
                transformExprsAndResolve
                    com
                    ctx
                    returnStrategy
                    exprs
                    (fun exprs ->
                        [ makeImmutableListExpr com ctx typ exprs ]
                        |> libCall com ctx (Fable.List typ) "List" "ofArray"
                    )

            | [ head ], Some tail ->
                transformExprsAndResolve
                    com
                    ctx
                    returnStrategy
                    [
                        head
                        tail
                    ]
                    (fun exprs ->
                        libCall com ctx (Fable.List typ) "List" "cons" exprs
                    )

            | exprs, Some tail ->
                transformExprsAndResolve
                    com
                    ctx
                    returnStrategy
                    (exprs @ [ tail ])
                    (fun exprs ->
                        let exprs, tail = List.splitLast exprs

                        [
                            makeImmutableListExpr com ctx typ exprs
                            tail
                        ]
                        |> libCall
                            com
                            ctx
                            (Fable.List typ)
                            "List"
                            "ofArrayWithTail"
                    )

    let transformOperation
        com
        ctx
        (_: SourceLocation option)
        t
        returnStrategy
        opKind
        : Statement list * CapturedExpr
        =
        match opKind with
        | Fable.Unary(op, expr) ->
            transformExprAndResolve
                com
                ctx
                returnStrategy
                expr
                (fun expr -> UnaryExpression(op, expr))

        | Fable.Binary(op, left, right) ->
            transformExprsAndResolve2
                com
                ctx
                returnStrategy
                left
                right
                (fun left right ->
                    BinaryExpression(op, left, right, transformType com ctx t)
                )

        | Fable.Logical(op, left, right) ->
            // We cannot combine expressions here because statements of the second expression
            // are supposed not to run if the first is false (AND) or true (OR)
            let statements1, expr1 = transformAndCaptureExpr com ctx left
            let statements2, expr2 = transformAndCaptureExpr com ctx right

            match statements2 with
            | [] ->
                let statements3, expr3 =
                    LogicalExpression(op, expr1, expr2)
                    |> resolveExpr returnStrategy

                statements1 @ statements3, expr3

            | statements2 ->
                let expr1, defValue =
                    match op with
                    | LogicalAnd -> expr1, Expression.booleanLiteral (false)
                    | LogicalOr ->
                        Expression.unaryExpression (UnaryNot, expr1),
                        Expression.booleanLiteral (true)

                let captureStatements, captureExpr, returnStrategy =
                    convertCaptureStrategyIntoAssign
                        com
                        ctx
                        Fable.Boolean
                        returnStrategy
                        (Some defValue)

                let statements2', _ = resolveExpr returnStrategy expr2
                let statements2 = statements2 @ statements2'

                statements1
                @ captureStatements
                @ [ Statement.ifStatement (expr1, statements2) ],
                captureExpr

    let transformEmit
        (com: IDartCompiler)
        ctx
        t
        returnStrategy
        (emitInfo: Fable.EmitInfo)
        =
        let info = emitInfo.CallInfo
        let statements, args = transformCallArgs com ctx (CallInfo info)
        let args = List.map snd args

        let emitExpr =
            Expression.emitExpression (
                emitInfo.Macro,
                args,
                transformType com ctx t
            )

        if emitInfo.IsStatement then
            // Ignore the return strategy
            statements @ [ ExpressionStatement(emitExpr) ], None
        else
            let statements2, captureExpr = resolveExpr returnStrategy emitExpr
            statements @ statements2, captureExpr

    let transformCall
        com
        ctx
        range
        (t: Fable.Type)
        returnStrategy
        callee
        callInfo
        =
        let argsLen (i: Fable.CallInfo) =
            List.length i.Args
            + (if Option.isSome i.ThisArg then
                   1
               else
                   0)

        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Return _, Some tc when
            tc.IsRecursiveRef(callee) && argsLen callInfo = List.length tc.Args
            ->
            let args =
                match callInfo.ThisArg with
                | Some thisArg -> thisArg :: callInfo.Args
                | None -> callInfo.Args

            optimizeTailCall com ctx range tc args, None
        | _ ->
            // Try to optimize some patterns after FableTransforms
            let optimized =
                match callInfo.Tags, callInfo.Args with
                | Fable.Tags.Contains "array",
                  [ Replacements.Util.ArrayOrListLiteral(vals, _) ] ->
                    Fable.Value(
                        Fable.NewArray(
                            Fable.ArrayValues vals,
                            Fable.Any,
                            Fable.MutableArray
                        ),
                        range
                    )
                    |> Some
                | Fable.Tags.Contains "ignore", [ arg ] ->
                    match returnStrategy with
                    // If we're not going to return or assign the value we can skip the `ignore` call
                    | Return(isVoid = true)
                    | Ignore -> Some arg
                    | _ -> None
                // TODO
                // | Some "const-map"
                | _ -> None

            match optimized with
            | Some e -> transform com ctx returnStrategy e
            | None ->
                let t = transformType com ctx t
                let genArgs = transformGenArgs com ctx callInfo.GenericArgs

                let calleeStatements, callee =
                    transformAndCaptureExpr com ctx callee

                let argStatements, args =
                    transformCallArgs com ctx (CallInfo callInfo)

                let statements, callee =
                    combineCalleeAndArgStatements
                        com
                        ctx
                        calleeStatements
                        argStatements
                        callee

                let isConst =
                    areConstTypes genArgs
                    && List.forall (snd >> isConstExpr ctx) args
                    && callInfo.MemberRef
                       |> Option.bind com.TryGetMember
                       |> Option.map (fun m -> hasConstAttribute m.Attributes)
                       |> Option.defaultValue false

                let args =
                    if isConst then
                        args
                        |> List.map (fun (name, arg) -> name, removeConst arg)
                    else
                        args

                let statements2, capturedExpr =
                    Expression.invocationExpression (
                        callee,
                        args,
                        t,
                        genArgs,
                        isConst = isConst
                    )
                    |> resolveExpr returnStrategy

                statements @ statements2, capturedExpr

    let transformCurriedApplyAsStatements
        com
        ctx
        range
        t
        returnStrategy
        callee
        args
        =
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Return _, Some tc when
            tc.IsRecursiveRef(callee) && List.sameLength args tc.Args
            ->
            optimizeTailCall com ctx range tc args, None
        | _ ->
            let t = transformType com ctx t

            let calleeStatements, callee =
                transformAndCaptureExpr com ctx callee

            let argStatements, args =
                transformCallArgs com ctx (NoCallInfo args)

            let statements, callee =
                combineCalleeAndArgStatements
                    com
                    ctx
                    calleeStatements
                    argStatements
                    callee

            let invocation =
                match args with
                | [] -> Expression.invocationExpression (callee, t)
                | args ->
                    (callee, args)
                    ||> List.fold (fun expr arg ->
                        Expression.invocationExpression (expr, [ arg ], t)
                    )

            let statements2, capturedExpr =
                resolveExpr returnStrategy invocation

            statements @ statements2, capturedExpr

    let typeImplementsOrExtends
        (com: IDartCompiler)
        (baseEnt: Fable.EntityRef)
        (t: Fable.Type)
        =
        match baseEnt.FullName, t with
        | baseFullName, Fable.DeclaredType(e, _) ->
            let baseEnt = com.GetEntity(baseEnt)
            let e = com.GetEntity(e)

            if baseEnt.IsInterface then
                e.AllInterfaces
                |> Seq.exists (fun i -> i.Entity.FullName = baseFullName)
            else
                let rec extends baseFullName (e: Fable.Entity) =
                    match e.BaseType with
                    | Some baseType ->
                        if baseType.Entity.FullName = baseFullName then
                            true
                        else
                            com.GetEntity(baseType.Entity)
                            |> extends baseFullName
                    | None -> false

                extends baseFullName e
        | baseFullName, Fable.GenericParam(_, _, constraints) ->
            constraints
            |> List.exists (
                function
                | Fable.Constraint.CoercesTo(Fable.DeclaredType(e, _)) ->
                    e.FullName = baseFullName
                | _ -> false
            )
        | _ -> false

    let transformCast
        (com: IDartCompiler)
        (ctx: Context)
        targetType
        returnStrategy
        (expr: Fable.Expr)
        =
        match targetType, expr with
        | Fable.DeclaredType(baseEnt, _), _ when
            typeImplementsOrExtends com baseEnt expr.Type
            ->
            com.Transform(ctx, returnStrategy, expr)

        | Fable.Any, _ -> com.Transform(ctx, returnStrategy, expr)

        | Fable.Unit, _ ->
            let returnStrategy =
                match returnStrategy with
                | Return(isVoid = true) -> returnStrategy
                | _ -> Ignore

            com.Transform(ctx, returnStrategy, expr)

        | _ ->
            transformExprAndResolve
                com
                ctx
                returnStrategy
                expr
                (fun expr ->
                    let source = expr.Type
                    let target = transformType com ctx targetType

                    match expr, target with
                    | IdentExpression {
                                          Name = name
                                          ImportModule = None
                                      },
                      target when
                        Map.matchesKeyValue name target ctx.AssertedTypes
                        ->
                        expr
                    | _, target ->
                        if Type.needsCast source target then
                            Expression.asExpression (expr, target)
                        else
                            expr
                )

    // TODO: Try to identify type testing in the catch clause and use Dart's `on ...` exception checking
    let transformTryCatch
        com
        ctx
        _r
        returnStrategy
        (body: Fable.Expr, catch, finalizer)
        =
        let prevStatements, captureExpr, returnStrategy =
            convertCaptureStrategyIntoAssign
                com
                ctx
                body.Type
                returnStrategy
                None
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }

        let handlers =
            catch
            |> Option.map (fun (param, body) ->
                let param = transformIdent com ctx param
                let body, _ = com.Transform(ctx, returnStrategy, body)
                //                let test = TypeReference(getExceptionTypeIdent com ctx, [])
                CatchClause(param = param, body = body)
            )
            |> Option.toList

        let finalizer =
            finalizer |> Option.map (transform com ctx Ignore >> fst)

        let statements, _ = transform com ctx returnStrategy body

        prevStatements
        @ [
            Statement.tryStatement (
                statements,
                handlers = handlers,
                ?finalizer = finalizer
            )
        ],
        captureExpr

    /// Branching expressions like conditionals, decision trees or try catch cannot capture
    /// the resulting expression at once so declare a variable and assign the potential results to it
    let convertCaptureStrategyIntoAssign com ctx t returnStrategy initialValue =
        match returnStrategy with
        | Capture(binding) ->
            let varDecl, ident =
                match binding with
                | Some ident -> [], ident
                | None ->
                    let t = transformType com ctx t
                    let isMutable = Option.isSome initialValue

                    let ident =
                        getUniqueNameInDeclarationScope ctx "tmp_capture"
                        |> makeIdent isMutable t

                    let varDecl =
                        Statement.tempVariableDeclaration (
                            ident,
                            isMutable = isMutable,
                            ?value = initialValue
                        )

                    [ varDecl ], ident

            varDecl, Some ident.Expr, Assign ident.Expr
        | _ -> [], None, returnStrategy

    let transformConditional
        (com: IDartCompiler)
        ctx
        returnStrategy
        guardExpr
        (thenExpr: Fable.Expr)
        elseExpr
        =
        let guardStatements, guardExpr =
            transformAndCaptureExpr com ctx guardExpr

        let captureStatements, captureExpr, returnStrategy =
            convertCaptureStrategyIntoAssign
                com
                ctx
                thenExpr.Type
                returnStrategy
                None

        let thenCtx = { ctx with CastedUnions = Dictionary(ctx.CastedUnions) }

        let thenCtx =
            match guardExpr with
            | IsExpression(IdentExpression id, assertedType, false) ->
                { thenCtx with
                    AssertedTypes =
                        Map.add id.Name assertedType thenCtx.AssertedTypes
                }
            | _ -> thenCtx

        let thenStatements, _ = com.Transform(thenCtx, returnStrategy, thenExpr)
        let elseStatements, _ = com.Transform(ctx, returnStrategy, elseExpr)

        let assignmentExpr =
            match captureExpr, returnStrategy with
            | Some(IdentExpression ident), _ -> Some ident
            | _, Assign(IdentExpression ident) -> Some ident
            | _ -> None

        match assignmentExpr, thenStatements, elseStatements with
        | Some ident,
          [ ExpressionStatement(AssignmentExpression(IdentExpression ident1,
                                                     AssignEqual,
                                                     value1)) ],
          [ ExpressionStatement(AssignmentExpression(IdentExpression ident2,
                                                     AssignEqual,
                                                     value2)) ] when
            ident.Name = ident1.Name && ident.Name = ident2.Name
            ->

            let cond =
                Expression.conditionalExpression (guardExpr, value1, value2)

            if Option.isSome captureExpr then
                guardStatements, Some cond
            else
                guardStatements @ (resolveExpr returnStrategy cond |> fst), None

        | Some ident,
          [ ExpressionStatement(AssignmentExpression(IdentExpression ident1,
                                                     AssignEqual,
                                                     value1)) ],
          [ IfStatement(guardExpr2,
                        [ ExpressionStatement(AssignmentExpression(IdentExpression ident2,
                                                                   AssignEqual,
                                                                   value2)) ],
                        [ ExpressionStatement(AssignmentExpression(IdentExpression ident3,
                                                                   AssignEqual,
                                                                   value3)) ]) ] when
            ident.Name = ident1.Name
            && ident.Name = ident2.Name
            && ident.Name = ident3.Name
            ->

            let cond =
                Expression.conditionalExpression (
                    guardExpr,
                    value1,
                    Expression.conditionalExpression (
                        guardExpr2,
                        value2,
                        value3
                    )
                )

            if Option.isSome captureExpr then
                guardStatements, Some cond
            else
                guardStatements @ (resolveExpr returnStrategy cond |> fst), None

        | _ ->
            guardStatements
            @ captureStatements
            @ [
                Statement.ifStatement (
                    guardExpr,
                    thenStatements,
                    elseStatements
                )
            ],
            captureExpr

    let transformGet
        (com: IDartCompiler)
        ctx
        _range
        t
        returnStrategy
        kind
        fableExpr
        =
        match kind with
        | Fable.ExprGet prop ->
            transformExprsAndResolve2
                com
                ctx
                returnStrategy
                fableExpr
                prop
                (fun expr prop ->
                    let t = transformType com ctx t
                    Expression.indexExpression (expr, prop, t)
                )

        | Fable.FieldGet info ->
            match fableExpr.Type with
            | Fable.AnonymousRecordType(fieldNames, _genArgs, _isStruct) ->
                let index =
                    fieldNames
                    |> Array.tryFindIndex ((=) info.Name)
                    |> Option.defaultValue 0

                transformExprAndResolve
                    com
                    ctx
                    returnStrategy
                    fableExpr
                    (fun expr ->
                        let t = transformType com ctx t

                        Expression.propertyAccess (
                            expr,
                            $"item%i{index + 1}",
                            t
                        )
                    )
            | _ ->
                let fieldName = sanitizeMember info.Name

                let fableExpr =
                    match fableExpr with
                    // If we're accessing a virtual member with default implementation (see #701)
                    // from base class, we can use `super` so we don't need the bound this arg
                    | Fable.Value(Fable.BaseValue(_, t), r) ->
                        Fable.Value(Fable.BaseValue(None, t), r)
                    | _ -> fableExpr

                transformExprAndResolve
                    com
                    ctx
                    returnStrategy
                    fableExpr
                    (fun expr ->
                        let t = transformType com ctx t

                        Expression.propertyAccess (
                            expr,
                            fieldName,
                            t,
                            isConst = List.contains "const" info.Tags
                        )
                    )

        | Fable.ListHead ->
            transformExprAndResolve
                com
                ctx
                returnStrategy
                fableExpr
                (fun expr -> libCall com ctx t "List" "head" [ expr ])

        | Fable.ListTail ->
            transformExprAndResolve
                com
                ctx
                returnStrategy
                fableExpr
                (fun expr -> libCall com ctx t "List" "tail" [ expr ])

        | Fable.TupleIndex index ->
            match fableExpr with
            // Check the erased expressions don't have side effects?
            | Fable.Value(Fable.NewTuple(exprs, _), _) ->
                List.item index exprs |> transform com ctx returnStrategy
            | fableExpr ->
                transformExprAndResolve
                    com
                    ctx
                    returnStrategy
                    fableExpr
                    (fun expr ->
                        let t = transformType com ctx t

                        Expression.propertyAccess (
                            expr,
                            $"item%i{index + 1}",
                            t
                        )
                    )

        | Fable.OptionValue ->
            transformExprAndResolve
                com
                ctx
                returnStrategy
                fableExpr
                (fun expr ->
                    let t = transformType com ctx t
                    libCallWithType com ctx t "Types" "value" [ expr ]
                )

        | Fable.UnionTag ->
            transformExprAndResolve
                com
                ctx
                returnStrategy
                fableExpr
                getUnionExprTag

        | Fable.UnionField info ->
            let statements, expr = transformAndCaptureExpr com ctx fableExpr

            let ent = com.GetEntity(info.Entity)
            let uci = ent.UnionCases |> List.item info.CaseIndex
            let field = uci.UnionCaseFields |> List.item info.FieldIndex

            let unionCaseType =
                match
                    transformDeclaredType com ctx info.Entity info.GenericArgs
                with
                | TypeReference(ident, generics, _info) ->
                    // Discard type info, as we don't consider the case type as union
                    // (this is mainly to skip the type in the variable declaration)
                    Type.reference (
                        { ident with
                            Name = getUnionCaseDeclarationName ident.Name uci
                        },
                        generics
                    )
                    |> Some
                | _ -> None // Unexpected, error?

            let statements2, expr =
                match unionCaseType with
                | None -> [], expr
                | Some unionCaseType ->
                    match expr with
                    | IdentExpression id ->
                        match ctx.CastedUnions.TryGetValue(id.Name) with
                        | true, newIdName ->
                            [],
                            IdentExpression
                                { id with
                                    Name = newIdName
                                    Type = unionCaseType
                                }
                        | false, _ ->
                            let newIdName =
                                getUniqueNameInDeclarationScope ctx id.Name

                            ctx.CastedUnions.Add(id.Name, newIdName)

                            let newIdent =
                                { id with
                                    Name = newIdName
                                    Type = unionCaseType
                                }

                            [
                                Statement.tempVariableDeclaration (
                                    newIdent,
                                    value =
                                        Expression.asExpression (
                                            expr,
                                            unionCaseType
                                        )
                                )
                            ],
                            IdentExpression newIdent
                    | _ -> [], Expression.asExpression (expr, unionCaseType)

            let statements3, capturedExpr =
                sanitizeMember field.Name
                |> get (transformType com ctx t) expr
                |> resolveExpr returnStrategy

            statements @ statements2 @ statements3, capturedExpr

    let transformFunction
        com
        ctx
        name
        (args: Fable.Ident list)
        (body: Fable.Expr)
        : Ident list * Statement list * Type
        =
        let tailcallChance =
            Option.map
                (fun name ->
                    NamedTailCallOpportunity(com, ctx, name, args)
                    :> ITailCallOpportunity
                )
                name

        let args = FSharp2Fable.Util.discardUnitArg args
        let mutable isTailCallOptimized = false
        let varsInScope = args |> List.map (fun a -> a.Name) |> HashSet

        let ctx =
            { ctx with
                TailCallOpportunity = tailcallChance
                VarsDeclaredInScope = varsInScope
                OptimizeTailCall = fun () -> isTailCallOptimized <- true
            }

        let returnType = transformType com ctx body.Type
        let returnStrategy = Return(isVoid = (returnType = Void))
        let body, _ = transform com ctx returnStrategy body

        match isTailCallOptimized, tailcallChance with
        | true, Some tc ->
            // Replace args, see NamedTailCallOpportunity constructor
            let args' =
                List.zip args tc.Args
                |> List.map (fun (id, tcArg) ->
                    let t = transformType com ctx id.Type
                    makeImmutableIdent t tcArg
                )

            let varDecls =
                List.zip args args'
                |> List.map (fun (id, tcArg) ->
                    let ident = transformIdent com ctx id

                    Statement.tempVariableDeclaration (
                        ident,
                        value = Expression.identExpression (tcArg)
                    )
                )

            let body =
                match returnStrategy with
                // Make sure we don't get trapped in an infinite loop, see #1624
                | Return(isVoid = true) ->
                    varDecls @ body @ [ Statement.breakStatement () ]
                | _ -> varDecls @ body

            args',
            [
                Statement.labeledStatement (
                    tc.Label,
                    Statement.whileStatement (
                        Expression.booleanLiteral (true),
                        body
                    )
                )
            ],
            returnType

        | _ -> args |> List.map (transformIdent com ctx), body, returnType

    let transformSet
        (com: IDartCompiler)
        ctx
        _range
        kind
        toBeSet
        (value: Fable.Expr)
        =
        let statements1, toBeSet = transformAndCaptureExpr com ctx toBeSet

        match kind with
        | Fable.ValueSet ->
            let statements2, _ = transform com ctx (Assign toBeSet) value
            statements1 @ statements2
        | Fable.ExprSet(prop) ->
            let statements2, prop = transformAndCaptureExpr com ctx prop
            let toBeSet = getExpr Dynamic toBeSet prop
            let statements3, _ = transform com ctx (Assign toBeSet) value
            statements1 @ statements2 @ statements3
        | Fable.FieldSet(fieldName) ->
            let fieldName = sanitizeMember fieldName
            let toBeSet = get Dynamic toBeSet fieldName
            let statements2, _ = transform com ctx (Assign toBeSet) value
            statements1 @ statements2

    let transformBinding
        (com: IDartCompiler)
        ctx
        (var: Fable.Ident)
        (value: Fable.Expr)
        =
        let ident = transformIdent com ctx var

        let valueStatements, value =
            match value with
            | Function(args, body) ->
                let genParams =
                    args
                    |> List.map (fun a -> a.Type)
                    |> getLocalFunctionGenericParams com ctx value.Range

                let ctx = ctx.AppendLocalGenParams(genParams)
                // Pass the name of the bound ident to enable tail-call optimizations
                let args, body, returnType =
                    transformFunction com ctx (Some var.Name) args body

                [],
                Expression.anonymousFunction (args, body, returnType, genParams)
                |> Some
            | value -> com.Transform(ctx, Capture(binding = Some ident), value)

        match value with
        | Some(IdentExpression ident2) when ident.Name = ident2.Name ->
            let kind =
                if var.IsMutable then
                    Var
                else
                    Final

            let varDecl =
                Statement.variableDeclaration (
                    ident,
                    kind,
                    addToScope = ctx.AddToScope
                )

            ctx, varDecl :: valueStatements
        | _ ->
            let value =
                value |> Option.defaultValue (Expression.nullLiteral ident.Type)

            let kind, value = getVarKind ctx var.IsMutable value

            let ctx =
                match kind with
                | Const ->
                    { ctx with
                        ConstIdents = Set.add ident.Name ctx.ConstIdents
                    }
                | Var
                | Final -> ctx
            // If value is an anonymous function this will be converted into function declaration in printing step
            ctx,
            valueStatements
            @ [
                Statement.variableDeclaration (
                    ident,
                    kind,
                    value = value,
                    addToScope = ctx.AddToScope
                )
            ]

    let transformSwitch
        (com: IDartCompiler)
        ctx
        returnStrategy
        evalExpr
        cases
        defaultCase
        =
        let cases =
            cases
            |> List.choose (fun (guards, expr) ->
                // Remove empty branches
                match returnStrategy, expr, guards with
                | (Return(isVoid = true) | Ignore),
                  Fable.Value(Fable.UnitConstant, _),
                  _
                | _, _, [] -> None
                | _, _, guards ->
                    // Switch is only activated when guards are literals so we can ignore the statements
                    let guards =
                        guards
                        |> List.map (transformAndCaptureExpr com ctx >> snd)
                    // Create new instance of CastedUnions dictionary as we do with conditional
                    // branches (switch guard may be a union tag test)
                    let ctx =
                        { ctx with
                            CastedUnions = Dictionary(ctx.CastedUnions)
                        }

                    let caseBody, _ = com.Transform(ctx, returnStrategy, expr)
                    SwitchCase(guards, caseBody) |> Some
            )

        let cases, defaultCase =
            match defaultCase with
            | Some expr ->
                cases, com.Transform(ctx, returnStrategy, expr) |> fst
            | None ->
                // Dart may complain if we're not covering all cases so turn the last case into default
                let cases, lastCase = List.splitLast cases
                cases, lastCase.Body

        let evalStatements, evalExpr = transformAndCaptureExpr com ctx evalExpr

        evalStatements
        @ [ Statement.switchStatement (evalExpr, cases, defaultCase) ]

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then
            []
        elif List.sameLength idents values then
            List.zip idents values
        else
            failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues
        (com: IDartCompiler)
        (ctx: Context)
        targetIndex
        boundValues
        =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues

        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects expr then
                        (ident, expr) :: bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements
                )

            let target = FableTransforms.replaceValues replacements target
            List.rev bindings, target
        else
            identsAndValues, target

    let transformDecisionTreeSuccess
        (com: IDartCompiler)
        (ctx: Context)
        returnStrategy
        targetIndex
        boundValues
        =
        match returnStrategy with
        | Target targetId ->
            let idents, _ = getDecisionTarget ctx targetIndex

            let assignments =
                matchTargetIdentAndValues idents boundValues
                |> List.collect (fun (id, value) ->
                    let id = transformIdentAsExpr com ctx id
                    transform com ctx (Assign id) value |> fst
                )

            let targetAssignment =
                assign
                    None
                    (IdentExpression targetId)
                    (Expression.integerLiteral targetIndex)
                |> ExpressionStatement

            targetAssignment :: assignments, None
        | ret ->
            let bindings, target =
                getDecisionTargetAndBindValues com ctx targetIndex boundValues

            let ctx, bindings =
                ((ctx, []), bindings)
                ||> List.fold (fun (ctx, bindings) (i, v) ->
                    let ctx, newBindings = transformBinding com ctx i v
                    ctx, bindings @ newBindings
                )

            let statements, capturedExpr = com.Transform(ctx, ret, target)
            bindings @ statements, capturedExpr

    let canTransformDecisionTreeAsSwitch expr =
        let (|Equals|_|) =
            function
            | Fable.Operation(Fable.Binary(BinaryEqual, expr, right), _, _, _) ->
                match expr with
                | Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _),
                              _) -> Some(expr, right)
                | _ -> None
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr =
                    Fable.Get(expr, Fable.UnionTag, numType Int32, None)

                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None

        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1, Fable.UnionTag, _, _),
              Fable.Get(Fable.IdentExpr i2, Fable.UnionTag, _, _) ->
                i1.Name = i2.Name
            | Fable.Get(Fable.IdentExpr i1, Fable.FieldGet fieldInfo1, _, _),
              Fable.Get(Fable.IdentExpr i2, Fable.FieldGet fieldInfo2, _, _) ->
                i1.Name = i2.Name && fieldInfo1.Name = fieldInfo2.Name
            | _ -> false

        let rec checkInner cases evalExpr =
            function
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex,
                                                         boundValues,
                                                         _),
                               treeExpr,
                               _) when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex,
                                            defaultBoundValues,
                                            _) ->
                    let cases =
                        (caseExpr, targetIndex, boundValues) :: cases
                        |> List.rev

                    Some(
                        evalExpr,
                        cases,
                        (defaultTargetIndex, defaultBoundValues)
                    )
                | treeExpr ->
                    checkInner
                        ((caseExpr, targetIndex, boundValues) :: cases)
                        evalExpr
                        treeExpr
            | _ -> None

        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _),
                           treeExpr,
                           _) ->
            match
                checkInner
                    [ caseExpr, targetIndex, boundValues ]
                    evalExpr
                    treeExpr
            with
            | Some(evalExpr, cases, defaultCase) ->
                Some(evalExpr, cases, defaultCase)
            | None -> None
        | _ -> None

    let groupSwitchCases
        t
        (cases: (Fable.Expr * int * Fable.Expr list) list)
        (defaultIndex, defaultBoundValues)
        =
        cases
        |> List.groupBy (fun (_, idx, boundValues) ->
            // Try to group cases with some target index and empty bound values
            // If bound values are non-empty use also a non-empty Guid to prevent grouping
            if List.isEmpty boundValues then
                idx, System.Guid.Empty
            else
                idx, System.Guid.NewGuid()
        )
        |> List.map (fun ((idx, _), cases) ->
            let caseExprs = cases |> List.map Tuple3.item1
            // If there are multiple cases, it means boundValues are empty
            // (see `groupBy` above), so it doesn't mind which one we take as reference
            let boundValues = cases |> List.head |> Tuple3.item3
            caseExprs, Fable.DecisionTreeSuccess(idx, boundValues, t)
        )
        |> function
            | [] -> []
            // Check if the last case can also be grouped with the default branch, see #2357
            | cases when List.isEmpty defaultBoundValues ->
                match List.splitLast cases with
                | cases, (_, Fable.DecisionTreeSuccess(idx, [], _)) when
                    idx = defaultIndex
                    ->
                    cases
                | _ -> cases
            | cases -> cases

    let getTargetsWithMultipleReferences expr =
        let rec findSuccess (targetRefs: Map<int, int>) =
            function
            | [] -> targetRefs
            | expr :: exprs ->
                match expr with
                // We shouldn't actually see this, but short-circuit just in case
                | Fable.DecisionTree _ -> findSuccess targetRefs exprs
                | Fable.DecisionTreeSuccess(idx, _, _) ->
                    let count =
                        Map.tryFind idx targetRefs |> Option.defaultValue 0

                    let targetRefs = Map.add idx (count + 1) targetRefs
                    findSuccess targetRefs exprs
                | expr ->
                    let exprs2 = getSubExpressions expr
                    findSuccess targetRefs (exprs @ exprs2)

        findSuccess Map.empty [ expr ]
        |> Seq.choose (fun kv ->
            if kv.Value > 1 then
                Some kv.Key
            else
                None
        )
        |> Seq.toList

    /// When several branches share target create first a switch to get the target index and bind value
    /// and another to execute the actual target
    let transformDecisionTreeWithTwoSwitches
        (com: IDartCompiler)
        ctx
        returnStrategy
        (targets: (Fable.Ident list * Fable.Expr) list)
        treeExpr
        =
        // Declare target and bound idents
        let targetId =
            getUniqueNameInDeclarationScope ctx "matchResult"
            |> makeTypedIdent (numType Int32)

        let varDecls =
            [
                transformIdent com ctx targetId
                yield!
                    targets
                    |> List.collect (fun (idents, _) ->
                        idents |> List.map (transformIdent com ctx)
                    )
            ]
            |> List.map (fun i ->
                Statement.variableDeclaration (
                    i,
                    Final,
                    addToScope = ctx.AddToScope
                )
            )
        // Transform targets as switch
        let switch2 =
            let cases =
                targets
                |> List.mapi (fun i (_, target) -> [ makeIntConst i ], target)

            transformSwitch
                com
                ctx
                returnStrategy
                (targetId |> Fable.IdentExpr)
                cases
                None
        // Transform decision tree
        let targetAssign = Target(transformIdent com ctx targetId)
        let ctx = { ctx with DecisionTargets = targets }

        match canTransformDecisionTreeAsSwitch treeExpr with
        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
            let cases =
                groupSwitchCases
                    (numType Int32)
                    cases
                    (defaultIndex, defaultBoundValues)

            let defaultCase =
                Fable.DecisionTreeSuccess(
                    defaultIndex,
                    defaultBoundValues,
                    numType Int32
                )

            let switch1 =
                transformSwitch
                    com
                    ctx
                    targetAssign
                    evalExpr
                    cases
                    (Some defaultCase)

            varDecls @ switch1 @ switch2
        | None ->
            let decisionTree, _ = com.Transform(ctx, targetAssign, treeExpr)
            varDecls @ decisionTree @ switch2

    let simplifyDecisionTree (treeExpr: Fable.Expr) =
        treeExpr
        |> visitFromInsideOut (
            function
            | Fable.IfThenElse(guardExpr1,
                               Fable.IfThenElse(guardExpr2,
                                                thenExpr,
                                                Fable.DecisionTreeSuccess(index2,
                                                                          [],
                                                                          _),
                                                _),
                               Fable.DecisionTreeSuccess(index1, [], t),
                               r) when index1 = index2 ->
                Fable.IfThenElse(
                    makeLogOp None guardExpr1 guardExpr2 LogicalAnd,
                    thenExpr,
                    Fable.DecisionTreeSuccess(index2, [], t),
                    r
                )
            | e -> e
        )

    let transformDecisionTree
        (com: IDartCompiler)
        (ctx: Context)
        returnStrategy
        (targets: (Fable.Ident list * Fable.Expr) list)
        (treeExpr: Fable.Expr)
        =
        let t = treeExpr.Type

        let prevStatements, captureExpr, returnStrategy =
            convertCaptureStrategyIntoAssign com ctx t returnStrategy None

        let resolve statements =
            prevStatements @ statements, captureExpr

        let treeExpr = simplifyDecisionTree treeExpr

        // If some targets are referenced multiple times, hoist bound idents,
        // resolve the decision index and compile the targets as a switch
        match getTargetsWithMultipleReferences treeExpr with
        | [] ->
            let ctx = { ctx with DecisionTargets = targets }

            match canTransformDecisionTreeAsSwitch treeExpr with
            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                let cases =
                    cases
                    |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                        [ caseExpr ],
                        Fable.DecisionTreeSuccess(targetIndex, boundValues, t)
                    )

                let defaultCase =
                    Fable.DecisionTreeSuccess(
                        defaultIndex,
                        defaultBoundValues,
                        t
                    )

                transformSwitch
                    com
                    ctx
                    returnStrategy
                    evalExpr
                    cases
                    (Some defaultCase)
                |> resolve
            | None ->
                let statements, _ = com.Transform(ctx, returnStrategy, treeExpr)

                match captureExpr, statements with
                | Some(IdentExpression ident1),
                  Patterns.ListLast(statements,
                                    ExpressionStatement(AssignmentExpression(IdentExpression ident2,
                                                                             AssignEqual,
                                                                             value))) when
                    ident1.Name = ident2.Name
                    ->
                    statements, Some value
                | _ -> prevStatements @ statements, captureExpr
        | targetsWithMultiRefs ->
            // If the bound idents are not referenced in the target, remove them
            let targets =
                targets
                |> List.map (fun (idents, expr) ->
                    idents
                    |> List.exists (fun i -> isIdentUsed i.Name expr)
                    |> function
                        | true -> idents, expr
                        | false -> [], expr
                )

            let hasAnyTargetWithMultiRefsBoundValues =
                targetsWithMultiRefs
                |> List.exists (fun idx ->
                    targets[idx] |> fst |> List.isEmpty |> not
                )

            if not hasAnyTargetWithMultiRefsBoundValues then
                match canTransformDecisionTreeAsSwitch treeExpr with
                | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                    let cases =
                        groupSwitchCases
                            t
                            cases
                            (defaultIndex, defaultBoundValues)

                    let ctx = { ctx with DecisionTargets = targets }

                    let defaultCase =
                        Fable.DecisionTreeSuccess(
                            defaultIndex,
                            defaultBoundValues,
                            t
                        )

                    transformSwitch
                        com
                        ctx
                        returnStrategy
                        evalExpr
                        cases
                        (Some defaultCase)
                    |> resolve
                | None ->
                    transformDecisionTreeWithTwoSwitches
                        com
                        ctx
                        returnStrategy
                        targets
                        treeExpr
                    |> resolve
            else
                transformDecisionTreeWithTwoSwitches
                    com
                    ctx
                    returnStrategy
                    targets
                    treeExpr
                |> resolve

    let transformTest
        (com: IDartCompiler)
        ctx
        _range
        returnStrategy
        kind
        fableExpr
        =
        transformExprAndResolve
            com
            ctx
            returnStrategy
            fableExpr
            (fun expr ->
                match kind with
                | Fable.TypeTest t ->
                    Expression.isExpression (expr, transformType com ctx t)
                | Fable.OptionTest isSome ->
                    let t =
                        match expr.Type with
                        | Nullable t -> t
                        | t -> t

                    let op =
                        if isSome then
                            BinaryUnequal
                        else
                            BinaryEqual

                    Expression.binaryExpression (
                        op,
                        expr,
                        Expression.nullLiteral t,
                        Boolean
                    )
                | Fable.ListTest nonEmpty ->
                    let expr =
                        libCall com ctx Fable.Boolean "List" "isEmpty" [ expr ]

                    if nonEmpty then
                        Expression.unaryExpression (UnaryNot, expr)
                    else
                        expr
                | Fable.UnionCaseTest tag ->
                    let expected = Expression.integerLiteral tag

                    let expected =
                        match fableExpr.Type with
                        | Fable.DeclaredType(entityRef, _genericArgs) ->
                            let ent = com.GetEntity(entityRef)

                            match List.tryItem tag ent.UnionCases with
                            | Some c ->
                                let caseName = getUnionCaseName c
                                Expression.commented caseName expected
                            | None -> expected
                        | _ -> expected

                    let actual = getUnionExprTag expr

                    Expression.binaryExpression (
                        BinaryEqual,
                        actual,
                        expected,
                        Boolean
                    )
            )

    let extractBaseArgs
        (com: IDartCompiler)
        (ctx: Context)
        (classDecl: Fable.ClassDecl)
        =
        match classDecl.BaseCall with
        | Some(Fable.Call(_baseRef, info, _, _) as e) ->
            match transformCallArgs com ctx (CallInfo info) with
            | [], args -> args
            | _, args ->
                $"Rewrite base arguments for {classDecl.Entity.FullName} so they can be compiled as Dart expressions"
                |> addWarning com [] e.Range

                args
        | Some(Fable.Value _ as e) ->
            $"Ignoring base call for {classDecl.Entity.FullName}"
            |> addWarning com [] e.Range

            []
        | Some e ->
            $"Unexpected base call for {classDecl.Entity.FullName}"
            |> addError com [] e.Range

            []
        | None -> []

    let transformAndCaptureExpr
        (com: IDartCompiler)
        (ctx: Context)
        (expr: Fable.Expr)
        : Statement list * Expression
        =
        match com.Transform(ctx, Capture(binding = None), expr) with
        | statements, Some expr -> statements, expr
        | statements, None ->
            statements, libCall com ctx Fable.Unit "Util" "ignore" []

    let transform
        (com: IDartCompiler)
        ctx
        (returnStrategy: ReturnStrategy)
        (expr: Fable.Expr)
        : Statement list * CapturedExpr
        =
        match expr with
        | Fable.Unresolved(_, _, r) ->
            addError com [] r "Unexpected unresolved expression"
            [], None

        | Fable.ObjectExpr(_, t, _) ->
            match returnStrategy with
            // Constructors usually have a useless object expression on top
            // (apparently it represents the call to the base Object type)
            | Ignore
            | Return(isVoid = true) -> [], None
            | _ ->
                let fullName =
                    match t with
                    | Fable.DeclaredType(e, _) -> e.FullName
                    | t -> $"%A{t}"

                $"TODO: Object expression is not supported yet: %s{fullName}"
                |> addWarning com [] expr.Range

                [], None

        | Fable.Extended(kind, _range) ->
            match kind with
            | Fable.Curry(e, arity) ->
                Replacements.Api.curryExprAtRuntime com arity e
                |> transform com ctx returnStrategy
            | Fable.Throw(None, t) ->
                [
                    Expression.rethrowExpression (transformType com ctx t)
                    |> Statement.ExpressionStatement
                ],
                None
            | Fable.Throw(Some expr, t) ->
                transformExprAndResolve
                    com
                    ctx
                    returnStrategy
                    expr
                    (fun expr ->
                        Expression.throwExpression (
                            expr,
                            transformType com ctx t
                        )
                    )
            | Fable.Debugger ->
                [
                    extLibCall com ctx Fable.Unit "dart:developer" "debugger" []
                    |> Statement.ExpressionStatement
                ],
                None

        | Fable.TypeCast(expr, t) ->
            match t with
            | Fable.DeclaredType(EntRefFullName(Types.ienumerableGeneric | Types.ienumerable),
                                 [ _ ]) ->
                match expr with
                // Optimization for (numeric) array or list literals casted to seq
                // Done at the very end of the compile pipeline to get more opportunities
                // of matching cast and literal expressions after resolving pipes, inlining...
                | Replacements.Util.ArrayOrListLiteral(exprs, typ) ->
                    transformExprsAndResolve
                        com
                        ctx
                        returnStrategy
                        exprs
                        (makeImmutableListExpr com ctx typ)

                | ExprType(Fable.Array _ | Fable.List _) ->
                    transform com ctx returnStrategy expr

                | ExprType(Fable.DeclaredType(EntRefFullName(Types.dictionary | Types.idictionary),
                                              _)) ->
                    transformExprAndResolve
                        com
                        ctx
                        returnStrategy
                        expr
                        (fun expr ->
                            let t = transformType com ctx t
                            get t expr "entries"
                        )

                | ExprType(Fable.String) ->
                    Dart.Replacements.stringToCharSeq expr
                    |> transform com ctx returnStrategy

                | ExprType(Fable.DeclaredType(EntRefFullName Types.regexMatch, _)) ->
                    Dart.Replacements.regexMatchToSeq com t expr
                    |> transform com ctx returnStrategy

                | _ -> transformCast com ctx t returnStrategy expr
            | _ -> transformCast com ctx t returnStrategy expr

        | Fable.Value(kind, r) -> transformValue com ctx r returnStrategy kind

        | Fable.IdentExpr id ->
            transformIdentAsExpr com ctx id |> resolveExpr returnStrategy

        | Fable.Import({
                           Selector = selector
                           Path = path
                       },
                       t,
                       r) ->
            transformImport com ctx r t selector path
            |> resolveExpr returnStrategy

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range returnStrategy kind expr

        | Fable.Lambda(arg, body, name) ->
            let genParams =
                getLocalFunctionGenericParams com ctx expr.Range [ arg.Type ]

            let ctx = ctx.AppendLocalGenParams(genParams)
            let args, body, t = transformFunction com ctx name [ arg ] body

            Expression.anonymousFunction (args, body, t, genParams)
            |> resolveExpr returnStrategy

        | Fable.Delegate(args, body, name, _) ->
            let genParams =
                args
                |> List.map (fun a -> a.Type)
                |> getLocalFunctionGenericParams com ctx expr.Range

            let ctx = ctx.AppendLocalGenParams(genParams)
            let args, body, t = transformFunction com ctx name args body

            Expression.anonymousFunction (args, body, t, genParams)
            |> resolveExpr returnStrategy

        | Fable.Call(callee, info, typ, range) ->
            transformCall com ctx range typ returnStrategy callee info

        | Fable.CurriedApply(callee, args, typ, range) ->
            transformCurriedApplyAsStatements
                com
                ctx
                range
                typ
                returnStrategy
                callee
                args

        | Fable.Emit(info, t, _range) ->
            transformEmit com ctx t returnStrategy info

        | Fable.Operation(kind, _, t, r) ->
            transformOperation com ctx r t returnStrategy kind

        | Fable.Get(expr, kind, t, range) ->
            transformGet com ctx range t returnStrategy kind expr

        | Fable.Set(expr, kind, _typ, value, range) ->
            transformSet com ctx range kind expr value, None

        | Fable.Let(ident, value, body) ->
            let ctx, binding = transformBinding com ctx ident value
            let body, captureExpr = transform com ctx returnStrategy body
            binding @ body, captureExpr

        | Fable.LetRec(bindings, body) ->
            let ctx, bindings =
                ((ctx, []), bindings)
                ||> List.fold (fun (ctx, bindings) (i, v) ->
                    let ctx, newBindings = transformBinding com ctx i v
                    ctx, bindings @ newBindings
                )

            let body, captureExpr = transform com ctx returnStrategy body
            bindings @ body, captureExpr

        | Fable.Sequential exprs ->
            let exprs, lastExpr = List.splitLast exprs

            let statements1 =
                exprs |> List.collect (transform com ctx Ignore >> fst)

            let statements2, expr = transform com ctx returnStrategy lastExpr
            statements1 @ statements2, expr

        | Fable.TryCatch(body, catch, finalizer, r) ->
            transformTryCatch com ctx r returnStrategy (body, catch, finalizer)

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, _r) ->
            transformConditional
                com
                ctx
                returnStrategy
                guardExpr
                thenExpr
                elseExpr

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTree com ctx returnStrategy targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccess com ctx returnStrategy idx boundValues

        | Fable.WhileLoop(guard, body, _range) ->
            let guardStatements, guard = transformAndCaptureExpr com ctx guard
            let body, _ = transform com ctx Ignore body

            match guardStatements with
            | [] -> [ Statement.whileStatement (guard, body) ], None
            // guard statements must be inside the loop so they're re-evaluated on each iteration
            | guardStatements ->
                [
                    Statement.whileStatement (
                        Expression.booleanLiteral (true),
                        [
                            yield! guardStatements
                            yield
                                Statement.ifStatement (
                                    guard,
                                    body,
                                    [ Statement.breakStatement () ]
                                )
                        ]
                    )
                ],
                None

        | Fable.ForLoop(var, start, limit, body, isUp, _range) ->
            let statements, startAndLimit =
                combineStatementsAndExprs
                    com
                    ctx
                    [
                        transformAndCaptureExpr com ctx start
                        transformAndCaptureExpr com ctx limit
                    ]

            let body, _ = transform com ctx Ignore body
            let param = transformIdent com ctx var
            let paramExpr = Expression.identExpression param

            let op1, op2 =
                if isUp then
                    BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                else
                    BinaryOperator.BinaryGreaterOrEqual,
                    UpdateOperator.UpdateMinus

            statements
            @ [
                Statement.forStatement (
                    body,
                    (param, startAndLimit[0]),
                    Expression.binaryExpression (
                        op1,
                        paramExpr,
                        startAndLimit[1],
                        Boolean
                    ),
                    Expression.updateExpression (op2, paramExpr)
                )
            ],
            None

    let getLocalFunctionGenericParams
        (_com: IDartCompiler)
        (ctx: Context)
        (_range: SourceLocation option)
        (argTypes: Fable.Type list)
        : string list
        =
        let rec getGenParams =
            function
            | Fable.GenericParam(name, isMeasure, _constraints) ->
                if isMeasure then
                    []
                else
                    [ name ] // discard measure generic params
            | t -> t.Generics |> List.collect getGenParams

        let genParams =
            (Set.empty, argTypes)
            ||> List.fold (fun genArgs t ->
                (genArgs, getGenParams t)
                ||> List.fold (fun genArgs n -> Set.add n genArgs)
            )
            |> List.ofSeq

        let genParams =
            match genParams, ctx.EntityAndMemberGenericParams with
            | [], _
            | _, [] -> genParams
            | localGenParams, memberGenParams ->
                let memberGenParams =
                    memberGenParams |> List.map (fun p -> p.Name) |> set

                localGenParams |> List.filter (memberGenParams.Contains >> not)

        // Sometimes nested generic functions cause issues in Dart, but I'm not sure of
        // the exact conditions so don't display the warning for now
        //        match range, genParams with
        //        | None, _ | Some _, [] -> ()
        //        | Some range, _ -> com.WarnOnlyOnce("Generic nested functions may cause issues with Dart compiler. Add type annotations or move the function to module scope.", range=range)

        genParams

    let getMemberArgsAndBody
        (com: IDartCompiler)
        ctx
        kind
        (genParams: Fable.GenericParam list)
        (paramGroups: Fable.Parameter list list)
        (args: Fable.Ident list)
        (body: Fable.Expr)
        =
        let funcName, args, body =
            match kind, args with
            | Attached(isStatic = false), (thisArg :: args) ->
                // AFAIK, there cannot be `this` conflicts in Dart (no class expressions)
                // so we can just replace the thisArg.Ident
                let thisExpr = Fable.ThisValue thisArg.Type |> makeValue None
                let replacements = Map [ thisArg.Name, thisExpr ]
                let body = FableTransforms.replaceValues replacements body
                None, args, body
            | Attached(isStatic = true), _
            | ClassConstructor, _ -> None, args, body
            | NonAttached funcName, _ -> Some funcName, args, body
            | _ -> None, args, body

        let ctx = { ctx with EntityAndMemberGenericParams = genParams }

        let args, body, returnType =
            transformFunction com ctx funcName args body

        let args =
            let parameters = List.concat paramGroups

            if List.sameLength args parameters then
                List.zip args parameters
                |> List.map (fun (a, p) ->
                    let defVal =
                        p.DefaultValue
                        |> Option.map (transformAndCaptureExpr com ctx >> snd)

                    FunctionArg(
                        a,
                        isOptional = p.IsOptional,
                        isNamed = p.IsNamed,
                        ?defaultValue = defVal
                    )
                )
            else
                args |> List.map FunctionArg

        args, body, returnType

    let getEntityAndMemberArgs
        (com: IDartCompiler)
        (info: Fable.MemberFunctionOrValue)
        =
        match info.DeclaringEntity with
        | Some e ->
            let e = com.GetEntity(e)

            if not e.IsFSharpModule then
                e.GenericParameters @ info.GenericParameters
            else
                info.GenericParameters
        | None -> info.GenericParameters

    let transformModuleFunction
        (com: IDartCompiler)
        ctx
        (info: Fable.MemberFunctionOrValue)
        (memb: Fable.MemberDecl)
        =
        let entAndMembGenParams = getEntityAndMemberArgs com info

        let args, body, returnType =
            getMemberArgsAndBody
                com
                ctx
                (NonAttached memb.Name)
                entAndMembGenParams
                info.CurriedParameterGroups
                memb.Args
                memb.Body

        let isEntryPoint =
            info.Attributes
            |> Seq.exists (fun att -> att.Entity.FullName = Atts.entryPoint)

        let name =
            if isEntryPoint then
                "main"
            else
                memb.Name

        let genParams =
            entAndMembGenParams |> List.choose (transformGenericParam com ctx)

        Declaration.functionDeclaration (
            name,
            args,
            body,
            returnType,
            genParams = genParams
        )

    let transformAbstractMember
        (com: IDartCompiler)
        ctx
        (m: Fable.MemberFunctionOrValue)
        =
        let kind =
            if m.IsGetter then
                IsGetter
            elif m.IsSetter then
                IsSetter
            else
                IsMethod

        let name = m.DisplayName

        let args =
            m.CurriedParameterGroups
            |> List.concat
            |> List.mapi (fun i p ->
                let name =
                    match p.Name with
                    | Some name -> name
                    | None -> $"arg{i}$"

                let t = transformType com ctx p.Type
                FunctionArg(makeImmutableIdent t name) // TODO, isOptional=p.IsOptional, isNamed=p.IsNamed)
            )

        let genParams =
            m.GenericParameters |> List.choose (transformGenericParam com ctx)

        InstanceMethod(
            name,
            kind = kind,
            args = args,
            genParams = genParams,
            returnType = transformType com ctx m.ReturnParameter.Type
        )

    let transformImplementedInterfaces com ctx (classEnt: Fable.Entity) =
        let mutable implementsEnumerable = None
        let mutable implementsDisposable = false
        let mutable implementsEnumerator = false
        let mutable implementsStructuralEquatable = false
        let mutable implementsStructuralComparable = false

        let implementedInterfaces =
            classEnt.DeclaredInterfaces
            |> Seq.choose (fun ifc ->
                match ifc.Entity.FullName with
                | Types.ienumerable
                | Types.icomparable
                | Types.iequatableGeneric -> None
                | Types.iStructuralComparable ->
                    implementsStructuralComparable <- true
                    None
                | Types.iStructuralEquatable ->
                    implementsStructuralEquatable <- true
                    None
                | Types.idisposable ->
                    implementsDisposable <- true

                    transformDeclaredType com ctx ifc.Entity ifc.GenericArgs
                    |> Some
                | Types.ienumeratorGeneric ->
                    implementsEnumerator <- true

                    transformDeclaredType com ctx ifc.Entity ifc.GenericArgs
                    |> Some
                | Types.ienumerableGeneric ->
                    implementsEnumerable <-
                        transformDeclaredType
                            com
                            ctx
                            ifc.Entity
                            ifc.GenericArgs
                        |> Some

                    None
                | _ ->
                    transformDeclaredType com ctx ifc.Entity ifc.GenericArgs
                    |> Some
            )
            |> Seq.toList

        let info =
            {|
                implementsEnumerable = implementsEnumerable
                implementsStructuralEquatable = implementsStructuralEquatable
                implementsStructuralComparable = implementsStructuralComparable
            |}

        if implementsEnumerator && not implementsDisposable then
            let disp =
                Type.reference (
                    libValue com ctx Fable.MetaType "Types" "IDisposable"
                )

            info, disp :: implementedInterfaces
        else
            info, implementedInterfaces

    let transformInheritedClass
        com
        ctx
        (classEnt: Fable.Entity)
        implementsIterable
        (baseType: Fable.DeclaredType option)
        =
        match implementsIterable, baseType with
        | Some iterable, Some _ ->
            $"Types implementing IEnumerable cannot inherit from another class: {classEnt.FullName}"
            |> addError com [] None

            Some iterable
        | Some iterable, None -> Some iterable
        | None, Some e ->
            Fable.DeclaredType(e.Entity, e.GenericArgs)
            |> transformType com ctx
            |> Some
        | None, None -> None

    // TODO: Inheriting interfaces
    let transformInterfaceDeclaration
        (com: IDartCompiler)
        ctx
        (decl: Fable.ClassDecl)
        (ent: Fable.Entity)
        =
        let genParams =
            ent.GenericParameters |> List.choose (transformGenericParam com ctx)

        let methods =
            ent.MembersFunctionsAndValues
            |> Seq.filter (fun memb -> not memb.IsProperty)
            |> Seq.mapToList (transformAbstractMember com ctx)

        [
            Declaration.classDeclaration (
                decl.Name,
                genParams = genParams,
                methods = methods,
                isAbstract = true
            )
        ]

    // Mirrors Dart.Replacements.equals
    let equals com ctx (left: Expression) (right: Expression) =
        let makeEqualsFunction t =
            let x = makeImmutableIdent t "x"
            let y = makeImmutableIdent t "y"

            Expression.anonymousFunction (
                [
                    x
                    y
                ],
                [ equals com ctx x.Expr y.Expr |> Statement.returnStatement ],
                Integer
            )

        match left.Type with
        | List t ->
            let fn = makeEqualsFunction t

            libCall
                com
                ctx
                Fable.Boolean
                "Util"
                "equalsList"
                [
                    left
                    right
                    fn
                ]
        | Dynamic
        | Generic _ ->
            libCall
                com
                ctx
                Fable.Boolean
                "Util"
                "equalsDynamic"
                [
                    left
                    right
                ]
        | _ -> Expression.binaryExpression (BinaryEqual, left, right, Boolean)

    // Mirrors Dart.Replacements.compare
    let compare com ctx (left: Expression) (right: Expression) =
        let makeComparerFunction t =
            let x = makeImmutableIdent t "x"
            let y = makeImmutableIdent t "y"

            Expression.anonymousFunction (
                [
                    x
                    y
                ],
                [ compare com ctx x.Expr y.Expr |> Statement.returnStatement ],
                Integer
            )

        match left.Type with
        | List t ->
            let fn = makeComparerFunction t

            libCall
                com
                ctx
                (numType Int32)
                "Util"
                "compareList"
                [
                    left
                    right
                    fn
                ]
        | Nullable t ->
            let fn = makeComparerFunction t

            libCall
                com
                ctx
                (numType Int32)
                "Util"
                "compareNullable"
                [
                    left
                    right
                    fn
                ]
        | Boolean ->
            libCall
                com
                ctx
                (numType Int32)
                "Util"
                "compareBool"
                [
                    left
                    right
                ]
        | Dynamic
        | Generic _ ->
            libCall
                com
                ctx
                (numType Int32)
                "Util"
                "compareDynamic"
                [
                    left
                    right
                ]
        | _ ->
            Expression.invocationExpression (
                left,
                "compareTo",
                [ right ],
                Integer
            )

    let makeStructuralEquals
        (com: IDartCompiler)
        (ctx: Context)
        (selfTypeRef: Type)
        (fields: Ident list)
        : InstanceMethod
        =
        let other = makeImmutableIdent Object "other"

        let makeFieldEq (field: Ident) =
            let otherField =
                Expression.propertyAccess (other.Expr, field.Name, field.Type)

            equals com ctx otherField field.Expr

        let rec makeFieldsEq fields acc =
            match fields with
            | [] -> acc
            | field :: fields ->
                let eq = makeFieldEq field

                Expression.logicalExpression (LogicalAnd, eq, acc)
                |> makeFieldsEq fields

        let typeTest = Expression.isExpression (other.Expr, selfTypeRef)

        let body =
            match List.rev fields with
            | [] -> typeTest
            | field :: fields ->
                let eq = makeFieldEq field |> makeFieldsEq fields
                Expression.logicalExpression (LogicalAnd, typeTest, eq)
            |> makeReturnBlock

        InstanceMethod(
            "==",
            [ FunctionArg other ],
            Boolean,
            body = body,
            kind = MethodKind.IsOperator,
            isOverride = true
        )

    let makeStructuralHashCode com ctx fields =
        let intType = Fable.Number(Int32, Fable.NumberInfo.Empty)

        let body =
            match fields with
            | [ field ] ->
                Expression.propertyAccess (
                    Expression.identExpression field,
                    "hashCode",
                    Integer
                )
                |> Statement.returnStatement
                |> List.singleton
            | fields ->
                fields
                |> List.map (fun f ->
                    Expression.propertyAccess (
                        Expression.identExpression f,
                        "hashCode",
                        Integer
                    )
                )
                |> makeImmutableListExpr com ctx intType
                |> List.singleton
                |> libCall com ctx (numType Int32) "Util" "combineHashCodes"
                |> makeReturnBlock

        InstanceMethod(
            "hashCode",
            [],
            Integer,
            kind = IsGetter,
            body = body,
            isOverride = true
        )

    let makeFieldCompare com ctx (other: Expression) (field: Ident) =
        let otherField =
            Expression.propertyAccess (other, field.Name, field.Type)

        compare com ctx field.Expr otherField

    let makeStructuralCompareTo com ctx wrapper selfTypeRef fields =
        let r = makeImmutableIdent Integer "$r"
        let other = makeImmutableIdent selfTypeRef "other"

        let makeAssign (field: Ident) =
            Expression.assignmentExpression (
                r.Expr,
                makeFieldCompare com ctx other.Expr field
            )

        let makeFieldCompareWithAssign (field: Ident) =
            Expression.binaryExpression (
                BinaryEqual,
                makeAssign field,
                Expression.integerLiteral 0,
                Boolean
            )

        let rec makeFieldsComp (fields: Ident list) (acc: Statement list) =
            match fields with
            | [] -> acc
            | field :: fields ->
                let eq = makeFieldCompareWithAssign field
                [ Statement.ifStatement (eq, acc) ] |> makeFieldsComp fields

        let body =
            match fields with
            | [ field ] ->
                makeFieldCompare com ctx other.Expr field
                |> Statement.returnStatement
                |> List.singleton
            | fields ->
                [
                    Statement.variableDeclaration (
                        r,
                        kind = Var,
                        addToScope = ignore
                    )
                    yield!
                        match List.rev fields with
                        | [] -> []
                        | field :: fields ->
                            [ makeAssign field |> ExpressionStatement ]
                            |> makeFieldsComp fields
                    Statement.returnStatement r.Expr
                ]

        let body =
            match wrapper with
            | None -> body
            | Some wrapper -> wrapper other.Expr body

        InstanceMethod(
            "compareTo",
            [ FunctionArg other ],
            Integer,
            body = body,
            isOverride = true
        )

    let transformFields (com: IDartCompiler) ctx (fields: Fable.Field list) =
        fields
        |> List.map (fun f ->
            let kind =
                if f.IsMutable then
                    Var
                else
                    Final

            let typ = FableTransforms.uncurryType f.FieldType

            let ident =
                sanitizeMember f.Name
                |> transformIdentWith com ctx f.IsMutable typ

            ident, InstanceVariable(ident, kind = kind)
        )
        |> List.unzip

    let transformUnionDeclaration
        (com: IDartCompiler)
        ctx
        (ent: Fable.Entity)
        (unionDecl: Fable.ClassDecl)
        classMethods
        =
        let genParams =
            ent.GenericParameters |> List.choose (transformGenericParam com ctx)

        let unionTypeRef =
            genParams
            |> List.map (fun g -> Generic g.Name)
            |> makeTypeRefFromName unionDecl.Name

        let interfaceInfo, interfaces =
            transformImplementedInterfaces com ctx ent

        let tagIdent = makeImmutableIdent Integer "tag"

        let extraDecls =
            let mutable tag = -1

            ent.UnionCases
            |> List.choose (fun uci ->
                tag <- tag + 1

                if List.isEmpty uci.UnionCaseFields then
                    None
                else
                    let caseDeclName =
                        getUnionCaseDeclarationName unionDecl.Name uci

                    let caseTypeRef =
                        genParams
                        |> List.map (fun g -> Generic g.Name)
                        |> makeTypeRefFromName caseDeclName

                    let fields, varDecls =
                        transformFields com ctx uci.UnionCaseFields

                    let wrapCompare otherExpr body =
                        [
                            Statement.ifStatement (
                                Expression.isExpression (
                                    otherExpr,
                                    caseTypeRef
                                ),
                                body,
                                [
                                    Statement.returnStatement (
                                        makeFieldCompare
                                            com
                                            ctx
                                            otherExpr
                                            tagIdent
                                    )
                                ]
                            )
                        ]

                    let methods =
                        [
                            if interfaceInfo.implementsStructuralEquatable then
                                makeStructuralEquals com ctx caseTypeRef fields

                                makeStructuralHashCode
                                    com
                                    ctx
                                    (tagIdent :: fields)
                            if
                                interfaceInfo.implementsStructuralComparable
                            then
                                makeStructuralCompareTo
                                    com
                                    ctx
                                    (Some wrapCompare)
                                    unionTypeRef
                                    fields
                        ]

                    let tag = Expression.integerLiteral (tag)

                    let consArgs =
                        fields
                        |> List.map (fun f ->
                            FunctionArg(f, isConsThisArg = true)
                        )

                    let constructor =
                        Constructor(
                            args = consArgs,
                            superArgs = unnamedArgs [ tag ],
                            isConst = true
                        )

                    Declaration.classDeclaration (
                        caseDeclName,
                        genParams = genParams,
                        constructor = constructor,
                        extends = unionTypeRef,
                        variables = varDecls,
                        methods = methods
                    )
                    |> Some
            )

        let hasCasesWithoutFields =
            ent.UnionCases
            |> List.exists (fun uci -> List.isEmpty uci.UnionCaseFields)

        let extends =
            transformInheritedClass
                com
                ctx
                ent
                interfaceInfo.implementsEnumerable
                None

        let implements =
            [
                libTypeRef com ctx "Types" "Union" []
                yield! interfaces
            ]

        let extraMethods =
            if not hasCasesWithoutFields then
                []
            else
                [
                    if interfaceInfo.implementsStructuralEquatable then
                        makeStructuralEquals com ctx unionTypeRef [ tagIdent ]
                        makeStructuralHashCode com ctx [ tagIdent ]
                    if interfaceInfo.implementsStructuralComparable then
                        makeStructuralCompareTo
                            com
                            ctx
                            None
                            unionTypeRef
                            [ tagIdent ]
                ]

        let constructor =
            Constructor(
                args = [ FunctionArg(tagIdent, isConsThisArg = true) ],
                isConst = true
            )

        let unionDecl =
            Declaration.classDeclaration (
                unionDecl.Name,
                isAbstract = not hasCasesWithoutFields,
                genParams = genParams,
                constructor = constructor,
                implements = implements,
                ?extends = extends,
                variables = [ InstanceVariable(tagIdent, kind = Final) ],
                methods = extraMethods @ classMethods
            )

        unionDecl :: extraDecls

    let transformRecordDeclaration
        (com: IDartCompiler)
        ctx
        (ent: Fable.Entity)
        (decl: Fable.ClassDecl)
        classMethods
        =
        let genParams =
            ent.GenericParameters |> List.choose (transformGenericParam com ctx)

        let selfTypeRef =
            genParams
            |> List.map (fun g -> Generic g.Name)
            |> makeTypeRefFromName decl.Name

        let interfaceInfo, interfaces =
            transformImplementedInterfaces com ctx ent

        let extends =
            transformInheritedClass
                com
                ctx
                ent
                interfaceInfo.implementsEnumerable
                None

        let implements =
            [
                libTypeRef com ctx "Types" "Record" []
                yield! interfaces
            ]

        let hasMutableFields =
            ent.FSharpFields |> List.exists (fun f -> f.IsMutable)

        let fields, varDecls = transformFields com ctx ent.FSharpFields

        let consArgs =
            fields |> List.map (fun f -> FunctionArg(f, isConsThisArg = true))

        let constructor =
            Constructor(args = consArgs, isConst = not hasMutableFields)

        // TODO: toString
        let methods =
            [
                if interfaceInfo.implementsStructuralEquatable then
                    makeStructuralEquals com ctx selfTypeRef fields
                    makeStructuralHashCode com ctx fields
                if interfaceInfo.implementsStructuralComparable then
                    makeStructuralCompareTo com ctx None selfTypeRef fields
                yield! classMethods
            ]

        [
            Declaration.classDeclaration (
                decl.Name,
                genParams = genParams,
                constructor = constructor,
                implements = implements,
                ?extends = extends,
                variables = varDecls,
                methods = methods
            )
        ]

    let transformAttachedMember
        (com: IDartCompiler)
        ctx
        (memb: Fable.MemberDecl)
        =
        let info = com.GetMember(memb.MemberRef)

        let abstractInfo =
            memb.ImplementedSignatureRef |> Option.map (com.GetMember)

        let abstractFullName = abstractInfo |> Option.map (fun i -> i.FullName)

        match abstractFullName with
        | Some "System.Collections.IEnumerable.GetEnumerator"
        | Some "System.Collections.IEnumerator.get_Current"
        | Some "System.Collections.IEnumerator.Reset" -> None
        | _ ->
            let isStatic = not info.IsInstance
            let entAndMembGenParams = getEntityAndMemberArgs com info

            let args, body, returnType =
                getMemberArgsAndBody
                    com
                    ctx
                    (Attached isStatic)
                    entAndMembGenParams
                    info.CurriedParameterGroups
                    memb.Args
                    memb.Body

            let kind, name =
                match abstractFullName with
                | Some "System.Collections.Generic.IEnumerable.GetEnumerator" ->
                    MethodKind.IsGetter, "iterator"
                | Some "System.Collections.Generic.IEnumerator.get_Current" ->
                    MethodKind.IsGetter, "current"
                | Some "System.Collections.IEnumerator.MoveNext" ->
                    MethodKind.IsMethod, "moveNext"
                | Some "System.IComparable.CompareTo" ->
                    MethodKind.IsMethod, "compareTo"
                | Some "System.Object.ToString" ->
                    MethodKind.IsMethod, "toString"
                | Some "System.Object.GetHashCode" ->
                    MethodKind.IsGetter, "hashCode"
                | Some "System.Object.Equals" -> MethodKind.IsOperator, "=="
                | _ ->
                    // If method implements an abstract signature, use that info to decide if it's a getter or setter
                    let info = defaultArg abstractInfo info

                    let kind =
                        if not memb.IsMangled && info.IsGetter then
                            MethodKind.IsGetter
                        elif not memb.IsMangled && info.IsSetter then
                            MethodKind.IsSetter
                        else
                            MethodKind.IsMethod

                    kind, sanitizeMember memb.Name

            // As the method is attached, we don't need the entity gen params here
            let genParams =
                info.GenericParameters
                |> List.choose (transformGenericParam com ctx)

            InstanceMethod(
                name,
                args,
                returnType,
                body = body,
                kind = kind,
                genParams = genParams,
                isStatic = isStatic,
                isOverride = info.IsOverrideOrExplicitInterfaceImplementation
            )
            |> Some

    let transformClass
        (com: IDartCompiler)
        ctx
        (classEnt: Fable.Entity)
        (classDecl: Fable.ClassDecl)
        classMethods
        (cons: Fable.MemberDecl option)
        =
        let genParams =
            classEnt.GenericParameters
            |> List.choose (transformGenericParam com ctx)

        let constructor, variables, otherDecls =
            match cons with
            // TODO: Check if we need to generate the constructor
            | None -> None, [], []
            | Some cons ->
                let consInfo = com.GetMember(cons.MemberRef)
                let entGenParams = classEnt.GenericParameters

                let consArgs, consBody, _ =
                    getMemberArgsAndBody
                        com
                        ctx
                        ClassConstructor
                        entGenParams
                        consInfo.CurriedParameterGroups
                        cons.Args
                        cons.Body

                // Analyze the constructor body to see if we can assign fields
                // directly and prevent declaring them as late final
                let thisArgsDic = Dictionary()

                let consBody =
                    let consArgsSet =
                        consArgs |> List.map (fun a -> a.Ident.Name) |> HashSet

                    consBody
                    |> List.filter (
                        function
                        | ExpressionStatement(AssignmentExpression(PropertyAccess(ThisExpression _,
                                                                                  field,
                                                                                  _,
                                                                                  _),
                                                                   AssignEqual,
                                                                   value)) ->
                            match value with
                            | IdentExpression ident when
                                consArgsSet.Contains(ident.Name)
                                ->
                                thisArgsDic.Add(ident.Name, field)
                                false
                            // Remove null initializations as they're not necessary and maybe
                            // they represent initializing to Unchecked.defaultof<'T>
                            | Literal(NullLiteral _) -> false
                            | _ -> true
                        | _ -> true
                    )

                let consArgs =
                    if thisArgsDic.Count = 0 then
                        consArgs
                    else
                        consArgs
                        |> List.map (fun consArg ->
                            match
                                thisArgsDic.TryGetValue(consArg.Ident.Name)
                            with
                            | false, _ -> consArg
                            | true, fieldName ->
                                consArg.AsConsThisArg(fieldName)
                        )

                let variables =
                    let thisArgsSet =
                        thisArgsDic |> Seq.map (fun kv -> kv.Value) |> HashSet

                    classEnt.FSharpFields
                    |> List.map (fun f ->
                        let fieldName = sanitizeMember f.Name

                        let t =
                            FableTransforms.uncurryType f.FieldType
                            |> transformType com ctx

                        let ident = makeImmutableIdent t fieldName

                        let kind =
                            if f.IsMutable then
                                Var
                            else
                                Final

                        let isLate =
                            match t, kind with
                            | Nullable _, Var -> false
                            | _ -> thisArgsSet.Contains(fieldName) |> not

                        InstanceVariable(ident, kind = kind, isLate = isLate)
                    )

                let constructor =
                    Constructor(
                        args = consArgs,
                        body = consBody,
                        superArgs = (extractBaseArgs com ctx classDecl),
                        isConst = (hasConstAttribute consInfo.Attributes)
                    )

                // let classIdent = makeImmutableIdent MetaType classDecl.Name
                // let classType = TypeReference(classIdent, genParams |> List.map (fun g -> Generic g.Name))
                // let exposedCons =
                //     let argExprs = consArgs |> List.map (fun a -> Expression.identExpression a.Ident)
                //     let exposedConsBody = Expression.invocationExpression(classIdent.Expr, argExprs, classType) |> makeReturnBlock
                //     Declaration.functionDeclaration(cons.Name, consArgs, exposedConsBody, classType, genParams=genParams)

                Some constructor, variables, [] // [exposedCons]

        let interfaceInfo, implements =
            transformImplementedInterfaces com ctx classEnt

        let extends =
            transformInheritedClass
                com
                ctx
                classEnt
                interfaceInfo.implementsEnumerable
                classEnt.BaseType

        let abstractMembers =
            classEnt.MembersFunctionsAndValues
            |> Seq.choose (fun m ->
                if m.IsDispatchSlot then
                    transformAbstractMember com ctx m |> Some
                else
                    None
            )
            |> Seq.toList

        let classDecl =
            Declaration.classDeclaration (
                classDecl.Name,
                isAbstract = classEnt.IsAbstractClass,
                genParams = genParams,
                ?extends = extends,
                implements = implements,
                ?constructor = constructor,
                methods = classMethods @ abstractMembers,
                variables = variables
            )

        classDecl :: otherDecls

    let transformDeclaration (com: IDartCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx =
                { ctx with
                    UsedNames =
                        { ctx.UsedNames with
                            CurrentDeclarationScope = HashSet usedNames
                        }
                    CastedUnions = Dictionary()
                }

            let result = f ctx

            ctx.UsedNames.DeclarationScopes.UnionWith(
                ctx.UsedNames.CurrentDeclarationScope
            )

            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members |> List.collect (transformDeclaration com ctx)

        | Fable.ActionDeclaration d ->
            "Standalone actions are not supported in Dart, please use a function"
            |> addError com [] d.Body.Range

            []

        // TODO: Prefix non-public values with underscore or raise warning?
        | Fable.MemberDeclaration memb ->
            withCurrentScope ctx memb.UsedNames
            <| fun ctx ->
                let info = com.GetMember(memb.MemberRef)

                if info.IsValue then
                    let ident =
                        transformIdentWith
                            com
                            ctx
                            info.IsMutable
                            memb.Body.Type
                            memb.Name

                    let value =
                        transformAndCaptureExpr com ctx memb.Body ||> iife

                    let kind, value = getVarKind ctx info.IsMutable value
                    [ Declaration.variableDeclaration (ident, kind, value) ]
                else
                    [ transformModuleFunction com ctx info memb ]

        | Fable.ClassDeclaration decl ->
            let entRef = decl.Entity
            let ent = com.GetEntity(entRef)

            if ent.IsInterface then
                transformInterfaceDeclaration com ctx decl ent
            else
                let instanceMethods =
                    decl.AttachedMembers
                    |> List.choose (fun memb ->
                        withCurrentScope
                            ctx
                            memb.UsedNames
                            (fun ctx -> transformAttachedMember com ctx memb)
                    )

                match decl.Constructor with
                | Some cons ->
                    withCurrentScope ctx cons.UsedNames
                    <| fun ctx ->
                        transformClass
                            com
                            ctx
                            ent
                            decl
                            instanceMethods
                            (Some cons)
                | None ->
                    if ent.IsFSharpUnion then
                        transformUnionDeclaration
                            com
                            ctx
                            ent
                            decl
                            instanceMethods
                    elif ent.IsFSharpRecord then
                        transformRecordDeclaration
                            com
                            ctx
                            ent
                            decl
                            instanceMethods
                    else
                        transformClass com ctx ent decl instanceMethods None

    let getIdentNameForImport (ctx: Context) (path: string) =
        Path
            .GetFileNameWithoutExtension(path)
            .Replace(".", "_")
            .Replace(":", "_")
        |> Naming.applyCaseRule Core.CaseRules.SnakeCase
        |> getUniqueNameInRootScope ctx

module Compiler =
    open Util

    type DartCompiler(com: Compiler) =
        let onlyOnceErrors = HashSet<string>()
        let imports = Dictionary<string, Import>()

        interface IDartCompiler with
            member _.WarnOnlyOnce(msg, ?values, ?range) =
                if onlyOnceErrors.Add(msg) then
                    let msg =
                        match values with
                        | None -> msg
                        | Some values -> System.String.Format(msg, values)

                    addWarning com [] range msg

            member _.ErrorOnlyOnce(msg, ?values, ?range) =
                if onlyOnceErrors.Add(msg) then
                    let msg =
                        match values with
                        | None -> msg
                        | Some values -> System.String.Format(msg, values)

                    addError com [] range msg

            member com.GetImportIdent(ctx, selector, path, t, r) =
                let localId =
                    match imports.TryGetValue(path) with
                    | true, i ->
                        match i.LocalIdent with
                        | Some localId -> localId
                        | None ->
                            let localId = getIdentNameForImport ctx path

                            imports[path] <-
                                {
                                    Path = path
                                    LocalIdent = Some localId
                                }

                            localId
                    | false, _ ->
                        let localId = getIdentNameForImport ctx path

                        imports.Add(
                            path,
                            {
                                Path = path
                                LocalIdent = Some localId
                            }
                        )

                        localId

                let t = transformType com ctx t
                let ident = makeImmutableIdent t localId

                match selector with
                | Naming.placeholder ->
                    "`importMember` must be assigned to a variable"
                    |> addError com [] r

                    ident
                | "*" -> ident
                | selector ->
                    { ident with
                        ImportModule = Some ident.Name
                        Name = selector
                    }

            member _.GetAllImports() = imports.Values |> Seq.toList
            member this.TransformType(ctx, t) = transformType this ctx t
            member this.Transform(ctx, ret, e) = transform this ctx ret e

            member this.TransformFunction(ctx, name, args, body) =
                transformFunction this ctx name args body

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.SourceFiles = com.SourceFiles
            member _.IncrementCounter() = com.IncrementCounter()

            member _.IsPrecompilingInlineFunction =
                com.IsPrecompilingInlineFunction

            member _.WillPrecompileInlineFunction(file) =
                com.WillPrecompileInlineFunction(file)

            member _.GetImplementationFile(fileName) =
                com.GetImplementationFile(fileName)

            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)

            member _.AddWatchDependency(fileName) =
                com.AddWatchDependency(fileName)

            member _.AddLog
                (
                    msg,
                    severity,
                    ?range,
                    ?fileName: string,
                    ?tag: string
                )
                =
                com.AddLog(
                    msg,
                    severity,
                    ?range = range,
                    ?fileName = fileName,
                    ?tag = tag
                )

    let makeCompiler com = DartCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IDartCompiler

        let declScopes =
            let hs = HashSet()

            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)

            hs

        let ctx =
            {
                File = file
                UsedNames =
                    {
                        RootScope = HashSet file.UsedNamesInRootScope
                        DeclarationScopes = declScopes
                        CurrentDeclarationScope = Unchecked.defaultof<_>
                    }
                DecisionTargets = []
                EntityAndMemberGenericParams = []
                AssertedTypes = Map.empty
                CastedUnions = Dictionary()
                TailCallOpportunity = None
                OptimizeTailCall = fun () -> ()
                VarsDeclaredInScope = HashSet()
                ConstIdents = Set.empty
            }

        let rootDecls =
            List.collect (transformDeclaration com ctx) file.Declarations

        let imports = com.GetAllImports()

        {
            File.Imports = imports
            Declarations = rootDecls
        }
