module Fable.Transforms.FableTransforms

open Fable
open Fable.AST.Fable

let isIdentCaptured identName expr =
    let rec loop isClosure exprs =
        match exprs with
        | [] -> false
        | expr :: restExprs ->
            match expr with
            | IdentExpr i when i.Name = identName -> isClosure
            | Lambda(_, body, _) ->
                loop true [ body ] || loop isClosure restExprs
            | Delegate(_, body, _, _) ->
                loop true [ body ] || loop isClosure restExprs
            | ObjectExpr(members, _, baseCall) ->
                let memberExprs = members |> List.map (fun m -> m.Body)

                loop true memberExprs
                || loop isClosure (Option.toList baseCall @ restExprs)
            | e ->
                let sub = getSubExpressions e
                loop isClosure (sub @ restExprs)

    loop false [ expr ]

let isTailRecursive identName expr =
    let mutable isTailRec = true
    let mutable isRecursive = false

    let rec loop inTailPos =
        function
        | CurriedApply(IdentExpr i, _, _, _)
        | Call(IdentExpr i, _, _, _) as e when i.Name = identName ->
            isRecursive <- true
            isTailRec <- isTailRec && inTailPos
            getSubExpressions e |> List.iter (loop false)
        | Sequential exprs ->
            let lastIndex = (List.length exprs) - 1
            exprs |> List.iteri (fun i e -> loop (i = lastIndex) e)
        | Let(_, value, body) ->
            loop false value
            loop inTailPos body
        | LetRec(bindings, body) ->
            List.map snd bindings |> List.iter (loop false)
            loop inTailPos body
        | IfThenElse(cond, thenExpr, elseExpr, _) ->
            loop false cond
            loop inTailPos thenExpr
            loop inTailPos elseExpr
        | DecisionTree(expr, targets) ->
            loop false expr
            List.map snd targets |> List.iter (loop inTailPos)
        | e -> getSubExpressions e |> List.iter (loop false)

    loop true expr
    isTailRec <- isTailRec && isRecursive
    isRecursive, isTailRec

let replaceValues replacements expr =
    if Map.isEmpty replacements then
        expr
    else
        expr
        |> visitFromInsideOut (
            function
            | IdentExpr id as e ->
                match Map.tryFind id.Name replacements with
                | Some e -> e
                | None -> e
            | e -> e
        )

let replaceValuesAndGenArgs (replacements: Map<string, Expr>) expr =
    if Map.isEmpty replacements then
        expr
    else
        expr
        |> visitFromInsideOut (
            function
            | IdentExpr id as e ->
                match Map.tryFind id.Name replacements with
                | Some e ->
                    if typeEquals true e.Type id.Type then
                        e
                    else
                        extractGenericArgs e id.Type |> replaceGenericArgs e
                | None -> e
            | e -> e
        )

let replaceNames replacements expr =
    if Map.isEmpty replacements then
        expr
    else
        expr
        |> visitFromInsideOut (
            function
            | IdentExpr id as e ->
                match Map.tryFind id.Name replacements with
                | Some name -> { id with Name = name } |> IdentExpr
                | None -> e
            | e -> e
        )

let countReferencesUntil limit identName body =
    let mutable count = 0

    body
    |> deepExists (
        function
        | IdentExpr id2 when id2.Name = identName ->
            count <- count + 1
            count >= limit
        | _ -> false
    )
    |> ignore

    count

let referencesMutableIdent body =
    body
    |> deepExists (
        function
        | IdentExpr id -> id.IsMutable
        | _ -> false
    )

let noSideEffectBeforeIdent identName expr =
    let mutable sideEffect = false

    let orSideEffect found =
        if found then
            true
        else
            sideEffect <- true
            true

    let rec findIdentOrSideEffect =
        function
        | Unresolved _ -> false
        | IdentExpr id ->
            if id.Name = identName then
                true
            elif id.IsMutable then
                sideEffect <- true
                true
            else
                false
        // If the field is mutable we cannot inline, see #2683
        | Get(e, FieldGet info, _, _) ->
            if info.CanHaveSideEffects then
                sideEffect <- true
                true
            else
                findIdentOrSideEffect e
        // We don't have enough information here, so just assume there's a side effect just in case
        | Get(_, ExprGet _, _, _) ->
            sideEffect <- true
            true
        | Get(e,
              (TupleIndex _ | UnionField _ | UnionTag | ListHead | ListTail | OptionValue),
              _,
              _) -> findIdentOrSideEffect e
        | Import _
        | Lambda _
        | Delegate _ -> false
        | Extended((Throw _ | Debugger), _) -> true
        | Extended(Curry(e, _), _) -> findIdentOrSideEffect e
        | CurriedApply(callee, args, _, _) ->
            callee :: args |> findIdentOrSideEffectInList |> orSideEffect
        | Call(e1, info, _, _) ->
            match info.Tags, info.Args with
            // HACK: let beta reduction jump over keyValueList/createObj in Fable.React
            | Tags.Contains "pojo", IdentExpr i :: _ -> i.Name = identName
            | _ ->
                e1 :: (Option.toList info.ThisArg) @ info.Args
                |> findIdentOrSideEffectInList
                |> orSideEffect
        | Operation(kind, _, _, _) ->
            match kind with
            | Unary(_, operand) -> findIdentOrSideEffect operand
            | Binary(_, left, right)
            | Logical(_, left, right) ->
                findIdentOrSideEffect left || findIdentOrSideEffect right
        | Value(value, _) ->
            match value with
            | ThisValue _
            | BaseValue _
            | TypeInfo _
            | Null _
            | UnitConstant
            | NumberConstant _
            | BoolConstant _
            | CharConstant _
            | StringConstant _
            | RegexConstant _ -> false
            | NewList(None, _)
            | NewOption(None, _, _) -> false
            | NewOption(Some e, _, _) -> findIdentOrSideEffect e
            | NewList(Some(h, t), _) ->
                findIdentOrSideEffect h || findIdentOrSideEffect t
            | NewArray(kind, _, _) ->
                match kind with
                | ArrayValues exprs -> findIdentOrSideEffectInList exprs
                | ArrayAlloc e
                | ArrayFrom e -> findIdentOrSideEffect e
            | StringTemplate(_, _, exprs)
            | NewTuple(exprs, _)
            | NewUnion(exprs, _, _, _)
            | NewRecord(exprs, _, _)
            | NewAnonymousRecord(exprs, _, _, _) ->
                findIdentOrSideEffectInList exprs
        | Sequential exprs -> findIdentOrSideEffectInList exprs
        | Let(_, v, b) -> findIdentOrSideEffect v || findIdentOrSideEffect b
        | TypeCast(e, _)
        | Test(e, _, _) -> findIdentOrSideEffect e
        | IfThenElse(cond, thenExpr, elseExpr, _) ->
            findIdentOrSideEffect cond
            || findIdentOrSideEffect thenExpr
            || findIdentOrSideEffect elseExpr
        // TODO: Check member bodies in ObjectExpr
        | ObjectExpr _
        | LetRec _
        | Emit _
        | Set _
        | DecisionTree _
        | DecisionTreeSuccess _ // Check sub expressions here?
        | WhileLoop _
        | ForLoop _
        | TryCatch _ ->
            sideEffect <- true
            true

    and findIdentOrSideEffectInList exprs =
        List.exists findIdentOrSideEffect exprs

    findIdentOrSideEffect expr && not sideEffect

let canInlineArg identName value body =
    match value with
    | Value((Null _ | UnitConstant | TypeInfo _ | BoolConstant _ | NumberConstant _ | CharConstant _),
            _) -> true
    | Value(StringConstant s, _) -> s.Length < 100
    | _ ->
        let refCount = countReferencesUntil 2 identName body

        (refCount <= 1 && not (canHaveSideEffects value))
        // If it can have side effects, make sure is at least referenced once so the expression is not erased
        || (refCount = 1
            && noSideEffectBeforeIdent identName body
            && not (isIdentCaptured identName body))

/// Returns arity of lambda (or lambda option) types
let (|Arity|) typ =
    let rec getArity arity =
        function
        | LambdaType(_, returnType) -> getArity (arity + 1) returnType
        | _ -> arity

    match typ with
    | MaybeOption(LambdaType(_, returnType)) -> getArity 1 returnType
    | _ -> 0

/// Returns arity of lambda (or lambda option) and uncurried type
let private uncurryType' typ =
    let rec uncurryType' accArity accArgs =
        function
        | LambdaType(arg, returnType) ->
            uncurryType' (accArity + 1) (arg :: accArgs) returnType
        | returnType ->
            let argTypes = List.rev accArgs

            let uncurried =
                match typ with
                | Option(_, isStruct) ->
                    Option(DelegateType(argTypes, returnType), isStruct)
                | _ -> DelegateType(argTypes, returnType)

            accArity, uncurried

    match typ with
    | MaybeOption(LambdaType(arg, returnType)) ->
        uncurryType' 1 [ arg ] returnType
    | _ -> 0, typ

let uncurryType typ = uncurryType' typ |> snd

module private Transforms =
    let rec (|ImmediatelyApplicable|_|) appliedArgsLen expr =
        if appliedArgsLen = 0 then
            None
        else
            match expr with
            | Lambda(arg, body, _) ->
                let appliedArgsLen = appliedArgsLen - 1

                if appliedArgsLen = 0 then
                    Some([ arg ], body)
                else
                    match body with
                    | ImmediatelyApplicable appliedArgsLen (args, body) ->
                        Some(arg :: args, body)
                    | _ -> Some([ arg ], body)
            // If the lambda is immediately applied we don't need the closures
            | NestedRevLets(bindings, Lambda(arg, body, _)) ->
                let body =
                    List.fold (fun body (i, v) -> Let(i, v, body)) body bindings

                let appliedArgsLen = appliedArgsLen - 1

                if appliedArgsLen = 0 then
                    Some([ arg ], body)
                else
                    match body with
                    | ImmediatelyApplicable appliedArgsLen (args, body) ->
                        Some(arg :: args, body)
                    | _ -> Some([ arg ], body)
            | _ -> None

    let tryInlineBinding (com: Compiler) (ident: Ident) value letBody =
        let canInlineBinding =
            match value with
            | Import(i, _, _) -> i.IsCompilerGenerated
            | Call(callee, info, _, _) when
                List.isEmpty info.Args && List.contains "value" info.Tags
                ->
                canInlineArg ident.Name callee letBody
            // Replace non-recursive lambda bindings
            | NestedLambda(_args, lambdaBody, _name) ->
                match lambdaBody with
                | Import(i, _, _) -> i.IsCompilerGenerated
                // Check the lambda doesn't reference itself recursively
                | _ ->
                    countReferencesUntil 1 ident.Name lambdaBody = 0
                    && canInlineArg ident.Name value letBody
                    // If we inline the lambda Fable2Rust doesn't have
                    // a chance to clone the mutable ident
                    && (if com.Options.Language = Rust then
                            referencesMutableIdent lambdaBody |> not
                        else
                            true)
            | _ -> canInlineArg ident.Name value letBody

        if canInlineBinding then
            let value =
                match value with
                // Ident becomes the name of the function (mainly used for tail call optimizations)
                | Lambda(arg, funBody, _) ->
                    Lambda(arg, funBody, Some ident.Name)
                | Delegate(args, funBody, _, tags) ->
                    Delegate(args, funBody, Some ident.Name, tags)
                | value -> value

            Some(ident, value)
        else
            None

    let applyArgs com r t (args: Ident list) (argExprs: Expr list) body =
        let argsLen = args.Length
        let argExprsLen = argExprs.Length

        let appliedArgs, restArgs, appliedArgExprs, restArgExprs =
            if argsLen = argExprs.Length then
                args, [], argExprs, []
            elif argsLen < argExprsLen then
                let appliedArgExprs, restArgExprs =
                    List.splitAt argsLen argExprs

                args, [], appliedArgExprs, restArgExprs
            else
                let appliedArgs, restArgs = List.splitAt argsLen args
                appliedArgs, restArgs, argExprs, []

        let bindings, replacements =
            (([], Map.empty), appliedArgs, appliedArgExprs)
            |||> List.fold2 (fun (bindings, replacements) ident expr ->
                match tryInlineBinding com ident expr body with
                | Some(ident, expr) ->
                    bindings, Map.add ident.Name expr replacements
                | None -> (ident, expr) :: bindings, replacements
            )

        let body = replaceValues replacements body
        let body = List.fold (fun body (i, v) -> Let(i, v, body)) body bindings

        match restArgs, restArgExprs with
        | [], [] -> body
        | [], restArgExprs -> CurriedApply(body, restArgExprs, t, r)
        | restArgs, _ -> makeLambda restArgs body

    let rec lambdaBetaReduction (com: Compiler) e =
        match e with
        | Call(Delegate(args, body, _, _), info, t, r) when
            List.sameLength args info.Args
            ->
            let body = visitFromOutsideIn (lambdaBetaReduction com) body

            let thisArgExpr =
                info.ThisArg
                |> Option.map (visitFromOutsideIn (lambdaBetaReduction com))

            let argExprs =
                info.Args
                |> List.map (visitFromOutsideIn (lambdaBetaReduction com))

            let info =
                { info with
                    ThisArg = thisArgExpr
                    Args = argExprs
                }

            applyArgs com r t args info.Args body |> Some

        | NestedApply(applied, argExprs, t, r) ->
            match applied with
            | ImmediatelyApplicable argExprs.Length (args, body) ->
                let argExprs =
                    argExprs
                    |> List.map (visitFromOutsideIn (lambdaBetaReduction com))

                let body = visitFromOutsideIn (lambdaBetaReduction com) body
                applyArgs com r t args argExprs body |> Some
            | _ -> None
        | _ -> None

    let bindingBetaReduction (com: Compiler) e =
        // Don't erase user-declared bindings in debug mode for better output
        let isErasingCandidate (ident: Ident) =
            (not com.Options.DebugMode) || ident.IsCompilerGenerated

        match e with
        | Let(ident, value, letBody) when
            (not ident.IsMutable) && isErasingCandidate ident
            ->
            match tryInlineBinding com ident value letBody with
            | Some(ident, value) ->
                // Sometimes we inline a local generic function, so we need to check
                // if the replaced ident has the concrete type. This happens in FSharp2Fable step,
                // see FSharpExprPatterns.CallWithWitnesses
                replaceValuesAndGenArgs (Map [ ident.Name, value ]) letBody
            | None -> e
        | e -> e

    let typeEqualsAtCompileTime t1 t2 =
        let stripMeasure =
            function
            | Number(kind, NumberInfo.IsMeasure _) ->
                Number(kind, NumberInfo.Empty)
            | t -> t

        typeEquals true (stripMeasure t1) (stripMeasure t2)

    let rec tryEqualsAtCompileTime a b =
        match a, b with
        | Value(TypeInfo(a, []), _), Value(TypeInfo(b, []), _) ->
            typeEqualsAtCompileTime a b |> Some
        | Value(Null _, _), Value(Null _, _)
        | Value(UnitConstant, _), Value(UnitConstant, _) -> Some true
        | Value(BoolConstant a, _), Value(BoolConstant b, _) -> Some(a = b)
        | Value(CharConstant a, _), Value(CharConstant b, _) -> Some(a = b)
        | Value(StringConstant a, _), Value(StringConstant b, _) -> Some(a = b)
        | Value(NumberConstant(a, _, _), _), Value(NumberConstant(b, _, _), _) ->
            Some(a = b)
        | Value(NewOption(None, _, _), _), Value(NewOption(None, _, _), _) ->
            Some true
        | Value(NewOption(Some a, _, _), _), Value(NewOption(Some b, _, _), _) ->
            tryEqualsAtCompileTime a b
        | _ -> None

    let operationReduction (_com: Compiler) e =
        match e with
        // TODO: Other binary operations and numeric types
        | Operation(Binary(AST.BinaryPlus, v1, v2), _, _, _) ->
            match v1, v2 with
            | Value(StringConstant v1, r1), Value(StringConstant v2, r2) ->
                Value(
                    StringConstant(v1 + v2),
                    addRanges
                        [
                            r1
                            r2
                        ]
                )
            // Assume NumberKind and NumberInfo are the same
            | Value(NumberConstant(:? int as v1, AST.Int32, NumberInfo.Empty),
                    r1),
              Value(NumberConstant(:? int as v2, AST.Int32, NumberInfo.Empty),
                    r2) ->
                Value(
                    NumberConstant(v1 + v2, AST.Int32, NumberInfo.Empty),
                    addRanges
                        [
                            r1
                            r2
                        ]
                )
            | _ -> e

        | Operation(Logical(AST.LogicalAnd, (Value(BoolConstant b, _) as v1), v2),
                    [],
                    _,
                    _) ->
            if b then
                v2
            else
                v1
        | Operation(Logical(AST.LogicalAnd, v1, (Value(BoolConstant b, _) as v2)),
                    [],
                    _,
                    _) ->
            if b then
                v1
            else
                v2
        | Operation(Logical(AST.LogicalOr, (Value(BoolConstant b, _) as v1), v2),
                    [],
                    _,
                    _) ->
            if b then
                v1
            else
                v2
        | Operation(Logical(AST.LogicalOr, v1, (Value(BoolConstant b, _) as v2)),
                    [],
                    _,
                    _) ->
            if b then
                v2
            else
                v1

        | Operation(Unary(AST.UnaryNot, Value(BoolConstant b, r)), [], _, _) ->
            Value(BoolConstant(not b), r)

        | Operation(Binary((AST.BinaryEqual | AST.BinaryUnequal as op), v1, v2),
                    [],
                    _,
                    _) ->
            let isNot = op = AST.BinaryUnequal

            tryEqualsAtCompileTime v1 v2
            |> Option.map (fun b ->
                (if isNot then
                     not b
                 else
                     b)
                |> makeBoolConst
            )
            |> Option.defaultValue e

        | Test(expr, kind, _) ->
            match kind, expr with
            // This optimization doesn't work well with erased unions
            // | TypeTest typ, expr ->
            //     typeEqualsAtCompileTime typ expr.Type |> makeBoolConst
            | OptionTest isSome, Value(NewOption(expr, _, _), _) ->
                isSome = Option.isSome expr |> makeBoolConst
            | ListTest isCons, Value(NewList(headAndTail, _), _) ->
                isCons = Option.isSome headAndTail |> makeBoolConst
            | UnionCaseTest tag1, Value(NewUnion(_, tag2, _, _), _) ->
                tag1 = tag2 |> makeBoolConst
            | _ -> e

        | IfThenElse(Value(BoolConstant b, _), thenExpr, elseExpr, _) ->
            if b then
                thenExpr
            else
                elseExpr

        | _ -> e

    let curryIdentsInBody replacements body =
        visitFromInsideOut
            (function
            | IdentExpr id as e ->
                match Map.tryFind id.Name replacements with
                | Some arity -> Extended(Curry(e, arity), e.Range)
                | None -> e
            | e -> e)
            body

    let curryArgIdentsAndReplaceInBody (args: Ident list) body =
        let replacements, args =
            ((Map.empty, []), args)
            ||> List.fold (fun (replacements, uncurriedArgs) arg ->
                match uncurryType' arg.Type with
                | arity, uncurriedType when arity > 1 ->
                    Map.add arg.Name arity replacements,
                    { arg with Type = uncurriedType } :: uncurriedArgs
                | _ -> replacements, arg :: uncurriedArgs
            )

        if Map.isEmpty replacements then
            List.rev args, body
        else
            List.rev args, curryIdentsInBody replacements body

    let uncurryExpr com arity expr =
        let matches arity arity2 =
            match arity with
            // TODO: check cases where arity <> arity2
            | Some arity -> arity = arity2
            // Remove currying for dynamic operations (no arity)
            | None -> true

        match expr, arity with
        | MaybeCasted(LambdaUncurriedAtCompileTime arity lambda), _ -> lambda
        | Extended(Curry(innerExpr, arity2), _), _ when matches arity arity2 ->
            innerExpr
        | Get(Extended(Curry(innerExpr, arity2), _), OptionValue, t, r), _ when
            matches arity arity2
            ->
            Get(innerExpr, OptionValue, t, r)
        | Value(NewOption(Some(Extended(Curry(innerExpr, arity2), _)),
                          t,
                          isStruct),
                r),
          _ when matches arity arity2 ->
            Value(NewOption(Some(innerExpr), t, isStruct), r)
        // User imports are uncurried even if they're typed as lambdas, see test "ofImport should inline properly"
        | Import({ Kind = UserImport _ }, _, _), _ -> expr
        | _, Some arity -> Replacements.Api.uncurryExprAtRuntime com arity expr
        | _, None -> expr

    let rec uncurryAnonRecordArg
        (com: Compiler)
        expectedFieldNames
        expectedGenArgs
        isStruct
        (expr: Expr)
        =
        let needsCurrying =
            expectedGenArgs
            |> List.exists (fun expectedGenArg ->
                // If the lambda returns a generic the actual arity may be higher than expected
                match uncurryType expectedGenArg with
                | MaybeOption(DelegateType(_, GenericParam _)) -> true
                | _ -> false
            )

        match expr.Type with
        | AnonymousRecordType(actualFieldNames, actualGenArgs, _) as argType when
            needsCurrying
            ->
            let binding, arg =
                match expr with
                | IdentExpr _ -> None, expr
                | arg ->
                    let ident =
                        makeTypedIdent
                            argType
                            $"anonRec{com.IncrementCounter()}"

                    Some(ident, arg), IdentExpr ident

            let actualGenArgs = Seq.zip actualFieldNames actualGenArgs |> Map

            let values =
                expectedFieldNames
                |> Array.mapToList (fun fieldName ->
                    let actualType =
                        Map.tryFind fieldName actualGenArgs
                        |> Option.defaultValue Any

                    let value =
                        getImmutableFieldWith None actualType arg fieldName

                    match actualType with
                    | Arity arity when arity > 1 ->
                        Extended(Curry(value, arity), None)
                    | _ -> value
                )
                |> uncurryArgs com false expectedGenArgs

            let anonRec =
                NewAnonymousRecord(
                    values,
                    expectedFieldNames,
                    expectedGenArgs,
                    isStruct
                )
                |> makeValue None

            match binding with
            | Some(ident, value) -> Let(ident, value, anonRec)
            | None -> anonRec
        | _ -> expr

    and uncurryArgs com autoUncurrying argTypes args =
        let mapArgs f argTypes args =
            let rec mapArgsInner f acc argTypes args =
                match argTypes, args with
                | head1 :: tail1, head2 :: tail2 ->
                    let x = f head1 head2
                    mapArgsInner f (x :: acc) tail1 tail2
                | [], head2 :: tail2 when autoUncurrying ->
                    let x = f Any head2
                    mapArgsInner f (x :: acc) [] tail2
                | [], args2 -> (List.rev acc) @ args2
                | _, [] -> List.rev acc

            mapArgsInner f [] argTypes args

        (argTypes, args)
        ||> mapArgs (fun expectedType arg ->
            match expectedType with
            | Any when autoUncurrying -> uncurryExpr com None arg

            | AnonymousRecordType(expectedFieldNames, expectedGenArgs, isStruct) ->
                uncurryAnonRecordArg
                    com
                    expectedFieldNames
                    expectedGenArgs
                    isStruct
                    arg

            | Arity arity when arity > 1 -> uncurryExpr com (Some arity) arg

            | _ -> arg
        )

    let uncurryInnerFunctions (_: Compiler) e =
        let curryIdentInBody identName (args: Ident list) body =
            curryIdentsInBody (Map [ identName, List.length args ]) body

        match e with
        | Let(ident, NestedLambdaWithSameArity(args, fnBody, _), letBody) when
            List.isMultiple args && not ident.IsMutable
            ->
            let fnBody = curryIdentInBody ident.Name args fnBody
            let letBody = curryIdentInBody ident.Name args letBody

            Let(
                { ident with Type = uncurryType ident.Type },
                Delegate(args, fnBody, None, Tags.empty),
                letBody
            )
        // Anonymous lambda immediately applied
        | CurriedApply(NestedLambdaWithSameArity(args, fnBody, Some name),
                       argExprs,
                       t,
                       r) when
            List.isMultiple args && List.sameLength args argExprs
            ->
            let fnBody = curryIdentInBody name args fnBody

            let info =
                makeCallInfo None argExprs (args |> List.map (fun a -> a.Type))

            Delegate(args, fnBody, Some name, Tags.empty) |> makeCall r t info
        | e -> e

    let propagateCurryingThroughLets (_: Compiler) =
        function
        | Let(ident, value, body) when not ident.IsMutable ->
            let ident, value, arity =
                match value with
                | Extended(Curry(innerExpr, arity), _) ->
                    ident, innerExpr, Some arity
                | Get(Extended(Curry(innerExpr, arity), _), OptionValue, t, r) ->
                    ident, Get(innerExpr, OptionValue, t, r), Some arity
                | Value(NewOption(Some(Extended(Curry(innerExpr, arity), _)),
                                  t,
                                  isStruct),
                        r) ->
                    ident,
                    Value(NewOption(Some(innerExpr), t, isStruct), r),
                    Some arity
                | _ -> ident, value, None

            match arity with
            | None -> Let(ident, value, body)
            | Some arity ->
                let replacements = Map [ ident.Name, arity ]

                Let(
                    { ident with Type = uncurryType ident.Type },
                    value,
                    curryIdentsInBody replacements body
                )
        | e -> e

    let uncurryMemberArgs (m: MemberDecl) =
        let args, body = curryArgIdentsAndReplaceInBody m.Args m.Body

        { m with
            Args = args
            Body = body
        }

    let (|GetField|_|) (com: Compiler) =
        function
        | Get(callee, kind, _, r) ->
            match kind with
            | FieldGet { FieldType = Some fieldType } ->
                Some(callee, fieldType, r)
            | UnionField info ->
                let e = com.GetEntity(info.Entity)

                List.tryItem info.CaseIndex e.UnionCases
                |> Option.bind (fun c ->
                    List.tryItem info.FieldIndex c.UnionCaseFields
                )
                |> Option.map (fun f -> callee, f.FieldType, r)
            | _ -> None
        | _ -> None

    let curryReceivedArgs (com: Compiler) e =
        match e with
        // Args passed to a lambda are not uncurried, as it's difficult to do it right, see #2657
        // | Lambda(arg, body, name)
        | Delegate(args, body, name, tags) ->
            let args, body = curryArgIdentsAndReplaceInBody args body
            Delegate(args, body, name, tags)

        // Uncurry also values received from getters
        | GetField com (_callee, Arity arity, r) when arity > 1 ->
            Extended(Curry(e, arity), r)

        | ObjectExpr(members, t, baseCall) ->
            let members =
                members
                |> List.map (fun m ->
                    let args, body =
                        curryArgIdentsAndReplaceInBody m.Args m.Body

                    { m with
                        Args = args
                        Body = body
                    }
                )

            ObjectExpr(members, t, baseCall)

        | e -> e

    let isGetterOrValueWithoutGenerics (mRef: MemberFunctionOrValue) =
        mRef.IsGetter || (mRef.IsValue && List.isEmpty mRef.GenericParameters)

    let uncurrySendingArgs (com: Compiler) e =
        let uncurryConsArgs args (fields: seq<Field>) =
            let argTypes =
                fields |> Seq.map (fun fi -> fi.FieldType) |> Seq.toList

            uncurryArgs com false argTypes args

        match e with
        | Call(callee, info, t, r) ->
            let args = uncurryArgs com false info.SignatureArgTypes info.Args
            let info = { info with Args = args }
            Call(callee, info, t, r)
        | Emit({ CallInfo = callInfo } as emitInfo, t, r) ->
            let args =
                uncurryArgs com true callInfo.SignatureArgTypes callInfo.Args

            Emit(
                { emitInfo with CallInfo = { callInfo with Args = args } },
                t,
                r
            )
        // Uncurry also values in setters or new record/union/tuple
        | Value(NewRecord(args, ent, genArgs), r) ->
            let args = com.GetEntity(ent).FSharpFields |> uncurryConsArgs args
            Value(NewRecord(args, ent, genArgs), r)
        | Value(NewAnonymousRecord(args, fieldNames, genArgs, isStruct), r) ->
            let args = uncurryArgs com false genArgs args
            Value(NewAnonymousRecord(args, fieldNames, genArgs, isStruct), r)
        | Value(NewUnion(args, tag, ent, genArgs), r) ->
            let uci = com.GetEntity(ent).UnionCases[tag]
            let args = uncurryConsArgs args uci.UnionCaseFields
            Value(NewUnion(args, tag, ent, genArgs), r)
        | Set(e, FieldSet(fieldName), t, value, r) ->
            let value = uncurryArgs com false [ t ] [ value ]
            Set(e, FieldSet(fieldName), t, List.head value, r)
        | ObjectExpr(members, t, baseCall) ->
            let members =
                members
                |> List.map (fun m ->
                    match m.Body.Type with
                    | Arity arity when arity > 1 ->
                        match com.TryGetMember(m.MemberRef) with
                        | Some mRef when isGetterOrValueWithoutGenerics mRef ->
                            match mRef.ReturnParameter.Type with
                            // It may happen the arity of the abstract signature is smaller than actual arity
                            | Arity arity when arity > 1 ->
                                { m with
                                    Body = uncurryExpr com (Some arity) m.Body
                                }
                            | _ -> m
                        | _ -> m
                    | _ -> m
                )

            ObjectExpr(members, t, baseCall)
        | e -> e

    let rec uncurryApplications (com: Compiler) e =
        let uncurryApply r t applied args uncurriedArity =
            let argsLen = List.length args

            if uncurriedArity = argsLen then
                // This is already uncurried we don't need the signature arg types anymore,
                // just make a normal call
                let info = makeCallInfo None args []
                makeCall r t info applied
            elif uncurriedArity < argsLen then
                let appliedArgs, restArgs = List.splitAt uncurriedArity args
                let info = makeCallInfo None appliedArgs []

                let intermediateType =
                    match List.rev restArgs with
                    | [] -> Any
                    | arg :: args ->
                        (LambdaType(arg.Type, t), args)
                        ||> List.fold (fun t a -> LambdaType(a.Type, t))

                let applied = makeCall None intermediateType info applied
                CurriedApply(applied, restArgs, t, r)
            else
                Replacements.Api.partialApplyAtRuntime
                    com
                    t
                    (uncurriedArity - argsLen)
                    applied
                    args

        match e with
        | Test(Extended(Curry(expr, _uncurriedArity), _), OptionTest isSome, r) ->
            let expr = visitFromOutsideIn (uncurryApplications com) expr
            Test(expr, OptionTest isSome, r) |> Some
        | NestedApply(applied, args, t, r) ->
            let applied = visitFromOutsideIn (uncurryApplications com) applied

            let args =
                args |> List.map (visitFromOutsideIn (uncurryApplications com))

            match applied with
            | Extended(Curry(applied, uncurriedArity), _) ->
                uncurryApply r t applied args uncurriedArity |> Some
            | Get(Extended(Curry(applied, uncurriedArity), _),
                  OptionValue,
                  t2,
                  r2) ->
                uncurryApply
                    r
                    t
                    (Get(applied, OptionValue, t2, r2))
                    args
                    uncurriedArity
                |> Some
            | _ -> CurriedApply(applied, args, t, r) |> Some
        | _ -> None

open Transforms

// ATTENTION: Order of transforms matters
let getTransformations (_com: Compiler) =
    [ // First apply beta reduction
        fun com e -> visitFromInsideOut (bindingBetaReduction com) e
        fun com e -> visitFromOutsideIn (lambdaBetaReduction com) e
        // Make a new binding beta reduction pass after applying lambdas
        fun com e -> visitFromInsideOut (bindingBetaReduction com) e
        fun com e -> visitFromInsideOut (operationReduction com) e
        // Then apply uncurry optimizations
        // Functions passed as arguments in calls (but NOT in curried applications) are being uncurried so we have to re-curry them
        // The next steps will uncurry them again if they're immediately applied or passed again as call arguments
        fun com e -> visitFromInsideOut (curryReceivedArgs com) e
        fun com e -> visitFromInsideOut (uncurryInnerFunctions com) e
        fun com e -> visitFromInsideOut (propagateCurryingThroughLets com) e
        fun com e -> visitFromInsideOut (uncurrySendingArgs com) e
        // uncurryApplications must come after uncurrySendingArgs as it erases argument type info
        fun com e -> visitFromOutsideIn (uncurryApplications com) e
    ]

let rec transformDeclaration transformations (com: Compiler) file decl =
    let transformExpr (com: Compiler) e =
        List.fold (fun e f -> f com e) e transformations

    let transformMemberBody com (m: MemberDecl) =
        { m with Body = transformExpr com m.Body }

    match decl with
    | ModuleDeclaration decl ->
        let members =
            decl.Members
            |> List.map (transformDeclaration transformations com file)

        { decl with Members = members } |> ModuleDeclaration

    | ActionDeclaration decl ->
        { decl with Body = transformExpr com decl.Body } |> ActionDeclaration

    | MemberDeclaration m ->
        m
        |> uncurryMemberArgs
        |> transformMemberBody com
        |> fun m -> com.ApplyMemberDeclarationPlugin(file, m)
        |> MemberDeclaration

    | ClassDeclaration decl ->
        // (ent, ident, cons, baseCall, attachedMembers)
        let attachedMembers =
            decl.AttachedMembers
            |> List.map (fun m ->
                let uncurriedMember =
                    if m.IsMangled then
                        None
                    else
                        match m.Body.Type with
                        | Arity arity when arity > 1 ->
                            m.ImplementedSignatureRef
                            |> Option.bind (com.TryGetMember)
                            |> Option.bind (fun mRef ->
                                if isGetterOrValueWithoutGenerics mRef then
                                    match mRef.ReturnParameter.Type with
                                    // It may happen the arity of the abstract signature is smaller than actual arity
                                    | Arity arity when arity > 1 ->
                                        Some
                                            { m with
                                                Body =
                                                    uncurryExpr
                                                        com
                                                        (Some arity)
                                                        m.Body
                                            }
                                    | _ -> None
                                else
                                    None
                            )
                        | _ -> None

                let m =
                    match uncurriedMember with
                    | Some m -> m
                    | None -> uncurryMemberArgs m

                transformMemberBody com m
            )

        let cons, baseCall =
            match decl.Constructor, decl.BaseCall with
            | None, _ -> None, None
            | Some cons, None ->
                uncurryMemberArgs cons |> transformMemberBody com |> Some, None
            | Some cons, Some baseCall ->
                // In order to uncurry correctly the baseCall arguments,
                // we need to include it in the constructor body
                let args, body =
                    Sequential
                        [
                            baseCall
                            cons.Body
                        ]
                    |> curryArgIdentsAndReplaceInBody cons.Args

                transformExpr com body
                |> function
                    | Sequential [ baseCall; body ] ->
                        Some
                            { cons with
                                Args = args
                                Body = body
                            },
                        Some baseCall
                    | body ->
                        Some
                            { cons with
                                Args = args
                                Body = body
                            },
                        None // Unexpected, raise error?

        { decl with
            Constructor = cons
            BaseCall = baseCall
            AttachedMembers = attachedMembers
        }
        |> ClassDeclaration

let transformFile (com: Compiler) (file: Fable.AST.Fable.File) =
    let transformations = getTransformations com

    let newDecls =
        List.map
            (transformDeclaration transformations com file)
            file.Declarations

    Fable.AST.Fable.File(newDecls, usedRootNames = file.UsedNamesInRootScope)
