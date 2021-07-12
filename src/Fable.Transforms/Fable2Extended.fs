module rec Fable.Transforms.Fable2Extended

open Fable
open Fable.AST
open System.Collections.Generic
open Fable.Transforms.AST

type ReturnStrategy =
    | Return
    | ReturnUnit
    | Assign of Fable.Expr
    | Target of Fable.Ident

type Import =
  { Selector: string
    LocalIdent: string option
    Path: string }

type ITailCallOpportunity =
    abstract Label: string
    abstract Args: string list
    abstract IsRecursiveRef: Fable.Expr -> bool

type UsedNames =
  { RootScope: HashSet<string>
    DeclarationScopes: HashSet<string>
    CurrentDeclarationScope: HashSet<string> }

type Context =
  { File: Fable.File
    UsedNames: UsedNames
    DecisionTargets: (Fable.Ident list * Fable.Expr) list
    HoistVars: Fable.Ident list -> bool
    TailCallOpportunity: ITailCallOpportunity option
    OptimizeTailCall: unit -> unit
    ScopedTypeParams: Set<string> }

type IExtendedCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> seq<Import>
    abstract GetImportExpr: Context * selector: string * path: string * SourceLocation option -> Fable.Expr
    abstract TransformAsExpr: Context * Fable.Expr -> Fable.Expr
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Fable.Expr array
    abstract TransformImport: Context * selector:string * path:string -> Fable.Expr
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> (Fable.Ident array) * Fable.Expr
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

module Util =
    let (|TransformExpr|) (com: IExtendedCompiler) ctx e =
        com.TransformAsExpr(ctx, e)

    let (|Function|_|) = function
        | Fable.Lambda(arg, body, _) -> Some([arg], body)
        | Fable.Delegate(args, body, _) -> Some(args, body)
        | _ -> None

    let (|Lets|_|) = function
        | Fable.Let(ident, value, body) -> Some([ident, value], body)
        | Fable.LetRec(bindings, body) -> Some(bindings, body)
        | _ -> None

    let discardUnitArg (args: Fable.Ident list) =
        match args with
        | [] -> []
        | [unitArg] when unitArg.Type = Fable.Unit -> []
        | [thisArg; unitArg] when thisArg.IsThisArgument && unitArg.Type = Fable.Unit -> [thisArg]
        | args -> args

    let getUniqueNameInRootScope (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun name ->
            ctx.UsedNames.RootScope.Contains(name)
            || ctx.UsedNames.DeclarationScopes.Contains(name))
        ctx.UsedNames.RootScope.Add(name) |> ignore
        name

    let getUniqueNameInDeclarationScope (ctx: Context) name =
        let name = (name, Naming.NoMemberPart) ||> Naming.sanitizeIdent (fun name ->
            ctx.UsedNames.RootScope.Contains(name) || ctx.UsedNames.CurrentDeclarationScope.Contains(name))
        ctx.UsedNames.CurrentDeclarationScope.Add(name) |> ignore
        name

    type NamedTailCallOpportunity(_com: Compiler, ctx, name, args: Fable.Ident list) =
        // Capture the current argument values to prevent delayed references from getting corrupted,
        // for that we use block-scoped ES2015 variable declarations. See #681, #1859
        // TODO: Local unique ident names
        let argIds = discardUnitArg args |> List.map (fun arg ->
            getUniqueNameInDeclarationScope ctx (arg.Name + "_mut"))
        interface ITailCallOpportunity with
            member _.Label = name
            member _.Args = argIds
            member _.IsRecursiveRef(e) =
                match e with Fable.IdentExpr id -> name = id.Name | _ -> false

    let getDecisionTarget (ctx: Context) targetIndex =
        match List.tryItem targetIndex ctx.DecisionTargets with
        | None -> failwithf "Cannot find DecisionTree target %i" targetIndex
        | Some(idents, target) -> idents, target

    let rec isStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Value _ | Fable.Import _  | Fable.IdentExpr _
        | Fable.Lambda _ | Fable.Delegate _ | Fable.ObjectExpr _
        | Fable.Call _ | Fable.CurriedApply _ | Fable.Operation _
        | Fable.Get _ | Fable.Test _ -> false

        | Fable.TypeCast(e,_) -> isStatement ctx preferStatement e

        | Fable.Set _ -> true // TODO: Depends on language target

        | Fable.TryCatch _
        | Fable.Sequential _ | Fable.Let _ | Fable.LetRec _
        | Fable.ForLoop _ | Fable.WhileLoop _ -> true

        | Fable.Extended(kind, _) ->
            match kind with
            | Fable.Throw _ // TODO: Depends on language target
            | Fable.Break _ | Fable.Debugger | Fable.Return _ -> true
            | Fable.Curry _ -> false

        // TODO: If IsSatement is false, still try to infer it? See #2414
        // /^\s*(break|continue|debugger|while|for|switch|if|try|let|const|var)\b/
        | Fable.Emit(i,_,_) -> i.IsStatement

        | Fable.DecisionTreeSuccess(targetIndex,_, _) ->
            getDecisionTarget ctx targetIndex
            |> snd |> isStatement ctx preferStatement

        // Make it also statement if we have more than, say, 3 targets?
        // That would increase the chances to convert it into a switch
        | Fable.DecisionTree(_,targets) ->
            preferStatement
            || List.exists (snd >> (isStatement ctx false)) targets

        | Fable.IfThenElse(_,thenExpr,elseExpr,_) ->
            preferStatement || isStatement ctx false thenExpr || isStatement ctx false elseExpr

//    let optimizeTailCall (com: IExtendedCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
//        let rec checkCrossRefs tempVars allArgs = function
//            | [] -> tempVars
//            | (argId, _arg)::rest ->
//                let found = allArgs |> List.exists (FableTransforms.deepExists (function
//                    | Fable.IdentExpr i -> argId = i.Name
//                    | _ -> false))
//                let tempVars =
//                    if found then
//                        let tempVarName = getUniqueNameInDeclarationScope ctx (argId + "_tmp")
//                        Map.add argId tempVarName tempVars
//                    else tempVars
//                checkCrossRefs tempVars allArgs rest
//        ctx.OptimizeTailCall()
//        let zippedArgs = List.zip tc.Args args
//        let tempVars = checkCrossRefs Map.empty args zippedArgs
//        let tempVarReplacements = tempVars |> Map.map (fun _ v -> makeIdentExpr v)
//        [|
//            // First declare temp variables
//            for (KeyValue(argId, tempVar)) in tempVars do
//                yield varDeclaration (Pattern.identifier(tempVar)) false (Expression.identifier(argId)) |> Declaration.VariableDeclaration |> Declaration
//            // Then assign argument expressions to the original argument identifiers
//            // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
//            for (argId, arg) in zippedArgs do
//                let arg = FableTransforms.replaceValues tempVarReplacements arg
//                let arg = com.TransformAsExpr(ctx, arg)
//                yield assign None (Expression.identifier(argId)) arg |> ExpressionStatement
//            yield Statement.continueStatement(Identifier.identifier(tc.Label), ?loc=range)
//        |]

//    let transformImport (com: IExtendedCompiler) ctx r (selector: string) (path: string) =
//        let selector, parts =
//            let parts = Array.toList(selector.Split('.'))
//            parts.Head, parts.Tail
//        com.GetImportExpr(ctx, selector, path, r)
//        |> getParts parts

    let transformValue (com: IExtendedCompiler) (ctx: Context) r value: Fable.ValueKind =
        match value with
        | Fable.TypeInfo t -> value // TODO: transformTypeInfo com ctx r Map.empty t
        | Fable.BaseValue _
        | Fable.ThisValue _
        | Fable.Null _
        | Fable.UnitConstant
        | Fable.BoolConstant _
        | Fable.CharConstant _
        | Fable.StringConstant _
        | Fable.NumberConstant _
        | Fable.RegexConstant _ -> value
        // TODO: Transform to array depending on language?
        | Fable.NewTuple _ -> value
        | _ -> value // TODO

//    let transformCallArgs (com: IExtendedCompiler) ctx hasSpread args =
//        match args with
//        | []
//        | [MaybeCasted(Fable.Value(Fable.UnitConstant,_))] -> []
//        | args when hasSpread ->
//            match List.rev args with
//            | [] -> []
//            | (Replacements.ArrayOrListLiteral(spreadArgs,_))::rest ->
//                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
//                rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
//            | last::rest ->
//                let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
//                rest @ [Expression.spreadElement(com.TransformAsExpr(ctx, last))]
//        | args -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args

    let resolveExpr t strategy expr: Fable.Expr =
        match strategy with
        | None | Some ReturnUnit -> expr
        | Some Return -> Fable.Extended(Fable.Return expr, expr.Range)
        | Some(Assign left) -> Fable.Set(left, Fable.ValueSet, left.Type, expr, None)
        | Some(Target left) -> Fable.Set(Fable.IdentExpr left, Fable.ValueSet, left.Type, expr, None)

//    let transformCall (com: IExtendedCompiler) ctx range callee (callInfo: Fable.CallInfo) =
//        let callee = com.TransformAsExpr(ctx, callee)
//        let args = transformCallArgs com ctx callInfo.HasSpread callInfo.Args
//        match callInfo.ThisArg with
//        | Some(TransformExpr com ctx thisArg) -> callFunction range callee (thisArg::args)
//        | None when callInfo.IsConstructor -> Expression.newExpression(callee, List.toArray args, ?loc=range)
//        | None -> callFunction range callee args

    let transformCallAsStatements com ctx range t returnStrategy callee callInfo =
        let argsLen (i: Fable.CallInfo) =
            List.length i.Args + (if Option.isSome i.ThisArg then 1 else 0)
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return|ReturnUnit), Some tc when tc.IsRecursiveRef(callee)
                                            && argsLen callInfo = List.length tc.Args ->
            let args =
                match callInfo.ThisArg with
                | Some thisArg -> thisArg::callInfo.Args
                | None -> callInfo.Args
            optimizeTailCall com ctx range tc args
        | _ ->
            [|transformCall com ctx range callee callInfo |> resolveExpr t returnStrategy|]

    let transformCurriedApplyAsStatements com ctx range t returnStrategy callee args =
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with
        | Some(Return|ReturnUnit), Some tc when tc.IsRecursiveRef(callee)
                                            && List.sameLength args tc.Args ->
            optimizeTailCall com ctx range tc args
        | _ ->
            [|transformCurriedApply com ctx range callee args |> resolveExpr t returnStrategy|]

    // When expecting a block, it's usually not necessary to wrap it
    // in a lambda to isolate its variable context
    let transformBlock (com: IExtendedCompiler) ctx ret expr: BlockStatement =
        com.TransformAsStatements(ctx, ret, expr) |> BlockStatement

    let transformTryCatch com ctx r returnStrategy (body, catch, finalizer) =
        // try .. catch statements cannot be tail call optimized
        let ctx = { ctx with TailCallOpportunity = None }
        let handler =
            catch |> Option.map (fun (param, body) ->
                CatchClause.catchClause(identAsPattern param, transformBlock com ctx returnStrategy body))
        let finalizer =
            finalizer |> Option.map (transformBlock com ctx None)
        [|Statement.tryStatement(transformBlock com ctx returnStrategy body,
            ?handler=handler, ?finalizer=finalizer, ?loc=r)|]

    let rec transformIfStatement (com: IExtendedCompiler) ctx r ret guardExpr thenStmnt elseStmnt =
        match com.TransformAsExpr(ctx, guardExpr) with
        | Literal(BooleanLiteral(value=value)) when value ->
            com.TransformAsStatements(ctx, ret, thenStmnt)
        | Literal(BooleanLiteral(value=value)) when not value ->
            com.TransformAsStatements(ctx, ret, elseStmnt)
        | guardExpr ->
            let thenStmnt = transformBlock com ctx ret thenStmnt
            match com.TransformAsStatements(ctx, ret, elseStmnt) with
            | [||] -> Statement.ifStatement(guardExpr, thenStmnt, ?loc=r)
            | [|elseStmnt|] -> Statement.ifStatement(guardExpr, thenStmnt, elseStmnt, ?loc=r)
            | statements -> Statement.ifStatement(guardExpr, thenStmnt, Statement.blockStatement(statements), ?loc=r)
            |> Array.singleton

    let transformGet (com: IExtendedCompiler) ctx range typ fableExpr kind =
        match kind with
        | Fable.ExprGet(TransformExpr com ctx prop) ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            getExpr range expr prop

        | Fable.FieldGet(fieldName,_) ->
            let fableExpr =
                match fableExpr with
                // If we're accessing a virtual member with default implementation (see #701)
                // from base class, we can use `super` in JS so we don't need the bound this arg
                | Fable.Value(Fable.BaseValue(_,t), r) -> Fable.Value(Fable.BaseValue(None, t), r)
                | _ -> fableExpr
            let expr = com.TransformAsExpr(ctx, fableExpr)
            get range expr fieldName

        | Fable.ListHead ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "head"
            libCall com ctx range "List" "head" [|com.TransformAsExpr(ctx, fableExpr)|]

        | Fable.ListTail ->
            // get range (com.TransformAsExpr(ctx, fableExpr)) "tail"
            libCall com ctx range "List" "tail" [|com.TransformAsExpr(ctx, fableExpr)|]

        | Fable.TupleIndex index ->
            match fableExpr with
            // TODO: Check the erased expressions don't have side effects?
            | Fable.Value(Fable.NewTuple(exprs,_), _) ->
                com.TransformAsExpr(ctx, List.item index exprs)
            | TransformExpr com ctx expr -> getExpr range expr (ofInt index)

        | Fable.OptionValue ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            if mustWrapOption typ || com.Options.Language = TypeScript
            then libCall com ctx None "Option" "value" [|expr|]
            else expr

        | Fable.UnionTag ->
            getUnionExprTag com ctx range fableExpr

        | Fable.UnionField(_, fieldIndex) ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            getExpr range (getExpr None expr (Expression.stringLiteral("fields"))) (ofInt fieldIndex)

    let transformSet (com: IExtendedCompiler) ctx range fableExpr typ (value: Fable.Expr) kind =
        let expr = com.TransformAsExpr(ctx, fableExpr)
        let value = com.TransformAsExpr(ctx, value) |> wrapIntExpression typ
        let ret =
            match kind with
            | Fable.ValueSet -> expr
            | Fable.ExprSet(TransformExpr com ctx e) -> getExpr None expr e
            | Fable.FieldSet(fieldName) -> get None expr fieldName
        assign range ret value

    let transformBindingExprBody (com: IExtendedCompiler) (ctx: Context) (var: Fable.Ident) (value: Fable.Expr) =
        match value with
        | Function(args, body) ->
            let name = Some var.Name
            transformFunctionWithAnnotations com ctx name args body
            |> makeArrowFunctionExpression name
        | _ ->
            if var.IsMutable then
                com.TransformAsExpr(ctx, value)
            else
                com.TransformAsExpr(ctx, value) |> wrapIntExpression value.Type

    let transformBindingAsExpr (com: IExtendedCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        transformBindingExprBody com ctx var value
        |> assign None (identAsExpr var)

    let transformBindingAsStatements (com: IExtendedCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if isStatement ctx false value then
            let varPattern, varExpr = identAsPattern var, identAsExpr var
            let decl = Statement.variableDeclaration(varPattern)
            let body = com.TransformAsStatements(ctx, Some(Assign varExpr), value)
            Array.append [|decl|] body
        else
            let value = transformBindingExprBody com ctx var value
            let decl = varDeclaration (identAsPattern var) var.IsMutable value |> Declaration.VariableDeclaration |> Declaration
            [|decl|]

    let transformTest (com: IExtendedCompiler) ctx range kind expr: Expression =
        match kind with
        | Fable.TypeTest t ->
            transformTypeTest com ctx range expr t
        | Fable.OptionTest nonEmpty ->
            let op = if nonEmpty then BinaryUnequal else BinaryEqual
            Expression.binaryExpression(op, com.TransformAsExpr(ctx, expr), Expression.nullLiteral(), ?loc=range)
        | Fable.ListTest nonEmpty ->
            let expr = com.TransformAsExpr(ctx, expr)
            // let op = if nonEmpty then BinaryUnequal else BinaryEqual
            // Expression.binaryExpression(op, get None expr "tail", Expression.nullLiteral(), ?loc=range)
            let expr = libCall com ctx range "List" "isEmpty" [|expr|]
            if nonEmpty then Expression.unaryExpression(UnaryNot, expr, ?loc=range) else expr
        | Fable.UnionCaseTest tag ->
            let expected = ofInt tag
            let actual = getUnionExprTag com ctx None expr
            Expression.binaryExpression(BinaryEqualStrict, actual, expected, ?loc=range)

    let transformSwitch (com: IExtendedCompiler) ctx useBlocks returnStrategy evalExpr cases defaultCase: Statement =
        let consequent caseBody =
            if useBlocks then [|Statement.blockStatement(caseBody)|] else caseBody
        let cases =
            cases |> List.collect (fun (guards, expr) ->
                // Remove empty branches
                match returnStrategy, expr, guards with
                | None, Fable.Value(Fable.UnitConstant,_), _
                | _, _, [] -> []
                | _, _, guards ->
                    let guards, lastGuard = List.splitLast guards
                    let guards = guards |> List.map (fun e -> SwitchCase.switchCase([||], com.TransformAsExpr(ctx, e)))
                    let caseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                    let caseBody =
                        match returnStrategy with
                        | Some Return -> caseBody
                        | _ -> Array.append caseBody [|Statement.breakStatement()|]
                    guards @ [SwitchCase.switchCase(consequent caseBody, com.TransformAsExpr(ctx, lastGuard))]
                )
        let cases =
            match defaultCase with
            | Some expr ->
                let defaultCaseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                cases @ [SwitchCase.switchCase(consequent defaultCaseBody)]
            | None -> cases
        Statement.switchStatement(com.TransformAsExpr(ctx, evalExpr), List.toArray cases)

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then []
        elif List.sameLength idents values then List.zip idents values
        else failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues (com: IExtendedCompiler) (ctx: Context) targetIndex boundValues =
        let idents, target = getDecisionTarget ctx targetIndex
        let identsAndValues = matchTargetIdentAndValues idents boundValues
        if not com.Options.DebugMode then
            let bindings, replacements =
                (([], Map.empty), identsAndValues)
                ||> List.fold (fun (bindings, replacements) (ident, expr) ->
                    if canHaveSideEffects expr then
                        (ident, expr)::bindings, replacements
                    else
                        bindings, Map.add ident.Name expr replacements)
            let target = FableTransforms.replaceValues replacements target
            List.rev bindings, target
        else
            identsAndValues, target

    let transformDecisionTreeSuccessAsExpr (com: IExtendedCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings ->
            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
            com.TransformAsExpr(ctx, target)

    let transformDecisionTreeSuccessAsStatements (com: IExtendedCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Statement[] =
        match returnStrategy with
        | Some(Target targetId) as target ->
            let idents, _ = getDecisionTarget ctx targetIndex
            let assignments =
                matchTargetIdentAndValues idents boundValues
                |> List.mapToArray (fun (id, TransformExpr com ctx value) ->
                    assign None (identAsExpr id) value |> ExpressionStatement)
            let targetAssignment = assign None (targetId |> Expression.Identifier) (ofInt targetIndex) |> ExpressionStatement
            Array.append [|targetAssignment|] assignments
        | ret ->
            let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
            Array.append bindings (com.TransformAsStatements(ctx, ret, target))

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) = function
            | Fable.Operation(Fable.Binary(BinaryEqualStrict, expr, right), _, _) ->
                Some(expr, right)
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, Fable.Number(Int32, None), None)
                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None
        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1,Fable.UnionTag,_,_), Fable.Get(Fable.IdentExpr i2,Fable.UnionTag,_,_) ->
                i1.Name = i2.Name
            | _ -> false
        let rec checkInner cases evalExpr = function
            | Fable.IfThenElse(Equals(evalExpr2, caseExpr),
                               Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _)
                                    when sameEvalExprs evalExpr evalExpr2 ->
                match treeExpr with
                | Fable.DecisionTreeSuccess(defaultTargetIndex, defaultBoundValues, _) ->
                    let cases = (caseExpr, targetIndex, boundValues)::cases |> List.rev
                    Some(evalExpr, cases, (defaultTargetIndex, defaultBoundValues))
                | treeExpr -> checkInner ((caseExpr, targetIndex, boundValues)::cases) evalExpr treeExpr
            | _ -> None
        match expr with
        | Fable.IfThenElse(Equals(evalExpr, caseExpr),
                           Fable.DecisionTreeSuccess(targetIndex, boundValues, _), treeExpr, _) ->
            match checkInner [caseExpr, targetIndex, boundValues] evalExpr treeExpr with
            | Some(evalExpr, cases, defaultCase) ->
                Some(evalExpr, cases, defaultCase)
            | None -> None
        | _ -> None

    let transformDecisionTreeAsExpr (com: IExtendedCompiler) (ctx: Context) targets expr: Expression =
        // TODO: Check if some targets are referenced multiple times
        let ctx = { ctx with DecisionTargets = targets }
        com.TransformAsExpr(ctx, expr)

    let groupSwitchCases t (cases: (Fable.Expr * int * Fable.Expr list) list) (defaultIndex, defaultBoundValues) =
        cases
        |> List.groupBy (fun (_,idx,boundValues) ->
            // Try to group cases with some target index and empty bound values
            // If bound values are non-empty use also a non-empty Guid to prevent grouping
            if List.isEmpty boundValues
            then idx, System.Guid.Empty
            else idx, System.Guid.NewGuid())
        |> List.map (fun ((idx,_), cases) ->
            let caseExprs = cases |> List.map Tuple3.item1
            // If there are multiple cases, it means boundValues are empty
            // (see `groupBy` above), so it doesn't mind which one we take as reference
            let boundValues = cases |> List.head |> Tuple3.item3
            caseExprs, Fable.DecisionTreeSuccess(idx, boundValues, t))
        |> function
            | [] -> []
            // Check if the last case can also be grouped with the default branch, see #2357
            | cases when List.isEmpty defaultBoundValues ->
                match List.splitLast cases with
                | cases, (_, Fable.DecisionTreeSuccess(idx, [], _))
                    when idx = defaultIndex -> cases
                | _ -> cases
            | cases -> cases

    let getTargetsWithMultipleReferences expr =
        let rec findSuccess (targetRefs: Map<int,int>) = function
            | [] -> targetRefs
            | expr::exprs ->
                match expr with
                // We shouldn't actually see this, but shortcircuit just in case
                | Fable.DecisionTree _ ->
                    findSuccess targetRefs exprs
                | Fable.DecisionTreeSuccess(idx,_,_) ->
                    let count =
                        Map.tryFind idx targetRefs
                        |> Option.defaultValue 0
                    let targetRefs = Map.add idx (count + 1) targetRefs
                    findSuccess targetRefs exprs
                | expr ->
                    let exprs2 = FableTransforms.getSubExpressions expr
                    findSuccess targetRefs (exprs @ exprs2)
        findSuccess Map.empty [expr] |> Seq.choose (fun kv ->
            if kv.Value > 1 then Some kv.Key else None) |> Seq.toList

    /// When several branches share target create first a switch to get the target index and bind value
    /// and another to execute the actual target
    let transformDecisionTreeWithTwoSwitches (com: IExtendedCompiler) ctx returnStrategy
                    (targets: (Fable.Ident list * Fable.Expr) list) treeExpr =
        // Declare target and bound idents
        let targetId = getUniqueNameInDeclarationScope ctx "pattern_matching_result" |> makeIdent
        let multiVarDecl =
            let boundIdents = targets |> List.collect (fun (idents,_) ->
                idents |> List.map (fun id -> typedIdent com ctx id, None))
            multiVarDeclaration Let ((typedIdent com ctx targetId, None)::boundIdents)
        // Transform targets as switch
        let switch2 =
            // TODO: Declare the last case as the default case?
            let cases = targets |> List.mapi (fun i (_,target) -> [makeIntConst i], target)
            transformSwitch com ctx true returnStrategy (targetId |> Fable.IdentExpr) cases None
        // Transform decision tree
        let targetAssign = Target(ident targetId)
        let ctx = { ctx with DecisionTargets = targets }
        match transformDecisionTreeAsSwitch treeExpr with
        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
            let cases = groupSwitchCases (Fable.Number(Int32, None)) cases (defaultIndex, defaultBoundValues)
            let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number(Int32, None))
            let switch1 = transformSwitch com ctx false (Some targetAssign) evalExpr cases (Some defaultCase)
            [|multiVarDecl; switch1; switch2|]
        | None ->
            let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
            [| yield multiVarDecl; yield! decisionTree; yield switch2 |]

    let transformDecisionTreeAsStatements (com: IExtendedCompiler) (ctx: Context) returnStrategy
                        (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Statement[] =
        // If some targets are referenced multiple times, hoist bound idents,
        // resolve the decision index and compile the targets as a switch
        let targetsWithMultiRefs =
            if com.Options.Language = TypeScript then [] // no hoisting when compiled with types
            else getTargetsWithMultipleReferences treeExpr
        match targetsWithMultiRefs with
        | [] ->
            let ctx = { ctx with DecisionTargets = targets }
            match transformDecisionTreeAsSwitch treeExpr with
            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                let t = treeExpr.Type
                let cases = cases |> List.map (fun (caseExpr, targetIndex, boundValues) ->
                    [caseExpr], Fable.DecisionTreeSuccess(targetIndex, boundValues, t))
                let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)
                [|transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase)|]
            | None ->
                com.TransformAsStatements(ctx, returnStrategy, treeExpr)
        | targetsWithMultiRefs ->
            // If the bound idents are not referenced in the target, remove them
            let targets =
                targets |> List.map (fun (idents, expr) ->
                    idents
                    |> List.exists (fun i -> FableTransforms.isIdentUsed i.Name expr)
                    |> function
                        | true -> idents, expr
                        | false -> [], expr)
            let hasAnyTargetWithMultiRefsBoundValues =
                targetsWithMultiRefs |> List.exists (fun idx ->
                    targets.[idx] |> fst |> List.isEmpty |> not)
            if not hasAnyTargetWithMultiRefsBoundValues then
                match transformDecisionTreeAsSwitch treeExpr with
                | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
                    let t = treeExpr.Type
                    let cases = groupSwitchCases t cases (defaultIndex, defaultBoundValues)
                    let ctx = { ctx with DecisionTargets = targets }
                    let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, t)
                    [|transformSwitch com ctx true returnStrategy evalExpr cases (Some defaultCase)|]
                | None ->
                    transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr
            else
                transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr

    let rec transformAsExpr (com: IExtendedCompiler) ctx (expr: Fable.Expr): Expression =
        match expr with
        | Fable.TypeCast(e, t) -> transformCast com ctx t e

        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.IdentExpr id -> identAsExpr id

        | Fable.Import({ Selector = selector; Path = path }, _, r) ->
            transformImport com ctx r selector path

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Lambda(arg, body, name) ->
            transformFunctionWithAnnotations com ctx name [arg] body
            |> makeArrowFunctionExpression name

        | Fable.Delegate(args, body, name) ->
            transformFunctionWithAnnotations com ctx name args body
            |> makeArrowFunctionExpression name

        | Fable.ObjectExpr (members, _, baseCall) ->
           transformObjectExpr com ctx members baseCall

        | Fable.Call(callee, info, _, range) ->
            transformCall com ctx range callee info

        | Fable.CurriedApply(callee, args, _, range) ->
            transformCurriedApply com ctx range callee args

        | Fable.Operation(kind, _, range) ->
            transformOperation com ctx range kind

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.IfThenElse(TransformExpr com ctx guardExpr,
                           TransformExpr com ctx thenExpr,
                           TransformExpr com ctx elseExpr, r) ->
            Expression.conditionalExpression(guardExpr, thenExpr, elseExpr, ?loc=r)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        | Fable.Set(expr, kind, typ, value, range) ->
            transformSet com ctx range expr typ value kind

        | Fable.Let(ident, value, body) ->
            if ctx.HoistVars [ident] then
                let assignment = transformBindingAsExpr com ctx ident value
                Expression.sequenceExpression([|assignment; com.TransformAsExpr(ctx, body)|])
            else iife com ctx expr

        | Fable.LetRec(bindings, body) ->
            if ctx.HoistVars(List.map fst bindings) then
                let values = bindings |> List.mapToArray (fun (id, value) ->
                    transformBindingAsExpr com ctx id value)
                Expression.sequenceExpression(Array.append values [|com.TransformAsExpr(ctx, body)|])
            else iife com ctx expr

        | Fable.Sequential exprs ->
            List.mapToArray (fun e -> com.TransformAsExpr(ctx, e)) exprs
            |> Expression.sequenceExpression

        | Fable.Emit(info, _, range) ->
            if info.IsStatement then iife com ctx expr
            else transformEmit com ctx range info

        // These cannot appear in expression position in JS, must be wrapped in a lambda
        | Fable.WhileLoop _ | Fable.ForLoop _ | Fable.TryCatch _ -> iife com ctx expr

        | Fable.Extended(instruction, _) ->
            match instruction with
            | Fable.Curry(e, arity) -> transformCurry com ctx e arity
            // TODO: Throw may be valid in expression position depending on the language
            | Fable.Throw _ | Fable.Break _ | Fable.Debugger -> iife com ctx expr

    let rec transformAsStatements (com: IExtendedCompiler) ctx returnStrategy (expr: Fable.Expr): Fable.Expr list =
        match expr with
        | Fable.Extended(kind, r) ->
            match kind with
            | Fable.Curry(e, arity) -> transformCurry com ctx e arity |> resolveExpr e.Type returnStrategy
            | Fable.Throw _
            | Fable.Return _
            | Fable.Debugger
            | Fable.Break _ -> expr
            |> List.singleton

        | Fable.TypeCast(e, t) ->
            [transformCast com ctx t e |> resolveExpr t returnStrategy]

        | Fable.Value(kind, r) ->
            [|transformValue com ctx r kind |> resolveExpr kind.Type returnStrategy|]

        | Fable.IdentExpr id ->
            [|identAsExpr id |> resolveExpr id.Type returnStrategy|]

        | Fable.Import({ Selector = selector; Path = path }, t, r) ->
            [|transformImport com ctx r selector path |> resolveExpr t returnStrategy|]

        | Fable.Test(expr, kind, range) ->
            [|transformTest com ctx range kind expr |> resolveExpr Fable.Boolean returnStrategy|]

        | Fable.Lambda(arg, body, name) ->
            [|transformFunctionWithAnnotations com ctx name [arg] body
                |> makeArrowFunctionExpression name
                |> resolveExpr expr.Type returnStrategy|]

        | Fable.Delegate(args, body, name) ->
            [|transformFunctionWithAnnotations com ctx name args body
                |> makeArrowFunctionExpression name
                |> resolveExpr expr.Type returnStrategy|]

        | Fable.ObjectExpr (members, t, baseCall) ->
            [|transformObjectExpr com ctx members baseCall |> resolveExpr t returnStrategy|]

        | Fable.Call(callee, info, typ, range) ->
            transformCallAsStatements com ctx range typ returnStrategy callee info

        | Fable.CurriedApply(callee, args, typ, range) ->
            transformCurriedApplyAsStatements com ctx range typ returnStrategy callee args

        | Fable.Emit(info, t, range) ->
            let e = transformEmit com ctx range info
            if info.IsStatement then
                [|ExpressionStatement(e)|] // Ignore the return strategy
            else [|resolveExpr t returnStrategy e|]

        | Fable.Operation(kind, t, range) ->
            [|transformOperation com ctx range kind |> resolveExpr t returnStrategy|]

        | Fable.Get(expr, kind, t, range) ->
            [|transformGet com ctx range t expr kind |> resolveExpr t returnStrategy|]

        | Fable.Let(ident, value, body) ->
            let binding = transformBindingAsStatements com ctx ident value
            Array.append binding (transformAsStatements com ctx returnStrategy body)

        | Fable.LetRec(bindings, body) ->
            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
            Array.append bindings (transformAsStatements com ctx returnStrategy body)

        | Fable.Set(expr, kind, typ, value, range) ->
            [|transformSet com ctx range expr typ value kind |> resolveExpr expr.Type returnStrategy|]

        | Fable.IfThenElse(guardExpr, thenExpr, elseExpr, r) ->
            let asStatement =
                match returnStrategy with
                | None | Some ReturnUnit -> true
                | Some(Target _) -> true // Compile as statement so values can be bound
                | Some(Assign _) -> (isStatement ctx false thenExpr) || (isStatement ctx false elseExpr)
                | Some Return ->
                    Option.isSome ctx.TailCallOpportunity
                    || (isStatement ctx false thenExpr) || (isStatement ctx false elseExpr)
            if asStatement then
                transformIfStatement com ctx r returnStrategy guardExpr thenExpr elseExpr
            else
                let guardExpr' = transformAsExpr com ctx guardExpr
                let thenExpr' = transformAsExpr com ctx thenExpr
                let elseExpr' = transformAsExpr com ctx elseExpr
                [|Expression.conditionalExpression(guardExpr', thenExpr', elseExpr', ?loc=r) |> resolveExpr thenExpr.Type returnStrategy|]

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1
            statements |> List.mapiToArray (fun i statement ->
                let ret = if i < lasti then None else returnStrategy
                com.TransformAsStatements(ctx, ret, statement))
            |> Array.concat

        | Fable.TryCatch (body, catch, finalizer, r) ->
            transformTryCatch com ctx r returnStrategy (body, catch, finalizer)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsStatements com ctx returnStrategy targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsStatements com ctx returnStrategy idx boundValues

        | Fable.WhileLoop(TransformExpr com ctx guard, body, label, range) ->
            let whileLoop = Statement.whileStatement(guard, transformBlock com ctx None body, ?loc=range)
            match label with
            | Some label -> [|Statement.labeledStatement(Identifier.identifier(label), whileLoop)|]
            | None -> [|whileLoop|]

        | Fable.ForLoop (var, TransformExpr com ctx start, TransformExpr com ctx limit, body, isUp, range) ->
            let op1, op2 =
                if isUp
                then BinaryOperator.BinaryLessOrEqual, UpdateOperator.UpdatePlus
                else BinaryOperator.BinaryGreaterOrEqual, UpdateOperator.UpdateMinus

            let a = start |> varDeclaration (typedIdent com ctx var |> Pattern.Identifier) true

            [|Statement.forStatement(
                transformBlock com ctx None body,
                start |> varDeclaration (typedIdent com ctx var |> Pattern.Identifier) true,
                Expression.binaryExpression(op1, identAsExpr var, limit),
                Expression.updateExpression(op2, false, identAsExpr var), ?loc=range)|]

    let transformFunction com ctx name (args: Fable.Ident list) (body: Fable.Expr): Pattern array * BlockStatement =
        let tailcallChance =
            Option.map (fun name ->
                NamedTailCallOpportunity(com, ctx, name, args) :> ITailCallOpportunity) name
        let args = discardUnitArg args
        let declaredVars = ResizeArray()
        let mutable isTailCallOptimized = false
        let ctx =
            { ctx with TailCallOpportunity = tailcallChance
                       HoistVars = fun ids -> declaredVars.AddRange(ids); true
                       OptimizeTailCall = fun () -> isTailCallOptimized <- true }
        let body =
            if body.Type = Fable.Unit then
                transformBlock com ctx (Some ReturnUnit) body
            elif isStatement ctx (Option.isSome tailcallChance) body then
                transformBlock com ctx (Some Return) body
            else
                transformAsExpr com ctx body |> wrapExprInBlockWithReturn
        let args, body =
            match isTailCallOptimized, tailcallChance with
            | true, Some tc ->
                // Replace args, see NamedTailCallOpportunity constructor
                let args' =
                    List.zip args tc.Args
                    |> List.map (fun (id, tcArg) ->
                        makeTypedIdent id.Type tcArg |> typedIdent com ctx)
                let varDecls =
                    List.zip args tc.Args
                    |> List.map (fun (id, tcArg) ->
                        id |> typedIdent com ctx, Some (Expression.identifier(tcArg)))
                    |> multiVarDeclaration Const

                let body = Array.append [|varDecls|] body.Body
                // Make sure we don't get trapped in an infinite loop, see #1624
                let body = BlockStatement(Array.append body [|Statement.breakStatement()|])
                args', Statement.labeledStatement(Identifier.identifier(tc.Label), Statement.whileStatement(Expression.booleanLiteral(true), body))
                |> Array.singleton |> BlockStatement
            | _ -> args |> List.map (typedIdent com ctx), body
        let body =
            if declaredVars.Count = 0 then body
            else
                let varDeclStatement = multiVarDeclaration Let [for v in declaredVars -> typedIdent com ctx v, None]
                BlockStatement(Array.append [|varDeclStatement|] body.Body)
        args |> List.mapToArray Pattern.Identifier, body

    let declareEntryPoint _com _ctx (funcExpr: Expression) =
        let argv = emitExpression None "typeof process === 'object' ? process.argv.slice(2) : []" []
        let main = Expression.callExpression(funcExpr, [|argv|])
        // Don't exit the process after leaving main, as there may be a server running
        // ExpressionStatement(emitExpression funcExpr.loc "process.exit($0)" [main], ?loc=funcExpr.loc)
        PrivateModuleDeclaration(ExpressionStatement(main))

    let declareModuleMember isPublic membName isMutable (expr: Expression) =
        let membName' = Pattern.identifier(membName)
        let membName = Identifier.identifier(membName)
        let decl: Declaration =
            match expr with
            | ClassExpression(body, id, superClass, implements, superTypeParameters, typeParameters, loc) ->
                Declaration.classDeclaration(
                    body,
                    ?id = Some membName,
                    ?superClass = superClass,
                    ?superTypeParameters = superTypeParameters,
                    ?typeParameters = typeParameters,
                    ?implements = implements)
            | FunctionExpression(_, ``params``, body, returnType, typeParameters, _) ->
                Declaration.functionDeclaration(
                    ``params``, body, membName,
                    ?returnType = returnType,
                    ?typeParameters = typeParameters)
            | _ -> varDeclaration membName' isMutable expr |> Declaration.VariableDeclaration
        if not isPublic then PrivateModuleDeclaration(decl |> Declaration)
        else ExportNamedDeclaration(decl)

    let makeEntityTypeParamDecl (com: IExtendedCompiler) _ctx (ent: Fable.Entity) =
        if com.Options.Language = TypeScript then
            getEntityGenParams ent |> makeTypeParamDecl
        else
            None

    let getClassImplements com ctx (ent: Fable.Entity) =
        let mkNative genArgs typeName =
            let id = Identifier.identifier(typeName)
            let typeParamInst = makeGenTypeParamInst com ctx genArgs
            ClassImplements.classImplements(id, ?typeParameters=typeParamInst) |> Some
//        let mkImport genArgs moduleName typeName =
//            let id = makeImportTypeId com ctx moduleName typeName
//            let typeParamInst = makeGenTypeParamInst com ctx genArgs
//            ClassImplements(id, ?typeParameters=typeParamInst) |> Some
        ent.AllInterfaces |> Seq.choose (fun ifc ->
            match ifc.Entity.FullName with
            | "Fable.Core.JS.Set`1" -> mkNative ifc.GenericArgs "Set"
            | "Fable.Core.JS.Map`2" -> mkNative ifc.GenericArgs "Map"
            | _ -> None
        )

    let getUnionFieldsAsIdents (_com: IExtendedCompiler) _ctx (_ent: Fable.Entity) =
        let tagId = makeTypedIdent (Fable.Number(Int32, None)) "tag"
        let fieldsId = makeTypedIdent (Fable.Array Fable.Any) "fields"
        [| tagId; fieldsId |]

    let getEntityFieldsAsIdents _com (ent: Fable.Entity) =
        ent.FSharpFields
        |> Seq.map (fun field ->
            let name = field.Name |> Naming.sanitizeIdentForbiddenChars |> Naming.checkJsKeywords
            let typ = field.FieldType
            let id: Fable.Ident = { makeTypedIdent typ name with IsMutable = field.IsMutable }
            id)
        |> Seq.toArray

    let getEntityFieldsAsProps (com: IExtendedCompiler) ctx (ent: Fable.Entity) =
        if ent.IsFSharpUnion then
            getUnionFieldsAsIdents com ctx ent
            |> Array.map (fun id ->
                let prop = identAsExpr id
                let ta = typeAnnotation com ctx id.Type
                ObjectTypeProperty.objectTypeProperty(prop, ta))
        else
            ent.FSharpFields
            |> Seq.map (fun field ->
                let prop, computed = memberFromName field.Name
                let ta = typeAnnotation com ctx field.FieldType
                let isStatic = if field.IsStatic then Some true else None
                ObjectTypeProperty.objectTypeProperty(prop, ta, computed_=computed, ?``static``=isStatic))
            |> Seq.toArray

    let declareClassType (com: IExtendedCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) (baseExpr: Expression option) classMembers =
        let typeParamDecl = makeEntityTypeParamDecl com ctx ent
        let implements =
            if com.Options.Language = TypeScript then
                let implements = Util.getClassImplements com ctx ent |> Seq.toArray
                if Array.isEmpty implements then None else Some implements
            else None
        let classCons = makeClassConstructor consArgs consBody
        let classFields =
            if com.Options.Language = TypeScript then
                getEntityFieldsAsProps com ctx ent
                |> Array.map (fun (ObjectTypeProperty(key, value, _, _, ``static``, _, _, _)) ->
                    let ta = value |> TypeAnnotation |> Some
                    ClassMember.classProperty(key, ``static``=``static``, ?typeAnnotation=ta))
            else Array.empty
        let classMembers = Array.append [| classCons |] classMembers
        let classBody = ClassBody.classBody([| yield! classFields; yield! classMembers |])
        let classExpr = Expression.classExpression(classBody, ?superClass=baseExpr, ?typeParameters=typeParamDecl, ?implements=implements)
        classExpr |> declareModuleMember ent.IsPublic entName false

    let declareType (com: IExtendedCompiler) ctx (ent: Fable.Entity) entName (consArgs: Pattern[]) (consBody: BlockStatement) baseExpr classMembers: ModuleDeclaration list =
        let typeDeclaration = declareClassType com ctx ent entName consArgs consBody baseExpr classMembers
        let reflectionDeclaration =
            let genArgs = Array.init (ent.GenericParameters.Length) (fun i -> "gen" + string i |> makeIdent)
            let generics = genArgs |> Array.map identAsExpr
            let body = transformReflectionInfo com ctx None ent generics
            let args = genArgs |> Array.map (fun x -> Pattern.identifier(x.Name, ?typeAnnotation=ta))
            makeFunctionExpression None (args, body, returnType, None)
            |> declareModuleMember ent.IsPublic (entName + Naming.reflectionSuffix) false
        [typeDeclaration; reflectionDeclaration]

    let transformModuleFunction (com: IExtendedCompiler) ctx (info: Fable.MemberInfo) (membName: string) args body =
        let args, body, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx (NonAttached membName) info.HasSpread args body
        let expr = Expression.functionExpression(args, body, ?returnType=returnType, ?typeParameters=typeParamDecl)
        info.Attributes
        |> Seq.exists (fun att -> att.Entity.FullName = Atts.entryPoint)
        |> function
        | true -> declareEntryPoint com ctx expr
        | false -> declareModuleMember info.IsPublic membName false expr

    let transformAction (com: IExtendedCompiler) ctx expr =
        let statements = transformAsStatements com ctx None expr
        // TODO: If one of the statements is a let binding (variable declaration)
        // we need to put them in their own scope (iife)
//        let hasVarDeclarations =
//            statements |> Array.exists (function
//                | Declaration(Declaration.VariableDeclaration(_)) -> true
//                | _ -> false)
        statements

    let transformAttachedProperty (com: IExtendedCompiler) ctx (memb: Fable.MemberDecl) =
        let isStatic = not memb.Info.IsInstance
        let kind = if memb.Info.IsGetter then ClassGetter else ClassSetter
        let args, body, _returnType, _typeParamDecl =
            getMemberArgsAndBody com ctx (Attached isStatic) false memb.Args memb.Body
        let key, computed = memberFromName memb.Name
        ClassMember.classMethod(kind, key, args, body, computed_=computed, ``static``=isStatic)
        |> Array.singleton

    let transformAttachedMethod (com: IExtendedCompiler) ctx (memb: Fable.MemberDecl) =
        let isStatic = not memb.Info.IsInstance
        let makeMethod name args body =
            let key, computed = memberFromName name
            ClassMember.classMethod(ClassFunction, key, args, body, computed_=computed, ``static``=isStatic)
        let args, body, _returnType, _typeParamDecl =
            getMemberArgsAndBody com ctx (Attached isStatic) memb.Info.HasSpread memb.Args memb.Body
        [|
            yield makeMethod memb.Name args body
            if memb.Info.IsEnumerator then
                yield makeMethod "Symbol.iterator" [||] (enumerator2iterator com ctx)
        |]

    let transformUnion (com: IExtendedCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getUnionFieldsAsIdents com ctx ent
        let args =
            [| typedIdent com ctx fieldIds.[0] |> Pattern.Identifier
               typedIdent com ctx fieldIds.[1] |> Pattern.Identifier |> restElement |]
        let body =
            BlockStatement([|
                yield callSuperAsStatement []
                yield! fieldIds |> Array.map (fun id ->
                    let left = get None thisExpr id.Name
                    let right =
                        match id.Type with
                        | Fable.Number _ ->
                            Expression.binaryExpression(BinaryOrBitwise, identAsExpr id, Expression.numericLiteral(0.))
                        | _ -> identAsExpr id
                    assign None left right |> ExpressionStatement)
            |])
        let cases =
            let body =
                ent.UnionCases
                |> Seq.map (getUnionCaseName >> makeStrConst)
                |> Seq.toList
                |> makeArray com ctx
                |>  Statement.returnStatement
                |> Array.singleton
                |> BlockStatement
            ClassMember.classMethod(ClassFunction, Expression.identifier("cases"), [||], body)

        let baseExpr = libValue com ctx "Types" "Union" |> Some
        let classMembers = Array.append [|cases|] classMembers
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithCompilerGeneratedConstructor (com: IExtendedCompiler) ctx (ent: Fable.Entity) (entName: string) classMembers =
        let fieldIds = getEntityFieldsAsIdents com ent
        let args = fieldIds |> Array.map identAsExpr
        let baseExpr =
            if ent.IsFSharpExceptionDeclaration
            then libValue com ctx "Types" "FSharpException" |> Some
            elif ent.IsFSharpRecord || ent.IsValueType
            then libValue com ctx "Types" "Record" |> Some
            else None
        let body =
            BlockStatement([|
                if Option.isSome baseExpr then
                    yield callSuperAsStatement []
                yield! ent.FSharpFields |> Seq.mapi (fun i field ->
                    let left = get None thisExpr field.Name
                    let right = wrapIntExpression field.FieldType args.[i]
                    assign None left right |> ExpressionStatement)
                |> Seq.toArray
            |])
        let typedPattern x = typedIdent com ctx x
        let args = fieldIds |> Array.map (typedPattern >> Pattern.Identifier)
        declareType com ctx ent entName args body baseExpr classMembers

    let transformClassWithImplicitConstructor (com: IExtendedCompiler) ctx (classDecl: Fable.ClassDecl) classMembers (cons: Fable.MemberDecl) =
        let classEnt = com.GetEntity(classDecl.Entity)
        let classIdent = Expression.identifier(classDecl.Name)
        let consArgs, consBody, returnType, typeParamDecl =
            getMemberArgsAndBody com ctx ClassConstructor cons.Info.HasSpread cons.Args cons.Body

        let returnType, typeParamDecl =
            // change constructor's return type from void to entity type
            if com.Options.Language = TypeScript then
                let genParams = getEntityGenParams classEnt
                let returnType = getGenericTypeAnnotation com ctx classDecl.Name genParams
                let typeParamDecl = makeTypeParamDecl genParams |> mergeTypeParamDecls typeParamDecl
                returnType, typeParamDecl
            else
                returnType, typeParamDecl

        let exposedCons =
            let argExprs = consArgs |> Array.map (fun p -> Expression.identifier(p.Name))
            let exposedConsBody = Expression.newExpression(classIdent, argExprs)
            makeFunctionExpression None (consArgs, exposedConsBody, returnType, typeParamDecl)

        let baseExpr, consBody =
            classDecl.BaseCall
            |> extractBaseExprFromBaseCall com ctx classEnt.BaseType
            |> Option.orElseWith (fun () ->
                if classEnt.IsValueType then Some(libValue com ctx "Types" "Record", [])
                else None)
            |> Option.map (fun (baseExpr, baseArgs) ->
                let consBody =
                    consBody.Body
                    |> Array.append [|callSuperAsStatement baseArgs|]
                    |> BlockStatement
                Some baseExpr, consBody)
            |> Option.defaultValue (None, consBody)

        [
            yield! declareType com ctx classEnt classDecl.Name consArgs consBody baseExpr classMembers
            yield declareModuleMember cons.Info.IsPublic cons.Name false exposedCons
        ]

    let rec transformDeclaration (com: IExtendedCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members |> List.collect (transformDeclaration com ctx)

        | Fable.ActionDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                transformAction com ctx decl.Body

        | Fable.MemberDeclaration decl ->
            withCurrentScope ctx decl.UsedNames <| fun ctx ->
                if decl.Info.IsValue then
                    let value = transformAsExpr com ctx decl.Body
                    [declareModuleMember decl.Info.IsPublic decl.Name decl.Info.IsMutable value]
                else
                    [transformModuleFunction com ctx decl.Info decl.Name decl.Args decl.Body]

        | Fable.ClassDeclaration decl ->
            let ent = decl.Entity

            let classMembers =
                decl.AttachedMembers
                |> List.toArray
                |> Array.collect (fun memb ->
                    withCurrentScope ctx memb.UsedNames <| fun ctx ->
                        if memb.Info.IsGetter || memb.Info.IsSetter then
                            transformAttachedProperty com ctx memb
                        else
                            transformAttachedMethod com ctx memb)

            match decl.Constructor with
            | Some cons ->
                withCurrentScope ctx cons.UsedNames <| fun ctx ->
                    transformClassWithImplicitConstructor com ctx decl classMembers cons
            | None ->
                let ent = com.GetEntity(ent)
                if ent.IsFSharpUnion then transformUnion com ctx ent decl.Name classMembers
                else transformClassWithCompilerGeneratedConstructor com ctx ent decl.Name classMembers

    let transformImports (imports: Import seq): ModuleDeclaration list =
        let statefulImports = ResizeArray()
        imports |> Seq.map (fun import ->
            let specifier =
                import.LocalIdent
                |> Option.map (fun localId ->
                    let localId = Identifier.identifier(localId)
                    match import.Selector with
                    | "*" -> ImportNamespaceSpecifier(localId)
                    | "default" | "" -> ImportDefaultSpecifier(localId)
                    | memb -> ImportMemberSpecifier(localId, Identifier.identifier(memb)))
            import.Path, specifier)
        |> Seq.groupBy fst
        |> Seq.collect (fun (path, specifiers) ->
            let mems, defs, alls =
                (([], [], []), Seq.choose snd specifiers)
                ||> Seq.fold (fun (mems, defs, alls) x ->
                    match x with
                    | ImportNamespaceSpecifier(_) -> mems, defs, x::alls
                    | ImportDefaultSpecifier(_) -> mems, x::defs, alls
                    | _ -> x::mems, defs, alls)
            // We used to have trouble when mixing member, default and namespace imports,
            // issue an import statement for each kind just in case
            [mems; defs; alls] |> List.choose (function
                | [] -> None
                | specifiers ->
                    ImportDeclaration(List.toArray specifiers, StringLiteral.stringLiteral(path))
                    |> Some)
            |> function
                | [] ->
                    // If there are no specifiers, this is just an import for side effects,
                    // put it after the other ones to match standard JS practices, see #2228
                    ImportDeclaration([||], StringLiteral.stringLiteral(path))
                    |> statefulImports.Add
                    []
                | decls -> decls
            )
        |> fun staticImports -> [
            yield! staticImports
            yield! statefulImports
        ]

    let getIdentForImport (ctx: Context) (path: string) (selector: string) =
        if System.String.IsNullOrEmpty selector then None
        else
            match selector with
            | "*" | "default" -> Path.GetFileNameWithoutExtension(path)
            | _ -> selector
            |> getUniqueNameInRootScope ctx
            |> Some

module Compiler =
    open Util

    type BabelCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string,Import>()

        interface IExtendedCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member _.GetImportExpr(ctx, selector, path, r) =
                let cachedName = path + "::" + selector
                match imports.TryGetValue(cachedName) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> Expression.identifier(localIdent)
                    | None -> Expression.nullLiteral()
                | false, _ ->
                    let localId = getIdentForImport ctx path selector
                    let i =
                      { Selector =
                            if selector = Naming.placeholder then
                                     "`importMember` must be assigned to a variable"
                                     |> addError com [] r; selector
                            else selector
                        Path = path
                        LocalIdent = localId }
                    imports.Add(cachedName, i)
                    match localId with
                    | Some localId -> Expression.identifier(localId)
                    | None -> Expression.nullLiteral()
            member _.GetAllImports() = imports.Values :> _
            member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e
            member bcom.TransformFunction(ctx, name, args, body) = transformFunction bcom ctx name args body
            member bcom.TransformImport(ctx, selector, path) = transformImport bcom ctx None selector path

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.ProjectFile = com.ProjectFile
            member _.GetEntity(fullName) = com.GetEntity(fullName)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.GetOrAddInlineExpr(fullName, generate) = com.GetOrAddInlineExpr(fullName, generate)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
            member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = BabelCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IExtendedCompiler
        let declScopes =
            let hs = HashSet()
            for decl in file.Declarations do
                hs.UnionWith(decl.UsedNames)
            hs

        let ctx =
          { File = file
            UsedNames = { RootScope = HashSet file.UsedNamesInRootScope
                          DeclarationScopes = declScopes
                          CurrentDeclarationScope = Unchecked.defaultof<_> }
            DecisionTargets = []
            HoistVars = fun _ -> false
            TailCallOpportunity = None
            OptimizeTailCall = fun () -> ()
            ScopedTypeParams = Set.empty }
        let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        let importDecls = com.GetAllImports() |> transformImports
        importDecls, rootDecls
