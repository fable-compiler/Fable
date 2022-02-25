module rec Fable.Transforms.Fable2Dart

open Fable
open Fable.AST
open Fable.AST.Dart
open System.Collections.Generic
open Fable.Transforms.AST

type ReturnStrategy =
    | Return
    | ReturnVoid
    | Assign of Expression
    | Target of Ident

type ArgsInfo =
    | CallInfo of Fable.CallInfo
    | NoCallInfo of args: Fable.Expr list

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
    OptimizeTailCall: unit -> unit }

type MemberKind =
    | ClassConstructor
    | NonAttached of funcName: string
    | Attached of isStatic: bool

type IDartCompiler =
    inherit Compiler
    abstract GetAllImports: unit -> Import list
    abstract GetImportIdent: Context * selector: string * path: string * SourceLocation option -> Ident
    abstract TransformAsExpr: Context * Fable.Expr -> Expression
    abstract TransformAsStatements: Context * ReturnStrategy option * Fable.Expr -> Statement list
    abstract TransformImport: Context * selector:string * path:string -> Expression
    abstract TransformFunction: Context * string option * Fable.Ident list * Fable.Expr -> Ident list * Statement list
    abstract WarnOnlyOnce: string * ?range: SourceLocation -> unit

[<RequireQualifiedAccess>]
module Lib =
    let getIdent (com: IDartCompiler) ctx moduleName memberName =
        com.GetImportIdent(ctx, memberName, getLibPath com moduleName, None)

module Util =
    let (|TransformExpr|) (com: IDartCompiler) ctx e =
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

    let addErrorAndReturnNull (com: Compiler) (range: SourceLocation option) (error: string) =
        addError com [] range error
        NullLiteral |> Literal

    let numType kind = Fable.Number(kind, Fable.NumberInfo.Empty)

    let makeIdent typ name =
        { Name = name; Type = typ; Prefix = None }

    let makePrefixedIdent typ prefix name =
        { Name = name; Type = typ; Prefix = Some prefix }

    let getEntityRef (com: IDartCompiler) ctx ent =
        let entRef = Dart.Replacements.entityRef com ent
        com.TransformAsExpr(ctx, entRef)

    // TODO: Check conversions like ToString > toString
    let get (_: SourceLocation option) left memberName =
        PropertyAccess(left, memberName)

    let getUnionExprTag expr =
        get None expr "$tag"

    let getUnionExprFields expr =
        get None expr "$fields"

    let rec getParts (parts: string list) (expr: Expression) =
        match parts with
        | [] -> expr
        | m::ms -> get None expr m |> getParts ms

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
        | None -> failwithf $"Cannot find DecisionTree target %i{targetIndex}"
        | Some(idents, target) -> idents, target

    let rec isStatement ctx preferStatement (expr: Fable.Expr) =
        match expr with
        | Fable.Unresolved _
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
            | Fable.Debugger | Fable.RegionStart _ -> true
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

    let isInt64OrLess = function
        | Fable.Number(kind, _) ->
            match kind with
            | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64 -> true
            | Float32 | Float64 | Decimal | NativeInt | UNativeInt | BigInt -> false
        | _ -> false

    let makeAnonymousFunction ((args: Ident list), (body: Statement list)): Expression =
        // TODO: gen params
        // TODO: Check if body is just a single return statement
        AnonymousFunction(args, Choice1Of2 body, [])

    let assign (_range: SourceLocation option) left right =
        AssignmentExpression(left, AssignEqual, right)

    /// Immediately Invoked Function Expression
    // let iife (com: IDartCompiler) ctx (expr: Fable.Expr) =
    //     let _, body = com.TransformFunction(ctx, None, [], expr)
    //     // Use an arrow function in case we need to capture `this`
    //     Expression.callExpression(Expression.arrowFunctionExpression([||], body), [||])

    // let optimizeTailCall (com: IDartCompiler) (ctx: Context) range (tc: ITailCallOpportunity) args =
    //     let rec checkCrossRefs tempVars allArgs = function
    //         | [] -> tempVars
    //         | (argId, _arg)::rest ->
    //             let found = allArgs |> List.exists (FableTransforms.deepExists (function
    //                 | Fable.IdentExpr i -> argId = i.Name
    //                 | _ -> false))
    //             let tempVars =
    //                 if found then
    //                     let tempVarName = getUniqueNameInDeclarationScope ctx (argId + "_tmp")
    //                     Map.add argId tempVarName tempVars
    //                 else tempVars
    //             checkCrossRefs tempVars allArgs rest
    //     ctx.OptimizeTailCall()
    //     let zippedArgs = List.zip tc.Args args
    //     let tempVars = checkCrossRefs Map.empty args zippedArgs
    //     let tempVarReplacements = tempVars |> Map.map (fun _ v -> makeIdentExpr v)
    //     [|
    //         // First declare temp variables
    //         for (KeyValue(argId, tempVar)) in tempVars do
    //             yield varDeclaration (Pattern.identifier(tempVar)) false (Expression.identifier(argId)) |> Declaration.VariableDeclaration |> Declaration
    //         // Then assign argument expressions to the original argument identifiers
    //         // See https://github.com/fable-compiler/Fable/issues/1368#issuecomment-434142713
    //         for (argId, arg) in zippedArgs do
    //             let arg = FableTransforms.replaceValues tempVarReplacements arg
    //             let arg = com.TransformAsExpr(ctx, arg)
    //             yield assign None (Expression.identifier(argId)) arg |> ExpressionStatement
    //         yield Statement.continueStatement(Identifier.identifier(tc.Label), ?loc=range)
    //     |]

    // TODO: gen args
    let callFunction (_: SourceLocation option) funcExpr (args: Expression list) =
        InvocationExpression(funcExpr, [], args)

    let transformCallArgs (com: IDartCompiler) ctx (r: SourceLocation option) (info: ArgsInfo) =
        // TODO: Named params
        let paramObjInfo, hasSpread, args =
            match info with
            | CallInfo i ->
                let paramObjInfo = None // TODO
                paramObjInfo, i.HasSpread, i.Args
            | NoCallInfo args -> None, false, args

        match args with
        | []
        | [MaybeCasted(Fable.Value(Fable.UnitConstant,_))] -> []
        // | args when hasSpread ->
        //     match List.rev args with
        //     | [] -> []
        //     | (Replacements.Util.ArrayOrListLiteral(spreadArgs,_))::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
        //         rest @ (List.map (fun e -> com.TransformAsExpr(ctx, e)) spreadArgs)
        //     | last::rest ->
        //         let rest = List.rev rest |> List.map (fun e -> com.TransformAsExpr(ctx, e))
        //         rest @ [Expression.spreadElement(com.TransformAsExpr(ctx, last))]
        | args -> List.map (fun e -> com.TransformAsExpr(ctx, e)) args

    let resolveExpr strategy expr: Statement =
        match strategy with
        | None | Some ReturnVoid -> ExpressionStatement expr
        | Some Return -> ReturnStatement expr
        | Some(Assign left) -> assign None left expr |> ExpressionStatement
        | Some(Target left) -> assign None (IdentExpression left) expr |> ExpressionStatement

    let transformType (_com: IDartCompiler) (_ctx: Context) (t: Fable.Type) =
        match t with
        | Fable.Unit -> Void
        | Fable.Boolean -> Boolean
        | Fable.String -> String
        | Fable.Number(kind, _) ->
            match kind with
            | Int8 | UInt8 | Int16 | UInt16 | Int32 | UInt32 | Int64 | UInt64 -> Integer
            | Float32 | Float64 -> Double
            | Decimal | BigInt | NativeInt | UNativeInt -> Dynamic // TODO
        | _ -> Dynamic // TODO failwith $"todo: type %A{t}"

    let transformIdentWith (com: IDartCompiler) ctx typ name: Ident =
        let typ = transformType com ctx typ
        makeIdent typ name

    let transformIdent (com: IDartCompiler) ctx (id: Fable.Ident): Ident =
        transformIdentWith com ctx id.Type id.Name

    let transformIdentAsExpr (com: IDartCompiler) ctx (id: Fable.Ident) =
        transformIdentWith com ctx id.Type id.Name |> Expression.identExpression

    let transformVarDeclaration com ctx (memb: Fable.MemberDecl) =
        // TODO: Prefix non-public values with underscore or raise warning?
        let ident = transformIdentWith com ctx memb.Body.Type memb.Name
        // TODO: If value is primitive, list, union or record without mutable fields
        // we can declare it as const (if var is mutable we can make only the value const)
        let kind = if memb.Info.IsMutable then Var else Final
        let value = transformAsExpr com ctx memb.Body
        Declaration.variableDeclaration(ident, kind, value)

    let transformLocalVarDeclaration com ctx (fableIdent: Fable.Ident) value =
        let ident = transformIdent com ctx fableIdent
        // TODO: If value is primitive, list, union or record without mutable fields
        // we can declare it as const (if var is mutable we can make only the value const)
        let kind = if fableIdent.IsMutable then Var else Final
        let value = value |> Option.map (transformAsExpr com ctx)
        ident, kind, value

    let transformImport (com: IDartCompiler) ctx r (selector: string) (path: string) =
        let selector, parts =
            let parts = Array.toList(selector.Split('.'))
            parts.Head, parts.Tail
        com.GetImportIdent(ctx, selector, path, r)
        |> Expression.identExpression
        |> getParts parts

    let transformValue (com: IDartCompiler) (ctx: Context) (r: SourceLocation option) value: Expression =
        match value with
        | Fable.NewUnion(values, tag, ref, _genArgs) ->
            // TODO: genArgs
            let ent = com.GetEntity(ref)
            let fields = List.map (fun x -> com.TransformAsExpr(ctx, x)) values
            let args = [Expression.integerLiteral(tag); Expression.listLiteral fields]
            let consRef = getEntityRef com ctx ent
            callFunction r consRef args
        | Fable.NewOption(None, _typ, _isStruct) -> Expression.nullLiteral()
        | Fable.NewOption(Some v, _typ, _isStruct) -> transformAsExpr com ctx v
        | Fable.BoolConstant v -> Expression.booleanLiteral v
        | Fable.StringConstant v -> Expression.stringLiteral v
        | Fable.NumberConstant(x, kind, _) ->
            match kind, x with
            | Int8, (:? int8 as x) -> Expression.integerLiteral(int64 x)
            | UInt8, (:? uint8 as x) -> Expression.integerLiteral(int64 x)
            | Int16, (:? int16 as x) -> Expression.integerLiteral(int64 x)
            | UInt16, (:? uint16 as x) -> Expression.integerLiteral(int64 x)
            | Int32, (:? int32 as x) -> Expression.integerLiteral(x)
            | UInt32, (:? uint32 as x) -> Expression.integerLiteral(int64 x)
            | Int64, (:? int64 as x) -> Expression.integerLiteral(x)
            | UInt64, (:? uint64 as x) -> Expression.integerLiteral(int64 x)
            | Float32, (:? float32 as x) -> Expression.doubleLiteral(float x)
            | Float64, (:? float as x) -> Expression.doubleLiteral(x)
            | _ ->
                $"Expected literal of type %A{kind} but got {x.GetType().FullName}"
                |> addErrorAndReturnNull com r
        | v -> failwith $"TODO: value %A{v}"

    let transformOperation com ctx (_: SourceLocation option) t opKind: Expression =
        match opKind with
        | Fable.Unary(op, TransformExpr com ctx expr) ->
            UnaryExpression(op, expr)
        | Fable.Binary(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            BinaryExpression(op, left, right, isInt64OrLess t)
        | Fable.Logical(op, TransformExpr com ctx left, TransformExpr com ctx right) ->
            LogicalExpression(op, left, right)

    let transformCall (com: IDartCompiler) ctx range callee (callInfo: Fable.CallInfo) =
        // Try to optimize some patterns after FableTransforms
        let optimized =
            match callInfo.OptimizableInto, callInfo.Args with
            | Some "array" , [Replacements.Util.ArrayOrListLiteral(vals,_)] -> Fable.Value(Fable.NewArray(vals, Fable.Any), range) |> Some
            | _ -> None

        match optimized with
        | Some e -> com.TransformAsExpr(ctx, e)
        | None ->
            let callee = com.TransformAsExpr(ctx, callee)
            let args = transformCallArgs com ctx range (CallInfo callInfo)
            match callInfo.ThisArg with
            | Some(TransformExpr com ctx thisArg) -> callFunction range callee (thisArg::args)
            | None -> callFunction range callee args

    let transformCallAsStatements com ctx range (_: Fable.Type) returnStrategy callee callInfo =
        let argsLen (i: Fable.CallInfo) =
            List.length i.Args + (if Option.isSome i.ThisArg then 1 else 0)
        // Warn when there's a recursive call that couldn't be optimized?
        match returnStrategy, ctx.TailCallOpportunity with // TODO
        // | Some(Return|ReturnVoid), Some tc when tc.IsRecursiveRef(callee)
        //                                     && argsLen callInfo = List.length tc.Args ->
        //     let args =
        //         match callInfo.ThisArg with
        //         | Some thisArg -> thisArg::callInfo.Args
        //         | None -> callInfo.Args
        //     optimizeTailCall com ctx range tc args
        | _ ->
            [transformCall com ctx range callee callInfo |> resolveExpr returnStrategy]

    let transformGet (com: IDartCompiler) ctx range typ fableExpr kind =
        match kind with
        | Fable.FieldGet(fieldName,_) ->
            let fableExpr =
                match fableExpr with
                // If we're accessing a virtual member with default implementation (see #701)
                // from base class, we can use `super` in JS so we don't need the bound this arg
                | Fable.Value(Fable.BaseValue(_,t), r) -> Fable.Value(Fable.BaseValue(None, t), r)
                | _ -> fableExpr
            let expr = com.TransformAsExpr(ctx, fableExpr)
            get range expr fieldName

        | Fable.UnionTag ->
            com.TransformAsExpr(ctx, fableExpr) |> getUnionExprTag

        | Fable.UnionField(_caseIndex, fieldIndex) ->
            let expr = com.TransformAsExpr(ctx, fableExpr)
            let fields = getUnionExprFields expr
            let index = Expression.indexExpression(fields, fieldIndex)
            match typ with
            | Fable.Any -> index
            | typ -> Expression.asExpression(index, transformType com ctx typ)

        | e -> failwith $"todo: get ${e}"

    // TODO: tail calls, hoist vars
    let transformFunction com ctx name (args: Fable.Ident list) (body: Fable.Expr): Ident list * Statement list =
        let args = discardUnitArg args |> List.map (transformIdent com ctx)
        let ret = if body.Type = Fable.Unit then ReturnVoid else Return
        let body = transformAsStatements com ctx (Some ret) body
        args, body

    // let transformBindingAsExpr (com: IDartCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
    //     transformBindingExprBody com ctx var value
    //     |> assign None (transformIdentAsExpr com ctx var)

    let transformBindingAsStatements (com: IDartCompiler) ctx (var: Fable.Ident) (value: Fable.Expr) =
        if isStatement ctx false value then
            let var, kind, _ = transformLocalVarDeclaration com ctx var None
            let varExpr = IdentExpression var
            [
                LocalVariableDeclaration(var, kind, None)
                yield! com.TransformAsStatements(ctx, Some(Assign varExpr), value)
            ]
        else
            // TODO: Check if we need a function declaration
            // (See transformBindingExprBody in Fable2Babel)
            let ident, kind, body = transformLocalVarDeclaration com ctx var (Some value)
            [LocalVariableDeclaration(ident, kind, body)]

    let transformSwitch (com: IDartCompiler) ctx returnStrategy evalExpr cases defaultCase: Statement =
        let cases =
            cases |> List.choose (fun (guards, expr) ->
                // Remove empty branches
                match returnStrategy, expr, guards with
                | None, Fable.Value(Fable.UnitConstant,_), _
                | _, _, [] -> None
                | _, _, guards ->
                    let guards = guards |> List.map (fun e -> com.TransformAsExpr(ctx, e))
                    let caseBody = com.TransformAsStatements(ctx, returnStrategy, expr)
                    SwitchCase(guards, caseBody) |> Some
                )
        let defaultCase =
            defaultCase
            |> Option.map (fun expr -> com.TransformAsStatements(ctx, returnStrategy, expr))
        Statement.switchStatement(com.TransformAsExpr(ctx, evalExpr), cases, defaultCase)

    let matchTargetIdentAndValues idents values =
        if List.isEmpty idents then []
        elif List.sameLength idents values then List.zip idents values
        else failwith "Target idents/values lengths differ"

    let getDecisionTargetAndBindValues (com: IDartCompiler) (ctx: Context) targetIndex boundValues =
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

    let transformDecisionTreeSuccessAsExpr (com: IDartCompiler) (ctx: Context) targetIndex boundValues =
        let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
        match bindings with
        | [] -> com.TransformAsExpr(ctx, target)
        | bindings ->
            let target = List.rev bindings |> List.fold (fun e (i,v) -> Fable.Let(i,v,e)) target
            com.TransformAsExpr(ctx, target)

    let transformDecisionTreeSuccessAsStatements (com: IDartCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Statement list =
        match returnStrategy with
        | Some(Target targetId) ->
            let idents, _ = getDecisionTarget ctx targetIndex
            let assignments =
                matchTargetIdentAndValues idents boundValues
                |> List.map (fun (id, TransformExpr com ctx value) ->
                    assign None (transformIdentAsExpr com ctx id) value
                    |> ExpressionStatement)
            let targetAssignment =
                assign None (IdentExpression targetId) (Expression.integerLiteral targetIndex)
                |> ExpressionStatement
            List.append [targetAssignment] assignments
        | ret ->
            let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
            let bindings = bindings |> List.collect (fun (i, v) -> transformBindingAsStatements com ctx i v)
            List.append bindings (com.TransformAsStatements(ctx, ret, target))

    let transformDecisionTreeAsSwitch expr =
        let (|Equals|_|) = function
            | Fable.Operation(Fable.Binary(BinaryEqual, expr, right), _, _) ->
                match expr with
                | Fable.Value((Fable.CharConstant _ | Fable.StringConstant _ | Fable.NumberConstant _), _) -> Some(expr, right)
                | _ -> None
            | Fable.Test(expr, Fable.UnionCaseTest tag, _) ->
                let evalExpr = Fable.Get(expr, Fable.UnionTag, numType Int32, None)
                let right = makeIntConst tag
                Some(evalExpr, right)
            | _ -> None
        let sameEvalExprs evalExpr1 evalExpr2 =
            match evalExpr1, evalExpr2 with
            | Fable.IdentExpr i1, Fable.IdentExpr i2
            | Fable.Get(Fable.IdentExpr i1,Fable.UnionTag,_,_), Fable.Get(Fable.IdentExpr i2,Fable.UnionTag,_,_) ->
                i1.Name = i2.Name
            | Fable.Get(Fable.IdentExpr i1, Fable.FieldGet(fieldName1, _),_,_), Fable.Get(Fable.IdentExpr i2, Fable.FieldGet(fieldName2, _),_,_) ->
                i1.Name = i2.Name && fieldName1 = fieldName2
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

    let transformDecisionTreeAsExpr (com: IDartCompiler) (ctx: Context) targets expr: Expression =
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
    let transformDecisionTreeWithTwoSwitches (com: IDartCompiler) ctx returnStrategy
                    (targets: (Fable.Ident list * Fable.Expr) list) treeExpr =
        // Declare target and bound idents
        let targetId =
            getUniqueNameInDeclarationScope ctx "pattern_matching_result"
            |> makeTypedIdent (numType Int32)
        let varDecls =
            [
                transformIdent com ctx targetId
                yield! targets |> List.collect (fun (idents,_) ->
                    idents |> List.map (transformIdent com ctx))
            ]
            |> List.map Statement.variableDeclaration
        // Transform targets as switch
        let switch2 =
            // TODO: Declare the last case as the default case?
            let cases = targets |> List.mapi (fun i (_,target) -> [makeIntConst i], target)
            transformSwitch com ctx returnStrategy (targetId |> Fable.IdentExpr) cases None
        // Transform decision tree
        let targetAssign = Target(transformIdent com ctx targetId)
        let ctx = { ctx with DecisionTargets = targets }
        match transformDecisionTreeAsSwitch treeExpr with
        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
            let cases = groupSwitchCases (numType Int32) cases (defaultIndex, defaultBoundValues)
            let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, numType Int32)
            let switch1 = transformSwitch com ctx (Some targetAssign) evalExpr cases (Some defaultCase)
            varDecls @ [switch1; switch2]
        | None ->
            let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
            varDecls @ decisionTree @ [switch2]

    let transformDecisionTreeAsStatements (com: IDartCompiler) (ctx: Context) returnStrategy
                        (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Statement list =
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
                [transformSwitch com ctx returnStrategy evalExpr cases (Some defaultCase)]
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
                    [transformSwitch com ctx returnStrategy evalExpr cases (Some defaultCase)]
                | None ->
                    transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr
            else
                transformDecisionTreeWithTwoSwitches com ctx returnStrategy targets treeExpr

    let transformTest (com: IDartCompiler) ctx range kind expr: Expression =
        let expr = com.TransformAsExpr(ctx, expr)
        match kind with
        | Fable.TypeTest t ->
            Expression.isExpression(expr, transformType com ctx t)
        | Fable.OptionTest isSome -> failwith "todo: option test"
        | Fable.ListTest nonEmpty -> failwith "todo: list test"
        | Fable.UnionCaseTest tag ->
            let expected = Expression.integerLiteral tag
            let actual = getUnionExprTag expr
            Expression.binaryExpression(BinaryEqual, actual, expected)

    let rec transformAsExpr (com: IDartCompiler) ctx (expr: Fable.Expr): Expression =
        match expr with
        | Fable.Value(kind, r) -> transformValue com ctx r kind

        | Fable.Operation(kind, t, r) -> transformOperation com ctx r t kind

        | Fable.IdentExpr ident -> transformIdentAsExpr com ctx ident

        | Fable.Import({ Selector = selector; Path = path }, _, r) ->
            transformImport com ctx r selector path

        | Fable.Test(expr, kind, range) ->
            transformTest com ctx range kind expr

        | Fable.Call(callee, info, _, range) ->
            transformCall com ctx range callee info

        | Fable.Get(expr, kind, typ, range) ->
            transformGet com ctx range typ expr kind

        | Fable.Lambda(arg, body, info) ->
            transformFunction com ctx info.Name [arg] body
            |> makeAnonymousFunction

        | Fable.IfThenElse(TransformExpr com ctx guardExpr,
                           TransformExpr com ctx thenExpr,
                           TransformExpr com ctx elseExpr, r) ->
            Expression.conditionalExpression(guardExpr, thenExpr, elseExpr)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsExpr com ctx targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsExpr com ctx idx boundValues

        // | Fable.Let(ident, value, body) ->
        //     if ctx.HoistVars [ident] then
        //         let assignment = transformBindingAsExpr com ctx ident value
        //         Expression.sequenceExpression([|assignment; com.TransformAsExpr(ctx, body)|])
        //     else iife com ctx expr

        // | Fable.LetRec(bindings, body) ->
        //     if ctx.HoistVars(List.map fst bindings) then
        //         let values = bindings |> List.mapToArray (fun (id, value) ->
        //             transformBindingAsExpr com ctx id value)
        //         Expression.sequenceExpression(Array.append values [|com.TransformAsExpr(ctx, body)|])
        //     else iife com ctx expr

        | e -> failwith $"todo: transform expr %A{e}"

    let rec transformAsStatements (com: IDartCompiler) ctx returnStrategy (expr: Fable.Expr): Statement list =
        match expr with
        | Fable.Value(kind, r) ->
            [transformValue com ctx r kind |> resolveExpr returnStrategy]

        | Fable.IdentExpr id ->
            [transformIdentAsExpr com ctx id |> resolveExpr returnStrategy]

        | Fable.Operation(kind, t, r) ->
            [transformOperation com ctx r t kind |> resolveExpr returnStrategy]

        | Fable.Call(callee, info, typ, range) ->
            transformCallAsStatements com ctx range typ returnStrategy callee info

        | Fable.Import({ Selector = selector; Path = path }, _t, r) ->
            [transformImport com ctx r selector path |> resolveExpr returnStrategy]

        | Fable.Get(expr, kind, t, range) ->
            [transformGet com ctx range t expr kind |> resolveExpr returnStrategy]

        | Fable.Lambda(arg, body, info) ->
            transformFunction com ctx info.Name [arg] body
            |> makeAnonymousFunction
            |> resolveExpr returnStrategy
            |> List.singleton

        | Fable.Sequential statements ->
            let lasti = (List.length statements) - 1
            statements |> List.mapi (fun i statement ->
                let ret = if i < lasti then None else returnStrategy
                transformAsStatements com ctx ret statement)
            |> List.concat

        | Fable.Let(ident, value, body) ->
            let binding = transformBindingAsStatements com ctx ident value
            List.append binding (transformAsStatements com ctx returnStrategy body)

        | Fable.LetRec(bindings, body) ->
            let bindings = bindings |> List.collect (fun (i, v) -> transformBindingAsStatements com ctx i v)
            List.append bindings (transformAsStatements com ctx returnStrategy body)

        | Fable.DecisionTree(expr, targets) ->
            transformDecisionTreeAsStatements com ctx returnStrategy targets expr

        | Fable.DecisionTreeSuccess(idx, boundValues, _) ->
            transformDecisionTreeSuccessAsStatements com ctx returnStrategy idx boundValues

        | e -> failwith $"todo: transformAsStatements %A{e}"

    let getMemberArgsAndBody (com: IDartCompiler) ctx kind (args: Fable.Ident list) (body: Fable.Expr) =
        let funcName, args, body =
            match kind, args with
            | Attached(isStatic=false), (thisArg::args) ->
                let body =
                    // TODO: If ident is not captured maybe we can just replace it with "this"
                    if FableTransforms.isIdentUsed thisArg.Name body then
                        let thisKeyword = Fable.IdentExpr { thisArg with Name = "this" }
                        Fable.Let(thisArg, thisKeyword, body)
                    else body
                None, args, body
            | Attached(isStatic=true), _
            | ClassConstructor, _ -> None, args, body
            | NonAttached funcName, _ -> Some funcName, args, body
            | _ -> None, args, body

        transformFunction com ctx funcName args body

    let transformModuleFunction (com: IDartCompiler) ctx (memb: Fable.MemberDecl) =
        let returnType = transformType com ctx memb.Body.Type
        let args, body = getMemberArgsAndBody com ctx (NonAttached memb.Name) memb.Args memb.Body
        let isEntryPoint =
            memb.Info.Attributes
            |> Seq.exists (fun att -> att.Entity.FullName = Atts.entryPoint)
        if isEntryPoint then
            failwith "todo: main function"
        else
            // TODO: generic params
            Declaration.functionDeclaration(memb.Name, args, body, returnType)

    let transformClassDeclaration (com: IDartCompiler) ctx (decl: Fable.ClassDecl) =
            let entRef = decl.Entity
            let ent = com.GetEntity(entRef)
            if ent.IsInterface then
                // TODO: Abstract members
                [Declaration.classDeclaration(decl.Name, isAbstract=true)]
            else
                // TODO: Members
                if ent.IsFSharpUnion then
                    let extends = Lib.getIdent com ctx "Types" "Union"
                    let args = [
                        makeIdent Integer "tag"
                        makeIdent (Type.List Object) "fields"
                    ]
                    let cons = Declaration.constructorDeclaration(args=args, superArgs=args)
                    [Declaration.classDeclaration(decl.Name, constructor=cons, extends=extends)]
                else
                    let cons = Declaration.constructorDeclaration()
                    [Declaration.classDeclaration(decl.Name, constructor=cons)]

    let rec transformDeclaration (com: IDartCompiler) ctx decl =
        let withCurrentScope ctx (usedNames: Set<string>) f =
            let ctx = { ctx with UsedNames = { ctx.UsedNames with CurrentDeclarationScope = HashSet usedNames } }
            let result = f ctx
            ctx.UsedNames.DeclarationScopes.UnionWith(ctx.UsedNames.CurrentDeclarationScope)
            result

        match decl with
        | Fable.ModuleDeclaration decl ->
            decl.Members |> List.collect (transformDeclaration com ctx)

        | Fable.MemberDeclaration memb ->
            withCurrentScope ctx memb.UsedNames <| fun ctx ->
                if memb.Info.IsValue then
                    // TODO: Prefix non-public values with underscore or raise warning?
                    let ident = transformIdentWith com ctx memb.Body.Type memb.Name
                    // TODO: If value is primitive, list, union or record without mutable fields
                    // we can declare it as const (if var is mutable we can make only the value const)
                    let kind = if memb.Info.IsMutable then Var else Final
                    let value = transformAsExpr com ctx memb.Body
                    [Declaration.variableDeclaration(ident, kind, value)]
                else
                    [transformModuleFunction com ctx memb]

        // TODO: Action declarations are not supported in Dart, compile as: var _ = ...
        | Fable.ActionDeclaration _ -> []
//            withCurrentScope ctx decl.UsedNames <| fun ctx ->
//                transformAction com ctx decl.Body

        | Fable.ClassDeclaration decl ->
            transformClassDeclaration com ctx decl

    let getIdentForImport (ctx: Context) (path: string) =
        Path.GetFileNameWithoutExtension(path).Replace(".", "_")
        |> Naming.applyCaseRule Core.CaseRules.SnakeCase
        |> getUniqueNameInRootScope ctx

module Compiler =
    open Util

    type DartCompiler (com: Compiler) =
        let onlyOnceWarnings = HashSet<string>()
        let imports = Dictionary<string, Import>()

        interface IDartCompiler with
            member _.WarnOnlyOnce(msg, ?range) =
                if onlyOnceWarnings.Add(msg) then
                    addWarning com [] range msg

            member _.GetImportIdent(ctx, selector, path, r) =
                let localId =
                    match imports.TryGetValue(path) with
                    | true, i ->
                        match i.LocalIdent with
                        | Some localId -> localId
                        | None ->
                            let localId = getIdentForImport ctx path
                            imports[path] <- { Path = path; LocalIdent = Some localId }
                            localId
                    | false, _ ->
                        let localId = getIdentForImport ctx path
                        imports.Add(path, { Path = path; LocalIdent = Some localId })
                        localId
                let ident = makeIdent Object localId
                match selector with
                | Naming.placeholder ->
                    "`importMember` must be assigned to a variable"
                    |> addError com [] r
                    ident
                | "*" -> ident
                | selector -> { ident with Prefix = Some ident.Name; Name = selector }

            member _.GetAllImports() = imports.Values |> Seq.toList
            member this.TransformAsExpr(ctx, e) = transformAsExpr this ctx e
            member this.TransformAsStatements(ctx, ret, e) = transformAsStatements this ctx ret e
            member this.TransformFunction(ctx, name, args, body) = transformFunction this ctx name args body
            member this.TransformImport(ctx, selector, path) = transformImport this ctx None selector path

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.IsPrecompilingInlineFunction = com.IsPrecompilingInlineFunction
            member _.WillPrecompileInlineFunction(file) = com.WillPrecompileInlineFunction(file)
            member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
            member _.GetRootModule(fileName) = com.GetRootModule(fileName)
            member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
            member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)
            member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
            member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
                com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

    let makeCompiler com = DartCompiler(com)

    let transformFile (com: Compiler) (file: Fable.File) =
        let com = makeCompiler com :> IDartCompiler
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
            OptimizeTailCall = fun () -> () }
        let rootDecls = List.collect (transformDeclaration com ctx) file.Declarations
        let imports = com.GetAllImports()
        { File.Imports = imports
          Declarations = rootDecls }
