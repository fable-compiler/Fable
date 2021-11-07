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
    abstract GetAllImports: unit -> Import list
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
        | None -> failwithf $"Cannot find DecisionTree target %i{targetIndex}"
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

//    let transformDecisionTreeSuccessAsStatements (com: IExtendedCompiler) (ctx: Context) returnStrategy targetIndex boundValues: Statement[] =
//        match returnStrategy with
//        | Some(Target targetId) as target ->
//            let idents, _ = getDecisionTarget ctx targetIndex
//            let assignments =
//                matchTargetIdentAndValues idents boundValues
//                |> List.mapToArray (fun (id, TransformExpr com ctx value) ->
//                    assign None (identAsExpr id) value |> ExpressionStatement)
//            let targetAssignment = assign None (targetId |> Expression.Identifier) (ofInt targetIndex) |> ExpressionStatement
//            Array.append [|targetAssignment|] assignments
//        | ret ->
//            let bindings, target = getDecisionTargetAndBindValues com ctx targetIndex boundValues
//            let bindings = bindings |> Seq.collect (fun (i, v) -> transformBindingAsStatements com ctx i v) |> Seq.toArray
//            Array.append bindings (com.TransformAsStatements(ctx, ret, target))

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

    let transformDecisionTreeAsExpr (com: IExtendedCompiler) (ctx: Context) targets expr =
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
//    let transformDecisionTreeWithTwoSwitches (com: IExtendedCompiler) ctx returnStrategy
//                    (targets: (Fable.Ident list * Fable.Expr) list) treeExpr =
//        // Declare target and bound idents
//        let targetId = getUniqueNameInDeclarationScope ctx "pattern_matching_result" |> makeIdent
//        let multiVarDecl =
//            let boundIdents = targets |> List.collect (fun (idents,_) ->
//                idents |> List.map (fun id -> typedIdent com ctx id, None))
//            multiVarDeclaration Let ((typedIdent com ctx targetId, None)::boundIdents)
//        // Transform targets as switch
//        let switch2 =
//            // TODO: Declare the last case as the default case?
//            let cases = targets |> List.mapi (fun i (_,target) -> [makeIntConst i], target)
//            transformSwitch com ctx true returnStrategy (targetId |> Fable.IdentExpr) cases None
//        // Transform decision tree
//        let targetAssign = Target(ident targetId)
//        let ctx = { ctx with DecisionTargets = targets }
//        match transformDecisionTreeAsSwitch treeExpr with
//        | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
//            let cases = groupSwitchCases (Fable.Number(Int32, None)) cases (defaultIndex, defaultBoundValues)
//            let defaultCase = Fable.DecisionTreeSuccess(defaultIndex, defaultBoundValues, Fable.Number(Int32, None))
//            let switch1 = transformSwitch com ctx false (Some targetAssign) evalExpr cases (Some defaultCase)
//            [|multiVarDecl; switch1; switch2|]
//        | None ->
//            let decisionTree = com.TransformAsStatements(ctx, Some targetAssign, treeExpr)
//            [| yield multiVarDecl; yield! decisionTree; yield switch2 |]

//    let transformDecisionTreeAsStatements (com: IExtendedCompiler) (ctx: Context) returnStrategy
//                        (targets: (Fable.Ident list * Fable.Expr) list) (treeExpr: Fable.Expr): Statement[] =
//        // If some targets are referenced multiple times, hoist bound idents,
//        // resolve the decision index and compile the targets as a switch
//        let targetsWithMultiRefs =
//            if com.Options.Language = TypeScript then [] // no hoisting when compiled with types
//            else getTargetsWithMultipleReferences treeExpr
//        match targetsWithMultiRefs with
//        | [] ->
//            let ctx = { ctx with DecisionTargets = targets }
//            match transformDecisionTreeAsSwitch treeExpr with
//            | Some(evalExpr, cases, (defaultIndex, defaultBoundValues)) ->
//                let t = treeExpr.Type
//                let cases = cases |> List.map (fun (caseExpr, targetIndex, boundValues) ->

    let rec transformDeclaration (com: IExtendedCompiler) ctx decl =
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
                    [decl] // TODO
                else
                    // TODO HACK: For now we just put Return in front of the expression
                    { memb with Body = Fable.Extended(Fable.Return memb.Body, memb.Body.Range) }
                    |> Fable.MemberDeclaration
                    |> List.singleton

        // TODO
        | Fable.ActionDeclaration _
//            withCurrentScope ctx decl.UsedNames <| fun ctx ->
//                transformAction com ctx decl.Body
        | Fable.ClassDeclaration _ -> [decl]

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

            // TODO: the returned expression should be typed
            member _.GetImportExpr(ctx, selector, path, r) =
                let cachedName = path + "::" + selector
                match imports.TryGetValue(cachedName) with
                | true, i ->
                    match i.LocalIdent with
                    | Some localIdent -> makeIdentExpr localIdent
                    | None -> makeNull()
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
                    | Some localId -> makeIdentExpr localId // TODO: type
                    | None -> makeNull()
            member _.GetAllImports() = imports.Values |> Seq.toList
            member bcom.TransformAsExpr(ctx, e) = failwith "todo" //transformAsExpr bcom ctx e
            member bcom.TransformAsStatements(ctx, ret, e) = failwith "todo" //transformAsStatements bcom ctx ret e
            member bcom.TransformFunction(ctx, name, args, body) = failwith "todo" //transformFunction bcom ctx name args body
            member bcom.TransformImport(ctx, selector, path) = failwith "todo" //transformImport bcom ctx None selector path

        interface Compiler with
            member _.Options = com.Options
            member _.Plugins = com.Plugins
            member _.LibraryDir = com.LibraryDir
            member _.CurrentFile = com.CurrentFile
            member _.OutputDir = com.OutputDir
            member _.OutputType = com.OutputType
            member _.ProjectFile = com.ProjectFile
            member _.GetEntity(fullName) = com.GetEntity(fullName)
            member _.TryGetNonCoreAssemblyEntity(fullName) = com.TryGetNonCoreAssemblyEntity(fullName)
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
        com.GetAllImports(), Fable.File(rootDecls)
