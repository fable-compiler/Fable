module rec Fable.FSharp2Fable.Compiler

#if !FABLE_COMPILER
open System.IO
#endif
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.AST.Fable.Util

open Patterns
open Types
open Identifiers
open Helpers
open Util

// Special values like seq, async, String.Empty...
let private (|SpecialValue|_|) com ctx = function
    | BasicPatterns.ILFieldGet (None, typ, fieldName) as fsExpr when typ.HasTypeDefinition ->
        match typ.TypeDefinition.TryFullName, fieldName with
        | Some "System.String", "Empty" -> Some (makeStrConst "")
        | Some "System.Guid", "Empty" -> Some (makeStrConst "00000000-0000-0000-0000-000000000000")
        | Some "System.TimeSpan", "Zero" ->
            Fable.Wrapped(makeIntConst 0, makeType com ctx.typeArgs fsExpr.Type) |> Some
        | Some ("System.DateTime" | "System.DateTimeOffset" as n), ("MaxValue" | "MinValue") ->
            let m = if n = "System.DateTime" then "Date" else "DateOffset"
            CoreLibCall(m, Some (Naming.lowerFirst fieldName), false, [])
            |> makeCall (makeRangeFrom fsExpr) (makeType com ctx.typeArgs fsExpr.Type) |> Some
        | Some "System.Decimal", "Zero" -> makeDecConst 0M |> Some
        | Some "System.Decimal", "One" -> makeDecConst 1M |> Some
        | _ -> None
    | _ -> None

let private transformLambda com ctx (fsExpr: FSharpExpr) args tupleDestructs body isDelegate =
    let lambdaType = makeType com ctx.typeArgs fsExpr.Type
    let ctx, args = makeLambdaArgs com ctx args
    let ctx =
        (ctx, tupleDestructs)
        ||> List.fold (fun ctx (var, value) ->
            transformExpr com ctx value |> bindExpr ctx var)
    let isDynamicCurried =
        if not isDelegate && not ctx.isDynamicCurriedLambda then
            match lambdaType with
            | Fable.Function(signatureArgs,_,_) -> signatureArgs.Length > args.Length
            | _ -> false
        else false
    let body =
        let ctx = { ctx with isDynamicCurriedLambda =
                                isDynamicCurried || ctx.isDynamicCurriedLambda }
        transformExpr com ctx body
    let lambda =
        let captureThis = ctx.thisAvailability <> ThisUnavailable
        Fable.Lambda(args, body, Fable.LambdaInfo(captureThis, isDelegate))
        |> Fable.Value
    if isDynamicCurried
    then makeDynamicCurriedLambda (makeRangeFrom fsExpr) lambdaType lambda
    else lambda

let private transformNewList com ctx (fsExpr: FSharpExpr) fsType argExprs =
    let rec flattenList (r: SourceLocation) accArgs = function
        | [] -> accArgs, None
        | arg::[BasicPatterns.NewUnionCase(_, _, rest)] ->
            flattenList r (arg::accArgs) rest
        | arg::[baseList] ->
            arg::accArgs, Some baseList
        | _ -> failwithf "Unexpected List constructor %O: %A" r fsExpr
    let unionType, range = makeType com ctx.typeArgs fsType, makeRange fsExpr.Range
    let buildArgs (args, baseList) =
        let args = args |> List.rev |> (List.map (transformExpr com ctx))
        let ar = Fable.Value (Fable.ArrayConst (Fable.ArrayValues args, Fable.Any))
        ar::(match baseList with Some li -> [transformExpr com ctx li] | None -> [])
    match argExprs with
    | [] -> CoreLibCall("List", None, true, [])
    | _ ->
        match flattenList range [] argExprs with
        | [arg], Some baseList ->
            let args = List.map (transformExpr com ctx) [arg; baseList]
            CoreLibCall("List", None, true, args)
        | args, baseList ->
            let args = buildArgs(args, baseList)
            CoreLibCall("List", Some "ofArray", false, args)
    |> makeCall (Some range) unionType

let private transformNonListNewUnionCase com ctx (fsExpr: FSharpExpr) fsType unionCase (argExprs: Fable.Expr list) =
    let unionType, range = makeType com ctx.typeArgs fsType, makeRange fsExpr.Range
    match fsType with
    | OptionUnion ->
        match argExprs with
        | expr::_ ->
            match expr.Type with
            // For unit, unresolved generics or nested options, create a runtime wrapper
            // See fable-core/Option.ts for more info
            | Fable.Unit | Fable.GenericParam _ | Fable.Option _ ->
                CoreLibCall("Option", Some "makeSome", false, [expr])
                |> makeCall (Some range) unionType
            // TODO: Check declared types that accept null? And Fable.Any?
            // | Fable.DeclaredType _ -> failwith "TODO"
            // For other types, just wrap the expression
            | _ -> Fable.Wrapped(expr, unionType)
        | _ -> Fable.Wrapped(Fable.Value Fable.Null, unionType)
    | ErasedUnion ->
        match argExprs with
        | [] -> Fable.Wrapped(Fable.Value Fable.Null, unionType)
        | [expr] -> Fable.Wrapped(expr, unionType)
        | _ ->
            "Erased Union Cases must have one single field: " + unionType.FullName
            |> addErrorAndReturnNull com ctx.fileName (Some range)
    | StringEnum ->
        if not(List.isEmpty argExprs)
        then
            "StringEnum types cannot have fields: " + unionType.FullName
            |> addErrorAndReturnNull com ctx.fileName (Some range)
        else lowerCaseName unionCase
    | ListUnion ->
        failwithf "transformNonListNewUnionCase must not be used with List %O" range
    | OtherType ->
        let tag = getUnionCaseIndex fsType unionCase.Name |> makeIntConst
        let argExprs =
            let argTypes =
                unionCase.UnionCaseFields
                |> Seq.map (fun x -> makeType com [] x.FieldType)
                |> Seq.toList
            ensureArity com argTypes argExprs
        let erasedUnion =
            // We can use the Erase attribute with union cases
            // to pass custom values to keyValueList
            if hasAtt Atts.erase unionCase.Attributes then
                match argExprs with
                | [Fable.Value(Fable.StringConst key); value] ->
                    Fable.Value(Fable.TupleConst [Fable.Value(Fable.StringConst key); value]) |> Some
                | _ ->
                    sprintf "Case %s from %s is decorated with %s, but the fields are not a key-value pair"
                        unionCase.Name unionType.FullName Atts.erase
                    |> addWarning com ctx.fileName (makeRange fsExpr.Range |> Some)
                    None
            else None
        match erasedUnion with
        | Some erasedUnion -> erasedUnion
        | None ->
            let argExprs =
                match argExprs with
                | [] -> [tag]
                | [argExpr] -> [tag; argExpr]
                | argExprs -> [tag; Fable.Value(Fable.ArrayConst(Fable.ArrayValues argExprs, Fable.Any))]
            buildApplyInfo com ctx (Some range) unionType unionType unionType.FullName
                ".ctor" Fable.Constructor ([],[],[],[]) (None, argExprs)
            |> tryBoth (tryPlugin com) (tryReplace com ctx (tryDefinition fsType))
            |> function
            | Some repl -> repl
            | None -> Fable.Apply(makeNonGenTypeRef com unionType, argExprs, Fable.ApplyCons, unionType, Some range)

let private transformObjExpr (com: IFableCompiler) (ctx: Context) (fsExpr: FSharpExpr) (objType: FSharpType)
                    (_baseCallExpr: FSharpExpr) (overrides: FSharpObjectExprOverride list) otherOverrides =
    // If `this` is available, capture it to avoid conflicts (see #158)
    let capturedThis =
        match ctx.thisAvailability with
        | ThisUnavailable -> None
        | ThisAvailable -> Some [None, com.GetUniqueVar() |> makeIdentExpr]
        | ThisCaptured(prevThis, prevVars) ->
            (prevThis, com.GetUniqueVar() |> makeIdentExpr)::prevVars |> Some
    let members =
        (objType, overrides)::otherOverrides
        |> List.collect (fun (typ, overrides) ->
            let overrides =
                if not typ.HasTypeDefinition then overrides else
                let typName = typ.TypeDefinition.FullName.Replace(".","-")
                overrides |> List.where (fun x ->
                    typName + "-" + x.Signature.Name
                    |> Naming.ignoredInterfaceMethods.Contains
                    |> not)
            overrides |> List.map (fun over ->
                let info = { isInstance = true; passGenerics = false }
                let args, range = over.CurriedParameterGroups, makeRange fsExpr.Range
                let ctx, thisArg, args', extraArgs' = bindMemberArgs com ctx info args
                let args' = args'@extraArgs'
                let ctx =
                    match capturedThis, thisArg with
                    | None, _ -> { ctx with thisAvailability = ThisAvailable }
                    | Some(capturedThis), Some thisArg ->
                        { ctx with thisAvailability=ThisCaptured(Some thisArg, capturedThis) }
                    | Some _, None -> failwithf "Unexpected Object Expression method withouth `this` argument %O" range
                // Don't use the typ argument as the override may come
                // from another type, like ToString()
                let typ =
                    if over.Signature.DeclaringType.HasTypeDefinition
                    then Some over.Signature.DeclaringType.TypeDefinition
                    else None
                // TODO: Check for indexed getter and setter also in object expressions?
                let name = over.Signature.Name |> Naming.removeGetSetPrefix
                let kind =
                    match over.Signature.Name with
                    | Naming.StartsWith "get_" _ -> Fable.Getter
                    | Naming.StartsWith "set_" _ -> Fable.Setter
                    | _ -> Fable.Method
                // FSharpObjectExprOverride.CurriedParameterGroups doesn't offer
                // information about ParamArray, we need to check the source method.
                let hasRestParams =
                    match typ with
                    | None -> false
                    | Some typ ->
                        typ.TryGetMembersFunctionsAndValues
                        |> Seq.tryFind (fun x -> x.CompiledName = over.Signature.Name)
                        |> function Some m -> hasRestParams m | None -> false
                let body = transformExpr com ctx over.Body
                let args = List.map Fable.Ident.getType args'
                let m = Fable.Member(name, kind, Fable.InstanceLoc, args, body.Type,
                            genParams = (over.GenericParameters |> List.map (fun x -> x.Name)),
                            hasRestParams = hasRestParams)
                m, args', body))
    let range = makeRangeFrom fsExpr
    let objExpr = Fable.ObjExpr (members, range)
    match capturedThis with
    | Some((_,Fable.Value(Fable.IdentValue capturedThis))::_) ->
        let varDecl = Fable.VarDeclaration(capturedThis, Fable.Value Fable.This, false, range)
        Fable.Sequential([varDecl; objExpr], range)
    | _ -> objExpr

let private transformDecisionTree (com: IFableCompiler) (ctx: Context) (fsExpr: FSharpExpr) decisionExpr (decisionTargets: (FSharpMemberOrFunctionOrValue list * FSharpExpr) list) =
    let rec getTargetRefsCount map = function
        | BasicPatterns.IfThenElse (_, thenExpr, elseExpr)
        | BasicPatterns.Let(_, BasicPatterns.IfThenElse (_, thenExpr, elseExpr)) ->
            let map = getTargetRefsCount map thenExpr
            getTargetRefsCount map elseExpr
        | BasicPatterns.Let(_, e) ->
            getTargetRefsCount map e
        | BasicPatterns.DecisionTreeSuccess (idx, _) ->
            match Map.tryFind idx map with
            | Some refCount -> Map.add idx (refCount + 1) map
            | None -> Map.add idx 1 map
        | e ->
            failwithf "Unexpected DecisionTree branch %O: %A" (makeRange e.Range) e
    let targetRefsCount = getTargetRefsCount (Map.empty<int,int>) decisionExpr
    if targetRefsCount |> Map.exists (fun _ v -> v > 1)
    then
        // If any of the decision targets is referred to more than once,
        // resolve the condition first and compile decision targets as a
        // switch to prevent code repetition (same target in different if branches)
        // or having to create inner functions
        let ctx = { ctx with decisionTargets = None }
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        let tempVar = com.GetUniqueVar() |> makeIdent
        let tempVarFirstItem =
            (Fable.Value(Fable.IdentValue tempVar), makeIntConst 0)
            ||> makeGet None (Fable.Number Int32)
        let cases =
            targetRefsCount
            |> Seq.map (fun kv ->
                let vars, body = decisionTargets.[kv.Key]
                let ctx =
                    let mutable i = 0
                    (ctx, vars) ||> List.fold (fun ctx var ->
                        i <- i + 1
                        (Fable.Value(Fable.IdentValue tempVar), makeIntConst i)
                        ||> makeGet None (makeType com ctx.typeArgs var.FullType)
                        |> bindExpr ctx var)
                [makeIntConst kv.Key], transformExpr com ctx body)
            |> Seq.toList
        [ Fable.VarDeclaration(tempVar, transformExpr com ctx decisionExpr, false, None)
          Fable.Switch(tempVarFirstItem, cases, None, typ, r) ]
        |> makeSequential r
    else
        let targets = targetRefsCount |> Map.map (fun k _ -> decisionTargets.[k])
        let ctx = { ctx with decisionTargets = Some targets }
        transformExpr com ctx decisionExpr

let private transformDecisionTreeSuccess (com: IFableCompiler) (ctx: Context) (range: SourceLocation) decisionTargets decIndex decBindings =
    match Map.tryFind decIndex decisionTargets with
    | None -> failwithf "Missing decision target %O" range
    | Some ([], Transform com ctx decBody) -> decBody
    // If we have values, bind them to context
    | Some (decVars, decBody) ->
        if not(List.sameLength decVars decBindings) then
            failwithf "Variables and bindings have different length %O" range
        let ctx =
            (ctx, decVars, decBindings)
            |||> List.fold2 (fun ctx var (Transform com ctx binding) ->
                bindExpr ctx var binding)
        transformExpr com ctx decBody

let private transformDelegate com ctx delegateType fsExpr =
    let wrapInZeroArgsLambda r typ (args: FSharpExpr list) fref =
        let args = List.map (transformExpr com ctx) args
        let captureThis = ctx.thisAvailability <> ThisUnavailable
        let body =
            Fable.Apply(fref, args, Fable.ApplyMeth, typ, r)
        Fable.Lambda([], body, Fable.LambdaInfo(captureThis)) |> Fable.Value
    let isSpecialCase t =
        tryDefinition t
        |> Option.bind (fun tdef -> tdef.TryFullName)
        |> Option.toBool (fun name -> name = "System.Func`1" || name = "System.Action")
    match fsExpr with
    // There are special cases (`Func` with one gen param and `Action` with no params)
    // the F# compiler translates as an application
    | BasicPatterns.Call(None,v,[],[],args)
    | BasicPatterns.Application(BasicPatterns.Value v,_,args)
    | BasicPatterns.Application(BasicPatterns.Application(BasicPatterns.Value v,_,args),_,_)
            when isSpecialCase delegateType ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeValueFrom com ctx r typ false v |> wrapInZeroArgsLambda r typ args
    | FlattenedLambda(args, tupleDestructs, body) ->
        transformLambda com ctx fsExpr args tupleDestructs body true
    | fsExpr -> transformExpr com ctx fsExpr

let private transformUnionCaseTest (com: IFableCompiler) (ctx: Context) fsExpr unionExpr (NonAbbreviatedType fsType) (unionCase: FSharpUnionCase) =
    let unionExpr = transformExpr com ctx unionExpr
    let checkCase propName right =
        let left = makeGet None Fable.String unionExpr (makeStrConst propName)
        makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [left; right] BinaryEqualStrict
    match fsType with
    | ErasedUnion ->
        let unionName = defaultArg fsType.TypeDefinition.TryFullName "unknown"
        if unionCase.UnionCaseFields.Count <> 1 then
            "Erased Union Cases must have one single field: " + unionName
            |> addErrorAndReturnNull com ctx.fileName (makeRange fsExpr.Range |> Some)
        else
            let fi = unionCase.UnionCaseFields.[0]
            if fi.FieldType.IsGenericParameter
            then
                let name = fi.FieldType.GenericParameter.Name
                let index =
                    fsType.TypeDefinition.GenericParameters
                    |> Seq.findIndex (fun arg -> arg.Name = name)
                fsType.GenericArguments |> Seq.item index
            else fi.FieldType
            |> makeType com ctx.typeArgs
            |> makeTypeTest com ctx.fileName (makeRangeFrom fsExpr) unionExpr
    | OptionUnion ->
        let opKind = if unionCase.Name = "None" then BinaryEqual else BinaryUnequal
        makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; Fable.Value Fable.Null] opKind
    | ListUnion ->
        let opKind = if unionCase.CompiledName = "Empty" then BinaryEqual else BinaryUnequal
        let expr = makeGet None Fable.Any unionExpr (makeStrConst "tail")
        makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [expr; Fable.Value Fable.Null] opKind
    | StringEnum ->
        makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; lowerCaseName unionCase] BinaryEqualStrict
    | OtherType ->
        getUnionCaseIndex fsType unionCase.Name
        |> makeIntConst
        |> checkCase "tag"

let private transformSwitch com ctx (fsExpr: FSharpExpr) (matchValue: FSharpMemberOrFunctionOrValue)
                                isUnionType cases (defaultCase, defaultBindings) decisionTargets =
    let decisionTargets = decisionTargets |> Seq.mapi (fun i d -> (i, d)) |> Map
    let r, typ = makeRange fsExpr.Range, makeType com ctx.typeArgs fsExpr.Type
    let cases =
        cases
        |> Seq.map (fun (KeyValue(idx, (bindings, cases))) ->
            let labels = cases |> List.map (function Choice1Of2 i -> makeIntConst i | Choice2Of2 s -> makeStrConst s)
            let body = transformDecisionTreeSuccess com ctx r decisionTargets idx bindings
            labels, body)
        |> Seq.toList
    let defaultCase =
        transformDecisionTreeSuccess com ctx r decisionTargets defaultCase defaultBindings
    let matchValue =
        let matchValueType = makeType com ctx.typeArgs matchValue.FullType
        let matchValue = makeValueFrom com ctx None matchValueType false matchValue
        if isUnionType
        then makeGet None Fable.String matchValue (makeStrConst "tag")
        else matchValue
    Fable.Switch(matchValue, cases, Some defaultCase, typ, Some r)

let private transformExpr (com: IFableCompiler) (ctx: Context) fsExpr =
    match fsExpr with
    (** ## Erased *)
    | MaybeWrapped(Transform com ctx expr) -> expr

    // TODO: Some cases of coertion shouldn't be erased
    // string :> seq #1279
    // list (and others) :> seq in Fable 2.0
    // concrete type :> interface in Fable 2.0
    | BasicPatterns.Coerce(_targetType, Transform com ctx inpExpr) -> inpExpr

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, Transform com ctx lambda) -> lambda

    (** ## Custom patterns *)
    | SpecialValue com ctx replacement ->
        replacement

    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOfLoop (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        Fable.ForOf (ident, value, transformExpr com newContext body)
        |> makeLoop (makeRangeFrom fsExpr)

    | TryGetValue (callee, meth, typArgs, methTypArgs, methArgs) ->
        let callee, args = Option.map (com.Transform ctx) callee, List.map (com.Transform ctx) methArgs
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) callee args

    | CreateEvent (callee, eventName, meth, typArgs, methTypArgs, methArgs) ->
        let callee, args = com.Transform ctx callee, List.map (com.Transform ctx) methArgs
        let callee = Fable.Apply(callee, [makeStrConst eventName], Fable.ApplyGet, Fable.Any, None)
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) (Some callee) args

    | CheckArrayLength (Transform com ctx arr, length, FableType com ctx typ) ->
        let r = makeRangeFrom fsExpr
        let lengthExpr = Fable.Apply(arr, [makeStrConst "length"], Fable.ApplyGet, Fable.Number Int32, r)
        makeEqOp r [lengthExpr; makeTypeConst typ length] BinaryEqualStrict

    | JsThis ->
        if ctx.thisAvailability <> ThisUnavailable then
            "JS `this` is already captured in this context, try to use it in a module function"
            |> addWarning com ctx.fileName (makeRange fsExpr.Range |> Some)
        Fable.Value Fable.This

    (** ## Flow control *)
    | BasicPatterns.FastIntegerForLoop(Transform com ctx start, Transform com ctx limit, body, isUp) ->
        match body with
        | BasicPatterns.Lambda (BindIdent com ctx (newContext, ident), body) ->
            Fable.For (ident, start, limit, com.Transform newContext body, isUp)
            |> makeLoop (makeRangeFrom fsExpr)
        | _ -> failwithf "Unexpected loop %O: %A" (makeRange fsExpr.Range) fsExpr

    | BasicPatterns.WhileLoop(Transform com ctx guardExpr, Transform com ctx bodyExpr) ->
        Fable.While (guardExpr, bodyExpr)
        |> makeLoop (makeRangeFrom fsExpr)

    (** Values *)
    | BasicPatterns.Const(value, FableType com ctx typ) ->
        let e = makeTypeConst typ value
        if e.Type = typ then
            e
        else
            // Enumerations are compiled as const but they have a different type
            Replacements.checkLiteral com ctx.fileName (makeRangeFrom fsExpr) value typ
            Fable.Wrapped (e, typ)

    | BasicPatterns.BaseValue _typ ->
        Fable.Super |> Fable.Value

    | BasicPatterns.ThisValue _typ ->
        makeThisRef com ctx (makeRangeFrom fsExpr) None

    | BasicPatterns.Value var ->
        if var.IsMemberThisValue
        then Some var |> makeThisRef com ctx (makeRangeFrom fsExpr)
        elif isInline var
        then
            match ctx.scopedInlines |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
            | Some (_,fsExpr) -> com.Transform ctx fsExpr
            | None ->
                "Cannot resolve locally inlined value: " + var.DisplayName
                |> addErrorAndReturnNull com ctx.fileName (makeRange fsExpr.Range |> Some)
        else
            let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
            makeValueFrom com ctx r typ true var

    | BasicPatterns.DefaultValue (FableType com ctx typ) ->
        let valueKind =
            match typ with
            | Fable.Boolean -> Fable.BoolConst false
            | Fable.Number kind -> Fable.NumberConst (0., kind)
            | _ -> Fable.Null
        Fable.Value valueKind

    (** ## Assignments *)
    | ImmutableBinding((var, value), body) ->
        transformExpr com ctx value |> bindExpr ctx var |> transformExpr com <| body

    | BasicPatterns.Let((var, value), body) ->
        if isInline var then
            let ctx = { ctx with scopedInlines = (var, value)::ctx.scopedInlines }
            transformExpr com ctx body
        else
            let r = makeRangeFrom fsExpr
            let value = transformExpr com ctx value
            let ctx, ident = bindIdent com ctx value.Type (Some var) var.CompiledName
            let body = transformExpr com ctx body
            let assignment = Fable.VarDeclaration(ident, value, var.IsMutable, r)
            makeSequential r [assignment; body]

    | BasicPatterns.LetRec(recBindings, body) ->
        let range = makeRangeFrom fsExpr
        let ctx, idents =
            (recBindings, (ctx, [])) ||> List.foldBack (fun (var,_) (ctx, idents) ->
                let (BindIdent com ctx (newContext, ident)) = var
                (newContext, ident::idents))
        let assignments =
            recBindings
            |> List.map2 (fun ident (var, Transform com ctx binding) ->
                Fable.VarDeclaration(ident, binding, var.IsMutable, range)) idents
        assignments @ [transformExpr com ctx body]
        |> makeSequential range

    (** ## Applications *)
    | BasicPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, _argTypes2, argExprs) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        addErrorAndReturnNull com ctx.fileName r "TODO: TraitCalls"

    | BasicPatterns.Call(callee, meth, typArgs, methTypArgs, args) ->
        let callee = Option.map (com.Transform ctx) callee
        let args = List.map (transformExpr com ctx) args
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) callee args

    // Application of locally inlined lambdas
    | BasicPatterns.Application(BasicPatterns.Value var, typeArgs, args) when isInline var ->
        let range = makeRange fsExpr.Range
        match ctx.scopedInlines |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
        | Some (_,fsExpr) ->
            let resolvedCtx = { ctx with typeArgs = matchGenericParams ctx var ([], typeArgs) }
            let callee = com.Transform resolvedCtx fsExpr
            match args with
            | [] -> callee
            | args ->
                let typ = makeType com ctx.typeArgs fsExpr.Type
                let args = List.map (transformExpr com ctx) args
                makeApply com (Some range) typ callee args
        | None ->
            "Cannot resolve locally inlined value: " + var.DisplayName
            |> addErrorAndReturnNull com ctx.fileName (Some range)

    | FlattenedApplication(Transform com ctx callee, _typeArgs, args) ->
        let typ, range = makeType com ctx.typeArgs fsExpr.Type, makeRangeFrom fsExpr
        let args = List.map (transformExpr com ctx) args
        match callee.Type with
        | Fable.DeclaredType(ent,_) when ent.FullName = "Fable.Core.Applicable" ->
            let args =
                match args with
                | [Fable.Value(Fable.TupleConst args)] -> args
                | args -> args
            Fable.Apply(callee, args, Fable.ApplyMeth, typ, range)
        | _ ->
        makeApply com range typ callee args

    | BasicPatterns.IfThenElse (Transform com ctx guardExpr, Transform com ctx thenExpr, Transform com ctx elseExpr) ->
        Fable.IfThenElse (guardExpr, thenExpr, elseExpr, makeRangeFrom fsExpr)

    | BasicPatterns.TryFinally (BasicPatterns.TryWith(body, _, _, catchVar, catchBody),finalBody) ->
        makeTryCatch com ctx fsExpr body (Some (catchVar, catchBody)) (Some finalBody)

    | BasicPatterns.TryFinally (body, finalBody) ->
        makeTryCatch com ctx fsExpr body None (Some finalBody)

    | BasicPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        makeTryCatch com ctx fsExpr body (Some (catchVar, catchBody)) None

    | BasicPatterns.Sequential (Transform com ctx first, Transform com ctx second) ->
        makeSequential (makeRangeFrom fsExpr) [first; second]

    (** ## Lambdas *)
    | BasicPatterns.NewDelegate(delegateType, fsExpr) ->
        transformDelegate com ctx delegateType fsExpr

    | FlattenedLambda(args, tupleDestructs, body) ->
        transformLambda com ctx fsExpr args tupleDestructs body false

    (** ## Getters and Setters *)

    // When using a self reference in constructor (e.g. `type MyType() as self =`)
    // the F# compiler wraps self references with code we don't need
    | BasicPatterns.FSharpFieldGet(Some ThisVar, RefType _, _)
    | BasicPatterns.FSharpFieldGet(Some(BasicPatterns.FSharpFieldGet(Some ThisVar, _, _)), RefType _, _) ->
        makeThisRef com ctx (makeRangeFrom fsExpr) None

    | BasicPatterns.FSharpFieldGet (callee, calleeType, FieldName fieldName) ->
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeType com ctx.typeArgs calleeType
                        |> makeNonGenTypeRef com
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeGetFrom r typ callee (makeStrConst fieldName)

    | BasicPatterns.TupleGet (_tupleType, tupleElemIndex, Transform com ctx tupleExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeGetFrom r typ tupleExpr (makeIntConst tupleElemIndex)

    | BasicPatterns.UnionCaseGet (Transform com ctx unionExpr, fsType, unionCase, FieldName fieldName) ->
        let typ, range = makeType com ctx.typeArgs fsExpr.Type, makeRangeFrom fsExpr
        match fsType with
        | ErasedUnion ->
            Fable.Wrapped(unionExpr, typ)
        | OptionUnion ->
            CoreLibCall("Option", Some "getValue", false, [unionExpr])
            |> makeCall range typ
        | ListUnion ->
            makeGet range typ unionExpr (Naming.lowerFirst fieldName |> makeStrConst)
        | StringEnum ->
            "StringEnum types cannot have fields"
            |> addErrorAndReturnNull com ctx.fileName range
        | OtherType ->
            if unionCase.UnionCaseFields.Count > 1 then
                let i = unionCase.UnionCaseFields |> Seq.findIndex (fun x -> x.Name = fieldName)
                let data = makeGet range typ unionExpr (makeStrConst "data")
                makeGet range typ data (makeIntConst i)
            else
                makeGet range typ unionExpr (makeStrConst "data")

    // When using a self reference in constructor (e.g. `type MyType() as self =`)
    // the F# compiler introduces artificial statements that we must ignore
    | BasicPatterns.FSharpFieldSet(Some(ThisVar _), RefType _, _, _)
    | BasicPatterns.FSharpFieldSet(Some(BasicPatterns.FSharpFieldGet(Some(ThisVar _), _, _)), RefType _, _, _) ->
        Fable.Value Fable.Null

    | BasicPatterns.FSharpFieldSet (callee, calleeType, FieldName fieldName, Transform com ctx value) ->
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None ->
                let calleeType = makeType com ctx.typeArgs calleeType
                makeNonGenTypeRef com calleeType
        Fable.Set (callee, Some (makeStrConst fieldName), value, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTag (Transform com ctx unionExpr, _unionType) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeGetFrom r typ unionExpr (makeStrConst "tag")

    | BasicPatterns.UnionCaseSet (Transform com ctx unionExpr, _type, _case, _caseField, _valueExpr) ->
        makeRange fsExpr.Range |> failwithf "Unexpected UnionCaseSet %O"

    | BasicPatterns.ValueSet (valToSet, Transform com ctx valueExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs valToSet.FullType
        match tryEnclosingEntity valToSet with
        | Some ent when ent.IsFSharpModule ->
            // Mutable module values are compiled as functions, because values
            // imported from ES2015 modules cannot be modified (see #986)
            // TODO: We should check for plugin, emit attribute...
            let callee = makeTypeFromDef com ctx.typeArgs ent [] |> makeNonGenTypeRef com
            let m = makeGet r Fable.Any callee (sanitizeMethodName valToSet |> makeStrConst)
            Fable.Apply(m, [valueExpr], Fable.ApplyMeth, typ, r)
        | _ ->
            let valToSet = makeValueFrom com ctx r typ false valToSet
            Fable.Set (valToSet, None, valueExpr, r)

    (** Instantiation *)
    | BasicPatterns.NewArray(FableType com ctx elTyp, arrExprs) ->
        makeArray elTyp (arrExprs |> List.map (transformExpr com ctx))

    | BasicPatterns.NewTuple(_, argExprs) ->
        argExprs |> List.map (transformExpr com ctx) |> Fable.TupleConst |> Fable.Value

    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, otherOverrides) ->
        transformObjExpr com ctx fsExpr objType baseCallExpr overrides otherOverrides

    | BasicPatterns.NewObject(meth, typArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        List.map (com.Transform ctx) args
        |> makeCallFrom com ctx r typ meth (typArgs, []) None

    | BasicPatterns.NewRecord(fsType, argExprs) ->
        let range = makeRangeFrom fsExpr
        let tdef = tryDefinition fsType
        let argExprs =
            let argExprs = List.map (transformExpr com ctx) argExprs
            match tdef with
            | Some tdef ->
                tdef.FSharpFields
                |> Seq.map (fun x -> makeType com [] x.FieldType)
                |> fun argTypes -> ensureArity com (Seq.toList argTypes) argExprs
            | None -> argExprs
        // TODO: Build Pojo
        // List.zip (Seq.toList tdef.FSharpFields) argExprs
        // |> List.map (fun (fi, e) -> fi.Name, e)
        // |> makeJsObject range
        let recordType = makeType com ctx.typeArgs fsType
        buildApplyInfo com ctx range recordType recordType recordType.FullName
            ".ctor" Fable.Constructor ([],[],[],[]) (None, argExprs)
        |> tryBoth (tryPlugin com) (tryReplace com ctx tdef)
        |> function
        | Some repl -> repl
        | None -> Fable.Apply(makeNonGenTypeRef com recordType, argExprs, Fable.ApplyCons,
                        makeType com ctx.typeArgs fsExpr.Type, range)

    | BasicPatterns.NewUnionCase(fsType, unionCase, argExprs) ->
        match fsType with
        | ListType _ -> transformNewList com ctx fsExpr fsType argExprs
        | _ -> List.map (com.Transform ctx) argExprs
                |> transformNonListNewUnionCase com ctx fsExpr fsType unionCase

    (** ## Type test *)
    | BasicPatterns.TypeTest (FableType com ctx typ, Transform com ctx expr) ->
        makeTypeTest com ctx.fileName (makeRangeFrom fsExpr) expr typ

    | BasicPatterns.UnionCaseTest(unionExpr, fsType, unionCase) ->
        transformUnionCaseTest com ctx fsExpr unionExpr fsType unionCase

    (** Pattern Matching *)
    | Switch(matchValue, isUnionType, cases, defaultCase, decisionTargets) ->
        transformSwitch com ctx fsExpr matchValue isUnionType cases defaultCase decisionTargets

    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        transformDecisionTree com ctx fsExpr decisionExpr decisionTargets

    | BasicPatterns.DecisionTreeSuccess (decIndex, decBindings) ->
        match ctx.decisionTargets with
        | Some decisionTargets ->
            transformDecisionTreeSuccess com ctx (makeRange fsExpr.Range) decisionTargets decIndex decBindings
        | None ->
            decBindings
            |> List.map (transformExpr com ctx)
            |> List.append [makeIntConst decIndex]
            |> makeArray Fable.Any

    | BasicPatterns.Quote(Transform com ctx expr) ->
        Fable.Quote(expr)

    (** Not implemented *)
    | BasicPatterns.ILAsm _
    | BasicPatterns.ILFieldGet _
    | BasicPatterns.ILFieldSet _
    | BasicPatterns.AddressSet _ // (lvalueExpr, rvalueExpr)
    | _ -> failwithf "Cannot compile expression in %O: %A"
                     (makeRange fsExpr.Range) fsExpr

let private isErasedEntity com (ctx: Context) (ent: FSharpEntity) =
    let fail (ent: FSharpEntity) msg =
        addError com ctx.fileName (getEntityLocation ent |> makeRange |> Some) msg; false
    let check (ent: FSharpEntity) name att expected =
        if name <> att
        then false
        elif (List.contains "union" expected && not ent.IsFSharpUnion)
            && (List.contains "record" expected && not ent.IsFSharpRecord)
        then fail ent (sprintf "%s can only decorate %s types" att (String.concat "/" expected))
        else true
    ent.Attributes |> tryFindAtt (fun name ->
        name = Atts.import
        || name = Atts.global_
        || name = Atts.erase
        || check ent name Atts.stringEnum ["union"])
    |> Option.isSome

let private isIgnoredEntity com ctx (ent: FSharpEntity) =
    ent.IsInterface
    || ent.IsEnum
    || ent.IsFSharpAbbreviation
    || isAttributeEntity ent
    || isErasedEntity com ctx ent

/// Is compiler generated (CompareTo...) or belongs to ignored entity?
/// (remember F# compiler puts class methods in enclosing modules)
let private isIgnoredMethod (meth: FSharpMemberOrFunctionOrValue) =
    (meth.IsCompilerGenerated && Naming.ignoredCompilerGenerated.Contains meth.CompiledName)
        || Option.isSome meth.LiteralValue
        || Option.isSome(meth.Attributes |> tryFindAtt (fun name ->
            name = Atts.import || name = Atts.global_ || name = Atts.emit || name = Atts.erase))
        || Naming.ignoredInterfaceMethods.Contains meth.CompiledName

let private tryGetImport (atts: #seq<FSharpAttribute>) =
    try
        tryFindAtt ((=) Atts.import) atts |> Option.bind (fun att ->
            if att.ConstructorArguments.Count = 2 then
                match att.ConstructorArguments.[0], att.ConstructorArguments.[1] with
                | (_, (:? string as selector)), (_, (:? string as path)) -> Some(selector, path)
                | _ -> None
            else None)
    with _ -> None

// When a member returns a function, there are issues with the uncurrying
// optimization when calling & applying at once (See #1041, #1154)
let private (|MultiArgFunction|_|) (ctx: Context) (body: FSharpExpr) (fableBody: Fable.Expr) =
    let hasNoTupledArgs (meth: FSharpMemberOrFunctionOrValue) =
        (false, meth.CurriedParameterGroups) ||> Seq.fold (fun hasTuple g ->
            hasTuple || g.Count > 1) |> not
    let funcBodyArgs =
        if body.Type.IsFunctionType
        then getFunctionGenericArgs [] ctx.typeArgs true body.Type |> List.length
        else 0
    if funcBodyArgs > 0 then
        if funcBodyArgs > 1
        then makeDynamicCurriedLambda fableBody.Range fableBody.Type fableBody |> Some
        else None
    else None

let private transformMethod com ctx range import meth args (body: FSharpExpr) =
    let memberName = sanitizeMethodName meth
    let memberLoc = getMemberLoc meth
    let ctx, privateName =
        // Bind module member names to context to prevent
        // name clashes (they will become variables in JS)
        if isModuleMember meth then
            let typ = makeType com ctx.typeArgs meth.FullType
            let ctx, privateName = bindIdent com ctx typ (Some meth) memberName
            ctx, Some (privateName.Name)
        else ctx, None
    let memberKind, args, extraArgs, body =
        match import with
        | Some(selector, path) ->
            Fable.Field, [], [], makeImport selector path
        | None ->
            let info = { isInstance = meth.IsInstanceMember
                         passGenerics = hasPassGenericsAtt com ctx meth }
            let ctx, _, args, extraArgs = bindMemberArgs com ctx info args
            let ctx =
                if memberLoc <> Fable.StaticLoc
                then { ctx with thisAvailability = ThisAvailable }
                else ctx
            if meth.IsImplicitConstructor then
                // TODO: compileDerivedConstructor?
                let body = transformExpr com ctx body
                Fable.Constructor, args, extraArgs, body
            else
                let fableBody = transformExpr com ctx body
                match fableBody with
                // Accept import expressions used instead of attributes, for example:
                // let foo x y = import "foo" "myLib"
                | Fable.Value(Fable.ImportRef(selector, path, importKind)) ->
                    let fableBody =
                        if selector = Naming.placeholder
                        then Fable.Value(Fable.ImportRef(meth.DisplayName, path, importKind))
                        else fableBody
                    Fable.Field, [], [], fableBody
                | MultiArgFunction ctx body fableBody ->
                    getMemberKind meth, args, extraArgs, fableBody
                | fableBody ->
                    getMemberKind meth, args, extraArgs, fableBody
    let entMember =
        let argTypes = List.map Fable.Ident.getType args
        let fullTyp = makeOriginalCurriedType com meth.CurriedParameterGroups body.Type
        let tryGetMember (e: Fable.Entity) = e.TryGetMember(memberName, memberKind, memberLoc, argTypes)
        match tryEnclosingEntity meth with
        | Some (FableEntity com (Try tryGetMember m)) -> m
        | _ -> makeMethodFrom com memberName memberKind memberLoc argTypes body.Type fullTyp None meth
    let entMember = Fable.MemberDeclaration(entMember, isPublicMethod meth, privateName, args@extraArgs, body, Some range)
    ctx, Some entMember

let private transformMemberDecl (com: IFableCompiler) (ctx: Context) (meth: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let range = getMethLocation meth |> makeRange
    let import = tryGetImport meth.Attributes
    if Option.isSome import then
        transformMethod com ctx range import meth args body
    elif isIgnoredMethod meth then
        ctx, None
    elif isInline meth then
        let args = Seq.collect id args |> countRefs body
        com.AddInlineExpr(fullNameAndArgCount meth, (upcast args, body))
        ctx, None
    else transformMethod com ctx range None meth args body

let rec private transformEntityDecl (com: IFableCompiler) (ctx: Context) (ent: FSharpEntity) subDecls =
    let range = getEntityLocation ent |> makeRange
    let import = tryGetImport ent.Attributes
    if Option.isSome import then
        let selector, path = import.Value
        let isPublic = isPublicEntity ctx ent
        let entName, body = ent.CompiledName, makeImport selector path
        // Bind entity name to context to prevent name clashes
        let ctx, ident = bindIdentWithExactName com ctx Fable.Any None entName
        let m = Fable.Member(entName, Fable.Field, Fable.StaticLoc, [], body.Type)
        let decl = Fable.MemberDeclaration(m, isPublic, Some ident.Name, [], body, Some range)
        ctx, Some decl
    elif isIgnoredEntity com ctx ent then
        ctx, None
    else
        let childDecls =
            let ctx = { ctx with enclosingModule = EnclosingModule(com.GetEntity ent, isPublicEntity ctx ent) }
            transformDeclarations com ctx subDecls
        if List.isEmpty childDecls && ent.IsFSharpModule
        then
            ctx, None
        else
            // Bind entity name to context to prevent name clashes (it will become a variable in JS)
            let ctx, _ident = bindIdentWithExactName com ctx Fable.Any None ent.CompiledName
            // TODO: declInfo.AddChild(com, ctx, ent, ident.Name, childDecls)
            ctx, None

and private transformDeclarations (com: IFableCompiler) (ctx: Context) fsDecls =
    let _ctx, fableDecls =
        ((ctx, []), fsDecls) ||> List.fold (fun (ctx, fableDecls) fsDecl ->
            let ctx, fableDecl =
                match fsDecl with
                | FSharpImplementationFileDeclaration.Entity (e, sub) ->
                    transformEntityDecl com ctx e sub
                | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
                    transformMemberDecl com ctx meth args body
                | FSharpImplementationFileDeclaration.InitAction fe ->
                    let e = com.Transform ctx fe
                    let decl = Fable.ActionDeclaration (e, makeRangeFrom fe)
                    ctx, Some decl
            match fableDecl with
            | Some d -> ctx, d::fableDecls
            | None -> ctx, fableDecls)
    List.rev fableDecls

let private getRootModuleAndDecls decls =
    let (|CommonNamespace|_|) = function
        | (FSharpImplementationFileDeclaration.Entity(ent, subDecls))::restDecls
            when ent.IsNamespace ->
            let commonName = ent.CompiledName
            (Some subDecls, restDecls) ||> List.fold (fun acc decl ->
                match acc, decl with
                | (Some subDecls), (FSharpImplementationFileDeclaration.Entity(ent, subDecls2)) ->
                    if ent.CompiledName = commonName
                    then Some(subDecls@subDecls2)
                    else None
                | _ -> None)
            |> Option.map (fun subDecls -> ent, subDecls)
        | _ -> None
    let rec getRootModuleAndDecls outerEnt decls =
        match decls with
        | [FSharpImplementationFileDeclaration.Entity (ent, decls)]
                when ent.IsFSharpModule || ent.IsNamespace ->
            getRootModuleAndDecls (Some ent) decls
        | CommonNamespace(ent, decls) ->
            getRootModuleAndDecls (Some ent) decls
        | decls -> outerEnt, decls
    getRootModuleAndDecls None decls

let private tryGetMethodArgsAndBody (implFiles: Map<string, FSharpImplementationFileContents>)
                                    fileName (meth: FSharpMemberOrFunctionOrValue) =
    let rec tryGetMethodArgsAndBody' (methFullName: string) = function
        | FSharpImplementationFileDeclaration.Entity (e, decls) ->
            let entFullName = getEntityFullName e
            if methFullName.StartsWith(entFullName)
            then List.tryPick (tryGetMethodArgsAndBody' methFullName) decls
            else None
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth2, args, body) ->
            if methFullName = meth2.FullName
            then Some(args, body)
            else None
        | FSharpImplementationFileDeclaration.InitAction _ -> None
    Map.tryFind fileName implFiles
    |> Option.bind (fun f ->
        f.Declarations |> List.tryPick (tryGetMethodArgsAndBody' meth.FullName))

let private tryGetEntityImplementation (implFiles: Map<string, FSharpImplementationFileContents>) (ent: FSharpEntity) =
    let rec tryGetEntityImplementation' (entFullName: string) = function
        | FSharpImplementationFileDeclaration.Entity (e, decls) ->
            let entFullName2 = getEntityFullName e
            if entFullName = entFullName2
            then Some e
            elif entFullName.StartsWith(entFullName2)
            then List.tryPick (tryGetEntityImplementation' entFullName) decls
            else None
        | _ -> None
    // For entities in checked project (no assembly), try to find the implementation entity
    // when the declaration location doesn't correspond to the implementation location (see #754)
    match ent.Assembly.FileName, ent.ImplementationLocation with
    | None, Some loc when ent.DeclarationLocation.FileName <> loc.FileName ->
        let fileName = Path.normalizePath loc.FileName
        Map.tryFind fileName implFiles
        |> Option.bind (fun f ->
            let entFullName = getEntityFullName ent
            f.Declarations |> List.tryPick (tryGetEntityImplementation' entFullName))
    | _ -> Some ent

type FableCompiler(com: ICompiler, state: ICompilerState, currentFile: string, implFiles: Map<string, FSharpImplementationFileContents>) =
    let replacePlugins =
        com.Plugins |> List.choose (function
            | { path=path; plugin=(:? IReplacePlugin as plugin)} -> Some (path, plugin)
            | _ -> None)
    let usedVarNames = HashSet<string>()
    let dependencies = HashSet<string>()
    member __.UsedVarNames = set usedVarNames
    member __.Dependencies = set dependencies
    interface IFableCompiler with
        member fcom.Transform ctx fsExpr =
            transformExpr fcom ctx fsExpr
        member __.IsReplaceCandidate ent =
            match ent.TryFullName, ent.Assembly.FileName with
            // TODO: Temporary HACK to fix #577
            | Some fullName, _ when fullName.StartsWith("Fable.Import") -> false
            | Some fullName, _ when fullName.StartsWith("Fable.Core.JsInterop") -> true // needed for REPL
            | _, Some asmPath when not(System.String.IsNullOrEmpty(asmPath)) -> true
            | _ -> false
        member fcom.TryGetInternalFile tdef =
            if (fcom :> IFableCompiler).IsReplaceCandidate tdef
            then None
            else Some (getEntityLocation tdef).FileName
        member fcom.GetEntity tdef =
            // TODO: Add dependency too for entities?
            state.GetOrAddEntity(getEntityFullName tdef, fun () ->
                match tryGetEntityImplementation implFiles tdef with
                | Some tdef -> makeEntity fcom tdef
                | None -> failwith ("Cannot find implementation of " + (getEntityFullName tdef)))
        member __.GetInlineExpr meth =
            let fileName = (getMethLocation meth).FileName |> Path.normalizePath
            if fileName <> currentFile then
                dependencies.Add(fileName) |> ignore
            state.GetOrAddInlineExpr(fullNameAndArgCount meth, fun () ->
                match tryGetMethodArgsAndBody implFiles fileName meth with
                | Some(args, body) ->
                    let args = Seq.collect id args |> countRefs body
                    (upcast args, body)
                | None ->
                    failwith ("Cannot find inline method " + meth.FullName))
        member __.AddInlineExpr(fullName, inlineExpr) =
            state.GetOrAddInlineExpr(fullName, fun () -> inlineExpr) |> ignore
        member __.AddUsedVarName varName =
            usedVarNames.Add varName |> ignore
        member __.ReplacePlugins =
            replacePlugins
    interface ICompiler with
        member __.Options = com.Options
        member __.Plugins = com.Plugins
        member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)
        member __.GetUniqueVar() = com.GetUniqueVar()

let getRootModuleFullName (file: FSharpImplementationFileContents) =
    let rootEnt, _ = getRootModuleAndDecls file.Declarations
    match rootEnt with
    | Some rootEnt -> getEntityFullName rootEnt
    | None -> ""

let transformFile (com: ICompiler) (state: ICompilerState)
                  (implFiles: Map<string, FSharpImplementationFileContents>)
                  (fileName: string) =
    try
        let file =
            // TODO: This shouldn't be necessary, but just in case
            let fileName = Path.normalizeFullPath fileName
            match Map.tryFind fileName implFiles with
            | Some file -> file
            | None -> failwithf "File %s doesn't belong to parsed project %s" fileName state.ProjectFile
        let fcom = FableCompiler(com, state, fileName, implFiles)
        let rootEnt, rootDecls =
            let fcom = fcom :> IFableCompiler
            let rootEnt, rootDecls = getRootModuleAndDecls file.Declarations
            match rootEnt with
            | Some e ->
                let rootEnt = fcom.GetEntity e
                let ctx = Context.Create(fileName, rootEnt)
                rootEnt, transformDeclarations fcom ctx rootDecls
            | None ->
                let emptyRootEnt = Fable.Entity.CreateRootModule fileName
                let ctx = Context.Create(fileName, emptyRootEnt)
                emptyRootEnt, transformDeclarations fcom ctx rootDecls
        Fable.File(fileName, rootEnt, rootDecls,
            usedVarNames=fcom.UsedVarNames, dependencies=fcom.Dependencies)
    with
    | ex -> exn (sprintf "%s (%s)" ex.Message fileName, ex) |> raise