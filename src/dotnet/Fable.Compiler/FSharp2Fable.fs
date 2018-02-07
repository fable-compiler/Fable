module rec Fable.FSharp2Fable.Compiler

#if !FABLE_COMPILER
open System.IO
#endif
open System
open System.Collections.Generic

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.AST.Fable.Util

open Patterns
open TypeHelpers
open Identifiers
open Helpers
open Util

let private transformLambda com ctx arg body =
    let ctx, args = makeFunctionArgs com ctx [arg]
    match args with
    | [arg] -> Fable.Function(Fable.Lambda arg, transformExpr com ctx body)
    | _ -> failwith "makeFunctionArgs returns args with different length"

let private transformNewUnion com ctx (fsExpr: FSharpExpr) fsType
                (unionCase: FSharpUnionCase) (argExprs: Fable.Expr list) =
    match fsType with
    | ErasedUnion ->
        match argExprs with
        | [expr] ->
            let genArgs = makeGenArgs com ctx.typeArgs fsType.GenericArguments
            Fable.ErasedUnionConst(expr, genArgs) |> Fable.Const
        | _ -> "Erased Union Cases must have one single field: " + (getFsTypeFullName fsType)
               |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)
    | StringEnum ->
        match argExprs with
        | [] -> lowerCaseName unionCase
        | _ -> "StringEnum types cannot have fields: " + (getFsTypeFullName fsType)
               |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)
    | OptionUnion typ ->
        let typ = makeType com ctx.typeArgs typ
        let expr =
            match argExprs with
            | [] -> None
            | [expr] -> Some expr
            | _ -> failwith "Unexpected args for Option constructor"
        Fable.OptionConst(expr, typ) |> Fable.Const
    | ListUnion typ ->
        let typ = makeType com ctx.typeArgs typ
        let headAndTail =
            match argExprs with
            | [] -> None
            | [head; tail] -> Some(head, tail)
            | _ -> failwith "Unexpected args for List constructor"
        Fable.ListConst(headAndTail, typ) |> Fable.Const
    | DiscriminatedUnion tdef ->
        let genArgs = makeGenArgs com ctx.typeArgs fsType.GenericArguments
        Fable.UnionConst(unionCase, argExprs, tdef, genArgs) |> Fable.Const

let private transformObjExpr (com: IFableCompiler) (ctx: Context) (fsExpr: FSharpExpr) (objType: FSharpType)
                    (_baseCallExpr: FSharpExpr) (overrides: FSharpObjectExprOverride list) otherOverrides =
    // let members =
    //     (objType, overrides)::otherOverrides
    //     |> List.collect (fun (typ, overrides) ->
    //         let overrides =
    //             if not typ.HasTypeDefinition then overrides else
    //             let typName = typ.TypeDefinition.FullName.Replace(".","-")
    //             overrides |> List.where (fun x ->
    //                 typName + "-" + x.Signature.Name
    //                 |> Naming.ignoredInterfaceMethods.Contains
    //                 |> not)
    //         overrides |> List.map (fun over ->
    //             // TODO: Check if we should pass generics?
    //             let ctx, args' = bindMemberArgs com ctx false over.CurriedParameterGroups
    //             // Don't use the typ argument as the override may come
    //             // from another type, like ToString()
    //             let typ =
    //                 if over.Signature.DeclaringType.HasTypeDefinition
    //                 then Some over.Signature.DeclaringType.TypeDefinition
    //                 else None
    //             // TODO: Check for indexed getter and setter also in object expressions?
    //             let name = over.Signature.Name |> Naming.removeGetSetPrefix
    //             let kind =
    //                 match over.Signature.Name with
    //                 | Naming.StartsWith "get_" _ -> Fable.Getter
    //                 | Naming.StartsWith "set_" _ -> Fable.Setter
    //                 | _ -> Fable.Method
    //             // FSharpObjectExprOverride.CurriedParameterGroups doesn't offer
    //             // information about ParamArray, we need to check the source method.
    //             let hasRestParams =
    //                 match typ with
    //                 | None -> false
    //                 | Some typ ->
    //                     typ.TryGetMembersFunctionsAndValues
    //                     |> Seq.tryFind (fun x -> x.CompiledName = over.Signature.Name)
    //                     |> function Some m -> hasRestParams m | None -> false
    //             let body = transformExpr com ctx over.Body
    //             let args = List.map Fable.Ident.getType args'
    //             let m = Fable.Member(name, kind, Fable.InstanceLoc, args, body.Type,
    //                         genParams = (over.GenericParameters |> List.map (fun x -> x.Name)),
    //                         hasRestParams = hasRestParams)
    //            m, args', body))
    Fable.ObjectExpr([], makeType com ctx.typeArgs fsExpr.Type)

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
    if Map.exists (fun _ v -> v > 1) targetRefsCount then
        // If any of the decision targets is referred to more than once,
        // resolve the condition first and compile decision targets as a
        // switch to prevent code repetition (same target in different if branches)
        // or having to create inner functions
        let ctx = { ctx with decisionTargets = None }
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        let tempVar = com.GetUniqueVar() |> makeIdent
        let tempVarFirstItem =
            (Fable.IdentExpr tempVar, makeIntConst 0)
            ||> makeGet None (Fable.Number Int32)
        let cases =
            targetRefsCount
            |> Seq.map (fun kv ->
                let vars, body = decisionTargets.[kv.Key]
                let ctx =
                    let mutable i = 0
                    (ctx, vars) ||> List.fold (fun ctx var ->
                        i <- i + 1
                        (Fable.IdentExpr tempVar, makeIntConst i)
                        ||> makeGet None (makeType com ctx.typeArgs var.FullType)
                        |> bindExpr ctx var)
                [makeIntConst kv.Key], transformExpr com ctx body)
            |> Seq.toList
        let bindings = [tempVar, transformExpr com ctx decisionExpr]
        Fable.Let(bindings, Fable.Switch(tempVarFirstItem, cases, None, typ, r))
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
        let body = Fable.Apply(fref, args, r)
        Fable.Lambda([], body, r)
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
    | BasicPatterns.Lambda(arg, body) ->
        transformLambda com ctx [arg] body
    | fsExpr -> transformExpr com ctx fsExpr

let private transformUnionCaseTest (com: IFableCompiler) (ctx: Context) (fsExpr: FSharpExpr)
                            unionExpr (NonAbbreviatedType fsType) (unionCase: FSharpUnionCase) =
    let unionExpr = transformExpr com ctx unionExpr
    match fsType with
    | ErasedUnion ->
        if unionCase.UnionCaseFields.Count <> 1 then
            "Erased Union Cases must have one single field: " + (getFsTypeFullName fsType)
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
    | OptionUnion t ->
        let t = makeType com ctx.typeArgs t
        let opKind = if unionCase.Name = "None" then BinaryEqual else BinaryUnequal
        makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; Fable.NoneConst t |> Fable.Const] opKind
    | ListUnion t ->
        let t = makeType com ctx.typeArgs t
        let opKind = if unionCase.CompiledName = "Empty" then BinaryEqual else BinaryUnequal
        makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; Fable.ListEmpty t |> Fable.Const] opKind
    | StringEnum ->
        makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; lowerCaseName unionCase] BinaryEqualStrict
    | DiscriminatedUnion hasCaseWithDataFields ->
        let tag2 = getUnionTagFrom fsType unionCase
        let tag1 =
            if hasCaseWithDataFields
            then makeGet None Fable.String unionExpr (makeIntConst 0)
            else unionExpr
        makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [tag1; tag2] BinaryEqualStrict

let private transformSwitch com ctx (fsExpr: FSharpExpr) (matchValue: FSharpMemberOrFunctionOrValue)
                                isUnionType cases (defaultCase, defaultBindings) decisionTargets =
    let decisionTargets = decisionTargets |> Seq.mapi (fun i d -> (i, d)) |> Map
    let r, typ = makeRange fsExpr.Range, makeType com ctx.typeArgs fsExpr.Type
    let cases =
        cases |> Seq.map (fun (KeyValue(idx, (bindings, labels))) ->
            labels, transformDecisionTreeSuccess com ctx r decisionTargets idx bindings)
        |> Seq.toList
    let defaultCase =
        transformDecisionTreeSuccess com ctx r decisionTargets defaultCase defaultBindings
    let matchValue =
        let matchValueType = makeType com ctx.typeArgs matchValue.FullType
        let matchValue = makeValueFrom com ctx None matchValueType false matchValue
        if isUnionType then getUnionTag matchValue else matchValue
    Fable.Switch(matchValue, cases, Some defaultCase, typ, Some r)

let private transformExpr (com: IFableCompiler) (ctx: Context) fsExpr =
    match fsExpr with
    // TODO: Some cases of coertion shouldn't be erased
    // string :> seq #1279
    // list (and others) :> seq in Fable 2.0
    // concrete type :> interface in Fable 2.0
    | BasicPatterns.Coerce(_targetType, Transform com ctx inpExpr) -> inpExpr

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, Transform com ctx lambda) -> lambda

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
        let callee = Fable.Get(callee, makeStrConst eventName, Fable.Any, None)
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) (Some callee) args

    | CheckArrayLength (Transform com ctx arr, length, FableType com ctx typ) ->
        let r = makeRangeFrom fsExpr
        let lengthExpr = Fable.Get(arr, makeStrConst "length", Fable.Number Int32, r)
        makeEqOp r [lengthExpr; makeTypeConst typ length] BinaryEqualStrict

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
        let expr = makeTypeConst typ value
        if expr.Type <> typ then // Enumerations are compiled as const but they have a different type
            Replacements.checkLiteral com ctx.fileName (makeRangeFrom fsExpr) value typ
        expr

    | BasicPatterns.BaseValue _typ
    | BasicPatterns.ThisValue _typ ->
        Fable.This

    | BasicPatterns.Value var ->
        if isInline var then
            match ctx.scopedInlines |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
            | Some (_,fsExpr) -> com.Transform ctx fsExpr
            | None ->
                "Cannot resolve locally inlined value: " + var.DisplayName
                |> addErrorAndReturnNull com ctx.fileName (makeRange fsExpr.Range |> Some)
        else
            let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
            makeValueFrom com ctx r typ true var

    | BasicPatterns.DefaultValue (FableType com ctx typ) ->
        match typ with
        | Fable.Boolean -> Fable.BoolConst false |> Fable.Const
        | Fable.Number kind -> Fable.NumberConst (0., kind) |> Fable.Const
        | typ -> Fable.Null typ

    (** ## Assignments *)
    | BasicPatterns.Let((var, value), body) ->
        if isInline var then
            let ctx = { ctx with scopedInlines = (var, value)::ctx.scopedInlines }
            transformExpr com ctx body
        else
            let r = makeRangeFrom fsExpr
            let value = transformExpr com ctx value
            let ctx, ident = bindIdentFrom com ctx var
            let body =
                match body with
                | BasicPatterns.Lambda _
                    when var.IsCompilerGenerated && ident.Name.StartsWith("clo") -> // TODO: Check /clo\d+/ ?
                    failwith "TODO"
                | body -> transformExpr com ctx body
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
        let range = makeRangeFrom fsExpr
        match ctx.scopedInlines |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
        | Some (_,fsExpr) ->
            let resolvedCtx = { ctx with typeArgs = matchGenericParams ctx var [] typeArgs }
            let callee = com.Transform resolvedCtx fsExpr
            match args with
            | [] -> callee
            | args -> Fable.Apply(callee, List.map (transformExpr com ctx) args, range)
        | None ->
            "Cannot resolve locally inlined value: " + var.DisplayName
            |> addErrorAndReturnNull com ctx.fileName range


    // // TODO: Ask why application without arguments happen. So far I've seen it
    // // to access None or struct values (like the Result type)
    // | Application(expr,_,[]) -> Some expr
    | BasicPatterns.Application(Transform com ctx callee, _typeArgs, args) ->
        let typ, range = makeType com ctx.typeArgs fsExpr.Type, makeRangeFrom fsExpr
        let args = List.map (transformExpr com ctx) args
        match callee.Type with
        | Fable.DeclaredType(entFullName,_) when entFullName = CoreTypes.applicable ->
            let args = CallHelper.PrepareArgs(args, untupleArgs = true)
            Fable.Call(Fable.Callee callee, None, args, false, typ, range)
        | _ ->
            Fable.Apply(callee, args, range)

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

    | BasicPatterns.Lambda(arg, body) ->
        transformLambda com (makeRangeFrom fsExpr) ctx [arg] body

    (** ## Getters and Setters *)

    // When using a self reference in constructor (e.g. `type MyType() as self =`)
    // the F# compiler wraps self references with code we don't need
    | BasicPatterns.FSharpFieldGet(Some ThisVar, RefType _, _)
    | BasicPatterns.FSharpFieldGet(Some(BasicPatterns.FSharpFieldGet(Some ThisVar, _, _)), RefType _, _) ->
        Fable.This

    | BasicPatterns.FSharpFieldGet (callee, _calleeType, FieldName fieldName) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> "Unexpected static FSharpFieldGet"
                      |> addErrorAndReturnNull com ctx.fileName r
        makeGet r typ callee (makeStrConst fieldName)

    | BasicPatterns.TupleGet (_tupleType, tupleElemIndex, Transform com ctx tupleExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeGet r typ tupleExpr (makeIntConst tupleElemIndex)

    | BasicPatterns.UnionCaseGet (Transform com ctx unionExpr, fsType, unionCase, FieldName fieldName) ->
        let typ, range = makeType com ctx.typeArgs fsExpr.Type, makeRangeFrom fsExpr
        match fsType with
        | StringEnum ->
            "StringEnum types cannot have fields"
            |> addErrorAndReturnNull com ctx.fileName range
        | ErasedUnion ->
            unionExpr
        // TODO: Add special expressions for option and list getters?
        | OptionUnion _ ->
            CoreLibCall("Option", Some "getValue", false, [unionExpr])
            |> makeCall range typ
        | ListUnion _ ->
            let idx = if fieldName= "Head" then 0 else 1
            makeGet range typ unionExpr (makeIntConst idx)
        | DiscriminatedUnion _ ->
            let i = unionCase.UnionCaseFields |> Seq.findIndex (fun x -> x.Name = fieldName)
            makeGet range typ unionExpr (makeIntConst (i+1)) // Index 0 holds the union tag

    // When using a self reference in constructor (e.g. `type MyType() as self =`)
    // the F# compiler introduces artificial statements that we must ignore
    | BasicPatterns.FSharpFieldSet(Some(ThisVar _), RefType _, _, _)
    | BasicPatterns.FSharpFieldSet(Some(BasicPatterns.FSharpFieldGet(Some(ThisVar _), _, _)), RefType _, _, _) ->
        Fable.Null Fable.Any

    | BasicPatterns.FSharpFieldSet (callee, _calleeType, fi, Transform com ctx value) ->
        let range = makeRangeFrom fsExpr
        let value = CallHelper.PrepareArgs([value], [makeType com [] fi.FieldType]) |> List.head
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> "Unexpected static FSharpFieldSet"
                      |> addErrorAndReturnNull com ctx.fileName range
        Fable.Set (callee, Some (makeStrConst fi.Name), value, range)

    | BasicPatterns.UnionCaseTag (Transform com ctx unionExpr, _unionType) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeGet r typ unionExpr (makeIntConst 0)

    | BasicPatterns.UnionCaseSet (_unionExpr, _type, _case, _caseField, _valueExpr) ->
        "Unexpected UnionCaseSet" |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)

    | BasicPatterns.ValueSet (valToSet, Transform com ctx valueExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs valToSet.FullType
        match valToSet.EnclosingEntity with
        | Some ent when ent.IsFSharpModule ->
            // Mutable module values are compiled as functions, because values
            // imported from ES2015 modules cannot be modified (see #986)
            let entRef = com.GetEntity(ent).FullName |> Fable.EntityRef
            let memb = makeStrConst valToSet.CompiledName |> Some
            Fable.Call(Fable.Callee entRef, memb, [valueExpr], false, typ, r)
        | _ ->
            let valToSet = makeValueFrom com ctx r typ false valToSet
            Fable.Set(valToSet, None, valueExpr, r)

    (** Instantiation *)
    | BasicPatterns.NewArray(FableType com ctx elTyp, arrExprs) ->
        makeArray elTyp (arrExprs |> List.map (transformExpr com ctx))

    | BasicPatterns.NewTuple(_, argExprs) ->
        argExprs |> List.map (transformExpr com ctx) |> Fable.TupleConst |> Fable.Const

    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, otherOverrides) ->
        transformObjExpr com ctx fsExpr objType baseCallExpr overrides otherOverrides

    | BasicPatterns.NewObject(meth, typArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        List.map (com.Transform ctx) args
        |> makeCallFrom com ctx r typ meth (typArgs, []) None

    | BasicPatterns.NewRecord(fsType, argExprs) ->
        let range = makeRangeFrom fsExpr
        match tryDefinition fsType with
        | Some tdef ->
            let fields = Seq.toList tdef.FSharpFields
            let argExprs = CallHelper.PrepareArgs(
                            List.map (transformExpr com ctx) argExprs,
                            argTypes = (fields |> List.map (fun x -> makeType com [] x.FieldType)))
            List.zip fields argExprs
            // Remove optional fields when argExpr is None
            |> List.choose (fun (fi, e) ->
                match e, fi.FieldType with
                | Fable.Const(Fable.NoneConst _), OptionUnion _ -> None
                | _ -> Some(fi.Name, e))
            |> makeJsObject range
        | None ->
            "Unexpected NewRecord without definition"
            |> addErrorAndReturnNull com ctx.fileName range

    | BasicPatterns.NewUnionCase(fsType, unionCase, argExprs) ->
        List.map (com.Transform ctx) argExprs
        |> transformNewUnion com ctx fsExpr fsType unionCase

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

    | BasicPatterns.ILFieldGet (callee, typ, fieldName) ->
        let ownerName = tryDefinition typ |> Option.bind (fun x -> x.TryFullName)
        // TODO: Move this to Replacements
        match callee, ownerName, fieldName with
        | None, Some Types.string, "Empty" ->
            makeStrConst ""
        | None, Some Types.guid, "Empty" ->
            makeStrConst "00000000-0000-0000-0000-000000000000"
        | None, Some Types.timespan, "Zero" ->
            makeIntConst 0
        | None, Some (Types.datetime | Types.datetimeOffset as n), ("MaxValue" | "MinValue") ->
            let m = if n = Types.datetime then "Date" else "DateOffset"
            CoreLibCall(m, Some (Naming.lowerFirst fieldName), false, [])
            |> makeCall (makeRangeFrom fsExpr) (makeType com ctx.typeArgs fsExpr.Type)
        | None, Some Types.decimal, "Zero" -> makeDecConst 0M
        | None, Some Types.decimal, "One" -> makeDecConst 1M
        | _ ->
            sprintf "Cannot compile ILFieldGet(%A, %s)" ownerName fieldName
            |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)

    | BasicPatterns.Quote _ ->
        "Quotes are not currently supported by Fable"
        |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)

    // TODO: Ask. I see this when accessing Result types (all structs?)
    | BasicPatterns.AddressOf(Transform com ctx expr) -> expr

    // | BasicPatterns.ILFieldSet _
    // | BasicPatterns.AddressSet _
    // | BasicPatterns.ILAsm _
    | expr ->
        sprintf "Cannot compile expression %A" expr
        |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)

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
let private isIgnoredMember (meth: FSharpMemberOrFunctionOrValue) =
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

let private transformConstructor com ctx (meth: FSharpMemberOrFunctionOrValue)
                                    publicName privateName args (body: FSharpExpr) =
    match meth.EnclosingEntity with
    | Some ent when meth.IsImplicitConstructor ->
        // LambdaType constructor (same name as entity)
        let body =
            match body with
            | BasicPatterns.Sequential(baseCons, expr) ->
                // TODO: Check base const
                // - Remove if it's obj()
                // - Normal call with `this` at the end if it's an F# type
                // - BaseCons.call(this, ...args) if it's a JS type
                expr
            | expr -> expr
        // Bind entity name to context to prevent name clashes (it will become a variable in JS)
        let ctx, ident = getEntityName ent |> bindIdentWithExactName com ctx Fable.Any None
        let body = transformExpr com ctx body
        let range = getMemberLocation meth |> makeRange
        let classCons = Fable.FunctionDeclaration(None, ident.Name, args, body, Some range)
        // Constructor call
        let consCallBody =
            let macro =
                String.Format("$this === void 0 ? new {0}({1}) : {0}.call($this, {1})",
                    ident.Name, args |> List.map (fun x -> x.Name) |> String.concat ", ")
            Fable.Call(Fable.Emit macro, None, [], false, Fable.Any, None) // TODO: Use proper type
        let consCall = Fable.FunctionDeclaration(publicName, privateName, args @ [makeIdent "$this"], consCallBody, None)
        // TODO: Object.setPrototypeOf for exceptions?
        ctx, [classCons; consCall]
    | _ ->
        // TODO: Normal method (overloaded name should have been resolved earlier)
        // Just add `$this` as last argument and pass it at the end when calling another constructor
        failwith "TODO: Secondary constructors"

let private transformMember com ctx import (meth: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let methName = getMemberDeclarationName com (getArgTypes com meth) meth
    let ctx, privateName =
        // Bind meth.CompiledName (TODO: DisplayName for interface meths?) to context
        // to prevent name clashes (they will become variables in JS)
        let typ = makeType com ctx.typeArgs meth.FullType
        let ctx, privateName = bindIdentWithTentativeName com ctx meth methName
        ctx, privateName.Name
    match import with
    | Some(selector, path) ->
        failwith "TODO: compile imports as ValueDeclarations (check if they're mutable, see Zaid's issue)"
        // [], makeImport selector path
    | None ->
        let publicName = if isPublicMember meth then Some methName else None
        let ctx, args = bindMemberArgs com ctx args
        if meth.IsImplicitConstructor || meth.IsConstructor then
            transformConstructor com ctx meth publicName privateName args body
        else
            let fableBody = transformExpr com ctx body
            match fableBody with
            // Accept import expressions used instead of attributes, for example:
            // let foo x y = import "foo" "myLib"
            | Fable.ImportRef(selector, path, importKind, _) ->
                failwith "TODO: compile import expressions as ValueDeclarations (check if they're mutable, see Zaid's issue)"
                // let fableBody =
                //     if selector = Naming.placeholder
                //     then Fable.Value(Fable.ImportRef(meth.DisplayName, path, importKind))
                //     else fableBody
                // [], fableBody
            | fableBody ->
                let range = getMemberLocation meth |> makeRange
                let entMember = Fable.FunctionDeclaration(publicName, privateName, args, fableBody, Some range)
                ctx, [entMember]

let private transformMemberDecl (com: IFableCompiler) (ctx: Context) (meth: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let import = tryGetImport meth.Attributes
    if Option.isSome import then
        transformMember com ctx import meth args body
    elif isIgnoredMember meth then
        ctx, []
    elif isInline meth then
        let args = Seq.collect id args |> countRefs body
        com.AddInlineExpr(meth, (upcast args, body))
        ctx, []
    else transformMember com ctx None meth args body

let rec private transformEntityDecl (com: IFableCompiler) (ctx: Context) (ent: FSharpEntity) subDecls =
    // Add entity to compiler cache
    com.GetEntity(ent) |> ignore
    // let range = getEntityLocation ent |> makeRange
    let import = tryGetImport ent.Attributes
    if Option.isSome import then
        failwith "TODO: Imported entity"
        // let selector, path = import.Value
        // let isPublic = isPublicEntity ctx ent
        // let entName, body = ent.CompiledName, makeImport selector path
        // // Bind entity name to context to prevent name clashes
        // let ctx, ident = bindIdentWithExactName com ctx Fable.Any None entName
        // let m = Fable.Member(entName, Fable.Field, Fable.StaticLoc, [], body.Type)
        // let decl = Fable.MemberDeclaration(m, isPublic, Some ident.Name, [], body, Some range)
        // ctx, Some decl
    elif isIgnoredEntity com ctx ent then
        ctx, []
    else
        let childDecls =
            let ctx = { ctx with enclosingModule = EnclosingModule(com.GetEntity(ent), isPublicEntity ctx ent) }
            transformDeclarations com ctx subDecls
        if List.isEmpty childDecls && ent.IsFSharpModule then
            ctx, []
        else
            // Bind entity name to context to prevent name clashes (it will become a variable in JS)
            let ctx, _ident = bindIdentWithExactName com ctx Fable.Any None ent.CompiledName
            // TODO: declInfo.AddChild(com, ctx, ent, ident.Name, childDecls)
            ctx, []

and private transformDeclarations (com: IFableCompiler) (ctx: Context) fsDecls =
    let _ctx, fableDecls =
        ((ctx, []), fsDecls) ||> List.fold (fun (ctx, accDecls) fsDecl ->
            let ctx, fableDecls =
                match fsDecl with
                | FSharpImplementationFileDeclaration.Entity (e, sub) ->
                    transformEntityDecl com ctx e sub
                | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
                    transformMemberDecl com ctx meth args body
                | FSharpImplementationFileDeclaration.InitAction fe ->
                    let e = com.Transform ctx fe
                    let decl = Fable.ActionDeclaration (e, makeRangeFrom fe)
                    ctx, [decl]
            ctx, (List.rev fableDecls)@accDecls)
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

let private tryGetMemberArgsAndBody (implFiles: Map<string, FSharpImplementationFileContents>)
                                    fileName (meth: FSharpMemberOrFunctionOrValue) =
    let rec tryGetMemberArgsAndBody' (methFullName: string) = function
        | FSharpImplementationFileDeclaration.Entity (e, decls) ->
            let entFullName = getEntityFullName e
            if methFullName.StartsWith(entFullName)
            then List.tryPick (tryGetMemberArgsAndBody' methFullName) decls
            else None
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth2, args, body) ->
            if methFullName = meth2.FullName
            then Some(args, body)
            else None
        | FSharpImplementationFileDeclaration.InitAction _ -> None
    Map.tryFind fileName implFiles
    |> Option.bind (fun f ->
        f.Declarations |> List.tryPick (tryGetMemberArgsAndBody' meth.FullName))

let private tryGetEntityEnclosingModule (implFiles: Map<string, FSharpImplementationFileContents>) (ent: FSharpEntity) =
    let rec tryGetEntityEnclosingModule' (entFullName: string) parent = function
        | FSharpImplementationFileDeclaration.Entity (e, decls) ->
            let entFullName2 = getEntityFullName e
            if entFullName = entFullName2
            then parent
            elif entFullName.StartsWith(entFullName2)
            then List.tryPick (tryGetEntityEnclosingModule' entFullName (Some e)) decls
            else None
        | _ -> None
    // For entities in checked project (no assembly), try to find the implementation entity
    // when the declaration location doesn't correspond to the implementation location (see #754)
    ent.ImplementationLocation |> Option.bind (fun loc ->
        let fileName = Path.normalizePath loc.FileName
        Map.tryFind fileName implFiles
        |> Option.bind (fun f ->
            let entFullName = getEntityFullName ent
            f.Declarations |> List.tryPick (tryGetEntityEnclosingModule' entFullName None)))

type FableCompiler(com: ICompiler, currentFile: string, implFiles: Map<string, FSharpImplementationFileContents>) =
    let usedVarNames = HashSet<string>()
    let dependencies = HashSet<string>()
    member __.UsedVarNames = set usedVarNames
    member __.Dependencies = set dependencies
    interface IFableCompiler with
        member fcom.Transform ctx fsExpr =
            transformExpr fcom ctx fsExpr
        member __.TryGetInternalFile tdef =
            match tdef.Assembly.FileName with
            | Some asmPath when not(System.String.IsNullOrEmpty(asmPath)) -> None
            | _ -> Some (getEntityLocation tdef).FileName
        member fcom.GetEntity tdef =
            com.GetOrAddEntity(getEntityFullName tdef, fun () -> makeEntity fcom tdef)
        member fcom.GetEntityEnclosingModule tdef =
            match tryGetEntityEnclosingModule implFiles tdef with
            | Some tdef -> com.GetOrAddEntity(getEntityFullName tdef, fun () -> makeEntity fcom tdef)
            | None -> failwith ("Cannot find enclosing module for " + (getEntityFullName tdef))
        member __.GetInlineExpr meth =
            let fileName = (getMemberLocation meth).FileName |> Path.normalizePath
            if fileName <> currentFile then
                dependencies.Add(fileName) |> ignore
            // TODO: fullNameAndArgCount is not safe, we need types of args
            let fullNameAndArgCount = meth.FullName + "(" + (getArgCount meth |> string) + ")"
            com.GetOrAddInlineExpr(fullNameAndArgCount, fun () ->
                failwith "TODO: compile inline expressions"
                // match tryGetMemberArgsAndBody implFiles fileName meth with
                // | Some(args, body) ->
                //     let args = Seq.collect id args |> countRefs body
                //     (upcast args, body)
                // | None -> failwith ("Cannot find inline method " + meth.FullName)
            )
        member __.AddInlineExpr(meth, inlineExpr) =
            // TODO: fullNameAndArgCount is not safe, we need types of args
            let fullNameAndArgCount = meth.FullName + "(" + (getArgCount meth |> string) + ")"
            com.GetOrAddInlineExpr(fullNameAndArgCount, fun () ->
                failwith "TODO: compile inline expressions"
                // inlineExpr
            ) |> ignore
        member __.AddUsedVarName varName =
            usedVarNames.Add varName |> ignore
    interface ICompiler with
        member __.Options = com.Options
        member __.ProjectFile = com.ProjectFile
        member __.GetUniqueVar() = com.GetUniqueVar()
        member __.GetRootModule(fileName) =
            com.GetRootModule(fileName)
        member __.GetOrAddEntity(fullName, generate) = // TODO
            com.GetOrAddEntity(fullName, generate)
        member __.GetOrAddInlineExpr(fullName, generate) = // TODO
            com.GetOrAddInlineExpr(fullName, generate)
        member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

let getRootModuleFullName (file: FSharpImplementationFileContents) =
    let rootEnt, _ = getRootModuleAndDecls file.Declarations
    match rootEnt with
    | Some rootEnt -> getEntityFullName rootEnt
    | None -> ""

let transformFile (com: ICompiler) (implFiles: Map<string, FSharpImplementationFileContents>) (fileName: string) =
    try
        let file =
            // TODO: This shouldn't be necessary, but just in case
            let fileName = Path.normalizeFullPath fileName
            match Map.tryFind fileName implFiles with
            | Some file -> file
            | None -> failwithf "File %s doesn't belong to parsed project %s" fileName com.ProjectFile
        let fcom = FableCompiler(com, fileName, implFiles)
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