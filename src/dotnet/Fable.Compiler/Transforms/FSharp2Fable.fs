module rec Fable.Transforms.FSharp2Fable.Compiler

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
open Fable.Transforms

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
            Fable.NewErasedUnion(expr, genArgs) |> Fable.Value
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
        Fable.NewOption(expr, typ) |> Fable.Value
    | ListUnion typ ->
        let typ = makeType com ctx.typeArgs typ
        let headAndTail =
            match argExprs with
            | [] -> None
            | [head; tail] -> Some(head, tail)
            | _ -> failwith "Unexpected args for List constructor"
        Fable.NewList(headAndTail, typ) |> Fable.Value
    | DiscriminatedUnion tdef ->
        let genArgs = makeGenArgs com ctx.typeArgs fsType.GenericArguments
        Fable.NewUnion(argExprs, unionCase, tdef, genArgs) |> Fable.Value

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
        let tempVar = com.GetUniqueVar() |> makeIdent
        let tempVarExpr = Fable.IdentExpr tempVar
        let tempVarFirstItem = makeIndexGet None (Fable.Number Int32) tempVarExpr 0
        let cases =
            targetRefsCount
            |> Seq.map (fun kv ->
                let vars, body = decisionTargets.[kv.Key]
                let ctx =
                    let mutable i = 0
                    (ctx, vars) ||> List.fold (fun ctx var ->
                        i <- i + 1
                        let t = makeType com ctx.typeArgs var.FullType
                        makeIndexGet None t tempVarExpr i
                        |> bindExpr ctx var)
                [makeIntConst kv.Key], transformExpr com ctx body)
            |> Seq.toList
        let typ = makeType com ctx.typeArgs fsExpr.Type
        let bindings = [tempVar, transformExpr com ctx decisionExpr]
        Fable.Let(bindings, Fable.Switch(tempVarFirstItem, cases, None, typ))
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
    // let wrapInZeroArgsFunction r typ (args: FSharpExpr list) argTypes fref =
    //     let args = List.map (transformExpr com ctx) args
    //     let argTypes = List.map (makeType com []) argTypes
    //     let body = Fable.Operation(Fable.Apply(fref, args, argTypes), typ, r)
    //     Fable.Function(Fable.Delegate [], body)
    // let isSpecialCase t =
    //     tryDefinition t
    //     |> Option.bind (fun tdef -> tdef.TryFullName)
    //     |> Option.toBool (fun name -> name = "System.Func`1" || name = "System.Action")
    match fsExpr with
    // TODO: Check which tests fail because of this
    // There are special cases (`Func` with one gen param and `Action` with no params)
    // the F# compiler translates as an application
    // | BasicPatterns.Call(None,v,[],[],args)
    // | BasicPatterns.Application(BasicPatterns.Value v, argTypes, args)
    // | BasicPatterns.Application(BasicPatterns.Application(BasicPatterns.Value v, argTypes, args),_,_)
    //         when isSpecialCase delegateType ->
    //     let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
    //     makeValueFrom com ctx r v |> wrapInZeroArgsFunction r typ args argTypes
    | BasicPatterns.Lambda(arg, body) ->
        transformLambda com ctx arg body
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
        let noneCase = Fable.NewOption(None, makeType com ctx.typeArgs t) |> Fable.Value
        if unionCase.Name = "None" then BinaryEqual else BinaryUnequal
        |> makeEqOp (makeRangeFrom fsExpr) unionExpr noneCase
    | ListUnion t ->
        let emptyList = Fable.NewList(None, makeType com ctx.typeArgs t) |> Fable.Value
        if unionCase.Name = "Empty" then BinaryEqual else BinaryUnequal
        |> makeEqOp (makeRangeFrom fsExpr) unionExpr emptyList
    | StringEnum ->
        makeEqOp (makeRangeFrom fsExpr) unionExpr (lowerCaseName unionCase) BinaryEqualStrict
    | DiscriminatedUnion tdef ->
        let tag1 = Fable.Get(unionExpr, Fable.UnionTag tdef, Fable.Any, None)
        let tag2 = Fable.UnionCaseTag(unionCase, tdef) |> Fable.Value
        makeEqOp (makeRangeFrom fsExpr) tag1 tag2 BinaryEqualStrict

// let private transformSwitch com ctx (fsExpr: FSharpExpr) (matchValue: FSharpMemberOrFunctionOrValue)
//                                 isUnionType cases (defaultCase, defaultBindings) decisionTargets =
//     let decisionTargets = decisionTargets |> Seq.mapi (fun i d -> (i, d)) |> Map
//     let r, typ = makeRange fsExpr.Range, makeType com ctx.typeArgs fsExpr.Type
//     let cases =
//         cases |> Seq.map (fun (KeyValue(idx, (bindings, labels))) ->
//             labels, transformDecisionTreeSuccess com ctx r decisionTargets idx bindings)
//         |> Seq.toList
//     let defaultCase =
//         transformDecisionTreeSuccess com ctx r decisionTargets defaultCase defaultBindings
//     let matchValue =
//         let matchValueType = makeType com ctx.typeArgs matchValue.FullType
//         let matchValue = makeValueFrom com ctx None matchValueType false matchValue
//         if isUnionType then getUnionTag matchValue else matchValue
//     Fable.Switch(matchValue, cases, Some defaultCase, typ, Some r)

let private transformExpr (com: IFableCompiler) (ctx: Context) fsExpr =
    match fsExpr with
    | BasicPatterns.Coerce(targetType, Transform com ctx inpExpr) ->
        Fable.Cast(inpExpr, makeType com ctx.typeArgs targetType)

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, Transform com ctx lambda) -> lambda

    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOfLoop (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        Fable.ForOf (ident, value, transformExpr com newContext body)
        |> makeLoop (makeRangeFrom fsExpr)

    | TryGetValue (callee, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let callee, args = Option.map (com.Transform ctx) callee, List.map (com.Transform ctx) membArgs
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeCallFrom com ctx r typ (ownerGenArgs@membGenArgs) callee args memb

    | CreateEvent (callee, eventName, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let callee, args = com.Transform ctx callee, List.map (com.Transform ctx) membArgs
        let callee = Fable.Get(callee, Fable.FieldGet eventName, Fable.Any, None)
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeCallFrom com ctx r typ (ownerGenArgs@membGenArgs) (Some callee) args memb

    | CheckArrayLength (Transform com ctx arr, length, FableType com ctx typ) ->
        let r = makeRangeFrom fsExpr
        let lengthExpr = Fable.Get(arr, Fable.FieldGet "length", Fable.Number Int32, r)
        makeEqOp r lengthExpr (makeTypeConst typ length) BinaryEqualStrict

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
        // TODO TODO TODO: Check literals and compile as Enum
        // if expr.Type <> typ then // Enumerations are compiled as const but they have a different type
        //     Replacements.checkLiteral com ctx.fileName (makeRangeFrom fsExpr) value typ
        expr

    | BasicPatterns.BaseValue typ
    | BasicPatterns.ThisValue typ ->
        makeType com ctx.typeArgs typ |> Fable.This |> Fable.Value

    | BasicPatterns.Value var ->
        if isInline var then
            match ctx.scopedInlines |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
            | Some (_,fsExpr) -> com.Transform ctx fsExpr
            | None ->
                "Cannot resolve locally inlined value: " + var.DisplayName
                |> addErrorAndReturnNull com ctx.fileName (makeRange fsExpr.Range |> Some)
        else
            makeValueFrom com ctx (makeRangeFrom fsExpr) var

    | BasicPatterns.DefaultValue (FableType com ctx typ) ->
        match typ with
        | Fable.Boolean -> Fable.BoolConstant false |> Fable.Value
        | Fable.Number kind -> Fable.NumberConstant (0., kind) |> Fable.Value
        | typ -> Fable.Null typ |> Fable.Value

    (** ## Assignments *)
    | BasicPatterns.Let((var, value), body) ->
        if isInline var then
            let ctx = { ctx with scopedInlines = (var, value)::ctx.scopedInlines }
            transformExpr com ctx body
        else
            let value = transformExpr com ctx value
            let ctx, ident = bindIdentFrom com ctx var
            match transformExpr com ctx body with
            | Fable.Let(bindings, body) -> Fable.Let((ident, value)::bindings, body)
            | body -> Fable.Let([ident, value], body)

    | BasicPatterns.LetRec(recBindings, body) ->
        // First get a context containing all idents and use it compile the values
        let ctx, idents =
            (recBindings, (ctx, []))
            ||> List.foldBack (fun (BindIdent com ctx (newContext, ident), _) (ctx, idents) ->
                (newContext, ident::idents))
        let bindings =
            recBindings
            |> List.map (fun (_, Transform com ctx value) -> value)
            |> List.zip idents
        match transformExpr com ctx body with
        | Fable.Let(bindings2, body) -> Fable.Let(bindings@bindings2, body)
        | body -> Fable.Let(bindings, body)

    (** ## Applications *)
    | BasicPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, _argTypes2, argExprs) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        addErrorAndReturnNull com ctx.fileName r "TODO: TraitCalls"

    | BasicPatterns.Call(callee, memb, ownerGenArgs, membGenArgs, args) ->
        let callee = Option.map (com.Transform ctx) callee
        let args = List.map (transformExpr com ctx) args
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeCallFrom com ctx r typ (ownerGenArgs@membGenArgs) callee args memb

    // Application of locally inlined lambdas
    | BasicPatterns.Application(BasicPatterns.Value var, typeArgs, args) when isInline var ->
        failwith "TODO: Locally inlined lambdas"
        // let range = makeRangeFrom fsExpr
        // match ctx.scopedInlines |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
        // | Some (_,fsExpr) ->
        //     let resolvedCtx = { ctx with typeArgs = matchGenericParams ctx var [] typeArgs }
        //     let callee = com.Transform resolvedCtx fsExpr
        //     match args with
        //     | [] -> callee
        //     | args -> Fable.Apply(callee, List.map (transformExpr com ctx) args, range)
        // | None ->
        //     "Cannot resolve locally inlined value: " + var.DisplayName
        //     |> addErrorAndReturnNull com ctx.fileName range

    // // TODO: Ask why application without arguments happen. So far I've seen it
    // // to access None or struct values (like the Result type)
    | BasicPatterns.Application(Transform com ctx expr, _, []) -> expr
    | BasicPatterns.Application(Transform com ctx applied, _, args) ->
        let range = makeRangeFrom fsExpr
        match List.map (transformExpr com ctx) args, applied.Type with
        | args, Fable.DeclaredType(ent,_)
                when ent.TryFullName = Some Types.dynamicApplicable ->
            match args with
            | [Fable.Value(Fable.NewTuple args)] -> makeDynamicCall range applied args
            | args -> makeDynamicCall range applied args
        | args, _ ->
            let typ = makeType com ctx.typeArgs fsExpr.Type
            Fable.Operation(Fable.CurriedApply(applied, args), typ, range)

    | BasicPatterns.IfThenElse (Transform com ctx guardExpr, Transform com ctx thenExpr, Transform com ctx elseExpr) ->
        Fable.IfThenElse (guardExpr, thenExpr, elseExpr)

    | BasicPatterns.TryFinally (BasicPatterns.TryWith(body, _, _, catchVar, catchBody),finalBody) ->
        makeTryCatch com ctx fsExpr body (Some (catchVar, catchBody)) (Some finalBody)

    | BasicPatterns.TryFinally (body, finalBody) ->
        makeTryCatch com ctx fsExpr body None (Some finalBody)

    | BasicPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        makeTryCatch com ctx fsExpr body (Some (catchVar, catchBody)) None

    | BasicPatterns.Sequential (Transform com ctx first, Transform com ctx second) ->
        Fable.Sequential [first; second]

    (** ## Lambdas *)
    | BasicPatterns.NewDelegate(delegateType, fsExpr) ->
        transformDelegate com ctx delegateType fsExpr

    | BasicPatterns.Lambda(arg, body) ->
        transformLambda com ctx arg body

    (** ## Getters and Setters *)

    // When using a self reference in constructor (e.g. `type MyType() as self =`)
    // the F# compiler wraps self references with code we don't need
    | BasicPatterns.FSharpFieldGet(Some ThisVar, RefType _, _)
    | BasicPatterns.FSharpFieldGet(Some(BasicPatterns.FSharpFieldGet(Some ThisVar, _, _)), RefType _, _) ->
        makeType com ctx.typeArgs fsExpr.Type |> Fable.This |> Fable.Value

    | BasicPatterns.FSharpFieldGet (callee, NonAbbreviatedType calleeType, field) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> "Unexpected static FSharpFieldGet"
                      |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)
        if calleeType.HasTypeDefinition && calleeType.TypeDefinition.IsFSharpRecord
        then Fable.Get(callee, Fable.RecordGet(field, calleeType.TypeDefinition), typ, r)
        else makeFieldGet (makeRangeFrom fsExpr) typ callee field.Name

    | BasicPatterns.TupleGet (_tupleType, tupleElemIndex, Transform com ctx tupleExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        // makeIndexGet r typ tupleExpr tupleElemIndex
        Fable.Get(tupleExpr, Fable.TupleGet tupleElemIndex, typ, r)

    | BasicPatterns.UnionCaseGet (Transform com ctx unionExpr, fsType, unionCase, field) ->
        let range = makeRangeFrom fsExpr
        match fsType with
        | ErasedUnion -> unionExpr
        | StringEnum ->
            "StringEnum types cannot have fields"
            |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)
        | OptionUnion t ->
            Fable.Get(unionExpr, Fable.OptionValue, makeType com ctx.typeArgs t, range)
        | ListUnion t ->
            let kind = if field.Name = "Head" then Fable.ListHead else Fable.ListTail
            Fable.Get(unionExpr, kind, makeType com ctx.typeArgs t, range)
        | DiscriminatedUnion tdef ->
            let t = makeType com ctx.typeArgs field.FieldType
            Fable.Get(unionExpr, Fable.UnionField(unionCase, tdef), t, range)

    // When using a self reference in constructor (e.g. `type MyType() as self =`)
    // the F# compiler introduces artificial statements that we must ignore
    | BasicPatterns.FSharpFieldSet(Some(ThisVar _), RefType _, _, _)
    | BasicPatterns.FSharpFieldSet(Some(BasicPatterns.FSharpFieldGet(Some(ThisVar _), _, _)), RefType _, _, _) ->
        Fable.Null Fable.Any |> Fable.Value

    | BasicPatterns.FSharpFieldSet(callee, NonAbbreviatedType calleeType, field, Transform com ctx value) ->
        let range = makeRangeFrom fsExpr
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> "Unexpected static FSharpFieldSet"
                      |> addErrorAndReturnNull com ctx.fileName range
        if calleeType.HasTypeDefinition && calleeType.TypeDefinition.IsFSharpRecord
        then Fable.Set(callee, Fable.RecordSet(field, calleeType.TypeDefinition), value, range)
        else Fable.Set(callee, Fable.FieldSet field.Name, value, range)

    | BasicPatterns.UnionCaseTag(Transform com ctx unionExpr, NonAbbreviatedType unionType) ->
        let range = makeRangeFrom fsExpr
        Fable.Get(unionExpr, Fable.UnionTag unionType.TypeDefinition, Fable.Any, range)

    | BasicPatterns.UnionCaseSet (_unionExpr, _type, _case, _caseField, _valueExpr) ->
        "Unexpected UnionCaseSet" |> addErrorAndReturnNull com ctx.fileName (makeRangeFrom fsExpr)

    | BasicPatterns.ValueSet (valToSet, Transform com ctx valueExpr) ->
        let r = makeRangeFrom fsExpr
        match valToSet.EnclosingEntity with
        | Some ent when ent.IsFSharpModule ->
            failwith "TODO: Mutable module values"
            // Mutable module values are compiled as functions, because values
            // imported from ES2015 modules cannot be modified (see #986)
            // Fable.Get(Fable.EntityRef ent, makeStrConst valToSet.CompiledName, None)
            // let op = Fable.Apply(Fable.EntityRef ent, [valueExpr])
            // Fable.Operation(op, Fable.Unit, r)
        | _ ->
            let valToSet = makeValueFrom com ctx r valToSet
            Fable.Set(valToSet, Fable.VarSet, valueExpr, r)

    (** Instantiation *)
    | BasicPatterns.NewArray(FableType com ctx elTyp, arrExprs) ->
        makeArray elTyp (arrExprs |> List.map (transformExpr com ctx))

    | BasicPatterns.NewTuple(_, argExprs) ->
        argExprs |> List.map (transformExpr com ctx) |> Fable.NewTuple |> Fable.Value

    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, otherOverrides) ->
        transformObjExpr com ctx fsExpr objType baseCallExpr overrides otherOverrides

    | BasicPatterns.NewObject(memb, genArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        let args = List.map (com.Transform ctx) args
        makeCallFrom com ctx r typ genArgs None args memb

    | BasicPatterns.NewRecord(NonAbbreviatedType fsType, argExprs) ->
        let argExprs = List.map (transformExpr com ctx) argExprs
        let genArgs = makeGenArgs com ctx.typeArgs fsType.GenericArguments
        Fable.NewRecord(argExprs, fsType.TypeDefinition, genArgs) |> Fable.Value

    | BasicPatterns.NewUnionCase(fsType, unionCase, argExprs) ->
        List.map (com.Transform ctx) argExprs
        |> transformNewUnion com ctx fsExpr fsType unionCase

    (** ## Type test *)
    | BasicPatterns.TypeTest (FableType com ctx typ, Transform com ctx expr) ->
        makeTypeTest com ctx.fileName (makeRangeFrom fsExpr) expr typ

    | BasicPatterns.UnionCaseTest(unionExpr, fsType, unionCase) ->
        transformUnionCaseTest com ctx fsExpr unionExpr fsType unionCase

    (** Pattern Matching *)
    // TODO TODO TODO
    // | Switch(matchValue, isUnionType, cases, defaultCase, decisionTargets) ->
    //     transformSwitch com ctx fsExpr matchValue isUnionType cases defaultCase decisionTargets

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
            // CoreLibCall(m, Some (Naming.lowerFirst fieldName), false, [])
            // |> makeCall (makeRangeFrom fsExpr) (makeType com ctx.typeArgs fsExpr.Type)
            failwith "TODO: Date.MaxValue"
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

let private transformConstructor com ctx (meth: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    failwith "TODO: Constructors"
    // match meth.EnclosingEntity with
    // | Some ent when meth.IsImplicitConstructor ->
    //     // LambdaType constructor (same name as entity)
    //     let body =
    //         match body with
    //         | BasicPatterns.Sequential(baseCons, expr) ->
    //             // TODO: Check base const
    //             // - Remove if it's obj()
    //             // - Normal call with `this` at the end if it's an F# type
    //             // - BaseCons.call(this, ...args) if it's a JS type
    //             expr
    //         | expr -> expr
    //     // Bind entity name to context to prevent name clashes (it will become a variable in JS)
    //     let ctx, ident = getEntityName ent |> bindIdentWithExactName com ctx Fable.Any
    //     // TODO: FunctionConstructor needs to be public too for type references
    //     let classCons = Fable.FunctionDeclaration(None, ident.Name, args, transformExpr com ctx body)
    //     // Constructor call
    //     let consCallBody =
    //         let args = args |> List.map (fun x -> x.Name) |> String.concat ", "
    //         let macro = String.Format("$this === void 0 ? new {0}({1}) : {0}.call($this, {1})", ident.Name, args)
    //         Fable.Operation(Fable.Emit(macro, None), Fable.Any, None) // TODO: Use proper type
    //     let consCall = Fable.FunctionDeclaration(publicName, privateName, args @ [makeIdent "$this"], consCallBody)
    //     // TODO: Object.setPrototypeOf for exceptions?
    //     ctx, [classCons; consCall]
    // | _ ->
    //     // TODO: Normal method (overloaded name should have been resolved earlier)
    //     // Just add `$this` as last argument and pass it at the end when calling another constructor
    //     failwith "TODO: Secondary constructors"

// TODO: compile imports as ValueDeclarations (check if they're mutable, see Zaid's issue)
// TODO: Import expressions must be exported if public too
let private transformImport com ctx typ publicName selector path =
//     if selector = Naming.placeholder
//     then Fable.Value(Fable.Import(meth.DisplayName, path, importKind))
//     else fableBody
    ctx, []

let private getPublicAndPrivateNames com ctx memb =
    let methName = getMemberDeclarationName com (getArgTypes com memb) memb
    let publicName = if isPublicMember memb then Some methName else None
    // Bind memb.CompiledName to context to prevent name clashes (become vars in JS)
    let ctx, privateIdent = bindIdentWithTentativeName com ctx memb methName
    ctx, publicName, privateIdent.Name

let private transformMemberValue com ctx (memb: FSharpMemberOrFunctionOrValue) (value: FSharpExpr) =
    let fableValue = transformExpr com ctx value
    let ctx, publicName, privateName = getPublicAndPrivateNames com ctx memb
    match fableValue with
    // Accept import expressions, e.g. let foo = import "foo" "myLib"
    | Fable.Import(selector, path, Fable.CustomImport, typ) ->
        transformImport com ctx typ publicName selector path
    | fableValue ->
        ctx, [Fable.ValueDeclaration(publicName, privateName, fableValue, memb.IsMutable)]

let private transformMemberFunction com ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let fableBody = transformExpr com bodyCtx body
    let ctx, publicName, privateName = getPublicAndPrivateNames com ctx memb
    match fableBody with
    // Accept import expressions , e.g. let foo x y = import "foo" "myLib"
    | Fable.Import(selector, path, Fable.CustomImport, typ) ->
        transformImport com ctx typ (Some publicName) selector path
    | fableBody ->
        let fn = Fable.Function(Fable.Delegate args, fableBody)
        ctx, [Fable.ValueDeclaration(publicName, privateName, fn, false)]

let private transformMemberDecl (com: IFableCompiler) (ctx: Context) (memb: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    if isIgnoredMember memb then
        ctx, []
    elif isInline memb then
        // TODO: Compiler flag to output inline expressions (e.g. for REPL libs)
        let args = Seq.collect id args |> countRefs body
        com.AddInlineExpr(memb, (upcast args, body))
        ctx, []
    elif memb.IsImplicitConstructor || memb.IsConstructor then
        transformConstructor com ctx memb args body
    elif isModuleValueForDeclaration memb then
        transformMemberValue com ctx memb body
    else
        transformMemberFunction com ctx memb args body

let rec private transformEntityDecl (com: IFableCompiler) (ctx: Context) (ent: FSharpEntity) subDecls =
    if isIgnoredEntity com ctx ent
    then ctx, []
    else transformDeclarations com ctx subDecls

and private transformDeclarations (com: IFableCompiler) (ctx: Context) fsDecls =
    ((ctx, []), fsDecls) ||> List.fold (fun (ctx, accDecls) fsDecl ->
        let ctx, fableDecls =
            match fsDecl with
            | FSharpImplementationFileDeclaration.Entity (e, sub) ->
                transformEntityDecl com ctx e sub
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
                transformMemberDecl com ctx meth args body
            | FSharpImplementationFileDeclaration.InitAction fe ->
                // TODO: Check if variables defined in several init actions can conflict
                let e = transformExpr com ctx fe
                let decl = Fable.ActionDeclaration e
                ctx, [decl]
        ctx, accDecls @ fableDecls)

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

type FableCompiler(com: ICompiler, state: ICompilerState, currentFile: string, implFiles: Map<string, FSharpImplementationFileContents>) =
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
        member __.GetRootModule filePath =
            state.GetRootModule filePath
        member fcom.GetInlineExpr memb =
            let fileName = (getMemberLocation memb).FileName |> Path.normalizePath
            if fileName <> currentFile then
                dependencies.Add(fileName) |> ignore
            let fullName = getMemberDeclarationFullname fcom (getArgTypes fcom memb) memb
            state.GetOrAddInlineExpr(fullName, fun () ->
                match tryGetMemberArgsAndBody implFiles fileName memb with
                | Some(args, body) ->
                    let args = Seq.collect id args |> countRefs body
                    (upcast args, body)
                | None -> failwith ("Cannot find inline member " + memb.FullName)
            )
        member fcom.AddInlineExpr(memb, inlineExpr) =
            let fullName = getMemberDeclarationFullname fcom (getArgTypes fcom memb) memb
            state.GetOrAddInlineExpr(fullName, fun () -> inlineExpr) |> ignore
        member __.AddUsedVarName varName =
            usedVarNames.Add varName |> ignore
    interface ICompiler with
        member __.Options = com.Options
        member __.ProjectFile = com.ProjectFile
        member __.GetUniqueVar() = com.GetUniqueVar()
        member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

let getRootModuleFullName (file: FSharpImplementationFileContents) =
    let rootEnt, _ = getRootModuleAndDecls file.Declarations
    match rootEnt with
    | Some rootEnt -> getEntityFullName rootEnt
    | None -> ""

let transformFile (com: ICompiler) (state: ICompilerState) (implFiles: Map<string, FSharpImplementationFileContents>) (fileName: string) =
    try
        let file =
            match Map.tryFind fileName implFiles with
            | Some file -> file
            | None -> failwithf "File %s doesn't belong to parsed project %s" fileName com.ProjectFile
        let fcom = FableCompiler(com, state, fileName, implFiles)
        let _, rootDecls = getRootModuleAndDecls file.Declarations
        let ctx = Context.Create(fileName)
        let _, rootDecls = transformDeclarations fcom ctx rootDecls
        Fable.File(fileName, rootDecls, fcom.UsedVarNames, fcom.Dependencies)
    with
    | ex -> exn (sprintf "%s (%s)" ex.Message fileName, ex) |> raise