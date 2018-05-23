module rec Fable.Transforms.FSharp2Fable.Compiler

open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.Transforms

open Patterns
open TypeHelpers
open Identifiers
open Helpers
open Util

let private transformNewUnion com ctx (fsExpr: FSharpExpr) fsType
                (unionCase: FSharpUnionCase) (argExprs: Fable.Expr list) =
    match fsType with
    | ErasedUnion(_, genArgs) ->
        match argExprs with
        | [expr] ->
            let genArgs = makeGenArgs com ctx.GenericArgs genArgs
            Fable.NewErasedUnion(expr, genArgs) |> Fable.Value
        | _ -> "Erased Union Cases must have one single field: " + (getFsTypeFullName fsType)
               |> addErrorAndReturnNull com (makeRangeFrom fsExpr)
    | StringEnum tdef ->
        let enumName = defaultArg tdef.TryFullName "unknown"
        match argExprs with
        | [] -> Fable.Enum(lowerCaseName unionCase |> Fable.StringEnum, enumName) |> Fable.Value
        | _ -> "StringEnum types cannot have fields: " + enumName
               |> addErrorAndReturnNull com (makeRangeFrom fsExpr)
    | OptionUnion typ ->
        let typ = makeType com ctx.GenericArgs typ
        let expr =
            match argExprs with
            | [] -> None
            | [expr] -> Some expr
            | _ -> failwith "Unexpected args for Option constructor"
        Fable.NewOption(expr, typ) |> Fable.Value
    | ListUnion typ ->
        let typ = makeType com ctx.GenericArgs typ
        let headAndTail =
            match argExprs with
            | [] -> None
            | [head; tail] -> Some(head, tail)
            | _ -> failwith "Unexpected args for List constructor"
        Fable.NewList(headAndTail, typ) |> Fable.Value
    | DiscriminatedUnion(tdef, genArgs) ->
        let genArgs = makeGenArgs com ctx.GenericArgs genArgs
        Fable.NewUnion(argExprs, unionCase, tdef, genArgs) |> Fable.Value

let private transformTraitCall com (ctx: Context) r typ (sourceTypes: FSharpType list) traitName (flags: MemberFlags) (argTypes: FSharpType list) (argExprs: FSharpExpr list) =
    let resolveMemberCall (entity: FSharpEntity) (genArgs: IList<_>) membCompiledName isInstance argTypes thisArg args =
        let genArgs =
            if entity.GenericParameters.Count = genArgs.Count
            then Seq.zip (entity.GenericParameters |> Seq.map (fun x -> x.Name)) genArgs |> Map
            // TODO: Log error if they're not same length?
            else Map.empty
        tryFindMember com entity genArgs membCompiledName isInstance argTypes
        |> Option.map (fun memb -> makeCallFrom com ctx r typ [] thisArg args memb)

    let isInstance = flags.IsInstance
    let argTypes = List.map (makeType com ctx.GenericArgs) argTypes
    let argExprs = List.map (fun e -> com.Transform(ctx, e)) argExprs
    let thisArg, args, argTypes =
        match argExprs, argTypes with
        | thisArg::args, _::argTypes when isInstance -> Some thisArg, args, argTypes
        | args, argTypes -> None, args, argTypes
    resolveTypes ctx sourceTypes |> Seq.tryPick (fun typ ->
        if not typ.HasTypeDefinition
        then None
        else
            let entity = typ.TypeDefinition
            // SRTP only works for records if there are no arguments
            if isInstance && entity.IsFSharpRecord && List.isEmpty args && Option.isSome thisArg then
                let fieldName = Naming.removeGetSetPrefix traitName
                entity.FSharpFields |> Seq.tryPick (fun fi ->
                    if fi.Name = fieldName then
                        let typ = makeType com ctx.GenericArgs typ
                        Fable.Get(thisArg.Value, Fable.RecordGet(fi, entity), typ, r) |> Some
                    else None)
                |> Option.orElseWith (fun () ->
                    resolveMemberCall entity typ.GenericArguments traitName isInstance argTypes thisArg args)
            else resolveMemberCall entity typ.GenericArguments traitName isInstance argTypes thisArg args)
    |> Option.defaultWith (fun () ->
        "Cannot resolve trait call " + traitName |> addErrorAndReturnNull com r)

let private transformObjExpr (com: IFableCompiler) (ctx: Context) (objType: FSharpType)
                    baseCallExpr (overrides: FSharpObjectExprOverride list) otherOverrides =
    let ctx, boundThis =
        match ctx.BoundThis, ctx.EnclosingMember with
         // If `this` is already bound we don't need to worry here
        | None, ImplicitConstructor ->
            let boundThis = com.GetUniqueVar("this") |> makeIdent |> Some
            { ctx with BoundThis = boundThis }, boundThis
        | _ -> ctx, None
    let baseCall =
        match baseCallExpr with
        // For interface implementations this should be BasicPatterns.NewObject
        // but check the baseCall.DeclaringEntity name just in case
        | BasicPatterns.Call(None,baseCall,genArgs1,genArgs2,baseArgs) ->
            match baseCall.DeclaringEntity with
            | Some baseType when baseType.TryFullName <> Some Types.object ->
                let typ = makeType com ctx.GenericArgs baseCallExpr.Type
                let baseArgs = List.map (transformExpr com ctx) baseArgs
                makeCallFrom com ctx None typ (genArgs1@genArgs2) None baseArgs baseCall |> Some
            | _ -> None
        | _ -> None
    let members =
        (objType, overrides)::otherOverrides |> List.collect (fun (typ, overrides) ->
            let overrides =
                if not typ.HasTypeDefinition then overrides else
                let typName = typ.TypeDefinition.FullName.Replace(".","-")
                overrides |> List.where (fun x ->
                    typName + "-" + x.Signature.Name
                    |> Naming.ignoredInterfaceMethods.Contains
                    |> not)
            overrides |> List.map (fun over ->
                let ctx, args = bindMemberArgs com ctx over.CurriedParameterGroups
                let value = Fable.Function(Fable.Delegate args, transformExpr com ctx over.Body, None)
                let name, kind =
                    match over.Signature.Name with
                    | Naming.StartsWith "get_" name when countNonCurriedParamsForOverride over = 0 ->
                        name, Fable.ObjectGetter
                    | Naming.StartsWith "set_" name when countNonCurriedParamsForOverride over = 1 ->
                        name, Fable.ObjectSetter
                    | name ->
                        // Don't use the typ argument as the override may come
                        // from another type, like ToString()
                        if over.Signature.DeclaringType.HasTypeDefinition then
                            let tdef = over.Signature.DeclaringType.TypeDefinition
                            match tdef.TryFullName with
                            | Some Types.enumerable ->
                                name, Fable.ObjectIterator
                            | _ ->
                                // FSharpObjectExprOverride.CurriedParameterGroups doesn't offer
                                // information about ParamArray, we need to check the source method.
                                let hasSpread =
                                    tdef.TryGetMembersFunctionsAndValues
                                    |> Seq.tryFind (fun x -> x.CompiledName = over.Signature.Name)
                                    |> function Some m -> hasSeqSpread m | None -> false
                                name, Fable.ObjectMethod hasSpread
                        else
                            name, Fable.ObjectMethod false
                makeStrConst name, value, kind
            ))
    let objExpr = Fable.ObjectExpr(members, makeType com ctx.GenericArgs objType, baseCall)
    match boundThis with
    | Some boundThis -> Fable.Let([boundThis, Fable.Value (Fable.This Fable.Any)], objExpr)
    | None -> objExpr

// TODO: Check code in Fable 1 and which tests fail because
// we're not checking special cases
let private transformDelegate com ctx delegateType expr =
    let expr = transformExpr com ctx expr
    match makeType com ctx.GenericArgs delegateType with
    | Fable.FunctionType(Fable.DelegateType argTypes, returnType) ->
        Replacements.uncurryExpr (Some(argTypes, returnType)) expr
    | _ -> expr

let private transformUnionCaseTest (com: IFableCompiler) (ctx: Context) (fsExpr: FSharpExpr)
                            unionExpr fsType (unionCase: FSharpUnionCase) =
    let unionExpr = transformExpr com ctx unionExpr
    match fsType with
    | ErasedUnion(tdef, genArgs) ->
        if unionCase.UnionCaseFields.Count <> 1 then
            "Erased Union Cases must have one single field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com (makeRange fsExpr.Range |> Some)
        else
            let fi = unionCase.UnionCaseFields.[0]
            let typ =
                if fi.FieldType.IsGenericParameter then
                    let name = fi.FieldType.GenericParameter.Name
                    let index =
                        tdef.GenericParameters
                        |> Seq.findIndex (fun arg -> arg.Name = name)
                    genArgs.[index]
                else fi.FieldType
            let kind = makeType com ctx.GenericArgs typ |> Fable.TypeTest
            Fable.Test(unionExpr, kind, makeRangeFrom fsExpr)
    | OptionUnion _ ->
        let kind = Fable.OptionTest(unionCase.Name <> "None")
        Fable.Test(unionExpr, kind, makeRangeFrom fsExpr)
    | ListUnion _ ->
        let kind = Fable.ListTest(unionCase.CompiledName <> "Empty")
        Fable.Test(unionExpr, kind, makeRangeFrom fsExpr)
    | StringEnum _ ->
        makeEqOp (makeRangeFrom fsExpr) unionExpr (lowerCaseName unionCase) BinaryEqualStrict
    | DiscriminatedUnion(tdef,_) ->
        let kind = Fable.UnionCaseTest(unionCase, tdef)
        Fable.Test(unionExpr, kind, makeRangeFrom fsExpr)

let private transformExpr (com: IFableCompiler) (ctx: Context) fsExpr =
    match fsExpr with
    | BasicPatterns.Coerce(targetType, Transform com ctx inpExpr) ->
        let typ = makeType com ctx.GenericArgs targetType
        match ctx.EnclosingMember, tryDefinition targetType with
        | InterfaceImplementation(Some interfaceFullName), Some interfaceEntity
            when interfaceEntity.TryFullName = Some interfaceFullName -> Fable.This typ |> Fable.Value
        | _, Some interfaceEntity when interfaceEntity.IsInterface ->
            let targetType = makeType com ctx.GenericArgs targetType
            match interfaceEntity.TryFullName, inpExpr.Type with
            | Some interfaceFullName, (Fable.DeclaredType(sourceEntity,_)) ->
                castToInterface com targetType sourceEntity interfaceFullName inpExpr
            | _ -> inpExpr
        | _ -> inpExpr

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, Transform com ctx lambda) -> lambda

    | TryGetValue (callee, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let callee, args = Option.map (transformExpr com ctx) callee, List.map (transformExpr com ctx) membArgs
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        makeCallFrom com ctx r typ (ownerGenArgs @ membGenArgs) callee args memb

    | CreateEvent (callee, eventName, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let callee, args = transformExpr com ctx callee, List.map (transformExpr com ctx) membArgs
        let callee = get None Fable.Any callee eventName
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        makeCallFrom com ctx r typ (ownerGenArgs @ membGenArgs) (Some callee) args memb

    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOf (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        let body = transformExpr com newContext body
        Replacements.iterate (makeRangeFrom fsExpr) ident body value

    (** ## Flow control *)
    | BasicPatterns.FastIntegerForLoop(Transform com ctx start, Transform com ctx limit, body, isUp) ->
        match body with
        | BasicPatterns.Lambda (BindIdent com ctx (newContext, ident), body) ->
            Fable.For (ident, start, limit, transformExpr com newContext body, isUp)
            |> makeLoop (makeRangeFrom fsExpr)
        | _ -> failwithf "Unexpected loop %O: %A" (makeRange fsExpr.Range) fsExpr

    | BasicPatterns.WhileLoop(Transform com ctx guardExpr, Transform com ctx bodyExpr) ->
        Fable.While (guardExpr, bodyExpr)
        |> makeLoop (makeRangeFrom fsExpr)

    (** Values *)
    | BasicPatterns.Const(value, FableType com ctx typ) ->
        Replacements.makeTypeConst typ value

    | BasicPatterns.BaseValue typ ->
        makeType com ctx.GenericArgs typ |> Fable.Super |> Fable.Value

    | BasicPatterns.ThisValue typ ->
        match ctx.BoundThis with
        | Some thisArg -> Fable.IdentExpr thisArg
        | _ -> makeType com ctx.GenericArgs typ |> Fable.This |> Fable.Value

    | BasicPatterns.Value var ->
        if isInline var then
            match ctx.ScopeInlineValues |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
            | Some (_,fsExpr) -> transformExpr com ctx fsExpr
            | None ->
                "Cannot resolve locally inlined value: " + var.DisplayName
                |> addErrorAndReturnNull com (makeRange fsExpr.Range |> Some)
        else
            makeValueFrom com ctx (makeRangeFrom fsExpr) var

    | BasicPatterns.DefaultValue (FableType com ctx typ) ->
        Replacements.defaultof typ

    (** ## Assignments *)
    | BasicPatterns.Let((var, value), body) ->
        if isInline var then
            let ctx = { ctx with ScopeInlineValues = (var, value)::ctx.ScopeInlineValues }
            transformExpr com ctx body
        else
            let value = transformExpr com ctx value
            let ctx, ident = bindIdentFrom com ctx var
            Fable.Let([ident, value], transformExpr com ctx body)

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
        Fable.Let(bindings, transformExpr com ctx body)

    (** ## Applications *)
    // TODO: `argTypes2` is always empty, asked about its purpose
    | BasicPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, argTypes2, argExprs) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        transformTraitCall com ctx r typ sourceTypes traitName flags argTypes argExprs

    | BasicPatterns.Call(callee, memb, ownerGenArgs, membGenArgs, args) ->
        let callee = Option.map (transformExpr com ctx) callee
        let args = List.map (transformExpr com ctx) args
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        makeCallFrom com ctx r typ (ownerGenArgs @ membGenArgs) callee args memb

    | BasicPatterns.Application(applied, genArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        match applied, args with
        // TODO: Ask why application without arguments happen. So far I've seen it
        // to access None or struct values (like the Result type)
        | _, [] -> transformExpr com ctx applied
        // Application of locally inlined lambdas
        | BasicPatterns.Value var, _ when isInline var ->
            let range = makeRangeFrom fsExpr
            match ctx.ScopeInlineValues |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
            | Some (_,fsExpr) ->
                let resolvedCtx = { ctx with GenericArgs = matchGenericParams ctx var genArgs |> Map }
                let callee = transformExpr com resolvedCtx fsExpr
                match args with
                | [] -> callee
                | args ->
                    let typ = makeType com ctx.GenericArgs fsExpr.Type
                    let args = List.map (transformExpr com ctx) args
                    Fable.Operation(Fable.CurriedApply(callee, args), typ, range)
            | None ->
                "Cannot resolve locally inlined value: " + var.DisplayName
                |> addErrorAndReturnNull com range
        // When using Fable dynamic operator, we must untuple arguments
        // Note F# compiler wraps the value in a closure if it detects it's a lambda
        | BasicPatterns.Let((_, BasicPatterns.Call(None,m,_,_,[e1; e2])),_), _
                when m.FullName = "Fable.Core.JsInterop.( ? )" ->
            let e1 = transformExpr com ctx e1
            let e2 = transformExpr com ctx e2
            let argInfo: Fable.ArgInfo =
                let args = List.map (transformExpr com ctx) args
                { argInfo (Some e1) args None with Spread = Fable.TupleSpread }
            Fable.Operation(Fable.Call(Fable.InstanceCall(Some e2), argInfo), typ, r)
        // TODO: Ask: for some reason the F# compiler translates `x.IsSome` as `Application(Call(x, get_IsSome),[unit])`
        | (BasicPatterns.Call(Some _, MemberFullName("Microsoft.FSharp.Core.IsSome"|"Microsoft.FSharp.Core.IsNone"), _, [], []) as optionProp), [BasicPatterns.Const(null, _)] ->
            transformExpr com ctx optionProp
        | _ ->
            let applied = transformExpr com ctx applied
            let args = List.map (transformExpr com ctx) args
            Fable.Operation(Fable.CurriedApply(applied, args), typ, r)

    | BasicPatterns.IfThenElse (Transform com ctx guardExpr, Transform com ctx thenExpr, Transform com ctx elseExpr) ->
        Fable.IfThenElse (guardExpr, thenExpr, elseExpr)

    | BasicPatterns.TryFinally (body, finalBody) ->
        match body with
        | BasicPatterns.TryWith(body, _, _, catchVar, catchBody) -> makeTryCatch com ctx body (Some (catchVar, catchBody)) (Some finalBody)
        | _ -> makeTryCatch com ctx body None (Some finalBody)

    | BasicPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        makeTryCatch com ctx body (Some (catchVar, catchBody)) None

    | BasicPatterns.Sequential (Transform com ctx first, Transform com ctx second) ->
        Fable.Sequential [first; second]

    (** ## Lambdas *)
    | BasicPatterns.NewDelegate(delegateType, fsExpr) ->
        transformDelegate com ctx delegateType fsExpr

    | BasicPatterns.Lambda(arg, body) ->
        let ctx, args = makeFunctionArgs com ctx [arg]
        match args with
        | [arg] -> Fable.Function(Fable.Lambda arg, transformExpr com ctx body, None)
        | _ -> failwith "makeFunctionArgs returns args with different length"

    (** ## Getters and Setters *)

    | BasicPatterns.FSharpFieldGet(callee, calleeType, field) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> entityRef com r calleeType.TypeDefinition
        if calleeType.HasTypeDefinition && calleeType.TypeDefinition.IsFSharpRecord
        then Fable.Get(callee, Fable.RecordGet(field, calleeType.TypeDefinition), typ, r)
        else get r typ callee field.Name

    | BasicPatterns.TupleGet (_tupleType, tupleElemIndex, Transform com ctx tupleExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        // makeIndexGet r typ tupleExpr tupleElemIndex
        Fable.Get(tupleExpr, Fable.TupleGet tupleElemIndex, typ, r)

    | BasicPatterns.UnionCaseGet (Transform com ctx unionExpr, fsType, unionCase, field) ->
        let range = makeRangeFrom fsExpr
        match fsType with
        | ErasedUnion _ -> unionExpr
        | StringEnum _ ->
            "StringEnum types cannot have fields"
            |> addErrorAndReturnNull com (makeRangeFrom fsExpr)
        | OptionUnion t ->
            Fable.Get(unionExpr, Fable.OptionValue, makeType com ctx.GenericArgs t, range)
        | ListUnion t ->
            let kind = if field.Name = "Head" then Fable.ListHead else Fable.ListTail
            Fable.Get(unionExpr, kind, makeType com ctx.GenericArgs t, range)
        | DiscriminatedUnion(tdef,_) ->
            let t = makeType com ctx.GenericArgs field.FieldType
            Fable.Get(unionExpr, Fable.UnionField(field, unionCase, tdef), t, range)

    | BasicPatterns.FSharpFieldSet(callee, calleeType, field, Transform com ctx value) ->
        let range = makeRangeFrom fsExpr
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> entityRef com range calleeType.TypeDefinition
        if calleeType.HasTypeDefinition && calleeType.TypeDefinition.IsFSharpRecord
        then Fable.Set(callee, Fable.RecordSet(field, calleeType.TypeDefinition), value, range)
        else Fable.Set(callee, makeStrConst field.Name |> Fable.ExprSet, value, range)

    | BasicPatterns.UnionCaseTag(Transform com ctx unionExpr, unionType) ->
        let range = makeRangeFrom fsExpr
        Fable.Get(unionExpr, Fable.UnionTag unionType.TypeDefinition, Fable.Any, range)

    | BasicPatterns.UnionCaseSet (_unionExpr, _type, _case, _caseField, _valueExpr) ->
        "Unexpected UnionCaseSet" |> addErrorAndReturnNull com (makeRangeFrom fsExpr)

    | BasicPatterns.ValueSet (valToSet, Transform com ctx valueExpr) ->
        let r = makeRangeFrom fsExpr
        match valToSet.DeclaringEntity with
        | Some ent when ent.IsFSharpModule && isPublicMember valToSet ->
            // Mutable and public module values are compiled as functions, because
            // values imported from ES2015 modules cannot be modified (see #986)
            let valToSet = makeValueFrom com ctx r valToSet
            Fable.Operation(Fable.CurriedApply(valToSet, [valueExpr]), Fable.Unit, r)
        | _ ->
            let valToSet = makeValueFrom com ctx r valToSet
            Fable.Set(valToSet, Fable.VarSet, valueExpr, r)

    (** Instantiation *)
    | BasicPatterns.NewArray(FableType com ctx elTyp, arrExprs) ->
        makeArray elTyp (arrExprs |> List.map (transformExpr com ctx))

    | BasicPatterns.NewTuple(_, argExprs) ->
        argExprs |> List.map (transformExpr com ctx) |> Fable.NewTuple |> Fable.Value

    | BasicPatterns.ObjectExpr(objType, baseCall, overrides, otherOverrides) ->
        transformObjExpr com ctx objType baseCall overrides otherOverrides

    | BasicPatterns.NewObject(memb, genArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let args = List.map (transformExpr com ctx) args
        makeCallFrom com ctx r typ genArgs None args memb

    | BasicPatterns.NewRecord(fsType, argExprs) ->
        let argExprs = List.map (transformExpr com ctx) argExprs
        let genArgs = makeGenArgs com ctx.GenericArgs fsType.GenericArguments
        Fable.NewRecord(argExprs, fsType.TypeDefinition, genArgs) |> Fable.Value

    | BasicPatterns.NewUnionCase(fsType, unionCase, argExprs) ->
        List.map (transformExpr com ctx) argExprs
        |> transformNewUnion com ctx fsExpr fsType unionCase

    (** ## Type test *)
    | BasicPatterns.TypeTest (FableType com ctx typ, Transform com ctx expr) ->
        Fable.Test(expr, Fable.TypeTest typ, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTest(unionExpr, fsType, unionCase) ->
        transformUnionCaseTest com ctx fsExpr unionExpr fsType unionCase

    (** Pattern Matching *)
    | BasicPatterns.DecisionTree(Transform com ctx decisionExpr, decisionTargets) ->
        let decisionTargets =
            decisionTargets |> List.map (fun (idents, expr) ->
                let ctx, idents =
                    (idents, (ctx, [])) ||> List.foldBack (fun ident (ctx, idents) ->
                        let ctx, ident = bindIdentFrom com ctx ident
                        ctx, ident::idents)
                idents, transformExpr com ctx expr)
        Fable.DecisionTree(decisionExpr, decisionTargets)

    | BasicPatterns.DecisionTreeSuccess(targetIndex, boundValues) ->
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        Fable.DecisionTreeSuccess(targetIndex, List.map (transformExpr com ctx) boundValues, typ)

    | BasicPatterns.ILFieldGet(None, ownerTyp, fieldName) ->
        let returnTyp = makeType com ctx.GenericArgs fsExpr.Type
        let ownerTyp = makeType com ctx.GenericArgs ownerTyp
        match Replacements.tryField returnTyp ownerTyp fieldName with
        | Some expr -> expr
        | None ->
            sprintf "Cannot compile ILFieldGet(%A, %s)" ownerTyp fieldName
            |> addErrorAndReturnNull com (makeRangeFrom fsExpr)

    | BasicPatterns.Quote _ ->
        "Quotes are not currently supported by Fable"
        |> addErrorAndReturnNull com (makeRangeFrom fsExpr)

    // TODO: Ask. I see this when accessing Result types (all structs?)
    | BasicPatterns.AddressOf(Transform com ctx expr) -> expr

    // | BasicPatterns.ILFieldSet _
    // | BasicPatterns.AddressSet _
    // | BasicPatterns.ILAsm _
    | expr ->
        sprintf "Cannot compile expression %A" expr
        |> addErrorAndReturnNull com (makeRangeFrom fsExpr)

let private isImportedOrErasedEntity (ent: FSharpEntity) =
    ent.Attributes |> Seq.exists (fun att ->
        match att.AttributeType.TryFullName with
        | Some(Atts.global_ | Atts.import | Atts.erase) -> true
        | _ -> false)

/// Is compiler generated (CompareTo...) or belongs to ignored entity?
/// (remember F# compiler puts class methods in enclosing modules)
let private isIgnoredMember (meth: FSharpMemberOrFunctionOrValue) =
    (meth.IsCompilerGenerated && Naming.ignoredCompilerGenerated.Contains meth.CompiledName)
        || Option.isSome meth.LiteralValue
        || meth.Attributes |> Seq.exists (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Atts.global_ | Atts.emit | Atts.erase) -> true
            | _ -> false)
        || Naming.ignoredInterfaceMethods.Contains meth.CompiledName
        || (match meth.DeclaringEntity with
            | Some ent -> isImportedOrErasedEntity ent
            | None -> false)

/// This function matches the pattern in F# implicit constructor when either:
/// - Calls the base constructor (if it's `obj()` will be erased)
/// - Makes checks for self referencing (as in `type Foo() as self =`): see #124
let rec private getBaseConsInfoAndBody com ctx acc body =

    let attachToThis memb value =
        Fable.Set(Fable.Value(Fable.This Fable.Any), Fable.ExprSet (makeStrConst memb), value, None)

    let transformBodyStatements com ctx acc body =
        acc @ [body] |> List.map (transformExpr com ctx) |> Fable.Sequential

    let getBaseConsInfo com ctx r (baseCall: FSharpMemberOrFunctionOrValue) baseArgs =
        // TODO: This should always be Some, but add check in case it's not
        let baseEntity = baseCall.DeclaringEntity.Value
        let baseEntityRef, baseConsRef =
            match tryImportedEntity com baseEntity with
            // Assume in imported entities (from JS), class and constructor have same reference
            | Some baseExpr -> baseExpr, baseExpr
            | None -> entityRef com r baseEntity, memberRef com r baseCall
        { Fable.BaseEntityRef = baseEntityRef
          Fable.BaseConsRef = baseConsRef
          Fable.BaseConsArgs = List.map (transformExpr com ctx) baseArgs
          Fable.BaseConsHasSpread = hasSeqSpread baseCall }

    let checkException com ctx (baseCall: FSharpMemberOrFunctionOrValue) baseArgs acc body =
        let acc = List.map (transformExpr com ctx) acc
        let body = transformExpr com ctx body
        match baseCall.DeclaringEntity, baseArgs with
        // Inheriting from JS Error doesn't work well (see https://stackoverflow.com/questions/8802845/inheriting-from-the-error-object-where-is-the-message-property)
        // Attach message and stack directly to object
        | Some ent, (Transform com ctx msg)::_ when ent.TryFullName = Some Types.exception_ ->
            [
                attachToThis "message" msg
                attachToThis "stack" <| Replacements.Helpers.stackTrace()
            ] @ acc @ [body] |> Fable.Sequential
        | _ -> acc @ [body] |> Fable.Sequential

    match body with
    | BasicPatterns.Sequential(baseCall, body) ->
        match baseCall with
        | BasicPatterns.NewObject(baseCall,_,baseArgs) ->
            None, checkException com ctx baseCall baseArgs acc body
        | BasicPatterns.Call(None,baseCall,_,_,baseArgs) as e
                when baseCall.IsConstructor && Option.isSome baseCall.DeclaringEntity ->
            let baseConsInfo = getBaseConsInfo com ctx (makeRangeFrom e) baseCall baseArgs
            Some baseConsInfo, transformBodyStatements com ctx acc body
        // This happens in constructors including self references
        | BasicPatterns.Let(_, BasicPatterns.NewObject(baseCall,_,baseArgs)) ->
            None, checkException com ctx baseCall baseArgs acc body
        // TODO: We're discarding the bound value, detect if there's a reference to it
        // in the base constructor arguments and throw an error in that case.
        | BasicPatterns.Let(_, BasicPatterns.Call(None,baseCall,_,_,baseArgs)) as e
                when baseCall.IsConstructor && Option.isSome baseCall.DeclaringEntity ->
            let baseConsInfo = getBaseConsInfo com ctx (makeRangeFrom e) baseCall baseArgs
            Some baseConsInfo, transformBodyStatements com ctx acc body
        | _ -> getBaseConsInfoAndBody com ctx (acc @ [baseCall]) body
    // TODO: Kindda unexpected, log warning?
    | body -> None, transformBodyStatements com ctx acc body

let private transformImplicitConstructor com ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    match memb.DeclaringEntity with
    | None -> "Unexpected constructor without declaring entity: " + memb.FullName
              |> addError com None; []
    | Some ent ->
        let bodyCtx, args = bindMemberArgs com ctx args
        let bodyCtx = { bodyCtx with EnclosingMember = ImplicitConstructor }
        let baseCons, body = getBaseConsInfoAndBody com bodyCtx [] body
        let name = getMemberDeclarationName com memb
        let entityName = getEntityDeclarationName com ent
        com.AddUsedVarName(name)
        com.AddUsedVarName(entityName)
        let info: Fable.ImplicitConstructorDeclarationInfo =
            { Name = name
              IsPublic = isPublicMember memb
              HasSpread = hasSeqSpread memb
              BaseConstructor = baseCons
              EntityName = entityName }
        [Fable.ImplicitConstructorDeclaration(args, body, info)]

/// When using `importMember`, uses the member display name as selector
let private importExprSelector (memb: FSharpMemberOrFunctionOrValue) selector =
    if selector = Naming.placeholder
    then getMemberDisplayName memb
    else selector

let private transformImport typ isPublic name selector path =
    let info: Fable.ValueDeclarationInfo =
        { Name = name
          IsPublic = isPublic
          // TODO!!!: compile imports as ValueDeclarations
          // (check if they're mutable, see #1314)
          IsMutable = false
          HasSpread = false }
    let fableValue = Fable.Import(selector, path, Fable.CustomImport, typ)
    [Fable.ValueDeclaration(fableValue, info)]

let private transformMemberValue (com: IFableCompiler) ctx isPublic name (memb: FSharpMemberOrFunctionOrValue) (value: FSharpExpr) =
    match transformExpr com ctx value with
    // Accept import expressions, e.g. let foo = import "foo" "myLib"
    | Fable.Import(selector, path, Fable.CustomImport, typ) ->
        match typ with
        | Fable.FunctionType(Fable.LambdaType _, Fable.FunctionType(Fable.LambdaType _, _)) ->
            "Change declaration of member: " + name + "\n"
            + "Importing JS functions with multiple arguments as `let add: int->int->int` won't uncurry parameters." + "\n"
            + "Use following syntax: `let add (x:int) (y:int): int = import ...`"
            |> addError com None
        | _ -> ()
        let selector = importExprSelector memb selector
        transformImport typ isPublic name selector path
    | fableValue ->
        let info: Fable.ValueDeclarationInfo =
            { Name = name
              IsPublic = isPublic
              IsMutable = memb.IsMutable
              HasSpread = false }
        [Fable.ValueDeclaration(fableValue, info)]

let private transformMemberFunction (com: IFableCompiler) ctx isPublic name (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body =
        match memb.IsConstructor, memb.DeclaringEntity with
        | true, Some ent ->
            let bodyCtx =
                ent.TryFullName |> Option.map (fun fullName ->
                    { bodyCtx with EnclosingMember = SecondaryConstructor fullName })
                |> Option.defaultValue bodyCtx
            transformExpr com bodyCtx body
        | _ -> transformExpr com bodyCtx body
    match body with
    // Accept import expressions , e.g. let foo x y = import "foo" "myLib"
    | Fable.Import(selector, path, Fable.CustomImport, _) ->
        // Use the full function type
        let typ = makeType com Map.empty memb.FullType
        let selector = importExprSelector memb selector
        transformImport typ isPublic name selector path
    | body ->
        let fn = Fable.Function(Fable.Delegate args, body, Some name)
        // If this is a static constructor, call it immediately
        if memb.CompiledName = ".cctor"
        then
            let apply = staticCall None Fable.Unit (argInfo None [] None) fn
            [Fable.ActionDeclaration apply]
        else
            // TODO!!! If the method has generics with constraints, force inline
            let info: Fable.ValueDeclarationInfo =
                { Name = name
                  IsPublic = isPublic
                  IsMutable = false
                  HasSpread = hasSeqSpread memb }
            [Fable.ValueDeclaration(fn, info)]

let private transformMemberFunctionOrValue (com: IFableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let isPublic = isPublicMember memb
    let name = getMemberDeclarationName com memb
    com.AddUsedVarName(name)
    match tryImportAttribute memb.Attributes with
    | Some(selector, path) ->
        let typ = makeType com Map.empty memb.FullType
        transformImport typ isPublic name selector path
    | None ->
        if isModuleValueForDeclarations memb
        then transformMemberValue com ctx isPublic name memb body
        else transformMemberFunction com ctx isPublic name memb args body

/// TODO!!! We cannot implement overrides on records and unions, throw error (at least for now)
let private transformOverride (com: FableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    match memb.DeclaringEntity with
    | None -> "Unexpected override without declaring entity: " + memb.FullName
              |> addError com None; []
    | Some ent when ent.IsFSharpRecord || ent.IsFSharpUnion ->
        sprintf "Current version cannot implement %s for records and unions" memb.DisplayName
        |> addError com None; []
    | Some ent ->
        let bodyCtx, args = bindMemberArgs com ctx args
        let body = transformExpr com bodyCtx body
        let kind =
            match args with
            | [_thisArg; unitArg] when memb.IsPropertyGetterMethod && unitArg.Type = Fable.Unit ->
                Fable.ObjectGetter
            | [_thisArg; _valueArg] when memb.IsPropertySetterMethod ->
                Fable.ObjectSetter
            | _ when memb.CompiledName = "System-Collections-Generic-IEnumerable`1-GetEnumerator" ->
                Fable.ObjectIterator
            | _ -> Fable.ObjectMethod (hasSeqSpread memb)
        let info: Fable.OverrideDeclarationInfo =
            { Name = memb.DisplayName
              Kind = kind
              EntityName = getEntityDeclarationName com ent }
        [Fable.OverrideDeclaration(args, body, info)]

let private transformInterfaceImplementation (com: FableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    if Set.contains memb.CompiledName Naming.interfaceMethodsImplementedInPrototype
    then transformOverride com ctx memb args body
    else
        let bodyCtx, args = bindMemberArgs com ctx args
        let interfaceFullName = tryGetInterfaceDefinitionFromMethod memb |> Option.bind (fun ent -> ent.TryFullName)
        let bodyCtx = { bodyCtx with EnclosingMember = InterfaceImplementation interfaceFullName }
        let body = transformExpr com bodyCtx body
        let value = Fable.Function(Fable.Delegate args, body, None)
        let kind =
            if memb.IsPropertyGetterMethod && countNonCurriedParams memb = 0
            then Fable.ObjectGetter
            elif memb.IsPropertySetterMethod && countNonCurriedParams memb = 1
            then Fable.ObjectSetter
            else hasSeqSpread memb |> Fable.ObjectMethod
        let objMember = makeStrConst memb.DisplayName, value, kind
        com.AddInterfaceImplementation(memb, objMember)
        []

let private transformMemberDecl (com: FableCompiler) (ctx: Context) (memb: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    if isIgnoredMember memb
    then []
    elif isInline memb then
        // TODO: Compiler flag to output inline expressions? (e.g. for REPL libs)
        com.AddInlineExpr(memb, (List.concat args, body))
        []
    elif memb.IsImplicitConstructor
    then transformImplicitConstructor com ctx memb args body
    elif memb.IsExplicitInterfaceImplementation
    then transformInterfaceImplementation com ctx memb args body
    elif memb.IsOverrideOrExplicitInterfaceImplementation
    then transformOverride com ctx memb args body
    else transformMemberFunctionOrValue com ctx memb args body

let private transformDeclarations (com: FableCompiler) fsDecls =
    let rec transformDeclarationsInner com (ctx: Context) fsDecls =
        fsDecls |> List.collect (fun fsDecl ->
            match fsDecl with
            | FSharpImplementationFileDeclaration.Entity(ent, sub) ->
                match tryImportAttribute ent.Attributes with
                | Some(selector, path) ->
                    let name = getEntityDeclarationName com ent
                    (com :> IFableCompiler).AddUsedVarName(name)
                    transformImport Fable.Any (not ent.Accessibility.IsPrivate) name selector path
                | None ->
                    transformDeclarationsInner com ctx sub
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(meth, args, body) ->
                transformMemberDecl com ctx meth args body
            | FSharpImplementationFileDeclaration.InitAction fe ->
                let e = transformExpr com ctx fe
                let decl = Fable.ActionDeclaration e
                [decl])
    let decls = transformDeclarationsInner com (Context.Create()) fsDecls
    let interfaceImplementations =
        com.InterfaceImplementations.Values |> Seq.map (fun (info, objMember) ->
            Fable.InterfaceCastDeclaration(Seq.toList objMember, info)) |> Seq.toList
    decls @ interfaceImplementations

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
    let rec getRootModuleAndDeclsInner outerEnt decls =
        match decls with
        | [FSharpImplementationFileDeclaration.Entity (ent, decls)]
                when ent.IsFSharpModule || ent.IsNamespace ->
            getRootModuleAndDeclsInner (Some ent) decls
        | CommonNamespace(ent, decls) ->
            getRootModuleAndDeclsInner (Some ent) decls
        | decls -> outerEnt, decls
    getRootModuleAndDeclsInner None decls

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

type FableCompiler(com: ICompiler, implFiles: Map<string, FSharpImplementationFileContents>) =
    member val UsedVarNames = HashSet<string>()
    member val Dependencies = HashSet<string>()
    member val InterfaceImplementations: Dictionary<_,_> = Dictionary()
    member __.AddInlineExpr(memb, inlineExpr) =
        let fullName = getMemberUniqueName com memb
        com.GetOrAddInlineExpr(fullName, fun () -> inlineExpr) |> ignore
    member this.AddInterfaceImplementation(memb: FSharpMemberOrFunctionOrValue, objMemb: Fable.ObjectMember) =
        match memb.DeclaringEntity, tryGetInterfaceDefinitionFromMethod memb with
        | Some implementingEntity, Some interfaceEntity ->
            let castFunctionName = getCastDeclarationName com implementingEntity interfaceEntity.FullName
            let inheritedInterfaces =
                if interfaceEntity.AllInterfaces.Count > 1 then
                    interfaceEntity.AllInterfaces |> Seq.choose (fun t ->
                        if t.HasTypeDefinition then
                            let fullName = getCastDeclarationName com implementingEntity t.TypeDefinition.FullName
                            if fullName <> castFunctionName then Some fullName else None
                        else None)
                    |> Seq.toList
                else []
            match this.InterfaceImplementations.TryGetValue(castFunctionName) with
            | false, _ ->
                let info: Fable.InterfaceCastDeclarationInfo =
                    { Name = castFunctionName
                      IsPublic = not implementingEntity.Accessibility.IsPrivate
                      ImplementingType = implementingEntity
                      InterfaceType = interfaceEntity
                      InheritedInterfaces = inheritedInterfaces
                    }
                let members = ResizeArray()
                members.Add(objMemb)
                this.UsedVarNames.Add(castFunctionName) |> ignore
                this.InterfaceImplementations.Add(castFunctionName, (info, members) )
            | true, (_, members) -> members.Add(objMemb)
        | _ ->
            "Cannot find implementing and/or interface entities for " + memb.FullName
            |> addError com None
    interface IFableCompiler with
        member this.Transform(ctx, fsExpr) =
            transformExpr this ctx fsExpr
        member this.TryReplace(ctx, r, t, info, thisArg, args) =
            Replacements.tryCall this ctx r t info thisArg args
        member __.TryReplaceInterfaceCast(t, name, e) =
            Replacements.tryReplaceInterfaceCast t name e
        member this.GetInlineExpr(memb) =
            let fileName = (getMemberLocation memb).FileName |> Path.normalizePath
            if fileName <> com.CurrentFile then
                this.Dependencies.Add(fileName) |> ignore
            let fullName = getMemberUniqueName com memb
            com.GetOrAddInlineExpr(fullName, fun () ->
                match tryGetMemberArgsAndBody implFiles fileName memb with
                | Some(args, body) -> List.concat args, body
                | None -> failwith ("Cannot find inline member " + memb.FullName))
        member this.AddUsedVarName(varName) =
            this.UsedVarNames.Add(varName) |> ignore
    interface ICompiler with
        member __.Options = com.Options
        member __.FableCore = com.FableCore
        member __.CurrentFile = com.CurrentFile
        member __.GetUniqueVar(name) =
            com.GetUniqueVar(?name=name)
        member __.GetRootModule(fileName) =
            com.GetRootModule(fileName)
        member __.GetOrAddInlineExpr(fullName, generate) =
            com.GetOrAddInlineExpr(fullName, generate)
        member __.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

let getRootModuleFullName (file: FSharpImplementationFileContents) =
    let rootEnt, _ = getRootModuleAndDecls file.Declarations
    match rootEnt with
    | Some rootEnt -> getEntityFullName rootEnt
    | None -> ""

let transformFile (com: ICompiler) (implFiles: Map<string, FSharpImplementationFileContents>) =
    try
        let file =
            match Map.tryFind com.CurrentFile implFiles with
            | Some file -> file
            | None -> failwithf "File %s doesn't belong to parsed project" com.CurrentFile
        let fcom = FableCompiler(com, implFiles)
        let _, rootDecls = getRootModuleAndDecls file.Declarations
        let rootDecls = transformDeclarations fcom rootDecls
        Fable.File(com.CurrentFile, rootDecls, set fcom.UsedVarNames, set fcom.Dependencies)
    with
    | ex -> exn (sprintf "%s (%s)" ex.Message com.CurrentFile, ex) |> raise
