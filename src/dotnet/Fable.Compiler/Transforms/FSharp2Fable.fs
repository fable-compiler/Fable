module rec Fable.Transforms.FSharp2Fable.Compiler

open System.Collections.Generic
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.Transforms

open MonadicTrampoline
open Patterns
open TypeHelpers
open Identifiers
open Helpers
open Util

let inline private transformExprList com ctx xs = trampolineListMap (transformExpr com ctx) xs
let inline private transformExprOpt com ctx opt = trampolineOptionMap (transformExpr com ctx) opt

let private transformNewUnion com ctx r fsType
                (unionCase: FSharpUnionCase) (argExprs: Fable.Expr list) =
    match fsType with
    | ErasedUnion(_, genArgs) ->
        match argExprs with
        | [expr] ->
            let genArgs = makeGenArgs com ctx.GenericArgs genArgs
            Fable.NewErasedUnion(expr, genArgs) |> Fable.Value
        | _ -> "Erased Union Cases must have one single field: " + (getFsTypeFullName fsType)
               |> addErrorAndReturnNull com ctx.InlinePath r
    | StringEnum(tdef, rule) ->
        let enumName = defaultArg tdef.TryFullName Naming.unknown
        match argExprs with
        | [] -> Fable.Enum(applyCaseRule rule unionCase |> Fable.StringEnum, enumName) |> Fable.Value
        | _ -> "StringEnum types cannot have fields: " + enumName
               |> addErrorAndReturnNull com ctx.InlinePath r
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
    let makeCallInfo traitName entityFullName argTypes genArgs: Fable.ReplaceCallInfo =
        { SignatureArgTypes = argTypes
          DeclaringEntityFullName = entityFullName
          Spread = Fable.NoSpread
          CompiledName = traitName
          OverloadSuffix = lazy ""
          GenericArgs =
            // TODO: Check the source F# entity to get the actual gen param names?
            match genArgs with
            | [] -> []
            | [genArg] -> ["T", genArg]
            | genArgs -> genArgs |> List.mapi (fun i genArg -> "T" + string i, genArg)
        }

    let resolveMemberCall (entity: FSharpEntity) genArgs membCompiledName isInstance argTypes thisArg args =
        let genArgs = matchGenericParams genArgs entity.GenericParameters
        tryFindMember com entity (Map genArgs) membCompiledName isInstance argTypes
        |> Option.map (fun memb -> makeCallFrom com ctx r typ false [] thisArg args memb)

    let isInstance = flags.IsInstance
    let argTypes = List.map (makeType com ctx.GenericArgs) argTypes
    let argExprs = List.map (fun e -> com.Transform(ctx, e)) argExprs
    let thisArg, args, argTypes =
        match argExprs, argTypes with
        | thisArg::args, _::argTypes when isInstance -> Some thisArg, args, argTypes
        | args, argTypes -> None, args, argTypes

    sourceTypes |> Seq.tryPick (fun t ->
        match makeType com ctx.GenericArgs t with
        // Types with specific entry in Fable.AST
        // TODO: Check other types like booleans or numbers?
        | Fable.String ->
            let info = makeCallInfo traitName Types.string argTypes []
            Replacements.strings com ctx r typ info thisArg args
        | Fable.Option genArg ->
            let info = makeCallInfo traitName Types.option argTypes [genArg]
            Replacements.options com ctx r typ info thisArg args
        | Fable.Array genArg ->
            let info = makeCallInfo traitName Types.array argTypes [genArg]
            Replacements.arrays com ctx r typ info thisArg args
        | Fable.List genArg ->
            let info = makeCallInfo traitName Types.list argTypes [genArg]
            Replacements.lists com ctx r typ info thisArg args
        // Declared types not in Fable AST
        | Fable.DeclaredType(entity, genArgs) ->
            // SRTP only works for records if there are no arguments
            if isInstance && entity.IsFSharpRecord && List.isEmpty args && Option.isSome thisArg then
                let fieldName = Naming.removeGetSetPrefix traitName
                entity.FSharpFields |> Seq.tryPick (fun fi ->
                    if fi.Name = fieldName then
                        let kind = Fable.FieldGet(fi.Name, fi.IsMutable, makeType com Map.empty fi.FieldType)
                        Fable.Get(thisArg.Value, kind, typ, r) |> Some
                    else None)
                |> Option.orElseWith (fun () ->
                    resolveMemberCall entity genArgs traitName isInstance argTypes thisArg args)
            else resolveMemberCall entity genArgs traitName isInstance argTypes thisArg args
        | _ -> None
    ) |> Option.defaultWith (fun () ->
        "Cannot resolve trait call " + traitName |> addErrorAndReturnNull com ctx.InlinePath r)

let private transformObjExpr (com: IFableCompiler) (ctx: Context) (objType: FSharpType)
                    baseCallExpr (overrides: FSharpObjectExprOverride list) otherOverrides =

    let mapOverride (over: FSharpObjectExprOverride) =
      trampoline {
        let ctx, args = bindMemberArgs com ctx over.CurriedParameterGroups
        let! body = transformExpr com ctx over.Body
        let value = Fable.Function(Fable.Delegate args, body, None)
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
                    | Some Types.ienumerableGeneric ->
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
        return Fable.ObjectMember(makeStrConst name, value, kind)
      }

    trampoline {
      let! baseCall =
        trampoline {
            match baseCallExpr with
            // TODO: For interface implementations this should be BasicPatterns.NewObject
            // but check the baseCall.DeclaringEntity name just in case
            | BasicPatterns.Call(None,baseCall,genArgs1,genArgs2,baseArgs) ->
                match baseCall.DeclaringEntity with
                | Some baseType when baseType.TryFullName <> Some Types.object ->
                    let typ = makeType com ctx.GenericArgs baseCallExpr.Type
                    let! baseArgs = transformExprList com ctx baseArgs
                    let genArgs = genArgs1 @ genArgs2 |> Seq.map (makeType com ctx.GenericArgs)
                    return makeCallFrom com ctx None typ false genArgs None baseArgs baseCall |> Some
                | _ -> return None
            | _ -> return None
        }

      let! members =
        (objType, overrides)::otherOverrides
        |> trampolineListMap (fun (typ, overrides) ->
            overrides |> trampolineListMap mapOverride)

      return Fable.ObjectExpr(members |> List.concat, makeType com ctx.GenericArgs objType, baseCall)
    }

// TODO: Check code in Fable 1 and which tests fail because
// we're not checking special cases
let private transformDelegate com ctx delegateType expr =
  trampoline {
    let! expr = transformExpr com ctx expr
    match makeType com ctx.GenericArgs delegateType with
    | Fable.FunctionType(Fable.DelegateType argTypes, _) ->
        let arity = List.length argTypes
        match expr with
        | LambdaUncurriedAtCompileTime (Some arity) lambda -> return lambda
        | _ -> return Replacements.uncurryExprAtRuntime arity expr
    | _ -> return expr
  }

let private transformUnionCaseTest (com: IFableCompiler) (ctx: Context) r
                            unionExpr fsType (unionCase: FSharpUnionCase) =
  trampoline {
    let! unionExpr = transformExpr com ctx unionExpr
    match fsType with
    | ErasedUnion(tdef, genArgs) ->
        if unionCase.UnionCaseFields.Count <> 1 then
            return "Erased Union Cases must have one single field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com ctx.InlinePath r
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
            return Fable.Test(unionExpr, kind, r)
    | OptionUnion _ ->
        let kind = Fable.OptionTest(unionCase.Name <> "None")
        return Fable.Test(unionExpr, kind, r)
    | ListUnion _ ->
        let kind = Fable.ListTest(unionCase.CompiledName <> "Empty")
        return Fable.Test(unionExpr, kind, r)
    | StringEnum(_, rule) ->
        return makeEqOp r unionExpr (applyCaseRule rule unionCase) BinaryEqualStrict
    | DiscriminatedUnion(tdef,_) ->
        let kind = Fable.UnionCaseTest(unionCase, tdef)
        return Fable.Test(unionExpr, kind, r)
  }

let private transformExpr (com: IFableCompiler) (ctx: Context) fsExpr =
  trampoline {
    let r = makeRangeFrom fsExpr
    let typ = makeType com ctx.GenericArgs fsExpr.Type
    match fsExpr with

    | BasicPatterns.Coerce(targetType, inpExpr) ->
        let! (inpExpr: Fable.Expr) = transformExpr com ctx inpExpr
        match tryDefinition targetType with
        | Some(interfaceEntity, Some interfaceFullName) when interfaceEntity.IsInterface ->
            let targetType = makeType com ctx.GenericArgs targetType
            match inpExpr.Type with
            | Fable.DeclaredType(sourceEntity,_) ->
                return castToInterface com ctx r targetType sourceEntity interfaceFullName inpExpr
            | _ ->
                return Replacements.tryInterfaceCast r targetType interfaceFullName inpExpr
                |> Option.defaultValue inpExpr
        | _ -> return inpExpr

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, lambda) ->
        let! lambda = transformExpr com ctx lambda
        return lambda

    | TryGetValue (callee, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let! callee = transformExprOpt com ctx callee
        let! args = transformExprList com ctx membArgs
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        return makeCallFrom com ctx r typ false genArgs callee args memb

    | CreateEvent (callee, eventName, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let! callee = transformExpr com ctx callee
        let! args = transformExprList com ctx membArgs
        let callee = get None Fable.Any callee eventName
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        return makeCallFrom com ctx r typ false genArgs (Some callee) args memb

    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOf (BindIdent com ctx (newContext, ident), value, body) ->
        let! value = transformExpr com ctx value
        let! body = transformExpr com newContext body
        return Replacements.iterate r ident body value

    // Flow control
    | BasicPatterns.FastIntegerForLoop(start, limit, body, isUp) ->
        match body with
        | BasicPatterns.Lambda (BindIdent com ctx (newContext, ident), body) ->
            let! start = transformExpr com ctx start
            let! limit = transformExpr com ctx limit
            let! body = transformExpr com newContext body
            return Fable.For (ident, start, limit, body, isUp)
            |> makeLoop r
        | _ -> return failwithf "Unexpected loop %O: %A" r fsExpr

    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) ->
        let! guardExpr = transformExpr com ctx guardExpr
        let! bodyExpr = transformExpr com ctx bodyExpr
        return Fable.While (guardExpr, bodyExpr)
        |> makeLoop r

    // Values
    | BasicPatterns.Const(value, FableType com ctx typ) ->
        return Replacements.makeTypeConst typ value

    | BasicPatterns.BaseValue typ ->
        let typ = makeType com Map.empty typ
        match ctx.BoundMemberThis, ctx.BoundConstructorThis with
        | Some thisArg, _ | _, Some thisArg ->
            return { thisArg with Kind = Fable.BaseValueIdent; Type = typ } |> Fable.IdentExpr
        | _ ->
            addError com ctx.InlinePath r "Unexpected unbound this for base value"
            return Fable.Value(Fable.Null Fable.Any)

    | BasicPatterns.ThisValue _typ ->
        // NOTE: We don't check ctx.BoundMemberThis here because F# compiler doesn't represent
        // `this` in members as BasicPatterns.ThisValue (but BasicPatterns.Value)
        match ctx.BoundConstructorThis with
        | Some thisArg -> return Fable.IdentExpr thisArg
        | _ ->
            addError com ctx.InlinePath r "Unexpected unbound this"
            return Fable.Value(Fable.Null Fable.Any)

    | BasicPatterns.Value var ->
        if isInline var then
            match ctx.ScopeInlineValues |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
            | Some (_,fsExpr) ->
                return! transformExpr com ctx fsExpr
            | None ->
                return "Cannot resolve locally inlined value: " + var.DisplayName
                |> addErrorAndReturnNull com ctx.InlinePath r
        else
            return makeValueFrom com ctx r var

    | BasicPatterns.DefaultValue (FableType com ctx typ) ->
        return Replacements.defaultof typ

    // Assignments
    | BasicPatterns.Let((var, value), body) ->
        if isInline var then
            let ctx = { ctx with ScopeInlineValues = (var, value)::ctx.ScopeInlineValues }
            return! transformExpr com ctx body
        else
            let! value = transformExpr com ctx value
            let ctx, ident = bindIdentFrom com ctx var
            let! body = transformExpr com ctx body
            return Fable.Let([ident, value], body)

    | BasicPatterns.LetRec(recBindings, body) ->
        // First get a context containing all idents and use it compile the values
        let ctx, idents =
            (recBindings, (ctx, []))
            ||> List.foldBack (fun (BindIdent com ctx (newContext, ident), _) (ctx, idents) ->
                (newContext, ident::idents))
        let _, bindingExprs = List.unzip recBindings
        let! exprs = transformExprList com ctx bindingExprs
        let bindings = List.zip idents exprs
        let! body = transformExpr com ctx body
        return Fable.Let(bindings, body)

    // Applications
    // TODO: `argTypes2` is always empty, asked about its purpose
    | BasicPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, _argTypes2, argExprs) ->
        return transformTraitCall com ctx r typ sourceTypes traitName flags argTypes argExprs

    | BasicPatterns.Call(callee, memb, ownerGenArgs, membGenArgs, args) ->
        let! callee = transformExprOpt com ctx callee
        let! args = transformExprList com ctx args
        // TODO: Check answer to #868 in FSC repo
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        return makeCallFrom com ctx r typ false genArgs callee args memb

    | BasicPatterns.Application(applied, genArgs, []) ->
        // TODO: Ask why application without arguments happen. So far I've seen it
        // to access None or struct values (like the Result type)
        return! transformExpr com ctx applied

    // Application of locally inlined lambdas
    | BasicPatterns.Application(BasicPatterns.Value var, genArgs, args) when isInline var ->
        match ctx.ScopeInlineValues |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
        | Some (_,fsExpr) ->
            let genArgs = Seq.map (makeType com ctx.GenericArgs) genArgs
            let resolvedCtx = { ctx with GenericArgs = matchGenericParamsFrom var genArgs |> Map }
            let! callee = transformExpr com resolvedCtx fsExpr
            match args with
            | [] -> return callee
            | args ->
                let typ = makeType com ctx.GenericArgs fsExpr.Type
                let! args = transformExprList com ctx args
                return Fable.Operation(Fable.CurriedApply(callee, args), typ, r)
        | None ->
            return "Cannot resolve locally inlined value: " + var.DisplayName
            |> addErrorAndReturnNull com ctx.InlinePath r

    // When using Fable dynamic operator, we must untuple arguments
    // Note F# compiler wraps the value in a closure if it detects it's a lambda
    | BasicPatterns.Application(BasicPatterns.Let((_, BasicPatterns.Call(None,m,_,_,[e1; e2])),_), genArgs, args)
            when m.FullName = "Fable.Core.JsInterop.( ? )" ->
        let! e1 = transformExpr com ctx e1
        let! e2 = transformExpr com ctx e2
        let! args = transformExprList com ctx args
        let argInfo: Fable.ArgInfo =
            { argInfo (Some e1) args Fable.AutoUncurrying with Spread = Fable.TupleSpread }
        return Fable.Operation(Fable.Call(Fable.InstanceCall(Some e2), argInfo), typ, r)

    // TODO: Ask: for some reason the F# compiler translates `x.IsSome` as `Application(Call(x, get_IsSome),[unit])`
    | BasicPatterns.Application(BasicPatterns.Call(Some _, memb, _, [], []) as optionProp, genArgs, [BasicPatterns.Const(null, _)])
        when memb.FullName = "Microsoft.FSharp.Core.IsSome" || memb.FullName = "Microsoft.FSharp.Core.IsNone" ->
        return! transformExpr com ctx optionProp

    | BasicPatterns.Application(applied, genArgs, args) ->
        let! applied = transformExpr com ctx applied
        let! args = transformExprList com ctx args
        return Fable.Operation(Fable.CurriedApply(applied, args), typ, r)

    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        let! guardExpr = transformExpr com ctx guardExpr
        let! thenExpr = transformExpr com ctx thenExpr
        let! elseExpr = transformExpr com ctx elseExpr
        return Fable.IfThenElse (guardExpr, thenExpr, elseExpr)

    | BasicPatterns.TryFinally (body, finalBody) ->
        match body with
        | BasicPatterns.TryWith(body, _, _, catchVar, catchBody) ->
            return makeTryCatch com ctx body (Some (catchVar, catchBody)) (Some finalBody)
        | _ -> return makeTryCatch com ctx body None (Some finalBody)

    | BasicPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        return makeTryCatch com ctx body (Some (catchVar, catchBody)) None

    // Lambdas
    | BasicPatterns.NewDelegate(delegateType, fsExpr) ->
        return! transformDelegate com ctx delegateType fsExpr

    | BasicPatterns.Lambda(arg, body) ->
        let ctx, args = makeFunctionArgs com ctx [arg]
        match args with
        | [arg] ->
            let! body = transformExpr com ctx body
            return Fable.Function(Fable.Lambda arg, body, None)
        | _ -> return failwith "makeFunctionArgs returns args with different length"

    // Getters and Setters
    | BasicPatterns.FSharpFieldGet(callee, calleeType, field) ->
        let! callee = transformExprOpt com ctx callee
        let callee =
            match callee with
            | Some callee -> callee
            | None -> entityRef com calleeType.TypeDefinition
        let kind = Fable.FieldGet(field.Name, field.IsMutable, makeType com Map.empty field.FieldType)
        return Fable.Get(callee, kind, typ, r)

    | BasicPatterns.TupleGet(_tupleType, tupleElemIndex, tupleExpr) ->
        let! tupleExpr = transformExpr com ctx tupleExpr
        return Fable.Get(tupleExpr, Fable.TupleGet tupleElemIndex, typ, r)

    | BasicPatterns.UnionCaseGet (unionExpr, fsType, unionCase, field) ->
        let! unionExpr = transformExpr com ctx unionExpr
        match fsType with
        | ErasedUnion _ -> return unionExpr
        | StringEnum _ ->
            return "StringEnum types cannot have fields"
            |> addErrorAndReturnNull com ctx.InlinePath r
        | OptionUnion t ->
            return Fable.Get(unionExpr, Fable.OptionValue, makeType com ctx.GenericArgs t, r)
        | ListUnion t ->
            let t = makeType com ctx.GenericArgs t
            let kind, t =
                if field.Name = "Head"
                then Fable.ListHead, t
                else Fable.ListTail, Fable.List t
            return Fable.Get(unionExpr, kind, t, r)
        | DiscriminatedUnion _ ->
            let t = makeType com Map.empty field.FieldType
            let kind = Fable.UnionField(field, unionCase, t)
            return Fable.Get(unionExpr, kind, typ, r)

    | BasicPatterns.FSharpFieldSet(callee, calleeType, field, value) ->
        let! callee = transformExprOpt com ctx callee
        let! value = transformExpr com ctx value
        let callee =
            match callee with
            | Some callee -> callee
            | None -> entityRef com calleeType.TypeDefinition
        return Fable.Set(callee, Fable.FieldSet(field.Name, makeType com Map.empty field.FieldType), value, r)

    | BasicPatterns.UnionCaseTag(unionExpr, _unionType) ->
        let! unionExpr = transformExpr com ctx unionExpr
        return Fable.Get(unionExpr, Fable.UnionTag, Fable.Any, r)

    | BasicPatterns.UnionCaseSet (_unionExpr, _type, _case, _caseField, _valueExpr) ->
        return "Unexpected UnionCaseSet" |> addErrorAndReturnNull com ctx.InlinePath r

    | BasicPatterns.ValueSet (valToSet, valueExpr) ->
        let! valueExpr = transformExpr com ctx valueExpr
        match valToSet.DeclaringEntity with
        | Some ent when ent.IsFSharpModule && isPublicMember valToSet ->
            // Mutable and public module values are compiled as functions, because
            // values imported from ES2015 modules cannot be modified (see #986)
            let valToSet = makeValueFrom com ctx r valToSet
            return Fable.Operation(Fable.CurriedApply(valToSet, [valueExpr]), Fable.Unit, r)
        | _ ->
            let valToSet = makeValueFrom com ctx r valToSet
            return Fable.Set(valToSet, Fable.VarSet, valueExpr, r)

    // Instantiation
    | BasicPatterns.NewArray(FableType com ctx elTyp, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return makeArray elTyp argExprs

    | BasicPatterns.NewTuple(_tupleType, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return Fable.NewTuple(argExprs) |> Fable.Value

    | BasicPatterns.ObjectExpr(objType, baseCall, overrides, otherOverrides) ->
        return! transformObjExpr com ctx objType baseCall overrides otherOverrides

    | BasicPatterns.NewObject(memb, genArgs, args) ->
        let! args = transformExprList com ctx args
        let genArgs = Seq.map (makeType com ctx.GenericArgs) genArgs
        return makeCallFrom com ctx r typ false genArgs None args memb

    | BasicPatterns.Sequential(ConstructorCall(baseCall,_,_), BasicPatterns.NewRecord(fsType, argExprs)) ->
        match baseCall.DeclaringEntity with
        | Some baseEnt when baseEnt.TryFullName = Some Types.object ->
            let! argExprs = transformExprList com ctx argExprs
            let genArgs = makeGenArgs com ctx.GenericArgs (getGenericArguments fsType)
            return Fable.NewRecord(argExprs, fsType.TypeDefinition, genArgs) |> Fable.Value
        | _ ->
            return "Internal constructor with inheritance are not supported"
            |> addErrorAndReturnNull com ctx.InlinePath r

    | BasicPatterns.Sequential (first, second) ->
        let! first = transformExpr com ctx first
        let! second = transformExpr com ctx second
        return Fable.Sequential [first; second]

    | BasicPatterns.NewRecord(fsType, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        let genArgs = makeGenArgs com ctx.GenericArgs (getGenericArguments fsType)
        return Fable.NewRecord(argExprs, fsType.TypeDefinition, genArgs) |> Fable.Value

    | BasicPatterns.NewUnionCase(fsType, unionCase, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return argExprs
        |> transformNewUnion com ctx r fsType unionCase

    // Type test
    | BasicPatterns.TypeTest (FableType com ctx typ, expr) ->
        let! expr = transformExpr com ctx expr
        return Fable.Test(expr, Fable.TypeTest typ, r)

    | BasicPatterns.UnionCaseTest(unionExpr, fsType, unionCase) ->
        return! transformUnionCaseTest com ctx r unionExpr fsType unionCase

    // Pattern Matching
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        let! decisionExpr = transformExpr com ctx decisionExpr
        let rec transformDecisionTargets acc xs =
            trampoline {
                match xs with
                | [] -> return List.rev acc
                | (idents, expr)::tail ->
                    let ctx, idents =
                        (idents, (ctx, [])) ||> List.foldBack (fun ident (ctx, idents) ->
                            let ctx, ident = bindIdentFrom com ctx ident
                            ctx, ident::idents)
                    let! expr = transformExpr com ctx expr
                    return! transformDecisionTargets ((idents, expr)::acc) tail
            }
        let! decisionTargets = transformDecisionTargets [] decisionTargets
        return Fable.DecisionTree(decisionExpr, decisionTargets)

    | BasicPatterns.DecisionTreeSuccess(targetIndex, boundValues) ->
        let! boundValues = transformExprList com ctx boundValues
        return Fable.DecisionTreeSuccess(targetIndex, boundValues, typ)

    | BasicPatterns.ILFieldGet(None, ownerTyp, fieldName) ->
        let ownerTyp = makeType com ctx.GenericArgs ownerTyp
        match Replacements.tryField typ ownerTyp fieldName with
        | Some expr -> return expr
        | None ->
            return sprintf "Cannot compile ILFieldGet(%A, %s)" ownerTyp fieldName
            |> addErrorAndReturnNull com ctx.InlinePath r

    | BasicPatterns.Quote _ ->
        return "Quotes are not currently supported by Fable"
        |> addErrorAndReturnNull com ctx.InlinePath r

    // TODO: Ask. I see this when accessing Result types (all structs?)
    | BasicPatterns.AddressOf(expr) ->
        let! expr = transformExpr com ctx expr
        return expr

    // | BasicPatterns.ILFieldSet _
    // | BasicPatterns.AddressSet _
    // | BasicPatterns.ILAsm _
    | expr ->
        return sprintf "Cannot compile expression %A" expr
        |> addErrorAndReturnNull com ctx.InlinePath r
  }

let private isImportedEntity (ent: FSharpEntity) =
    ent.Attributes |> Seq.exists (fun att ->
        match att.AttributeType.TryFullName with
        | Some(Atts.global_ | Atts.import) -> true
        | _ -> false)

let private isErasedUnion (ent: FSharpEntity) =
    ent.Attributes |> Seq.exists (fun att ->
        match att.AttributeType.TryFullName with
        | Some(Atts.erase | Atts.stringEnum) -> true
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
        || (match meth.DeclaringEntity with
            | Some ent -> isImportedEntity ent
            | None -> false)

/// This function matches the pattern in F# implicit constructor when either:
/// - Calls the base constructor
/// - Makes checks for self referencing (as in `type Foo() as self =`): see #124
let rec private getBaseConsAndBody com ctx (baseType: FSharpType option) acc body =
    let transformBodyStatements com ctx acc body =
        acc @ [body] |> transformExprList com ctx |> run

    let getBaseConsInfo com ctx r (baseCall: FSharpMemberOrFunctionOrValue) genArgs baseArgs =
        baseType |> Option.bind (fun (NonAbbreviatedType baseType) ->
            if baseType.HasTypeDefinition then
                let ent = baseType.TypeDefinition
                match ent.TryFullName with
                | Some name when name <> Types.object -> Some ent
                | _ -> None
            else None)
        |> Option.map (fun baseEntity ->
            let thisArg = ctx.BoundConstructorThis |> Option.map Fable.IdentExpr
            let baseArgs = transformExprList com ctx baseArgs |> run
            let genArgs = genArgs |> Seq.map (makeType com ctx.GenericArgs)
            match Replacements.tryBaseConstructor com baseEntity baseCall genArgs baseArgs with
            | Some(baseRef, args) ->
                // TODO: Should Replacements.tryBaseConstructor return the argInfo?
                let argInfo: Fable.ArgInfo =
                  { ThisArg = thisArg
                    Args = args
                    SignatureArgTypes = getArgTypes com baseCall |> Fable.Typed
                    Spread = Fable.NoSpread
                    IsBaseOrSelfConstructorCall = true }
                baseRef, staticCall r Fable.Unit argInfo baseRef
            | None ->
                if not(hasImplicitConstructor baseEntity) then
                    "Classes without a primary constructor cannot be inherited: " + baseEntity.FullName
                    |> addError com ctx.InlinePath None
                let baseCons = makeCallFrom com ctx r Fable.Unit true genArgs thisArg baseArgs baseCall
                entityRefMaybeImported com baseEntity, baseCons)

    match body with
    | BasicPatterns.Sequential(baseCall, body) ->
        match baseCall with
        | ConstructorCall(baseCall, genArgs, baseArgs) as fsExpr ->
            let baseConsInfo = getBaseConsInfo com ctx (makeRangeFrom fsExpr) baseCall genArgs baseArgs
            baseConsInfo, transformBodyStatements com ctx acc body
        // This happens in constructors including self references
        // TODO: We're discarding the bound value, detect if there's a reference to it
        // in the base constructor arguments and throw an error in that case.
        | BasicPatterns.Let(_, (ConstructorCall(baseCall, genArgs, baseArgs) as fsExpr)) ->
            let baseConsInfo = getBaseConsInfo com ctx (makeRangeFrom fsExpr) baseCall genArgs baseArgs
            baseConsInfo, transformBodyStatements com ctx acc body
        | _ -> getBaseConsAndBody com ctx baseType (acc @ [baseCall]) body
    // TODO: Kindda unexpected, log warning?
    | body -> None, transformBodyStatements com ctx acc body

let private transformImplicitConstructor com (ctx: Context)
            (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    match memb.DeclaringEntity with
    | None -> "Unexpected constructor without declaring entity: " + memb.FullName
              |> addError com ctx.InlinePath None; []
    | Some ent ->
        let bodyCtx, args = bindMemberArgs com ctx args
        let boundThis = com.GetUniqueVar("this") |> makeIdent
        let bodyCtx = { bodyCtx with BoundConstructorThis = boundThis |> Some }
        let baseCons, body = getBaseConsAndBody com bodyCtx ent.BaseType [] body
        let baseExpr, body =
            match baseCons with
            | Some(baseExpr, baseCons) -> Some baseExpr, Fable.Sequential(baseCons::body)
            | None -> None, Fable.Sequential body
        let name = getMemberDeclarationName com memb
        let entityName = getEntityDeclarationName com ent
        com.AddUsedVarName(name)
        com.AddUsedVarName(entityName)
        let info: Fable.ClassImplicitConstructorInfo =
            { Name = name
              Entity = ent
              EntityName = entityName
              IsEntityPublic = isPublicEntity ent
              IsConstructorPublic = isPublicMember memb
              HasSpread = hasSeqSpread memb
              Base = baseExpr
              Arguments = args
              BoundConstructorThis = boundThis
              Body = body
            }
        let interfaces = interfaceImplementations com ent
        [Fable.ConstructorDeclaration(Fable.ClassImplicitConstructor info, interfaces)]

/// When using `importMember`, uses the member display name as selector
let private importExprSelector (memb: FSharpMemberOrFunctionOrValue) selector =
    match selector with
    | Fable.Value(Fable.StringConstant Naming.placeholder) ->
        getMemberDisplayName memb |> makeStrConst
    | _ -> selector

let private transformImport r typ isPublic name selector path =
    let info: Fable.ValueDeclarationInfo =
        { Name = name
          IsPublic = isPublic
          // TODO: Check if they're mutable, see #1314
          IsMutable = false
          IsEntryPoint = false
          HasSpread = false }
    let fableValue = Fable.Import(selector, path, Fable.CustomImport, typ, r)
    [Fable.ValueDeclaration(fableValue, info)]

let private transformMemberValue (com: IFableCompiler) ctx isPublic name (memb: FSharpMemberOrFunctionOrValue) (value: FSharpExpr) =
    let value = transformExpr com ctx value |> run
    match value with
    // Accept import expressions, e.g. let foo = import "foo" "myLib"
    | Fable.Import(selector, path, Fable.CustomImport, typ, r) ->
        match typ with
        | Fable.FunctionType(Fable.LambdaType _, Fable.FunctionType(Fable.LambdaType _, _)) ->
            "Change declaration of member: " + name + "\n"
            + "Importing JS functions with multiple arguments as `let add: int->int->int` won't uncurry parameters." + "\n"
            + "Use following syntax: `let add (x:int) (y:int): int = import ...`"
            |> addError com ctx.InlinePath None
        | _ -> ()
        let selector = importExprSelector memb selector
        transformImport r typ isPublic name selector path
    | fableValue ->
        let info: Fable.ValueDeclarationInfo =
            { Name = name
              IsPublic = isPublic
              IsMutable = memb.IsMutable
              IsEntryPoint = false
              HasSpread = false }
        [Fable.ValueDeclaration(fableValue, info)]

let private transformMemberFunction (com: IFableCompiler) ctx isPublic name (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    match body with
    // Accept import expressions, e.g. let foo x y = import "foo" "myLib"
    | Fable.Import(selector, path, Fable.CustomImport, _, r) ->
        // Use the full function type
        let typ = makeType com Map.empty memb.FullType
        let selector = importExprSelector memb selector
        transformImport r typ isPublic name selector path
    | body ->
        let fn = Fable.Function(Fable.Delegate args, body, Some name)
        // If this is a static constructor, call it immediately
        if memb.CompiledName = ".cctor" then
            let apply = staticCall None Fable.Unit (argInfo None [] Fable.NoUncurrying) fn
            [Fable.ActionDeclaration apply]
        else
            let info: Fable.ValueDeclarationInfo =
                { Name = name
                  IsPublic = isPublic
                  IsMutable = false
                  IsEntryPoint = memb.Attributes |> hasAttribute Atts.entryPoint
                  HasSpread = hasSeqSpread memb }
            [Fable.ValueDeclaration(fn, info)]

let private transformMemberFunctionOrValue (com: IFableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let isPublic = isPublicMember memb
    let name = getMemberDeclarationName com memb
    com.AddUsedVarName(name)
    match tryImportAttribute memb.Attributes with
    | Some(selector, path) ->
        let typ = makeType com Map.empty memb.FullType
        transformImport None typ isPublic name (makeStrConst selector) (makeStrConst path)
    | None ->
        if isModuleValueForDeclarations memb
        then transformMemberValue com ctx isPublic name memb body
        else transformMemberFunction com ctx isPublic name memb args body

let private transformOverride (com: FableCompiler) (ctx: Context)
            (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    match memb.DeclaringEntity with
    | None -> "Unexpected override without declaring entity: " + memb.FullName
              |> addError com ctx.InlinePath None; []
    | Some ent ->
        let bodyCtx, args = bindMemberArgs com ctx args
        let body = transformExpr com bodyCtx body |> run
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

let private transformInterfaceImplementationMember (com: FableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    // Some interfaces will be implemented in the prototype
    if Set.contains memb.CompiledName Naming.interfaceMethodsImplementedInPrototype
    then transformOverride com ctx memb args body
    else
        match memb.DeclaringEntity, tryGetInterfaceDefinitionFromMethod memb with
        | Some ent, Some interfaceEntity when not(Naming.ignoredInterfaces.Contains interfaceEntity.FullName) ->
            let bodyCtx, args = bindMemberArgs com ctx args
            let body = transformExpr com bodyCtx body |> run
            let value = Fable.Function(Fable.Delegate args, body, None)
            let kind =
                if memb.IsPropertyGetterMethod && countNonCurriedParams memb = 0
                then Fable.ObjectGetter
                elif memb.IsPropertySetterMethod && countNonCurriedParams memb = 1
                then Fable.ObjectSetter
                else hasSeqSpread memb |> Fable.ObjectMethod
            let objMember = Fable.ObjectMember(makeStrConst memb.DisplayName, value, kind)
            let ifcImplName = getInterfaceImplementationName com ent interfaceEntity.FullName
            com.InterfaceImplementationMembers.Add(ifcImplName, objMember)
        | _ -> ()
        []

let private transformMemberDecl (com: FableCompiler) (ctx: Context) (memb: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let ctx = { ctx with EnclosingMember = Some memb }
    if isIgnoredMember memb
    then []
    // TODO: Compiler flag to output pseudo-inline expressions? (e.g. for REPL libs)
    elif isInline memb then
        let inlineExpr = { Args = List.concat args
                           Body = body
                           FileName = (com :> ICompiler).CurrentFile }
        com.AddInlineExpr(memb, inlineExpr)
        []
    elif memb.IsImplicitConstructor
    then transformImplicitConstructor com ctx memb args body
    elif memb.IsExplicitInterfaceImplementation
    then transformInterfaceImplementationMember com ctx memb args body
    elif memb.IsOverrideOrExplicitInterfaceImplementation
    then transformOverride com ctx memb args body
    else transformMemberFunctionOrValue com ctx memb args body

let interfaceImplementations (com: IFableCompiler) (ent: FSharpEntity) =
    ent.AllInterfaces |> Seq.choose (fun interfaceType ->
        match tryDefinition interfaceType with
        | Some(interfaceEnt, Some interfaceFullName)
                when not(Naming.ignoredInterfaces.Contains interfaceFullName)
                    && ent.DeclaredInterfaces |> Seq.exists (testInterfaceHierarcy interfaceFullName) ->
            let castFunctionName = getInterfaceImplementationName com ent interfaceFullName
            com.AddUsedVarName(castFunctionName)
            let inheritedInterfaces =
                interfaceEnt.DeclaredInterfaces |> Seq.choose (fun t ->
                    match tryDefinition t with
                    // If the parent interface doesn't bring any method, we can ignore it
                    | Some(tdef, Some fullName)
                            when not(Naming.ignoredInterfaces.Contains fullName)
                                && not(isInterfaceEmpty tdef) ->
                        getInterfaceImplementationName com ent tdef.FullName |> Some
                    | _ -> None)
                |> Seq.toList
            { Name = castFunctionName
              IsPublic = not ent.Accessibility.IsPrivate
              ImplementingType = ent
              InterfaceType = interfaceEnt
              InheritedInterfaces = inheritedInterfaces
              Members = []
            }: Fable.InterfaceImplementation |> Some
        | _ -> None)
    |> Seq.toList

let private transformDeclarations (com: FableCompiler) ctx rootEnt rootDecls =
    let rec transformDeclarationsInner (com: FableCompiler) (ctx: Context) fsDecls =
        fsDecls |> List.collect (fun fsDecl ->
            match fsDecl with
            | FSharpImplementationFileDeclaration.Entity(ent, sub) ->
                match tryImportAttribute ent.Attributes with
                | Some(selector, path) ->
                    let name = getEntityDeclarationName com ent
                    com.AddUsedVarName(name)
                    (makeStrConst selector, makeStrConst path)
                    ||> transformImport None Fable.Any (not ent.Accessibility.IsPrivate) name
                // Discard erased unions and string enums
                | None when ent.IsFSharpUnion && not (isErasedUnion ent) ->
                    let entityName = getEntityDeclarationName com ent
                    com.AddUsedVarName(entityName)
                    // TODO: Check Equality/Comparison attributes
                    let info: Fable.UnionConstructorInfo =
                      { Entity = ent
                        EntityName = entityName
                        IsPublic = isPublicEntity ent }
                    let interfaces = interfaceImplementations com ent
                    [Fable.ConstructorDeclaration(Fable.UnionConstructor info, interfaces)]
                | None when ent.IsFSharpRecord
                        || ent.IsFSharpExceptionDeclaration
                        || ((ent.IsClass || ent.IsValueType) && not ent.IsMeasure && not (hasImplicitConstructor ent)) ->
                    let entityName = getEntityDeclarationName com ent
                    com.AddUsedVarName(entityName)
                    // TODO: Check Equality/Comparison attributes
                    let info: Fable.CompilerGeneratedConstructorInfo =
                      { Entity = ent
                        EntityName = entityName
                        IsPublic = isPublicEntity ent }
                    let interfaces = interfaceImplementations com ent
                    [Fable.ConstructorDeclaration(Fable.CompilerGeneratedConstructor info, interfaces)]
                | None ->
                    transformDeclarationsInner com { ctx with EnclosingEntity = Some ent } sub
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(meth, args, body) ->
                transformMemberDecl com ctx meth args body
            | FSharpImplementationFileDeclaration.InitAction fe ->
                [transformExpr com ctx fe |> run |> Fable.ActionDeclaration])
    // In case this is a recursive module, do a first pass to add
    // all entity and member names as used var names
    rootDecls |> List.iter (function
        | FSharpImplementationFileDeclaration.Entity(ent,_) ->
            com.AddUsedVarName(ent.CompiledName)
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memb,_,_) ->
            com.AddUsedVarName(memb.CompiledName)
        | FSharpImplementationFileDeclaration.InitAction _ -> ()
    )
    let decls = transformDeclarationsInner com ctx rootDecls
    if com.InterfaceImplementationMembers.Count > 0 then
        decls |> List.map (function
            | Fable.ConstructorDeclaration(kind, interfaces) when not(List.isEmpty interfaces) ->
                Fable.ConstructorDeclaration(kind, interfaces |> List.map (fun info ->
                    { info with Members = com.InterfaceImplementationMembers.Get(info.Name) }))
            | decl -> decl)
    else decls

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

let private tryGetMemberArgsAndBody com (implFiles: Map<string, FSharpImplementationFileContents>)
                                    fileName (meth: FSharpMemberOrFunctionOrValue) =
    let rec tryGetMemberArgsAndBody' (methFullName: string) = function
        | FSharpImplementationFileDeclaration.Entity (e, decls) ->
            let entFullName = getEntityFullName e
            if methFullName.StartsWith(entFullName)
            then List.tryPick (tryGetMemberArgsAndBody' methFullName) decls
            else None
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth2, args, body) ->
            if getMemberUniqueName com meth2 = methFullName
            then Some(args, body)
            else None
        | FSharpImplementationFileDeclaration.InitAction _ -> None
    let fullName = getMemberUniqueName com meth
    Map.tryFind fileName implFiles
    |> Option.bind (fun f ->
        f.Declarations |> List.tryPick (tryGetMemberArgsAndBody' fullName))

type FableCompiler(com: ICompiler, implFiles: Map<string, FSharpImplementationFileContents>) =
    member val UsedVarNames = HashSet<string>()
    member val Dependencies = HashSet<string>()
    member val InterfaceImplementationMembers: ResizeArrayDictionary<_,_> = ResizeArrayDictionary<string, Fable.ObjectMember>()
    member this.AddUsedVarName(varName) =
        this.UsedVarNames.Add(varName) |> ignore
    member __.AddInlineExpr(memb, inlineExpr: InlineExpr) =
        let fullName = getMemberUniqueName com memb
        com.GetOrAddInlineExpr(fullName, fun () -> inlineExpr) |> ignore
    interface IFableCompiler with
        member this.Transform(ctx, fsExpr) =
            transformExpr this ctx fsExpr |> run
        member this.TryReplace(ctx, r, t, info, thisArg, args) =
            Replacements.tryCall this ctx r t info thisArg args
        member __.TryReplaceInterfaceCast(r, t, name, e) =
            Replacements.tryInterfaceCast r t name e
        member this.InjectArgument(ctx, r, genArgs, parameter) =
            Inject.injectArg this ctx r genArgs parameter
        member this.GetInlineExpr(memb) =
            let fileName =
                (getMemberLocation memb).FileName
                |> Path.normalizePathAndEnsureFsExtension
            if fileName <> com.CurrentFile then
                this.Dependencies.Add(fileName) |> ignore
            let fullName = getMemberUniqueName com memb
            com.GetOrAddInlineExpr(fullName, fun () ->
                match tryGetMemberArgsAndBody com implFiles fileName memb with
                | Some(args, body) ->
                    { Args = List.concat args
                      Body = body
                      FileName = fileName }
                | None -> failwith ("Cannot find inline member " + memb.FullName))
        member this.AddUsedVarName(varName) =
            this.AddUsedVarName(varName) |> ignore
        member this.IsUsedVarName(varName) =
            this.UsedVarNames.Contains(varName)
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
        let rootEnt, rootDecls = getRootModuleAndDecls file.Declarations
        let fcom = FableCompiler(com, implFiles)
        let ctx = Context.Create(rootEnt)
        let rootDecls = transformDeclarations fcom ctx rootEnt rootDecls
        Fable.File(com.CurrentFile, rootDecls, set fcom.UsedVarNames, set fcom.Dependencies)
    with
    | ex -> exn (sprintf "%s (%s)" ex.Message com.CurrentFile, ex) |> raise
