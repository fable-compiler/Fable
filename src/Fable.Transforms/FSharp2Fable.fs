module rec Fable.Transforms.FSharp2Fable.Compiler

open System.Collections.Generic
open FSharp.Compiler.Symbols

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

let private transformBaseConsCall com ctx r (baseEnt: FSharpEntity) (baseCons: FSharpMemberOrFunctionOrValue) genArgs baseArgs =
    let baseEntRef = FsEnt.Ref(baseEnt)
    let argTypes = lazy getArgTypes com baseCons
    let baseArgs = transformExprList com ctx baseArgs |> run
    let genArgs = genArgs |> List.map (makeType ctx.GenericArgs)
    match Replacements.Api.tryBaseConstructor com ctx baseEntRef argTypes genArgs baseArgs with
    | Some(baseRef, args) ->
        let callInfo = Fable.CallInfo.Create(args=args, sigArgTypes=getArgTypes com baseCons)
        makeCall r Fable.Unit callInfo baseRef
    | None ->
        if not baseCons.IsImplicitConstructor then
            "Only inheriting from primary constructors is supported"
            |> addWarning com [] r
        match makeCallFrom com ctx r Fable.Unit genArgs None baseArgs baseCons with
        | Fable.Call(_baseExpr, info, t, r) ->
            // The baseExpr will be the exposed constructor function,
            // replace with a direct reference to the entity
            let baseExpr =
                match tryGlobalOrImportedFSharpEntity com baseEnt with
                | Some baseExpr -> baseExpr
                | None -> FsEnt.Ref baseEnt |> entityIdent com
            Fable.Call(baseExpr, info, t, r)
        // Other cases, like Emit will call directly the base expression
        | e -> e

let private transformNewUnion com ctx r fsType (unionCase: FSharpUnionCase) (argExprs: Fable.Expr list) =
    match getUnionPattern fsType unionCase with
    | ErasedUnionCase ->
        makeTuple r false argExprs
    | ErasedUnion(tdef, _genArgs, rule) ->
        match argExprs with
        | [] -> transformStringEnum rule unionCase
        | [argExpr] -> argExpr
        | _ when tdef.UnionCases.Count > 1 ->
            "Erased unions with multiple cases must have one single field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com ctx.InlinePath r
        | argExprs -> makeTuple r false argExprs
    | TypeScriptTaggedUnion _  ->
        match argExprs with
        | [argExpr] -> argExpr
        | _ ->
            "TS tagged unions must have one single field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com ctx.InlinePath r
    | StringEnum(tdef, rule) ->
        match argExprs with
        | [] -> transformStringEnum rule unionCase
        | _ -> $"StringEnum types cannot have fields: {tdef.TryFullName}"
               |> addErrorAndReturnNull com ctx.InlinePath r
    | OptionUnion(typ, isStruct) ->
        let typ = makeType ctx.GenericArgs typ
        let expr =
            match argExprs with
            | [] -> None
            | [expr] -> Some expr
            | _ -> failwith "Unexpected args for Option constructor"
        Fable.NewOption(expr, typ, isStruct) |> makeValue r
    | ListUnion typ ->
        let typ = makeType ctx.GenericArgs typ
        let headAndTail =
            match argExprs with
            | [] -> None
            | [head; tail] -> Some(head, tail)
            | _ -> failwith "Unexpected args for List constructor"
        Fable.NewList(headAndTail, typ) |> makeValue r
    | DiscriminatedUnion(tdef, genArgs) ->
        let genArgs = makeTypeGenArgs ctx.GenericArgs genArgs
        let tag = unionCaseTag com tdef unionCase
        Fable.NewUnion(argExprs, tag, FsEnt.Ref tdef, genArgs) |> makeValue r

let private transformTraitCall com (ctx: Context) r typ (sourceTypes: Fable.Type list) traitName isInstance (argTypes: Fable.Type list) (argExprs: Fable.Expr list) =
    let makeCallInfo traitName entityFullName argTypes genArgs: Fable.ReplaceCallInfo =
        { SignatureArgTypes = argTypes
          DeclaringEntityFullName = entityFullName
          HasSpread = false
          IsModuleValue = false
          // We only need this for types with own entries in Fable AST
          // (no interfaces, see below) so it's safe to set this to false
          IsInterface = false
          CompiledName = traitName
          OverloadSuffix = ""
          GenericArgs = genArgs
        }

    let thisArg, args, argTypes =
        match argExprs, argTypes with
        | thisArg::args, _::argTypes when isInstance -> Some thisArg, args, argTypes
        | args, argTypes -> None, args, argTypes

    let rec matchGenericType (genArgs: Map<string, Fable.Type>) (signatureType: Fable.Type, concreteType: Fable.Type) =
        match signatureType with
        | Fable.GenericParam(name=name) when not(genArgs.ContainsKey(name)) -> Map.add name concreteType genArgs
        | signatureType ->
            let signatureTypeGenerics = signatureType.Generics
            if List.isEmpty signatureTypeGenerics then
                genArgs
            else
                let concreteTypeGenerics = concreteType.Generics
                if List.sameLength signatureTypeGenerics concreteTypeGenerics then
                    (genArgs, List.zip signatureTypeGenerics concreteTypeGenerics) ||> List.fold matchGenericType
                else
                    genArgs // Unexpected, error?

    let resolveMemberCall (entity: Fable.Entity) (entGenArgs: Fable.Type list) membCompiledName isInstance argTypes thisArg args =
        let entGenParamNames = entity.GenericParameters |> List.map (fun x -> x.Name)
        let entGenArgsMap = List.zip entGenParamNames entGenArgs |> Map
        tryFindMember entity entGenArgsMap membCompiledName isInstance argTypes
        |> Option.map (fun memb ->
            // Resolve method generic args before making the call, see #2135
            let genArgsMap =
                let membParamTypes = memb.CurriedParameterGroups |> Seq.collect (fun group -> group |> Seq.map (fun p -> p.Type)) |> Seq.toList
                if List.sameLength argTypes membParamTypes then
                    let argTypes = argTypes @ [typ]
                    let membParamTypes = membParamTypes @ [memb.ReturnParameter.Type]
                    (entGenArgsMap, List.zip membParamTypes argTypes) ||> List.fold (fun genArgs (paramType, argType) ->
                        let paramType = makeType Map.empty paramType
                        matchGenericType genArgs (paramType, argType))
                else
                    Map.empty // Unexpected, error?

            let genArgs = memb.GenericParameters |> Seq.mapToList (fun p ->
                let name = genParamName p
                match Map.tryFind name genArgsMap with
                | Some t -> t
                | None -> Fable.GenericParam(name, p.IsMeasure, p.Constraints |> Seq.chooseToList FsGenParam.Constraint))

            makeCallFrom com ctx r typ (entGenArgs @ genArgs) thisArg args memb)

    sourceTypes |> Seq.tryPick (fun t ->
        let typeOpt = Replacements.Api.tryType com t
        match typeOpt with
        | Some(entityFullName, makeCall, genArgs) ->
            let info = makeCallInfo traitName entityFullName argTypes genArgs
            makeCall com ctx r typ info thisArg args
        | None ->
            match t with
            | Fable.DeclaredType(entity, entGenArgs) ->
                let entity = com.GetEntity(entity)
                // SRTP only works for records if there are no arguments
                if isInstance && entity.IsFSharpRecord && List.isEmpty args && Option.isSome thisArg then
                    let fieldName = Naming.removeGetSetPrefix traitName
                    entity.FSharpFields |> Seq.tryPick (fun fi ->
                        if fi.Name = fieldName then
                            let kind = Fable.FieldInfo.Create(fi.Name, fieldType=fi.FieldType, isMutable=fi.IsMutable)
                            Fable.Get(thisArg.Value, kind, typ, r) |> Some
                        else None)
                    |> Option.orElseWith (fun () ->
                        resolveMemberCall entity entGenArgs traitName isInstance argTypes thisArg args)
                else resolveMemberCall entity entGenArgs traitName isInstance argTypes thisArg args
            | Fable.AnonymousRecordType(sortedFieldNames, entGenArgs, _isStruct)
                    when isInstance && List.isEmpty args && Option.isSome thisArg ->
                let fieldName = Naming.removeGetSetPrefix traitName
                Seq.zip sortedFieldNames entGenArgs
                |> Seq.tryPick (fun (fi, fiType) ->
                    if fi = fieldName then
                        Fable.Get(thisArg.Value, Fable.FieldInfo.Create(fi, fieldType=fiType), typ, r) |> Some
                    else None)
            | _ -> None
    ) |> Option.defaultWith (fun () ->
        "Cannot resolve trait call " + traitName |> addErrorAndReturnNull com ctx.InlinePath r)

let private transformCallee com ctx callee (calleeType: FSharpType) =
  trampoline {
    let! callee = transformExprOpt com ctx callee
    let callee =
        match callee with
        | Some callee -> callee
        | None -> FsEnt.Ref calleeType.TypeDefinition |> entityIdent com
    return callee
  }

let private resolveImportMemberBinding (ident: Fable.Ident) (info: Fable.ImportInfo) =
    if info.Selector = Naming.placeholder then { info with Selector = ident.Name }
    else info

type private SignatureInfo = {|
    name: string
    isMangled: bool
    memberRef: Fable.MemberRef
|}

let private getImplementedSignatureInfo com ctx r nonMangledNameConflicts (implementingEntity: FSharpEntity option) (sign: FSharpAbstractSignature) =
    let implementingEntityFields = HashSet<_>()
    let implementingEntityName =
        match implementingEntity with
        | Some e ->
            e.FSharpFields |> Seq.iter (fun x -> implementingEntityFields.Add(x.Name) |> ignore)
            e.FullName
        | None -> ""

    // Don't use the type from the arguments as the override may come from another type, like ToString()
    tryDefinition sign.DeclaringType
    |> Option.bind (fun (ent, _entFullName) ->
        // Only compare param types for overloads (single curried parameter group)
        let paramTypes =
            if sign.AbstractArguments.Count = 1 then
                sign.AbstractArguments[0] |> Seq.map (fun p -> makeType Map.empty p.Type) |> Seq.toArray |> Some
            else None
        tryFindAbstractMember ent sign.Name paramTypes
        |> Option.map (fun m -> ent, m))
    |> Option.map (fun (ent, memb) ->
        let info = getAbstractMemberInfo com ent memb
        // Setters can have same name as getters, assume there will always be a getter
        if not info.isMangled
            && not info.isSetter
            && (nonMangledNameConflicts implementingEntityName info.name || implementingEntityFields.Contains(info.name)) then
                $"Member %s{info.name} is duplicated, use Mangle attribute to prevent conflicts with interfaces"
                // TODO: Temporarily emitting a warning, because this errors in old libraries, like Fable.React.HookBindings
                |> addWarning com ctx.InlinePath r
        {|
            name = info.name
            isMangled = info.isMangled
            memberRef = getFunctionMemberRef memb
        |}
    )
    |> Option.defaultWith (fun () ->
        let isGetter = sign.Name.StartsWith("get_") && countNonCurriedParamsForSignature sign = 0
        let isSetter = not isGetter && sign.Name.StartsWith("set_") && countNonCurriedParamsForSignature sign = 1
        let name = if isGetter || isSetter then Naming.removeGetSetPrefix sign.Name else sign.Name
        let generatedMember =
            if isGetter then Fable.GeneratedMember.Getter(name, makeType Map.empty sign.AbstractReturnType)
            elif isSetter then Fable.GeneratedMember.Setter(name, makeType Map.empty (sign.AbstractArguments[0].[1].Type))
            else Fable.GeneratedMember.Function(name,
                sign.AbstractArguments |> Seq.concat |> Seq.mapToList (fun p -> makeType Map.empty p.Type),
                makeType Map.empty sign.AbstractReturnType)
        {|
            name = name
            isMangled = false
            memberRef = generatedMember
        |}
    )

let private transformObjExpr (com: IFableCompiler) (ctx: Context) (objType: FSharpType)
                    baseCallExpr (overrides: FSharpObjectExprOverride list) otherOverrides =

    let nonMangledMemberNames = HashSet()
    let nonMangledNameConflicts _ name =
        nonMangledMemberNames.Add(name) |> not

    let mapOverride (over: FSharpObjectExprOverride): Thunk<Fable.ObjectExprMember> =
      trampoline {
        let signature = over.Signature
        let r = makeRangeFrom over.Body
        let info = getImplementedSignatureInfo com ctx r nonMangledNameConflicts None signature
        let ctx, args = bindMemberArgs com ctx over.CurriedParameterGroups
        let! body = transformExpr com ctx over.Body
        return { Name = info.name
                 Args = args
                 Body = body
                 IsMangled = info.isMangled
                 MemberRef = info.memberRef }
      }

    trampoline {
      let! baseCall =
        trampoline {
            match baseCallExpr with
            // TODO: For interface implementations this should be FSharpExprPatterns.NewObject
            // but check the baseCall.DeclaringEntity name just in case
            | FSharpExprPatterns.Call(None,baseCall,genArgs1,genArgs2,baseArgs) ->
                match baseCall.DeclaringEntity with
                | Some baseEnt when baseEnt.TryFullName <> Some Types.object ->
                    let r = makeRangeFrom baseCallExpr
                    let genArgs = genArgs1 @ genArgs2
                    return transformBaseConsCall com ctx r baseEnt baseCall genArgs baseArgs |> Some
                | _ -> return None
            | _ -> return None
        }

      let! members =
        (objType, overrides)::otherOverrides
        |> trampolineListMap (fun (_typ, overrides) ->
            overrides |> trampolineListMap mapOverride)

      return Fable.ObjectExpr(members |> List.concat, makeType ctx.GenericArgs objType, baseCall)
    }

let private transformDelegate com ctx (delegateType: FSharpType) expr =
  trampoline {
    let! expr = transformExpr com ctx expr

    // For some reason, when transforming to Func<'T> (no args) the F# compiler
    // applies a unit arg to the expression, see #2400
    let expr =
        match tryDefinition delegateType with
        | Some(_, Some "System.Func`1") ->
            match expr with
            | Fable.CurriedApply(expr, [Fable.Value(Fable.UnitConstant, _)],_,_) -> expr
            | Fable.Call(expr, { Args = [Fable.Value(Fable.UnitConstant, _)] },_,_) -> expr
            | _ -> expr
        | _ -> expr

    match makeType ctx.GenericArgs delegateType with
    | Fable.DelegateType(argTypes, _) as t ->
        let arity = List.length argTypes |> max 1
        match expr with
        | LambdaUncurriedAtCompileTime (Some arity) lambda -> return lambda
        | _ when arity > 1 -> return Replacements.Api.uncurryExprAtRuntime com t arity expr
        | _ -> return expr
    | _ -> return expr
  }

let private transformUnionCaseTest (com: IFableCompiler) (ctx: Context) r
                            unionExpr fsType (unionCase: FSharpUnionCase) =
  trampoline {
    let! unionExpr = transformExpr com ctx unionExpr
    match getUnionPattern fsType unionCase with
    | ErasedUnionCase ->
        return "Cannot test erased union cases"
        |> addErrorAndReturnNull com ctx.InlinePath r
    | ErasedUnion(tdef, genArgs, rule) ->
        match unionCase.Fields.Count with
        | 0 -> return makeEqOp r unionExpr (transformStringEnum rule unionCase) BinaryEqual
        | 1 ->
            let fi = unionCase.Fields[0]
            let typ =
                if fi.FieldType.IsGenericParameter then
                    let name = genParamName fi.FieldType.GenericParameter
                    let index =
                        tdef.GenericParameters
                        |> Seq.findIndex (fun arg -> genParamName arg = name)
                    genArgs[index]
                else fi.FieldType
            let kind = makeType ctx.GenericArgs typ |> Fable.TypeTest
            return Fable.Test(unionExpr, kind, r)
        | _ ->
            return "Erased unions with multiple cases cannot have more than one field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com ctx.InlinePath r
    | TypeScriptTaggedUnion (_, _, tagName, rule) ->
        match unionCase.Fields.Count with
        | 1 ->
            let value =
                match FsUnionCase.CompiledValue unionCase with
                | None -> transformStringEnum rule unionCase
                | Some (CompiledValue.Integer i) -> makeIntConst i
                | Some (CompiledValue.Float f) -> makeFloatConst f
                | Some (CompiledValue.Boolean b) -> makeBoolConst b
            return makeEqOp r
                (Fable.Get(unionExpr, Fable.FieldInfo.Create(tagName), value.Type, r))
                value
                BinaryEqual
        | _ ->
            return "TS tagged unions must have one single field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com ctx.InlinePath r
    | OptionUnion _ ->
        let kind = Fable.OptionTest(unionCase.Name <> "None" && unionCase.Name <> "ValueNone")
        return Fable.Test(unionExpr, kind, r)
    | ListUnion _ ->
        let kind = Fable.ListTest(unionCase.CompiledName <> "Empty")
        return Fable.Test(unionExpr, kind, r)
    | StringEnum(_, rule) ->
        return makeEqOp r unionExpr (transformStringEnum rule unionCase) BinaryEqual
    | DiscriminatedUnion(tdef,_) ->
        let tag = unionCaseTag com tdef unionCase
        return Fable.Test(unionExpr, Fable.UnionCaseTest(tag), r)
  }

let rec private transformDecisionTargets (com: IFableCompiler) (ctx: Context) acc
                    (xs: (FSharpMemberOrFunctionOrValue list * FSharpExpr) list) =
    trampoline {
        match xs with
        | [] -> return List.rev acc
        | (idents, expr)::tail ->
            let ctx, idents =
                (idents, (ctx, [])) ||> List.foldBack (fun ident (ctx, idents) ->
                    let ctx, ident = putIdentInScope com ctx ident None
                    ctx, ident::idents)
            let! expr = transformExpr com ctx expr
            return! transformDecisionTargets com ctx ((idents, expr)::acc) tail
    }


let private transformExpr (com: IFableCompiler) (ctx: Context) fsExpr =
  trampoline {
    match fsExpr with
    // | ByrefArgToTuple (callee, memb, ownerGenArgs, membGenArgs, membArgs) ->
    //     let! callee = transformExprOpt com ctx callee
    //     let! args = transformExprList com ctx membArgs
    //     let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
    //     let typ = makeType ctx.GenericArgs fsExpr.Type
    //     return makeCallFrom com ctx (makeRangeFrom fsExpr) typ genArgs callee args memb

    // | ByrefArgToTupleOptimizedIf (outArg, callee, memb, ownerGenArgs, membGenArgs, membArgs, thenExpr, elseExpr) ->
    //     let ctx, ident = putArgInScope com ctx outArg
    //     let! callee = transformExprOpt com ctx callee
    //     let! args = transformExprList com ctx membArgs
    //     let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
    //     let byrefType = makeType ctx.GenericArgs (List.last membArgs).Type
    //     let tupleType = [Fable.Boolean; byrefType] |> Fable.Tuple
    //     let tupleIdent = getIdentUniqueName ctx "tuple" |> makeIdent
    //     let tupleIdentExpr = Fable.IdentExpr tupleIdent
    //     let tupleExpr = makeCallFrom com ctx None tupleType genArgs callee args memb
    //     let identExpr = Fable.Get(tupleIdentExpr, Fable.TupleIndex 1, tupleType, None)
    //     let guardExpr = Fable.Get(tupleIdentExpr, Fable.TupleIndex 0, tupleType, None)
    //     let! thenExpr = transformExpr com ctx thenExpr
    //     let! elseExpr = transformExpr com ctx elseExpr
    //     let ifThenElse = Fable.IfThenElse(guardExpr, thenExpr, elseExpr, None)
    //     return Fable.Let([tupleIdent, tupleExpr], Fable.Let([ident, identExpr], ifThenElse))

    // | ByrefArgToTupleOptimizedIf (outArg, callee, memb, ownerGenArgs, membGenArgs, membArgs, thenExpr, elseExpr) ->
    //     let ctx, ident = putArgInScope com ctx outArg
    //     let! callee = transformExprOpt com ctx callee
    //     let! args = transformExprList com ctx membArgs
    //     let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
    //     let byrefType = makeType ctx.GenericArgs (List.last membArgs).Type
    //     let tupleType = [Fable.Boolean; byrefType] |> Fable.Tuple
    //     let tupleIdent = getIdentUniqueName ctx "tuple" |> makeIdent
    //     let tupleIdentExpr = Fable.IdentExpr tupleIdent
    //     let tupleExpr = makeCallFrom com ctx None tupleType genArgs callee args memb
    //     let identExpr = Fable.Get(tupleIdentExpr, Fable.TupleIndex 1, tupleType, None)
    //     let guardExpr = Fable.Get(tupleIdentExpr, Fable.TupleIndex 0, tupleType, None)
    //     let! thenExpr = transformExpr com ctx thenExpr
    //     let! elseExpr = transformExpr com ctx elseExpr
    //     let ifThenElse = Fable.IfThenElse(guardExpr, thenExpr, elseExpr, None)
    //     return Fable.Let([tupleIdent, tupleExpr], Fable.Let([ident, identExpr], ifThenElse))

    // | ByrefArgToTupleOptimizedTree (outArg, callee, memb, ownerGenArgs, membGenArgs, membArgs, thenExpr, elseExpr, targetsExpr) ->
    //     let ctx, ident = putArgInScope com ctx outArg
    //     let! callee = transformExprOpt com ctx callee
    //     let! args = transformExprList com ctx membArgs
    //     let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
    //     let byrefType = makeType ctx.GenericArgs (List.last membArgs).Type
    //     let tupleType = [Fable.Boolean; byrefType] |> Fable.Tuple
    //     let tupleIdentExpr = Fable.IdentExpr ident
    //     let tupleExpr = makeCallFrom com ctx None tupleType genArgs callee args memb
    //     let guardExpr = Fable.Get(tupleIdentExpr, Fable.TupleIndex 0, tupleType, None)
    //     let! thenExpr = transformExpr com ctx thenExpr
    //     let! elseExpr = transformExpr com ctx elseExpr
    //     let! targetsExpr = transformDecisionTargets com ctx [] targetsExpr
    //     let ifThenElse = Fable.IfThenElse(guardExpr, thenExpr, elseExpr, None)
    //     return Fable.Let([ident, tupleExpr], Fable.DecisionTree(ifThenElse, targetsExpr))

    // | ByrefArgToTupleOptimizedLet (id1, id2, callee, memb, ownerGenArgs, membGenArgs, membArgs, restExpr) ->
    //     let ctx, ident1 = putArgInScope com ctx id1
    //     let ctx, ident2 = putArgInScope com ctx id2
    //     let! callee = transformExprOpt com ctx callee
    //     let! args = transformExprList com ctx membArgs
    //     let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
    //     let byrefType = makeType ctx.GenericArgs (List.last membArgs).Type
    //     let tupleType = [Fable.Boolean; byrefType] |> Fable.Tuple
    //     let tupleIdent = getIdentUniqueName ctx "tuple" |> makeIdent
    //     let tupleIdentExpr = Fable.IdentExpr tupleIdent
    //     let tupleExpr = makeCallFrom com ctx None tupleType genArgs callee args memb
    //     let id1Expr = Fable.Get(tupleIdentExpr, Fable.TupleIndex 0, tupleType, None)
    //     let id2Expr = Fable.Get(tupleIdentExpr, Fable.TupleIndex 1, tupleType, None)
    //     let! restExpr = transformExpr com ctx restExpr
    //     let body = Fable.Let([ident1, id1Expr], Fable.Let([ident2, id2Expr], restExpr))
    //     return Fable.Let([tupleIdent, tupleExpr], body)

    // | ForOf (PutArgInScope com ctx (newContext, ident), value, body) ->
    //     let! value = transformExpr com ctx value
    //     let! body = transformExpr com newContext body
    //     return Replacements.iterate com (makeRangeFrom fsExpr) ident body value

    // work-around for optimized "for x in list" (erases this sequential)
    // | FSharpExprPatterns.Sequential (FSharpExprPatterns.ValueSet (current, FSharpExprPatterns.Value next1),
    //                             (FSharpExprPatterns.ValueSet (next2, FSharpExprPatterns.UnionCaseGet
    //                                 (_value, typ, unionCase, field))))
    //         when next1.FullName = "next" && next2.FullName = "next"
    //             && current.FullName = "current" && (getFsTypeFullName typ) = Types.list
    //             && unionCase.Name = "op_ColonColon" && field.Name = "Tail" ->
    //     // replace with nothing
    //     return Fable.UnitConstant |> makeValue None

    | OptimizedOperator com (memb, comp, opName, argTypes, argExprs) ->
        let r, typ = makeRangeFrom fsExpr, makeType ctx.GenericArgs fsExpr.Type
        let argTypes = argTypes |> List.map (makeType ctx.GenericArgs)
        let! args = transformExprList com ctx argExprs
        let entity: Fable.Entity =
            match comp with
            | Some comp -> upcast FsEnt comp.DeclaringEntity.Value
            | None -> upcast FsEnt memb.DeclaringEntity.Value
        let membOpt = tryFindMember entity ctx.GenericArgs opName false argTypes
        return (match membOpt with
                | Some memb -> makeCallFrom com ctx r typ argTypes None args memb
                | None -> failwith $"Cannot find member %s{entity.FullName}.%s{opName}")

    | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
        let! (inpExpr: Fable.Expr) = transformExpr com ctx inpExpr
        let t = makeType ctx.GenericArgs targetType
        return Fable.TypeCast(inpExpr, t)

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in FSharpExprPatterns.Let (see below)
    | FSharpExprPatterns.TypeLambda (_genArgs, lambda) ->
        let! lambda = transformExpr com ctx lambda
        return lambda

    | FSharpExprPatterns.FastIntegerForLoop(start, limit, body, isUp, _, _) ->
        let r = makeRangeFrom fsExpr
        match body with
        | FSharpExprPatterns.Lambda (PutIdentInScope com ctx (newContext, ident), body) ->
            let! start = transformExpr com ctx start
            let! limit = transformExpr com ctx limit
            let! body = transformExpr com newContext body
            return makeForLoop r isUp ident start limit body
        | _ -> return failwithf $"Unexpected loop {r}: %A{fsExpr}"

    | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr, _) ->
        let! guardExpr = transformExpr com ctx guardExpr
        let! bodyExpr = transformExpr com ctx bodyExpr
        return (guardExpr, bodyExpr) ||> makeWhileLoop (makeRangeFrom fsExpr)

    | FSharpExprPatterns.Const(value, typ) ->
        let typ = makeType ctx.GenericArgs typ
        let expr = makeTypeConst (makeRangeFrom fsExpr) typ value
        return expr

    | FSharpExprPatterns.BaseValue typ ->
        let r = makeRangeFrom fsExpr
        let typ = makeType Map.empty typ
        return Fable.Value(Fable.BaseValue(ctx.BoundMemberThis, typ), r)

    // F# compiler doesn't represent `this` in non-constructors as FSharpExprPatterns.ThisValue (but FSharpExprPatterns.Value)
    | FSharpExprPatterns.ThisValue typ ->
        let r = makeRangeFrom fsExpr
        return
            match typ, ctx.BoundConstructorThis with
            // When it's ref type, this is the x in `type C() as x =`
            | RefType _, _ ->
                tryGetIdentFromScopeIf ctx r (fun fsRef -> fsRef.IsConstructorThisValue)
                |> Option.defaultWith (fun () -> "Cannot find ConstructorThisValue"
                                                 |> addErrorAndReturnNull com ctx.InlinePath r)
            // Check if `this` has been bound previously to avoid conflicts with an object expression
            | _, Some i -> identWithRange r i |> Fable.IdentExpr
            | _, None -> Fable.Value(makeType Map.empty typ |> Fable.ThisValue, r)

    | FSharpExprPatterns.Value var ->
        let r = makeRangeFrom fsExpr
        if isInline var then
            match ctx.ScopeInlineValues |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
            | Some (_, fsExpr) ->
                return! transformExpr com ctx fsExpr
            | None ->
                return "Cannot resolve locally inlined value: " + var.DisplayName
                |> addErrorAndReturnNull com ctx.InlinePath r
        else
            let v = makeValueFrom com ctx r var
            if isByRefValue var &&
               // The replacement only needs to happen when var.FullType = byref<fsExpr.Type>
               fsExpr.Type = var.FullType.GenericArguments.[0] &&
               com.Options.Language <> Rust then
                // Getting byref value is compiled as FSharpRef op_Dereference
                return Replacements.Api.getRefCell com r v.Type v
            else
                return v

    // This is usually used to fill missing [<Optional>] arguments.
    // Unchecked.defaultof<'T> is resolved in Replacements instead.
    | FSharpExprPatterns.DefaultValue (FableType com ctx typ) ->
        let r = makeRangeFrom fsExpr
        match Compiler.Language with
        // In Dart we don't want the compiler to pass default values other than null to [<Optional>] args
        | Dart -> return Fable.Value(Fable.Null typ, r)
        | _ -> return Replacements.Api.defaultof com ctx r typ

    | FSharpExprPatterns.Let((var, value, _), body) ->
        match value with
        | CreateEvent(value, event) as createEvent ->
            let! value = transformExpr com ctx value
            let typ = makeType ctx.GenericArgs createEvent.Type
            let value = makeCallFrom com ctx (makeRangeFrom createEvent) typ [] (Some value) [] event
            let ctx, ident = putIdentInScope com ctx var (Some value)
            let! body = transformExpr com ctx body
            return Fable.Let(ident, value, body)

        // Because in Dart we compile DefaultValue as null when it's passed to optional arguments,
        // check if it's directly assigned in a binding and use the actual default value in that case
        // (This is necessary to properly initialize the out arg in `TryParse` methods)
        | FSharpExprPatterns.DefaultValue (FableType com ctx typ) ->
            let r = makeRangeFrom fsExpr
            let value = Replacements.Api.defaultof com ctx r typ
            let ctx, ident = putIdentInScope com ctx var (Some value)
            let! body = transformExpr com ctx body
            return Fable.Let(ident, value, body)

        // F# compiler generates a tuple when matching against multiple values,
        // we replace with immutable bindings instead which generates better code
        // and increases the chances of the tuple being removed in beta reduction
        | FSharpExprPatterns.NewTuple(tupleType, tupleValues) as tupleExpr
            when var.IsCompilerGenerated &&
                (var.CompiledName = "matchValue" || var.CompiledName = "patternInput") ->

            let! tupleValues = transformExprList com ctx tupleValues

            let bindings, tupleValues =
                (([], []), tupleValues) ||> List.fold (fun (bindings, tupleValues) value ->
                    match value with
                    | Fable.IdentExpr id ->
                        if not id.IsMutable then
                            bindings, value::tupleValues
                        else
                            let i = getIdentUniqueName ctx id.Name |> makeTypedIdent id.Type
                            (i, value)::bindings, (Fable.IdentExpr i)::tupleValues
                    | value ->
                        let i = getIdentUniqueName ctx "matchValue" |> makeTypedIdent value.Type
                        (i, value)::bindings, (Fable.IdentExpr i)::tupleValues)

            let value =
                Fable.NewTuple(List.rev tupleValues, tupleType.IsStructTupleType)
                |> makeValue (makeRangeFrom tupleExpr)

            let ctx, ident = putIdentInScope com ctx var (Some value)
            let! body = transformExpr com ctx body
            let expr = Fable.Let(ident, value, body)
            return (expr, bindings) ||> List.fold (fun e (i, v) -> Fable.Let(i, v, e))

        | _ when isInline var ->
            let ctx = { ctx with ScopeInlineValues = (var, value)::ctx.ScopeInlineValues }
            return! transformExpr com ctx body

        | _ ->
            let! value = transformExpr com ctx value
            let ctx, ident = putIdentInScope com ctx var (Some value)
            let! body = transformExpr com ctx body
            match value with
            | Fable.Import(info, t, r) when not info.IsCompilerGenerated ->
                return Fable.Let(ident, Fable.Import(resolveImportMemberBinding ident info, t, r), body)
            // Unwrap lambdas for user-generated imports, as in: `let add (x:int) (y:int): int = importMember "./util.js"`
            | AST.NestedLambda(args, Fable.Import(info,_,r), _) when not info.IsCompilerGenerated ->
                let t = value.Type
                let info = resolveImportMemberBinding ident info
                return Fable.Let(ident, Fable.Extended(Fable.Curry(Fable.Import(info,t,r), List.length args), r), body)
            | _ -> return Fable.Let(ident, value, body)

    | FSharpExprPatterns.LetRec(recBindings, body) ->
        // First get a context containing all idents and use it compile the values
        let ctx, idents =
            (recBindings, (ctx, []))
            ||> List.foldBack (fun (PutIdentInScope com ctx (newContext, ident), _, _) (ctx, idents) ->
                (newContext, ident::idents))
        let _, bindingExprs, _ = List.unzip3 recBindings
        let! exprs = transformExprList com ctx bindingExprs
        let bindings = List.zip idents exprs
        let! body = transformExpr com ctx body
        match bindings with
        // If there's only one binding compile as Let to play better with optimizations
        | [ident, value] -> return Fable.Let(ident, value, body)
        | bindings -> return Fable.LetRec(bindings, body)

    // `argTypes2` is always empty
    | FSharpExprPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, _argTypes2, argExprs) ->
        let r = makeRangeFrom fsExpr
        let typ = makeType ctx.GenericArgs fsExpr.Type
        let! argExprs = transformExprList com ctx argExprs
        let argTypes = List.map (makeType ctx.GenericArgs) argTypes

        match ctx.PrecompilingInlineFunction with
        | Some _ ->
            let sourceTypes = List.map (makeType ctx.GenericArgs) sourceTypes
            let e = Fable.UnresolvedTraitCall(sourceTypes, traitName, flags.IsInstance, argTypes, argExprs)
            return Fable.Unresolved(e, typ, r)
        | None ->
            match tryFindWitness ctx argTypes flags.IsInstance traitName with
            | None ->
                let sourceTypes = List.map (makeType ctx.GenericArgs) sourceTypes
                return transformTraitCall com ctx r typ sourceTypes traitName flags.IsInstance argTypes argExprs
            | Some w ->
                let callInfo = makeCallInfo None argExprs argTypes
                return makeCall r typ callInfo w.Expr

    | FSharpExprPatterns.CallWithWitnesses(callee, memb, ownerGenArgs, membGenArgs, witnesses, args) ->
        match callee with
        | Some(CreateEvent(callee, event) as createEvent) ->
            let! callee = transformExpr com ctx callee
            let typ = makeType ctx.GenericArgs createEvent.Type
            let callee = makeCallFrom com ctx (makeRangeFrom createEvent) typ [] (Some callee) [] event
            let! args = transformExprList com ctx args
            let genArgs = ownerGenArgs @ membGenArgs |> List.map (makeType ctx.GenericArgs)
            let typ = makeType ctx.GenericArgs fsExpr.Type
            return makeCallFrom com ctx (makeRangeFrom fsExpr) typ genArgs (Some callee) args memb

        | callee ->
            let r = makeRangeFrom fsExpr
            let! callee = transformExprOpt com ctx callee
            let! args = transformExprList com ctx args
            let genArgs = ownerGenArgs @ membGenArgs |> List.map (makeType ctx.GenericArgs)
            let typ = makeType ctx.GenericArgs fsExpr.Type
            let! ctx = trampoline {
                match witnesses with
                | [] -> return ctx
                | witnesses ->
                    let witnesses =
                        witnesses |> List.choose (function
                            // Index is not reliable, just append witnesses from parent call
                            | FSharpExprPatterns.WitnessArg _idx -> None
                            | NestedLambda(args, body) ->
                                match body with
                                | FSharpExprPatterns.Call(callee, memb, _, _, _args) ->
                                    Some(memb.CompiledName, Option.isSome callee, args, body)
                                | FSharpExprPatterns.AnonRecordGet(_, calleeType, fieldIndex) ->
                                    let fieldName = calleeType.AnonRecordTypeDetails.SortedFieldNames[fieldIndex]
                                    Some("get_" + fieldName, true, args, body)
                                | FSharpExprPatterns.FSharpFieldGet(_, _, field) ->
                                    Some("get_" + field.Name, true, args, body)
                                | _ -> None
                            | _ -> None)

                    // Seems witness act like a stack (that's why we reverse them)
                    // so a witness may need other witnesses to be resolved
                    return! (ctx, List.rev witnesses) ||> trampolineListFold (fun ctx (traitName, isInstance, args, body) -> trampoline {
                        let ctx, args = makeFunctionArgs com ctx args
                        let! body = transformExpr com ctx body
                        let w: Fable.Witness = {
                            TraitName = traitName
                            IsInstance = isInstance
                            FileName = com.CurrentFile
                            Expr = Fable.Delegate(args, body, None, Fable.Tags.empty)
                        }
                        return { ctx with Witnesses = w::ctx.Witnesses }
                    })
                }

            return makeCallFrom com ctx r typ genArgs callee args memb

    | FSharpExprPatterns.Application(applied, genArgs, args) ->
        match applied, args with
        // Why do application without arguments happen? So far I've seen it
        // to access None or struct values (like the Result type)
        | _, [] -> return! transformExpr com ctx applied

        // Application of locally inlined lambdas
        | FSharpExprPatterns.Value var, args when isInline var ->
            let r = makeRangeFrom fsExpr
            match ctx.ScopeInlineValues |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
            | Some (_,fsExpr) ->
                let genArgs = List.map (makeType ctx.GenericArgs) genArgs |> matchGenericParamsFrom var
                let ctx = { ctx with GenericArgs = (ctx.GenericArgs, genArgs) ||> Seq.fold (fun map (k, v) -> Map.add k v map) }
                let! callee = transformExpr com ctx fsExpr
                match args with
                | [] -> return callee
                | args ->
                    let typ = makeType ctx.GenericArgs fsExpr.Type
                    let! args = transformExprList com ctx args
                    return Fable.CurriedApply(callee, args, typ, r)
            | None ->
                return "Cannot resolve locally inlined lambda: " + var.DisplayName
                |> addErrorAndReturnNull com ctx.InlinePath r

        // When using Fable dynamic operator, we must untuple arguments
        // Note F# compiler wraps the value in a closure if it detects it's a lambda
        | FSharpExprPatterns.Let((_, FSharpExprPatterns.Call(None,m,_,_,[e1; e2]),_),_), args
                when m.FullName = "Fable.Core.JsInterop.(?)" || m.FullName = "Fable.Core.PyInterop.(?)" ->
            let! e1 = transformExpr com ctx e1
            let! e2 = transformExpr com ctx e2
            let e = Fable.Get(e1, Fable.ExprGet e2, Fable.Any, e1.Range)
            let! args = transformExprList com ctx args
            let args = destructureTupleArgs args
            let typ = makeType ctx.GenericArgs fsExpr.Type
            let r = makeRangeFrom fsExpr
            // Convert this to emit so auto-uncurrying is applied
            return emitExpr r typ (e::args) "$0($1...)"

        // Some instance members such as Option.get_IsSome are compiled as static members, and the F# compiler
        // wraps calls with an application. But in Fable they will be replaced so the application is not needed
        | FSharpExprPatterns.Call(Some _, memb, _, [], []) as call, [FSharpExprPatterns.Const(null, _)]
                        when memb.IsInstanceMember && not memb.IsInstanceMemberInCompiledCode ->
             return! transformExpr com ctx call

        | applied, args ->
            let! applied = transformExpr com ctx applied
            let! args = transformExprList com ctx args
            let typ = makeType ctx.GenericArgs fsExpr.Type
            return Fable.CurriedApply(applied, args, typ, makeRangeFrom fsExpr)

    | FSharpExprPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        let! guardExpr = transformExpr com ctx guardExpr
        let! thenExpr = transformExpr com ctx thenExpr
        let! fableElseExpr = transformExpr com ctx elseExpr

        let altElseExpr =
            match elseExpr with
            | RaisingMatchFailureExpr _infoWhereErrorOccurs ->
                let errorMessage = "Match failure"
                let rangeOfElseExpr = makeRangeFrom elseExpr
                let errorExpr = Fable.Value(Fable.StringConstant errorMessage, None) |> Replacements.Api.error com
                makeThrow rangeOfElseExpr Fable.Any errorExpr
            | _ ->
                fableElseExpr

        return Fable.IfThenElse(guardExpr, thenExpr, altElseExpr, makeRangeFrom fsExpr)

    | FSharpExprPatterns.TryFinally (body, finalBody, _, _) ->
        let r = makeRangeFrom fsExpr
        match body with
        | FSharpExprPatterns.TryWith(body, _, _, catchVar, catchBody, _, _) ->
            return makeTryCatch com ctx r body (Some (catchVar, catchBody)) (Some finalBody)
        | _ -> return makeTryCatch com ctx r body None (Some finalBody)

    | FSharpExprPatterns.TryWith (body, _, _, catchVar, catchBody, _, _) ->
        return makeTryCatch com ctx (makeRangeFrom fsExpr) body (Some (catchVar, catchBody)) None

    | FSharpExprPatterns.NewDelegate(delegateType, fsExpr) ->
        return! transformDelegate com ctx delegateType fsExpr

    | FSharpExprPatterns.Lambda(arg, body) ->
        let ctx, args = makeFunctionArgs com ctx [arg]
        match args with
        | [arg] ->
            let! body = transformExpr com ctx body
            let body = flattenLambdaBodyWithTupleArgs arg body
            return Fable.Lambda(arg, body, None)
        | _ -> return failwith "makeFunctionArgs returns args with different length"

    // Getters and Setters
    | FSharpExprPatterns.AnonRecordGet(callee, calleeType, fieldIndex) ->
        let r = makeRangeFrom fsExpr
        let! callee = transformExpr com ctx callee
        let fieldName = calleeType.AnonRecordTypeDetails.SortedFieldNames[fieldIndex]
        let typ = makeType ctx.GenericArgs fsExpr.Type
        // Don't use generics from the inlined context for the field type as this is used for uncurrying
        let fieldType = makeType Map.empty fsExpr.Type
        return Fable.Get(callee, Fable.FieldInfo.Create(fieldName, fieldType=fieldType), typ, r)

    | FSharpExprPatterns.FSharpFieldGet(callee, calleeType, field) ->
        let r = makeRangeFrom fsExpr
        let! callee = transformCallee com ctx callee calleeType
//      let typ = makeType ctx.GenericArgs fsExpr.Type // Doesn't always work
        let typ = resolveFieldType ctx calleeType field.FieldType
        let kind = Fable.FieldInfo.Create(
            FsField.FSharpFieldName field,
            fieldType=makeType Map.empty field.FieldType,
            isMutable=field.IsMutable)
        return Fable.Get(callee, kind, typ, r)

    | FSharpExprPatterns.TupleGet(tupleType, tupleElemIndex, IgnoreAddressOf tupleExpr) ->
        // F# compiler generates a tuple when matching against multiple values,
        // if the TupleGet accesses an immutable ident in scope we use the ident directly
        // to increase the chances of the tuple being removed in beta reduction
        let tupleElemValue =
            match tupleExpr with
            | FSharpExprPatterns.Value tupleIdent
                when tupleIdent.IsCompilerGenerated &&
                    (tupleIdent.CompiledName = "matchValue" || tupleIdent.CompiledName = "patternInput") ->
                tryGetValueFromScope ctx tupleIdent
                |> Option.bind (function
                    | Fable.Value(Fable.NewTuple(values,_),_) ->
                        List.tryItem tupleElemIndex values
                    | _ -> None)
            | _ -> None

        match tupleElemValue with
        | Some(Fable.IdentExpr id as e) when not id.IsMutable -> return e
        | _ ->
            let! tupleExpr = transformExpr com ctx tupleExpr
//            let typ = makeType ctx.GenericArgs fsExpr.Type // Doesn't always work
            let typ = Seq.item tupleElemIndex tupleType.GenericArguments |> makeType ctx.GenericArgs
            return Fable.Get(tupleExpr, Fable.TupleIndex tupleElemIndex, typ, makeRangeFrom fsExpr)

    | FSharpExprPatterns.UnionCaseGet (IgnoreAddressOf unionExpr, fsType, unionCase, field) ->
        let r = makeRangeFrom fsExpr
        let! unionExpr = transformExpr com ctx unionExpr
        match getUnionPattern fsType unionCase with
        | ErasedUnionCase ->
            let index = unionCase.Fields |> Seq.findIndex (fun x -> x.Name = field.Name)
            return Fable.Get(unionExpr, Fable.TupleIndex(index), makeType ctx.GenericArgs fsType, r)
        | ErasedUnion _  ->
            if unionCase.Fields.Count = 1 then return unionExpr
            else
                let index = unionCase.Fields |> Seq.findIndex (fun x -> x.Name = field.Name)
                return Fable.Get(unionExpr, Fable.TupleIndex index, makeType ctx.GenericArgs fsType, r)
        | TypeScriptTaggedUnion _ ->
            if unionCase.Fields.Count = 1 then return unionExpr
            else
                return "Tagged unions must have one single field: " + (getFsTypeFullName fsType)
                |> addErrorAndReturnNull com ctx.InlinePath r
        | StringEnum _ ->
            return "StringEnum types cannot have fields"
            |> addErrorAndReturnNull com ctx.InlinePath r
        | OptionUnion(t, _) ->
            return Fable.Get(unionExpr, Fable.OptionValue, makeType ctx.GenericArgs t, r)
        | ListUnion t ->
            let t = makeType ctx.GenericArgs t
            let kind, t =
                if field.Name = "Head"
                then Fable.ListHead, t
                else Fable.ListTail, Fable.List t
            return Fable.Get(unionExpr, kind, t, r)
        | DiscriminatedUnion(tdef, genArgs) ->
            let caseIndex = unionCaseTag com tdef unionCase
            let fieldIndex = unionCase.Fields |> Seq.findIndex (fun fi -> fi.Name = field.Name)
            let kind = Fable.UnionFieldInfo.Create(
                entity=FsEnt.Ref tdef,
                genArgs=makeTypeGenArgs ctx.GenericArgs genArgs,
                caseIndex=caseIndex,
                fieldIndex=fieldIndex
            )
//            let typ = makeType ctx.GenericArgs fsExpr.Type // Doesn't always work
            let typ = resolveFieldType ctx fsType field.FieldType
            return Fable.Get(unionExpr, kind, typ, r)

    | FSharpExprPatterns.FSharpFieldSet(callee, calleeType, field, value) ->
        let r = makeRangeFrom fsExpr
        let t = makeType Map.empty field.FieldType
        let! callee = transformCallee com ctx callee calleeType
        let! value = transformExpr com ctx value
        return Fable.Set(callee, Fable.FieldSet(FsField.FSharpFieldName field), t, value, r)

    | FSharpExprPatterns.UnionCaseTag(IgnoreAddressOf unionExpr, unionType) ->
        // TODO: This is an inconsistency. For new unions and union tests we calculate
        // the tag in this step but here we delay the calculation until Fable2Babel
        do tryDefinition unionType
           |> Option.iter (fun (tdef, _) -> com.AddWatchDependency(FsEnt.SourcePath tdef))
        let! unionExpr = transformExpr com ctx unionExpr
        return Fable.Get(unionExpr, Fable.UnionTag, Fable.Any, makeRangeFrom fsExpr)

    | FSharpExprPatterns.UnionCaseSet (_unionExpr, _type, _case, _caseField, _valueExpr) ->
        return "Unexpected UnionCaseSet" |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    | FSharpExprPatterns.ValueSet (valToSet, valueExpr) ->
        let r = makeRangeFrom fsExpr
        let! valueExpr = transformExpr com ctx valueExpr
        match valToSet.DeclaringEntity with
        | Some ent when ent.IsFSharpModule && com.Options.Language = Rust ->
            // For Rust mutable module values are compiled as functions returning refcells
            let typ = makeType ctx.GenericArgs valToSet.FullType
            let memberRef = Fable.GeneratedMember.Function(valToSet.CompiledName, [], typ, entRef = FsEnt.Ref(ent))
            let callInfo = Fable.CallInfo.Create(memberRef = memberRef)
            let valToSet = makeValueFrom com ctx r valToSet
            let callExpr = makeCall r valToSet.Type callInfo valToSet
            return Fable.Set(callExpr, Fable.ValueSet, valueExpr.Type, valueExpr, r)
        | Some ent when ent.IsFSharpModule && isModuleValueCompiledAsFunction com valToSet ->
            // Mutable and public module values are compiled as functions, because
            // values imported from ES2015 modules cannot be modified (see #986)
            let valToSet = makeValueFrom com ctx r valToSet
            let args = [valueExpr]
            let info = makeCallInfo None args [valToSet.Type; Fable.Boolean]
            return makeCall r Fable.Unit info valToSet
        | _ ->
            let valToSet = makeValueFrom com ctx r valToSet
            // It can happen that we're assigning to a value of unit type
            // and Fable replaces it with unit constant, see #2548
            return
                match valToSet.Type with
                | Fable.Unit -> valueExpr
                | _ -> Fable.Set(valToSet, Fable.ValueSet, valueExpr.Type, valueExpr, r)

    | FSharpExprPatterns.NewArray(FableType com ctx elTyp, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return makeArray elTyp argExprs

    | FSharpExprPatterns.NewTuple(tupleType, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return Fable.NewTuple(argExprs, tupleType.IsStructTupleType) |> makeValue (makeRangeFrom fsExpr)

    | FSharpExprPatterns.ObjectExpr(objType, baseCall, overrides, otherOverrides) ->
        match ctx.EnclosingMember with
        | Some m when m.IsImplicitConstructor ->
            let thisArg = getIdentUniqueName ctx "_this" |> makeIdent
            let thisValue = Fable.Value(Fable.ThisValue Fable.Any, None)
            let ctx = { ctx with BoundConstructorThis = Some thisArg }
            let! objExpr = transformObjExpr com ctx objType baseCall overrides otherOverrides
            return Fable.Let(thisArg, thisValue, objExpr)
        | _ -> return! transformObjExpr com ctx objType baseCall overrides otherOverrides

    | FSharpExprPatterns.NewObject(memb, genArgs, args) ->
        let! args = transformExprList com ctx args
        let genArgs = List.map (makeType ctx.GenericArgs) genArgs
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return makeCallFrom com ctx (makeRangeFrom fsExpr) typ genArgs None args memb

    | FSharpExprPatterns.Sequential (first, second) ->
        let exprs =
            match ctx.CaptureBaseConsCall with
            | Some(baseEnt, captureBaseCall) ->
                match first with
                | ConstructorCall(call, genArgs, args)
                // This pattern occurs in constructors that define a this value: `type C() as this`
                // We're discarding the bound `this` value, it "shouldn't" be used in the base constructor arguments
                | FSharpExprPatterns.Let(_, (ConstructorCall(call, genArgs, args))) ->
                    match call.DeclaringEntity with
                    | Some ent when ent = baseEnt ->
                        let r = makeRangeFrom first
                        transformBaseConsCall com ctx r baseEnt call genArgs args |> captureBaseCall
                        [second]
                    | _ -> [first; second]
                | _ -> [first; second]
            | _ -> [first; second]
        let! exprs = transformExprList com ctx exprs
        return Fable.Sequential exprs

    | FSharpExprPatterns.NewRecord(fsType, argExprs) ->
        let r = makeRangeFrom fsExpr
        let! argExprs = transformExprList com ctx argExprs
        let genArgs = makeTypeGenArgs ctx.GenericArgs (getGenericArguments fsType)
        return Fable.NewRecord(argExprs, FsEnt.Ref fsType.TypeDefinition, genArgs) |> makeValue r

    | FSharpExprPatterns.NewAnonRecord(fsType, argExprs) ->
        let r = makeRangeFrom fsExpr
        let! argExprs = transformExprList com ctx argExprs
        let fieldNames = fsType.AnonRecordTypeDetails.SortedFieldNames
        let genArgs = makeTypeGenArgs ctx.GenericArgs (getGenericArguments fsType)
        let isStruct =
            match fsType.BaseType with
            | Some typ -> (getFsTypeFullName typ) = Types.valueType
            | None -> false
        return Fable.NewAnonymousRecord(argExprs, fieldNames, genArgs, isStruct) |> makeValue r

    | FSharpExprPatterns.NewUnionCase(fsType, unionCase, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return argExprs
        |> transformNewUnion com ctx (makeRangeFrom fsExpr) fsType unionCase

    | FSharpExprPatterns.TypeTest (FableType com ctx typ, expr) ->
        let! expr = transformExpr com ctx expr
        return Fable.Test(expr, Fable.TypeTest typ, makeRangeFrom fsExpr)

    | FSharpExprPatterns.UnionCaseTest(IgnoreAddressOf unionExpr, fsType, unionCase) ->
        return! transformUnionCaseTest com ctx (makeRangeFrom fsExpr) unionExpr fsType unionCase

    // Pattern Matching
    | FSharpExprPatterns.DecisionTree(IgnoreAddressOf decisionExpr, decisionTargets) ->
        let! fableDecisionExpr = transformExpr com ctx decisionExpr
        let! fableDecisionTargets = transformDecisionTargets com ctx [] decisionTargets

        // rewrite last decision target if it throws MatchFailureException
        let compiledFableTargets =
            match snd (List.last decisionTargets) with
            | RaisingMatchFailureExpr _infoWhereErrorOccurs ->
                match decisionExpr with
                | FSharpExprPatterns.IfThenElse(FSharpExprPatterns.UnionCaseTest(_unionValue, unionType, _unionCaseInfo), _, _) ->
                    let rangeOfLastDecisionTarget = makeRangeFrom (snd (List.last decisionTargets))
                    let errorMessage = "Match failure: " + unionType.TypeDefinition.FullName
                    let errorExpr = Fable.Value(Fable.StringConstant errorMessage, None) |> Replacements.Api.error com
                    // Creates a "throw Error({errorMessage})" expression
                    let throwExpr = makeThrow rangeOfLastDecisionTarget Fable.Any errorExpr

                    fableDecisionTargets
                    |> List.replaceLast (fun _lastExpr -> [], throwExpr)

                | _ ->
                    // TODO: rewrite other `MatchFailureException` to `failwith "The match cases were incomplete"`
                    fableDecisionTargets

            | _ -> fableDecisionTargets

        return Fable.DecisionTree(fableDecisionExpr, compiledFableTargets)

    | FSharpExprPatterns.DecisionTreeSuccess(targetIndex, boundValues) ->
        let! boundValues = transformExprList com ctx boundValues
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return Fable.DecisionTreeSuccess(targetIndex, boundValues, typ)

    | FSharpExprPatterns.ILFieldGet(None, ownerTyp, fieldName) ->
        let ownerTyp = makeType ctx.GenericArgs ownerTyp
        let typ = makeType ctx.GenericArgs fsExpr.Type
        match Replacements.Api.tryField com typ ownerTyp fieldName with
        | Some expr -> return expr
        | None ->
            return $"Cannot compile ILFieldGet(%A{ownerTyp}, %s{fieldName})"
            |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    | FSharpExprPatterns.Quote _ ->
        return "Quotes are not currently supported by Fable"
        |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    | FSharpExprPatterns.AddressOf expr ->
        let r = makeRangeFrom fsExpr
        match expr with
        // This matches passing variables by reference
        | FSharpExprPatterns.Call(None, memb, _, _, _)
        | FSharpExprPatterns.Value memb ->
            let value = makeValueFrom com ctx r memb
            if memb.IsMutable || isByRefValue memb then
                match memb.DeclaringEntity with
                // TODO: check if it works for mutable module let bindings
                | Some ent when ent.IsFSharpModule && isNotPrivate memb ->
                    return Replacements.Api.makeRefFromMutableFunc com ctx r value.Type value
                | _ ->
                    return Replacements.Api.makeRefFromMutableValue com ctx r value.Type value
            else
                if com.Options.Language = Rust
                then return Replacements.Api.makeRefFromMutableValue com ctx r value.Type value
                else return Replacements.Api.makeRefCell com r value.Type value
        // This matches passing fields by reference
        | FSharpExprPatterns.FSharpFieldGet(callee, calleeType, field) ->
            let r = makeRangeFrom fsExpr
            let! callee = transformCallee com ctx callee calleeType
            let typ = makeType ctx.GenericArgs expr.Type
            let key = FsField.FSharpFieldName field
            return Replacements.Api.makeRefFromMutableField com ctx r typ callee key
        | _ ->
            // ignore AddressOf, pass by value
            return! transformExpr com ctx expr

    | FSharpExprPatterns.AddressSet expr ->
        let r = makeRangeFrom fsExpr
        match expr with
        | FSharpExprPatterns.Value valToSet, valueExpr when isByRefValue valToSet ->
            // Setting byref value is compiled as FSharpRef op_ColonEquals
            let! value = transformExpr com ctx valueExpr
            let valToSet = makeValueFrom com ctx r valToSet
            return Replacements.Api.setRefCell com r valToSet value
        | _ ->
            return "Mutating this argument passed by reference is not supported"
            |> addErrorAndReturnNull com ctx.InlinePath r

    // | FSharpExprPatterns.ILFieldSet _
    // | FSharpExprPatterns.ILAsm _
    | expr ->
        return $"Cannot compile expression %A{expr}"
        |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)
  }

let private isIgnoredNonAttachedMember (meth: FSharpMemberOrFunctionOrValue) =
    Option.isSome meth.LiteralValue
    || meth.Attributes |> Seq.exists (fun att ->
        match att.AttributeType.TryFullName with
        | Some(Atts.global_ | Naming.StartsWith Atts.import _ | Naming.StartsWith Atts.emit _) -> true
        | _ -> false)
    || (match meth.DeclaringEntity with
        | Some ent -> isGlobalOrImportedFSharpEntity ent
        | None -> false)

let private transformPrimaryConstructor (com: FableCompiler) (ctx: Context)
            (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    match memb.DeclaringEntity with
    | None -> "Unexpected constructor without declaring entity: " + memb.FullName
              |> addError com ctx.InlinePath None; []
    | Some ent ->
        let mutable baseCall = None
        let captureBaseCall =
            tryGetBaseEntity ent
            |> Option.map (fun (ent, _) -> ent, fun c -> baseCall <- Some c)
        let bodyCtx, args = bindMemberArgs com ctx args
        let bodyCtx = { bodyCtx with CaptureBaseConsCall = captureBaseCall }
        let body = transformExpr com bodyCtx body |> run
        let consName, _ = getMemberDeclarationName com memb
        let cons: Fable.MemberDecl =
            { Name = consName
              Args = args
              Body = body
              UsedNames = set ctx.UsedNamesInDeclarationScope
              IsMangled = false
              MemberRef = getFunctionMemberRef memb
              ImplementedSignatureRef = None
              Tags = Fable.Tags.empty
              XmlDoc = tryGetXmlDoc memb.XmlDoc }
        com.AddConstructor(ent.FullName, cons, baseCall)
        []

/// When using `importMember`, uses the member display name as selector
let private importExprSelector (memb: FSharpMemberOrFunctionOrValue) selector =
    match selector with
    | Naming.placeholder -> getMemberDisplayName memb
    | _ -> selector

let private transformImport _com r typ name args memberRef selector path =
    [Fable.MemberDeclaration
        { Name = name
          Args = args
          Body = makeImportUserGenerated r typ selector path
          UsedNames = Set.empty
          IsMangled = true
          MemberRef = memberRef
          ImplementedSignatureRef = None
          Tags = Fable.Tags.empty
          XmlDoc = None }]

let private transformImportValue com r typ name (memb: FSharpMemberOrFunctionOrValue) selector path =
    if memb.IsMutable && isNotPrivate memb then // See #1314
        "Imported members cannot be mutable and public, please make it private: " + name
        |> addError com [] None
    let memberRef = Fable.GeneratedMember.Value(name, typ)
    transformImport com r typ name [] memberRef selector path

let private transformMemberValue (com: IFableCompiler) ctx name (memb: FSharpMemberOrFunctionOrValue) (value: FSharpExpr) =
    let value = transformExpr com ctx value |> run
    match value with
    // Accept import expressions, e.g. let foo = import "foo" "myLib"
    | Fable.Import(info, typ, r) when not info.IsCompilerGenerated ->
        match typ with
        | Fable.LambdaType(_, Fable.LambdaType _) ->
            "Change declaration of member: " + name + "\n"
            + "Importing functions with multiple arguments as `let add: int->int->int` won't uncurry parameters." + "\n"
            + "Use following syntax: `let add (x:int) (y:int): int = import ...`"
            |> addError com ctx.InlinePath None
        | _ -> ()
        let selector = importExprSelector memb info.Selector
        transformImportValue com r typ name memb selector info.Path
    | fableValue ->
        // Mutable public values must be compiled as functions (see #986)
        // because values imported from ES2015 modules cannot be modified
        let fableValue =
            if memb.IsMutable && isNotPrivate memb
            then Replacements.Api.createMutablePublicValue com fableValue
            else fableValue
        [Fable.MemberDeclaration
            { Name = name
              Args = [] //Kind = Fable.MemberValue(memb.IsMutable)
              Body = fableValue
              IsMangled = true
              MemberRef = getValueMemberRef memb
              ImplementedSignatureRef = None
              UsedNames = set ctx.UsedNamesInDeclarationScope
              Tags = Fable.Tags.empty
              XmlDoc = tryGetXmlDoc memb.XmlDoc }]

// TODO: This should be moved to Fable2Babel/Fable2Python
let private applyJsPyDecorators (com: IFableCompiler) (_ctx: Context) name (memb: FSharpMemberOrFunctionOrValue) (args: Fable.Ident list) (body: Fable.Expr) =
    let methodInfo =
        lazy
            let returnType = makeType Map.empty memb.ReturnParameter.Type
            let parameters =
                memb.CurriedParameterGroups
                |> Seq.collect id
                |> Seq.mapi (fun i p -> defaultArg p.Name $"arg{i}", makeType Map.empty p.Type)
                |> Seq.toList
            Replacements.Api.makeMethodInfo com None name parameters returnType

    let newDecorator (ent: FSharpEntity) (args: IList<FSharpType * obj>) =
        let args =
            args |> Seq.map (fun (typ, value) ->
                let typ = makeType Map.empty typ
                makeTypeConst None typ value)
            |> Seq.toList
        let callInfo = Fable.CallInfo.Create(args=args, isCons=true)
        FsEnt.Ref(ent) |> entityIdent com
        |> makeCall None Fable.Any callInfo

    let applyDecorator (body: Fable.Expr)
                       (attr: {| Entity: FSharpEntity
                                 Args: IList<FSharpType * obj>
                                 MethodInfo: bool |}) =
        let extraArgs =
            if attr.MethodInfo then [ methodInfo.Value ]
            else []
        let callInfo = makeCallInfo None (body::extraArgs) []
        let newAttr = newDecorator attr.Entity attr.Args
        getExpr None Fable.Any newAttr (makeStrConst "Decorate")
        |> makeCall None body.Type callInfo

    memb.Attributes
    |> Seq.choose (fun att ->
        let attEnt = nonAbbreviatedDefinition att.AttributeType
        match attEnt.BaseType with
        | Some tbase when tbase.HasTypeDefinition ->
            match tbase.TypeDefinition.TryFullName with
            | Some (Atts.jsDecorator | Atts.pyDecorator) ->
                Some {| Entity = attEnt; Args = att.ConstructorArguments; MethodInfo = false |}
            | Some (Atts.jsReflectedDecorator | Atts.pyReflectedDecorator) ->
                Some {| Entity = attEnt; Args = att.ConstructorArguments; MethodInfo = true |}
            | _ -> None
        | _ -> None)
    |> Seq.rev
    |> Seq.toList
    |> function
        | [] -> None
        | decorators ->
            // This must be compiled as JS `function` (not arrow) so we don't have issues with bound this
            let body = Fable.Delegate(args, body, None, ["not-arrow"])
            List.fold applyDecorator body decorators |> Some

let private transformMemberFunction (com: IFableCompiler) ctx (name: string) (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    match body with
    // Accept import expressions, e.g. let foo x y = import "foo" "myLib"
    | Fable.Import(info, _, r) when not info.IsCompilerGenerated ->
        // Use the full function type
        let typ = makeType Map.empty memb.FullType
        let selector = importExprSelector memb info.Selector
        let memberRef =
            // If this is a getter, it means the imported value is an object but Fable will call it as a function, see #2329
            if memb.IsPropertyGetterMethod // || (memb.IsFunction && com.Options.Language = Rust)
            then Fable.GeneratedMember.Function(name, [], typ)
            else Fable.GeneratedMember.Value(name, typ)
        transformImport com r typ name [] memberRef selector info.Path
    | body ->
        // If this is a static constructor, call it immediately
        if memb.CompiledName = ".cctor" then
            [Fable.ActionDeclaration
                { Body =
                    Fable.Delegate(args, body, Some name, Fable.Tags.empty)
                    |> makeCall None Fable.Unit (makeCallInfo None [] [])
                  UsedNames = set ctx.UsedNamesInDeclarationScope }]
        else
            let body, memberRef =
                match com.Options.Language with
                | JavaScript | TypeScript | Python ->
                    match applyJsPyDecorators com ctx name memb args body with
                    | Some body -> body, Fable.GeneratedMember.Value(name, body.Type, isInstance=memb.IsInstanceMember)
                    | None -> body, getFunctionMemberRef memb
                | _ -> body, getFunctionMemberRef memb

            [Fable.MemberDeclaration
                { Name = name
                  Args = args
                  Body = body
                  UsedNames = set ctx.UsedNamesInDeclarationScope
                  IsMangled = true
                  MemberRef = memberRef
                  ImplementedSignatureRef = None
                  Tags = Fable.Tags.empty
                  XmlDoc = tryGetXmlDoc memb.XmlDoc }]

let private transformMemberFunctionOrValue (com: IFableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let name, _ = getMemberDeclarationName com memb
    memb.Attributes
    |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
    |> function
    | ImportAtt(selector, path) ->
        let selector =
            if selector = Naming.placeholder then getMemberDisplayName memb
            else selector
        let typ = makeType Map.empty memb.FullType
        transformImportValue com None typ name memb selector path
    | _ ->
        if isModuleValueForDeclarations memb
        then transformMemberValue com ctx name memb body
        else transformMemberFunction com ctx name memb args body

let private transformImplementedSignature (com: FableCompiler) (ctx: Context)
            (implementingEntity: FSharpEntity) (signature: FSharpAbstractSignature)
            (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    let entFullName = implementingEntity.FullName
    let info = getImplementedSignatureInfo com ctx body.Range com.NonMangledAttachedMemberConflicts (Some implementingEntity) signature
    com.AddAttachedMember(entFullName, isMangled=info.isMangled, memb=
        { Name = info.name
          Args = args
          Body = body
          IsMangled = info.isMangled
          MemberRef = getFunctionMemberRef memb
          ImplementedSignatureRef = Some info.memberRef
          UsedNames = set ctx.UsedNamesInDeclarationScope
          Tags = Fable.Tags.empty
          XmlDoc = tryGetXmlDoc memb.XmlDoc })

let private transformExplicitlyAttachedMember (com: FableCompiler) (ctx: Context)
            (declaringEntity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    let entFullName = declaringEntity.FullName
    let name, isMangled =
        match Compiler.Language with
        | Rust -> getMemberDeclarationName com memb |> fst, true
        | _ -> Naming.removeGetSetPrefix memb.CompiledName, false
    com.AddAttachedMember(entFullName, isMangled=false, memb =
        { Name = name
          Body = body
          Args = args
          IsMangled = isMangled
          UsedNames = set ctx.UsedNamesInDeclarationScope
          MemberRef = getFunctionMemberRef memb
          ImplementedSignatureRef = None
          Tags = Fable.Tags.empty
          XmlDoc = tryGetXmlDoc memb.XmlDoc })

let private transformMemberDecl (com: FableCompiler) (ctx: Context) (memb: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let ctx = { ctx with EnclosingMember = Some memb
                         UsedNamesInDeclarationScope = HashSet() }
    if isIgnoredNonAttachedMember memb then
        if memb.IsMutable && isNotPrivate memb && hasAttribute Atts.global_ memb.Attributes then
            "Global members cannot be mutable and public, please make it private: " + memb.DisplayName
            |> addError com [] None
        []
    // for Rust, retain inlined functions that have [<CompiledName("...")>] attribute
    elif isInline memb && (Compiler.Language <> Rust || not (hasAttribute Atts.compiledName memb.Attributes)) then
        []
    elif memb.IsImplicitConstructor then
        transformPrimaryConstructor com ctx memb args body
    elif memb.IsOverrideOrExplicitInterfaceImplementation then
        // Ignore attached members generated by the F# compiler (for comparison and equality)
        if not memb.IsCompilerGenerated then
            match memb.DeclaringEntity with
            | Some ent ->
                if isGlobalOrImportedFSharpEntity ent then ()
                elif isErasedOrStringEnumFSharpEntity ent then
                    // Ignore abstract members for classes, see #2295
                    if ent.IsFSharpUnion || ent.IsFSharpRecord then
                        let r = makeRange memb.DeclarationLocation |> Some
                        "Erased unions/records cannot implement abstract members"
                        |> addError com ctx.InlinePath r
                else
                    // Not sure when it's possible that a member implements multiple abstract signatures
                    memb.ImplementedAbstractSignatures
                    |> Seq.tryHead
                    |> Option.iter (fun s -> transformImplementedSignature com ctx ent s memb args body)
            | None -> ()
        []
    else
        match memb.DeclaringEntity with
        | Some ent when (isAttachMembersEntity com ent && memb.CompiledName <> ".cctor") ->
            transformExplicitlyAttachedMember com ctx ent memb args body; []
        | _ -> transformMemberFunctionOrValue com ctx memb args body

let private addUsedRootName (com: Compiler) name (usedRootNames: Set<string>) =
    if com.Options.Language <> Rust && Set.contains name usedRootNames then
        "Cannot have two module members with same name: " + name
        |> addError com [] None
    Set.add name usedRootNames

// Entities that are not output to other languages
// Interfaces should be part of the AST, see #2673
let private isIgnoredLeafEntity (ent: FSharpEntity) =
    // ent.IsInterface
    ent.IsEnum
    || ent.IsMeasure
    || ent.IsFSharpAbbreviation //&& com.Options.Language <> Rust
    || ent.IsDelegate
    || ent.IsNamespace // Ignore empty namespaces

// In case this is a recursive module, do a first pass to get all entity and member names
let rec private getUsedRootNames (com: Compiler) (usedNames: Set<string>) decls =
    (usedNames, decls) ||> List.fold (fun usedNames decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity(ent, sub) ->
            match sub with
            | [] when isIgnoredLeafEntity ent -> usedNames
            | [] ->
                let entRef = FsEnt.Ref ent
                let ent = com.GetEntity(entRef)
                if isErasedOrStringEnumEntity ent || isGlobalOrImportedEntity ent then
                    usedNames
                // Interfaces won't be output in JS code so prevent potential name conflicts, see #2864
                elif com.Options.Language = JavaScript && ent.IsInterface then
                    usedNames
                else
                    match getEntityDeclarationName com entRef with
                    | "" -> usedNames
                    | entName ->
                        let reflectionSuffix =
                            match com.Options.Language with
                            | Python -> Fable.Py.Naming.reflectionSuffix
                            | _ -> Naming.reflectionSuffix

                        addUsedRootName com entName usedNames
                        // Fable will inject an extra declaration for reflection,
                        // so add also the name with the reflection suffix
                        |> addUsedRootName com (entName + reflectionSuffix)
            | sub ->
                getUsedRootNames com usedNames sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memb,_,_) ->
            if memb.IsOverrideOrExplicitInterfaceImplementation
                || isInline memb || isEmittedOrImportedMember memb then usedNames
            else
                let memberName, _ = getMemberDeclarationName com memb
                addUsedRootName com memberName usedNames
        | FSharpImplementationFileDeclaration.InitAction _ -> usedNames)

let rec private transformDeclarations (com: FableCompiler) ctx fsDecls =
    fsDecls |> List.collect (fun fsDecl ->
        match fsDecl with
        | FSharpImplementationFileDeclaration.Entity(fsEnt, sub) ->
            match sub with
            | [] when isIgnoredLeafEntity fsEnt -> []
            | [] ->
                let entRef = FsEnt.Ref fsEnt
                let ent = (com :> Compiler).GetEntity(entRef)
                if isErasedOrStringEnumEntity ent || isGlobalOrImportedEntity ent then
                    []
                else
                    // If the file is empty F# creates a class for the module, but Fable clears the name
                    // because it matches the root module so it becomes invalid JS, see #2350
                    match getEntityDeclarationName com entRef with
                    | "" -> []
                    | name ->
                        [Fable.ClassDeclaration
                            { Name = name
                              Entity = entRef
                              Constructor = None
                              BaseCall = None
                              AttachedMembers = []
                              Tags = Fable.Tags.empty
                              XmlDoc = tryGetXmlDoc fsEnt.XmlDoc }]
            // This adds modules in the AST for languages that support them (like Rust)
            | sub when (fsEnt.IsFSharpModule || fsEnt.IsNamespace) && (com :> Compiler).Options.Language = Rust ->
                let entRef = FsEnt.Ref fsEnt
                let members = transformDeclarations com ctx sub
                [Fable.ModuleDeclaration
                    { Name = fsEnt.CompiledName
                      Entity = entRef
                      Members = members }]
            | sub ->
                transformDeclarations com ctx sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(meth, args, body) ->
            transformMemberDecl com ctx meth args body
        | FSharpImplementationFileDeclaration.InitAction fe ->
            let ctx = { ctx with UsedNamesInDeclarationScope = HashSet() }
            let e = transformExpr com ctx fe |> run
            [Fable.ActionDeclaration
                { Body = e
                  UsedNames = set ctx.UsedNamesInDeclarationScope }])

let rec getRootFSharpEntities (declarations: FSharpImplementationFileDeclaration list) =
    let rec getRootFSharpEntitiesInner decl = seq {
        match decl with
        | FSharpImplementationFileDeclaration.Entity (ent, nested) ->
            yield ent
            if ent.IsNamespace then
                for d in nested do
                    yield! getRootFSharpEntitiesInner d
        | _ -> ()
    }
    Seq.collect getRootFSharpEntitiesInner declarations

let getRootModule (declarations: FSharpImplementationFileDeclaration list) =
    let rec getRootModuleInner outerEnt decls =
        match decls, outerEnt with
        | [FSharpImplementationFileDeclaration.Entity (ent, decls)], _ when ent.IsFSharpModule || ent.IsNamespace ->
            getRootModuleInner (Some ent) decls
        | CommonNamespace(ent, decls), _ ->
            getRootModuleInner (Some ent) decls
        | _, Some e -> FsEnt.FullName e
        | _, None -> ""
    getRootModuleInner None declarations

let resolveInlineType (ctx: Context) = function
    | Fable.GenericParam(name=name) as v ->
        match Map.tryFind name ctx.GenericArgs with
        | Some v -> v
        | None -> v
    | t -> t.MapGenerics(resolveInlineType ctx)

let resolveFieldType (ctx: Context) (entityType: FSharpType) (fieldType: FSharpType) =
    let entityGenArgs =
        match tryDefinition entityType with
        | Some(def, _) when def.GenericParameters.Count = entityType.GenericArguments.Count->
            Seq.zip def.GenericParameters entityType.GenericArguments
            |> Seq.map (fun (p, a) -> genParamName p, makeType Map.empty a)
            |> Map
        | _ -> Map.empty
    let fieldType = makeType entityGenArgs fieldType
    if Map.isEmpty ctx.GenericArgs then fieldType
    else resolveInlineType ctx fieldType

type InlineExprInfo = {
    FileName: string
    ScopeIdents: Set<string>
    ResolvedIdents: Dictionary<string, string>
}

let resolveInlineIdent (ctx: Context) (info: InlineExprInfo) (ident: Fable.Ident) =
    if info.ScopeIdents.Contains ident.Name then
        let sanitizedName =
            match info.ResolvedIdents.TryGetValue(ident.Name) with
            | true, resolvedName -> resolvedName
            | false, _ ->
                let resolvedName = Naming.preventConflicts (isUsedName ctx) ident.Name
                ctx.UsedNamesInDeclarationScope.Add(resolvedName) |> ignore
                info.ResolvedIdents.Add(ident.Name, resolvedName)
                resolvedName
        { ident with Name = sanitizedName; Type = resolveInlineType ctx ident.Type }
    else ident

let resolveInlinedCallInfo com ctx info (callInfo: Fable.CallInfo) =
    { callInfo with
          ThisArg = Option.map (resolveInlineExpr com ctx info) callInfo.ThisArg
          Args = List.map (resolveInlineExpr com ctx info) callInfo.Args
          GenericArgs = List.map (resolveInlineType ctx) callInfo.GenericArgs }

let resolveInlineExpr (com: IFableCompiler) ctx info expr =
    match expr with
    | Fable.Let(i, v, b) ->
        let i = resolveInlineIdent ctx info i
        let v = resolveInlineExpr com ctx info v
        let ctx = { ctx with Scope = (None, i, Some v)::ctx.Scope }
        Fable.Let(i, v, resolveInlineExpr com ctx info b)

    | Fable.LetRec(bindings, b) ->
        let ctx, bindings =
            ((ctx, bindings), bindings) ||> List.fold(fun (ctx, bindings) (i, e) ->
                let i = resolveInlineIdent ctx info i
                let e = resolveInlineExpr com ctx info e
                { ctx with Scope = (None, i, Some e)::ctx.Scope }, (i, e)::bindings)
        Fable.LetRec(List.rev bindings, resolveInlineExpr com ctx info b)

    | Fable.Call(callee, callInfo, typ, r) ->
        let callInfo = resolveInlinedCallInfo com ctx info callInfo
        Fable.Call(resolveInlineExpr com ctx info callee, callInfo, resolveInlineType ctx typ, r)

    | Fable.Emit(emitInfo, typ, r) ->
        let emitInfo = { emitInfo with CallInfo = resolveInlinedCallInfo com ctx info emitInfo.CallInfo }
        Fable.Emit(emitInfo, resolveInlineType ctx typ, r)

    | Fable.CurriedApply(callee, args, typ, r) ->
        let args = List.map (resolveInlineExpr com ctx info) args
        Fable.CurriedApply(resolveInlineExpr com ctx info callee, args, resolveInlineType ctx typ, r)

    | Fable.Operation(kind, tags, t, r) ->
        let t = resolveInlineType ctx t
        match kind with
        | Fable.Unary(operator, operand) ->
            Fable.Operation(Fable.Unary(operator, resolveInlineExpr com ctx info operand), tags, t, r)
        | Fable.Binary(op, left, right) ->
            Fable.Operation(Fable.Binary(op, resolveInlineExpr com ctx info left, resolveInlineExpr com ctx info right), tags, t, r)
        | Fable.Logical(op, left, right) ->
            Fable.Operation(Fable.Logical(op, resolveInlineExpr com ctx info left, resolveInlineExpr com ctx info right), tags, t, r)

    | Fable.Get(e, kind, t, r) ->
        let kind =
            match kind with
            | Fable.ExprGet e2 -> Fable.ExprGet(resolveInlineExpr com ctx info e2)
            | Fable.ListHead | Fable.ListTail | Fable.OptionValue | Fable.TupleIndex _ | Fable.UnionTag
            | Fable.UnionField _ | Fable.FieldGet _ -> kind
        Fable.Get(resolveInlineExpr com ctx info e, kind, resolveInlineType ctx t, r)

    | Fable.Set(e, kind, t, v, r) ->
        let kind =
            match kind with
            | Fable.ExprSet e2 -> Fable.ExprSet(resolveInlineExpr com ctx info e2)
            | Fable.FieldSet _
            | Fable.ValueSet -> kind
        Fable.Set(resolveInlineExpr com ctx info e, kind, resolveInlineType ctx t, resolveInlineExpr com ctx info v, r)

    | Fable.Test(e, kind, r) ->
        let kind =
            match kind with
            | Fable.TypeTest t -> Fable.TypeTest(resolveInlineType ctx t)
            | Fable.OptionTest _
            | Fable.ListTest _
            | Fable.UnionCaseTest _ -> kind
        Fable.Test(resolveInlineExpr com ctx info e, kind, r)

    | Fable.Sequential exprs ->
        List.map (resolveInlineExpr com ctx info) exprs |> Fable.Sequential

    | Fable.IdentExpr i -> Fable.IdentExpr(resolveInlineIdent ctx info i)

    | Fable.Lambda(arg, b, n) -> Fable.Lambda(resolveInlineIdent ctx info arg, resolveInlineExpr com ctx info b, n)

    | Fable.Delegate(args, b, n, t) -> Fable.Delegate(List.map (resolveInlineIdent ctx info) args, resolveInlineExpr com ctx info b, n, t)

    | Fable.IfThenElse(cond, thenExpr, elseExpr, r) -> Fable.IfThenElse(resolveInlineExpr com ctx info cond, resolveInlineExpr com ctx info thenExpr, resolveInlineExpr com ctx info elseExpr, r)

    | Fable.DecisionTree(e, targets) ->
        let targets = targets |> List.map(fun (idents, e) ->
            List.map (resolveInlineIdent ctx info) idents, resolveInlineExpr com ctx info e)
        Fable.DecisionTree(resolveInlineExpr com ctx info e, targets)

    | Fable.DecisionTreeSuccess(idx, boundValues, t) ->
        let boundValues = List.map (resolveInlineExpr com ctx info) boundValues
        Fable.DecisionTreeSuccess(idx, boundValues, resolveInlineType ctx t)

    | Fable.ForLoop(i, s, l, b, u, r) -> Fable.ForLoop(resolveInlineIdent ctx info i, resolveInlineExpr com ctx info s, resolveInlineExpr com ctx info l, resolveInlineExpr com ctx info b, u, r)

    | Fable.WhileLoop(e1, e2, r) -> Fable.WhileLoop(resolveInlineExpr com ctx info e1, resolveInlineExpr com ctx info e2, r)

    | Fable.TryCatch(b, c, d, r) -> Fable.TryCatch(resolveInlineExpr com ctx info b, (c |> Option.map (fun (i, e) -> resolveInlineIdent ctx info i, resolveInlineExpr com ctx info e)), (d |> Option.map (resolveInlineExpr com ctx info)), r)

    | Fable.TypeCast(e, t) -> Fable.TypeCast(resolveInlineExpr com ctx info e, resolveInlineType ctx t)

    | Fable.ObjectExpr(members, t, baseCall) ->
        let members = members |> List.map (fun m ->
            { m with Args = m.Args |> List.map (resolveInlineIdent ctx info)
                     Body = resolveInlineExpr com ctx info m.Body })
        Fable.ObjectExpr(members, resolveInlineType ctx t, baseCall |> Option.map (resolveInlineExpr com ctx info))

    // TODO: add test
    | Fable.Import(importInfo, t, r) as e ->
        let t = resolveInlineType ctx t
        if Path.isRelativePath importInfo.Path then
            // If it happens we're importing a member in the current file
            // use IdentExpr instead of Import
            let isImportToSameFile =
                Path.Combine(Path.GetDirectoryName(info.FileName), importInfo.Path)
                |> Path.normalizeFullPath
                |> (=) com.CurrentFile
            if isImportToSameFile then
                Fable.IdentExpr { makeTypedIdent t importInfo.Selector with Range = r }
            else
                let path = fixImportedRelativePath com importInfo.Path info.FileName
                Fable.Import({ importInfo with Path = path }, t, r)
        else e

    | Fable.Value(kind, r) as e ->
        match kind with
        | Fable.UnitConstant
        | Fable.BoolConstant _
        | Fable.CharConstant _
        | Fable.StringConstant _
        | Fable.NumberConstant _
        | Fable.RegexConstant _ -> e
        | Fable.StringTemplate(tag, parts, exprs) -> Fable.StringTemplate(tag, parts, List.map (resolveInlineExpr com ctx info) exprs) |> makeValue r
        | Fable.NewOption(e, t, isStruct) -> Fable.NewOption(Option.map (resolveInlineExpr com ctx info) e, resolveInlineType ctx t, isStruct) |> makeValue r
        | Fable.NewTuple(exprs, isStruct) -> Fable.NewTuple(List.map (resolveInlineExpr com ctx info) exprs, isStruct) |> makeValue r
        | Fable.NewArray(Fable.ArrayValues exprs, t, i) -> Fable.NewArray(List.map (resolveInlineExpr com ctx info) exprs |> Fable.ArrayValues, resolveInlineType ctx t, i) |> makeValue r
        | Fable.NewArray(Fable.ArrayFrom expr, t, i) -> Fable.NewArray(resolveInlineExpr com ctx info expr |> Fable.ArrayFrom, resolveInlineType ctx t, i) |> makeValue r
        | Fable.NewArray(Fable.ArrayAlloc expr, t, i) -> Fable.NewArray(resolveInlineExpr com ctx info expr |> Fable.ArrayAlloc, resolveInlineType ctx t, i) |> makeValue r
        | Fable.NewList(ht, t) ->
            let ht = ht |> Option.map (fun (h,t) -> resolveInlineExpr com ctx info h, resolveInlineExpr com ctx info t)
            Fable.NewList(ht, resolveInlineType ctx t) |> makeValue r
        | Fable.NewRecord(exprs, ent, genArgs) ->
            let genArgs = List.map (resolveInlineType ctx) genArgs
            Fable.NewRecord(List.map (resolveInlineExpr com ctx info) exprs, ent, genArgs) |> makeValue r
        | Fable.NewAnonymousRecord(exprs, fields, genArgs, isStruct) ->
            let genArgs = List.map (resolveInlineType ctx) genArgs
            Fable.NewAnonymousRecord(List.map (resolveInlineExpr com ctx info) exprs, fields, genArgs, isStruct) |> makeValue r
        | Fable.NewUnion(exprs, uci, ent, genArgs) ->
            let genArgs = List.map (resolveInlineType ctx) genArgs
            Fable.NewUnion(List.map (resolveInlineExpr com ctx info) exprs, uci, ent, genArgs) |> makeValue r
        | Fable.ThisValue t -> Fable.ThisValue(resolveInlineType ctx t) |> makeValue r
        | Fable.Null t -> Fable.Null(resolveInlineType ctx t) |> makeValue r
        | Fable.BaseValue(i, t) -> Fable.BaseValue(Option.map (resolveInlineIdent ctx info) i, resolveInlineType ctx t) |> makeValue r
        | Fable.TypeInfo(t, d) -> Fable.TypeInfo(resolveInlineType ctx t, d) |> makeValue r

    | Fable.Extended(kind, r) as e ->
        match kind with
        | Fable.Curry(e, arity) ->
            Fable.Extended(Fable.Curry(resolveInlineExpr com ctx info e, arity), r)
        | Fable.Throw(e, t) ->
            Fable.Extended(Fable.Throw(Option.map (resolveInlineExpr com ctx info) e, resolveInlineType ctx t), r)
        | Fable.Debugger -> e

    | Fable.Unresolved(e, t, r) ->
        match e with
        | Fable.UnresolvedTraitCall(sourceTypes, traitName, isInstance, argTypes, argExprs) ->
            let t = resolveInlineType ctx t
            let argTypes = argTypes |> List.map (resolveInlineType ctx)
            let argExprs = argExprs |> List.map (resolveInlineExpr com ctx info)

            match tryFindWitness ctx argTypes isInstance traitName with
            | None ->
               let sourceTypes = sourceTypes |> List.map (resolveInlineType ctx)
               transformTraitCall com ctx r t sourceTypes traitName isInstance argTypes argExprs
            | Some w ->
                // As witnesses come from the context, idents may be duplicated, see #2855
                let info = { info with ResolvedIdents = Dictionary(); FileName = w.FileName }
                let callee = resolveInlineExpr com ctx { info with FileName = w.FileName } w.Expr
                let callInfo = makeCallInfo None argExprs argTypes
                makeCall r t callInfo callee

        | Fable.UnresolvedInlineCall(membUniqueName, witnesses, callee, callInfo) ->
            let t = resolveInlineType ctx t
            let callee = callee |> Option.map (resolveInlineExpr com ctx info)
            let callInfo = resolveInlinedCallInfo com ctx info callInfo
            let ctx = { ctx with Witnesses = witnesses @ ctx.Witnesses }
            inlineExpr com ctx r t callee callInfo membUniqueName

        | Fable.UnresolvedReplaceCall(thisArg, args, callInfo, attachedCall) ->
            let typ = resolveInlineType ctx t
            let thisArg = thisArg |> Option.map (resolveInlineExpr com ctx info)
            let args = args |> List.map (resolveInlineExpr com ctx info)
            let callInfo = { callInfo with GenericArgs = callInfo.GenericArgs |> List.map (resolveInlineType ctx) }
            match com.TryReplace(ctx, r, typ, callInfo, thisArg, args) with
            | Some e -> e
            | None when callInfo.IsInterface ->
                match attachedCall with
                | Some e -> resolveInlineExpr com ctx info e
                | None ->
                    "Unexpected, missing attached call in unresolved replace call"
                    |> addErrorAndReturnNull com ctx.InlinePath r
            | None -> failReplace com ctx r callInfo thisArg

type private AttachedMembers = {|
    NonMangledNames: HashSet<string>
    Members: ResizeArray<Fable.MemberDecl>
    Cons: Fable.MemberDecl option
    BaseCall: Fable.Expr option
|}

type FableCompiler(com: Compiler) =
    let attachedMembers = Dictionary<string, AttachedMembers>()
    let onlyOnceWarnings = HashSet<string>()

    member _.ReplaceAttachedMembers(entityFullName, f) =
        if attachedMembers.ContainsKey(entityFullName) then
            attachedMembers[entityFullName] <- f attachedMembers[entityFullName]
        else
            let members = {|
                NonMangledNames = HashSet()
                Members = ResizeArray()
                Cons = None
                BaseCall = None
            |}
            attachedMembers.Add(entityFullName, f members)

    member _.TryGetAttachedMembers(entityFullName) =
        match attachedMembers.TryGetValue(entityFullName) with
        | true, members -> Some members
        | false, _ -> None

    member this.AddConstructor(entityFullName, cons: Fable.MemberDecl, baseCall: Fable.Expr option) =
        this.ReplaceAttachedMembers(entityFullName, fun members ->
            {| members with Cons = Some cons
                            BaseCall = baseCall |})

    member this.AddAttachedMember(entityFullName, isMangled, memb: Fable.MemberDecl) =
        this.ReplaceAttachedMembers(entityFullName, fun members ->
            if not isMangled then
                members.NonMangledNames.Add(memb.Name) |> ignore
            members.Members.Add(memb)
            members)

    member this.NonMangledAttachedMemberConflicts entityFullName memberName =
        this.TryGetAttachedMembers(entityFullName)
        |> Option.map (fun members -> members.NonMangledNames.Contains(memberName))
        |> Option.defaultValue false

    member this.TryReplace(ctx, r, t, info, thisArg, args) =
        Replacements.Api.tryCall this ctx r t info thisArg args

    member this.ResolveInlineExpr(ctx: Context, inExpr: InlineExpr, args: Fable.Expr list) =
        let rec foldArgs acc = function
            | argIdent::restArgIdents, argExpr::restArgExprs ->
                foldArgs ((argIdent, argExpr)::acc) (restArgIdents, restArgExprs)
            | (argIdent: Fable.Ident)::restArgIdents, [] ->
                foldArgs ((argIdent, Fable.Value(Fable.NewOption(None, argIdent.Type, false), None))::acc) (restArgIdents, [])
            | [], _ -> List.rev acc

        let info: InlineExprInfo =
            { FileName = inExpr.FileName
              ScopeIdents = inExpr.ScopeIdents
              ResolvedIdents = Dictionary() }

        let ctx, bindings =
            ((ctx, []), foldArgs [] (inExpr.Args, args)) ||> List.fold (fun (ctx, bindings) (argId, arg) ->
                let argId = resolveInlineIdent ctx info argId
                // Change type and mark argId as compiler-generated so Fable also
                // tries to inline it in DEBUG mode (some patterns depend on this)
                let argId = { argId with Type = arg.Type; IsCompilerGenerated = true }
                let ctx = { ctx with Scope = (None, argId, Some arg)::ctx.Scope }
                ctx, (argId, arg)::bindings)

        let ctx =
            { ctx with CapturedBindings =
                       if isNull ctx.CapturedBindings then HashSet()
                       else ctx.CapturedBindings }

        let resolved = resolveInlineExpr this ctx info inExpr.Body

        // Some patterns depend on inlined arguments being captured by "magic" Fable.Core functions like
        // importValueDynamic. If the value can have side effects, it won't be removed by beta binding
        // reduction, so we try to eliminate it here.
        bindings |> List.filter (fun (i, v) ->
            if ctx.CapturedBindings.Contains(i.Name) && canHaveSideEffects v then
                if isIdentUsed i.Name resolved then
                    $"Inlined argument {i.Name} is being captured but is also used somewhere else. " +
                    "There's a risk of double evaluation."
                    |> addWarning com [] i.Range
                    true
                else false
            else true), resolved

    interface IFableCompiler with
        member _.WarnOnlyOnce(msg, ?range) =
            if onlyOnceWarnings.Add(msg) then
                addWarning com [] range msg

        member this.Transform(ctx, fsExpr) =
            transformExpr this ctx fsExpr |> run

        member this.TryReplace(ctx, r, t, info, thisArg, args) =
            this.TryReplace(ctx, r, t, info, thisArg, args)

        member this.ResolveInlineExpr(ctx, inExpr, args) =
            this.ResolveInlineExpr(ctx, inExpr, args)

    interface Compiler with
        member _.CurrentFile = com.CurrentFile
        member _.LibraryDir = com.LibraryDir
        member _.Options = com.Options
        member _.Plugins = com.Plugins
        member _.OutputDir = com.OutputDir
        member _.OutputType = com.OutputType
        member _.ProjectFile = com.ProjectFile
        member _.SourceFiles = com.SourceFiles
        member _.IsPrecompilingInlineFunction = com.IsPrecompilingInlineFunction
        member _.WillPrecompileInlineFunction(file) = com.WillPrecompileInlineFunction(file)
        member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
        member _.GetRootModule(fileName) = com.GetRootModule(fileName)
        member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
        member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)
        member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
        member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)


let rec attachClassMembers (com: FableCompiler) = function
    | Fable.ModuleDeclaration decl ->
        { decl with Members = decl.Members |> List.map (attachClassMembers com) }
        |> Fable.ModuleDeclaration
    | Fable.ClassDeclaration decl as classDecl ->
        com.TryGetAttachedMembers(decl.Entity.FullName)
        |> Option.map (fun members ->
            { decl with Constructor = members.Cons
                        BaseCall = members.BaseCall
                        AttachedMembers = members.Members.ToArray() |> List.ofArray }
            |> Fable.ClassDeclaration)
        |> Option.defaultValue classDecl
    | decl -> decl

let getInlineExprs fileName (declarations: FSharpImplementationFileDeclaration list) =

    let rec getInlineExprsInner decls =
        decls |> List.collect (function
            | FSharpImplementationFileDeclaration.Entity(_, decls) -> getInlineExprsInner decls
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (memb, argIds, body) when isInline memb ->
                [
                    getMemberUniqueName memb,
                    InlineExprLazy(fun com ->
                        let com =
                            com.WillPrecompileInlineFunction(fileName)
                            |> FableCompiler
                            :> IFableCompiler

                        let ctx = { Context.Create() with
                                        PrecompilingInlineFunction = Some memb
                                        UsedNamesInDeclarationScope = HashSet() }

                        let ctx, idents =
                            ((ctx, []), List.concat argIds) ||> List.fold (fun (ctx, idents) argId ->
                                let ctx, ident = putIdentInScope com ctx argId None
                                ctx, ident::idents)

                        // It looks as we don't need memb.DeclaringEntity.GenericParameters here
                        let genArgs = memb.GenericParameters |> Seq.mapToList (genParamName)

                        { Args = List.rev idents
                          Body = com.Transform(ctx, body)
                          FileName = fileName
                          GenericArgs = genArgs
                          ScopeIdents = set ctx.UsedNamesInDeclarationScope })
                ]
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue _
            | FSharpImplementationFileDeclaration.InitAction _ -> []
        )
    getInlineExprsInner declarations

let transformFile (com: Compiler) =
    let declarations = com.GetImplementationFile(com.CurrentFile)
    let usedRootNames = getUsedRootNames com Set.empty declarations
    let ctx = Context.Create(usedRootNames)
    let com = FableCompiler(com)
    let rootDecls =
        transformDeclarations com ctx declarations
        |> List.map (attachClassMembers com)
    Fable.File(rootDecls, usedRootNames)
