module rec Fable.Transforms.FSharp2Fable.Compiler

open System.Collections.Generic
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax

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
    let baseEnt = FsEnt baseEnt
    let argTypes = lazy getArgTypes com baseCons
    let baseArgs = transformExprList com ctx baseArgs |> run
    let genArgs = genArgs |> Seq.map (makeType ctx.GenericArgs)
    match Replacements.tryBaseConstructor com ctx baseEnt argTypes genArgs baseArgs with
    | Some(baseRef, args) ->
        let callInfo = Fable.CallInfo.Make(args=args, sigArgTypes=getArgTypes com baseCons)
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
                match tryGlobalOrImportedEntity com baseEnt with
                | Some baseExpr -> baseExpr
                | None -> entityRef com baseEnt
            Fable.Call(baseExpr, info, t, r)
        // Other cases, like Emit will call directly the base expression
        | e -> e

let private transformNewUnion com ctx r fsType (unionCase: FSharpUnionCase) (argExprs: Fable.Expr list) =
    match fsType, unionCase with
    // TODO: Erased unions should be represented in Fable AST,
    // not erased already to tuples/strings
    | ErasedUnionCase ->
        makeTuple r argExprs
    | ErasedUnion(tdef, _genArgs, rule) ->
        match argExprs with
        | [] -> transformStringEnum rule unionCase
        | [argExpr] -> argExpr
        | _ when tdef.UnionCases.Count > 1 ->
            "Erased unions with multiple cases must have one single field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com ctx.InlinePath r
        | argExprs -> makeTuple r argExprs
    | StringEnum(tdef, rule) ->
        match argExprs with
        | [] -> transformStringEnum rule unionCase
        | _ -> sprintf "StringEnum types cannot have fields: %O" tdef.TryFullName
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
        let genArgs = makeGenArgs ctx.GenericArgs genArgs
        let tag = unionCaseTag com tdef unionCase
        Fable.NewUnion(argExprs, tag, FsEnt.Ref tdef, genArgs) |> makeValue r

let private transformTraitCall com (ctx: Context) r typ (sourceTypes: Fable.Type list) traitName (flags: SynMemberFlags) (argTypes: Fable.Type list) (argExprs: Fable.Expr list) =
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
          GenericArgs =
            // TODO: Check the source F# entity to get the actual gen param names?
            match genArgs with
            | [] -> []
            | [genArg] -> ["T", genArg]
            | genArgs -> genArgs |> List.mapi (fun i genArg -> "T" + string i, genArg)
        }

    let resolveMemberCall (entity: Fable.Entity) genArgs membCompiledName isInstance argTypes thisArg args =
        let genParamNames = entity.GenericParameters |> List.map (fun x -> x.Name)
        let genArgs = List.zip genParamNames genArgs
        tryFindMember com entity (Map genArgs) membCompiledName isInstance argTypes
        |> Option.map (fun memb -> makeCallFrom com ctx r typ [] thisArg args memb)

    let isInstance = flags.IsInstance
    let thisArg, args, argTypes =
        match argExprs, argTypes with
        | thisArg::args, _::argTypes when isInstance -> Some thisArg, args, argTypes
        | args, argTypes -> None, args, argTypes

    sourceTypes |> Seq.tryPick (fun t ->
        match Replacements.tryType t with
        | Some(entityFullName, makeCall, genArgs) ->
            let info = makeCallInfo traitName entityFullName argTypes genArgs
            makeCall com ctx r typ info thisArg args
        | None ->
            match t with
            | Fable.DeclaredType(entity, genArgs) ->
                let entity = com.GetEntity(entity)
                // SRTP only works for records if there are no arguments
                if isInstance && entity.IsFSharpRecord && List.isEmpty args && Option.isSome thisArg then
                    let fieldName = Naming.removeGetSetPrefix traitName
                    entity.FSharpFields |> Seq.tryPick (fun fi ->
                        if fi.Name = fieldName then
                            Fable.Get(thisArg.Value, Fable.FieldGet(fi.Name, fi.IsMutable), typ, r) |> Some
                        else None)
                    |> Option.orElseWith (fun () ->
                        resolveMemberCall entity genArgs traitName isInstance argTypes thisArg args)
                else resolveMemberCall entity genArgs traitName isInstance argTypes thisArg args
            | Fable.AnonymousRecordType(sortedFieldNames, genArgs)
                    when isInstance && List.isEmpty args && Option.isSome thisArg ->
                let fieldName = Naming.removeGetSetPrefix traitName
                Seq.zip sortedFieldNames genArgs
                |> Seq.tryPick (fun (fi, fiType) ->
                    if fi = fieldName then
                        Fable.Get(thisArg.Value, Fable.FieldGet(fi, false), typ, r) |> Some
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
        | None -> entityRef com (FsEnt calleeType.TypeDefinition)
    return callee
  }

let private resolveImportMemberBinding (ident: Fable.Ident) (info: Fable.ImportInfo) =
    if info.Selector = Naming.placeholder then { info with Selector = ident.Name }
    else info

let private getAttachedMemberInfo com ctx r nonMangledNameConflicts
                (declaringEntity: Fable.Entity option) (sign: FSharpAbstractSignature) attributes =
    let declaringEntityFields = HashSet<_>()
    let declaringEntityName =
        match declaringEntity with
        | Some x ->
            x.FSharpFields |> List.iter (fun x -> declaringEntityFields.Add(x.Name) |> ignore)
            x.FullName
        | None   -> ""

    let isGetter = sign.Name.StartsWith("get_")
    let isSetter = not isGetter && sign.Name.StartsWith("set_")
    let indexedProp = (isGetter && countNonCurriedParamsForSignature sign > 0)
                        || (isSetter && countNonCurriedParamsForSignature sign > 1)
    let name, isMangled, isGetter, isSetter, isEnumerator, hasSpread =
        // Don't use the type from the arguments as the override may come
        // from another type, like ToString()
        match tryDefinition sign.DeclaringType with
        | Some(ent, fullName) ->
            let isEnumerator =
                sign.Name = "GetEnumerator"
                && fullName = Some "System.Collections.Generic.IEnumerable`1"
            let hasSpread =
                if isGetter || isSetter then false
                else
                    // FSharpObjectExprOverride.CurriedParameterGroups doesn't offer
                    // information about ParamArray, we need to check the source method.
                    ent.TryGetMembersFunctionsAndValues()
                    |> Seq.tryFind (fun x -> x.CompiledName = sign.Name)
                    |> function Some m -> hasParamArray m | None -> false
            let isMangled = isMangledAbstractEntity com ent
            let name, isGetter, isSetter =
                if isMangled then
                    let overloadHash =
                        if (isGetter || isSetter) && not indexedProp then ""
                        else
                            sign.AbstractArguments
                            |> Seq.mapToList (Seq.mapToList (fun x -> FsParam x :> Fable.Parameter))
                            |> OverloadSuffix.getHashFromCurriedParamGroups (FsEnt ent)
                    getMangledAbstractMemberName ent sign.Name overloadHash, false, false
                else
                    let name, isGetter, isSetter =
                        // For indexed properties, keep the get_/set_ prefix and compile as method
                        if indexedProp then sign.Name, false, false
                        else Naming.removeGetSetPrefix sign.Name, isGetter, isSetter
                    // Setters can have same name as getters, assume there will always be a getter
                    if not isSetter &&
                        (nonMangledNameConflicts declaringEntityName name || declaringEntityFields.Contains(name)) then
                        sprintf "Member %s is duplicated, use Mangle attribute to prevent conflicts with interfaces" name
                        // TODO: Temporarily emitting a warning, because this errors in old libraries,
                        // like Fable.React.HookBindings
                        |> addWarning com ctx.InlinePath r
                    name, isGetter, isSetter
            name, isMangled, isGetter, isSetter, isEnumerator, hasSpread
        | None ->
            Naming.removeGetSetPrefix sign.Name, false, isGetter, isSetter, false, false
    name, MemberInfo(attributes=attributes,
                         hasSpread=hasSpread,
                         isGetter=isGetter,
                         isSetter=isSetter,
                         isEnumerator=isEnumerator,
                         isMangled=isMangled)

let private transformObjExpr (com: IFableCompiler) (ctx: Context) (objType: FSharpType)
                    baseCallExpr (overrides: FSharpObjectExprOverride list) otherOverrides =

    let nonMangledMemberNames = HashSet()
    let nonMangledNameConflicts _ name =
        nonMangledMemberNames.Add(name) |> not

    let mapOverride (over: FSharpObjectExprOverride): Thunk<Fable.MemberDecl> =
      trampoline {
        let ctx, args = bindMemberArgs com ctx over.CurriedParameterGroups
        let! body = transformExpr com ctx over.Body
        let name, info = getAttachedMemberInfo com ctx body.Range nonMangledNameConflicts None over.Signature []
        return { Name = name
                 FullDisplayName =
                     match tryDefinition over.Signature.DeclaringType with
                     | Some(_, Some entFullName) -> entFullName + "." + over.Signature.Name
                     | _ -> over.Signature.Name
                 Args = args
                 Body = body
                 // UsedNames are not used for obj expr members
                 UsedNames = Set.empty
                 Info = info
                 ExportDefault = false }
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
    | Fable.DelegateType(argTypes, _) ->
        let arity = List.length argTypes |> max 1
        if arity > 1 then
            match expr with
            | LambdaUncurriedAtCompileTime (Some arity) lambda -> return lambda
            | _ -> return Replacements.uncurryExprAtRuntime com arity expr
        else
            return expr
    | _ -> return expr
  }

let private transformUnionCaseTest (com: IFableCompiler) (ctx: Context) r
                            unionExpr fsType (unionCase: FSharpUnionCase) =
  trampoline {
    let! unionExpr = transformExpr com ctx unionExpr
    match fsType, unionCase with
    | ErasedUnionCase ->
        return "Cannot test erased union cases"
        |> addErrorAndReturnNull com ctx.InlinePath r
    | ErasedUnion(tdef, genArgs, rule) ->
        match unionCase.Fields.Count with
        | 0 -> return makeEqOp r unionExpr (transformStringEnum rule unionCase) BinaryEqualStrict
        | 1 ->
            let fi = unionCase.Fields.[0]
            let typ =
                if fi.FieldType.IsGenericParameter then
                    let name = genParamName fi.FieldType.GenericParameter
                    let index =
                        tdef.GenericParameters
                        |> Seq.findIndex (fun arg -> arg.Name = name)
                    genArgs.[index]
                else fi.FieldType
            let kind = makeType ctx.GenericArgs typ |> Fable.TypeTest
            return Fable.Test(unionExpr, kind, r)
        | _ ->
            return "Erased unions with multiple cases cannot have more than one field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com ctx.InlinePath r
    | OptionUnion _ ->
        let kind = Fable.OptionTest(unionCase.Name <> "None" && unionCase.Name <> "ValueNone")
        return Fable.Test(unionExpr, kind, r)
    | ListUnion _ ->
        let kind = Fable.ListTest(unionCase.CompiledName <> "Empty")
        return Fable.Test(unionExpr, kind, r)
    | StringEnum(_, rule) ->
        return makeEqOp r unionExpr (transformStringEnum rule unionCase) BinaryEqualStrict
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
                    let ctx, ident = putArgInScope com ctx ident
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
        let membOpt = tryFindMember com entity ctx.GenericArgs opName false argTypes
        return (match membOpt with
                | Some memb -> makeCallFrom com ctx r typ argTypes None args memb
                | None -> failwithf "Cannot find member %s.%s" (entity.FullName) opName)

    | FSharpExprPatterns.Coerce(targetType, inpExpr) ->
        let! (inpExpr: Fable.Expr) = transformExpr com ctx inpExpr
        let t = makeType ctx.GenericArgs targetType
        match tryDefinition targetType with
        | Some(_, Some fullName) ->
            match fullName with
            | Types.ienumerableGeneric | Types.ienumerable -> return Replacements.toSeq t inpExpr
            | _ -> return Fable.TypeCast(inpExpr, t)
        | _ -> return Fable.TypeCast(inpExpr, t)

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in FSharpExprPatterns.Let (see below)
    | FSharpExprPatterns.TypeLambda (_genArgs, lambda) ->
        let! lambda = transformExpr com ctx lambda
        return lambda

    | FSharpExprPatterns.FastIntegerForLoop(start, limit, body, isUp) ->
        let r = makeRangeFrom fsExpr
        match body with
        | FSharpExprPatterns.Lambda (PutArgInScope com ctx (newContext, ident), body) ->
            let! start = transformExpr com ctx start
            let! limit = transformExpr com ctx limit
            let! body = transformExpr com newContext body
            return makeForLoop r isUp ident start limit body
        | _ -> return failwithf "Unexpected loop %O: %A" r fsExpr

    | FSharpExprPatterns.WhileLoop(guardExpr, bodyExpr) ->
        let! guardExpr = transformExpr com ctx guardExpr
        let! bodyExpr = transformExpr com ctx bodyExpr
        return (guardExpr, bodyExpr) ||> makeWhileLoop (makeRangeFrom fsExpr)

    | FSharpExprPatterns.Const(value, typ) ->
        let typ = makeType ctx.GenericArgs typ
        return Replacements.makeTypeConst com (makeRangeFrom fsExpr) typ value

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
            | Some (_,fsExpr) ->
                return! transformExpr com ctx fsExpr
            | None ->
                return "Cannot resolve locally inlined value: " + var.DisplayName
                |> addErrorAndReturnNull com ctx.InlinePath r
        else
            if isByRefValue var then
                // Getting byref value is compiled as FSharpRef op_Dereference
                let var = makeValueFrom com ctx r var
                return Replacements.getReference r var.Type var
            else
                return makeValueFrom com ctx r var

    | FSharpExprPatterns.DefaultValue (FableType com ctx typ) ->
        return Replacements.defaultof com ctx typ

    | FSharpExprPatterns.Let((var, value), body) ->
        match value, body with
        | (CreateEvent(value, event) as createEvent), _ ->
            let! value = transformExpr com ctx value
            let typ = makeType ctx.GenericArgs createEvent.Type
            let value = makeCallFrom com ctx (makeRangeFrom createEvent) typ Seq.empty (Some value) [] event
            let ctx, ident = putBindingInScope com ctx var value
            let! body = transformExpr com ctx body
            return Fable.Let(ident, value, body)

        | value, body ->
            if isInline var then
                let ctx = { ctx with ScopeInlineValues = (var, value)::ctx.ScopeInlineValues }
                return! transformExpr com ctx body
            else
                let! value = transformExpr com ctx value
                let ctx, ident = putBindingInScope com ctx var value
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
            ||> List.foldBack (fun (PutArgInScope com ctx (newContext, ident), _) (ctx, idents) ->
                (newContext, ident::idents))
        let _, bindingExprs = List.unzip recBindings
        let! exprs = transformExprList com ctx bindingExprs
        let bindings = List.zip idents exprs
        let! body = transformExpr com ctx body
        match bindings with
        // If there's only one binding compile as Let to play better with optimizations
        | [ident, value] -> return Fable.Let(ident, value, body)
        | bindings -> return Fable.LetRec(bindings, body)

    // `argTypes2` is always empty
    | FSharpExprPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, argTypes2, argExprs) ->
        let r = makeRangeFrom fsExpr
        let typ = makeType ctx.GenericArgs fsExpr.Type
        let! argExprs = transformExprList com ctx argExprs
        let argTypes = List.map (makeType ctx.GenericArgs) argTypes

        let witness = ctx.Witnesses |> List.tryFind (fun w ->
            w.TraitName = traitName
            && w.IsInstance = flags.IsInstance
            && listEquals (typeEquals false) argTypes w.ArgTypes)

        match witness with
        | None ->
            let sourceTypes = List.map (makeType ctx.GenericArgs) sourceTypes
            return transformTraitCall com ctx r typ sourceTypes traitName flags argTypes argExprs
        | Some w ->
            let callInfo = makeCallInfo None argExprs w.ArgTypes
            return makeCall r typ callInfo w.Expr

    | FSharpExprPatterns.CallWithWitnesses(callee, memb, ownerGenArgs, membGenArgs, witnesses, args) ->
        match callee with
        | Some(CreateEvent(callee, event) as createEvent) ->
            let! callee = transformExpr com ctx callee
            let typ = makeType ctx.GenericArgs createEvent.Type
            let callee = makeCallFrom com ctx (makeRangeFrom createEvent) typ Seq.empty (Some callee) [] event
            let! args = transformExprList com ctx args
            let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
            let typ = makeType ctx.GenericArgs fsExpr.Type
            return makeCallFrom com ctx (makeRangeFrom fsExpr) typ genArgs (Some callee) args memb

        | callee ->
            let r = makeRangeFrom fsExpr
            let! callee = transformExprOpt com ctx callee
            let! args = transformExprList com ctx args
            let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
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
                                    let fieldName = calleeType.AnonRecordTypeDetails.SortedFieldNames.[fieldIndex]
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
                        let w = {
                            Witness.TraitName = traitName
                            IsInstance = isInstance
                            Expr = Fable.Delegate(args, body, None)
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
                let genArgs = Seq.map (makeType ctx.GenericArgs) genArgs
                let resolvedCtx = { ctx with GenericArgs = matchGenericParamsFrom var genArgs |> Map }
                let! callee = transformExpr com resolvedCtx fsExpr
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
        | FSharpExprPatterns.Let((_, FSharpExprPatterns.Call(None,m,_,_,[e1; e2])),_), args
                                when m.FullName = "Fable.Core.JsInterop.( ? )" ->
            let! e1 = transformExpr com ctx e1
            let! e2 = transformExpr com ctx e2
            let e = Fable.Get(e1, Fable.ExprGet e2, Fable.Any, e1.Range)
            let! args = transformExprList com ctx args
            let args = destructureTupleArgs args
            let typ = makeType ctx.GenericArgs fsExpr.Type
            let r = makeRangeFrom fsExpr
            // Convert this to emit so auto-uncurrying is applied
            return emitJsExpr r typ (e::args) "$0($1...)"

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
            | RaisingMatchFailureExpr _fileNameWhereErrorOccurs ->
                let errorMessage = "Match failure"
                let rangeOfElseExpr = makeRangeFrom elseExpr
                let errorExpr = Replacements.Helpers.error (Fable.Value(Fable.StringConstant errorMessage, None))
                makeThrow rangeOfElseExpr Fable.Any errorExpr
            | _ ->
                fableElseExpr

        return Fable.IfThenElse(guardExpr, thenExpr, altElseExpr, makeRangeFrom fsExpr)

    | FSharpExprPatterns.TryFinally (body, finalBody) ->
        let r = makeRangeFrom fsExpr
        match body with
        | FSharpExprPatterns.TryWith(body, _, _, catchVar, catchBody) ->
            return makeTryCatch com ctx r body (Some (catchVar, catchBody)) (Some finalBody)
        | _ -> return makeTryCatch com ctx r body None (Some finalBody)

    | FSharpExprPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        return makeTryCatch com ctx (makeRangeFrom fsExpr) body (Some (catchVar, catchBody)) None

    | FSharpExprPatterns.NewDelegate(delegateType, fsExpr) ->
        return! transformDelegate com ctx delegateType fsExpr

    | FSharpExprPatterns.Lambda(arg, body) ->
        let ctx, args = makeFunctionArgs com ctx [arg]
        match args with
        | [arg] ->
            let! body = transformExpr com ctx body
            return Fable.Lambda(arg, body, None)
        | _ -> return failwith "makeFunctionArgs returns args with different length"

    // Getters and Setters
    | FSharpExprPatterns.AnonRecordGet(callee, calleeType, fieldIndex) ->
        let r = makeRangeFrom fsExpr
        let! callee = transformExpr com ctx callee
        let fieldName = calleeType.AnonRecordTypeDetails.SortedFieldNames.[fieldIndex]
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return Fable.Get(callee, Fable.FieldGet(fieldName, false), typ, r)

    | FSharpExprPatterns.FSharpFieldGet(callee, calleeType, field) ->
        let r = makeRangeFrom fsExpr
        let! callee = transformCallee com ctx callee calleeType
        // let typ = makeType ctx.GenericArgs fsExpr.Type
        let typ = makeType Map.empty field.FieldType
        return Fable.Get(callee, Fable.FieldGet(FsField.FSharpFieldName field, field.IsMutable), typ, r)

    | FSharpExprPatterns.TupleGet(tupleType, tupleElemIndex, IgnoreAddressOf tupleExpr) ->
        let! tupleExpr = transformExpr com ctx tupleExpr
        let typ = makeType ctx.GenericArgs fsExpr.Type // doesn't always work (could be Fable.Any)
        let typ2 = makeType ctx.GenericArgs tupleType
        let typ =
            // if type is Fable.Any, get the actual type from the tuple element
            match typ, typ2 with
            | Fable.Any, Fable.Tuple(genArgs,_) -> List.item tupleElemIndex genArgs
            | _ -> typ
        return Fable.Get(tupleExpr, Fable.TupleIndex tupleElemIndex, typ, makeRangeFrom fsExpr)

    | FSharpExprPatterns.UnionCaseGet (IgnoreAddressOf unionExpr, fsType, unionCase, field) ->
        let r = makeRangeFrom fsExpr
        let! unionExpr = transformExpr com ctx unionExpr
        match fsType, unionCase with
        | ErasedUnionCase ->
            let index = unionCase.Fields |> Seq.findIndex (fun x -> x.Name = field.Name)
            return Fable.Get(unionExpr, Fable.TupleIndex(index), makeType ctx.GenericArgs fsType, r)
        | ErasedUnion _ ->
            if unionCase.Fields.Count = 1 then return unionExpr
            else
                let index = unionCase.Fields |> Seq.findIndex (fun x -> x.Name = field.Name)
                return Fable.Get(unionExpr, Fable.TupleIndex index, makeType ctx.GenericArgs fsType, r)
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
        | DiscriminatedUnion(tdef, _) ->
            let caseIndex = unionCaseTag com tdef unionCase
            let fieldIndex = unionCase.Fields |> Seq.findIndex (fun fi -> fi.Name = field.Name)
            let kind = Fable.UnionField(caseIndex, fieldIndex)
            let typ = makeType Map.empty field.FieldType
            // let typ = makeType ctx.GenericArgs fsExpr.Type // doesn't work (Fable.Any)
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
        | Some ent when ent.IsFSharpModule && isPublicMember valToSet ->
            // Mutable and public module values are compiled as functions, because
            // values imported from ES2015 modules cannot be modified (see #986)
            let valToSet = makeValueFrom com ctx r valToSet
            let args = [valueExpr; makeBoolConst true]
            let info = makeCallInfo None args [valToSet.Type; Fable.Boolean]
            return makeCall r Fable.Unit info valToSet
        | _ ->
            let valToSet = makeValueFrom com ctx r valToSet
            return Fable.Set(valToSet, Fable.ValueSet, valueExpr.Type, valueExpr, r)

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
        let genArgs = Seq.map (makeType ctx.GenericArgs) genArgs
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
        let genArgs = makeGenArgs ctx.GenericArgs (getGenericArguments fsType)
        return Fable.NewRecord(argExprs, FsEnt.Ref fsType.TypeDefinition, genArgs) |> makeValue r

    | FSharpExprPatterns.NewAnonRecord(fsType, argExprs) ->
        let r = makeRangeFrom fsExpr
        let! argExprs = transformExprList com ctx argExprs
        let fieldNames = fsType.AnonRecordTypeDetails.SortedFieldNames
        let genArgs = makeGenArgs ctx.GenericArgs (getGenericArguments fsType)
        return Fable.NewAnonymousRecord(argExprs, fieldNames, genArgs) |> makeValue r

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
            | RaisingMatchFailureExpr _fileNameWhereErrorOccurs ->
                match decisionExpr with
                | FSharpExprPatterns.IfThenElse(FSharpExprPatterns.UnionCaseTest(_unionValue, unionType, _unionCaseInfo), _, _) ->
                    let rangeOfLastDecisionTarget = makeRangeFrom (snd (List.last decisionTargets))
                    let errorMessage = "Match failure: " + unionType.TypeDefinition.FullName
                    let errorExpr = Replacements.Helpers.error (Fable.Value(Fable.StringConstant errorMessage, None))
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
        match Replacements.tryField com typ ownerTyp fieldName with
        | Some expr -> return expr
        | None ->
            return sprintf "Cannot compile ILFieldGet(%A, %s)" ownerTyp fieldName
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
            match memb.DeclaringEntity with
            | Some ent when ent.IsFSharpModule && isPublicMember memb ->
                return Replacements.makeRefFromMutableFunc com ctx r value.Type value
            | _ ->
                return Replacements.makeRefFromMutableValue com ctx r value.Type value
        // This matches passing fields by reference
        | FSharpExprPatterns.FSharpFieldGet(callee, calleeType, field) ->
            let r = makeRangeFrom fsExpr
            let! callee = transformCallee com ctx callee calleeType
            let typ = makeType ctx.GenericArgs expr.Type
            let key = FsField.FSharpFieldName field
            return Replacements.makeRefFromMutableField com ctx r typ callee key
        | _ ->
            // ignore AddressOf, pass by value
            return! transformExpr com ctx expr

    | FSharpExprPatterns.AddressSet expr ->
        let r = makeRangeFrom fsExpr
        match expr with
        | FSharpExprPatterns.Value valToSet, valueExpr
                when isByRefValue valToSet ->
            // Setting byref value is compiled as FSharpRef op_ColonEquals
            let! value = transformExpr com ctx valueExpr
            let valToSet = makeValueFrom com ctx r valToSet
            return Replacements.setReference r valToSet value
        | _ ->
            return "Mutating this argument passed by reference is not supported"
            |> addErrorAndReturnNull com ctx.InlinePath r

    // | FSharpExprPatterns.ILFieldSet _
    // | FSharpExprPatterns.ILAsm _
    | expr ->
        return sprintf "Cannot compile expression %A" expr
        |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)
  }

let private isIgnoredNonAttachedMember (meth: FSharpMemberOrFunctionOrValue) =
    Option.isSome meth.LiteralValue
    || meth.Attributes |> Seq.exists (fun att ->
        match att.AttributeType.TryFullName with
        | Some(Atts.global_ | Naming.StartsWith Atts.import _ | Naming.StartsWith Atts.emit _) -> true
        | _ -> false)
    || (match meth.DeclaringEntity with
        | Some ent -> isGlobalOrImportedEntity (FsEnt ent)
        | None -> false)

let private transformImplicitConstructor (com: FableCompiler) (ctx: Context)
            (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    match memb.DeclaringEntity with
    | None -> "Unexpected constructor without declaring entity: " + memb.FullName
              |> addError com ctx.InlinePath None; []
    | Some ent ->
        let mutable baseCall = None
        let captureBaseCall =
            getBaseEntity ent
            |> Option.map (fun (ent, _) -> ent, fun c -> baseCall <- Some c)
        let bodyCtx, args = bindMemberArgs com ctx args
        let bodyCtx = { bodyCtx with CaptureBaseConsCall = captureBaseCall }
        let body = transformExpr com bodyCtx body |> run
        let consName, _ = getMemberDeclarationName com memb
        let info = MemberInfo(memb.Attributes,
                    hasSpread=hasParamArray memb,
                    isPublic=isPublicMember memb,
                    isInstance=false)
        let fullName = ent.FullName
        let cons: Fable.MemberDecl =
            { Name = consName
              FullDisplayName = fullName
              Args = args
              Body = body
              UsedNames = set ctx.UsedNamesInDeclarationScope
              Info = info
              ExportDefault = false }
        com.AddConstructor(fullName, cons, baseCall)
        []

/// When using `importMember`, uses the member display name as selector
let private importExprSelector (memb: FSharpMemberOrFunctionOrValue) selector =
    match selector with
    | Naming.placeholder -> getMemberDisplayName memb
    | _ -> selector

let private transformImportWithInfo _com r typ info name fullDisplayName selector path =
    [Fable.MemberDeclaration
        { Name = name
          FullDisplayName = fullDisplayName
          Args = []
          Body = makeImportUserGenerated r typ selector path
          UsedNames = Set.empty
          Info = info
          ExportDefault = false }]

let private transformImport com r typ isMutable isPublic name fullDisplayName selector path =
    if isMutable && isPublic then // See #1314
        "Imported members cannot be mutable and public, please make it private: " + name
        |> addError com [] None
    let info = MemberInfo(isValue=true, isPublic=isPublic, isMutable=isMutable)
    transformImportWithInfo com r typ info name fullDisplayName selector path

let private transformMemberValue (com: IFableCompiler) ctx isPublic name fullDisplayName (memb: FSharpMemberOrFunctionOrValue) (value: FSharpExpr) =
    let value = transformExpr com ctx value |> run
    match value with
    // Accept import expressions, e.g. let foo = import "foo" "myLib"
    | Fable.Import(info, typ, r) when not info.IsCompilerGenerated ->
        match typ with
        | Fable.LambdaType(_, Fable.LambdaType(_, _)) ->
            "Change declaration of member: " + name + "\n"
            + "Importing JS functions with multiple arguments as `let add: int->int->int` won't uncurry parameters." + "\n"
            + "Use following syntax: `let add (x:int) (y:int): int = import ...`"
            |> addError com ctx.InlinePath None
        | _ -> ()
        let selector = importExprSelector memb info.Selector
        transformImport com r typ memb.IsMutable isPublic name fullDisplayName selector info.Path
    | fableValue ->
        let info = MemberInfo(memb.Attributes, isValue=true, isPublic=isPublic, isMutable=memb.IsMutable)

        // Mutable public values must be compiled as functions (see #986)
        // because values imported from ES2015 modules cannot be modified
        // (Note: Moved here from Fable2Babel)
        let fableValue =
            if memb.IsMutable && isPublic
            then Replacements.createAtom com fableValue
            else fableValue

        [Fable.MemberDeclaration
            { Name = name
              FullDisplayName = fullDisplayName
              Args = []
              Body = fableValue
              UsedNames = set ctx.UsedNamesInDeclarationScope
              Info = info
              ExportDefault = false }]

let private moduleMemberDeclarationInfo isPublic (memb: FSharpMemberOrFunctionOrValue): Fable.MemberInfo =
    MemberInfo(memb.Attributes,
                   hasSpread=hasParamArray memb,
                   isPublic=isPublic,
                   isInstance=memb.IsInstanceMember,
                   isMutable=memb.IsMutable) :> _

let private transformMemberFunction (com: IFableCompiler) ctx isPublic name fullDisplayName (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    match body with
    // Accept import expressions, e.g. let foo x y = import "foo" "myLib"
    | Fable.Import(info, _, r) when not info.IsCompilerGenerated ->
        // Use the full function type
        let typ = makeType Map.empty memb.FullType
        let selector = importExprSelector memb info.Selector
        // If this is a getter, it means the imported value is an object but Fable will call it as a function, see #2329
        let minfo = MemberInfo(isValue=not memb.IsPropertyGetterMethod, isPublic=isPublic)
        transformImportWithInfo com r typ minfo name fullDisplayName selector info.Path
    | body ->
        // If this is a static constructor, call it immediately
        if memb.CompiledName = ".cctor" then
            [Fable.ActionDeclaration
                { Body =
                    Fable.Delegate(args, body, Some name)
                    |> makeCall None Fable.Unit (makeCallInfo None [] [])
                  UsedNames = set ctx.UsedNamesInDeclarationScope }]
        else
            [Fable.MemberDeclaration
                { Name = name
                  FullDisplayName = fullDisplayName
                  Args = args
                  Body = body
                  UsedNames = set ctx.UsedNamesInDeclarationScope
                  Info = moduleMemberDeclarationInfo isPublic memb
                  ExportDefault = false }]

let private transformMemberFunctionOrValue (com: IFableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let isPublic = isPublicMember memb
    let name, _ = getMemberDeclarationName com memb
    let fullDisplayName = memb.TryGetFullDisplayName() |> Option.defaultValue name
    memb.Attributes
    |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
    |> function
    | ImportAtt(selector, path) ->
        let selector =
            if selector = Naming.placeholder then getMemberDisplayName memb
            else selector
        let typ = makeType Map.empty memb.FullType
        transformImport com None typ memb.IsMutable isPublic name fullDisplayName selector path
    | _ ->
        let noArgs = memb.CurriedParameterGroups.Count = 0
        if noArgs && memb.GenericParameters.Count = 0 then
            transformMemberValue com ctx isPublic name fullDisplayName memb body
        else
        if isModuleValueForDeclarations memb
        then transformMemberValue com ctx isPublic name fullDisplayName memb body
        else transformMemberFunction com ctx isPublic name fullDisplayName memb args body

let private transformAttachedMember (com: FableCompiler) (ctx: Context)
            (declaringEntity: Fable.Entity) (signature: FSharpAbstractSignature)
            (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    let entFullName = declaringEntity.FullName
    let name, info = getAttachedMemberInfo com ctx body.Range com.NonMangledAttachedMemberConflicts (Some declaringEntity) signature memb.Attributes
    com.AddAttachedMember(entFullName,
        { Name = name
          FullDisplayName = entFullName + "." + signature.Name
          Args = args
          Body = body
          UsedNames = set ctx.UsedNamesInDeclarationScope
          Info = info
          ExportDefault = false })

let private transformExplicitlyAttachedMember (com: FableCompiler) (ctx: Context)
            (declaringEntity: FSharpEntity) (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    let entFullName = declaringEntity.FullName
    let name = Naming.removeGetSetPrefix memb.CompiledName
    let isGetter = memb.IsPropertyGetterMethod && countNonCurriedParams memb = 0
    let isSetter = not isGetter && memb.IsPropertySetterMethod && countNonCurriedParams memb = 1
    let hasSpread = not isGetter && not isSetter && hasParamArray memb
    let info = MemberInfo(hasSpread=hasSpread ,
                         isGetter=isGetter,
                         isSetter=isSetter,
                         isInstance=memb.IsInstanceMember)
    com.AddAttachedMember(entFullName,
        { Name = name
          FullDisplayName = entFullName + "." + name
          Args = args
          Body = body
          UsedNames = set ctx.UsedNamesInDeclarationScope
          Info = info
          ExportDefault = false })

let private transformMemberDecl (com: FableCompiler) (ctx: Context) (memb: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let ctx = { ctx with EnclosingMember = Some memb
                         UsedNamesInDeclarationScope = HashSet() }
    if isIgnoredNonAttachedMember memb then
        if memb.IsMutable && isPublicMember memb && hasAttribute Atts.global_ memb.Attributes then
            "Global members cannot be mutable and public, please make it private: " + memb.DisplayName
            |> addError com [] None
        []
    elif isInline memb then
        let inlineExpr = { Args = List.concat args
                           Body = body
                           FileName = (com :> Compiler).CurrentFile }
        com.AddInlineExpr(memb, inlineExpr)
        []
    elif memb.IsImplicitConstructor then
        transformImplicitConstructor com ctx memb args body
    elif memb.IsOverrideOrExplicitInterfaceImplementation then
        // Ignore attached members generated by the F# compiler (for comparison and equality)
        if not memb.IsCompilerGenerated then
            match memb.DeclaringEntity with
            | Some declaringEntity ->
                let declaringEntity = FsEnt declaringEntity :> Fable.Entity
                if isGlobalOrImportedEntity declaringEntity then ()
                elif isErasedOrStringEnumEntity declaringEntity then
                    // Ignore abstract members for classes, see #2295
                    if declaringEntity.IsFSharpUnion || declaringEntity.IsFSharpRecord then
                        let r = makeRange memb.DeclarationLocation |> Some
                        "Erased unions/records cannot implement abstract members"
                        |> addError com ctx.InlinePath r
                else
                    // Not sure when it's possible that a member implements multiple abstract signatures
                    memb.ImplementedAbstractSignatures
                    |> Seq.tryHead
                    |> Option.iter (fun s -> transformAttachedMember com ctx declaringEntity s memb args body)
            | None -> ()
        []
    else
        match memb.DeclaringEntity with
        | Some ent when (isAttachMembersEntity com ent && memb.CompiledName <> ".cctor") ->
            transformExplicitlyAttachedMember com ctx ent memb args body; []
        | _ -> transformMemberFunctionOrValue com ctx memb args body

let private addUsedRootName com name (usedRootNames: Set<string>) =
    if Set.contains name usedRootNames then
        "Cannot have two module members with same name: " + name
        |> addError com [] None
    Set.add name usedRootNames

// Entities that are not output to JS
let private isIgnoredLeafEntity (ent: FSharpEntity) =
    ent.IsInterface
    || ent.IsEnum
    || ent.IsMeasure
    || ent.IsFSharpAbbreviation
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
                else
                    match getEntityDeclarationName com entRef with
                    | "" -> usedNames
                    | entName ->
                        let reflectionSuffix =
                            match com.Options.Language with
                            | Python -> Fable.PY.Naming.reflectionSuffix
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
        | FSharpImplementationFileDeclaration.Entity(ent, sub) ->
            match sub with
            | [] when isIgnoredLeafEntity ent -> []
            | [] ->
                let entFullName = FsEnt.Ref ent
                let ent = (com :> Compiler).GetEntity(entFullName)
                if isErasedOrStringEnumEntity ent || isGlobalOrImportedEntity ent then
                    []
                else
                    // If the file is empty F# creates a class for the module, but Fable clears the name
                    // because it matches the root module so it becomes invalid JS, see #2350
                    match getEntityDeclarationName com entFullName with
                    | "" -> []
                    | name ->
                        [Fable.ClassDeclaration
                            { Name = name
                              Entity = entFullName
                              Constructor = None
                              BaseCall = None
                              AttachedMembers = [] }]
            // This adds modules in the AST for languages that support them (like Rust)
            | sub when (ent.IsFSharpModule || ent.IsNamespace) && (com :> Compiler).Options.Language = Rust ->
                let entRef = FsEnt.Ref ent
                let members = transformDeclarations com ctx sub
                [Fable.ModuleDeclaration
                    { Name = ent.CompiledName
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

let getRootFSharpEntities (file: FSharpImplementationFileContents) =
    let rec getRootFSharpEntitiesInner decl = seq {
        match decl with
        | FSharpImplementationFileDeclaration.Entity (ent, nested) ->
            if ent.IsNamespace then
                for d in nested do
                    yield! getRootFSharpEntitiesInner d
            else ent
        | _ -> ()
    }
    file.Declarations |> Seq.collect getRootFSharpEntitiesInner

let getRootModule (file: FSharpImplementationFileContents) =
    let rec getRootModuleInner outerEnt decls =
        match decls, outerEnt with
        | [FSharpImplementationFileDeclaration.Entity (ent, decls)], _ when ent.IsFSharpModule || ent.IsNamespace ->
            getRootModuleInner (Some ent) decls
        | CommonNamespace(ent, decls), _ ->
            getRootModuleInner (Some ent) decls
        | _, Some e -> FsEnt.FullName e
        | _, None -> ""
    getRootModuleInner None file.Declarations

let private tryGetMemberArgsAndBody (com: Compiler) fileName entityFullName memberUniqueName =
    let rec tryGetMemberArgsAndBodyInner (entityFullName: string) (memberUniqueName: string) = function
        | FSharpImplementationFileDeclaration.Entity (e, decls) ->
            let entityFullName2 = FsEnt.FullName e
            if entityFullName.StartsWith(entityFullName2)
            then List.tryPick (tryGetMemberArgsAndBodyInner entityFullName memberUniqueName) decls
            else None
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (memb2, args, body) ->
            if getMemberUniqueName com memb2 = memberUniqueName
            then Some(args, body)
            else None
        | FSharpImplementationFileDeclaration.InitAction _ -> None
    let file = com.GetImplementationFile(fileName)
    file.Declarations |> List.tryPick (tryGetMemberArgsAndBodyInner entityFullName memberUniqueName)

type FableCompiler(com: Compiler) =
    let attachedMembers = Dictionary<string, _>()
    let onlyOnceWarnings = HashSet<string>()

    member __.Options = com.Options

    member _.AddInlineExpr(memb, inlineExpr: InlineExpr) =
        let fullName = getMemberUniqueName com memb
        com.GetOrAddInlineExpr(fullName, fun () -> inlineExpr) |> ignore

    member _.ReplaceAttachedMembers(entityFullName, f) =
        if attachedMembers.ContainsKey(entityFullName) then
            attachedMembers.[entityFullName] <- f attachedMembers.[entityFullName]
        else
            let members = {| NonMangledNames = HashSet()
                             Members = ResizeArray()
                             Cons = None
                             BaseCall = None |}
            attachedMembers.Add(entityFullName, f members)

    member _.TryGetAttachedMembers(entityFullName) =
        match attachedMembers.TryGetValue(entityFullName) with
        | true, members -> Some members
        | false, _ -> None

    member this.AddConstructor(entityFullName, cons: Fable.MemberDecl, baseCall: Fable.Expr option) =
        this.ReplaceAttachedMembers(entityFullName, fun members ->
            {| members with Cons = Some cons
                            BaseCall = baseCall |})

    member this.AddAttachedMember(entityFullName, memb: Fable.MemberDecl) =
        this.ReplaceAttachedMembers(entityFullName, fun members ->
            if not memb.Info.IsMangled then
                members.NonMangledNames.Add(memb.Name) |> ignore
            members.Members.Add(memb)
            members)

    member this.NonMangledAttachedMemberConflicts entityFullName memberName =
        this.TryGetAttachedMembers(entityFullName)
        |> Option.map (fun members -> members.NonMangledNames.Contains(memberName))
        |> Option.defaultValue false

    interface IFableCompiler with
        member _.WarnOnlyOnce(msg, ?range) =
            if onlyOnceWarnings.Add(msg) then
                addWarning com [] range msg

        member this.Transform(ctx, fsExpr) =
            transformExpr this ctx fsExpr |> run

        member this.TryReplace(ctx, r, t, info, thisArg, args) =
            match this.Options.Language with
            | Python -> PY.Replacements.tryCall this ctx r t info thisArg args
            | Rust -> Rust.Replacements.tryCall this ctx r t info thisArg args
            | _ -> Replacements.tryCall this ctx r t info thisArg args

        member this.GetInlineExpr(memb) =
            let membUniqueName = getMemberUniqueName com memb
            match memb.DeclaringEntity with
            | None -> failwith ("Unexpected inlined member without declaring entity. Please report: " + membUniqueName)
            | Some ent ->
                // The entity name is not included in the member unique name for type extensions, see #1667
                let entRef = FsEnt.Ref ent
                match entRef.SourcePath with
                | None -> failwith ("Cannot access source path of %s" + entRef.FullName)
                | Some fileName ->
                    com.AddWatchDependency(fileName)
                    com.GetOrAddInlineExpr(membUniqueName, fun () ->
                        match tryGetMemberArgsAndBody com fileName entRef.FullName membUniqueName with
                        | Some(args, body) ->
                            { Args = List.concat args
                              Body = body
                              FileName = fileName }
                        | None -> failwith ("Cannot find inline member. Please report: " + membUniqueName))

    interface Compiler with
        member _.Options = com.Options
        member _.Plugins = com.Plugins
        member _.LibraryDir = com.LibraryDir
        member _.CurrentFile = com.CurrentFile
        member _.OutputDir = com.OutputDir
        member _.ProjectFile = com.ProjectFile
        member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)
        member _.GetRootModule(fileName) = com.GetRootModule(fileName)
        member _.GetEntity(fullName) = com.GetEntity(fullName)
        member _.GetOrAddInlineExpr(fullName, generate) = com.GetOrAddInlineExpr(fullName, generate)
        member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)
        member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

let transformFile (com: Compiler) =
    let file = com.GetImplementationFile(com.CurrentFile)
    let usedRootNames = getUsedRootNames com Set.empty file.Declarations
    let ctx = Context.Create(usedRootNames)
    let com = FableCompiler(com)
    let rootDecls =
        transformDeclarations com ctx file.Declarations
        |> List.map (function
            | Fable.ClassDeclaration decl as classDecl ->
                com.TryGetAttachedMembers(decl.Entity.FullName)
                |> Option.map (fun members ->
                    { decl with Constructor = members.Cons
                                BaseCall = members.BaseCall
                                AttachedMembers = members.Members.ToArray() |> List.ofArray }
                    |> Fable.ClassDeclaration)
                |> Option.defaultValue classDecl
            | decl -> decl)
    Fable.File(rootDecls, usedRootNames)
