module rec Fable.Transforms.FSharp2Fable.Compiler

open System.Collections.Generic
open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.SourceCodeServices

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
        let callInfo: Fable.CallInfo =
          { ThisArg = None
            Args = args
            SignatureArgTypes = getArgTypes com baseCons
            HasSpread = false
            IsJsConstructor = false }
        makeCall r Fable.Unit callInfo baseRef
    | None ->
        if not baseCons.IsImplicitConstructor then
            "Only inheriting from primary constructors is supported"
            |> addWarning com [] r
        match makeCallFrom com ctx r Fable.Unit genArgs None baseArgs baseCons with
        | Fable.Call(_, info, t, r) ->
            let baseExpr = entityRef com baseEnt
            Fable.Call(baseExpr, info, t, r)
        // Other cases, like Emit will call directly the base expression
        | e -> e

let private transformNewUnion com ctx r fsType (unionCase: FSharpUnionCase) (argExprs: Fable.Expr list) =
    match fsType, unionCase with
    | ErasedUnionCase ->
        Fable.NewTuple argExprs |> makeValue r
    | ErasedUnion(tdef, _genArgs, rule) ->
        match argExprs with
        | [] -> transformStringEnum rule unionCase
        | [argExpr] -> argExpr
        | _ when tdef.UnionCases.Count > 1 ->
            "Erased unions with multiple cases must have one single field: " + (getFsTypeFullName fsType)
            |> addErrorAndReturnNull com ctx.InlinePath r
        | argExprs -> Fable.NewTuple argExprs |> makeValue r
    | StringEnum(tdef, rule) ->
        match argExprs with
        | [] -> transformStringEnum rule unionCase
        | _ -> sprintf "StringEnum types cannot have fields: %O" tdef.TryFullName
               |> addErrorAndReturnNull com ctx.InlinePath r
    | OptionUnion typ ->
        let typ = makeType ctx.GenericArgs typ
        let expr =
            match argExprs with
            | [] -> None
            | [expr] -> Some expr
            | _ -> failwith "Unexpected args for Option constructor"
        Fable.NewOption(expr, typ) |> makeValue r
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
        let tag = unionCaseTag tdef unionCase
        Fable.NewUnion(argExprs, tag, FsEnt tdef, genArgs) |> makeValue r

let private transformTraitCall com (ctx: Context) r typ (sourceTypes: FSharpType list) traitName (flags: MemberFlags) (argTypes: FSharpType list) (argExprs: FSharpExpr list) =
    let makeCallInfo traitName entityFullName argTypes genArgs: Fable.ReplaceCallInfo =
        { SignatureArgTypes = argTypes
          DeclaringEntityFullName = entityFullName
          HasSpread = false
          IsModuleValue = false
          // We only need this for types with own entries in Fable AST
          // (no interfaces, see below) so it's safe to set this to false
          IsInterface = false
          CompiledName = traitName
          OverloadSuffix = lazy ""
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
    let argTypes = List.map (makeType ctx.GenericArgs) argTypes
    let argExprs = List.map (fun e -> com.Transform(ctx, e)) argExprs
    let thisArg, args, argTypes =
        match argExprs, argTypes with
        | thisArg::args, _::argTypes when isInstance -> Some thisArg, args, argTypes
        | args, argTypes -> None, args, argTypes

    sourceTypes |> Seq.tryPick (fun sourceType ->
        let t = makeType ctx.GenericArgs sourceType
        match t with
        // Types with specific entry in Fable.AST
        // TODO: Check other types like booleans or numbers?
        | Fable.String ->
            let info = makeCallInfo traitName Types.string argTypes []
            Replacements.strings com ctx r typ info thisArg args
        | Fable.Tuple genArgs ->
            let info = makeCallInfo traitName (getTypeFullName false t) argTypes genArgs
            Replacements.tuples com ctx r typ info thisArg args
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
                        let kind = Fable.FieldKey(fi) |> Fable.ByKey
                        Fable.Get(thisArg.Value, kind, typ, r) |> Some
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
                    let kind =
                        FsField(fi, lazy fiType) :> Fable.Field
                        |> Fable.FieldKey
                        |> Fable.ByKey
                    Fable.Get(thisArg.Value, kind, typ, r) |> Some
                else None)
        | _ -> None
    ) |> Option.defaultWith (fun () ->
        "Cannot resolve trait call " + traitName |> addErrorAndReturnNull com ctx.InlinePath r)

let private getAttachedMemberInfo com ctx r nonMangledNameConflicts
                (declaringEntityName: string option) (sign: FSharpAbstractSignature) attributes =
    let declaringEntityName = defaultArg declaringEntityName ""
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
                    ent.TryGetMembersFunctionsAndValues
                    |> Seq.tryFind (fun x -> x.CompiledName = sign.Name)
                    |> function Some m -> hasParamArray m | None -> false
            let isMangled = isMangledAbstractEntity ent
            let name, isGetter, isSetter =
                if isMangled then
                    let overloadHash =
                        if (isGetter || isSetter) && not indexedProp then ""
                        else OverloadSuffix.getAbstractSignatureHash ent sign
                    getMangledAbstractMemberName ent sign.Name overloadHash, false, false
                else
                    let name, isGetter, isSetter =
                        // For indexed properties, keep the get_/set_ prefix and compile as method
                        if indexedProp then sign.Name, false, false
                        else Naming.removeGetSetPrefix sign.Name, isGetter, isSetter
                    // Setters can have same name as getters, assume there will always be a getter
                    if not isSetter && nonMangledNameConflicts declaringEntityName name then
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
                 Args = args
                 Body = body
                 // UsedNames are not used for obj expr members
                 UsedNames = Set.empty
                 Info = info }
      }

    trampoline {
      let! baseCall =
        trampoline {
            match baseCallExpr with
            // TODO: For interface implementations this should be BasicPatterns.NewObject
            // but check the baseCall.DeclaringEntity name just in case
            | BasicPatterns.Call(None,baseCall,genArgs1,genArgs2,baseArgs) ->
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

let private transformDelegate com ctx delegateType expr =
  trampoline {
    let! expr = transformExpr com ctx expr
    match makeType ctx.GenericArgs delegateType with
    | Fable.DelegateType(argTypes, _) ->
        let arity = List.length argTypes |> max 1
        match expr with
        | LambdaUncurriedAtCompileTime (Some arity) lambda -> return lambda
        | _ -> return Replacements.uncurryExprAtRuntime com arity expr
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
        match unionCase.UnionCaseFields.Count with
        | 0 -> return makeEqOp r unionExpr (transformStringEnum rule unionCase) BinaryEqualStrict
        | 1 ->
            let fi = unionCase.UnionCaseFields.[0]
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
        let tag = unionCaseTag tdef unionCase
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

    // | OptimizedOperator(memb, comp, opName, argTypes, argExprs) ->
    //     let r, typ = makeRangeFrom fsExpr, makeType ctx.GenericArgs fsExpr.Type
    //     let argTypes = argTypes |> List.map (makeType ctx.GenericArgs)
    //     let! args = transformExprList com ctx argExprs
    //     let entity: Fable.Entity =
    //         match comp with
    //         | Some comp -> upcast FsEnt comp.DeclaringEntity.Value
    //         | None -> upcast FsEnt memb.DeclaringEntity.Value
    //     let membOpt = tryFindMember com entity ctx.GenericArgs opName false argTypes
    //     return (match membOpt with
    //             | Some memb -> makeCallFrom com ctx r typ argTypes None args memb
    //             | None -> failwithf "Cannot find member %s.%s" (entity.FullName) opName)

    // work-around for optimized "for x in list" (erases this sequential)
    // | BasicPatterns.Sequential (BasicPatterns.ValueSet (current, BasicPatterns.Value next1),
    //                             (BasicPatterns.ValueSet (next2, BasicPatterns.UnionCaseGet
    //                                 (_value, typ, unionCase, field))))
    //         when next1.FullName = "next" && next2.FullName = "next"
    //             && current.FullName = "current" && (getFsTypeFullName typ) = Types.list
    //             && unionCase.Name = "op_ColonColon" && field.Name = "Tail" ->
    //     // replace with nothing
    //     return Fable.UnitConstant |> makeValue None

    | BasicPatterns.Coerce(targetType, inpExpr) ->
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
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, lambda) ->
        let! lambda = transformExpr com ctx lambda
        return lambda

    | BasicPatterns.FastIntegerForLoop(start, limit, body, isUp) ->
        let r = makeRangeFrom fsExpr
        match body with
        | BasicPatterns.Lambda (PutArgInScope com ctx (newContext, ident), body) ->
            let! start = transformExpr com ctx start
            let! limit = transformExpr com ctx limit
            let! body = transformExpr com newContext body
            return makeForLoop r isUp ident start limit body
        | _ -> return failwithf "Unexpected loop %O: %A" r fsExpr

    | BasicPatterns.WhileLoop(guardExpr, bodyExpr) ->
        let! guardExpr = transformExpr com ctx guardExpr
        let! bodyExpr = transformExpr com ctx bodyExpr
        return (guardExpr, bodyExpr) ||> makeWhileLoop (makeRangeFrom fsExpr)

    | BasicPatterns.Const(value, typ) ->
        let typ = makeType ctx.GenericArgs typ
        return Replacements.makeTypeConst com (makeRangeFrom fsExpr) typ value

    | BasicPatterns.BaseValue typ ->
        let r = makeRangeFrom fsExpr
        let typ = makeType Map.empty typ
        return Fable.Value(Fable.BaseValue(ctx.BoundMemberThis, typ), r)

    // F# compiler doesn't represent `this` in non-constructors as BasicPatterns.ThisValue (but BasicPatterns.Value)
    | BasicPatterns.ThisValue typ ->
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

    | BasicPatterns.Value var ->
        let r = makeRangeFrom fsExpr
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
        return Replacements.defaultof com ctx typ

    // Capture variable generic type mapping
    | BasicPatterns.Let((var, value), (BasicPatterns.Application(_body, genArgs, _args) as expr)) ->
        let genArgs = Seq.map (makeType ctx.GenericArgs) genArgs
        let ctx = { ctx with GenericArgs = matchGenericParamsFrom var genArgs |> Map }
        let! value = transformExpr com ctx value
        let ctx, ident = putBindingInScope com ctx var value
        let! body = transformExpr com ctx expr
        return Fable.Let([ident, value], body)

    | BasicPatterns.Let((var, value), body) ->
        match value with
        | CreateEvent(value, eventName) ->
            let! value = transformExpr com ctx value
            let value = get None Fable.Any value eventName
            let ctx, ident = putBindingInScope com ctx var value
            let! body = transformExpr com ctx body
            return Fable.Let([ident, value], body)

        | value ->
            if isInline var then
                let ctx = { ctx with ScopeInlineValues = (var, value)::ctx.ScopeInlineValues }
                return! transformExpr com ctx body
            else
                let! value = transformExpr com ctx value
                let ctx, ident = putBindingInScope com ctx var value
                let! body = transformExpr com ctx body
                return Fable.Let([ident, value], body)

    | BasicPatterns.LetRec(recBindings, body) ->
        // First get a context containing all idents and use it compile the values
        let ctx, idents =
            (recBindings, (ctx, []))
            ||> List.foldBack (fun (PutArgInScope com ctx (newContext, ident), _) (ctx, idents) ->
                (newContext, ident::idents))
        let _, bindingExprs = List.unzip recBindings
        let! exprs = transformExprList com ctx bindingExprs
        let bindings = List.zip idents exprs
        let! body = transformExpr com ctx body
        return Fable.Let(bindings, body)

    // `argTypes2` is always empty
    | BasicPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, _argTypes2, argExprs) ->
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return transformTraitCall com ctx (makeRangeFrom fsExpr) typ sourceTypes traitName flags argTypes argExprs

    | BasicPatterns.Call(callee, memb, ownerGenArgs, membGenArgs, args) ->
        match callee with
        | Some(CreateEvent(callee, eventName)) ->
            let! callee = transformExpr com ctx callee
            let! args = transformExprList com ctx args
            let callee = get None Fable.Any callee eventName
            let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
            let typ = makeType ctx.GenericArgs fsExpr.Type
            return makeCallFrom com ctx (makeRangeFrom fsExpr) typ genArgs (Some callee) args memb

        | callee ->
            let! callee = transformExprOpt com ctx callee
            let! args = transformExprList com ctx args
            // TODO: Check answer to #868 in FSC repo
            let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType ctx.GenericArgs)
            let typ = makeType ctx.GenericArgs fsExpr.Type
            return makeCallFrom com ctx (makeRangeFrom fsExpr) typ genArgs callee args memb

    | BasicPatterns.Application(applied, _genArgs, []) ->
        // TODO: Ask why application without arguments happen. So far I've seen it
        // to access None or struct values (like the Result type)
        return! transformExpr com ctx applied

    // Application of locally inlined lambdas
    | BasicPatterns.Application(BasicPatterns.Value var, genArgs, args) when isInline var ->
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
            return "Cannot resolve locally inlined value: " + var.DisplayName
            |> addErrorAndReturnNull com ctx.InlinePath r

    // When using Fable dynamic operator, we must untuple arguments
    // Note F# compiler wraps the value in a closure if it detects it's a lambda
    | BasicPatterns.Application(BasicPatterns.Let((_, BasicPatterns.Call(None,m,_,_,[e1; e2])),_), _genArgs, args)
            when m.FullName = "Fable.Core.JsInterop.( ? )" ->
        let! e1 = transformExpr com ctx e1
        let! e2 = transformExpr com ctx e2
        let e = Fable.Get(e1, Fable.ByKey(Fable.ExprKey e2), Fable.Any, e1.Range)
        let! args = transformExprList com ctx args
        let args = destructureTupleArgs args
        let typ = makeType ctx.GenericArgs fsExpr.Type
        // Convert this to emit so auto-uncurrying is applied
        let emitInfo: Fable.EmitInfo =
            { Macro = "$0($1...)"
              Args = e::args
              SignatureArgTypes = [] // TODO
              IsJsStatement = false }
        return Fable.Emit(emitInfo, typ, makeRangeFrom fsExpr)

    // Some instance members such as Option.get_IsSome are compiled as static members, and the F# compiler
    // wraps calls with an application. But in Fable they will be replaced so the application is not needed
    | BasicPatterns.Application(BasicPatterns.Call(Some _, memb, _, [], []) as call, _genArgs, [BasicPatterns.Const(null, _)])
         when memb.IsInstanceMember && not memb.IsInstanceMemberInCompiledCode ->
         return! transformExpr com ctx call

    | BasicPatterns.Application(applied, _genArgs, args) ->
        let! applied = transformExpr com ctx applied
        let! args = transformExprList com ctx args
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return Fable.CurriedApply(applied, args, typ, makeRangeFrom fsExpr)

    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        let! guardExpr = transformExpr com ctx guardExpr
        let! thenExpr = transformExpr com ctx thenExpr
        let! fableElseExpr = transformExpr com ctx elseExpr

        let altElseExpr =
            match elseExpr with
            | RaisingMatchFailureExpr _fileNameWhereErrorOccurs ->
                let errorMessage = "The match cases were incomplete"
                let rangeOfElseExpr = makeRangeFrom elseExpr
                let errorExpr = Replacements.Helpers.error (Fable.Value(Fable.StringConstant errorMessage, None))
                makeThrow rangeOfElseExpr Fable.Any errorExpr
            | _ ->
                fableElseExpr

        return Fable.IfThenElse(guardExpr, thenExpr, altElseExpr, makeRangeFrom fsExpr)

    | BasicPatterns.TryFinally (body, finalBody) ->
        let r = makeRangeFrom fsExpr
        match body with
        | BasicPatterns.TryWith(body, _, _, catchVar, catchBody) ->
            return makeTryCatch com ctx r body (Some (catchVar, catchBody)) (Some finalBody)
        | _ -> return makeTryCatch com ctx r body None (Some finalBody)

    | BasicPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        return makeTryCatch com ctx (makeRangeFrom fsExpr) body (Some (catchVar, catchBody)) None

    | BasicPatterns.NewDelegate(delegateType, fsExpr) ->
        return! transformDelegate com ctx delegateType fsExpr

    | BasicPatterns.Lambda(arg, body) ->
        let ctx, args = makeFunctionArgs com ctx [arg]
        match args with
        | [arg] ->
            let! body = transformExpr com ctx body
            return Fable.Lambda(arg, body, None)
        | _ -> return failwith "makeFunctionArgs returns args with different length"

    // Getters and Setters
    | BasicPatterns.AnonRecordGet(callee, calleeType, fieldIndex) ->
        let! callee = transformExpr com ctx callee
        let fieldName = calleeType.AnonRecordTypeDetails.SortedFieldNames.[fieldIndex]
        let typ = makeType ctx.GenericArgs fsExpr.Type
        let key = FsField(fieldName, lazy typ) :> Fable.Field |> Fable.FieldKey
        return Fable.Get(callee, Fable.ByKey key, typ, makeRangeFrom fsExpr)

    | BasicPatterns.FSharpFieldGet(callee, calleeType, field) ->
        let! callee = transformExprOpt com ctx callee
        let callee =
            match callee with
            | Some callee -> callee
            | None -> entityRef com (FsEnt calleeType.TypeDefinition)
        let key = FsField field :> Fable.Field |> Fable.FieldKey
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return Fable.Get(callee, Fable.ByKey key, typ, makeRangeFrom fsExpr)

    | BasicPatterns.TupleGet(_tupleType, tupleElemIndex, tupleExpr) ->
        let! tupleExpr = transformExpr com ctx tupleExpr
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return Fable.Get(tupleExpr, Fable.TupleIndex tupleElemIndex, typ, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseGet (unionExpr, fsType, unionCase, field) ->
        let r = makeRangeFrom fsExpr
        let! unionExpr = transformExpr com ctx unionExpr
        match fsType, unionCase with
        | ErasedUnionCase ->
            let index = unionCase.UnionCaseFields |> Seq.findIndex (fun x -> x.Name = field.Name)
            return Fable.Get(unionExpr, Fable.TupleIndex(index), makeType ctx.GenericArgs fsType, r)
        | ErasedUnion _ ->
            if unionCase.UnionCaseFields.Count = 1 then return unionExpr
            else
                let index = unionCase.UnionCaseFields |> Seq.findIndex (fun x -> x.Name = field.Name)
                return Fable.Get(unionExpr, Fable.TupleIndex index, makeType ctx.GenericArgs fsType, r)
        | StringEnum _ ->
            return "StringEnum types cannot have fields"
            |> addErrorAndReturnNull com ctx.InlinePath r
        | OptionUnion t ->
            return Fable.Get(unionExpr, Fable.OptionValue, makeType ctx.GenericArgs t, r)
        | ListUnion t ->
            let t = makeType ctx.GenericArgs t
            let kind, t =
                if field.Name = "Head"
                then Fable.ListHead, t
                else Fable.ListTail, Fable.List t
            return Fable.Get(unionExpr, kind, t, r)
        | DiscriminatedUnion _ ->
            let t = makeType Map.empty field.FieldType
            let index =
                unionCase.UnionCaseFields
                |> Seq.findIndex (fun fi -> fi.Name = field.Name)
            let kind = Fable.UnionField(index, t)
            let typ = makeType ctx.GenericArgs fsExpr.Type
            return Fable.Get(unionExpr, kind, typ, r)

    | BasicPatterns.FSharpFieldSet(callee, calleeType, field, value) ->
        let! callee = transformExprOpt com ctx callee
        let! value = transformExpr com ctx value
        let callee =
            match callee with
            | Some callee -> callee
            | None -> entityRef com (FsEnt calleeType.TypeDefinition)
        let field = FsField field :> Fable.Field |> Fable.FieldKey |> Some
        return Fable.Set(callee, field, value, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTag(unionExpr, _unionType) ->
        let! unionExpr = transformExpr com ctx unionExpr
        return Fable.Get(unionExpr, Fable.UnionTag, Fable.Any, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseSet (_unionExpr, _type, _case, _caseField, _valueExpr) ->
        return "Unexpected UnionCaseSet" |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    | BasicPatterns.ValueSet (valToSet, valueExpr) ->
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
            return Fable.Set(valToSet, None, valueExpr, r)

    | BasicPatterns.NewArray(FableType com ctx elTyp, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return makeArray elTyp argExprs

    | BasicPatterns.NewTuple(_tupleType, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return Fable.NewTuple(argExprs) |> makeValue (makeRangeFrom fsExpr)

    | BasicPatterns.ObjectExpr(objType, baseCall, overrides, otherOverrides) ->
        match ctx.EnclosingMember with
        | Some m when m.IsImplicitConstructor ->
            let thisArg = getIdentUniqueName ctx "_this" |> makeIdent
            let thisValue = Fable.Value(Fable.ThisValue Fable.Any, None)
            let ctx = { ctx with BoundConstructorThis = Some thisArg }
            let! objExpr = transformObjExpr com ctx objType baseCall overrides otherOverrides
            return Fable.Let([thisArg, thisValue], objExpr)
        | _ -> return! transformObjExpr com ctx objType baseCall overrides otherOverrides

    | BasicPatterns.NewObject(memb, genArgs, args) ->
        let! args = transformExprList com ctx args
        let genArgs = Seq.map (makeType ctx.GenericArgs) genArgs
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return makeCallFrom com ctx (makeRangeFrom fsExpr) typ genArgs None args memb

    | BasicPatterns.Sequential (first, second) ->
        let exprs =
            match ctx.CaptureBaseConsCall with
            | Some(baseEnt, captureBaseCall) ->
                match first with
                | ConstructorCall(call, genArgs, args)
                // This pattern occurs in constructors that define a this value: `type C() as this`
                // We're discarding the bound `this` value, it "shouldn't" be used in the base constructor arguments
                | BasicPatterns.Let(_, (ConstructorCall(call, genArgs, args))) ->
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

    | BasicPatterns.NewRecord(fsType, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        let genArgs = makeGenArgs ctx.GenericArgs (getGenericArguments fsType)
        return Fable.NewRecord(argExprs, FsEnt fsType.TypeDefinition, genArgs) |> makeValue (makeRangeFrom fsExpr)

    | BasicPatterns.NewAnonRecord(fsType, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        let fieldNames = fsType.AnonRecordTypeDetails.SortedFieldNames
        let genArgs = makeGenArgs ctx.GenericArgs (getGenericArguments fsType)
        return Fable.NewAnonymousRecord(argExprs, fieldNames, genArgs) |> makeValue (makeRangeFrom fsExpr)

    | BasicPatterns.NewUnionCase(fsType, unionCase, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return argExprs
        |> transformNewUnion com ctx (makeRangeFrom fsExpr) fsType unionCase

    | BasicPatterns.TypeTest (FableType com ctx typ, expr) ->
        let! expr = transformExpr com ctx expr
        return Fable.Test(expr, Fable.TypeTest typ, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTest(unionExpr, fsType, unionCase) ->
        return! transformUnionCaseTest com ctx (makeRangeFrom fsExpr) unionExpr fsType unionCase

    // Pattern Matching
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        let! fableDecisionExpr = transformExpr com ctx decisionExpr
        let! fableDecisionTargets = transformDecisionTargets com ctx [] decisionTargets

        // rewrite last decision target if it throws MatchFailureException
        let compiledFableTargets =
            match snd (List.last decisionTargets) with
            | RaisingMatchFailureExpr fileNameWhereErrorOccurs ->
                match decisionExpr with
                | BasicPatterns.IfThenElse(BasicPatterns.UnionCaseTest(_unionValue, unionType, _unionCaseInfo), _, _) ->
                    let rangeOfLastDecisionTarget = makeRangeFrom (snd (List.last decisionTargets))
                    let errorMessage =
                        sprintf "The match cases were incomplete against type of '%s' at %s"
                            unionType.TypeDefinition.DisplayName
                            fileNameWhereErrorOccurs
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

    | BasicPatterns.DecisionTreeSuccess(targetIndex, boundValues) ->
        let! boundValues = transformExprList com ctx boundValues
        let typ = makeType ctx.GenericArgs fsExpr.Type
        return Fable.DecisionTreeSuccess(targetIndex, boundValues, typ)

    | BasicPatterns.ILFieldGet(None, ownerTyp, fieldName) ->
        let ownerTyp = makeType ctx.GenericArgs ownerTyp
        let typ = makeType ctx.GenericArgs fsExpr.Type
        match Replacements.tryField com typ ownerTyp fieldName with
        | Some expr -> return expr
        | None ->
            return sprintf "Cannot compile ILFieldGet(%A, %s)" ownerTyp fieldName
            |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    | BasicPatterns.Quote _ ->
        return "Quotes are not currently supported by Fable"
        |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    // This is used when passing arguments by reference,
    // and also when accessing Result types (not sure why)
    | BasicPatterns.AddressOf(expr) ->
        return! transformExpr com ctx expr

    | BasicPatterns.AddressSet _ ->
        return "Mutating an argument passed by reference is not supported"
        |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    // | BasicPatterns.ILFieldSet _
    // | BasicPatterns.AddressSet _
    // | BasicPatterns.ILAsm _
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
            ent.BaseType |> Option.bind (fun (NonAbbreviatedType baseType) ->
                if baseType.HasTypeDefinition then
                    let ent = baseType.TypeDefinition
                    match ent.TryFullName with
                    | Some name when name <> Types.object ->
                        Some(ent, fun c -> baseCall <- Some c)
                    | _ -> None
                else None)
        let bodyCtx, args = bindMemberArgs com ctx args
        let bodyCtx = { bodyCtx with CaptureBaseConsCall = captureBaseCall }
        let body = transformExpr com bodyCtx body |> run
        let consName, _ = getMemberDeclarationName com memb
        let info = MemberInfo(memb.Attributes,
                    hasSpread=hasParamArray memb,
                    isPublic=isPublicMember memb,
                    isInstance=false)
        let cons: Fable.MemberDecl =
            { Name = consName
              Args = args
              Body = body
              UsedNames = set ctx.UseNamesInDeclarationScope
              Info = info }
        com.AddConstructor(ent.FullName, cons, baseCall)
        []

/// When using `importMember`, uses the member display name as selector
let private importExprSelector (memb: FSharpMemberOrFunctionOrValue) selector =
    match selector with
    | Fable.Value(Fable.StringConstant Naming.placeholder,_) ->
        getMemberDisplayName memb |> makeStrConst
    | _ -> selector

let private transformImport com r typ isMutable isPublic name selector path =
    if isMutable && isPublic then // See #1314
        "Imported members cannot be mutable and public, please make it private: " + name
        |> addError com [] None
    let info = MemberInfo(isValue=true, isPublic=isPublic, isMutable=isMutable)
    [Fable.MemberDeclaration
        { Name = name
          Args = []
          Body = makeImportUserGenerated r typ selector path
          UsedNames = Set.empty
          Info = info }]

let private transformMemberValue (com: IFableCompiler) ctx isPublic name (memb: FSharpMemberOrFunctionOrValue) (value: FSharpExpr) =
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
        transformImport com r typ memb.IsMutable isPublic name selector info.Path
    | fableValue ->
        let info = MemberInfo(memb.Attributes, isValue=true, isPublic=isPublic, isMutable=memb.IsMutable)
        [Fable.MemberDeclaration
            { Name = name
              Args = []
              Body = fableValue
              UsedNames = set ctx.UseNamesInDeclarationScope
              Info = info }]

let private moduleMemberDeclarationInfo isPublic (memb: FSharpMemberOrFunctionOrValue): Fable.MemberInfo =
    MemberInfo(memb.Attributes,
                   hasSpread=hasParamArray memb,
                   isPublic=isPublic,
                   isInstance=memb.IsInstanceMember,
                   isMutable=memb.IsMutable) :> _

let private transformMemberFunction (com: IFableCompiler) ctx isPublic name (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    match body with
    // Accept import expressions, e.g. let foo x y = import "foo" "myLib"
    // TODO: This should likely be a declaration replacement plugin
    | Fable.Import(info, _, r) when not info.IsCompilerGenerated ->
        // Use the full function type
        let typ = makeType Map.empty memb.FullType
        let selector = importExprSelector memb info.Selector
        transformImport com r typ false isPublic name selector info.Path
    | body ->
        // If this is a static constructor, call it immediately
        if memb.CompiledName = ".cctor" then
            [Fable.ActionDeclaration
                { Body =
                    Fable.Delegate(args, body, Some name)
                    |> makeCall None Fable.Unit (makeCallInfo None [] [])
                  UsedNames = set ctx.UseNamesInDeclarationScope }]
        else
            [Fable.MemberDeclaration
                { Name = name
                  Args = args
                  Body = body
                  UsedNames = set ctx.UseNamesInDeclarationScope
                  Info = moduleMemberDeclarationInfo isPublic memb }]

let private transformMemberFunctionOrValue (com: IFableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let isPublic = isPublicMember memb
    let name, _ = getMemberDeclarationName com memb
    memb.Attributes
    |> Seq.map (fun x -> FsAtt(x) :> Fable.Attribute)
    |> function
    | ImportAtt(selector, path) ->
        let selector =
            if selector = Naming.placeholder then getMemberDisplayName memb
            else selector
        let typ = makeType Map.empty memb.FullType
        transformImport com None typ memb.IsMutable isPublic name (makeStrConst selector) (makeStrConst path)
    | _ ->
        if isModuleValueForDeclarations memb
        then transformMemberValue com ctx isPublic name memb body
        else transformMemberFunction com ctx isPublic name memb args body

let private transformAttachedMember (com: FableCompiler) (ctx: Context)
            (declaringEntity: Fable.Entity) (signature: FSharpAbstractSignature)
            (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    let entFullName = declaringEntity.FullName
    let name, info = getAttachedMemberInfo com ctx body.Range com.NonMangledAttachedMemberConflicts (Some entFullName) signature memb.Attributes
    com.AddAttachedMember(entFullName,
        { Name = name
          Args = args
          Body = body
          UsedNames = set ctx.UseNamesInDeclarationScope
          Info = info })

let private transformMemberDecl (com: FableCompiler) (ctx: Context) (memb: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let ctx = { ctx with EnclosingMember = Some memb
                         UseNamesInDeclarationScope = HashSet() }
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
                    let r = makeRange memb.DeclarationLocation |> Some
                    "Erased types cannot implement abstract members"
                    |> addError com ctx.InlinePath r
                else
                    // Not sure when it's possible that a member implements multiple abstract signatures
                    memb.ImplementedAbstractSignatures
                    |> Seq.tryHead
                    |> Option.iter (fun s -> transformAttachedMember com ctx declaringEntity s memb args body)
            | None -> ()
        []
    else transformMemberFunctionOrValue com ctx memb args body

let private addUsedRootName com name (usedRootNames: Set<string>) =
    if Set.contains name usedRootNames then
        "Cannot have two module members with same name: " + name
        |> addError com [] None
    Set.add name usedRootNames

// In case this is a recursive module, do a first pass to get all entity and member names
let rec private getUsedRootNames com (usedNames: Set<string>) decls =
    (usedNames, decls) ||> List.fold (fun usedNames decl ->
        match decl with
        | Entity(ent, []) ->
            let ent = FsEnt(ent) :> Fable.Entity
            if ent.IsInterface || ent.IsFSharpAbbreviation
                || isErasedOrStringEnumEntity ent
                || isGlobalOrImportedEntity ent then
                usedNames
            else
                let entName = getEntityDeclarationName com ent
                addUsedRootName com entName usedNames
                // Fable will inject an extra declaration for reflection,
                // so add also the name with the reflection suffix
                |> addUsedRootName com (entName + Naming.reflectionSuffix)
        | Entity(_, sub) ->
            getUsedRootNames com usedNames sub
        | MemberOrFunctionOrValue(memb,_,_) ->
            if memb.IsOverrideOrExplicitInterfaceImplementation then usedNames
            else
                let memberName, _ = getMemberDeclarationName com memb
                addUsedRootName com memberName usedNames
        | InitAction _ -> usedNames)

let rec private transformDeclarations (com: FableCompiler) ctx fsDecls =
    fsDecls |> List.collect (fun fsDecl ->
        match fsDecl with
        | Entity(ent, []) ->
            let fableEnt = FsEnt(ent) :> Fable.Entity
            if ent.IsInterface || ent.IsFSharpAbbreviation
                || isErasedOrStringEnumEntity fableEnt
                || isGlobalOrImportedEntity fableEnt then
                []
            else
                [Fable.ClassDeclaration
                    { Name = getEntityDeclarationName com fableEnt
                      Entity = fableEnt
                      Constructor = None
                      BaseCall = None
                      AttachedMembers = [] }]
        | Entity(_, sub) ->
            transformDeclarations com ctx sub
        | MemberOrFunctionOrValue(meth, args, body) ->
            transformMemberDecl com ctx meth args body
        | InitAction fe ->
            let ctx = { ctx with UseNamesInDeclarationScope = HashSet() }
            let e = transformExpr com ctx fe |> run
            [Fable.ActionDeclaration
                { Body = e
                  UsedNames = set ctx.UseNamesInDeclarationScope }])

let private getRootModuleAndDecls decls =
    let rec getRootModuleAndDeclsInner outerEnt decls =
        match decls with
        | [Entity (ent, decls)]
                when ent.IsFSharpModule || ent.IsNamespace ->
            getRootModuleAndDeclsInner (Some ent) decls
        | CommonNamespace(ent, decls) ->
            getRootModuleAndDeclsInner (Some ent) decls
        | decls -> outerEnt, decls
    getRootModuleAndDeclsInner None decls

let private tryGetMemberArgsAndBody (com: Compiler) fileName entityFullName memberUniqueName =
    let rec tryGetMemberArgsAndBodyInner (entityFullName: string) (memberUniqueName: string) = function
        | Entity (e, decls) ->
            let entityFullName2 = getEntityFullName e
            if entityFullName.StartsWith(entityFullName2)
            then List.tryPick (tryGetMemberArgsAndBodyInner entityFullName memberUniqueName) decls
            else None
        | MemberOrFunctionOrValue (memb2, args, body) ->
            if getMemberUniqueName com memb2 = memberUniqueName
            then Some(args, body)
            else None
        | InitAction _ -> None
    match com.ImplementationFiles.TryGetValue(fileName) with
    | true, f -> f.Declarations |> List.tryPick (tryGetMemberArgsAndBodyInner entityFullName memberUniqueName)
    | false, _ -> None

type FableCompiler(com: Compiler) =
    let attachedMembers = Dictionary<string, _>()

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
        member this.Transform(ctx, fsExpr) =
            transformExpr this ctx fsExpr |> run

        member this.TryReplace(ctx, r, t, info, thisArg, args) =
            Replacements.tryCall this ctx r t info thisArg args

        member this.InjectArgument(ctx, r, genArgs, parameter) =
            Inject.injectArg this ctx r genArgs parameter

        member this.GetInlineExpr(memb) =
            let membUniqueName = getMemberUniqueName com memb
            match memb.DeclaringEntity with
            | None -> failwith ("Unexpected inlined member without declaring entity. Please report: " + membUniqueName)
            | Some ent ->
                // The entity name is not included in the member unique name
                // for type extensions, see #1667
                let entFullName = getEntityFullName ent
                let fileName = FsMemberFunctionOrValue.SourcePath memb
                com.AddWatchDependency(fileName)

                com.GetOrAddInlineExpr(membUniqueName, fun () ->
                    match tryGetMemberArgsAndBody com fileName entFullName membUniqueName with
                    | Some(args, body) ->
                        { Args = List.concat args
                          Body = body
                          FileName = fileName }
                    | None -> failwith ("Cannot find inline member. Please report: " + membUniqueName))

        member _.TryGetImplementationFile (fileName) =
            let fileName = Path.normalizePathAndEnsureFsExtension fileName
            match com.ImplementationFiles.TryGetValue(fileName) with
            | true, f -> Some f
            | false, _ -> None

    interface Compiler with
        member _.Options = com.Options
        member _.LibraryDir = com.LibraryDir
        member _.CurrentFile = com.CurrentFile
        member _.ImplementationFiles = com.ImplementationFiles
        member _.GetRootModule(fileName) =
            com.GetRootModule(fileName)
        member _.GetOrAddInlineExpr(fullName, generate) =
            com.GetOrAddInlineExpr(fullName, generate)
        member _.AddWatchDependency(fileName) =
            com.AddWatchDependency(fileName)
        member _.AddLog(msg, severity, ?range, ?fileName:string, ?tag: string) =
            com.AddLog(msg, severity, ?range=range, ?fileName=fileName, ?tag=tag)

let getRootModuleFullName (file: FSharpImplementationFileContents) =
    let rootEnt, _ = getRootModuleAndDecls file.Declarations
    match rootEnt with
    | Some rootEnt -> getEntityFullName rootEnt
    | None -> ""

let transformFile (com: Compiler) =
    let file =
        match com.ImplementationFiles.TryGetValue(com.CurrentFile) with
        | true, file -> file
        | false, _ ->
            let projFiles = com.ImplementationFiles |> Seq.map (fun kv -> kv.Key) |> String.concat "\n"
            failwithf "File %s cannot be found in source list:\n%s" com.CurrentFile projFiles
    let fcom = FableCompiler(com)
    let rootEnt, rootDecls = getRootModuleAndDecls file.Declarations
    let usedRootNames = getUsedRootNames com Set.empty rootDecls
    let ctx = Context.Create(rootEnt, usedRootNames)
    let rootDecls =
        transformDeclarations fcom ctx rootDecls
        |> List.map (function
            | Fable.ClassDeclaration decl as classDecl ->
                fcom.TryGetAttachedMembers(decl.Entity.FullName)
                |> Option.map (fun members ->
                    { decl with Constructor = members.Cons
                                BaseCall = members.BaseCall
                                AttachedMembers = members.Members.ToArray() |> List.ofArray }
                    |> Fable.ClassDeclaration)
                |> Option.defaultValue classDecl
            | decl -> decl)
    Fable.File(rootDecls, usedRootNames)
