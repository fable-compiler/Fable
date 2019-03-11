module rec Fable.Transforms.FSharp2Fable.Compiler

open System.Collections.Generic
open FSharp.Compiler.Ast
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

// Fable doesn't support arguments passed by ref, see #1696
let private checkArgumentsPassedByRef com ctx (args: FSharpExpr list) =
    for arg in args do
        match arg with
        | BasicPatterns.AddressOf _ ->
            "Arguments cannot be passed byref"
            |> addWarning com ctx.InlinePath (makeRangeFrom arg)
        | _ -> ()

let private transformBaseConsCall com ctx r baseEnt (baseCons: FSharpMemberOrFunctionOrValue) genArgs baseArgs =
    let thisArg = ctx.BoundConstructorThis |> Option.map Fable.IdentExpr
    let baseArgs = transformExprList com ctx baseArgs |> run
    let genArgs = genArgs |> Seq.map (makeType com ctx.GenericArgs)
    match Replacements.tryBaseConstructor com baseEnt baseCons genArgs baseArgs with
    | Some(baseRef, args) ->
        // TODO: Should Replacements.tryBaseConstructor return the argInfo?
        let argInfo: Fable.ArgInfo =
          { ThisArg = thisArg
            Args = args
            SignatureArgTypes = getArgTypes com baseCons |> Fable.Typed
            Spread = Fable.NoSpread
            IsBaseOrSelfConstructorCall = true }
        baseRef, staticCall r Fable.Unit argInfo baseRef
    | None ->
        if not(hasImplicitConstructor baseEnt) then
            "Classes without a primary constructor cannot be inherited: " + baseEnt.FullName
            |> addError com ctx.InlinePath r
        let baseCons = makeCallFrom com ctx r Fable.Unit true genArgs thisArg baseArgs baseCons
        entityRefMaybeGlobalOrImported com baseEnt, baseCons

let private transformNewUnion com ctx r fsType
                (unionCase: FSharpUnionCase) (argExprs: Fable.Expr list) =
    match fsType with
    | ErasedUnion(_, genArgs) ->
        match argExprs with
        | [expr] ->
            let genArgs = makeGenArgs com ctx.GenericArgs genArgs
            Fable.NewErasedUnion(expr, genArgs) |> makeValue r
        | _ -> "Erased Union Cases must have one single field: " + (getFsTypeFullName fsType)
               |> addErrorAndReturnNull com ctx.InlinePath r
    | StringEnum(tdef, rule) ->
        let enumName = defaultArg tdef.TryFullName Naming.unknown
        match argExprs with
        | [] -> Fable.Enum(applyCaseRule rule unionCase |> Fable.StringEnum, enumName) |> makeValue r
        | _ -> "StringEnum types cannot have fields: " + enumName
               |> addErrorAndReturnNull com ctx.InlinePath r
    | OptionUnion typ ->
        let typ = makeType com ctx.GenericArgs typ
        let expr =
            match argExprs with
            | [] -> None
            | [expr] -> Some expr
            | _ -> failwith "Unexpected args for Option constructor"
        Fable.NewOption(expr, typ) |> makeValue r
    | ListUnion typ ->
        let typ = makeType com ctx.GenericArgs typ
        let headAndTail =
            match argExprs with
            | [] -> None
            | [head; tail] -> Some(head, tail)
            | _ -> failwith "Unexpected args for List constructor"
        Fable.NewList(headAndTail, typ) |> makeValue r
    | DiscriminatedUnion(tdef, genArgs) ->
        let genArgs = makeGenArgs com ctx.GenericArgs genArgs
        Fable.NewUnion(argExprs, unionCase, tdef, genArgs) |> makeValue r

let private transformTraitCall com (ctx: Context) r typ (sourceTypes: FSharpType list) traitName (flags: MemberFlags) (argTypes: FSharpType list) (argExprs: FSharpExpr list) =
    let makeCallInfo traitName entityFullName argTypes genArgs: Fable.ReplaceCallInfo =
        { SignatureArgTypes = argTypes
          DeclaringEntityFullName = entityFullName
          Spread = Fable.NoSpread
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

// TODO: Fix Action and Func tests in ConvertTests
let private transformDelegate com ctx delegateType expr =
  trampoline {
    let! expr = transformExpr com ctx expr
    match makeType com ctx.GenericArgs delegateType with
    | Fable.FunctionType(Fable.DelegateType argTypes, _) ->
        let arity = List.length argTypes |> max 1
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
        let kind = Fable.OptionTest(unionCase.Name <> "None" && unionCase.Name <> "ValueNone")
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
    match fsExpr with
    | OptimizedOperator(memb, comp, opName, argTypes, argExprs) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let argTypes = argTypes |> List.map (makeType com ctx.GenericArgs)
        let! args = transformExprList com ctx argExprs
        let entity =
            match comp with
            | Some comp -> comp.DeclaringEntity.Value
            | None -> memb.DeclaringEntity.Value
        let membOpt = tryFindMember com entity ctx.GenericArgs opName false argTypes
        return (match membOpt with
                | Some memb -> makeCallFrom com ctx r typ false argTypes None args memb
                | None -> failwithf "Cannot find member %A.%A" (entity.FullName) opName)

    | BasicPatterns.Coerce(targetType, inpExpr) ->
        let! (inpExpr: Fable.Expr) = transformExpr com ctx inpExpr
        let t = makeType com ctx.GenericArgs targetType
        match tryDefinition targetType with
        | Some(_, Some (Types.ienumerableGeneric | Types.ienumerable)) ->
            return Replacements.toSeq t inpExpr
        | _ -> return Fable.TypeCast(inpExpr, t)

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, lambda) ->
        let! lambda = transformExpr com ctx lambda
        return lambda

    | ByrefArgToTuple (callee, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let! callee = transformExprOpt com ctx callee
        let! args = transformExprList com ctx membArgs
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return makeCallFrom com ctx (makeRangeFrom fsExpr) typ false genArgs callee args memb

    | ByrefArgToTupleOptimizedIf (outArg, callee, memb, ownerGenArgs, membGenArgs, membArgs, elseExpr) ->
        let ctx, ident = bindIdentFrom com ctx outArg
        let! callee = transformExprOpt com ctx callee
        let! args = transformExprList com ctx membArgs
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        let identExpr = Fable.IdentExpr ident
        let tupleExpr = makeCallFrom com ctx None typ false genArgs callee args memb
        let guardExpr = Fable.Get(identExpr, Fable.TupleGet 0, typ, None)
        let thenExpr = Fable.Get(identExpr, Fable.TupleGet 1, typ, None)
        let! elseExpr = transformExpr com ctx elseExpr
        let body = Fable.IfThenElse(guardExpr, thenExpr, elseExpr, None)
        return Fable.Let([ident, tupleExpr], body)

    | ByrefArgToTupleOptimizedLet (id1, id2, callee, memb, ownerGenArgs, membGenArgs, membArgs, restExpr) ->
        let ctx, ident1 = bindIdentFrom com ctx id1
        let ctx, ident2 = bindIdentFrom com ctx id2
        let! callee = transformExprOpt com ctx callee
        let! args = transformExprList com ctx membArgs
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        let ident = makeIdentUnique com "tuple"
        let identExpr = Fable.IdentExpr ident
        let tupleExpr = makeCallFrom com ctx None typ false genArgs callee args memb
        let id1Expr = Fable.Get(identExpr, Fable.TupleGet 0, typ, None)
        let id2Expr = Fable.Get(identExpr, Fable.TupleGet 1, typ, None)
        let! restExpr = transformExpr com ctx restExpr
        let body = Fable.Let([ident1, id1Expr], Fable.Let([ident2, id2Expr], restExpr))
        return Fable.Let([ident, tupleExpr], body)

    | CreateEvent (callee, eventName, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let! callee = transformExpr com ctx callee
        let! args = transformExprList com ctx membArgs
        let callee = get None Fable.Any callee eventName
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return makeCallFrom com ctx (makeRangeFrom fsExpr) typ false genArgs (Some callee) args memb

    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOf (BindIdent com ctx (newContext, ident), value, body) ->
        let! value = transformExpr com ctx value
        let! body = transformExpr com newContext body
        return Replacements.iterate (makeRangeFrom fsExpr) ident body value

    // Flow control
    | BasicPatterns.FastIntegerForLoop(start, limit, body, isUp) ->
        let r = makeRangeFrom fsExpr
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
        |> makeLoop (makeRangeFrom fsExpr)

    // Values
    | BasicPatterns.Const(value, FableType com ctx typ) ->
        return Replacements.makeTypeConst (makeRangeFrom fsExpr) typ value

    | BasicPatterns.BaseValue typ ->
        let typ = makeType com Map.empty typ
        match ctx.BoundMemberThis, ctx.BoundConstructorThis with
        | Some thisArg, _ | _, Some thisArg ->
            return { thisArg with Kind = Fable.BaseValueIdent; Type = typ } |> Fable.IdentExpr
        | _ ->
            addError com ctx.InlinePath (makeRangeFrom fsExpr) "Unexpected unbound this for base value"
            return Fable.Value(Fable.Null Fable.Any, None)

    | BasicPatterns.ThisValue typ ->
        let fail r msg =
            addError com ctx.InlinePath r msg
            Fable.Value(Fable.Null Fable.Any, None)
        // NOTE: We don't check ctx.BoundMemberThis here because F# compiler doesn't represent
        // `this` in members as BasicPatterns.ThisValue (but BasicPatterns.Value)
        return
            match typ, ctx.BoundConstructorThis with
            // When the type is a ref type, it means this is a reference to a constructor this value `type C() as x`
            | RefType _, _ ->
                let r = makeRangeFrom fsExpr
                match tryGetBoundExprWhere ctx r (fun fsRef -> fsRef.IsConstructorThisValue) with
                | Some e -> e
                | None -> fail r "Cannot find ConstructorThisValue"
            | _, Some thisArg -> Fable.IdentExpr thisArg
            | _ -> fail (makeRangeFrom fsExpr) "Unexpected unbound this"

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
    | CapturedBaseConsCall com ctx transformBaseConsCall nextExpr ->
        return! transformExpr com ctx nextExpr

    // TODO: `argTypes2` is always empty, asked about its purpose
    | BasicPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, _argTypes2, argExprs) ->
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return transformTraitCall com ctx (makeRangeFrom fsExpr) typ sourceTypes traitName flags argTypes argExprs

    | BasicPatterns.Call(callee, memb, ownerGenArgs, membGenArgs, args) ->
        checkArgumentsPassedByRef com ctx args
        let! callee = transformExprOpt com ctx callee
        let! args = transformExprList com ctx args
        // TODO: Check answer to #868 in FSC repo
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return makeCallFrom com ctx (makeRangeFrom fsExpr) typ false genArgs callee args memb

    | BasicPatterns.Application(applied, _genArgs, []) ->
        // TODO: Ask why application without arguments happen. So far I've seen it
        // to access None or struct values (like the Result type)
        return! transformExpr com ctx applied

    // Application of locally inlined lambdas
    | BasicPatterns.Application(BasicPatterns.Value var, genArgs, args) when isInline var ->
        let r = makeRangeFrom fsExpr
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
    | BasicPatterns.Application(BasicPatterns.Let((_, BasicPatterns.Call(None,m,_,_,[e1; e2])),_), _genArgs, args)
            when m.FullName = "Fable.Core.JsInterop.( ? )" ->
        let! e1 = transformExpr com ctx e1
        let! e2 = transformExpr com ctx e2
        let! args = transformExprList com ctx args
        let argInfo: Fable.ArgInfo =
            { argInfo (Some e1) args Fable.AutoUncurrying with Spread = Fable.TupleSpread }
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return Fable.Operation(Fable.Call(Fable.InstanceCall(Some e2), argInfo), typ, makeRangeFrom fsExpr)

    // TODO: Ask: for some reason the F# compiler translates `x.IsSome` as `Application(Call(x, get_IsSome),[unit])`
    | BasicPatterns.Application(BasicPatterns.Call(Some _, memb, _, [], []) as optionProp, genArgs, [BasicPatterns.Const(null, _)])
        when memb.FullName = "Microsoft.FSharp.Core.IsSome" || memb.FullName = "Microsoft.FSharp.Core.IsNone" ->
        return! transformExpr com ctx optionProp

    | BasicPatterns.Application(applied, _genArgs, args) ->
        let! applied = transformExpr com ctx applied
        let! args = transformExprList com ctx args
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return Fable.Operation(Fable.CurriedApply(applied, args), typ, makeRangeFrom fsExpr)

    | BasicPatterns.IfThenElse (guardExpr, thenExpr, elseExpr) ->
        let! guardExpr = transformExpr com ctx guardExpr
        let! thenExpr = transformExpr com ctx thenExpr
        let! fableElseExpr = transformExpr com ctx elseExpr

        let altElseExpr =
            match elseExpr with
            | RaisingMatchFailureExpr fileNameWhereErrorOccurs ->
                let errorMessage = "The match cases were incomplete"
                let rangeOfElseExpr = makeRangeFrom elseExpr
                let errorExpr = Replacements.Helpers.error (Fable.Value(Fable.StringConstant errorMessage, None))
                Fable.Throw(errorExpr, Fable.Any, rangeOfElseExpr)
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
    | BasicPatterns.AnonRecordGet(callee, calleeType, fieldIndex) ->
        let! callee = transformExpr com ctx callee
        let fieldName = calleeType.AnonRecordTypeDetails.SortedFieldNames.[fieldIndex]
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        let kind = Fable.FieldGet(fieldName, false, typ)
        return Fable.Get(callee, kind, typ, makeRangeFrom fsExpr)

    | BasicPatterns.FSharpFieldGet(callee, calleeType, field) ->
        let! callee = transformExprOpt com ctx callee
        let callee =
            match callee with
            | Some callee -> callee
            | None -> entityRef com calleeType.TypeDefinition
        let kind = Fable.FieldGet(field.Name, field.IsMutable, makeType com Map.empty field.FieldType)
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return Fable.Get(callee, kind, typ, makeRangeFrom fsExpr)

    | BasicPatterns.TupleGet(_tupleType, tupleElemIndex, tupleExpr) ->
        let! tupleExpr = transformExpr com ctx tupleExpr
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return Fable.Get(tupleExpr, Fable.TupleGet tupleElemIndex, typ, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseGet (unionExpr, fsType, unionCase, field) ->
        let r = makeRangeFrom fsExpr
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
            let typ = makeType com ctx.GenericArgs fsExpr.Type
            return Fable.Get(unionExpr, kind, typ, r)

    | BasicPatterns.FSharpFieldSet(callee, calleeType, field, value) ->
        let! callee = transformExprOpt com ctx callee
        let! value = transformExpr com ctx value
        let callee =
            match callee with
            | Some callee -> callee
            | None -> entityRef com calleeType.TypeDefinition
        return Fable.Set(callee, Fable.FieldSet(field.Name, makeType com Map.empty field.FieldType), value, makeRangeFrom fsExpr)

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
        return Fable.NewTuple(argExprs) |> makeValue (makeRangeFrom fsExpr)

    | BasicPatterns.ObjectExpr(objType, baseCall, overrides, otherOverrides) ->
        return! transformObjExpr com ctx objType baseCall overrides otherOverrides

    | BasicPatterns.NewObject(memb, genArgs, args) ->
        // TODO: Check arguments passed byref here too?
        let! args = transformExprList com ctx args
        let genArgs = Seq.map (makeType com ctx.GenericArgs) genArgs
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return makeCallFrom com ctx (makeRangeFrom fsExpr) typ false genArgs None args memb

    // work-around for optimized "for x in list" (erases this sequential)
    | BasicPatterns.Sequential (BasicPatterns.ValueSet (current, BasicPatterns.Value next1),
                                (BasicPatterns.ValueSet (next2, BasicPatterns.UnionCaseGet
                                    (_value, typ, unionCase, field))))
            when next1.FullName = "next" && next2.FullName = "next"
                && current.FullName = "current" && (getFsTypeFullName typ) = Types.list
                && unionCase.Name = "op_ColonColon" && field.Name = "Tail" ->
        // replace with nothing
        return Fable.UnitConstant |> makeValue None

    | BasicPatterns.Sequential (first, second) ->
        let! first = transformExpr com ctx first
        let! second = transformExpr com ctx second
        return Fable.Sequential [first; second]

    | BasicPatterns.NewRecord(fsType, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        let genArgs = makeGenArgs com ctx.GenericArgs (getGenericArguments fsType)
        return Fable.NewRecord(argExprs, Fable.DeclaredRecord fsType.TypeDefinition, genArgs) |> makeValue (makeRangeFrom fsExpr)

    | BasicPatterns.NewAnonRecord(fsType, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return Fable.NewRecord(argExprs, Fable.AnonymousRecord fsType.AnonRecordTypeDetails.SortedFieldNames, []) |> makeValue (makeRangeFrom fsExpr)

    | BasicPatterns.NewUnionCase(fsType, unionCase, argExprs) ->
        let! argExprs = transformExprList com ctx argExprs
        return argExprs
        |> transformNewUnion com ctx (makeRangeFrom fsExpr) fsType unionCase

    // Type test
    | BasicPatterns.TypeTest (FableType com ctx typ, expr) ->
        let! expr = transformExpr com ctx expr
        return Fable.Test(expr, Fable.TypeTest typ, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTest(unionExpr, fsType, unionCase) ->
        return! transformUnionCaseTest com ctx (makeRangeFrom fsExpr) unionExpr fsType unionCase

    // Pattern Matching
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        let! fableDecisionExpr = transformExpr com ctx decisionExpr
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

        let! fableDecisionTargets = transformDecisionTargets [] decisionTargets

        // rewrite last decision target if it throws MatchFailureException
        let compiledFableTargets =
            match snd (List.last decisionTargets) with
            | RaisingMatchFailureExpr fileNameWhereErrorOccurs ->
                match decisionExpr with
                | BasicPatterns.IfThenElse(BasicPatterns.UnionCaseTest(unionValue, unionType, unionCaseInfo), _, _) ->
                    let rangeOfLastDecisionTarget = makeRangeFrom (snd (List.last decisionTargets))
                    let errorMessage =
                        sprintf "The match cases were incomplete against type of '%s' at %s"
                            unionType.TypeDefinition.DisplayName
                            fileNameWhereErrorOccurs
                    let errorExpr = Replacements.Helpers.error (Fable.Value(Fable.StringConstant errorMessage, None))
                    // Creates a "throw Error({errorMessage})" expression
                    let throwExpr = Fable.Throw(errorExpr, Fable.Any, rangeOfLastDecisionTarget)

                    fableDecisionTargets
                    |> List.replaceLast (fun lastExpr -> [], throwExpr)

                | _ ->
                    // TODO: rewrite other `MatchFailureException` to `failwith "The match cases were incomplete"`
                    fableDecisionTargets

            | _ -> fableDecisionTargets

        return Fable.DecisionTree(fableDecisionExpr, compiledFableTargets)

    | BasicPatterns.DecisionTreeSuccess(targetIndex, boundValues) ->
        let! boundValues = transformExprList com ctx boundValues
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        return Fable.DecisionTreeSuccess(targetIndex, boundValues, typ)

    | BasicPatterns.ILFieldGet(None, ownerTyp, fieldName) ->
        let ownerTyp = makeType com ctx.GenericArgs ownerTyp
        let typ = makeType com ctx.GenericArgs fsExpr.Type
        match Replacements.tryField typ ownerTyp fieldName with
        | Some expr -> return expr
        | None ->
            return sprintf "Cannot compile ILFieldGet(%A, %s)" ownerTyp fieldName
            |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    | BasicPatterns.Quote _ ->
        return "Quotes are not currently supported by Fable"
        |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)

    // TODO: Ask. I see this when accessing Result types (all structs?)
    | BasicPatterns.AddressOf(expr) ->
        let! expr = transformExpr com ctx expr
        return expr

    // | BasicPatterns.ILFieldSet _
    // | BasicPatterns.AddressSet _
    // | BasicPatterns.ILAsm _
    | expr ->
        return sprintf "Cannot compile expression %A" expr
        |> addErrorAndReturnNull com ctx.InlinePath (makeRangeFrom fsExpr)
  }

/// Is compiler generated (CompareTo...) or belongs to ignored entity?
/// (remember F# compiler puts class methods in enclosing modules)
let private isIgnoredMember (meth: FSharpMemberOrFunctionOrValue) =
    (meth.IsCompilerGenerated && Naming.ignoredCompilerGenerated.Contains meth.CompiledName)
        || Option.isSome meth.LiteralValue
        || meth.Attributes |> Seq.exists (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Atts.erase | Atts.global_ | Atts.import | Atts.importAll | Atts.importDefault | Atts.importMember
                    | Atts.emit | Atts.emitMethod | Atts.emitConstructor | Atts.emitIndexer | Atts.emitProperty) -> true
            | _ -> false)
        || (match meth.DeclaringEntity with
            | Some ent -> isErasedEntity ent
            | None -> false)

let private transformImplicitConstructor com (ctx: Context)
            (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    match memb.DeclaringEntity with
    | None -> "Unexpected constructor without declaring entity: " + memb.FullName
              |> addError com ctx.InlinePath None; []
    | Some ent ->
        let bodyCtx, args = bindMemberArgs com ctx args
        let boundThis = makeIdentUnique com "this"
        let mutable baseCons = None
        let captureBaseCall =
            ent.BaseType |> Option.bind (fun (NonAbbreviatedType baseType) ->
                if baseType.HasTypeDefinition then
                    let ent = baseType.TypeDefinition
                    match ent.TryFullName with
                    | Some name when name <> Types.object ->
                        // TODO: Throw if assigning to baseCons more than once?
                        Some(ent, fun c -> baseCons <- Some c)
                    | _ -> None
                else None)
        let bodyCtx = { bodyCtx with BoundConstructorThis = Some boundThis
                                     CaptureBaseConsCall = captureBaseCall }
        let body = transformExpr com bodyCtx body |> run
        let baseExpr, body =
            match baseCons with
            | Some(baseExpr, baseCons) -> Some baseExpr, Fable.Sequential [baseCons; body]
            | None -> None, body
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
        // TODO!!! When adding a ConstructorDeclaration check if there are
        // name clashes for interface/abstract members
        let r = getEntityLocation ent |> makeRange
        [Fable.ConstructorDeclaration(Fable.ClassImplicitConstructor info, Some r)]

/// When using `importMember`, uses the member display name as selector
let private importExprSelector (memb: FSharpMemberOrFunctionOrValue) selector =
    match selector with
    | Fable.Value(Fable.StringConstant Naming.placeholder,_) ->
        getMemberDisplayName memb |> makeStrConst
    | _ -> selector

let private transformImport com r typ isMutable isPublic name selector path =
    if isMutable && isPublic then
        "Imported members cannot be mutable and public, please make it private: " + name
        |> addError com [] None
    let info: Fable.ValueDeclarationInfo =
        { Name = name
          IsPublic = isPublic
          // TODO: Check if they're mutable, see #1314
          IsMutable = isMutable
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
        transformImport com r typ memb.IsMutable isPublic name selector path
    | fableValue ->
        let info: Fable.ValueDeclarationInfo =
            { Name = name
              IsPublic = isPublic
              IsMutable = memb.IsMutable
              IsEntryPoint = false
              HasSpread = false }
        [Fable.ValueDeclaration(fableValue, info)]

let private functionDeclarationInfo name isPublic (memb: FSharpMemberOrFunctionOrValue): Fable.ValueDeclarationInfo =
    { Name = name
      IsPublic = isPublic
      IsMutable = memb.IsMutable
      IsEntryPoint = memb.Attributes |> hasAttribute Atts.entryPoint
      HasSpread = hasSeqSpread memb }

let private transformMemberFunction (com: IFableCompiler) ctx isPublic name (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let body = transformExpr com bodyCtx body |> run
    match body with
    // Accept import expressions, e.g. let foo x y = import "foo" "myLib"
    | Fable.Import(selector, path, Fable.CustomImport, _, r) ->
        // Use the full function type
        let typ = makeType com Map.empty memb.FullType
        let selector = importExprSelector memb selector
        transformImport com r typ false isPublic name selector path
    | body ->
        let fn = Fable.Function(Fable.Delegate args, body, Some name)
        // If this is a static constructor, call it immediately
        if memb.CompiledName = ".cctor" then
            let apply = staticCall None Fable.Unit (argInfo None [] Fable.NoUncurrying) fn
            [Fable.ActionDeclaration apply]
        else
            let info = functionDeclarationInfo name isPublic memb
            [Fable.ValueDeclaration(fn, info)]

let private transformMemberFunctionOrValue (com: IFableCompiler) ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let isPublic = isPublicMember memb
    let name = getMemberDeclarationName com memb
    com.AddUsedVarName(name)
    match memb.Attributes with
    | ImportAtt(selector, path) ->
        let selector =
            if selector = Naming.placeholder then getMemberDisplayName memb
            else selector
        let typ = makeType com Map.empty memb.FullType
        transformImport com None typ memb.IsMutable isPublic name (makeStrConst selector) (makeStrConst path)
    | EmitDeclarationAtt macro ->
        let typ = makeType com Map.empty memb.FullType
        let info = functionDeclarationInfo name isPublic memb
        [Fable.ValueDeclaration(Fable.Operation(Fable.Emit(macro, None), typ, None), info)]
    | NoAtt ->
        if isModuleValueForDeclarations memb
        then transformMemberValue com ctx isPublic name memb body
        else transformMemberFunction com ctx isPublic name memb args body

let private transformAttachedMember (com: FableCompiler) (ctx: Context)
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
        let info: Fable.AttachedMemberDeclarationInfo =
            { Name = memb.DisplayName
              Kind = kind
              EntityName = getEntityDeclarationName com ent }
        [Fable.AttachedMemberDeclaration(args, body, info)]

let private transformMemberDecl (com: FableCompiler) (ctx: Context) (memb: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let ctx = { ctx with EnclosingMember = Some memb }
    if isIgnoredMember memb then
        if memb.IsMutable && isPublicMember memb && hasAttribute Atts.global_ memb.Attributes then
            "Global members cannot be mutable and public, please make it private: " + memb.DisplayName
            |> addError com [] None
        []
    elif isInline memb then
        let inlineExpr = { Args = List.concat args
                           Body = body
                           FileName = (com :> ICompiler).CurrentFile }
        com.AddInlineExpr(memb, inlineExpr)
        if com.Options.outputPublicInlinedFunctions && isPublicMember memb then
            transformMemberFunctionOrValue com ctx memb args body
        else []
    elif memb.IsImplicitConstructor
    then transformImplicitConstructor com ctx memb args body
    elif memb.IsExplicitInterfaceImplementation then
        if Set.contains memb.CompiledName Naming.interfaceMethodsImplementedInPrototype
        then transformAttachedMember com ctx memb args body
        else
            match tryGetInterfaceDefinitionFromMethod memb with
            | Some interfaceEntity when not(Naming.ignoredInterfaces.Contains interfaceEntity.FullName) ->
                transformAttachedMember com ctx memb args body
            | _ -> []
    elif memb.IsOverrideOrExplicitInterfaceImplementation
    then transformAttachedMember com ctx memb args body
    else transformMemberFunctionOrValue com ctx memb args body

// In case this is a recursive module, do a first pass to add
// all entity and member names as used var names
let rec checkMemberNames (com: FableCompiler) decls =
    for decl in decls do
        match decl with
        | FSharpImplementationFileDeclaration.Entity(ent, sub) ->
            match sub with
            | [] when ent.IsFSharpAbbreviation -> ()
            | [] -> com.AddUsedVarName(getEntityDeclarationName com ent, isRoot=true)
            | sub -> checkMemberNames com sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memb,_,_) ->
            if not(isIgnoredMember memb) then
                let memberName = getMemberDeclarationName com memb
                com.AddUsedVarName(memberName, isRoot=true)
        | FSharpImplementationFileDeclaration.InitAction _ -> ()

let private transformDeclarations (com: FableCompiler) ctx rootEnt rootDecls =
    let rec transformDeclarationsInner (com: FableCompiler) (ctx: Context) fsDecls =
        fsDecls |> List.collect (fun fsDecl ->
            match fsDecl with
            | FSharpImplementationFileDeclaration.Entity(ent, sub) ->
                match ent.Attributes with
                | ImportAtt(selector, path) ->
                    let selector =
                        if selector = Naming.placeholder then ent.DisplayName
                        else selector
                    let name = getEntityDeclarationName com ent
                    com.AddUsedVarName(name)
                    (makeStrConst selector, makeStrConst path)
                    ||> transformImport com None Fable.Any false (not ent.Accessibility.IsPrivate) name
                | _ when isErasedEntity ent ->
                    []
                | _ when ent.IsFSharpUnion ->
                    let entityName = getEntityDeclarationName com ent
                    com.AddUsedVarName(entityName)
                    // TODO: Check Equality/Comparison attributes
                    let info: Fable.UnionConstructorInfo =
                      { Entity = ent
                        EntityName = entityName
                        IsPublic = isPublicEntity ent }
                    let r = getEntityLocation ent |> makeRange
                    [Fable.ConstructorDeclaration(Fable.UnionConstructor info, Some r)]
                | _ when ent.IsFSharpRecord
                        || ent.IsFSharpExceptionDeclaration
                        || ((ent.IsClass || ent.IsValueType) && not ent.IsMeasure && not (hasImplicitConstructor ent)) ->
                    let entityName = getEntityDeclarationName com ent
                    com.AddUsedVarName(entityName)
                    // TODO: Check Equality/Comparison attributes
                    let info: Fable.CompilerGeneratedConstructorInfo =
                      { Entity = ent
                        EntityName = entityName
                        IsPublic = isPublicEntity ent }
                    let r = getEntityLocation ent |> makeRange
                    [Fable.ConstructorDeclaration(Fable.CompilerGeneratedConstructor info, Some r)]
                | _ ->
                    transformDeclarationsInner com { ctx with EnclosingEntity = Some ent } sub
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(meth, args, body) ->
                transformMemberDecl com ctx meth args body
            | FSharpImplementationFileDeclaration.InitAction fe ->
                [transformExpr com ctx fe |> run |> Fable.ActionDeclaration])

    checkMemberNames com rootDecls
    transformDeclarationsInner com ctx rootDecls

let private getRootModuleAndDecls decls =
    let rec getRootModuleAndDeclsInner outerEnt decls =
        match decls with
        | [FSharpImplementationFileDeclaration.Entity (ent, decls)]
                when ent.IsFSharpModule || ent.IsNamespace ->
            getRootModuleAndDeclsInner (Some ent) decls
        | CommonNamespace(ent, decls) ->
            getRootModuleAndDeclsInner (Some ent) decls
        | decls -> outerEnt, decls
    getRootModuleAndDeclsInner None decls

let private tryGetMemberArgsAndBody com (implFiles: IDictionary<string, FSharpImplementationFileContents>)
                                    fileName entityFullName memberUniqueName =
    let rec tryGetMemberArgsAndBodyInner (entityFullName: string) (memberUniqueName: string) = function
        | FSharpImplementationFileDeclaration.Entity (e, decls) ->
            let entityFullName2 = getEntityFullName e
            if entityFullName.StartsWith(entityFullName2)
            then List.tryPick (tryGetMemberArgsAndBodyInner entityFullName memberUniqueName) decls
            else None
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (memb2, args, body) ->
            if getMemberUniqueName com memb2 = memberUniqueName
            then Some(args, body)
            else None
        | FSharpImplementationFileDeclaration.InitAction _ -> None
    match implFiles.TryGetValue(fileName) with
    | true, f -> f.Declarations |> List.tryPick (tryGetMemberArgsAndBodyInner entityFullName memberUniqueName)
    | false, _ -> None

type FableCompiler(com: ICompiler, implFiles: IDictionary<string, FSharpImplementationFileContents>) =
    member val UsedVarNames = HashSet<string>()
    member val InlineDependencies = HashSet<string>()
    member __.Options = com.Options
    member this.AddUsedVarName(varName, ?isRoot) =
        let isRoot = defaultArg isRoot false
        let success = this.UsedVarNames.Add(varName)
        if not success && isRoot then
            sprintf "Cannot have two module members with same name: %s" varName
            |> addError com [] None
    member __.AddInlineExpr(memb, inlineExpr: InlineExpr) =
        let fullName = getMemberUniqueName com memb
        com.GetOrAddInlineExpr(fullName, fun () -> inlineExpr) |> ignore
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
                let fileName =
                    (getMemberLocation memb).FileName
                    |> Path.normalizePathAndEnsureFsExtension
                if fileName <> com.CurrentFile then
                    // TODO: Add literal values as InlineDependencies too?
                    this.InlineDependencies.Add(fileName) |> ignore
                com.GetOrAddInlineExpr(membUniqueName, fun () ->
                    match tryGetMemberArgsAndBody com implFiles fileName entFullName membUniqueName with
                    | Some(args, body) ->
                        { Args = List.concat args
                          Body = body
                          FileName = fileName }
                    | None -> failwith ("Cannot find inline member. Please report: " + membUniqueName))
        member this.AddUsedVarName(varName, ?isRoot) =
            this.AddUsedVarName(varName, ?isRoot=isRoot)
        member this.IsUsedVarName(varName) =
            this.UsedVarNames.Contains(varName)
    interface ICompiler with
        member __.Options = com.Options
        member __.LibraryDir = com.LibraryDir
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

let transformFile (com: ICompiler) (implFiles: IDictionary<string, FSharpImplementationFileContents>) =
    try
        let file =
            match implFiles.TryGetValue(com.CurrentFile) with
            | true, file -> file
            | false, _ ->
                let projFiles = implFiles |> Seq.map (fun kv -> kv.Key) |> String.concat "\n"
                failwithf "File %s cannot be found in source list:\n%s" com.CurrentFile projFiles
        let rootEnt, rootDecls = getRootModuleAndDecls file.Declarations
        let fcom = FableCompiler(com, implFiles)
        let ctx = Context.Create(rootEnt)
        let rootDecls = transformDeclarations fcom ctx rootEnt rootDecls
        Fable.File(com.CurrentFile, rootDecls, set fcom.UsedVarNames, set fcom.InlineDependencies)
    with
    | ex -> exn (sprintf "%s (%s)" ex.Message com.CurrentFile, ex) |> raise
