module Fable.FSharp2Fable.Compiler

#if !FABLE_COMPILER
open System.IO
#endif
open System.Collections.Generic
open System.Text.RegularExpressions

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
        | Some "System.DateTime", "MaxValue"
        | Some "System.DateTime", "MinValue" ->
            CoreLibCall("Date", Some (Naming.lowerFirst fieldName), false, [])
            |> makeCall (makeRangeFrom fsExpr) (makeType com ctx.typeArgs fsExpr.Type) |> Some
        | _ -> None
    | _ -> None

let private (|BaseCons|_|) com ctx (fsExpr: FSharpExpr) =
    let equalsEntName meth entName =
        match tryEnclosingEntity meth with
        | Some ent -> (sanitizeEntityFullName ent) = entName
        | None -> false
    let validateGenArgs' typArgs =
        tryDefinition fsExpr.Type |> Option.iter (fun tdef ->
            validateGenArgs ctx (makeRangeFrom fsExpr) tdef.GenericParameters typArgs)
    match fsExpr with
    | BasicPatterns.NewObject(meth, typArgs, args) ->
        match ctx.baseClass with
        | Some baseFullName when equalsEntName meth baseFullName ->
            validateGenArgs' typArgs
            Some (meth, args)
        | _ -> None
    | BasicPatterns.Call(None, meth, typArgs, _, args) ->
        match ctx.baseClass with
        | Some baseFullName when meth.CompiledName = ".ctor" && equalsEntName meth baseFullName ->
            if not meth.IsImplicitConstructor then
                FableError("Inheritance is only possible with base class primary constructor: "
                            + baseFullName, makeRange fsExpr.Range) |> raise
            validateGenArgs' typArgs
            Some (meth, args)
        | _ -> None
    | _ -> None

let rec private transformNewList com ctx (fsExpr: FSharpExpr) fsType argExprs =
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

and private transformNonListNewUnionCase com ctx (fsExpr: FSharpExpr) fsType unionCase argExprs =
    let unionType, range = makeType com ctx.typeArgs fsType, makeRange fsExpr.Range
    match fsType with
    | OptionUnion ->
        match argExprs: Fable.Expr list with
        // Represent `Some ()` with an empty object, see #478
        | expr::_ when expr.Type = Fable.Unit ->
            Fable.Wrapped(Fable.ObjExpr([], [], None, Some range), unionType)
        | expr::_ -> Fable.Wrapped(expr, unionType)
        | _ -> Fable.Wrapped(Fable.Value Fable.Null, unionType)
    | ErasedUnion ->
        match argExprs with
        | [] -> Fable.Wrapped(Fable.Value Fable.Null, unionType)
        | [expr] -> Fable.Wrapped(expr, unionType)
        | _ -> FableError("Erased Union Cases must have one single field: " + unionType.FullName, range) |> raise
    | StringEnum ->
        if not(List.isEmpty argExprs) then
            FableError("StringEnum types cannot have fields", range) |> raise
        lowerCaseName unionCase
    | PojoUnion ->
        List.zip (Seq.toList unionCase.UnionCaseFields) argExprs
        |> List.map (fun (fi, e) -> fi.Name, e)
        |> List.append ["type", makeStrConst unionCase.Name]
        |> makeJsObject (Some range)
    | ListUnion ->
        failwithf "transformNonListNewUnionCase must not be used with List %O" range
    | OtherType ->
        let argExprs =
            let tag = getUnionCaseIndex fsExpr.Range fsType unionCase.Name |> makeIntConst
            match argExprs with
            | [] -> [tag]
            | argExprs -> [tag; Fable.Value(Fable.ArrayConst(Fable.ArrayValues argExprs, Fable.Any))]
        buildApplyInfo com ctx (Some range) unionType unionType (unionType.FullName)
            ".ctor" Fable.Constructor ([],[],[],[]) (None, argExprs)
        |> tryBoth (tryPlugin com) (tryReplace com (tryDefinition fsType))
        |> function
        | Some repl -> repl
        | None -> Fable.Apply(makeNonGenTypeRef com unionType, argExprs, Fable.ApplyCons, unionType, Some range)

and private transformComposableExpr com ctx fsExpr argExprs =
    // See (|ComposableExpr|_|) active pattern to check which expressions are valid here
    match fsExpr with
    | BasicPatterns.Call(None, meth, typArgs, methTypArgs, _) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) None argExprs
    | BasicPatterns.NewObject(meth, typArgs, _) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        tryDefinition fsExpr.Type |> Option.iter (fun tdef ->
            validateGenArgs ctx r tdef.GenericParameters typArgs)
        makeCallFrom com ctx r typ meth (typArgs, []) None argExprs
    | BasicPatterns.NewUnionCase(fsType, unionCase, _) ->
        transformNonListNewUnionCase com ctx fsExpr fsType unionCase argExprs
    | _ -> failwithf "Expected ComposableExpr %O" (makeRange fsExpr.Range)

and private transformExpr (com: IFableCompiler) ctx fsExpr =
    match fsExpr with
    (** ## Custom patterns *)
    | SpecialValue com ctx replacement ->
        replacement

    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOf (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        Fable.ForOf (ident, value, transformExpr com newContext body)
        |> makeLoop (makeRangeFrom fsExpr)

    | ErasableLambda (expr, argExprs) ->
        List.map (transformExpr com ctx) argExprs
        |> transformComposableExpr com ctx expr

    // Pipe must come after ErasableLambda
    | Pipe (Transform com ctx callee, args) ->
        let typ, range = makeType com ctx.typeArgs fsExpr.Type, makeRangeFrom fsExpr
        makeApply com range typ callee (List.map (transformExpr com ctx) args)

    | Composition (expr1, args1, expr2, args2) ->
        let lambdaArg = com.GetUniqueVar() |> makeIdent
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        let expr1 =
            (List.map (transformExpr com ctx) args1)
                @ [Fable.Value (Fable.IdentValue lambdaArg)]
            |> transformComposableExpr com ctx expr1
        let expr2 =
            (List.map (transformExpr com ctx) args2)@[expr1]
            |> transformComposableExpr com ctx expr2
        makeLambdaExpr [lambdaArg] expr2

    | BaseCons com ctx (meth, args) ->
        let args = List.map (transformExpr com ctx) args
        let typ, range = makeType com ctx.typeArgs fsExpr.Type, makeRangeFrom fsExpr
        let superCall = Fable.Apply(Fable.Value Fable.Super, args, Fable.ApplyMeth, typ, range)
        if ctx.baseClass = Some "System.Exception"
        then Fable.Sequential([superCall; setProto com ctx.enclosingEntity], range)
        else superCall

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

    | PrintFormat (Transform com ctx expr) -> expr

    | Applicable (Transform com ctx expr) ->
        let appType =
            let ent = Fable.Entity(lazy Fable.Interface, None, "Fable.Core.Applicable", lazy [])
            Fable.DeclaredType(ent, [Fable.Any; Fable.Any])
        Fable.Wrapped(expr, appType)

    | JsThis ->
        if ctx.thisAvailability <> ThisUnavailable then
            "JS `this` is already captured in this context, try to use it in a module function"
            |> addWarning com ctx.fileName (makeRange fsExpr.Range |> Some)
        Fable.Value Fable.This

    (** ## Erased *)
    | BasicPatterns.Coerce(_targetType, Transform com ctx inpExpr) -> inpExpr
    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, Transform com ctx lambda) -> lambda

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
        if e.Type = typ then e
        // Enumerations are compiled as const but they have a different type
        else Fable.Wrapped (e, typ)

    | BasicPatterns.BaseValue typ ->
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
            | None -> FableError("Cannot resolve locally inlined value: " + var.DisplayName, makeRange fsExpr.Range) |> raise
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
            let value = transformExpr com ctx value
            let ctx, ident = bindIdent com ctx value.Type (Some var) var.CompiledName
            let body = transformExpr com ctx body
            let assignment = Fable.VarDeclaration (ident, value, var.IsMutable)
            makeSequential (makeRangeFrom fsExpr) [assignment; body]

    | BasicPatterns.LetRec(recBindings, body) ->
        let ctx, idents =
            (recBindings, (ctx, [])) ||> List.foldBack (fun (var,_) (ctx, idents) ->
                let (BindIdent com ctx (newContext, ident)) = var
                (newContext, ident::idents))
        let assignments =
            recBindings
            |> List.map2 (fun ident (var, Transform com ctx binding) ->
                Fable.VarDeclaration (ident, binding, var.IsMutable)) idents
        assignments @ [transformExpr com ctx body]
        |> makeSequential (makeRangeFrom fsExpr)

    (** ## Applications *)
    | BasicPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, _argTypes2, argExprs) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        let giveUp() =
            FableError("Cannot resolve trait call " + traitName, ?range=r) |> raise
        let (|ResolveGeneric|_|) genArgs (t: FSharpType) =
            if not t.IsGenericParameter then Some t else
            let genParam = t.GenericParameter
            genArgs |> Seq.tryPick (fun (name,t) ->
                if name = genParam.Name then Some t else None)
        let makeCall meth =
            let callee, args =
                if flags.IsInstance
                then
                    (transformExpr com ctx argExprs.Head |> Some),
                    (List.map (transformExpr com ctx) argExprs.Tail)
                else None, List.map (transformExpr com ctx) argExprs
            makeCallFrom com ctx r typ meth ([],[]) callee args
        sourceTypes
        |> List.tryPick (function
            | ResolveGeneric ctx.typeArgs (TypeDefinition tdef as typ) ->
                tdef.MembersFunctionsAndValues |> Seq.filter (fun m ->
                    // Property members that are no getter nor setter don't actually get implemented
                    not(m.IsProperty && not(m.IsPropertyGetterMethod || m.IsPropertySetterMethod))
                    && m.IsInstanceMember = flags.IsInstance
                    && m.CompiledName = traitName)
                |> Seq.toList |> function [] -> None | ms -> Some (typ, tdef, ms)
            | _ -> None)
        |> function
        | Some(_, _, [meth]) ->
            makeCall meth
        | Some(typ, tdef, candidates) ->
            let genArgs =
                if tdef.GenericParameters.Count = typ.GenericArguments.Count
                then Seq.zip (tdef.GenericParameters |> Seq.map (fun p -> p.Name)) typ.GenericArguments |> Seq.toArray |> Some
                else None
            let argTypes =
                if not flags.IsInstance then argTypes else argTypes.Tail
                |> List.map (makeType com ctx.typeArgs)
            candidates |> List.tryPick (fun meth ->
                let methTypes =
                    // FSharpParameters don't contain the `this` arg
                    // The F# compiler "untuples" the args in methods
                    let methTypes = Seq.concat meth.CurriedParameterGroups |> Seq.map (fun x -> x.Type)
                    match genArgs with
                    | Some genArgs -> methTypes |> Seq.map (function ResolveGeneric genArgs x -> x | x -> x)
                    | None -> methTypes
                    |> Seq.map (makeType com [])
                    |> Seq.toList
                if compareConcreteAndGenericTypes argTypes methTypes
                then Some meth else None)
            |> function Some m -> makeCall m | None -> giveUp()
        | None -> giveUp()

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
            let typ = makeType com ctx.typeArgs fsExpr.Type
            let args = List.map (transformExpr com ctx) args
            let resolvedCtx = { ctx with typeArgs = matchGenericParams com ctx var ([], typeArgs) }
            let callee = com.Transform resolvedCtx fsExpr
            makeApply com (Some range) typ callee args
        | None ->
            FableError("Cannot resolve locally inlined value: " + var.DisplayName, range) |> raise

    | FlattenedApplication(Transform com ctx callee, typeArgs, args) ->
        // So far I've only seen application without arguments when accessing None values
        if args.Length = 0 then callee else
        let typ, range = makeType com ctx.typeArgs fsExpr.Type, makeRangeFrom fsExpr
        let args = List.map (transformExpr com ctx) args
        if callee.Type.FullName = "Fable.Core.Applicable" then
            let args =
                match args with
                | [Fable.Value(Fable.TupleConst args)] -> args
                | args -> args
            Fable.Apply(callee, args, Fable.ApplyMeth, typ, range)
        else makeApply com range typ callee args

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
        let wrapInZeroArgsLambda r typ (args: FSharpExpr list) fref =
            let args = List.map (transformExpr com ctx) args
            let captureThis = ctx.thisAvailability <> ThisUnavailable
            let body =
                Fable.Apply(fref, args, Fable.ApplyMeth, typ, r)
            Fable.Lambda([], body, captureThis) |> Fable.Value
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
        | fsExpr -> transformExpr com ctx fsExpr

    | FlattenedLambda(args, tupleDestructs, body) ->
        let rec getNestedLambdas acc = function
            | Fable.Function(args, returnType) -> getNestedLambdas ((List.length args)::acc) returnType
            | _ -> acc
        let ctx, args = makeLambdaArgs com ctx args
        let ctx =
            (ctx, tupleDestructs)
            ||> List.fold (fun ctx (var, value) ->
                transformExpr com ctx value |> bindExpr ctx var)
        let captureThis = ctx.thisAvailability <> ThisUnavailable
        let body = transformExpr com { ctx with isLambdaBody = true } body
        let lambda = Fable.Lambda(args, body, captureThis) |> Fable.Value
        if ctx.isLambdaBody
        then lambda
        else
            let nestedLambdas = getNestedLambdas [List.length args] body.Type
            if List.length nestedLambdas <= 2
            then lambda
            else
                let args =
                    [ yield List.rev nestedLambdas
                            |> List.map makeIntConst
                            |> makeArray Fable.Any
                      yield lambda
                      if captureThis then yield Fable.Value Fable.This ]
                CoreLibCall("CurriedLambda", None, false, args)
                |> makeCall (makeRangeFrom fsExpr) lambda.Type

    (** ## Getters and Setters *)
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
        | ErasedUnion | OptionUnion ->
            Fable.Wrapped(unionExpr, typ)
        | ListUnion ->
            makeGet range typ unionExpr (Naming.lowerFirst fieldName |> makeStrConst)
        | PojoUnion ->
            makeStrConst fieldName |> makeGet range typ unionExpr
        | StringEnum ->
            FableError("StringEnum types cannot have fields", ?range=range) |> raise
        | OtherType ->
            let i = unionCase.UnionCaseFields |> Seq.findIndex (fun x -> x.Name = fieldName)
            let fields = makeGet range typ unionExpr (makeStrConst "fields")
            makeGet range typ fields (makeIntConst i)

    | BasicPatterns.ILFieldSet (callee, typ, fieldName, value) ->
        failwithf "Unsupported ILField reference %O: %A" (makeRange fsExpr.Range) fsExpr

    | BasicPatterns.FSharpFieldSet (callee, FableType com ctx calleeType, FieldName fieldName, Transform com ctx value) ->
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeNonGenTypeRef com calleeType
        Fable.Set (callee, Some (makeStrConst fieldName), value, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTag (Transform com ctx unionExpr, _unionType) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        makeGetFrom r typ unionExpr (makeStrConst "tag")

    | BasicPatterns.UnionCaseSet (Transform com ctx unionExpr, _type, _case, _caseField, _valueExpr) ->
        makeRange fsExpr.Range |> failwithf "Unexpected UnionCaseSet %O"

    | BasicPatterns.ValueSet (valToSet, Transform com ctx valueExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs valToSet.FullType
        let valToSet = makeValueFrom com ctx r typ false valToSet
        Fable.Set (valToSet, None, valueExpr, r)

    (** Instantiation *)
    | BasicPatterns.NewArray(FableType com ctx elTyp, arrExprs) ->
        makeArray elTyp (arrExprs |> List.map (transformExpr com ctx))

    | BasicPatterns.NewTuple(_, argExprs) ->
        argExprs |> List.map (transformExpr com ctx) |> Fable.TupleConst |> Fable.Value

    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, otherOverrides) ->
        // If `this` is available, capture it to avoid conflicts (see #158)
        let capturedThis =
            match ctx.thisAvailability with
            | ThisUnavailable -> None
            | ThisAvailable -> Some [None, com.GetUniqueVar() |> makeIdentExpr]
            | ThisCaptured(prevThis, prevVars) ->
                (prevThis, com.GetUniqueVar() |> makeIdentExpr)::prevVars |> Some
        let baseClass, baseCons =
            match baseCallExpr with
            | BasicPatterns.Call(None, meth, _, _, args) ->
                let args = List.map (com.Transform ctx) args
                let typ, range = makeType com ctx.typeArgs baseCallExpr.Type, makeRange baseCallExpr.Range
                let baseClass =
                    tryEnclosingEntity meth
                    |> Option.map (fun ent ->
                        makeTypeFromDef com ctx.typeArgs ent []
                        |> makeNonGenTypeRef com)
                let baseCons =
                    let c = Fable.Apply(Fable.Value Fable.Super, args, Fable.ApplyMeth, typ, Some range)
                    let m = Fable.Member(".ctor", Fable.Constructor, Fable.InstanceLoc, [], Fable.Any)
                    Some(m, [], c)
                baseClass, baseCons
            | _ -> None, None
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
                            typ.MembersFunctionsAndValues
                            |> Seq.tryFind (fun x -> x.CompiledName = over.Signature.Name)
                            |> function Some m -> hasRestParams m | None -> false
                    let body = transformExpr com ctx over.Body
                    let args = List.map Fable.Ident.getType args'
                    let m = Fable.Member(name, kind, Fable.InstanceLoc, args, body.Type, Fable.Function(args, body.Type),
                                over.GenericParameters |> List.map (fun x -> x.Name),
                                hasRestParams = hasRestParams)
                    m, args', body))
        let interfaces =
            objType::(otherOverrides |> List.map fst)
            |> List.map (fun x -> sanitizeEntityFullName x.TypeDefinition)
            |> List.filter (Naming.ignoredInterfaces.Contains >> not)
            |> List.distinct
        let members =
            [ match baseCons with Some c -> yield c | None -> ()
              yield! members
              if List.contains "System.Collections.Generic.IEnumerable" interfaces
              then yield makeIteratorMethodArgsAndBody()
              yield makeReflectionMethodArgsAndBody com None false false interfaces None None
            ]
        let range = makeRangeFrom fsExpr
        let objExpr = Fable.ObjExpr (members, interfaces, baseClass, range)
        match capturedThis with
        | Some((_,Fable.Value(Fable.IdentValue capturedThis))::_) ->
            let varDecl = Fable.VarDeclaration(capturedThis, Fable.Value Fable.This, false)
            Fable.Sequential([varDecl; objExpr], range)
        | _ -> objExpr

    | BasicPatterns.NewObject(meth, typArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        tryDefinition fsExpr.Type |> Option.iter (fun tdef ->
            validateGenArgs ctx r tdef.GenericParameters typArgs)
        List.map (com.Transform ctx) args
        |> makeCallFrom com ctx r typ meth (typArgs, []) None

    | BasicPatterns.NewRecord(fsType, argExprs) ->
        let range = makeRangeFrom fsExpr
        let argExprs = argExprs |> List.map (transformExpr com ctx)
        match tryDefinition fsType with
        | Some tdef when tdef.Attributes |> hasAtt Atts.pojo ->
            List.zip (Seq.toList tdef.FSharpFields) argExprs
            |> List.map (fun (fi, e) -> fi.Name, e)
            |> makeJsObject range
        | _ ->
            let recordType = makeType com ctx.typeArgs fsType
            buildApplyInfo com ctx range recordType recordType (recordType.FullName)
                ".ctor" Fable.Constructor ([],[],[],[]) (None, argExprs)
            |> tryBoth (tryPlugin com) (tryReplace com (tryDefinition fsType))
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
        makeTypeTest com (makeRangeFrom fsExpr) typ expr

    | BasicPatterns.UnionCaseTest(Transform com ctx unionExpr, NonAbbreviatedType fsType, unionCase) ->
        let checkCase propName right =
            let left = makeGet None Fable.String unionExpr (makeStrConst propName)
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [left; right] BinaryEqualStrict
        match fsType with
        | ErasedUnion ->
            let unionName = defaultArg fsType.TypeDefinition.TryFullName "unknown"
            if unionCase.UnionCaseFields.Count <> 1 then
                FableError("Erased Union Cases must have one single field: " + unionName, makeRange fsExpr.Range) |> raise
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
                |> makeTypeTest com (makeRangeFrom fsExpr) <| unionExpr
        | OptionUnion ->
            let opKind = if unionCase.Name = "None" then BinaryEqual else BinaryUnequal
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; Fable.Value Fable.Null] opKind
        | ListUnion ->
            let opKind = if unionCase.CompiledName = "Empty" then BinaryEqual else BinaryUnequal
            let expr = makeGet None Fable.Any unionExpr (makeStrConst "tail")
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [expr; Fable.Value Fable.Null] opKind
        | StringEnum ->
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; lowerCaseName unionCase] BinaryEqualStrict
        | PojoUnion ->
            makeStrConst unionCase.Name |> checkCase "type"
        | OtherType ->
            getUnionCaseIndex fsExpr.Range fsType unionCase.Name
            |> makeIntConst
            |> checkCase "tag"

    (** Pattern Matching *)
    | Switch(matchValue, cases, defaultCase, decisionTargets) ->
        let matchValueType = makeType com ctx.typeArgs matchValue.FullType
        let matchValue = makeValueFrom com ctx None matchValueType false matchValue
        let cases =
            cases
            |> Seq.map (fun kv ->
                let labels = List.map (makeTypeConst matchValueType) kv.Value
                let body = snd decisionTargets.[kv.Key] |> transformExpr com ctx
                labels, body)
            |> Seq.toList
        let defaultCase =
            snd decisionTargets.[defaultCase] |> transformExpr com ctx
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.typeArgs fsExpr.Type
        Fable.Switch(matchValue, cases, Some defaultCase, typ, r)

    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
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
            [ Fable.VarDeclaration(tempVar, transformExpr com ctx decisionExpr, false)
            ; Fable.Switch(tempVarFirstItem, cases, None, typ, r) ]
            |> makeSequential r
        else
            let targets = targetRefsCount |> Map.map (fun k _ -> decisionTargets.[k])
            let ctx = { ctx with decisionTargets = Some targets }
            transformExpr com ctx decisionExpr

    | BasicPatterns.DecisionTreeSuccess (decIndex, decBindings) ->
        match ctx.decisionTargets with
        | Some decisionTargets ->
            match Map.tryFind decIndex decisionTargets with
            | None -> failwithf "Missing decision target %O" (makeRange fsExpr.Range)
            | Some ([], Transform com ctx decBody) -> decBody
            // If we have values, bind them to context
            | Some (decVars, decBody) ->
                let ctx =
                    (ctx, decVars, decBindings)
                    |||> List.fold2 (fun ctx var (Transform com ctx binding) ->
                        bindExpr ctx var binding)
                transformExpr com ctx decBody
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
    | BasicPatterns.AddressOf _ // (lvalueExpr)
    | BasicPatterns.AddressSet _ // (lvalueExpr, rvalueExpr)
    | _ -> failwithf "Cannot compile expression in %O: %A"
                     (makeRange fsExpr.Range) fsExpr

let private processMemberDecls (com: IFableCompiler) ctx (fableEnt: Fable.Entity) (childDecls: #seq<Fable.Declaration>) =
    if fableEnt.Kind = Fable.Module then Seq.toList childDecls else
    // If F# union or records implement System.IComparable/System.Equatable generate the methods
    // Note: F# compiler generates these methods too but see `IsIgnoredMethod`
    let needsEqImpl =
        fableEnt.HasInterface "System.IEquatable"
        && fableEnt.TryGetFullDecorator("Microsoft.FSharp.Core.CustomEquality").IsNone
    let needsCompImpl =
        fableEnt.HasInterface "System.IComparable"
        && fableEnt.TryGetFullDecorator("Microsoft.FSharp.Core.CustomComparison").IsNone
    let nullable =
        fableEnt.TryGetFullDecorator("Microsoft.FSharp.Core.AllowNullLiteral").IsSome
    let fableType =
        Fable.DeclaredType(fableEnt, fableEnt.GenericParameters |> List.map Fable.GenericParam)
    // Unions, records and F# exceptions don't have a constructor
    match fableEnt.Kind with
    | Fable.Union cases ->
      [ yield makeUnionCons cases
        yield makeReflectionMethod com fableEnt false nullable (Some cases) None
        if needsEqImpl then yield makeUnionEqualMethod fableType
        if needsCompImpl then yield makeUnionCompareMethod fableType ]
    | Fable.Record fields
    | Fable.Exception fields ->
      // Structs are considered equivalent to records but
      // some already include a constructor (see #569)
      [ if fableEnt.Members |> Seq.exists (fun m -> m.Kind = Fable.Constructor) |> not
        then yield makeRecordCons com fableEnt fields
        yield makeReflectionMethod com fableEnt false nullable None (Some fields)
        if needsEqImpl then yield makeRecordEqualMethod fableType
        if needsCompImpl then yield makeRecordCompareMethod fableType ]
    | Fable.Class(baseClass, properties) ->
      [makeReflectionMethod com fableEnt baseClass.IsSome nullable None (Some properties)]
    | _ -> []
    |> fun autoMeths ->
        [ yield! autoMeths
          if fableEnt.HasInterface "System.Collections.Generic.IEnumerable"
          then yield makeIteratorMethod()
          yield! childDecls ]

// The F# compiler considers class methods as children of the enclosing module.
// We use this type to correct that, see type DeclInfo below.
type private TmpDecl =
    | Decl of Fable.Declaration
    | Ent of Fable.Entity * string * ResizeArray<Fable.Declaration> * SourceLocation option
    | IgnoredEnt

type private DeclInfo() =
    let publicNames = ResizeArray<string>()
    // Check there're no conflicting entity or function names (see #166)
    let checkPublicNameConflicts name =
        if publicNames.Contains name then
            FableError("Public types, modules or functions with same name "
                        + "at same level are not supported: " + name) |> raise
        publicNames.Add name
    let isErasedEntity (ent: FSharpEntity) =
        let fail (ent: FSharpEntity) msg =
            let loc = getEntityLocation ent
            FableError(msg, makeRange loc, loc.FileName) |> raise
        let check (ent: FSharpEntity) name att expected =
            if name <> att
            then false
            elif (List.contains "union" expected && not ent.IsFSharpUnion)
                && (List.contains "record" expected && not ent.IsFSharpRecord)
            then fail ent (sprintf "%s can only decorate %s types" att (String.concat "/" expected))
            elif ent.MembersFunctionsAndValues
                |> Seq.exists (fun m -> not m.IsCompilerGenerated)
            then fail ent "Erased types cannot contain members"
            else true
        ent.Attributes |> tryFindAtt (fun name ->
            name = Atts.import
            || name = Atts.global_
            || name = Atts.erase
            || check ent name Atts.stringEnum ["union"]
            || check ent name Atts.pojo ["union"; "record"])
        |> Option.isSome
    let decls = ResizeArray<_>()
    let children = Dictionary<string, TmpDecl>()
    let tryFindChild (meth: FSharpMemberOrFunctionOrValue) =
        tryEnclosingEntity meth
        |> Option.bind (fun ent ->
            if children.ContainsKey ent.FullName
            then Some children.[ent.FullName] else None)
    member self.IsIgnoredEntity (ent: FSharpEntity) =
        ent.IsEnum
        || ent.IsInterface
        || ent.IsFSharpAbbreviation
        || isAttributeEntity ent
        || isErasedEntity ent
    /// Is compiler generated (CompareTo...) or belongs to ignored entity?
    /// (remember F# compiler puts class methods in enclosing modules)
    member self.IsIgnoredMethod (meth: FSharpMemberOrFunctionOrValue) =
        if (meth.IsCompilerGenerated && Naming.ignoredCompilerGenerated.Contains meth.CompiledName)
            || Option.isSome(meth.Attributes |> tryFindAtt (fun name ->
                name = Atts.import || name = Atts.global_ || name = Atts.emit || name = Atts.erase))
            || Naming.ignoredInterfaceMethods.Contains meth.CompiledName
        then true
        else match tryFindChild meth with
             | Some IgnoredEnt -> true
             | _ -> false
    member self.AddMethod (meth: FSharpMemberOrFunctionOrValue, methDecl: Fable.Declaration) =
        match tryFindChild meth with
        | None ->
            if meth.IsModuleValueOrMember
                && isPublicMethod meth
                && not meth.IsCompilerGenerated
                && not meth.IsExtensionMember then
                checkPublicNameConflicts meth.CompiledName
            decls.Add(Decl methDecl)
        | Some (Ent (_,_,entDecls,_)) -> entDecls.Add methDecl
        | Some _ -> () // TODO: log warning
    member self.AddDeclaration (decl: Fable.Declaration, ?publicName: string) =
        publicName |> Option.iter checkPublicNameConflicts
        decls.Add(Decl decl)
    member self.AddChild (com: IFableCompiler, ctx, newChild: FSharpEntity, privateName, newChildDecls: _ list) =
        if not newChild.Accessibility.IsPrivate then
            sanitizeEntityName newChild |> checkPublicNameConflicts
        let ent = Ent (com.GetEntity newChild, privateName,
                    ResizeArray<_> newChildDecls,
                    getEntityLocation newChild |> makeRange |> Some)
        children.Add(newChild.FullName, ent)
        decls.Add(ent)
    member self.AddIgnoredChild (ent: FSharpEntity) =
        // Entities with no FullName will be abbreviations, so we don't need to
        // check if there're members in the enclosing module belonging to them
        match ent.TryFullName with
        | Some fullName -> children.Add(fullName, IgnoredEnt)
        | None -> ()
    member self.TryGetOwner (meth: FSharpMemberOrFunctionOrValue) =
        match tryFindChild meth with
        | Some (Ent (ent,_,_,_)) -> Some ent
        | _ -> None
    member self.GetDeclarations (com, ctx): Fable.Declaration list =
        decls |> Seq.map (function
            | IgnoredEnt -> failwith "Unexpected ignored entity"
            | Decl decl -> decl
            | Ent (ent, privateName, decls, range) ->
                let range =
                    match decls.Count, range with
                    | 0, _ | _, None -> range
                    | _, Some r1 -> (Seq.last decls).Range |> function Some r2 -> Some(r1+r2) | None -> range
                Fable.EntityDeclaration(ent, privateName, processMemberDecls com ctx ent decls, range))
        |> Seq.toList

let private tryGetImport r methName (atts: #seq<FSharpAttribute>) (body: FSharpExpr option) =
    let (|ImportExpr|_|) e =
        let rec matchExpr = function
            | BasicPatterns.Call(None, meth, _, _, args)
                when meth.FullName.StartsWith("Fable.Core.JsInterop.import") -> Some(meth,args)
            // Apparently the F# compiler creates bindings for function arguments, see #721 (5th comment)
            | BasicPatterns.Let((var, _), body)
                when var.IsCompilerGenerated -> matchExpr body
            | _ -> None
        matchExpr e
    let getStringConst = function
        | BasicPatterns.Const(:? string as str, _) -> str
        | _ -> FableError("Import expressions can only be used with string literals", r) |> raise
    try
        match tryFindAtt ((=) Atts.import) atts, body with
        | Some att, _ ->
            match att.ConstructorArguments.[0], att.ConstructorArguments.[1] with
            | (_, (:? string as selector)), (_, (:? string as path)) -> Some(selector, path)
            | _ -> None
        | _, Some(ImportExpr(meth, args)) ->
            let importMeth = meth.FullName.Substring(meth.FullName.LastIndexOf(".") + 1)
            match importMeth with
            | "import" -> Some(getStringConst args.Head, getStringConst args.Tail.Head)
            | "importMember" ->  Some(methName, getStringConst args.Head)
            | "importDefault" -> Some("default", getStringConst args.Head)
            | _ -> Some("*", getStringConst args.Head) // importAll
        | _ -> None
    with _ -> None

let private transformMemberDecl (com: IFableCompiler) ctx (declInfo: DeclInfo)
    (meth: FSharpMemberOrFunctionOrValue) (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let addMethod range import meth args body =
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
                let info =
                    { isInstance = meth.IsInstanceMember
                    ; passGenerics = hasPassGenericsAtt meth }
                bindMemberArgs com ctx info args
                |> fun (ctx, _, args, extraArgs) ->
                    if memberLoc <> Fable.StaticLoc
                    then { ctx with thisAvailability = ThisAvailable }, args, extraArgs
                    else ctx, args, extraArgs
                |> fun (ctx, args, extraArgs) ->
                    match meth.IsImplicitConstructor, declInfo.TryGetOwner meth with
                    | true, Some(EntityKind(Fable.Class(Some(fullName, _), _)) as ent) ->
                        { ctx with baseClass = Some fullName; enclosingEntity=ent}, args, extraArgs
                    | _ -> ctx, args, extraArgs
                |> fun (ctx, args, extraArgs) ->
                    getMemberKind meth, args, extraArgs, transformExpr com ctx body
        let entMember =
            let argTypes = List.map Fable.Ident.getType args
            let fullTyp = makeOriginalCurriedType com meth.CurriedParameterGroups body.Type
            let tryGetMember (e: Fable.Entity) = e.TryGetMember(memberName, memberKind, memberLoc, argTypes)
            match tryEnclosingEntity meth with
            | Some (FableEntity com (Try tryGetMember m)) -> m
            | _ -> makeMethodFrom com memberName memberKind memberLoc argTypes body.Type fullTyp None meth
        let entMember = Fable.MemberDeclaration(entMember, privateName, args@extraArgs, body, Some range)
        declInfo.AddMethod(meth, entMember)
        declInfo, ctx
    let range = getMethLocation meth |> makeRange
    let import = tryGetImport range meth.DisplayName meth.Attributes (Some body)
    if Option.isSome import && hasAtt Atts.emit meth.Attributes then
        let msg = sprintf "%s cannot be combined with %s for relative paths" Atts.emit Atts.import
        FableError(msg, range) |> raise
    if Option.isSome import
    then
        addMethod range import meth args body
    elif declInfo.IsIgnoredMethod meth
    then
        declInfo, ctx
    elif isInline meth
    then
        // Inlining custom type operators is problematic, see #230
        if not (isModuleMember meth) && meth.CompiledName.StartsWith "op_"
        then
            sprintf "Custom type operators cannot be inlined: %s" meth.FullName
            |> addWarning com ctx.fileName (Some range)
            addMethod range None meth args body
        else
            let vars =
                match args with
                | [thisArg]::args when meth.IsInstanceMember -> args
                | _ -> args
                |> Seq.collect id |> countRefs body
            com.AddInlineExpr(meth.FullName, (upcast vars, body))
            declInfo, ctx
    else addMethod range None meth args body

let rec private transformEntityDecl (com: IFableCompiler) ctx (declInfo: DeclInfo)
                                    (ent: FSharpEntity) subDecls =
    let range = getEntityLocation ent |> makeRange
    let import = tryGetImport range ent.DisplayName ent.Attributes None
    if Option.isSome import
    then
        let selector, path = import.Value
        let entName, body = sanitizeEntityName ent, makeImport selector path
        // Bind entity name to context to prevent name clashes
        let ctx, ident = bindIdentWithExactName com ctx Fable.Any None entName
        let m = Fable.Member(entName, Fable.Field, Fable.StaticLoc, [], body.Type,
                            isPublic = not ent.Accessibility.IsPrivate)
        let decl = Fable.MemberDeclaration(m, Some ident.Name, [], body, Some range)
        let publicName = if m.IsPublic then Some entName else None
        declInfo.AddIgnoredChild ent
        declInfo.AddDeclaration(decl, ?publicName=publicName)
        declInfo, ctx
    elif declInfo.IsIgnoredEntity ent
    then
        declInfo.AddIgnoredChild ent
        declInfo, ctx
    else
    let childDecls = transformDeclarations com ctx subDecls
    if List.isEmpty childDecls && ent.IsFSharpModule
    then
        declInfo, ctx
    else
        // Bind entity name to context to prevent name clashes (it will become a variable in JS)
        let ctx, ident = sanitizeEntityName ent |> bindIdentWithExactName com ctx Fable.Any None
        declInfo.AddChild(com, ctx, ent, ident.Name, childDecls)
        declInfo, ctx

and private transformDeclarations (com: IFableCompiler) ctx decls =
    let declInfo, _ =
        decls |> List.fold (fun (declInfo: DeclInfo, ctx) decl ->
            match decl with
            | FSharpImplementationFileDeclaration.Entity (e, sub) ->
                transformEntityDecl com ctx declInfo e sub
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
                transformMemberDecl com ctx declInfo meth args body
            | FSharpImplementationFileDeclaration.InitAction fe ->
                let e = com.Transform ctx fe
                declInfo.AddDeclaration(Fable.ActionDeclaration (e, makeRangeFrom fe))
                declInfo, ctx
        ) (DeclInfo(), ctx)
    declInfo.GetDeclarations(com, ctx)

let private getRootModuleAndDecls decls =
    let rec getRootModuleAndDecls outerEnt decls =
        match decls with
        | [FSharpImplementationFileDeclaration.Entity (ent, decls)]
                when ent.IsFSharpModule || ent.IsNamespace ->
            getRootModuleAndDecls (Some ent) decls
        | decls -> outerEnt, decls
    getRootModuleAndDecls None decls

let private tryGetMethodArgsAndBody (checkedProject: FSharpCheckProjectResults)
                                    (meth: FSharpMemberOrFunctionOrValue) =
    let rec tryGetMethodArgsAndBody' (methFullName: string) = function
        | FSharpImplementationFileDeclaration.Entity (e, decls) ->
            let entFullName = getEntityFullName e
            if entFullName.StartsWith(entFullName)
            then List.tryPick (tryGetMethodArgsAndBody' methFullName) decls
            else None
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth2, args, body) ->
            if methFullName = meth2.FullName
            then Some(args, body)
            else None
        | FSharpImplementationFileDeclaration.InitAction fe -> None
    let fileName = (getMethLocation meth).FileName
    checkedProject.AssemblyContents.ImplementationFiles
    |> Seq.tryFind (fun f -> f.FileName = fileName)
    |> Option.bind (fun f ->
        f.Declarations |> List.tryPick (tryGetMethodArgsAndBody' meth.FullName))

type FableCompiler(com: ICompiler, state: ICompilerState, checkedProject: FSharpCheckProjectResults) =
    let replacePlugins =
        com.Plugins |> List.choose (function
            | { path=path; plugin=(:? IReplacePlugin as plugin)} -> Some (path, plugin)
            | _ -> None)
    let usedVarNames = HashSet<string>()
    member fcom.UsedVarNames = set usedVarNames
    interface IFableCompiler with
        member fcom.Transform ctx fsExpr =
            transformExpr fcom ctx fsExpr
        member fcom.IsReplaceCandidate ent =
            match ent.TryFullName, ent.Assembly.FileName with
            // TODO: Temporary HACK to fix #577
            | Some fullName, _ when fullName.StartsWith("Fable.Import.Node") -> false
            | _, Some asmPath when not(System.String.IsNullOrEmpty(asmPath)) -> true
            | _ -> false
        member fcom.TryGetInternalFile tdef =
            if (fcom :> IFableCompiler).IsReplaceCandidate tdef
            then None
            else Some (getEntityLocation tdef).FileName
        member fcom.GetEntity tdef =
            state.GetOrAddEntity(getEntityFullName tdef, fun () -> makeEntity fcom tdef)
        member fcom.GetInlineExpr meth =
            state.GetOrAddInlineExpr(meth.FullName, fun () ->
                match tryGetMethodArgsAndBody checkedProject meth with
                | Some(args, body) ->
                    let vars =
                        match args with
                        | [thisArg]::args when meth.IsInstanceMember -> args
                        | _ -> args
                        |> Seq.collect id |> countRefs body
                    (upcast vars, body)
                | None ->
                    FableError("Cannot find inline method " + meth.FullName) |> raise)
        member fcom.AddInlineExpr(fullName, inlineExpr) =
            state.GetOrAddInlineExpr(fullName, fun () -> inlineExpr) |> ignore
        member fcom.AddUsedVarName varName =
            usedVarNames.Add varName |> ignore
        member fcom.ReplacePlugins =
            replacePlugins
    interface ICompiler with
        member __.CoreLib = com.CoreLib
        member __.Options = com.Options
        member __.Plugins = com.Plugins
        member __.AddLog msg = com.AddLog msg
        member __.GetUniqueVar() = com.GetUniqueVar()

let getRootModuleFullName (file: FSharpImplementationFileContents) =
    let rootEnt, _ = getRootModuleAndDecls file.Declarations
    match rootEnt with
    | Some rootEnt -> sanitizeEntityFullName rootEnt
    | None -> ""

let transformFile (com: ICompiler) (state: ICompilerState)
                  (checkedProject: FSharpCheckProjectResults) (fileName: string) =
    try
        let file =
            let fileName = Path.normalizeFullPath fileName
            checkedProject.AssemblyContents.ImplementationFiles
            |> Seq.tryFind (fun f -> Path.normalizeFullPath f.FileName = fileName)
            |> function
                | Some file -> file
                | None -> FableError("File doesn't belong to parsed project") |> raise
        let fcom = FableCompiler(com, state, checkedProject)
        let rootEnt, rootDecls =
            let fcom = fcom :> IFableCompiler
            let rootEnt, rootDecls = getRootModuleAndDecls file.Declarations
            match rootEnt with
            | Some e ->
                let rootEnt = fcom.GetEntity e
                let ctx = Context.Create(file.FileName, rootEnt)
                rootEnt, transformDeclarations fcom ctx rootDecls
            | None ->
                let emptyRootEnt = Fable.Entity.CreateRootModule file.FileName
                let ctx = Context.Create(file.FileName, emptyRootEnt)
                emptyRootEnt, transformDeclarations fcom ctx rootDecls
        Fable.File(file.FileName, rootEnt, rootDecls, usedVarNames=fcom.UsedVarNames)
    with
    | :? FableError as e -> FableError(e.Message, ?range=e.Range, file=fileName) |> raise
    | ex -> exn (sprintf "%s (%s)" ex.Message fileName, ex) |> raise
