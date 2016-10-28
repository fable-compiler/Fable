module Fable.FSharp2Fable.Compiler

open System.IO
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
        | Some "System.String", "Empty" -> Some (makeConst "")
        | Some "System.Guid", "Empty" -> Some (makeConst "00000000-0000-0000-0000-000000000000")
        | Some "System.TimeSpan", "Zero" ->
            Fable.Wrapped(makeConst 0, makeType com ctx fsExpr.Type) |> Some
        | Some "System.DateTime", "MaxValue"
        | Some "System.DateTime", "MinValue" ->
            CoreLibCall("Date", Some (Naming.lowerFirst fieldName), false, [])
            |> makeCall com (makeRangeFrom fsExpr) (makeType com ctx fsExpr.Type) |> Some
        | _ -> None
    | _ -> None

let private (|BaseCons|_|) com ctx = function
    | BasicPatterns.Call(None, meth, _, _, args) ->
        let methOwnerName (meth: FSharpMemberOrFunctionOrValue) =
            sanitizeEntityFullName meth.EnclosingEntity
        match ctx.baseClass with
        | Some baseFullName when meth.CompiledName = ".ctor"
                            && (methOwnerName meth) = baseFullName ->
            if not meth.IsImplicitConstructor then
                failwithf "Inheritance is only possible with base class implicit constructor: %s"
                          baseFullName
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
    let isKeyValueList (fsType: FSharpType) =
        match Seq.toList fsType.GenericArguments with
        | [arg] when arg.HasTypeDefinition ->
            arg.TypeDefinition.Attributes |> hasAtt Atts.keyValueList
        | _ -> false
    let unionType, range = makeType com ctx fsType, makeRange fsExpr.Range
    if isKeyValueList fsType then
        let (|KeyValue|_|) = function
            | Fable.Value(Fable.TupleConst([Fable.Value(Fable.StringConst k);v])) -> Some(k, v)
            | _ -> None
        match flattenList range [] argExprs with
        | _, Some baseList ->
            failwithf "KeyValue lists cannot be composed %O" range
        | args, None ->
            (Some [], args) ||> List.fold (fun acc x ->
                match acc, transformExpr com ctx x with
                | Some acc, Fable.Wrapped(KeyValue(k,v),_)
                | Some acc, KeyValue(k,v) -> (k,v)::acc |> Some
                | None, _ -> None // If a case cannot be determined at compile time
                | _ -> None       // the whole list must be converted at runtime
            ) |> function
            | Some cases -> makeJsObject range cases
            | None ->
                let args =
                    let args = args |> List.map (transformExpr com ctx)
                    Fable.Value (Fable.ArrayConst (Fable.ArrayValues args, Fable.Any))
                let builder =
                    Fable.Emit("(o, kv) => { o[kv[0]] = kv[1]; return o; }") |> Fable.Value
                CoreLibCall("Seq", Some "fold", false, [builder;Fable.ObjExpr([],[],None,None);args])
                |> makeCall com (Some range) Fable.Any
    else
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
        |> makeCall com (Some range) unionType

and private transformNonListNewUnionCase com ctx (fsExpr: FSharpExpr) fsType unionCase argExprs =
    let unionType, range = makeType com ctx fsType, makeRange fsExpr.Range
    match unionType with
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
        | _ -> failwithf "Erased Union Cases must have one single field: %s" unionType.FullName
    | KeyValueUnion ->
        let key, value =
            match argExprs with
            | [] -> lowerCaseName unionCase, makeConst true
            | [expr] -> lowerCaseName unionCase, expr
            | [key; expr] when hasAtt Atts.erase unionCase.Attributes -> key, expr
            | _ -> failwithf "KeyValue Union Cases must have one or zero fields: %s" unionType.FullName
        Fable.TupleConst [key; value] |> Fable.Value
    | StringEnum ->
        // if argExprs.Length > 0 then
        //     failwithf "StringEnum must not have fields: %s" unionType.FullName
        lowerCaseName unionCase
    | ListUnion ->
        failwithf "transformNonListNewUnionCase must not be used with List %O" range
    | OtherType ->
        let argExprs = [
            makeConst unionCase.Name    // Include Tag name in args
            Fable.Value(Fable.ArrayConst(Fable.ArrayValues argExprs, Fable.Any))
        ]
        if isReplaceCandidate com fsType.TypeDefinition then
            let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
            buildApplyInfo com ctx r typ unionType (unionType.FullName) ".ctor" Fable.Constructor ([],[],[],0) (None,argExprs)
            |> replace com r
        else
            Fable.Apply(makeNonGenTypeRef com (Some range) ctx.fileName unionType,
                    argExprs, Fable.ApplyCons, makeType com ctx fsExpr.Type, Some range)

and private transformComposableExpr com ctx fsExpr argExprs =
    // See (|ComposableExpr|_|) active pattern to check which expressions are valid here
    match fsExpr with
    | BasicPatterns.Call(None, meth, typArgs, methTypArgs, _) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) None argExprs
    | BasicPatterns.NewObject(meth, typArgs, _) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, []) None argExprs
    | BasicPatterns.NewUnionCase(fsType, unionCase, _) ->
        transformNonListNewUnionCase com ctx fsExpr fsType unionCase argExprs
    | _ -> failwith "ComposableExpr expected"

and private transformExpr (com: IFableCompiler) ctx fsExpr =
    transformExprWithRole UnknownRole com ctx fsExpr

and private transformExprWithRole (role: Role) (com: IFableCompiler) ctx fsExpr =
    match fsExpr with
    (** ## Custom patterns *)
    | SpecialValue com ctx replacement ->
        replacement

    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOf (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        Fable.ForOf (ident, value, transformExpr com newContext body)
        |> makeLoop (makeRangeFrom fsExpr)

    | ErasableLambda (expr, argExprs) ->
        List.map (transformExprWithRole AppliedArgument com ctx) argExprs
        |> transformComposableExpr com ctx expr

    // Pipe must come after ErasableLambda
    | Pipe (Transform com ctx callee, args) ->
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        makeApply range typ callee (List.map (transformExprWithRole AppliedArgument com ctx) args)

    | Composition (expr1, args1, expr2, args2) ->
        let lambdaArg = com.GetUniqueVar() |> makeIdent
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        let expr1 =
            (List.map (transformExprWithRole AppliedArgument com ctx) args1)
                @ [Fable.Value (Fable.IdentValue lambdaArg)]
            |> transformComposableExpr com ctx expr1
        let expr2 =
            (List.map (transformExprWithRole AppliedArgument com ctx) args2)@[expr1]
            |> transformComposableExpr com ctx expr2
        makeLambdaExpr [lambdaArg] expr2

    | BaseCons com ctx (meth, args) ->
        let args = List.map (transformExprWithRole AppliedArgument com ctx) args
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        Fable.Apply(Fable.Value Fable.Super, args, Fable.ApplyMeth, typ, range)

    | TryGetValue (callee, meth, typArgs, methTypArgs, methArgs) ->
        let callee, args = Option.map (com.Transform ctx) callee, List.map (com.Transform ctx) methArgs
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) callee args

    | CreateEvent (callee, eventName, meth, typArgs, methTypArgs, methArgs) ->
        let callee, args = com.Transform ctx callee, List.map (com.Transform ctx) methArgs
        let callee = Fable.Apply(callee, [makeConst eventName], Fable.ApplyGet, Fable.Any, None)
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) (Some callee) args

    | CheckArrayLength (Transform com ctx arr, length) ->
        let r = makeRangeFrom fsExpr
        let lengthExpr = Fable.Apply(arr, [makeConst "length"], Fable.ApplyGet, Fable.Number Int32, r)
        makeEqOp r [lengthExpr; makeConst length] BinaryEqualStrict

    | PrintFormat (Transform com ctx expr) -> expr

    | Applicable (Transform com ctx expr) ->
        let appType =
            let ent = Fable.Entity(lazy Fable.Interface, None, "Fable.Core.Applicable", lazy [])
            Fable.DeclaredType(ent, [Fable.Any; Fable.Any])
        Fable.Wrapped(expr, appType)

    | RecordMutatingUpdate(NonAbbreviatedType fsType, record, updatedFields) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsType
        // TODO: Use a different role type?
        let record = makeValueFrom com ctx r typ AppliedArgument record
        let assignments =
            ([record], updatedFields)
            ||> List.fold (fun acc (FieldName fieldName, e) ->
                let r, value = makeRangeFrom e, com.Transform ctx e
                let e = Fable.Set(record, Some(makeConst fieldName), value, r)
                e::acc)
        Fable.Sequential(assignments, r)

    | JsFunc(thisVar, lambda) ->
        let thisType = makeType com ctx thisVar.FullType
        let ctx, ident = bindIdent com ctx thisType (Some thisVar) thisVar.CompiledName
        com.Transform ctx lambda
        |> makeDelegate com None
        |> function
            | Fable.Value(Fable.Lambda(args,body,_)) ->
                // Make a explicit reference to `this` to prevent an inner lambda
                // tries to capture the enclosing `this`.
                let assignment = Fable.VarDeclaration (ident, Fable.Value Fable.This, false)
                let body = makeSequential body.Range [assignment; body]
                Fable.Value(Fable.Lambda(args,body,false))
            | e -> e // TODO: failwith "unexpected"?

    (** ## Erased *)
    | BasicPatterns.Coerce(_targetType, Transform com ctx inpExpr) -> inpExpr
    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // TODO: We may need to resolve the genArgs, probably adding them to the context
    // and matching them with typeArgs in BasicPatterns.Application(callee, typeArgs, args)
    | BasicPatterns.TypeLambda (_genArgs, Transform com ctx lambda) -> lambda

    (** ## Flow control *)
    | BasicPatterns.FastIntegerForLoop(Transform com ctx start, Transform com ctx limit, body, isUp) ->
        match body with
        | BasicPatterns.Lambda (BindIdent com ctx (newContext, ident), body) ->
            Fable.For (ident, start, limit, com.Transform newContext body, isUp)
            |> makeLoop (makeRangeFrom fsExpr)
        | _ -> failwithf "Unexpected loop in %O: %A" (makeRange fsExpr.Range) fsExpr

    | BasicPatterns.WhileLoop(Transform com ctx guardExpr, Transform com ctx bodyExpr) ->
        Fable.While (guardExpr, bodyExpr)
        |> makeLoop (makeRangeFrom fsExpr)

    (** Values *)
    // Arrays with small data (ushort, byte) won't fit the NewArray pattern
    // as they would require too much memory
    | BasicPatterns.Const(:? System.Array as arr, typ) ->
        let arrExprs = [
            for i in 0 .. (arr.GetLength(0) - 1) ->
                arr.GetValue(i) |> makeConst
        ]
        match arr.GetType().GetElementType().FullName with
        | NumberKind kind -> Fable.Number kind
        | _ -> Fable.Any
        |> makeArray <| arrExprs

    | BasicPatterns.Const(value, FableType com ctx typ) ->
        let e = makeConst value
        if e.Type = typ then e
        // Enumerations are compiled as const but they have a different type
        else Fable.Wrapped (e, typ)

    | BasicPatterns.BaseValue typ ->
        Fable.Super |> Fable.Value

    | BasicPatterns.ThisValue _typ ->
        makeThisRef com ctx None

    | BasicPatterns.Value v when v.IsMemberThisValue ->
        Some v |> makeThisRef com ctx

    | BasicPatterns.Value v ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeValueFrom com ctx r typ role v

    | BasicPatterns.DefaultValue (FableType com ctx typ) ->
        let valueKind =
            match typ with
            | Fable.Boolean -> Fable.BoolConst false
            | Fable.Number kind -> Fable.NumberConst (U2.Case1 0, kind)
            | _ -> Fable.Null
        Fable.Value valueKind

    (** ## Assignments *)
    // Optimization
    | ImmutableBinding((var, value), body) ->
        transformExpr com ctx value |> bindExpr ctx var |> transformExpr com <| body

    | BasicPatterns.Let((var, Transform com ctx value), body) ->
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
    | BasicPatterns.TraitCall (sourceTypes, traitName, flags, argTypes, _, argExprs) ->
        let listsEqual f li1 li2 =
            if List.length li1 <> List.length li2
            then false
            else List.fold2 (fun b x y -> if b then f x y else false) true li1 li2
        // TraitCalls don't know the generic definition of the method argument types,
        // thus we need a bit more convoluted function to compare them.
        let argsEqual (argTypes1: Fable.Type list) (argTypes2: Fable.Type list) =
            let genArgs = Dictionary<string, Fable.Type>()
            let rec argEqual x y =
                match x, y with
                | Fable.GenericParam name1, Fable.GenericParam name2 -> name1 = name2
                | Fable.GenericParam name, y ->
                    if genArgs.ContainsKey name
                    then genArgs.[name] = y
                    else genArgs.Add(name, y); true
                | Fable.Array genArg1, Fable.Array genArg2 ->
                    argEqual genArg1 genArg2
                | Fable.Tuple genArgs1, Fable.Tuple genArgs2 ->
                    listsEqual argEqual genArgs1 genArgs2
                | Fable.Function (genArgs1, typ1), Fable.Function (genArgs2, typ2) ->
                    argEqual typ1 typ2 && listsEqual argEqual genArgs1 genArgs2
                | Fable.DeclaredType(ent1, genArgs1), Fable.DeclaredType(ent2, genArgs2) ->
                    ent1 = ent2 && listsEqual argEqual genArgs1 genArgs2
                | x, y -> x = y
            listsEqual argEqual argTypes1 argTypes2
        let sourceType =
            sourceTypes |> List.tryFind (fun (NonAbbreviatedType t) ->
                if not t.HasTypeDefinition
                then false
                else t.TypeDefinition.MembersFunctionsAndValues
                     |> Seq.exists (fun m -> m.CompiledName = traitName))
            |> defaultArg <| sourceTypes.Head // TODO: Throw exception instead?
            |> makeType com ctx
        let range, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        let callee, args =
            if flags.IsInstance
            then transformExpr com ctx argExprs.Head,
                 List.map (transformExprWithRole AppliedArgument com ctx) argExprs.Tail
            else makeNonGenTypeRef com range ctx.fileName sourceType,
                 List.map (transformExprWithRole AppliedArgument com ctx) argExprs
        let argTypes = List.map (makeType com ctx) argTypes
        let methName =
            match sourceType with
            | Fable.DeclaredType(ent,_) ->
                let loc = if flags.IsInstance then Fable.InstanceLoc else Fable.StaticLoc
                ent.TryGetMember(traitName, Fable.Method, loc, argTypes, argsEqual)
                |> function Some m -> m.OverloadName | None -> traitName
            | _ -> traitName
        makeGet range (Fable.Function(argTypes, typ)) callee (makeConst methName)
        |> fun m -> Fable.Apply (m, args, Fable.ApplyMeth, typ, range)

    | BasicPatterns.Call(callee, meth, typArgs, methTypArgs, args) ->
        let callee = Option.map (com.Transform ctx) callee
        let args = List.map (transformExprWithRole AppliedArgument com ctx) args
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) callee args

    | BasicPatterns.Application(Transform com ctx callee, _typeArgs, args) ->
        let args = List.map (transformExprWithRole AppliedArgument com ctx) args
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        match callee.Type.FullName, args with
        | "Fable.Core.Applicable", args ->
            match args with
            | [Fable.Value(Fable.TupleConst args)] -> args
            | args -> args
            |> List.map (makeDelegate com None)
            |> fun args -> Fable.Apply(callee, args, Fable.ApplyMeth, typ, range)
        | _ -> makeApply range typ callee args

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
    | BasicPatterns.Lambda (var, body) ->
        let ctx, args = makeLambdaArgs com ctx [var]
        Fable.Lambda (args, transformExpr com ctx body, true) |> Fable.Value

    | BasicPatterns.NewDelegate(_delegateType, Transform com ctx delegateBodyExpr) ->
        makeDelegate com None delegateBodyExpr

    (** ## Getters and Setters *)
    | BasicPatterns.FSharpFieldGet (callee, calleeType, FieldName fieldName) ->
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeType com ctx calleeType
                      |> makeNonGenTypeRef com (makeRangeFrom fsExpr) ctx.fileName
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeGetFrom com ctx r typ callee (makeConst fieldName)

    | BasicPatterns.TupleGet (_tupleType, tupleElemIndex, Transform com ctx tupleExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeGetFrom com ctx r typ tupleExpr (makeConst tupleElemIndex)

    | BasicPatterns.UnionCaseGet (Transform com ctx unionExpr, FableType com ctx unionType, unionCase, FieldName fieldName) ->
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        match unionType with
        | ErasedUnion | OptionUnion ->
            Fable.Wrapped(unionExpr, typ)
        | ListUnion ->
            makeGet range typ unionExpr (Naming.lowerFirst fieldName |> makeConst)
        | _ ->
            let i = unionCase.UnionCaseFields |> Seq.findIndex (fun x -> x.Name = fieldName)
            let fields = makeGet range typ unionExpr ("Fields" |> makeConst)
            makeGet range typ fields (i |> makeConst)

    | BasicPatterns.ILFieldSet (callee, typ, fieldName, value) ->
        failwithf "Found unsupported ILField reference in %O: %A"
                  (makeRange fsExpr.Range) fsExpr

    | BasicPatterns.FSharpFieldSet (callee, FableType com ctx calleeType, FieldName fieldName, Transform com ctx value) ->
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeNonGenTypeRef com (makeRangeFrom fsExpr) ctx.fileName calleeType
        Fable.Set (callee, Some (makeConst fieldName), value, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTag (Transform com ctx unionExpr, _unionType) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeGetFrom com ctx r typ unionExpr (makeConst "tag")

    | BasicPatterns.UnionCaseSet (Transform com ctx unionExpr, _type, _case, FieldName caseField, Transform com ctx valueExpr) ->
        makeRange fsExpr.Range
        |> failwithf "Unexpected UnionCaseSet %O"

    | BasicPatterns.ValueSet (valToSet, Transform com ctx valueExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx valToSet.FullType
        let valToSet = makeValueFrom com ctx r typ UnknownRole valToSet
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
            | ThisAvailable -> Some [None, com.GetUniqueVar() |> makeIdent]
            | ThisCaptured(prevThis, prevVars) ->
                (Some prevThis, com.GetUniqueVar() |> makeIdent)::prevVars |> Some
        let baseClass, baseCons =
            match baseCallExpr with
            | BasicPatterns.Call(None, meth, _, _, args)
                when not(isExternalEntity com meth.EnclosingEntity) ->
                let args = List.map (com.Transform ctx) args
                let typ, range = makeType com ctx baseCallExpr.Type, makeRange baseCallExpr.Range
                let baseClass =
                    makeTypeFromDef com ctx meth.EnclosingEntity []
                    |> makeNonGenTypeRef com (Some SourceLocation.Empty) ctx.fileName
                    |> Some
                let baseCons =
                    let c = Fable.Apply(Fable.Value Fable.Super, args, Fable.ApplyMeth, typ, Some range)
                    let m = Fable.Member(".ctor", Fable.Constructor, Fable.InstanceLoc, [], Fable.Any)
                    Fable.MemberDeclaration(m, None, [], c, range)
                    |> Some
                baseClass, baseCons
            | _ -> None, None
        let members =
            (objType, overrides)::otherOverrides
            |> List.collect (fun (typ, overrides) ->
                overrides |> List.map (fun over ->
                    let info = { isInstance = true; passGenerics = false }
                    let args, range = over.CurriedParameterGroups, makeRange fsExpr.Range
                    let ctx, thisArg, args' = bindMemberArgs com ctx info args
                    let ctx =
                        match capturedThis, thisArg with
                        | None, _ -> ctx
                        | Some(capturedThis), Some thisArg ->
                            { ctx with thisAvailability=ThisCaptured(thisArg, capturedThis) }
                        | Some _, None -> failwithf "Unexpected Object Expression method withouth this argument %O" range
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
                    Fable.MemberDeclaration(m, None, args', body, range)))
        let members =
            match baseCons with
            | Some baseCons -> baseCons::members
            | None -> members
        let interfaces =
            objType::(otherOverrides |> List.map fst)
            |> List.map (fun x -> sanitizeEntityFullName x.TypeDefinition)
            |> List.distinct
        let range = makeRangeFrom fsExpr
        let objExpr = Fable.ObjExpr (members, interfaces, baseClass, range)
        match capturedThis with
        | Some((_,capturedThis)::_) ->
            let varDecl = Fable.VarDeclaration(capturedThis, Fable.Value Fable.This, false)
            Fable.Sequential([varDecl; objExpr], range)
        | _ -> objExpr

    | BasicPatterns.NewObject(meth, typArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, []) None (List.map (com.Transform ctx) args)

    | BasicPatterns.NewRecord(NonAbbreviatedType fsType, argExprs) ->
        let recordType, range = makeType com ctx fsType, makeRange fsExpr.Range
        let argExprs = argExprs |> List.map (transformExpr com ctx)
        if isReplaceCandidate com fsType.TypeDefinition then
            let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
            buildApplyInfo com ctx r typ recordType (recordType.FullName) ".ctor" Fable.Constructor ([],[],[],0) (None,argExprs)
            |> replace com r
        else
            Fable.Apply(makeNonGenTypeRef com (Some range) ctx.fileName recordType,
                    argExprs, Fable.ApplyCons, makeType com ctx fsExpr.Type, Some range)

    | BasicPatterns.NewUnionCase(NonAbbreviatedType fsType, unionCase, argExprs) ->
        match fsType with
        | ListType _ -> transformNewList com ctx fsExpr fsType argExprs
        | _ ->
            List.map (com.Transform ctx) argExprs
            |> transformNonListNewUnionCase com ctx fsExpr fsType unionCase

    (** ## Type test *)
    | BasicPatterns.TypeTest (FableType com ctx typ as fsTyp, Transform com ctx expr) ->
        makeTypeTest com (makeRangeFrom fsExpr) ctx.fileName typ expr

    | BasicPatterns.UnionCaseTest(Transform com ctx unionExpr,
                                  (FableType com ctx unionType as fsType),
                                  unionCase) ->
        match unionType with
        | ErasedUnion ->
            if unionCase.UnionCaseFields.Count <> 1 then
                failwithf "Erased Union Cases must have one single field: %s"
                          unionType.FullName
            else
                let typ =
                    let m = Regex.Match(unionCase.Name, @"^Case(\d+)$")
                    if m.Success
                    then
                        let idx = int m.Groups.[1].Value - 1
                        if fsType.GenericArguments.Count > idx
                        then makeType com ctx fsType.GenericArguments.[idx]
                        else unionType
                    else unionType
                makeTypeTest com (makeRangeFrom fsExpr) ctx.fileName typ unionExpr
        | OptionUnion ->
            let opKind = if unionCase.Name = "None" then BinaryEqual else BinaryUnequal
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; Fable.Value Fable.Null] opKind
        | ListUnion ->
            let opKind = if unionCase.CompiledName = "Empty" then BinaryEqual else BinaryUnequal
            let expr = makeGet None Fable.Any unionExpr (makeConst "tail")
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [expr; Fable.Value Fable.Null] opKind
        | StringEnum ->
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; lowerCaseName unionCase] BinaryEqualStrict
        | _ ->
            let left = makeGet None Fable.String unionExpr (makeConst "Case")
            let right = makeConst unionCase.Name
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [left; right] BinaryEqualStrict

    (** Pattern Matching *)
    | Switch(matchValue, cases, defaultCase, decisionTargets) ->
        let transformCases assignVar =
            let transformBody idx =
                let body = transformExpr com ctx (snd decisionTargets.[idx])
                match assignVar with
                | Some assignVar -> Fable.Set(assignVar, None, body, body.Range)
                | None -> body
            let cases =
                cases |> Seq.map (fun kv ->
                    List.map makeConst kv.Value, transformBody kv.Key)
                |> Seq.toList
            let defaultCase = transformBody defaultCase
            cases, defaultCase
        let matchValue =
            let t = makeType com ctx matchValue.FullType
            makeValueFrom com ctx None t UnknownRole matchValue
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        match typ with
        | Fable.Unit ->
            let cases, defaultCase = transformCases None
            Fable.Switch(matchValue, cases, Some defaultCase, typ, r)
        | _ ->
            let assignVar = com.GetUniqueVar() |> makeIdent
            let cases, defaultCase =
                Fable.IdentValue assignVar |> Fable.Value |> Some |> transformCases
            makeSequential r [
                Fable.VarDeclaration(assignVar, Fable.Value Fable.Null, true)
                Fable.Switch(matchValue, cases, Some defaultCase, typ, r)
                Fable.Value(Fable.IdentValue assignVar)
            ]

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
                failwithf "Unexpected DecisionTree branch in %O: %A"
                          (makeRange e.Range) e
        let targetRefsCount = getTargetRefsCount (Map.empty<int,int>) decisionExpr
        // Convert targets referred more than once into functions
        // and just pass the F# implementation for the others
        let ctx, assignments =
            targetRefsCount
            |> Map.filter (fun k v -> v > 1)
            |> Map.fold (fun (ctx, acc) k v ->
                let targetVars, targetExpr = decisionTargets.[k]
                let targetVars, targetCtx =
                    (targetVars, ([], ctx)) ||> List.foldBack (fun var (vars, ctx) ->
                        let ctx, var = bindIdentFrom com ctx var
                        var::vars, ctx)
                let lambda =
                    com.Transform targetCtx targetExpr |> makeLambdaExpr targetVars
                let ctx, ident = bindIdent com ctx lambda.Type None (sprintf "$target%i" k)
                ctx, Map.add k (ident, lambda) acc) (ctx, Map.empty<_,_>)
        let decisionTargets =
            targetRefsCount |> Map.map (fun k v ->
                match v with
                | 1 -> TargetImpl decisionTargets.[k]
                | _ -> TargetRef (fst assignments.[k]))
        let ctx = { ctx with decisionTargets = decisionTargets }
        if assignments.Count = 0 then
            transformExpr com ctx decisionExpr
        else
            let assignments =
                assignments
                |> Seq.map (fun pair ->
                    let ident, lambda = pair.Value
                    Fable.VarDeclaration (ident, lambda, false))
                |> Seq.toList
            Fable.Sequential (assignments @ [transformExpr com ctx decisionExpr], makeRangeFrom fsExpr)

    | BasicPatterns.DecisionTreeSuccess (decIndex, decBindings) ->
        match Map.tryFind decIndex ctx.decisionTargets with
        | None -> failwith "Missing decision target"
        // If we get a reference to a function, call it
        | Some (TargetRef targetRef) ->
            Fable.Apply (Fable.IdentValue targetRef |> Fable.Value,
                (decBindings |> List.map (transformExpr com ctx)),
                Fable.ApplyMeth, makeType com ctx fsExpr.Type, makeRangeFrom fsExpr)
        // If we get an implementation without bindings, just transform it
        | Some (TargetImpl ([], Transform com ctx decBody)) -> decBody
        // If we have bindings, create the assignments
        | Some (TargetImpl (decVars, decBody)) ->
            let newContext, assignments =
                List.foldBack2 (fun var (Transform com ctx binding) (accContext, accAssignments) ->
                    let (BindIdent com accContext (newContext, ident)) = var
                    let assignment = Fable.VarDeclaration (ident, binding, var.IsMutable)
                    newContext, (assignment::accAssignments)) decVars decBindings (ctx, [])
            assignments @ [transformExpr com newContext decBody]
            |> makeSequential (makeRangeFrom fsExpr)

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
    // Check there're no instance methods conflicting with interface methods
    // TODO: The whole inheritance chain should actually be checked
    let memberByLoc loc = function
        | Fable.MemberDeclaration(m,_,_,_,_) as decl
            when sameMemberLoc m.Location loc -> Some m
        | _ -> None
    // Allow interface overloads of these methods,
    // as sometimes the compiler forces you to do so
    let overloadExceptions = Set ["Equals"; "GetEnumerator"; "Current"]
    let instanceMeths =
        childDecls |> Seq.choose (memberByLoc Fable.InstanceLoc)
        |> Seq.map (fun m -> m.Name) |> Set
    childDecls
    |> Seq.choose (memberByLoc (Fable.InterfaceLoc ""))
    |> Seq.groupBy (fun m -> m.Name)
    |> Seq.iter (fun (name, Array ms) ->
        if ms.Length > 1 && not(overloadExceptions.Contains name) then
            match ms.[0].Location, ms.[1].Location with
            | Fable.InterfaceLoc ifc1, Fable.InterfaceLoc ifc2 ->
                if ifc1 = ifc2
                then failwithf "Interface overloads cannot be implemented: %s.%s" ifc1 name
                else failwithf "Implementing two interfaces with same method name is not allowed. Both %s and %s contain %s" ifc1 ifc2 name
            | _ -> ()
        elif instanceMeths.Contains name && not(overloadExceptions.Contains name) then
            match ms.[0].Location with
            | Fable.InterfaceLoc ifc -> ifc, fableEnt.FullName, name
            | _ -> "unknown", fableEnt.FullName, name
            |||> failwithf "Interface %s conflicts with type %s method %s"
    )
    // If F# union or records implement System.IComparable/System.Equatable generate the methods
    // Note: F# compiler generates these methods too but see `IsIgnoredMethod`
    let needsEqImpl =
        fableEnt.HasInterface "System.IEquatable"
        && fableEnt.TryGetDecorator("Microsoft.FSharp.Core.CustomEquality").IsNone
    let needsCompImpl =
        fableEnt.HasInterface "System.IComparable"
        && fableEnt.TryGetDecorator("Microsoft.FSharp.Core.CustomComparison").IsNone
    let fableType =
        Fable.DeclaredType(fableEnt, fableEnt.GenericParameters |> List.map Fable.GenericParam)
    // Unions, records and F# exceptions don't have a constructor
    match fableEnt.Kind with
    | Fable.Union cases ->
      [ yield makeUnionCons()
        yield makeTypeNameMeth com fableEnt.FullName
        yield makeInterfacesMethod com ctx.fileName fableEnt
                false ("FSharpUnion"::fableEnt.Interfaces)
        yield makeCasesMethod com ctx.fileName cases
        if needsEqImpl then yield makeUnionEqualMethod com fableType
        if needsCompImpl then yield makeUnionCompareMethod com fableType ]
    // TODO: Use specific interface for FSharpException?
    | Fable.Record fields
    | Fable.Exception fields ->
      [ yield makeRecordCons fields
        yield makeTypeNameMeth com fableEnt.FullName
        yield makeInterfacesMethod com ctx.fileName fableEnt
                false ("FSharpRecord"::fableEnt.Interfaces)
        yield makePropertiesMethod com ctx.fileName fableEnt false fields
        if needsEqImpl then yield makeRecordEqualMethod com fableType
        if needsCompImpl then yield makeRecordCompareMethod com fableType ]
    | Fable.Class(baseClass, properties) ->
      [ yield makeTypeNameMeth com fableEnt.FullName
        if baseClass.IsSome || fableEnt.Interfaces.Length > 0 then
            yield makeInterfacesMethod com ctx.fileName fableEnt baseClass.IsSome fableEnt.Interfaces
        yield makePropertiesMethod com ctx.fileName fableEnt baseClass.IsSome properties ]
    | _ -> []
    |> fun autoMeths -> [yield! autoMeths; yield! childDecls]

// The F# compiler considers class methods as children of the enclosing module.
// We use this type to correct that, see type DeclInfo below.
type private TmpDecl =
    | Decl of Fable.Declaration
    | Ent of Fable.Entity * string * ResizeArray<Fable.Declaration> * SourceLocation
    | IgnoredEnt

type private DeclInfo() =
    let publicNames = ResizeArray<string>()
    // Check there're no conflicting entity or function names (see #166)
    let checkPublicNameConflicts name =
        if publicNames.Contains name then
            failwithf "%s %s: %s"
                "Public types, modules or functions with same name"
                "at same level are not supported" name
        publicNames.Add name
    let decls = ResizeArray<_>()
    let children = Dictionary<string, TmpDecl>()
    let tryFindChild (ent: FSharpEntity) =
        if children.ContainsKey ent.FullName
        then Some children.[ent.FullName] else None
    let hasIgnoredAtt atts =
        atts |> tryFindAtt (fun name ->
            Naming.importAtts.Contains name || Naming.eraseAtts.Contains name)
        |> Option.isSome
    // Interfaces don't appear in the AST so we don't need to check them
    member self.IsIgnoredEntity (ent: FSharpEntity) =
        ent.IsEnum
        || ent.IsFSharpAbbreviation
        || isAttributeEntity ent
        || (hasIgnoredAtt ent.Attributes)
    /// Is compiler generated (CompareTo...) or belongs to ignored entity?
    /// (remember F# compiler puts class methods in enclosing modules)
    member self.IsIgnoredMethod (meth: FSharpMemberOrFunctionOrValue) =
        if (meth.IsCompilerGenerated && Naming.ignoredCompilerGenerated.Contains meth.CompiledName)
            || (hasIgnoredAtt meth.Attributes)
        then true
        else match tryFindChild meth.EnclosingEntity with
             | Some IgnoredEnt -> true
             | _ -> false
    member self.AddMethod (meth: FSharpMemberOrFunctionOrValue, methDecl: Fable.Declaration) =
        match tryFindChild meth.EnclosingEntity with
        | None ->
            if meth.IsModuleValueOrMember
                && not meth.Accessibility.IsPrivate
                && not meth.IsCompilerGenerated
                && not meth.IsExtensionMember then
                checkPublicNameConflicts meth.CompiledName
            decls.Add(Decl methDecl)
        | Some (Ent (_,_,entDecls,_)) -> entDecls.Add methDecl
        | Some _ -> () // TODO: log warning
    member self.AddInitAction (actionDecl: Fable.Declaration) =
        decls.Add(Decl actionDecl)
    member self.AddChild (com: IFableCompiler, ctx, newChild: FSharpEntity, privateName, newChildDecls: _ list) =
        if not newChild.Accessibility.IsPrivate then
            sanitizeEntityName newChild |> checkPublicNameConflicts
        let ent = Ent (com.GetEntity ctx newChild, privateName,
                    ResizeArray<_> newChildDecls,
                    getEntityLocation newChild |> makeRange)
        children.Add(newChild.FullName, ent)
        decls.Add(ent)
    member self.AddIgnoredChild (ent: FSharpEntity) =
        // Entities with no FullName will be abbreviations, so we don't need to
        // check if there're members in the enclosing module belonging to them
        match ent.TryFullName with
        | Some fullName -> children.Add(fullName, IgnoredEnt)
        | None -> ()
    member self.TryGetOwner (meth: FSharpMemberOrFunctionOrValue) =
        match tryFindChild meth.EnclosingEntity with
        | Some (Ent (ent,_,_,_)) -> Some ent
        | _ -> None
    member self.GetDeclarations (com, ctx): Fable.Declaration list =
        decls |> Seq.map (function
            | IgnoredEnt -> failwith "Unexpected ignored entity"
            | Decl decl -> decl
            | Ent (ent, privateName, decls, range) ->
                let range =
                    match decls.Count with
                    | 0 -> range
                    | _ -> range + (Seq.last decls).Range
                Fable.EntityDeclaration(ent, privateName, processMemberDecls com ctx ent decls, range))
        |> Seq.toList

let private transformMemberDecl (com: IFableCompiler) ctx (declInfo: DeclInfo)
    (meth: FSharpMemberOrFunctionOrValue) (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let addMethod() =
        let memberName, memberKind = sanitizeMethodName meth
        let memberLoc = getMemberLoc meth
        let ctx, privateName =
            // Bind module member names to context to prevent
            // name clashes (they will become variables in JS)
            if meth.EnclosingEntity.IsFSharpModule then
                let typ = makeType com ctx meth.FullType
                let ctx, privateName = bindIdent com ctx typ (Some meth) memberName
                ctx, Some (privateName.Name)
            else ctx, None
        let args, body =
            let info =
                { isInstance = meth.IsInstanceMember
                  passGenerics = hasAtt Atts.passGenerics meth.Attributes }
            bindMemberArgs com ctx info args
            |> fun (ctx, _, args) ->
                if memberLoc <> Fable.StaticLoc
                then { ctx with thisAvailability = ThisAvailable }, args
                else ctx, args
            |> fun (ctx, args) ->
                match meth.IsImplicitConstructor, declInfo.TryGetOwner meth with
                | true, Some(EntityKind(Fable.Class(Some(fullName, _), _))) ->
                    { ctx with baseClass = Some fullName }, args
                | _ -> ctx, args
            |> fun (ctx, args) ->
                args, transformExpr com ctx body
        let entMember =
            let fableEnt = makeEntity com ctx meth.EnclosingEntity
            let argTypes = List.map Fable.Ident.getType args
            let fullTyp = makeOriginalCurriedType com meth.CurriedParameterGroups body.Type
            match fableEnt.TryGetMember(memberName, memberKind, memberLoc, argTypes) with
            | Some m -> m
            | None -> makeMethodFrom com memberName memberKind memberLoc argTypes body.Type fullTyp None meth
            |> fun m -> Fable.MemberDeclaration(m, privateName, args, body, SourceLocation.Empty)
        declInfo.AddMethod (meth, entMember)
        ctx
    if declInfo.IsIgnoredMethod meth then ctx
    elif isInline meth then
        // Inlining custom type operators is problematic, see #230
        if not meth.EnclosingEntity.IsFSharpModule && meth.CompiledName.StartsWith "op_" then
            sprintf "Custom type operators cannot be inlined: %s" meth.FullName
            |> Warning |> com.AddLog
            addMethod()
        else
            com.AddInlineExpr meth.FullName (List.collect id args, body)
            ctx
    else addMethod()
    |> fun ctx -> declInfo, ctx

let rec private transformEntityDecl (com: IFableCompiler) ctx (declInfo: DeclInfo)
                                    (ent: FSharpEntity) subDecls =
    if declInfo.IsIgnoredEntity ent then
        declInfo.AddIgnoredChild ent
        declInfo, ctx
    else
        let childDecls = transformDeclarations com ctx subDecls
        // Even if a module is marked with Erase, transform its members
        // in case they contain inline methods
        if hasAtt Atts.erase ent.Attributes
        then declInfo, ctx
        else
            // Bind entity name to context to prevent name
            // clashes (it will become a variable in JS)
            let ctx, ident = sanitizeEntityName ent |> bindIdent com ctx Fable.Any None
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
            | FSharpImplementationFileDeclaration.InitAction (Transform com ctx e as fe) ->
                declInfo.AddInitAction (Fable.ActionDeclaration (e, makeRange fe.Range))
                declInfo, ctx
        ) (DeclInfo(), ctx)
    declInfo.GetDeclarations(com, ctx)

let makeFileMap (rootEntities: #seq<FSharpEntity>) (filePairs: Map<string, string>) =
    rootEntities
    |> Seq.groupBy (fun ent -> (getEntityLocation ent).FileName)
    |> Seq.map (fun (file, ents) ->
        let ns =
            match List.ofSeq ents with
            | [] -> ""
            | [ent] ->
                if ent.IsFSharpModule
                then defaultArg ent.TryFullName ""
                else defaultArg ent.Namespace ""
            | ents ->
                let rootNs =
                    ents
                    |> List.choose (fun ent ->
                        match ent.TryFullName with
                        | Some fullName -> fullName.Split('.') |> Some
                        | None -> None)
                    |> Path.getCommonPrefix
                    |> String.concat "."
                if rootNs.EndsWith(".")
                then rootNs.Substring(0, rootNs.Length - 1)
                else rootNs
        let fileInfo: Fable.FileInfo =
            {targetFile=filePairs.[file]; rootModule=ns}
        file, fileInfo)
    |> Map

type FableCompiler(com: ICompiler, projectMaps: Map<string, Fable.FileInfo> list,
                   entitiesCache: Dictionary<string, Fable.Entity>,
                   inlineExprsCache: Dictionary<string, FSharpMemberOrFunctionOrValue list * FSharpExpr>) =
    let replacePlugins =
        com.Plugins |> List.choose (function
            | path, (:? IReplacePlugin as plugin) -> Some (path, plugin)
            | _ -> None)
    let usedVarNames = HashSet<string>()
    member fcom.UsedVarNames = set usedVarNames
    interface IFableCompiler with
        member fcom.Transform ctx fsExpr =
            transformExpr fcom ctx fsExpr
        member fcom.GetInternalFile tdef =
            let file = (getEntityLocation tdef).FileName
            if List.exists (Map.containsKey file) projectMaps
            then Some file
            else None
        member fcom.GetEntity ctx tdef =
            entitiesCache.GetOrAdd(
                defaultArg tdef.TryFullName tdef.CompiledName,
                fun _ -> makeEntity fcom ctx tdef)
        member fcom.TryGetInlineExpr meth =
            let success, expr = inlineExprsCache.TryGetValue meth.FullName
            if success then Some expr else None
        member fcom.AddInlineExpr fullName inlineExpr =
            inlineExprsCache.AddOrUpdate(fullName,
                (fun _ -> inlineExpr), (fun _ _ -> inlineExpr))
            |> ignore
        member fcom.AddUsedVarName varName =
            usedVarNames.Add varName |> ignore
        member fcom.ReplacePlugins =
            replacePlugins
    interface ICompiler with
        member __.Options = com.Options
        member __.ProjDir = com.ProjDir
        member __.Plugins = com.Plugins
        member __.AddLog msg = com.AddLog msg
        member __.GetLogs() = com.GetLogs()
        member __.GetUniqueVar() = com.GetUniqueVar()

type FSProjectInfo(projectOpts: FSharpProjectOptions, filePairs: Map<string, string>,
                    ?fileMask: string, ?extra: Map<string, obj>) =
    let extra = defaultArg extra Map.empty
    let dependencies: IDictionary<string, string list> =
        ("dependencies", extra)
        ||> Map.findOrRun (fun () -> upcast Dictionary())
    let arePathsEqual p1 p2 =
        (Path.normalizeFullPath p1) = (Path.normalizeFullPath p2)
    member __.ProjectOpts = projectOpts
    member __.FilePairs = filePairs
    member __.FileMask = fileMask
    member __.Extra = extra
    member __.IsMasked(fileName) =
        match fileMask with
        | Some mask ->
            if arePathsEqual fileName mask
            then true
            else
                let success, deps = dependencies.TryGetValue(fileName)
                success && List.exists (arePathsEqual mask) deps
        | None -> true

let private getProjectMaps (com: ICompiler) (parsedProj: FSharpCheckProjectResults) (projInfo: FSProjectInfo) =
    // Resolve relative paths and then make them relative to outDir: see #472
    let resolveRelativePath (projFile: string option) (path: string) =
        if not(path.StartsWith ".") then path else
        let path =
            match projFile with
            | Some projFile ->
                let projDir = Path.GetDirectoryName projFile
                Path.GetFullPath(Path.Combine(projDir, path))
            // `--refs` from options are resolved with the workingDir
            | None -> Path.GetFullPath path
        Fable.Path.getRelativePath com.Options.outDir path
    let curProj =
        makeFileMap parsedProj.AssemblySignature.Entities projInfo.FilePairs
    let refAssemblies =
        parsedProj.ProjectContext.GetReferencedAssemblies()
        |> List.choose (fun assembly ->
            assembly.FileName
            |> Option.bind (fun asmPath ->
                try
                    let asmDir = Path.GetDirectoryName(asmPath)
                    let makeAbsolute (path: string) =
                        Path.GetFullPath(Path.Combine(asmDir, path))
                    let json = File.ReadAllText(Path.ChangeExtension(asmPath, Naming.fablemapExt))
                    let fableMap = Newtonsoft.Json.JsonConvert.DeserializeObject<Fable.FableMap>(json)
                    fableMap.files |> Seq.map (fun kv ->
                        kv.Key, { kv.Value with targetFile = makeAbsolute kv.Value.targetFile })
                    |> Map |> Some
                with _ -> None // TODO: Raise error or warning?
            ))
    curProj::refAssemblies


let transformFiles (com: ICompiler) (parsedProj: FSharpCheckProjectResults) (projInfo: FSProjectInfo) =
    let rec getRootDecls rootNs ent decls =
        if rootNs = "" then ent, decls else
        match decls with
        | [FSharpImplementationFileDeclaration.Entity (ent, decls)]
            when ent.IsNamespace || ent.IsFSharpModule ->
            // TODO: Report Bug when ent.IsNamespace, FullName doesn't work
            let fullName =
                let fullName = defaultArg ent.TryFullName ""
                if ent.IsFSharpModule then fullName else
                [|defaultArg ent.Namespace ""; fullName|]
                |> Array.filter (System.String.IsNullOrEmpty >> not)
                |> String.concat "."
            if fullName = rootNs
            then Some ent, decls
            else getRootDecls rootNs (Some ent) decls
        | _ -> failwith "Multiple namespaces in same file is not supported"
    let projectMaps =
        ("projectMaps", projInfo.Extra)
        ||> Map.findOrRun (fun () -> getProjectMaps com parsedProj projInfo)
    // Cache for entities and inline expressions
    let entitiesCache = Dictionary<string, Fable.Entity>()
    let inlineExprsCache: Dictionary<string, FSharpMemberOrFunctionOrValue list * FSharpExpr> =
        Map.findOrNew "inline" projInfo.Extra
    // Start transforming files
    let entryFile =
        parsedProj.AssemblyContents.ImplementationFiles
        |> List.last |> fun file -> file.FileName
    parsedProj.AssemblyContents.ImplementationFiles
    |> Seq.choose (fun file ->
        try
        if not(projectMaps.Head.ContainsKey file.FileName && projInfo.IsMasked file.FileName)
        then None
        else
            let fcom = FableCompiler(com, projectMaps, entitiesCache, inlineExprsCache)
            let rootEnt, rootDecls =
                let ctx = { Context.Empty with fileName = file.FileName }
                let rootNs = projectMaps.Head.[file.FileName].rootModule
                let rootEnt, rootDecls = getRootDecls rootNs None file.Declarations
                let rootDecls = transformDeclarations fcom ctx rootDecls
                match rootEnt with
                | Some e when hasAtt Atts.erase e.Attributes -> makeEntity fcom ctx e, []
                | Some e -> makeEntity fcom ctx e, rootDecls
                | None -> Fable.Entity.CreateRootModule file.FileName rootNs, rootDecls
            match rootDecls with
            | [] -> None
            | rootDecls -> Fable.File(file.FileName, projInfo.FilePairs.[file.FileName], rootEnt, rootDecls,
                            isEntry=(file.FileName = entryFile), usedVarNames=fcom.UsedVarNames) |> Some
        with
        | ex -> exn (sprintf "%s (%s)" ex.Message file.FileName, ex) |> raise
    )
    |> fun seq ->
        let extra =
            projInfo.Extra
            |> Map.add "projectMaps" (box projectMaps)
            |> Map.add "inline" (box inlineExprsCache)
        extra, seq
