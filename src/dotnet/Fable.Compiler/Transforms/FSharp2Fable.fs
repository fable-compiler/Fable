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

let private transformLambda com ctx arg body =
    let ctx, args = makeFunctionArgs com ctx [arg]
    match args with
    | [arg] -> Fable.Function(Fable.Lambda arg, transformExpr com ctx body)
    | _ -> failwith "makeFunctionArgs returns args with different length"

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
    | StringEnum _ ->
        match argExprs with
        | [] -> lowerCaseName unionCase
        | _ -> "StringEnum types cannot have fields: " + (getFsTypeFullName fsType)
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

let private transformTraitCall com (ctx: Context) r typ sourceTypes traitName (flags: MemberFlags) (argTypes: FSharpType list) (argExprs: FSharpExpr list) =
    let isInstance = flags.IsInstance
    let argTypes = List.map (makeType com Map.empty) argTypes
    let argExprs = List.map (fun e -> com.Transform(ctx, e)) argExprs
    let thisArg, args, argTypes =
        match argExprs, argTypes with
        | thisArg::args, _::argTypes when isInstance -> Some thisArg, args, argTypes
        | args, argTypes -> None, args, argTypes
    sourceTypes |> List.tryPick (fun typ ->
        match makeType com ctx.GenericArgs typ with
        | Fable.DeclaredType(ent,_) ->
            // printfn "LOOK FOR MEMBER %s (isInstance %b) in %s with args: %A"
            //     traitName isInstance ent.DisplayName argTypes
            tryFindMember com ent traitName isInstance argTypes
        | _ -> None)
    |> function
        | Some memb -> makeCallFrom com ctx r typ [] thisArg args memb
        | None -> "Cannot resolve trait call " + traitName
                  |> addErrorAndReturnNull com r

let private transformObjExpr (com: IFableCompiler) (ctx: Context) (objType: FSharpType)
                        (overrides: FSharpObjectExprOverride list) otherOverrides =
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
            // TODO: Check if we should pass generics?
            let ctx, args = bindMemberArgs com ctx over.CurriedParameterGroups
            let value = Fable.Function(Fable.Delegate args, transformExpr com ctx over.Body)
            // Don't use the typ argument as the override may come
            // from another type, like ToString()
            let typ =
                if over.Signature.DeclaringType.HasTypeDefinition
                then Some over.Signature.DeclaringType.TypeDefinition
                else None
            // TODO: Check for indexed getter and setter also in object expressions?
            match over.Signature.Name with
            | Naming.StartsWith "get_" name ->
                name, value, Fable.ObjectGetter
            | Naming.StartsWith "set_" name ->
                name, value, Fable.ObjectSetter
            | name ->
                // FSharpObjectExprOverride.CurriedParameterGroups doesn't offer
                // information about ParamArray, we need to check the source method.
                let hasSpread =
                    match typ with
                    | None -> false
                    | Some typ ->
                        typ.TryGetMembersFunctionsAndValues
                        |> Seq.tryFind (fun x -> x.CompiledName = over.Signature.Name)
                        |> function Some m -> hasSeqSpread m | None -> false
                name, value, Fable.ObjectMethod hasSpread
    )) |> fun members ->
        let typ = makeType com ctx.GenericArgs objType
        Fable.ObjectExpr(members, typ)

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
        failwith "TODO: Lamba to Delegate"
    | fsExpr -> transformExpr com ctx fsExpr

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
        Fable.Cast(inpExpr, makeType com ctx.GenericArgs targetType)

    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    // Sometimes these must be inlined, but that's resolved in BasicPatterns.Let (see below)
    | BasicPatterns.TypeLambda (_genArgs, Transform com ctx lambda) -> lambda

    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOfLoop (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        Fable.ForOf (ident, value, transformExpr com newContext body)
        |> makeLoop (makeRangeFrom fsExpr)

    | TryGetValue (callee, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let callee, args = Option.map (transformExpr com ctx) callee, List.map (transformExpr com ctx) membArgs
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        makeCallFrom com ctx r typ genArgs callee args memb

    | CreateEvent (callee, eventName, memb, ownerGenArgs, membGenArgs, membArgs) ->
        let callee, args = transformExpr com ctx callee, List.map (transformExpr com ctx) membArgs
        let callee = get None Fable.Any callee eventName
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        makeCallFrom com ctx r typ genArgs (Some callee) args memb

    | CheckArrayLength (Transform com ctx arr, length, FableType com ctx typ) ->
        let r = makeRangeFrom fsExpr
        let lengthExpr = get None (Fable.Number Int32) arr "length"
        makeEqOp r lengthExpr (Replacements.makeTypeConst typ length) BinaryEqualStrict

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
        let expr = Replacements.makeTypeConst typ value
        // TODO TODO TODO: Check literals and compile as Enum
        // if expr.Type <> typ then // Enumerations are compiled as const but they have a different type
        //     Replacements.checkLiteral com (makeRangeFrom fsExpr) value typ
        expr

    | BasicPatterns.BaseValue typ
    | BasicPatterns.ThisValue typ ->
        makeType com ctx.GenericArgs typ |> Fable.This |> Fable.Value

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
        match typ with
        | Fable.Boolean -> Fable.BoolConstant false |> Fable.Value
        | Fable.Number kind -> Fable.NumberConstant (0., kind) |> Fable.Value
        | typ -> Fable.Null typ |> Fable.Value

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
    | BasicPatterns.TraitCall(sourceTypes, traitName, flags, argTypes, _argTypes2, argExprs) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        transformTraitCall com ctx r typ sourceTypes traitName flags argTypes argExprs

    | BasicPatterns.Call(callee, memb, ownerGenArgs, membGenArgs, args) ->
        let callee = Option.map (transformExpr com ctx) callee
        let args = List.map (transformExpr com ctx) args
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let genArgs = ownerGenArgs @ membGenArgs |> Seq.map (makeType com ctx.GenericArgs)
        makeCallFrom com ctx r typ genArgs callee args memb

    // Application of locally inlined lambdas
    | BasicPatterns.Application(BasicPatterns.Value var, genArgs, args) when isInline var ->
        let range = makeRangeFrom fsExpr
        match ctx.ScopeInlineValues |> List.tryFind (fun (v,_) -> obj.Equals(v, var)) with
        | Some (_,fsExpr) ->
            let genArgs = Seq.map (makeType com ctx.GenericArgs) genArgs
            let resolvedCtx = { ctx with GenericArgs = matchGenericParams var genArgs }
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

    // TODO: Ask why application without arguments happen. So far I've seen it
    // to access None or struct values (like the Result type)
    | BasicPatterns.Application(Transform com ctx expr, _, []) -> expr

    | BasicPatterns.Application(FableCoreDynamicOp(Transform com ctx e1, Transform com ctx e2), _, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let argInfo: Fable.ArgInfo =
          { ThisArg = Some e1
            Args = List.map (transformExpr com ctx) args
            ArgTypes = None
            Spread = Fable.TupleSpread }
        Fable.Operation(Fable.Call(Fable.InstanceCall(Some e2), argInfo), typ, r)

    | BasicPatterns.Application(Transform com ctx applied, _, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let args = List.map (transformExpr com ctx) args
        Fable.Operation(Fable.CurriedApply(applied, args), typ, r)

    | BasicPatterns.IfThenElse (Transform com ctx guardExpr, Transform com ctx thenExpr, Transform com ctx elseExpr) ->
        Fable.IfThenElse (guardExpr, thenExpr, elseExpr)

    | BasicPatterns.TryFinally (BasicPatterns.TryWith(body, _, _, catchVar, catchBody),finalBody) ->
        makeTryCatch com ctx body (Some (catchVar, catchBody)) (Some finalBody)

    | BasicPatterns.TryFinally (body, finalBody) ->
        makeTryCatch com ctx body None (Some finalBody)

    | BasicPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        makeTryCatch com ctx body (Some (catchVar, catchBody)) None

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
        makeType com ctx.GenericArgs fsExpr.Type |> Fable.This |> Fable.Value

    | BasicPatterns.FSharpFieldGet (callee, calleeType, field) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> "Unexpected static FSharpFieldGet"
                      |> addErrorAndReturnNull com (makeRangeFrom fsExpr)
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

    // When using a self reference in constructor (e.g. `type MyType() as self =`)
    // the F# compiler introduces artificial statements that we must ignore
    | BasicPatterns.FSharpFieldSet(Some(ThisVar _), RefType _, _, _)
    | BasicPatterns.FSharpFieldSet(Some(BasicPatterns.FSharpFieldGet(Some(ThisVar _), _, _)), RefType _, _, _) ->
        Fable.Null Fable.Any |> Fable.Value

    | BasicPatterns.FSharpFieldSet(callee, calleeType, field, Transform com ctx value) ->
        let range = makeRangeFrom fsExpr
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> "Unexpected static FSharpFieldSet"
                      |> addErrorAndReturnNull com range
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

    | BasicPatterns.ObjectExpr(objType, _baseCallExpr, overrides, otherOverrides) ->
        transformObjExpr com ctx objType overrides otherOverrides

    | BasicPatterns.NewObject(memb, genArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx.GenericArgs fsExpr.Type
        let args = List.map (transformExpr com ctx) args
        let genArgs = Seq.map (makeType com ctx.GenericArgs) genArgs
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
        Replacements.makeTypeTest com (makeRangeFrom fsExpr) expr typ

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

/// Is compiler generated (CompareTo...) or belongs to ignored entity?
/// (remember F# compiler puts class methods in enclosing modules)
let private isIgnoredMember (meth: FSharpMemberOrFunctionOrValue) =
    (meth.IsCompilerGenerated && Naming.ignoredCompilerGenerated.Contains meth.CompiledName)
        || Option.isSome meth.LiteralValue
        || meth.Attributes |> Seq.exists (fun att ->
            match att.AttributeType.TryFullName with
            | Some(Atts.import | Atts.global_ | Atts.emit | Atts.erase) -> true
            | _ -> false)
        || Naming.ignoredInterfaceMethods.Contains meth.CompiledName

let private transformConstructor com ctx (meth: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    ctx, [] // TODO: Constructors
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
    let methName = getMemberDeclarationName com memb
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
        let info: Fable.DeclarationInfo =
            { PrivateName = privateName
              PublicName = publicName
              IsMutable = memb.IsMutable
              HasSpread = false }
        ctx, [Fable.ValueDeclaration(fableValue, info)]

let private transformMemberFunction com ctx (memb: FSharpMemberOrFunctionOrValue) args (body: FSharpExpr) =
    let bodyCtx, args = bindMemberArgs com ctx args
    let fableBody = transformExpr com bodyCtx body
    let ctx, publicName, privateName = getPublicAndPrivateNames com ctx memb
    match fableBody with
    // Accept import expressions , e.g. let foo x y = import "foo" "myLib"
    | Fable.Import(selector, path, Fable.CustomImport, typ) ->
        transformImport com ctx typ (Some publicName) selector path
    | fableBody ->
        let info: Fable.DeclarationInfo =
            { PrivateName = privateName
              PublicName = publicName
              IsMutable = false
              HasSpread = hasSeqSpread memb }
        let fn = Fable.Function(Fable.Delegate args, fableBody)
        ctx, [Fable.ValueDeclaration(fn, info)]

let private transformMemberDecl (com: IFableCompiler) (ctx: Context) (memb: FSharpMemberOrFunctionOrValue)
                                (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    if isIgnoredMember memb
    then ctx, []
    elif isInline memb then
        // TODO: Compiler flag to output inline expressions (e.g. for REPL libs)
        com.AddInlineExpr(memb, (List.concat args, body))
        ctx, []
    elif memb.IsImplicitConstructor || memb.IsConstructor
    then transformConstructor com ctx memb args body
    elif isModuleValueForDeclaration memb
    then transformMemberValue com ctx memb body
    else transformMemberFunction com ctx memb args body

let private transformDeclarations (com: IFableCompiler) (ctx: Context) fsDecls =
    ((ctx, []), fsDecls) ||> List.fold (fun (ctx, accDecls) fsDecl ->
        let ctx, fableDecls =
            match fsDecl with
            | FSharpImplementationFileDeclaration.Entity (e, sub) ->
                transformDeclarations com ctx sub
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

type FableCompiler(com: ICompiler, implFiles: Map<string, FSharpImplementationFileContents>) =
    let usedVarNames = HashSet<string>()
    let dependencies = HashSet<string>()
    member __.UsedVarNames = set usedVarNames
    member __.Dependencies = set dependencies
    interface IFableCompiler with
        member fcom.Transform(ctx, fsExpr) =
            transformExpr fcom ctx fsExpr
        member __.TryGetInternalFile tdef =
            match tdef.Assembly.FileName with
            | Some asmPath when not(System.String.IsNullOrEmpty(asmPath)) -> None
            | _ -> Some (getEntityLocation tdef).FileName
        member fcom.TryReplace(ctx, r, t, info, thisArg, args) =
            Replacements.tryCall fcom ctx r t info thisArg args
        member fcom.GetInlineExpr memb =
            let fileName = (getMemberLocation memb).FileName |> Path.normalizePath
            if fileName <> com.CurrentFile then
                dependencies.Add(fileName) |> ignore
            let fullName = getMemberDeclarationFullname fcom memb
            com.GetOrAddInlineExpr(fullName, fun () ->
                match tryGetMemberArgsAndBody implFiles fileName memb with
                | Some(args, body) -> List.concat args, body
                | None -> failwith ("Cannot find inline member " + memb.FullName))
        member fcom.AddInlineExpr(memb, inlineExpr) =
            let fullName = getMemberDeclarationFullname fcom memb
            com.GetOrAddInlineExpr(fullName, fun () -> inlineExpr) |> ignore
        member __.AddUsedVarName varName =
            usedVarNames.Add varName |> ignore
    interface ICompiler with
        member __.Options = com.Options
        member __.FableCore = com.FableCore
        member __.CurrentFile = com.CurrentFile
        member __.GetUniqueVar() =
            com.GetUniqueVar()
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
        let ctx = Context.Create()
        let _, rootDecls = transformDeclarations fcom ctx rootDecls
        Fable.File(com.CurrentFile, rootDecls, fcom.UsedVarNames, fcom.Dependencies)
    with
    | ex -> exn (sprintf "%s (%s)" ex.Message com.CurrentFile, ex) |> raise