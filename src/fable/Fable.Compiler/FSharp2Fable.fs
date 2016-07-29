module Fable.FSharp2Fable.Compiler

open System.IO
open System.Collections.Generic
open System.Collections.Concurrent
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
            arg.TypeDefinition.Attributes
            |> tryFindAtt ((=) "KeyValueList")
            |> Option.isSome
        | _ -> false
    let unionType, range = makeType com ctx fsType, makeRange fsExpr.Range
    if isKeyValueList fsType then
        let (|KeyValue|_|) = function
            | Fable.Value(Fable.ArrayConst(Fable.ArrayValues
                            [Fable.Value(Fable.StringConst k);v],_)) -> Some(k, v)
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
        | _ -> CoreLibCall("List", Some "ofArray", false,
                flattenList range [] argExprs |> buildArgs)
        |> makeCall com (Some range) unionType

and private transformNonListNewUnionCase com ctx (fsExpr: FSharpExpr) fsType unionCase argExprs =
    let unionType, range = makeType com ctx fsType, makeRange fsExpr.Range
    match unionType with
    | ErasedUnion | OptionUnion ->
        match argExprs with
        | [] -> Fable.Value Fable.Null 
        | [expr] -> expr
        | _ -> failwithf "Erased Union Cases must have one single field: %s" unionType.FullName
        |> fun v -> Fable.Wrapped(v, unionType)
    | KeyValueUnion ->
        let v =
            match argExprs with
            | [] -> makeConst true 
            | [expr] -> expr
            | _ -> failwithf "KeyValue Union Cases must have one or zero fields: %s"
                            unionType.FullName
        Fable.TupleConst [lowerUnionCaseName unionCase; v] |> Fable.Value 
    | StringEnum ->
        // if argExprs.Length > 0 then
        //     failwithf "StringEnum must not have fields: %s" unionType.FullName
        lowerUnionCaseName unionCase
    | ListUnion ->
        failwithf "transformNonListNewUnionCase must not be used with List %O" range
    | OtherType ->
        let argExprs = [
            makeConst unionCase.Name    // Include Tag name in args
            Fable.Value(Fable.ArrayConst(Fable.ArrayValues argExprs, Fable.Any))
        ]
        if isReplaceCandidate com fsType.TypeDefinition then
            let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
            buildApplyInfo com ctx r typ (unionType.FullName) ".ctor" Fable.Constructor ([],[],[],0) (None,argExprs)
            |> replace com r
        else
            Fable.Apply (makeTypeRef com (Some range) unionType, argExprs, Fable.ApplyCons,
                            makeType com ctx fsExpr.Type, Some range)

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
    match fsExpr with
    (** ## Custom patterns *)
    | SpecialValue com ctx replacement ->
        replacement
    
    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOf (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        Fable.ForOf (ident, value, transformExpr com newContext body)
        |> makeLoop (makeRangeFrom fsExpr)
        
    | ErasableLambda (expr, argExprs) ->
        List.map (com.Transform ctx) argExprs
        |> transformComposableExpr com ctx expr

    // Pipe must come after ErasableLambda
    | Pipe (Transform com ctx callee, args) ->
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        makeApply range typ callee (List.map (transformExpr com ctx) args)
        
    | Composition (expr1, args1, expr2, args2) ->
        let lambdaArg = Naming.getUniqueVar() |> makeIdent
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        let expr1 =
            (List.map (com.Transform ctx) args1)@[Fable.Value (Fable.IdentValue lambdaArg)]
            |> transformComposableExpr com ctx expr1
        let expr2 =
            (List.map (com.Transform ctx) args2)@[expr1]
            |> transformComposableExpr com ctx expr2
        Fable.Lambda([lambdaArg], expr2) |> Fable.Value

    | BaseCons com ctx (meth, args) ->
        let args = List.map (com.Transform ctx) args
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
            let ent = Fable.Entity(Fable.Interface, None, "Fable.Core.Applicable", [], [], [], true)
            Fable.DeclaredType(ent, [Fable.Any;Fable.Any])
        Fable.Wrapped(expr, appType)

    (** ## Erased *)
    | BasicPatterns.Coerce(_targetType, Transform com ctx inpExpr) -> inpExpr
    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
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

    | BasicPatterns.ThisValue typ ->
        match typ with
        | RefType _ -> makeIdent "$self" |> Fable.IdentValue |> Fable.Value // See #124
        | _ -> Fable.This |> Fable.Value

    | BasicPatterns.Value v ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeValueFrom com ctx r typ v

    | BasicPatterns.DefaultValue (FableType com ctx typ) ->
        let valueKind =
            match typ with
            | Fable.Boolean -> Fable.BoolConst false
            | Fable.Number kind -> Fable.NumberConst (U2.Case1 0, kind)
            | _ -> Fable.Null
        Fable.Value valueKind

    (** ## Assignments *)
    // HACK to fix constructors with self references (see #124)
    | BasicPatterns.Let((_, BasicPatterns.ThisValue(RefType _)), body) ->
        let r = makeRange fsExpr.Range
        let assignment = Fable.VarDeclaration (makeIdent "$self", makeJsObject r [], true)
        makeSequential (Some r) [assignment; transformExpr com ctx body]

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
    | BasicPatterns.TraitCall (sourceTypes, traitName, flags, _typeArgs, _typeInstantiation, argExprs) ->
        let range = makeRangeFrom fsExpr
        let callee, args =
            if flags.IsInstance
            then transformExpr com ctx argExprs.Head, List.map (transformExpr com ctx) argExprs.Tail
            else makeType com ctx sourceTypes.Head |> makeTypeRef com range, List.map (transformExpr com ctx) argExprs
        let callee = makeGet range (makeUnknownFnType argExprs.Length) callee (makeConst traitName)
        Fable.Apply (callee, args, Fable.ApplyMeth, makeType com ctx fsExpr.Type, range)

    | BasicPatterns.Call(callee, meth, typArgs, methTypArgs, args) ->
        let callee, args = Option.map (com.Transform ctx) callee, List.map (com.Transform ctx) args
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) callee args

    | BasicPatterns.Application(Transform com ctx callee, _typeArgs, args) ->
        let args = List.map (transformExpr com ctx) args
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        match callee.Type.FullName, args with
        | "Fable.Core.Applicable", [Fable.Value(Fable.TupleConst args)] ->
            Fable.Apply(callee, args, Fable.ApplyMeth, typ, range)
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
        Fable.Lambda (args, transformExpr com ctx body) |> Fable.Value

    | BasicPatterns.NewDelegate(_delegateType, Transform com ctx delegateBodyExpr) ->
        makeDelegate None delegateBodyExpr

    (** ## Getters and Setters *)
    | BasicPatterns.FSharpFieldGet (callee, calleeType, FieldName fieldName) ->
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeType com ctx calleeType
                      |> makeTypeRef com (makeRangeFrom fsExpr)
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
            | None -> makeTypeRef com (makeRangeFrom fsExpr) calleeType
        Fable.Set (callee, Some (makeConst fieldName), value, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTag (Transform com ctx unionExpr, _unionType) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeGetFrom com ctx r typ unionExpr (makeConst "tag")

    | BasicPatterns.UnionCaseSet (Transform com ctx unionExpr, _type, _case, FieldName caseField, Transform com ctx valueExpr) ->
        makeRange fsExpr.Range
        |> failwithf "Unexpected UnionCaseSet %O"

    | BasicPatterns.ValueSet (valToSet, Transform com ctx valueExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx valToSet.FullType
        let valToSet = makeValueFrom com ctx r typ valToSet
        Fable.Set (valToSet, None, valueExpr, r)

    (** Instantiation *)
    | BasicPatterns.NewArray(FableType com ctx elTyp, arrExprs) ->
        makeArray elTyp (arrExprs |> List.map (transformExpr com ctx))

    | BasicPatterns.NewTuple(_, argExprs) ->
        argExprs |> List.map (transformExpr com ctx) |> Fable.TupleConst |> Fable.Value

    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, otherOverrides) ->
        // If `this` is bound to context, replace it to avoid conflicts (see #158)
        let capturedThis, ctx =
            (ctx.scope, (None, [])) ||> List.foldBack (fun (fsRef, expr) (capturedThis, scope) ->
                match expr with
                | Fable.Value Fable.This ->
                    let thisVar =
                        match capturedThis with
                        | Some v -> v
                        | None -> Naming.getUniqueVar() |> makeIdent
                    Some thisVar, (fsRef, Fable.IdentValue thisVar |> Fable.Value)::scope
                | _ -> capturedThis, (fsRef, expr)::scope)
            |> fun (capturedThis, scope) -> capturedThis, { ctx with scope = scope}
        let baseClass, baseCons =
            match baseCallExpr with
            | BasicPatterns.Call(None, meth, _, _, args)
                when not(isExternalEntity com meth.EnclosingEntity) ->
                let args = List.map (com.Transform ctx) args
                let typ, range = makeType com ctx baseCallExpr.Type, makeRange baseCallExpr.Range
                let baseClass =
                    makeTypeFromDef com ctx meth.EnclosingEntity []
                    |> makeTypeRef com (Some SourceLocation.Empty)
                    |> Some
                let baseCons =
                    Fable.Apply(Fable.Value Fable.Super, args, Fable.ApplyMeth, typ, Some range)
                    |> fun c -> Fable.Member(".ctor", Fable.Constructor, range, [], c)
                    |> Some
                baseClass, baseCons
            | _ -> None, None
        let members =
            (objType, overrides)::otherOverrides
            |> List.map (fun (typ, overrides) ->
                overrides |> List.map (fun over ->
                    let args, range = over.CurriedParameterGroups, makeRange fsExpr.Range
                    let ctx, args' = getMethodArgs com ctx true args
                    // Don't use the typ argument as the override may come
                    // from another type, like ToString()
                    let typ =
                        if over.Signature.DeclaringType.HasTypeDefinition
                        then Some over.Signature.DeclaringType.TypeDefinition
                        else None
                    // TODO: Check for indexed getter and setter also in object expressions?
                    let name = lowerToString over.Signature.Name |> Naming.removeGetSetPrefix
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
                    Fable.Member(name, kind, range, args',
                                 transformExpr com ctx over.Body,
                                 hasRestParams = hasRestParams)))
            |> List.concat
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
        | None -> objExpr
        | Some thisVar ->
            let varDecl = Fable.VarDeclaration (thisVar, Fable.Value Fable.This, false)
            Fable.Sequential([varDecl; objExpr], range)

    | BasicPatterns.NewObject(meth, typArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, []) None (List.map (com.Transform ctx) args)

    | BasicPatterns.NewRecord(NonAbbreviatedType fsType, argExprs) ->
        let recordType, range = makeType com ctx fsType, makeRange fsExpr.Range
        let argExprs = argExprs |> List.map (transformExpr com ctx)
        if isReplaceCandidate com fsType.TypeDefinition then
            let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
            buildApplyInfo com ctx r typ (recordType.FullName) ".ctor" Fable.Constructor ([],[],[],0) (None,argExprs)
            |> replace com r
        else
            Fable.Apply (makeTypeRef com (Some range) recordType, argExprs, Fable.ApplyCons,
                            makeType com ctx fsExpr.Type, Some range)

    | BasicPatterns.NewUnionCase(NonAbbreviatedType fsType, unionCase, argExprs) ->
        match fsType with
        | ListType _ -> transformNewList com ctx fsExpr fsType argExprs
        | _ ->
            List.map (com.Transform ctx) argExprs
            |> transformNonListNewUnionCase com ctx fsExpr fsType unionCase

    (** ## Type test *)
    | BasicPatterns.TypeTest (FableType com ctx typ as fsTyp, Transform com ctx expr) ->
        makeTypeTest com (makeRangeFrom fsExpr) typ expr 

    | BasicPatterns.UnionCaseTest (Transform com ctx unionExpr, FableType com ctx unionType, unionCase) ->
        match unionType with
        | ErasedUnion ->
            if unionCase.UnionCaseFields.Count <> 1 then
                failwithf "Erased Union Cases must have one single field: %s"
                          unionType.FullName
            else
                let typ = makeType com ctx unionCase.UnionCaseFields.[0].FieldType
                makeTypeTest com (makeRangeFrom fsExpr) typ unionExpr
        | OptionUnion ->
            let opKind = if unionCase.Name = "None" then BinaryEqual else BinaryUnequal
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; Fable.Value Fable.Null] opKind 
        | ListUnion ->
            let opKind = if unionCase.CompiledName = "Empty" then BinaryEqual else BinaryUnequal
            let expr = makeGet None Fable.Any unionExpr (makeConst "tail")
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [expr; Fable.Value Fable.Null] opKind 
        | StringEnum ->
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [unionExpr; lowerUnionCaseName unionCase] BinaryEqualStrict 
        | _ ->
            let left = makeGet None Fable.String unionExpr (makeConst "Case")
            let right = makeConst unionCase.Name
            makeBinOp (makeRangeFrom fsExpr) Fable.Boolean [left; right] BinaryEqualStrict

    (** Pattern Matching *)
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        let rec getTargetRefsCount map = function
            | BasicPatterns.IfThenElse (_, thenExpr, elseExpr)
            | BasicPatterns.Let(_, BasicPatterns.IfThenElse (_, thenExpr, elseExpr)) ->
                let map = getTargetRefsCount map thenExpr
                getTargetRefsCount map elseExpr
            | BasicPatterns.Let(_, e) ->
                getTargetRefsCount map e
            | BasicPatterns.DecisionTreeSuccess (idx, _) ->
                match (Map.tryFind idx map) with
                | Some refCount -> Map.remove idx map |> Map.add idx (refCount + 1)
                | None -> Map.add idx 1 map
            | _ as e ->
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
                    Fable.Lambda (targetVars, com.Transform targetCtx targetExpr)
                    |> Fable.Value
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
                |> Seq.map (fun pair -> pair.Value)
                |> Seq.map (fun (ident, lambda) ->
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

// The F# compiler considers class methods as children of the enclosing module.
// We use this type to correct that, see type DeclInfo below.
type private TmpDecl =
    | Decl of Fable.Declaration
    | Ent of Fable.Entity * string * ResizeArray<Fable.Declaration> * SourceLocation
    | IgnoredEnt

type private DeclInfo(init: Fable.Declaration list) =
    let publicNames = ResizeArray<string>()
    // Check there're no conflicting entity or function names (see #166)
    let checkPublicNameConflicts name =
        if publicNames.Contains name then
            failwithf "%s %s: %s"
                "Public types, modules or functions with same name"
                "at same level are not supported" name
        publicNames.Add name
    let decls = ResizeArray<_>(Seq.map Decl init)
    let children = Dictionary<string, TmpDecl>()
    let tryFindChild (ent: FSharpEntity) =
        if children.ContainsKey ent.FullName
        then Some children.[ent.FullName] else None
    let hasIgnoredAtt atts =
        atts |> tryFindAtt (Naming.ignoredAtts.Contains) |> Option.isSome
    member self.IsIgnoredEntity (ent: FSharpEntity) =
        ent.IsFSharpAbbreviation || ent.IsInterface
        || (hasIgnoredAtt ent.Attributes) || isAttributeEntity ent
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
    member self.AddChild (com: IFableCompiler, newChild: FSharpEntity, privateName, newChildDecls: _ list) =
        if not newChild.Accessibility.IsPrivate then
            sanitizeEntityName newChild |> checkPublicNameConflicts
        let ent = Ent (com.GetEntity newChild, privateName,
                    ResizeArray<_> newChildDecls,
                    getEntityLocation newChild |> makeRange)
        children.Add(newChild.FullName, ent)
        decls.Add(ent)
    member self.AddIgnoredChild (ent: FSharpEntity) =
        children.Add(ent.FullName, IgnoredEnt)
    member self.TryGetOwner (meth: FSharpMemberOrFunctionOrValue) =
        match tryFindChild meth.EnclosingEntity with
        | Some (Ent (ent,_,_,_)) -> Some ent
        | _ -> None
    member self.GetDeclarations (): Fable.Declaration list =
        decls |> Seq.map (function
            | IgnoredEnt -> failwith "Unexpected ignored entity"
            | Decl decl -> decl
            | Ent (ent, privateName, decls, range) ->
                let range =
                    match decls.Count with
                    | 0 -> range
                    | _ -> range + (Seq.last decls).Range
                Fable.EntityDeclaration(ent, privateName, List.ofSeq decls, range))
        |> Seq.toList
    
let private transformMemberDecl (com: IFableCompiler) ctx (declInfo: DeclInfo)
    (meth: FSharpMemberOrFunctionOrValue) (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let addMethod() =
        let memberName, memberKind = sanitizeMethodName com meth
        let ctx', args' = getMethodArgs com ctx meth.IsInstanceMember args
        let body =
            let ctx' =
                match meth.IsImplicitConstructor, declInfo.TryGetOwner meth with
                | true, Some(EntityKind(Fable.Class(Some(fullName, _)))) ->
                    { ctx' with baseClass = Some fullName }
                | _ -> ctx'
            transformExpr com ctx' body
        let ctx, privateName =
            // Bind module member names to context to prevent
            // name clashes (they will become variables in JS)
            if meth.EnclosingEntity.IsFSharpModule then
                let typ = makeType com ctx meth.FullType
                let ctx, privateName = bindIdent com ctx typ (Some meth) memberName
                ctx, Some (privateName.name)
            // Make some checks on custom type operators
            elif memberName.StartsWith "op_" then
                let arg = meth.CurriedParameterGroups.[0].[0]
                if not arg.Type.HasTypeDefinition
                    || arg.Type.TypeDefinition <> meth.EnclosingEntity
                then failwithf "First argument of custom type operators must be same as type: %s" meth.FullName
                elif Regex.IsMatch(memberName, "_\d+$")
                then failwithf "Custom type operator overloads is not supported: %s" meth.FullName
                else ctx, None
            else ctx, None
        let entMember =
            Fable.Member(memberName, memberKind,
                getRefLocation meth |> makeRange, args', body,
                meth.Attributes |> Seq.choose (makeDecorator com) |> Seq.toList,
                isPublic = (not meth.Accessibility.IsPrivate && not meth.IsCompilerGenerated),
                isMutable = meth.IsMutable,
                isStatic = not meth.IsInstanceMember,
                hasRestParams = hasRestParams meth,
                ?privateName = privateName)
            |> Fable.MemberDeclaration
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
            let args = args |> Seq.collect id |> Seq.toList
            com.AddInlineExpr meth.FullName (args, body)
            ctx
    else addMethod()
    |> fun ctx -> declInfo, ctx
   
let rec private transformEntityDecl
    (com: IFableCompiler) ctx (declInfo: DeclInfo) (ent: FSharpEntity) subDecls =
    if declInfo.IsIgnoredEntity ent then
        declInfo.AddIgnoredChild ent
        declInfo, ctx
    else
        // Unions, records and F# exceptions don't have a constructor
        let cons =
            if ent.IsFSharpUnion
            then [makeUnionCons()]
            elif ent.IsFSharpRecord || ent.IsFSharpExceptionDeclaration
            then ent.FSharpFields
                 |> Seq.map (fun x -> x.Name) |> Seq.toList
                 |> makeRecordCons
                 |> List.singleton
            else []
        let compareMeths =
            // If F# union or records implement System.IComparable (in that case they
            // allways implement System.Equatable too) generate Equals and CompareTo methods
            // Note: F# compiler generates these methods too but see `IsIgnoredMethod`
            // Note: If `ReferenceEqualityAttribute` is used, the type doesn't implement IComparable 
            let fableEnt = com.GetEntity ent
            if ent.IsFSharpUnion && fableEnt.HasInterface "System.IComparable"
            then makeUnionCompareMethods com
            elif ent.IsFSharpRecord && fableEnt.HasInterface "System.IComparable"
            then makeRecordCompareMethods com
            else []
        let childDecls = transformDeclarations com ctx (cons@compareMeths) subDecls
        // Even if a module is marked with Erase, transform its members
        // in case they contain inline methods
        if isErased ent
        then declInfo, ctx
        else
            // Bind entity name to context to prevent name
            // clashes (it will become a variable in JS)
            let ctx, ident = sanitizeEntityName ent |> bindIdent com ctx Fable.Any None
            declInfo.AddChild(com, ent, ident.name, childDecls)
            declInfo, ctx

and private transformDeclarations (com: IFableCompiler) ctx init decls =
    let declInfo, _ =
        decls |> List.fold (fun (declInfo: DeclInfo, ctx) decl ->
            match decl with
            | FSharpImplementationFileDeclaration.Entity (e, sub) ->
                if e.IsFSharpAbbreviation
                then declInfo, ctx
                else transformEntityDecl com ctx declInfo e sub
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
                transformMemberDecl com ctx declInfo meth args body
            | FSharpImplementationFileDeclaration.InitAction (Transform com ctx e as fe) ->
                declInfo.AddInitAction (Fable.ActionDeclaration (e, makeRange fe.Range))
                declInfo, ctx
        ) (DeclInfo init, ctx)
    declInfo.GetDeclarations()
    
let private makeFileMap (rootEntities: #seq<FSharpEntity>) =
    rootEntities
    |> Seq.groupBy (fun ent -> (getEntityLocation ent).FileName)
    |> Seq.map (fun (file, ents) -> 
        let ent =
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
        file, ent)
    |> Map
    
// Make inlineExprs static so they can be reused in --watch compilations
let private inlineExprs =
    ConcurrentDictionary<string, FSharpMemberOrFunctionOrValue list * FSharpExpr>()

type FableCompiler(com: ICompiler, projs: Fable.Project list, entities: ConcurrentDictionary<string, Fable.Entity>) =
    let refAssemblies =
        projs |> Seq.choose (fun p -> p.AssemblyFileName) |> Set
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
            // In F# scripts the location of referenced libraries
            // becomes the .fsx file, so check first if the entity belongs
            // to an assembly already compiled (external to the project)
            match tdef.Assembly.FileName with
            | Some assembly when not(refAssemblies.Contains assembly) -> None
            | _ ->
                let file = (getEntityLocation tdef).FileName
                if projs |> Seq.exists (fun p -> p.FileMap.ContainsKey file)
                then Some file
                else None
        member fcom.GetEntity tdef =
            entities.GetOrAdd (defaultArg tdef.TryFullName tdef.CompiledName,
                               fun _ -> makeEntity fcom tdef)
        member fcom.TryGetInlineExpr fullName =
            let success, expr = inlineExprs.TryGetValue fullName
            if success then Some expr else None
        member fcom.AddInlineExpr fullName inlineExpr =
            inlineExprs.AddOrUpdate(fullName,
                System.Func<_,_>(fun _ -> inlineExpr),
                System.Func<_,_,_>(fun _ _ -> inlineExpr))
            |> ignore
        member fcom.AddUsedVarName varName =
            usedVarNames.Add varName |> ignore
        member fcom.ReplacePlugins =
            replacePlugins
    interface ICompiler with
        member __.Options = com.Options
        member __.Plugins = com.Plugins
        member __.AddLog msg = com.AddLog msg
        member __.GetLogs() = com.GetLogs()
        
let private addInjections (com: ICompiler) (curProj: Fable.Project) =
    let createDeclaration (injection: IInjection) =
        let args =
            match injection.ArgumentsLength with
            | 0 -> []
            | l -> [1..l] |> List.map (fun i -> "$arg" + (string i) |> makeIdent)
        let body =
            args |> List.map (Fable.IdentValue >> Fable.Value)
            |> injection.GetBody
        Fable.Member(injection.Name, Fable.Method,
            SourceLocation.Empty, args, body)
        |> Fable.MemberDeclaration
    com.Plugins
    |> List.choose (function
        | path, (:? IInjectPlugin as plugin) -> Some (path, plugin)
        | _ -> None)
    |> List.collect (fun (path, plugin) ->
        try plugin.Inject com |> List.map createDeclaration
        with ex -> failwithf "Error in plugin %s: %s" path ex.Message)
    |> function
        | [] -> []
        | rootDecls ->
            let fileName = Path.Combine(curProj.BaseDir, Naming.fableInjectFile)
            let rootEnt = Fable.Entity.CreateRootModule fileName ""
            [Fable.File(fileName, rootEnt, rootDecls)]

type FSProjectInfo =
    {
        projectOpts: FSharpProjectOptions
        fileMask: string option
        dependencies: Map<string, string list>
    }
    member info.IsMasked(file: FSharpImplementationFileContents) =
        let arePathsEqual p1 p2 =
            let normalize = Path.GetFullPath >> Path.normalizePath
            (normalize p1) = (normalize p2)
        match info.fileMask with
        | Some mask ->
            if arePathsEqual file.FileName mask
            then true
            else info.dependencies |> Map.exists (fun key deps ->
                arePathsEqual file.FileName key && List.exists (arePathsEqual mask) deps)
        | None -> true

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
    let curProj =
        let projName = Path.GetFileNameWithoutExtension com.Options.projFile
        let fileMap = makeFileMap parsedProj.AssemblySignature.Entities
        let baseDir = Path.GetDirectoryName com.Options.projFile
        Fable.Project(projName, baseDir, fileMap)
    let refProjs =
        projInfo.projectOpts.ReferencedProjects
        |> Seq.choose (fun (assemblyPath, opts) ->
            let projName = Path.GetFileNameWithoutExtension opts.ProjectFileName
            parsedProj.ProjectContext.GetReferencedAssemblies()
            |> Seq.tryFind (fun a -> a.FileName = Some assemblyPath)
            |> function
            | Some assembly when not(com.Options.refs.ContainsKey projName) ->
                failwithf "Cannot find import path for referenced project %s. %s"
                            projName "Have you forgotten --refs argument?"
            | Some assembly ->
                let fileMap = makeFileMap assembly.Contents.Entities
                let baseDir = Path.GetDirectoryName opts.ProjectFileName |> Path.GetFullPath
                Fable.Project(projName, baseDir, fileMap, assemblyPath, com.Options.refs.[projName])
                |> Some
            | None -> None)
        |> Seq.toList
    let refAssemblies =
        parsedProj.ProjectContext.GetReferencedAssemblies()
        |> List.choose (fun assembly ->
            match assembly.FileName with
            | Some asmFullName ->
                let asmName = Path.GetFileNameWithoutExtension asmFullName
                match Map.tryFind asmName com.Options.refs with
                | None -> None
                | Some importPath ->
                    let fileMap = makeFileMap assembly.Contents.Entities
                    let baseDir = fileMap |> Seq.map (fun kv -> kv.Key) |> Path.getCommonBaseDir
                    Fable.Project(asmName, baseDir, fileMap, asmFullName, importPath)
                    |> Some
            | None -> None)
    let projs = curProj::(refProjs @ refAssemblies)
    let entities = ConcurrentDictionary<string, Fable.Entity>()
    parsedProj.AssemblyContents.ImplementationFiles
    |> Seq.where (fun file ->
        curProj.FileMap.ContainsKey file.FileName
        && not (Naming.ignoredFilesRegex.IsMatch file.FileName)
        && projInfo.IsMasked file)
    |> Seq.choose (fun file ->
        try
            let fcom = FableCompiler(com, projs, entities)
            let rootEnt, rootDecls =
                let rootNs = curProj.FileMap.[file.FileName]
                let rootEnt, rootDecls = getRootDecls rootNs None file.Declarations
                let rootDecls = transformDeclarations fcom Context.Empty [] rootDecls
                match rootEnt with
                | Some rootEnt when isErased rootEnt -> makeEntity fcom rootEnt, []
                | Some rootEnt -> makeEntity fcom rootEnt, rootDecls
                | None -> Fable.Entity.CreateRootModule file.FileName rootNs, rootDecls
            match rootDecls with
            | [] -> None
            | rootDecls -> Fable.File(file.FileName, rootEnt, rootDecls, fcom.UsedVarNames) |> Some
        with
        | ex -> exn (sprintf "%s (%s)" ex.Message file.FileName, ex) |> raise
    )
    // In first compilation (fileMask is None) add injections
    |> Seq.append (if projInfo.fileMask.IsNone
                   then addInjections com curProj else [])
    |> fun seq -> projs, seq
