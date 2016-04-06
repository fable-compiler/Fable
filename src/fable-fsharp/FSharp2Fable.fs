module Fable.FSharp2Fable.Compiler

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fable
open Fable.AST
open Fable.AST.Fable.Util

open Patterns
open Types
open Identifiers
open Util

// Special values like seq, async, String.Empty...
let private (|SpecialValue|_|) com ctx = function
    | BasicPatterns.ILFieldGet (None, typ, fieldName) as fsExpr when typ.HasTypeDefinition ->
        match typ.TypeDefinition.FullName, fieldName with
        | "System.String", "Empty" -> Some (makeConst "")
        | "System.TimeSpan", "Zero" ->
            Fable.Wrapped(makeConst 0, makeType com ctx fsExpr.Type) |> Some
        | "System.DateTime", "MaxValue"
        | "System.DateTime", "MinValue" ->
            CoreLibCall("Date", Some (Naming.lowerFirst fieldName), false, [])
            |> makeCall com (makeRangeFrom fsExpr) (makeType com ctx fsExpr.Type) |> Some 
        | _ -> None
    | _ -> None
    
let private (|BaseCons|_|) com ctx = function
    | BasicPatterns.Call(None, meth, _, _, args) ->
        let methOwnerName (meth: FSharpMemberOrFunctionOrValue) =
            sanitizeEntityName meth.EnclosingEntity
        match ctx.baseClass with
        | Some baseFullName when meth.DisplayName = "( .ctor )"
                            && (methOwnerName meth) = baseFullName ->
            if not meth.IsImplicitConstructor then
                failwithf "Inheritance is only possible with base class implicit constructor: %s"
                          baseFullName
            Some (meth, args)
        | _ -> None
    | _ -> None

let private (|FSharpExceptionGet|_|) = function
    | BasicPatterns.FSharpFieldGet (Some callee, fsType, fieldInfo)
        when fsType.HasTypeDefinition && fsType.TypeDefinition.IsFSharpExceptionDeclaration ->
            Some (callee, fsType.TypeDefinition, fieldInfo)
    | _ -> None

let rec private transformExpr (com: IFableCompiler) ctx fsExpr =
    match fsExpr with
    (** ## Custom patterns *)
    | SpecialValue com ctx replacement ->
        replacement
    
    // TODO: Detect if it's ResizeArray and compile as FastIntegerForLoop?
    | ForOf (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        Fable.ForOf (ident, value, transformExpr com newContext body)
        |> makeLoop (makeRangeFrom fsExpr)
        
    | ErasableLambda (meth, typArgs, methTypArgs, methArgs) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs)
            None (List.map (com.Transform ctx) methArgs)

    // Pipe must come after ErasableLambda
    | Pipe (Transform com ctx callee, args) ->
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        makeApply range typ callee (List.map (transformExpr com ctx) args)
        
    | Composition (meth1, typArgs1, methTypArgs1, args1, meth2, typArgs2, methTypArgs2, args2) ->
        let lambdaArg = makeIdent "$arg"
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        let expr1 =
            (List.map (com.Transform ctx) args1)@[Fable.Value (Fable.IdentValue lambdaArg)]
            |> makeCallFrom com ctx r typ meth1 (typArgs1, methTypArgs1) None
        let expr2 =
            (List.map (com.Transform ctx) args2)@[expr1]
            |> makeCallFrom com ctx r typ meth2 (typArgs2, methTypArgs2) None
        Fable.Lambda([lambdaArg], expr2) |> Fable.Value

    | BaseCons com ctx (meth, args) ->
        let args = List.map (com.Transform ctx) args
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        Fable.Apply(Fable.Value Fable.Super, args, Fable.ApplyMeth, typ, range)

    | FSharpExceptionGet (Transform com ctx exExpr, exEnt, FieldName fieldName) ->
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        let i = exEnt.FSharpFields |> Seq.findIndex (fun x -> x.Name = fieldName)
        makeGet range typ exExpr (sprintf "data%i" i |> makeConst)
            
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
        | _ -> failwithf "Unexpected loop in %A: %A" fsExpr.Range fsExpr

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
        | NumberKind kind -> Fable.Number kind |> Fable.PrimitiveType
        | _ -> Fable.UnknownType
        |> makeArray <| arrExprs

    | BasicPatterns.Const(value, FableType com ctx typ) ->
        let e = makeConst value
        if e.Type = typ then e
        // Enumerations are compiled as const but they have a different type
        else Fable.Wrapped (e, typ)

    | BasicPatterns.BaseValue typ ->
        Fable.Super |> Fable.Value 

    | BasicPatterns.ThisValue typ ->
        Fable.This |> Fable.Value 

    | BasicPatterns.Value thisVar when thisVar.IsMemberThisValue ->
        Fable.This |> Fable.Value 

    | BasicPatterns.Value v ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeValueFrom com ctx r typ v

    | BasicPatterns.DefaultValue (FableType com ctx typ) ->
        let valueKind =
            match typ with
            | Fable.PrimitiveType Fable.Boolean -> Fable.BoolConst false
            | Fable.PrimitiveType (Fable.Number kind) -> Fable.NumberConst (U2.Case1 0, kind)
            | _ -> Fable.Null
        Fable.Value valueKind

    (** ## Assignments *)
    | BasicPatterns.Let((var, Transform com ctx value), body) ->
        let ctx, ident = bindIdentFrom com ctx var
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
    | BasicPatterns.TraitCall (_sourceTypes, traitName, _typeArgs, _typeInstantiation, argExprs) ->
        // printfn "TraitCall detected in %A: %A" fsExpr.Range fsExpr // TODO: Check
        let range = makeRangeFrom fsExpr
        let callee, args = transformExpr com ctx argExprs.Head, List.map (transformExpr com ctx) argExprs.Tail
        let callee = makeGet range (Fable.PrimitiveType (Fable.Function argExprs.Length)) callee (makeConst traitName)
        Fable.Apply (callee, args, Fable.ApplyMeth, makeType com ctx fsExpr.Type, range)

    | BasicPatterns.Call(callee, meth, typArgs, methTypArgs, args) ->
        let callee, args = Option.map (com.Transform ctx) callee, List.map (com.Transform ctx) args
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, methTypArgs) callee args

    | BasicPatterns.Application(Transform com ctx callee, _typeArgs, args) ->
        let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        makeApply range typ callee (List.map (transformExpr com ctx) args)
        
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
        makeDelegate delegateBodyExpr

    (** ## Getters and Setters *)
    // TODO: Change name of automatically generated fields
    | BasicPatterns.FSharpFieldGet (callee, calleeType, FieldName fieldName) ->
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeType com ctx calleeType
                      |> makeTypeRef com (makeRange fsExpr.Range)
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeGetFrom com ctx r typ callee (makeConst fieldName)

    | BasicPatterns.TupleGet (_tupleType, tupleElemIndex, Transform com ctx tupleExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeGetFrom com ctx r typ tupleExpr (makeConst tupleElemIndex)

    | BasicPatterns.UnionCaseGet (Transform com ctx unionExpr, FableType com ctx unionType, unionCase, FieldName fieldName) ->
        match unionType with
        | ErasedUnion | OptionUnion -> unionExpr
        | ListUnion ->
            makeGet (makeRangeFrom fsExpr) (makeType com ctx fsExpr.Type)
                    unionExpr (Naming.lowerFirst fieldName |> makeConst)
        | OtherType ->
            let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
            let i = unionCase.UnionCaseFields |> Seq.findIndex (fun x -> x.Name = fieldName)
            makeGet range typ unionExpr (sprintf "data%i" i |> makeConst)

    | BasicPatterns.ILFieldSet (callee, typ, fieldName, value) ->
        failwithf "Found unsupported ILField reference in %A: %A" fsExpr.Range fsExpr

    // TODO: Change name of automatically generated fields
    | BasicPatterns.FSharpFieldSet (callee, FableType com ctx calleeType, FieldName fieldName, Transform com ctx value) ->
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeTypeRef com (makeRange fsExpr.Range) calleeType
        Fable.Set (callee, Some (makeConst fieldName), value, makeRangeFrom fsExpr)

    | BasicPatterns.UnionCaseTag (Transform com ctx unionExpr, _unionType) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeGetFrom com ctx r typ unionExpr (makeConst "tag")

    | BasicPatterns.UnionCaseSet (Transform com ctx unionExpr, _type, _case, FieldName caseField, Transform com ctx valueExpr) ->
        failwith "Unexpected UnionCaseSet"

    | BasicPatterns.ValueSet (valToSet, Transform com ctx valueExpr) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx valToSet.FullType
        let valToSet = makeValueFrom com ctx r typ valToSet
        Fable.Set (valToSet, None, valueExpr, r)

    (** Instantiation *)
    | BasicPatterns.NewArray(FableType com ctx elTyp, arrExprs) ->
        makeArray elTyp (arrExprs |> List.map (transformExpr com ctx))

    | BasicPatterns.NewTuple(_, argExprs) ->
        (argExprs |> List.map (transformExpr com ctx) |> Fable.ArrayValues, Fable.Tuple)
        |> Fable.ArrayConst |> Fable.Value

    | BasicPatterns.ObjectExpr(objType, baseCallExpr, overrides, otherOverrides) ->
        let lowerFirstKnownInterfaces typ name =
            match typ with
            | Some typ ->
                let typName = sanitizeEntityName typ
                if Naming.knownInterfaces.Contains typName
                then Naming.lowerFirst name
                else name
            | None -> name
        let baseClass, baseCons =
            match baseCallExpr with
            | BasicPatterns.Call(None, meth, _, _, args)
                when not(isExternalEntity com meth.EnclosingEntity) ->
                let args = List.map (com.Transform ctx) args
                let typ, range = makeType com ctx baseCallExpr.Type, makeRange baseCallExpr.Range
                let baseClass =
                    makeTypeFromDef com meth.EnclosingEntity
                    |> makeTypeRef com SourceLocation.Empty
                    |> Some
                let baseCons =
                    Fable.Apply(Fable.Value Fable.Super, args, Fable.ApplyMeth, typ, Some range)
                    |> fun c -> Fable.Member(Fable.Constructor, range, [], c, [], true, false, false)
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
                    let kind =
                        let name =
                            over.Signature.Name
                            |> Naming.removeParens
                            |> Naming.removeGetSetPrefix
                            |> lowerFirstKnownInterfaces typ
                        match over.Signature.Name with
                        | Naming.StartsWith "get_" _ -> Fable.Getter (name, false)
                        | Naming.StartsWith "set_" _ -> Fable.Setter name
                        | _ -> Fable.Method name
                    // FSharpObjectExprOverride.CurriedParameterGroups doesn't offer
                    // information about ParamArray, we need to check the source method.
                    let hasRestParams =
                        match typ with
                        | None -> false
                        | Some typ ->
                            typ.MembersFunctionsAndValues
                            |> Seq.tryFind (fun x -> x.DisplayName = over.Signature.Name)
                            |> function Some m -> hasRestParams m | None -> false
                    Fable.Member(kind, range, args',
                                    transformExpr com ctx over.Body,
                                    hasRestParams = hasRestParams)))
            |> List.concat
        let members =
            match baseCons with
            | Some baseCons -> baseCons::members
            | None -> members
        let interfaces =
            objType::(otherOverrides |> List.map fst)
            |> List.map (fun x -> sanitizeEntityName x.TypeDefinition)
            |> List.distinct
        Fable.ObjExpr (members, interfaces, baseClass, makeRangeFrom fsExpr)

    // TODO: Check for erased constructors with property assignment (Call + Sequential)
    | BasicPatterns.NewObject(meth, typArgs, args) ->
        let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
        makeCallFrom com ctx r typ meth (typArgs, []) None (List.map (com.Transform ctx) args)

    | BasicPatterns.NewRecord(NonAbbreviatedType fsType, argExprs) ->
        let recordType, range = makeType com ctx fsType, makeRange fsExpr.Range
        let argExprs = argExprs |> List.map (transformExpr com ctx)
        if isReplaceCandidate com fsType.TypeDefinition then
            let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
            replace com ctx r typ (recordType.FullName) ".ctor" ([],[],[]) (None,argExprs)
        else
            Fable.Apply (makeTypeRef com range recordType, argExprs, Fable.ApplyCons,
                            makeType com ctx fsExpr.Type, Some range)

    | BasicPatterns.NewUnionCase(NonAbbreviatedType fsType, unionCase, argExprs) ->
        let unionType, range = makeType com ctx fsType, makeRange fsExpr.Range
        match unionType with
        | ErasedUnion | OptionUnion ->
            match List.map (transformExpr com ctx) argExprs with
            | [] -> Fable.Value Fable.Null 
            | [expr] -> expr
            | _ -> failwithf "Erased Union Cases must have one single field: %A" unionType
        | ListUnion ->
            let buildArgs args =
                let args = args |> List.rev |> (List.map (transformExpr com ctx))
                Fable.Value (Fable.ArrayConst (Fable.ArrayValues args, Fable.DynamicArray))
            let rec ofArray accArgs = function
                | [] ->
                    CoreLibCall("List", Some "ofArray", false, [buildArgs accArgs])
                | arg::[BasicPatterns.NewUnionCase(_, _, rest)] ->
                    ofArray (arg::accArgs) rest
                | arg::[Transform com ctx list2] ->
                    CoreLibCall("List", Some "ofArray", false, (buildArgs (arg::accArgs))::[list2])
                | _ ->
                    failwithf "Unexpected List constructor %A at %A" fsExpr fsExpr.Range
            match argExprs with
            | [] -> CoreLibCall("List", None, true, [])
            | _ -> ofArray [] argExprs
            |> makeCall com (Some range) unionType
        | OtherType ->
            let argExprs =
                // Include Tag name in args
                let tag = makeConst unionCase.Name
                tag::(List.map (transformExpr com ctx) argExprs)
            if isReplaceCandidate com fsType.TypeDefinition then
                let r, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
                replace com ctx r typ (unionType.FullName) ".ctor" ([],[],[]) (None,argExprs)
            else
                Fable.Apply (makeTypeRef com range unionType, argExprs, Fable.ApplyCons,
                                makeType com ctx fsExpr.Type, Some range)

    (** ## Type test *)
    | BasicPatterns.TypeTest (FableType com ctx typ as fsTyp, Transform com ctx expr) ->
        makeTypeTest com (makeRangeFrom fsExpr) typ expr 

    | BasicPatterns.UnionCaseTest (Transform com ctx unionExpr, FableType com ctx unionType, unionCase) ->
        let boolType = Fable.PrimitiveType Fable.Boolean
        match unionType with
        | ErasedUnion ->
            if unionCase.UnionCaseFields.Count <> 1 then
                failwithf "Erased Union Cases must have one single field: %A" unionType
            else
                let typ = makeType com ctx unionCase.UnionCaseFields.[0].FieldType
                makeTypeTest com (makeRangeFrom fsExpr) typ unionExpr
        | OptionUnion ->
            let opKind = if unionCase.Name = "None" then BinaryEqual else BinaryUnequal
            makeBinOp (makeRangeFrom fsExpr) boolType [unionExpr; Fable.Value Fable.Null] opKind 
        | ListUnion ->
            let opKind = if unionCase.CompiledName = "Empty" then BinaryEqual else BinaryUnequal
            let expr = makeGet None Fable.UnknownType unionExpr (makeConst "tail")
            makeBinOp (makeRangeFrom fsExpr) boolType [expr; Fable.Value Fable.Null] opKind 
        | OtherType ->
            let left = makeGet None (Fable.PrimitiveType Fable.String) unionExpr (makeConst "tag")
            let right = makeConst unionCase.Name
            makeBinOp (makeRangeFrom fsExpr) boolType [left; right] BinaryEqualStrict

    (** Pattern Matching *)
    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        let rec getTargetRefsCount map = function
            | BasicPatterns.IfThenElse (_, thenExpr, elseExpr)
            | BasicPatterns.Let(_, BasicPatterns.IfThenElse (_, thenExpr, elseExpr)) ->
                let map = getTargetRefsCount map thenExpr
                getTargetRefsCount map elseExpr
            | BasicPatterns.DecisionTreeSuccess (idx, _) ->
                match (Map.tryFind idx map) with
                | Some refCount -> Map.remove idx map |> Map.add idx (refCount + 1)
                | None -> Map.add idx 1 map
            | _ as e ->
                failwithf "Unexpected DecisionTree branch in %A: %A" e.Range e
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
                let ctx, ident = bindIdent ctx lambda.Type None (sprintf "$target%i" k)
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

    (** Not implemented *)
    | BasicPatterns.ILAsm _
    | BasicPatterns.ILFieldGet _
    | BasicPatterns.Quote _ // (quotedExpr)
    | BasicPatterns.AddressOf _ // (lvalueExpr)
    | BasicPatterns.AddressSet _ // (lvalueExpr, rvalueExpr)
    | _ -> failwithf "Cannot compile expression in %A: %A" fsExpr.Range fsExpr

// The F# compiler considers class methods as children of the enclosing module.
// We use this type to correct that, see type DeclInfo below.
type private TmpDecl =
    | Decl of Fable.Declaration
    | Ent of Fable.Entity * SourceLocation * ResizeArray<Fable.Declaration>
    | IgnoredEnt

type private DeclInfo(init: Fable.Declaration list) =
    let ignoredAtts = set ["Erase"; "Import"; "Global"; "Emit"]
    let decls = ResizeArray<_>(Seq.map Decl init)
    let children = System.Collections.Generic.Dictionary<string, TmpDecl>()
    let tryFindChild (ent: FSharpEntity) =
        if children.ContainsKey ent.FullName
        then Some children.[ent.FullName] else None
    let hasIgnoredAtt atts =
        atts |> tryFindAtt (ignoredAtts.Contains) |> Option.isSome
    member self.IsIgnoredEntity (ent: FSharpEntity) =
        ent.IsInterface || (hasIgnoredAtt ent.Attributes) || isAttributeEntity ent
    /// Is compiler generated (CompareTo...) or belongs to ignored entity?
    /// (remember F# compiler puts class methods in enclosing modules)
    member self.IsIgnoredMethod (meth: FSharpMemberOrFunctionOrValue) =
        if (meth.IsCompilerGenerated && Naming.ignoredCompilerGenerated.Contains meth.DisplayName)
            || (hasIgnoredAtt meth.Attributes)
        then true
        else match tryFindChild meth.EnclosingEntity with
             | Some IgnoredEnt -> true
             | _ -> false
    member self.AddMethod (meth: FSharpMemberOrFunctionOrValue, methDecl: Fable.Declaration) =
        match tryFindChild meth.EnclosingEntity with
        | None -> decls.Add(Decl methDecl)
        | Some (Ent (_, _, entDecls)) -> entDecls.Add methDecl
        | Some _ -> () // TODO: fail?
    member self.AddInitAction (actionDecl: Fable.Declaration) =
        decls.Add(Decl actionDecl)
    member self.AddChild (com: IFableCompiler, newChild, newChildDecls: _ list) =
        let ent = Ent (
                    com.GetEntity newChild,
                    makeRange newChild.DeclarationLocation,
                    ResizeArray<_> newChildDecls)
        children.Add(newChild.FullName, ent)
        decls.Add(ent)
    member self.AddIgnoredChild (ent: FSharpEntity) =
        children.Add(ent.FullName, IgnoredEnt)
    member self.TryGetOwner (meth: FSharpMemberOrFunctionOrValue) =
        match tryFindChild meth.EnclosingEntity with
        | Some (Ent (ent, _, _)) -> Some ent
        | _ -> None
    member self.GetDeclarations (): Fable.Declaration list =
        decls |> Seq.map (function
            | IgnoredEnt -> failwith "Unexpected ignored entity"
            | Decl decl -> decl
            | Ent (ent, range, decls) ->
                let range =
                    match decls.Count with
                    | 0 -> range
                    | _ -> range + (Seq.last decls).Range
                Fable.EntityDeclaration(ent, List.ofSeq decls, range))
        |> Seq.toList
    
let private transformMemberDecl (com: IFableCompiler) ctx (declInfo: DeclInfo)
    (meth: FSharpMemberOrFunctionOrValue) (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    match meth with
    | meth when declInfo.IsIgnoredMethod meth -> ctx
    | meth when isInline meth ->
        let args = args |> Seq.collect id |> Seq.toList
        com.AddInlineExpr meth.FullName (args, body)
        ctx
    | _ ->
        let memberKind =
            let name = sanitizeMethodName com meth
            getMemberKind name meth
        let ctx', args' = getMethodArgs com ctx meth.IsInstanceMember args
        let body =
            let ctx' =
                match meth.IsImplicitConstructor, declInfo.TryGetOwner meth with
                | true, Some(EntityKind(Fable.Class(Some(fullName, _)))) ->
                    { ctx' with baseClass = Some fullName }
                | _ -> ctx'
            transformExpr com ctx' body
        let entMember =
            Fable.Member(memberKind,
                makeRange meth.DeclarationLocation, args', body,
                meth.Attributes |> Seq.choose (makeDecorator com) |> Seq.toList,
                meth.Accessibility.IsPublic, not meth.IsInstanceMember, hasRestParams meth)
            |> Fable.MemberDeclaration
        declInfo.AddMethod (meth, entMember)
        // Bind sanitized module member names to context to prevent
        // name clashes (they will become variables in JS)
        match memberKind with
        | Fable.Method name | Fable.Getter (name, _)
            when meth.EnclosingEntity.IsFSharpModule ->
            Naming.sanitizeIdent (fun _ -> false) name
            |> bindIdent ctx Fable.UnknownType None |> fst
        | _ -> ctx
    |> fun ctx -> declInfo, ctx
   
// TODO: Check that nested entities' names don't clash with parent members
let rec private transformEntityDecl
    (com: IFableCompiler) ctx (declInfo: DeclInfo) (ent: FSharpEntity) subDecls =
    if declInfo.IsIgnoredEntity ent then
        declInfo.AddIgnoredChild ent
        declInfo, ctx
    else
        // Unions and Records don't have a constructor, generate it
        let init =
            if ent.IsFSharpUnion
            then [makeUnionCons()]
            elif ent.IsFSharpExceptionDeclaration
            then [makeExceptionCons()]
            elif ent.IsFSharpRecord
            then ent.FSharpFields
                 |> Seq.map (fun x -> x.DisplayName) |> Seq.toList
                 |> makeRecordCons
                 |> List.singleton
            else []
        let childDecls = transformDeclarations com ctx init subDecls
        declInfo.AddChild (com, ent, childDecls)
        // Bind sanitized entity name to context to prevent
        // name clashes (it will become a variable in JS)
        let ctx, _ =
            ent.DisplayName
            |> Naming.sanitizeIdent (fun _ -> false)
            |> bindIdent ctx Fable.UnknownType None
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
    |> Seq.groupBy (fun ent -> ent.DeclarationLocation.FileName)
    |> Seq.map (fun (file, ents) -> 
        let ent =
            match List.ofSeq ents with
            | [] -> failwith "Unexpected empty file"
            | [ent] ->
                if ent.IsFSharpModule
                then ent.FullName
                else defaultArg ent.Namespace ""
            | ent::ents ->
                let rootNs =
                    ents
                    |> List.map (fun ent -> ent.FullName)
                    |> Naming.getCommonPrefix
                if rootNs.EndsWith(".")
                then rootNs.Substring(0, rootNs.Length - 1)
                else rootNs
        file, ent)
    |> Map
    
// Make inlineExprs static so they can be reused in --watch compilations
let private inlineExprs =
    System.Collections.Concurrent.ConcurrentDictionary<
        string, FSharpMemberOrFunctionOrValue list * FSharpExpr>()

let private makeCompiler (com: ICompiler) (projs: Fable.Project list) =
    let entities =
        System.Collections.Concurrent.ConcurrentDictionary<string, Fable.Entity>()
    let refAssemblies =
        projs |> Seq.choose (fun p -> p.AssemblyFileName) |> Set
    let replacePlugins =
        com.Plugins |> List.choose (function
            | :? IReplacePlugin as plugin -> Some plugin
            | _ -> None)
    { new IFableCompiler with
        member fcom.Transform ctx fsExpr =
            transformExpr fcom ctx fsExpr
        member fcom.GetInternalFile tdef =
            // In F# scripts the DeclarationLocation of referenced libraries
            // becomes the .fsx file, so check first if the entity belongs
            // to an assembly already compiled (external to the project)
            match tdef.Assembly.FileName with
            | Some assembly when not(refAssemblies.Contains assembly) -> None
            | _ ->
                let file = tdef.DeclarationLocation.FileName
                if projs |> Seq.exists (fun p -> p.FileMap.ContainsKey file)
                then Some file
                else None
        member fcom.GetEntity tdef =
            entities.GetOrAdd (tdef.FullName, fun _ -> makeEntity fcom tdef)
        member fcom.TryGetInlineExpr fullName =
            let success, expr = inlineExprs.TryGetValue fullName
            if success then Some expr else None
        member fcom.AddInlineExpr fullName inlineExpr =
            inlineExprs.AddOrUpdate(fullName,
                System.Func<_,_>(fun _ -> inlineExpr),
                System.Func<_,_,_>(fun _ _ -> inlineExpr))
            |> ignore
        member fcom.ReplacePlugins =
            replacePlugins
    interface ICompiler with
        member __.Options = com.Options
        member __.Plugins = com.Plugins }
        
let transformFiles (com: ICompiler) (fileMask: string option)
                   (projOpts: FSharpProjectOptions) (proj: FSharpCheckProjectResults) =
    let isMasked (file: FSharpImplementationFileContents) =
        let arePathsEqual p1 p2 =
            let normalize = System.IO.Path.GetFullPath >> Naming.normalizePath 
            (normalize p1) = (normalize p2)
        match fileMask with
        | Some mask when not(arePathsEqual file.FileName mask) -> false
        | _ -> true
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
        | _ -> failwithf "Cannot find namespace %s" rootNs

    let curProj =
        Fable.Project(
            com.Options.projFile,
            makeFileMap proj.AssemblySignature.Entities)
    let projs =
        projOpts.ReferencedProjects
        |> Seq.map (fun (assemblyPath, opts) ->
            let projName = System.IO.Path.GetFileNameWithoutExtension opts.ProjectFileName
            proj.ProjectContext.GetReferencedAssemblies()
            |> Seq.tryFind (fun a -> a.FileName = Some assemblyPath)
            |> function
            | Some assembly when not(com.Options.refs.ContainsKey projName) ->
                failwithf "Cannot find import path for referenced project %s. %s"
                            projName "Have you forgotten --refs argument?"
            | Some assembly ->
                Fable.Project(opts.ProjectFileName,
                            makeFileMap assembly.Contents.Entities,
                            assemblyPath, com.Options.refs.[projName])
                |> Some
            | None -> None)
        |> Seq.choose id
        |> fun refs -> curProj::(List.ofSeq refs)

    let com = makeCompiler com projs
    proj.AssemblyContents.ImplementationFiles
    |> Seq.where (fun file ->
        curProj.FileMap.ContainsKey file.FileName
        && not (Naming.ignoredFilesRegex.IsMatch file.FileName)
        && isMasked file)
    |> Seq.map (fun file ->
        try
            let rootEnt, rootDecls =
                let rootNs = curProj.FileMap.[file.FileName]
                let rootEnt, rootDecls = getRootDecls rootNs None file.Declarations
                let rootDecls = transformDeclarations com Context.Empty [] rootDecls
                match rootEnt with
                | Some rootEnt -> makeEntity com rootEnt, rootDecls
                | None -> Fable.Entity.CreateRootModule file.FileName rootNs, rootDecls
            Fable.File(file.FileName, rootEnt, rootDecls)
        with
        | ex -> failwithf "%s (%s)" ex.Message file.FileName)
    |> fun seq -> projs, seq
