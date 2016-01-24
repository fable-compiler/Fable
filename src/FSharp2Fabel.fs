[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Fabel.FSharp2Fabel

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel.AST
open Fabel.FSharp2Fabel.Util

let rec private transformExpr (com: IFabelCompiler) ctx fsExpr =
    match fsExpr with
    (** ## Erased *)
    | BasicPatterns.Coerce(_targetType, Transform com ctx inpExpr) -> inpExpr
    | BasicPatterns.NewDelegate(_delegateType, Transform com ctx delegateBodyExpr) -> delegateBodyExpr
    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    | BasicPatterns.TypeLambda (_genArgs, Transform com ctx lambda) -> lambda

    | BasicPatterns.ILAsm (_asmCode, _typeArgs, argExprs) ->
        printfn "ILAsm detected in %A: %A" fsExpr.Range fsExpr // TODO: Check
        let typ, range = makeTypeRange com fsExpr
        match argExprs with
        | [] -> makeNull range
        | [Transform com ctx expr] -> expr
        | exprs -> Fabel.Sequential (List.map (transformExpr com ctx) exprs)
                    |> makeExpr range typ

    (** ## Flow control *)
    | BasicPatterns.FastIntegerForLoop(Transform com ctx start, Transform com ctx limit, body, isUp) ->
        match body with
        | BasicPatterns.Lambda (BindIdent com ctx (newContext, ident), body) ->
            Fabel.For (ident, start, limit, com.Transform newContext body, isUp)
            |> Fabel.Loop |> makeExprFrom com fsExpr
        | _ -> failwithf "Unexpected loop in %A: %A" fsExpr.Range fsExpr

    | BasicPatterns.WhileLoop(Transform com ctx guardExpr, Transform com ctx bodyExpr) ->
        Fabel.While (guardExpr, bodyExpr)
        |> Fabel.Loop |> makeExprFrom com fsExpr

    // This must appear before BasicPatterns.Let
    | ForOf (BindIdent com ctx (newContext, ident), Transform com ctx value, body) ->
        Fabel.ForOf (ident, value, transformExpr com newContext body)
        |> Fabel.Loop |> makeExprFrom com fsExpr

    (** Values *)
    | BasicPatterns.Const(value, _typ) ->
        makeConst value ||> makeExpr (makeRange fsExpr.Range)

    | BasicPatterns.BaseValue _type ->
        makeExprFrom com fsExpr (Fabel.Value Fabel.Super)

    | BasicPatterns.ThisValue _type ->
        makeExprFrom com fsExpr (Fabel.Value Fabel.This)

    | BasicPatterns.Value thisVar when thisVar.IsMemberThisValue ->
        makeExprFrom com fsExpr (Fabel.Value Fabel.This)

    | BasicPatterns.Value v ->
        if not v.IsModuleValueOrMember then
            let (GetIdent com ctx ident) = v
            upcast ident
        else
            let range = makeRange fsExpr.Range
            let typeRef = makeTypeFromDef com v.EnclosingEntity
                          |> makeTypeRef range
            Fabel.Get (typeRef, makeLiteral range v.DisplayName)
            |> makeExprFrom com fsExpr

    | BasicPatterns.DefaultValue (FabelType com typ) ->
        let valueKind =
            match typ with
            | Fabel.PrimitiveType Fabel.Boolean -> Fabel.BoolConst false
            | Fabel.PrimitiveType (Fabel.Number _) -> Fabel.IntConst 0
            | _ -> Fabel.Null
        makeExprFrom com fsExpr (Fabel.Value valueKind)

    (** ## Assignments *)
    // TODO: Possible optimization if binding to another ident (let x = y), just replace it in the ctx
    | BasicPatterns.Let((BindIdent com ctx (newContext, ident) as var,
                            Transform com ctx binding), body) ->
        let varRange = makeRange var.DeclarationLocation
        let body = transformExpr com newContext body
        let assignment = makeVarDecl varRange (ident, var.IsMutable) binding
        match body.Kind with
        // Check if this this is just a wrapper to a call as it happens in pipelines
        // e.g., let x = 5 in fun y -> methodCall x y
        | Fabel.Lambda (lambdaArgs,
                        (ExprKind (Fabel.Apply (eBody, ReplaceArgs [ident,binding] args, isCons)) as e),
                        Fabel.Immediate, false) ->
            Fabel.Lambda (lambdaArgs,
                Fabel.Expr (Fabel.Apply (eBody, args, isCons), e.Type, e.Range),
                Fabel.Immediate, false) |> makeExprFrom com fsExpr
        | _ -> makeSequential (makeRange fsExpr.Range) [assignment; body]

    | BasicPatterns.LetRec(recursiveBindings, body) ->
        let newContext, idents =
            recursiveBindings
            |> List.foldBack (fun (var, _) (accContext, accIdents) ->
                let (BindIdent com accContext (newContext, ident)) = var
                newContext, (ident::accIdents)) <| (ctx, [])
        let assignments =
            recursiveBindings
            |> List.map2 (fun ident (var, Transform com ctx binding) ->
                let varRange = makeRange var.DeclarationLocation
                makeVarDecl varRange (ident, var.IsMutable) binding) idents
        assignments @ [transformExpr com newContext body] 
        |> makeSequential (makeRange fsExpr.Range)

    (** ## Applications *)
    | BasicPatterns.TraitCall (_sourceTypes, traitName, _typeArgs, _typeInstantiation, argExprs) ->
        printfn "TraitCall detected in %A: %A" fsExpr.Range fsExpr // TODO: Check
        let typ, range = makeTypeRange com fsExpr
        makeGetApply range typ
            (transformExpr com ctx argExprs.Head)
            (List.map (transformExpr com ctx) argExprs.Tail)
            traitName

    // TODO: Check `inline` annotation?
    // TODO: Watch for restParam attribute
    | BasicPatterns.Call(callee, meth, _typeArgs1, _typeArgs2, args) ->
        makeCall com ctx fsExpr callee meth args

    | BasicPatterns.Application(Transform com ctx expr, _typeArgs, args) ->
        let typ, range = makeTypeRange com fsExpr
        makeApply ctx range typ expr (List.map (transformExpr com ctx) args)

    | BasicPatterns.IfThenElse (Transform com ctx guardExpr, Transform com ctx thenExpr, Transform com ctx elseExpr) ->
        Fabel.IfThenElse (guardExpr, thenExpr, elseExpr)
        |> makeExprFrom com fsExpr

    | BasicPatterns.TryFinally (BasicPatterns.TryWith(body, _, _, catchVar, catchBody),finalBody) ->
        makeTryCatch com ctx fsExpr body (Some (catchVar, catchBody)) (Some finalBody)

    | BasicPatterns.TryFinally (body, finalBody) ->
        makeTryCatch com ctx fsExpr body None (Some finalBody)

    | BasicPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        makeTryCatch com ctx fsExpr body (Some (catchVar, catchBody)) None

    | BasicPatterns.Sequential (Transform com ctx first, Transform com ctx second) ->
        makeSequential (makeRange fsExpr.Range) [first; second]

    (** ## Lambdas *)
    | BasicPatterns.Lambda (var, body) ->
        makeLambda com ctx (makeRange fsExpr.Range) [var] body

    (** ## Getters and Setters *)
    | BasicPatterns.ILFieldGet (callee, typ, fieldName) ->
        failwithf "Found unsupported ILField reference in %A: %A" fsExpr.Range fsExpr

    // TODO: Check if it's FSharpException
    // TODO: Change name of automatically generated fields
    | BasicPatterns.FSharpFieldGet (callee, FabelType com calleeType, FieldName fieldName) ->
        let range = makeRange fsExpr.Range
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeTypeRef range calleeType
        Fabel.Get (callee, makeLiteral range fieldName)
        |> makeExprFrom com fsExpr

    | BasicPatterns.TupleGet (_tupleType, tupleElemIndex, Transform com ctx tupleExpr) ->
        Fabel.Get (tupleExpr, makeLiteral (makeRange fsExpr.Range) tupleElemIndex)
        |> makeExprFrom com fsExpr

    // Single field: Item; Multiple fields: Item1, Item2...
    | BasicPatterns.UnionCaseGet (Transform com ctx unionExpr, FabelType com unionType, unionCase, FieldName fieldName) ->
        match unionType with
        | ErasedUnion | OptionUnion -> unionExpr
        | ListUnion -> failwith "TODO"
        | OtherType ->
            Fabel.Get (unionExpr, makeLiteral (makeRange fsExpr.Range) fieldName)
            |> makeExprFrom com fsExpr

    | BasicPatterns.ILFieldSet (callee, typ, fieldName, value) ->
        failwithf "Found unsupported ILField reference in %A: %A" fsExpr.Range fsExpr

    // TODO: Change name of automatically generated fields
    | BasicPatterns.FSharpFieldSet (callee, FabelType com calleeType, FieldName fieldName, Transform com ctx value) ->
        let range = makeRange fsExpr.Range
        let callee =
            match callee with
            | Some (Transform com ctx callee) -> callee
            | None -> makeTypeRef range calleeType
        Fabel.Set (callee, Some (makeLiteral range fieldName), value)
        |> makeExprFrom com fsExpr

    | BasicPatterns.UnionCaseTag (Transform com ctx unionExpr, _unionType) ->
        Fabel.Get (unionExpr, makeLiteral (makeRange fsExpr.Range) "Tag")
        |> makeExprFrom com fsExpr

    // We don't need to check if this an erased union, as union case values are only set
    // in constructors, which are ignored for erased unions
    | BasicPatterns.UnionCaseSet (Transform com ctx unionExpr, _type, _case, FieldName caseField, Transform com ctx valueExpr) ->
        Fabel.Set (unionExpr, Some (makeLiteral (makeRange fsExpr.Range) caseField), valueExpr)
        |> makeExprFrom com fsExpr

    | BasicPatterns.ValueSet (GetIdent com ctx valToSet, Transform com ctx valueExpr) ->
        Fabel.Set (valToSet, None, valueExpr)
        |> makeExprFrom com fsExpr

    (** Instantiation *)
    | BasicPatterns.NewArray(FabelType com typ, argExprs) ->
        match typ with
        | Fabel.PrimitiveType (Fabel.TypedArray numberKind) -> failwith "TODO: NewArray args"
        | _ -> Fabel.Value (Fabel.ArrayConst (argExprs |> List.map (transformExpr com ctx)))
        |> makeExprFrom com fsExpr

    | BasicPatterns.NewTuple(_, argExprs) ->
        Fabel.Value (Fabel.ArrayConst (argExprs |> List.map (transformExpr com ctx)))
        |> makeExprFrom com fsExpr

    | BasicPatterns.ObjectExpr(_objType, _baseCallExpr, _overrides, interfaceImplementations) ->
        failwith "TODO"

    // TODO: Check for erased constructors with property assignment (Call + Sequential)
    | BasicPatterns.NewObject(meth, _typeArgs, args) ->
        makeCall com ctx fsExpr None meth args

    // TODO: Check if it's FSharpException
    // TODO: Create constructors for Record and Union types
    // TODO: Detect Record copies, when one of the arguments is:
    // FSharpFieldGet (Some Value val originalRecord,type _,field _)])
    | BasicPatterns.NewRecord(FabelType com recordType, argExprs) ->
        let range = makeRange fsExpr.Range 
        let argExprs = argExprs |> List.map (transformExpr com ctx)
        Fabel.Apply (makeTypeRef range recordType, argExprs, true)
        |> makeExprFrom com fsExpr

    | BasicPatterns.NewUnionCase(FabelType com unionType, unionCase, argExprs) ->
        let range = makeRange fsExpr.Range 
        let argExprs = argExprs |> List.map (transformExpr com ctx)
        match unionType with
        | ErasedUnion | OptionUnion ->
            match argExprs with
            | [] -> Fabel.Value Fabel.Null |> makeExprFrom com fsExpr
            | [expr] -> expr
            | _ -> failwithf "Erased Union Cases must have one single field: %A" unionType
        | ListUnion ->
            match unionCase.Name with
            | "Cons" ->
                makeCoreCall range (Fabel.CoreType ("List", None)) argExprs ("List", None, true)
            | _ -> makeNull range
        | OtherType ->
            // Include Tag name in args
            let argExprs = (makeLiteral range unionCase.Name)::argExprs
            Fabel.Apply (makeTypeRef range unionType, argExprs, true)
            |> makeExprFrom com fsExpr

    (** ## Type test *)
    | BasicPatterns.TypeTest (FabelType com typ as fsTyp, Transform com ctx expr) ->
        makeTypeTest (makeRange fsExpr.Range) typ expr

    | BasicPatterns.UnionCaseTest (Transform com ctx unionExpr, FabelType com unionType, unionCase) ->
        let range = makeRange fsExpr.Range 
        let stringType, boolType =
            Fabel.PrimitiveType (Fabel.String false), Fabel.PrimitiveType Fabel.Boolean
        match unionType with
        | ErasedUnion ->
            if unionCase.UnionCaseFields.Count <> 1 then
                failwithf "Erased Union Cases must have one single field: %A" unionType
            else
                let typ = makeType com unionCase.UnionCaseFields.[0].FieldType
                makeTypeTest range typ unionExpr
        | OptionUnion | ListUnion ->
            if (unionCase.Name = "None" || unionCase.Name = "Empty")
            then BinaryEqual
            else BinaryUnequal
            |> makeBinOp range boolType [unionExpr; (makeNull range)]
        | OtherType ->
            let left = Fabel.Get (unionExpr, makeLiteral range "Tag")
                       |> makeExpr range stringType
            let right = makeLiteral range unionCase.Name
            makeBinOp range boolType [left; right] BinaryEqualStrict

    (** Pattern Matching *)
    | BasicPatterns.DecisionTreeSuccess (decIndex, decBindings) ->
        match Map.tryFind decIndex ctx.decisionTargets with
        | None -> failwith "Missing decision target"
        // If we get a reference to a function, call it
        | Some (TargetRef targetRef) ->
            Fabel.Apply (targetRef, (decBindings |> List.map (transformExpr com ctx)), false)
            |> makeExprFrom com fsExpr
        // If we get an implementation without bindings, just transform it
        | Some (TargetImpl ([], Transform com ctx decBody)) -> decBody
        // If we have bindings, create the assignments
        | Some (TargetImpl (decVars, decBody)) ->
            let newContext, assignments =
                List.foldBack2 (fun var (Transform com ctx binding) (accContext, accAssignments) ->
                    let (BindIdent com accContext (newContext, ident)) = var
                    let varRange = makeRange var.DeclarationLocation
                    let assignment = makeVarDecl varRange (ident, var.IsMutable) binding
                    newContext, (assignment::accAssignments)) decVars decBindings (ctx, [])
            assignments @ [transformExpr com newContext decBody]
            |> makeSequential (makeRange fsExpr.Range)

    | BasicPatterns.DecisionTree(decisionExpr, decisionTargets) ->
        let rec getTargetRefsCount map = function
            | BasicPatterns.IfThenElse (_, thenExpr, elseExpr) ->
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
                let decTargetVars, decTargetExpr = decisionTargets.[k]
                let range = makeRange decTargetExpr.Range
                let lambda = makeLambda com ctx range decTargetVars decTargetExpr
                let ctx, ident = makeSanitizedIdent ctx range lambda.Type (sprintf "target%i" k)
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
                    makeVarDecl ident.Range (ident, false) lambda)
                |> Seq.toList
            Fabel.Sequential (assignments @ [transformExpr com ctx decisionExpr])
            |> makeExprFrom com fsExpr

    (** Not implemented *)
    | BasicPatterns.Quote _ // (quotedExpr)
    | BasicPatterns.AddressOf _ // (lvalueExpr)
    | BasicPatterns.AddressSet _ // (lvalueExpr, rvalueExpr)
    | _ -> failwithf "Cannot compile expression in %A: %A" fsExpr.Range fsExpr

type private DeclInfo() =
    let mutable child: Fabel.Entity option = None
    let decls = ResizeArray<Fabel.Declaration>()
    let childDecls = ResizeArray<Fabel.Declaration>()
    let extMods = ResizeArray<Fabel.ExternalEntity>()
    // The F# compiler considers class methods as children of the enclosing module, correct that
    member self.AddMethod (meth: FSharpMemberOrFunctionOrValue, methDecl: Fabel.Declaration) =
        let methParentFullName =
            sanitizeEntityName meth.EnclosingEntity.FullName
        match child with
        | Some x when x.FullName = methParentFullName ->
            childDecls.Add methDecl
        | _ ->
            self.ClearChild ()
            decls.Add methDecl
    member self.AddInitAction (actionDecl: Fabel.Declaration) =
        self.ClearChild ()
        decls.Add actionDecl
    member self.AddExternalModule extMod =
        extMods.Add extMod
    member self.ClearChild () =
        if child.IsSome then
            Fabel.EntityDeclaration (child.Value, List.ofSeq childDecls)
            |> decls.Add
        child <- None
        childDecls.Clear ()
    member self.AddChild (newChild, newChildDecls, childExtMods) =
        self.ClearChild ()
        child <- Some newChild
        childDecls.AddRange newChildDecls
        extMods.AddRange childExtMods
    member self.GetDeclarationsAndExternalModules () =
        self.ClearChild ()
        List.ofSeq decls, List.ofSeq extMods        
    
let private transformMemberDecl
    (com: IFabelCompiler) ctx (declInfo: DeclInfo) (meth: FSharpMemberOrFunctionOrValue)
    (args: FSharpMemberOrFunctionOrValue list list) (body: FSharpExpr) =
    let memberKind =
        let name = meth.DisplayName
        // TODO: Another way to check module values?
        // TODO: Mutable module values
        if meth.EnclosingEntity.IsFSharpModule then
            match meth.XmlDocSig.[0] with
            | 'P' -> Fabel.Getter name
            | _ -> Fabel.Method name
        else
            // TODO: Check overloads
            if meth.IsImplicitConstructor then Fabel.Constructor
            elif meth.IsPropertyGetterMethod then Fabel.Getter name
            elif meth.IsPropertySetterMethod then Fabel.Setter name
            else Fabel.Method name
    let ctx, args =
        let args = if meth.IsInstanceMember then Seq.skip 1 args |> Seq.toList else args
        match args with
        | [] -> ctx, []
        | [[singleArg]] ->
            makeType com singleArg.FullType |> function
            | Fabel.PrimitiveType Fabel.Unit -> ctx, []
            | _ -> let (BindIdent com ctx (ctx, arg)) = singleArg in ctx, [arg]
        | _ ->
            List.foldBack (fun tupledArg (accContext, accArgs) ->
                match tupledArg with
                | [] -> failwith "Unexpected empty tupled in curried arguments"
                | [nonTupledArg] ->
                    let (BindIdent com accContext (newContext, arg)) = nonTupledArg
                    newContext, arg::accArgs
                | _ ->
                    // The F# compiler "untuples" the args in methods
                    let newContext, untupledArg = makeLambdaArgs com ctx tupledArg
                    newContext, untupledArg@accArgs
            ) args (ctx, []) // TODO: Reset Context?
    let entMember =
        let func =
            let fnRange, fnBody = makeRange body.Range, transformExpr com ctx body
            Fabel.LambdaExpr (args, fnBody, Fabel.Immediate, hasRestParams meth, makeFnType args, fnRange)
        Fabel.Member (memberKind, makeRange meth.DeclarationLocation, func,
            meth.Attributes |> Seq.choose (makeDecorator com) |> Seq.toList,
            meth.Accessibility.IsPublic, not meth.IsInstanceMember)
        |> Fabel.MemberDeclaration
    declInfo.AddMethod (meth, entMember)
    declInfo
   
let rec private transformEntityDecl
    (com: IFabelCompiler) ctx (declInfo: DeclInfo) ent subDecls =
    match ent with
    | WithAttribute "Global" _ ->
        Fabel.GlobalModule ent.FullName
        |> declInfo.AddExternalModule
        declInfo
    | WithAttribute "Import" args ->
        match args with
        | [:? string as modName] ->
            Fabel.ImportModule(ent.FullName, modName)
            |> declInfo.AddExternalModule
            declInfo
        | _ -> failwith "Import attributes must have a single string argument"
    | WithAttribute "Erase" _ | AbstractEntity _ ->
        declInfo // Ignore 
    | _ ->
        let childDecls, childExtMods = transformDeclarations com ctx subDecls
        declInfo.AddChild (com.GetEntity ent, childDecls, childExtMods)
        declInfo

and private transformDeclarations (com: IFabelCompiler) ctx decls =
    let declInfo =
        decls |> List.fold (fun (declInfo: DeclInfo) decl ->
            match decl with
            | FSharpImplementationFileDeclaration.Entity (e, sub) ->
                transformEntityDecl com ctx declInfo e sub
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
                transformMemberDecl com ctx declInfo meth args body
            | FSharpImplementationFileDeclaration.InitAction (Transform com ctx expr) ->
                declInfo.AddInitAction (Fabel.ActionDeclaration expr); declInfo
        ) (DeclInfo())
    declInfo.GetDeclarationsAndExternalModules ()
        
let transformFiles (com: ICompiler) (fsProj: FSharpCheckProjectResults) =
    let emptyContext parent = { scope = []; decisionTargets = Map.empty<_,_> }
    let rec getRootDecls rootEnt = function
        | [FSharpImplementationFileDeclaration.Entity (e, subDecls)]
            when e.IsNamespace || e.IsFSharpModule ->
            getRootDecls (Some e) subDecls
        | _ as decls -> rootEnt, decls
    let entities =
        System.Collections.Concurrent.ConcurrentDictionary<string, Fabel.Entity>()
    let fileNames =
        fsProj.AssemblyContents.ImplementationFiles
        |> Seq.map (fun x -> x.FileName) |> Set.ofSeq
    let com =
        { new IFabelCompiler with
            member fcom.Transform ctx fsExpr =
                transformExpr fcom ctx fsExpr
            member fcom.GetInternalFile tdef =
                let file = tdef.DeclarationLocation.FileName
                if Set.contains file fileNames then Some file else None
            member fcom.GetEntity tdef =
                entities.GetOrAdd (tdef.FullName, fun _ -> makeEntity fcom tdef)
        interface ICompiler with
            member __.Options = com.Options }    
    fsProj.AssemblyContents.ImplementationFiles
    |> List.map (fun file ->
        let rootEnt, rootDecls = getRootDecls None file.Declarations
        let rootDecls, extDecls = transformDeclarations com (emptyContext rootEnt) rootDecls
        match rootDecls with
        | [] -> Fabel.File(file.FileName, None, extDecls)
        | _ ->
            match rootEnt with
            | Some rootEnt -> makeEntity com rootEnt
            | None -> Fabel.Entity.CreateRootModule file.FileName
            |> fun rootEnt -> Some(Fabel.EntityDeclaration(rootEnt, rootDecls))
            |> fun rootDecl -> Fabel.File(file.FileName, rootDecl, extDecls))
            // TODO: Add main call for files with explicit entry point
