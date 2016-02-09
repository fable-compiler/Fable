module Fabel.FSharp2Fabel.Util

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel
open Fabel.AST
open Fabel.Plugins
open Fabel.AST.Fabel.Util

type DecisionTarget =
    | TargetRef of Fabel.Ident
    | TargetImpl of FSharpMemberOrFunctionOrValue list * FSharpExpr

type Context =
    {
    scope: (string * Fabel.Expr) list
    decisionTargets: Map<int, DecisionTarget>
    parentEntities: FSharpEntity list
    }
    member ctx.Reset() =
        { ctx with scope=[]; decisionTargets=Map.empty<_,_> }
    
type IFabelCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fabel.Expr
    abstract GetInternalFile: FSharpEntity -> string option
    abstract GetEntity: FSharpEntity -> Fabel.Entity
    
[<AutoOpen>]
module Patterns =
    open BasicPatterns

    let (|Rev|) = List.rev
    let (|Transform|) (com: IFabelCompiler) = com.Transform
    let (|FieldName|) (fi: FSharpField) = fi.Name
    let (|ExprType|) (expr: Fabel.Expr) = expr.Type
    
    let (|NonAbbreviatedType|) (t: FSharpType) =
        let rec abbr (t: FSharpType) =
            if t.IsAbbreviation then abbr t.AbbreviatedType else t
        abbr t
    
    // Special values like seq, async, String.Empty...
    let (|SpecialValue|_|) com = function
        | Value v ->
            match v.FullName with
            | "Microsoft.FSharp.Core.Operators.seq" ->
                makeCoreRef com "Seq" |> Some
            | "Microsoft.FSharp.Core.ExtraTopLevelOperators.async" ->
                makeCoreRef com "Async" |> Some
            | _ -> None
        | ILFieldGet (None, typ, fieldName) when typ.HasTypeDefinition ->
            match typ.TypeDefinition.FullName, fieldName with
            | "System.String", "Empty" -> Some (makeConst "")
            | _ -> None
        | _ -> None
    
    let (|ForOf|_|) = function
        | Let((_, value),
              Let((_, Call(_, meth, _, [], [])),
                TryFinally(
                  WhileLoop(_,
                    Let((ident, _), body)), _)))
            when meth.DisplayName = "GetEnumerator" ->
            Some(ident, value, body)
        | _ -> None

    // These are closures created by F# compiler, e.g. given `let add x y z = x+y+z`
    // `3 |> add 1 2` will become `let x=1 in let y=2 in fun z -> add(x,y,z)`
    let (|Closure|_|) fsExpr =
        let checkArgs (identAndRepls: (FSharpMemberOrFunctionOrValue*FSharpExpr) list) args =
            if identAndRepls.Length <> (List.length args) then false else
            (args, identAndRepls)
            ||> List.forall2 (fun arg (ident, _) ->
                if ident.IsMutable then false else 
                match arg with
                | Coerce(_, Value arg) | Value arg ->
                    ident.IsEffectivelySameAs arg
                | _ -> false)
        let checkArgs2 lambdaArgs methArgs =
            (lambdaArgs, methArgs)
            ||> List.forall2 (fun larg marg ->
                match marg with
                | Coerce(_, Value marg) | Value marg -> marg.IsEffectivelySameAs larg
                | _ -> false)                
        let rec visit identAndRepls = function
            | Let((letArg, letValue), letBody) ->
                let identAndRepls = identAndRepls@[(letArg, letValue)]
                match letBody with
                | Lambda(lambdaArg1, Call(None, meth, typArgs, methTypArgs, Rev (last1::args))) ->
                    if checkArgs identAndRepls (List.rev args) &&
                        checkArgs2 [lambdaArg1] [last1]
                    then Some(1, meth, typArgs, methTypArgs, List.map snd identAndRepls)
                    else None
                | Lambda(lambdaArg1,
                         Lambda(lambdaArg2, Call(None, meth, typArgs, methTypArgs, Rev (last2::last1::args)))) ->
                    if checkArgs identAndRepls (List.rev args) &&
                        checkArgs2 [lambdaArg1;lambdaArg2] [last1;last2]
                    then Some(2, meth, typArgs, methTypArgs, List.map snd identAndRepls)
                    else None
                | Lambda(lambdaArg1,
                         Lambda(lambdaArg2,
                            Lambda(lambdaArg3,Call(None, meth, typArgs, methTypArgs, Rev (last3::last2::last1::args))))) ->
                    if checkArgs identAndRepls (List.rev args) &&
                        checkArgs2 [lambdaArg1;lambdaArg2;lambdaArg3] [last1;last2;last3]
                    then Some(3, meth, typArgs, methTypArgs, List.map snd identAndRepls)
                    else None
                | _ -> visit identAndRepls letBody
            | _ -> None
        match fsExpr with
        | Lambda(larg1, Call(None, meth, typArgs, methTypArgs, [marg1]))
            when checkArgs2 [larg1] [marg1] ->
                Some(1, meth, typArgs, methTypArgs, [])
        | Lambda(larg1, Lambda(larg2, Call(None, meth, typArgs, methTypArgs, [marg1;marg2])))
            when checkArgs2 [larg1;larg2] [marg1;marg2] ->
                Some(2, meth, typArgs, methTypArgs, [])
        | Lambda(larg1, Lambda(larg2, Lambda(larg3, Call(None, meth, typArgs, methTypArgs, [marg1;marg2;marg3]))))
            when checkArgs2 [larg1;larg2;larg3] [marg1;marg2;marg3] ->
                Some(3, meth, typArgs, methTypArgs, [])
        | _ -> visit [] fsExpr

    let (|Pipe|_|) = function
        | Call(None, meth, _, _, [arg1; arg2]) ->
            match meth.FullName with
            | "Microsoft.FSharp.Core.Operators.( |> )" ->
                Some (arg2, [arg1])
            | "Microsoft.FSharp.Core.Operators.( <| )" ->
                Some (arg1, [arg2])
            | _ -> None
        | Call(None, meth, _, _, [arg1; arg2; arg3]) ->
            match meth.FullName with
            | "Microsoft.FSharp.Core.Operators.( ||> )" ->
                Some (arg3, [arg1; arg2])
            | "Microsoft.FSharp.Core.Operators.( <|| )" ->
                Some (arg1, [arg2; arg3])
            | _ -> None
        | Call(None, meth, _, _, [arg1; arg2; arg3; arg4]) ->
            match meth.FullName with
            | "Microsoft.FSharp.Core.Operators.( |||> )" ->
                Some (arg4, [arg1; arg2; arg3])
            | "Microsoft.FSharp.Core.Operators.( <||| )" ->
                Some (arg1, [arg2; arg3; arg4])
            | _ -> None
        | _ -> None
        
    // TODO: Make it recursive 
    let (|Composition|_|) = function
        | Call(None, comp, _, _,
                [Closure(1, meth1, typArgs1, methTypArgs1, args1);
                 Closure(1, meth2, typArgs2, methTypArgs2, args2)]) ->
            match comp.FullName with
            | "Microsoft.FSharp.Core.Operators.( >> )" ->
                Some (meth1, typArgs1, methTypArgs1, args1, meth2, typArgs2, methTypArgs2, args2)
            | "Microsoft.FSharp.Core.Operators.( << )" ->
                Some (meth2, typArgs2, methTypArgs2, args2, meth1, typArgs1, methTypArgs1, args1)
            | _ -> None
        | _ -> None

    let (|ErasableLambda|_|) fsExpr =
        match fsExpr with
        | Pipe(Closure (arity, meth, typArgs, methTypArgs, methArgs), exprs)
            when arity = exprs.Length ->
                Some (meth, typArgs, methTypArgs, methArgs@exprs)
        | Pipe(Closure (arity, meth, typArgs, methTypArgs, methArgs), exprs)
            when arity = exprs.Length ->
                Some (meth, typArgs, methTypArgs, methArgs@exprs)
        | Pipe(Closure (arity, meth, typArgs, methTypArgs, methArgs), exprs)
            when arity = exprs.Length ->
                Some (meth, typArgs, methTypArgs, methArgs@exprs)
        | _ -> None

    let (|NumberKind|_|) = function
        | "System.SByte" -> Some Int8
        | "System.Byte" -> Some UInt8
        | "System.Int16" -> Some Int16
        | "System.UInt16" -> Some UInt16
        | "System.Int32" -> Some Int32
        | "System.UInt32" -> Some UInt32
        | "System.Int64" -> Some Float64
        | "System.UInt64" -> Some Float64
        | "System.Single" -> Some Float32
        | "System.Double"
        | "Microsoft.FSharp.Core.float`1" -> Some Float64
        | "Microsoft.FSharp.Core.float32`1" -> Some Float32
        | _ -> None
        
    let (|Location|_|) (com: IFabelCompiler) (ent: FSharpEntity) =
        match com.GetInternalFile ent with
        | Some file -> Some { Fabel.file=file; Fabel.fullName=ent.FullName }
        | None -> None

    let tryFindAtt f (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some fullName ->
                fullName.Substring(fullName.LastIndexOf "." + 1).Replace("Attribute", "")
                |> f |> function true -> Some att | false -> None
            | None -> None)
        
    let (|ContainsAtt|_|) (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> tryFindAtt ((=) name) |> Option.map (fun att ->
            att.ConstructorArguments |> Seq.map snd |> Seq.toList) 

    let (|Namespace|_|) (ns: string) (decl: FSharpImplementationFileDeclaration) =
        let rec matchNamespace (nsParts: string list) decl =
            match decl with
            | FSharpImplementationFileDeclaration.Entity (e, sub)
                when e.IsNamespace && e.DisplayName = nsParts.Head ->
                match nsParts, sub with
                | [x], _ -> Some sub
                | x::xs, [sub] -> matchNamespace xs sub
                | _ -> None
            | _ -> None
        if ns.Length = 0
        then Some [decl]
        else matchNamespace (ns.Split('.') |> Array.toList) decl

    let (|OptionUnion|ListUnion|ErasedUnion|OtherType|) (typ: Fabel.Type) =
        match typ with
        | Fabel.DeclaredType ent ->
            match ent.FullName with
            | "Microsoft.FSharp.Core.Option" -> OptionUnion
            | "Microsoft.FSharp.Collections.List" -> ListUnion
            | _ when Option.isSome (ent.TryGetDecorator "Erase") -> ErasedUnion
            | _ -> OtherType
        | _ -> failwithf "Unexpected union type: %A" typ

[<AutoOpen>]
module Types =
    let sanitizeEntityName (ent: FSharpEntity) =
        let ns = match ent.Namespace with
                 | Some ns -> ns + "." | None -> ""
        ns + ent.DisplayName
        
    let getBaseClassLocation (tdef: FSharpEntity) =
        match tdef.BaseType with
        | None -> None
        | Some (NonAbbreviatedType t) ->
            (not t.HasTypeDefinition ||
                t.TypeDefinition.FullName = "System.Object")
            |> function
            | true -> None
            | false -> Some {
                Fabel.file = t.TypeDefinition.DeclarationLocation.FileName
                Fabel.fullName = t.TypeDefinition.FullName
            }

    // Some attributes (like ComDefaultInterface) will throw an exception
    // when trying to access ConstructorArguments
    let makeDecorator (com: IFabelCompiler) (att: FSharpAttribute) =
        try
            let args = att.ConstructorArguments |> Seq.map snd |> Seq.toList
            let fullName =
                let fullName = sanitizeEntityName att.AttributeType
                if fullName.EndsWith ("Attribute")
                then fullName.Substring (0, fullName.Length - 9)
                else fullName
            Fabel.Decorator(fullName, args) |> Some
        with _ ->
            None

    let makeEntity (com: IFabelCompiler) (tdef: FSharpEntity) =
        let kind =
            if tdef.IsInterface then Fabel.Interface
            // elif tdef.IsFSharpExceptionDeclaration then Fabel.Exception
            elif tdef.IsFSharpRecord then Fabel.Record
            elif tdef.IsFSharpUnion then Fabel.Union
            elif tdef.IsFSharpModule || tdef.IsNamespace then Fabel.Module
            else Fabel.Class (getBaseClassLocation tdef)
        // Take only interfaces with internal declaration
        let infcs =
            tdef.DeclaredInterfaces
            |> Seq.filter (fun (NonAbbreviatedType x) ->
                x.HasTypeDefinition &&
                com.GetInternalFile x.TypeDefinition |> Option.isSome)
            |> Seq.map (fun x -> sanitizeEntityName x.TypeDefinition)
            |> Seq.toList
        let decs =
            tdef.Attributes
            |> Seq.choose (makeDecorator com)
            |> Seq.toList
        Fabel.Entity (kind, com.GetInternalFile tdef,
            sanitizeEntityName tdef, infcs, decs, tdef.Accessibility.IsPublic)

    let rec makeTypeFromDef (com: IFabelCompiler) (tdef: FSharpEntity) =
        // Guard: F# abbreviations shouldn't be passed as argument
        if tdef.IsFSharpAbbreviation
        then failwith "Abbreviation passed to makeTypeFromDef"
        // Array
        elif tdef.IsArrayType then
            match tdef.GenericParameters.[0].FullName with
            | NumberKind kind -> Fabel.TypedArray kind
            | _ -> Fabel.DynamicArray
            |> Fabel.Array |> Fabel.PrimitiveType
        // Object
        elif tdef.FullName = "System.Object"
        then Fabel.UnknownType
        else
        // .NET Primitives
        match tdef.FullName with
        | NumberKind kind -> Fabel.Number kind |> Fabel.PrimitiveType
        | "System.Boolean" -> Fabel.Boolean |> Fabel.PrimitiveType
        | "System.Char" | "System.String" -> Fabel.String |> Fabel.PrimitiveType
        | "System.Text.RegularExpressions.Regex" -> Fabel.Regex |> Fabel.PrimitiveType
        | "Microsoft.FSharp.Core.Unit" -> Fabel.Unit |> Fabel.PrimitiveType
        | "System.Collections.Generic.List`1" -> Fabel.DynamicArray |> Fabel.Array |> Fabel.PrimitiveType
        // Declared Type
        | _ -> com.GetEntity tdef |> Fabel.DeclaredType

    and makeType (com: IFabelCompiler) (NonAbbreviatedType t) =
        let rec countFuncArgs (fn: FSharpType) =
            if fn.IsFunctionType
            then countFuncArgs (Seq.last fn.GenericArguments) + 1
            else 0
        if t.IsGenericParameter then Fabel.UnknownType else
        if t.IsTupleType then Fabel.Tuple |> Fabel.Array |> Some
        elif t.IsFunctionType then Fabel.Function (countFuncArgs t) |> Some
        else None
        |> function
            | Some fabelType -> fabelType |> Fabel.PrimitiveType
            | None ->
                if not t.HasTypeDefinition
                then failwithf "Unexpected non-declared F# type: %A" t
                else makeTypeFromDef com t.TypeDefinition

    let (|FabelType|) = makeType

[<AutoOpen>]
module Identifiers =
    /// Make a sanitized identifier from a tentative name
    let bindIdent (ctx: Context) typ (tentativeName: string) =
        let sanitizedName = tentativeName |> Naming.sanitizeIdent (fun x ->
            List.exists (fun (_,x') ->
                match x' with
                | Fabel.Value (Fabel.IdentValue {name=name}) -> x = name
                | _ -> false) ctx.scope)
        let ident: Fabel.Ident = { name=sanitizedName; typ=typ}
        let identValue = Fabel.Value (Fabel.IdentValue ident)
        { ctx with scope = (tentativeName, identValue)::ctx.scope}, ident

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fabel.Ident =
        bindIdent ctx (makeType com fsRef.FullType) fsRef.DisplayName
    
    let (|BindIdent|) = bindIdentFrom

    let bindExpr ctx fsName expr =
        { ctx with scope = (fsName, expr)::ctx.scope}

    /// Get corresponding identifier to F# value in current scope
    let getBoundExpr com (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fun (fsName,_) -> fsName = fsRef.DisplayName)
        |> function
        | Some (_,boundExpr) -> boundExpr
        | None -> failwithf "Detected non-bound identifier: %s in %A"
                    fsRef.DisplayName fsRef.DeclarationLocation

// module Fabel.FSharp2Fabel.Util

let isReplaceCandidate (com: IFabelCompiler) (meth: FSharpMemberOrFunctionOrValue) =
    // Is external method or contains Replace attribute?
    match com.GetInternalFile meth.EnclosingEntity, meth.Attributes with
    | None, _ | _, ContainsAtt "Replace" _-> true
    | _ -> false

let sanitizeMethodName com (meth: FSharpMemberOrFunctionOrValue) =
    let isOverloadable (meth: FSharpMemberOrFunctionOrValue) =
        meth.EnclosingEntity.IsInterface
        || meth.IsProperty
        || meth.IsEvent
        || meth.IsImplicitConstructor
        |> not
    let overloadSuffix (meth: FSharpMemberOrFunctionOrValue) =
        (isOverloadable meth && not <| isReplaceCandidate com meth)
        |> function
        | false -> ""
        | true ->
            meth.EnclosingEntity.MembersFunctionsAndValues
            |> Seq.filter (fun x -> isOverloadable x && x.DisplayName = meth.DisplayName)
            |> Seq.toArray
            |> function
            | overloads when overloads.Length = 1 -> ""
            | overloads ->
                overloads
                |> Seq.mapi (fun i x -> i,x)
                |> Seq.tryPick (fun (i,x) ->
                    if x.XmlDocSig = meth.XmlDocSig then Some i else None)
                |> function
                | Some i when i > 0 -> sprintf "_%i" i
                | _ -> ""
    (Naming.removeBrackets meth.DisplayName) + (overloadSuffix meth)

let makeRange (r: Range.range) = {
    // source = Some r.FileName
    start = { line = r.StartLine; column = r.StartColumn }
    ``end``= { line = r.EndLine; column = r.EndColumn }
}

let makeRangeFrom (fsExpr: FSharpExpr) = 
    Some (makeRange fsExpr.Range)

let makeLambdaArgs com ctx (vars: FSharpMemberOrFunctionOrValue list) =
    let isUnitVar =
        match vars with
        | [var] ->
            match makeType com var.FullType with
            | Fabel.PrimitiveType Fabel.Unit -> true
            | _ -> false
        | _ -> false
    if isUnitVar
    then ctx, []
    else List.foldBack (fun var (ctx, accArgs) ->
        let newContext, arg = bindIdentFrom com ctx var
        newContext, arg::accArgs) vars (ctx, [])

let makeTryCatch com ctx (fsExpr: FSharpExpr) (Transform com ctx body) catchClause finalBody =
    let catchClause =
        match catchClause with
        | Some (BindIdent com ctx (catchContext, catchVar), Transform com ctx catchBody) ->
            Some (catchVar, catchBody)
        | None -> None
    let finalizer =
        match finalBody with
        | Some (Transform com ctx finalBody) -> Some finalBody
        | None -> None
    Fabel.TryCatch (body, catchClause, finalizer, makeRangeFrom fsExpr)

let makeGetFrom com (fsExpr: FSharpExpr) callee propExpr =
    Fabel.Apply (callee, [propExpr], Fabel.ApplyGet, makeType com fsExpr.Type, makeRangeFrom fsExpr)

let hasRestParams (meth: FSharpMemberOrFunctionOrValue) =
    if meth.CurriedParameterGroups.Count <> 1 then false else
    let args = meth.CurriedParameterGroups.[0]
    args.Count > 0 && args.[args.Count - 1].IsParamArrayArg
    
let (|Replaced|_|) (com: IFabelCompiler) fsExpr (typArgs, methTypArgs) (callee, args)
                 (meth: FSharpMemberOrFunctionOrValue) =
    if not (isReplaceCandidate com meth) then None else
    let applyInfo: Fabel.ApplyInfo = {
        ownerFullName = sanitizeEntityName meth.EnclosingEntity
        methodName = sanitizeMethodName com meth |> Naming.lowerFirst
        range = makeRangeFrom fsExpr
        callee = callee
        args = args
        returnType = makeType com fsExpr.Type
        decorators = meth.Attributes |> Seq.choose (makeDecorator com) |> Seq.toList
        calleeTypeArgs = typArgs |> List.map (makeType com) 
        methodTypeArgs = methTypArgs |> List.map (makeType com)
    }
    match Replacements.tryReplace com applyInfo with
    | Some _ as repl -> repl
    | None -> failwithf "Cannot find replacement for %s.%s at %A"
                applyInfo.ownerFullName applyInfo.methodName fsExpr.Range

// TODO                    
let (|Emitted|_|) (meth: FSharpMemberOrFunctionOrValue) =
    None

// TODO: Check `inline` annotation?
// TODO: If it's an imported method with ParamArray, spread the last argument
let makeCallFrom (com: IFabelCompiler) fsExpr (meth: FSharpMemberOrFunctionOrValue)
                 (typArgs, methTypArgs) callee args =
    let args =
        if not (hasRestParams meth) then args else
        let args = List.rev args
        match args.Head with
        | Fabel.Value(Fabel.ArrayConst(Fabel.ArrayValues items, _)) ->
            (List.rev args.Tail)@items
        | _ ->
            (Fabel.Spread args.Head |> Fabel.Value)::args.Tail |> List.rev
    match meth with
    (** -Check for replacements, emits... *)
    | Replaced com fsExpr (typArgs, methTypArgs) (callee, args) replaced -> replaced
    | Emitted replaced -> replaced
    (** -If the call is not resolved, then: *)
    | _ ->
        let methName = sanitizeMethodName com meth |> makeConst
        let typ, range = makeType com fsExpr.Type, makeRangeFrom fsExpr
    (**     *Check if this an extension *)
        let callee, args =
            if meth.IsExtensionMember
            then match callee with Some a -> None, a::args | None -> None, args
            else callee, args
            |> function
            | Some callee, args -> callee, args
            | None, args ->
                makeTypeFromDef com meth.EnclosingEntity |> makeTypeRef, args
    (**     *Check if this a getter or setter  *)
        if meth.IsPropertyGetterMethod then
            makeGetFrom com fsExpr callee methName
        elif meth.IsPropertySetterMethod then
            Fabel.Set (callee, Some methName, args.Head, range)
    (**     *Check if this is an implicit constructor *)
        elif meth.IsImplicitConstructor then
            Fabel.Apply (callee, args, Fabel.ApplyCons, typ, range)
    (**     *If nothing of the above applies, call the method normally *)
        else
            let calleeType = Fabel.PrimitiveType (Fabel.Function args.Length) 
            let callee = makeGet range calleeType callee methName
            Fabel.Apply (callee, args, Fabel.ApplyMeth, typ, range)
