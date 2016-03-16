namespace Fable.FSharp2Fable

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.AST.Fable.Util

type DecisionTarget =
    | TargetRef of Fable.Ident
    | TargetImpl of FSharpMemberOrFunctionOrValue list * FSharpExpr

type Context =
    {
    scope: (obj * Fable.Expr) list
    typeArgs: (string * Fable.Type) list
    decisionTargets: Map<int, DecisionTarget>
    baseClass: string option
    }
    static member Empty =
        { scope=[]; typeArgs=[]; decisionTargets=Map.empty<_,_>; baseClass=None }
    
type IReplacePlugin =
    inherit Fable.IPlugin
    abstract TryReplace: com: Fable.ICompiler -> info: Fable.ApplyInfo -> Fable.Expr option
    
type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fable.Expr
    abstract GetInternalFile: FSharpEntity -> string option
    abstract GetEntity: FSharpEntity -> Fable.Entity
    abstract TryGetInlineExpr: string -> (FSharpMemberOrFunctionOrValue list * FSharpExpr) option
    abstract AddInlineExpr: string -> (FSharpMemberOrFunctionOrValue list * FSharpExpr) -> unit
    abstract ReplacePlugins: IReplacePlugin list
    
module Patterns =
    open BasicPatterns

    let (|Rev|) = List.rev
    let (|Try|_|) (f: 'a -> 'b option) a = f a
    let (|Transform|) (com: IFableCompiler) = com.Transform
    let (|FieldName|) (fi: FSharpField) = fi.Name
    let (|ExprType|) (expr: Fable.Expr) = expr.Type
    let (|EntityKind|) (ent: Fable.Entity) = ent.Kind
    
    let (|NonAbbreviatedType|) (t: FSharpType) =
        let rec abbr (t: FSharpType) =
            if t.IsAbbreviation then abbr t.AbbreviatedType else t
        abbr t
    
    let (|ForOf|_|) = function
        | Let((_, value),
              Let((_, Call(None, meth, _, [], [])),
                TryFinally(
                  WhileLoop(_,
                    Let((ident, _), body)), _)))
        | Let((_, Call(Some value, meth, _, [], [])),
                TryFinally(
                    WhileLoop(_,
                        Let((ident, _), body)), _))
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
                | Coerce(_, Value marg) | Value marg ->
                    marg.IsEffectivelySameAs larg
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
        | "System.Int32"
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.int" _ -> Some Int32
        | "System.UInt32" -> Some UInt32
        | "System.Int64" -> Some Float64
        | "System.UInt64" -> Some Float64
        | "System.Single" -> Some Float32
        | "System.Double"
        // Units of measure
        | Naming.StartsWith "Microsoft.FSharp.Core.float" _ -> Some Float64
        | Naming.StartsWith "Microsoft.FSharp.Core.float32" _ -> Some Float32
        | _ -> None
        
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

    let (|OptionUnion|ListUnion|ErasedUnion|OtherType|) (typ: Fable.Type) =
        match typ with
        | Fable.DeclaredType ent ->
            match ent.FullName with
            | "Microsoft.FSharp.Core.Option" -> OptionUnion
            | "Microsoft.FSharp.Collections.List" -> ListUnion
            | _ when Option.isSome (ent.TryGetDecorator "Erase") -> ErasedUnion
            | _ -> OtherType
        | _ -> failwithf "Unexpected union type: %A" typ

module Types =
    open Patterns

    let sanitizeEntityName (ent: FSharpEntity) =
        match ent.FullName.LastIndexOf(".") with
        | -1 -> ent.DisplayName
        | i -> ent.FullName.Substring(0, i + 1) + ent.DisplayName

    // TODO: Exclude attributes meant to be compiled to JS
    let rec isAttributeEntity (ent: FSharpEntity) =
        match ent.BaseType with
        | Some (NonAbbreviatedType t) when t.HasTypeDefinition ->
            match t.TypeDefinition.TryFullName with
            | Some "System.Attribute" -> true
            | _ -> isAttributeEntity t.TypeDefinition
        | _ -> false

    let rec getBaseClass (com: IFableCompiler) (tdef: FSharpEntity) =
        let isIgnored (t: FSharpType) =
            not t.HasTypeDefinition
            || Option.isNone (com.GetInternalFile t.TypeDefinition)
        match tdef.BaseType with
        | None -> None
        | Some (NonAbbreviatedType t) ->
            if isIgnored t then None else
            let typeRef = makeType com Context.Empty t |> makeTypeRef com SourceLocation.Empty
            Some (sanitizeEntityName t.TypeDefinition, typeRef)
            
    // Some attributes (like ComDefaultInterface) will throw an exception
    // when trying to access ConstructorArguments
    and makeDecorator (com: IFableCompiler) (att: FSharpAttribute) =
        try
            let args = att.ConstructorArguments |> Seq.map snd |> Seq.toList
            let fullName =
                let fullName = sanitizeEntityName att.AttributeType
                if fullName.EndsWith ("Attribute")
                then fullName.Substring (0, fullName.Length - 9)
                else fullName
            Fable.Decorator(fullName, args) |> Some
        with _ ->
            None

    and makeEntity (com: IFableCompiler) (tdef: FSharpEntity) =
        let kind =
            if tdef.IsInterface then Fable.Interface
            elif tdef.IsFSharpRecord then Fable.Record
            elif tdef.IsFSharpUnion then Fable.Union
            elif tdef.IsFSharpExceptionDeclaration then Fable.Exception
            elif tdef.IsFSharpModule || tdef.IsNamespace then Fable.Module
            else Fable.Class (getBaseClass com tdef)
        let infcs =
            tdef.DeclaredInterfaces
            |> Seq.map (fun x -> sanitizeEntityName x.TypeDefinition)
            |> Seq.distinct
            |> Seq.toList
        let decs =
            tdef.Attributes
            |> Seq.choose (makeDecorator com)
            |> Seq.toList
        Fable.Entity (kind, com.GetInternalFile tdef,
            sanitizeEntityName tdef, infcs, decs, tdef.Accessibility.IsPublic)

    and makeTypeFromDef (com: IFableCompiler) (tdef: FSharpEntity) =
        // Guard: F# abbreviations shouldn't be passed as argument
        if tdef.IsFSharpAbbreviation
        then failwith "Abbreviation passed to makeTypeFromDef"
        // Delegate
        elif tdef.IsDelegate
        then Fable.Function (tdef.GenericParameters.Count - 1) |> Fable.PrimitiveType
        // Array
        elif tdef.IsArrayType then
            match tdef.GenericParameters.[0].FullName with
            | NumberKind kind -> Fable.TypedArray kind
            | _ -> Fable.DynamicArray
            |> Fable.Array |> Fable.PrimitiveType
        // Object
        elif tdef.FullName = "System.Object"
        then Fable.UnknownType
        else
        // .NET Primitives
        match tdef.FullName with
        | NumberKind kind -> Fable.Number kind |> Fable.PrimitiveType
        | "System.Boolean" -> Fable.Boolean |> Fable.PrimitiveType
        | "System.Char" | "System.String" -> Fable.String |> Fable.PrimitiveType
        | "System.Text.RegularExpressions.Regex" -> Fable.Regex |> Fable.PrimitiveType
        | "Microsoft.FSharp.Core.Unit" -> Fable.Unit |> Fable.PrimitiveType
        | "System.Collections.Generic.List`1" -> Fable.DynamicArray |> Fable.Array |> Fable.PrimitiveType
        // Declared Type
        | _ -> com.GetEntity tdef |> Fable.DeclaredType

    and makeType (com: IFableCompiler) (ctx: Context) (NonAbbreviatedType t) =
        let resolveGenParam (genParam: FSharpGenericParameter) =
            ctx.typeArgs
            |> List.tryFind (fun (name,_) -> name = genParam.Name)
            |> function Some (_,typ) -> typ | None -> Fable.UnknownType
        let rec countFuncArgs (fn: FSharpType) =
            if fn.IsFunctionType
            then countFuncArgs (Seq.last fn.GenericArguments) + 1
            else 0
        if t.IsGenericParameter
        then resolveGenParam t.GenericParameter
        elif t.IsTupleType
        then Fable.Tuple |> Fable.Array |> Fable.PrimitiveType
        elif t.IsFunctionType
        then Fable.Function (countFuncArgs t) |> Fable.PrimitiveType
        elif t.HasTypeDefinition
        then makeTypeFromDef com t.TypeDefinition
        else Fable.UnknownType // failwithf "Unexpected non-declared F# type: %A" t

    let (|FableType|) = makeType

module Identifiers =
    open Types

    /// Make a sanitized identifier from a tentative name
    let bindIdent (ctx: Context) typ (var: obj) =
        let tentativeName =
            match var with
            | :? FSharpMemberOrFunctionOrValue as fsRef -> fsRef.DisplayName
            | :? string as s -> s
            | _ -> sprintf "$var%i" ctx.scope.Length
        let sanitizedName = tentativeName |> Naming.sanitizeIdent (fun x ->
            List.exists (fun (_,x') ->
                match x' with
                | Fable.Value (Fable.IdentValue {name=name}) -> x = name
                | _ -> false) ctx.scope)
        let ident: Fable.Ident = { name=sanitizedName; typ=typ}
        let identValue = Fable.Value (Fable.IdentValue ident)
        { ctx with scope = (box var, identValue)::ctx.scope}, ident

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        bindIdent ctx (makeType com ctx fsRef.FullType) fsRef
    
    let (|BindIdent|) = bindIdentFrom

    let bindExpr ctx fsRef expr =
        { ctx with scope = (fsRef, expr)::ctx.scope}

    /// Get corresponding identifier to F# value in current scope
    let getBoundExpr com (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fun (fsName,_) -> obj.Equals(fsName, fsRef))
        |> function
        | Some (_,boundExpr) -> boundExpr
        | None -> failwithf "Detected non-bound identifier: %s in %A"
                    fsRef.DisplayName fsRef.DeclarationLocation

module Util =
    open Patterns
    open Types
    open Identifiers

    let isInline (meth: FSharpMemberOrFunctionOrValue) =
        match meth.InlineAnnotation with
        | FSharpInlineAnnotation.NeverInline
        | FSharpInlineAnnotation.OptionalInline -> false
        | FSharpInlineAnnotation.PseudoValue
        | FSharpInlineAnnotation.AlwaysInline -> true

    let isImported (ent: FSharpEntity) =
        let isImportedAtt att =
            att = "Global" || att = "Import"
        ent.FullName.StartsWith "Fable.Import"
        || Option.isSome(tryFindAtt isImportedAtt ent.Attributes)
        
    let isReplaceCandidate (com: IFableCompiler) (ent: FSharpEntity) =
        not(isImported ent)
        && (ent.FullName.StartsWith "Fable.Core" || Option.isNone(com.GetInternalFile ent))

    let getMemberKind name (meth: FSharpMemberOrFunctionOrValue) =
        if meth.IsImplicitConstructor then Fable.Constructor
        elif meth.IsPropertyGetterMethod then Fable.Getter (name, false)
        elif meth.IsPropertySetterMethod then Fable.Setter name
        else Fable.Method name
        
    let sanitizeMethodName com (meth: FSharpMemberOrFunctionOrValue) =
        let lowerFirstKnownInterfaces (meth: FSharpMemberOrFunctionOrValue) name =
            if meth.IsOverrideOrExplicitInterfaceImplementation &&
                meth.ImplementedAbstractSignatures.Count = 1
            then
                let sign = meth.ImplementedAbstractSignatures.[0].DeclaringType
                if sign.HasTypeDefinition &&
                    sign.TypeDefinition |> sanitizeEntityName |> Naming.knownInterfaces.Contains
                then Naming.lowerFirst name
                else name
            else name
        let isOverloadable (meth: FSharpMemberOrFunctionOrValue) =
            (meth.IsProperty
            || meth.IsEvent
            || meth.IsImplicitConstructor)
            |> not
        let overloadSuffix (meth: FSharpMemberOrFunctionOrValue) =
            if not(isOverloadable meth)
                || meth.EnclosingEntity.IsInterface
                || isImported meth.EnclosingEntity
                || isReplaceCandidate com meth.EnclosingEntity
            then ""
            else
                let kind = getMemberKind "" meth
                meth.EnclosingEntity.MembersFunctionsAndValues
                |> Seq.filter (fun x ->
                    isOverloadable x
                    && (getMemberKind "" x) = kind
                    && x.DisplayName = meth.DisplayName)
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
        meth.DisplayName
        |> Naming.removeParens
        |> Naming.removeGetSetPrefix
        |> Naming.sanitizeActivePattern
        // For known interfaces (IDisposable, etc), lower first letter
        // to make the method more idiomatic in JS
        |> lowerFirstKnownInterfaces meth
        |> (+) <| overloadSuffix meth

    let makeRange (r: Range.range) = {
        // source = Some r.FileName
        start = { line = r.StartLine; column = r.StartColumn }
        ``end``= { line = r.EndLine; column = r.EndColumn }
    }

    let makeRangeFrom (fsExpr: FSharpExpr) = 
        Some (makeRange fsExpr.Range)

    let makeLambdaArgs com ctx (vars: FSharpMemberOrFunctionOrValue list) =
        List.foldBack (fun var (ctx, accArgs) ->
            let newContext, arg = bindIdentFrom com ctx var
            newContext, arg::accArgs) vars (ctx, [])

    let getMethodArgs com ctx isInstance (args: FSharpMemberOrFunctionOrValue list list) =
        let args = if isInstance then Seq.skip 1 args |> Seq.toList else args
        match args with
        | [] -> ctx, []
        | [[singleArg]] ->
            makeType com ctx singleArg.FullType |> function
            | Fable.PrimitiveType Fable.Unit -> ctx, []
            | _ -> let ctx, arg = bindIdentFrom com ctx singleArg in ctx, [arg]
        | _ ->
            List.foldBack (fun tupledArg (ctx, accArgs) ->
                match tupledArg with
                | [] -> failwith "Unexpected empty tupled in curried arguments"
                | [nonTupledArg] ->
                    let ctx, arg = bindIdentFrom com ctx nonTupledArg
                    ctx, arg::accArgs
                | _ ->
                    // The F# compiler "untuples" the args in methods
                    let ctx, untupledArg = makeLambdaArgs com ctx tupledArg
                    ctx, untupledArg@accArgs
            ) args (ctx, [])

    let makeTryCatch com ctx (fsExpr: FSharpExpr) (Transform com ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some (BindIdent com ctx (catchContext, catchVar), catchBody) ->
                Some (catchVar, com.Transform catchContext catchBody)
            | None -> None
        let finalizer =
            match finalBody with
            | Some (Transform com ctx finalBody) -> Some finalBody
            | None -> None
        Fable.TryCatch (body, catchClause, finalizer, makeRangeFrom fsExpr)

    let makeGetFrom com ctx (fsExpr: FSharpExpr) callee propExpr =
        Fable.Apply (callee, [propExpr], Fable.ApplyGet, makeType com ctx fsExpr.Type, makeRangeFrom fsExpr)

    // TODO: This method doesn't work, the arguments don't keep
    // the ParamArray attribute
//    let hasRestParams (args: FSharpMemberOrFunctionOrValue list list) =
//        match args with
//        | [args] when args.Length > 0 ->
//            tryFindAtt ((=) "ParamArray") (Seq.last args).Attributes
//            |> Option.isSome
//        | _ -> false

    let hasRestParams (meth: FSharpMemberOrFunctionOrValue) =
        if meth.CurriedParameterGroups.Count <> 1 then false else
        let args = meth.CurriedParameterGroups.[0]
        args.Count > 0 && args.[args.Count - 1].IsParamArrayArg
        
    let replace (com: IFableCompiler) ctx fsExpr ownerName methName (atts, typArgs, methTypArgs) (callee, args) =
        let pluginReplace i =
            com.ReplacePlugins |> Seq.tryPick (fun plugin -> plugin.TryReplace com i)
        let applyInfo: Fable.ApplyInfo = {
            ownerFullName = ownerName
            methodName = Naming.lowerFirst methName
            range = makeRangeFrom fsExpr
            callee = callee
            args = args
            returnType = makeType com ctx fsExpr.Type
            decorators = atts |> Seq.choose (makeDecorator com) |> Seq.toList
            calleeTypeArgs = typArgs |> List.map (makeType com ctx) 
            methodTypeArgs = methTypArgs |> List.map (makeType com ctx)
        }
        match applyInfo with
        | Try pluginReplace repl -> repl
        | Try (Replacements.tryReplace com) repl -> repl
        | _ -> failwithf "Cannot find replacement for %s.%s at %A"
                applyInfo.ownerFullName applyInfo.methodName fsExpr.Range
        
    let (|Replaced|_|) (com: IFableCompiler) ctx fsExpr
                    (typArgs, methTypArgs) (callee, args)
                    (meth: FSharpMemberOrFunctionOrValue) =
        if isReplaceCandidate com meth.EnclosingEntity
        then replace com ctx fsExpr
                (sanitizeEntityName meth.EnclosingEntity) (sanitizeMethodName com meth)
                (meth.Attributes, typArgs, methTypArgs) (callee, args) |> Some
        else None
                
    let (|Emitted|_|) com ctx fsExpr (callee, args) (meth: FSharpMemberOrFunctionOrValue) =
        match meth.Attributes with
        | ContainsAtt "Emit" [:? string as macro] ->
            let args = match callee with None -> args | Some c -> c::args
            let range, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
            Fable.Apply(Fable.Emit(macro) |> Fable.Value, args, Fable.ApplyMeth, typ, range)
            |> Some
        | _ -> None
        
    let (|Imported|_|) com ctx fsExpr (args: Fable.Expr list) (meth: FSharpMemberOrFunctionOrValue) =
        meth.Attributes
        |> Seq.choose (makeDecorator com)
        |> tryImported com meth.DisplayName
        |> function
            | Some expr ->
                let range, typ = makeRangeFrom fsExpr, makeType com ctx fsExpr.Type
                if meth.IsPropertyGetterMethod
                then expr
                elif meth.IsPropertySetterMethod
                then Fable.Set (expr, None, args.Head, range)
                else Fable.Apply(expr, args, Fable.ApplyMeth, typ, range)
                |> Some
            | None -> None

    let (|Inlined|_|) (com: IFableCompiler) fsExpr methTypArgs
                      (callee, args) (meth: FSharpMemberOrFunctionOrValue) =
        if not(isInline meth) then None else
        match com.TryGetInlineExpr meth.FullName with
        | Some (vars, fsExpr) ->
            let args = match callee with Some x -> x::args | None -> args
            let ctx =
                (Context.Empty, vars, args)
                |||> Seq.fold2 (fun ctx var arg -> bindExpr ctx var arg)
            let ctx =
                let typeArgs =
                    ([], meth.GenericParameters, List.map (makeType com ctx) methTypArgs)
                    |||> Seq.fold2 (fun acc genPar typArg ->
                        (genPar.Name, typArg)::acc)
                { ctx with typeArgs = typeArgs }
            com.Transform ctx fsExpr |> Some
        | None -> None

    let makeCallFrom (com: IFableCompiler) ctx fsExpr (meth: FSharpMemberOrFunctionOrValue)
                    (typArgs, methTypArgs) callee args =
        let args =
            if not (hasRestParams meth) then args else
            let args = List.rev args
            match args.Head with
            | Fable.Value(Fable.ArrayConst(Fable.ArrayValues items, _)) ->
                (List.rev args.Tail)@items
            | _ ->
                (Fable.Spread args.Head |> Fable.Value)::args.Tail |> List.rev
        match meth with
        (** -Check for replacements, emits... *)
        | Replaced com ctx fsExpr (typArgs, methTypArgs) (callee, args) replaced -> replaced
        | Emitted com ctx fsExpr (callee, args) emitted -> emitted
        | Imported com ctx fsExpr args imported -> imported
        | Inlined com fsExpr methTypArgs (callee, args) expr -> expr
        (** -If the call is not resolved, then: *)
        | _ ->
            let methExpr = sanitizeMethodName com meth |> makeConst
            let typ, range = makeType com ctx fsExpr.Type, makeRangeFrom fsExpr
        (**     *Check if this an extension *)
            let callee, args =
                if meth.IsExtensionMember
                then match callee with Some a -> None, a::args | None -> None, args
                else callee, args
                |> function
                | Some callee, args -> callee, args
                | None, args ->
                    makeTypeFromDef com meth.EnclosingEntity |> makeTypeRef com range, args
        (**     *Check if this a getter or setter  *)
            if meth.IsPropertyGetterMethod then
                makeGetFrom com ctx fsExpr callee methExpr
            elif meth.IsPropertySetterMethod then
                Fable.Set (callee, Some methExpr, args.Head, range)
        (**     *Check if this is an implicit constructor *)
            elif meth.IsImplicitConstructor then
                Fable.Apply (callee, args, Fable.ApplyCons, typ, range)
        (**     *If nothing of the above applies, call the method normally *)
            else
                let calleeType = Fable.PrimitiveType (Fable.Function args.Length) 
                let callee = makeGet range calleeType callee methExpr
                Fable.Apply (callee, args, Fable.ApplyMeth, typ, range)

    let wrapInLambda com ctx (fsExpr: FSharpExpr) (meth: FSharpMemberOrFunctionOrValue) =
        let arity =
            match makeType com ctx fsExpr.Type with
            | Fable.PrimitiveType (Fable.Function arity) -> arity
            | _ -> failwithf "Expecting a function value but got %A" fsExpr
        let lambdaArgs =
            [1..arity] |> List.map (fun i -> makeIdent (sprintf "$arg%i" i))
        let lambdaBody =
            let args = lambdaArgs |> List.map (Fable.IdentValue >> Fable.Value)
            makeCallFrom com ctx fsExpr meth ([],[]) None args
        Fable.Lambda (lambdaArgs, lambdaBody) |> Fable.Value
