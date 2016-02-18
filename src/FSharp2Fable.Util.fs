module Fable.FSharp2Fable.Util

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.Plugins
open Fable.AST.Fable.Util

type DecisionTarget =
    | TargetRef of Fable.Ident
    | TargetImpl of FSharpMemberOrFunctionOrValue list * FSharpExpr

type Context =
    {
    scope: (string * Fable.Expr) list
    decisionTargets: Map<int, DecisionTarget>
    owner: Fable.Entity option
    }
    static member Empty =
        { scope=[]; decisionTargets=Map.empty<_,_>; owner=None }
    
type IFableCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fable.Expr
    abstract GetInternalFile: FSharpEntity -> string option
    abstract GetEntity: FSharpEntity -> Fable.Entity
    
[<AutoOpen>]
module Patterns =
    open BasicPatterns

    let (|Rev|) = List.rev
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
        
    let (|Location|_|) (com: IFableCompiler) (ent: FSharpEntity) =
        match com.GetInternalFile ent with
        | Some file -> Some { Fable.file=file; Fable.fullName=ent.FullName }
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

    let (|OptionUnion|ListUnion|ErasedUnion|OtherType|) (typ: Fable.Type) =
        match typ with
        | Fable.DeclaredType ent ->
            match ent.FullName with
            | "Microsoft.FSharp.Core.Option" -> OptionUnion
            | "Microsoft.FSharp.Collections.List" -> ListUnion
            | _ when Option.isSome (ent.TryGetDecorator "Erase") -> ErasedUnion
            | _ -> OtherType
        | _ -> failwithf "Unexpected union type: %A" typ

[<AutoOpen>]
module Types =
    let sanitizeEntityName (ent: FSharpEntity) =
        match ent.FullName.LastIndexOf(".") with
        | -1 -> ent.DisplayName
        | i -> ent.FullName.Substring(0, i + 1) + ent.DisplayName
        
    let getBaseClassLocation (tdef: FSharpEntity) =
        let ignored = set ["System.Object"; "System.Exception"]
        match tdef.BaseType with
        | None -> None
        | Some (NonAbbreviatedType t)
            when not t.HasTypeDefinition || ignored.Contains t.TypeDefinition.FullName ->
            None
        | Some (NonAbbreviatedType t) ->
            Some { Fable.file = t.TypeDefinition.DeclarationLocation.FileName
                   Fable.fullName = t.TypeDefinition.FullName }
            
    // Some attributes (like ComDefaultInterface) will throw an exception
    // when trying to access ConstructorArguments
    let makeDecorator (com: IFableCompiler) (att: FSharpAttribute) =
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

    let makeEntity (com: IFableCompiler) (tdef: FSharpEntity) =
        let kind =
            if tdef.IsInterface then Fable.Interface
            elif tdef.IsFSharpRecord then Fable.Record
            elif tdef.IsFSharpUnion then Fable.Union
            elif tdef.IsFSharpExceptionDeclaration then Fable.Exception
            elif tdef.IsFSharpModule || tdef.IsNamespace then Fable.Module
            else Fable.Class (getBaseClassLocation tdef)
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

    let rec makeTypeFromDef (com: IFableCompiler) (tdef: FSharpEntity) =
        // Guard: F# abbreviations shouldn't be passed as argument
        if tdef.IsFSharpAbbreviation
        then failwith "Abbreviation passed to makeTypeFromDef"
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

    and makeType (com: IFableCompiler) (NonAbbreviatedType t) =
        let rec countFuncArgs (fn: FSharpType) =
            if fn.IsFunctionType
            then countFuncArgs (Seq.last fn.GenericArguments) + 1
            else 0
        if t.IsGenericParameter then Fable.UnknownType else
        if t.IsTupleType then Fable.Tuple |> Fable.Array |> Some
        elif t.IsFunctionType then Fable.Function (countFuncArgs t) |> Some
        else None
        |> function
            | Some fableType -> fableType |> Fable.PrimitiveType
            | None ->
                if not t.HasTypeDefinition
                then Fable.UnknownType // failwithf "Unexpected non-declared F# type: %A" t
                else makeTypeFromDef com t.TypeDefinition

    let (|FableType|) = makeType

[<AutoOpen>]
module Identifiers =
    /// Make a sanitized identifier from a tentative name
    let bindIdent (ctx: Context) typ (tentativeName: string) =
        let sanitizedName = tentativeName |> Naming.sanitizeIdent (fun x ->
            List.exists (fun (_,x') ->
                match x' with
                | Fable.Value (Fable.IdentValue {name=name}) -> x = name
                | _ -> false) ctx.scope)
        let ident: Fable.Ident = { name=sanitizedName; typ=typ}
        let identValue = Fable.Value (Fable.IdentValue ident)
        { ctx with scope = (tentativeName, identValue)::ctx.scope}, ident

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
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

// module Fable.FSharp2Fable.Util

// Is external entity?
let isExternalEntity (com: IFableCompiler) (ent: FSharpEntity) =
    if ent.FullName.StartsWith("Fable.Core")
    then true
    else match com.GetInternalFile ent with
         | None -> true | Some _ -> false
    
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
        meth.EnclosingEntity.IsInterface
        || meth.IsProperty
        || meth.IsEvent
        || meth.IsImplicitConstructor
        |> not
    let overloadSuffix (meth: FSharpMemberOrFunctionOrValue) =
        (isOverloadable meth && not(isExternalEntity com meth.EnclosingEntity))
        |> function
        | false -> ""
        | true ->
            let kind = getMemberKind "" meth
            meth.EnclosingEntity.MembersFunctionsAndValues
            |> Seq.filter (fun x -> isOverloadable x &&
                                    (getMemberKind "" x) = kind &&
                                    x.DisplayName = meth.DisplayName)
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
    let isUnitVar =
        match vars with
        | [var] ->
            match makeType com var.FullType with
            | Fable.PrimitiveType Fable.Unit -> true
            | _ -> false
        | _ -> false
    if isUnitVar
    then ctx, []
    else List.foldBack (fun var (ctx, accArgs) ->
        let newContext, arg = bindIdentFrom com ctx var
        newContext, arg::accArgs) vars (ctx, [])

let getMethodArgs com ctx isInstance (args: FSharpMemberOrFunctionOrValue list list) =
    let args = if isInstance then Seq.skip 1 args |> Seq.toList else args
    match args with
    | [] -> ctx, []
    | [[singleArg]] ->
        makeType com singleArg.FullType |> function
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

let makeGetFrom com (fsExpr: FSharpExpr) callee propExpr =
    Fable.Apply (callee, [propExpr], Fable.ApplyGet, makeType com fsExpr.Type, makeRangeFrom fsExpr)

let hasRestParams (args: FSharpMemberOrFunctionOrValue list list) =
    match args with
    | [args] when args.Length > 0 ->
        let last = Seq.last args
        last.Attributes |> Seq.exists (fun att ->
            att.AttributeType.FullName = "System.ParamArrayAttribute")
    | _ -> false

let hasRestParamsFrom (meth: FSharpMemberOrFunctionOrValue) =
    if meth.CurriedParameterGroups.Count <> 1 then false else
    let args = meth.CurriedParameterGroups.[0]
    args.Count > 0 && args.[args.Count - 1].IsParamArrayArg
    
let replace com fsExpr ownerName methName (atts, typArgs, methTypArgs) (callee, args) =
    let applyInfo: Fable.ApplyInfo = {
        ownerFullName = ownerName
        methodName = Naming.lowerFirst methName
        range = makeRangeFrom fsExpr
        callee = callee
        args = args
        returnType = makeType com fsExpr.Type
        decorators = atts |> Seq.choose (makeDecorator com) |> Seq.toList
        calleeTypeArgs = typArgs |> List.map (makeType com) 
        methodTypeArgs = methTypArgs |> List.map (makeType com)
    }
    match Replacements.tryReplace com applyInfo with
    | Some repl -> repl
    | None -> failwithf "Cannot find replacement for %s.%s at %A"
                applyInfo.ownerFullName applyInfo.methodName fsExpr.Range
    
let (|Replaced|_|) (com: IFableCompiler) fsExpr
                   (typArgs, methTypArgs) (callee, args)
                   (meth: FSharpMemberOrFunctionOrValue) =
    if isExternalEntity com meth.EnclosingEntity
    then replace com fsExpr
            (sanitizeEntityName meth.EnclosingEntity) (sanitizeMethodName com meth)
            (meth.Attributes, typArgs, methTypArgs) (callee, args) |> Some
    else None
               
let (|Emitted|_|) com fsExpr (callee, args) (meth: FSharpMemberOrFunctionOrValue) =
    match meth.Attributes with
    | ContainsAtt "Emit" [:? string as macro] ->
        let args = match callee with None -> args | Some c -> c::args
        let range, typ = makeRangeFrom fsExpr, makeType com fsExpr.Type
        Fable.Apply(Fable.Emit(macro) |> Fable.Value, args, Fable.ApplyMeth, typ, range)
        |> Some
    | _ -> None

// TODO: Check `inline` annotation?
let makeCallFrom (com: IFableCompiler) fsExpr (meth: FSharpMemberOrFunctionOrValue)
                 (typArgs, methTypArgs) callee args =
    let args =
        if not (hasRestParamsFrom meth) then args else
        let args = List.rev args
        match args.Head with
        | Fable.Value(Fable.ArrayConst(Fable.ArrayValues items, _)) ->
            (List.rev args.Tail)@items
        | _ ->
            (Fable.Spread args.Head |> Fable.Value)::args.Tail |> List.rev
    match meth with
    (** -Check for replacements, emits... *)
    | Replaced com fsExpr (typArgs, methTypArgs) (callee, args) replaced -> replaced
    | Emitted com fsExpr (callee, args) emitted -> emitted
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
                makeTypeFromDef com meth.EnclosingEntity |> makeTypeRef com, args
    (**     *Check if this a getter or setter  *)
        if meth.IsPropertyGetterMethod then
            makeGetFrom com fsExpr callee methName
        elif meth.IsPropertySetterMethod then
            Fable.Set (callee, Some methName, args.Head, range)
    (**     *Check if this is an implicit constructor *)
        elif meth.IsImplicitConstructor then
            Fable.Apply (callee, args, Fable.ApplyCons, typ, range)
    (**     *If nothing of the above applies, call the method normally *)
        else
            let calleeType = Fable.PrimitiveType (Fable.Function args.Length) 
            let callee = makeGet range calleeType callee methName
            Fable.Apply (callee, args, Fable.ApplyMeth, typ, range)

let wrapInLambda com (fsExpr: FSharpExpr) (meth: FSharpMemberOrFunctionOrValue) =
    let arity =
        match makeType com fsExpr.Type with
        | Fable.PrimitiveType (Fable.Function arity) -> arity
        | _ -> failwithf "Expecting a function value but got %A" fsExpr
    // TODO: More arguments just in case?
    let lambdaArgs =
        [1..arity] |> List.map (fun i -> makeIdent (sprintf "$arg%i" i))
    let lambdaBody =
        let args = lambdaArgs |> List.map (Fable.IdentValue >> Fable.Value)
        makeCallFrom com fsExpr meth ([],[]) None args
    Fable.Lambda (lambdaArgs, lambdaBody) |> Fable.Value
