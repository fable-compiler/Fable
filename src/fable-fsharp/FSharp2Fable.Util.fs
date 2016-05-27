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
    scope: (FSharpMemberOrFunctionOrValue option * Fable.Expr) list
    typeArgs: (string * Fable.Type) list
    decisionTargets: Map<int, DecisionTarget>
    baseClass: string option
    logs: ResizeArray<LogMessage>
    }
    static member Empty(logs) =
        { scope=[]; typeArgs=[]; decisionTargets=Map.empty<_,_>; baseClass=None; logs=logs }
    
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
    
module Helpers =
    let tryFindAtt f (atts: #seq<FSharpAttribute>) =
        atts |> Seq.tryPick (fun att ->
            match att.AttributeType.TryFullName with
            | Some fullName ->
                fullName.Substring(fullName.LastIndexOf "." + 1).Replace("Attribute", "")
                |> f |> function true -> Some att | false -> None
            | None -> None)
        
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
        
    let hasReplaceAtt (atts: #seq<FSharpAttribute>) =
        tryFindAtt ((=) "Replace") atts |> Option.isSome
        
    let isExternalEntity (com: IFableCompiler) (ent: FSharpEntity) =
        not(isImported ent) && Option.isNone(com.GetInternalFile ent)

    let isReplaceCandidate (com: IFableCompiler) (ent: FSharpEntity) =
        ent.FullName.StartsWith "Fable.Core" || isExternalEntity com ent

    let makeRange (r: Range.range) = {
        start = { line = r.StartLine; column = r.StartColumn }
        ``end``= { line = r.EndLine; column = r.EndColumn }
    }

    let makeRangeFrom (fsExpr: FSharpExpr) = 
        Some (makeRange fsExpr.Range)
        
    let rec countFuncArgs (fn: FSharpType) =
        if fn.IsFunctionType
        then countFuncArgs (Seq.last fn.GenericArguments) + 1
        else 0
        
    let getEntityLocation (ent: FSharpEntity) =
        match ent.ImplementationLocation with
        | Some loc -> loc
        | None -> ent.DeclarationLocation        

    let getRefLocation (ent: FSharpMemberOrFunctionOrValue) =
        match ent.ImplementationLocation with
        | Some loc -> loc
        | None -> ent.DeclarationLocation   

    /// Lower first letter if there's no explicit compiled name
    let lowerUnionCaseName (unionCase: FSharpUnionCase) =
        unionCase.Attributes
        |> tryFindAtt ((=) "CompiledName")
        |> function
            | Some name -> name.ConstructorArguments.[0] |> snd |> string
            | None -> Naming.lowerFirst unionCase.DisplayName
        |> makeConst
    
module Patterns =
    open BasicPatterns
    open Helpers

    let (|Rev|) = List.rev
    let (|Transform|) (com: IFableCompiler) = com.Transform
    let (|FieldName|) (fi: FSharpField) = fi.Name
    let (|ExprType|) (expr: Fable.Expr) = expr.Type
    let (|EntityKind|) (ent: Fable.Entity) = ent.Kind
    
    let (|TypeDefinition|_|) (t: FSharpType) =
        if t.HasTypeDefinition then Some t.TypeDefinition else None

    let (|NonAbbreviatedType|) (t: FSharpType) =
        let rec abbr (t: FSharpType) =
            if t.IsAbbreviation then abbr t.AbbreviatedType else t
        abbr t

    let (|RefType|_|) = function
        | NonAbbreviatedType(TypeDefinition tdef) as t
            when tdef.TryFullName = Some "Microsoft.FSharp.Core.FSharpRef`1" -> Some t
        | _ -> None
        
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
                | Coerce(_, Value arg) | Value arg -> ident = arg
                | _ -> false)
        let checkArgs2 lambdaArgs methArgs =
            (lambdaArgs, methArgs)
            ||> List.forall2 (fun larg marg ->
                match marg with
                | Coerce(_, Value marg) | Value marg -> marg = larg
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
        | Pipe(Closure(arity, meth, typArgs, methTypArgs, methArgs), exprs)
            when arity = exprs.Length ->
                Some (meth, typArgs, methTypArgs, methArgs@exprs)
        | _ -> None
        
    /// This matches the boilerplate F# compiler generates for methods
    /// like Dictionary.TryGetValue (see #154)
    let (|TryGetValue|_|) = function
        | Let((outArg1, (DefaultValue _ as def)),
                NewTuple(_, [Call(callee, meth, typArgs, methTypArgs,
                                    [arg; AddressOf(Value outArg2)]); Value outArg3]))
            when outArg1 = outArg2 && outArg1 = outArg3 ->
            Some (callee, meth, typArgs, methTypArgs, [arg; def])
        | _ -> None

    /// This matches the boilerplate generated to wrap .NET events from F#
    let (|CreateEvent|_|) = function
        | Call(Some(Call(None, createEvent,_,_,
                        [Lambda(eventDelegate, Call(Some callee, addEvent,[],[],[Value eventDelegate']));
                         Lambda(eventDelegate2, Call(Some callee2, removeEvent,[],[],[Value eventDelegate2']));
                         Lambda(callback, NewDelegate(_, Lambda(delegateArg0, Lambda(delegateArg1, Application(Value callback',[],[Value delegateArg0'; Value delegateArg1'])))))])),
                meth, typArgs, methTypArgs, args)
                when createEvent.FullName = "Microsoft.FSharp.Core.CompilerServices.RuntimeHelpers.CreateEvent" ->
            let eventName = addEvent.DisplayName.Replace("add_","") |> Naming.lowerFirst 
            Some (callee, eventName, meth, typArgs, methTypArgs, args)
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
        
    let (|ContainsAtt|_|) (name: string) (atts: #seq<FSharpAttribute>) =
        atts |> tryFindAtt ((=) name) |> Option.map (fun att ->
            att.ConstructorArguments |> Seq.map snd |> Seq.toList) 

    let (|OptionUnion|ListUnion|ErasedUnion|KeyValueUnion|StringEnum|OtherType|) (typ: Fable.Type) =
        let (|FullName|) (ent: Fable.Entity) = ent.FullName
        let (|TryDecorator|_|) dec (ent: Fable.Entity) = ent.TryGetDecorator dec
        match typ with
        | Fable.DeclaredType ent ->
            match ent with
            | FullName "Microsoft.FSharp.Core.Option" -> OptionUnion
            | FullName "Microsoft.FSharp.Collections.List" -> ListUnion
            | TryDecorator "Erase" _ -> ErasedUnion
            | TryDecorator "KeyValueList" _ -> KeyValueUnion
            | TryDecorator "StringEnum" _ -> StringEnum
            | _ -> OtherType
        | _ -> failwithf "Unexpected union type: %s" typ.FullName

module Types =
    open Helpers
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
            not t.HasTypeDefinition || isExternalEntity com t.TypeDefinition
        match tdef.BaseType with
        | None -> None
        | Some (NonAbbreviatedType t) ->
            if isIgnored t then None else
            let typeRef =
                makeType com (ResizeArray<_>() |> Context.Empty) t
                |> makeTypeRef com (Some SourceLocation.Empty)
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
            sanitizeEntityName tdef, infcs, decs,
            tdef.Accessibility.IsPublic || tdef.Accessibility.IsInternal)

    and makeTypeFromDef (com: IFableCompiler) (tdef: FSharpEntity) =
        // Guard: F# abbreviations shouldn't be passed as argument
        if tdef.IsFSharpAbbreviation
        then failwith "Abbreviation passed to makeTypeFromDef"
        // Enum
        elif tdef.IsEnum
        then Fable.Enum tdef.FullName |> Fable.PrimitiveType
        // Delegate
        elif tdef.IsDelegate
        then Fable.Function (tdef.GenericParameters.Count - 1) |> Fable.PrimitiveType
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
        // Generic parameter (try to resolve for inline functions)
        if t.IsGenericParameter
        then resolveGenParam t.GenericParameter
        // Tuple
        elif t.IsTupleType
        then Fable.Tuple |> Fable.Array |> Fable.PrimitiveType
        // Funtion
        elif t.IsFunctionType
        then Fable.Function (countFuncArgs t) |> Fable.PrimitiveType
        elif t.HasTypeDefinition then
            // Array
            if t.TypeDefinition.IsArrayType then
                match makeType com ctx t.GenericArguments.[0] with
                | Fable.PrimitiveType(Fable.Number kind) -> Fable.TypedArray kind
                | _ -> Fable.DynamicArray
                |> Fable.Array |> Fable.PrimitiveType
            // Declared type
            else makeTypeFromDef com t.TypeDefinition
        else Fable.UnknownType // failwithf "Unexpected non-declared F# type: %A" t

    let (|FableType|) = makeType

module Identifiers =
    open Helpers
    open Types

    let bindExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) expr =
        { ctx with scope = (Some fsRef, expr)::ctx.scope}

    /// Make a sanitized identifier from a tentative name
    let bindIdent (ctx: Context) typ (fsRef: FSharpMemberOrFunctionOrValue option) tentativeName =
        let sanitizedName = tentativeName |> Naming.sanitizeIdent (fun x ->
            List.exists (fun (_,x') ->
                match x' with
                | Fable.Value (Fable.IdentValue {name=name}) -> x = name
                | _ -> false) ctx.scope)
        let ident: Fable.Ident = { name=sanitizedName; typ=typ}
        let identValue = Fable.Value (Fable.IdentValue ident)
        { ctx with scope = (fsRef, identValue)::ctx.scope}, ident

    /// Sanitize F# identifier and create new context
    let bindIdentFrom com ctx (fsRef: FSharpMemberOrFunctionOrValue): Context*Fable.Ident =
        bindIdent ctx (makeType com ctx fsRef.FullType) (Some fsRef) fsRef.DisplayName
    
    let (|BindIdent|) = bindIdentFrom

    let tryGetBoundExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fst >> function Some fsRef' -> obj.Equals(fsRef, fsRef') | None -> false)
        |> function Some (_,boundExpr) -> Some boundExpr | None -> None

    /// Get corresponding identifier to F# value in current scope
    let getBoundExpr (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        match tryGetBoundExpr ctx fsRef with
        | Some boundExpr -> boundExpr
        | None -> failwithf "Detected non-bound identifier: %s in %O"
                    fsRef.DisplayName (getRefLocation fsRef |> makeRange)

module Util =
    open Helpers
    open Patterns
    open Types
    open Identifiers

    let getMemberKind name (meth: FSharpMemberOrFunctionOrValue) =
        if meth.EnclosingEntity.IsFSharpModule then
            // TODO: Another way to check module values?
            match meth.XmlDocSig.[0] with
            | 'P' -> Fable.Getter (name, true)
            | _ -> Fable.Method name
        elif meth.IsImplicitConstructor then Fable.Constructor
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
        if meth.CompiledName.StartsWith "op_"
        then meth.CompiledName
        else meth.DisplayName
        |> Naming.removeParens
        |> Naming.removeGetSetPrefix
        |> Naming.sanitizeActivePattern
        // For known interfaces (IDisposable, etc), lower first letter
        // to make the method more idiomatic in JS
        |> lowerFirstKnownInterfaces meth
        |> (+) <| overloadSuffix meth

    let makeLambdaArgs com ctx (vars: FSharpMemberOrFunctionOrValue list) =
        List.foldBack (fun var (ctx, accArgs) ->
            let newContext, arg = bindIdentFrom com ctx var
            newContext, arg::accArgs) vars (ctx, [])

    let getMethodArgs com ctx isInstance (args: FSharpMemberOrFunctionOrValue list list) =
        let ctx, args =
            match args with
            | [thisArg]::args when isInstance ->
                bindExpr ctx thisArg (Fable.Value Fable.This), args
            | _ -> ctx, args
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

    let makeGetFrom com ctx r typ callee propExpr =
        Fable.Apply (callee, [propExpr], Fable.ApplyGet, typ, r)

    // TODO: This method doesn't work, the arguments don't keep the attributes
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

    let replace (com: IFableCompiler) ctx r typ ownerName methName methKind
                (atts, typArgs, methTypArgs, lambdaArgArity) (callee, args) =
        let pluginReplace i =
            com.ReplacePlugins |> Seq.tryPick (fun plugin -> plugin.TryReplace com i)
        let applyInfo: Fable.ApplyInfo = {
            ownerFullName = ownerName
            methodName = Naming.lowerFirst methName
            methodKind = methKind
            range = r
            callee = callee
            args = args
            returnType = typ
            decorators = atts |> Seq.choose (makeDecorator com) |> Seq.toList
            calleeTypeArgs = typArgs |> List.map (makeType com ctx) 
            methodTypeArgs = methTypArgs |> List.map (makeType com ctx)
            lambdaArgArity = lambdaArgArity
        }
        match applyInfo with
        | Try pluginReplace repl -> repl
        | Try (Replacements.tryReplace com) repl -> repl
        | _ ->
            sprintf "Cannot find replacement for %s.%s"
                applyInfo.ownerFullName applyInfo.methodName
            |> attachRange r |> failwith
        
    let (|Replaced|_|) (com: IFableCompiler) ctx r typ
                    (typArgs, methTypArgs) (callee, args)
                    (meth: FSharpMemberOrFunctionOrValue) =
        if hasReplaceAtt meth.Attributes || isReplaceCandidate com meth.EnclosingEntity then
            let lambdaArgArity =
                if meth.CurriedParameterGroups.Count > 0
                    && meth.CurriedParameterGroups.[0].Count > 0
                then countFuncArgs meth.CurriedParameterGroups.[0].[0].Type
                else 0
            let methName = sanitizeMethodName com meth
            replace com ctx r typ
                (sanitizeEntityName meth.EnclosingEntity)
                methName (getMemberKind methName meth)
                (meth.Attributes, typArgs, methTypArgs, lambdaArgArity)
                (callee, args) |> Some
        else
            None
                
    let (|Emitted|_|) com ctx r typ (callee, args) (meth: FSharpMemberOrFunctionOrValue) =
        match meth.Attributes with
        | ContainsAtt "Emit" [:? string as macro] ->
            let args = match callee with None -> args | Some c -> c::args
            Fable.Apply(Fable.Emit(macro) |> Fable.Value, args, Fable.ApplyMeth, typ, r)
            |> Some
        | _ -> None
        
    let (|Imported|_|) com ctx r typ (args: Fable.Expr list) (meth: FSharpMemberOrFunctionOrValue) =
        meth.Attributes
        |> Seq.choose (makeDecorator com)
        |> tryImported com meth.DisplayName
        |> function
            | Some expr ->
                match getMemberKind "" meth with
                | Fable.Getter _ -> expr
                | Fable.Setter _ -> Fable.Set (expr, None, args.Head, r)
                | Fable.Constructor
                | Fable.Method _ -> Fable.Apply(expr, args, Fable.ApplyMeth, typ, r)
                |> Some
            | None -> None

    let (|Inlined|_|) (com: IFableCompiler) (ctx: Context) methTypArgs
                      (callee, args) (meth: FSharpMemberOrFunctionOrValue) =
        if not(isInline meth) then None else
        match com.TryGetInlineExpr meth.FullName with
        | Some (vars, fsExpr) ->
            let args = match callee with Some x -> x::args | None -> args
            let ctx =
                (Context.Empty ctx.logs, vars, args)
                |||> Seq.fold2 (fun ctx var arg ->
                    { ctx with scope = (Some var, arg)::ctx.scope })
            let ctx =
                let typeArgs =
                    ([], meth.GenericParameters, List.map (makeType com ctx) methTypArgs)
                    |||> Seq.fold2 (fun acc genPar typArg ->
                        (genPar.Name, typArg)::acc)
                { ctx with typeArgs = typeArgs }
            com.Transform ctx fsExpr |> Some
        | None ->
            failwithf "%s is inlined but is not reachable. %s"
                meth.FullName "If it belongs to an external project try removing inline modifier."

    let makeCallFrom (com: IFableCompiler) ctx r typ
                     (meth: FSharpMemberOrFunctionOrValue)
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
        | Replaced com ctx r typ (typArgs, methTypArgs) (callee, args) replaced -> replaced
        | Emitted com ctx r typ (callee, args) emitted -> emitted
        | Imported com ctx r typ args imported -> imported
        | Inlined com ctx methTypArgs (callee, args) expr -> expr
        (** -If the call is not resolved, then: *)
        | _ ->
            let methName = sanitizeMethodName com meth
        (**     *Check if this an extension *)
            match meth.IsExtensionMember, callee with
            | true, Some callee ->
                let typRef = makeTypeFromDef com meth.EnclosingEntity |> makeTypeRef com r
                let ext = makeGet r Fable.UnknownType typRef (makeConst methName)
                let bind = Fable.Emit("$0.bind($1)($2...)") |> Fable.Value
                Fable.Apply (bind, ext::callee::args, Fable.ApplyMeth, typ, r)
            | _ ->
                let callee =
                    match callee with
                    | Some callee -> callee
                    | None -> makeTypeFromDef com meth.EnclosingEntity |> makeTypeRef com r
        (**     *Check if this a getter or setter  *)
                match getMemberKind methName meth with
                | Fable.Getter (m, _) ->
                    match tryGetBoundExpr ctx meth with
                    | Some e -> e
                    | _ -> makeGetFrom com ctx r typ callee (makeConst m)
                | Fable.Setter m ->
                    match tryGetBoundExpr ctx meth with
                    | Some e -> Fable.Set (e, None, args.Head, r)
                    | _ -> Fable.Set (callee, Some (makeConst m), args.Head, r)
        (**     *Check if this is an implicit constructor *)
                | Fable.Constructor ->
                    Fable.Apply (callee, args, Fable.ApplyCons, typ, r)
        (**     *If nothing of the above applies, call the method normally *)
                | Fable.Method m ->
                    match tryGetBoundExpr ctx meth with
                    | Some e -> e
                    | _ ->
                        let calleeType = Fable.PrimitiveType (Fable.Function args.Length) 
                        makeGet r calleeType callee (makeConst m)
                    |> fun m -> Fable.Apply (m, args, Fable.ApplyMeth, typ, r)

    let wrapInLambda com ctx r typ (meth: FSharpMemberOrFunctionOrValue) =
        let arity =
            match typ with
            | Fable.PrimitiveType (Fable.Function arity) -> arity
            | _ -> failwithf "Expecting a function value but got %s" meth.FullName
        let lambdaArgs =
            [for i=1 to arity do yield Naming.getUniqueVar() |> makeIdent]
        let lambdaBody =
            let args = lambdaArgs |> List.map (Fable.IdentValue >> Fable.Value)
            makeCallFrom com ctx r typ meth ([],[]) None args
        Fable.Lambda (lambdaArgs, lambdaBody) |> Fable.Value

    let makeValueFrom com ctx r typ (v: FSharpMemberOrFunctionOrValue) =
        if not v.IsModuleValueOrMember
        then getBoundExpr ctx v
        elif v.IsMemberThisValue
        then Fable.This |> Fable.Value
        // External entities contain functions that will be replaced,
        // when they appear as a stand alone values, they must be wrapped in a lambda
        elif isReplaceCandidate com v.EnclosingEntity
        then wrapInLambda com ctx r typ v
        else
            match v with
            | Emitted com ctx r typ (None, []) emitted -> emitted
            | Imported com ctx r typ [] imported -> imported
            | Try (tryGetBoundExpr ctx) e -> e 
            | _ ->
                let typeRef =
                    makeTypeFromDef com v.EnclosingEntity
                    |> makeTypeRef com r
                Fable.Apply (typeRef, [makeConst v.DisplayName], Fable.ApplyGet, typ, r)
