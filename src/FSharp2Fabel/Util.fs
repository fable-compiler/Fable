module Fabel.FSharp2Fabel.Util

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel
open Fabel.AST
open Fabel.Plugins

type DecisionTarget =
    | TargetRef of Fabel.IdentifierExpr
    | TargetImpl of FSharpMemberOrFunctionOrValue list * FSharpExpr

type Context = {
        scope: (string * string) list
        decisionTargets: Map<int, DecisionTarget>
    }
    
type IFabelCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fabel.Expr
    abstract IsInternal: FSharpEntity -> bool
    abstract GetEntity: FSharpEntity -> Fabel.Entity
    abstract GetTypeEntity: FSharpEntity -> Fabel.TypeEntity
    abstract GetSource: FSharpEntity -> Fabel.SourceKind
    
[<AutoOpen>]
module Patterns =
    let (|Transform|) (com: IFabelCompiler) = com.Transform
    let (|FieldName|) (fi: FSharpField) = fi.Name
    let (|ExprKind|) (expr: Fabel.Expr) = expr.Kind
    let (|ExprType|) (expr: Fabel.Expr) = expr.Type

    let (|Range|) (r: Range.range) = {
        // source = Some r.FileName
        start = { line = r.StartLine + 1; column = r.StartColumn }
        ``end``= { line = r.EndLine + 1; column = r.EndColumn }
    }

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
        | "System.Double" -> Some Float64
        | _ -> None

    let (|ReplaceArgs|_|) (lambdaArgs: (Fabel.IdentifierExpr*Fabel.Expr) list) (nestedArgs: Fabel.Expr list) =
        let (|SplitList|) f li =
            List.foldBack (fun (x: 'a) (li1: 'a list, li2: 'a list) ->
                if f x then li1, x::li2 else x::li1, li2) li ([],[])
        if lambdaArgs.Length = 0 then
            None
        else
            let lambdaArgs, nestedArgs =
                nestedArgs |> List.fold (fun (lambdaArgs, nestedArgs) arg ->
                    match arg.Kind with
                    | Fabel.Value (Fabel.Identifier ident) ->
                        let splitter (a : Fabel.IdentifierExpr,_) = a.Name = ident
                        match lambdaArgs with
                        | SplitList splitter (lambdaArgs,[_,e]) -> lambdaArgs, e::nestedArgs
                        | _ -> lambdaArgs, arg::nestedArgs
                    | _ -> lambdaArgs, arg::nestedArgs) (lambdaArgs, [])
            if lambdaArgs.Length = 0 then Some(List.rev nestedArgs) else None

    let (|ForOf|_|) = function
        | BasicPatterns.Let
            ((_, value),
                BasicPatterns.Let ((_, BasicPatterns.Call (_, meth, _, [], [])),
                    BasicPatterns.TryFinally (
                        BasicPatterns.WhileLoop (_,
                            BasicPatterns.Let ((ident, _), body)), _)))
            when meth.DisplayName = "GetEnumerator" ->
            Some (ident, value, body)
        | _ -> None

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

    let (|NonAbbreviatedType|) (t: FSharpType) =
        let rec abbr (t: FSharpType) = if t.IsAbbreviation then abbr t.AbbreviatedType else t
        abbr t

    let (|OptionUnion|ListUnion|ErasedUnion|OtherType|) (typ: Fabel.Type) =
        match typ with
        | Fabel.DeclaredType typ ->
            match typ.FullName with
            | "Microsoft.FSharp.Core.Option" -> OptionUnion
            | "Microsoft.FSharp.Collections.List" -> ListUnion
            | _ when Option.isSome (typ.HasDecoratorNamed "Erase") -> ErasedUnion
            | _ -> OtherType
        | _ -> OtherType

[<AutoOpen>]
module Types =
    let sanitizeEntityName (name: string) =
        let idx = name.IndexOf ('`')
        if idx >= 0 then name.Substring (0, idx) else name

    let makeDecorator (com: IFabelCompiler) (att: FSharpAttribute) =
        if not(com.IsInternal att.AttributeType) then None else
        let args = att.ConstructorArguments |> Seq.map snd |> Seq.toList
        let fullName =
            let fullName = sanitizeEntityName att.AttributeType.FullName
            if fullName.EndsWith ("Attribute")
            then fullName.Substring (0, fullName.Length - 9)
            else fullName
        Fabel.Decorator(att.AttributeType.FullName, args) |> Some

    let makeTypeEntity (com: IFabelCompiler) (tdef: FSharpEntity) =
        let kind =
            if tdef.IsInterface then Fabel.Interface
            // elif tdef.IsFSharpExceptionDeclaration then Fabel.Exception
            elif tdef.IsFSharpRecord then Fabel.Record
            elif tdef.IsFSharpUnion then Fabel.Union
            elif tdef.IsFSharpModule then Fabel.Module
            else
                let parentClass =
                    match tdef.BaseType with
                    | None -> None
                    | Some (NonAbbreviatedType x) when not x.HasTypeDefinition
                        || x.TypeDefinition.FullName = "System.Object" -> None
                    | Some x -> Some x.TypeDefinition.FullName
                Fabel.Class parentClass
        // Take only interfaces and attributes with internal declaration
        let infcs =
            tdef.DeclaredInterfaces
            |> Seq.filter (fun (NonAbbreviatedType x) ->
                x.HasTypeDefinition && com.IsInternal x.TypeDefinition)
            |> Seq.map (fun x -> sanitizeEntityName x.TypeDefinition.FullName)
            |> Seq.toList
        let decs =
            tdef.Attributes |> Seq.choose (makeDecorator com) |> Seq.toList
        Fabel.TypeEntity (kind, sanitizeEntityName tdef.FullName, infcs, decs,
                    tdef.Accessibility.IsPublic, com.GetSource tdef)

    let makeEntity (com: IFabelCompiler) (tdef: FSharpEntity) =
        Fabel.Entity (sanitizeEntityName tdef.FullName,
            tdef.Attributes |> Seq.choose (makeDecorator com) |> Seq.toList,
            tdef.Accessibility.IsPublic,
            com.GetSource tdef)

    let rec makeTypeFromDef (com: IFabelCompiler) (tdef: FSharpEntity) =
        // Guard: F# abbreviations shouldn't be passed as argument
        if tdef.IsFSharpAbbreviation
        then failwith "Abbreviation passed to makeTypeFromDef"
        // Object
        elif tdef.FullName = "System.Object"
        then Fabel.UnknownType
        // Array
        elif tdef.IsArrayType then
            match tdef.GenericParameters.[0].FullName with
            | NumberKind kind -> Some kind | _ -> None
            |> function
                | Some numbeKind -> Fabel.TypedArray numbeKind
                | _ -> Fabel.DynamicArray
            |> Fabel.PrimitiveType
        else
        // .NET Primitives
        match tdef.FullName with
        | NumberKind kind -> Fabel.Number kind |> Fabel.PrimitiveType
        | "System.Boolean" -> Fabel.Boolean |> Fabel.PrimitiveType
        | "System.Char" -> Fabel.String true |> Fabel.PrimitiveType
        | "System.String" -> Fabel.String false |> Fabel.PrimitiveType
        | "Microsoft.FSharp.Core.Unit" -> Fabel.Unit |> Fabel.PrimitiveType
        | "System.Collections.Generic.List`1" -> Fabel.DynamicArray |> Fabel.PrimitiveType
        // Declared Type
        | _ -> com.GetTypeEntity tdef |> Fabel.DeclaredType

    and makeType (com: IFabelCompiler) (NonAbbreviatedType t) =
        let rec countFuncArgs (fn: FSharpType) =
            if not fn.IsFunctionType then 1 else
            fn.GenericArguments
            |> Seq.fold (fun (acc: int) x -> acc + (countFuncArgs fn)) 0
            |> (-) <| 1
        if t.IsTupleType then Fabel.DynamicArray |> Some
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
    let private sanitizeIdent (ctx: Context) (fsName: string) =
        let preventConflicts exists str =
            let rec check n =
                let name = if n > 0 then sprintf "%s%i" str n else str
                if not (exists name) then name else check (n+1)
            check 0
        let sanitizedName =
            // Replace Forbidden Chars
            let sanitizedName = IdentForbiddenChars.Regex.Replace(fsName, "_")
            // Check if it's a keyword
            Literals.jsKeywords.Contains sanitizedName
            |> function true -> "_" + sanitizedName | false -> sanitizedName
            // Check if it already exists in scope
            |> preventConflicts (fun x -> List.exists (fun (_,x') -> x = x') ctx.scope)
        { ctx with scope = (fsName, sanitizedName)::ctx.scope }, sanitizedName

    /// Make a sanitized identifier from a speculative name
    let makeSanitizedIdent ctx typ speculativeName =
        let _, sanitizedIdent = sanitizeIdent ctx speculativeName
        Fabel.IdentifierExpr (sanitizedIdent, typ)

    /// Get corresponding identifier to F# value in current scope
    let (|GetIdent|) com (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fun (fsName,_) -> fsName = fsRef.DisplayName)
        |> function
        | Some (_,fabelName) -> Fabel.IdentifierExpr (fabelName, makeType com fsRef.FullType)
        | None -> failwithf "Detected non-bound identifier: %s in %A" fsRef.DisplayName fsRef.DeclarationLocation

    /// sanitizeEntityName F# identifier and create new context
    let (|BindIdent|) com ctx (fsRef: FSharpMemberOrFunctionOrValue) =
        let newContext, sanitizedIdent = sanitizeIdent ctx fsRef.DisplayName
        newContext, Fabel.IdentifierExpr (sanitizedIdent, makeType com fsRef.FullType)

let makeExpr com (ctx: Context) (fsExpr: FSharpExpr) kind =
    let (FabelType com typ, Range range) = fsExpr.Type, fsExpr.Range
    Fabel.Expr (kind, typ, range)

let makeSequential statements =
    Fabel.Expr (Fabel.Sequential statements, (List.last statements).Type)

let makeTypeRef typ =
    Fabel.Expr (Fabel.Value (Fabel.TypeRef typ))

let makeConst (value: obj) =
    match value with
    | :? bool as x -> Fabel.BoolConst x
    | :? char as x -> Fabel.StringConst (string x)
    | :? string as x -> Fabel.StringConst x
    // Integer types
    | :? int as x -> Fabel.IntConst x
    | :? byte as x -> Fabel.IntConst (int x)
    | :? sbyte as x -> Fabel.IntConst (int x)
    | :? int16 as x -> Fabel.IntConst (int x)
    | :? uint16 as x -> Fabel.IntConst (int x)
    | :? uint32 as x -> Fabel.IntConst (int x)
    // Float types
    | :? float as x -> Fabel.FloatConst x
    | :? int64 as x -> Fabel.FloatConst (float x)
    | :? uint64 as x -> Fabel.FloatConst (float x)
    | :? float32 as x -> Fabel.FloatConst (float x)
    // TODO: Regex
    | :? unit | _ when value = null -> Fabel.Null
    | _ -> failwithf "Unexpected literal %O" value
    |> Fabel.Value

let makeLiteral (value: obj) = makeConst value |> Fabel.Expr

let makeLambdaArgs com ctx vars =
    List.foldBack (fun var (accContext, accArgs) ->
        let (BindIdent com accContext (newContext, arg)) = var
        newContext, arg::accArgs) vars (ctx, [])

let makeLambda com ctx range vars (Transform com ctx body) =
    let range = match range with Some (Range r) -> Some r | None -> None
    let newContext, args = makeLambdaArgs com ctx vars
    let args, body =
        match body.Kind with
        | Fabel.Lambda (args', body, Fabel.Immediate, false) -> args@args', body
        | _ -> args, body
    let typ = Fabel.Function args.Length |> Fabel.PrimitiveType
    Fabel.Expr (Fabel.Lambda (args, body, Fabel.Immediate, false), typ, ?range=range)

let makeTryCatch com ctx fsExpr (Transform com ctx body) catchClause finalBody =
    let catchClause =
        match catchClause with
        | Some (BindIdent com ctx (catchContext, catchVar), Transform com ctx catchBody) ->
            Some (catchVar, catchBody)
        | None -> None
    let finalizers =
        match finalBody with
        | Some (Transform com ctx finalBody) ->
            match finalBody with
            | ExprKind (Fabel.Sequential statements) -> statements
            | finalBody -> [finalBody]
        | None -> []
    Fabel.TryCatch (body, catchClause, finalizers)
    |> makeExpr com ctx fsExpr

let makeCoreCall modName (methName: string) args =
    let meth = Fabel.Get (Fabel.Value (Fabel.CoreModule modName) |> Fabel.Expr,
                makeLiteral methName) |> Fabel.Expr
    Fabel.Apply (meth, args, false)

let makeTypeTest (typ: Fabel.Type) expr =
    let checkType expr (primitiveType: string) =
        Fabel.Binary (BinaryEqualStrict,
            Fabel.Unary (UnaryTypeof, expr) |> Fabel.Operation |> Fabel.Expr,
            makeLiteral primitiveType) |> Fabel.Operation
    match typ with
    | Fabel.PrimitiveType kind ->
        match kind with
        | Fabel.String _ -> checkType expr "string"
        | Fabel.Number _ -> checkType expr "number"
        | Fabel.Boolean -> checkType expr "boolean"
        | Fabel.Unit -> Fabel.Binary (BinaryEqual, expr, Fabel.Value Fabel.Null |> Fabel.Expr)
                        |> Fabel.Operation
        | _ -> failwithf "Unsupported type test: %A" typ
    | Fabel.DeclaredType typEnt ->
        match typEnt.Kind with
        | Fabel.Interface -> makeCoreCall "Util" "hasInterface" [makeLiteral typEnt.FullName]
        | _ -> Fabel.Binary (BinaryInstanceOf, expr, makeTypeRef typ) |> Fabel.Operation
    | _ -> failwithf "Unsupported type test in: %A" typ

let makeGetApply expr methName args =
    let expr = Fabel.Get (expr, makeLiteral methName) |> Fabel.Expr
    Fabel.Apply (expr, args, false)

let makeApply ctx (expr: Fabel.Expr) (args: Fabel.Expr list) =
    let apply expr args = Fabel.Apply (expr, args, false)
    match expr with
    // Optimize id lambdas applied right away, as in `x |> unbox`
    | ExprKind (Fabel.Lambda ([lambdaArg], (:? Fabel.IdentifierExpr as identExpr), Fabel.Immediate, false))
        when lambdaArg.Name = identExpr.Name && args.Length = 1 ->
        args.Head.Kind
    // As Fabel lambdas have multiple args, they cannot be curried and we have to recreate the extra lambda
    // when applying less parameters than necessary (F# compiler already does that for methods)
    | ExprType (Fabel.PrimitiveType (Fabel.Function fnArgCount)) when fnArgCount > args.Length ->
        let lambdaArgs =
            [for i=1 to (fnArgCount - args.Length)
                do yield makeSanitizedIdent ctx Fabel.UnknownType (sprintf "i%i" i)]
        Fabel.Lambda (lambdaArgs,
            apply expr (args @ (lambdaArgs |> List.map unbox)) |> Fabel.Expr,
            Fabel.Immediate, false)
    // Flatten nested curried applications as it happens in pipelines: e.g., (fun x -> fnCall 2 x) 3
    | ExprKind (Fabel.Lambda (lambdaArgs,
                    ExprKind (Fabel.Apply (nestedExpr, nestedArgs, _)),
                    Fabel.Immediate, false)) ->
        match nestedArgs with
        | ReplaceArgs (List.zip lambdaArgs args) nestedArgs -> apply nestedExpr nestedArgs
        | _ -> apply expr args
    | _ -> apply expr args

let hasRestParams (meth: FSharpMemberOrFunctionOrValue) =
    if meth.CurriedParameterGroups.Count <> 1 then false else
    let args = meth.CurriedParameterGroups.[0]
    args |> Seq.iter (fun x ->
        if x.IsOutArg then failwithf "Out parameters are not supported: %s" meth.FullName)
    args.Count > 0 && args.[args.Count - 1].IsParamArrayArg

let splitLast (li: 'a list) =
    let rec split' (acc: 'a list) (li: 'a list): 'a list * 'a =
        match li with
        | [] -> failwith "splitLast doesn't support empty lists"
        | [last] -> List.rev acc, last
        | x::xs -> split' (x::acc) xs
    split' [] li

let makeCall com ctx fsExpr callee (meth: FSharpMemberOrFunctionOrValue) (args: FSharpExpr list) =
    let sanitizeMethFullName (meth: FSharpMemberOrFunctionOrValue) =
        let isOverloadable (meth: FSharpMemberOrFunctionOrValue) =
            meth.IsProperty || meth.IsEvent || meth.IsImplicitConstructor |> not
        let overloadSuffix (meth: FSharpMemberOrFunctionOrValue) =
            if not (isOverloadable meth) then "" else
            let overloads =
                meth.EnclosingEntity.MembersFunctionsAndValues
                |> Seq.filter (fun x -> isOverloadable x && x.DisplayName = meth.DisplayName)
                |> Seq.toArray
            if overloads.Length = 1 then "" else
            overloads
            |> Seq.mapi (fun i x -> i,x)
            |> Seq.tryPick (fun (i,x) -> if x.XmlDocSig = meth.XmlDocSig then Some i else None)
            |> function Some i when i > 0 -> string i | _ -> ""
        let ent = meth.EnclosingEntity
        let ns = match ent.Namespace with Some ns -> ns + "." | None -> ""
        let methName =
            if meth.DisplayName <> "( .ctor )" then meth.DisplayName
            elif meth.IsImplicitConstructor then meth.DisplayName
            else failwith "TODO: Secondary constructors"
        ns + ent.DisplayName + "." + methName + (overloadSuffix meth)
    (** ###Method call processing *)
    let methType, methFullName =
        let methType = makeTypeFromDef com meth.EnclosingEntity
        methType, sanitizeMethFullName meth
    let callee =
        callee |> Option.map (com.Transform ctx)
    let args =
        let args =
            if not (hasRestParams meth) then args else
            match splitLast args with
            | args, BasicPatterns.NewArray (_, restArgs) -> args@restArgs
            | _ -> failwithf "TODO: Spread when passing array ref to ParamArray arg"
        List.map (com.Transform ctx) args
    (** -If this a pipe, optimize *)
    match methFullName with
    | "Microsoft.FSharp.Core.Operators.( |> )" -> makeApply ctx args.Tail.Head [args.Head]
    | "Microsoft.FSharp.Core.Operators.( <| )" -> makeApply ctx args.Head args.Tail
    | _ ->
        (** -If this is an external method, check for replacements *)
        let resolved =
            match methType with
            | Fabel.DeclaredType typEnt
                when typEnt.Source <> Fabel.External -> None
            | _ ->
                match Replacements.tryReplace methFullName callee args with
                | Some _ as repl -> repl
                | None -> failwithf "Couldn't find replacemente for external method %s"
                                    methFullName
        (** -If no Emit attribute nor replacement has been found then: *)
        match resolved with
        | Some exprKind -> exprKind
        | None ->
        (**     *Check if this an extension *)
            let callee, args =
                if meth.IsExtensionMember
                then match callee with Some a -> None, a::args | None -> None, args
                else callee, args
                |> function
                | Some callee, args -> callee, args
                | None, args -> Fabel.Value (Fabel.TypeRef methType) |> Fabel.Expr, args
        (**     *Check if this a getter or setter  *)
            if meth.IsPropertyGetterMethod then
                Fabel.Get (callee, makeLiteral meth.DisplayName)
            elif meth.IsPropertySetterMethod then
                Fabel.Set (callee, Some (makeLiteral meth.DisplayName), args.Head)
        (**     *Check if this is an implicit constructor *)
            elif meth.IsImplicitConstructor then
                Fabel.Apply (callee, args, true)
        (**     *If nothing of the above applies, call the method normally *)
            else
                let methName = methFullName.Substring (methFullName.LastIndexOf "." + 1)
                makeGetApply callee methName args
    |> makeExpr com ctx fsExpr
