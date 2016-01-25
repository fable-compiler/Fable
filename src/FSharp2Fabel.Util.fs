module Fabel.FSharp2Fabel.Util

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel
open Fabel.AST
open Fabel.Plugins

type DecisionTarget =
    | TargetRef of Fabel.Ident
    | TargetImpl of FSharpMemberOrFunctionOrValue list * FSharpExpr

type Context = {
    scope: (string * string) list
    decisionTargets: Map<int, DecisionTarget>
    }

type IFabelCompiler =
    inherit ICompiler
    abstract Transform: Context -> FSharpExpr -> Fabel.Expr
    abstract GetInternalFile: FSharpEntity -> string option
    abstract GetEntity: FSharpEntity -> Fabel.Entity

[<AutoOpen>]
module Patterns =
    let (|Transform|) (com: IFabelCompiler) = com.Transform
    let (|FieldName|) (fi: FSharpField) = fi.Name
    let (|ExprType|) (expr: Fabel.Expr) = expr.Type

    let (|NumberKind|_|) = function
        | "System.SByte" -> Some Int8
        | "System.Byte" -> Some UInt8Clamped
        | "System.Int16" -> Some Int16
        | "System.UInt16" -> Some UInt16
        | "System.Char" -> Some UInt16
        | "System.Int32" -> Some Int32
        | "System.UInt32" -> Some UInt32
        | "System.Int64" -> Some Float64
        | "System.UInt64" -> Some Float64
        | "System.Single" -> Some Float32
        | "System.Double" -> Some Float64
        | _ -> None

    let (|Location|_|) (com: IFabelCompiler) (ent: FSharpEntity) =
        match com.GetInternalFile ent with
        | Some file -> Some { Fabel.file=file; Fabel.fullName=ent.FullName }
        | None -> None

    let (|WithAttribute|_|) (name: string) (ent: FSharpEntity) =
        ent.Attributes
        |> Seq.tryPick (fun x ->
            match x.AttributeType.TryFullName with
            | Some fullName ->
                let attName = fullName.Substring(fullName.LastIndexOf "." + 1)
                if attName = name
                then Some (x.ConstructorArguments |> Seq.map snd |> Seq.toList)
                else None
            | None -> None)

    /// Is interface o inherits from System.Attribute?
    let (|AbstractEntity|_|) (ent: FSharpEntity) =
        if ent.IsInterface then Some ent else
        match ent.BaseType with
        | None -> None
        | Some t ->
            if not t.HasTypeDefinition then None else
            match t.TypeDefinition.TryFullName with
            | Some "System.Attribute" -> Some ent
            | _ -> None

    let (|ReplaceArgs|_|) (lambdaArgs: (Fabel.Ident * Fabel.Expr) list)
                          (nestedArgs: Fabel.Expr list) =
        let (|SplitList|) f li =
            List.foldBack (fun (x: 'a) (li1: 'a list, li2: 'a list) ->
                if f x then li1, x::li2 else x::li1, li2) li ([],[])
        if lambdaArgs.Length = 0 then
            None
        else
            let lambdaArgs, nestedArgs =
                nestedArgs |> List.fold (fun (lambdaArgs, nestedArgs) arg ->
                    match arg with
                    | Fabel.Value (Fabel.IdentValue ident) ->
                        let splitter (a: Fabel.Ident,_) = a.name = ident.name
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

    let makeDecorator (com: IFabelCompiler) (att: FSharpAttribute) =
        match com.GetInternalFile att.AttributeType with
        | None -> None
        | Some _ ->
            let args = att.ConstructorArguments |> Seq.map snd |> Seq.toList
            let fullName =
                let fullName = sanitizeEntityName att.AttributeType.FullName
                if fullName.EndsWith ("Attribute")
                then fullName.Substring (0, fullName.Length - 9)
                else fullName
            Fabel.Decorator(att.AttributeType.FullName, args) |> Some

    let makeEntity (com: IFabelCompiler) (tdef: FSharpEntity) =
        let kind =
            if tdef.IsInterface then Fabel.Interface
            // elif tdef.IsFSharpExceptionDeclaration then Fabel.Exception
            elif tdef.IsFSharpRecord then Fabel.Record
            elif tdef.IsFSharpUnion then Fabel.Union
            elif tdef.IsFSharpModule || tdef.IsNamespace then Fabel.Module
            else Fabel.Class (getBaseClassLocation tdef)
        // Take only interfaces and attributes with internal declaration
        let infcs =
            tdef.DeclaredInterfaces
            |> Seq.filter (fun (NonAbbreviatedType x) ->
                x.HasTypeDefinition &&
                com.GetInternalFile x.TypeDefinition |> Option.isSome)
            |> Seq.map (fun x -> sanitizeEntityName x.TypeDefinition.FullName)
            |> Seq.toList
        let decs =
            tdef.Attributes
            |> Seq.choose (makeDecorator com)
            |> Seq.toList
        Fabel.Entity (kind, com.GetInternalFile tdef,
            sanitizeEntityName tdef.FullName,
            makeRange tdef.DeclarationLocation,
            infcs, decs, tdef.Accessibility.IsPublic)

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
            |> Fabel.Array |> Fabel.PrimitiveType
        else
        // .NET Primitives
        match tdef.FullName with
        | NumberKind kind -> Fabel.Number kind |> Fabel.PrimitiveType
        | "System.Boolean" -> Fabel.Boolean |> Fabel.PrimitiveType
        | "System.Char" -> Fabel.Number UInt16 |> Fabel.PrimitiveType
        | "System.String" -> Fabel.String |> Fabel.PrimitiveType
        | "System.Text.RegularExpressions.Regex" -> Fabel.Regex |> Fabel.PrimitiveType
        | "Microsoft.FSharp.Core.Unit" -> Fabel.Unit |> Fabel.PrimitiveType
        | "System.Collections.Generic.List`1" -> Fabel.DynamicArray |> Fabel.Array |> Fabel.PrimitiveType
        // Declared Type
        | _ -> com.GetEntity tdef |> Fabel.DeclaredType

    and makeType (com: IFabelCompiler) (NonAbbreviatedType t) =
        if t.IsGenericParameter then Fabel.UnknownType else
        let rec countFuncArgs (fn: FSharpType) =
            if fn.IsFunctionType
            then countFuncArgs (Seq.last fn.GenericArguments) + 1
            else 0
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
    let makeIdent name typ = {Fabel.name=name; Fabel.typ=typ}

    let sanitizeIdent (ctx: Context) (fsName: string) =
        let sanitizedName = fsName |> Naming.sanitizeIdent (fun x ->
            List.exists (fun (_,x') -> x = x') ctx.scope)
        { ctx with scope = (fsName, sanitizedName)::ctx.scope }, sanitizedName

    /// Make a sanitized identifier from a speculative name
    let makeSanitizedIdent ctx typ speculativeName =
        let ctx, sanitizedIdent = sanitizeIdent ctx speculativeName
        ctx, makeIdent sanitizedIdent typ

    /// Get corresponding identifier to F# value in current scope
    let (|GetIdent|) com (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fun (fsName,_) -> fsName = fsRef.DisplayName)
        |> function
        | Some (_,fabelName) -> makeIdent fabelName (makeType com fsRef.FullType)
        | None -> failwithf "Detected non-bound identifier: %s in %A" fsRef.DisplayName fsRef.DeclarationLocation

    /// sanitizeEntityName F# identifier and create new context
    let (|BindIdent|) com ctx (fsRef: FSharpMemberOrFunctionOrValue) =
        let newContext, sanitizedIdent = sanitizeIdent ctx fsRef.DisplayName
        newContext, makeIdent sanitizedIdent (makeType com fsRef.FullType)

let makeRange (r: Range.range) = {
    // source = Some r.FileName
    start = { line = r.StartLine; column = r.StartColumn }
    ``end``= { line = r.EndLine; column = r.EndColumn }
}

let makeRangeFrom (fsExpr: FSharpExpr) =
    Some (makeRange fsExpr.Range)

let makeFnType args =
    List.length args |> Fabel.Function |> Fabel.PrimitiveType

let makeLogOp, makeBinOp, makeUnOp =
    let makeOp range typ args op =
        Fabel.Apply (Fabel.Value op, args, false, typ, range)
    (fun range typ args op -> makeOp range typ args (Fabel.LogicalOp op)),
    (fun range typ args op -> makeOp range typ args (Fabel.BinaryOp op)),
    (fun range typ args op -> makeOp range typ args (Fabel.UnaryOp op))

let rec makeSequential range statements =
    match statements with
    | [] -> Fabel.Value Fabel.Null
    | [expr] -> expr
    | first::rest ->
        match first with
        | Fabel.Value (Fabel.Null)
        // Calls to System.Object..ctor in class constructors
        | Fabel.Value (Fabel.ObjExpr []) -> makeSequential range rest
        | Fabel.Sequential (firstStatements, _) -> makeSequential range (firstStatements @ rest)
        | _ ->
            match rest with
            | [Fabel.Sequential (statements, _)] -> makeSequential range (first::statements)
            | _ -> Fabel.Sequential (statements, range)

let makeTypeRef typ =
    Fabel.Value (Fabel.TypeRef typ)

let makeConst (value: obj) =
    match value with
    | :? bool as x -> Fabel.BoolConst x
    | :? string as x -> Fabel.StringConst x
    // Integer types
    | :? int as x -> Fabel.IntConst (x, Int32)
    | :? byte as x -> Fabel.IntConst (int x, UInt8Clamped)
    | :? sbyte as x -> Fabel.IntConst (int x, Int8)
    | :? int16 as x -> Fabel.IntConst (int x, Int16)
    | :? uint16 as x -> Fabel.IntConst (int x, UInt16)
    | :? char as x -> Fabel.IntConst (int x, UInt16)
    | :? uint32 as x -> Fabel.IntConst (int x, UInt32)
    // Float types
    | :? float as x -> Fabel.FloatConst (x, Float64)
    | :? int64 as x -> Fabel.FloatConst (float x, Float64)
    | :? uint64 as x -> Fabel.FloatConst (float x, Float64)
    | :? float32 as x -> Fabel.FloatConst (float x, Float32)
    // TODO: Regex
    | :? unit | _ when value = null -> Fabel.Null
    | _ -> failwithf "Unexpected literal %O" value
    |> Fabel.Value

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
    else List.foldBack (fun var (accContext, accArgs) ->
        let (BindIdent com accContext (newContext, arg)) = var
        newContext, arg::accArgs) vars (ctx, [])

let makeLambda (com: IFabelCompiler) ctx vars (body: FSharpExpr) =
    let args, body =
        let ctx, args = makeLambdaArgs com ctx vars
        match com.Transform ctx body with
        | Fabel.Value (Fabel.Lambda {args=args2; body=body2; kind=Fabel.Immediate; restParams=false}) ->
            args@args2, body2
        | body -> args, body
    Fabel.Lambda {args=args; body=body; kind=Fabel.Immediate; restParams=false}
    |> Fabel.Value

let makeLoop (fsExpr: FSharpExpr) loopKind =
    Fabel.Loop (loopKind, makeRangeFrom fsExpr)

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

let makeCoreCall range typ args (modName: string, methName: string option, isCons: bool) =
    let meth =
        let coreExpr = Fabel.CoreModule modName |> Fabel.Value
        match methName with
        | None -> coreExpr
        | Some methName ->
            Fabel.Get (coreExpr, makeConst methName, makeFnType args)
    Fabel.Apply (meth, args, isCons, typ, range)

let makeTypeTest range (typ: Fabel.Type) expr =
    let stringType, boolType =
        Fabel.PrimitiveType Fabel.String, Fabel.PrimitiveType Fabel.Boolean
    let checkType (primitiveType: string) expr =
        let typof = makeUnOp None stringType [expr] UnaryTypeof
        makeBinOp range boolType [typof; makeConst primitiveType] BinaryEqualStrict
    match typ with
    | Fabel.PrimitiveType kind ->
        match kind with
        | Fabel.String _ -> checkType "string" expr
        | Fabel.Number _ -> checkType "number" expr
        | Fabel.Boolean -> checkType "boolean" expr
        | Fabel.Unit ->
            makeBinOp range boolType [expr; Fabel.Value Fabel.Null] BinaryEqual
        | _ -> failwithf "Unsupported type test: %A" typ
    | Fabel.DeclaredType typEnt ->
        match typEnt.Kind with
        | Fabel.Interface ->
            ("Util", Some "hasInterface", false)
            |> makeCoreCall range boolType [makeConst typEnt.FullName]
        | _ ->
            makeBinOp range boolType [expr; makeTypeRef typ] BinaryInstanceOf
    | _ -> failwithf "Unsupported type test in: %A" typ

let makeApply com ctx (fsExpr: FSharpExpr) (expr: Fabel.Expr) (args: Fabel.Expr list) =
    let typ, range = makeType com fsExpr.Type, makeRangeFrom fsExpr
    match expr with
    // Optimize id lambdas applied right away, as in `x |> unbox`
    | Fabel.Value
        (Fabel.Lambda { args = [lambdaArg]; kind = Fabel.Immediate; restParams = false;
            body = Fabel.Value (Fabel.IdentValue body) })
        when lambdaArg.name = body.name && args.Length = 1 -> args.Head
    // As Fabel lambdas have multiple args, they cannot be curried and we have to recreate the extra lambda
    // when applying less parameters than necessary (F# compiler already does that for methods)
    | ExprType (Fabel.PrimitiveType (Fabel.Function fnArgCount)) when fnArgCount > args.Length ->
        let _, lambdaArgs =
            [1..(fnArgCount - args.Length)] |> List.fold (fun (ctx, args) i ->
                let ctx, arg = makeSanitizedIdent ctx Fabel.UnknownType (sprintf "$arg%i" i)
                ctx, arg::args) (ctx, [])
        let args = args @ (lambdaArgs |> List.map (Fabel.IdentValue >> Fabel.Value))
        let apply = Fabel.Apply (expr, args, false, typ, None)
        Fabel.Lambda {args=lambdaArgs; body=apply; kind=Fabel.Immediate; restParams=false}
        |> Fabel.Value
    // Flatten nested curried applications as it happens in pipelines: e.g., (fun x -> fnCall 2 x) 3
    | Fabel.Value
        (Fabel.Lambda { args = lambdaArgs; kind = Fabel.Immediate; restParams = false;
             body = Fabel.Apply (nestedExpr, nestedArgs, isCons, _, _) }) ->
        match nestedArgs with
        | ReplaceArgs (List.zip lambdaArgs args) nestedArgs ->
            Fabel.Apply (nestedExpr, nestedArgs, isCons, typ, range)
        | _ -> Fabel.Apply (expr, args, false, typ, range)
    | _ -> Fabel.Apply (expr, args, false, typ, range)

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
        ns + ent.DisplayName + "." + meth.DisplayName + (overloadSuffix meth)
    (** ###Method call processing *)
    let typ, range = makeTypeRange com fsExpr
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
    | "Microsoft.FSharp.Core.Operators.( |> )" -> makeApply com ctx fsExpr args.Tail.Head [args.Head]
    | "Microsoft.FSharp.Core.Operators.( <| )" -> makeApply com ctx fsExpr args.Head args.Tail
    | "System.Object..ctor" -> Fabel.Value (Fabel.ObjExpr [])
    | _ ->
        (** -If this is an external method, check for replacements *)
        let resolved =
            match methType with
            | Fabel.DeclaredType typEnt when typEnt.File.IsSome ->
                None // TODO: Check for Emit attribute
            | _ ->
                match Replacements.tryReplace range methFullName callee args with
                | Some _ as repl -> repl
                | None -> failwithf "Couldn't find replacemente for external method %s"
                                    methFullName
        (** -If no Emit attribute nor replacement has been found then: *)
        match resolved with
        | Some exprKind -> exprKind |> makeExpr range typ
        | None ->
        (**     *Check if this an extension *)
            let callee, args =
                if meth.IsExtensionMember
                then match callee with Some a -> None, a::args | None -> None, args
                else callee, args
                |> function
                | Some callee, args -> callee, args
                | None, args -> Fabel.Value (Fabel.TypeRef methType), args
            let typ, range = makeType com fsExpr.Type, makeRangeFrom fsExpr
        (**     *Check if this a getter or setter  *)
            if meth.IsPropertyGetterMethod then
                Fabel.Get (callee, makeConst meth.DisplayName, typ)
            elif meth.IsPropertySetterMethod then
                Fabel.Set (callee, Some (makeConst meth.DisplayName), args.Head, range)
        (**     *Check if this is an implicit constructor *)
            elif meth.IsImplicitConstructor then
                Fabel.Apply (callee, args, true, typ, range)
        (**     *If nothing of the above applies, call the method normally *)
            else
                let methName = methFullName.Substring (methFullName.LastIndexOf "." + 1)
                let callee = Fabel.Get (callee, makeConst methName, makeFnType args)
                Fabel.Apply (callee, args, false, typ, range)
