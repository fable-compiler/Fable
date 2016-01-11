module Fabel.Transform.FSharp2Fabel

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fabel.AST

module private Util =
    type Emitter = Fabel.Expr list -> Fabel.ExprKind

    type DecisionTarget =
        | TargetRef of Fabel.IdentifierExpr
        | TargetImpl of FSharpMemberOrFunctionOrValue list * FSharpExpr

    type Context =
        {
            filenames: string list
            scope: (string * string) list
            decisionTargets: Map<int, DecisionTarget>
            entities: System.Collections.Concurrent.ConcurrentDictionary<string, Fabel.Entity>
        }
        member x.IsInternal (entity: FSharpEntity): bool =
            x.filenames |> List.exists ((=) entity.DeclarationLocation.FileName)

    let (|FieldName|) (fi: FSharpField) = fi.Name
    let (|ExprKind|) (expr: Fabel.Expr) = expr.Kind
    let (|ExprType|) (expr: Fabel.Expr) = expr.Type

    let (|Range|) (r: Range.range) = {
        source = Some r.FileName
        start = { line = r.StartLine + 1; column = r.StartColumn }
        ``end``= { line = r.EndLine + 1; column = r.EndColumn }
    }
    
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
    
module private Types =
    open Util

    let private sanitize (name: string) =
        let idx = name.IndexOf ('`')
        if idx >= 0 then name.Substring (0, idx) else name

    let private (|NonAbbreviatedType|) (t: FSharpType) =
        let rec abbr (t: FSharpType) = if t.IsAbbreviation then abbr t.AbbreviatedType else t
        abbr t

    let private (|NumberKind|_|) = function
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
        
    let private isNetPrimitive = function
        | NumberKind kind -> Fabel.Number kind |> Some
        | "System.Boolean" -> Fabel.Boolean |> Some
        | "System.Char" -> Fabel.String true |> Some
        | "System.String" -> Fabel.String false |> Some
        | "Microsoft.FSharp.Core.Unit" -> Fabel.Unit |> Some
        | "System.Collections.Generic.List`1" -> Fabel.DynamicArray |> Some
        | _ -> None

    let private makeDecorator (ctx: Context) (att: FSharpAttribute) =
        if not (ctx.IsInternal att.AttributeType) then None else
        let args = att.ConstructorArguments |> Seq.map snd |> Seq.toList
        let fullName =
            let fullName = sanitize att.AttributeType.FullName
            if fullName.EndsWith ("Attribute")
            then fullName.Substring (0, fullName.Length - 9)
            else fullName
        Fabel.Decorator(att.AttributeType.FullName, args) |> Some

    let rec private makeTypeEntity (ctx: Context) (tdef: FSharpEntity): Fabel.TypeEntity =
        ctx.entities.GetOrAdd (tdef.FullName, System.Func<string,Fabel.Entity>(fun _ ->
            let kind =
                if tdef.IsInterface then Fabel.Interface
                // elif tdef.IsFSharpExceptionDeclaration then Fabel.Exception
                elif tdef.IsFSharpRecord then Fabel.Record
                elif tdef.IsFSharpUnion then Fabel.Union
                else
                    let baseClass =
                        match tdef.BaseType with
                        | None -> None
                        | Some x when not x.HasTypeDefinition
                            || x.TypeDefinition.FullName = "System.Object" -> None
                        | Some x -> makeTypeEntity ctx x.TypeDefinition |> Some
                    Fabel.Class (baseClass)
            // Take only interfaces and attributes with internal declaration
            let infcs =
                tdef.DeclaredInterfaces
                |> Seq.filter (fun t -> t.HasTypeDefinition && ctx.IsInternal t.TypeDefinition)
                |> Seq.map (fun x -> sanitize x.TypeDefinition.FullName)
                |> Seq.toList
            let decs =
                tdef.Attributes |> Seq.choose (makeDecorator ctx) |> Seq.toList
            upcast Fabel.TypeEntity (kind, sanitize tdef.FullName, infcs, decs,
                        tdef.Accessibility.IsPublic, ctx.IsInternal tdef))
        ) :?> Fabel.TypeEntity

    let rec makeTypeFromDef (ctx: Context) (tdef: FSharpEntity) =
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
        match isNetPrimitive tdef.FullName with
        | Some typ -> Fabel.PrimitiveType typ
        | None -> makeTypeEntity ctx tdef |> Fabel.DeclaredType

    and makeType (ctx: Context) (NonAbbreviatedType t) =
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
                else makeTypeFromDef ctx t.TypeDefinition

    let (|FabelType|) = makeType
    
    let (|OptionUnion|ListUnion|ErasedUnion|OtherType|) (typ: Fabel.Type) =
        match typ with
        | Fabel.DeclaredType typ ->
            match typ.FullName with
            | "Microsoft.FSharp.Core.Option" -> OptionUnion
            | "Microsoft.FSharp.Collections.List" -> ListUnion
            | _ when Option.isSome (typ.HasDecoratorNamed "Erase") -> ErasedUnion
            | _ -> OtherType
        | _ -> OtherType

// end module Types

module private Identifiers =
    open Util
    open Types

    // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#Keywords
    let private keywordsJS =
        set["abstract"; "await"; "boolean"; "break"; "byte"; "case"; "catch"; "char"; "class"; "const"; "continue"; "debugger"; "default"; "delete"; "do"; "double";
            "else"; "enum"; "export"; "extends"; "false"; "final"; "finally"; "float"; "for"; "function"; "goto"; "if"; "implements"; "import"; "in"; "instanceof"; "int"; "interface";
            "let"; "long"; "native"; "new"; "null"; "package"; "private"; "protected"; "public"; "return"; "short"; "static"; "super"; "switch"; "synchronized";
            "this"; "throw"; "throws"; "transient"; "true"; "try"; "typeof"; "undefined"; "var"; "void"; "volatile"; "while"; "with"; "yield" ]

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
            keywordsJS.Contains sanitizedName
            |> function true -> "_" + sanitizedName | false -> sanitizedName
            // Check if it already exists in scope
            |> preventConflicts (fun x -> List.exists (fun (_,x') -> x = x') ctx.scope)
        { ctx with scope = (fsName, sanitizedName)::ctx.scope }, sanitizedName

    /// Make a sanitized identifier from a speculative name
    let makeSanitizedIdent ctx typ speculativeName =
        let _, sanitizedIdent = sanitizeIdent ctx speculativeName
        Fabel.IdentifierExpr (sanitizedIdent, typ)

    /// Get corresponding identifier to F# value in current scope
    let (|GetIdent|) (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        ctx.scope
        |> List.tryFind (fun (fsName,_) -> fsName = fsRef.DisplayName)
        |> function
        | Some (_,fabelName) -> Fabel.IdentifierExpr (fabelName, makeType ctx fsRef.FullType)
        | None -> failwithf "Detected non-bound identifier: %s in %A" fsRef.DisplayName fsRef.DeclarationLocation

    /// Sanitize F# identifier and create new context
    let (|BindIdent|) (ctx: Context) (fsRef: FSharpMemberOrFunctionOrValue) =
        let newContext, sanitizedIdent = sanitizeIdent ctx fsRef.DisplayName
        newContext, Fabel.IdentifierExpr (sanitizedIdent, makeType ctx fsRef.FullType)
// end module Identifiers

open Util
open Types
open Identifiers

let rec private (|Transform|) (ctx: Context) (fsExpr: FSharpExpr): Fabel.Expr =
    transform ctx fsExpr

and private transform ctx fsExpr =
    let makeExpr (ctx: Context) (fsExpr: FSharpExpr) kind =
        let (FabelType ctx typ, Range range) = fsExpr.Type, fsExpr.Range
        Fabel.Expr (kind, typ, range)

    let makeAstOnlyExpr kind = Fabel.Expr (kind, Fabel.UnknownType)

    let makeSequential statements =
        Fabel.Expr (Fabel.Sequential statements, (List.last statements).Type)

    let makeTypeRef typ =
        Fabel.Expr (Fabel.Value (Fabel.TypeRef typ), Fabel.UnknownType)
    
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

    let makeLiteral (value: obj) = makeConst value |> makeAstOnlyExpr
    
    let makeLambda ctx range vars body =
        let range = match range with Some (Range r) -> Some r | None -> None
        let newContext, args = List.foldBack (fun var (accContext, accArgs) ->
            let (BindIdent accContext (newContext, arg)) = var
            newContext, arg::accArgs) vars (ctx, [])
        let (Transform newContext body) = body
        let args, body =
            match body.Kind with
            | Fabel.Lambda (args', body, Fabel.Immediate, false) -> args@args', body
            | _ -> args, body
        let typ = Fabel.Function args.Length |> Fabel.PrimitiveType
        Fabel.Expr (Fabel.Lambda (args, body, Fabel.Immediate, false), typ, ?range=range)

    let makeTryCatch ctx fsExpr (Transform ctx body) catchClause finalBody =
        let catchClause =
            match catchClause with
            | Some (BindIdent ctx (catchContext, catchVar), catchBody) ->
                let (Transform catchContext catchBody) = catchBody
                Some (catchVar, catchBody)
            | None -> None
        let finalizers =
            match finalBody with
            | Some (Transform ctx finalBody) ->
                match finalBody.Kind with
                | Fabel.Sequential statements -> statements
                | _ -> [finalBody]
            | None -> []
        Fabel.TryCatch (body, catchClause, finalizers)
        |> makeExpr ctx fsExpr
        
    let makeCoreCall modName (methName: string) args =
        let meth = Fabel.Get (Fabel.Value (Fabel.CoreModule modName) |> makeAstOnlyExpr,
                    makeLiteral methName) |> makeAstOnlyExpr
        Fabel.Apply (meth, args, false)
        
    let makeTypeTest (typ: Fabel.Type) expr =
        let checkType expr (primitiveType: string) =
            Fabel.Binary (BinaryEqualStrict,
                Fabel.Unary (UnaryTypeof, expr) |> Fabel.Operation |> makeAstOnlyExpr,
                makeLiteral primitiveType) |> Fabel.Operation
        match typ with
        | Fabel.PrimitiveType kind ->
            match kind with
            | Fabel.String _ -> checkType expr "string"
            | Fabel.Number _ -> checkType expr "number"
            | Fabel.Boolean -> checkType expr "boolean"
            | Fabel.Unit -> Fabel.Binary (BinaryEqual, expr, Fabel.Value Fabel.Null |> makeAstOnlyExpr)
                            |> Fabel.Operation
            | _ -> failwithf "Unsupported type test in %A: %A" fsExpr.Range typ
        | Fabel.DeclaredType typEnt ->
            match typEnt.Kind with
            | Fabel.Interface -> makeCoreCall "Util" "hasInterface" [makeLiteral typEnt.FullName]
            | _ -> Fabel.Binary (BinaryInstanceOf, expr, makeTypeRef typ) |> Fabel.Operation
        | _ -> failwithf "Unsupported type test in %A: %A" fsExpr.Range typ

    let makeGetApply expr methName args =
        let expr = Fabel.Get (expr, makeLiteral methName) |> makeAstOnlyExpr
        Fabel.Apply (expr, args, false)

    let makeApply (expr: Fabel.Expr) (args: Fabel.Expr list) =
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
                apply expr (args @ (lambdaArgs |> List.map unbox)) |> makeAstOnlyExpr,
                Fabel.Immediate, false)
        // Flatten nested curried applications as it happens in pipelines: e.g., (fun x -> fnCall 2 x) 3
        | ExprKind (Fabel.Lambda (lambdaArgs, 
                        ExprKind (Fabel.Apply (nestedExpr, nestedArgs, _)),
                        Fabel.Immediate, false)) ->
            match nestedArgs with
            | ReplaceArgs (List.zip lambdaArgs args) nestedArgs -> apply nestedExpr nestedArgs
            | _ -> apply expr args
        | _ -> apply expr args
        
    let makeCall ctx (fsExpr: FSharpExpr) callee (meth: FSharpMemberOrFunctionOrValue) args = 
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
            ns + ent.DisplayName + methName + (overloadSuffix meth)
        let hasRestParams (meth: FSharpMemberOrFunctionOrValue) argExprs =
            if meth.CurriedParameterGroups.Count <> 1 then None else
            let args = meth.CurriedParameterGroups.[0]
            args |> Seq.iter (fun x ->
                if x.IsOutArg then failwithf "Out parameters are not supported: %s" meth.FullName)
            let lastIndex = args.Count - 1
            if lastIndex < 0 || not args.[lastIndex].IsParamArrayArg then None else
            let lastArgExpr = Seq.last argExprs
            match lastArgExpr with
            | BasicPatterns.NewArray (_, restArgs) -> Some (List.take lastIndex argExprs, restArgs)
            | _ -> failwithf "TODO: Spread when passing array ref to ParamArray arg: %A" fsExpr.Range
        (** ###Method call processing *)
        let methType, methFullName =
            let methType = makeTypeFromDef ctx meth.EnclosingEntity
            methType, sanitizeMethFullName meth
        let callee, args =
            callee |> Option.map (transform ctx),
            match hasRestParams meth args with
            | Some (args, restArgs) -> args@restArgs
            | None -> args
            |> List.map (transform ctx)
        (** -If this a pipe, optimize *)
        match methFullName with
        | "Microsoft.FSharp.Core.Operators.( |> )" -> makeApply args.Tail.Head [args.Head]
        | "Microsoft.FSharp.Core.Operators.( <| )" -> makeApply args.Head args.Tail
        | _ ->
            (** -If this is an internal method, check for Emit attributes *)
            (** -If this is an external method, check for replacements *)
            let resolved =
                match methType with
                | Fabel.DeclaredType typEnt when typEnt.IsInternal ->
                    match typEnt.HasDecoratorNamed "ASTEmit" with
                    | Some (Fabel.Decorator (_, [:? Emitter as emitter])) ->
                        match callee with Some a -> a::args | None -> args
                        |> emitter |> Some
                    | _ -> None
                | _ ->
                    match Replacements.tryReplace methFullName callee args with
                    | Some _ as repl -> repl 
                    | None -> failwithf "Couldn't find replacemente for external method %s" methFullName
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
                    | None, args -> Fabel.Value (Fabel.TypeRef methType) |> makeAstOnlyExpr, args
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
                
        |> makeExpr ctx fsExpr

    match fsExpr with
    (** ## Erased *)
    | BasicPatterns.Coerce(_targetType,  Transform ctx inpExpr) -> inpExpr
    | BasicPatterns.NewDelegate(_delegateType, Transform ctx delegateBodyExpr) -> delegateBodyExpr
    // TypeLambda is a local generic lambda
    // e.g, member x.Test() = let typeLambda x = x in typeLambda 1, typeLambda "A"
    | BasicPatterns.TypeLambda (_genArgs, Transform ctx lambda) -> lambda
        
    | BasicPatterns.ILAsm (_asmCode, _typeArgs, argExprs) ->        
        printfn "ILAsm detected in %A: %A" fsExpr.Range fsExpr // TODO: Check
        match argExprs with
        | [] -> Fabel.Value Fabel.Null |> makeExpr ctx fsExpr
        | [expr] -> transform ctx expr
        | exprs -> Fabel.Sequential (List.map (transform ctx) exprs) |> makeExpr ctx fsExpr
    
    (** ## Flow control *)
    | BasicPatterns.FastIntegerForLoop(Transform ctx start, Transform ctx limit, body, isUp) ->
        match body with
        | BasicPatterns.Lambda (BindIdent ctx (newContext, ident), body) ->
            Fabel.For (ident, start, limit, transform newContext body, isUp)
            |> Fabel.Loop |> makeExpr ctx fsExpr
        | _ -> failwithf "Unexpected loop in %A: %A" fsExpr.Range fsExpr
    
    | BasicPatterns.WhileLoop(Transform ctx guardExpr, Transform ctx bodyExpr) ->
        Fabel.While (guardExpr, bodyExpr)
        |> Fabel.Loop |> makeExpr ctx fsExpr
        
    // This must appear before BasicPatterns.Let
    | ForOf (BindIdent ctx (newContext, ident), Transform ctx value, body) ->
        Fabel.ForOf (ident, value, transform newContext body)
        |> Fabel.Loop |> makeExpr ctx fsExpr
    
    (** Values *)
    | BasicPatterns.Const(value, _typ) ->
        makeExpr ctx fsExpr (makeConst value)

    | BasicPatterns.BaseValue _type ->
        makeExpr ctx fsExpr (Fabel.Value Fabel.Super)

    | BasicPatterns.ThisValue _type ->
        makeExpr ctx fsExpr (Fabel.Value Fabel.This)

    | BasicPatterns.DefaultValue (FabelType ctx typ) ->
        let valueKind =
            match typ with
            | Fabel.PrimitiveType Fabel.Boolean -> Fabel.BoolConst false
            | Fabel.PrimitiveType (Fabel.Number _) -> Fabel.IntConst 0
            | _ -> Fabel.Null
        makeExpr ctx fsExpr (Fabel.Value valueKind)

    | BasicPatterns.Value(GetIdent ctx ident) ->
        upcast ident

    (** ## Assignments *)
    // TODO: Possible optimization if binding to another ident (let x = y), just replace it in the ctx
    | BasicPatterns.Let((BindIdent ctx (newContext, ident) as var, Transform ctx value), body) ->
        let (Transform newContext body) = body
        let assignment = Fabel.VarDeclaration (ident, value, var.IsMutable) |> makeExpr ctx fsExpr
        match body.Kind with
        // Check if this this is just a wrapper to a call as it happens in pipelines
        // e.g., let x = 5 in fun y -> methodCall x y
        | Fabel.Lambda (lambdaArgs,
                        (ExprKind (Fabel.Apply (eBody, ReplaceArgs [ident,value] args, isCons)) as e),
                        Fabel.Immediate, false) ->
            Fabel.Lambda (lambdaArgs,
                Fabel.Expr (Fabel.Apply (eBody, args, isCons), e.Type, ?range=e.Range),
                Fabel.Immediate, false) |> makeExpr ctx fsExpr
        | Fabel.Sequential statements ->
            makeSequential (assignment::statements)
        | _ -> makeSequential [assignment; body]

    | BasicPatterns.LetRec(recursiveBindings, body) ->
        let newContext, idents =
            recursiveBindings
            |> Seq.foldBack (fun (var, _) (accContext, accIdents) ->
                let (BindIdent accContext (newContext, ident)) = var
                newContext, (ident::accIdents)) <| (ctx, [])
        let assignments =
            recursiveBindings
            |> List.map2 (fun ident (var, Transform ctx binding) ->
                Fabel.VarDeclaration (ident, binding, var.IsMutable)
                |> makeExpr ctx fsExpr) idents
        let (Transform newContext body) = body
        match body.Kind with
        | Fabel.Sequential statements -> assignments @ statements
        | _ -> assignments @ [ body ]
        |> makeSequential

    (** Applications *)
    | BasicPatterns.TraitCall (_sourceTypes, traitName, _typeArgs, _typeInstantiation, argExprs) ->
        printfn "TraitCall detected in %A: %A" fsExpr.Range fsExpr // TODO: Check
        makeGetApply (transform ctx argExprs.Head) traitName
                     (List.map (transform ctx) argExprs.Tail)
        |> makeExpr ctx fsExpr
    
    // TODO: Check `inline` annotation?
    // TODO: Watch for restParam attribute
    | BasicPatterns.Call(callee, meth, _typeArgs1, _typeArgs2, args) ->
        makeCall ctx fsExpr callee meth args

    | BasicPatterns.Application(Transform ctx expr, _typeArgs, args) ->
        makeApply expr (List.map (transform ctx) args)
        |> makeExpr ctx fsExpr

    | BasicPatterns.IfThenElse (Transform ctx guardExpr, Transform ctx thenExpr, Transform ctx elseExpr) ->
        Fabel.IfThenElse (guardExpr, thenExpr, elseExpr)
        |> makeExpr ctx fsExpr

    | BasicPatterns.TryFinally (BasicPatterns.TryWith(body, _, _, catchVar, catchBody),finalBody) ->
        makeTryCatch ctx fsExpr body (Some (catchVar, catchBody)) (Some finalBody)

    | BasicPatterns.TryFinally (body, finalBody) ->
        makeTryCatch ctx fsExpr body None (Some finalBody)

    | BasicPatterns.TryWith (body, _, _, catchVar, catchBody) ->
        makeTryCatch ctx fsExpr body (Some (catchVar, catchBody)) None

    | BasicPatterns.Sequential (Transform ctx first, Transform ctx second) ->
        match first.Kind with
        | Fabel.Value (Fabel.Null) -> second
        | _ ->
            match second.Kind with
            | Fabel.Sequential statements -> first::statements
            | _ -> [first; second]
            |> makeSequential

    (** ## Lambdas *)
    | BasicPatterns.Lambda (var, body) ->
        makeLambda ctx (Some fsExpr.Range) [var] body

    (** ## Getters and Setters *)
    | BasicPatterns.ILFieldGet (callee, typ, fieldName) ->
        failwithf "Found unsupported ILField reference in %A: %A" fsExpr.Range fsExpr

    // TODO: Check if it's FSharpException
    // TODO: Change name of automatically generated fields
    | BasicPatterns.FSharpFieldGet (callee, FabelType ctx calleeType, FieldName fieldName) ->
        let callee =
            match callee with
            | Some (Transform ctx callee) -> callee
            | None -> makeTypeRef calleeType
        Fabel.Get (callee, makeLiteral fieldName)
        |> makeExpr ctx fsExpr

    | BasicPatterns.TupleGet (_tupleType, tupleElemIndex, Transform ctx tupleExpr) ->
        Fabel.Get (tupleExpr, makeLiteral tupleElemIndex)
        |> makeExpr ctx fsExpr

    // Single field: Item; Multiple fields: Item1, Item2...
    | BasicPatterns.UnionCaseGet (Transform ctx unionExpr, FabelType ctx unionType, unionCase, FieldName fieldName) ->
        match unionType with
        | ErasedUnion | OptionUnion -> unionExpr
        | ListUnion -> failwith "TODO"
        | OtherType ->
            Fabel.Get (unionExpr, makeLiteral fieldName)
            |> makeExpr ctx fsExpr

    | BasicPatterns.ILFieldSet (callee, typ, fieldName, value) ->
        failwithf "Found unsupported ILField reference in %A: %A" fsExpr.Range fsExpr
        
    // TODO: Change name of automatically generated fields
    | BasicPatterns.FSharpFieldSet (callee, FabelType ctx calleeType, FieldName fieldName, Transform ctx value) ->
        let callee =
            match callee with
            | Some (Transform ctx callee) -> callee
            | None -> makeTypeRef calleeType
        Fabel.Set (callee, Some (makeLiteral fieldName), value)
        |> makeExpr ctx fsExpr

    | BasicPatterns.UnionCaseTag (Transform ctx unionExpr, _unionType) ->
        Fabel.Get (unionExpr, makeLiteral "Tag")
        |> makeExpr ctx fsExpr

    // We don't need to check if this an erased union, as union case values are only set
    // in constructors, which are ignored for erased unions 
    | BasicPatterns.UnionCaseSet (Transform ctx unionExpr, _type, _case, FieldName caseField, Transform ctx valueExpr) ->
        Fabel.Set (unionExpr, Some (makeLiteral caseField), valueExpr)
        |> makeExpr ctx fsExpr

    | BasicPatterns.ValueSet (GetIdent ctx valToSet, Transform ctx valueExpr) ->
        Fabel.Set (valToSet, None, valueExpr)
        |> makeExpr ctx fsExpr
    
    (** Instantiation *)
    | BasicPatterns.NewArray(FabelType ctx typ, argExprs) ->
        match typ with
        | Fabel.PrimitiveType (Fabel.TypedArray numberKind) -> failwith "TODO: NewArray args"
        | _ -> Fabel.Value (Fabel.ArrayConst (argExprs |> List.map (transform ctx)))
        |> makeExpr ctx fsExpr
    
    | BasicPatterns.NewTuple(_, argExprs) ->
        Fabel.Value (Fabel.ArrayConst (argExprs |> List.map (transform ctx)))
        |> makeExpr ctx fsExpr
        
    | BasicPatterns.ObjectExpr(_objType, _baseCallExpr, _overrides, interfaceImplementations) ->
        failwith "TODO"

    // TODO: Unify with BasicPatterns.Call
    | BasicPatterns.NewObject(meth, _typeArgs, args) ->
        makeCall ctx fsExpr None meth args

    // TODO: Check if it's FSharpException
    // TODO: Create constructors for Record and Union types 
    | BasicPatterns.NewRecord(FabelType ctx recordType, argExprs) ->
        let argExprs = argExprs |> List.map (transform ctx)
        Fabel.Apply (makeTypeRef recordType, argExprs, true)
        |> makeExpr ctx fsExpr
            
    | BasicPatterns.NewUnionCase(FabelType ctx unionType, unionCase, argExprs) ->
        let argExprs = argExprs |> List.map (transform ctx)
        match unionType with
        | ErasedUnion | OptionUnion ->
            match argExprs with
            | [] -> Fabel.Value Fabel.Null |> makeExpr ctx fsExpr
            | [expr] -> expr
            | _ -> failwithf "Erased Union Cases must have one single field: %A" unionType
        | ListUnion ->
            match unionCase.Name with
            | "Cons" -> Fabel.Apply (Fabel.Value (Fabel.CoreModule "List") |> makeAstOnlyExpr,
                            (makeLiteral "Cons")::argExprs, true)
            | _ -> Fabel.Value Fabel.Null
            |> makeExpr ctx fsExpr
        | OtherType ->
            // Include Tag name in args
            let argExprs = (makeLiteral unionCase.Name)::argExprs
            Fabel.Apply (makeTypeRef unionType, argExprs, true)
            |> makeExpr ctx fsExpr

    (** ## Type test *)
    | BasicPatterns.TypeTest (FabelType ctx typ as fsTyp, Transform ctx expr) ->
        makeTypeTest typ expr |> makeExpr ctx fsExpr
           
    | BasicPatterns.UnionCaseTest (Transform ctx unionExpr, FabelType ctx unionType, unionCase) ->
        match unionType with
        | ErasedUnion ->
            if unionCase.UnionCaseFields.Count <> 1 then
                failwithf "Erased Union Cases must have one single field: %A" unionType
            else
                let typ = makeType ctx unionCase.UnionCaseFields.[0].FieldType
                makeTypeTest typ unionExpr
        | OptionUnion | ListUnion ->
            let op = if (unionCase.Name = "None" || unionCase.Name = "Empty") then BinaryEqual else BinaryUnequal
            Fabel.Binary (op, unionExpr, Fabel.Value Fabel.Null |> makeAstOnlyExpr)
            |> Fabel.Operation
        | OtherType ->
            Fabel.Binary (BinaryEqualStrict,
                Fabel.Get (unionExpr, makeLiteral "Tag") |> makeAstOnlyExpr,
                makeLiteral unionCase.Name) |> Fabel.Operation
        |> makeExpr ctx fsExpr

    (** Pattern Matching *)
    | BasicPatterns.DecisionTreeSuccess (decIndex, decBindings) ->
        match Map.tryFind decIndex ctx.decisionTargets with
        | None -> failwith "Missing decision target"
        // If we get a reference to a function, call it
        | Some (TargetRef targetRef) ->
            Fabel.Apply (targetRef, (decBindings |> List.map (transform ctx)), false)
            |> makeExpr ctx fsExpr
        // If we get an implementation without bindings, just transform it
        | Some (TargetImpl ([], decBody)) ->
            transform ctx decBody
        // If we have bindings, create the assignments
        | Some (TargetImpl (decVars, decBody)) ->
            let newContext, assignments =
                List.foldBack2 (fun var (Transform ctx binding) (accContext, accAssignments) ->
                    let (BindIdent accContext (newContext, ident)) = var
                    let assignment =
                        Fabel.VarDeclaration (ident, binding, var.IsMutable)
                        |> makeAstOnlyExpr
                    newContext, (assignment::accAssignments)) decVars decBindings (ctx, [])
            let (Transform newContext decBody) = decBody
            match decBody.Kind with
            | Fabel.Sequential statements -> assignments @ statements
            | _ -> assignments @ [ decBody ]
            |> makeSequential

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
        let assignments =
            targetRefsCount
            |> Map.filter (fun k v -> v > 1)
            |> Map.fold (fun acc k v ->
                let decTargetVars, decTargetExpr = List.item k decisionTargets
                let lambda = makeLambda ctx None decTargetVars decTargetExpr
                let ident = makeSanitizedIdent ctx lambda.Type (sprintf "$target%i" k)
                Map.add k (ident, lambda) acc) (Map.empty<_,_>)
        let decisionTargets =
            targetRefsCount |> Map.map (fun k v ->
                match v with
                | 1 -> TargetImpl (List.item k decisionTargets)
                | _ -> TargetRef (fst assignments.[k]))        
        let newContext = { ctx with decisionTargets = decisionTargets }
        if assignments.Count = 0 then
            transform newContext decisionExpr
        else
            let assignments =
                assignments
                |> Seq.map (fun pair -> pair.Value)
                |> Seq.map (fun (ident, lambda) -> Fabel.VarDeclaration (ident, lambda, false))
                |> Seq.map makeAstOnlyExpr
                |> Seq.toList
            Fabel.Sequential (assignments @ [transform newContext decisionExpr])
            |> makeExpr ctx fsExpr

    (** Not implemented *)
    | BasicPatterns.Quote _ // (quotedExpr)
    | BasicPatterns.AddressOf _ // (lvalueExpr)
    | BasicPatterns.AddressSet _ // (lvalueExpr, rvalueExpr)
    | _ -> failwithf "Cannot compile expression in %A: %A" fsExpr.Range fsExpr

