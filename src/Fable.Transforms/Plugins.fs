module Fable.Transforms.Plugins

open System
open FSharp.Compiler.SourceCodeServices
open Fable
open Fable.AST
open Fable.SimpleAst

let private typeCache = ConcurrentDictionary<string, Type option>()

// TODO: Compiler warning if type cannot be found
let findType (ent: FSharpEntity) entFullName: Type option =
#if FABLE_COMPILER
    None // TODO
#else
    let rec findDll dir =
        let binDir = IO.Path.Combine(dir, "bin")
        if IO.Directory.Exists(binDir) then
            let dllFiles = IO.Directory.GetFiles(binDir, "*.dll")
            if dllFiles.Length <> 1 then
                failwithf "Expecting one .dll file in %s" binDir
            dllFiles.[0]
        else
            let parent = IO.Directory.GetParent(dir)
            if isNull parent then
                failwith "Couldn't find dll"
            findDll parent.FullName

    /// Prevent ReflectionTypeLoadException
    /// From http://stackoverflow.com/a/7889272
    let getTypes (asm: System.Reflection.Assembly) =
        let mutable types: Option<Type[]> = None
        try
            types <- Some(asm.GetTypes())
        with
        | :? System.Reflection.ReflectionTypeLoadException as e ->
            types <- Some e.Types
        match types with
        | None -> Seq.empty
        | Some types ->
            types |> Seq.filter ((<>) null)
//            |> Seq.collect (fun t -> t.GetNestedTypes() |> Array.append [|t|])

    let assemblyPath =
        match ent.Assembly.FileName with
        | Some path -> path
        | None ->
            FSharp.getEntityFile ent
            |> IO.Path.GetDirectoryName
            |> findDll
    // The assembly may be already loaded, so use `LoadFrom` which takes
    // the copy in memory unlike `LoadFile`, see: http://stackoverflow.com/a/1477899
    System.Reflection.Assembly.LoadFrom(assemblyPath)
    |> getTypes
    // Normalize type name
    |> Seq.tryFind (fun t -> t.FullName.Replace("+", ".") = entFullName)
#endif

let activateType (ent: FSharpEntity) (consArgs: obj seq): 'T option =
    match ent.TryFullName with
    | None -> None
    | Some entFullName ->
        // TODO: Can it happen that we have two attributes implementing TransformDeclaration with same full name?
        typeCache.GetOrAdd(entFullName, fun _ -> findType ent entFullName)
        |> Option.map (fun t -> Activator.CreateInstance(t, Seq.toArray consArgs) :?> 'T)

let transformIdent (ident: Ident) =
    match ident with
    | :? Fable.Ident as i -> i
    // TODO: Check if name is unique?
    | _ -> makeIdentNonMangled ident.CompiledName

let rec makeArgInfo argExprs: Fable.ArgInfo =
    { ThisArg = None
      Args = List.map transformExpr argExprs
      SignatureArgTypes = Fable.NoUncurrying
      Spread = Fable.NoSpread
      IsBaseOrSelfConstructorCall = false }

and transformApply withNew expr argExprs =
    let expr = transformExpr expr
    let callKind = if withNew then Fable.ConstructorCall expr else Fable.StaticCall expr
    let argInfo = makeArgInfo argExprs
    let opKind = Fable.Call(callKind, argInfo)
    Fable.Operation(opKind, Fable.Any, None)

and transformFunction name args body =
    let args = List.map transformIdent args
    Fable.Function(Fable.Delegate args, transformExpr body, name)

and transformConst (c: SimpleConstant) =
    match c with
    | NullConst -> Fable.Value(Fable.Null Fable.Any, None)
    | UndefinedConst -> Fable.Value(Fable.NewOption(None, Fable.Any), None)
    | BooleanConst x -> makeBoolConst x
    | NumberConst x -> makeFloatConst x
    | StringConst x -> makeStrConst x
    | ArrayConst xs ->
        let xs = List.map transformExpr xs
        Fable.Value(Fable.NewArray(Fable.ArrayValues xs, Fable.Any), None)
    | RegexConst(source, flags) ->
        let flags = flags |> List.map (function
            | 'g' -> RegexGlobal
            | 'i' -> RegexIgnoreCase
            | 'm' -> RegexMultiline
            | 'y' -> RegexSticky
            | f -> failwithf "Unknown regex flag %O" f)
        Fable.Value(Fable.RegexConstant(source, flags), None)

and transformExpr (expr: Expr) =
    match expr with
    | :? Fable.Expr as e -> e
    | :? SimpleExpr as e ->
        match e with
        | Constant c -> transformConst c
        | IdentExpr i -> transformIdent i |> Fable.IdentExpr
        // TODO: Check if path is "." (referencing self file)
        | Import(selector, path) -> makeCustomImport Fable.Any selector path
        | Function(args, body) -> transformFunction None args body
        | ObjectExpr(keyValueParis) ->
            keyValueParis
            |> List.map (fun (k, v) -> k, transformExpr v)
            |> makeObjExpr Fable.Any
        | Apply(expr, argExprs) -> transformApply false expr argExprs
        | ApplyNew(expr, argExprs) -> transformApply true expr argExprs
        | Let(var, value, body) ->
            Fable.Let([transformIdent var, transformExpr value], transformExpr body)
        | GetField(expr, key) ->
            let kind = makeStrConst key |> Fable.ExprGet
            Fable.Get(transformExpr expr, kind, Fable.Any, None)
        | GetIndex(expr, index) ->
            let kind = makeIntConst index |> Fable.ExprGet
            Fable.Get(transformExpr expr, kind, Fable.Any, None)
        | Set(expr, value) ->
            Fable.Set(transformExpr expr, Fable.VarSet, transformExpr value, None)
        | Sequential exprs ->
            Fable.Sequential(List.map transformExpr exprs)
        | EmitJs(macro, argExprs) ->
            Fable.Operation(Fable.Emit(macro, Some(makeArgInfo argExprs)), Fable.Any, None)
    | _ -> failwithf "Unexpected SimpleAst.Expr: %A" expr

let transformDeclaration (plugin: TransformDeclaration) (com: ICompiler) (decl: Fable.Declaration): Fable.Declaration =
    match decl with
    | Fable.ValueDeclaration(expr, info) ->
        let args, body =
            match expr with
            | Fable.Function(Fable.Delegate args, body, _) ->
                Some(args |> List.map (fun x -> x :> Ident)), body
            | _ -> None, expr

        let enclosingFullDisplayName =
            info.EnclosingEntity
            |> Option.bind (fun ent -> ent.TryGetFullDisplayName())
            |> Option.map (Naming.unsafeReplaceIdentForbiddenChars '_')

        let displayName =
            info.Range
            |> Option.bind (fun r -> r.identifierName)

        let simpleDeclaration =
            { new Declaration with
                member _.Args = args
                member _.Body = body :> _
                member _.CompiledName = info.Name
                member _.DisplayName = defaultArg displayName info.Name
                member _.FullDisplayName =
                    match enclosingFullDisplayName, displayName with
                    | Some n1, Some n2 -> n1 + "_" + n2
                    | None, Some n2 -> n2
                    | _ -> info.Name }

        let helper =
            { new Helper with
                member _.GetUniqueVar(name) = com.GetUniqueVar(name)
                member _.LogError(msg) = addError com [] info.Range msg
                member _.LogWarning(msg) = addWarning com [] info.Range msg }

        let transformed = plugin.TransformDeclaration(helper, simpleDeclaration)

        let expr =
            match transformed.Args with
            | None -> transformExpr transformed.Body
            | Some args -> transformFunction (Some transformed.CompiledName) args transformed.Body

        Fable.ValueDeclaration(expr, info)

    // Do nothing. TODO: Show compiler warning
    | _ -> decl

let transformDeclarationInterfaceFullName = typeof<TransformDeclaration>.FullName

type TransformDeclaration =
    abstract TransformDeclaration: ICompiler * Fable.Declaration -> Fable.Declaration

let tryTransformDeclarationAtt (att: FSharpAttribute) =
    let implementsTransformDeclarationInterface =
        // TODO: Check also parent interfaces?
        att.AttributeType.DeclaredInterfaces |> Seq.exists (fun ifc ->
            if ifc.HasTypeDefinition then
                match ifc.TypeDefinition.TryFullName with
                | Some fullName -> fullName = transformDeclarationInterfaceFullName
                | None -> false
            else false)
    if implementsTransformDeclarationInterface then
        Seq.map snd att.ConstructorArguments
        |> activateType att.AttributeType
        |> Option.map (fun plugin ->
            { new TransformDeclaration with
                member _.TransformDeclaration(com, decl) =
                    transformDeclaration plugin com decl })
    else None
