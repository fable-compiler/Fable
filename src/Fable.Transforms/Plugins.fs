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
            match dllFiles.Length with
            | 0 -> failwithf "Cannot find assembly %s for in %s" entFullName binDir
            | 1 -> dllFiles.[0]
            // Try to disambiguate
            | _ ->
                dllFiles |> Array.tryFind (fun dllFile ->
                    entFullName.StartsWith(Path.GetFileNameWithoutExtension(dllFile)))
                |> function Some x -> x
                          | None -> failwithf "Cannot pick correct assembly for %s in %s" entFullName binDir
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

type TransformInfo = {
    PluginSourcePath: string
    Compiler: ICompiler
}

let transformIdent (ident: Ident) =
    match ident with
    | :? Fable.Ident as i -> i
    // TODO: Check if name is unique?
    | _ -> makeIdentNonMangled ident.CompiledName

let rec makeArgInfo info argExprs: Fable.ArgInfo =
    { ThisArg = None
      Args = List.map (transformExpr info) argExprs
      SignatureArgTypes = Fable.NoUncurrying
      Spread = Fable.NoSpread
      IsBaseOrSelfConstructorCall = false }

and transformApply info withNew expr argExprs =
    let expr = transformExpr info expr
    let callKind = if withNew then Fable.ConstructorCall expr else Fable.StaticCall expr
    let argInfo = makeArgInfo info argExprs
    let opKind = Fable.Call(callKind, argInfo)
    Fable.Operation(opKind, Fable.Any, None)

and transformFunction info name args body =
    let args = List.map transformIdent args
    Fable.Function(Fable.Delegate args, transformExpr info body, name)

and transformConst info (c: SimpleConstant) =
    match c with
    | NullConst -> Fable.Value(Fable.Null Fable.Any, None)
    | UndefinedConst -> Fable.Value(Fable.NewOption(None, Fable.Any), None)
    | BooleanConst x -> makeBoolConst x
    | NumberConst x -> makeFloatConst x
    | StringConst x -> makeStrConst x
    | ArrayConst xs ->
        let xs = List.map (transformExpr info) xs
        Fable.Value(Fable.NewArray(Fable.ArrayValues xs, Fable.Any), None)
    | RegexConst(source, flags) ->
        let flags = flags |> List.map (function
            | 'g' -> RegexGlobal
            | 'i' -> RegexIgnoreCase
            | 'm' -> RegexMultiline
            | 'y' -> RegexSticky
            | f -> failwithf "Unknown regex flag %O" f)
        Fable.Value(Fable.RegexConstant(source, flags), None)

and transformExpr (info: TransformInfo) (expr: Expr) =
    match expr with
    | :? Fable.Expr as e -> e
    | :? SimpleExpr as e ->
        match e with
        | Constant c -> transformConst info c
        | IdentExpr i -> transformIdent i |> Fable.IdentExpr
        | Import(selector, path) ->
            let path =
                if path = "." then info.PluginSourcePath
                else
                    lazy info.PluginSourcePath
                    |> fixImportedRelativePath info.Compiler path
            makeCustomImport Fable.Any selector path
        | Function(args, body) -> transformFunction info None args body
        | ObjectExpr(keyValueParis) ->
            keyValueParis
            |> List.map (fun (k, v) -> k, transformExpr info v)
            |> objExpr Fable.Any
        | Apply(expr, argExprs) -> transformApply info false expr argExprs
        | ApplyNew(expr, argExprs) -> transformApply info true expr argExprs
        | Let(var, value, body) ->
            Fable.Let([transformIdent var, transformExpr info value], transformExpr info body)
        | GetField(expr, key) ->
            let kind = makeStrConst key |> Fable.ExprGet
            Fable.Get(transformExpr info expr, kind, Fable.Any, None)
        | GetIndex(expr, index) ->
            let kind = makeIntConst index |> Fable.ExprGet
            Fable.Get(transformExpr info expr, kind, Fable.Any, None)
        | Set(expr, value) ->
            Fable.Set(transformExpr info expr, Fable.VarSet, transformExpr info value, None)
        | Sequential exprs ->
            Fable.Sequential(List.map (transformExpr info) exprs)
        | EmitJs(macro, argExprs) ->
            Fable.Operation(Fable.Emit(macro, Some(makeArgInfo info argExprs)), Fable.Any, None)
    | _ -> failwithf "Unexpected SimpleAst.Expr: %A" expr

let transformDeclaration (plugin: TransformDeclaration) (sourcePath: string)
                         (com: ICompiler) (decl: Fable.Declaration)
                         : Fable.Declaration =
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
            let info = { PluginSourcePath = sourcePath; Compiler = com }
            match transformed.Args with
            | None -> transformExpr info transformed.Body
            | Some args -> transformFunction info (Some transformed.CompiledName) args transformed.Body

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
            let sourcePath = FSharp.getEntityFile att.AttributeType
            { new TransformDeclaration with
                member _.TransformDeclaration(com, decl) =
                    transformDeclaration plugin sourcePath com decl })
    else None
