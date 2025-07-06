module Fable.Transforms.Python.Compiler

open System.Collections.Generic

open Fable
open Fable.AST

open Fable.Transforms
open Fable.Transforms.Python.AST
open Fable.Transforms.Python.Types
open Fable.Transforms.Python.Util
open Fable.Transforms.Python.Transforms

type PythonCompiler(com: Compiler) =
    let onlyOnceWarnings = HashSet<string>()
    let imports = Dictionary<string, Import>()
    let exports: HashSet<string> = HashSet()
    let typeVars: HashSet<string> = HashSet()

    interface IPythonCompiler with
        member _.WarnOnlyOnce(msg, ?range) =
            if onlyOnceWarnings.Add(msg) then
                addWarning com [] range msg

        member _.GetImportExpr(ctx, moduleName, ?name, ?r) =
            // printfn "GetImportExpr: %A" (moduleName, name)
            let name =
                match name with
                | None
                | Some null -> ""
                | Some name -> name.Trim()

            let isQualifiedPythonImport =
                match name with
                | ""
                | "default"
                | "*" -> false
                | _ -> true

            let cachedName =
                moduleName
                + "::"
                + if isQualifiedPythonImport then
                      name
                  else
                      ""

            match imports.TryGetValue(cachedName) with
            | true, i -> i.LocalIdent |> Expression.identifier
            | false, _ ->
                let local_id =
                    if isQualifiedPythonImport then
                        Some name
                    else
                        None
                    |> getIdentForImport ctx moduleName

                let importName =
                    match name with
                    | _ when not isQualifiedPythonImport -> None
                    | Naming.placeholder ->
                        "`importMember` must be assigned to a variable" |> addError com [] r
                        Some name
                    | _ -> Some name

                let i =
                    {
                        Name = importName
                        Module = moduleName
                        LocalIdent = local_id
                    }

                imports.Add(cachedName, i)

                // If the import member is empty we understand this is done for side-effects only
                if name = "" then
                    Expression.none
                else
                    Expression.identifier local_id

        member _.GetAllImports() =
            // printfn "GetAllImports: %A" imports
            imports.Values :> Import seq |> List.ofSeq

        member _.GetAllTypeVars() = typeVars
        member _.GetAllExports() = exports

        member _.AddTypeVar(ctx, name: string) =
            // For Python 3.12, use clean type parameter names (no underscores)
            let name = name.ToUpperInvariant() |> Helpers.clean

            // Don't add to global typeVars collection since Python 3.12 uses scoped type parameters
            ctx.UsedNames.DeclarationScopes.Add(name) |> ignore

            Expression.name name

        member _.AddExport(name: string) =
            exports.Add name |> ignore
            Expression.name name

        member bcom.TransformAsExpr(ctx, e) = transformAsExpr bcom ctx e

        member bcom.TransformAsStatements(ctx, ret, e) = transformAsStatements bcom ctx ret e

        member bcom.TransformFunction(ctx, name, args, body, generics) =
            transformFunction bcom ctx name args body generics

        member bcom.TransformImport(ctx, selector, path) =
            transformImport bcom ctx None selector path

        member bcom.GetIdentifier(ctx, name) = Util.getIdentifier bcom ctx name

        member bcom.GetIdentifierAsExpr(ctx, name) =
            Util.getIdentifier bcom ctx name |> Expression.name

    interface Compiler with
        member _.Options = com.Options
        member _.Plugins = com.Plugins
        member _.LibraryDir = com.LibraryDir
        member _.CurrentFile = com.CurrentFile
        member _.OutputDir = com.OutputDir
        member _.OutputType = com.OutputType
        member _.ProjectFile = com.ProjectFile
        member _.SourceFiles = com.SourceFiles
        member _.IncrementCounter() = com.IncrementCounter()

        member _.IsPrecompilingInlineFunction = com.IsPrecompilingInlineFunction

        member _.WillPrecompileInlineFunction(file) = com.WillPrecompileInlineFunction(file)

        member _.GetImplementationFile(fileName) = com.GetImplementationFile(fileName)

        member _.GetRootModule(fileName) = com.GetRootModule(fileName)
        member _.TryGetEntity(fullName) = com.TryGetEntity(fullName)
        member _.GetInlineExpr(fullName) = com.GetInlineExpr(fullName)

        member _.AddWatchDependency(fileName) = com.AddWatchDependency(fileName)

        member _.AddLog(msg, severity, ?range, ?fileName: string, ?tag: string) =
            com.AddLog(msg, severity, ?range = range, ?fileName = fileName, ?tag = tag)

let makeCompiler com = PythonCompiler(com)

let transformFile (com: Compiler) (file: Fable.File) =
    let com = makeCompiler com :> IPythonCompiler

    transformFile com file
