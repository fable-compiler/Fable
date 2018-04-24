module InjectProcessor

open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns
open Fable

/// ATTENTION: Make sure this is the same as Fable.Transforms.Replacements.Helpers.getMangledName
let getMangledName (entityName: string) isStatic memberCompiledName =
    entityName + (Naming.getMemberMangledNameSeparator isStatic) + (Naming.sanitizeIdentForbiddenChars memberCompiledName)

let parse (checker: FSharpChecker) projFile =
    let projFile = Path.GetFullPath(projFile)
    let options =
        match Path.GetExtension(projFile) with
        | ".fsx" ->
            let projCode = File.ReadAllText projFile
            checker.GetProjectOptionsFromScript(projFile, projCode)
            |> Async.RunSynchronously
            |> fst
        | ".fsproj" ->
            let opts, _, _ = Fable.CLI.ProjectCoreCracker.GetProjectOptionsFromProjectFile(projFile)
            opts
        | ext -> failwithf "Unexpected extension: %s" ext
    // for f in options.OtherOptions do
    //     printfn "%s" f
    options
    |> checker.ParseAndCheckProject
    |> Async.RunSynchronously


let (|InjectAttribute|_|) (arg: FSharpParameter) =
    arg.Attributes |> Seq.tryPick (fun att ->
        match att.AttributeType.TryFullName with
        | Some "Fable.Core.InjectAttribute" when arg.Type.HasTypeDefinition ->
            match arg.Type.TypeDefinition.TryFullName, Seq.toList arg.Type.GenericArguments with
            | Some typeArgName, [genArg] ->
                Some(typeArgName, genArg.GenericParameter.Name)
            | _ -> None
        | _ -> None)

let rec getInjects initialized decls =
    seq {
        for decl in decls do
            match decl with
            | FSharpImplementationFileDeclaration.Entity(_, sub) ->
                // If the entity contains multiple declarations it must be the root module
                if not initialized then
                    yield! getInjects (List.isMultiple sub) sub
            | FSharpImplementationFileDeclaration.InitAction _ -> ()
            | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(memb, _, _) ->
                match Seq.concat memb.CurriedParameterGroups |> Seq.tryLast with
                | Some(InjectAttribute(typeArgName, genArg)) ->
                    let membName =
                        match memb.DeclaringEntity with
                        | Some ent when not ent.IsFSharpModule ->
                            getMangledName ent.CompiledName (not memb.IsInstanceMember) memb.CompiledName
                        | _ -> memb.CompiledName
                    // let index =
                    //     (memb.CurriedParameterGroups |> Seq.sumBy (fun g -> g.Count)) - 1
                    yield membName, typeArgName, genArg
                    ()
                | _ -> ()
        }

[<EntryPoint>]
let main _argv =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let proj = parse checker (IO.Path.Combine(__SOURCE_DIRECTORY__,"../../js/fable-core/Fable.Core.JS.fsproj"))
    let lines = ResizeArray()
    lines.Add("/// AUTOMATICALLY GENERATED - DO NOT TOUCH!")
    lines.Add("module Fable.Transforms.Inject")
    lines.Add("")
    for file in proj.AssemblyContents.ImplementationFiles do
        let injects = getInjects false file.Declarations |> Seq.toArray
        if Array.length injects > 0 then
            let fileName = System.IO.Path.GetFileNameWithoutExtension(file.FileName)
            lines.Add("let " + fileName + " =")
            lines.Add("  Map [")
            for (membName, typeArgName, genArg) in injects do
                lines.Add(sprintf "    \"%s\", (\"%s\", \"%s\")" membName typeArgName genArg)
            lines.Add("]")
            lines.Add("")
    File.WriteAllLines(IO.Path.Combine(__SOURCE_DIRECTORY__,"../../dotnet/Fable.Compiler/Transforms/Inject.fs"), lines)
    printfn "Finished!"
    0
