#r "../../packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
#r "../../packages/FSharp.Compiler.Service.ProjectCracker/lib/net45/FSharp.Compiler.Service.ProjectCracker.dll"

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parse projFile =
    let options =
        match Path.GetExtension(projFile) with
        | ".fsx" ->
            let projCode = File.ReadAllText projFile
            checker.GetProjectOptionsFromScript(projFile, projCode)
            |> Async.RunSynchronously
        | ".fsproj" ->
            ProjectCracker.GetProjectOptionsFromProjectFile(Path.GetFullPath projFile)
        | ext -> failwithf "Unexpected extension: %s" ext
    options
    |> checker.ParseAndCheckProject
    |> Async.RunSynchronously
    
let rec printDecls prefix decls =
    decls |> Seq.iteri (fun i decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity (e, sub) ->
            printfn "%s%i) ENTITY: %s" prefix i e.DisplayName
            printDecls (prefix + "\t") sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
            if meth.IsCompilerGenerated |> not then
                printfn "%s%i) METHOD: %s" prefix i meth.DisplayName
                // match body with
                // | NewDelegate(_, Lambda(arg1, Lambda(arg2, _))) ->
                //     printfn "arg1 = arg2 %A" (arg1.IsCompilerGenerated)
                // | _ -> ()
                printfn "%A" body
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            printfn "%s%i) ACTION" prefix i
            printfn "%A" expr
        )

let proj = parse "temp/Test.fsx"
proj.AssemblyContents.ImplementationFiles
|> Seq.iteri (fun i file -> printfn "%i) %s" i file.FileName)
proj.AssemblyContents.ImplementationFiles.[0].Declarations
|> printDecls ""
