module ASTViewer

open System
open System.IO
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

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

let rec printDecls prefix decls =
    decls |> Seq.iteri (fun i decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity (e, sub) ->
            printfn "%s%i) ENTITY: %s" prefix i e.DisplayName
            printDecls (prefix + "\t") sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
            if meth.IsCompilerGenerated |> not then
                printfn "%s%i) METHOD: %s" prefix i meth.FullName
                match body with
                | BasicPatterns.Call(_,m,_,_,_)
                | BasicPatterns.NewObject(m,_,_) ->
                    printfn "%s CALL to %s (const %b, implicit %b)"
                        prefix m.FullName m.IsConstructor m.IsImplicitConstructor
                | _ -> printfn "%A" body
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            printfn "%s%i) ACTION" prefix i
            printfn "%A" expr
        )

and lookup f (expr: FSharpExpr) =
    f expr
    List.iter (lookup f) expr.ImmediateSubExpressions

[<EntryPoint>]
let main argv =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let proj = parse checker argv.[0]
    // proj.AssemblyContents.ImplementationFiles
    // |> Seq.iteri (fun i file -> printfn "%i) %s" i file.FileName)
    let lastFile = List.last proj.AssemblyContents.ImplementationFiles
    printDecls "" lastFile.Declarations
    0
