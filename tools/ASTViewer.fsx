#r "../packages/FSharp.Compiler.Service/lib/net45/FSharp.Compiler.Service.dll"
open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parse projFile =
    let projCode = File.ReadAllText projFile
    checker.GetProjectOptionsFromScript(projFile, projCode)
    |> Async.RunSynchronously
    |> checker.ParseAndCheckProject
    |> Async.RunSynchronously
    
let rec printDecls prefix decls =
    decls |> Seq.iteri (fun i decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity (e, sub) ->
            printfn "%s%i) %s" prefix i e.DisplayName
            printDecls (prefix + "\t") sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (meth, args, body) ->
            printfn "%s%i) %s" prefix i meth.DisplayName
            printfn "%A" body
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            printfn "%s%i) ACTION" prefix i
            printfn "%A" expr)
            
let proj = parse "temp/Test.fsx"
proj.AssemblyContents.ImplementationFiles.[0].Declarations
|> printDecls ""
