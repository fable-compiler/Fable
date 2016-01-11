module Fabel.Main

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

let rec visitDecl (decl: FSharpImplementationFileDeclaration) =
    match decl with
    | FSharpImplementationFileDeclaration.InitAction e ->
        printfn "ACTION: %A" e
    | FSharpImplementationFileDeclaration.Entity (e, sub) ->
        printfn "ENTITY: %A" e
        sub |> List.iter visitDecl 
    | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (v, args, body) ->
        if v.IsCompilerGenerated then () else
        printfn "MEMBER: %A" v
        printfn "ARGS: %A" args
        printfn "BODY: %A" body

let parseFSharpProject (mainFilePath: string) =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let projOptions =
        let code = File.ReadAllText mainFilePath
        checker.GetProjectOptionsFromScript(mainFilePath, code, otherFlags=[|"--define:DEBUG"|])
        |> Async.RunSynchronously
    let checkProjectResults =
        checker.ParseAndCheckProject(projOptions)
        |> Async.RunSynchronously
    // TODO: Check errors
    checkProjectResults.AssemblyContents.ImplementationFiles
    |> List.iter (fun file -> file.Declarations |> List.iter visitDecl)

[<EntryPoint>]
let main argv =
    parseFSharpProject argv.[0]
    0 // return an integer exit code
