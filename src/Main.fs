module Fabel.Main

open System
open System.IO
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices

type Decl (decl: FSharpImplementationFileDeclaration) =
    member x.Self = decl
    member x.Declarations = decl |> function
        | FSharpImplementationFileDeclaration.Entity (e, sub) -> sub
        | _ -> failwith "Unexpected declaration"
    member x.Entity = decl |> function
        | FSharpImplementationFileDeclaration.Entity (e, sub) -> e
        | _ -> failwith "Unexpected declaration"
    member x.Expr = decl |> function
        | FSharpImplementationFileDeclaration.InitAction e -> e
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (v, args, body) -> body
        | _ -> failwith "Unexpected declaration"
    member x.Value = decl |> function
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (v, args, body) -> v
        | _ -> failwith "Unexpected declaration"
    member x.Args = decl |> function
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (v, args, body) -> args
        | _ -> failwith "Unexpected declaration"

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
        
let rec visitPath (path: string) (decls: FSharpImplementationFileDeclaration list) =
    let paths = path.Split ('/') |> Array.toList
    decls |> List.tryPick (fun decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity (e, sub)
            when e.DisplayName = paths.Head -> Some (Decl decl, true)
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue (v, args, body)
            when v.DisplayName = paths.Head -> Some (Decl decl, false)
        | _ -> None)
    |> function
        | None -> None
        | Some (decl, isEnt) when not isEnt || paths.Length = 1 -> Some decl
        | Some (decl, _) -> visitPath (paths.Tail |> String.concat "/") decl.Declarations        

let parseFSharpScript (mainFilePath: string) =
    let checker = FSharpChecker.Create(keepAssemblyContents=true)
    let projOptions =
        let code = File.ReadAllText mainFilePath
        checker.GetProjectOptionsFromScript(mainFilePath, code, otherFlags=[|"--define:DEBUG"|])
        |> Async.RunSynchronously
    checker.ParseAndCheckProject(projOptions)
    |> Async.RunSynchronously
    // TODO: Check errors

[<EntryPoint>]
let main argv =
    try
        let proj = parseFSharpScript argv.[0]
        let filenames =
            proj.AssemblyContents.ImplementationFiles
            |> List.map (fun x -> x.FileName)
        let actionDecl =
            proj.AssemblyContents.ImplementationFiles.[0].Declarations.[0] |> Decl
            |> fun modDecl -> modDecl.Declarations.[0] |> Decl
        let expr =
            Transform.FSharp2Fabel.transformTest filenames actionDecl.Expr
            |> Transform.Fabel2Babel.transformTest
        Newtonsoft.Json.JsonConvert.SerializeObject (expr, Fabel.Util.Json.converters)
        |> fun json -> File.WriteAllText("./Program.json", json)
    with e ->
        printfn "ERROR: %s" e.Message
    0 // return an integer exit code
