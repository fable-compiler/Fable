#r "/Users/alfonsogarciacaronunez/.nuget/packages/FSharp.Compiler.Service/17.0.1/lib/net45/FSharp.Compiler.Service.dll"

open System
open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.SourceCodeServices.BasicPatterns

let (|NonAbbreviatedType|) (t: FSharpType) =
    let rec abbr (t: FSharpType) =
        if t.IsAbbreviation then abbr t.AbbreviatedType else t
    abbr t

let (|Typ|) (e: FSharpMemberOrFunctionOrValue) = e.FullType

let checker = FSharpChecker.Create(keepAssemblyContents=true)

let parse projFile =
    let options, _ =
        match Path.GetExtension(projFile) with
        | ".fsx" ->
            let projCode = File.ReadAllText projFile
            checker.GetProjectOptionsFromScript(projFile, projCode)
            |> Async.RunSynchronously
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
                let name =
                    match meth.EnclosingEntity with
                    | Some tdef ->
                        let separator = if meth.IsInstanceMember then "$" else "$$"
                        tdef.FullName + separator + meth.CompiledName
                    | _ -> meth.FullName
                printfn "%s%i) METHOD: %s" prefix i name
                printfn "%A" body
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            printfn "%s%i) ACTION" prefix i
            // printfn "%A" expr
        )

and lookup f (expr: FSharpExpr) =
    f expr
    List.iter (lookup f) expr.ImmediateSubExpressions

let proj = parse "temp/Test.fsx"
proj.AssemblyContents.ImplementationFiles
|> Seq.iteri (fun i file -> printfn "%i) %s" i file.FileName)
proj.AssemblyContents.ImplementationFiles.[0].Declarations
|> printDecls ""
