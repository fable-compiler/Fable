module ASTViewer

open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions
open FSharp.Compiler
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols

let parse (checker: FSharpChecker) projFile =
    let projFile = Path.GetFullPath(projFile)

    let options =
        match Path.GetExtension(projFile) with
        | ".fsx" ->
            let projCode = File.ReadAllText projFile

            checker.GetProjectOptionsFromScript(
                projFile,
                projCode |> FSharp.Compiler.Text.SourceText.ofString
            )
            |> Async.RunSynchronously
            |> fst
        | ".fsproj" ->
            let opts, _, _ =
                Fable.Cli.ProjectCoreCracker.GetProjectOptionsFromProjectFile
                    "Release"
                    projFile

            opts
        | ext -> failwithf "Unexpected extension: %s" ext
    // for f in options.OtherOptions do
    //     printfn "%s" f
    options |> checker.ParseAndCheckProject |> Async.RunSynchronously

let printShort limit (e: FSharpExpr) =
    let s = Regex.Replace(sprintf "%A" e, "\\s+", " ")

    if s.Length > limit then
        s.[..limit] + "..."
    else
        s

let rec printExpr =
    function
    | FSharpExprPatterns.Sequential(e1, e2) ->
        sprintf "SEQUENTIAL: %s\n%s" (printExpr e1) (printExpr e2)
    | FSharpExprPatterns.Let((var, value), e) ->
        sprintf "LET: (%A, %A)\n>>>> %A" var value e
    | e -> printShort 100 e

let printVar (var: FSharpMemberOrFunctionOrValue) =
    sprintf
        "var %s (isMemberThis %b isConstructorThis %b)"
        var.LogicalName
        var.IsMemberThisValue
        var.IsConstructorThisValue

let rec deepSearch (f: FSharpExpr -> 'a option) e =
    match f e with
    | Some x -> Some x
    | None -> e.ImmediateSubExpressions |> List.tryPick (deepSearch f)

let rec printDecls prefix decls =
    decls
    |> Seq.iteri (fun i decl ->
        match decl with
        | FSharpImplementationFileDeclaration.Entity(e, sub) ->
            printfn "%s%i) ENTITY: %s" prefix i e.DisplayName
            printDecls (prefix + "\t") sub
        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(meth,
                                                                      args,
                                                                      body) ->
            if meth.IsValue then
                printfn "%s%i) VALUE: %s " prefix i meth.FullName
            else
                printfn "%s%i) METHOD: %s" prefix i meth.FullName
            // match body with
            // | FSharpExprPatterns.Call(_,call,_,_,_) ->
            //     printfn "%s Call %s (IsDispatchSlot %b)" prefix call.FullName call.IsDispatchSlot
            // | _ -> ()
            if meth.IsCompilerGenerated then
                printfn "%s(Compiler generated)" prefix
            else
                printfn "%A" body
        | FSharpImplementationFileDeclaration.InitAction(expr) ->
            printfn "%s%i) ACTION" prefix i
            printfn "%A" expr
    )

and lookup f (expr: FSharpExpr) =
    f expr
    List.iter (lookup f) expr.ImmediateSubExpressions

[<EntryPoint>]
let main argv =
    let checker = FSharpChecker.Create(keepAssemblyContents = true)
    let proj = parse checker argv.[0]
    // proj.AssemblyContents.ImplementationFiles
    // |> Seq.iteri (fun i file -> printfn "%i) %s" i file.FileName)
    let lastFile = List.last proj.AssemblyContents.ImplementationFiles
    printDecls "" lastFile.Declarations
    0
