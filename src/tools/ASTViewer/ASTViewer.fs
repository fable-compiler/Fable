module ASTViewer

open System
open System.IO
open System.Collections.Generic
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

let findOverloadIndex (m: FSharpMemberOrFunctionOrValue): int option =
    if m.IsImplicitConstructor || m.IsOverrideOrExplicitInterfaceImplementation
    then None
    else
        // m.Overloads(false) doesn't work
        m.EnclosingEntity |> Option.map (fun e ->
            printfn "Enclosing entity %s (abbr. %b)" e.FullName e.IsFSharpAbbreviation
            let name = m.CompiledName
            let isInstance = m.IsInstanceMember
            let paramGroups = m.CurriedParameterGroups |> Seq.concat |> Seq.toList
            // printfn "START WITH %s with %i args (instance %b)"
            //     m.CompiledName paramGroups.Length m.IsInstanceMember
            // for m2 in e.MembersFunctionsAndValues do
            //     if m2.IsInstanceMember = isInstance && m2.CompiledName = name then
            //         let argCount = m2.CurriedParameterGroups |> Seq.sumBy (fun g -> g.Count)
            //         printfn "FOUND member %s with %i arg(s)" m2.CompiledName argCount
            ((0, false), e.MembersFunctionsAndValues) ||> Seq.fold (fun (i, found) m2 ->
                let paramGroups2 = m2.CurriedParameterGroups |> Seq.concat |> Seq.toList
                // printfn "COMPARE TO %s with %i args (instance %b) (index %i, found %b)"
                //     m2.CompiledName paramGroups2.Length m2.IsInstanceMember i found
                if not found && m2.IsInstanceMember = isInstance && m2.CompiledName = name then
                    // .Equals() doesn't work. TODO: Compare arg types for trait calls
                    // .IsEffectivelySameAs() doesn't work for constructors
                    (paramGroups, paramGroups2)
                    ||> List.compareWith (fun x y -> if x = y then 0 else -1)
                    |> function 0 -> (i, true) | _ -> (i + 1, false)
                else i, found)
            |> fst)

let parse projFile =
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
                let name =
                    match meth.EnclosingEntity with
                    | Some tdef ->
                        let separator = if meth.IsInstanceMember then "$" else "$$"
                        tdef.FullName + separator + meth.CompiledName
                    | _ -> meth.FullName
                printfn "%s%i) METHOD: %s" prefix i name
                findOverloadIndex meth |> Option.iter (fun i ->
                    printfn "%s>> overload index: %i" prefix i)
                // printfn "%A" body
        | FSharpImplementationFileDeclaration.InitAction (expr) ->
            printfn "%s%i) ACTION" prefix i
            match expr with
            | BasicPatterns.Call(_,m,_,_,_)
            | BasicPatterns.NewObject(m,_,_) ->
                printfn "%s>> call to %s (implicit cons %b)" prefix m.FullName m.IsImplicitConstructor
                // m.Overloads(false) |> Option.iter (fun overloads ->
                //     Seq.length overloads |> printfn "%s>> overloads: %i" prefix)
                findOverloadIndex m |> Option.iter (fun i ->
                    printfn "%s>> overload index: %i" prefix i)
            | _ -> ()
            // printfn "%A" expr
        )

and lookup f (expr: FSharpExpr) =
    f expr
    List.iter (lookup f) expr.ImmediateSubExpressions

[<EntryPoint>]
let main argv =
    let proj = parse argv.[0]
    // proj.AssemblyContents.ImplementationFiles
    // |> Seq.iteri (fun i file -> printfn "%i) %s" i file.FileName)
    let lastFile = List.last proj.AssemblyContents.ImplementationFiles
    printDecls "" lastFile.Declarations
    0
