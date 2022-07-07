module Fable.Compiler.App

open FSharp.Compiler
open FSharp.Compiler.EditorServices
open FSharp.Compiler.SourceCodeServices
open Fable.Compiler.Platform

// let references = Metadata.references_full
// let metadataPath = "../../../../temp/metadata/" // .NET BCL binaries
let references = Metadata.references_core
let metadataPath =  __SOURCE_DIRECTORY__ + "/../../../../Fable/src/fable-metadata/lib/" // .NET BCL binaries

[<EntryPoint>]
let main _argv =
    printfn "Parsing begins..."

    let defines = [||]
    let optimize = false
    let readAllBytes dllName = readAllBytes (metadataPath + dllName)
    let checker = InteractiveChecker.Create(references, readAllBytes, defines, optimize)

    let projectFileName = "project"
    let fileName = __SOURCE_DIRECTORY__ + "/test_script.fsx"
    let source = readAllText fileName

    let parseResults, typeCheckResults, projectResults =
        checker.ParseAndCheckFileInProject(fileName, projectFileName, [|fileName|], [|source|])

    // print errors
    projectResults.Diagnostics |> Array.iter (fun e -> printfn "%A: %A" (e.Severity) e)

    printfn "Typed AST (optimize=%A):" optimize
    // let implFiles = typeCheckResults.ImplementationFile |> Option.toArray
    let implFiles =
        let assemblyContents =
            if not optimize then projectResults.AssemblyContents
            else projectResults.GetOptimizedAssemblyContents()
        assemblyContents.ImplementationFiles
    let decls = implFiles
                |> Seq.collect (fun file -> AstPrint.printFSharpDecls "" file.Declarations)
                |> String.concat "\n"
    decls |> printfn "%s"
    // writeAllText (fileName + ".ast.txt") decls

    let inputLines = source.Split('\n')

    // Get tool tip at the specified location
    let tip = typeCheckResults.GetToolTip(4, 7, inputLines.[3], ["foo"], Tokenization.FSharpTokenTag.IDENT)
    (sprintf "%A" tip).Replace("\n","") |> printfn "\n---> ToolTip Text = %A" // should be "FSharpToolTipText [...]"

    // Get declarations (autocomplete) for msg
    let partialName = { QualifyingIdents = []; PartialIdent = "msg"; EndColumn = 17; LastDotPos = None }
    let decls = typeCheckResults.GetDeclarationListInfo(Some parseResults, 6, inputLines.[5], partialName, (fun _ -> []))
    [ for item in decls.Items -> item.Name ] |> printfn "\n---> msg AutoComplete = %A" // should be string methods

    // Get declarations (autocomplete) for canvas
    let partialName = { QualifyingIdents = []; PartialIdent = "canvas"; EndColumn = 10; LastDotPos = None }
    let decls = typeCheckResults.GetDeclarationListInfo(Some parseResults, 8, inputLines.[7], partialName, (fun _ -> []))
    [ for item in decls.Items -> item.Name ] |> printfn "\n---> canvas AutoComplete = %A"

    0
