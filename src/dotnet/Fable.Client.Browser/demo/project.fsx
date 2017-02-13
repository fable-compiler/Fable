#load "../Fable.Compiler.fsx"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable.Client

let createChecker readAllBytes references =
    InteractiveChecker(List.ofArray references, readAllBytes)

let compileSource checker source =
    let opts = readOptions [||]
    let com = makeCompiler opts []
    let fileName = "stdin.fsx"
    let files = compileAst com checker (fileName, source)
    files
    |> Seq.map (fun file -> Fable.Core.JsInterop.toJson file)
    |> Seq.head
