#load "../Fable.Compiler.fsx"

open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable.Client

let compileSource readAllBytes references source =
    let checker = InteractiveChecker(List.ofArray references, readAllBytes)
    let opts = readOptions [||]
    let com = makeCompiler opts []
    let fileName = "stdin.fsx"
    let files = compileAst com checker (fileName, source)
    files |> Array.ofSeq
