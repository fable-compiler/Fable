module App

open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable.Client

#if !DOTNETCORE && !DOTNET40

[<Fable.Core.Import("readFileSync", "fs")>]
let readFileSync: System.Func<string, byte[]> = failwith "JS only"

#else

[<EntryPoint>]
#endif
let main argv =
    try
        let source = """
printfn "answer: %A" 42
"""
        let metadataPath = "/temp/metadata/"
        let references = ["mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics"]
#if DOTNETCORE || DOTNET40
        let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes (metadataPath + fileName)
#else
        let readAllBytes = fun (fileName:string) -> readFileSync.Invoke (metadataPath + fileName)
#endif

        let checker = InteractiveChecker(references, readAllBytes)
        let opts = readOptions argv
        let com = makeCompiler opts []
        let fileName = "stdin.fsx"
        let success = compile com checker (fileName, source)
        ()
    with ex ->
        printException ex
    0
