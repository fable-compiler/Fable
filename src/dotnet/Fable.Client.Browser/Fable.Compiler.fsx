#if FABLE_COMPILER && !DOTNETCORE && !DOTNET40
#r "System.Text.RegularExpressions.dll"
#endif

#load
        "../../../../FSharp.Compiler.Service_fable/src/fsharp/fcs-fable/fcs-fable.fsx"

        "../Fable.Core/Compiler.fs"
        "../Fable.Core/Util.fs"
        "../Fable.Core/Fable.Core.fs"
        "../Fable.Core/AST/AST.Common.fs"
        "../Fable.Core/AST/AST.Fable.fs"
        "../Fable.Core/AST/AST.Fable.Util.fs"
        "../Fable.Core/AST/AST.Babel.fs"
        "../Fable.Core/Plugins.fs"
        // "../Fable.Core/Import/Fable.Import.JS.fs"
        // "../Fable.Core/Import/Fable.Import.Browser.fs"
        // "../Fable.Core/Import/Fable.Import.Node.fs"
        // "../Fable.Core/Fable.Core.Extensions.fs"

        "../Fable.Compiler/Utils.fs"
        "../Fable.Compiler/Replacements.fs"
        "../Fable.Compiler/FSharp2Fable.Util.fs"
        "../Fable.Compiler/FSharp2Fable.fs"
        "../Fable.Compiler/Fable2Babel.fs"

        "Fable.Client.fs"

// For Rollup bundling to work correctly, the last file cannot be empty
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

let compileToJson checker source =
    compileSource checker source
    |> Seq.map (fun file -> Fable.Core.JsInterop.toJson file)
    |> Seq.head

#if !FABLE_COMPILER
let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes ("/temp/metadata/" + fileName)
let readAllText = fun (filePath:string) -> System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)

[<EntryPoint>]
let main argv =
    try
        let references = ["mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics";"FSharp.Core";"Fable.Core"]
        let checker = InteractiveChecker(references, readAllBytes)
        let ast =
            System.IO.File.ReadAllText("/temp/test.fsx")
            |> compileSource checker
        printfn "%A" ast
    with ex ->
        printException ex
    0
#endif