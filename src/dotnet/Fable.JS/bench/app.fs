module App

let use_net45_meta = false
let references = Metadata.references use_net45_meta
let metadataPath =
    if use_net45_meta
    then "/temp/repl/metadata/"  // dotnet 4.5 binaries
    else "/temp/repl/metadata2/" // dotnet core 2.0 binaries

#if !DOTNET_FILE_SYSTEM

let readFileSync: System.Func<string, byte[]> = Fable.Core.JsInterop.import "readFileSync" "fs"
let readTextSync: System.Func<string, string, string> = Fable.Core.JsInterop.import "readFileSync" "fs"
let writeTextSync: System.Action<string, string> = Fable.Core.JsInterop.import "writeFileSync" "fs"
let hrTimeNow: System.Func<float[]> = Fable.Core.JsInterop.import "hrtime" "process"
let hrTimeElapsed: System.Func<float[], float[]> = Fable.Core.JsInterop.import "hrtime" "process"

let readAllBytes (fileName:string) = readFileSync.Invoke (metadataPath + fileName)
let readAllText (filePath:string) = readTextSync.Invoke (filePath, "utf8")
let writeAllText (filePath:string) (text:string) = writeTextSync.Invoke (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = hrTimeNow.Invoke()
    let res = f x
    let elapsed = hrTimeElapsed.Invoke(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let toJson (value: obj) = Fable.Core.JsInterop.toJson value

#else // DOTNET_FILE_SYSTEM

let readAllBytes (fileName:string) = System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText (filePath:string) = System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let writeAllText (filePath:string) (text:string) = System.IO.File.WriteAllText (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    sw.ElapsedMilliseconds, res

let toJson (value: obj) = Newtonsoft.Json.JsonConvert.SerializeObject(value)

#endif


[<EntryPoint>]
let main argv =
    try
        let fileName = "test_script.fsx"
        let fsAstFile = Fable.Path.ChangeExtension(fileName, ".fsharp.ast.txt")
        let babelAstFile = Fable.Path.ChangeExtension(fileName, ".babel.ast.json")
        let source = readAllText fileName
        let fable = Fable.JS.Main.defaultManager
        let createChecker () = fable.CreateChecker(references, readAllBytes)
        let ms0, checker = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0
        let fableCoreDir = "../../../../build/fable-core"
        let optimized = false // todo: from compiler option
        let writeAst = false
        let parseFSharp () = fable.ParseFSharpProject(checker, fileName, source)
        let parseFable ast = fable.CompileToBabelAst(fableCoreDir, ast, fileName, optimized)
        let bench i =
            let ms1, fsAst = measureTime parseFSharp ()
            let errors = fable.GetParseErrors fsAst
            errors |> Array.iter (printfn "Error: %A")
            if errors.Length > 0 then failwith "Too many errors."
            let ms2, babelAst = measureTime parseFable fsAst
            if writeAst && i = 1 then
                let fsAstStr = fable.FSharpAstToString(fsAst, optimized)
                printfn "Typed AST (unoptimized): %s" fsAstStr
                writeAllText fsAstFile fsAstStr
                printfn "Babel AST: %s" (toJson babelAst)
                writeAllText babelAstFile (toJson babelAst)
            printfn "iteration %d, FCS time: %d ms, Fable time: %d ms" i ms1 ms2
        [1..10] |> List.iter bench
    with ex ->
        printfn "Error: %A" ex.Message
    0
