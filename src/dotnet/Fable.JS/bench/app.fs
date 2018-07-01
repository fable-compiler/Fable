module App

let use_net45_meta = false
let references = Metadata.references use_net45_meta
let metadataPath =
    if use_net45_meta
    then "/temp/repl/metadata/"  // dotnet 4.5 binaries
    else "/temp/repl/metadata2/" // dotnet core 2.0 binaries

#if !DOTNET_FILE_SYSTEM

[<Fable.Core.Import("readFileSync", "fs")>]
let readFileSync: System.Func<string, byte[]> = failwith "JS only"
[<Fable.Core.Import("readFileSync", "fs")>]
let readTextSync: System.Func<string, string, string> = failwith "JS only"
[<Fable.Core.Import("writeFileSync", "fs")>]
let writeTextSync: System.Action<string, string> = failwith "JS only"
[<Fable.Core.Emit("process.hrtime()")>]
let hrTimeNow(): float[] = failwith "JS only"
[<Fable.Core.Emit("process.hrtime($0)")>]
let hrTimeElapsed(time: float[]): float[] = failwith "JS only"

let readAllBytes (fileName:string) = readFileSync.Invoke (metadataPath + fileName)
let readAllText (filePath:string) = readTextSync.Invoke (filePath, "utf8")
let writeAllText (filePath:string) (text:string) = writeTextSync.Invoke (filePath, text)
let measureTime (f: 'a -> 'b) x =
    let startTime = hrTimeNow()
    let res = f x
    let elapsed = hrTimeElapsed(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let toJson (value: obj) = value |> Fable.Core.JsInterop.toJson

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
        let fsAstFile = Fable.Path.ChangeExtension(fileName, ".fsharp.ast.json")
        let babelAstFile = Fable.Path.ChangeExtension(fileName, ".babel.ast.json")
        let source = readAllText fileName
        let fable = Fable.JS.Main.defaultManager
        let createChecker () = fable.CreateChecker(references, readAllBytes)
        let ms0, checker = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0
        let fableCoreDir = "../../../../build/fable-core"
        //let com = fable.CreateCompiler(fableCoreDir)
        let optimized = false // todo: from compiler option
        let parseFSharp () = fable.ParseFSharpProject(checker, fileName, source)
        let parseFable ast = fable.CompileToBabelAst(fableCoreDir, ast, fileName, optimized)
        let bench i =
            let ms1, fsAst = measureTime parseFSharp ()
            let errors = fable.GetParseErrors fsAst
            errors |> Array.iter (printfn "Error: %A")
            if errors.Length > 0 then failwith "Too many errors."
            let ms2, babelAst = measureTime parseFable fsAst
            if i = 1 then
                // fable.PrintFSharpAst (fsAst, false, printfn "%s")
                // printfn "Babel AST: %s" (toJson babelAst)
                // writeAllText fsAstFile (toJson fsAst)
                writeAllText babelAstFile (toJson babelAst)
            printfn "iteration %d, FCS time: %d ms, Fable time: %d ms" i ms1 ms2
        [1..10] |> List.iter bench
    with ex ->
        printfn "Error: %A" ex.Message
    0
