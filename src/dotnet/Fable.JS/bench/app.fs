module App

let use_net45_meta = true
let references = Metadata.references use_net45_meta
let metadataPath =
    if use_net45_meta
    then "../demo/repl/metadata/"  // dotnet 4.5 binaries
    else "../demo/repl/metadata2/" // dotnet core 2.0 binaries

#if !DOTNET

[<Fable.Core.Import("readFileSync", "fs")>]
let readFileSync: System.Func<string, byte[]> = failwith "JS only"
[<Fable.Core.Import("readFileSync", "fs")>]
let readTextSync: System.Func<string, string, string> = failwith "JS only"
[<Fable.Core.Emit("process.hrtime()")>]
let hrTimeNow(): float[] = failwith "JS only"
[<Fable.Core.Emit("process.hrtime($0)")>]
let hrTimeElapsed(time: float[]): float[] = failwith "JS only"

let readAllBytes = fun (fileName:string) -> readFileSync.Invoke (metadataPath + fileName)
let readAllText = fun (filePath:string) -> readTextSync.Invoke (filePath, "utf8")
let measureTime (f: 'a -> 'b) x =
    let startTime = hrTimeNow()
    let res = f x
    let elapsed = hrTimeElapsed(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let toJson (value: obj) = value |> Fable.Core.JsInterop.toJson

#else // DOTNET

let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText = fun (filePath:string) -> System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
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
        let source = readAllText fileName
        let fable = Fable.JS.Main.defaultManager
        let createChecker () = fable.CreateChecker(references, readAllBytes)
        let ms0, checker = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0
        let fableCoreDir = "../../../../build/fable-core"
        let com = fable.CreateCompiler(fableCoreDir)
        let optimized = false // todo: from compiler option
        let parseFSharp () = fable.ParseFSharpProject(checker, fileName, source)
        let parseFable ast = fable.CompileToBabelAst(com, ast, fileName, optimized)
        let bench i =
            let ms1, fcsAST = measureTime parseFSharp ()
            let ms2, babelAST = measureTime parseFable fcsAST
            printfn "iteration %d, FCS time: %d ms, Fable time: %d ms" i ms1 ms2
            //printfn "Babel AST: %s" (toJson babelAST)
        [1..10] |> List.iter bench
    with ex ->
        printfn "Error: %A" ex.Message
    0
