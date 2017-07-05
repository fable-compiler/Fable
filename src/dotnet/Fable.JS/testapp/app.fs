module App

open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable.JS

let metadataPath = "/temp/metadata/"

#if !(DOTNETCORE || DOTNET40)

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

#else // (DOTNETCORE || DOTNET40)

let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText = fun (filePath:string) -> System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    sw.ElapsedMilliseconds, res

#endif

[<EntryPoint>]
let main argv =
    try
        let references = [|"FSharp.Core";"mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics";"Fable.Core"|]
        let fileName = "test_script.fsx"
        let source = readAllText fileName
        let createChecker() = references |> createChecker readAllBytes
        let ms0, checker = measureTime createChecker ()
        printfn "InteractiveChecker created in %d ms" ms0
        let com = makeCompiler ()
        let parseFSharp () = parseFSharpProject checker com fileName source
        let parseFable ast = compileAst com ast fileName
        let bench i =
            let ms1, fsAST = measureTime parseFSharp ()
            let ms2, fableAST = measureTime parseFable fsAST
            printfn "iteration %d, FCS time: %d ms, Fable time: %d ms" i ms1 ms2
        [1..10] |> List.iter bench
    with ex ->
        printfn "Error: %A" ex.Message
    0
