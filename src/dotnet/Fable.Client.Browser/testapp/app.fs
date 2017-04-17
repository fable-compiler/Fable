module App

open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable.Client

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
let measureTime (f: unit -> 'a) =
    let startTime = hrTimeNow()
    let res = f()
    let elapsed = hrTimeElapsed(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

#else // (DOTNETCORE || DOTNET40)

let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText = fun (filePath:string) -> System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let measureTime (f: unit -> 'a) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f()
    sw.Stop()
    sw.ElapsedMilliseconds, res

#endif

// #if (DOTNETCORE || DOTNET40)
// [<EntryPoint>]
// #endif
let main () =
    try
        let references = [|"FSharp.Core";"mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics";"Fable.Core"|]
        let opts = getDefaultOptions ()
        let com = makeCompiler opts []
        let fileName = "test_script.fsx"
        let source = readAllText fileName
        let createChecker() = references |> createChecker readAllBytes
        let ms, checker = measureTime createChecker
        printfn "InteractiveChecker created in %d ms" ms
        let parseFCS() =
            parseFSharpProject com checker (fileName, source)
            |> ignore
        let parseAll() =
            compileAst com checker (fileName, source)
            |> ignore
        let bench i =
            let ms, _ = measureTime parseFCS
            printfn "iteration %d, parse time (FCS) = %d ms" i ms
            let ms, _ = measureTime parseAll
            printfn "iteration %d, parse time (ALL) = %d ms" i ms
        [1..10] |> List.iter bench
    with ex ->
        printfn "Error: %A" ex.Message
    0

main () |> ignore
