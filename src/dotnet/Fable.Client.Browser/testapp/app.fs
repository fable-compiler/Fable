module App

open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable.Client

let metadataPath = "/temp/metadata/"

#if !DOTNETCORE && !DOTNET40
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
let measureTime (f: unit -> unit) =
    let startTime = hrTimeNow()
    f()
    let elapsed = hrTimeElapsed(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6)
#else
let readAllBytes = fun (fileName:string) -> System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText = fun (filePath:string) -> System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let measureTime (f: unit -> unit) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    f()
    sw.Stop()
    sw.ElapsedMilliseconds
#endif

// let measureTime f =
//     let startTime = System.DateTime.UtcNow
//     f()
//     (System.DateTime.UtcNow - startTime).Milliseconds

#if DOTNETCORE || DOTNET40
[<EntryPoint>]
#endif
let main argv =
    try
        let references = ["FSharp.Core";"mscorlib";"System";"System.Core";"System.Data";"System.IO";"System.Xml";"System.Numerics";"Fable.Core"]
        let checker = InteractiveChecker(references, readAllBytes)
        let opts = readOptions argv
        let com = makeCompiler opts []
        let fileName = "test_script.fsx"
        let source = readAllText fileName
        //let success = compile com checker (fileName, source)
        let f() =
            compileAst com checker (fileName, source)
            |> Seq.map (fun file -> Fable.Core.JsInterop.toJson file)
            |> ignore
        let bench i =
            let ms = measureTime f
            printfn "iteration %d, duration %d ms" i ms
        [1..10] |> List.iter bench
    with ex ->
        printException ex
    0
