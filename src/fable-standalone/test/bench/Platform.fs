module Bench.Platform

#if DOTNET_FILE_SYSTEM && !FABLE_COMPILER

open System.IO

let readAllBytes (filePath: string) = File.ReadAllBytes(filePath)

let readAllText (filePath: string) =
    File.ReadAllText(filePath, System.Text.Encoding.UTF8)

let writeAllText (filePath: string) (text: string) =
    File.WriteAllText(filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    res, sw.ElapsedMilliseconds

#else

open Fable.Core.JsInterop

module JS =
    type IFileSystem =
        abstract readFileSync: string -> byte[]
        abstract readFileSync: string * string -> string
        abstract writeFileSync: string * string -> unit

    type IProcess =
        abstract hrtime: unit -> float[]
        abstract hrtime: float[] -> float[]

    let fs: IFileSystem = importAll "fs"
    let ``process``: IProcess = importAll "process"

let readAllBytes (filePath: string) = JS.fs.readFileSync (filePath)

let readAllText (filePath: string) =
    JS.fs.readFileSync(filePath, "utf8").TrimStart('\uFEFF')

let writeAllText (filePath: string) (text: string) =
    JS.fs.writeFileSync (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = JS.``process``.hrtime ()
    let res = f x
    let elapsed = JS.``process``.hrtime (startTime)
    res, int64 (elapsed[0] * 1e3 + elapsed[1] / 1e6)

#endif
