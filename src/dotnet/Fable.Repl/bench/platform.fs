module Bench.Platform

#if DOTNET_FILE_SYSTEM

let fableReplacementsDir = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../build/fable-replacements")

let readAllBytes metadataPath (fileName:string) = System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText (filePath:string) = System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let writeAllText (filePath:string) (text:string) = System.IO.File.WriteAllText (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    sw.ElapsedMilliseconds, res

// not really serializing, just a stub
let toJson (value: obj) = sprintf "%A" value // Newtonsoft.Json.JsonConvert.SerializeObject(value)

#else

let fableReplacementsDir = "${entryDir}/../../../build/fable-replacements"

type private IFileSystem =
    abstract readFileSync: string -> byte[]
    abstract readFileSync: string * string -> string
    abstract writeFileSync: string * string -> unit

type private IProcess =
    abstract hrtime: unit -> float []
    abstract hrtime: float[] -> float[]

let private FileSystem: IFileSystem = Fable.Core.JsInterop.importAll "fs"
let private Process: IProcess = Fable.Core.JsInterop.importAll "process"

let readAllBytes metadataPath (fileName:string) = FileSystem.readFileSync(metadataPath + fileName)
let readAllText (filePath:string) = (FileSystem.readFileSync (filePath, "utf8")).TrimStart('\uFEFF')
let writeAllText (filePath:string) (text:string) = FileSystem.writeFileSync (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = Process.hrtime()
    let res = f x
    let elapsed = Process.hrtime(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let toJson (value: obj) = value |> Fable.Core.JsInterop.toJson

#endif