module Platform

#if DOTNET_FILE_SYSTEM

open System.IO

let readAllBytes metadataPath (fileName:string) = File.ReadAllBytes (metadataPath + fileName)
let readAllText (filePath:string) = File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let writeAllText (filePath:string) (text:string) = File.WriteAllText (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    sw.ElapsedMilliseconds, res

let normalizeFullPath (path: string) =
    System.IO.Path.GetFullPath(path).Replace('\\', '/')

let getRelativePath (pathFrom: string) (pathTo: string) =
    System.IO.Path.GetRelativePath(pathFrom, pathTo).Replace('\\', '/')

#else // !DOTNET_FILE_SYSTEM

open Fable.Core.JsInterop

type private IFileSystem =
    abstract readFileSync: string -> byte[]
    abstract readFileSync: string * string -> string
    abstract writeFileSync: string * string -> unit

type private IProcess =
    abstract hrtime: unit -> float []
    abstract hrtime: float[] -> float[]

type private IPath =
    abstract resolve: string -> string
    abstract relative: string * string -> string

let private File: IFileSystem = importAll "fs"
let private Process: IProcess = importAll "process"
let private Path: IPath = importAll "path"

let readAllBytes metadataPath (fileName:string) = File.readFileSync (metadataPath + fileName)
let readAllText (filePath:string) = (File.readFileSync (filePath, "utf8")).TrimStart('\uFEFF')
let writeAllText (filePath:string) (text:string) = File.writeFileSync (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = Process.hrtime()
    let res = f x
    let elapsed = Process.hrtime(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let normalizeFullPath (path: string) =
    Path.resolve(path).Replace('\\', '/')

let getRelativePath (pathFrom: string) (pathTo: string) =
    Path.relative(pathFrom, pathTo).Replace('\\', '/')

#endif

module Path =

    let Combine (path1: string, path2: string) =
        let path1 =
            if path1.Length = 0 then path1
            else (path1.TrimEnd [|'\\';'/'|]) + "/"
        path1 + (path2.TrimStart [|'\\';'/'|])

    let GetFileName (path: string) =
        let normPath = path.Replace("\\", "/").TrimEnd('/')
        let i = normPath.LastIndexOf("/")
        normPath.Substring(i + 1)

    let GetDirectoryName (path: string) =
        let normPath = path.Replace("\\", "/")
        let i = normPath.LastIndexOf("/")
        if i < 0 then ""
        else normPath.Substring(0, i)
