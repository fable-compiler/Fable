module Fable.Compiler.Platform

#if DOTNET_FILE_SYSTEM && !FABLE_COMPILER

open System.IO

let readAllBytes (filePath: string) = File.ReadAllBytes(filePath)
let readAllText (filePath: string) = File.ReadAllText(filePath, System.Text.Encoding.UTF8)
let writeAllText (filePath: string) (text: string) = File.WriteAllText(filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    sw.ElapsedMilliseconds, res

let normalizeFullPath (path: string) =
    let path = if System.String.IsNullOrWhiteSpace path then "." else path
    Path.GetFullPath(path).Replace('\\', '/')

let getRelativePath (path: string) (pathTo: string) =
    let path = if System.String.IsNullOrWhiteSpace path then "." else path
    Path.GetRelativePath(path, pathTo).Replace('\\', '/')

let getHomePath () =
    System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)

#else

open Fable.Core.JsInterop

module JS =
    type IFileSystem =
        abstract readFileSync: string -> byte[]
        abstract readFileSync: string * string -> string
        abstract writeFileSync: string * string -> unit

    type IProcess =
        abstract hrtime: unit -> float []
        abstract hrtime: float[] -> float[]

    type IPath =
        abstract resolve: string -> string
        abstract relative: string * string -> string

    type IOperSystem =
        abstract homedir: unit -> string
        abstract tmpdir: unit -> string
        abstract platform: unit -> string
        abstract arch: unit -> string

    let fs: IFileSystem = importAll "fs"
    let os: IOperSystem = importAll "os"
    let proc: IProcess = importAll "process"
    let path: IPath = importAll "path"

let readAllBytes (filePath: string) = JS.fs.readFileSync(filePath)
let readAllText (filePath: string) = JS.fs.readFileSync(filePath, "utf8").TrimStart('\uFEFF')
let writeAllText (filePath: string) (text: string) = JS.fs.writeFileSync(filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = JS.proc.hrtime()
    let res = f x
    let elapsed = JS.proc.hrtime(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let normalizeFullPath (path: string) =
    JS.path.resolve(path).Replace('\\', '/')

let getRelativePath (path: string) (pathTo: string) =
    JS.path.relative(path, pathTo).Replace('\\', '/')

let getHomePath () =
    JS.os.homedir()

#endif

module Path =

    let Combine (path1: string, path2: string) =
        let path1 =
            if path1.Length = 0 then path1
            else (path1.TrimEnd [|'\\';'/'|]) + "/"
        path1 + (path2.TrimStart [|'\\';'/'|])

    let ChangeExtension (path: string, ext: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then path
        else path.Substring(0, i) + ext

    let GetFileName (path: string) =
        let normPath = path.Replace("\\", "/").TrimEnd('/')
        let i = normPath.LastIndexOf("/")
        normPath.Substring(i + 1)

    let GetFileNameWithoutExtension (path: string) =
        let path = GetFileName path
        let i = path.LastIndexOf(".")
        path.Substring(0, i)

    let GetDirectoryName (path: string) =
        let normPath = path.Replace("\\", "/")
        let i = normPath.LastIndexOf("/")
        if i < 0 then ""
        else normPath.Substring(0, i)
