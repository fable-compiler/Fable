module Fable.Compiler.Platform

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

    type IUtil =
        abstract getVersion: unit -> string
        abstract ensureDirExists: dir: string -> unit
        abstract copyFolder: from: string * dest: string -> unit
        abstract transformAndSaveBabelAst: babelAst: obj * outPath: string * projDir: string * outDir: string * libDir: string * commonjs: bool -> unit
        abstract runCmdAndExitIfFails: cmd: string -> unit

    let FileSystem: IFileSystem = importAll "fs"
    let Process: IProcess = importAll "process"
    let Path: IPath = importAll "path"
    let Util: IUtil = importAll "./util.js"

let readAllBytes (filePath: string) = JS.FileSystem.readFileSync(filePath)
let readAllText (filePath: string) = JS.FileSystem.readFileSync(filePath, "utf8").TrimStart('\uFEFF')
let writeAllText (filePath: string) (text: string) = JS.FileSystem.writeFileSync(filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = JS.Process.hrtime()
    let res = f x
    let elapsed = JS.Process.hrtime(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let getVersion = JS.Util.getVersion
let ensureDirExists = JS.Util.ensureDirExists
let copyFolder = JS.Util.copyFolder
let transformAndSaveBabelAst = JS.Util.transformAndSaveBabelAst
let runCmdAndExitIfFails = JS.Util.runCmdAndExitIfFails

let normalizeFullPath (path: string) =
    JS.Path.resolve(path).Replace('\\', '/')

let getRelativePath (pathFrom: string) (pathTo: string) =
    JS.Path.relative(pathFrom, pathTo).Replace('\\', '/')

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

    let GetFileNameWithoutExtension (path: string) =
        let path = GetFileName path
        let i = path.LastIndexOf(".")
        path.Substring(0, i)

    let GetDirectoryName (path: string) =
        let normPath = path.Replace("\\", "/")
        let i = normPath.LastIndexOf("/")
        if i < 0 then ""
        else normPath.Substring(0, i)
