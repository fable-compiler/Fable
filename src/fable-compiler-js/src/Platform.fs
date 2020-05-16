module Fable.Compiler.Platform

open Fable.Core.JsInterop

type CmdLineOptions = {
    commonjs: bool
    optimize: bool
    sourceMaps: bool
    classTypes: bool
    typeDecls: bool
    watchMode: bool
}

module JS =
    type IFileSystem =
        abstract readFileSync: string -> byte[]
        abstract readFileSync: string * string -> string
        abstract writeFileSync: string * string -> unit

    type IOperSystem =
        abstract homedir: unit -> string
        abstract tmpdir: unit -> string
        abstract platform: unit -> string
        abstract arch: unit -> string

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
        abstract transformAndSaveBabelAst: babelAst: obj * outPath: string * projDir: string * outDir: string * libDir: string * options: CmdLineOptions -> unit
        abstract runCmdAndExitIfFails: cmd: string -> unit
        abstract getDirFiles: dir: string -> string[]

    let fs: IFileSystem = importAll "fs"
    let os: IOperSystem = importAll "os"
    let process: IProcess = importAll "process"
    let path: IPath = importAll "path"
    let util: IUtil = importAll "./util.js"

let readAllBytes (filePath: string) = JS.fs.readFileSync(filePath)
let readAllText (filePath: string) = JS.fs.readFileSync(filePath, "utf8").TrimStart('\uFEFF')
let writeAllText (filePath: string) (text: string) = JS.fs.writeFileSync(filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = JS.process.hrtime()
    let res = f x
    let elapsed = JS.process.hrtime(startTime)
    res, int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6)

let getVersion = JS.util.getVersion
let ensureDirExists = JS.util.ensureDirExists
let copyFolder = JS.util.copyFolder
let transformAndSaveBabelAst = JS.util.transformAndSaveBabelAst
let runCmdAndExitIfFails = JS.util.runCmdAndExitIfFails

let normalizeFullPath (path: string) =
    JS.path.resolve(path).Replace('\\', '/')

let getRelativePath (path: string) (pathTo: string) =
    JS.path.relative(path, pathTo).Replace('\\', '/')

let getHomePath () =
    JS.os.homedir()

let getDirFiles (path: string) (extension: string) =
    JS.util.getDirFiles(path)
    |> Array.filter (fun x -> x.EndsWith(extension))
    |> Array.map (fun x -> x.Replace('\\', '/'))
    |> Array.sort


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
        let normPath = path.Replace('\\', '/').TrimEnd('/')
        let i = normPath.LastIndexOf('/')
        normPath.Substring(i + 1)

    let GetFileNameWithoutExtension (path: string) =
        let path = GetFileName path
        let i = path.LastIndexOf('.')
        path.Substring(0, i)

    let GetDirectoryName (path: string) =
        let normPath = path.Replace('\\', '/')
        let i = normPath.LastIndexOf('/')
        if i < 0 then ""
        else normPath.Substring(0, i)
