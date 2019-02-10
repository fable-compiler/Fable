module Fable.Compiler.Platform
open Fable.Core
open Fable.Core.JsInterop

type private IFileSystem =
    abstract readFileSync: string -> byte[]
    abstract readFileSync: string * string -> string
    abstract writeFileSync: string * string -> unit

type private IProcess =
    abstract hrtime: unit -> float []
    abstract hrtime: float[] -> float[]

let private FileSystem: IFileSystem = importAll "fs"
let private Process: IProcess = importAll "process"

let readAllBytes (fileName:string) = FileSystem.readFileSync(fileName)
let readAllText (filePath:string) = (FileSystem.readFileSync (filePath, "utf8")).TrimStart('\uFEFF')
let writeAllText (filePath:string) (text:string) = FileSystem.writeFileSync (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = Process.hrtime()
    let res = f x
    let elapsed = Process.hrtime(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let getVersion (): string =
    importMember "./util.js"

let ensureDirExists (dir: string): unit =
    importMember "./util.js"

let copyFolder (from: string, dest: string): unit =
    importMember "./util.js"

let transformAndSaveBabelAst (babelAst: obj, fileName: string, outDir: string, commonjs: bool): unit =
    importMember "./util.js"

let initFable (): Fable.Standalone.IFableManager =
    import "init" "./bundle/fable-standalone.min.js"

let runCmdAndExitIfFails (cmd: string): unit =
    importMember "./util.js"

let [<Emit("__dirname")>] __dirname: string = jsNative

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
