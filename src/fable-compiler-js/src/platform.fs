module Fable.Compiler.Platform
open Fable.Core
open Fable.Core.JsInterop

let [<Emit("__dirname")>] __dirname: string = jsNative

#if TEST_LOCAL
let initFable (): Fable.Standalone.IFableManager = import "init" "${entryDir}../../fable-standalone"
let getMetadataDir(): string = __dirname + "/" + "${entryDir}../../fable-metadata/lib"
#else
let initFable (): Fable.Standalone.IFableManager = import "init" "fable-standalone"
let getMetadataDir(): string = importDefault "fable-metadata"
#endif

type private IFileSystem =
    abstract readFileSync: string -> byte[]
    abstract readFileSync: string * string -> string
    abstract writeFileSync: string * string -> unit

type private IProcess =
    abstract hrtime: unit -> float []
    abstract hrtime: float[] -> float[]

type private IPath =
    abstract resolve: string -> string

let private FileSystem: IFileSystem = importAll "fs"
let private Process: IProcess = importAll "process"
let private Path: IPath = importAll "path"

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

let runCmdAndExitIfFails (cmd: string): unit =
    importMember "./util.js"

let fullPath (path: string) =
    Path.resolve(path)

let normalizeFullPath (path: string) =
    Path.resolve(path).Replace('\\', '/')

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
