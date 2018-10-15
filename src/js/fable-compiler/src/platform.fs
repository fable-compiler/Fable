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

let readAllBytes metadataPath (fileName:string) = FileSystem.readFileSync(metadataPath + fileName)
let readAllText (filePath:string) = (FileSystem.readFileSync (filePath, "utf8")).TrimStart('\uFEFF')
let writeAllText (filePath:string) (text:string) = FileSystem.writeFileSync (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = Process.hrtime()
    let res = f x
    let elapsed = Process.hrtime(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let toJson (value: obj) = value |> toJson

let ensureDirExists (dir: string): unit = importMember "./util.js"

type private IBabelResult =
    abstract code: string
type private IBabel =
    abstract transformFromAst: babelAst: obj -> IBabelResult
let private Babel: IBabel = importAll "@babel/core"

let writeJs (filePath: string) (babelAst: obj) =
    let res = Babel.transformFromAst babelAst
    writeAllText filePath res.code

let initFable (): Fable.Repl.IFableManager =
    import "init" "./bundle/bundle.min.js"

// let initFable (): Fable.Repl.IFableManager =
//     import "init" "${entryDir}/../../../dotnet/Fable.Repl/out/Main"

let [<Emit("__dirname")>] __dirname: string = jsNative

module Path =

    let Combine (path1: string, path2: string) =
        (path1.TrimEnd [|'\\';'/'|]) + "/" + (path2.TrimStart [|'\\';'/'|])

    let ChangeExtension (path: string, ext: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then path
        else path.Substring(0, i) + ext

    let GetExtension (path: string) =
        let i = path.LastIndexOf(".")
        if i < 0 then ""
        else path.Substring(i)

    let GetFileName (path: string) =
        let normPath = path.Replace("\\", "/").TrimEnd('/')
        let i = normPath.LastIndexOf("/")
        path.Substring(i + 1)

    let GetDirectoryName (path: string) =
        let normPath = path.Replace("\\", "/")
        let i = normPath.LastIndexOf("/")
        if i < 0 then ""
        else path.Substring(0, i)
