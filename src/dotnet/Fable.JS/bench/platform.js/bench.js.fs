module Bench.Platform

let testScriptPath =  "${entryDir}/test_script.fsx"
let fableCoreDir = "${entryDir}/../../../../build/fable-core"

type IFileSystem =
    abstract readFileSync: string -> byte[]
    abstract readFileSync: string * string -> string
    abstract writeFileSync: string * string -> unit

type IProcess =
    abstract hrtime: unit -> float []
    abstract hrtime: float[] -> float[]

let FileSystem: IFileSystem = Fable.Core.JsInterop.importAll "fs"
let Process: IProcess = Fable.Core.JsInterop.importAll "process"

let initFable (): Fable.JS.IFableManager = Fable.Core.JsInterop.import "init" "${entryDir}/../../out/Main"
let readAllBytes metadataPath (fileName:string) = FileSystem.readFileSync(metadataPath + fileName)
let readAllText (filePath:string) = FileSystem.readFileSync (filePath, "utf8")
let writeAllText (filePath:string) (text:string) = FileSystem.writeFileSync (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = Process.hrtime()
    let res = f x
    let elapsed = Process.hrtime(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res


type private IBabelResult =
    abstract code: string

let private transformFromAst (babelAst: obj): IBabelResult =
    Fable.Core.JsInterop.importMember "babel-core"

let writeJs (filePath:string) (babelAst:obj) =
    let res = transformFromAst babelAst
    writeAllText filePath res.code
