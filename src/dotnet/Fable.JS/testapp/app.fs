module App

open Fable.Core
open Fable.Core.JsInterop
open Fable.JS.Interfaces

type IFileSystem =
    abstract readFileSync: string -> byte[]
    [<Emit("$0.readFileSync($1,'utf8')")>]
    abstract readTextSync: string -> string
    abstract writeFileSync: string * obj -> unit

type IProcess =
    abstract hrtime: unit->float*float
    abstract hrtime: (float*float)->float*float

let fs: IFileSystem = importAll "fs"
let path: Metadata.IPath = importAll "path"
let [<Global("process")>] proc: IProcess = jsNative
let babelAstToJs(jsonAst: string): string = importMember "./util.js"

let readAllBytes (fileName:string) =
    fs.readFileSync(path.join(Metadata.metadataPath, fileName))

let readAllText (filePath:string) =
    fs.readTextSync(filePath)

let measureTime (f: 'a -> 'b) x =
    let startTime = proc.hrtime()
    let res = f x
    let sec, nanosec = proc.hrtime(startTime)
    int64 (sec * 1e3 + nanosec / 1e6), res

let toJson (value: obj) = value |> Fable.Core.JsInterop.toJson

let Fable: IFableManager = importDefault "${entryDir}/../demo/repl/build/bundle.js"

let createChecker() =
    Fable.CreateChecker(Metadata.references, readAllBytes)

let fcsCompile(checker, fileName, source) =
    Fable.ParseFSharpProject(checker, fileName, source)

let fableCompile(com, fileName, fcsAst) =
    let fableCore = path.resolveWithFile("${entryDir}/../demo/repl/build/fable-core")
    Fable.CompileToBabelJsonAst(com, fcsAst, fableCore, fileName)

let babelCompile(babelAst) =
    babelAstToJs babelAst

[<EntryPoint>]
let main argv =
    let fileName = path.resolve argv.[0]
    let targetFileName = path.resolve argv.[1]
    let source = readAllText fileName
    let ms0, checker = measureTime createChecker ()
    let com = Fable.CreateCompiler()
    let ms1, fsAST = measureTime fcsCompile (checker, fileName, source)
    let ms2, babelAST = measureTime fableCompile (com, fileName, fsAST)
    let ms3, jsCode = measureTime babelCompile babelAST
    fs.writeFileSync(targetFileName, jsCode)
    printfn "InteractiveChecker created in %d ms" ms0
    printfn "F# AST parsed in %d ms" ms1
    printfn "F# AST parsed in %d ms" ms2
    printfn "F# AST parsed in %d ms" ms3
    0
