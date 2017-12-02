module App

open Fable.Core
open Fable.Core.JsInterop
open Fable.JS.Interfaces

type IPath =
    abstract join: string * string -> string
    abstract basename: string -> string
    abstract dirname: string -> string
    abstract relative: string * string -> string
    abstract resolve: string -> string
    [<Emit("$0.join(__dirname, $1)")>]
    abstract resolveWithFile: string -> string

type IFileSystem =
    abstract readFileSync: string -> byte[]
    [<Emit("$0.readFileSync($1,'utf8')")>]
    abstract readTextSync: string -> string
    abstract writeFileSync: string * obj -> unit

type IProcess =
    abstract hrtime: unit->float*float
    abstract hrtime: (float*float)->float*float

let fs: IFileSystem = importAll "fs"
let path: IPath = importAll "path"
let [<Global("process")>] proc: IProcess = jsNative
let babelAstToJs(jsonAst: string): string = importMember "./util.js"

let use_net45_meta = true
let references = Metadata.references use_net45_meta
let metadataPath =
    if use_net45_meta
    then path.resolveWithFile("${entryDir}/../demo/repl/metadata")  // dotnet 4.5 binaries
    else path.resolveWithFile("${entryDir}/../demo/repl/metadata2") // dotnet core 2.0 binaries

let readAllBytes (fileName:string) =
    fs.readFileSync(path.join(metadataPath, fileName))

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
    Fable.CreateChecker(references, readAllBytes)

let fcsCompile(checker, sourceFileName, source) =
    Fable.ParseFSharpProject(checker, sourceFileName, source)

let normalize(path: string) =
    path.Replace("\\", "/")

let fableCompile(fableCoreDir, sourceFileName, targetFileName, fcsAst) =
    let fableCoreDir = path.relative(path.dirname(targetFileName), fableCoreDir).TrimEnd('/') |> normalize
    let com = Fable.CreateCompiler(fableCoreDir)
    Fable.CompileToBabelJsonAst(com, fcsAst, sourceFileName)

let babelCompile(babelAst) =
    babelAstToJs babelAst

[<EntryPoint>]
let main argv =
    let sourceFileName = path.resolve argv.[0] |> normalize
    let targetFileName = path.resolve argv.[1] |> normalize
    let fableCoreDir = path.resolveWithFile "${entryDir}/../demo/repl/build/fable-core"

    printfn "SOURCE FILE: %s" sourceFileName
    printfn "TARGET FILE: %s" targetFileName
    printfn "FABLE CORE DIR: %s" fableCoreDir
    printfn "METADATA NET45: %b" use_net45_meta
    printfn "METADATA PATH: %s" metadataPath
    printfn "METADATA REFERENCES: %A" references

    let source = readAllText sourceFileName
    let ms0, checker = measureTime createChecker ()
    let ms1, fsAST = measureTime fcsCompile (checker, sourceFileName, source)
    let ms2, babelAST = measureTime fableCompile (fableCoreDir, sourceFileName, targetFileName, fsAST)
    let ms3, jsCode = measureTime babelCompile babelAST
    fs.writeFileSync(targetFileName, jsCode)

    printfn "InteractiveChecker created in %d ms" ms0
    printfn "F.C.S compile time: %d ms" ms1
    printfn "Fable compile time: %d ms" ms2
    printfn "Babel compile time: %d ms" ms3
    0
