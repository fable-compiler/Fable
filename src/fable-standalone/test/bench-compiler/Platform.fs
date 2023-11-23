module Fable.Compiler.Platform

type CmdLineOptions =
    {
        outDir: string option
        libDir: string option
        benchmark: bool
        optimize: bool
        sourceMaps: bool
        typedArrays: bool option
        language: string
        printAst: bool
    // watch: bool
    }

#if DOTNET_FILE_SYSTEM && !FABLE_COMPILER

open System.IO

let readAllBytes (filePath: string) = File.ReadAllBytes(filePath)

let readAllText (filePath: string) =
    File.ReadAllText(filePath, System.Text.Encoding.UTF8)

let writeAllText (filePath: string) (text: string) =
    File.WriteAllText(filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    res, sw.ElapsedMilliseconds

let ensureDirExists (path: string) : unit =
    Directory.CreateDirectory(path) |> ignore

let normalizePath (path: string) = path.Replace('\\', '/')

let normalizeFullPath (path: string) =
    let path =
        if System.String.IsNullOrWhiteSpace path then
            "."
        else
            path

    Path.GetFullPath(path).Replace('\\', '/')

let getRelativePath (path: string) (pathTo: string) =
    let relPath = Path.GetRelativePath(path, pathTo).Replace('\\', '/')

    if relPath.StartsWith('.') then
        relPath
    else
        "./" + relPath

let getHomePath () =
    System.Environment.GetFolderPath(
        System.Environment.SpecialFolder.UserProfile
    )

let getDirFiles (path: string) (extension: string) =
    if not (Directory.Exists(path)) then
        [||]
    else
        Directory.GetFiles(path, "*" + extension, SearchOption.AllDirectories)
    |> Array.map (fun x -> x.Replace('\\', '/'))
    |> Array.sort

let getGlobFiles (path: string) =
    if path.Contains("*") || path.Contains("?") then
        let normPath = path.Replace('\\', '/')
        let i = normPath.LastIndexOf('/')
        let pattern = normPath.Substring(i + 1)

        let dirPath =
            if i < 0 then
                ""
            else
                normPath.Substring(0, i)

        Directory.GetFiles(dirPath, pattern, SearchOption.AllDirectories)
        |> Array.map (fun x -> x.Replace('\\', '/'))
        |> Array.sort
    else
        [| path |]

let serializeToJson (value: obj) =
    System.Text.Json.JsonSerializer.Serialize(value)

#else

open Fable.Core.JsInterop

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
        abstract hrtime: unit -> float[]
        abstract hrtime: float[] -> float[]

    type IPath =
        abstract resolve: string -> string
        abstract relative: string * string -> string

    // type IGlob =
    //     abstract sync: pattern: string * ?options: obj -> array<string>

    type IUtil =
        abstract getDirFiles: dir: string -> string[]
        abstract ensureDirExists: dir: string -> unit
        abstract escapeJsStringLiteral: str: string -> string
        abstract serializeToJson: data: obj -> string

    // type IPerformance =
    //     abstract now: unit -> float

    let fs: IFileSystem = importAll "fs"
    let os: IOperSystem = importAll "os"
    let proc: IProcess = importAll "process"
    let path: IPath = importAll "path"
    // let glob: IGlob = importAll "glob"
    let util: IUtil = importAll "./util.js"
// let performance: IPerformance = importMember "perf_hooks"

let readAllBytes (filePath: string) = JS.fs.readFileSync (filePath)

let readAllText (filePath: string) =
    JS.fs.readFileSync(filePath, "utf8").TrimStart('\uFEFF')

let writeAllText (filePath: string) (text: string) =
    JS.fs.writeFileSync (filePath, text)

// let measureTime (f: 'a -> 'b) x =
//     let t0 = JS.performance.now()
//     let res = f x
//     let t1 = JS.performance.now()
//     res, int64 (t1 - t0)

let measureTime (f: 'a -> 'b) x =
    let startTime = JS.proc.hrtime ()
    let res = f x
    let elapsed = JS.proc.hrtime (startTime)
    res, int64 (elapsed[0] * 1e3 + elapsed[1] / 1e6)

let ensureDirExists (dir: string) = JS.util.ensureDirExists (dir)
let serializeToJson (data: obj) = JS.util.serializeToJson (data)

let normalizePath (path: string) = path.Replace('\\', '/')

let normalizeFullPath (path: string) =
    JS.path.resolve(path).Replace('\\', '/')

let getRelativePath (path: string) (pathTo: string) =
    let relPath = JS.path.relative(path, pathTo).Replace('\\', '/')

    if relPath.StartsWith('.') then
        relPath
    else
        "./" + relPath

let getHomePath () = JS.os.homedir ()

let getDirFiles (path: string) (extension: string) =
    JS.util.getDirFiles (path)
    |> Array.filter (fun x -> x.EndsWith(extension))
    |> Array.map (fun x -> x.Replace('\\', '/'))
    |> Array.sort

let getGlobFiles (path: string) =
    if path.Contains("*") || path.Contains("?") then
        // JS.glob.sync(path) // commented to remove dependency
        // replaced with fixed globbing pattern (*.fs)
        let dirPath =
            let normPath = path.Replace('\\', '/')
            let i = normPath.LastIndexOf('/')

            if i < 0 then
                ""
            else
                normPath.Substring(0, i)

        getDirFiles dirPath ".fs"
    else
        [| path |]

#endif

module Path =

    let Combine (path1: string, path2: string) =
        let path1 =
            if path1.Length = 0 then
                path1
            else
                (path1.TrimEnd
                    [|
                        '\\'
                        '/'
                    |])
                + "/"

        path1
        + (path2.TrimStart
            [|
                '\\'
                '/'
            |])

    let ChangeExtension (path: string, ext: string) =
        let i = path.LastIndexOf(".")

        if i < 0 then
            path
        else
            path.Substring(0, i) + ext

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

        if i < 0 then
            ""
        else
            normPath.Substring(0, i)
