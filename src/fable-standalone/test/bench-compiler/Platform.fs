module Fable.Compiler.Platform

type CmdLineOptions = {
    commonjs: bool
    optimize: bool
    sourceMaps: bool
    typescript: bool
    watchMode: bool
}

#if DOTNET_FILE_SYSTEM && !FABLE_COMPILER

open System.IO

let readAllBytes (filePath: string) = File.ReadAllBytes(filePath)
let readAllText (filePath: string) = File.ReadAllText(filePath, System.Text.Encoding.UTF8)
let writeAllText (filePath: string) (text: string) = File.WriteAllText(filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    res, sw.ElapsedMilliseconds

module Json =
    open Newtonsoft.Json
    open FSharp.Reflection
    open System.Collections.Concurrent

    let isErasedUnion (t: System.Type) =
        t.Name = "FSharpOption`1" ||
        FSharpType.IsUnion t &&
            t.GetCustomAttributes true
            |> Seq.exists (fun a -> (a.GetType ()).Name = "EraseAttribute")

    let getErasedUnionValue (v: obj) =
        match FSharpValue.GetUnionFields (v, v.GetType()) with
        | _, [|v|] -> Some v
        | _ -> None

    type ErasedUnionConverter() =
        inherit JsonConverter()
        let typeCache = ConcurrentDictionary<System.Type, bool>()
        override __.CanConvert t =
            typeCache.GetOrAdd(t, isErasedUnion)
        override __.ReadJson(_reader, _t, _v, _serializer) =
            failwith "Not implemented"
        override __.WriteJson(writer, v, serializer) =
            match getErasedUnionValue v with
            | Some v -> serializer.Serialize(writer, v)
            | None -> writer.WriteNull()

    let jsonSettings = JsonSerializerSettings(
                        Converters = [| ErasedUnionConverter() |],
                        ContractResolver = Serialization.CamelCasePropertyNamesContractResolver(),
                        NullValueHandling = NullValueHandling.Ignore)

    let serializeToJson (value: obj) = JsonConvert.SerializeObject(value, jsonSettings)

let serializeToJson = Json.serializeToJson

let ensureDirExists (path: string): unit =
    Directory.CreateDirectory(path) |> ignore

let normalizeFullPath (path: string) =
    let path = if System.String.IsNullOrWhiteSpace path then "." else path
    Path.GetFullPath(path).Replace('\\', '/')

let getRelativePath (path: string) (pathTo: string) =
    let path = if System.String.IsNullOrWhiteSpace path then "." else path
    Path.GetRelativePath(path, pathTo).Replace('\\', '/')

let getHomePath () =
    System.Environment.GetFolderPath(System.Environment.SpecialFolder.UserProfile)

let getDirFiles (path: string) (extension: string) =
    if not (Directory.Exists(path)) then [||]
    else Directory.GetFiles(path, "*" + extension, SearchOption.AllDirectories)
    |> Array.map (fun x -> x.Replace('\\', '/'))
    |> Array.sort

let getGlobFiles (path: string) =
    if path.Contains("*") || path.Contains("?") then
        let normPath = path.Replace('\\', '/')
        let i = normPath.LastIndexOf('/')
        let pattern = normPath.Substring(i + 1)
        let dirPath = if i < 0 then "" else normPath.Substring(0, i)
        Directory.GetFiles(dirPath, pattern, SearchOption.AllDirectories)
        |> Array.map (fun x -> x.Replace('\\', '/'))
        |> Array.sort
    else [| path |]

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
        abstract hrtime: unit -> float []
        abstract hrtime: float[] -> float[]

    type IPath =
        abstract resolve: string -> string
        abstract relative: string * string -> string

    type IGlob =
        abstract sync: pattern: string * ?options: obj -> array<string>

    type IUtil =
        abstract serializeToJson: data: obj -> string
        abstract ensureDirExists: dir: string -> unit
        abstract getDirFiles: dir: string -> string[]

    // type IPerformance =
    //     abstract now: unit -> float

    let fs: IFileSystem = importAll "fs"
    let os: IOperSystem = importAll "os"
    let process: IProcess = importAll "process"
    let path: IPath = importAll "path"
    // let glob: IGlob = importAll "glob"
    let util: IUtil = importAll "./util.js"
    // let performance: IPerformance = importMember "perf_hooks"

let readAllBytes (filePath: string) = JS.fs.readFileSync(filePath)
let readAllText (filePath: string) = JS.fs.readFileSync(filePath, "utf8").TrimStart('\uFEFF')
let writeAllText (filePath: string) (text: string) = JS.fs.writeFileSync(filePath, text)

// let measureTime (f: 'a -> 'b) x =
//     let t0 = JS.performance.now()
//     let res = f x
//     let t1 = JS.performance.now()
//     res, int64 (t1 - t0)

let measureTime (f: 'a -> 'b) x =
    let startTime = JS.process.hrtime()
    let res = f x
    let elapsed = JS.process.hrtime(startTime)
    res, int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6)

let serializeToJson = JS.util.serializeToJson
let ensureDirExists = JS.util.ensureDirExists

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

let getGlobFiles (path: string) =
    if path.Contains("*") || path.Contains("?") then
        // JS.glob.sync(path) // commented to remove dependency
        // replaced with fixed globbing pattern (*.fs)
        let normPath = path.Replace('\\', '/')
        let i = normPath.LastIndexOf('/')
        // let pattern = normPath.Substring(i + 1) // ignored
        let dirPath = if i < 0 then "" else normPath.Substring(0, i)
        getDirFiles dirPath ".fs"
    else [| path |]

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
