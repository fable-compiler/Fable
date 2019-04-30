module Fable.Compiler.Platform

#if DOTNET_FILE_SYSTEM && !FABLE_COMPILER

open System.IO

let readAllBytes (filePath: string) = File.ReadAllBytes(filePath)
let readAllText (filePath: string) = File.ReadAllText(filePath, System.Text.Encoding.UTF8)
let writeAllText (filePath: string) (text: string) = File.WriteAllText(filePath, text)

let measureTime (f: 'a -> 'b) x =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let res = f x
    sw.Stop()
    sw.ElapsedMilliseconds, res

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

let ensureDirExists (dir: string): unit =
    Directory.CreateDirectory(dir) |> ignore

let normalizeFullPath (path: string) =
    Path.GetFullPath(path).Replace('\\', '/')

let getRelativePath (pathFrom: string) (pathTo: string) =
    Path.GetRelativePath(pathFrom, pathTo).Replace('\\', '/')

#else

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
        abstract serializeToJson: data: obj -> string
        abstract ensureDirExists: dir: string -> unit

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

let serializeToJson = JS.Util.serializeToJson
let ensureDirExists = JS.Util.ensureDirExists

let normalizeFullPath (path: string) =
    JS.Path.resolve(path).Replace('\\', '/')

let getRelativePath (pathFrom: string) (pathTo: string) =
    JS.Path.relative(pathFrom, pathTo).Replace('\\', '/')

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
