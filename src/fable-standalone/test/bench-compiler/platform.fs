module Bench.Platform

#if DOTNET_FILE_SYSTEM && !FABLE_COMPILER

let readAllBytes metadataPath (fileName:string) = System.IO.File.ReadAllBytes (metadataPath + fileName)
let readAllText (filePath:string) = System.IO.File.ReadAllText (filePath, System.Text.Encoding.UTF8)
let writeAllText (filePath:string) (text:string) = System.IO.File.WriteAllText (filePath, text)

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

    let toJson (value: obj) = JsonConvert.SerializeObject(value, jsonSettings)

let toJson = Json.toJson

let ensureDirExists (dir: string): unit =
    System.IO.Directory.CreateDirectory(dir) |> ignore

let normalizeFullPath (path: string) =
    System.IO.Path.GetFullPath(path).Replace('\\', '/')

let getRelativePath (pathFrom: string) (pathTo: string) =
    System.IO.Path.GetRelativePath(pathFrom, pathTo).Replace('\\', '/')

#else

open Fable.Core.JsInterop

type private IFileSystem =
    abstract readFileSync: string -> byte[]
    abstract readFileSync: string * string -> string
    abstract writeFileSync: string * string -> unit

type private IProcess =
    abstract hrtime: unit -> float []
    abstract hrtime: float[] -> float[]

type private IPath =
    abstract resolve: string -> string
    abstract relative: string * string -> string

let private FileSystem: IFileSystem = importAll "fs"
let private Process: IProcess = importAll "process"
let private Path: IPath = importAll "path"

let readAllBytes metadataPath (fileName:string) = FileSystem.readFileSync(metadataPath + fileName)
let readAllText (filePath:string) = (FileSystem.readFileSync (filePath, "utf8")).TrimStart('\uFEFF')
let writeAllText (filePath:string) (text:string) = FileSystem.writeFileSync (filePath, text)

let measureTime (f: 'a -> 'b) x =
    let startTime = Process.hrtime()
    let res = f x
    let elapsed = Process.hrtime(startTime)
    int64 (elapsed.[0] * 1e3 + elapsed.[1] / 1e6), res

let toJson (value: obj) = value |> toJson

let ensureDirExists (dir: string): unit =
    importMember "./util.js"

let normalizeFullPath (path: string) =
    Path.resolve(path).Replace('\\', '/')

let getRelativePath (pathFrom: string) (pathTo: string) =
    Path.relative(pathFrom, pathTo).Replace('\\', '/')

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

    let GetDirectoryName (path: string) =
        let normPath = path.Replace("\\", "/")
        let i = normPath.LastIndexOf("/")
        if i < 0 then ""
        else normPath.Substring(0, i)
