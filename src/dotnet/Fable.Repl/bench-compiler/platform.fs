module Bench.Platform

#if DOTNET_FILE_SYSTEM

let fableCoreDir = System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../build/fable-core")

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
    // open FSharp.Reflection
    // open System.Collections.Concurrent

    // let isErasedUnion (t: System.Type) =
    //     t.Name = "FSharpOption`1" ||
    //     FSharpType.IsUnion t &&
    //         t.GetCustomAttributes true
    //         |> Seq.exists (fun a -> (a.GetType ()).Name = "EraseAttribute")

    // let getErasedUnionValue (v: obj) =
    //     match FSharpValue.GetUnionFields (v, v.GetType()) with
    //     | _, [|v|] -> Some v
    //     | _ -> None

    // type ErasedUnionConverter() =
    //     inherit JsonConverter()
    //     let typeCache = ConcurrentDictionary<System.Type, bool>()
    //     override __.CanConvert t =
    //         typeCache.GetOrAdd(t, isErasedUnion)
    //     override __.ReadJson(_reader, _t, _v, _serializer) =
    //         failwith "Not implemented"
    //     override __.WriteJson(writer, v, serializer) =
    //         match getErasedUnionValue v with
    //         | Some v -> serializer.Serialize(writer, v)
    //         | None -> writer.WriteNull()

    let jsonSettings = JsonSerializerSettings(
                        // Converters = [| ErasedUnionConverter() |],
                        ContractResolver = Serialization.CamelCasePropertyNamesContractResolver(),
                        NullValueHandling = NullValueHandling.Ignore)

    let toJson (value: obj) = JsonConvert.SerializeObject(value, jsonSettings)

let toJson = Json.toJson

let ensureDirExists (dir: string): unit =
    System.IO.Directory.CreateDirectory(dir) |> ignore

let writeJs (filePath:string) (babelAst:obj) = () // Does nothing in .NET for now

#else

open Fable.Core.JsInterop

let fableCoreDir = "${entryDir}/../../../build/fable-core"

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

#endif