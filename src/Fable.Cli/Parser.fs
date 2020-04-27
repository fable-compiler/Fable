module Fable.Cli.Parser

open System.Collections.Generic
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Fable

type Message =
    { path: string
      rootDir: string
      define: string[]
      typedArrays: bool
      clampByteArrays: bool
      extra: IDictionary<string,string> }

let private parseStringArray (def: string[]) (key: string) (o: JObject)  =
    match o.[key] with
    | null -> def
    | :? JArray as ar -> ar.ToObject<string[]>()
    | :? JValue as v when v.Type = JTokenType.String -> [|v.ToObject<string>()|]
    | _ -> def

let private parseBoolean (def: bool) (key: string) (o: JObject)  =
    match o.[key] with
    | null -> def
    | :? JValue as v when v.Type = JTokenType.Boolean -> v.ToObject<bool>()
    | _ -> def

let private parseString (def: string) (key: string) (o: JObject)  =
    match o.[key] with
    | null -> def
    | :? JValue as v when v.Type = JTokenType.String -> v.ToObject<string>()
    | _ -> def

let private tryParseString (key: string) (o: JObject)  =
    match o.[key] with
    | null -> None
    | :? JValue as v when v.Type = JTokenType.String -> v.ToObject<string>() |> Some
    | _ -> None

let private parseStringRequired (key: string) (o: JObject)  =
    match o.[key] with
    | null -> failwithf "Missing argument %s" key
    | :? JValue as v when v.Type = JTokenType.String -> v.ToObject<string>()
    | _ -> failwithf "Missing argument %s" key

let private parseDic (key: string) (o: JObject): IDictionary<string,string> =
    match o.[key] with
    | null -> dict []
    | :? JObject as v -> v.ToObject<IDictionary<string,string>>()
    | _ -> dict []

let toCompilerOptions (msg: Message): CompilerOptions =
    { typedArrays = msg.typedArrays
      clampByteArrays = msg.clampByteArrays
      debugMode = Array.contains "DEBUG" msg.define
      verbosity = GlobalParams.Singleton.Verbosity
      outputPublicInlinedFunctions = Array.contains "FABLE_REPL_LIB" msg.define
      quotations = Array.contains "FABLE_QUOTATIONS" msg.define
      precompiledLib = None
    }

let parse (msg: string) =
    let json = JsonConvert.DeserializeObject<JObject>(msg)
    { path =
        parseStringRequired "path" json
        |> Path.normalizeFullPath
      rootDir =
        parseString GlobalParams.Singleton.WorkingDir "rootDir" json
        |> Path.normalizeFullPath
      define =
        parseStringArray [||] "define" json
        |> Array.append [|Naming.fableCompilerConstant|]
        |> Array.distinct
      typedArrays = parseBoolean true "typedArrays" json
      clampByteArrays = parseBoolean false "clampByteArrays" json
      extra = parseDic "extra" json }
