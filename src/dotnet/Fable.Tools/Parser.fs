module Fable.Client.Webpack.Parser

open System
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Fable

type Message =
    { path: string
    ; define: string[]
    ; plugins: string[]
    ; options: CompilerOptions }

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

let private parseStringRequired (key: string) (o: JObject)  =
    match o.[key] with
    | null -> failwithf "Missing argument %s" key
    | :? JValue as v when v.Type = JTokenType.String -> v.ToObject<string>()
    | _ -> failwithf "Missing argument %s" key

let makePathRelative path =
    let path = Path.normalizeFullPath path
    let cwd = System.IO.Directory.GetCurrentDirectory()
    Path.getRelativeFileOrDirPath true cwd true path

let (|Parse|) (msg: string) =
    let json = JsonConvert.DeserializeObject<JObject>(msg)
    let path =  parseStringRequired "path" json |> Path.normalizeFullPath
    let define = parseStringArray [||] "define" json |> Array.append [|"FABLE_COMPILER"|]
    let plugins = parseStringArray [||] "plugins" json
    let opts =
        // TODO: Check fable-core version
        { fableCore =
            match parseString "fable-core" "fableCore" json with
            | "fable-core" -> "fable-core"
            | path -> makePathRelative path
        ; declaration = parseBoolean false "declaration" json
        ; typedArrays = parseBoolean true "typedArrays" json
        ; clampByteArrays = parseBoolean false "clampByteArrays" json }
    // printfn "Parsed options: path=%s; define=%A; plugins=%A; options=%A" path define plugins opts
    { path=path; define=define; plugins=plugins; options=opts }

let getDefaultOptions() =
    { fableCore = "fable-core"
    ; declaration = false
    ; typedArrays = true
    ; clampByteArrays = false }