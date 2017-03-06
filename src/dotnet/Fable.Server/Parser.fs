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

let private parseString (key: string) (o: JObject)  =
    match o.[key] with
    | null -> failwithf "Missing argument %s" key
    | :? JValue as v when v.Type = JTokenType.String -> v.ToObject<string>()
    | _ ->  failwithf "Missing argument %s" key

let (|Parse|) (msg: string) =
    let json = JsonConvert.DeserializeObject<JObject>(msg)
    let path =  parseString "path" json |> Path.normalizeFullPath
    let define = parseStringArray [||] "define" json
    let plugins = parseStringArray [||] "plugins" json
    let opts =
        { declaration = parseBoolean false "declaration" json
        ; typedArrays = parseBoolean true "typedArrays" json
        ; clampByteArrays = parseBoolean false "clampByteArrays" json }
    { path=path; define=define; plugins=plugins; options=opts }

let getDefaultOptions() =
    { declaration = false
    ; typedArrays = true
    ; clampByteArrays = false }