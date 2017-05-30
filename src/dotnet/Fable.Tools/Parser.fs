module Fable.Tools.Parser

open System
open System.Collections.Generic
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Fable

type Message =
    { path: string
    ; define: string[]
    ; plugins: string[]
    ; options: CompilerOptions
    ; extra: IDictionary<string,string> }

let foldi f init (xs: 'T seq) =
    let mutable i = -1
    (init, xs) ||> Seq.fold (fun state x ->
        i <- i + 1
        f i state x)

type ComparisonResult = Smaller | Same | Bigger

let compareVersions (expected: string) (actual: string) =
    if actual = "*" // Wildcard for custom fable-core builds
    then Same
    else
        let expected = expected.Split('.', '-')
        let actual = actual.Split('.', '-')
        (Same, expected) ||> foldi (fun i comp expectedPart ->
            match comp with
            | Bigger -> Bigger
            | Same when actual.Length <= i -> Smaller
            | Same ->
                let actualPart = actual.[i]
                match Int32.TryParse(expectedPart), Int32.TryParse(actualPart) with
                // TODO: Don't allow bigger for major version?
                | (true, expectedPart), (true, actualPart) ->
                    if actualPart > expectedPart
                    then Bigger
                    elif actualPart = expectedPart
                    then Same
                    else Smaller
                | _ ->
                    if actualPart = expectedPart
                    then Same
                    else Smaller
            | Smaller -> Smaller)

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

let private parseDic (key: string) (o: JObject): IDictionary<string,string> =
    match o.[key] with
    | null -> dict []
    | :? JObject as v -> v.ToObject<IDictionary<string,string>>()
    | _ -> dict []

let makePathRelative path =
    let path = Path.normalizeFullPath path
    let cwd = System.IO.Directory.GetCurrentDirectory()
    Path.getRelativeFileOrDirPath true cwd true path

let parse (msg: string) =
    let json = JsonConvert.DeserializeObject<JObject>(msg)
    let path = parseStringRequired "path" json |> Path.normalizeFullPath
    let define = parseStringArray [||] "define" json
    let plugins = parseStringArray [||] "plugins" json
    let opts =
        { fableCore =
            match parseString "fable-core" "fableCore" json with
            | "fable-core" -> "fable-core"
            | path -> (makePathRelative path).TrimEnd('/')
        ; declaration = parseBoolean false "declaration" json
        ; typedArrays = parseBoolean true "typedArrays" json
        ; clampByteArrays = parseBoolean false "clampByteArrays" json }
    let options = { path=path; define=define; plugins=plugins; options=opts; extra=parseDic "extra" json }
    Log.logVerbose(sprintf "Parsed options: %A" options)
    options
