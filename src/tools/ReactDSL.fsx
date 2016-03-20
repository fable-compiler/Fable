open System
open System.IO
open System.Text.RegularExpressions

let firstToUpper (s: string) =
    s.Substring(0,1).ToUpper() + s.Substring(1)
    
let handler (s: string) =
    if s.Contains "Handler"
    then sprintf "(%s -> unit)" <| s.Replace("Handler", "")
    else s

let reg = Regex(@"(\s*)abstract (?:``)?(\w+)(?:``)?: (.+?) option with get, set")
File.ReadAllLines("import/react/Fable.Import.React.fs")
|> Array.map (fun line ->
    let m = reg.Match(line)
    if not m.Success
    then line
    else sprintf "%s| %s of %s"
            (m.Groups.[1].Value)
            (firstToUpper m.Groups.[2].Value)
            (handler m.Groups.[3].Value))
|> fun lines -> File.WriteAllLines("temp/ReactDSL.fs", lines)
