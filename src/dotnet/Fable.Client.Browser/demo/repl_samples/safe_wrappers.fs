open Fable.Core

[<Emit("(parsed => isNaN(parsed) ? null : parsed)(parseFloat($0))")>]
let parseFloat (input: string) : float option = failwith "JS"

[<Emit("(parsed => isNaN(parsed) ? null : parsed)(parseInt($0))")>]
let parseInt (input: string) : int option = failwith "JS"

[<Emit("((o, p) => p in o ? o[p] : null)($1,$0)")>]
let getProp<'t> (propName: string) (any: obj) : 't option = failwith "JS"

[<Emit("$2[$0] = $1")>]
let setProp<'t> (propName: string) (propValue: 't) (any: obj) : unit = failwith "JS"


match parseFloat "3.35" with
| Some value -> printfn "%A" value
| None -> printfn "Nothing found"


match parseInt "20" with
| Some value -> printfn "%A" value
| None -> printfn "Nothing found"


let x = new obj()
x |> setProp "message" "Hello there"

let propValue = getProp<string> "message" x

match propValue with
| Some value -> printfn "%s" value
| None -> printfn "Nothing found"

let unknownProp = getProp<string> "unknown" x

match unknownProp with
| Some value -> printfn "%s" value
| None -> printfn "Nothing found"
