module Program

open Fable.Import
open System

[<EntryPoint>]
let main argv =
    let a = React.Globals.createElement("div", obj())
    let b = ReactDom.DOMServer.Globals.renderToString(a)

    Console.WriteLine(b)

    0
