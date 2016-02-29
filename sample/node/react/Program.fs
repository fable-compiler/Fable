module Program

open System
open Fable.Import
open Fable.Core

let createElement (a : string) b c = React.Globals.createElement (a, b, c) :> obj :?> React.ReactNode

[<EntryPoint>]
let main argv =
    let root = (createElement "div" (createObj [ ("className", "root" :> obj) ]) [||])
    root :> obj :?> React.ReactElement<obj>
    |> ReactDom.DOMServer.Globals.renderToString
    |> Console.WriteLine
    0
