// Contributed by @davidtme

#load "../../../lib/Fable.Core.fs"
#load "../../../lib/Fable.Import.JS.fs"
#load "../../../lib/Fable.Import.Browser.fs"
#load "../../../lib/Fable.Import.React.fs"

open System
open Fable.Core
open Fable.Import

// Make the function inline so createObj and List.toArray can be optimized
let inline createElement (a : string) b c =
    React.Globals.createElement (a, createObj b, List.toArray c)

createElement "div" ["className" ==> "root"] [unbox "Hello World!"]
|> React.DOMServer.Globals.renderToString
|> Console.WriteLine
