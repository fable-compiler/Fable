// Contributed by @davidtme

#load "../../../lib/Fable.Core.fs"
#load "../../../lib/Fable.Import.JS.fs"
#load "../../../lib/Fable.Import.Browser.fs"
#load "../../../lib/Fable.Import.React.fs"

open System
open Fable.Core
open Fable.Import

// Helper methods
let inline dom (a: string) b c =
    React.Globals.createElement (a, createObj b, List.toArray c)

let inline el<'T,'P when 'T :> React.Component<'P,obj>> (b: 'P) c =
    React.Globals.createElement (U2.Case1(unbox typeof<'T>), b, List.toArray c)

// This won't work as `React.createClass` expects a plain object not a class instance
// type CC() =
//     inherit React.ComponentSpec<obj, obj>()
//     override x.render() = upcast createElement "div" [] [unbox "Hello World!"]

type CCProps = {
    msg: string
}

type CC() =
    inherit React.Component<CCProps,obj>()
    member x.render() =
        dom "div" [] [unbox x.props.msg]

el<CC,_> { msg = "Hello World!" } []
|> ReactDom.Server.renderToString
|> Console.WriteLine
