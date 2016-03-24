module Client

open System
open Fable.Core
open Fable.Import

// Check components.fs to see how we can build React componenst from F#
open Components

// Polyfill for ES6 features in old browsers
Node.Globals.require.Invoke("core-js") |> ignore

ReactDom.Globals.render(
    ReactHelper.com<CommentBox,_,_> { url="/api/comments"; pollInterval=2000. } [],
    Browser.Globals.document.getElementById "content")
|> ignore