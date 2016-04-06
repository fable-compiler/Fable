module Client

open System
open Fable.Core
open Fable.Import

// Check components.fs to see how to build React components from F#
open Components

// Polyfill for ES6 features in old browsers
Node.require.Invoke("core-js") |> ignore

ReactDom.render(
    ReactHelper.com<CommentBox,_,_> { url="/api/comments"; pollInterval=2000. } [],
    Browser.document.getElementById "content")
|> ignore