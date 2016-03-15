module Client

open System
open Fable.Core
open Fable.Import

open Components
module R = ReactHelper

ReactDom.Globals.render(
    R.com<CommentBox,_,_> { url="/api/comments"; pollInterval=2000. } [],
    Browser.Globals.document.getElementById "content")
|> ignore