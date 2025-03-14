module Fable.Tests.Global

open Fable.Core
open Fable.Core.JS

[<Pojo>]
type Toast
    (
        id : string,
        title : string
    ) =
    member val id : string = jsNative with get, set
    member val title : string = jsNative with get, set
