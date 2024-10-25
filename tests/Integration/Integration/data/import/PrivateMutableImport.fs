module PrivateMutableImport

open Fable.Core.JsInterop

type ToastrOptions =
    abstract Thing : string with get, set

[<RequireQualifiedAccess>]
module Toastr =
    let mutable private theOptions: ToastrOptions =
        //()
        import "path" "toastr"
