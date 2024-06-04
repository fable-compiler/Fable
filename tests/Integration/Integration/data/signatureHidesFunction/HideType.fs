module HideType

open Fable.Core

/// This type should be hidden by the signature file
type public Foobar =
    | Foo of int
    | Bar of string

let someFunction () =
    JS.console.log "meh"
