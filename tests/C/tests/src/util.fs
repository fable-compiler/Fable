module Util

open Fable.Core

[<Emit("assert($0)")>]
let assertTrue (x: bool) =
    nativeOnly