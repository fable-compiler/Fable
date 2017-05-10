module Pacman.Sound

open Fable.Core
open Fable.Core.JsInterop


/// Uses Fable's Emit to call JavaScript directly and play sounds
[<Emit("(new Audio($0)).play();")>]
let play (fileName: string) = jsNative