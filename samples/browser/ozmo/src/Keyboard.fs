module Ozmo.Keyboard

open Fable.Import

let mutable keysPressed = Set.empty
let code x = if keysPressed.Contains(x) then 1 else 0
let arrows () = (code 39 - code 37, code 38 - code 40)
let update (e : Browser.KeyboardEvent, pressed) =
  let keyCode = int e.keyCode
  let op =  if pressed then Set.add else Set.remove
  keysPressed <- op keyCode keysPressed
  null
let init () =
  Browser.window.addEventListener_keydown(fun e -> update(e, true))
  Browser.window.addEventListener_keyup(fun e -> update(e, false))