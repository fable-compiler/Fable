module Pacman.Keyboard 

open Fable.Import

/// Set of currently pressed keys
let mutable keysPressed = Set.empty
/// Update the keys as requested
let reset () = keysPressed <- Set.empty
let isPressed keyCode = Set.contains keyCode keysPressed

/// Triggered when key is pressed/released
let update (e : Browser.KeyboardEvent, pressed) =
  let keyCode = int e.keyCode
  let op =  if pressed then Set.add else Set.remove
  keysPressed <- op keyCode keysPressed
  null

/// Register DOM event handlers
let init () =
  Browser.window.addEventListener_keydown(fun e -> update(e, true))
  Browser.window.addEventListener_keyup(fun e -> update(e, false))