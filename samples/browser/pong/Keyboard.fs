module Keyboard

open Fable.Import.Browser

module Keyboard =
    let mutable keysPressed = Set.empty

    /// Returns 1 if key with given code is pressed
    let code x =
      if keysPressed.Contains(x) then 1 else 0

    /// Update the state of the set for given key event
    let update (e : KeyboardEvent, pressed) =
        let keyCode = int e.keyCode
        let op =  if pressed then Set.add else Set.remove
        keysPressed <- op keyCode keysPressed
        null

    /// code 87 = w, code 83 = s
    let leftControlsPressed() = (code 87, code 83)

    /// code 79 = o, code 76 = l
    let rightControlsPressed() = (code 79, code 76)

    let rKeyPressed() = code 82

    let init () =
      document.addEventListener_keydown(fun e -> update(e, true))
      document.addEventListener_keyup(fun e -> update(e, false))