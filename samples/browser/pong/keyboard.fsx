#r "../../../build/fable/Fable.Core.dll"
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

    /// code 38 = up, code 40 = down
    let rightControlsPressed() = (code 38, code 40)

    let spacePressed() = code 32

    let init () =
      document.addEventListener_keydown(fun e -> update(e, true))
      document.addEventListener_keyup(fun e -> update(e, false))