// ---
// header: Mario
// tagline: Playable Mario with HTML5 Canvas
// popup-style: width:580px
// ---

#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core 
open Fable.Import.Browser

Fable.Import.Node.require.Invoke("core-js") |> ignore
/// Handles keydown and keyup events of the window and exposes them
/// using the 'arrows' property (which is a tuple int*int with -1 if the
/// left/up key is pressed, 1 if right/down key is pressed and 0 otherwise)
module Keyboard =
  let mutable keysPressed = Set.empty
  let code x = if keysPressed.Contains(x) then 1 else 0
  let arrows () = (code 39 - code 37, code 38 - code 40)
  let update (e : KeyboardEvent, pressed) =
      let keyCode = int e.keyCode
      let op =  if pressed then Set.add else Set.remove
      keysPressed <- op keyCode keysPressed
  let init () =
      document.addEventListener_keydown(fun e -> update(e, true); null)
      document.addEventListener_keyup(fun e -> update(e, false); null)  

/// Basic functionality for creating and rendering window using HTML canvas
/// (the functions here fill the window, set position of image and create image)
module Window =
  let canvas = document.getElementsByTagName_canvas().[0.]
  let context = canvas.getContext_2d()

  let ($) s n = s + n.ToString()
  let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

  let filled color rect =
      let ctx = context  
      ctx.fillStyle <- U3.Case1 color
      ctx.fillRect rect    

  let position (x,y) (img : HTMLImageElement) =
      img.style.left <- x.ToString() + "px"
      img.style.top <- (canvas.offsetTop + y).ToString() + "px"

  let dimensions () = canvas.width, canvas.height

  let image (src:string) = 
      let image = document.getElementsByTagName_img().[0.]
      if image.src.IndexOf(src) = -1 then image.src <- src
      image

// ----------------------------------------------------------------------------
// The rest of the code contains the commented web site content
// ----------------------------------------------------------------------------

open Window

(**
Mario game sample, based on functional reactive [Elm script](http://elm-lang.org/edit/examples/Intermediate/Mario.elm).
The F# version is using HTML 5 canvas to render the background and an `img`
tag showing the Mario (using animated GIFs). To play the game, click the "Open sample"
button on the right and use keyboard keys to play!


## Mario step

We keep information about Mario in a single record type with fields that 
represent the current x and y coordinates (`x` and `y`), current velocity
(`vx` and `vy`) and the current direction `dir`. The direction is used to
pick the correct Mario image when rendering: 
*)

type Mario = { x:float; y:float; vx:float; vy:float; dir:string }

(**
The step function of the game takes previvous `Mario` value and returns a new 
one. It is composed from 4 functions that represent different aspects of the game.

The functions that depend on keyboard take the current keyboard state as the 
first argument. This is represented as a tuple `int*int` consisting of x and y
directions. For example, when the left key is pressed, the value is `(-1, 0)`.
*)

// If the Up key is pressed (y > 0) and Mario is on the ground, 
// then create Mario with the y velocity 'vy' set to 5
let jump (_,y) m = if y > 0 && m.y = 0. then  { m with vy = 5. } else m
// If Mario is in the air, then his "up" velocity is decreasing
let gravity m = if m.y > 0. then { m with vy = m.vy - 0.1 } else m
// Apply physics - move Mario according to the current velocities
let physics m = { m with x = m.x + m.vx; y = max 0. (m.y + m.vy) }
// When Left or Right keys are pressed, change the 'vx' velocity and direction
let walk (x,_) m = 
    { m with vx = float x 
             dir = if x < 0 then "left" elif x > 0 then "right" else m.dir }

(**
The `step` function takes a `dir` parameter representing the keyboard status
and a current `Mario` state. It simply runs 4 components in a pipeline:
*)

let step dir mario = mario |> physics |> walk dir |> gravity |> jump dir

(**

## Rendering Mario

Now we're ready to render Mario using HTML 5 canvas! To do that, we need the 
width and height of the canvas and the current state of Mario. The following
function fills the bottom half of the canvas with green, upper half with blue
and then chooses the right Mario image:

*)

/// Render mario on canvas 
let render (w,h) (mario:Mario) =
    // Render background
    (0., 0., w, h) |> filled (rgb 174 238 238)
    (0., h-50., w, 50.) |> filled (rgb 74 163 41)
    // Select and position Mario
    // (walking is represented as an animated gif)
    let verb =
        if mario.y > 0. then "jump"
        elif mario.vx <> 0. then "walk"
        else "stand"
    "./images/mario" + verb + mario.dir + ".gif" |> image 
    |> position (w/2.-16.+mario.x,  h-50.-31.-mario.y)

(**

## Driving the game 

The last thing that needs to be done is to write the `main` function that drives
the game. The function does some initialization and then starts a recursive `update` 
function that calculates a new game state using `step` and renders it in a loop:
*)

// Some initialization
Keyboard.init()
let w,h = dimensions()
// Recursive function that updates the state & renders it
let rec update mario () =
    let mario = mario |> step (Keyboard.arrows())
    render (w,h) mario      
    window.setTimeout(update mario, 1000. / 60.) |> ignore
// Start the game with Mario in the center
let mario = { x=0.; y=0.; vx=0.; vy=0.; dir="right" }
update mario ()