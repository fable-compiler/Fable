module Pacman.App

open Fable.Import
open Fable.Core.JsInterop

open Pacman.Images
open Pacman.Keyboard
open Pacman.Types
open Pacman.Sound

(**
Here we define the maze, tile bits and blank block. The maze is defined as one big string
using ASCII-art encoding. Where `/`, `7`, `L` and `J` represent corners (upper-left, upper-right,
lower-left and lower-right), `!`, `|`, `-` and `_` represent walls (left, right, top, bottom) while
`o` and `.` represent two kinds of pills in the maze.
*)

let maze = ("\
##/------------7/------------7##,\
##|............|!............|##,\
##|./__7./___7.|!./___7./__7.|##,\
##|o|  !.|   !.|!.|   !.|  !o|##,\
##|.L--J.L---J.LJ.L---J.L--J.|##,\
##|..........................|##,\
##|./__7./7./______7./7./__7.|##,\
##|.L--J.|!.L--7/--J.|!.L--J.|##,\
##|......|!....|!....|!......|##,\
##L____7.|L__7 |! /__J!./____J##,\
#######!.|/--J LJ L--7!.|#######,\
#######!.|!          |!.|#######,\
#######!.|! /__==__7 |!.|#######,\
-------J.LJ |      ! LJ.L-------,\
########.   | **** !   .########,\
_______7./7 |      ! /7./_______,\
#######!.|! L______J |!.|#######,\
#######!.|!          |!.|#######,\
#######!.|! /______7 |!.|#######,\
##/----J.LJ L--7/--J LJ.L----7##,\
##|............|!............|##,\
##|./__7./___7.|!./___7./__7.|##,\
##|.L-7!.L---J.LJ.L---J.|/-J.|##,\
##|o..|!.......<>.......|!..o|##,\
##L_7.|!./7./______7./7.|!./_J##,\
##/-J.LJ.|!.L--7/--J.|!.LJ.L-7##,\
##|......|!....|!....|!......|##,\
##|./____JL__7.|!./__JL____7.|##,\
##|.L--------J.LJ.L--------J.|##,\
##|..........................|##,\
##L--------------------------J##").Split(',')


let tileBits =
 [| [|0b00000000;0b00000000;0b00000000;
      0b00000000;0b00000011;0b00000100;
      0b00001000;0b00001000|]

    [|0b00000000;0b00000000;0b00000000;0b00000000;0b11111111;0b00000000;0b00000000;0b00000000|] // top
    [|0b00000000;0b00000000;0b00000000;0b00000000;0b11000000;0b00100000;0b00010000;0b00010000|] // tr
    [|0b00001000;0b00001000;0b00001000;0b00001000;0b00001000;0b00001000;0b00001000;0b00001000|] // left
    [|0b00010000;0b00010000;0b00010000;0b00010000;0b00010000;0b00010000;0b00010000;0b00010000|] // right
    [|0b00001000;0b00001000;0b00000100;0b00000011;0b00000000;0b00000000;0b00000000;0b00000000|] // bl
    [|0b00000000;0b00000000;0b00000000;0b11111111;0b00000000;0b00000000;0b00000000;0b00000000|] // bottom
    [|0b00010000;0b00010000;0b00100000;0b11000000;0b00000000;0b00000000;0b00000000;0b00000000|] // br
    [|0b00000000;0b00000000;0b00000000;0b00000000;0b11111111;0b00000000;0b00000000;0b00000000|] // door
    [|0b00000000;0b00000000;0b00000000;0b00011000;0b00011000;0b00000000;0b00000000;0b00000000|] // pill
    [|0b00000000;0b00011000;0b00111100;0b01111110;0b01111110;0b00111100;0b00011000;0b00000000|] // power
 |]

let blank =
  [| 0b00000000;0b00000000;0b00000000; 0b00000000;0b00000000;0b00000000;0b00000000;0b00000000 |]


(**
Check for walls:
The following functions parse the maze representation and check various properties of the maze.
Those are used for rendering, but also for checking whether Pacman can go in a given direction.
Characters _|!/7LJ represent different walls
*) 
let isWall (c:char) =
  "_|!/7LJ-".IndexOf(c) <> -1

/// Returns ' ' for positions outside of range
let tileAt (x,y) =
  if x < 0 || x > 30 then ' ' else maze.[y].[x]

/// Is the maze tile at x,y a wall?
let isWallAt (x,y) =
  tileAt(x,y) |> isWall

// Is Pacman at a point where it can turn?
let verticallyAligned (x,y) =  (x % 8) = 5
let horizontallyAligned (x,y) = (y % 8) = 5
let isAligned n = (n % 8) = 5


// Check whether Pacman can go in given direction
let noWall (x,y) (ex,ey) =
  let bx, by = (x+6+ex) >>> 3, (y+6+ey) >>> 3
  isWallAt (bx,by) |> not

let canGoUp (x,y) = isAligned x && noWall (x,y) (0,-4)
let canGoDown (x,y) = isAligned x && noWall (x,y) (0,5)
let canGoLeft (x,y) = isAligned y && noWall (x,y) (-4,0)
let canGoRight (x,y) = isAligned y && noWall (x,y) (5,0)


(**
Background rendering
================================
To render the background, we first fill the background
and then iterate over the string lines that represent the maze and we draw images of
walls specified in the `tileBits` value earlier (or use `blank` tile for all other characters).

The following is used to map from tile characters to the `tileBits` values and to draw individual lines:
*)
let tileColors = "BBBBBBBBBYY"
let tileChars =  "/_7|!L-J=.o"

/// Returns tile for a given Maze character
let toTile (c:char) =
  let i = tileChars.IndexOf(c)
  if i = -1 then blank, 'B'
  else tileBits.[i], tileColors.[i]

/// Draw the lines specified by a wall tile
let draw f (lines:int[]) =
  let width = 8
  lines |> Array.iteri (fun y line ->
    for x = 0 to width-1 do
      let bit = (1 <<< (width - 1 - x))
      let pattern = line &&& bit
      if pattern <> 0 then f (x,y) )  

/// Creates a brush for rendering the given RGBA color
let createBrush (context:Browser.CanvasRenderingContext2D) (r,g,b,a) =
  let id = context.createImageData(!^ 1.0, 1.0)
  let d = id.data
  d.[0] <- float r; d.[1] <- float g
  d.[2] <- float b; d.[3] <- float a
  id

(**
The main function for rendering background just fills the canvas with a black color and
then iterates over the maze tiles and renders individual walls:
*)
let createBackground () =
  // Fill background with black
  let background = Browser.document.createElement_canvas()
  background.width <- 256.
  background.height <- 256.
  let context = background.getContext_2d()
  context.fillStyle <- !^ "rgb(0,0,0)"
  context.fillRect (0., 0. , 256., 256.);

  // Render individual tiles of the maze
  let blue = createBrush context (63,63,255,255)
  let yellow = createBrush context (255,255,0,255)
  let lines = maze
  for y = 0 to lines.Length-1 do
    let line = lines.[y]
    for x = 0 to line.Length-1 do
      let c = line.[x]
      let tile, color = toTile c
      let brush = match color with 'Y' -> yellow | _ -> blue
      let f (x',y') =
        context.putImageData
          (brush, float (x*8 + x'), float (y*8 + y'))
      draw f tile
  background

/// Clear whatever is rendered in the specified Maze cell
let clearCell (background : Browser.HTMLCanvasElement) (x,y) =
  let context = background.getContext_2d()
  context.fillStyle <- !^ "rgb(0,0,0)"
  context.fillRect (float (x*8), float (y*8), 8., 8.)

let createGhosts context =
  [| Images.redd, (16, 11), (1,0)
     Images.cyand, (14, 15), (1,0)
     Images.pinkd, (16, 13), (0,-1)
     Images.oranged, (18, 15), (-1,0) |]
  |> Array.map (fun (data,(x,y),v) ->
        Ghost(Images.createImage data, (x*8)-7, (y*8)-3, v) )

(**
Generating Ghost movement
=========================
For generating Ghost movements, we need an implementation of the [Flood fill algorithm](https://en.wikipedia.org/wiki/Flood_fill),
which we use to generate the shortest path home when Ghosts are returning. The `fillValue` function does this, by starting
at a specified location (which can be one of the directions in which ghosts can go).
*)

/// Recursive flood fill function
let flood canFill fill (x,y) =
  let rec f n = function
    | [] -> ()
    | ps ->
        let ps = ps |> List.filter (fun (x,y) -> canFill (x,y))
        ps |> List.iter (fun (x,y) -> fill (x,y,n))
        ps |> List.collect (fun (x,y) ->
            [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]) |> f (n+1)
  f 0 [(x,y)]

/// Possible routes that take the ghost home
let homeRoute =
  let numbers =
    maze |> Array.map (fun line ->
      line.ToCharArray()
      |> Array.map (fun c -> if isWall c then 999 else -1) )
  let canFill (x:int,y:int) =
    y>=0 && y < (numbers.Length-1) &&
    x>=0 && x < (numbers.[y].Length-1) &&
    numbers.[y].[x] = -1
  let fill (x,y,n) = numbers.[y].[x] <- n
  flood canFill fill (16,15)
  numbers

/// Find the shortest way home from specified location
/// (adjusted by offset in which ghosts start)
let fillValue (x,y) (ex,ey) =
  let bx = int (floor(float ((x+6+ex)/8)))
  let by = int (floor(float ((y+6+ey)/8)))
  homeRoute.[by].[bx]


let fillUp (x,y) = fillValue (x,y) (0,-4)
let fillDown (x,y) = fillValue (x,y) (0,5)
let fillLeft (x,y) = fillValue (x,y) (-4,0)
let fillRight (x,y) = fillValue (x,y) (5,0)



(**
When choosing a direction, ghosts that are returning will go in the direction
that leads them home. Other ghosts generate a list of possible directions (the `directions` array)
and then filter those that are in the direction of Pacman and choose one of the options. If they
are stuck and cannot go in any way, they stay where they are.
*)
let chooseDirection (ghost:Ghost) =
  let x,y = ghost.X, ghost.Y
  let dx,dy = ghost.V
  // Are we facing towards the given point?
  let isBackwards (a,b) =
    (a <> 0 && a = -dx) || (b <> 0 && b = -dy)
  // Generate array with possible directions
  let directions =
    [|if canGoLeft(x,y) then yield (-1,0), fillLeft(x,y)
      if canGoDown(x,y) then yield (0,1), fillDown(x,y)
      if canGoRight(x,y) then yield (1,0), fillRight(x,y)
      if canGoUp(x,y) then yield (0,-1), fillUp(x,y) |]

  if ghost.IsReturning then
    // Returning ghosts find the shortest way home
    let xs = directions |> Array.sortBy snd
    let v, n = xs.[0]
    if n = 0 then ghost.IsReturning <- false
    v
  else
    // Other ghosts pick one direction twoards Pacman
    let xs =
      directions
      |> Array.map fst
      |> Array.filter (not << isBackwards)
    if xs.Length = 0 then 0, 0
    else
      let randomNum = System.Random().NextDouble()
      let i = randomNum * float xs.Length
      xs.[int (floor i)]


/// Count number of dots in the maze
let countDots () =
  maze |> Array.sumBy (fun line ->
    line.ToCharArray()
    |> Array.sumBy (function '.' -> 1 | 'o' -> 1 | _ -> 0))


(**
## The game play function

Most of the Pacman game logic is wrapped in the top-level `playLevel` function. This takes two functions - that are called
when the game completes - and then it initializes the world state and runs in a loop until the end of the game.
The following outlines the structure of the function:

    let playLevel (onLevelCompleted, onGameOver) =
      // (Create canvas, background and ghosts)
      // (Define the Pacman state)
      // (Move ghosts and Pacman)
      // (Detect pills and collisiions)
      // (Rendering everything in the game)
      let rec update () =
        logic ()
        render ()
        if !dotsLeft = 0 then onLevelCompleted()
        elif !energy <= 0 then onGameOver()
        else window.setTimeout(update, 1000. / 60.) |> ignore

      update()

After defining all the helpers, the `update` function runs in a loop (via a timer) until there are no dots
left or until the Pacman is out of energy and then it calls one of the continuations.

In the following 5 sections, we'll look at the 5 blocks of code that define the body of the function.
*)

let playLevel (onLevelCompleted, onGameOver) =
  (**
  ### Create canvas, background and ghosts
  In the first part, the function finds the `<canvas>` element, paints it with black background and
  creates other graphical elements - namely the game background, ghosts and eyes:
  *)
  // Fill the canvas element
  let canvas = Browser.document.getElementsByTagName_canvas().[0]
  canvas.width <- 256.
  canvas.height <- 256.
  let context = canvas.getContext_2d()
  context.fillStyle <- !^ "rgb(0,0,0)"
  context.fillRect (0., 0. , 256., 256.);
  let bonusImages =
    [| createImage Images._200; createImage Images._400;
       createImage Images._800; createImage Images._1600 |]

  // Load images for rendering
  let background = createBackground()
  let ghosts = createGhosts(context)
  let blue,eyed = createImage Images.blue, createImage Images.eyed
  (**
  ### Define the Pacman state
  Next, we define the game state. Pacman game uses mutable state, so the following uses
  F# reference cells; `ref 0` creates a mutable cell containing `0`. Later, we will access
  the value by writing `!score` and mutate it by writing `score := !score + 1`.

  *)
  let pills = maze |> Array.map (fun line ->
    line.ToCharArray() |> Array.map id)
  let dotsLeft = ref (countDots())
  let score = ref 0
  let bonus = ref 0
  let bonuses = ref []
  let energy = ref 128
  let flashCountdown = ref 0
  let powerCountdown = ref 0
  let x, y = ref (16 * 8 - 7), ref (23 * 8 - 3)
  let v = ref (0,0)

  let moveGhosts () =
    ghosts |> Array.iter (fun ghost ->
      ghost.Move(chooseDirection ghost) 
    )

  let movePacman () =
    // In which directions should pacman go?
    let inputs =
       [| if Keyboard.isPressed 38 (*up*) then
            yield canGoUp (!x,!y), (0,-1)
          if Keyboard.isPressed 40 (*down*) then
            yield canGoDown (!x,!y), (0,1)
          if Keyboard.isPressed 37 (*left*) then
            yield canGoLeft (!x,!y), (-1,0)
          if Keyboard.isPressed 39 (*right*) then
            yield canGoRight (!x,!y), (1,0) |]
    // Can we continue in the same direction?
    let canGoForward =
      match !v with
      | 0,-1 -> canGoUp(!x,!y)
      | 0,1  -> canGoDown(!x,!y)
      | -1,0 -> canGoLeft(!x,!y)
      | 1, 0 -> canGoRight(!x,!y)
      | _ -> false
    // What new directions can we take?
    let availableDirections =
      inputs
      |> Array.filter fst
      |> Array.map snd
      |> Array.sortBy (fun v' -> v' = !v)
    if availableDirections.Length > 0 then
      // Choose the first one, prefers no change
      v := availableDirections.[0]
    elif inputs.Length = 0 || not canGoForward then
      // There are no options - stop
      v := 0,0

    // Update X and Y accordingly
    let x',y' = wrap (!x,!y) !v
    x := x'
    y := y'

  // Check if Pacman eats a pill at current cell
  let eatPills () =
    let tx = int (floor(float ((!x+6)/8)))
    let ty = int (floor(float ((!y+6)/8)))
    let c = pills.[ty].[tx]
    if c = '.' then
      // Eating a small pill increments the score
      pills.[ty].[tx] <- ' '
      clearCell background (tx,ty)
      score := !score + 10
      decr dotsLeft
      Sound.play "./fx/Dot5.wav"
    if c = 'o' then
      // Eating a large pill turns on the power mode
      pills.[ty].[tx] <- ' '
      clearCell background (tx,ty)
      bonus := 0
      score := !score + 50
      powerCountdown := 250
      decr dotsLeft
      Sound.play "./fx/Powerup.wav"

  /// Are there any ghosts that collide with Pacman?
  let touchingGhosts () =
    let px, py = !x, !y
    ghosts |> Array.filter (fun ghost ->
      let x,y = ghost.X, ghost.Y
      ((px >= x && px < x + 13) ||
       (x < px + 13 && x >= px)) &&
      ((py >= y && py < y + 13) ||
       (y < py + 13 && y >= py)) )
(**
The `collisionDetection` function implements the right response to collision with a ghost:
*)
  /// Handle collision detections between Pacman and ghosts
  let collisionDetection () =
    let touched = touchingGhosts ()
    if touched.Length > 0 then
      if !powerCountdown > 0 then
        // Pacman is eating ghosts!
        touched |> Array.iter (fun ghost ->
          if not ghost.IsReturning then
            Sound.play "./fx/EatGhost.wav"
            ghost.IsReturning <- true
            let added = int (2. ** (float !bonus))
            score := !score + added * 200
            let image = bonusImages.[!bonus]
            bonuses := (100, (image, ghost.X, ghost.Y)) :: !bonuses
            bonus :=  min 3 (!bonus + 1) )
      else
        // Pacman loses energy when hitting ghosts
        decr energy
        if !flashCountdown = 0 then Sound.play "./fx/Hurt.wav"
        flashCountdown := 30
    if !flashCountdown > 0 then decr flashCountdown

  /// Updates bonus points
  let updateBonus () =
    let removals,remainders =
      !bonuses
      |> List.map (fun (count,x) -> count-1,x)
      |> List.partition (fst >> (=) 0)
    bonuses := remainders

(**
The logic is called from the following single `logic` function that includes all the checks:
*)
  let logic () =
    moveGhosts()
    movePacman()
    eatPills ()
    if !powerCountdown > 0 then decr powerCountdown
    collisionDetection()
    updateBonus ()
(**
### Rendering everything in the game

When rendering everything in the game, we first draw the background and then we render
individual components. Those include the score, remaining energy, pacman, ghosts and bonuses.
Each of those is handled by a single nested function that are put together in `render`.
We start with Pacman and remaining energy:
*)
  let renderPacman () =
    let p = Images.imageAt(x,y,v)
    if (!flashCountdown >>> 1) % 2 = 0
    then context.drawImage(!^ p, float !x, float !y)

  let renderEnergy () =
    context.fillStyle <- !^ "yellow"
    context.fillRect(120., 250., float !energy, 2.)
(**
The next three functions render ghosts, current score and bonuses:
*)
  let renderGhosts () =
    ghosts |> Array.iter (fun ghost ->
      let image =
        if ghost.IsReturning then eyed
        else
          if !powerCountdown = 0 then ghost.Image
          elif !powerCountdown > 100 ||
                ((!powerCountdown >>> 3) % 2) <> 0 then blue
          else ghost.Image
      context.drawImage(!^ image, float ghost.X, float ghost.Y) )

  let renderScore () =
    context.fillStyle <- !^ "white"
    context.font <- "bold 8px";
    context.fillText("Score " + (!score).ToString(), 0., 255.)

  let renderBonus () =
    !bonuses |> List.iter (fun (_,(image,x,y)) ->
      context.drawImage(!^ image, float x, float y))

  let render () =
    context.drawImage(!^ background, 0., 0.)
    renderScore ()
    renderEnergy ()
    renderPacman()
    renderGhosts ()
    renderBonus ()

  let rec update () =
    logic ()
    render ()
    if !dotsLeft = 0 then onLevelCompleted()
    elif !energy <= 0 then onGameOver()
    else Browser.window.setTimeout(update, 1000. / 60.) |> ignore

  update()

(**
Game entry point
================
Now we have everything we need to start the game, so the last step is to define the
`levelCompleted` and `gameOver` functions (that are called when the game ends), render
the starting state of the game (with "CLICK TO START" text) and start the game!
*)
let rec game () =
  // Initialize keyboard and canvas
  Keyboard.reset()
  let canvas = Browser.document.getElementsByTagName_canvas().[0]
  let context = canvas.getContext_2d()

  // A helper function to draw text
  let drawText(text,x,y) =
    context.fillStyle <- !^ "white"
    context.font <- "bold 8px";
    context.fillText(text, x, y)

  // Called when level is completed
  let levelCompleted () =
    drawText("COMPLETED",96.,96.)
    Browser.window.setTimeout((fun () -> game()),5000.) |> ignore
  // Called when the game ends
  let gameOver () =
    drawText("GAME OVER",96.,96.)
    Browser.window.setTimeout((fun () -> game()),5000.) |> ignore

  // Start a new game after click!
  let start () =
    let background = createBackground()
    context.drawImage(!^ background, 0., 0.)
    context.fillStyle <- !^ "white"
    context.font <- "bold 8px";
    drawText("CLICK TO START", 88., 96.)
    canvas.onclick <- (fun e ->
      canvas.onclick <- null
      playLevel (levelCompleted, gameOver)
      box true )

  // Resize canvas and get ready for a game
  let canvas = Browser.document.getElementsByTagName_canvas().[0]
  canvas.width <- 256.
  canvas.height <- 256.
  start()

// At the beginning, initialize keyboard & start the first game.
Keyboard.init ()
game ()
