module Ozmo.Main

open Fable.Import
open Fable.Core.JsInterop

/// The width of the canvas
let width = 900.
/// The height of the canvas
let height = 668.
/// Height of the floor - the bottom black part
let floorHeight = 100.
/// Height of the atmosphere - the yellow gradient
let atmosHeight = 300.


Keyboard.init()

let canvas = Browser.document.getElementsByTagName_canvas().[0]
let ctx = canvas.getContext_2d()
canvas.width <- width
canvas.height <- height


/// Draw gradient between two Y offsets and two colours
let drawGrd (ctx:Browser.CanvasRenderingContext2D)
    (canvas:Browser.HTMLCanvasElement) (y0,y1) (c0,c1) =
  let grd = ctx.createLinearGradient(0.,y0,0.,y1)
  grd.addColorStop(0.,c0)
  grd.addColorStop(1.,c1)
  ctx.fillStyle <- !^ grd
  ctx.fillRect(0.,y0, canvas.width, y1- y0)


/// Draw background of the Ozmo game
let drawBg ctx canvas =
  drawGrd ctx canvas
    (0.,atmosHeight) ("yellow","orange")
  drawGrd ctx canvas
    (atmosHeight, canvas.height-floorHeight)
    ("grey","white")
  ctx.fillStyle <- !^ "black"
  ctx.fillRect
    ( 0.,canvas.height-floorHeight,
      canvas.width,floorHeight )

/// Draw the specified text (when game finishes)
let drawText(text,x,y) =
  ctx.fillStyle <- !^ "white"
  ctx.font <- "bold 40pt";
  ctx.fillText(text, x, y)


type Blob =
  { X:float; Y:float;
    vx:float; vy:float;
    Radius:float; color:string }

let drawBlob (ctx:Browser.CanvasRenderingContext2D)
    (canvas:Browser.HTMLCanvasElement) (blob:Blob) =
  ctx.beginPath()
  ctx.arc
    ( blob.X, canvas.height - (blob.Y + floorHeight + blob.Radius),
      blob.Radius, 0., 2. * System.Math.PI, false )
  ctx.fillStyle <- !^ blob.color
  ctx.fill()
  ctx.lineWidth <- 3.
  ctx.strokeStyle <- !^ blob.color
  ctx.stroke()


/// Apply key effects on Player's blob - changes X speed
let direct (dx,dy) (blob:Blob) =
  { blob with vx = blob.vx + (float dx)/4.0 }

/// Apply gravity on falling blobs - gets faster every step
let gravity (blob:Blob) =
  if blob.Y > 0. then { blob with vy = blob.vy - 0.1 }
  else blob

/// Bounde Player's blob off the wall if it hits it
let bounce (blob:Blob) =
  let n = width
  if blob.X < 0. then
    { blob with X = -blob.X; vx = -blob.vx }
  elif (blob.X > n) then
    { blob with X = n - (blob.X - n); vx = -blob.vx }
  else blob


/// Move blob by one step - adds X and Y
/// velocities to the X and Y coordinates
let move (blob:Blob) =
  { blob with
      X = blob.X + blob.vx
      Y = max 0.0 (blob.Y + blob.vy) }

/// Apply step on Player's blob. Composes above functions.
let step dir blob =
  blob |> direct dir |> move |> bounce

/// Check whether two blobs collide
let collide (a:Blob) (b:Blob) =
  let dx = (a.X - b.X)*(a.X - b.X)
  let dy = (a.Y - b.Y)*(a.Y - b.Y)
  let dist = sqrt(dx + dy)
  dist < abs(a.Radius - b.Radius)

/// Remove all falling blobs that hit Player's blob
let absorb (blob:Blob) (drops:Blob list) =
  drops |> List.filter (fun drop ->
    collide blob drop |> not )


// Game helpers
// =============

let grow = "black"
let shrink = "white"

let newDrop color =
  { X = JS.Math.random()*width*0.8 + (width*0.1)
    Y=600.; Radius=10.; vx=0.; vy = 0.0
    color=color }
let newGrow () = newDrop grow
let newShrink () = newDrop shrink

/// Update drops and countdown in each step
let updateDrops drops countdown =
  if countdown > 0 then
    drops, countdown - 1
  elif floor(JS.Math.random()*8.) = 0. then
    let drop =
      if floor(JS.Math.random()*3.) = 0. then newGrow()
      else newShrink()
    drop::drops, 8
  else drops, countdown


/// Count growing and shrinking drops in the list
let countDrops drops =
  let count color =
    drops
    |> List.filter (fun drop -> drop.color = color)
    |> List.length
  count grow, count shrink

// Asynchronous game loop
// ========================

let rec game () = async {
  let blob =
    { X = 300.; Y=0.; Radius=50.;
      vx=0.; vy=0.; color="black" }
  return! update blob [newGrow ()] 0 }

and completed () = async {
  drawText ("COMPLETED",320.,300.)
  do! Async.Sleep 10000
  return! game () }

/// Keeps current state for Player's blob, falling
/// drops and the countdown since last drop was generated
and update blob drops countdown = async {
  // Update the drops & countdown
  let drops, countdown = updateDrops drops countdown

  // Count drops, apply physics and count them again
  let beforeGrow, beforeShrink = countDrops drops
  let drops =
    drops
    |> List.map (gravity >> move)
    |> absorb blob
  let afterGrow, afterShrink = countDrops drops
  let drops = drops |> List.filter (fun blob -> blob.Y > 0.)

  // Calculate new player's size based on absorbed drops
  let radius = blob.Radius + float (beforeGrow - afterGrow) *4.
  let radius = radius - float (beforeShrink - afterShrink) * 4.
  let radius = max 5.0 radius

  // Update radius and apply keyboard events
  let blob = { blob with Radius = radius }
  let blob = blob |> step (Keyboard.arrows())

  // Render the new game state
  drawBg ctx canvas
  for drop in drops do drawBlob ctx canvas drop
  drawBlob ctx canvas blob

  // If the game completed, switch state
  // otherwise sleep and update recursively!
  if blob.Radius > 150. then
    return! completed()
  else
    do! Async.Sleep(int (1000. / 60.))
    return! update blob drops countdown }


game () |> Async.StartImmediate
