// ---
// header: Ozmo
// tagline: Using HTML5 canvas
// ---

#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

let incr r = r := !r + 1
let decr r = r := !r - 1
let min a b = if a > b then b else a
let max a b = if a > b then a else b

[<Emit("Math.random()")>]
let random (): float = failwith "JS only"

module Keyboard =
  let mutable keysPressed = Set.empty
  let code x = if keysPressed.Contains(x) then 1 else 0
  let arrows () = (code 39 - code 37, code 38 - code 40)
  let update (e : KeyboardEvent, pressed) =
      let keyCode = int e.keyCode
      let op =  if pressed then Set.add else Set.remove
      keysPressed <- op keyCode keysPressed
  let init () =
      window.addEventListener_keydown(fun e -> update(e, true); null)
      window.addEventListener_keyup(fun e -> update(e, false); null)  

let width = 1024.
let height = 768.
let floorHeight = 200.
let atmosHeight = 300.

let drawGrd (ctx:CanvasRenderingContext2D) (canvas:HTMLCanvasElement) (y0,y1) (c0,c1) =
   let grd = ctx.createLinearGradient(0.,y0,0.,y1)
   grd.addColorStop(0.,c0)
   grd.addColorStop(1.,c1)
   ctx.fillStyle <- U3.Case2 grd
   ctx.fillRect(0.,y0, canvas.width, y1- y0)

let drawBg (ctx:CanvasRenderingContext2D) (canvas:HTMLCanvasElement) =
   drawGrd ctx canvas (0.,atmosHeight) ("yellow","orange")
   drawGrd ctx canvas (atmosHeight, canvas.height-floorHeight) ("grey","white")
   ctx.fillStyle <- U3.Case1 "black"
   ctx.fillRect(0.,canvas.height-floorHeight,canvas.width,floorHeight)

type Blob = { X:float; Y:float; Radius:float; vx:float; vy:float; color:string }

let drawBlob (ctx:CanvasRenderingContext2D) (canvas:HTMLCanvasElement) (blob:Blob) =
   ctx.beginPath();
   ctx.arc(blob.X, canvas.height - (blob.Y + floorHeight + blob.Radius), blob.Radius, 0., 2. * System.Math.PI, false);
   ctx.fillStyle <- U3.Case1 blob.color
   ctx.fill();
   ctx.lineWidth <- 3.
   ctx.strokeStyle <- U3.Case1 blob.color
   ctx.stroke();

let direct (dx,dy) (blob:Blob) =
   { blob with vx = blob.vx + (float dx)/4.0; vy = blob.vy }

let gravity (blob:Blob) =
   if blob.Y > 0. then { blob with vy = blob.vy - 0.1 }
   else blob

let bounce (blob:Blob) =
   let n = width
   if blob.X < 0. then { blob with X = -blob.X; vx = -blob.vx }
   elif (blob.X > n) then { blob with X = n - (blob.X - n); vx = -blob.vx }
   else blob

let move (blob:Blob) =
   { blob with X = blob.X + blob.vx; Y = max 0.0 (blob.Y + blob.vy) }

let step dir blob =
   blob |> direct dir |> move |> bounce

let collide (a:Blob) (b:Blob) =
   let dist = sqrt(((a.X - b.X)*(a.X - b.X)) + ((a.Y - b.Y)*(a.Y - b.Y)))
   dist < abs(a.Radius - b.Radius)

let absorb (blob:Blob) (drops:Blob list) =
   let drops = 
      drops |> List.filter (fun drop ->
         collide blob drop |> not
      )
   drops

let game (canvas: HTMLCanvasElement) (onCompleted) =

   let ctx = canvas.getContext_2d()
   canvas.width <- width
   canvas.height <- height

   let rand() = random()
   let floor x = floor x

   let newDrop color = {X = rand()*canvas.width*0.8 + (canvas.width*0.1); Y=600.; Radius=10.; vx=0.; vy = 0.0; color=color}
   let grow = "black"
   let shrink = "white"
   let newGrow () = newDrop grow
   let newShrink () = newDrop shrink

   let drops = ref [newGrow ()]
   let countdown = ref 0

   let rec update blob () =
      drawBg ctx canvas
      if !countdown > 0 
      then decr countdown
      elif floor(rand()*8.) = 0. then 
            let drop = if floor(rand()*3.) = 0. then newGrow() else newShrink()
            drops := drop::!drops
            countdown := 8
      let count color = !drops |> List.filter (fun drop -> drop.color = color) |> List.length
      let beforeGrow = count grow
      let beforeShrink = count shrink
      drops := !drops |> List.map (gravity >> move) |> (absorb blob)
      let afterGrow = count grow
      let blob = { blob with Radius = blob.Radius + float (beforeGrow - afterGrow) *4. }
      let afterShrink = count shrink
      let blob = { blob with Radius = max 5.0 (blob.Radius - float (beforeShrink - afterShrink) * 4.) }
      drops := !drops |> List.filter (fun blob -> blob.Y > 0.)
      for drop in !drops do drawBlob ctx canvas drop
      let blob = blob |> step (Keyboard.arrows())
      drawBlob ctx canvas blob
      if blob.Radius > 150. then onCompleted()
      else window.setTimeout(update blob, 1000. / 60.) |> ignore

   let blob = {X = 300.; Y=0.; Radius=50.; vx=0.; vy=0.; color="black" }
   update blob ()

let main () =
   Keyboard.init()

   let canvas = document.getElementsByTagName_canvas().[0.]
   let ctx = canvas.getContext_2d()

   let drawText(text,x,y) =
      ctx.fillStyle <- U3.Case1 "white"
      ctx.font <- "bold 40pt";
      ctx.fillText(text, x, y)

   let rec onCompleted () =
      drawText ("COMPLETED",320.,300.)
      window.setTimeout((fun () -> game canvas onCompleted), 10000.) |> ignore 

   game canvas onCompleted

do main ()