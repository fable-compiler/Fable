// ---
// header: Pacman
// tagline: Pacman finds himself in a grid filled with monsters...
// popup-style: width:400px
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

(** 

Pacman finds himself in a grid filled with monsters. Will he be able to eat all 
the dots on the board before the monsters eat him? 

 * Click the "Open sample" to play the game!
 * Browse the [source code](https://github.com/ZachBray/FunScript/tree/master/Examples/Pacman) on GitHub...

*)

let cyand = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAiUlEQVQoU8WSURKAIAhE8Sh6Fc/tVfQoJdqiMDTVV4wfufAAmw3kxEHUz4pA1I8OJVjAKZZ6+XiC0ATTB/gW2mEFtlpHLqaktrQ6TxUQSRCAPX2AWPMLyM0VmPOcV8palxt6uoAMpDjfWJt+o6cr0DPDnfYjyL94NwIcYjXcR/FuYklcxrZ3OO0Ep4dJ/3dR5jcAAAAASUVORK5CYII="
let oranged = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAgklEQVQoU8WS0RGAIAxDZRRYhblZBUZBsBSaUk/9kj9CXlru4g7r1FxBdsFpGwoa2NwrYIFPEIeM6QS+hQQMYC70EjzuuOlt6gT5kRGGTf0Cx5qfwJYOYIw0L6W1bg+09Al2wAcCS8Y/WjqAZhluxD/B3ghZBO6n1sadzLLEbNSg8pzXIVLvbNvPwAAAAABJRU5ErkJggg=="
let pinkd = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAj0lEQVQoU8WSsRWAIAxEZRQpXITGVZzIVWxYxAJHwRfwMInxqZV0XPIvgXeuM05eUuayG73TbULQwKWZGTTwCYIJphfwLcRhAW5DLfWrXFLrNLWBKAIBbOkFxJpfQDIXYAh1XoznumRo6Q0kwE8VTLN8o6UL0ArDnfYjSF/Mg4CEaA330sxD3ApHLvUdSdsBdgNkr9L8gxYAAAAASUVORK5CYII="
let redd = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAkklEQVQoU8WSvRWAIAyEZRQtXIRCV3EiVtGCRSx0FHxBD5MYn1pJl0u+/PDOVcZLY5e47PrJ6TIhaOBSzBoU8AlCE0zP4FuIwwJc25Bz9TyILbVOUwuIJAjAlp5BrPkFpOYC9H6fF+O5LjW09AIS0Az7jUuQN1q6AC0z3Gk/gvTF3AhwiNYQ52Ju4pI4fKljOG0DA3tp97vN6C8AAAAASUVORK5CYII="
let pu1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAWElEQVQoU62SUQoAIAhD9f6HNiYYolYi9VfzuXIxDRYbI0LCTHsfe3ldi3BgRRUY9Rnku1Rupf4NgiPeVjVU7STckphBceSvrHHtNPI21HWz4NO3eUUAgwVpmjX/zwK8KQAAAABJRU5ErkJggg=="
let pu2 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAW0lEQVQoU8WSwQoAIAhD9f8/2lIwdKRIl7o1e010THBESJiJXca76qnoDxFC3SD9LRpWkLnsLt4gdImtlLX/EK4iDapqr4VuI2+BauQjaOrmSz8xillDp5gQrS054jv/0fkNVAAAAABJRU5ErkJggg=="
let pd1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAXElEQVQoU62SUQoAIAhD9f6HNgyMWpMs6k/XU5mqwDMTw5yq6JwbAfucwR2qAFHAu75BN11Gt6+Qz54VpMJsMV3BaS9UR8txkUzfLC9DUY0BYbOPGfpyU3g2WdwAOvU1/9KZsT4AAAAASUVORK5CYII="
let pd2 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAU0lEQVQoU62SUQoAIAhD9f6HNgwUGw4s6q/pc6KqwDMTQ01VtGr56ZIZvKEJEAXc9Q26cUm3r5D3zgrywHeoG3ldJrZIRz6C0I1BoR83FTBCeHsLIlw7/wOkQycAAAAASUVORK5CYII="
let pl1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAVUlEQVQoU62S2woAIAhD9f8/2jAwvGRMyDfF49iQKZUISZ4xE/vZaW7LHbwhBLADqjpSUjBAdglRDQa9hxfcQi+vf5RGnpDlkB4KlMgR0N6pBIH83gIPFCb/N+MLCwAAAABJRU5ErkJggg=="
let pl2 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAUklEQVQoU52SUQoAIAhD3f0PbRQoZgnT/hyttYeQdFRFswYIoubD73JlPibGYA/s1Jmpk+JpDIinWxbiXP3iQslCwbhTxzhHbsWZNFsnCkTevQW2bCb/VRTuVwAAAABJRU5ErkJggg=="
let pr1 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAWElEQVQoU52S4Q4AIASE3fs/tKalSTHyL/O5CyAXzMQ+BxBsbj9exRE8oQqgDUS1BalNVFSuP2WQL94WIygCBEzttZWOvbz2VBnGtLXg1sgV/L8I679yewN9sScO5wcxLQAAAABJRU5ErkJggg=="
let pr2 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA0AAAANCAYAAABy6+R8AAAAVElEQVQoU62SWwoAIAgE9f6HNgqU3BK2R3+J48KoCjwzMaypis61+OyaK3hADOADeuoddJISaQy0iKggbEz2viah7mVPTNq7cp/ApLmcdFPVdaDJBnWdJwjk629HAAAAAElFTkSuQmCC"
let blue = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAeklEQVQoU62S0Q3AIAhEyyi6UcfoRB2jG+koNkeCoVcaTaw/huMeEkS24KTUmpdrFWHbQ2CAzb5AB0eQFTFYwVnIw/+B5by0cD52vTmGhnaF25wBAb/A6HsibR0ctch5fRHi1zCigvCut4oR+wnbhrBmsZr9DlqCQfbcnfZjDyiZqCEAAAAASUVORK5CYII="
let eyed = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAUElEQVQoU2NkIBMwkqmPYYA13rt37z/I6UpKSiguwSYOVwCThPkZphmXOHU0OjtD7Nu7F+FckI3YxFH8oqgI8eP9+6h+xCY+wNFBSiqiv1MBDgYsD185vj8AAAAASUVORK5CYII="
let _200 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAS0lEQVQoU2NkIBMwkqmPYYA0vpVR+Q9zsvCTO4yE+CC1KE4FaYBpxEfDNWKzgWiNIIUw5xKyGa+N+PyM4UdS4nSA4pEUJ8LUku1UAMC0VA8iscBNAAAAAElFTkSuQmCC"
let _400 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAASElEQVQoU2NkIBMwkqmPYYA0vpVR+S/85A4jMg3zAkwcmQ9ig52KTSO6Qch8FI3oNhClEaaJWJvhNmLTSJQfyYnLAYpHujoVAChTXA9pVJi5AAAAAElFTkSuQmCC"
let _800 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAQElEQVQoU2NkIBMwkqmPYYA0vpVR+Q9zsvCTO4yE+CC1YKeCFMI0EEOjaES3EZ8BtLERn5/hNpITlwMUj3R1KgCe5lwPHtUmcwAAAABJRU5ErkJggg=="
let _1600 = "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAOCAYAAAAfSC3RAAAAQ0lEQVQoU2NkIBMwkqmPYQA0vpVR+S/85A4jiIY5mxg+WANMIYiGaUYXR+ejaES3EdlAvBrxKSTJRnx+HoDoGDopBwDHLGwPAhDgRQAAAABJRU5ErkJggg=="

(**

This is a full blown Pacman game. If you're looking for an introduction to FunScript,
then visit other tutorials, in particular the [Mario game](../mario/index.html) which
is much simpler. 

## Images, mazes and graphics

Some of the graphics, maze structure and walls are defined as embedded strings or
arrays in the following section, so that the game is stand-alone and easily portable.

*)

// Create image using the specified data
let createImage data =
  let img = document.createElement_img()
  img.src <- data
  img

// Define the structure of the maze using ASCII
let maze = ("\
##/------------7/------------7##,\
##|............|!............|##,\
##|./__7./___7.|!./___7./__7.|##,\
##|o|  !.|   !.|!.|   !.|  !o|##," + (*[omit:(...)]*)"\
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
##L--------------------------J##" (*[/omit]*)).Split(',')

let tileBits = [| (*[omit:(...)]*)
  [|0b00000000;0b00000000;0b00000000;0b00000000;0b00000011;0b00000100;0b00001000;0b00001000|] // tl
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
  (*[/omit]*) |]

let blank = [| (*[omit:(...)]*) 0b00000000;0b00000000;0b00000000;0b00000000;0b00000000;0b00000000;0b00000000;0b00000000 (*[/omit]*)|]

(*** hide ***)
// [<AutoOpen>]
// module Maze =

(** 
## Maze functionality
*)

let isWall (c:char) = "_|!/7LJ-".IndexOf(c) <> -1

let tileAt (x,y) = if x < 0 || x > 30 then ' ' else maze.[y].[x]
    
let isWallAt (x,y) = tileAt(x,y) |> isWall

let noWall (x,y) (ex,ey) =
  let bx, by = (x+6+ex) >>> 3, (y+6+ey) >>> 3
  isWallAt (bx,by) |> not

let verticallyAligned (x,y) =  (x % 8) = 5
let horizontallyAligned (x,y) = (y % 8) = 5
let isAligned n = (n % 8) = 5

let canGoUp (x,y) = isAligned x && noWall (x,y) (0,-4)
let canGoDown (x,y) = isAligned x && noWall (x,y) (0,5)
let canGoLeft (x,y) = isAligned y && noWall (x,y) (-4,0)
let canGoRight (x,y) = isAligned y && noWall (x,y) (5,0)

(*** hide ***)
// [<AutoOpen>]
// module Background =

(** 
## Background rendering
*)

let tileColors = "BBBBBBBBBYY"
let tileChars =  "/_7|!L-J=.o"

let toTile (c:char) =
  let i = tileChars.IndexOf(c)
  if i = -1 then blank, 'B'
  else tileBits.[i], tileColors.[i]

let draw f (lines:int[]) =
  let width = 8   
  lines |> Array.iteri (fun y line ->
    for x = 0 to width-1 do
      let bit = (1 <<< (width - 1 - x))
      let pattern = line &&& bit
      if pattern <> 0 then f (x,y)
  )

let createBrush (context:CanvasRenderingContext2D) (r,g,b,a) =
  let id = context.createImageData(U2.Case1 1.0, 1.0)
  let d = id.data
  d.[0.] <- float r; d.[1.] <- float g
  d.[2.] <- float b; d.[3.] <- float a
  id

let createBackground () =
  let background = document.createElement_canvas() 
  background.width <- 256.
  background.height <- 256.  
  let context = background.getContext_2d()
  context.fillStyle <- U3.Case1 "rgb(0,0,0)"
  context.fillRect (0., 0. , 256., 256.);
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

let countDots () =
  maze |> Array.sumBy (fun line -> 
    line.ToCharArray() 
    |> Array.sumBy (function '.' -> 1 | 'o' -> 1 | _ -> 0))

let clearCell (background : HTMLCanvasElement) (x,y) =
  let context = background.getContext_2d()
  context.fillStyle <- U3.Case1 "rgb(0,0,0)"
  context.fillRect (float (x*8), float (y*8), 8., 8.);

(** 
## Ghosts
*)

let wrap (x,y) (dx,dy) =
  let x = 
    if dx = -1 && x = 0 then 30 * 8
    elif dx = 1  && x = 30 *8 then 0
    else x
  x + dx, y + dy

type Ghost(image:HTMLImageElement,x,y,v) =
  let mutable x' = x
  let mutable y' = y
  let mutable v' = v
  member val Image = image
  member val IsReturning = false with get, set
  member __.X = x'
  member __.Y = y' 
  member __.V = v'
  member ghost.Reset() =
    x' <- x
    y' <- y
  member ghost.Move(v) =
    v' <- v    
    let dx,dy = v
    let x,y = wrap (x',y') (dx,dy)
    x' <- x
    y' <- y

let createGhosts context = 
  [|
     redd, (16, 11), (1,0)
     cyand, (14, 15), (1,0)
     pinkd, (16, 13), (0,-1)
     oranged, (18, 15), (-1,0)
  |]
  |> Array.map (fun (data,(x,y),v) -> 
    Ghost(createImage data, (x*8)-7, (y*8)-3, v)    
  )

(** 
## Ghost movement
*)
let flood canFill fill (x,y) =
  let rec f n = function
    | [] -> ()
    | ps ->
      let ps = ps |> List.filter (fun (x,y) -> canFill (x,y))
      ps |> List.iter (fun (x,y) -> fill (x,y,n))
      ps 
      |> List.collect (fun (x,y) -> 
          [(x-1,y);(x+1,y);(x,y-1);(x,y+1)])
      |> f (n+1)
  f 0 [(x,y)]

let route_home =
  let numbers =
    maze |> Array.map (fun line ->
      line.ToCharArray() 
      |> Array.map (fun c -> if isWall c then 999 else -1)
    )
  let canFill (x:int,y:int) =
    y>=0 && y < (numbers.Length-1) &&
    x>=0 && x < (numbers.[y].Length-1) &&
    numbers.[y].[x] = -1      
  let fill (x,y,n) = numbers.[y].[x] <- n
  flood canFill fill (16,15)
  numbers

let fillValue (x,y) (ex,ey) =
  let bx = int (floor(float ((x+6+ex)/8)))
  let by = int (floor(float ((y+6+ey)/8)))
  route_home.[by].[bx]

let fillUp (x,y) = fillValue (x,y) (0,-4)
let fillDown (x,y) = fillValue (x,y) (0,5)
let fillLeft (x,y) = fillValue (x,y) (-4,0)
let fillRight (x,y) = fillValue (x,y) (5,0)

let chooseDirection (ghost:Ghost) =
  let x,y = ghost.X, ghost.Y
  let dx,dy = ghost.V
  let isBackwards (a,b) =
    (a <> 0 && a = -dx) || (b <> 0 && b = -dy)
  let directions = 
    [|if canGoLeft(x,y) then yield (-1,0), fillLeft(x,y)
      if canGoDown(x,y) then yield (0,1), fillDown(x,y)
      if canGoRight(x,y) then yield (1,0), fillRight(x,y)
      if canGoUp(x,y) then yield (0,-1), fillUp(x,y) |]
  let dx,dy =
    if ghost.IsReturning 
    then
      directions
      |> Array.sortBy snd        
      |> fun xs -> 
        let v, n = xs.[0]
        if n = 0 then ghost.IsReturning <- false
        v
    else      
      directions
      |> Array.map fst
      |> Array.filter (not << isBackwards)
      |> fun xs ->
        if xs.Length = 0 then 0, 0
        else
            let i = random() * float xs.Length
            xs.[int (floor i)]
  dx,dy

(** 
## Keyboard input
*)

type Keys() =
  let mutable keysPressed = Set.empty
  member keys.Reset () = keysPressed <- Set.empty
  member keys.IsPressed keyCode = Set.contains keyCode keysPressed
  member keys.Update (e : KeyboardEvent, pressed) =
    let keyCode = int e.keyCode
    let op =  if pressed then Set.add else Set.remove
    keysPressed <- op keyCode keysPressed

(** 
## Pacman
*)

type Pacman () =
  let pu1, pu2 = createImage pu1, createImage pu2
  let pd1, pd2 = createImage pd1, createImage pd2
  let pl1, pl2 = createImage pl1, createImage pl2
  let pr1, pr2 = createImage pr1, createImage pr2
  let lastp = ref pr1
  member __.ImageAt(x,y,v) =
    let p1, p2 =
      match !v with
      | -1,  0 -> pl1, pl2
      |  1,  0 -> pr1, pr2
      |  0, -1 -> pu1, pu2
      |  0,  1 -> pd1, pd2
      |  _,  _ -> !lastp, !lastp
    let x' = int (floor(float (!x/6)))
    let y' = int (floor(float (!y/6)))
    let p = if (x' + y') % 2 = 0 then p1 else p2
    lastp := p
    p
     
[<Emit("(new Audio($0)).play();")>]
let sound(file:string) : unit = failwith "never"

(** 
## Level gameplay
*)

let playLevel (keys:Keys, onLevelCompleted, onGameOver) =
  let pacman = Pacman()
  let pills = maze |> Array.map (fun line -> 
    line.ToCharArray() 
    |> Array.map (fun c -> c))

  let canvas = document.getElementsByTagName_canvas().[0.]
  canvas.width <- 256.
  canvas.height <- 256.  
  let context = canvas.getContext_2d()
  context.fillStyle <- U3.Case1 "rgb(0,0,0)"
  context.fillRect (0., 0. , 256., 256.);
  let bonusImages = 
    [| createImage _200; createImage _400; 
       createImage _800; createImage _1600 |]
  let background = createBackground()
  let ghosts = createGhosts(context)
  let blue,eyed = createImage blue, createImage eyed

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
    let inputs =
       [| if keys.IsPressed 81 (*q*) then 
            yield canGoUp (!x,!y), (0,-1)
          if keys.IsPressed 65 (*a*) then 
            yield canGoDown (!x,!y), (0,1)
          if keys.IsPressed 90 (*z*) then 
            yield canGoLeft (!x,!y), (-1,0)
          if keys.IsPressed 88 (*x*) then 
            yield canGoRight (!x,!y), (1,0) |]
    let canGoForward =
      match !v with
      | 0,-1 -> canGoUp(!x,!y)
      | 0,1  -> canGoDown(!x,!y)
      | -1,0 -> canGoLeft(!x,!y)
      | 1, 0 -> canGoRight(!x,!y)
      | _ -> false
    let availableDirections =
      inputs
      |> Array.filter fst
      |> Array.map snd
      |> Array.sortBy (fun v' -> v' = !v)
    if availableDirections.Length > 0 then
      v := availableDirections.[0]
    elif inputs.Length = 0 || not canGoForward then
      v := 0,0         
    let x',y' = wrap (!x,!y) !v
    x := x'
    y := y'

  let eatPills () =
    let tx = int (floor(float ((!x+6)/8)))
    let ty = int (floor(float ((!y+6)/8)))     
    let c = pills.[ty].[tx]
    if c = '.' then
      pills.[ty].[tx] <- ' '
      clearCell background (tx,ty)
      score := !score + 10
      decr dotsLeft      
      sound("./fx/Dot5.wav")
    if c = 'o' then       
      pills.[ty].[tx] <- ' '
      clearCell background (tx,ty)
      bonus := 0
      score := !score + 50
      powerCountdown := 250
      decr dotsLeft
      sound("./fx/Powerup.wav")
      
  let touchingGhosts () =
    let px, py = !x, !y
    ghosts |> Array.filter (fun ghost ->
      let x,y = ghost.X, ghost.Y
      ((px >= x && px < x + 13) ||
       (x < px + 13 && x >= px)) &&
      ((py >= y && py < y + 13) ||
       (y < py + 13 && y >= py))
    )

  let collisionDetection () =
    let touched = touchingGhosts ()
    if touched.Length > 0
    then 
      if !powerCountdown > 0 then
        touched |> Array.iter (fun ghost -> 
          if not ghost.IsReturning then
            sound "./fx/EatGhost.wav"
            ghost.IsReturning <- true
            let added = int (2. ** (float !bonus))
            score := !score + added * 200 
            let image = bonusImages.[!bonus]
            bonuses := (100, (image, ghost.X, ghost.Y)) :: !bonuses
            bonus :=  min 3 (!bonus + 1)
        )
      else
        decr energy
        if !flashCountdown = 0 then sound "./fx/Hurt.wav"
        flashCountdown := 30
    if !flashCountdown > 0 then decr flashCountdown 
  
  let updateBonus () =
    let removals,remainders =
      !bonuses 
      |> List.map (fun (count,x) -> count-1,x)
      |> List.partition (fst >> (=) 0)
    bonuses := remainders

  let logic () =
    moveGhosts()
    movePacman()
    eatPills ()
    if !powerCountdown > 0 then decr powerCountdown
    collisionDetection()
    updateBonus ()

  let renderPacman () =
    let p = pacman.ImageAt(x,y,v)
    if (!flashCountdown >>> 1) % 2 = 0
    then context.drawImage(U3.Case1 p, float !x, float !y)

  let renderEnergy () =
    context.fillStyle <- U3.Case1 "yellow"
    context.fillRect(120., 250., float !energy, 2.)

  let renderGhosts () =
    ghosts |> Array.iter (fun ghost ->
      let image =
        if ghost.IsReturning then eyed
        else 
          if !powerCountdown = 0
          then ghost.Image
          elif !powerCountdown > 100 || 
                ((!powerCountdown >>> 3) % 2) <> 0 
          then blue
          else ghost.Image
      context.drawImage(U3.Case1 image, float ghost.X, float ghost.Y) 
    )

  let renderScore () =
    context.fillStyle <- U3.Case1 "white"
    context.font <- "bold 8px";
    context.fillText("Score " + (!score).ToString(), 0., 255.)

  let renderBonus () =
    !bonuses |> List.iter (fun (_,(image,x,y)) -> 
      context.drawImage(U3.Case1 image, float x, float y))

  let render () =
    context.drawImage(U3.Case2 background, 0., 0.)
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
    else window.setTimeout(update, 1000. / 60.) |> ignore

  update()

(** 
## Main gaim entry point
*)

let rec game (keys:Keys) =
  keys.Reset()
  let canvas = document.getElementsByTagName_canvas().[0.]
  let context = canvas.getContext_2d()
  let drawText(text,x,y) =
    context.fillStyle <- U3.Case1 "white"
    context.font <- "bold 8px";
    context.fillText(text, x, y)
  let levelCompleted () =
    drawText("COMPLETED",96.,96.)
    window.setTimeout((fun () -> game(keys)),5000.) |> ignore 
  let gameOver () =
    drawText("GAME OVER",96.,96.)
    window.setTimeout((fun () -> game(keys)),5000.) |> ignore
  let start () =
    let background = createBackground()
    context.drawImage(U3.Case2 background, 0., 0.)
    context.fillStyle <- U3.Case1 "white"
    context.font <- "bold 8px";
    drawText("CLICK TO START", 88., 96.)
    canvas.onclick <- (fun e -> 
      canvas.onclick <- null
      playLevel (keys, levelCompleted, gameOver)
      box true
    )

  let canvas = document.getElementsByTagName_canvas().[0.]
  canvas.width <- 256.
  canvas.height <- 256.  
  start()
  
let main () =
  let keys = Keys()
  window.addEventListener_keydown(fun e -> keys.Update(e, true); null)
  window.addEventListener_keyup(fun e -> keys.Update(e, false); null)
  game (keys)  

do main ()
