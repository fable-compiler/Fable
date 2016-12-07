#r "../../node_modules/fable-core/Fable.Core.dll"
open Fable.Core
open Fable.Import.Browser
open System

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
    let init () =
      document.addEventListener_keydown(fun e -> update(e, true))
      document.addEventListener_keyup(fun e -> update(e, false))

module Win =
    let canvas = document.getElementsByTagName_canvas().[0]
    let context = canvas.getContext_2d()

    // Format RGB color as "rgb(r,g,b)"
    let ($) s n = s + n.ToString()
    let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"
    let color name =
        match name with
        | "black" -> rgb 0 0 0
        | "white" -> rgb 255 255 255
        | "green" -> rgb 0 255 0
        | _ -> rgb 0 0 0

    /// Fill rectangle with given color
    let filled color rect =
        let ctx = context
        ctx.fillStyle <- U3.Case1 color
        ctx.fillRect rect

    let dimensions() =
      canvas.width, canvas.height

let w, h = Win.dimensions()

// Pong stuff
type PongElement = { 
    x : float; 
    y : float; 
    width : float; 
    height : float; 
}

type BallElement = { 
    element : PongElement; 
    speed : float;
    angle : float; 
}

let canMove direction pong =
    match direction with
    | (1, _) -> pong.y > 0.
    | (_, 1) -> pong.y + pong.height < h
    | _ -> false

let move direction pong =
    if pong |> canMove direction then
        match direction with
        | (1, _) -> { pong with y = pong.y - 5. }
        | (_, 1) -> { pong with y = pong.y + 5. }
        | _ -> pong
    else
        pong

type Collision = 
    | None
    | Top
    | Bottom
    | Left
    | Right
    | LeftPong
    | RightPong

let checkCollision pong1 pong2 ball =
    let hitTop = ball.element.y <= 0.
    let hitBottom = ball.element.y + ball.element.height >= h
    let hitLeft = ball.element.x <= pong1.x && ((ball.element.y >= pong1.y && ball.element.y <= pong1.y + pong1.height) |> not)
    let hitRight = ball.element.x + ball.element.width >= pong2.x + pong2.width && ((ball.element.y >= pong2.y && ball.element.y <= pong2.y + pong2.height) |> not)
    let hitLeftPong = ball.element.x <= pong1.x + pong1.width && ball.element.y >= pong1.y && ball.element.y <= pong1.y + pong1.height
    let hitRightPong = ball.element.x + ball.element.width >= pong2.x && ball.element.y >= pong2.y && ball.element.y <= pong2.y + pong2.height
    match (hitTop, hitBottom, hitLeft, hitRight, hitLeftPong, hitRightPong) with
    | (true, _, _, _, _, _) -> Top
    | (_, true, _, _, _, _) -> Bottom
    | (_, _, true, _, _, _) -> Left
    | (_, _, _, true, _, _) -> Right
    | (_, _, _, _, true, _) -> LeftPong
    | (_, _, _, _, _, true) -> RightPong
    | _ -> None

let calculateAngle pong factor ball =
    let relativeIntersectY = (pong.y + (pong.height / 2.)) - ball.element.y
    let normalizedRelativeIntersectionY = (relativeIntersectY / (pong.height / 2.))
    factor * normalizedRelativeIntersectionY * (5. * Math.PI / 12.) // Max. bounce = 75°    

let collision pong1 pong2 ball =
    match ball |> checkCollision pong1 pong2 with
    | None -> ball.angle
    | Top | Bottom -> -ball.angle
    | Left | Right -> ball.angle // Implement points
    | LeftPong -> ball |> calculateAngle pong1 1.
    | RightPong -> ball |> calculateAngle pong2 Math.PI

let moveBall angle ball = { 
    element = { x = ball.element.x + ball.speed * cos angle;
        y = ball.element.y + ball.speed * -sin angle; 
        width = ball.element.width; 
        height = ball.element.height }; 
    speed = ball.speed;
    angle = angle; 
}

//---------------------------------------------------

let render (w, h) pong1 pong2 ball  =
    (0., 0., w, h) |> Win.filled(Win.color("black"))
    (pong1.x, pong1.y, pong1.width, pong1.height) |> Win.filled(Win.color("white"))
    (pong2.x, pong2.y, pong2.width, pong2.height) |> Win.filled(Win.color("white"))
    (ball.element.x, ball.element.y, ball.element.width, ball.element.height) |> Win.filled(Win.color("green"))

Keyboard.init()
let rec update pong1 pong2 ball () =
    let pong1 = pong1 |> move (Keyboard.leftControlsPressed())
    let pong2 = pong2 |> move (Keyboard.rightControlsPressed())
    let angle = collision pong1 pong2 ball
    let ball = ball |> moveBall angle 
    render (w, h) pong1 pong2 ball
    window.setTimeout(update pong1 pong2 ball, 1000. / 60.) |> ignore


let pong1 = { x = 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let pong2 = { x = w - 15. - 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let ball = 
    { element = { x = w / 2. - 10. / 2.; y = h / 2. - 10. / 2.; width = 10.; height = 10. }; 
    speed = 3.;
    angle = 0.; }

update pong1 pong2 ball ()
