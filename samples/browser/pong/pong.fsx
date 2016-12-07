#r "../../node_modules/fable-core/Fable.Core.dll"
#load "keyboard.fsx"
#load "win.fsx"

open Fable.Core
open Fable.Import.Browser
open System

open Keyboard
open Win

let w, h = Win.dimensions()

let mutable pointsLeft = 0
let mutable pointsRight = 0
let mutable restart = false

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

let playerScored scoring =
    match scoring with
    | (1, _) -> pointsLeft <- pointsLeft + 1; restart <- true
    | (_, 1) -> pointsRight <- pointsRight + 1; restart <- true
    | _ -> ()

let collision pong1 pong2 ball =
    match ball |> checkCollision pong1 pong2 with
    | None -> ball.angle
    | Top | Bottom -> -ball.angle
    | Left -> playerScored (0, 1); ball.angle
    | Right -> playerScored (1, 0); ball.angle
    | LeftPong -> ball |> calculateAngle pong1 1.
    | RightPong -> ball |> calculateAngle pong2 Math.PI

let moveBall angle ball = { 
    element = { x = ball.element.x + ball.speed * cos angle;
        y = ball.element.y + ball.speed * -sin angle; 
        width = ball.element.width; 
        height = ball.element.height }; 
    speed = ball.speed + 0.01;
    angle = angle; 
}

// ------------------------------------------------------------------------------------------------------------------------

let render (w, h) pong1 pong2 ball  =
    (0., 0., w, h) |> Win.filled("black")
    (pong1.x, pong1.y, pong1.width, pong1.height) |> Win.filled("white")
    (pong2.x, pong2.y, pong2.width, pong2.height) |> Win.filled("white")
    (w / 4., 40.) |> Win.drawText (pointsLeft.ToString()) "white" "30px Arial"
    (w / 1.25 - 30., 40.) |> Win.drawText (pointsRight.ToString()) "white" "30px Arial"
    (ball.element.x, ball.element.y, ball.element.width, ball.element.height) |> Win.filled("yellow")
    

Keyboard.init()

let initialPong1 = { x = 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let initialPong2 = { x = w - 15. - 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let initialBall = 
    { element = { x = w / 2. - 10. / 2.; y = h / 2. - 10. / 2.; width = 10.; height = 10. }; 
    speed = 3.;
    angle = 0.; }

let rec update pong1 pong2 ball () =
    let pong1 = if restart then restart <- false; initialPong1 else pong1 |> move (Keyboard.leftControlsPressed())
    let pong2 = if restart then restart <- false; initialPong2 else pong2 |> move (Keyboard.rightControlsPressed())
    let angle = if restart then restart <- false; initialBall.angle else collision pong1 pong2 ball
    let ball = if restart then restart <- false; initialBall else ball |> moveBall angle       
    render (w, h) pong1 pong2 ball
    window.setTimeout(update pong1 pong2 ball, 1000. / 60.) |> ignore

update initialPong1 initialPong2 initialBall ()
