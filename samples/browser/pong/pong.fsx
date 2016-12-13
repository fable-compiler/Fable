(**
 - title: Fable Pong
 - tagline: Pong clone using HTML5 canvas
*)

#r "../../node_modules/fable-core/Fable.Core.dll"
#load "keyboard.fsx"
#load "win.fsx"

open Fable.Core
open Fable.Import.Browser
open System

open Keyboard
open Win

let w, h = Win.dimensions()

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

type GameStatus = {
    scoreLeft : int;
    scoreRight : int;
    active : bool;
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

let calculateAngle pong hitRightPong determineAngle ball =
    let relativeIntersectY = (pong.y + (pong.height / 2.)) - ball.element.y
    let normalizedRelativeIntersectionY = (relativeIntersectY / (pong.height / 2.))
    if normalizedRelativeIntersectionY = 0. && hitRightPong then
        Math.PI
    else
        normalizedRelativeIntersectionY |> determineAngle

let collision pong1 pong2 ball =
    match ball |> checkCollision pong1 pong2 with
    | None -> ball.angle
    | Top | Bottom -> -ball.angle
    | Left | Right -> ball.angle
    | LeftPong -> ball |> calculateAngle pong1 false (fun intersection -> intersection * (5. * Math.PI / 12.)) // Max. bounce = 75°
    | RightPong -> ball |> calculateAngle pong2 true (fun intersection -> Math.PI - intersection * (5. * Math.PI / 12.))

let moveBall angle ball = { 
    element = { x = ball.element.x + ball.speed * cos angle;
                y = ball.element.y + ball.speed * -sin angle; 
                width = ball.element.width; 
                height = ball.element.height }; 
    speed = ball.speed + 0.005;
    angle = angle; 
}

let checkGameStatus pong1 pong2 ball gameStatus =
    match ball |> checkCollision pong1 pong2 with
    | Left -> { gameStatus with scoreRight = gameStatus.scoreRight + 1; active = false }
    | Right -> { gameStatus with scoreLeft = gameStatus.scoreLeft + 1; active = false }
    | _ -> gameStatus

// ------------------------------------------------------------------------------------------------------------------------

let render (w, h) pong1 pong2 ball gameStatus  =
    (0., 0., w, h) |> Win.drawRect("black")
    (pong1.x, pong1.y, pong1.width, pong1.height) |> Win.drawRect("white")
    (pong2.x, pong2.y, pong2.width, pong2.height) |> Win.drawRect("white")
    (w / 4., 40.) |> Win.drawText (string(gameStatus.scoreLeft)) "white" "30px Arial"
    (w / 1.25 - 30., 40.) |> Win.drawText (string(gameStatus.scoreRight)) "white" "30px Arial"
    (ball.element.x, ball.element.y, ball.element.width, 0., 2. * Math.PI) |> Win.drawCircle("yellow")

let initialPong1 = { x = 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let initialPong2 = { x = w - 15. - 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let initialBall = 
    { element = { x = w / 2.; y = h / 2.; width = 5.; height = 5. }; 
    speed = 3.;
    angle = 0. }

let initialGameStatus = { scoreLeft = 0; scoreRight = 0; active = false; }

Keyboard.init()

let rec update pong1 pong2 ball gameStatus () =
    let pong1 = if gameStatus.active then pong1 |> move (Keyboard.leftControlsPressed()) else initialPong1
    let pong2 = if gameStatus.active then pong2 |> move (Keyboard.rightControlsPressed()) else initialPong2
    let angle = if gameStatus.active then collision pong1 pong2 ball else ball.angle
    let ball = if gameStatus.active then ball |> moveBall angle else { initialBall with angle = if angle = 0. then Math.PI else 0. }
    let gameStatus = if Keyboard.spacePressed() = 1 then { gameStatus with active = true } else gameStatus |> checkGameStatus pong1 pong2 ball
    render (w, h) pong1 pong2 ball gameStatus
    window.setTimeout(update pong1 pong2 ball gameStatus, 1000. / 60.) |> ignore

update initialPong1 initialPong2 initialBall initialGameStatus ()
