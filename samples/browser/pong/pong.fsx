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

let canMove direction paddle =
    match direction with
    | (1, _) -> paddle.y > 0.
    | (_, 1) -> paddle.y + paddle.height < h
    | _ -> false

let move direction paddle =
    if paddle |> canMove direction then
        match direction with
        | (1, _) -> { paddle with y = paddle.y - 5. }
        | (_, 1) -> { paddle with y = paddle.y + 5. }
        | _ -> paddle
    else
        paddle

type Collision = 
    | None
    | Top
    | Bottom
    | Left
    | Right
    | LeftPong
    | RightPong

let checkCollision leftPaddle rightPaddle ball =
    let hitTop = ball.element.y <= 0.
    let hitBottom = ball.element.y + ball.element.height >= h
    let hitLeft = ball.element.x <= leftPaddle.x && ((ball.element.y >= leftPaddle.y && ball.element.y <= leftPaddle.y + leftPaddle.height) |> not)
    let hitRight = ball.element.x + ball.element.width >= rightPaddle.x + rightPaddle.width && ((ball.element.y >= rightPaddle.y && ball.element.y <= rightPaddle.y + rightPaddle.height) |> not)
    let hitLeftPong = ball.element.x <= leftPaddle.x + leftPaddle.width && ball.element.y >= leftPaddle.y && ball.element.y <= leftPaddle.y + leftPaddle.height
    let hitRightPong = ball.element.x + ball.element.width >= rightPaddle.x && ball.element.y >= rightPaddle.y && ball.element.y <= rightPaddle.y + rightPaddle.height
    match (hitTop, hitBottom, hitLeft, hitRight, hitLeftPong, hitRightPong) with
    | (true, _, _, _, _, _) -> Top
    | (_, true, _, _, _, _) -> Bottom
    | (_, _, true, _, _, _) -> Left
    | (_, _, _, true, _, _) -> Right
    | (_, _, _, _, true, _) -> LeftPong
    | (_, _, _, _, _, true) -> RightPong
    | _ -> None

let calculateAngle paddle hitRightPaddle determineAngle ball =
    let relativeIntersectY = (paddle.y + (paddle.height / 2.)) - ball.element.y
    let normalizedRelativeIntersectionY = (relativeIntersectY / (paddle.height / 2.))
    if normalizedRelativeIntersectionY = 0. && hitRightPaddle then
        Math.PI
    else
        normalizedRelativeIntersectionY |> determineAngle

let collision leftPaddle rightPaddle ball =
    match ball |> checkCollision leftPaddle rightPaddle with
    | None -> ball.angle
    | Top | Bottom -> -ball.angle
    | Left | Right -> ball.angle
    | LeftPong -> ball |> calculateAngle leftPaddle false (fun intersection -> intersection * (5. * Math.PI / 12.)) // Max. bounce = 75°
    | RightPong -> ball |> calculateAngle rightPaddle true (fun intersection -> Math.PI - intersection * (5. * Math.PI / 12.))

let moveBall angle ball = { 
    element = { x = ball.element.x + ball.speed * cos angle;
                y = ball.element.y + ball.speed * -sin angle; 
                width = ball.element.width; 
                height = ball.element.height }; 
    speed = ball.speed + 0.005;
    angle = angle; 
}

let checkGameStatus leftPaddle rightPaddle ball gameStatus =
    match ball |> checkCollision leftPaddle rightPaddle with
    | Left -> { gameStatus with scoreRight = gameStatus.scoreRight + 1; active = false }
    | Right -> { gameStatus with scoreLeft = gameStatus.scoreLeft + 1; active = false }
    | _ -> gameStatus

// ------------------------------------------------------------------------------------------------------------------------

let render (w, h) leftPaddle rightPaddle ball gameStatus  =
    (0., 0., w, h) |> Win.drawRect("black")
    (leftPaddle.x, leftPaddle.y, leftPaddle.width, leftPaddle.height) |> Win.drawRect("white")
    (rightPaddle.x, rightPaddle.y, rightPaddle.width, rightPaddle.height) |> Win.drawRect("white")
    (w / 4., 40.) |> Win.drawText (string(gameStatus.scoreLeft)) "white" "30px Arial"
    (w / 1.25 - 30., 40.) |> Win.drawText (string(gameStatus.scoreRight)) "white" "30px Arial"
    (ball.element.x, ball.element.y, ball.element.width, 0., 2. * Math.PI) |> Win.drawCircle("yellow")
    if gameStatus.active |> not then
        (w / 2. - 230., h / 2. + 40.) |> Win.drawText "Press space to start" "green" "40px Lucida Console"

let initialLeftPaddle = { x = 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let initialRightPaddle = { x = w - 15. - 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let initialBall = 
    { element = { x = w / 2.; y = h / 2.; width = 5.; height = 5. }; 
    speed = 3.;
    angle = 0. }

let initialGameStatus = { scoreLeft = 0; scoreRight = 0; active = false; }

Keyboard.init()

let rec update leftPaddle rightPaddle ball gameStatus () =
    let leftPaddle = if gameStatus.active then leftPaddle |> move (Keyboard.leftControlsPressed()) else initialLeftPaddle
    let rightPaddle = if gameStatus.active then rightPaddle |> move (Keyboard.rightControlsPressed()) else initialRightPaddle
    let angle = if gameStatus.active then collision leftPaddle rightPaddle ball else ball.angle
    let ball = if gameStatus.active then ball |> moveBall angle else { initialBall with angle = if angle = 0. then Math.PI else 0. }
    let gameStatus = if Keyboard.spacePressed() = 1 then { gameStatus with active = true } else gameStatus |> checkGameStatus leftPaddle rightPaddle ball
    render (w, h) leftPaddle rightPaddle ball gameStatus
    window.setTimeout(update leftPaddle rightPaddle ball gameStatus, 1000. / 60.) |> ignore

update initialLeftPaddle initialRightPaddle initialBall initialGameStatus ()
