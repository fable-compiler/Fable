(**
 - title: Fable Pong
 - tagline: Two player Pong clone using HTML5 canvas
 - intro: This demo shows a Fable implementation of Pong, which is also part of the [F# Advent Calendar in English 2016](https://sergeytihon.wordpress.com/2016/10/23/f-advent-calendar-in-english-2016/).
   You can find the [full source code on GitHub](https://github.com/fsprojects/Fable/blob/master/samples/browser/pong/pong.fsx) and learn more in [this post](http://oopbase.de/posts/implementing-pong-in-a-functional-manner-with-fable.html).
*)

(*** hide ***)
module Pong

open Fable.Core
open Fable.Import.Browser
open System

open Keyboard
open Win

let w, h = Win.dimensions()

(**
## Defining the model

When thinking about the Pong game, there are basically three types of models:

1. **Paddles:** The paddles have got a position and a specific size.
2. **Ball:** The ball has got a position and a size as well. But it has also got speed and an angle.
3. **Game status:** Some kind of storage containing information about the current score.

*)

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

(**
## Controlling the paddles

To control a paddle there are only two functions necessary. A canMove-function, to indicate whether a paddle can move in a certain direction, and an actual move-function to move the paddle.
The parameter direction is a tuple (int * int). When the first parameter of the tuple is set to 1, we want the paddle to move up. When the second parameter of the tuple is set to 1, we want the paddle to move down. By using pattern matching we can check which value of the tuple is set to 1. Since every object is immutable, we either return a copy of the current paddle with its new Y-position (line 10 and 11) or simply return the input-paddle if no movement is allowed.
*)

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

(**
## Collision detection

This discriminated union describes the whole collision system of the game: There can be no collision (None), the Top or Bottom of the canvas may be hit, the Left or Right part of the canvas may be hit (so a player scored) or finally a paddle was hit (LeftPaddle & RightPaddle). The following function takes the paddles and the ball as input parameters and returns the found type of collision.
*)


type Collision =
    | None
    | Top
    | Bottom
    | Left
    | Right
    | LeftPaddle
    | RightPaddle


(**
With this function, implementing a final collision-function to determine the new angle of the ball is straight forward again. (Thanks to pattern matching)
*)


let checkCollision leftPaddle rightPaddle ball =
    let hitTop = ball.element.y <= 0.
    let hitBottom = ball.element.y + ball.element.height >= h
    let hitLeft =
        ball.element.x <= leftPaddle.x 
        && ((ball.element.y >= leftPaddle.y 
            && ball.element.y <= leftPaddle.y + leftPaddle.height) 
            |> not)
    let hitRight =
        ball.element.x + ball.element.width >= rightPaddle.x + rightPaddle.width 
        && ((ball.element.y >= rightPaddle.y 
            && ball.element.y <= rightPaddle.y + rightPaddle.height) 
            |> not)
    let hitLeftPaddle =
        ball.element.x <= leftPaddle.x + leftPaddle.width 
        && ball.element.y >= leftPaddle.y 
        && ball.element.y <= leftPaddle.y + leftPaddle.height
    let hitRightPaddle =
        ball.element.x + ball.element.width >= rightPaddle.x 
        && ball.element.y >= rightPaddle.y 
        && ball.element.y <= rightPaddle.y + rightPaddle.height
    match (hitTop, hitBottom, hitLeft, hitRight, hitLeftPaddle, hitRightPaddle) with
    | (true, _, _, _, _, _) -> Top
    | (_, true, _, _, _, _) -> Bottom
    | (_, _, true, _, _, _) -> Left
    | (_, _, _, true, _, _) -> Right
    | (_, _, _, _, true, _) -> LeftPaddle
    | (_, _, _, _, _, true) -> RightPaddle
    | _ -> None

(**
When hitting either the top or the bottom of the canvas, we negate the value of the angle (angle of incidence is equal to the angle of reflection). When hitting the left or right part of the canvas, we simply keep the input angle, since evaluating the score isn’t done here. To actually calculate the angle when a paddle is hit, we use yet another function.
*)

let calculateAngle paddle hitRightPaddle determineAngle ball =
    let relativeIntersectY = (paddle.y + (paddle.height / 2.)) - ball.element.y
    let normalizedRelativeIntersectionY = (relativeIntersectY / (paddle.height / 2.))
    if normalizedRelativeIntersectionY = 0. && hitRightPaddle then
        Math.PI
    else
        normalizedRelativeIntersectionY |> determineAngle

(**
For the calculation, we determine the relative intersection where the ball hit the paddle. Afterwards we normalize that value. So for example, if the paddle is 20 pixels high, that value will be between -10 and 10. Therefore we can dynamically calculate the angle depending on the impact. As seen in the collision function, the determineAngle parameter of this calculation-function is a function itself. Depending on which paddle is hit, we have to use a slightly modified calculation of the final angle. As you can see in line 5, we’ve also got a special case we have to deal with. If the right paddle got hit in the exact center, so normalizedRelativeIntersectionY = 0. && hitRightPaddle, we will have to return Pi as the new angle, since the radiant value of Pi is equal to 180°.
*)

let collision leftPaddle rightPaddle ball =
    match ball |> checkCollision leftPaddle rightPaddle with
    | None -> ball.angle
    | Top | Bottom -> -ball.angle
    | Left | Right -> ball.angle
    | LeftPaddle -> ball 
                    |> calculateAngle leftPaddle false 
                        (fun intersection -> 
                                intersection * (5. * Math.PI / 12.))
    | RightPaddle -> ball 
                    |> calculateAngle rightPaddle true 
                        (fun intersection -> 
                                Math.PI - intersection * (5. * Math.PI / 12.))

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
    | Left -> { gameStatus with scoreRight
                                    = gameStatus.scoreRight + 1; active = false }
    | Right -> { gameStatus with scoreLeft 
                                    = gameStatus.scoreLeft + 1; active = false }
    | _ -> gameStatus


(**
## Game loop and rendering

*)

let render (w, h) leftPaddle rightPaddle ball gameStatus  =
    (0., 0., w, h) |> Win.drawRect("black")
    (leftPaddle.x, leftPaddle.y, leftPaddle.width, leftPaddle.height)
    |> Win.drawRect("white")
    (rightPaddle.x, rightPaddle.y, rightPaddle.width, rightPaddle.height) 
    |> Win.drawRect("white")
    (w / 4., 40.) |> Win.drawText (string(gameStatus.scoreLeft)) 
        "white" "30px Arial"
    (w / 1.25 - 30., 40.) |> Win.drawText (string(gameStatus.scoreRight)) 
        "white" "30px Arial"
    (ball.element.x, ball.element.y, ball.element.width, 0., 2. * Math.PI) 
    |> Win.drawCircle("yellow")
    if gameStatus.active |> not then
        (w / 2. - 230., h / 2. + 40.) 
        |> Win.drawText "Press 'r' to start" "green" "40px Lucida Console"

let initialLeftPaddle =
    { x = 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let initialRightPaddle =
    { x = w - 15. - 10.; y = h / 2. - 70. / 2.; width = 15.; height = 70. }
let initialBall =
    { element = { x = w / 2.; y = h / 2.; width = 5.; height = 5. };
    speed = 3.;
    angle = 0. }

let initialGameStatus = { scoreLeft = 0; scoreRight = 0; active = false; }

Keyboard.init()

let rec update leftPaddle rightPaddle ball gameStatus () =
    let leftPaddle = if gameStatus.active then 
                        leftPaddle 
                        |> move (Keyboard.leftControlsPressed()) 
                     else initialLeftPaddle
    let rightPaddle = if gameStatus.active then
                        rightPaddle
                        |> move (Keyboard.rightControlsPressed()) 
                      else initialRightPaddle
    let angle = if gameStatus.active then
                    collision leftPaddle rightPaddle ball 
                else ball.angle
    let ball = if gameStatus.active then
                    ball |> moveBall angle 
               else 
                   { initialBall with angle = if angle = 0. then Math.PI else 0. }
    let gameStatus = if Keyboard.rKeyPressed() = 1 then 
                        { gameStatus with active = true } 
                     else
                        gameStatus |> checkGameStatus leftPaddle rightPaddle ball
    render (w, h) leftPaddle rightPaddle ball gameStatus
    window.setTimeout(update leftPaddle rightPaddle ball gameStatus, 1000. / 60.)
    |> ignore

update initialLeftPaddle initialRightPaddle initialBall initialGameStatus ()
