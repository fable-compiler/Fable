module Pixi

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

// You can use either `new PIXI.WebGLRenderer`, `new PIXI.CanvasRenderer`, or `PIXI.autoDetectRenderer`
// which will try to choose the best renderer for the environment you are in.
let renderer = PIXI.WebGLRenderer(800., 600.)

// The renderer will create a canvas element for you that you can then insert into the DOM.
Browser.document.body.appendChild(renderer.view)
|> ignore
// You need to create a root container that will hold the scene you want to draw.
let stage = PIXI.Container()

// Declare a global variable for our sprite so that the animate function can access it.
let mutable bunny = Unchecked.defaultof<PIXI.Sprite>

let rec animate _ =
    // start the timer for the next animation loop
    Browser.window.requestAnimationFrame(Browser.FrameRequestCallback animate) |> ignore

    // each frame we spin the bunny around a bit
    bunny.rotation <- bunny.rotation + 0.01

    // this is the main render call that makes pixi draw your container and its children.
    renderer.render(stage);

// load the texture we need
PIXI.Globals.loader.add("bunny", "bunny.png").load(fun loader resources ->
    // This creates a texture from a 'bunny.png' image.
    bunny <- PIXI.Sprite(unbox resources?bunny?texture)

    // Setup the position and scale of the bunny
    bunny.position.x <- 400.
    bunny.position.y <- 300.

    bunny.scale.x <- 2.
    bunny.scale.y <- 2.

    // Add the bunny to the scene we are building.
    stage.addChild(bunny) |> ignore

    // kick off the animation loop (defined below)
    animate 0. |> ignore
) |> ignore
