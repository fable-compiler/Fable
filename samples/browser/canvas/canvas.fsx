// ---
// header: Canvas
// tagline: Using HTML5 canvas (adapted from FunScript)
// ---

#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

let canvas =  document.getElementsByTagName_canvas().[0]
canvas.width <- 1000.
canvas.height <- 800.
let ctx = canvas.getContext_2d()
ctx.fillStyle <- U3.Case1 "rgb(200,0,0)"
ctx.fillRect (10., 10., 55., 50.);
ctx.fillStyle <- U3.Case1 "rgba(0, 0, 200, 0.5)"
ctx.fillRect (30., 30., 55., 50.)
