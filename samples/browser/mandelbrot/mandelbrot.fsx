// ---
// header: Mandelbrot
// tagline: Using HTML5 canvas
// ---

#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import.Browser

type Complex = { r : double; i : double }
type Color = { r : int; g : int; b : int; a : int }

let maxIter = 512

let height = 800
let width = 1000

let minX = -2.1 
let maxX = 0.5
let minY = -1.4
let maxY = 1.4

let iteratePoint (s : Complex) (p : Complex) : Complex =
    { r = s.r + p.r*p.r - p.i*p.i; i = s.i + 2.0 * p.i * p.r }

let getIterationCount (p : Complex) =
    let mutable z = p
    let mutable i = 0
    while i < maxIter && (z.r*z.r + z.i*z.i < 4.0) do
      z <- iteratePoint p z
      i <- i + 1
    i
    
let iterCountToColor (i : int) : Color =
    let i = maxIter - i
    { r = 0; g = i % 256; b = 100 * (i / 256); a = 255 }

let getCoordColor (x : int, y : int) : Color =
    let p = { r = float x * (maxX - minX) / float width + minX
            ; i = float y * (maxY - minY) / float height + minY }
    let i = getIterationCount p
    iterCountToColor i

let showSet() =
    let ctx = document.getElementsByTagName_canvas().[0.].getContext_2d()
    
    let img = ctx.createImageData(U2.Case1 (float width), float height)
    for y = 0 to height-1 do
        for x = 0 to width-1 do
            let index = (x + y * width) * 4
            let color = getCoordColor (x, y)
            img.data.[float (index+0)] <- float color.r
            img.data.[float (index+1)] <- float color.g
            img.data.[float (index+2)] <- float color.b
            img.data.[float (index+3)] <- float color.a
    ctx.putImageData(img, 0., 0.)

showSet()