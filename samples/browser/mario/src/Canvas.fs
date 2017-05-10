module Mario.Canvas

open Fable.Core.JsInterop
open Fable.Import.Browser

// Get the canvas context for drawing
let canvas = document.getElementsByTagName_canvas().[0]
let context = canvas.getContext_2d()

// Format RGB color as "rgb(r,g,b)"
let ($) s n = s + n.ToString()
let rgb r g b = "rgb(" $ r $ "," $ g $ "," $ b $ ")"

/// Fill rectangle with given color
let filled (color: string) rect =
    let ctx = context
    ctx.fillStyle <- !^ color
    ctx.fillRect rect

/// Move element to a specified X Y position
let position (x,y) (img : HTMLImageElement) =
    img.style.left <- x.ToString() + "px"
    img.style.top <- (canvas.offsetTop + y).ToString() + "px"

let getWindowDimensions () =
  canvas.width, canvas.height

/// Get the first <img /> element and set `src` (do
/// nothing if it is the right one to keep animation)
let image (src:string) =
    let image = document.getElementsByTagName_img().[0]
    if image.src.IndexOf(src) = -1 then image.src <- src
    image