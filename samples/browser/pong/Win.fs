module Win

open Fable.Core
open Fable.Import.Browser

module Win =
    let canvas = document.getElementsByTagName_canvas().[0]
    let context = canvas.getContext_2d()

    /// Fill rectangle with given color
    let drawRect color rect =
        let ctx = context
        ctx.fillStyle <- U3.Case1 color
        ctx.fillRect rect

    let drawCircle color (x, y, radius, startAngle, endAngle) =
        let ctx = context
        ctx.beginPath()
        ctx.arc(x, y, radius, startAngle, endAngle)
        ctx.fillStyle <- U3.Case1 color
        ctx.fill()

    let drawText text color font position =
        let ctx = context
        ctx.fillStyle <- U3.Case1 color
        ctx.font <- font
        ctx.fillText(text, (fst position), (snd position))

    let dimensions() =
      canvas.width, canvas.height