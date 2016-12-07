#r "../../node_modules/fable-core/Fable.Core.dll"
open Fable.Core
open Fable.Import.Browser

module Win =
    let canvas = document.getElementsByTagName_canvas().[0]
    let context = canvas.getContext_2d()

    /// Fill rectangle with given color
    let filled color rect =
        let ctx = context
        ctx.fillStyle <- U3.Case1 color
        ctx.fillRect rect

    let drawText text color font position =
        let ctx = context
        ctx.fillStyle <- U3.Case1 color
        ctx.font <- font
        ctx.fillText(text, (fst position), (snd position))

    let dimensions() =
      canvas.width, canvas.height