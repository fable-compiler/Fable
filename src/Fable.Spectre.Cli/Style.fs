module Fable.Spectre.Cli.SpectreOutput

open SpectreCoff
open Spectre.Console

let Dim = fun inp -> MarkupD([ Decoration.Dim ], inp)
let dim = Dim >> toMarkedUpString

let AnsiPanel color =
    {
        Border = BoxBorder.Ascii
        BorderColor = color
        Sizing = SizingBehaviour.Collapse
        Padding = Padding.AllEqual 1
    }

let HeaderPanel = AnsiPanel(Some Color.Teal)

let fable =
    let defaultFigletFont = FigletFont.Default
    FigletText "FABLE"
