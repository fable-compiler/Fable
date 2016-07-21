// Load Fable.Core and bindings to JS global objects
#r "../../../../build/fable/bin/Fable.Core.dll"
// #load "../node_modules/fable-import-react/Fable.Import.React.fs"
// #load "../node_modules/fable-import-react/Fable.Helpers.React.fs"
#load "../node_modules/fable-import-d3/Fable.Import.D3.fs"
// #load "../node_modules/fable-import-dc/Fable.Import.DC.fs"
#load "../../../../import/dc/Fable.Import.DC.fs"

open System
open Fable.Core
open Fable.Import
type dc = Fable.Import.DC.Globals

[<Import("default", "crossfilter")>]
let crossfilter(o: obj): obj = failwith "JS only"

[<Emit("+$0")>]
let (~+) (x: obj): int = failwith "JS only"

let chart = dc.barChart("#test")

D3.Globals.csv.Invoke("data/morley.csv", fun error experiments ->
    for x in experiments do
        x?Speed <- +x?Speed

    let ndx = crossfilter experiments
    let runDimension = ndx?dimension(fun d -> +d?Run)
    let speedSumGroup =
        runDimension
            ?group()
            ?reduceSum(fun d -> (+d?Speed * +d?Run) / 1000)

    chart
        .width(768.)
        .height(480.)
        .x(D3.Scale.Globals.linear().domain([|6.;20.|]))
        .dimension(runDimension)
        .group(speedSumGroup)
        .on("renderlet", fun chart ->
            chart.selectAll("rect")?on("click", fun d ->
                Browser.console.log("click!", d)
            ) |> ignore
        )
        ?brushOn(false)
        ?yAxisLabel("This is the Y Axis!")
    |> ignore

    dc.renderAll()
)