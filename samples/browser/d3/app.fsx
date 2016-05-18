// ---
// Using D3
// Ported from Mike Bostock's sample http://bl.ocks.org/mbostock/4183330
// ---

#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-d3/Fable.Import.D3.fs"

open System
open Fable.Core
open Fable.Import
open Fable.Import.Browser

type IQueue =
    abstract member defer: obj * obj -> IQueue
    abstract member await: obj -> unit

type ITopojson =
    abstract member feature: obj * obj -> obj
    abstract member mesh: obj * obj * obj -> obj

// Globally imported JS libs (loaded with <script> tag)
let [<Global>] queue: unit->IQueue = failwith "JS only"
let [<Global>] topojson: ITopojson = failwith "JS only"

// Helpers
let inline get k o = o?(k)
let inline f1 (f: 'a->'b) = Func<_,_> f
let inline f2 (f: 'a->'b->'c) = Func<_,_,_> f
let inline f3 (f: 'a->'b->'c->'d) = Func<_,_,_,_> f

let width, height = 960., 960.
    
let projection =
    D3.geo
        .orthographic()
        .translate((width / 2., height / 2.))
        .scale(width / 2. - 20.)
        .clipAngle(90.)
        .precision(0.6)
        
let canvas =  document.getElementsByTagName_canvas().[0]
canvas.width <- 1000.
canvas.height <- 800.
let ctx = canvas.getContext_2d()

let path =
    D3.geo.path()
        .projection(unbox<D3.Geo.Transform> projection)
        .context(ctx)

let title = d3.select("h1")

queue()
    // Use the dynamic operator here so the F# compiler
    // doesn't wrap d3.json/d3.tsv in a lambda
    .defer(d3?json, "/data/world-110m.json")
    .defer(d3?tsv, "/data/world-country-names.tsv")
    .await(f3 <| fun error world names ->
        if error then error |> unbox |> raise
        
        let globe = createObj [ "type" ==> "Sphere" ]
        let landFeature = topojson.feature(world, world?objects?``land``)
        // Use the helper f2 here to make sure (<>) converts to a delegate
        // i.e. it's called from JS like `f(x, y)` instead of `f(x)(y)`
        let borders = topojson.mesh(world, world?objects?countries, f2 (<>))
        let countries =
            topojson.feature(world, world?objects?countries)
            |> get "features" |> unbox<obj[]>
        let mutable i = -1
        let n = countries.Length
        
        let countries =
            countries |> Array.filter (fun d -> 
                names |> Seq.exists (fun n ->
                    if (string d?id) = (string n?id)
                    then d?name <- n?name; true
                    else false))
            |> Array.sortWith (fun a b ->
                compare (string a?name) (string b?name))

        let rec transition() =
            d3.transition()
                .duration(1250.)
                .each("start", f2 (fun _ _ ->
                    i <- (i + 1) % n
                    upcast title.text(unbox<D3.Primitive> countries.[i]?name)))
                .tween("rotate", f1 (fun () ->
                    let p1, p2 = D3.geo.centroid(countries.[i])
                    let r = d3.interpolate(projection.rotate(), (-p1, -p2))
                    f1 (fun t ->
                        projection.rotate(r.Invoke(t) |> unbox) |> ignore
                        ctx.clearRect(0., 0., width, height)
                        ctx.fillStyle <- U3.Case1 "#ccc"; ctx.beginPath(); path.Invoke(landFeature) |> ignore; ctx.fill()
                        ctx.fillStyle <- U3.Case1 "#f00"; ctx.beginPath(); path.Invoke(countries.[i]) |> ignore; ctx.fill()
                        ctx.strokeStyle <- U3.Case1 "#fff"; ctx.lineWidth <- 0.5; ctx.beginPath(); path.Invoke(borders) |> ignore; ctx.stroke()
                        ctx.strokeStyle <- U3.Case1 "#000"; ctx.lineWidth <- 2.; ctx.beginPath(); path.Invoke(globe) |> ignore; ctx.stroke()
                        box null)
                ))
                .transition()
                .each("end", unbox transition)
                |> box
        transition())

d3.select(self.frameElement).style("height", sprintf "%fpx" height)
