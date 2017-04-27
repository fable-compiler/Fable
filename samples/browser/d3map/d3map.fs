module App

(**
 - title: D3 world tour
 - tagline: Looping through countries of the world
 - app-style: height:500px; width:500px; margin:10px auto 10px auto;
 - require-paths: `'d3':'https://d3js.org/d3.v3.min','queue': 'https://d3js.org/queue.v1.min','topojson': 'https://d3js.org/topojson.v1.min' `
 - intro: This demo is a Fable port of [Mike Bostock's World Tour](http://bl.ocks.org/mbostock/4183330)
   D3 demo. It uses the D3 library to create a visualization that loops through all countries of
   the world and shows them on the globe one by one.
   You can find the [full source code on GitHub](https://github.com/fable-compiler/Fable/blob/master/samples/browser/d3map/d3map.fsx).

   On the technical side, the demo shows some of the more interesting aspects of
   calling JavaScript libraries from Fable. You'll learn how to define mappings for
   imported scripts, how to pass lambdas to JS code and the `?` operator.
*)

(**
JavaScript helpers and imports
------------------------------

Fable comes with [an F# mapping for the D3 library](https://github.com/fable-compiler/Fable/tree/master/import/d3),
which defines all the types and functions for D3 that we'll need in this example. In addition to
D3, this demo uses [d3-queue](https://github.com/d3/d3-queue) and [topojson](https://github.com/mbostock/topojson)
which we'll just import and use dynamically:
*)
open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

let queue(): obj = importDefault "queue"
let topojson: obj = importAll "topojson"

(**

The `Import` attribute on the two values is used to import the code and make it available.
We write the arguments as if we were writing [EcmaScript 2015 modules](https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import)
like `import defaultMember from 'queue'`, then Fable/Babel will transform the modules as needed.
In this case, we use amd as a target so we can load them with [Require.js](http://requirejs.org/docs/whyamd.html).

*)

(**
Setting up the canvas and projection
------------------------------------

We will be using D3 together with HTML5 canvas, so the first step is to get the context object
and set the size of the canvas to 500x500:
*)
let width, height = 500., 500.
let canvas =  document.getElementsByTagName_canvas().[0]
canvas.width <- width
canvas.height <- height
let ctx = canvas.getContext_2d()
(**
Next, we setup the D3 orthographic projection for the globe. The `projection` object will be
used later for rotating the globe. The `path` value is used for transforming paths that we
want to render to match with the projection.
*)
let projection =
  D3.Geo.Globals.orthographic()
    .translate((width / 2., height / 2.))
    .scale(width / 2. - 20.)
    .clipAngle(90.)
    .precision(0.6)

let path =
  D3.Geo.Globals.path()
    .projection(projection :?> D3.Geo.Transform)
    .context(ctx)

let title = D3.Globals.select(".country-name")
(**
Finally, the `title` value is the HTML element in the middle of the globe that shows the
current country name. This is just an ordinary HTML element and we will set its body text
during the animation.

Generating the visualization
----------------------------

The main part of the code is defined inside a `dataLoaded` function. This gets called
after D3 loads the country names and locations. The structure of the code looks as follows:

    let dataLoaded world names =
      // (more definitions and setup)

      // Generate next transition
      let rec transition (i) =
        // (...)

      // Start the first transition
      transition (0)

The `dataLoaded` function will be called by D3 with a `world` value loaded from the
[world-110m.json](https://github.com/fable-compiler/Fable/blob/master/samples/browser/d3map/data/world-110m.json)
file that represents individual country areas; `names` loads country names from
[world-country-names.tsv](https://github.com/fable-compiler/Fable/blob/master/samples/browser/d3/data/world-country-names.tsv).

After some setup, the code defines a recursive `transition` function, which performs one transition
and then calls itself to setup the next transition step. In each step, it increments the index of
the current country, which is stored in `i`. The `transition 0` call then starts the animation.
*)
(*** hide ***)
let dataLoaded world names =
(**

### Preparing the data

Once the data is loaded, we need to do some pre-processing. First, we create a number of
D3 objects that are used to render the map - this includes `globe` for the border around
the globe, `landFeatures` (for filling land) and `borders` for rendering borders between
couuntries. We also find all countries for which we have both name and map:

*)
  // Create globe object (to render the border)
  let globe = createObj [ "type" ==> "Sphere" ]
  // Create land feature (fill the world)
  let landFeature = topojson?feature(world, world?objects?``land``)

  // Used to render country borders, specify filter to
  // prune overlapping borders (shared by 2 countries)
  let borders =
    topojson?mesh(world, world?objects?countries, (<>))

  // Get countries for which we have a name and set
  // their name property using the `?` operator.
  // We also use the `!!` operator for dynamic casting.
  let countries =
    !!topojson?feature(world, world?objects?countries)?features
    |> Array.filter (fun d ->
        names |> Seq.exists (fun n ->
          if (string d?id) = (string n?id)
          then d?name <- n?name; true
          else false))
    |> Array.sortWith (fun a b ->
          compare (string a?name) (string b?name))
(**

### Rendering the map

Now we have all we need to render the map! Given a selected country `country` and a
rotation `angle`, the following function renders the map:
*)
  /// Helper that draws or fills a line
  let draw color width line fill =
    if fill then ctx.fillStyle <- U3.Case1 color
    else ctx.strokeStyle <- U3.Case1 color
    ctx.lineWidth <- width
    ctx.beginPath()
    path.Invoke(line) |> ignore
    if fill then ctx.fill() else ctx.stroke()

  /// Render background, current country, borders & globe
  let render country angle =
      projection.rotate(!!angle) |> ignore
      ctx.clearRect(0., 0., width, height)
      draw "#ACA2AD" 0.0 landFeature true
      draw "#9E4078" 0.0 country true
      draw "#EAF1F7" 0.5 borders false
      draw "#726B72" 2.0 globe false

(**

### Creating the transition

Perhaps the most interesting part of the demo is the next one. Here, we use D3
to create the animated transition. This is done by calling `d3.transition()` and
then setting up a number of parameters:

*)
  let rec transition i =
    D3.Globals.transition()
      .duration(1250.)
      .each("start", fun _ _ ->
        // Set the text of the HTML element
        let name: D3.Primitive = !!countries.[i]?name
        !!title.text(name))
      .tween("rotate", fun _ ->
        // Interpolate the rotation & return function
        // that renders everything at a given time 't'
        let p1, p2 = D3.Geo.Globals.centroid(countries.[i])
        let r = D3.Globals.interpolate(projection.rotate(), (-p1, -p2))
        Func<_,_>(fun t -> !!render countries.[i] (r.Invoke(t))) )
      .transition()
      .each("end", fun _ _ ->
        // At the end, start the transition again!
        !!transition ((i + 1) % countries.Length))
(*** hide ***)
  transition(0)
(**
Loading the data
----------------

The last thing that we need to do in order to put everything together is to trigger the loading
of data. This is done by calling `queue().defer(...)`, which specifies that a file should be
(eventually) loaded. When the loading is done, we check for potential errors and call the
`dataLoaded` function, which then starts the first transition.
*)
queue()
  ?defer((fun url callback -> D3.Globals.json(url, callback)), "data/world-110m.json")
  ?defer(D3.Globals.tsv, "data/world-country-names.tsv")
  ?await(fun error world names ->
    if error <> null then raise error
    dataLoaded world names)
|> ignore
