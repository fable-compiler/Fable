# LSystems: Interactive fractal generator
This demo is based on [L-system workshop](https://github.com/Andrea/Lsystem) by Andrea Magnorsky
and Ross McKinlay. Rather than working on the tasks they gave us, I compiled it with Fable, so that you can play with it in a browser!

The demo lets you enter a simple [L-system](https://en.wikipedia.org/wiki/L-system) with a single
entry point and a number of rewriting rules written using `->`. You can use letters to draw a line
forward, `-` and `+` to turn (left and right) and `!` to randomly change color. It also supports
brackets, e.g. `[[-X]+X]`, for undoing state changes.

# Install
1. Install npm dependencies: `npm install` or `yarn install`
2. Install dotnet dependencies: `dotnet restore`
3. Start Fable server and Webpack dev server: `dotnet fable npm-run start`
4. In your browser, open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

# The Code
### Rendering fractals with SVG
The demo uses SVG to render fractals. Once the fractal is generated, it is turned into a sequence
of lines that are then rendered. Each line has a starting and ending point together with a
color. We represent this using a simple F# domain model:
```fs
type Point = { x : float; y : float }
type Color = { r:int; g:int; b:int; }
type LineSegment = {startPoint : Point; endPoint : Point; color : Color }
```
When rendering lines, we generate `<svg>` tag containing a number of
`<line>` elements. This is done using a simple helper library (which you can find
[in the Fable repository](https://github.com/fable-compiler/Fable/blob/master/samples/browser/lsystem/src/Html.fs).
The library uses the dynamic operator `?` to generate SVG elements. For example, you can write:

    s?svg [ "width" => 600 ] [
      s?line [ "x1" => 100; "y1" => 100; "x2" => 200; "y2" => 200 ]
    ]

The result of `s?svg` is a function that takes a list of attributes and a list of nested elements.
Writing a rendering function is now easy - we just generate root SVG element with a line for each
`LineSegment`. The only slightly complex aspect is that we re-scale the image to fit into the
600x600 box automatically:
```fs
let render lineWidth lines =
  // Calculate minimal and maximal X/Y values for scaling
  let xs = lines |> Seq.collect (fun l -> [l.startPoint.x; l.endPoint.x])
  let ys = lines |> Seq.collect (fun l -> [l.startPoint.y; l.endPoint.y])
  let minx, maxx = Seq.min xs, Seq.max xs
  let miny, maxy = Seq.min ys, Seq.max ys
  let convx x = (x - minx) / (maxx - minx) * 600.0
  let convy y = (y - miny) / (maxy - miny) * 600.0

  // Generate root SVG tag with line tag for each line sgement
  s?svg ["width" => 600; "height" => 600] [
    for line in lines ->
      s?line
        [ "x1" => convx line.startPoint.x; "y1" => convy line.startPoint.y
          "x2" => convx line.endPoint.x; "y2" => convy line.endPoint.y
          "style" =>
            sprintf "stroke:rgb(%i,%i,%i);stroke-width:%i"
              line.color.r line.color.g line.color.b lineWidth ] []
  ]
```
### From turtle graphics to line sgemnets
To make the evaluation of L-systems easier, we first turn L-system into commands of a simple
turtle graphics engine:
```fs
type LogoCommand =
  | DrawForward of float
  | Turn of float
  | Push
  | Pop
  | RandomColor

type LTurtle =
  { angle : float
    x : float
    y : float
    c : Color }
```
The `DrawForward` and `Turn` commands move the turtle forward or rotate it. The `Push` and `Pop`
commands are used to implement the brackets - `Push` stores the current state of the turtle and
`Pop` restores the previous state. Finally, `RandomColor` corresponds to `!` and changes the color
to a new, randomly generated one:
```fs
let chaos = System.Random()
let randomColor() =
  { r = (chaos.Next 256);
    g = (chaos.Next 256);
    b = (chaos.Next 256) }
```
The interpreter of the turtle graphics needs to keep track of the current position and direction
of the turtle. It iterates over the commands one by one and either updates the current state or
generates a line segment:
```fs
/// interprets a logo program and produces a line segment list to render
let processTurtle turtle program =
  let rec phono stack output turtle = function
    | [] -> output
    | RandomColor :: t ->
        // Change current color of the turtle
        phono stack output { turtle with c = randomColor() } t
    | Push :: t ->
        // Store current state on the stack
        phono (turtle::stack) output turtle t
    | Pop :: t when List.isEmpty stack ->
        // Silently ignore errors when stack is empty
        phono stack output turtle t
    | Pop :: t ->
        // Pop the most recent turtle state from the stack
        phono (List.tail stack) output (List.head stack) t

    | DrawForward d :: t ->
        // Move forward by `d` in the current direction
        let rads = turtle.angle * (System.Math.PI / 180.0)
        let x = turtle.x + d * cos rads
        let y = turtle.y + d * sin rads
        let newTurtle = {turtle with x = x; y= y }
        let seg =
          { startPoint = {x = turtle.x; y = turtle.y}
            endPoint = {x = x; y = y}; color = turtle.c }
        phono stack (seg::output) newTurtle t

    | Turn delta :: t ->
        // Rotate by the specified angle
        let d = turtle.angle + delta
        let d =
          if delta > 0.0 && d > 360.0 then d - 360.0
          elif delta < 0.0 && d < 0.0 then 360.0 + d
          else d
        phono stack output {turtle with angle = d} t

  List.rev (phono [] [] turtle program)
```
### Processing and rendering L-systems
An L-system is represented by an initial state (called an axiom) and a function that returns the
production (new L-system string on the right of `->`) for a given character:
```fs
type LSystem =
  { Axiom : string
    Productions : char -> string }
```
When processing an L-system, we start with an initial string and repeatedly (for a given number
of iterations) replace all the characters in the string with a new string produced by the
`Productions` function. To do this efficiently, we're going to use mutable JavaScript arrays.
In JavaScript, you can call `ar.push(x)` on an array `ar` to add an element `x` to the end of the
array. We can use F# extensions and `Emit` attribtue to enable calling this member on an F# array
```fs
[<Fable.Core.Emit("$0.push($1)")>]
let push (sb:'a[]) (v:'a) = failwith "js"
[<Fable.Core.Emit("$0.join($1)")>]
let join (sb:'a[]) (sep:string) = failwith "js"

type ``[]``<'a> with
  member x.push(v) = push x v
  member x.join(s) = join x s
```
(this is slightly ugly, but it does the trick!):
Now we have everything we need to implement a function that processes the L-system:
```fs
let processLsystem max lsystem =
  let rec gen (current:string) iteration =
    if iteration = max then current else
    // Iterate over characters, appending the result of
    // production to the mutable array `sb` of strings
    let sb = [||]
    for x in current.ToCharArray() do
      sb.push(lsystem.Productions x)
    gen (sb.join("")) (iteration+1)

  // Start with the initial axiom
  gen lsystem.Axiom 0
```
The `processLsystem` function turns an L-system specification into a single large string that
represents the actions that we want to do. This pretty much directly corresponds to the turtle
commands that we defined earlier:
```fs
// By default, go forward by 10 pixels
let defaultLength = 10.0

// Convert processed l-system string to turtle commands
let convertToTurtle angle (lSystemString: string) =
  let defaultAngle = 1.0 * angle
  lSystemString.ToCharArray() |> Array.map (function
    // Special commands that mean something
    | '+' -> Turn(defaultAngle)
    | '-' -> Turn(-defaultAngle)
    | '!' -> RandomColor
    | '[' -> Push
    | ']' -> Pop
    // Anything else is treated as move forward
    | _ -> DrawForward(defaultLength) )
  |> Array.toList
```
### Parsing L-system specifications
Now we are almost done - we just need to parse the L-system specification given by the user and
then we need to put everything together. There are a number of things that can go wrong when parsing
the specification, so we'll need a function for reporting errors. This uses the HTML library
that we used for generating SVG images:
```fs
let error msg =
  h?p [] [
    h?strong [] [text "Error: "]
    text msg
  ] |> renderTo (document.getElementById("errors"))
```
When parsing an input, we split it into lines. Each line is either an axiom (when it does not
contain `->`) or a production rule. When parsing productions, we simply find the part before
`->` and after `->` and create a function that uses the collected rules to implement mapping
that we can use to construct `LSystem` value:
```fs
let parse (s:string) =
  // Clear errors and split lines into axioms and procutions
  h?div [] [] |> renderTo (document.getElementById("errors"))
  let prods, ax = s.Trim().Split('\n') |> Array.partition (fun s -> s.Contains("->"))

  // There shold be exactly one axiom
  let axiom =
    if ax.Length <> 1 then error("There should be exactly one axiom"); "A"
    else ax.[0].Trim()

  // Collect production rules
  let prods =
    prods |> Array.map (fun s ->
      let i = s.IndexOf("->")
      let c = s.Substring(0, i).Trim()
      let c =
        if c.Length = 1 then c.ToCharArray().[0]
        else error("Production rule should have one thing on the left"); 'A'
      let t = s.Substring(i+2).Trim()
      c, t)

  // Build an L-system specification
  { Axiom = axiom
    Productions = fun c ->
      match prods |> Array.tryFind (fun (k, _) -> k = c) with
      | Some(_, r) -> r
      | _ -> string c }
```
### Putting everything together
The last step is to implement the user interface. We have a number of elements on the page
that we can access using `document.getElementById` - for some, we need to access their value,
so we cast them to the appropriate HTML element type:
```fs
let cont = document.getElementById("output")
let input = document.getElementById("input") :?> HTMLTextAreaElement
let iters = document.getElementById("iterations") :?> HTMLInputElement
let angle = document.getElementById("angle") :?> HTMLInputElement
let width = document.getElementById("width") :?> HTMLInputElement
```
An initial turtle starts in the middle (the position will be re-scaled later, so it does not
matter) and has initially red color:
```fs
let turtle =
  { angle = 0.0; x = 0.0; y = 0.0
    c = { r = 255; g = 0; b = 0 } }
```
Now the most beautiful part of the source code - the `run` function just composes all the steps
that we created so far using the `|>` operator. We take the input, parse it, run the L-system using
the given number of iterations, turn it into Turtle commands, turn that into line segments and
render the line segments:
```fs
let run () =
  parse input.value
  |> processLsystem (int iters.value)
  |> convertToTurtle (float angle.value)
  |> processTurtle turtle
  |> render (int width.value)
  |> renderTo cont
```
Finally, we call `run` when any of the configuration parameters change and we also run it when
the page loads:
```fs
input.addEventListener_keyup(fun _ -> run(); box())
iters.addEventListener_change(fun _ -> run(); box())
angle.addEventListener_change(fun _ -> run(); box())
width.addEventListener_change(fun _ -> run(); box())
run()
```