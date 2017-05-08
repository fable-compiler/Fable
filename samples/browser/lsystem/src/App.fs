module LSystem


open Fable.Import
open Fable.Import.Browser

open HtmlUtil

// Array hacks
[<Fable.Core.Emit("$0.push($1)")>]
let push (sb:'a[]) (v:'a) = failwith "js"
[<Fable.Core.Emit("$0.join($1)")>]
let join (sb:'a[]) (sep:string) = failwith "js"

type ``[]``<'a> with
  member x.push(v) = push x v
  member x.join(s) = join x s



type Point = { x : float; y : float }
type Color = { r:int; g:int; b:int; }
type LineSegment = {startPoint : Point; endPoint : Point; color : Color }

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


let chaos = System.Random()
let randomColor() =
  { r = (chaos.Next 256);
    g = (chaos.Next 256);
    b = (chaos.Next 256) }


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

type LSystem =
  { Axiom : string
    Productions : char -> string }


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


let error msg =
  h?p [] [
    h?strong [] [text "Error: "]
    text msg
  ] |> renderTo (document.getElementById("errors"))

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

let cont = document.getElementById("output")
let input = document.getElementById("input") :?> HTMLTextAreaElement
let iters = document.getElementById("iterations") :?> HTMLInputElement
let angle = document.getElementById("angle") :?> HTMLInputElement
let width = document.getElementById("width") :?> HTMLInputElement

let turtle =
  { angle = 0.0; x = 0.0; y = 0.0
    c = { r = 255; g = 0; b = 0 } }

let run () =
  parse input.value
  |> processLsystem (int iters.value)
  |> convertToTurtle (float angle.value)
  |> processTurtle turtle
  |> render (int width.value)
  |> renderTo cont

input.addEventListener_keyup(fun _ -> run(); box())
iters.addEventListener_change(fun _ -> run(); box())
angle.addEventListener_change(fun _ -> run(); box())
width.addEventListener_change(fun _ -> run(); box())
run()
