// ---
// header: Canvas
// tagline: Using HTML5 canvas (adapted from FunScript)
// ---
module LSystem

open Fable.Import.Browser
open Fable.Html

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

let render lines = 
  let xs = lines |> Seq.collect (fun l -> [l.startPoint.x; l.endPoint.x])
  let ys = lines |> Seq.collect (fun l -> [l.startPoint.y; l.endPoint.y])
  let minx, maxx = Seq.min xs, Seq.max xs
  let miny, maxy = Seq.min ys, Seq.max ys
  let convx x = (x - minx) / (maxx - minx) * 800.0
  let convy y = (y - miny) / (maxy - miny) * 800.0

  s?svg ["width" => 800; "height" => 800] [
    for line in lines ->
      s?line 
        [ "x1" => convx line.startPoint.x; "y1" => convy line.startPoint.y 
          "x2" => convx line.endPoint.x; "y2" => convy line.endPoint.y
          "style" => 
            sprintf "stroke:rgb(%i,%i,%i);stroke-width:1"
              line.color.r line.color.g line.color.b ] []
  ]

// A basic LOGO system
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
      c : Color}

let reverse l = 
  let rec loop acc = function [] -> acc | x::xs -> loop (x::acc) xs
  loop [] l

let chaos = System.Random(System.DateTime.Now.Millisecond)
let randomColor() = { r = (chaos.Next 256); g = (chaos.Next 256); b = (chaos.Next 256) }

/// interprets a logo program and produces a line segment list to render
let processTurtle turtle program =
    let rec phono stack output turtle = function        
        | [] -> output
        | RandomColor :: t ->
            phono stack output { turtle with c = randomColor() } t
        | Push :: t ->            
            phono (turtle::stack) output turtle t
        | Pop :: t when List.isEmpty stack ->
            failwith "Error - pop from an empty stack"
        | Pop :: t ->
            phono (List.tail stack) output (List.head stack) t
        | DrawForward d :: t -> 
            let rads = turtle.angle * (System.Math.PI / 180.0)
            let x = turtle.x + d * cos rads
            let y = turtle.y + d * sin rads
            let newTurtle = {turtle with x = x; y= y }
            let seg = 
                {   startPoint = {x = turtle.x; y = turtle.y}
                    endPoint = {x = x; y = y}
                    color = newTurtle.c }
            phono stack (seg::output) newTurtle t
            
        | Turn delta :: t -> 
            let d = turtle.angle + delta
            let d =
                // warp around logic
                if delta > 0.0 && d > 360.0 then d - 360.0
                elif delta < 0.0 && d < 0.0 then 360.0 + d
                else d
            phono stack output {turtle with angle = d} t 
                
    reverse(phono [] [] turtle program)


let defaultLength = 10.0

let cont = document.getElementById("output")
let input = document.getElementById("input") :?> HTMLTextAreaElement
let iters = document.getElementById("iterations") :?> HTMLInputElement
let angle = document.getElementById("angle") :?> HTMLInputElement

let turtle = 
  { angle = 0.0
    x = 400.0
    y = 400.0
    c = { r = 255; g = 0; b = 0 } }

let convertToTurtle (lSystemString: string) =
    let defaultAngle = 1.0 * float angle.value
    lSystemString.ToCharArray() 
    |> Array.map (function
      | '+' -> Turn(defaultAngle)
      | '-' -> Turn(-defaultAngle)
      | '!' -> RandomColor
      | '[' -> Push
      | ']' -> Pop
      | c when c >= 'A' && c <= 'Z' -> DrawForward(defaultLength)
      | _ -> failwith "Unsupported command")
    |> Array.toList

type LSystem = {
    Axiom : string
    Productions : char -> string
}

let processLsystem max lsystem =
    let rec gen (current:string) iteration =
        if iteration = max then current
        else
            let sb = [||]
            for x in current.ToCharArray() do
                sb.push(lsystem.Productions x) 
            gen (sb.join("")) (iteration+1)
              
    gen lsystem.Axiom 0
    
let ls = 
  { Axiom = "A"
    Productions = function 'A' -> "+B-A-B+" | 'B' -> "-A+B+A-" | c -> string c }

let error msg = 
  h?p [] [text msg] |> renderTo (document.getElementById("errors"))
  
let parse (s:string) = 
  h?div [] [] |> renderTo (document.getElementById("errors"))
  let prods, ax = s.Trim().Split('\n') |> Array.partition (fun s -> s.Contains("->"))
  let axiom = 
    if ax.Length <> 1 then error("There should be exactly one axiom"); "A"
    else ax.[0].Trim()
  let prods = 
    prods |> Array.map (fun s ->
      let i = s.IndexOf("->")
      let c = s.Substring(0, i).Trim()
      let c = 
        if c.Length = 1 then c.ToCharArray().[0]
        else error("Production rule should have one thing on the left"); 'A'
      let t = s.Substring(i+2).Trim()
      c, t) 
  { Axiom = axiom 
    Productions = fun c ->
      match prods |> Array.tryFind (fun (k, _) -> k = c) with
      | Some(_, r) -> r
      | _ -> string c }

let run () =
  parse input.value
  |> processLsystem (int iters.value)
  |> convertToTurtle 
  |> processTurtle turtle
  |> render
  |> renderTo cont

input.addEventListener_keyup(fun _ -> run(); box())
iters.addEventListener_change(fun _ -> run(); box())
angle.addEventListener_change(fun _ -> run(); box())

run()