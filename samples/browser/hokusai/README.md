## Hokusai and Julia: rendering fractals using HTML5 canvas
This demo is based on Tomas Petricek's [F# Advent Calendar post](http://tomasp.net/blog/2014/japan-advent-art-en/) that explores Japanese art using the (now defunct) Freebase type provider and renders The Great Wave by Hokusai using the Julia fractal.

## Build and running the app

1. Install npm dependencies: `npm install` or `yarn install`
2. Install dotnet dependencies: `dotnet restore`
3. Start Fable server and Webpack dev server: `dotnet fable npm-run start`
4. In your browser, open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

# The Code

### Complex Numbers
Before looking at the fractal, we need a simple type for working with
complex numbers that supports the `+` operation and the `abs` and `pow` functions.
We define the type as a simple wrapper over a pair of floating point numbers and
add `Abs` and `+` as static methods. This way, they can be used through the usual
F# functions:
```fs
type Complex =
  | Complex of float * float
  /// Calculate the absolute value of a complex number
  static member Abs(Complex(r, i)) =
    let num1, num2 = abs r, abs i
    if (num1 > num2) then
      let num3 = num2 / num1
      num1 * sqrt(1.0 + num3 * num3)
    elif num2 = 0.0 then
      num1
    else
      let num4 = num1 / num2
      num2 * sqrt(1.0 + num4 * num4)
  /// Add real and imaginary components pointwise
  static member (+) (Complex(r1, i1), Complex(r2, i2)) =
    Complex(r1+r2, i1+i2)
```
Before moving forward, we also need to calculate a power of complex numbers.
To do this, we define a `Pow` function in a helper module:
```fs
module ComplexModule =
  /// Calculates nth power of a complex number
  let Pow(Complex(r, i), power) =
    let num = Complex.Abs(Complex(r, i))
    let num2 = atan2 i r
    let num3 = power * num2
    let num4 = num ** power
    Complex(num4 * cos(num3), num4 * sin(num3))
```
### Calculating the Julia set
Now we have all we need to calculate the Julia set fractal. We choose
a carefuly chosen (handcrafted!) starting point. Then we create a sequence
of powers using F# sequence expressions:
```fs
/// Constant that generates nice fractal
let c = Complex(-0.70176, -0.3842)

/// Generates sequence for given coordinates
let iterate x y =
  let rec loop current = seq {
    yield current
    yield! loop (ComplexModule.Pow(current, 2.0) + c) }
  loop (Complex(x, y))
```
The `iterate` lazilly function generates potentially infinite sequence of
values. We take at most `max` iterations or stop when the absolute value of
the number is greater than 2. This can be nicely written using `Seq` functions
from the standard F# library (supported by Fable):
```fs
// int -> float -> float -> int
let countIterations max x y =
  iterate x y
  |> Seq.take (max - 1)
  |> Seq.takeWhile (fun v -> Complex.Abs(v) < 2.0)
  |> Seq.length
```
### Generating the color palette
To generate a pretty picture, we need to carefuly generate the color palette.
To do this, we define a pair of operators that let us write
`(rgb1) --n--> (rbg2)` and generate a range of colors between `rgb1` and `rgb2`
consisting of `n` steps.
```fs
let (--) clr count = clr, count
let (-->) ((r1,g1,b1), count) (r2,g2,b2) = [
  for c in 0 .. count - 1 ->
    let k = float c / float count
    let mid v1 v2 =
      (float v1 + ((float v2) - (float v1)) * k)
    (mid r1 r2, mid g1 g2, mid b1 b2) ]
```
Now we can generate palette that is based on Hokusai's famous painting:
```fs
// Palette with colors used by Hokusai
let palette =
  [| // 3x sky color & transition to light blue
     yield! (245,219,184) --3--> (245,219,184)
     yield! (245,219,184) --4--> (138,173,179)
     // to dark blue and then medium dark blue
     yield! (138,173,179) --4--> (2,12,74)
     yield! (2,12,74)     --4--> (61,102,130)
     // to wave color, then light blue & back to wave
     yield! (61,102,130)  -- 8--> (249,243,221)
     yield! (249,243,221) --32--> (138,173,179)
     yield! (138,173,179) --32--> (61,102,130) |]
```
### Drawing the fractal
The last step is to render the fractal. To do that, we first define a couple of constants
and helpers. The following constants define what part of the fractal we're rendering and
how big is the canvas:
```fs
// Specifies what range of the set to draw
let w = -0.4, 0.4
let h = -0.95, -0.35
// Create bitmap that matches the size of the canvas
let width = 400.0
let height = 300.0
```
Next, we define `setPixel` that sets the RGBA colours of a specified pixel in the canvas
and we'll use F# dynamic operator so that `doc?canvas` returns an HTML element with ID
`canvas`:
```fs
let setPixel (img:ImageData) x y width (r, g, b) =
  let index = (x + y * int width) * 4
  img.data.[index+0] <- r
  img.data.[index+1] <- g
  img.data.[index+2] <- b
  img.data.[index+3] <- 255.0
```
We also use a helper dynamic operator to get an element from the document:
```fs
/// Dynamic operator that returns HTML element by ID
let (?) (doc:Document) name :'R =
  doc.getElementById(name) :?> 'R
```
The rendering itself is written as an F# asynchronous workflow. The workflow sleeps for
1ms after rendering each line of the fractal. Behind the scenes, this unblocks the window
via a timer, so that the JavaScript function call does not block the browser while running.
```fs
/// Render fractal asynchronously with sleep after every line
let render () = async {
  // Get <canvas> element & create image for drawing
  let canv : HTMLCanvasElement = document?canvas
  let ctx = canv.getContext_2d()
  let img = ctx.createImageData(U2.Case1 (float width), float height)

  // For each pixel, transform to the specified range
  // and get color using countInterations and palette
  for x in 0 .. int width - 1 do
    for y in 0 .. int height - 1 do
      let x' = (float x / width * (snd w - fst w)) + fst w
      let y' = (float y / height * (snd h - fst h)) + fst h
      let it = countIterations palette.Length x' y'
      setPixel img x y width palette.[it]

    // Insert non-blocking waiting & update the fractal
    do! Async.Sleep(1)
    ctx.putImageData(img, 0.0, 0.0) }
```
Now we just need to register the event handler for the `go` button and start the
asynchronous workflow to do the rendering. Note that this is done using `Async.StartImmediate`:
```fs
let go : HTMLButtonElement = document?go
go.addEventListener_click(fun _ ->
  render() |> Async.StartImmediate; null)
```