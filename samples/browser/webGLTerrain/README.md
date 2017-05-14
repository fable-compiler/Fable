# WebGL Geometry Terrain with Fable
A 3D world right in the browser, This demo is a Fable port of the [WebGL Geometry Terrain](http://threejs.org/examples/#webgl_geometry_terrain)
three.js demo. It uses the three.js library to randomly generate a 3D terrain which can be navigated in a first-person view.The code was originally written by [John Quigley](https://github.com/jmquigs) for FunScript.

On the technical side, the demo shows some of the more interesting aspects ofcalling JavaScript libraries from Fable. You'll learn how to define mapping for global objects and other useful functions.

Because we are using a script tag in `index.html` to reference [three.js](https://threejs.org/) we have to intruct webpack to use an `external` library instead of a library from the `node_modules` directory:
```js
externals: {
   "three": "THREE"
}
```

## Build and running the app

1. Install npm dependencies: `npm install`
2. Install dotnet dependencies: `dotnet restore`
3. Start Fable server and Webpack dev server: `dotnet fable npm-run start`
4. In your browser, open: http://localhost:8080/

# The Code
JavaScript helpers and imports
------------------------------

Fable has with [an F# mapping for three.js](https://github.com/fable-compiler/Fable/tree/master/import/three), which defines all the types and functions for three.js that we'll need in this example.

In addition this demo uses custom scripts for `ImprovedNoise` and `FirstPersonControls`. We'll write the mappings for those two inline.

```fs
/// Represents the API exposed by ImprovedNoise script
type IPerlin =
    abstract noise: x:float * y:float * z:float -> float

/// Represents the API exposed by FirstPersonControls script
type IFirstPersonControls =
    abstract movementSpeed: float with get, set
    abstract lookSpeed: float with get, set
    abstract handleResize: unit -> unit
    abstract update: float -> unit

let ImprovedNoise: unit -> IPerlin = importDefault "./js/ImprovedNoise.js"

let FirstPersonControls: JsConstructor<Three.Camera, Browser.HTMLElement, IFirstPersonControls> =
    importDefault "./js/FirstPersonControls.js"
```

Initial settings and helper functions
------------------------------------

Note: this sample is intended to be a direct port of the original and doesn't attempt to refactor the original to be more "functional".
```fs
let worldWidth = 256
let worldDepth = 256
let worldHalfWidth = worldWidth / 2
let worldHalfDepth = worldDepth / 2
let clock = Three.Clock()
```
Using the perlin library (ImprovedNoise script) define the peaks
of the mountains in our random terrain.
```fs
// generateHeight : int -> int -> float
let generateHeight width height =
    let size = width * height
    let data:float[] = Array.zeroCreate size
    let perlin = ImprovedNoise()
    let mutable quality = 1.0
    let z = rand() * 100.0

    for j in 0..3 do
        for i in 0..(size-1) do
            let x = i % width
            let y = i / width
            let noise =
                perlin.noise(float x / quality, float y / quality, z)
                    * quality * 1.75
            data.[i] <- data.[i] + (JS.Math.abs ( noise ))
        quality <- quality * 5.0
    data
```
To generate the textures for the terrain, we'll be using a canvas element
to draw the image and later pass it directly to THREE.Texture class.
```fs
let generateTexture (data:float[]) (width:int) (height:int) =
    let vector3 = Three.Vector3(0.0, 0.0, 0.0)
    let sun = (Three.Vector3(1.0, 1.0, 1.0) :> Three.Vector).normalize()

    let canvas = Browser.document.createElement_canvas()
    canvas.width <- float width
    canvas.height <- float height

    let context = canvas.getContext_2d()
    context.fillStyle <- U3.Case1 "#000"
    context.fillRect(0.0, 0.0, float width, float height)

    let image = context.getImageData(
                    0.0, 0.0, canvas.width, canvas.height)
    let imageData = image.data

    let mutable i = 0
    let mutable j = 0
    let mutable l = int imageData.length

    while i < l do
        // Note: data elements -2 and -1 are accessed here at the start
        // of the loop. It's a bug in the original (producing NaN after
        // normalize()), but JS just keeps on truckin'. There is a similar
        // issue with z.  The result is that imageData is set to zero (black)
        // in these cases
        vector3.x <- data.[ j - 2 ] - data.[ j + 2 ]
        vector3.y <- 2.0
        vector3.z <- data.[ j - width * 2 ] - data.[ j + width * 2 ]
        (vector3 :> Three.Vector).normalize() |> ignore

        let shade = vector3.dot(sun :?> Three.Vector3)

        imageData.[ i ] <-
            (96.0 + shade * 128.0) * (0.5 + data.[ j ] * 0.007)
        imageData.[ i + 1 ] <-
            (32.0 + shade * 96.0) * (0.5 + data.[ j ] * 0.007)
        imageData.[ i + 2 ] <-
            (shade * 96.0) * (0.5 + data.[ j ] * 0.007)

        i <- i + 4
        j <- j + 1

    context.putImageData( image, 0.0, 0.0 );

    let canvasScaled = Browser.document.createElement_canvas()
    canvasScaled.width <- float(width * 4)
    canvasScaled.height <- float(height * 4)

    let context = canvasScaled.getContext_2d()
    context.scale(4.0,4.0)
    context.drawImage(U3.Case2 canvas, 0.0, 0.0)

    let image = context.getImageData(
                    0.0, 0.0, canvasScaled.width, canvasScaled.height)
    let imageData = image.data

    let mutable i = 0
    let mutable l = int imageData.length
    while i < l do
        // I presume double-not is used here for this reason:
        // http://james.padolsey.com/javascript/double-bitwise-not/
        let v = ~~~ (~~~ (rand() * 5.0 |> int)) |> float
        imageData.[ i ] <- imageData.[ i ] + v
        imageData.[ i + 1 ] <- imageData.[ i + 1 ] + v
        imageData.[ i + 2 ] <- imageData.[ i + 2 ] + v
        i <- i + 4

    context.putImageData(image, 0.0, 0.0)
    canvasScaled
```
Initialize elements
-------------------

Here we initialize the elements necessary to draw the scene:
the renderer, the scene itself, a camera and controls to move it.
```fs
let init() =
    let getWidth() = Browser.window.innerWidth - 300.0
    let getHeight() = Browser.window.innerHeight - 40.0

    let container = Browser.document.getElementById("container")
    let camera = Three.PerspectiveCamera(
                    60.0, getWidth() / getHeight(), 1.0, 20000.0)
    let scene = Three.Scene()

    let renderer = Three.WebGLRenderer()
    renderer.setClearColor("#bfd1e5")
    (renderer :> Three.Renderer).setSize(getWidth(), getHeight())
    let domElement = (renderer :> Three.Renderer).domElement
    container.innerHTML <- ""
    container.appendChild(domElement) |> ignore
    Browser.console.log(camera)
    let controls = FirstPersonControls.Create(!!camera, !!domElement)
    controls.movementSpeed <- 1000.0
    controls.lookSpeed <- 0.1

    let data = generateHeight worldWidth worldDepth

    camera.position.y <-
        data.[worldHalfWidth + worldHalfDepth * worldWidth] * 10. + 500.

    let geometry = Three.PlaneBufferGeometry(
                        7500.0, 7500.0,
                        float (worldWidth - 1), float (worldDepth - 1))
    geometry.applyMatrix(Three.Matrix4()
            .makeRotationX(-JS.Math.PI / 2.0))
            |> ignore

    let vertices = geometry.getAttribute("position")
                    |> unbox<Three.BufferAttribute>
    let vertices = vertices.array
    let l = int vertices.length
    let mutable i = 0
    let mutable j = 0
    while i < l do
        vertices.[j + 1] <- data.[i] * 10.0
        i <- i + 1
        j <- j + 3

    let texCanvas = generateTexture data worldWidth worldDepth
    let texture = Three.Texture(
                    U3.Case2 texCanvas, Three.UVMapping,
                    Three.ClampToEdgeWrapping,
                    Three.ClampToEdgeWrapping)
    texture.needsUpdate <- true

    // We use createEmpty here to create an empty object used to set
    // configuration parameters. The type qualifier indicates what fields
    // we will be able to set on the resulting object. For those fields that
    // are enum types, the possible values are usually found in three globals.
    let matProps = createEmpty<Three.MeshBasicMaterialParameters>
    matProps.map <- Some texture

    let mesh = Three.Mesh(geometry, Three.MeshBasicMaterial(matProps))
    scene.add mesh

    let onWindowResize(e:Browser.UIEvent):obj =
        camera.aspect <- getWidth() / getHeight()
        camera.updateProjectionMatrix()
        (renderer :> Three.Renderer).setSize(getWidth(), getHeight())
        controls.handleResize()
        null

    Browser.window.addEventListener_resize(
        Func<_,_> onWindowResize, false)

    renderer, scene, camera, controls
```
Start animation
---------------

Now the only thing left is to start the animation. Note we use the
`window.requestAnimationFrame` function here, this will make sure
`animate` is executed at a proper frame rate.
```fs
let render() =
    controls.update(clock.getDelta())
    renderer.render(scene, camera)

let rec animate (dt:float) =
    Browser.window.requestAnimationFrame(Func<_,_> animate)
    |> ignore
    render()

// kick it off
animate(0.0)

```