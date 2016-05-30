(**
 - title: WebGL Geometry Terrain
 - tagline: A 3D world right in the browser
 - app-style: height:500px; width:500px; margin:10px auto 10px auto;
 - require-paths: ` 'three.js':'http://cdnjs.cloudflare.com/ajax/libs/three.js/r77/three.js' `
 - intro: This demo is a Fable port of the [WebGL Geometry Terrain](http://threejs.org/examples/#webgl_geometry_terrain)
   three.js demo. It uses the three.js library to randomly generate a 3D terrain which can be navigated in a first-person view.
   The code was originally written by [John Quigley](https://github.com/jmquigs) for FunScript,
   you can find [Fable's version on GitHub](https://github.com/fsprojects/Fable/blob/master/samples/browser/webGLTerrain/webGLTerrain.fsx).

   On the technical side, the demo shows some of the more interesting aspects of
   calling JavaScript libraries from Fable. You'll learn how to define mapping for
   global objects and other useful functions.
*)
(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-three/Fable.Import.Three.fs"
(**
JavaScript helpers and imports
------------------------------

Fable comes with [an F# mapping for THREE.js](https://github.com/fsprojects/Fable/tree/master/import/THREE),
which defines all the types and functions for THREE.js that we'll need in this example.
*)

open System
open Fable.Core
open Fable.Import

type IPerlin =
    abstract noise: x:float * y:float * z:float -> float

type IFirstPersonControls =
    abstract movementSpeed: float with get, set
    abstract lookSpeed: float with get, set
    abstract handleResize: unit -> unit
    abstract update: float -> unit
    
let [<Global>] ImprovedNoise(): IPerlin = failwith "JS only"
[<Emit("new THREE.FirstPersonControls($0...)")>]
let FirstPersonControls(camera: THREE.Camera): IFirstPersonControls = failwith "JS only"

// Note: this sample is intended to be a direct port of the original
// and doesn't attempt to refactor the original to be more "functional".
let main() =
    let worldWidth = 256
    let worldDepth = 256
    let worldHalfWidth = worldWidth / 2
    let worldHalfDepth = worldDepth / 2

    let clock = THREE.Clock()

    let rand() =
        JS.Math.random()

    let generateHeight width height =
        let size = width * height
        let data:float[] = Array.zeroCreate size
        let perlin = ImprovedNoise()
        let mutable quality = 1.0
        let z = rand() * 100.0

        for j in 0..3 do
            for i in 0..(size-1) do
                let x = i % width
                // I presume double-not is used here for this reason: http://james.padolsey.com/javascript/double-bitwise-not/
                let y = ~~~ (~~~ ( i / width ))
                let noise = perlin.noise(float x / quality, float y / quality, z) * quality * 1.75
                data.[i] <- data.[i] + (JS.Math.abs ( noise ))
            quality <- quality * 5.0
        data

    let generateTexture (data:float[]) (width:int) (height:int) = 
        let vector3 = THREE.Vector3(0.0, 0.0, 0.0)
        let sun = (THREE.Vector3(1.0, 1.0, 1.0) :> THREE.Vector).normalize()

        let canvas = Browser.document.createElement_canvas()
        canvas.width <- float width
        canvas.height <- float height

        let context = canvas.getContext_2d()
        context.fillStyle <- U3.Case1 "#000"
        context.fillRect(0.0, 0.0, float width, float height)

        let image = context.getImageData( 0.0, 0.0, canvas.width, canvas.height)
        let imageData = image.data

        let mutable i = 0
        let mutable j = 0
        let mutable l = int imageData.length

        while i < l do
            // Note, data elements -2 and -1 are accessed here at the start of the loop.  
            // Its a bug in the original (producing NaN after normalize()), but javascript
            // just keeps on truckin'.  There is a similar issue with z.  The result is 
            // that imageData is set to zero (black) in these cases
            vector3.x <- data.[ j - 2 ] - data.[ j + 2 ]
            vector3.y <- 2.0
            vector3.z <- data.[ j - width * 2 ] - data.[ j + width * 2 ]
            (vector3 :> THREE.Vector).normalize() |> ignore

            let shade = vector3.dot(sun :?> THREE.Vector3)

            imageData.[ i ] <- ( 96.0 + shade * 128.0 ) * ( 0.5 + data.[ j ] * 0.007 )
            imageData.[ i + 1 ] <- ( 32.0 + shade * 96.0 ) * ( 0.5 + data.[ j ] * 0.007 )
            imageData.[ i + 2 ] <- ( shade * 96.0 ) * ( 0.5 + data.[ j ] * 0.007 )

            i <- i + 4
            j <- j + 1

        context.putImageData( image, 0.0, 0.0 );

        let canvasScaled = Browser.document.createElement_canvas()
        canvasScaled.width <- float(width * 4)
        canvasScaled.height <- float(height * 4)

        let context = canvasScaled.getContext_2d()
        context.scale(4.0,4.0)
        context.drawImage(U3.Case2 canvas, 0.0, 0.0)

        let image = context.getImageData( 0.0, 0.0, canvasScaled.width, canvasScaled.height)
        let imageData = image.data

        let mutable i = 0
        let mutable l = int imageData.length
        while i < l do
            let v = ~~~ (~~~ (rand() * 5.0 |> unbox)) |> float
            imageData.[ i ] <- imageData.[ i ] + v
            imageData.[ i + 1 ] <- imageData.[ i + 1 ] + v
            imageData.[ i + 2 ] <- imageData.[ i + 2 ] + v
            i <- i + 4

        context.putImageData( image, 0.0, 0.0 )

        canvasScaled

    let init() =
        // bind window just because we use it all over the place.
        let wnd = Browser.window
        let container = Browser.document.getElementById("container")
        let camera = THREE.PerspectiveCamera(60.0, wnd.innerWidth / wnd.innerHeight, 1.0, 20000.0 )
        let scene = THREE.Scene()

        let controls = FirstPersonControls(camera :> THREE.Camera)
        controls.movementSpeed <- 1000.0
        controls.lookSpeed <- 0.1

        let data = generateHeight worldWidth worldDepth

        camera.position.y <- data.[worldHalfWidth + worldHalfDepth * worldWidth] * 10. + 500.

        let geometry = THREE.PlaneBufferGeometry(7500.0, 7500.0, float (worldWidth - 1), float (worldDepth - 1))
        geometry.applyMatrix(THREE.Matrix4().makeRotationX( - JS.Math.PI  / 2.0 )) |> ignore

        let vertices = geometry.getAttribute("position") |> unbox<THREE.BufferAttribute>
        let vertices = vertices.array
        let l = int vertices.length
        let mutable i = 0
        let mutable j = 0
        while i < l do
            vertices.[j + 1] <- data.[i] * 10.0
            i <- i + 1
            j <- j + 3

        let texCanvas = generateTexture data worldWidth worldDepth
        let texture = THREE.Texture(U3.Case2 texCanvas, three.UVMapping, three.ClampToEdgeWrapping, three.ClampToEdgeWrapping)
        texture.needsUpdate <- true

        // We use createEmpty here to create an empty object used to set configuration parameters.
        // The type qualifier indicates what fields we will be able to set on the resulting object.
        // For those fields that are enum types, the possible values are usually found in three globals.
        let matProps = createEmpty<THREE.MeshBasicMaterialParameters>
        matProps.map <- Some texture

        let mesh = THREE.Mesh(geometry, THREE.MeshBasicMaterial(matProps))
        scene.add mesh

        let renderer = THREE.WebGLRenderer()
        renderer.setClearColor("#bfd1e5")
        (renderer :> THREE.Renderer).setSize(wnd.innerWidth, wnd.innerHeight )
        container.innerHTML <- ""
        container.appendChild((renderer :> THREE.Renderer).domElement) |> ignore

        let onWindowResize(e:Browser.UIEvent):obj =
            let wnd = Browser.window
            camera.aspect <- wnd.innerWidth / wnd.innerHeight
            camera.updateProjectionMatrix()
            (renderer :> THREE.Renderer).setSize(wnd.innerWidth, wnd.innerHeight)
            controls.handleResize() 
            null

        Browser.window.addEventListener_resize(Func<_,_> onWindowResize, false)

        renderer, scene, camera, controls

    let renderer,scene,camera,controls = init()

    let render() =
        controls.update(clock.getDelta())
        renderer.render(scene, camera)

    let rec animate (dt:float) =
        Browser.window.requestAnimationFrame(Func<_,_> animate) |> ignore
        render()

    // kick it off
    animate(0.0)

main()