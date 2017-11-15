// Source: http://www.tryfsharp.org/create/cpoulain/shared/raytracer.fsx

module RayTrace
open System
open System.Text

type Vector(x:float, y:float, z:float) =
    member this.X = x
    member this.Y = y
    member this.Z = z
    static member ( * ) (k, (v:Vector)) = Vector(k*v.X, k*v.Y, k*v.Z)
    static member ( - ) (v1:Vector, v2:Vector) = Vector(v1.X-v2.X, v1.Y-v2.Y, v1.Z-v2.Z)
    static member ( + ) (v1:Vector, v2:Vector) = Vector(v1.X+v2.X, v1.Y+v2.Y, v1.Z+v2.Z)
    static member Dot (v1:Vector, v2:Vector) = v1.X*v2.X + v1.Y*v2.Y + v1.Z*v2.Z
    static member Mag (v:Vector) = sqrt(v.X*v.X + v.Y*v.Y + v.Z*v.Z)
    static member Norm (v:Vector) =
        let mag = Vector.Mag v
        let div = if mag = 0.0 then infinity else 1.0/mag
        div * v
    static member Cross (v1:Vector, v2:Vector) =
        Vector(v1.Y * v2.Z - v1.Z * v2.Y,
               v1.Z * v2.X - v1.X * v2.Z,
               v1.X * v2.Y - v1.Y * v2.X)

type Color(r:float, g:float, b:float) =
    static let clamp v = Math.Floor(255.0 * Math.Min(v, 1.0))
    member this.R = r
    member this.G = g
    member this.B = b
    static member Scale (k, v:Color) = Color(k*v.R, k*v.G, k*v.B)
    static member ( + ) (v1:Color, v2:Color) = Color(v1.R+v2.R, v1.G+v2.G, v1.B+v2.B)
    static member ( * ) (v1:Color, v2:Color) = Color(v1.R*v2.R, v1.G*v2.G, v1.B*v2.B)
    static member White = Color(1.0,1.0,1.0)
    static member Grey = Color(0.5,0.5,0.5)
    static member Black = Color(0.0,0.0,0.0)
    static member Background = Color.Black
    static member DefaultColor = Color.Black
    static member ToDrawingColor (c:Color) = new Color(clamp c.R, clamp c.G, clamp c.B)

type Camera(pos : Vector, lookAt : Vector) =
    let forward = Vector.Norm(lookAt - pos)
    let down = Vector(0.0,-1.0,0.0)
    let right = 1.5 * Vector.Norm(Vector.Cross(forward, down))
    let up = 1.5 * Vector.Norm(Vector.Cross(forward, right))
    member c.Pos     = pos
    member c.Forward = forward
    member c.Up      = up
    member c.Right   = right

type Ray =
    { Start: Vector;
      Dir: Vector }

type Surface =
    abstract Diffuse: Vector -> Color;
    abstract Specular: Vector -> Color;
    abstract Reflect: Vector -> double;
    abstract Roughness : double

type Intersection =
    { Thing: SceneObject;
      Ray: Ray;
      Dist: double }

and SceneObject =
    abstract Surface : Surface
    abstract Intersect : Ray -> Intersection option
    abstract Normal : Vector -> Vector

let Sphere(center, radius, surface) =
    let radius2 = radius * radius
    { new SceneObject with
        member this.Surface = surface
        member this.Normal pos = Vector.Norm(pos - center)
        member this.Intersect (ray : Ray)  =
            let eo = center - ray.Start
            let v = Vector.Dot(eo, ray.Dir)
            let dist =
                if (v<0.0)
                then 0.0
                else let disc = radius2 - (Vector.Dot(eo,eo) - (v*v))
                     if disc < 0.0
                     then 0.0
                     else v - (sqrt(disc))
            if dist = 0.0
            then None
            else Some {Thing = this; Ray = ray; Dist = dist}
    }

let Plane(norm, offset, surface) =
    { new SceneObject with
        member this.Surface = surface
        member this.Normal pos = norm
        member this.Intersect (ray) =
            let denom = Vector.Dot(norm, ray.Dir)
            if denom > 0.0
            then None
            else let dist = (Vector.Dot(norm, ray.Start) + offset) / (-denom)
                 Some { Thing = this; Ray = ray; Dist = dist }
    }

type Light =
    { Pos : Vector;
      Color : Color }

type Scene =
    { Things : SceneObject list;
      Lights : Light list;
      Camera : Camera }

type RayTracer(screenWidth, screenHeight) =

    let maxDepth = 5

    let Intersections ray scene =
        scene.Things
        |> List.choose (fun sceneObj -> sceneObj.Intersect(ray))
        |> List.sortBy (fun intersection -> intersection.Dist)

    let TestRay (ray, scene) =
        match Intersections ray scene with
        | [] -> None
        | isect::_ -> Some isect.Dist

    let rec TraceRay (ray,scene,depth : int) =
        match Intersections ray scene with
        | [] -> Color.Background
        | isect::_ -> Shade isect  scene  depth

    and Shade isect scene depth =
        let d = isect.Ray.Dir
        let pos = isect.Dist * d + isect.Ray.Start
        let normal = isect.Thing.Normal(pos)
        let reflectDir = d - 2.0 * Vector.Dot(normal, d) * normal
        let naturalcolor = Color.DefaultColor +
                           GetNaturalColor(isect.Thing, pos, normal, reflectDir, scene)
        let reflectedColor = if depth >= maxDepth
                             then Color(0.5,0.5,0.5)
                             else GetReflectionColor(isect.Thing, pos + (0.001*reflectDir), normal, reflectDir, scene, depth)
        naturalcolor + reflectedColor

    and GetReflectionColor (thing : SceneObject ,pos,normal : Vector,rd : Vector,scene : Scene, depth : int) =
        Color.Scale(thing.Surface.Reflect(pos), TraceRay ( { Start = pos; Dir = rd }, scene, depth + 1))

    and GetNaturalColor (thing, pos, norm, rd, scene) =
        let addLight col (light : Light) =
            let ldis = light.Pos - pos
            let livec = Vector.Norm(ldis)
            let neatIsect = TestRay({Start = pos; Dir = livec}, scene)
            let isInShadow = match neatIsect with
                             | None -> false
                             | Some d -> not (d > Vector.Mag(ldis))
            if isInShadow
            then col
            else let illum = Vector.Dot(livec, norm)
                 let lcolor = if illum > 0.0
                              then Color.Scale(illum, light.Color)
                              else Color.DefaultColor
                 let specular = Vector.Dot(livec, Vector.Norm(rd))
                 let scolor = if specular > 0.0
                              then Color.Scale(System.Math.Pow(specular, thing.Surface.Roughness), light.Color)
                              else Color.DefaultColor
                 col + thing.Surface.Diffuse(pos) * lcolor +
                       thing.Surface.Specular(pos) * scolor
        List.fold addLight
                       Color.DefaultColor
                       scene.Lights

    let GetPoint x y (camera:Camera) =
        let RecenterX x =  (float x - (float screenWidth / 2.0))  / (2.0 * float screenWidth)
        let RecenterY y = -(float y - (float screenHeight / 2.0)) / (2.0 * float screenHeight)
        Vector.Norm(camera.Forward + RecenterX(x) * camera.Right + RecenterY(y) * camera.Up)

    member this.Render(scene, rgb : Color[]) =
        for y = 0 to  screenHeight - 1 do
            let stride = y * screenWidth
            for x = 0 to screenWidth - 1 do
                let color = TraceRay ({Start = scene.Camera.Pos; Dir = GetPoint x y scene.Camera }, scene, 0)
                rgb.[x + stride] <- Color.ToDrawingColor color
        rgb

module Surfaces =
    let Shiny =
        { new Surface with
            member s.Diffuse pos = Color.White
            member s.Specular pos = Color.Grey
            member s.Reflect pos = 0.7
            member s.Roughness = 250.0 }
    let Checkerboard =
        { new Surface with
            member s.Diffuse pos =
                if (int (System.Math.Floor(pos.Z) + System.Math.Floor(pos.X))) % 2 <> 0
                then Color.White
                else Color.Black
            member s.Specular pos = Color.White
            member s.Reflect pos =
                if (int (System.Math.Floor(pos.Z) + System.Math.Floor(pos.X))) % 2 <> 0
                then 0.1
                else 0.7
            member s.Roughness = 150.0 }

let scene =
    { Things = [ Plane( Vector(0.0,1.0,0.0), 0.0, Surfaces.Checkerboard);
                 Sphere( Vector(0.0,1.0,-0.25), 1.0, Surfaces.Shiny)
                 Sphere( Vector(-1.0, 0.5, 1.5), 0.5, Surfaces.Shiny) ];
      Lights = [ { Pos = Vector(-2.0, 2.5, 0.0); Color = Color(0.49, 0.07, 0.07) };
                 { Pos = Vector(1.5, 2.5, 1.5); Color = Color(0.07, 0.07, 0.49) };
                 { Pos = Vector(1.5, 2.5, -1.5); Color = Color(0.07, 0.49, 0.071) };
                 { Pos = Vector(0.0, 3.5, 0.0); Color = Color(0.21, 0.21, 0.35) } ];
      Camera = Camera(Vector(3.0, 2.0, 4.0), Vector(-1.0, 0.5, 0.0)) }

// Compute the scene
let computeScene width height =
    let raytracer = RayTracer(width, height)
    let rgbBuffer = Array.zeroCreate (width * height)
    let colors = raytracer.Render(scene, rgbBuffer)
    colors
    // |> Array.fold (fun (sb:StringBuilder) c -> sb.AppendFormat("#{0:x2}{1:x2}{2:x2}", int c.R, int c.G, int c.B)) (new StringBuilder())
    // |> fun s -> TryFSharp.Canvas.JavaScriptFunction("RayTracer.render").Invoke(s.ToString())
    // |> ignore

let measure f x y =
    let dtStart = DateTime.UtcNow
    let res = f x y
    let elapsed = DateTime.UtcNow - dtStart
    res, elapsed.TotalSeconds

let x,y = 100,100
[1..3] |> List.iter (fun i ->
    let colors, elapsed = measure computeScene x y
    printfn "run %d: Ray tracing scene size (%d,%d), elapsed %f sec" i x y elapsed )
