module Fable.Tests.NBody

open Util.Testing

let PI: float = 3.141592653589793
let SOLAR_MASS: float = 4.0 * PI * PI
let YEAR: float = 365.24
let N_BODIES: int = 5

type Planet =
    { mutable x: float
      mutable y: float
      mutable z: float
      mutable vx: float
      mutable vy: float
      mutable vz: float
      mass: float }

let Sun: Planet =
    { x = 0.0
      y = 0.0
      z = 0.0
      vx = 0.0
      vy = 0.0
      vz = 0.0
      mass = SOLAR_MASS }

let Jupiter: Planet =
    { x = 4.84143144246472090e+00
      y = -1.16032004402742839e+00
      z = -1.03622044471123109e-01
      vx = 1.66007664274403694e-03 * YEAR
      vy = 7.69901118419740425e-03 * YEAR
      vz = -6.90460016972063023e-05 * YEAR
      mass = 9.54791938424326609e-04 * SOLAR_MASS }

let Saturn: Planet =
    { x = 8.34336671824457987e+00
      y = 4.12479856412430479e+00
      z = -4.03523417114321381e-01
      vx = -2.76742510726862411e-03 * YEAR
      vy = 4.99852801234917238e-03 * YEAR
      vz = 2.30417297573763929e-05 * YEAR
      mass = 2.85885980666130812e-04 * SOLAR_MASS }

let Uranus: Planet =
    { x = 1.28943695621391310e+01
      y = -1.51111514016986312e+01
      z = -2.23307578892655734e-01
      vx = 2.96460137564761618e-03 * YEAR
      vy = 2.37847173959480950e-03 * YEAR
      vz = -2.96589568540237556e-05 * YEAR
      mass = 4.36624404335156298e-05 * SOLAR_MASS }

let Neptune: Planet =
    { x = 1.53796971148509165e+01
      y = -2.59193146099879641e+01
      z = 1.79258772950371181e-01
      vx = 2.68067772490389322e-03 * YEAR
      vy = 1.62824170038242295e-03 * YEAR
      vz = -9.51592254519715870e-05 * YEAR
      mass = 5.15138902046611451e-05 * SOLAR_MASS }

let BODIES =
    [| Sun
       Jupiter
       Saturn
       Uranus
       Neptune |]

let advance (bodies: Planet[], dt: float) =
    let size = N_BODIES - 1

    for i in 0 .. size do
        let mutable bi = bodies.[i]

        for j in (i + 1) .. size do
            let mutable bj = bodies.[j]

            let dx = bi.x - bj.x
            let dy = bi.y - bj.y
            let dz = bi.z - bj.z

            let d2 = dx * dx + dy * dy + dz * dz
            let mag = dt / (d2 * sqrt (d2))

            let massj_mag = bj.mass * mag
            bi.vx <- bi.vx - dx * massj_mag
            bi.vy <- bi.vy - dy * massj_mag
            bi.vz <- bi.vz - dz * massj_mag

            let massi_mag = bi.mass * mag
            bj.vx <- bj.vx + dx * massi_mag
            bj.vy <- bj.vy + dy * massi_mag
            bj.vz <- bj.vz + dz * massi_mag

        bi.x <- bi.x + dt * bi.vx
        bi.y <- bi.y + dt * bi.vy
        bi.z <- bi.z + dt * bi.vz

let energy (bodies: Planet[]) : float =
    let mutable e = 0.0
    let size = N_BODIES - 1

    for i in 0 .. size do
        let bi = bodies.[i]
        e <- e + (bi.vx * bi.vx + bi.vy * bi.vy + bi.vz * bi.vz) * bi.mass / 2.0

        for j in (i + 1) .. size do
            let bj = bodies.[j]
            let dx = bi.x - bj.x
            let dy = bi.y - bj.y
            let dz = bi.z - bj.z
            let dist = sqrt (dx * dx + dy * dy + dz * dz)
            e <- e - bi.mass * bj.mass / dist
    e

let offset_momentum (bodies: Planet[]) =
    let mutable px = 0.0
    let mutable py = 0.0
    let mutable pz = 0.0

    for bi in bodies do
        px <- px + bi.vx * bi.mass
        py <- py + bi.vy * bi.mass
        pz <- pz + bi.vz * bi.mass

    let sun = bodies.[0]
    sun.vx <- -px / SOLAR_MASS
    sun.vy <- -py / SOLAR_MASS
    sun.vz <- -pz / SOLAR_MASS

let init () = offset_momentum (BODIES)

let step () : float =
    advance (BODIES, 0.01)
    energy (BODIES)

let bench (steps: int, dt: float) : float =
    for _ in 0 .. steps do
        advance (BODIES, dt)
    energy (BODIES)

[<Fact>]
let ``NBody calc works`` () =
    let dt = 0.01
    let steps = 1_000_000
    init()
    let energy = bench(steps, dt)
    energy |> equal -0.16908633014999394
