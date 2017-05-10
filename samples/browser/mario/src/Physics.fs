module Mario.Physics

type MarioModel =
  { x:float; y:float;
    vx:float; vy:float;
    dir:string }    


// If the Up key is pressed (y > 0) and Mario is on the ground,
// then create Mario with the y velocity 'vy' set to 5.0
let jump (_,y) m =
  if y > 0 && m.y = 0. then { m with vy = 5. } else m

// If Mario is in the air, then his "up" velocity is decreasing
let gravity m =
  if m.y > 0. then { m with vy = m.vy - 0.1 } else m

// Apply physics - move Mario according to the current velocities
let physics m =
  { m with x = m.x + m.vx; y = max 0. (m.y + m.vy) }

// When Left/Right keys are pressed, change 'vx' and direction
let walk (x,_) m =
  let dir = if x < 0 then "left" elif x > 0 then "right" else m.dir
  { m with vx = float x; dir = dir }


let marioStep dir mario =
  mario 
  |> physics 
  |> walk dir 
  |> gravity 
  |> jump dir