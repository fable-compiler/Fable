module Pacman.Types

open Fable.Import

(**
Creating ghosts
===============
Ghosts are represented by a simple F# class type that contains the image of the ghost,
current X, Y positions and a velocity in both directions. In Pacman, ghosts are mutable
and expose `Move` and `Reset` methods that change their properties.
*)


/// Wrap around the sides of the Maze
let wrap (x,y) (dx,dy) =
  let x =
    if dx = -1 && x = 0 then 30 * 8
    elif dx = 1  && x = 30 *8 then 0
    else x
  x + dx, y + dy

/// Mutable representation of a ghost
type Ghost(image: Browser.HTMLImageElement,x,y,v) =
  let mutable x' = x
  let mutable y' = y
  let mutable v' = v
  member val Image = image
  member val IsReturning = false with get, set
  member __.X = x'
  member __.Y = y'
  member __.V = v'
  /// Move back to initial location
  member ghost.Reset() =
    x' <- x
    y' <- y
  /// Move in the current direction
  member ghost.Move(v) =
    v' <- v
    let dx,dy = v
    let x,y = wrap (x',y') (dx,dy)
    x' <- x
    y' <- y