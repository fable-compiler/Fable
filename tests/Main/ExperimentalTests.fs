module Fable.Tests.Experimental

open Util.Testing
open Fable.Core.Experimental
open Api
open Domain
open Behaviors

// Points
let p1 = { x = 5; y = 12 }
let p2 = { x = 23; y = -4 }

// Squares
let s1 = { size = 20 }
let s2 = { size = 5 }

module Implicits1 =
    let [<Implicit>] pointAdder     = Implementors1.pointAdder
    let [<Implicit>] squareAdder    = Implementors1.squareAdder
    let [<Implicit>] pointSquarer   = Implementors1.pointSquarer
    let [<Implicit>] squareSquarer  = Implementors1.squareSquarer

    let addAndSquarePoints (p1: Point) p2 = Helper.AddAndSquare(p1, p2)
    let addAndSquareSquares (s1: Square) s2 = Helper.AddAndSquare(s1, s2)

module Implicits2 =
    let [<Implicit>] pointAdder     = Implementors2.pointAdder
    let [<Implicit>] squareAdder    = Implementors2.squareAdder
    let [<Implicit>] pointSquarer   = Implementors2.pointSquarer
    let [<Implicit>] squareSquarer  = Implementors2.squareSquarer

    let addAndSquarePoints (p1: Point) p2 = Helper.AddAndSquare(p1, p2)
    let addAndSquareSquares (s1: Square) s2 = Helper.AddAndSquare(s1, s2)

let tests =
  testList "Experimentals" [
    testCase "Implicits work" <| fun () ->
        Implicits1.addAndSquarePoints p1 p2 |> equal { x = 784; y = 64 }
        Implicits1.addAndSquareSquares s1 s2 |> equal { size = 625 }

        Implicits2.addAndSquarePoints p1 p2 |> equal { x = 230; y = -96 }
        Implicits2.addAndSquareSquares s1 s2 |> equal { size = 200 }
  ]
