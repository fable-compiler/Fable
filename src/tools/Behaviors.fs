module Behaviors

open Api
open Domain

type PointAdder1() =
    interface IAdder<Point> with
        member __.Add(p1, p2) = { x = p1.x + p2.x; y = p1.y + p2.y }

type PointAdder2() =
    interface IAdder<Point> with
        member __.Add(p1, p2) = { x = p1.y + p2.y; y = p1.x + p2.x }

type PointAdder3() =
    interface IAdder<Point> with
        member __.Add(p1, p2) = { x = p1.x * p2.x; y = p1.y * p2.y }
