module Behaviors

open Domain

module Implementors1 =
    let pointAdder p1 p2 = { x = p1.x + p2.x; y = p1.y + p2.y }
    let squareAdder s1 s2 = { size = s1.size + s2.size }
    let pointSquarer p = { x = p.x * p.x; y = p.y * p.y }
    let squareSquarer s = { size = s.size * s.size }

// Let's do it the other way around!
// Multiply instead of adding, and adding instead of multiplying
module Implementors2 =
    let pointAdder p1 p2 = { x = p1.x * p2.x; y = p1.y * p2.y }
    let squareAdder s1 s2 = { size = s1.size * s2.size }
    let pointSquarer p = { x = p.x + p.x; y = p.y + p.y }
    let squareSquarer s = { size = s.size + s.size }
