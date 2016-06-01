module Fable.Tests.Util

open System
open NUnit.Framework

let equal (expected: 'T) (actual: 'T) =
    Assert.AreEqual(expected, actual)
    
let f2 a b = a + b
let mutable a = 10

module B = 
  let c = a
  a <- a + 5
  let mutable a = 20
  let d = f2 2 2
  let f2 a b = a - b
  
  module D = 
    let d = a
    a <- a + 5
    let e = f2 2 2

// Test members with names conflicting with JS    
let Int32Array = [|1;2|]

module Float64Array =
    let Float64Array = [|3.;4.|]
