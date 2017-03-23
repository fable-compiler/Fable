module Fable.Tests.Util

open System

module Testing =
    #if FABLE_COMPILER
    type Assert = Fable.Core.Testing.Assert
    type TestAttribute = Fable.Core.Testing.TestAttribute
    type TestFixtureAttribute = Fable.Core.Testing.TestFixtureAttribute
    #else
    type Assert = NUnit.Framework.Assert
    type TestAttribute = NUnit.Framework.TestAttribute
    type TestFixtureAttribute = NUnit.Framework.TestFixtureAttribute
    #endif

open Testing

#if FABLE_COMPILER
let foo: string = Fable.Core.JsInterop.importMember "../js/foo.js"

[<Fable.Core.Import("foo", "../js/foo.js")>]
let foo2: string = failwith "JS only"

let apply (f:Func<int,int,int>) (x:int) (y:int) = Fable.Core.JsInterop.importMember "../js/foo.js"
#else
let foo = "foo"
let foo2 = "foo"
let apply (f:Func<int,int,int>) (x:int) (y:int) = f.Invoke(x, y)
#endif

let equal (expected: 'T) (actual: 'T) =
    Assert.AreEqual(expected, actual)

let rec sumFirstSeq (zs: seq<float>) (n: int): float =
   match n with
   | 0 -> 0.
   | 1 -> Seq.head zs
   | _ -> (Seq.head zs) + sumFirstSeq (Seq.skip 1 zs) (n-1)

let rec sumFirstList (zs: float list) (n: int): float =
   match n with
   | 0 -> 0.
   | 1 -> zs.Head
   | _ -> zs.Head + sumFirstList zs.Tail (n-1)

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
