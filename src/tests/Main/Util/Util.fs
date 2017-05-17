module Fable.Tests.Util

open System

module Testing =
    #if FABLE_COMPILER
    type Assert = Fable.Core.Testing.Assert
    type TestAttribute = Fable.Core.Testing.TestAttribute
    #else
    type Assert = Xunit.Assert
    type TestAttribute = Xunit.FactAttribute
    #endif
    type TestFixtureAttribute = Fable.Core.Testing.TestFixtureAttribute


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
    #if FABLE_COMPILER
    Assert.AreEqual(expected, actual)
    #else
    Assert.Equal<'T>(expected, actual)
    #endif

let throwsError (expected: string) (f: unit -> 'a): unit =
    let success =
        try
            f () |> ignore
            true
        with e ->
            equal e.Message expected
            false

    // TODO better error messages
    equal false success

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

type private R = { a: int }
let mutable private x = 5

// Check that variables with same name (the compiler generated `matchValue` here)
// at module level don't conflict. See https://github.com/fable-compiler/Fable/issues/718#issuecomment-281533299
match { a = 5 } with
| { a = 3 } -> ()
| _ -> x <- 4

match { a = 2 } with
| { a = 2 } -> x <- 2
| _ -> x <- 4

module Foo =
    let update () = ()

module Bar =
    let rec nestedRecursive i = update (i+2)
    and update i = i + 5

let rec nonNestedRecursive s = update s
and update s = String.replicate 3 s
