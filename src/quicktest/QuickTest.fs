module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing
open System.Globalization


// Report #1

type Items =
    {
        xs: ResizeArray<int>
    }

    static member val empty = { xs = ResizeArray() }

let a = Items.empty
let b = Items.empty

a.xs.Add 1
b.xs.Add 1

printfn $"{a.xs.Count} should match {b.xs.Count}"

// Report #2

type X =
    {
        id: int
    }

    static let _a = 2
    static member a = _a

// returns 0, since the generated constructor takes three params
// and only one is provided when a static let is presnet
let test1 = { id = 1 }
JS.console.log (test1.id = 1)

type Y(id: int) =
    static let _a = 2
    static member a = 2
    member _.id = id

// returns 1, as expected, since the constructor takes a single parameter
let test2 = Y(id = 1)
JS.console.log (test2.id = 1)
