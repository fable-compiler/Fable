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

type IInterface =
    abstract member LOL: int

let typeMatchSomeBoxedObject (o: obj) =
    match o with
    | :? int -> 1
    | :? IInterface -> 2
    | _ -> 3

printfn "%A" (typeMatchSomeBoxedObject 1)

printfn
    "%A"
    (typeMatchSomeBoxedObject
        { new IInterface with
            member this.LOL = 1
        })

printfn "%A" (typeMatchSomeBoxedObject "lol")
