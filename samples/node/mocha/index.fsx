(**
 - title: Writing Mocha tests with Fable
 - tagline: Use any JS testing library directly from Fable
*)

#r "../../node_modules/fable-core/Fable.Core.dll"
#r "../../node_modules/fable-powerpack/Fable.PowerPack.dll"

open System
open Fable.Core
open Fable.Import
open Fable.PowerPack

[<Emit("describe($0,$1)")>]
let suite(msg: string) (f: unit->unit) = jsNative

[<Emit("it($0,$1)")>]
let test(msg: string) (f: unit->unit) = jsNative

[<Emit("it($0,$1)")>]
let testAsync(msg: string) (f: unit->JS.Promise<'T>) = jsNative

(** Synchronous tests *)

let inline equals expected actual =
    Fable.Core.Testing.Assert.AreEqual(expected, actual)

type R = { a: int; b: int }

suite "Structural equality" <| fun () ->
    test "This test should pass" <| fun () ->
        let r1 = { a=2; b=4 }
        let r2 = { a=2; b=4 }
        equals r1 r2

    test "This test should fail" <| fun () ->
        let r1 = { a=2; b=4 }
        let r2 = { a=2; b=6 }
        equals r1 r2

(** Asynchronous tests *)

type User(name) =
    member x.Name: string = name
    member x.Save(): JS.Promise<unit> =
        // Saving code here
        Promise.sleep(500)

suite "User" <| fun () ->
    testAsync "should save without error" <| fun () ->
        let user = User("Luna")
        user.Save()
