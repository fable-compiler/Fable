(**
 - title: Writing Mocha tests with Fable
 - tagline: Use any JS testing library directly from Fable 

> ATTENTION: This sample is currently broken at the moment. Fable compiles `unit->unit`
lambdas with a `unitArg` argument, which confuses Mocha.

*)

#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-mocha/Fable.Import.Mocha.fs"

open System
open Fable.Core
open Fable.Import
open Fable.Import.Node
open Fable.Import.Mocha.Globals

(** Synchronous tests *)

describe.Invoke("Array", fun () ->
    describe.Invoke("#indexOf()", fun () ->
        it.Invoke("should return -1 when the value is not present", fun () ->
          let jsArray = [|1;2;3|] |> unbox<JS.Array<int>>
          ``assert``.equal(jsArray.indexOf(5), -1)
          ``assert``.equal(jsArray.indexOf(0), -1)
        ) |> ignore
    ) |> ignore
) |> ignore

(** Asynchronous tests *)

type User(name) =
    member x.Name: string = name
    member x.Save(f: JS.Error option->unit) =
        try
            // Saving code here
            f(None)
        with
        | ex -> f(unbox ex |> Some)

describe.Invoke("User", fun () ->
    describe.Invoke("#save()", fun () ->
        it.Invoke("should save without error", fun doneFn ->
            let user = User("Luna")
            user.Save(doneFn)
        ) |> ignore
    ) |> ignore
) |> ignore