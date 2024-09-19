module Fable.Tests.LiteTsInterop

open System
open Util.Testing
open Fable.Core

#if FABLE_COMPILER
open Fable.Core.JsInterop
open Fable.Core.DynamicExtensions
open Fable.Core.Experimental
#endif

type JsOptions =
    abstract foo: string with get, set
    abstract bar: int with get, set
let tests =
  testList "LiteTsInterop" [
    #if FABLE_COMPILER
    testCase "jsOptions works" <| fun _ ->
        let opts = jsOptions<JsOptions>(fun o ->
            o.foo <- "bar"
            o.bar <- 5
        )
        opts.foo |> equal "bar"
        opts.bar |> equal 5

    testCase "jsOptions works when it is directly replaced as a POJO" <| fun () ->
        let opts = jsOptions<JsOptions>(fun o ->
            // This function call avoid the optimization to literal POJO
            let foo () = "foo"
            o.foo <- foo()
            o.bar <- 5
        )
        opts.foo |> equal "foo"
        opts.bar |> equal 5
    #endif
  ]
