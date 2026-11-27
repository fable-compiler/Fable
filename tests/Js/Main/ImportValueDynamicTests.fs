module Fable.Tests.ImportValueDynamic

open Util.Testing
open Fable.Tests.Util2.DynamicImports

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

[<Import("URL", "node:url")>]
type JsUrl(url: string) =
    member _.href: string = jsNative
#else
let importValueDynamic (x: 'T) = x

type Microsoft.FSharp.Control.Async with
    static member AwaitPromise(x: 'T) = async.Return x
#endif

type Logger() =
    let log = ResizeArray<string>()
    member _.Add(s: string) = log.Add s
    member _.Say(s: string, v: 'T) =
        log.Add s
        v
    member _.Text = System.String.Join(",", log)

let loadWithLocalNamedMDollar (``m$``: int) =
    importValueDynamic (curried ``m$``) |> Async.AwaitPromise

let loadWithLocalNamedM (m: int) =
    importValueDynamic (curried m) |> Async.AwaitPromise

let tests =
  testList "ImportValueDynamic" [
    testCaseAsync "importValueDynamic works with values and single argument functions" <| fun () -> async {
        let! value = importValueDynamic Fable.Tests.Util2.DynamicImports.value |> Async.AwaitPromise
        value |> equal 42
        let! single = importValueDynamic Fable.Tests.Util2.DynamicImports.single |> Async.AwaitPromise
        single 1 |> equal 2
        let! takesUnit = importValueDynamic Fable.Tests.Util2.DynamicImports.takesUnit |> Async.AwaitPromise
        takesUnit () |> equal "unit"
    }

    testCaseAsync "importValueDynamic works with tupled functions" <| fun () -> async { // #3919
        let! tupled = importValueDynamic Fable.Tests.Util2.DynamicImports.tupled |> Async.AwaitPromise
        tupled (1, "a") |> equal "1a"
    }

    testCaseAsync "importValueDynamic works with curried functions" <| fun () -> async { // #3919
        let! curried = importValueDynamic Fable.Tests.Util2.DynamicImports.curried |> Async.AwaitPromise
        curried 1 "a" |> equal "1a"
        let partial = curried 2
        partial "b" |> equal "2b"
        let! tupledThenCurried = importValueDynamic Fable.Tests.Util2.DynamicImports.tupledThenCurried |> Async.AwaitPromise
        tupledThenCurried (1, "a") 2 |> equal "1a2"
    }

    testCaseAsync "importValueDynamic keeps the arguments of the imported function" <| fun () -> async { // #3919
        let! namedLikeModule = importValueDynamic Fable.Tests.Util2.DynamicImports.namedLikeModule |> Async.AwaitPromise
        namedLikeModule 3 4 |> equal 12
        let! takesFunction = importValueDynamic Fable.Tests.Util2.DynamicImports.takesFunction |> Async.AwaitPromise
        takesFunction (fun a b -> a + b) 2 |> equal 4
    }

    testCaseAsync "importValueDynamic: single argument taking a curried function" <| fun () -> async {
        let! f = importValueDynamic takesFnSingle |> Async.AwaitPromise
        f (fun a b -> a * 10 + b) |> equal 34
    }

    testCaseAsync "importValueDynamic: single argument taking a 3-argument function" <| fun () -> async {
        let! f = importValueDynamic takesFn3 |> Async.AwaitPromise
        f (fun a b c -> a * 100 + b * 10 + c) |> equal 123
    }

    testCaseAsync "importValueDynamic: single argument taking an optional function" <| fun () -> async {
        let! f = importValueDynamic takesFnOption |> Async.AwaitPromise
        f (Some(fun a b -> a + b)) |> equal 3
        f None |> equal -1
    }

    testCaseAsync "importValueDynamic: single argument instance method keeps its receiver" <| fun () -> async {
        let! f = importValueDynamic dynamicInstance.Inst |> Async.AwaitPromise
        f 1 |> equal 2
    }

    testCaseAsync "importValueDynamic: single argument ParamArray call" <| fun () -> async {
        let! f = importValueDynamic (fun (xs: int[]) -> DynamicStatic.Params xs) |> Async.AwaitPromise
        f [| 1; 2; 3 |] |> equal 3
    }

#if FABLE_COMPILER
    testCaseAsync "importValueDynamic: single argument imported constructor uses new" <| fun () -> async {
        let! f = importValueDynamic (fun (url: string) -> JsUrl url) |> Async.AwaitPromise
        (f "http://a.b/").href |> equal "http://a.b/"
    }
#endif

    testCaseAsync "importValueDynamic: partial application captures a mutable local" <| fun () -> async {
        let mutable n = 1
        let p = importValueDynamic (curried n)
        n <- 2
        let! f = p |> Async.AwaitPromise
        f "a" |> equal "1a"
    }

    testCaseAsync "importValueDynamic: partial application captures a mutable from another file" <| fun () -> async {
        partialModuleState <- 1
        let p = importValueDynamic (curried partialModuleState)
        partialModuleState <- 2
        let! f = p |> Async.AwaitPromise
        f "a" |> equal "1a"
    }

    testCaseAsync "importValueDynamic: full application captures a mutable local" <| fun () -> async {
        let mutable n = 1
        let p = importValueDynamic (single n)
        n <- 2
        let! r = p |> Async.AwaitPromise
        r |> equal 2
    }

    testCaseAsync "importValueDynamic: full application captures a mutable from another file" <| fun () -> async {
        fullModuleState <- 1
        let p = importValueDynamic (single fullModuleState)
        fullModuleState <- 2
        let! r = p |> Async.AwaitPromise
        r |> equal 2
    }

    testCaseAsync "importValueDynamic: full application evaluates arguments before the import" <| fun () -> async {
        let log = Logger()
        log.Add "before"
        let p = importValueDynamic (single (log.Say("arg", 1)))
        log.Add "after"
        let! r = p |> Async.AwaitPromise
        r |> equal 2
        log.Text |> equal "before,arg,after"
    }

    testCaseAsync "importValueDynamic: arguments are evaluated in F# order" <| fun () -> async {
        let log = Logger()
        let p = importValueDynamic (curried3 (log.Say("a", 1)) (log.Say("b", "x")))
        log.Add "after"
        let! f = p |> Async.AwaitPromise
        f true |> equal "1xt"
        log.Text |> equal "a,b,after"
    }

    testCaseAsync "importValueDynamic: lambda bodies stay evaluated on each call" <| fun () -> async {
        let log = Logger()
        perCallModuleState <- 1
        let! f = importValueDynamic (fun (s: string) -> curried (log.Say("call", perCallModuleState)) s) |> Async.AwaitPromise
        perCallModuleState <- 2
        f "a" |> equal "2a"
        f "b" |> equal "2b"
        log.Text |> equal "call,call"
    }

    testCaseAsync "importValueDynamic: generic curried function" <| fun () -> async {
        let! f = importValueDynamic genericPair<int, string> |> Async.AwaitPromise
        f 1 "x" |> equal (1, "x")
    }

    testCaseAsync "importValueDynamic: generic curried function with one type argument" <| fun () -> async {
        let! f = importValueDynamic genericToString<int> |> Async.AwaitPromise
        f 1 2 |> equal "12"
    }

    testCaseAsync "importValueDynamic: generic static member" <| fun () -> async {
        let! f = importValueDynamic DynamicStatic.Gen<int> |> Async.AwaitPromise
        f (1, 2) |> equal [ 1; 2 ]
    }

    testCaseAsync "importValueDynamic: exported parameter named m$" <| fun () -> async {
        let! f = importValueDynamic weird |> Async.AwaitPromise
        f 3 4 |> equal 12
    }

    testCaseAsync "importValueDynamic: local named m$ passed to a partial application" <| fun () -> async {
        let! f = loadWithLocalNamedMDollar 1
        f "a" |> equal "1a"
    }

    testCaseAsync "importValueDynamic: local named m passed to a partial application" <| fun () -> async {
        let! f = loadWithLocalNamedM 1
        f "a" |> equal "1a"
    }

    testCaseAsync "importValueDynamic: call as partial argument is evaluated once" <| fun () -> async {
        let log = Logger()
        let! f = importValueDynamic (curried (log.Say("arg", 1))) |> Async.AwaitPromise
        f "a" |> equal "1a"
        f "b" |> equal "1b"
        log.Text |> equal "arg"
    }

    testCaseAsync "importValueDynamic: array item as partial argument" <| fun () -> async {
        let a = [| 1 |]
        let p = importValueDynamic (curried a.[0])
        a.[0] <- 2
        let! f = p |> Async.AwaitPromise
        f "a" |> equal "1a"
    }

    testCaseAsync "importValueDynamic: mutable field as partial argument" <| fun () -> async {
        let m = { X = 1 }
        let p = importValueDynamic (curried m.X)
        m.X <- 2
        let! f = p |> Async.AwaitPromise
        f "a" |> equal "1a"
    }

    testCaseAsync "importValueDynamic: ref cell as partial argument" <| fun () -> async {
        let r = ref 1
        let p = importValueDynamic (curried r.Value)
        r.Value <- 2
        let! f = p |> Async.AwaitPromise
        f "a" |> equal "1a"
    }

    testCaseAsync "importValueDynamic: BCL call as partial argument" <| fun () -> async {
        let! f = importValueDynamic (curried (String.length "abc")) |> Async.AwaitPromise
        f "a" |> equal "3a"
    }

    testCaseAsync "importValueDynamic: record as partial argument" <| fun () -> async {
        let! f = importValueDynamic (curriedRec { A = 1; B = 2 }) |> Async.AwaitPromise
        f "s" |> equal "12s"
    }

    testCaseAsync "importValueDynamic: array as partial argument" <| fun () -> async {
        let! f = importValueDynamic (curriedArr [| 1; 2 |]) |> Async.AwaitPromise
        f "s" |> equal "2s"
    }

    testCaseAsync "importValueDynamic: forward pipe into a partial application" <| fun () -> async {
        let! f = importValueDynamic (1 |> curried) |> Async.AwaitPromise
        f "s" |> equal "1s"
    }

    testCaseAsync "importValueDynamic: forward pipe evaluates the argument once" <| fun () -> async {
        let log = Logger()
        let! f = importValueDynamic (log.Say("p", 1) |> curried) |> Async.AwaitPromise
        f "a" |> equal "1a"
        f "b" |> equal "1b"
        log.Text |> equal "p"
    }

    testCaseAsync "importValueDynamic: backward pipe into a partial application" <| fun () -> async {
        let! f = importValueDynamic (curried <| 1) |> Async.AwaitPromise
        f "s" |> equal "1s"
    }

    testCaseAsync "importValueDynamic: instance method of a new object" <| fun () -> async {
        let! f = importValueDynamic (DynamicInstance 1).Inst |> Async.AwaitPromise
        f 1 |> equal 2
    }

    testCaseAsync "importValueDynamic: tupled instance method keeps its receiver" <| fun () -> async {
        let! f = importValueDynamic dynamicInstance.Inst2 |> Async.AwaitPromise
        f (1, 2) |> equal 4
    }

    testCaseAsync "importValueDynamic: partial application of a function value" <| fun () -> async {
        let! f = importValueDynamic (fnValue 1) |> Async.AwaitPromise
        f 2 |> equal 3
    }

    testCaseAsync "importValueDynamic: full application returning a function" <| fun () -> async {
        let! r = importValueDynamic (returnsFn 1 2) |> Async.AwaitPromise
        r |> equal 3
    }
  ]
