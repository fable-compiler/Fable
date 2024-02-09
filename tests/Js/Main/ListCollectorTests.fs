module Fable.Tests.ListCollector

open Util.Testing
open Fable.Tests.Util
open Microsoft.FSharp.Core.CompilerServices

let cutOffLast list =
    let mutable headList = ListCollector<'a>()

    let rec visit list =
        match list with
        | []
        | [ _ ] -> ()
        | head :: tail ->
            headList.Add(head)
            visit tail

    visit list
    headList.Close()

let tests =
    testList "ListCollector" [
        testCase "ListCollector.Add and .Close" <| fun () ->
            let result = cutOffLast [ 1; 2; 3 ]
            result |> equal [ 1; 2 ]
    ]
