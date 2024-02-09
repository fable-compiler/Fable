module Fable.Tests.ListCollector

open Util.Testing
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

        testCase "ListCollector.Close works for empty list" <| fun () ->
            let mutable l = ListCollector<_>()
            let result = l.Close()

            result |> equal []

        testCase "ListCollector.AddMany works" <| fun () ->
            let mutable l = ListCollector<_>()
            l.AddMany([ 1; 2; 3 ])

            let result = l.Close()

            result |> equal [ 1; 2; 3 ]

        testCase "ListCollector.AddMany works for empty list" <| fun () ->
            let mutable l = ListCollector<_>()
            l.AddMany([])

            let result = l.Close()

            result |> equal []

        testCase "ListCollector.AddManyAndClose works" <| fun () ->
            let mutable l = ListCollector<_>()
            let result = l.AddManyAndClose([ 1; 2; 3 ])

            result |> equal [ 1; 2; 3 ]
    ]
