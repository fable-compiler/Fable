module Fable.Tests.UtilTests

open System
open Util.Testing
open Fable.Tests.Util

let tests =
  testList "Tests.Util" [
    testList "doesntThrow" [
      testCase "succeeds when no exception" <| fun _ ->
        (fun _ -> ())
        |> doesntThrow

      testCase "fails when exception" <| fun _ ->
        let exThrown =
          try
            (fun _ -> failwith "oh no")
            |> doesntThrow
            false
          with
          | _ -> true
        equal true exThrown
    ]

    testList "throwsAnyError" [
      testCase "fails when no exception" <| fun _ ->
        let exThrown =
          try
            (fun _ -> ())
            |> throwsAnyError
            false
          with
          | _ -> true
        equal true exThrown

      testCase "succeeds when exception" <| fun _ ->
        (fun _ -> failwith "oh no")
        |> throwsAnyError
    ]

    testList "throwsError" [
      testCase "fails when no exception" <| fun _ ->
        let exThrown =
          try
            (fun _ -> ())
            |> throwsError "oh no"
            false
          with
          | _ -> true
        equal true exThrown

      testCase "succeeds when exception with same message" <| fun _ ->
        (fun _ -> failwith "oh no")
        |> throwsError "oh no"

      testCase "fails when different exception" <| fun _ ->
        let exThrown =
          try
            (fun _ -> failwith "oh no")
            |> throwsError "hell yeah!"
            false
          with
          | _ -> true
        equal true exThrown

      testCase "fails when exception not exact match" <| fun _ ->
        let exThrown =
          try
            (fun _ -> failwith "oh no")
            |> throwsError "no"
            false
          with
          | _ -> true
        equal true exThrown
    ]

    testList "throwsErrorContaining" [
      testCase "fails when no exception" <| fun _ ->
        let exThrown =
          try
            (fun _ -> ())
            |> throwsErrorContaining "oh no"
            false
          with
          | _ -> true
        equal true exThrown

      testCase "succeeds when exception with same message" <| fun _ ->
        (fun _ -> failwith "oh no")
        |> throwsErrorContaining "oh no"

      testCase "fails when different exception" <| fun _ ->
        let exThrown =
          try
            (fun _ -> failwith "oh no")
            |> throwsErrorContaining "hell yeah!"
            false
          with
          | _ -> true
        equal true exThrown

      testCase "succeeds when exception with containing match" <| fun _ ->
        (fun _ -> failwith "oh no")
        |> throwsErrorContaining "no"

      testCase "fails when exception part of match" <| fun _ ->
        let exThrown =
          try
            (fun _ -> failwith "oh no")
            |> throwsErrorContaining "oh no, not again"
            false
          with
          | _ -> true
        equal true exThrown
    ]
  ]
