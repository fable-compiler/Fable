module Fable.Tests.DateOnly

open System
open Util.Testing

let tests =
    testList "DateOnly" [
        testCase "DateOnly DayNumber works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.DayNumber |> equal 1
            let date = DateOnly (2000, 1, 2)
            date.DayNumber |> equal 730120
            let date = DateOnly.MaxValue
            date.DayNumber |> equal 3652058
            let date = DateOnly.MinValue
            date.DayNumber |> equal 0

        testCase "DateOnly FromDayNumber works" <| fun () ->
            let number = 1
            DateOnly.FromDayNumber number |> equal (DateOnly (1, 1, 2))
            let number = 730120
            DateOnly.FromDayNumber number |> equal (DateOnly (2000, 1, 2))
            let number = 3652058
            DateOnly.FromDayNumber number |> equal DateOnly.MaxValue
            let number = 0
            DateOnly.FromDayNumber number |> equal DateOnly.MinValue

        testCase "DateOnly Day works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.Day |> equal 2
            let date = DateOnly (2000, 2, 28)
            date.Day |> equal 28
            let date = DateOnly.MaxValue
            date.Day |> equal 31
            let date = DateOnly.MinValue
            date.Day |> equal 1

        testCase "DateOnly Month works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.Month |> equal 1
            let date = DateOnly (2000, 2, 28)
            date.Month |> equal 2
            let date = DateOnly.MaxValue
            date.Month |> equal 12
            let date = DateOnly.MinValue
            date.Month |> equal 1

        testCase "DateOnly Year works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.Year |> equal 1
            let date = DateOnly (2000, 2, 28)
            date.Year |> equal 2000
            let date = DateOnly.MaxValue
            date.Year |> equal 9999
            let date = DateOnly.MinValue
            date.Year |> equal 1

        testCase "DateOnly DayOfWeek works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.DayOfWeek |> equal DayOfWeek.Tuesday
            let date = DateOnly (2000, 2, 28)
            date.DayOfWeek |> equal DayOfWeek.Monday
            let date = DateOnly.MaxValue
            date.DayOfWeek |> equal DayOfWeek.Friday
            let date = DateOnly.MinValue
            date.DayOfWeek |> equal DayOfWeek.Monday

        testCase "DateOnly DayOfYear works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.DayOfYear |> equal 2
            let date = DateOnly (2000, 2, 28)
            date.DayOfYear |> equal 59
            let date = DateOnly.MaxValue
            date.DayOfYear |> equal 365
            let date = DateOnly.MinValue
            date.DayOfYear |> equal 1

        testCase "DateOnly FromDateTime works" <| fun () ->
            let dateTime = DateTime (2000, 2, 27, 23, 30, 0, DateTimeKind.Local)
            DateOnly.FromDateTime dateTime |> equal (DateOnly (2000, 2, 27))
            let dateTime = DateTime (2000, 2, 27, 23, 30, 0, DateTimeKind.Utc)
            DateOnly.FromDateTime dateTime |> equal (DateOnly (2000, 2, 27))
            let dateTime = DateTime (2000, 2, 28, 0, 30, 0, DateTimeKind.Local)
            DateOnly.FromDateTime dateTime |> equal (DateOnly (2000, 2, 28))
            let dateTime = DateTime (2000, 2, 28, 0, 30, 0, DateTimeKind.Utc)
            DateOnly.FromDateTime dateTime |> equal (DateOnly (2000, 2, 28))
            let dateTime = DateTime.Now
            DateOnly.FromDateTime dateTime |> equal (DateOnly (DateTime.Now.Year, DateTime.Now.Month, DateTime.Now.Day))
            //let dateTime = DateTime.MaxValue
            //DateOnly.FromDateTime dateTime |> equal DateOnly.MaxValue
            //let dateTime = DateTime.MinValue
            //DateOnly.FromDateTime dateTime |> equal DateOnly.MinValue
    ]
