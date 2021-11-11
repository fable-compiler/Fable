module Fable.Tests.DateOnly

open System
open Util.Testing

let tests =
    testList "DateOnly" [
        testCase "DayNumber works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.DayNumber |> equal 1
            let date = DateOnly (2000, 1, 2)
            date.DayNumber |> equal 730120
            let date = DateOnly.MaxValue
            date.DayNumber |> equal 3652058
            let date = DateOnly.MinValue
            date.DayNumber |> equal 0

        testCase "FromDayNumber works" <| fun () ->
            let number = 1
            DateOnly.FromDayNumber number |> equal (DateOnly (1, 1, 2))
            let number = 730120
            DateOnly.FromDayNumber number |> equal (DateOnly (2000, 1, 2))
            let number = 3652058
            DateOnly.FromDayNumber number |> equal DateOnly.MaxValue
            let number = 0
            DateOnly.FromDayNumber number |> equal DateOnly.MinValue

        testCase "Day works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.Day |> equal 2
            let date = DateOnly (2000, 2, 28)
            date.Day |> equal 28
            let date = DateOnly.MaxValue
            date.Day |> equal 31
            let date = DateOnly.MinValue
            date.Day |> equal 1

        testCase "Month works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.Month |> equal 1
            let date = DateOnly (2000, 2, 28)
            date.Month |> equal 2
            let date = DateOnly.MaxValue
            date.Month |> equal 12
            let date = DateOnly.MinValue
            date.Month |> equal 1

        testCase "Year works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.Year |> equal 1
            let date = DateOnly (2000, 2, 28)
            date.Year |> equal 2000
            let date = DateOnly.MaxValue
            date.Year |> equal 9999
            let date = DateOnly.MinValue
            date.Year |> equal 1

        testCase "DayOfWeek works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.DayOfWeek |> equal DayOfWeek.Tuesday
            let date = DateOnly (2000, 2, 28)
            date.DayOfWeek |> equal DayOfWeek.Monday
            let date = DateOnly.MaxValue
            date.DayOfWeek |> equal DayOfWeek.Friday
            let date = DateOnly.MinValue
            date.DayOfWeek |> equal DayOfWeek.Monday

        testCase "DayOfYear works" <| fun () ->
            let date = DateOnly (1, 1, 2)
            date.DayOfYear |> equal 2
            let date = DateOnly (2000, 2, 28)
            date.DayOfYear |> equal 59
            let date = DateOnly.MaxValue
            date.DayOfYear |> equal 365
            let date = DateOnly.MinValue
            date.DayOfYear |> equal 1

        testCase "FromDateTime works" <| fun () ->
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

        testCase "ToDateTime works" <| fun () ->
            let date = DateOnly (2000, 10, 10)
            let time = TimeOnly (20, 30, 40)
            date.ToDateTime (time, DateTimeKind.Utc) |> equal (DateTime (2000, 10, 10, 20, 30, 40, DateTimeKind.Utc))

            let date = DateOnly (2000, 10, 10)
            let time = TimeOnly (20, 30, 40)
            date.ToDateTime (time, DateTimeKind.Local) |> equal (DateTime (2000, 10, 10, 20, 30, 40, DateTimeKind.Local))

            let date = DateOnly (2000, 10, 10)
            let time = TimeOnly (20, 30, 40)
            date.ToDateTime time |> equal (DateTime (2000, 10, 10, 20, 30, 40))

        testCase "AddYears works" <| fun () ->
            let date = DateOnly (2004, 2, 29)
            date.AddYears 3 |> equal (DateOnly (2007, 2, 28))

            let date = DateOnly (1453, 5, 29)
            date.AddYears -200 |> equal (DateOnly (1253, 5, 29))

        testCase "AddMonths works" <| fun () ->
            let date = DateOnly (2001, 1, 31)
            date.AddMonths 1 |> equal (DateOnly (2001, 2, 28))

            let date = DateOnly (2001, 1, 31)
            date.AddMonths -4 |> equal (DateOnly (2000, 9, 30))

        testCase "AddDays works" <| fun () ->
            let date = DateOnly (2000, 12, 31)
            date.AddDays 1 |> equal (DateOnly (2001, 1, 1))

            let date = DateOnly (1453, 5, 29)
            date.AddDays -29 |> equal (DateOnly (1453, 4, 30))
    ]
