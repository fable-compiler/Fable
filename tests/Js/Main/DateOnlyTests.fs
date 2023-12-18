module Fable.Tests.DateOnly

open System
open Util.Testing
open System.Globalization

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

        testCase "ToString works" <| fun () ->
            let date = DateOnly.MaxValue
            date.ToString(CultureInfo.InvariantCulture) |> equal "12/31/9999"
            date.ToString("d", CultureInfo.InvariantCulture) |> equal "12/31/9999"
            date.ToString("o", CultureInfo.InvariantCulture) |> equal "9999-12-31"
            date.ToString("O", CultureInfo.InvariantCulture) |> equal "9999-12-31"

            let date = DateOnly(1, 2, 3)
            date.ToString(CultureInfo.InvariantCulture) |> equal "02/03/0001"
            date.ToString("d", CultureInfo.InvariantCulture) |> equal "02/03/0001"
            date.ToString("o", CultureInfo.InvariantCulture) |> equal "0001-02-03"
            date.ToString("O", CultureInfo.InvariantCulture) |> equal "0001-02-03"

        testCase "Parse parses valid DateOnly" <| fun () ->
            equal DateOnly.MaxValue (DateOnly.Parse("9999-12-31", CultureInfo.InvariantCulture))
            equal DateOnly.MaxValue (DateOnly.Parse("12/31/9999", CultureInfo.InvariantCulture))
            equal DateOnly.MinValue (DateOnly.Parse("1/01/001", CultureInfo.InvariantCulture))
            equal DateOnly.MinValue (DateOnly.Parse("001-1-01", CultureInfo.InvariantCulture))
            equal DateOnly.MinValue (DateOnly.Parse("1/01/0001", CultureInfo.InvariantCulture))
            equal DateOnly.MinValue (DateOnly.Parse("0001.1.01", CultureInfo.InvariantCulture))

            equal (DateOnly (2001, 1, 1)) (DateOnly.Parse("   01/1/01", CultureInfo.InvariantCulture))
            equal (DateOnly (2001, 1, 1)) (DateOnly.Parse("1-01-01   ", CultureInfo.InvariantCulture))
            equal (DateOnly (2005, 1, 1)) (DateOnly.Parse("01/1/5", CultureInfo.InvariantCulture))
            equal (DateOnly (2001, 5, 1)) (DateOnly.Parse("5-01-01", CultureInfo.InvariantCulture))
            equal (DateOnly (2000, 11, 30)) (DateOnly.Parse("2000-11-30", CultureInfo.InvariantCulture))
            equal (DateOnly (2000, 11, 30)) (DateOnly.Parse("11/30/2000", CultureInfo.InvariantCulture))
            equal (DateOnly (2020, 1, 3)) (DateOnly.Parse("01/03/20", CultureInfo.InvariantCulture))
            equal (DateOnly (1999, 1, 3)) (DateOnly.Parse("01,03,99", CultureInfo.InvariantCulture))
#if FABLE_COMPILER
            equal (DateOnly (1930, 1, 3)) (DateOnly.Parse("01 -03- 30", CultureInfo.InvariantCulture))
#else //NET8_0_OR_GREATER
            equal (DateOnly (2030, 1, 3)) (DateOnly.Parse("01 -03- 30", CultureInfo.InvariantCulture))
#endif
            equal (DateOnly (2000, 12, 3)) (DateOnly.Parse("12.03.00", CultureInfo.InvariantCulture))
            equal (DateOnly (2000, 1, 12)) (DateOnly.Parse("01-12-00", CultureInfo.InvariantCulture))

            equal (DateOnly (20, 1, 3)) (DateOnly.Parse("020,01,3", CultureInfo.InvariantCulture))
            equal (DateOnly (20, 1, 3)) (DateOnly.Parse("01 /   3  /   020", CultureInfo.InvariantCulture))
            equal (DateOnly (20, 1, 30)) (DateOnly.Parse("0020-1-30", CultureInfo.InvariantCulture))
            equal (DateOnly (20, 1, 30)) (DateOnly.Parse("1/30/0020", CultureInfo.InvariantCulture))

            equal (DateOnly (DateTime.Now.Year, 10, 30)) (DateOnly.Parse("10/30", CultureInfo.InvariantCulture))
            equal (DateOnly (300, 10, 1)) (DateOnly.Parse("10,300", CultureInfo.InvariantCulture))
            equal (DateOnly (2000, 12, 1)) (DateOnly.Parse("2000-12", CultureInfo.InvariantCulture))

        testCase "TryParse returns false for invalid DateOnly" <| fun () ->
            let isValid, _ = DateOnly.TryParse("4")
            equal false isValid

            let isValid, _ = DateOnly.TryParse("0000-2-5", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

            let isValid, _ = DateOnly.TryParse("2000-2-30", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

            let isValid, _ = DateOnly.TryParse("2000-2?2", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

            let isValid, _ = DateOnly.TryParse("13/01/2000", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

            let isValid, _ = DateOnly.TryParse("01/00/2000", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

            let isValid, _ = DateOnly.TryParse("0/10/2000", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

            let isValid, _ = DateOnly.TryParse("20/2000", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

            let isValid, _ = DateOnly.TryParse("20/200", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

            let isValid, _ = DateOnly.TryParse("200/20", CultureInfo.InvariantCulture, DateTimeStyles.None)
            equal false isValid

        testCase "Comparison works" <| fun () ->
            equal true (DateOnly (2000, 1, 1) < DateOnly (2000, 1, 2))
            equal true (DateOnly (2000, 1, 1) <= DateOnly (2000, 1, 1))
            equal false (DateOnly (2000, 1, 1) > DateOnly (2000, 1, 1))

        testCase "Can be used as map key" <| fun () ->
            let m = [ DateOnly (2000, 1, 2), () ] |> Map.ofList
            equal true (Map.containsKey (DateOnly(2000, 1, 1).AddDays(1)) m)
            equal false (Map.containsKey (DateOnly (1999, 1, 1)) m)
    ]
