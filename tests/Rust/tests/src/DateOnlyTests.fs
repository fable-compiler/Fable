module Fable.Tests.DateOnlyTests

open System
open System.Globalization
open Util.Testing

[<Fact>]
let ``DayNumber works`` () =
    let date = DateOnly (1, 1, 2)
    date.DayNumber |> equal 1
    let date = DateOnly (2000, 1, 2)
    date.DayNumber |> equal 730120
    let date = DateOnly.MaxValue
    date.DayNumber |> equal 3652058
    let date = DateOnly.MinValue
    date.DayNumber |> equal 0

[<Fact>]
let ``FromDayNumber works`` () =
    let number = 1
    DateOnly.FromDayNumber number |> equal (DateOnly (1, 1, 2))
    let number = 730120
    DateOnly.FromDayNumber number |> equal (DateOnly (2000, 1, 2))
    let number = 3652058
    DateOnly.FromDayNumber number |> equal DateOnly.MaxValue
    let number = 0
    DateOnly.FromDayNumber number |> equal DateOnly.MinValue

[<Fact>]
let ``Day works`` () =
    let date = DateOnly (1, 1, 2)
    date.Day |> equal 2
    let date = DateOnly (2000, 2, 28)
    date.Day |> equal 28
    let date = DateOnly.MaxValue
    date.Day |> equal 31
    let date = DateOnly.MinValue
    date.Day |> equal 1

[<Fact>]
let ``Month works`` () =
    let date = DateOnly (1, 1, 2)
    date.Month |> equal 1
    let date = DateOnly (2000, 2, 28)
    date.Month |> equal 2
    let date = DateOnly.MaxValue
    date.Month |> equal 12
    let date = DateOnly.MinValue
    date.Month |> equal 1

[<Fact>]
let ``Year works`` () =
    let date = DateOnly (1, 1, 2)
    date.Year |> equal 1
    let date = DateOnly (2000, 2, 28)
    date.Year |> equal 2000
    let date = DateOnly.MaxValue
    date.Year |> equal 9999
    let date = DateOnly.MinValue
    date.Year |> equal 1

[<Fact>]
let ``DayOfWeek works`` () =
    let date = DateOnly (1, 1, 2)
    date.DayOfWeek |> equal DayOfWeek.Tuesday
    let date = DateOnly (2000, 2, 28)
    date.DayOfWeek |> equal DayOfWeek.Monday
    let date = DateOnly.MaxValue
    date.DayOfWeek |> equal DayOfWeek.Friday
    let date = DateOnly.MinValue
    date.DayOfWeek |> equal DayOfWeek.Monday

[<Fact>]
let ``DayOfYear works`` () =
    let date = DateOnly (1, 1, 2)
    date.DayOfYear |> equal 2
    let date = DateOnly (2000, 2, 28)
    date.DayOfYear |> equal 59
    let date = DateOnly.MaxValue
    date.DayOfYear |> equal 365
    let date = DateOnly.MinValue
    date.DayOfYear |> equal 1

[<Fact>]
let ``FromDateTime works`` () =
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

[<Fact>]
let ``ToDateTime works`` () =
    let date = DateOnly (2000, 10, 10)
    let time = TimeOnly (20, 30, 40)
    date.ToDateTime (time, DateTimeKind.Utc) |> equal (DateTime (2000, 10, 10, 20, 30, 40, DateTimeKind.Utc))

    let date = DateOnly (2000, 10, 10)
    let time = TimeOnly (20, 30, 40)
    date.ToDateTime (time, DateTimeKind.Local) |> equal (DateTime (2000, 10, 10, 20, 30, 40, DateTimeKind.Local))

    let date = DateOnly (2000, 10, 10)
    let time = TimeOnly (20, 30, 40)
    date.ToDateTime time |> equal (DateTime (2000, 10, 10, 20, 30, 40))

[<Fact>]
let ``AddYears works`` () =
    let date = DateOnly (2004, 2, 29)
    date.AddYears 3 |> equal (DateOnly (2007, 2, 28))

    let date = DateOnly (1453, 5, 29)
    date.AddYears -200 |> equal (DateOnly (1253, 5, 29))

[<Fact>]
let ``AddMonths works`` () =
    let date = DateOnly (2001, 1, 31)
    date.AddMonths 1 |> equal (DateOnly (2001, 2, 28))

    let date = DateOnly (2001, 1, 31)
    date.AddMonths -4 |> equal (DateOnly (2000, 9, 30))

[<Fact>]
let ``AddDays works`` () =
    let date = DateOnly (2000, 12, 31)
    date.AddDays 1 |> equal (DateOnly (2001, 1, 1))

    let date = DateOnly (1453, 5, 29)
    date.AddDays -29 |> equal (DateOnly (1453, 4, 30))

[<Fact>]
let ``ToString works`` () =
    let date = DateOnly.MaxValue
    date.ToString(CultureInfo.InvariantCulture) |> equal "12/31/9999"
    date.ToString("d", CultureInfo.InvariantCulture) |> equal "12/31/9999"
    date.ToString("o", CultureInfo.InvariantCulture) |> equal "9999-12-31"
    date.ToString("O", CultureInfo.InvariantCulture) |> equal "9999-12-31"

    let date = DateOnly.FromDateTime DateTime.Now
    let fill d = if d < 10 then "0" + (string d) else string d
    date.ToString(CultureInfo.InvariantCulture) |> equal $"{fill date.Month}/{fill date.Day}/{date.Year}"
    date.ToString("d", CultureInfo.InvariantCulture) |> equal $"{fill date.Month}/{fill date.Day}/{date.Year}"
    date.ToString("o", CultureInfo.InvariantCulture) |> equal $"{date.Year}-{fill date.Month}-{fill date.Day}"
    date.ToString("O", CultureInfo.InvariantCulture) |> equal $"{date.Year}-{fill date.Month}-{fill date.Day}"

[<Fact>]
let ``Parse parses valid DateOnly`` () =
    let test expected (s: string) =
        DateOnly.Parse(s, CultureInfo.InvariantCulture) |> equal expected

    test DateOnly.MaxValue "9999-12-31"
    test DateOnly.MaxValue "12/31/9999"
    // test DateOnly.MinValue "1/01/001"
    // test DateOnly.MinValue "001-1-01"
    // test DateOnly.MinValue "1/01/0001"
    // test DateOnly.MinValue "0001.1.01"

    // test (DateOnly (2001, 1, 1)) "   01/1/01"
    // test (DateOnly (2001, 1, 1)) "1-01-01   "
    // test (DateOnly (2005, 1, 1)) "01/1/5"
    // test (DateOnly (2001, 5, 1)) "5-01-01"
    // test (DateOnly (2000, 11, 30)) "2000-11-30"
    // test (DateOnly (2000, 11, 30)) "11/30/2000"
    // test (DateOnly (2020, 1, 3)) "01/03/20"
    // test (DateOnly (1999, 1, 3)) "01,03,99"
    // #if FABLE_COMPILER
    //     test (DateOnly (1930, 1, 3)) "01 -03- 30"
    // #else //NET8_0_OR_GREATER
    //     test (DateOnly (2030, 1, 3)) "01 -03- 30"
    // #endif
    // test (DateOnly (2000, 12, 3)) "12.03.00"
    // test (DateOnly (2000, 1, 12)) "01-12-00"

    // test (DateOnly (20, 1, 3)) "020,01,3"
    // test (DateOnly (20, 1, 3)) "01 /   3  /   020"
    // test (DateOnly (20, 1, 30)) "0020-1-30"
    // test (DateOnly (20, 1, 30)) "1/30/0020"

    // test (DateOnly (DateTime.Now.Year, 10, 30)) "10/30"
    // test (DateOnly (300, 10, 1)) "10,300"
    // test (DateOnly (2000, 12, 1)) "2000-12"

[<Fact>]
let ``TryParse returns false for invalid DateOnly`` () =
    let test (s: string) =
        let isValid, _ = DateOnly.TryParse(s, CultureInfo.InvariantCulture, DateTimeStyles.None)
        isValid |> equal false
    test "4"
    // test "0000-2-5"
    // test "2000-2-30"
    // test "2000-2?2"
    // test "13/01/2000"
    // test "01/00/2000"
    // test "0/10/2000"
    // test "20/2000"
    // test "20/200"
    // test "200/20"

[<Fact>]
let ``Comparison works`` () =
    equal true (DateOnly (2000, 1, 1) < DateOnly (2000, 1, 2))
    equal true (DateOnly (2000, 1, 1) <= DateOnly (2000, 1, 1))
    equal false (DateOnly (2000, 1, 1) > DateOnly (2000, 1, 1))

[<Fact>]
let ``Can be used as map key`` () =
    let m = [ DateOnly (2000, 1, 2), () ] |> Map.ofList
    equal true (Map.containsKey (DateOnly(2000, 1, 1).AddDays(1)) m)
    equal false (Map.containsKey (DateOnly (1999, 1, 1)) m)
