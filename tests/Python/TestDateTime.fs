module Fable.Tests.DateTime

open System
open System.Globalization
open Util.Testing
open Fable.Tests

let toSigFigs nSigFigs x =
    let absX = abs x
    let digitsToStartOfNumber = floor(log10 absX) + 1. // x > 0 => +ve | x < 0 => -ve
    let digitsToAdjustNumberBy = int digitsToStartOfNumber - nSigFigs
    let scale = pown 10. digitsToAdjustNumberBy
    round(x / scale) * scale

let thatYearSeconds (dt: DateTime) =
    (dt - DateTime(dt.Year, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds

let thatYearMilliseconds (dt: DateTime) =
    (dt - DateTime(dt.Year, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalMilliseconds

[<Fact>]
let ``test DateTime.ToString with custom format works`` () =
    DateTime(2014, 9, 11, 16, 37, 0).ToString("HH:mm", System.Globalization.CultureInfo.InvariantCulture)
    |> equal "16:37"

[<Fact>]
let ``test DateTime.ToString without separator works`` () =
    DateTime(2017, 9, 5).ToString("yyyyMM")
    |> equal "201709"

[<Fact>]
let ``test DateTime.ToString with milliseconds`` () =
    DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("ss.fff")
    |> equal "11.345"

[<Fact>]
let ``test DateTime.ToString with Round-trip format works for Utc`` () =
    let str = DateTime(2014, 9, 11, 16, 37, 2, DateTimeKind.Utc).ToString("O")
    // FIXME: missing regex module
    // System.Text.RegularExpressions.Regex.Replace(str, "0{3,}", "000")
    // Hardcode the replace string so we can test that "O" format is supported
    str.Replace("0000000Z", "000000Z")
    |> equal "2014-09-11T16:37:02.000000Z"

[<Fact>]
let ``test DateTime from Year 1 to 99 works`` () =
    let date = DateTime(1, 1, 2)
    date.Year |> equal 1
    let date = DateTime(99, 1, 2)
    date.Year |> equal 99

[<Fact>]
let ``test DateTime UTC from Year 1 to 99 works`` () =
    let date = DateTime(1, 1, 2, 0, 0, 0, DateTimeKind.Utc)
    date.Year |> equal 1
    let date = DateTime(99, 1, 2, 0, 0, 0, DateTimeKind.Utc)
    date.Year |> equal 99

[<Fact>]
let ``test DateTime.MaxValue works`` () =
    let d1 = DateTime.Now
    let d2 = DateTime.MaxValue
    d1 < d2 |> equal true

[<Fact>]
let ``test DateTime.MinValue works`` () =
    let d1 = DateTime.Now
    let d2 = DateTime.MinValue
    d1 < d2 |> equal false

[<Fact>]
let ``test DateTime.MinValue works in pattern match`` () =
    let d1 = Some DateTime.Now
    match d1 with
    | Some date when date <> DateTime.MinValue -> ()
    | _ -> failwith "expected pattern match above"

[<Fact>]
let ``test DateTime Subtraction with TimeSpan works`` () =
    let test ms expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc)
        let ts = TimeSpan.FromMilliseconds(ms)
        let res1 = dt.Subtract(ts) |> thatYearSeconds
        let res2 = (dt - ts) |> thatYearSeconds
        equal true (res1 = res2)
        equal expected res1
    test 1000. 21945599.0
    test -1000. 21945601.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTime Subtraction with DateTime works`` () =
    let test ms expected =
        let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
        let dt2 = dt1.AddMilliseconds(ms)
        let res1 = dt1.Subtract(dt2).TotalSeconds
        let res2 = (dt1 - dt2).TotalSeconds
        equal true (res1 = res2)
        equal expected res1
    test 1000. -1.0
    test -1000. 1.0
    test 0. 0.0

[<Fact>]
let ``test DateTime.Parse works`` () =

    let d =
        DateTime.Parse(
            "2014-09-10T13:50:34.0000000",
            CultureInfo.InvariantCulture
        )

    d.Year + d.Month + d.Day + d.Hour + d.Minute |> equal 2096

    let d = DateTime.Parse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute
    |> equal 2096

    let d = DateTime.Parse("9/10/2014 1:50:34 AM", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute
    |> equal 2084

    let d = DateTime.Parse("9/10/2014 13:50:34", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute
    |> equal 2096

    let d = DateTime.Parse("9/10/2014 1:50:34", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute
    |> equal 2084

    // Disabled because it is timezone dependent
    // I left it here in case, we need to test it in the future
    // Currently, it is setup for Europe/Paris timezone
    // let d = DateTime.Parse("2016-07-07T01:00:00.000Z", CultureInfo.InvariantCulture)
    // d.Year + d.Month + d.Day + d.Hour + d.Minute
    // |> equal 2033

[<Fact>]
let ``test DateTime.Parse with time-only string works`` () = // See #1045
    let d = DateTime.Parse("13:50:34", CultureInfo.InvariantCulture)
    d.Hour + d.Minute + d.Second |> equal 97

    let d = DateTime.Parse("1:5:34 AM", CultureInfo.InvariantCulture)
    d.Hour + d.Minute + d.Second |> equal 40

    let d = DateTime.Parse("1:5:34 PM", CultureInfo.InvariantCulture)
    d.Hour + d.Minute + d.Second |> equal 52

    let d = DateTime.Parse("15:5:34 PM", CultureInfo.InvariantCulture)
    d.Hour + d.Minute + d.Second |> equal 54

// [<Fact>]
// let ``test DateTime.Ticks works`` () =
//     let d = DateTime(2014, 10, 9, 13, 23, 30, 999, DateTimeKind.Utc)
//     d.Ticks |> equal 635484578109990000L
//     let d = DateTime(2014, 10, 9, 13, 23, 30, 999, DateTimeKind.Local)
//     d.Ticks |> equal 635484578109990000L
//     let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
//     d.Ticks |> equal 635484578109990000L

[<Fact>]
let ``test DateTime.Year works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    d.Year + d'.Year
    |> equal 4028

[<Fact>]
let ``test DateTime.Day works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    d.Day + d'.Day |> equal 18

[<Fact>]
let ``test DateTime.Month works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    d.Month + d'.Month
    |> equal 20

[<Fact>]
let ``test DateTime.Hour works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    d.Hour |> equal 13

[<Fact>]
let ``test DateTime.Minute works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    d.Minute + d'.Minute
    |> equal 46

[<Fact>]
let ``test DateTime.Second works`` () =
    let d = DateTime(2014,9,12,0,0,30)
    let d' = DateTime(2014,9,12,0,0,59)
    d.Second + d'.Second
    |> equal 89

[<Fact>]
let ``test DateTime.Millisecond works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
    d.Millisecond |> equal 999

[<Fact>]
let ``DateTime.DayOfWeek works`` () =
    DateTime(2014, 10, 5).DayOfWeek |> equal DayOfWeek.Sunday
    DateTime(2014, 10, 6).DayOfWeek |> equal DayOfWeek.Monday
    DateTime(2014, 10, 7).DayOfWeek |> equal DayOfWeek.Tuesday
    DateTime(2014, 10, 8).DayOfWeek |> equal DayOfWeek.Wednesday
    DateTime(2014, 10, 9).DayOfWeek |> equal DayOfWeek.Thursday
    DateTime(2014, 10, 10).DayOfWeek |> equal DayOfWeek.Friday
    DateTime(2014, 10, 11).DayOfWeek |> equal DayOfWeek.Saturday

[<Fact>]
let ``test DateTime.DayOfYear works`` () =
    // Standard year
    DateTime(2014, 10, 9).DayOfYear |> equal 282
    // Leap year
    DateTime(2020, 10, 9).DayOfYear |> equal 283

// [<Fact>]
// let ``test DateTime.ToLocalTime works`` () =
//     let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//     let d' = d.ToLocalTime()
//     d.Kind <> d'.Kind
//     |> equal true
