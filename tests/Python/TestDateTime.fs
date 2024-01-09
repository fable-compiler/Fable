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
    DateTime(2014, 9, 11, 16, 37, 0).ToString("HH:mm", CultureInfo.InvariantCulture)
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
let ``test DateTime.ToString("d") works`` () =
    DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("d")
    |> equal "9/11/2014"

    DateTime(2014, 9, 1, 16, 37, 11, 345).ToString("d")
    |> equal "9/1/2014"

// Needs to add (upper) and (lower) in the test name because
// the names of the function are lowered making them equal

[<Fact>]
let ``test DateTime.ToString("T") (upper) works`` () =
    #if FABLE_COMPILER
    DateTime(2014, 9, 11, 3, 37, 11, 345).ToString("T")
    #else
    DateTime(2014, 9, 11, 3, 37, 11, 345).ToString("T", CultureInfo.InvariantCulture)
    #endif
    |> equal "03:37:11"

    #if FABLE_COMPILER
    DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("T")
    #else
    DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("T", CultureInfo.InvariantCulture)
    #endif
    |> equal "16:37:11"

[<Fact>]
let ``test DateTime.ToString("t") (lower) works`` () =
    #if FABLE_COMPILER
    DateTime(2014, 9, 11, 3, 37, 11, 345).ToString("t")
    #else
    DateTime(2014, 9, 11, 3, 37, 11, 345).ToString("t", CultureInfo.InvariantCulture)
    #endif
    |> equal "03:37"

    #if FABLE_COMPILER
    DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("t")
    #else
    DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("t", CultureInfo.InvariantCulture)
    #endif
    |> equal "16:37"


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

    d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second |> equal 2130

    let d = DateTime.Parse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
    |> equal 2130

    let d = DateTime.Parse("9/10/2014 1:50:34 AM", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
    |> equal 2118

    let d = DateTime.Parse("9/10/2014 13:50:34", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
    |> equal 2130

    let d = DateTime.Parse("9/10/2014 1:50:34", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
    |> equal 2118

    // Disabled because it is timezone dependent
    // I left it here in case, we need to test it in the future
    // Currently, it is setup for Europe/Paris timezone
    // let d = DateTime.Parse("2016-07-07T01:00:00.000Z", CultureInfo.InvariantCulture)
    // d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
    // |> equal 2033

[<Fact>]
let ``test DateTime.Parse with time-only string works`` () = // See #1045
    // Time-only should use now as the reference date
    let now = DateTime.Now

    let d = DateTime.Parse("13:50:34", CultureInfo.InvariantCulture)
    d.Year |> equal now.Year
    d.Month |> equal now.Month
    d.Day |> equal now.Day
    d.Hour + d.Minute + d.Second |> equal 97

    let d = DateTime.Parse("1:5:34 AM", CultureInfo.InvariantCulture)
    d.Year |> equal now.Year
    d.Month |> equal now.Month
    d.Day |> equal now.Day
    d.Hour + d.Minute + d.Second |> equal 40

    let d = DateTime.Parse("1:5:34 PM", CultureInfo.InvariantCulture)
    d.Year |> equal now.Year
    d.Month |> equal now.Month
    d.Day |> equal now.Day
    d.Hour + d.Minute + d.Second |> equal 52

    let d = DateTime.Parse("0:5:34 PM")
    d.Hour + d.Minute + d.Second |> equal 51

    let d1 = DateTime.Parse("12:5:34 PM")
    d1.Hour + d1.Minute + d1.Second |> equal 51

    let d2 = DateTime.Parse("3:5:34 PM")
    d2.Hour + d2.Minute + d2.Second |> equal 54

    let d3 = DateTime.Parse("15:5:34 PM")
    d3.Hour + d3.Minute + d3.Second |> equal 54

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
    // Summer time (allowed to detect invalid utcoffset for Europe/Paris)
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    d.Hour |> equal 13

    // Winter time
    let d = DateTime(2014, 1, 9, 13, 23, 30, DateTimeKind.Local)
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

// [<Fact>]
// let ``test DateTime.Microsecond works`` () =
//     let d = DateTime(2014, 10, 9, 13, 23, 30, 999).AddMicroseconds(2)
//     d.Microsecond |> equal 2

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

[<Fact>]
let ``test DateTime.TryParse works`` () =
    let (isSuccess, _) = DateTime.TryParse("foo", CultureInfo.InvariantCulture, DateTimeStyles.None)
    isSuccess |> equal false

    let (isSuccess, dateTime) = DateTime.TryParse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture, DateTimeStyles.None)
    isSuccess |> equal true
    dateTime.Year + dateTime.Month + dateTime.Day + dateTime.Hour + dateTime.Minute + dateTime.Second |> equal 2130

    let (isSuccess, _) = DateTime.TryParse("foo", CultureInfo.InvariantCulture)
    isSuccess |> equal false

    let (isSuccess, dateTime) = DateTime.TryParse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture)
    isSuccess |> equal true
    dateTime.Year + dateTime.Month + dateTime.Day + dateTime.Hour + dateTime.Minute + dateTime.Second |> equal 2130

    let (isSuccess, _) = DateTime.TryParse("foo")
    isSuccess |> equal false

    let (isSuccess, dateTime) = DateTime.TryParse("9/10/2014 1:50:34 PM")
    isSuccess |> equal true
    dateTime.Year + dateTime.Month + dateTime.Day + dateTime.Hour + dateTime.Minute + dateTime.Second |> equal 2130


[<Fact>]
let ``test "Parsing doesn't succeed for invalid dates`` () =
    let invalidAmericanDate = "13/1/2020"
    let r, _date = DateTime.TryParse(invalidAmericanDate, CultureInfo.InvariantCulture, DateTimeStyles.None)
    r |> equal false


[<Fact>]
let ``test DateTime.Today works`` () =
    let d = DateTime.Today
    equal 0 d.Hour
    equal 0 d.Minute
    equal 0 d.Second
    equal 0 d.Millisecond

[<Fact>]
let ``test DateTime.Date works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30)
    d.Date.Hour |> equal 0
    d.Date.Minute |> equal 0
    d.Date.Second |> equal 0
    d.Date.Millisecond |> equal 0
    d.Date.Day |> equal 9
    d.Date.Month |> equal 10
    d.Date.Year |> equal 2014

[<Fact>]
let ``test DateTime.DaysInMonth works`` () =
    DateTime.DaysInMonth(2014, 1) |> equal 31
    DateTime.DaysInMonth(2014, 2) |> equal 28
    DateTime.DaysInMonth(2014, 4) |> equal 30
    DateTime.DaysInMonth(2016, 2) |> equal 29

[<Fact>]
let ``test DateTime.Now works`` () =
    let d = DateTime.Now
    d > DateTime.MinValue |> equal true

[<Fact>]
let ``test DateTime.UtcNow works`` () =
    let d = DateTime.UtcNow
    d > DateTime.MinValue |> equal true


[<Fact>]
let ``test DateTime.AddYears works`` () =
    let test v expected =
        let dt = DateTime(2016,2,29,0,0,0,DateTimeKind.Utc).AddYears(v)
        equal expected (dt.Month + dt.Day)
    test 100 31
    test 1 30
    test -1 30
    test -100 31
    test 0 31

[<Fact>]
let ``test DateTime.AddMonths works`` () =
    let test v expected =
        let dt = DateTime(2016,1,31,0,0,0,DateTimeKind.Utc).AddMonths(v)
        dt.Year + dt.Month + dt.Day
        |> equal expected
    test 100 2060
    test 20 2056
    test 6 2054
    test 5 2052
    test 1 2047
    test 0 2048
    test -1 2058
    test -5 2054
    test -20 2050
    test -100 2046

[<Fact>]
let ``test DateTime.AddDays works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddDays(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 30585600.0
    test -100. 13305600.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTime.AddHours works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddHours(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 22305600.0
    test -100. 21585600.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTime.AddMinutes works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddMinutes(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21951600.0
    test -100. 21939600.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTime.AddSeconds works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddSeconds(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21945700.0
    test -100. 21945500.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTime.AddMilliseconds works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddMilliseconds(v)
        thatYearMilliseconds dt
        |> equal expected
    test 100. 2.19456001e+10
    test -100. 2.19455999e+10
    test 0. 2.19456e+10

[<Fact>]
let ``test DateTime Addition works`` () =
    let test ms expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc)
        let ts = TimeSpan.FromMilliseconds(ms)
        let res1 = dt.Add(ts) |> thatYearSeconds
        let res2 = (dt + ts) |> thatYearSeconds
        equal true (res1 = res2)
        equal expected res1
    test 1000. 21945601.0
    test -1000. 21945599.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTime constructors works`` () =
    let d1 = DateTime(2014, 10, 9)
    let d2 = DateTime(2014, 10, 9, 13, 23, 30)
    let d3 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    let d4 = DateTime(2014, 10, 9, 13, 23, 30, 500)
    let d5 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
    d1.Day + d2.Second + d3.Second + d4.Millisecond + d5.Millisecond
    |> equal 1069

[<Fact>]
let ``test DateTime constructor from Ticks works`` () =
    let d = DateTime(624059424000000000L, DateTimeKind.Utc)
    equal 1978 d.Year
    equal 7 d.Month
    equal 27 d.Day
    equal 0 d.Hour
    equal 0 d.Minute

    let d = DateTime(624059424000000000L, DateTimeKind.Local)
    equal 1978 d.Year
    equal 7 d.Month
    equal 27 d.Day
    equal 0 d.Hour
    equal 0 d.Minute

    let d = DateTime(624059424000000000L)
    equal 1978 d.Year
    equal 7 d.Month
    equal 27 d.Day
    equal 0 d.Hour
    equal 0 d.Minute

[<Fact>]
let ``test DateTime.Ticks does not care about kind`` () =
    let d1 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Local)
    let d2 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
    let d3 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Unspecified)
    equal d1.Ticks d2.Ticks
    equal d1.Ticks d3.Ticks
    equal d2.Ticks d3.Ticks

    let t = DateTime.UtcNow.Ticks
    let d1 = DateTime(t, DateTimeKind.Local)
    let d2 = DateTime(t, DateTimeKind.Utc)
    let d3 = DateTime(t, DateTimeKind.Unspecified)
    equal d1.Ticks d2.Ticks
    equal d1.Ticks d3.Ticks
    equal d2.Ticks d3.Ticks

[<Fact>]
let ``test DateTime.ToLocalTime works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    let d' = d.ToLocalTime()
    d.Kind <> d'.Kind
    |> equal true

[<Fact>]
let ``test DateTime.ToUniversalTime works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    d.ToUniversalTime().Kind <> d.Kind
    |> equal true

[<Fact>]
let ``test DateTime.SpecifyKind works`` () = // See #1844
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d2 = DateTime.SpecifyKind(d, DateTimeKind.Utc)
    d2.Kind |> equal DateTimeKind.Utc
    d.Ticks = d2.Ticks |> equal true
    let d3 = d.ToUniversalTime()
    d.Ticks = d3.Ticks |> equal false

// [<Fact>]
// let ``test DateTime <-> Ticks isomorphism`` () =
//     let checkIsomorphism (d: DateTime) =
//         try
//             let ticks = d.Ticks
//             let kind = d.Kind
//             let fromTicks = DateTime ticks
//             let fromTicksWithKind = DateTime (ticks, kind)

//             equal d fromTicks
//             equal ticks fromTicks.Ticks
//             equal d fromTicksWithKind
//             equal ticks fromTicksWithKind.Ticks
//             equal kind fromTicksWithKind.Kind
//         with e ->
//             failwithf "%A: %O" d e

//         try
//             equal d.Ticks (DateTime d.Ticks).Ticks
//         with e ->
//             failwithf "%s%O" "replacement bug. " e

//     checkIsomorphism DateTime.MinValue
//     checkIsomorphism DateTime.MaxValue
//     checkIsomorphism DateTime.Now
//     checkIsomorphism DateTime.UtcNow
//     checkIsomorphism <| DateTime(2014, 10, 9)
//     checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30)
//     checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//     checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500)
//     checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
