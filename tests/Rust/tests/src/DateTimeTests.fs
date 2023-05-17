module Fable.Tests.DateTimeTests

open System
open Util.Testing
open Fable.Tests
open System.Globalization

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

// [<Fact>]
// let ``DateTime.ToString with custom format works`` () =
//     DateTime(2014, 9, 11, 16, 37, 0).ToString("HH:mm", CultureInfo.InvariantCulture)
//     |> equal "16:37"

[<Fact>]
let ``DateTime.ToString without separator works`` () = // See #1131
    DateTime(2017, 9, 5).ToString("yyyyMM")
    |> equal "201709"

[<Fact>]
let ``DateTime.ToString with milliseconds`` () = // See #1726
    DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("ss.fff")
    |> equal "11.345"

// [<Fact>]
// let ``DateTime.ToString with Round-trip format works for Utc`` () =
//     let str = DateTime(2014, 9, 11, 16, 37, 2, DateTimeKind.Utc).ToString("O")
//     System.Text.RegularExpressions.Regex.Replace(str, "0{3,}", "000")
//     |> equal "2014-09-11T16:37:02.000Z"

// TODO
// Next test is disabled because it's depends on the time zone of the machine
//A fix could be to use a regex or detect the time zone
//
// [<Fact>]
// let ``DateTime.ToString with Round-trip format works for local`` () =
//     DateTime(2014, 9, 11, 16, 37, 2, DateTimeKind.Local).ToString("O")
//     |> equal "2014-09-11T16:37:02.000+02:00" // Here the time zone is Europe/Paris (GMT+2)

[<Fact>]
let ``DateTime from Year 1 to 99 works`` () =
    let date = DateTime(1, 1, 2)
    date.Year |> equal 1
    let date = DateTime(99, 1, 2)
    date.Year |> equal 99

[<Fact>]
let ``DateTime UTC from Year 1 to 99 works`` () =
    let date = DateTime(1, 1, 2, 0, 0, 0, DateTimeKind.Utc)
    date.Year |> equal 1
    let date = DateTime(99, 1, 2, 0, 0, 0, DateTimeKind.Utc)
    date.Year |> equal 99

[<Fact>]
let ``DateTime.UnixEpoch works`` () =
    let dt = DateTime.UnixEpoch
    dt.Ticks |> equal 621355968000000000L

[<Fact>]
let ``DateTime.MaxValue works`` () =
    let dt = DateTime.MaxValue
    dt.Ticks |> equal 3155378975999999999L

[<Fact>]
let ``DateTime.MinValue works`` () =
    let dt = DateTime.MinValue
    dt.Ticks |> equal 0L

[<Fact>]
let ``DateTime.MinValue works in pattern match`` () =
    let d1 = Some DateTime.Now
    match d1 with
    | Some date when date <> DateTime.MinValue -> ()
    | _ -> failwith "expected pattern match above"

[<Fact>]
let ``DateTime.ToLocalTime works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    let d' = d.ToLocalTime()
    d.Kind <> d'.Kind
    |> equal true

[<Fact>]
let ``Creating DateTimeOffset from DateTime and back works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    let dto = DateTimeOffset(d)
    let d' = dto.DateTime
    d' |> equal d

// [<Fact>]
// let ``Formatting DateTimeOffset works`` () =
//     let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//     let dto = DateTimeOffset(d)
//     // dto.ToString() |> equal "2014-10-09 13:23:30 +00:00"
//     dto.ToString("HH:mm:ss", CultureInfo.InvariantCulture) |> equal "13:23:30"

[<Fact>]
let ``DateTime.Parse without offset works`` () =
    let dtStr = "2016-07-07T01:02:03.004"
    let dt = DateTime.Parse(dtStr)
    dt.Kind |> equal DateTimeKind.Unspecified
    dt.Day |> equal 7
    dt.Hour |> equal 1
    dt.Minute |> equal 2
    dt.Second |> equal 3
    dt.Millisecond |> equal 4

[<Fact>]
let ``DateTime.Parse with offset works`` () =
    let dtStr = "2016-07-07T01:02:03.004-05:00"
    let dto = DateTimeOffset.Parse(dtStr)
    let dt = DateTime.Parse(dtStr)
    dt.Kind |> equal DateTimeKind.Local
    dt |> equal dto.LocalDateTime

[<Fact>]
let ``DateTime.Hour works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    d.Hour |> equal 13

// // TODO: These four tests don't match exactly between .NET and JS
// // Think of a way to compare the results approximately
// [<Fact>]
// let ``DateTime.ToLongDateString works`` () =
//     let dt = DateTime(2014, 9, 11, 16, 37, 0)
//     let s = dt.ToLongDateString()
//     s.Length > 0
//     |> equal true

// [<Fact>]
// let ``DateTime.ToShortDateString works`` () =
//     let dt = DateTime(2014, 9, 11, 16, 37, 0)
//     let s = dt.ToShortDateString()
//     s.Length > 0
//     |> equal true

// [<Fact>]
// let ``DateTime.ToLongTimeString works`` () =
//     let dt = DateTime(2014, 9, 11, 16, 37, 0)
//     let s = dt.ToLongTimeString()
//     s.Length > 0
//     |> equal true

// [<Fact>]
// let ``DateTime.ToShortTimeString works`` () =
//     let dt = DateTime(2014, 9, 11, 16, 37, 0)
//     let s = dt.ToShortTimeString()
//     s.Length > 0
//     |> equal true

[<Fact>]
let ``DateTime constructors work`` () =
    let d1 = DateTime(2014, 10, 9)
    let d2 = DateTime(2014, 10, 9, 13, 23, 30)
    let d3 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    let d4 = DateTime(2014, 10, 9, 13, 23, 30, 500)
    let d5 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
    d1.Day + d2.Second + d3.Second + d4.Millisecond + d5.Millisecond
    |> equal 1069

[<Fact>]
let ``DateTime constructor from Ticks works`` () =
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
let ``DateTime.Ticks does not care about kind`` () =
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
let ``DateTime <-> Ticks isomorphism`` () =
    let checkIsomorphism (d: DateTime) =
        try
            let ticks = d.Ticks
            let kind = d.Kind
            let fromTicks = DateTime ticks
            let fromTicksWithKind = DateTime (ticks, kind)

            equal d fromTicks
            equal ticks fromTicks.Ticks
            equal d fromTicksWithKind
            equal ticks fromTicksWithKind.Ticks
            equal kind fromTicksWithKind.Kind
        with e ->
            failwithf "%A: %O" d e

        try
            equal d.Ticks (DateTime d.Ticks).Ticks
        with e ->
            failwithf "%s%O" "replacement bug. " e

    checkIsomorphism DateTime.MinValue
    checkIsomorphism DateTime.MaxValue
    checkIsomorphism DateTime.Now
    checkIsomorphism DateTime.UtcNow
    checkIsomorphism <| DateTime(2014, 10, 9)
    checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30)
    checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500)
    checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)

[<Fact>]
let ``DateTime.IsLeapYear works`` () =
    DateTime.IsLeapYear(2014) |> equal false
    DateTime.IsLeapYear(2016) |> equal true

// // TODO: Re-enable this test when we can fix it in the CI servers
// // [<Fact>]
// // let ``DateTime.IsDaylightSavingTime works`` () =
// //     let d1 = DateTime(2017, 7, 18, 2, 0, 0)
// //     let d2 = DateTime(2017, 12, 18, 2, 0, 0)
// //     d1.IsDaylightSavingTime() |> equal true
// //     d2.IsDaylightSavingTime() |> equal false

[<Fact>]
let ``DateTime.DaysInMonth works`` () =
    DateTime.DaysInMonth(2014, 1) |> equal 31
    DateTime.DaysInMonth(2014, 2) |> equal 28
    DateTime.DaysInMonth(2014, 4) |> equal 30
    DateTime.DaysInMonth(2016, 2) |> equal 29

[<Fact>]
let ``DateTime.Now works`` () =
    let d = DateTime.Now
    d > DateTime.MinValue |> equal true

[<Fact>]
let ``DateTime.UtcNow works`` () =
    let d = DateTime.UtcNow
    d > DateTime.MinValue |> equal true

[<Fact>]
let ``DateTime.Parse Now works`` () =
    let d = DateTime.Now
    let d2 = DateTime.Parse(d.ToString("o"))
    d2 |> equal d

[<Fact>]
let ``DateTime.Parse UtcNow works`` () =
    let d = DateTime.UtcNow
    let d2 = DateTime.Parse(d.ToString("o")).ToUniversalTime()
    d2 |> equal d

[<Fact>]
let ``DateTime.Parse with provider works`` () =
    let d = DateTime.Parse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture)
    d.Year + d.Month + d.Day + d.Hour + d.Minute
    |> equal 2096

// [<Fact>]
// let ``DateTime.Parse with time-only string works`` () = // See #1045
//     let d = DateTime.Parse("13:50:34", CultureInfo.InvariantCulture)
//     d.Hour + d.Minute + d.Second |> equal 97
//     let d = DateTime.Parse("1:5:34 AM", CultureInfo.InvariantCulture)
//     d.Hour + d.Minute + d.Second |> equal 40
//     let d = DateTime.Parse("1:5:34 PM", CultureInfo.InvariantCulture)
//     d.Hour + d.Minute + d.Second |> equal 52

[<Fact>]
let ``DateTime.TryParse works`` () =
    let f (d: string) =
        match DateTime.TryParse(d, CultureInfo.InvariantCulture, DateTimeStyles.None) with
        | true, _ -> true
        | false, _ -> false
    f "foo" |> equal false
    f "9/10/2014 1:50:34 PM" |> equal true
    // f "1:50:34" |> equal true //TODO:

// [<Fact>]
// let ``Parsing doesn't succeed for invalid dates`` () =
//     let invalidAmericanDate = "13/1/2020"
//     let r, _date = DateTime.TryParse(invalidAmericanDate, CultureInfo.InvariantCulture, DateTimeStyles.None)
//     r |> equal false

[<Fact>]
let ``DateTime.Today works`` () =
    let d = DateTime.Today
    equal 0 d.Hour

[<Fact>]
let ``DateTime.ToUniversalTime works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    d.ToUniversalTime().Kind <> d.Kind
    |> equal true

[<Fact>]
let ``DateTime.SpecifyKind works`` () = // See #1844
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d2 = DateTime.SpecifyKind(d, DateTimeKind.Utc)
    d2.Kind |> equal DateTimeKind.Utc
    d.Ticks = d2.Ticks |> equal true
    // let d3 = d.ToUniversalTime()
    // d.Ticks = d3.Ticks |> equal false

[<Fact>]
let ``DateTime.Date works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30)
    d.Date.Hour |> equal 0
    d.Date.Day |> equal 9

[<Fact>]
let ``DateTime.Day works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    d.Day + d'.Day |> equal 18

[<Fact>]
let ``DateTime.DayOfWeek works`` () =
    let d = DateTime(2014, 10, 9)
    d.DayOfWeek |> equal DayOfWeek.Thursday

[<Fact>]
let ``DateTime.DayOfYear works`` () =
    let d = DateTime(2014, 10, 9)
    d.DayOfYear |> equal 282

[<Fact>]
let ``DateTime.Millisecond works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
    d.Millisecond |> equal 999

[<Fact>]
let ``DateTime.Ticks works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, 999, DateTimeKind.Utc)
    d.Ticks |> equal 635484578109990000L
    let d = DateTime(2014, 10, 9, 13, 23, 30, 999, DateTimeKind.Local)
    d.Ticks |> equal 635484578109990000L
    let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
    d.Ticks |> equal 635484578109990000L

[<Fact>]
let ``DateTime.Minute works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    d.Minute + d'.Minute
    |> equal 46

[<Fact>]
let ``DateTime.Month works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    d.Month + d'.Month
    |> equal 20

[<Fact>]
let ``DateTime.Second works`` () =
    let d = DateTime(2014,9,12,0,0,30)
    let d' = DateTime(2014,9,12,0,0,59)
    d.Second + d'.Second
    |> equal 89

[<Fact>]
let ``DateTime.Year works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    d.Year + d'.Year
    |> equal 4028

[<Fact>]
let ``DateTime.AddYears works`` () =
    let test v expected =
        let dt = DateTime(2016,2,29,0,0,0,DateTimeKind.Utc).AddYears(v)
        dt.Month + dt.Day
        |> equal expected
    test 100 31
    test 1 30
    test -1 30
    test -100 31
    test 0 31

[<Fact>]
let ``DateTime.AddMonths works`` () =
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
let ``DateTime.AddDays works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddDays(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 30585600.0
    test -100. 13305600.0
    test 0. 21945600.0

[<Fact>]
let ``DateTime.AddHours works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddHours(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 22305600.0
    test -100. 21585600.0
    test 0. 21945600.0

[<Fact>]
let ``DateTime.AddMinutes works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddMinutes(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21951600.0
    test -100. 21939600.0
    test 0. 21945600.0

[<Fact>]
let ``DateTime.AddSeconds works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddSeconds(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21945700.0
    test -100. 21945500.0
    test 0. 21945600.0

[<Fact>]
let ``DateTime.AddMilliseconds works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddMilliseconds(v)
        thatYearMilliseconds dt
        |> equal expected
    test 100. 2.19456001e+10
    test -100. 2.19455999e+10
    test 0. 2.19456e+10

[<Fact>]
let ``DateTime.AddTicks works`` () =
    let test v expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddTicks(v)
        dt.Ticks
        |> equal expected
    let ticks = 635460768000000000L
    test 100000L (ticks + 100000L)
    test -100000L (ticks - 100000L)
    test 0L ticks

[<Fact>]
let ``DateTime Addition works`` () =
    let test ms expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc)
        let ts = TimeSpan.FromMilliseconds(ms)
        let res1 = dt.Add(ts) |> thatYearSeconds
        let res2 = (dt + ts) |> thatYearSeconds
        (res1 = res2) |> equal true
        res1 |> equal expected
    test 1000. 21945601.0
    test -1000. 21945599.0
    test 0. 21945600.0

[<Fact>]
let ``DateTime Subtraction with TimeSpan works`` () =
    let test ms expected =
        let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc)
        let ts = TimeSpan.FromMilliseconds(ms)
        let res1 = dt.Subtract(ts) |> thatYearSeconds
        let res2 = (dt - ts) |> thatYearSeconds
        (res1 = res2) |> equal true
        res1 |> equal expected
    test 1000. 21945599.0
    test -1000. 21945601.0
    test 0. 21945600.0

[<Fact>]
let ``DateTime Subtraction with DateTime works`` () =
    let test ms expected =
        let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
        let dt2 = dt1.AddMilliseconds(ms)
        let res1 = dt1.Subtract(dt2).TotalSeconds
        let res2 = (dt1 - dt2).TotalSeconds
        (res1 = res2) |> equal true
        res1 |> equal expected
    test 1000. -1.0
    test -1000. 1.0
    test 0. 0.0

[<Fact>]
let ``DateTime Comparison works`` () =
    let test ms expected =
        let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
        let dt2 = dt1.AddMilliseconds(ms)
        let res1 = compare dt1 dt2
        let res2 = dt1.CompareTo(dt2)
        let res3 = DateTime.Compare(dt1, dt2)
        (res1 = res2 && res2 = res3) |> equal true
        res1 |> equal expected
    test 1000. -1
    test -1000. 1
    test 0. 0

[<Fact>]
let ``DateTime GreaterThan works`` () =
    let test ms expected =
        let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 > dt2 |> equal expected
    test 1000. false
    test -1000. true
    test 0. false

[<Fact>]
let ``DateTime LessThan works`` () =
    let test ms expected =
        let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 < dt2 |> equal expected
    test 1000. true
    test -1000. false
    test 0. false

[<Fact>]
let ``DateTime Equality works`` () =
    let test ms expected =
        let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 = dt2 |> equal expected
    test 1000. false
    test -1000. false
    test 0. true

[<Fact>]
let ``DateTime Inequality works`` () =
    let test ms expected =
        let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 <> dt2 |> equal expected
    test 1000. true
    test -1000. true
    test 0. false

[<Fact>]
let ``DateTime TimeOfDay works`` () =
    let d = DateTime(2014, 10, 9, 13, 23, 30, 1, DateTimeKind.Utc)
    let t = d.TimeOfDay
    t |> equal (TimeSpan(0, 13, 23, 30, 1))

// In regions with daylight saving time, 20/10/2019 will have different timezone
// offset than 29/10/2019
[<Fact>]
let ``Adding days to a local date works even if daylight saving time changes`` () =
    let dt = DateTime(2019, 10, 20, 0, 0, 0, DateTimeKind.Local)
    dt.AddDays(9.).Day |> equal 29
