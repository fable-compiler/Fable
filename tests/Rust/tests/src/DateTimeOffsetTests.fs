module Fable.Tests.DateTimeOffsetTests

open System
open System.Globalization
open Util.Testing

let toSigFigs nSigFigs x =
    let absX = abs x
    let digitsToStartOfNumber = floor(log10 absX) + 1. // x > 0 => +ve | x < 0 => -ve
    let digitsToAdjustNumberBy = int digitsToStartOfNumber - nSigFigs
    let scale = pown 10. digitsToAdjustNumberBy
    round(x / scale) * scale

let thatYearSeconds (dt: DateTimeOffset) =
    (dt - DateTimeOffset(dt.Year, 1, 1, 0, 0, 0, TimeSpan.Zero)).TotalSeconds

let thatYearMilliseconds (dt: DateTimeOffset) =
    (dt - DateTimeOffset(dt.Year, 1, 1, 0, 0, 0, TimeSpan.Zero)).TotalMilliseconds

[<Fact>]
let ``DateTimeOffset.ToString with custom format works`` () =
    DateTimeOffset(2014, 9, 11, 16, 37, 0, TimeSpan.Zero).ToString("HH:mm", CultureInfo.InvariantCulture)
    |> equal "16:37"

[<Fact>]
let ``DateTimeOffset.ToString without separator works`` () = // See #1131
    DateTimeOffset(2017, 9, 5, 0, 0, 0, TimeSpan.Zero).ToString("yyyyMM")
    |> equal "201709"

[<Fact>]
let ``DateTimeOffset.ToString custom format with offset and fractions works`` () =
    DateTimeOffset(1, 2, 3, 4, 5, 6, 7, 89, TimeSpan.FromHours 5.0)
        .ToString("yyyy-MM-ddTHH:mm:ss.fffffff z zz zzz K", CultureInfo.InvariantCulture)
    |> equal "0001-02-03T04:05:06.0070890 +5 +05 +05:00 +05:00"

[<Fact>]
let ``DateTimeOffset.ToString with Roundtrip format works for Utc`` () =
    let str = DateTimeOffset(2014, 9, 11, 16, 37, 2, TimeSpan.Zero).ToString("O")
    System.Text.RegularExpressions.Regex.Replace(str, "0{3,}", "000")
    |> equal "2014-09-11T16:37:02.000+00:00"

[<Fact>]
let ``DateTimeOffset.ToString with d specifiers work`` () =
    // %d: day-of-month, no leading zero (1-31)
    // dd: day-of-month, with leading zero (01-31)
    // ddd: abbreviated day name
    // dddd: full day name
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt.ToString("%d", CultureInfo.InvariantCulture)   |> equal "15"
    dt.ToString("dd", CultureInfo.InvariantCulture)   |> equal "15"
    dt.ToString("ddd", CultureInfo.InvariantCulture)  |> equal "Mon"
    dt.ToString("dddd", CultureInfo.InvariantCulture) |> equal "Monday"
    let dt2 = DateTimeOffset(2009, 6, 3, 13, 45, 30, TimeSpan.Zero)
    dt2.ToString("%d", CultureInfo.InvariantCulture)  |> equal "3"
    dt2.ToString("dd", CultureInfo.InvariantCulture)  |> equal "03"

[<Fact>]
let ``DateTimeOffset.ToString with M specifiers work`` () =
    // %M: month number, no leading zero (1-12)
    // MM: month number, with leading zero (01-12)
    // MMM: abbreviated month name
    // MMMM: full month name
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt.ToString("%M", CultureInfo.InvariantCulture)   |> equal "6"
    dt.ToString("MM", CultureInfo.InvariantCulture)   |> equal "06"
    dt.ToString("MMM", CultureInfo.InvariantCulture)  |> equal "Jun"
    dt.ToString("MMMM", CultureInfo.InvariantCulture) |> equal "June"
    let dt2 = DateTimeOffset(2009, 11, 15, 13, 45, 30, TimeSpan.Zero)
    dt2.ToString("%M", CultureInfo.InvariantCulture)  |> equal "11"
    dt2.ToString("MM", CultureInfo.InvariantCulture)  |> equal "11"
    dt2.ToString("MMM", CultureInfo.InvariantCulture) |> equal "Nov"

[<Fact>]
let ``DateTimeOffset.ToString with y specifiers work`` () =
    // %y: year mod 100 without leading zero
    // yy: year mod 100 with leading zero
    // yyy: 3-digit year
    // yyyy: 4-digit year
    // yyyyy: 5-digit year
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt.ToString("%y", CultureInfo.InvariantCulture)    |> equal "9"
    dt.ToString("yy", CultureInfo.InvariantCulture)    |> equal "09"
    dt.ToString("yyy", CultureInfo.InvariantCulture)   |> equal "2009"
    dt.ToString("yyyy", CultureInfo.InvariantCulture)  |> equal "2009"
    dt.ToString("yyyyy", CultureInfo.InvariantCulture) |> equal "02009"

[<Fact>]
let ``DateTimeOffset.ToString with h and hh specifiers work`` () =
    // h: 12-hour clock, no leading zero (1-12)
    // hh: 12-hour clock, with leading zero (01-12)
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt.ToString("%h", CultureInfo.InvariantCulture)  |> equal "1"
    dt.ToString("hh", CultureInfo.InvariantCulture)  |> equal "01"
    let dt2 = DateTimeOffset(2009, 6, 15, 0, 45, 30, TimeSpan.Zero)
    dt2.ToString("%h", CultureInfo.InvariantCulture) |> equal "12"
    dt2.ToString("hh", CultureInfo.InvariantCulture) |> equal "12"
    let dt3 = DateTimeOffset(2009, 6, 15, 12, 45, 30, TimeSpan.Zero)
    dt3.ToString("%h", CultureInfo.InvariantCulture) |> equal "12"
    dt3.ToString("hh", CultureInfo.InvariantCulture) |> equal "12"

[<Fact>]
let ``DateTimeOffset.ToString with H and HH specifiers work`` () =
    // H: 24-hour clock, no leading zero (0-23)
    // HH: 24-hour clock, with leading zero (00-23)
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt.ToString("%H", CultureInfo.InvariantCulture)  |> equal "13"
    dt.ToString("HH", CultureInfo.InvariantCulture)  |> equal "13"
    let dt2 = DateTimeOffset(2009, 6, 15, 1, 45, 30, TimeSpan.Zero)
    dt2.ToString("%H", CultureInfo.InvariantCulture) |> equal "1"
    dt2.ToString("HH", CultureInfo.InvariantCulture) |> equal "01"
    let dt3 = DateTimeOffset(2009, 6, 15, 0, 45, 30, TimeSpan.Zero)
    dt3.ToString("%H", CultureInfo.InvariantCulture) |> equal "0"
    dt3.ToString("HH", CultureInfo.InvariantCulture) |> equal "00"

[<Fact>]
let ``DateTimeOffset.ToString with m and mm specifiers work`` () =
    // %m: minute, no leading zero (0-59)
    // mm: minute, with leading zero (00-59)
    let dt = DateTimeOffset(2009, 6, 15, 13, 5, 30, TimeSpan.Zero)
    dt.ToString("%m", CultureInfo.InvariantCulture)  |> equal "5"
    dt.ToString("mm", CultureInfo.InvariantCulture)  |> equal "05"
    let dt2 = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt2.ToString("%m", CultureInfo.InvariantCulture) |> equal "45"
    dt2.ToString("mm", CultureInfo.InvariantCulture) |> equal "45"

[<Fact>]
let ``DateTimeOffset.ToString with s and ss specifiers work`` () =
    // %s: second, no leading zero (0-59)
    // ss: second, with leading zero (00-59)
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 3, TimeSpan.Zero)
    dt.ToString("%s", CultureInfo.InvariantCulture)  |> equal "3"
    dt.ToString("ss", CultureInfo.InvariantCulture)  |> equal "03"
    let dt2 = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt2.ToString("%s", CultureInfo.InvariantCulture) |> equal "30"
    dt2.ToString("ss", CultureInfo.InvariantCulture) |> equal "30"

[<Fact>]
let ``DateTimeOffset.ToString with f fraction specifiers work`` () =
    // 617ms = 6170000 ticks of sub-second fraction
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 30, 617, TimeSpan.Zero)
    dt.ToString("ss.f", CultureInfo.InvariantCulture)       |> equal "30.6"
    dt.ToString("ss.ff", CultureInfo.InvariantCulture)      |> equal "30.61"
    dt.ToString("ss.fff", CultureInfo.InvariantCulture)     |> equal "30.617"
    dt.ToString("ss.ffff", CultureInfo.InvariantCulture)    |> equal "30.6170"
    dt.ToString("ss.fffff", CultureInfo.InvariantCulture)   |> equal "30.61700"
    dt.ToString("ss.ffffff", CultureInfo.InvariantCulture)  |> equal "30.617000"
    dt.ToString("ss.fffffff", CultureInfo.InvariantCulture) |> equal "30.6170000"

[<Fact>]
let ``DateTimeOffset.ToString with F fraction specifiers work`` () =
    // F: like f but trailing zeros are stripped; dot is suppressed when empty
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 30, 617, TimeSpan.Zero)
    dt.ToString("ss.F", CultureInfo.InvariantCulture)       |> equal "30.6"
    dt.ToString("ss.FF", CultureInfo.InvariantCulture)      |> equal "30.61"
    dt.ToString("ss.FFF", CultureInfo.InvariantCulture)     |> equal "30.617"
    dt.ToString("ss.FFFF", CultureInfo.InvariantCulture)    |> equal "30.617"
    dt.ToString("ss.FFFFF", CultureInfo.InvariantCulture)   |> equal "30.617"
    dt.ToString("ss.FFFFFF", CultureInfo.InvariantCulture)  |> equal "30.617"
    dt.ToString("ss.FFFFFFF", CultureInfo.InvariantCulture) |> equal "30.617"
    // all-zero fraction: dot and fraction entirely suppressed
    let dt0 = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt0.ToString("ss.FFFFFFF", CultureInfo.InvariantCulture) |> equal "30"
    dt0.ToString("ss.F", CultureInfo.InvariantCulture)       |> equal "30"

[<Fact>]
let ``DateTimeOffset.ToString with t and tt specifiers work`` () =
    // t: first character of AM/PM designator
    // tt: full AM/PM designator
    let dtAm = DateTimeOffset(2009, 6, 15, 5, 45, 30, TimeSpan.Zero)
    dtAm.ToString("%t", CultureInfo.InvariantCulture)   |> equal "A"
    dtAm.ToString("tt", CultureInfo.InvariantCulture)   |> equal "AM"
    let dtPm = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dtPm.ToString("%t", CultureInfo.InvariantCulture)   |> equal "P"
    dtPm.ToString("tt", CultureInfo.InvariantCulture)   |> equal "PM"
    let dtMid = DateTimeOffset(2009, 6, 15, 0, 0, 0, TimeSpan.Zero)
    dtMid.ToString("tt", CultureInfo.InvariantCulture)  |> equal "AM"
    let dtNoon = DateTimeOffset(2009, 6, 15, 12, 0, 0, TimeSpan.Zero)
    dtNoon.ToString("tt", CultureInfo.InvariantCulture) |> equal "PM"

[<Fact>]
let ``DateTimeOffset.ToString with g and gg specifiers work`` () =
    // g / gg: era ("A.D." in Gregorian calendar with InvariantCulture)
    let dt = DateTimeOffset(2009, 6, 15, 13, 45, 30, TimeSpan.Zero)
    dt.ToString("%g", CultureInfo.InvariantCulture) |> equal "A.D."
    dt.ToString("gg", CultureInfo.InvariantCulture) |> equal "A.D."

[<Fact>]
let ``DateTimeOffset from Year 1 to 99 works`` () =
    let date = DateTimeOffset(1, 1, 2, 0, 0, 0, TimeSpan.Zero)
    date.Year |> equal 1
    let date = DateTimeOffset(99, 1, 2, 0, 0, 0, TimeSpan.Zero)
    date.Year |> equal 99

// TODO: These two tests give different values for .NET and JS because DateTimeOffset
// becomes as a plain JS Date object, so I'm just checking the fields get translated
[<Fact>]
let ``DateTimeOffset.MaxValue works`` () =
    let d1 = DateTimeOffset.Now
    let d2 = DateTimeOffset.MaxValue
    d1 < d2 |> equal true

[<Fact>]
let ``DateTimeOffset.MinValue works`` () =
    let d1 = DateTimeOffset.Now
    let d2 = DateTimeOffset.MinValue
    d1 < d2 |> equal false

[<Fact>]
let ``DateTimeOffset.MinValue works in pattern match`` () =
    let d1 = Some DateTimeOffset.Now
    match d1 with
    | Some date when date <> DateTimeOffset.MinValue -> ()
    | _ -> failwith "expected pattern match above"

[<Fact>]
let ``DateTimeOffset.ToLocalTime works`` () =
    let d = DateTimeOffset.UtcNow
    let localTime = d.ToLocalTime()
    let utcTime = d.ToUniversalTime()
    localTime.ToUniversalTime() |> equal utcTime

[<Fact>]
let ``DateTime.ToUniversalTime works`` () =
    let d = DateTimeOffset.Now
    let localTime = d.ToLocalTime()
    let utcTime = d.ToUniversalTime()
    localTime.ToUniversalTime() |> equal utcTime

[<Fact>]
let ``DateTimeOffset.Hour works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Hour |> equal 13

[<Fact>]
let ``DateTimeOffset constructors work`` () =
    let d3 = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d5 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
    let d6 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, 222, TimeSpan.Zero)
    d3.Second + d5.Millisecond |> equal 530

    d6.Year |> equal 2014
    d6.Month |> equal 10
    d6.Day |> equal 9
    d6.Hour |> equal 13
    d6.Minute |> equal 23
    d6.Second |> equal 30
    d6.Millisecond |> equal 500
    d6.Microsecond |> equal 222
    d6.Nanosecond |> equal 0

[<Fact>]
let ``DateTimeOffset constructor from DateOnly and TimeOnly works`` () =
    let d = DateOnly(2014, 10, 9)
    let t = TimeOnly(13, 23, 30, 500)
    let o = TimeSpan.FromHours 5.0
    let dt = DateTimeOffset(d, t, o)
    let dt_exp = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, o)
    dt.Ticks |> equal dt_exp.Ticks

[<Fact>]
let ``DateTimeOffset constructor from Ticks works`` () =
    let d = DateTimeOffset(624059424000000000L, TimeSpan.Zero)
    equal 1978 d.Year
    equal 7 d.Month
    equal 27 d.Day

[<Fact>]
let ``DateTimeOffset.Ticks does not care about offset`` () =
    let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
    let d2 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours 1.0)
    let d3 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours -5.0)
    equal d1.Ticks d2.Ticks
    equal d1.Ticks d3.Ticks
    equal d2.Ticks d3.Ticks

[<Fact>]
let ``DateTimeOffset equality cares about offset`` () =
    let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
    let d2 = DateTimeOffset(2014, 10, 9, 14, 23, 30, 500, TimeSpan.FromHours 1.0)
    equal d1 d2

[<Fact>]
let ``DateTimeOffset DateTime - Offset - UtcDateTime relationship`` () =
    let dto = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours -5.0)
    let date = dto.DateTime
    let utcDate = dto.UtcDateTime
    let offset = dto.Offset
    let compound = date - offset
    equal compound utcDate

// Note: This test is "trivial" if current offset to UTC is 0.
[<Fact>]
let ``DateTimeOffset UtcDateTime - LocalDateTime relationship`` () =
    let dto = DateTimeOffset.Now
    let localOffset = dto.Offset
    let localDate = dto.LocalDateTime
    let utcDate = dto.UtcDateTime
    let compound = localDate - localOffset
    equal compound utcDate

[<Fact>]
let ``DateTimeOffset to Ticks isomorphism`` () =
    let checkIsomorphism (d: DateTimeOffset) =
        try
            let ticks = d.Ticks
            let utcTicks = d.UtcTicks
            let offset = d.Offset
            let fromTicks = DateTimeOffset (ticks, offset)
            let fromUtcTicks = DateTimeOffset (utcTicks, TimeSpan.Zero)

            equal d fromTicks
            equal d fromUtcTicks
            equal ticks fromTicks.Ticks
            equal utcTicks fromTicks.UtcTicks
            if offset <> TimeSpan.Zero
            then notEqual ticks utcTicks
            else equal ticks utcTicks
        with e ->
            failwithf "%A: %O" d e
    checkIsomorphism DateTimeOffset.MinValue
    checkIsomorphism DateTimeOffset.MaxValue
    checkIsomorphism DateTimeOffset.Now
    checkIsomorphism DateTimeOffset.UtcNow
    checkIsomorphism <| DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
    checkIsomorphism <| DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours 1.0)
    checkIsomorphism <| DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours -5.0)

// DateTimeOffset specific tests -----------------------

[<Fact>]
let ``DateTimeOffset constructors with DateTime work`` () =
    let d1 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    let d1' = DateTimeOffset(d1)
    equal d1.Year d1'.Year
    equal d1.Day d1'.Day
    equal d1.Hour d1'.Hour
    let d2 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    let d2' = DateTimeOffset(d1)
    equal d2.Year d2'.Year
    equal d2.Day d2'.Day
    equal d2.Hour d2'.Hour

[<Fact>]
let ``DateTimeOffset.FromUnixTimeSeconds work`` () =
    let d = DateTimeOffset.FromUnixTimeSeconds(1510310885L)
    equal 2017 d.Year
    equal 11 d.Month
    equal 10 d.Day

[<Fact>]
let ``DateTimeOffset.FromUnixTimeMilliseconds work`` () =
    let d = DateTimeOffset.FromUnixTimeMilliseconds(1510310885000L)
    equal 2017 d.Year
    equal 11 d.Month
    equal 10 d.Day

[<Fact>]
let ``DateTimeOffset.ToUnixTimeSeconds works`` () =
    let d = DateTimeOffset(2017, 11, 10, 0, 0, 0, TimeSpan.Zero)
    d.ToUnixTimeSeconds() |> equal 1510272000L

[<Fact>]
let ``DateTimeOffset.ToUnixTimeMilliseconds works`` () =
    let d = DateTimeOffset(2017, 11, 10, 0, 0, 0, TimeSpan.Zero)
    d.ToUnixTimeMilliseconds() |> equal 1510272000000L

[<Fact>]
let ``DateTimeOffset.LocalDateTime works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d2 = d.LocalDateTime
    equal d.Year d2.Year
    equal d.Month d2.Month
    equal d.Day d2.Day

[<Fact>]
let ``DateTimeOffset.UtcDateTime works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d2 = d.UtcDateTime
    equal d.Year d2.Year
    equal d.Month d2.Month
    equal d.Day d2.Day

[<Fact>]
let ``DateTimeOffset.UtcTicks works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.UtcTicks |> equal 635484578109990000L

// -----------------------------------------

[<Fact>]
let ``DateTimeOffset.Now works`` () =
    let d = DateTimeOffset.Now
    d > DateTimeOffset.MinValue |> equal true

[<Fact>]
let ``DateTimeOffset.UtcNow works`` () =
    let d = DateTimeOffset.UtcNow
    d > DateTimeOffset.MinValue |> equal true

[<Fact>]
let ``DateTimeOffset.Parse Now works`` () =
    let d = DateTimeOffset.Now
    let d2 = DateTimeOffset.Parse(d.ToString("o"))
    d2 |> equal d

[<Fact>]
let ``DateTimeOffset.Parse UtcNow works`` () =
    let d = DateTimeOffset.UtcNow
    let d2 = DateTimeOffset.Parse(d.ToString("o"))
    d2 |> equal d

[<Fact>]
let ``DateTimeOffset.Parse works`` () =
    let d = DateTimeOffset.Parse("9/10/2014 1:50:34 PM")
    d.Year |> equal 2014
    d.Month + d.Day |> equal 19
    d.Hour |> equal 13
    d.Minute |> equal 50
    d.Second |> equal 34

[<Fact>]
let ``DateTimeOffset.Parse with time-only string works`` () = // See #1045
    let d = DateTimeOffset.Parse("13:50:34")
    d.Hour + d.Minute + d.Second |> equal 97
    let d = DateTimeOffset.Parse("1:5:34 AM")
    d.Hour + d.Minute + d.Second |> equal 40
    let d = DateTimeOffset.Parse("1:5:34 PM")
    d.Hour + d.Minute + d.Second |> equal 52

[<Fact>]
let ``DateTimeOffset.Parse with only date and offset works`` () = // See #1422
    let d = DateTimeOffset.Parse("05/01/2008 +03:00")
    d.Year + d.Month + d.Day |> equal 2014
    d.Offset |> equal (TimeSpan.FromHours(3.))

[<Fact>]
let ``DateTimeOffset.Parse doesn't confuse day and offset`` () =
    let d = DateTimeOffset.Parse("2021-11-15")
    d.Year |> equal 2021
    d.Month |> equal 11
    d.Day |> equal 15
    d.Offset = (TimeSpan.FromHours(1.) + TimeSpan.FromMinutes(5.)) |> equal false
    d.Offset = (TimeSpan.FromHours(1.) + TimeSpan.FromMinutes(50.)) |> equal false
    d.Offset = (TimeSpan.FromHours(15.)) |> equal false
    let d = DateTimeOffset.Parse("2021-11-08-08")
    d.Year + d.Month + d.Day |> equal 2040
    d.Offset |> equal (TimeSpan.FromHours(-8.))

[<Fact>]
let ``DateTimeOffset.TryParse works`` () =
    let f (d: string) =
        match DateTimeOffset.TryParse(d) with
        | true, _ -> true
        | false, _ -> false
    f "foo" |> equal false
    f "9/10/2014 1:50:34" |> equal true
    f "9/10/2014 1:50:34 PM" |> equal true
    f "9/10/2014 1:50:34 +03" |> equal true
    f "9/10/2014 1:50:34 -05:00" |> equal true
    f "2014-09-10 01:50:34 +03:00" |> equal true
    f "2014-09-10 01:50:34.1234567 -05:00" |> equal true
    f "1:50:34" |> equal true

[<Fact>]
let ``DateTimeOffset.Date works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Date.Hour |> equal 0
    d.Date.Day |> equal 9

[<Fact>]
let ``DateTimeOffset.Day works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Day |> equal 9

[<Fact>]
let ``DateTimeOffset.DayOfWeek works`` () =
    let d = DateTimeOffset(2014, 10, 9, 0, 0, 0, TimeSpan.Zero)
    d.DayOfWeek |> equal DayOfWeek.Thursday

[<Fact>]
let ``DateTimeOffset.DayOfYear works`` () =
    let d = DateTimeOffset(2014, 10, 9, 0, 0, 0, TimeSpan.Zero)
    d.DayOfYear |> equal 282

[<Fact>]
let ``DateTimeOffset.Millisecond works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.Millisecond |> equal 999

[<Fact>]
let ``DateTimeOffset.Ticks works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.Ticks |> equal 635484578109990000L
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.FromMinutes(-150.))
    d.Ticks |> equal 635484578109990000L

[<Fact>]
let ``DateTimeOffset.Minute works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Minute |> equal 23

[<Fact>]
let ``DateTimeOffset.Month works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Month |> equal 10

[<Fact>]
let ``DateTimeOffset.Second works`` () =
    let d = DateTimeOffset(2014,9,12,0,0,30,TimeSpan.Zero)
    d.Second |> equal 30

[<Fact>]
let ``DateTimeOffset.Year works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Year |> equal 2014

[<Fact>]
let ``DateTimeOffset.AddYears works`` () =
    let test v expected =
        let dt = DateTimeOffset(2016,2,29,0,0,0,TimeSpan.Zero).AddYears(v)
        equal expected (dt.Month + dt.Day)
    test 100 31
    test 1 30
    test -1 30
    test -100 31
    test 0 31

[<Fact>]
let ``DateTimeOffset.AddMonths works`` () =
    let test v expected =
        let dt = DateTimeOffset(2016,1,31,0,0,0,TimeSpan.Zero).AddMonths(v)
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
let ``DateTimeOffset.AddDays works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddDays(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 30585600.0
    test -100. 13305600.0
    test 0. 21945600.0

[<Fact>]
let ``DateTimeOffset.AddHours works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddHours(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 22305600.0
    test -100. 21585600.0
    test 0. 21945600.0

[<Fact>]
let ``DateTimeOffset.AddMinutes works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddMinutes(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21951600.0
    test -100. 21939600.0
    test 0. 21945600.0

[<Fact>]
let ``DateTimeOffset.AddSeconds works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddSeconds(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21945700.0
    test -100. 21945500.0
    test 0. 21945600.0

[<Fact>]
let ``DateTimeOffset.AddMilliseconds works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddMilliseconds(v)
        thatYearMilliseconds dt
        |> equal expected
    test 100. 2.19456001e+10
    test -100. 2.19455999e+10
    test 0. 2.19456e+10

// NOTE: Doesn't work for values between 10000L (TimeSpan.TicksPerMillisecond) and -10000L, except 0L
[<Fact>]
let ``DateTimeOffset.AddTicks works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddTicks(v)
        dt.Ticks
        |> equal expected
    let ticks = 635460768000000000L
    test 100000L (ticks + 100000L)
    test -100000L (ticks - 100000L)
    test 0L ticks

[<Fact>]
let ``DateTimeOffset Addition works`` () =
    let test (ms: float) expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero)
        let ts = TimeSpan.FromMilliseconds(ms)
        let res1 = dt.Add(ts) |> thatYearSeconds
        let res2 = (dt + ts) |> thatYearSeconds
        equal true (res1 = res2)
        equal expected res1
    test 1000. 21945601.0
    test -1000. 21945599.0
    test 0. 21945600.0

[<Fact>]
let ``DateTimeOffset Subtraction with TimeSpan works`` () =
    let test (ms: float) expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero)
        let ts = TimeSpan.FromMilliseconds(ms)
        let res1 = dt.Subtract(ts) |> thatYearSeconds
        let res2 = (dt - ts) |> thatYearSeconds
        equal true (res1 = res2)
        equal expected res1
    test 1000. 21945599.0
    test -1000. 21945601.0
    test 0. 21945600.0

[<Fact>]
let ``DateTimeOffset Subtraction with DateTimeOffset works`` () =
    let test (ms: float) expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        let res1 = dt1.Subtract(dt2).TotalSeconds
        let res2 = (dt1 - dt2).TotalSeconds
        equal true (res1 = res2)
        equal expected res1
    test 1000. -1.0
    test -1000. 1.0
    test 0. 0.0

[<Fact>]
let ``DateTimeOffset Comparison works`` () =
    let test (ms: float) expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        let res1 = compare dt1 dt2
        let res2 = dt1.CompareTo(dt2)
        let res3 = DateTimeOffset.Compare(dt1, dt2)
        equal true (res1 = res2 && res2 = res3)
        equal expected res1
    test 1000. -1
    test -1000. 1
    test 0. 0

[<Fact>]
let ``DateTimeOffset GreaterThan works`` () =
    let test (ms: float) expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 > dt2 |> equal expected
    test 1000. false
    test -1000. true
    test 0. false

[<Fact>]
let ``DateTimeOffset LessThan works`` () =
    let test (ms: float) expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 < dt2 |> equal expected
    test 1000. true
    test -1000. false
    test 0. false

[<Fact>]
let ``DateTimeOffset Equality works`` () =
    let test (ms: float) expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 = dt2 |> equal expected
    test 1000. false
    test -1000. false
    test 0. true

[<Fact>]
let ``DateTimeOffset.EqualsExact works`` () =
    let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
    let d2 = DateTimeOffset(2014, 10, 9, 14, 23, 30, 234, TimeSpan.FromHours(1.))
    d1.Equals(d2) |> equal true
    d1.EqualsExact(d2) |> equal false

[<Fact>]
let ``DateTimeOffset Inequality works`` () =
    let test (ms: float) expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 <> dt2 |> equal expected
    test 1000. true
    test -1000. true
    test 0. false

[<Fact>]
let ``DateTimeOffset TimeOfDay works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 1, TimeSpan.Zero)
    let t = d.TimeOfDay

    t |> equal (TimeSpan(0, 13, 23, 30, 1))


/// date is 2007-09-01
/// -> UTC + 2 in Europe! (summer time)
let usedDate = DateTime(2007, 9, 1, 9, 30, 17, DateTimeKind.Unspecified)
let dateWithKind (kind: DateTimeKind) =
    DateTime(usedDate.Year, usedDate.Month, usedDate.Day, usedDate.Hour, usedDate.Minute, usedDate.Second, kind)

let fromDateList (offset: TimeSpan) =
    DateTimeOffset(usedDate.Year, usedDate.Month, usedDate.Day, usedDate.Hour, usedDate.Minute, usedDate.Second, offset)
let fromDateTime (offset: TimeSpan) =
    let dt = DateTime(usedDate.Year, usedDate.Month, usedDate.Day, usedDate.Hour, usedDate.Minute, usedDate.Second)
    DateTimeOffset(dt, offset)
let fromTicks (offset: TimeSpan) =
    //             633242358000000000L
    DateTimeOffset(usedDate.Ticks, offset)
let fromLocalDateTime (offset: TimeSpan) =
    let dt = dateWithKind DateTimeKind.Local
    DateTimeOffset(dt, offset)
let fromUTCDateTime (offset: TimeSpan) =
    let dt = dateWithKind DateTimeKind.Utc
    DateTimeOffset(dt, offset)
let fromUnspecifiedDateTime (offset: TimeSpan) =
    let dt = dateWithKind DateTimeKind.Unspecified
    DateTimeOffset(dt, offset)

/// Necessary for DateTime(..., DateTimeKind.Local) tests.
/// Unfortunately that's not ideal:
/// * different tests on different systems (with different local time)
/// * Local Time might be same as UTC -> `Offset = 0` -> quite often different behaviour than `Offset <> 0`
///
/// NOTE: local utc offset of `usedDate`, NOT `DateTime.Now`
/// -> `usedDate` is in september -> summer time in Europe (`+2`)!
let localOffset: TimeSpan =
    DateTimeOffset(dateWithKind DateTimeKind.Local).Offset
// #if FABLE_COMPILER
//     !!(usedDate?getTimezoneOffset() * -60_000)
// #else
//     TimeZoneInfo.Local.GetUtcOffset(usedDate)
// #endif

let shouldSucceed = Ok ()
let shouldThrowOutOfRange = Error "Offset must be within plus or minus 14 hours."
let shouldThrowNotMinutes = Error "Offset must be specified in whole minutes."
let shouldThrowUTCOffsetAndLocalDateTimeDontMatch = Error "The UTC Offset of the local dateTime parameter does not match the offset argument."
let shouldThrowUTCOffsetForUTCDateTimeMustBeZero = Error "The UTC Offset for Utc DateTime instances must be 0." // .net: `Utc`... -.-

let offsets = [
    TimeSpan.FromHours(0.0), shouldSucceed

    // positive range
    TimeSpan.FromMinutes(30.0), shouldSucceed
    TimeSpan.FromHours(2.0), shouldSucceed
    TimeSpan(13, 59, 0), shouldSucceed
    TimeSpan.FromHours(14.0), shouldSucceed
    TimeSpan(14, 01, 0), shouldThrowOutOfRange
    TimeSpan(14, 59, 0), shouldThrowOutOfRange
    TimeSpan.FromHours(15.0), shouldThrowOutOfRange
    TimeSpan(15, 01, 0), shouldThrowOutOfRange
    TimeSpan(23, 59, 0), shouldThrowOutOfRange
    TimeSpan.FromHours(24.0), shouldThrowOutOfRange
    TimeSpan(24, 01, 0), shouldThrowOutOfRange
    TimeSpan.FromHours(100.0), shouldThrowOutOfRange

    // negative range
    TimeSpan.FromMinutes(-30.0), shouldSucceed
    TimeSpan.FromHours(-2.0), shouldSucceed
    TimeSpan(-13, -59, 0), shouldSucceed
    TimeSpan.FromHours(-14.0), shouldSucceed
    TimeSpan(-14, -01, 0), shouldThrowOutOfRange
    TimeSpan(-14, -59, 0), shouldThrowOutOfRange
    TimeSpan.FromHours(-15.0), shouldThrowOutOfRange
    TimeSpan(-15, -01, 0), shouldThrowOutOfRange
    TimeSpan(-23, -59, 0), shouldThrowOutOfRange
    TimeSpan.FromHours(-24.0), shouldThrowOutOfRange
    TimeSpan(-24, -01, 0), shouldThrowOutOfRange
    TimeSpan.FromHours(-100.0), shouldThrowOutOfRange

    // positive seconds
    TimeSpan.FromSeconds(10.0), shouldThrowNotMinutes
    TimeSpan.FromSeconds(60.0), shouldSucceed
    // .Net: check for seconds before check for range
    TimeSpan(24, 01, 01), shouldThrowNotMinutes

    // negative seconds
    TimeSpan.FromSeconds(-10.0), shouldThrowNotMinutes
    TimeSpan.FromSeconds(-60.0), shouldSucceed
    TimeSpan(-24, -01, -01), shouldThrowNotMinutes
]

let withCtor ctor offsets =
    offsets
    |> List.map (fun (offset, expected) -> (ctor, offset, expected))

let testSucceeds (ctor: TimeSpan -> DateTimeOffset) offset =
    (fun () -> ctor offset)
    |> doesntThrow

let testThrows expected (ctor: TimeSpan -> DateTimeOffset) offset =
    (fun () -> ctor offset)
    |> throwsErrorContaining expected

let toTestCase (ctor, offset, expected: Result<unit, string>) =
    let formatOffset (offset: TimeSpan) =
        if offset.TotalMinutes - Math.Floor(offset.TotalMinutes) > 0.0 then
            // doesn't handle ms and ticks
            sprintf "%ih %02imin %02is" (int offset.TotalHours) (offset.Minutes) (offset.Seconds)
        else
            sprintf "%ih %02imin" (int offset.TotalHours) (offset.Minutes)
    let formatExpected expected =
        match expected with
        | Ok _ -> "succeeds"
        | Error msg -> sprintf "throws '%s'" msg
    let name =
        sprintf "offset '%s' %s"
            (formatOffset offset)
            (formatExpected expected)
    let test =
        match expected with
        | Ok _ -> testSucceeds
        | Error msg -> testThrows msg
    test ctor offset

[<Fact>]
let ``(year, month, ..., second, offset)`` () =
    offsets
    |> withCtor fromDateList
    |> List.iter toTestCase

[<Fact>]
let ``(ticks, offset)`` () =
    offsets
    |> withCtor fromTicks
    |> List.iter toTestCase

[<Fact>]
let ``Default (= unspecified)`` () =
    let dt = DateTime(usedDate.Year, usedDate.Month, usedDate.Day, usedDate.Hour, usedDate.Minute, usedDate.Second)
    let dto = DateTimeOffset(dt)
    equal dto.Offset localOffset

[<Fact>]
let ``Unspecified`` () =
    let dt = DateTime(usedDate.Year, usedDate.Month, usedDate.Day, usedDate.Hour, usedDate.Minute, usedDate.Second, DateTimeKind.Unspecified)
    let dto = DateTimeOffset(dt)    // no custom offset -> local offset
    equal dto.Offset localOffset

[<Fact>]
let ``UTC`` () =
    let dt = DateTime(usedDate.Year, usedDate.Month, usedDate.Day, usedDate.Hour, usedDate.Minute, usedDate.Second, DateTimeKind.Utc)
    let dto = DateTimeOffset(dt)
    equal dto.Offset TimeSpan.Zero

[<Fact>]
let ``Local`` () =
    let dt = DateTime(usedDate.Year, usedDate.Month, usedDate.Day, usedDate.Hour, usedDate.Minute, usedDate.Second, DateTimeKind.Local)
    let dto = DateTimeOffset(dt)
    equal dto.Offset localOffset

[<Fact>]
let ``(DateTime, offset)`` () =
    offsets
    |> withCtor fromDateTime
    |> List.iter toTestCase

// Unspecified is default -> same as test list above
[<Fact>]
let ``(DateTime(Unspecified), offset)`` () =
    offsets
    |> withCtor fromUnspecifiedDateTime
    |> List.iter toTestCase

// all must fail -- except when offset = localOffset
[<Fact>]
let ``(DateTime(Local)), offset)`` () =
    offsets
    |> List.map (fun (offset, _) ->
        // offset = local offset doesn't throw
        if offset = localOffset then
            // don't adjust to throw: generated test name might conflict with other test case
            // instead emit succeeding test to keep test count same
            // -> same as single testCase below (`offset = localOffset (...) succeeds`)
            (offset, shouldSucceed)
        else
            (offset, shouldThrowUTCOffsetAndLocalDateTimeDontMatch)
    )
    |> withCtor fromLocalDateTime
    |> List.iter toTestCase

[<Fact>]
let ``offset = localOffset succeeds`` () =
    localOffset
    |> testSucceeds fromLocalDateTime

// all except zero offset must fail
[<Fact>]
let ``(DateTime(UTC), offset)`` () =
    offsets
    |> List.map (fun (offset, _) -> (offset, if offset = TimeSpan.Zero then shouldSucceed else shouldThrowUTCOffsetForUTCDateTimeMustBeZero))
    |> withCtor fromUTCDateTime
    |> List.iter toTestCase

// source: https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset.tooffset?view=net-5.0#System_DateTimeOffset_ToOffset_System_TimeSpan_

[<Fact>]
let ``Convert to same time`` () =
    let source = DateTimeOffset(2007, 9, 1, 9, 30, 0, TimeSpan(-5, 0, 0))
    let target = source.ToOffset(TimeSpan(-5, 0, 0))
    let expected = source
    target |> equal expected

[<Fact>]
let ``Convert to UTC (0 offset)`` () =
    let source = DateTimeOffset(2007, 9, 1, 9, 30, 0, TimeSpan(-5, 0, 0))
    let target = source.ToOffset(TimeSpan.Zero)
    let expected = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan(0, 0, 0))
    target |> equal expected

[<Fact>]
let ``Convert to 8 hours behind UTC`` () =
    let source = DateTimeOffset(2007, 9, 1, 9, 30, 0, TimeSpan(-5, 0, 0))
    let target = source.ToOffset(TimeSpan(-8, 0, 0))
    let expected = DateTimeOffset(2007, 9, 1, 6, 30, 0, TimeSpan(-8, 0, 0))
    target |> equal expected

[<Fact>]
let ``Convert to 3 hours ahead of UTC`` () =
    let source = DateTimeOffset(2007, 9, 1, 9, 30, 0, TimeSpan(-5, 0, 0))
    let target = source.ToOffset(TimeSpan(3, 0, 0))
    let expected = DateTimeOffset(2007, 9, 1, 17, 30, 0, TimeSpan(3, 0, 0))
    target |> equal expected

[<Fact>]
let ``Convert to 3 hours and 30 min ahead of UTC`` () =
    let source = DateTimeOffset(2007, 9, 1, 9, 30, 0, TimeSpan(-5, 0, 0))
    let target = source.ToOffset(TimeSpan(3, 30, 0))
    let expected = DateTimeOffset(2007, 9, 1, 18, 0, 0, TimeSpan(3, 30, 0))
    target |> equal expected

[<Fact>]
let ``UTC: Convert to 5 hours behind UTC`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    let target = source.ToOffset(TimeSpan(-5, 0, 0))
    let expected = DateTimeOffset(2007, 9, 1, 9, 30, 0, TimeSpan(-5, 0, 0))
    target |> equal expected

[<Fact>]
let ``UTC: Convert to UTC (0 offset)`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    let target = source.ToOffset(TimeSpan.Zero)
    let expected = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan(0, 0, 0))
    target |> equal expected

[<Fact>]
let ``UTC: Convert to 8 hours behind UTC`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    let target = source.ToOffset(TimeSpan(-8, 0, 0))
    let expected = DateTimeOffset(2007, 9, 1, 6, 30, 0, TimeSpan(-8, 0, 0))
    target |> equal expected

[<Fact>]
let ``UTC: Convert to 3 hours ahead of UTC`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    let target = source.ToOffset(TimeSpan(3, 0, 0))
    let expected = DateTimeOffset(2007, 9, 1, 17, 30, 0, TimeSpan(3, 0, 0))
    target |> equal expected

[<Fact>]
let ``UTC: Convert to 3 hours and 30 min ahead of UTC`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    let target = source.ToOffset(TimeSpan(3, 30, 0))
    let expected = DateTimeOffset(2007, 9, 1, 18, 0, 0, TimeSpan(3, 30, 0))
    target |> equal expected

[<Fact>]
let ``+14h offset doesn't throw exception`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    source.ToOffset(TimeSpan(14, 0, 0))
    |> ignore

[<Fact>]
let ``-14h offset doesn't throw exception`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    source.ToOffset(TimeSpan(-14, 0, 0))
    |> ignore

[<Fact>]
let ``+14h:30min offset throws exception`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    (fun () -> source.ToOffset(TimeSpan(14, 30, 0)))
    |> throwsErrorContaining "Offset must be within plus or minus 14 hour"

[<Fact>]
let ``-14h:30min offset throws exception`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    (fun () -> source.ToOffset(TimeSpan(-14, -30, 0)))
    |> throwsErrorContaining "Offset must be within plus or minus 14 hour"

[<Fact>]
let ``+100h offset throws exception`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    (fun () -> source.ToOffset(TimeSpan(100, 0, 0)))
    |> throwsErrorContaining "Offset must be within plus or minus 14 hour"

[<Fact>]
let ``-100h offset throws exception`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    (fun () -> source.ToOffset(TimeSpan(-100, 0, 0)))
    |> throwsErrorContaining "Offset must be within plus or minus 14 hour"

[<Fact>]
let ``+10s offset throws exception`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    (fun () -> source.ToOffset(TimeSpan(0, 0, 10)))
    |> throwsErrorContaining "Offset must be specified in whole minutes"

[<Fact>]
let ``-10s offset throws exception`` () =
    let source = DateTimeOffset(2007, 9, 1, 14, 30, 0, TimeSpan.Zero)
    (fun () -> source.ToOffset(TimeSpan(0, 0, -10)))
    |> throwsErrorContaining "Offset must be specified in whole minutes"
