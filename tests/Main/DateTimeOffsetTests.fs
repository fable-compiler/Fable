module Fable.Tests.DateTimeOffset

open System
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

let tests =
  testList "DateTimeOffset" [
    testCase "DateTimeOffset.ToString with format works" <| fun () ->
        DateTimeOffset(2014, 9, 11, 16, 37, 0, TimeSpan.Zero).ToString("HH:mm", System.Globalization.CultureInfo.InvariantCulture)
        |> equal "16:37"

    testCase "DateTimeOffset.ToString without separator works" <| fun () -> // See #1131
        DateTimeOffset(2017, 9, 5, 0, 0, 0, TimeSpan.Zero).ToString("yyyyMM")
        |> equal "201709"

    // TODO
    // testCase "TimeSpan.ToString with format works" <| fun () ->
    //     TimeSpan.FromMinutes(234.).ToString("hh\:mm\:ss")
    //     |> equal "03:54:00"

    testCase "DateTimeOffset.ToString with Roundtrip format works for Utc" <| fun () ->
        let str = DateTimeOffset(2014, 9, 11, 16, 37, 2, TimeSpan.Zero).ToString("O")
        System.Text.RegularExpressions.Regex.Replace(str, "0{3,}", "000")
        |> equal "2014-09-11T16:37:02.000+00:00"

    testCase "DateTimeOffset from Year 1 to 99 works" <| fun () ->
        let date = DateTimeOffset(1, 1, 1, 0, 0, 0, TimeSpan.Zero)
        date.Year |> equal 1
        let date = DateTimeOffset(99, 1, 1, 0, 0, 0, TimeSpan.Zero)
        date.Year |> equal 99

    // TODO: These two tests give different values for .NET and JS because DateTimeOffset
    // becomes as a plain JS Date object, so I'm just checking the fields get translated
    testCase "DateTimeOffset.MaxValue works" <| fun () ->
        let d1 = DateTimeOffset.Now
        let d2 = DateTimeOffset.MaxValue
        d1 < d2 |> equal true

    testCase "DateTimeOffset.MinValue works" <| fun () ->
        let d1 = DateTimeOffset.Now
        let d2 = DateTimeOffset.MinValue
        d1 < d2 |> equal false

    testCase "DateTimeOffset.MinValue works in pattern match" <| fun () ->
        let d1 = Some DateTimeOffset.Now
        match d1 with
        | Some date when date <> DateTimeOffset.MinValue -> ()
        | _ -> failwith "expected pattern match above"

    // TODO: Enable these tests
    // testCase "DateTimeOffset.ToLocalTime works" <| fun () ->
    //     let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    //     let d' = d.ToLocalTime()

    // testCase "DateTime.ToUniversalTime works" <| fun () ->
    //     let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    //     let d' = d.ToUniversalTime()

    testCase "DateTimeOffset.Hour works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        d.Hour |> equal 13

    // TODO: Unfortunately, JS will happily create invalid dates like DateTimeOffset(2014,2,29)
    //       But this problem also happens when parsing, so I haven't tried to fix it
    testCase "DateTimeOffset constructors work" <| fun () ->
        let d3 = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        let d5 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
        d3.Second + d5.Millisecond
        |> equal 530

    testCase "DateTimeOffset constructor from Ticks works" <| fun () ->
        let d = DateTimeOffset(624059424000000000L, TimeSpan.Zero)
        equal 1978 d.Year
        equal 7 d.Month
        equal 27 d.Day

    testCase "DateTimeOffset.Ticks does not care about offset" <| fun () ->
        let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
        let d2 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours 1.0)
        let d3 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours -5.0)
        equal d1.Ticks d2.Ticks
        equal d1.Ticks d3.Ticks
        equal d2.Ticks d3.Ticks

    testCase "DateTimeOffset equality cares about offset" <| fun () ->
        let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
        let d2 = DateTimeOffset(2014, 10, 9, 14, 23, 30, 500, TimeSpan.FromHours 1.0)
        equal d1 d2

    testCase "DateTimeOffset DateTime - Offset - UtcDateTime relationship" <| fun () ->
        let dto = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours -5.0)
        let date = dto.DateTime
        let utcDate = dto.UtcDateTime
        let offset = dto.Offset
        let compound = date - offset
        equal compound utcDate

    // Note: This test is "trivial" if current offset to UTC is 0.
    testCase "DateTimeOffset UtcDateTime - LocalDateTime relationship" <| fun () ->
        let dto = DateTimeOffset.Now
        let localOffset = dto.Offset
        let localDate = dto.LocalDateTime
        let utcDate = dto.UtcDateTime
        let compound = localDate - localOffset
        equal compound utcDate

    testCase "DateTimeOffset <-> Ticks isomorphism" <| fun () ->
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
        // checkIsomorphism <| DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours 1.0)
        // checkIsomorphism <| DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours -5.0)

    // DateTimeOffset specific tests -----------------------

    testCase "DateTimeOffset constructors with DateTime work" <| fun () ->
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

    testCase "DateTimeOffset.FromUnixTimeSeconds work" <| fun () ->
        let d = DateTimeOffset.FromUnixTimeSeconds(1510310885L)
        equal 2017 d.Year
        equal 11 d.Month
        equal 10 d.Day

    testCase "DateTimeOffset.FromUnixTimeMilliseconds work" <| fun () ->
        let d = DateTimeOffset.FromUnixTimeMilliseconds(1510310885000L)
        equal 2017 d.Year
        equal 11 d.Month
        equal 10 d.Day

    testCase "DateTimeOffset.ToUnixTimeSeconds works" <| fun () ->
        let d = DateTimeOffset(2017, 11, 10, 0, 0, 0, TimeSpan.Zero)
        d.ToUnixTimeSeconds() |> equal 1510272000L

    testCase "DateTimeOffset.ToUnixTimeMilliseconds works" <| fun () ->
        let d = DateTimeOffset(2017, 11, 10, 0, 0, 0, TimeSpan.Zero)
        d.ToUnixTimeMilliseconds() |> equal 1510272000000L

    testCase "DateTimeOffset.LocalDateTime works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        let d2 = d.LocalDateTime
        equal d.Year d2.Year
        equal d.Month d2.Month
        equal d.Day d2.Day

    testCase "DateTimeOffset.UtcDateTime works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        let d2 = d.UtcDateTime
        equal d.Year d2.Year
        equal d.Month d2.Month
        equal d.Day d2.Day

    testCase "DateTimeOffset.UtcTicks works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
        d.UtcTicks |> equal 635484578109990000L

    // -----------------------------------------

    testCase "DateTimeOffset.Now works" <| fun () ->
        let d = DateTimeOffset.Now
        d > DateTimeOffset.MinValue |> equal true

    testCase "DateTimeOffset.UtcNow works" <| fun () ->
        let d = DateTimeOffset.UtcNow
        d > DateTimeOffset.MinValue |> equal true

    testCase "DateTimeOffset.Parse works" <| fun () ->
        let d = DateTimeOffset.Parse("9/10/2014 1:50:34 PM")
        d.Year + d.Month + d.Day + d.Hour + d.Minute
        |> equal 2096

    testCase "DateTimeOffset.Parse with time-only string works" <| fun () -> // See #1045
        let d = DateTimeOffset.Parse("13:50:34")
        d.Hour + d.Minute + d.Second |> equal 97
        let d = DateTimeOffset.Parse("1:5:34 AM")
        d.Hour + d.Minute + d.Second |> equal 40
        let d = DateTimeOffset.Parse("1:5:34 PM")
        d.Hour + d.Minute + d.Second |> equal 52

    testCase "DateTimeOffset.Parse with only date and offset works" <| fun () -> // See #1422
        let d = DateTimeOffset.Parse("05/01/2008 +3:00")
        d.Year + d.Month + d.Day |> equal 2014
        d.Offset |> equal (TimeSpan.FromHours(3.))

    testCase "DateTimeOffset.TryParse works" <| fun () ->
        let f (d: string) =
            match DateTimeOffset.TryParse(d) with
            | true, _ -> true
            | false, _ -> false
        f "foo" |> equal false
        f "9/10/2014 1:50:34 PM" |> equal true
        f "1:50:34" |> equal true

    testCase "DateTimeOffset.Date works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        d.Date.Hour |> equal 0
        d.Date.Day |> equal 9

    testCase "DateTimeOffset.Day works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        d.Day |> equal 9

    testCase "DateTimeOffset.DayOfWeek works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 0, 0, 0, TimeSpan.Zero)
        d.DayOfWeek |> equal DayOfWeek.Thursday

    testCase "DateTimeOffset.DayOfYear works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 0, 0, 0, TimeSpan.Zero)
        d.DayOfYear |> equal 282

    testCase "DateTimeOffset.Millisecond works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
        d.Millisecond |> equal 999

    testCase "DateTimeOffset.Ticks works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
        d.Ticks |> equal 635484578109990000L
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.FromMinutes(-150.))
        d.Ticks |> equal 635484578109990000L

    testCase "DateTimeOffset.Minute works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        d.Minute |> equal 23

    testCase "DateTimeOffset.Month works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        d.Month |> equal 10

    testCase "DateTimeOffset.Second works" <| fun () ->
        let d = DateTimeOffset(2014,9,12,0,0,30,TimeSpan.Zero)
        d.Second |> equal 30

    testCase "DateTimeOffset.Year works" <| fun () ->
        let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
        d.Year |> equal 2014

    testCase "DateTimeOffset.AddYears works" <| fun () ->
        let test v expected =
            let dt = DateTimeOffset(2016,2,29,0,0,0,TimeSpan.Zero).AddYears(v)
            equal expected (dt.Month + dt.Day)
        test 100 31
        test 1 30
        test -1 30
        test -100 31
        test 0 31

    testCase "DateTimeOffset.AddMonths works" <| fun () ->
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

    testCase "DateTimeOffset.AddDays works" <| fun () ->
        let test v expected =
            let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddDays(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 30585600.0
        test -100. 13305600.0
        test 0. 21945600.0

    testCase "DateTimeOffset.AddHours works" <| fun () ->
        let test v expected =
            let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddHours(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 22305600.0
        test -100. 21585600.0
        test 0. 21945600.0

    testCase "DateTimeOffset.AddMinutes works" <| fun () ->
        let test v expected =
            let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddMinutes(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 21951600.0
        test -100. 21939600.0
        test 0. 21945600.0

    testCase "DateTimeOffset.AddSeconds works" <| fun () ->
        let test v expected =
            let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddSeconds(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 21945700.0
        test -100. 21945500.0
        test 0. 21945600.0

    testCase "DateTimeOffset.AddMilliseconds works" <| fun () ->
        let test v expected =
            let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddMilliseconds(v)
            thatYearMilliseconds dt
            |> equal expected
        test 100. 2.19456001e+10
        test -100. 2.19455999e+10
        test 0. 2.19456e+10

    // NOTE: Doesn't work for values between 10000L (TimeSpan.TicksPerMillisecond) and -10000L, except 0L
    testCase "DateTimeOffset.AddTicks works" <| fun () ->
        let test v expected =
            let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddTicks(v)
            dt.Ticks
            |> equal expected
        let ticks = 635460768000000000L
        test 100000L (ticks + 100000L)
        test -100000L (ticks - 100000L)
        test 0L ticks

    testCase "DateTimeOffset Addition works" <| fun () ->
        let test ms expected =
            let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero)
            let ts = TimeSpan.FromMilliseconds(ms)
            let res1 = dt.Add(ts) |> thatYearSeconds
            let res2 = (dt + ts) |> thatYearSeconds
            equal true (res1 = res2)
            equal expected res1
        test 1000. 21945601.0
        test -1000. 21945599.0
        test 0. 21945600.0

    testCase "DateTimeOffset Subtraction with TimeSpan works" <| fun () ->
        let test ms expected =
            let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero)
            let ts = TimeSpan.FromMilliseconds(ms)
            let res1 = dt.Subtract(ts) |> thatYearSeconds
            let res2 = (dt - ts) |> thatYearSeconds
            equal true (res1 = res2)
            equal expected res1
        test 1000. 21945599.0
        test -1000. 21945601.0
        test 0. 21945600.0

    testCase "DateTimeOffset Subtraction with DateTimeOffset works" <| fun () ->
        let test ms expected =
            let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
            let dt2 = dt1.AddMilliseconds(ms)
            let res1 = dt1.Subtract(dt2).TotalSeconds
            let res2 = (dt1 - dt2).TotalSeconds
            equal true (res1 = res2)
            equal expected res1
        test 1000. -1.0
        test -1000. 1.0
        test 0. 0.0

    testCase "DateTimeOffset Comparison works" <| fun () ->
        let test ms expected =
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

    testCase "DateTimeOffset GreaterThan works" <| fun () ->
        let test ms expected =
            let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 > dt2 |> equal expected
        test 1000. false
        test -1000. true
        test 0. false

    testCase "DateTimeOffset LessThan works" <| fun () ->
        let test ms expected =
            let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 < dt2 |> equal expected
        test 1000. true
        test -1000. false
        test 0. false

    testCase "DateTimeOffset Equality works" <| fun () ->
        let test ms expected =
            let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 = dt2 |> equal expected
        test 1000. false
        test -1000. false
        test 0. true

    testCase "DateTimeOffset.EqualsExact works" <| fun () ->
        let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let d2 = DateTimeOffset(2014, 10, 9, 14, 23, 30, 234, TimeSpan.FromHours(1.))
        d1.Equals(d2) |> equal true
        d1.EqualsExact(d2) |> equal false

    testCase "DateTimeOffset Inequality works" <| fun () ->
        let test ms expected =
            let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 <> dt2 |> equal expected
        test 1000. true
        test -1000. true
        test 0. false

    testCase "DateTimeOffset TimeOfDay works" <| fun () ->
        let d = System.DateTimeOffset(2014, 10, 9, 13, 23, 30, 1, TimeSpan.Zero)
        let t = d.TimeOfDay

        t |> equal (TimeSpan(0, 13, 23, 30, 1))
  ]