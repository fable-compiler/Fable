[<Util.Testing.TestFixture>]
module Fable.Tests.DateTimeOffset
open System
open Util.Testing
open Fable.Tests.Util

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

[<Test>]
let ``DateTimeOffset.ToString with format works``() =
    DateTimeOffset(2014, 9, 11, 16, 37, 0, TimeSpan.Zero).ToString("HH:mm")
    |> equal "16:37"

[<Test>]
let ``DateTimeOffset.ToString without separator works``() = // See #1131
    DateTimeOffset(2017, 9, 5, 0, 0, 0, TimeSpan.Zero).ToString("yyyyMM")
    |> equal "201709"

// TODO
// [<Test>]
// let ``TimeSpan.ToString with format works``() =
//     TimeSpan.FromMinutes(234.).ToString("hh\:mm\:ss")
//     |> equal "03:54:00"

[<Test>]
let ``DateTimeOffset.ToString with Roundtrip format works for Utc``() =
    let str = DateTimeOffset(2014, 9, 11, 16, 37, 2, TimeSpan.Zero).ToString("O")
    System.Text.RegularExpressions.Regex.Replace(str, "0{3,}", "000")
    |> equal "2014-09-11T16:37:02.000+00:00"

[<Test>]
let ``DateTimeOffset can be JSON serialized forth and back``() =
    let dof = DateTimeOffset(2016, 8, 4, 17, 30, 0, TimeSpan.FromHours(3.))
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson dof
    let dof = Fable.Core.JsInterop.ofJson<DateTimeOffset> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject dof
    let dof = Newtonsoft.Json.JsonConvert.DeserializeObject<DateTimeOffset> json
    #endif
    dof.Offset |> equal (TimeSpan.FromHours(3.))
    dof.ToString("HH:mm") |> equal "17:30"

[<Test>]
let ``DateTimeOffset from Year 1 to 99 works``() =
    let date = DateTimeOffset(1, 1, 1, 0, 0, 0, TimeSpan.Zero)
    date.Year |> equal 1
    let date = DateTimeOffset(99, 1, 1, 0, 0, 0, TimeSpan.Zero)
    date.Year |> equal 99

// TODO: These two tests give different values for .NET and JS because DateTimeOffset
// becomes as a plain JS Date object, so I'm just checking the fields get translated
[<Test>]
let ``DateTimeOffset.MaxValue works``() =
    let d1 = DateTimeOffset.Now
    let d2 = DateTimeOffset.MaxValue
    d1 < d2 |> equal true

[<Test>]
let ``DateTimeOffset.MinValue works``() =
    let d1 = DateTimeOffset.Now
    let d2 = DateTimeOffset.MinValue
    d1 < d2 |> equal false

[<Test>]
let ``DateTimeOffset.MinValue works in pattern match``() =
    let d1 = Some DateTimeOffset.Now
    match d1 with
    | Some date when date <> DateTimeOffset.MinValue -> ()
    | _ -> failwith "expected pattern match above"

// TODO: Enable these tests

// [<Test>]
// let ``DateTimeOffset.ToLocalTime works``() =
//     let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
//     let d' = d.ToLocalTime()

// [<Test>]
// let ``DateTime.ToUniversalTime works``() =
//     let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
//     let d' = d.ToUniversalTime()

[<Test>]
let ``DateTimeOffset.Hour works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Hour |> equal 13

// TODO: Unfortunately, JS will happily create invalid dates like DateTimeOffset(2014,2,29)
//       But this problem also happens when parsing, so I haven't tried to fix it
[<Test>]
let ``DateTimeOffset constructors work``() =
    let d3 = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d5 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
    d3.Second + d5.Millisecond
    |> equal 530

[<Test>]
let ``DateTimeOffset constructor from Ticks works``() =
    let d = DateTimeOffset(624059424000000000L, TimeSpan.Zero)
    equal 1978 d.Year
    equal 7 d.Month
    equal 27 d.Day

// DateTimeOffset specific tests -----------------------

[<Test>]
let ``DateTimeOffset constructors with DateTime work``() =
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

[<Test>]
let ``DateTimeOffset.FromUnixTimeSeconds work``() =
    let d = DateTimeOffset.FromUnixTimeSeconds(1510310885L)
    equal 2017 d.Year
    equal 11 d.Month
    equal 10 d.Day

[<Test>]
let ``DateTimeOffset.FromUnixTimeMilliseconds work``() =
    let d = DateTimeOffset.FromUnixTimeMilliseconds(1510310885000L)
    equal 2017 d.Year
    equal 11 d.Month
    equal 10 d.Day

[<Test>]
let ``DateTimeOffset.ToUnixTimeSeconds works``() =
    let d = DateTimeOffset(2017, 11, 10, 0, 0, 0, TimeSpan.Zero)
    d.ToUnixTimeSeconds() |> equal 1510272000L

[<Test>]
let ``DateTimeOffset.ToUnixTimeMilliseconds works``() =
    let d = DateTimeOffset(2017, 11, 10, 0, 0, 0, TimeSpan.Zero)
    d.ToUnixTimeMilliseconds() |> equal 1510272000000L

[<Test>]
let ``DateTimeOffset.LocalDateTime works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d2 = d.LocalDateTime
    equal d.Year d2.Year
    equal d.Month d2.Month
    equal d.Day d2.Day

[<Test>]
let ``DateTimeOffset.UtcDateTime works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d2 = d.UtcDateTime
    equal d.Year d2.Year
    equal d.Month d2.Month
    equal d.Day d2.Day

[<Test>]
let ``DateTimeOffset.UtcTicks works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.UtcTicks |> equal 635484578109990000L

// -----------------------------------------

[<Test>]
let ``DateTimeOffset.Now works``() =
    let d = DateTimeOffset.Now
    d > DateTimeOffset.MinValue |> equal true

[<Test>]
let ``DateTimeOffset.UtcNow works``() =
    let d = DateTimeOffset.UtcNow
    d > DateTimeOffset.MinValue |> equal true

[<Test>]
let ``DateTimeOffset.Parse works``() =
    let d = DateTimeOffset.Parse("9/10/2014 1:50:34 PM")
    d.Year + d.Month + d.Day + d.Hour + d.Minute
    |> equal 2096

[<Test>]
let ``DateTimeOffset.Parse with time-only string works``() = // See #1045
    let d = DateTimeOffset.Parse("13:50:34")
    d.Hour + d.Minute + d.Second |> equal 97
    let d = DateTimeOffset.Parse("1:5:34 AM")
    d.Hour + d.Minute + d.Second |> equal 40
    let d = DateTimeOffset.Parse("1:5:34 PM")
    d.Hour + d.Minute + d.Second |> equal 52

[<Test>]
let ``DateTimeOffset.TryParse works``() =
    let f d =
        match DateTimeOffset.TryParse(d) with
        | true, _ -> true
        | false, _ -> false
    f "foo" |> equal false
    f "9/10/2014 1:50:34 PM" |> equal true
    f "1:50:34" |> equal true

[<Test>]
let ``DateTimeOffset.Date works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Date.Hour |> equal 0
    d.Date.Day |> equal 9

[<Test>]
let ``DateTimeOffset.Day works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Day |> equal 9

[<Test>]
let ``DateTimeOffset.DayOfWeek works``() =
    let d = DateTimeOffset(2014, 10, 9, 0, 0, 0, TimeSpan.Zero)
    d.DayOfWeek |> equal DayOfWeek.Thursday

[<Test>]
let ``DateTimeOffset.DayOfYear works``() =
    let d = DateTimeOffset(2014, 10, 9, 0, 0, 0, TimeSpan.Zero)
    d.DayOfYear |> equal 282

[<Test>]
let ``DateTimeOffset.Millisecond works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.Millisecond |> equal 999

[<Test>]
let ``DateTimeOffset.Ticks works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.Ticks |> equal 635484578109990000L
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.FromMinutes(-150.))
    d.Ticks |> equal 635484578109990000L

[<Test>]
let ``DateTimeOffset.Minute works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Minute |> equal 23

[<Test>]
let ``DateTimeOffset.Month works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Month |> equal 10

[<Test>]
let ``DateTimeOffset.Second works``() =
    let d = DateTimeOffset(2014,9,12,0,0,30,TimeSpan.Zero)
    d.Second |> equal 30

[<Test>]
let ``DateTimeOffset.Year works``() =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Year |> equal 2014

[<Test>]
let ``DateTimeOffset.AddYears works``() =
    let test v expected =
        let dt = DateTimeOffset(2016,2,29,0,0,0,TimeSpan.Zero).AddYears(v)
        equal expected (dt.Month + dt.Day)
    test 100 31
    test 1 30
    test -1 30
    test -100 31
    test 0 31

[<Test>]
let ``DateTimeOffset.AddMonths works``() =
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

[<Test>]
let ``DateTimeOffset.AddDays works``() =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddDays(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 30585600.0
    test -100. 13305600.0
    test 0. 21945600.0

[<Test>]
let ``DateTimeOffset.AddHours works``() =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddHours(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 22305600.0
    test -100. 21585600.0
    test 0. 21945600.0

[<Test>]
let ``DateTimeOffset.AddMinutes works``() =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddMinutes(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21951600.0
    test -100. 21939600.0
    test 0. 21945600.0

[<Test>]
let ``DateTimeOffset.AddSeconds works``() =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddSeconds(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21945700.0
    test -100. 21945500.0
    test 0. 21945600.0

[<Test>]
let ``DateTimeOffset.AddMilliseconds works``() =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddMilliseconds(v)
        thatYearMilliseconds dt
        |> equal expected
    test 100. 2.19456001e+10
    test -100. 2.19455999e+10
    test 0. 2.19456e+10

// NOTE: Doesn't work for values between 10000L (TimeSpan.TicksPerMillisecond) and -10000L, except 0L
[<Test>]
let ``DateTimeOffset.AddTicks works``() =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddTicks(v)
        dt.Ticks
        |> equal expected
    let ticks = 635460768000000000L
    test 100000L (ticks + 100000L)
    test -100000L (ticks - 100000L)
    test 0L ticks

[<Test>]
let ``DateTimeOffset Addition works``() =
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

[<Test>]
let ``DateTimeOffset Subtraction with TimeSpan works``() =
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

[<Test>]
let ``DateTimeOffset Subtraction with DateTimeOffset works``() =
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

[<Test>]
let ``DateTimeOffset Comparison works``() =
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

[<Test>]
let ``DateTimeOffset GreaterThan works``() =
    let test ms expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 > dt2 |> equal expected
    test 1000. false
    test -1000. true
    test 0. false

[<Test>]
let ``DateTimeOffset LessThan works``() =
    let test ms expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 < dt2 |> equal expected
    test 1000. true
    test -1000. false
    test 0. false

[<Test>]
let ``DateTimeOffset Equality works``() =
    let test ms expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 = dt2 |> equal expected
    test 1000. false
    test -1000. false
    test 0. true

[<Test>]
let ``DateTimeOffset Inequality works``() =
    let test ms expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        dt1 <> dt2 |> equal expected
    test 1000. true
    test -1000. true
    test 0. false

[<Test>]
let ``DateTimeOffset TimeOfDay works``() =
    let d = System.DateTimeOffset(2014, 10, 9, 13, 23, 30, 1, TimeSpan.Zero)
    let t = d.TimeOfDay

    t |> equal (TimeSpan(0, 13, 23, 30, 1))
