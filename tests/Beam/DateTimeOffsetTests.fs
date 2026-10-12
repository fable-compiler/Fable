module Fable.Tests.DateTimeOffsetTests

open System
open Util.Testing
open System.Globalization

let thatYearSeconds (dt: DateTimeOffset) =
    (dt - DateTimeOffset(dt.Year, 1, 1, 0, 0, 0, TimeSpan.Zero)).TotalSeconds

let thatYearMilliseconds (dt: DateTimeOffset) =
    (dt - DateTimeOffset(dt.Year, 1, 1, 0, 0, 0, TimeSpan.Zero)).TotalMilliseconds

// ---------------------------------------------------------------------------
// Constructors
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset constructors work`` () =
    let d3 = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d5 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
    d3.Second + d5.Millisecond
    |> equal 530

[<Fact>]
let ``test DateTimeOffset from Year 1 to 99 works`` () =
    let date = DateTimeOffset(1, 1, 2, 0, 0, 0, TimeSpan.Zero)
    date.Year |> equal 1
    let date = DateTimeOffset(99, 1, 2, 0, 0, 0, TimeSpan.Zero)
    date.Year |> equal 99

[<Fact>]
let ``test DateTimeOffset constructor from Ticks works`` () =
    let d = DateTimeOffset(624059424000000000L, TimeSpan.Zero)
    equal 1978 d.Year
    equal 7 d.Month
    equal 27 d.Day

[<Fact>]
let ``test DateTimeOffset constructor from DateTime works`` () =
    let d1 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    let d1' = DateTimeOffset(d1, TimeSpan.Zero)
    equal d1.Year d1'.Year
    equal d1.Day d1'.Day
    equal d1.Hour d1'.Hour

// ---------------------------------------------------------------------------
// Static fields
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset.MaxValue works`` () =
    let d1 = DateTimeOffset.Now
    let d2 = DateTimeOffset.MaxValue
    d1 < d2 |> equal true

[<Fact>]
let ``test DateTimeOffset.MinValue works`` () =
    let d1 = DateTimeOffset.Now
    let d2 = DateTimeOffset.MinValue
    d1 < d2 |> equal false

[<Fact>]
let ``test DateTimeOffset.UnixEpoch works`` () =
    let epoch = DateTimeOffset.UnixEpoch
    equal 1970 epoch.Year
    equal 1 epoch.Month
    equal 1 epoch.Day

// ---------------------------------------------------------------------------
// Properties
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset.Now works`` () =
    let d = DateTimeOffset.Now
    d > DateTimeOffset.MinValue |> equal true

[<Fact>]
let ``test DateTimeOffset.UtcNow works`` () =
    let d = DateTimeOffset.UtcNow
    d > DateTimeOffset.MinValue |> equal true

[<Fact>]
let ``test DateTimeOffset.Hour works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Hour |> equal 13

[<Fact>]
let ``test DateTimeOffset.Year works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Year |> equal 2014

[<Fact>]
let ``test DateTimeOffset.Month works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Month |> equal 10

[<Fact>]
let ``test DateTimeOffset.Day works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Day |> equal 9

[<Fact>]
let ``test DateTimeOffset.Minute works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Minute |> equal 23

[<Fact>]
let ``test DateTimeOffset.Second works`` () =
    let d = DateTimeOffset(2014,9,12,0,0,30,TimeSpan.Zero)
    d.Second |> equal 30

[<Fact>]
let ``test DateTimeOffset.Millisecond works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.Millisecond |> equal 999

[<Fact>]
let ``test DateTimeOffset.Offset works`` () =
    let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d1.Offset |> equal TimeSpan.Zero
    let d2 = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.FromHours(3.0))
    d2.Offset |> equal (TimeSpan.FromHours(3.0))

[<Fact>]
let ``test DateTimeOffset.DayOfWeek works`` () =
    let d = DateTimeOffset(2014, 10, 9, 0, 0, 0, TimeSpan.Zero)
    d.DayOfWeek |> equal DayOfWeek.Thursday

[<Fact>]
let ``test DateTimeOffset.DayOfYear works`` () =
    let d = DateTimeOffset(2014, 10, 9, 0, 0, 0, TimeSpan.Zero)
    d.DayOfYear |> equal 282

[<Fact>]
let ``test DateTimeOffset.Date works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Date.Hour |> equal 0
    d.Date.Day |> equal 9

[<Fact>]
let ``test DateTimeOffset.DateTime works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let dt = d.DateTime
    dt.Year |> equal 2014
    dt.Month |> equal 10
    dt.Day |> equal 9

[<Fact>]
let ``test DateTimeOffset.TimeOfDay works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 1, TimeSpan.Zero)
    let t = d.TimeOfDay
    t |> equal (TimeSpan(0, 13, 23, 30, 1))

[<Fact>]
let ``test DateTimeOffset.Ticks does not care about offset`` () =
    let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
    let d2 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours 1.0)
    let d3 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours -5.0)
    equal d1.Ticks d2.Ticks
    equal d1.Ticks d3.Ticks
    equal d2.Ticks d3.Ticks

[<Fact>]
let ``test DateTimeOffset.Ticks works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.Ticks |> equal 635484578109990000L
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.FromMinutes(-150.))
    d.Ticks |> equal 635484578109990000L

[<Fact>]
let ``test DateTimeOffset.UtcTicks works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, 999, TimeSpan.Zero)
    d.UtcTicks |> equal 635484578109990000L

// ---------------------------------------------------------------------------
// Equality / Comparison
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset equality cares about offset`` () =
    let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.Zero)
    let d2 = DateTimeOffset(2014, 10, 9, 14, 23, 30, 500, TimeSpan.FromHours 1.0)
    d1.Equals(d2) |> equal true

[<Fact>]
let ``test DateTimeOffset Comparison works`` () =
    let test (ms: float) expected =
        let dt1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
        let dt2 = dt1.AddMilliseconds(ms)
        let res1 = dt1.CompareTo(dt2)
        let res2 = DateTimeOffset.Compare(dt1, dt2)
        equal true (res1 = res2)
        equal expected res1
    test 1000. -1
    test -1000. 1
    test 0. 0

[<Fact>]
let ``test DateTimeOffset.EqualsExact works`` () =
    let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, 234, TimeSpan.Zero)
    let d2 = DateTimeOffset(2014, 10, 9, 14, 23, 30, 234, TimeSpan.FromHours(1.))
    d1.Equals(d2) |> equal true
    d1.EqualsExact(d2) |> equal false

// ---------------------------------------------------------------------------
// Conversion
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset.ToLocalTime works`` () =
    let d = DateTimeOffset.UtcNow
    let localTime = d.ToLocalTime()
    let utcTime = d.ToUniversalTime()
    localTime.ToUniversalTime().Equals(utcTime) |> equal true

[<Fact>]
let ``test DateTimeOffset.ToUniversalTime works`` () =
    let d = DateTimeOffset.Now
    let localTime = d.ToLocalTime()
    let utcTime = d.ToUniversalTime()
    localTime.ToUniversalTime().Equals(utcTime) |> equal true

[<Fact>]
let ``test DateTimeOffset DateTime - Offset - UtcDateTime relationship`` () =
    let dto = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, TimeSpan.FromHours -5.0)
    let date = dto.DateTime
    let utcDate = dto.UtcDateTime
    let offset = dto.Offset
    let compound = date - offset
    // Compare by ticks since DateTime Kind may differ (Unspecified vs UTC)
    equal compound.Ticks utcDate.Ticks

[<Fact>]
let ``test DateTimeOffset.LocalDateTime works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d2 = d.LocalDateTime
    equal d.Year d2.Year
    equal d.Month d2.Month
    equal d.Day d2.Day

[<Fact>]
let ``test DateTimeOffset.UtcDateTime works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let d2 = d.UtcDateTime
    equal d.Year d2.Year
    equal d.Month d2.Month
    equal d.Day d2.Day

// ---------------------------------------------------------------------------
// Unix time
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset.FromUnixTimeSeconds work`` () =
    let d = DateTimeOffset.FromUnixTimeSeconds(1510310885L)
    equal 2017 d.Year
    equal 11 d.Month
    equal 10 d.Day

[<Fact>]
let ``test DateTimeOffset.FromUnixTimeMilliseconds work`` () =
    let d = DateTimeOffset.FromUnixTimeMilliseconds(1510310885000L)
    equal 2017 d.Year
    equal 11 d.Month
    equal 10 d.Day

[<Fact>]
let ``test DateTimeOffset.ToUnixTimeSeconds works`` () =
    let d = DateTimeOffset(2017, 11, 10, 0, 0, 0, TimeSpan.Zero)
    d.ToUnixTimeSeconds() |> equal 1510272000L

[<Fact>]
let ``test DateTimeOffset.ToUnixTimeMilliseconds works`` () =
    let d = DateTimeOffset(2017, 11, 10, 0, 0, 0, TimeSpan.Zero)
    d.ToUnixTimeMilliseconds() |> equal 1510272000000L

// ---------------------------------------------------------------------------
// Add methods
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset.AddYears works`` () =
    let test v expected =
        let dt = DateTimeOffset(2016,2,29,0,0,0,TimeSpan.Zero).AddYears(v)
        equal expected (dt.Month + dt.Day)
    test 100 31
    test 1 30
    test -1 30
    test -100 31
    test 0 31

[<Fact>]
let ``test DateTimeOffset.AddMonths works`` () =
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
let ``test DateTimeOffset.AddDays works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddDays(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 30585600.0
    test -100. 13305600.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTimeOffset.AddHours works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddHours(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 22305600.0
    test -100. 21585600.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTimeOffset.AddMinutes works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddMinutes(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21951600.0
    test -100. 21939600.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTimeOffset.AddSeconds works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddSeconds(v)
        thatYearSeconds dt
        |> equal expected
    test 100. 21945700.0
    test -100. 21945500.0
    test 0. 21945600.0

[<Fact>]
let ``test DateTimeOffset.AddMilliseconds works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddMilliseconds(v)
        thatYearMilliseconds dt
        |> equal expected
    test 100. 2.19456001e+10
    test -100. 2.19455999e+10
    test 0. 2.19456e+10

[<Fact>]
let ``test DateTimeOffset.AddTicks works`` () =
    let test v expected =
        let dt = DateTimeOffset(2014,9,12,0,0,0,TimeSpan.Zero).AddTicks(v)
        dt.Ticks
        |> equal expected
    let ticks = 635460768000000000L
    test 100000L (ticks + 100000L)
    test -100000L (ticks - 100000L)
    test 0L ticks

[<Fact>]
let ``test DateTimeOffset Addition works`` () =
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

// ---------------------------------------------------------------------------
// Subtract
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset Subtraction with TimeSpan works`` () =
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
let ``test DateTimeOffset Subtraction with DateTimeOffset works`` () =
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

// ---------------------------------------------------------------------------
// Parse / ToString
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset.TryParse works`` () =
    let f (d: string) =
        match DateTimeOffset.TryParse(d) with
        | true, _ -> true
        | false, _ -> false
    f "foo" |> equal false
    f "9/10/2014 1:50:34 PM" |> equal true
    f "9/10/2014 1:50:34" |> equal true
    f "2014-09-10T13:50:34" |> equal true

[<Fact>]
let ``test DateTimeOffset.TryParse preserves a numeric zone offset`` () =
    let parse (s: string) =
        let (isSuccess, d) = DateTimeOffset.TryParse s
        isSuccess |> equal true
        d

    let d = parse "2026-08-03T16:30:15+02:00"
    d.Hour |> equal 16
    d.Offset |> equal (TimeSpan.FromHours 2.0)

    let d = parse "2026-08-03T09:30:15-05:00"
    d.Hour |> equal 9
    d.Offset |> equal (TimeSpan.FromHours -5.0)

    // Compact and hour-only forms
    let d = parse "2026-08-03T16:30:15+0200"
    d.Offset |> equal (TimeSpan.FromHours 2.0)

    let d = parse "2026-08-03T16:30:15+02"
    d.Offset |> equal (TimeSpan.FromHours 2.0)

    // Fractional seconds combined with an offset
    let d = parse "2026-08-03T16:30:15.1234567+02:00"
    d.Millisecond |> equal 123
    d.Offset |> equal (TimeSpan.FromHours 2.0)

    // "Z" keeps a zero offset
    let d = parse "2026-08-03T14:30:15Z"
    d.Offset |> equal TimeSpan.Zero

    // A bare timestamp still parses, keeping its wall clock (its offset is the
    // machine's local offset, so it is not asserted here)
    let d = parse "2026-08-03T14:30:15"
    d.Hour |> equal 14

[<Fact>]
let ``test DateTimeOffset.Parse with a zone offset denotes the same instant as Z`` () =
    let d = DateTimeOffset.Parse "2026-08-03T16:30:15+02:00"
    let utc = DateTimeOffset.Parse "2026-08-03T14:30:15Z"
    d = utc |> equal true
    d.EqualsExact utc |> equal false

[<Fact>]
let ``test DateTimeOffset equality compares the instant`` () =
    let utc = DateTimeOffset(2026, 8, 3, 14, 30, 15, TimeSpan.Zero)
    let plus2 = DateTimeOffset(2026, 8, 3, 16, 30, 15, TimeSpan.FromHours 2.0)
    utc = plus2 |> equal true
    utc <> plus2 |> equal false
    utc.Equals plus2 |> equal true
    // EqualsExact additionally requires the same offset
    utc.EqualsExact plus2 |> equal false
    utc <= plus2 |> equal true
    utc >= plus2 |> equal true
    utc < plus2 |> equal false
    compare utc plus2 |> equal 0

[<Fact>]
let ``test DateTimeOffset round-trips through the O format`` () =
    let d = DateTimeOffset(2026, 8, 3, 16, 30, 15, TimeSpan.FromHours 2.0)
    d.ToString("O") |> equal "2026-08-03T16:30:15.0000000+02:00"
    (DateTimeOffset.Parse (d.ToString "O")).EqualsExact d |> equal true

[<Fact>]
let ``test DateTimeOffset.ToString() default works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let str = d.ToString()
    str.Length > 0 |> equal true

[<Fact>]
let ``test DateTimeOffset.ToString with custom format works`` () =
    DateTimeOffset(2014, 9, 11, 16, 37, 0, TimeSpan.Zero).ToString("HH:mm", CultureInfo.InvariantCulture)
    |> equal "16:37"

// ---------------------------------------------------------------------------
// DateOnly + TimeOnly constructor
// ---------------------------------------------------------------------------

[<Fact>]
let ``test DateTimeOffset constructor from DateOnly and TimeOnly works`` () =
    let d = DateOnly(2014, 10, 9)
    let t = TimeOnly(13, 23, 30, 500)
    let o = TimeSpan.FromHours 5.0
    let dt = DateTimeOffset(d, t, o)
    let dt_exp = DateTimeOffset(2014, 10, 9, 13, 23, 30, 500, o)
    dt.Year |> equal dt_exp.Year
    dt.Month |> equal dt_exp.Month
    dt.Day |> equal dt_exp.Day
    dt.Hour |> equal dt_exp.Hour
    dt.Minute |> equal dt_exp.Minute
    dt.Second |> equal dt_exp.Second
    dt.Millisecond |> equal dt_exp.Millisecond
