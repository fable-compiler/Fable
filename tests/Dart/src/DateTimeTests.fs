module Fable.Tests.Dart.DateTime

open System
open System.Globalization
open Util

let thatYearSeconds (dt: DateTime) =
    (dt - DateTime(dt.Year, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds

let thatYearMilliseconds (dt: DateTime) =
    (dt - DateTime(dt.Year, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalMilliseconds

let tests() =
    testCase "DateTime.ToString with custom format works" <| fun () ->
        DateTime(2014, 9, 11, 16, 37, 0).ToString("HH:mm", CultureInfo.InvariantCulture)
        |> equal "16:37"

    testCase "DateTime.ToString without separator works" <| fun () -> // See #1131
        DateTime(2017, 9, 5).ToString("yyyyMM")
        |> equal "201709"

    testCase "DateTime.ToString with milliseconds" <| fun () -> // See #1726
        DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("ss.fff")
        |> equal "11.345"

    testCase "DateTime.ToString with Round-trip format works for Utc" <| fun () ->
        let str = DateTime(2014, 9, 11, 16, 37, 2, DateTimeKind.Utc).ToString("O")
        System.Text.RegularExpressions.Regex.Replace(str, "0{3,}", "000")
        |> equal "2014-09-11T16:37:02.000Z"

    // TODO
    // Next test is disabled because it's depends on the time zone of the machine
    // A fix could be to use a regex or detect the time zone
    //
    // testCase "DateTime.ToString with Round-trip format works for local" <| fun () ->
    //     DateTime(2014, 9, 11, 16, 37, 2, DateTimeKind.Local).ToString("O")
    //     |> equal "2014-09-11T16:37:02.000+02:00" // Here the time zone is Europe/Paris (GMT+2)

    testCase "DateTime from Year 1 to 99 works" <| fun () ->
       let date = DateTime(1, 1, 2)
       date.Year |> equal 1
       let date = DateTime(99, 1, 2)
       date.Year |> equal 99

    testCase "DateTime UTC from Year 1 to 99 works" <| fun () ->
       let date = DateTime(1, 1, 2, 0, 0, 0, DateTimeKind.Utc)
       date.Year |> equal 1
       let date = DateTime(99, 1, 2, 0, 0, 0, DateTimeKind.Utc)
       date.Year |> equal 99

    // These two tests give different values for .NET and Dart
    // so I'm just checking the fields get translated
    testCase "DateTime.MaxValue works" <| fun () ->
        let d1 = DateTime.Now
        let d2 = DateTime.MaxValue
        d1 < d2 |> equal true

    testCase "DateTime.MinValue works" <| fun () ->
        let d1 = DateTime.Now
        let d2 = DateTime.MinValue
        d1 < d2 |> equal false

    testCase "DateTime.MinValue works in pattern match" <| fun () ->
       let d1 = Some DateTime.Now
       match d1 with
       | Some date when date <> DateTime.MinValue -> ()
       | _ -> failwith "expected pattern match above"

    testCase "DateTime.ToLocalTime works" <| fun () ->
       let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
       let d' = d.ToLocalTime()
       d.Kind <> d'.Kind
       |> equal true

    // TODO DateTimeOffset
//    testCase "Creating DateTimeOffset from DateTime and back works" <| fun () ->
//       let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//       let dto = DateTimeOffset(d)
//       let d' = dto.DateTime
//       d' |> equal d

//    testCase "Formatting DateTimeOffset works" <| fun () ->
//        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//        let dto = DateTimeOffset(d)
//        // dto.ToString() |> equal "2014-10-09 13:23:30 +00:00"
//        dto.ToString("HH:mm:ss", CultureInfo.InvariantCulture) |> equal "13:23:30"

    testCase "DateTime.Hour works" <| fun () ->
       let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
       d.Hour |> equal 13

//    testCase "DateTime.ToLongDateString works" <| fun () ->
//        let dt = DateTime(2014, 9, 11, 16, 37, 0)
//        let s = dt.ToLongDateString()
//        s.Length > 0
//        |> equal true
//
//    testCase "DateTime.ToShortDateString works" <| fun () ->
//        let dt = DateTime(2014, 9, 11, 16, 37, 0)
//        let s = dt.ToShortDateString()
//        s.Length > 0
//        |> equal true
//
//    testCase "DateTime.ToLongTimeString works" <| fun () ->
//        let dt = DateTime(2014, 9, 11, 16, 37, 0)
//        let s = dt.ToLongTimeString()
//        s.Length > 0
//        |> equal true
//
//    testCase "DateTime.ToShortTimeString works" <| fun () ->
//        let dt = DateTime(2014, 9, 11, 16, 37, 0)
//        let s = dt.ToShortTimeString()
//        s.Length > 0
//        |> equal true

    // TODO: Unfortunately, Dart will happily create invalid dates like DateTime(2014,2,29)
    //       But this problem also happens when parsing, so I haven't tried to fix it
    testCase "DateTime constructors work" <| fun () ->
       let d1 = DateTime(2014, 10, 9)
       let d2 = DateTime(2014, 10, 9, 13, 23, 30)
       let d3 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
       let d4 = DateTime(2014, 10, 9, 13, 23, 30, 500)
       let d5 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
       d1.Day + d2.Second + d3.Second + d4.Millisecond + d5.Millisecond
       |> equal 1069

    testCase "DateTime constructor from Ticks works" <| fun () ->
       let d = DateTime(624059424000000000L, DateTimeKind.Utc)
       equal 1978 d.Year
       equal 7 d.Month
       equal 27 d.Day
       equal 0 d.Hour
       equal 0 d.Minute

    // FIXME
    //    let d = DateTime(624059424000000000L, DateTimeKind.Local)
    //    equal 1978 d.Year
    //    equal 7 d.Month
    //    equal 27 d.Day
    //    equal 0 d.Hour
    //    equal 0 d.Minute

    //    let d = DateTime(624059424000000000L)
    //    equal 1978 d.Year
    //    equal 7 d.Month
    //    equal 27 d.Day
    //    equal 0 d.Hour
    //    equal 0 d.Minute

    testCase "DateTime.Ticks does not care about kind" <| fun () ->
    // Not sure why this is working in .NET and other languages, dates should be different :?
    //    let d1 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Local)
    //    let d2 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
    //    equal d1.Ticks d2.Ticks

       let t = DateTime.UtcNow.Ticks
       let d1 = DateTime(t, DateTimeKind.Local)
       let d2 = DateTime(t, DateTimeKind.Utc)
       equal d1.Ticks d2.Ticks

    testCase "DateTime <-> Ticks isomorphism" <| fun () ->
       let checkIsomorphism (d: DateTime) =
            let ticks = d.Ticks
            let kind = d.Kind
            let fromTicks = DateTime ticks
            let fromTicksWithKind = DateTime (ticks, kind)

            equal d fromTicks
            equal ticks fromTicks.Ticks
            equal d fromTicksWithKind
            equal ticks fromTicksWithKind.Ticks
            equal kind fromTicksWithKind.Kind

            equal d.Ticks (DateTime d.Ticks).Ticks

    //    checkIsomorphism DateTime.MinValue
    //    checkIsomorphism DateTime.MaxValue
       checkIsomorphism DateTime.Now
       checkIsomorphism <| DateTime(2014, 10, 9)
       checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30)
       checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500)

    // FIXME: Isomorphims doesn't work for UTC dates
    //    checkIsomorphism DateTime.UtcNow
    //    checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
    //    checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)

    testCase "DateTime.IsLeapYear works" <| fun () ->
       DateTime.IsLeapYear(2014) |> equal false
       DateTime.IsLeapYear(2016) |> equal true

   // TODO: IsDaylightSavingTime
    // testCase "DateTime.IsDaylightSavingTime works" <| fun () ->
    //    let d1 = DateTime(2017, 7, 18, 2, 0, 0)
    //    let d2 = DateTime(2017, 12, 18, 2, 0, 0)
    //    d1.IsDaylightSavingTime() |> equal true
    //    d2.IsDaylightSavingTime() |> equal false

    testCase "DateTime.DaysInMonth works" <| fun () ->
       DateTime.DaysInMonth(2014, 1) |> equal 31
       DateTime.DaysInMonth(2014, 2) |> equal 28
       DateTime.DaysInMonth(2014, 4) |> equal 30
       DateTime.DaysInMonth(2016, 2) |> equal 29

    testCase "DateTime.Now works" <| fun () ->
       let d = DateTime.Now
       d > DateTime.MinValue |> equal true

    testCase "DateTime.UtcNow works" <| fun () ->
       let d = DateTime.UtcNow
       d > DateTime.MinValue |> equal true

    testCase "DateTime.Parse works" <| fun () ->
       let d = DateTime.Parse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture)
       d.Year + d.Month + d.Day + d.Hour + d.Minute
       |> equal 2096

    testCase "DateTime.Parse with time-only string works" <| fun () -> // See #1045
       let d = DateTime.Parse("13:50:34", CultureInfo.InvariantCulture)
       d.Hour + d.Minute + d.Second |> equal 97
       let d = DateTime.Parse("1:5:34 AM", CultureInfo.InvariantCulture)
       d.Hour + d.Minute + d.Second |> equal 40
       let d = DateTime.Parse("1:5:34 PM", CultureInfo.InvariantCulture)
       d.Hour + d.Minute + d.Second |> equal 52

    testCase "DateTime.TryParse works" <| fun () ->
        let f (d: string) =
            match DateTime.TryParse(d, CultureInfo.InvariantCulture, DateTimeStyles.None) with
            | true, _ -> true
            | false, _ -> false
        f "foo" |> equal false
        f "9/10/2014 1:50:34 PM" |> equal true
        f "1:50:34" |> equal true

    testCase "Parsing doesn't succeed for invalid dates" <| fun () ->
        let invalidAmericanDate = "13/1/2020"
        let r, _date = DateTime.TryParse(invalidAmericanDate, CultureInfo.InvariantCulture, DateTimeStyles.None)
        r |> equal false

    testCase "DateTime.Today works" <| fun () ->
       let d = DateTime.Today
       equal 0 d.Hour

    testCase "DateTime.ToUniversalTime works" <| fun () ->
       let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
       d.ToUniversalTime().Kind <> d.Kind
       |> equal true

    testCase "DateTime.SpecifyKind works" <| fun () -> // See #1844
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d2 = DateTime.SpecifyKind(d, DateTimeKind.Utc)
        d2.Kind |> equal DateTimeKind.Utc
        d.Ticks = d2.Ticks |> equal true
        // let d3 = d.ToUniversalTime()
        // d.Ticks = d3.Ticks |> equal false

    testCase "DateTime.Date works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30)
        d.Date.Hour |> equal 0
        d.Date.Day |> equal 9

    testCase "DateTime.Day works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        d.Day + d'.Day |> equal 18

    testCase "DateTime.DayOfWeek works" <| fun () ->
        let d = DateTime(2014, 10, 9)
        d.DayOfWeek |> equal DayOfWeek.Thursday

    testCase "DateTime.DayOfYear works" <| fun () ->
        let d = DateTime(2014, 10, 9)
        d.DayOfYear |> equal 282

    testCase "DateTime.Millisecond works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
        d.Millisecond |> equal 999

    // FIXME
    // testCase "DateTime.Ticks works" <| fun () ->
    //     let d = DateTime(2014, 10, 9, 13, 23, 30, 999, DateTimeKind.Utc)
    //     d.Ticks |> equal 635484578109990000L
    //     let d = DateTime(2014, 10, 9, 13, 23, 30, 999, DateTimeKind.Local)
    //     d.Ticks |> equal 635484578109990000L
    //     let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
    //     d.Ticks |> equal 635484578109990000L

    testCase "DateTime.Minute works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        d.Minute + d'.Minute
        |> equal 46

    testCase "DateTime.Month works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        d.Month + d'.Month
        |> equal 20

    testCase "DateTime.Second works" <| fun () ->
        let d = DateTime(2014,9,12,0,0,30)
        let d' = DateTime(2014,9,12,0,0,59)
        d.Second + d'.Second
        |> equal 89

    testCase "DateTime.Year works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        d.Year + d'.Year
        |> equal 4028

    testCase "DateTime.AddYears works" <| fun () ->
        let test v expected =
            let dt = DateTime(2016,2,29,0,0,0,DateTimeKind.Utc).AddYears(v)
            equal expected (dt.Month + dt.Day)
        test 100 31
        test 1 30
        test -1 30
        test -100 31
        test 0 31

    testCase "DateTime.AddMonths works" <| fun () ->
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

    testCase "DateTime.AddDays works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddDays(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 30585600.0
        test -100. 13305600.0
        test 0. 21945600.0

    // In regions with daylight saving time, 20/10/2019 will have different timezone
    // offset than 29/10/2019
    testCase "Adding days to a local date works even if daylight saving time changes" <| fun () ->
        let dt = DateTime(2019, 10, 20, 0, 0, 0, DateTimeKind.Local)
        dt.AddDays(9.).Day |> equal 29

    testCase "DateTime.AddHours works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddHours(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 22305600.0
        test -100. 21585600.0
        test 0. 21945600.0

    testCase "DateTime.AddMinutes works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddMinutes(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 21951600.0
        test -100. 21939600.0
        test 0. 21945600.0

    testCase "DateTime.AddSeconds works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddSeconds(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 21945700.0
        test -100. 21945500.0
        test 0. 21945600.0

    testCase "DateTime.AddMilliseconds works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddMilliseconds(v)
            thatYearMilliseconds dt
            |> equal expected
        test 100. 2.19456001e+10
        test -100. 2.19455999e+10
        test 0. 2.19456e+10

    // NOTE: Doesn't work for values between 10L (TimeSpan.TicksPerMicrosecond) and -10L, except 0L
    testCase "DateTime.AddTicks works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddTicks(v)
            dt.Ticks
            |> equal expected
        let ticks = 635460768000000000L
        test 100000L (ticks + 100000L)
        test -100000L (ticks - 100000L)
        test 0L ticks

    testCase "DateTime Addition works" <| fun () ->
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

    testCase "DateTime Subtraction with TimeSpan works" <| fun () ->
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

    testCase "DateTime Subtraction with DateTime works" <| fun () ->
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

    testCase "DateTime Comparison works" <| fun () ->
        let test ms expected =
            let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
            let dt2 = dt1.AddMilliseconds(ms)
            let res1 = compare dt1 dt2
            let res2 = dt1.CompareTo(dt2)
            let res3 = DateTime.Compare(dt1, dt2)
            equal true (res1 = res2 && res2 = res3)
            equal expected res1
        test 1000. -1
        test -1000. 1
        test 0. 0

    testCase "DateTime GreaterThan works" <| fun () ->
        let test ms expected =
            let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 > dt2 |> equal expected
        test 1000. false
        test -1000. true
        test 0. false

    testCase "DateTime LessThan works" <| fun () ->
        let test ms expected =
            let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 < dt2 |> equal expected
        test 1000. true
        test -1000. false
        test 0. false

    testCase "DateTime Equality works" <| fun () ->
       let test ms expected =
           let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
           let dt2 = dt1.AddMilliseconds(ms)
           dt1 = dt2 |> equal expected
       test 1000. false
       test -1000. false
       test 0. true

    testCase "DateTime Inequality works" <| fun () ->
       let test ms expected =
           let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
           let dt2 = dt1.AddMilliseconds(ms)
           dt1 <> dt2 |> equal expected
       test 1000. true
       test -1000. true
       test 0. false

    testCase "DateTime TimeOfDay works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, 1, DateTimeKind.Utc)
        let t = d.TimeOfDay

        t |> equal (TimeSpan(0, 13, 23, 30, 1))

//    testCaseAsync "Timer with AutoReset = true works" <| fun () ->
//        async {
//            let res = ref 0
//            let t = new Timers.Timer(50.)
//            t.Elapsed.Add(fun ev -> res := !res + 5)
//            t.Start()
//            do! Async.Sleep 125
//            t.Stop()
//            do! Async.Sleep 50
//            equal 10 !res
//        }
//
//    testCaseAsync "Timer with AutoReset = false works" <| fun () ->
//        async {
//            let res = ref 0
//            let t = new Timers.Timer()
//            t.Elapsed.Add(fun ev -> res := !res + 5)
//            t.AutoReset <- false
//            t.Interval <- 25.
//            t.Enabled <- true
//            do! Async.Sleep 100
//            equal 5 !res
//        }
//
//    testCaseAsync "Timer.Elapsed.Subscribe works" <| fun () ->
//        async {
//            let res = ref 0
//            let t = new Timers.Timer(50.)
//            let disp = t.Elapsed.Subscribe(fun ev -> res := !res + 5)
//            t.Start()
//            do! Async.Sleep 125
//            disp.Dispose()
//            do! Async.Sleep 50
//            equal 10 !res
//            t.Stop()
//        }
//
//    testCaseAsync "Assigning an event to a variable works" <| fun () -> // See #1863
//        let createTimerAndObservable timerInterval =
//            // setup a timer
//            let timer = new System.Timers.Timer(float timerInterval)
//            timer.AutoReset <- true
//            // events are automatically IObservable
//            let observable = timer.Elapsed
//            // return an async task
//            let task = async {
//                timer.Start()
//                do! Async.Sleep 200
//                timer.Stop()
//            }
//            // return a async task and the observable
//            (task,observable)
//        // create the timer and the corresponding observable
//        let basicTimer2 , timerEventStream = createTimerAndObservable 50
//
//        let mutable acc = 1
//        // register that everytime something happens on the
//        // event stream, print the time.
//        timerEventStream |> Observable.subscribe (fun _ ->
//            acc <- acc + 1) |>ignore
//
//        async {
//            do! basicTimer2
//            // printfn "%i" acc
//            acc > 2 |> equal true
//        }
//
