module Fable.Tests.TimeSpanTests

open System
open System.Globalization
open Util.Testing

[<Fact>]
let ``Timespan static fields work`` () =
    TimeSpan.Zero.Ticks |> equal 0L
    TimeSpan.MinValue.Ticks |> equal System.Int64.MinValue
    TimeSpan.MaxValue.Ticks |> equal System.Int64.MaxValue
    TimeSpan.TicksPerMillisecond |> equal 10000L
    TimeSpan.TicksPerSecond |> equal (10000L * 1000L)
    TimeSpan.TicksPerMinute |> equal (10000L * 1000L * 60L)
    TimeSpan.TicksPerHour |> equal (10000L * 1000L * 60L * 60L)
    TimeSpan.TicksPerDay |> equal (10000L * 1000L * 60L * 60L* 24L)

[<Fact>]
let ``TimeSpan.ToString() works`` () =
    TimeSpan(0L).ToString() |> equal "00:00:00"
    TimeSpan.FromSeconds(12345.).ToString() |> equal "03:25:45"
    TimeSpan.FromDays(18.).ToString() |> equal "18.00:00:00"
    TimeSpan.FromMilliseconds(25.).ToString() |> equal "00:00:00.0250000"

[<Fact>]
let ``TimeSpan.ToString() works for negative TimeSpan`` () =
    TimeSpan.FromSeconds(-5.).ToString() |> equal "-00:00:05"
    TimeSpan.FromDays(-5.23).ToString() |> equal "-5.05:31:12"

[<Fact>]
let ``TimeSpan.ToString(\"c\", CultureInfo.InvariantCulture) works`` () =
    TimeSpan(0L).ToString("c", CultureInfo.InvariantCulture) |> equal "00:00:00"
    TimeSpan.FromSeconds(12345.).ToString("c", CultureInfo.InvariantCulture) |> equal "03:25:45"
    TimeSpan.FromDays(18.).ToString("c", CultureInfo.InvariantCulture) |> equal "18.00:00:00"
    TimeSpan.FromMilliseconds(25.).ToString("c", CultureInfo.InvariantCulture) |> equal "00:00:00.0250000"

// [<Fact>]
// let ``TimeSpan.ToString(\"g\", CultureInfo.InvariantCulture) works`` () =
//     TimeSpan(0L).ToString("g", CultureInfo.InvariantCulture) |> equal "0:00:00"
//     TimeSpan.FromSeconds(12345.).ToString("g", CultureInfo.InvariantCulture) |> equal "3:25:45"
//     TimeSpan.FromDays(18.).ToString("g", CultureInfo.InvariantCulture) |> equal "18:0:00:00"
//     TimeSpan.FromMilliseconds(25.).ToString("g", CultureInfo.InvariantCulture) |> equal "0:00:00.025"

// [<Fact>]
// let ``TimeSpan.ToString(\"G\", CultureInfo.InvariantCulture) works`` () =
//     TimeSpan(0L).ToString("G", CultureInfo.InvariantCulture) |> equal "0:00:00:00.0000000"
//     TimeSpan.FromSeconds(12345.).ToString("G", CultureInfo.InvariantCulture) |> equal "0:03:25:45.0000000"
//     TimeSpan.FromDays(18.).ToString("G", CultureInfo.InvariantCulture) |> equal "18:00:00:00.0000000"
//     TimeSpan.FromMilliseconds(25.).ToString("G", CultureInfo.InvariantCulture) |> equal "0:00:00:00.0250000"

// [<Fact>]
// let ``TimeSpan.ToString with custom format works`` () =
//     TimeSpan.FromMinutes(234.).ToString("hh\:mm\:ss")
//     |> equal "03:54:00"

[<Fact>]
let ``TimeSpan constructors work`` () =
    let t1 = TimeSpan(20000L)
    let t2 = TimeSpan(3, 3, 3)
    let t3 = TimeSpan(5, 5, 5, 5)
    let t4 = TimeSpan(7, 7, 7, 7, 7)
    let t5 = TimeSpan(-2,0,0,0)

    t1.TotalMilliseconds |> equal 2.0
    t2.TotalMilliseconds |> equal 10983000.0
    t3.TotalMilliseconds |> equal 450305000.0
    t4.TotalMilliseconds |> equal 630427007.0
    t5.TotalMilliseconds |> equal -172800000.0

    t1.TotalMilliseconds + t2.TotalMilliseconds + t3.TotalMilliseconds + t4.TotalMilliseconds
    |> equal 1091715009.0

[<Fact>]
let ``TimeSpan static creation works`` () =
    let t1 = TimeSpan.FromTicks(20000L)
    let t2 = TimeSpan.FromMilliseconds(2.)
    let t3 = TimeSpan.FromDays   (2.)
    let t4 = TimeSpan.FromHours  (2.)
    let t5 = TimeSpan.FromMinutes(2.)
    let t6 = TimeSpan.FromSeconds(2.)
    let t7 = TimeSpan.Zero
    t1.TotalMilliseconds + t2.TotalMilliseconds + t3.TotalMilliseconds + t4.TotalMilliseconds +
        t5.TotalMilliseconds + t6.TotalMilliseconds + t7.TotalMilliseconds
    |> equal 180122004.0

[<Fact>]
let ``TimeSpan components work`` () =
    let t = TimeSpan.FromMilliseconds(96441615.)
    t.Days + t.Hours + t.Minutes + t.Seconds + t.Milliseconds |> float
    |> equal 686.

[<Fact>]
let ``TimeSpan.Ticks works`` () =
    let t = TimeSpan.FromTicks(20000L)
    t.Ticks |> equal 20000L

[<Fact>]
let ``TimeSpan totals work`` () =
    let t = TimeSpan.FromMilliseconds(96441615.)
    t.TotalDays + t.TotalHours + t.TotalMinutes + t.TotalSeconds |> floor
    |> equal 98076.0

[<Fact>]
let ``TimeSpan.Duration works`` () =
    let test ms expected =
        let t = TimeSpan.FromMilliseconds(ms)
        t.Duration().TotalMilliseconds
        |> equal expected
    test 1. 1.
    test -1. 1.
    test 0. 0.

[<Fact>]
let ``TimeSpan.Negate works`` () =
    let test ms expected =
        let t = TimeSpan.FromMilliseconds(ms)
        t.Negate().TotalMilliseconds
        |> equal expected
    test 1. -1.
    test -1. 1.
    test 0. 0.

[<Fact>]
let ``TimeSpan Addition works`` () =
    let test ms1 ms2 expected =
        let t1 = TimeSpan.FromMilliseconds(ms1)
        let t2 = TimeSpan.FromMilliseconds(ms2)
        let res1 = t1.Add(t2).TotalMilliseconds
        let res2 = (t1 + t2).TotalMilliseconds
        equal true (res1 = res2)
        equal expected res1
    test 1000. 2000. 3000.
    test 200. -1000. -800.
    test -2000. 1000. -1000.
    test -200. -300. -500.
    test 0. 1000. 1000.
    test -2000. 0. -2000.
    test 0. 0. 0.

[<Fact>]
let ``TimeSpan Subtraction works`` () =
    let test ms1 ms2 expected =
        let t1 = TimeSpan.FromMilliseconds(ms1)
        let t2 = TimeSpan.FromMilliseconds(ms2)
        let res1 = t1.Subtract(t2).TotalMilliseconds
        let res2 = (t1 - t2).TotalMilliseconds
        equal true (res1 = res2)
        equal expected res1
    test 1000. 2000. -1000.
    test 200. -2000. 2200.
    test -2000. 1000. -3000.
    test 200. -300. 500.
    test 0. 1000. -1000.
    test 1000. 1000. 0.
    test 0. 0. 0.

[<Fact>]
let ``TimeSpan Multiplication works`` () =
    let test ms factor expected =
            // sprintf "(%f, %f) = %f" ms factor expected
            let t = TimeSpan.FromMilliseconds(ms)
            let res1 = t.Multiply(factor).TotalMilliseconds
            let res2 = (t * factor).TotalMilliseconds
            equal res1 res2
            equal true (res1 = res2)
            equal expected res1
    test 1000. -1. -1000.
    test 200. 11. 2200.
    test -2000. 1.5 -3000.
    test 0. 1000. 0.
    test 1000. 0. 0.
    test 0. 0. 0.

[<Fact>]
let ``TimeSpan Division works`` () =
    // Note: there are two overloads, one with TimeSpan, one with float
    let test_ts ms1 ms2 expected =
        // sprintf "ts(%f, %f) = %f" ms1 ms2 expected
        let t1 = TimeSpan.FromMilliseconds(ms1)
        let t2 = TimeSpan.FromMilliseconds(ms2)
        let res1 = t1.Divide(t2)
        let res2 = (t1 / t2)
        equal res1 res2
        equal true (res1 = res2)
        equal expected res1
    let test_float ms (factor: float) expected =
        // sprintf "float(%f, %f) = %f" ms factor expected
        let t = TimeSpan.FromMilliseconds(ms)
        let res1 = t.Divide(factor).TotalMilliseconds
        let res2 = (t / factor).TotalMilliseconds
        equal res1 res2
        equal true (res1 = res2)
        equal expected res1

    test_ts 1000. -1. -1000.
    test_ts 2200. 11. 200.
    test_ts -3000. 1.5 -2000.
    test_ts 0. 1000. 0.

    test_float 1000. -1. -1000.
    test_float 2200. 11. 200.
    test_float -3000. 1.5 -2000.
    test_float 0. 1000. 0.

[<Fact>]
let ``TimeSpan Comparison works`` () =
    let test ms1 ms2 expected =
        let t1 = TimeSpan.FromMilliseconds(ms1)
        let t2 = TimeSpan.FromMilliseconds(ms2)
        let res1 = compare t1 t2
        let res2 = t1.CompareTo(t2)
        let res3 = TimeSpan.Compare(t1, t2)
        equal true (res1 = res2 && res2 = res3)
        equal expected res1
    test 1000. 2000. -1
    test 2000. 1000. 1
    test -2000. -2000. 0
    test 200. -200. 1
    test 0. 1000. -1
    test 1000. 1000. 0
    test 0. 0. 0

[<Fact>]
let ``TimeSpan GreaterThan works`` () =
    let test ms1 ms2 expected =
        let t1 = TimeSpan.FromMilliseconds(ms1)
        let t2 = TimeSpan.FromMilliseconds(ms2)
        t1 > t2
        |> equal expected
    test 1000. 2000. false
    test 2000. 1000. true
    test -2000. -2000. false

[<Fact>]
let ``TimeSpan LessThan works`` () =
    let test ms1 ms2 expected =
        let t1 = TimeSpan.FromMilliseconds(ms1)
        let t2 = TimeSpan.FromMilliseconds(ms2)
        t1 < t2
        |> equal expected
    test 1000. 2000. true
    test 2000. 1000. false
    test -2000. -2000. false

[<Fact>]
let ``TimeSpan Equality works`` () =
    let test ms1 ms2 expected =
        let t1 = TimeSpan.FromMilliseconds(ms1)
        let t2 = TimeSpan.FromMilliseconds(ms2)
        t1 = t2
        |> equal expected
    test 1000. 2000. false
    test 2000. 1000. false
    test -2000. -2000. true

[<Fact>]
let ``TimeSpan Inequality works`` () =
    let test ms1 ms2 expected =
        let t1 = TimeSpan.FromMilliseconds(ms1)
        let t2 = TimeSpan.FromMilliseconds(ms2)
        t1 <> t2
        |> equal expected
    test 1000. 2000. true
    test 2000. 1000. true
    test -2000. -2000. false

// **************************************************
// Test Cases From :
// https://docs.microsoft.com/en-us/dotnet/api/system.timespan.tryparse
//
// **************************************************
//
//            String to Parse                TimeSpan
//            ---------------   ---------------------
//                          0        00:00:00
//                         14     14.00:00:00
//                      1:2:3        01:02:03
//                  0:0:0.250        00:00:00.2500000
//            10.20:30:40.050     10.20:30:40.0050000
//        99.23:59:59.9990000     99.23:59:59.9999000
//        0023:0059:0059.0009        23:59:59.0009000
//                     23:0:0        23:00:00
//                     24:0:0   Parse operation failed. (actually not true, it's parsed to 24 days...)
//                     0:59:0        00:59:00
//                     0:60:0   Parse operation failed.
//                     0:0:59        00:00:59
//                     0:0:60   Parse operation failed.
//                        10:   Parse operation failed.
//                       10:0        10:00:00
//                        :10   Parse operation failed.
//                       0:10        00:10:00
//                     10:20:   Parse operation failed.
//                    10:20:0        10:20:00
//                       .123   Parse operation failed.
//                    0.12:00        12:00:00
//                        10.   Parse operation failed.
//                      10.12   Parse operation failed.
//                   10.12:00     10.12:00:00

[<Fact>]
let ``TimeSpan 0 parse works`` () =
    let actual = TimeSpan.Parse("0")
    let expected = TimeSpan(0, 0, 0, 0, 0)
    equal actual expected

[<Fact>]
let ``TimeSpan 14 parse works`` () =
    let actual = TimeSpan.Parse("14")
    let expected = TimeSpan(14, 0, 0, 0, 0)
    equal actual expected

[<Fact>]
let ``TimeSpan 1:2:3 parse works`` () =
    let actual = TimeSpan.Parse("1:2:3")
    let expected = TimeSpan(0, 1, 2, 3, 0)
    equal actual expected

[<Fact>]
let ``TimeSpan 0:0:0.250 parse works`` () =
    let actual = TimeSpan.Parse("0:0:0.250")
    let expected = TimeSpan(0, 0, 0, 0, 250)
    equal actual expected

[<Fact>]
let ``TimeSpan 10.20:30:40.050 parse works`` () =
    let actual = TimeSpan.Parse("10.20:30:40.050")
    let expected = TimeSpan(10, 20, 30, 40, 50)
    equal actual expected

[<Fact>]
let ``TimeSpan 99.23:59:59.999 parse works`` () =
    let actual = TimeSpan.Parse("99.23:59:59.999")
    let expected = TimeSpan(99, 23, 59, 59, 999)
    equal actual expected

[<Fact>]
let ``TimeSpan 0023:0059:0059.0099 parse works`` () =
    let actual = TimeSpan.Parse("0023:0059:0059.009")
    let expected = TimeSpan(00, 23, 59, 59, 9)
    equal actual expected

[<Fact>]
let ``TimeSpan 23:0:0 parse works`` () =
    let actual = TimeSpan.Parse("23:0:0")
    let expected = TimeSpan(0, 23, 0, 0, 0)
    equal actual expected

// msft assumes it's 24 days...
// https://docs.microsoft.com/en-us/dotnet/api/system.timespan.parse
// or
// https://github.com/dotnet/dotnet-api-docs/blob/7f6a3882631bc008b858adfadb43cd17bbd55d49/xml/System/TimeSpan.xml#L2772

// [<Fact>]
// let ``TimeSpan 24:0:0 parse fails`` () =
//     throwsAnyError (fun () -> TimeSpan.Parse("24:0:0"))
//     // |> throwsError "String '24:0:0' was not recognized as a valid TimeSpan."

// [<Fact>]
// let ``TimeSpan 24:0:0 TryParse fails`` () =
//     let status, _ = TimeSpan.TryParse("24:0:0")
//     equal status false

[<Fact>]
let ``TimeSpan 0:0:59 parse works`` () =
    let actual = TimeSpan.Parse("0:0:59")
    let expected = TimeSpan(0, 0, 0, 59, 0)
    equal actual expected

[<Fact>]
let ``TimeSpan 0:60:0 parse fails`` () =
    throwsAnyError (fun () -> TimeSpan.Parse("0:60:0"))
// #if FABLE_COMPILER
//     |> throwsError "String '0:60:0' was not recognized as a valid TimeSpan."
// #else
//     |> throwsError "The TimeSpan string '0:60:0' could not be parsed because at least one of the numeric components is out of range or contains too many digits."
// #endif

[<Fact>]
let ``TimeSpan 10: parse fails`` () =
    throwsAnyError (fun () -> TimeSpan.Parse("10:"))
    // |> throwsError "String '10:' was not recognized as a valid TimeSpan."

[<Fact>]
let ``TimeSpan 10:0 parse works`` () =
    let actual = TimeSpan.Parse("10:0")
    let expected = TimeSpan(0, 10, 0, 0, 0)
    equal actual expected

[<Fact>]
let ``TimeSpan 10:20: parse fails`` () =
    throwsAnyError (fun () -> TimeSpan.Parse("10:20:"))
    // |> throwsError "String '10:20:' was not recognized as a valid TimeSpan."

[<Fact>]
let ``TimeSpan 10:20:0 parse works`` () =
    let actual = TimeSpan.Parse("10:20:0")
    let expected = TimeSpan(0, 10, 20, 0, 0)
    equal actual expected

[<Fact>]
let ``TimeSpan .123 parse fails`` () =
    throwsAnyError (fun () -> TimeSpan.Parse(".123"))
    // |> throwsError "String '.123' was not recognized as a valid TimeSpan."

[<Fact>]
let ``TimeSpan 0.12:00 parse works`` () =
    let actual = TimeSpan.Parse("0.12:00")
    let expected = TimeSpan(0, 12, 00, 0, 0)
    equal actual expected

[<Fact>]
let ``TimeSpan 10. parse fails`` () =
    throwsAnyError (fun () -> TimeSpan.Parse("10."))
    // |> throwsError "String '10.' was not recognized as a valid TimeSpan."

[<Fact>]
let ``TimeSpan 10.12 parse fails`` () =
    throwsAnyError (fun () -> TimeSpan.Parse("10.12"))
    // |> throwsError "String '10.12' was not recognized as a valid TimeSpan."

[<Fact>]
let ``TimeSpan 10.12:00 parse works`` () =
    let actual = TimeSpan.Parse("10.12:00")
    let expected = TimeSpan(10, 12, 00, 0, 0)
    equal actual expected

[<Fact>]
let ``TimeSpan 0 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("0")
    let expected = TimeSpan(0, 0, 0, 0, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 14 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("14")
    let expected = TimeSpan(14, 0, 0, 0, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 1:2:3 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("1:2:3")
    let expected = TimeSpan(0, 1, 2, 3, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 0:0:0.250 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("0:0:0.250")
    let expected = TimeSpan(0, 0, 0, 0, 250)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 10.20:30:40.050 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("10.20:30:40.050")
    let expected = TimeSpan(10, 20, 30, 40, 50)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 99.23:59:59.999 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("99.23:59:59.999")
    let expected = TimeSpan(99, 23, 59, 59, 999)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 0023:0059:0059.009 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("0023:0059:0059.009")
    let expected = TimeSpan(00, 23, 59, 59, 9)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 23:0:0 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("23:0:0")
    let expected = TimeSpan(0, 23, 0, 0, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 0:0:59 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("0:0:59")
    let expected = TimeSpan(0, 0, 0, 59, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 0:60:0 TryParse fails`` () =
    let status, _ = TimeSpan.TryParse("0:60:0")
    equal status false

[<Fact>]
let ``TimeSpan 10: TryParse fails`` () =
    let status, _ = TimeSpan.TryParse("10:")
    equal status false

[<Fact>]
let ``TimeSpan 10:0 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("10:0")
    let expected = TimeSpan(0, 10, 0, 0, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 10:20: TryParse fails`` () =
    let status, _ = TimeSpan.TryParse("10:20:")
    equal status false

[<Fact>]
let ``TimeSpan 10:20:0 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("10:20:0")
    let expected = TimeSpan(0, 10, 20, 0, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan .123 TryParse fails`` () =
    let status, _ = TimeSpan.TryParse(".123")
    equal status false

[<Fact>]
let ``TimeSpan 0.12:00 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("0.12:00")
    let expected = TimeSpan(0, 12, 00, 0, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 10. TryParse fails`` () =
    let status, _ = TimeSpan.TryParse("10.")
    equal status false

[<Fact>]
let ``TimeSpan 10.12 TryParse fails`` () =
    let status, _ = TimeSpan.TryParse("10.12")
    equal status false

[<Fact>]
let ``TimeSpan 10.12:00 TryParse works`` () =
    let status, actual = TimeSpan.TryParse("10.12:00")
    let expected = TimeSpan(10, 12, 00, 0, 0)
    equal status true
    equal actual expected

[<Fact>]
let ``TimeSpan 00:00:00.1 Parse handle correctly the milliseconds`` () =
    let actual = TimeSpan.Parse("00:00:00.1").TotalMilliseconds
    let expected = 100.
    equal actual expected

[<Fact>]
let ``TimeSpan 00:00:00.12 Parse handle correctly the milliseconds`` () =
    let actual = TimeSpan.Parse("00:00:00.12").TotalMilliseconds
    let expected = 120.
    equal actual expected

[<Fact>]
let ``TimeSpan 00:00:00.123 Parse handle correctly the milliseconds`` () =
    let actual = TimeSpan.Parse("00:00:00.123").TotalMilliseconds
    let expected = 123.
    equal actual expected

[<Fact>]
let ``TimeSpan 00:00:00.1234 Parse handle correctly the milliseconds`` () =
    let actual = TimeSpan.Parse("00:00:00.1234").TotalMilliseconds
    let expected = 123.4
    equal actual expected

[<Fact>]
let ``TimeSpan 00:00:00.12345 Parse handle correctly the milliseconds`` () =
    let actual = TimeSpan.Parse("00:00:00.12345").TotalMilliseconds
    let expected = 123.45
    equal actual expected

[<Fact>]
let ``TimeSpan 00:00:00.123456 Parse handle correctly the milliseconds`` () =
    let actual = TimeSpan.Parse("00:00:00.123456").TotalMilliseconds
    let expected = 123.456
    equal actual expected

[<Fact>]
let ``TimeSpan 00:00:00.0034567 Parse handle correctly the milliseconds`` () =
    let actual = TimeSpan.Parse("00:00:00.0034567").TotalMilliseconds
    let expected = 3.4567
    equal actual expected

[<Fact>]
let ``TimeSpan Parse work with negative TimeSpan`` () =
    let actual = TimeSpan.Parse("-2:00:00")
    equal actual.TotalMilliseconds -7200000.0

[<Fact>]
let ``TimeSpan 1.23:45:06.789 TotalMilliseconds works`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").TotalMilliseconds
    let expected = 171906789.0
    equal actual expected

[<Fact>]
let ``TimeSpan 1.23:45:06.789 TotalSeconds works`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").TotalSeconds
    let expected = 171906.789
    equal actual expected

[<Fact>]
let ``TimeSpan 1.23:45:06.789 TotalMinutes works`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").TotalMinutes
    let expected = 2865.11315
    equal actual expected

[<Fact>]
let ``TimeSpan 1.23:45:06.789 TotalHours works`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").TotalHours
    let expected = 47.75188583333333
    equal actual expected

[<Fact>]
let ``TimeSpan 1.23:45:06.789 TotalDays works`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").TotalDays
    let expected = 1.9896619097222221
    equal actual expected

// TODO: This tests fails because of the decimal part of the milliseconds, see #1867
// [<Fact>]
// let ``TimeSpan TotalSeconds & friends work`` () =
//     let ts = TimeSpan.FromDays(0.005277777778)
//     ts.TotalMilliseconds |> equal 456000.0
//     ts.TotalSeconds |> equal 456.
//     ts.TotalMinutes |> equal 7.6

[<Fact>]
let ``TimeSpan.Milliseconds works with positive TimeSpan`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").Milliseconds
    let expected = 789
    equal actual expected

[<Fact>]
let ``TimeSpan.Milliseconds works with negative TimeSpan`` () =
    let actual = TimeSpan.Parse("-1.23:45:06.78999").Milliseconds
    let expected = -789
    equal actual expected

[<Fact>]
let ``TimeSpan.Seconds works with positive TimeSpan`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").Seconds
    let expected = 6
    equal actual expected

[<Fact>]
let ``TimeSpan.Seconds works with negative TimeSpan`` () =
    let actual = TimeSpan.Parse("-1.23:45:06.78999").Seconds
    let expected = -6
    equal actual expected

[<Fact>]
let ``TimeSpan.Minutes works with positive TimeSpan`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").Minutes
    let expected = 45
    equal actual expected

[<Fact>]
let ``TimeSpan.Minutes works with negative TimeSpan`` () =
    let actual = TimeSpan.Parse("-1.23:45:06.78999").Minutes
    let expected = -45
    equal actual expected

[<Fact>]
let ``TimeSpan.Hours works with positive TimeSpan`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").Hours
    let expected = 23
    equal actual expected

[<Fact>]
let ``TimeSpan.Hours works with negative TimeSpan`` () =
    let actual = TimeSpan.Parse("-1.23:45:06.78999").Hours
    let expected = -23
    equal actual expected

[<Fact>]
let ``TimeSpan.Days works with positive TimeSpan`` () =
    let actual = TimeSpan.Parse("1.23:45:06.789").Days
    let expected = 1
    equal actual expected

[<Fact>]
let ``TimeSpan.Days works with negative TimeSpan`` () =
    let actual = TimeSpan.Parse("-1.23:45:06.78999").Days
    let expected = -1
    equal actual expected
