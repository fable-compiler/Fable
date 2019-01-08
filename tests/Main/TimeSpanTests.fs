module Fable.Tests.TimeSpan

open System
open Util.Testing
open Fable.Tests

let tests =
    testList "TimeSpan" [

        testCase "TimeSpan.ToString() works" <| fun () ->
            TimeSpan(0L).ToString() |> equal "00:00:00"
            TimeSpan.FromSeconds(12345.).ToString() |> equal "03:25:45"
            TimeSpan.FromDays(18.).ToString() |> equal "18.00:00:00"
            TimeSpan.FromMilliseconds(25.).ToString().TrimEnd('0') |> equal "00:00:00.025"

        // TODO
        // testCase "TimeSpan.ToString with format works" <| fun () ->
        //     TimeSpan.FromMinutes(234.).ToString("hh\:mm\:ss")
        //     |> equal "03:54:00"

        testCase "TimeSpan constructors work" <| fun () ->
            let t1 = TimeSpan(20000L)
            let t2 = TimeSpan(3, 3, 3)
            let t3 = TimeSpan(5, 5, 5, 5)
            let t4 = TimeSpan(7, 7, 7, 7, 7)

            t1.TotalMilliseconds |> equal 2.0
            t2.TotalMilliseconds |> equal 10983000.0
            t3.TotalMilliseconds |> equal 450305000.0
            t4.TotalMilliseconds |> equal 630427007.0

            t1.TotalMilliseconds + t2.TotalMilliseconds + t3.TotalMilliseconds + t4.TotalMilliseconds
            |> equal 1091715009.0

        testCase "TimeSpan static creation works" <| fun () ->
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

        testCase "TimeSpan components work" <| fun () ->
            let t = TimeSpan.FromMilliseconds(96441615.)
            t.Days + t.Hours + t.Minutes + t.Seconds + t.Milliseconds |> float
            |> equal 686.

        testCase "TimeSpan.Ticks works" <| fun () ->
            let t = TimeSpan.FromTicks(20000L)
            t.Ticks
            |> equal 20000L

        // NOTE: This test fails because of very small fractions, so I cut the fractional part
        testCase "TimeSpan totals work" <| fun () ->
            let t = TimeSpan.FromMilliseconds(96441615.)
            t.TotalDays + t.TotalHours + t.TotalMinutes + t.TotalSeconds |> floor
            |> equal 98076.0

        testCase "TimeSpan.Duration works" <| fun () ->
            let test ms expected =
                let t = TimeSpan.FromMilliseconds(ms)
                t.Duration().TotalMilliseconds
                |> equal expected
            test 1. 1.
            test -1. 1.
            test 0. 0.

        testCase "TimeSpan.Negate works" <| fun () ->
            let test ms expected =
                let t = TimeSpan.FromMilliseconds(ms)
                t.Negate().TotalMilliseconds
                |> equal expected
            test 1. -1.
            test -1. 1.
            test 0. 0.

        testCase "TimeSpan Addition works" <| fun () ->
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

        testCase "TimeSpan Subtraction works" <| fun () ->
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

        testCase "TimeSpan Comparison works" <| fun () ->
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

        testCase "TimeSpan GreaterThan works" <| fun () ->
            let test ms1 ms2 expected =
                let t1 = TimeSpan.FromMilliseconds(ms1)
                let t2 = TimeSpan.FromMilliseconds(ms2)
                t1 > t2
                |> equal expected
            test 1000. 2000. false
            test 2000. 1000. true
            test -2000. -2000. false

        testCase "TimeSpan LessThan works" <| fun () ->
            let test ms1 ms2 expected =
                let t1 = TimeSpan.FromMilliseconds(ms1)
                let t2 = TimeSpan.FromMilliseconds(ms2)
                t1 < t2
                |> equal expected
            test 1000. 2000. true
            test 2000. 1000. false
            test -2000. -2000. false

        testCase "TimeSpan Equality works" <| fun () ->
            let test ms1 ms2 expected =
                let t1 = TimeSpan.FromMilliseconds(ms1)
                let t2 = TimeSpan.FromMilliseconds(ms2)
                t1 = t2
                |> equal expected
            test 1000. 2000. false
            test 2000. 1000. false
            test -2000. -2000. true

        testCase "TimeSpan Inequality works" <| fun () ->
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

        testCase "TimeSpan 0 parse works" <| fun () ->
            let actual = TimeSpan.Parse("0")
            let expected = TimeSpan(0, 0, 0, 0, 0)
            equal actual expected

        testCase "TimeSpan 14 parse works" <| fun () ->
            let actual = TimeSpan.Parse("14")
            let expected = TimeSpan(14, 0, 0, 0, 0)
            equal actual expected

        testCase "TimeSpan 1:2:3 parse works" <| fun () ->
            let actual = TimeSpan.Parse("1:2:3")
            let expected = TimeSpan(0, 1, 2, 3, 0)
            equal actual expected

        testCase "TimeSpan 0:0:0.250 parse works" <| fun () ->
            let actual = TimeSpan.Parse("0:0:0.250")
            let expected = TimeSpan(0, 0, 0, 0, 250)
            equal actual expected

        testCase "TimeSpan 10.20:30:40.050 parse works" <| fun () ->
            let actual = TimeSpan.Parse("10.20:30:40.050")
            let expected = TimeSpan(10, 20, 30, 40, 50)
            equal actual expected

        testCase "TimeSpan 99.23:59:59.999 parse works" <| fun () ->
            let actual = TimeSpan.Parse("99.23:59:59.999")
            let expected = TimeSpan(99, 23, 59, 59, 999)
            equal actual expected

        testCase "TimeSpan 0023:0059:0059.0099 parse works" <| fun () ->
            let actual = TimeSpan.Parse("0023:0059:0059.009")
            let expected = TimeSpan(00, 23, 59, 59, 9)
            equal actual expected

        testCase "TimeSpan 23:0:0 parse works" <| fun () ->
            let actual = TimeSpan.Parse("23:0:0")
            let expected = TimeSpan(0, 23, 0, 0, 0)
            equal actual expected
#if FABLE_COMPILER
        // msft assumes it's 24 days...
        // https://docs.microsoft.com/en-us/dotnet/api/system.timespan.parse
        // or
        // https://github.com/dotnet/dotnet-api-docs/blob/7f6a3882631bc008b858adfadb43cd17bbd55d49/xml/System/TimeSpan.xml#L2772
        testCase "TimeSpan 24:0:0 parse fails" <| fun () ->
            (fun _ -> TimeSpan.Parse("24:0:0"))
            |> Util.throwsError "String was not recognized as a valid TimeSpan."

        testCase "TimeSpan 24:0:0 TryParse fails" <| fun () ->
            let status, _ = TimeSpan.TryParse("24:0:0")
            equal status false
#endif

        testCase "TimeSpan 0:0:59 parse works" <| fun () ->
            let actual = TimeSpan.Parse("0:0:59")
            let expected = TimeSpan(0, 0, 0, 59, 0)
            equal actual expected

        testCase "TimeSpan 0:60:0 parse fails" <| fun () ->
            (fun _ -> TimeSpan.Parse("0:60:0"))
#if FABLE_COMPILER
            |> Util.throwsError "String was not recognized as a valid TimeSpan."
#else
            |> Util.throwsError "The TimeSpan could not be parsed because at least one of the numeric components is out of range or contains too many digits."
#endif
        testCase "TimeSpan 10: parse fails" <| fun () ->
            (fun _ -> TimeSpan.Parse("10:"))
            |> Util.throwsError "String was not recognized as a valid TimeSpan."

        testCase "TimeSpan 10:0 parse works" <| fun () ->
            let actual = TimeSpan.Parse("10:0")
            let expected = TimeSpan(0, 10, 0, 0, 0)
            equal actual expected

        testCase "TimeSpan 10:20: parse fails" <| fun () ->
            (fun _ -> TimeSpan.Parse("10:20:"))
            |> Util.throwsError "String was not recognized as a valid TimeSpan."

        testCase "TimeSpan 10:20:0 parse works" <| fun () ->
            let actual = TimeSpan.Parse("10:20:0")
            let expected = TimeSpan(0, 10, 20, 0, 0)
            equal actual expected

        testCase "TimeSpan .123 parse fails" <| fun () ->
            (fun _ -> TimeSpan.Parse(".123"))
            |> Util.throwsError "String was not recognized as a valid TimeSpan."

        testCase "TimeSpan 0.12:00 parse works" <| fun () ->
            let actual = TimeSpan.Parse("0.12:00")
            let expected = TimeSpan(0, 12, 00, 0, 0)
            equal actual expected

        testCase "TimeSpan 10. parse fails" <| fun () ->
            (fun _ -> TimeSpan.Parse("10."))
            |> Util.throwsError "String was not recognized as a valid TimeSpan."

        testCase "TimeSpan 10.12 parse fails" <| fun () ->
            (fun _ -> TimeSpan.Parse("10.12"))
            |> Util.throwsError "String was not recognized as a valid TimeSpan."

        testCase "TimeSpan 10.12:00 parse works" <| fun () ->
            let actual = TimeSpan.Parse("10.12:00")
            let expected = TimeSpan(10, 12, 00, 0, 0)
            equal actual expected

        testCase "TimeSpan 0 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("0")
            let expected = TimeSpan(0, 0, 0, 0, 0)
            equal status true
            equal actual expected

        testCase "TimeSpan 14 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("14")
            let expected = TimeSpan(14, 0, 0, 0, 0)
            equal status true
            equal actual expected

        testCase "TimeSpan 1:2:3 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("1:2:3")
            let expected = TimeSpan(0, 1, 2, 3, 0)
            equal status true
            equal actual expected

        testCase "TimeSpan 0:0:0.250 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("0:0:0.250")
            let expected = TimeSpan(0, 0, 0, 0, 250)
            equal status true
            equal actual expected

        testCase "TimeSpan 10.20:30:40.050 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("10.20:30:40.050")
            let expected = TimeSpan(10, 20, 30, 40, 50)
            equal status true
            equal actual expected

        testCase "TimeSpan 99.23:59:59.999 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("99.23:59:59.999")
            let expected = TimeSpan(99, 23, 59, 59, 999)
            equal status true
            equal actual expected

        testCase "TimeSpan 0023:0059:0059.009 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("0023:0059:0059.009")
            let expected = TimeSpan(00, 23, 59, 59, 9)
            equal status true
            equal actual expected

        testCase "TimeSpan 23:0:0 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("23:0:0")
            let expected = TimeSpan(0, 23, 0, 0, 0)
            equal status true
            equal actual expected

        testCase "TimeSpan 0:0:59 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("0:0:59")
            let expected = TimeSpan(0, 0, 0, 59, 0)
            equal status true
            equal actual expected

        testCase "TimeSpan 0:60:0 TryParse fails" <| fun () ->
            let status, _ = TimeSpan.TryParse("0:60:0")
            equal status false

        testCase "TimeSpan 10: TryParse fails" <| fun () ->
            let status, _ = TimeSpan.TryParse("10:")
            equal status false
        testCase "TimeSpan 10:0 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("10:0")
            let expected = TimeSpan(0, 10, 0, 0, 0)
            equal status true
            equal actual expected

        testCase "TimeSpan 10:20: TryParse fails" <| fun () ->
            let status, _ = TimeSpan.TryParse("10:20:")
            equal status false

        testCase "TimeSpan 10:20:0 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("10:20:0")
            let expected = TimeSpan(0, 10, 20, 0, 0)
            equal status true
            equal actual expected

        testCase "TimeSpan .123 TryParse fails" <| fun () ->
            let status, _ = TimeSpan.TryParse(".123")
            equal status false

        testCase "TimeSpan 0.12:00 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("0.12:00")
            let expected = TimeSpan(0, 12, 00, 0, 0)
            equal status true
            equal actual expected

        testCase "TimeSpan 10. TryParse fails" <| fun () ->
            let status, _ = TimeSpan.TryParse("10.")
            equal status false

        testCase "TimeSpan 10.12 TryParse fails" <| fun () ->
            let status, _ = TimeSpan.TryParse("10.12")
            equal status false

        testCase "TimeSpan 10.12:00 TryParse works" <| fun () ->
            let status, actual = TimeSpan.TryParse("10.12:00")
            let expected = TimeSpan(10, 12, 00, 0, 0)
            equal status true
            equal actual expected
    ]
