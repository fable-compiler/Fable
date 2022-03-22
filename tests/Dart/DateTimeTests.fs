module Fable.Tests.Dart.DateTime

open System
open Util

let tests() =
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

//    // TODO: These two tests give different values for .NET and JS because DateTime
//    // becomes as a plain JS Date object, so I'm just checking the fields get translated
//    testCase "DateTime.MaxValue works" <| fun () ->
//        let d1 = DateTime.Now
//        let d2 = DateTime.MaxValue
//        d1 < d2 |> equal true
//
//    testCase "DateTime.MinValue works" <| fun () ->
//        let d1 = DateTime.Now
//        let d2 = DateTime.MinValue
//        d1 < d2 |> equal false
//
//    testCase "DateTime.MinValue works in pattern match" <| fun () ->
//        let d1 = Some DateTime.Now
//        match d1 with
//        | Some date when date <> DateTime.MinValue -> ()
//        | _ -> failwith "expected pattern match above"
//
//    testCase "DateTime.ToLocalTime works" <| fun () ->
//        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//        let d' = d.ToLocalTime()
//        d.Kind <> d'.Kind
//        |> equal true
//
//    testCase "Creating DateTimeOffset from DateTime and back works" <| fun () ->
//        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//        let dto = DateTimeOffset(d)
//        let d' = dto.DateTime
//
//        d' |> equal d
//
//    testCase "Formatting DateTimeOffset works" <| fun () ->
//        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//        let dto = DateTimeOffset(d)
//
//        // dto.ToString() |> equal "2014-10-09 13:23:30 +00:00"
//        dto.ToString("HH:mm:ss", CultureInfo.InvariantCulture) |> equal "13:23:30"
//
//    testCase "DateTime.Hour works" <| fun () ->
//        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
//        d.Hour |> equal 13
//
//    // TODO: These four tests don't match exactly between .NET and JS
//    // Think of a way to compare the results approximately
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
//
//    // TODO: Unfortunately, JS will happily create invalid dates like DateTime(2014,2,29)
//    //       But this problem also happens when parsing, so I haven't tried to fix it
//    testCase "DateTime constructors work" <| fun () ->
//        let d1 = DateTime(2014, 10, 9)
//        let d2 = DateTime(2014, 10, 9, 13, 23, 30)
//        let d3 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//        let d4 = DateTime(2014, 10, 9, 13, 23, 30, 500)
//        let d5 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
//        d1.Day + d2.Second + d3.Second + d4.Millisecond + d5.Millisecond
//        |> equal 1069
//
//    testCase "DateTime constructor from Ticks works" <| fun () ->
//        let d = DateTime(624059424000000000L, DateTimeKind.Utc)
//        equal 1978 d.Year
//        equal 7 d.Month
//        equal 27 d.Day
//        equal 0 d.Hour
//        equal 0 d.Minute
//
//        let d = DateTime(624059424000000000L, DateTimeKind.Local)
//        equal 1978 d.Year
//        equal 7 d.Month
//        equal 27 d.Day
//        equal 0 d.Hour
//        equal 0 d.Minute
//
//        let d = DateTime(624059424000000000L)
//        equal 1978 d.Year
//        equal 7 d.Month
//        equal 27 d.Day
//        equal 0 d.Hour
//        equal 0 d.Minute
//
//    testCase "DateTime.Ticks does not care about kind" <| fun () ->
//        let d1 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Local)
//        let d2 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
//        let d3 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Unspecified)
//        equal d1.Ticks d2.Ticks
//        equal d1.Ticks d3.Ticks
//        equal d2.Ticks d3.Ticks
//
//        let t = DateTime.UtcNow.Ticks
//        let d1 = DateTime(t, DateTimeKind.Local)
//        let d2 = DateTime(t, DateTimeKind.Utc)
//        let d3 = DateTime(t, DateTimeKind.Unspecified)
//        equal d1.Ticks d2.Ticks
//        equal d1.Ticks d3.Ticks
//        equal d2.Ticks d3.Ticks
//
//    testCase "DateTime <-> Ticks isomorphism" <| fun () ->
//        let checkIsomorphism (d: DateTime) =
//            try
//                let ticks = d.Ticks
//                let kind = d.Kind
//                let fromTicks = DateTime ticks
//                let fromTicksWithKind = DateTime (ticks, kind)
//
//                equal d fromTicks
//                equal ticks fromTicks.Ticks
//                equal d fromTicksWithKind
//                equal ticks fromTicksWithKind.Ticks
//                equal kind fromTicksWithKind.Kind
//            with e ->
//                failwithf "%A: %O" d e
//
//            try
//                equal d.Ticks (DateTime d.Ticks).Ticks
//            with e ->
//                failwithf "%s%O" "replacement bug. " e
//
//        checkIsomorphism DateTime.MinValue
//        checkIsomorphism DateTime.MaxValue
//        checkIsomorphism DateTime.Now
//        checkIsomorphism DateTime.UtcNow
//        checkIsomorphism <| DateTime(2014, 10, 9)
//        checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30)
//        checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//        checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500)
//        checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
//
//    testCase "DateTime.IsLeapYear works" <| fun () ->
//        DateTime.IsLeapYear(2014) |> equal false
//        DateTime.IsLeapYear(2016) |> equal true
