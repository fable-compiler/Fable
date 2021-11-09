module Fable.Tests.TimeOnly

open System
open Util.Testing

let tests =
    testList "TimeOnly" [
        testCase "TimeSpan constructors work" <| fun () ->
            let t1 = TimeOnly(20000L)
            let t2 = TimeOnly(3, 3, 3)
            let t3 = TimeOnly(5, 5, 5, 5)
            let t4 = TimeOnly(7, 7, 7, 7)
            let t5 = TimeOnly.MaxValue
            let t6 = TimeOnly.MinValue

            t1.Ticks |> equal 20000L
            t2.Second |> equal 3
            t3.Millisecond |> equal 5
            t4.Millisecond |> equal 7
            t5.Hour |> equal 23
            t5.Minute |> equal 59
            t5.Millisecond |> equal 999
            t6.Ticks |> equal 0L

        testCase "FromTimeSpan works" <| fun () ->
            let t1 = TimeOnly.FromTimeSpan (TimeSpan.FromHours 2.)
            let t2 = TimeOnly.FromTimeSpan (TimeSpan (23, 59, 59))

            t1.Hour |> equal 2
            t2.Hour |> equal 23
            t2.Minute |> equal 59
            t2.Second |> equal 59

            Util.throwsAnyError (fun () -> TimeOnly.FromTimeSpan (TimeSpan.FromHours 24.))
            Util.throwsAnyError (fun () -> TimeOnly.FromTimeSpan (TimeSpan.FromHours -1.))

        testCase "ToTimeSpan works" <| fun () ->
            let t1 = TimeOnly.MaxValue.ToTimeSpan()
            let t2 = TimeOnly(10, 20, 30, 40).ToTimeSpan()

            t1.Hours |> equal 23
            t2.Days |> equal 0
            t2.Hours |> equal 10
            t2.Minutes |> equal 20
            t2.Seconds |> equal 30
            t2.Milliseconds |> equal 40

        testCase "IsBetween works" <| fun () ->
            let start = TimeOnly (3, 0, 0)
            let end' = start
            let t = start 
            t.IsBetween (start, end') |> equal false

            let start = TimeOnly (3, 0, 0)
            let end' = TimeOnly (4, 0, 0)
            let t = start 
            t.IsBetween (start, end') |> equal true

            let start = TimeOnly (3, 0, 0)
            let end' = TimeOnly (4, 0, 0)
            let t = end'
            t.IsBetween (start, end') |> equal false

            let start = TimeOnly (3, 0, 0)
            let end' = TimeOnly (4, 0, 0)
            let t = TimeOnly (3, 30, 0)
            t.IsBetween (start, end') |> equal true

            let start = TimeOnly (4, 0, 0)
            let end' = TimeOnly (3, 0, 0)
            let t = TimeOnly (3, 30, 0)
            t.IsBetween (start, end') |> equal false

            let start = TimeOnly (23, 0, 0)
            let end' = TimeOnly (1, 0, 0)
            let t = TimeOnly (23, 30, 0)
            t.IsBetween (start, end') |> equal true

        testCase "Add* works" <| fun () ->
            let t = TimeOnly (6, 0, 0)
            t.Add (TimeSpan.FromHours 18.) |> equal TimeOnly.MinValue
            t.Add (TimeSpan.FromHours 19.) |> equal (TimeOnly (1, 0))
            t.Add (TimeSpan.FromHours -7.) |> equal (TimeOnly (23, 0))
            t.AddHours 18 |> equal TimeOnly.MinValue
            t.AddHours -31 |> equal (TimeOnly (23, 0))
            t.AddMinutes 59 |> equal (TimeOnly (6, 59))
            t.AddMinutes -361 |> equal (TimeOnly (23, 59))
    ]
