module Fable.Tests.TimeOnly

open System
open Util.Testing
open System.Globalization

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

        testCase "FromDateTime works" <| fun () ->
            let now = DateTime.Now
            let t = TimeOnly.FromDateTime now

            equal now.Hour t.Hour
            equal now.Minute t.Minute
            equal now.Second t.Second
            equal now.Millisecond t.Millisecond

            let now = DateTime.UtcNow
            let t = TimeOnly.FromDateTime now

            equal now.Hour t.Hour
            equal now.Minute t.Minute
            equal now.Second t.Second
            equal now.Millisecond t.Millisecond

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
            // F# 6 can cast int implicitly to float,
            // but fable-standalone cannot do it until it's updated
            t.AddHours 18. |> equal TimeOnly.MinValue
            t.AddHours -31. |> equal (TimeOnly (23, 0))
            t.AddMinutes 59. |> equal (TimeOnly (6, 59))
            t.AddMinutes -361. |> equal (TimeOnly (23, 59))

        testCase "Add with wrapped days works" <| fun () ->
            let t = TimeOnly (6, 0, 0)

            let newT, wrappedDays = t.Add (TimeSpan.FromHours 17.)
            equal (TimeOnly (23, 0)) newT
            equal 0 wrappedDays

            let newT, wrappedDays = t.Add (TimeSpan.FromHours 18.)
            equal TimeOnly.MinValue newT
            equal 1 wrappedDays

            let newT, wrappedDays = t.Add (TimeSpan.FromHours 19.)
            equal (TimeOnly (1, 0)) newT
            equal 1 wrappedDays

            let newT, wrappedDays = t.Add (TimeSpan.FromHours -7.)
            equal (TimeOnly (23, 0)) newT
            equal -1 wrappedDays

            let newT, wrappedDays = t.Add (TimeSpan.FromDays 3.)
            equal t newT
            equal 3 wrappedDays

        testCase "Subtract works" <| fun () ->
            let left = TimeOnly (6, 0)
            let right = TimeOnly (5, 30)
            left - right |> equal (TimeSpan.FromMinutes 30.)

            let left = TimeOnly.MinValue
            let right = TimeOnly.MinValue
            left - right |> equal TimeSpan.Zero

            let left = TimeOnly.MinValue
            let right = TimeOnly (23, 59, 59, 999)
            left - right |> equal (TimeSpan.FromMilliseconds 1.)

            let left = TimeOnly (23, 59, 59, 999)
            let right = TimeOnly.MinValue
            left - right |> equal (TimeSpan (0, 23, 59, 59, 999))

            let left = TimeOnly (1, 0)
            let right = TimeOnly (23, 59, 59)
            left - right |> equal (TimeSpan (1, 0, 1))

        testCase "ToString works" <| fun () ->
            let t1 = TimeOnly (5, 0, 2, 22)
            let t2 = TimeOnly (14, 2)

            // t2.ToString() |> equal "14:02"

            t1.ToString(CultureInfo.InvariantCulture) |> equal "05:00"
            t2.ToString(CultureInfo.InvariantCulture) |> equal "14:02"

            t1.ToString("r", CultureInfo.InvariantCulture) |> equal "05:00:02"
            t2.ToString("r", CultureInfo.InvariantCulture) |> equal "14:02:00"

            t1.ToString("R", CultureInfo.InvariantCulture) |> equal "05:00:02"
            t2.ToString("R", CultureInfo.InvariantCulture) |> equal "14:02:00"

            t1.ToString("o", CultureInfo.InvariantCulture) |> equal "05:00:02.0220000"
            t2.ToString("o", CultureInfo.InvariantCulture) |> equal "14:02:00.0000000"

            t1.ToString("O", CultureInfo.InvariantCulture) |> equal "05:00:02.0220000"
            t2.ToString("O", CultureInfo.InvariantCulture) |> equal "14:02:00.0000000"

            t1.ToString("t", CultureInfo.InvariantCulture) |> equal "05:00"
            t2.ToString("t", CultureInfo.InvariantCulture) |> equal "14:02"

            t1.ToString("T", CultureInfo.InvariantCulture) |> equal "05:00:02"
            t2.ToString("T", CultureInfo.InvariantCulture) |> equal "14:02:00"

        testCase "Parse parses valid TimeOnly" <| fun () ->
            equal (TimeOnly (23, 0)) (TimeOnly.Parse "23:0:0")
            equal (TimeOnly (23, 0)) (TimeOnly.Parse "23:00")
            equal (TimeOnly (3, 0)) (TimeOnly.Parse "3:00")
            equal (TimeOnly (0, 0, 5)) (TimeOnly.Parse "00:0:5   ")
            equal TimeOnly.MinValue (TimeOnly.Parse "   0   :   0    : 0   ")
            equal (TimeOnly (0, 40, 5, 3)) (TimeOnly.Parse "00:40:5.003")
            // We're limited to millisecond precision in JS
#if FABLE_COMPILER
            equal (TimeOnly (0, 0, 5, 3)) (TimeOnly.Parse "00:0:5.0031")
            equal (TimeOnly (0, 0, 5, 3)) (TimeOnly.Parse "00:0:5.00313213213213")
#else
            equal (TimeOnly 50031000L) (TimeOnly.Parse "00:0:5.0031")
            equal (TimeOnly 50031321L) (TimeOnly.Parse "00:0:5.00313213213213")
#endif
            equal (TimeOnly (0, 0, 59, 30)) (TimeOnly.Parse "00:  0:  59.03")
            equal (TimeOnly (0, 3, 5, 300)) (TimeOnly.Parse "00 :03:5.3")
            equal (TimeOnly (2, 0, 5, 300)) (TimeOnly.Parse "  02:0:5.30")

        testCase "TryParse returns false for invalid TimeOnly" <| fun () ->
            let isValid, _ = TimeOnly.TryParse "4"
            equal false isValid

            let isValid, _ = TimeOnly.TryParse "24:00"
            equal false isValid

            let isValid, _ = TimeOnly.TryParse "22:60"
            equal false isValid

            let isValid, _ = TimeOnly.TryParse "002:10"
            equal false isValid

            let isValid, _ = TimeOnly.TryParse "22:50:60"
            equal false isValid

            let isValid, _ = TimeOnly.TryParse "-04:00"
            equal false isValid

            // let isValid, _ = TimeOnly.TryParse "02:00:00,333" // not invalid in NET8_0
            // equal false isValid

            let isValid, _ = TimeOnly.TryParse "02:00:00:33"
            equal false isValid

        testCase "TryParse returns true and correct TimeOnly for valid TimeOnly" <| fun () ->
            let isValid, t = TimeOnly.TryParse "23:0:0"
            equal true isValid
            equal (TimeOnly (23, 0)) t

            let isValid, t = TimeOnly.TryParse "23:00"
            equal true isValid
            equal (TimeOnly (23, 0)) t

            let isValid, t = TimeOnly.TryParse "3:00"
            equal true isValid
            equal (TimeOnly (3, 0)) t

            let isValid, t = TimeOnly.TryParse "00:0:5"
            equal true isValid
            equal (TimeOnly (0, 0, 5)) t

            let isValid, t = TimeOnly.TryParse "0:0:0"
            equal true isValid
            equal TimeOnly.MinValue t

            let isValid, t = TimeOnly.TryParse "00:40:5.003"
            equal true isValid
            equal (TimeOnly (0, 40, 5, 3)) t

            let isValid, t = TimeOnly.TryParse "00:0:59.03"
            equal true isValid
            equal (TimeOnly (0, 0, 59, 30)) t

            let isValid, t = TimeOnly.TryParse "00:03:5.3"
            equal true isValid
            equal (TimeOnly (0, 3, 5, 300)) t

            let isValid, t = TimeOnly.TryParse "02:0:5.30"
            equal true isValid
            equal (TimeOnly (2, 0, 5, 300)) t

            // We're limited to millisecond precision in JS
#if FABLE_COMPILER
            let isValid, t = TimeOnly.TryParse "00:0:5.0031"
            equal true isValid
            equal (TimeOnly (0, 0, 5, 3)) t

            let isValid, t = TimeOnly.TryParse "00:0:5.00313213213213"
            equal true isValid
            equal (TimeOnly (0, 0, 5, 3)) t
#else
            let isValid, t = TimeOnly.TryParse "00:0:5.0031"
            equal true isValid
            equal (TimeOnly 50031000L) t

            let isValid, t = TimeOnly.TryParse "00:0:5.00313213213213"
            equal true isValid
            equal (TimeOnly 50031321L) t
#endif

        testCase "Comparison works" <| fun () ->
            equal true (TimeOnly (20, 1, 1) < TimeOnly (20, 1, 2))
            equal true (TimeOnly (20, 1, 1) <= TimeOnly (20, 1, 1))
            equal false (TimeOnly (20, 1, 1) > TimeOnly (20, 1, 1))

        testCase "Can be used as map key" <| fun () ->
            let m = [ TimeOnly (20, 5, 1), () ] |> Map.ofList
            equal true (Map.containsKey (TimeOnly(20, 1, 1).AddMinutes(4.)) m)
            equal false (Map.containsKey (TimeOnly (20, 1, 1)) m)
    ]
