module Fable.Tests.TimeOnlyTests

open System
open System.Globalization
open Util.Testing

[<Fact>]
let ``TimeOnly constructors work`` () =
    let t1 = TimeOnly(20000L)
    let t2 = TimeOnly(3, 3, 3)
    let t3 = TimeOnly(5, 5, 5, 5)
    let t4 = TimeOnly(7, 7, 7, 7)
    let t5 = TimeOnly.MaxValue
    let t6 = TimeOnly.MinValue
    let t7 = TimeOnly(1, 2, 3, 4, 5)
    let t8 = TimeOnly(8L)

    t1.Ticks |> equal 20000L
    t2.Second |> equal 3
    t3.Millisecond |> equal 5
    t4.Millisecond |> equal 7
    t5.Hour |> equal 23
    t5.Minute |> equal 59
    t5.Millisecond |> equal 999
    t6.Ticks |> equal 0L

    t7.Hour |> equal 1
    t7.Minute |> equal 2
    t7.Second |> equal 3
    t7.Millisecond |> equal 4
    t7.Microsecond |> equal 5
    t7.Nanosecond |> equal 0
    t8.Nanosecond |> equal 800

[<Fact>]
let ``FromTimeSpan works`` () =
    let t1 = TimeOnly.FromTimeSpan (TimeSpan.FromHours 2.)
    let t2 = TimeOnly.FromTimeSpan (TimeSpan (23, 59, 59))

    t1.Hour |> equal 2
    t2.Hour |> equal 23
    t2.Minute |> equal 59
    t2.Second |> equal 59

    throwsAnyError (fun () -> TimeOnly.FromTimeSpan (TimeSpan.FromHours 24.))
    throwsAnyError (fun () -> TimeOnly.FromTimeSpan (TimeSpan.FromHours -1.))

[<Fact>]
let ``FromDateTime works`` () =
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

[<Fact>]
let ``ToTimeSpan works`` () =
    let t1 = TimeOnly.MaxValue.ToTimeSpan()
    let t2 = TimeOnly(10, 20, 30, 40).ToTimeSpan()

    t1.Hours |> equal 23
    t2.Days |> equal 0
    t2.Hours |> equal 10
    t2.Minutes |> equal 20
    t2.Seconds |> equal 30
    t2.Milliseconds |> equal 40

[<Fact>]
let ``IsBetween works`` () =
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

[<Fact>]
let ``Add* works`` () =
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

[<Fact>]
let ``Add with wrapped days works`` () =
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

[<Fact>]
let ``Subtract works`` () =
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

[<Fact>]
let ``ToString works`` () =
    let t1 = TimeOnly (5, 0, 2, 22)
    let t2 = TimeOnly (14, 2)

    // Disabled because depending on the CurrentCulture the result can be different
    // Is it possible to use an env variable to set the culture?
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

[<Fact>]
let ``TimeOnly.ToString with custom format works`` () =
    TimeOnly(16, 3, 5, 7, 89).ToString("h:m:s tt ffffff", CultureInfo.InvariantCulture)
    |> equal "4:3:5 PM 007089"

[<Fact>]
let ``TimeOnly.ToString with h specifier works`` () =
    // h: 12-hour clock, no leading zero (1-12)
    TimeOnly(1, 30, 0).ToString("%h", CultureInfo.InvariantCulture)  |> equal "1"
    TimeOnly(9, 30, 0).ToString("%h", CultureInfo.InvariantCulture)  |> equal "9"
    TimeOnly(12, 0, 0).ToString("%h", CultureInfo.InvariantCulture)  |> equal "12"
    TimeOnly(13, 0, 0).ToString("%h", CultureInfo.InvariantCulture)  |> equal "1"
    TimeOnly(23, 0, 0).ToString("%h", CultureInfo.InvariantCulture)  |> equal "11"
    TimeOnly(0, 0, 0).ToString("%h", CultureInfo.InvariantCulture)   |> equal "12"

[<Fact>]
let ``TimeOnly.ToString with hh specifier works`` () =
    // hh: 12-hour clock, with leading zero (01-12)
    TimeOnly(1, 30, 0).ToString("hh", CultureInfo.InvariantCulture)  |> equal "01"
    TimeOnly(9, 30, 0).ToString("hh", CultureInfo.InvariantCulture)  |> equal "09"
    TimeOnly(12, 0, 0).ToString("hh", CultureInfo.InvariantCulture)  |> equal "12"
    TimeOnly(13, 0, 0).ToString("hh", CultureInfo.InvariantCulture)  |> equal "01"
    TimeOnly(0, 0, 0).ToString("hh", CultureInfo.InvariantCulture)   |> equal "12"

[<Fact>]
let ``TimeOnly.ToString with H specifier works`` () =
    // H: 24-hour clock, no leading zero (0-23)
    TimeOnly(1, 30, 0).ToString("%H", CultureInfo.InvariantCulture)  |> equal "1"
    TimeOnly(9, 30, 0).ToString("%H", CultureInfo.InvariantCulture)  |> equal "9"
    TimeOnly(13, 0, 0).ToString("%H", CultureInfo.InvariantCulture)  |> equal "13"
    TimeOnly(0, 0, 0).ToString("%H", CultureInfo.InvariantCulture)   |> equal "0"

[<Fact>]
let ``TimeOnly.ToString with HH specifier works`` () =
    // HH: 24-hour clock, with leading zero (00-23)
    TimeOnly(1, 30, 0).ToString("HH", CultureInfo.InvariantCulture)  |> equal "01"
    TimeOnly(9, 30, 0).ToString("HH", CultureInfo.InvariantCulture)  |> equal "09"
    TimeOnly(13, 0, 0).ToString("HH", CultureInfo.InvariantCulture)  |> equal "13"
    TimeOnly(0, 0, 0).ToString("HH", CultureInfo.InvariantCulture)   |> equal "00"

[<Fact>]
let ``TimeOnly.ToString with m and mm specifiers work`` () =
    // m: minutes, no leading zero (0-59)
    TimeOnly(5, 9, 0).ToString("H:m", CultureInfo.InvariantCulture)    |> equal "5:9"
    TimeOnly(5, 45, 0).ToString("H:m", CultureInfo.InvariantCulture)   |> equal "5:45"
    // mm: minutes, with leading zero (00-59)
    TimeOnly(5, 9, 0).ToString("HH:mm", CultureInfo.InvariantCulture)  |> equal "05:09"
    TimeOnly(5, 45, 0).ToString("HH:mm", CultureInfo.InvariantCulture) |> equal "05:45"

[<Fact>]
let ``TimeOnly.ToString with s and ss specifiers work`` () =
    // s: seconds, no leading zero (0-59)
    TimeOnly(5, 9, 3).ToString("H:m:s", CultureInfo.InvariantCulture)    |> equal "5:9:3"
    TimeOnly(5, 45, 30).ToString("H:m:s", CultureInfo.InvariantCulture)  |> equal "5:45:30"
    // ss: seconds, with leading zero (00-59)
    TimeOnly(5, 9, 3).ToString("HH:mm:ss", CultureInfo.InvariantCulture)   |> equal "05:09:03"
    TimeOnly(5, 45, 30).ToString("HH:mm:ss", CultureInfo.InvariantCulture) |> equal "05:45:30"

[<Fact>]
let ``TimeOnly.ToString with f fraction specifiers work`` () =
    // 18 ms = 0.018 seconds; sub-second fraction in 7-digit form: 0180000
    let t = TimeOnly(7, 27, 15, 18)
    t.ToString("hh:mm:ss.f", CultureInfo.InvariantCulture)       |> equal "07:27:15.0"       // tenths
    t.ToString("hh:mm:ss.ff", CultureInfo.InvariantCulture)      |> equal "07:27:15.01"      // hundredths
    t.ToString("hh:mm:ss.fff", CultureInfo.InvariantCulture)     |> equal "07:27:15.018"     // milliseconds
    t.ToString("hh:mm:ss.ffff", CultureInfo.InvariantCulture)    |> equal "07:27:15.0180"    // ten-thousandths
    t.ToString("hh:mm:ss.fffff", CultureInfo.InvariantCulture)   |> equal "07:27:15.01800"   // hundred-thousandths
    t.ToString("hh:mm:ss.ffffff", CultureInfo.InvariantCulture)  |> equal "07:27:15.018000"  // millionths
    t.ToString("hh:mm:ss.fffffff", CultureInfo.InvariantCulture) |> equal "07:27:15.0180000" // ten-millionths (ticks)

[<Fact>]
let ``TimeOnly.ToString with F fraction specifiers work`` () =
    // 18 ms: tenths digit is 0, so F suppresses output; trailing zeros are stripped
    let t = TimeOnly(7, 27, 15, 18)
    t.ToString("hh:mm:ss.F", CultureInfo.InvariantCulture)       |> equal "07:27:15"        // 0 → entirely suppressed
    t.ToString("hh:mm:ss.FF", CultureInfo.InvariantCulture)      |> equal "07:27:15.01"     // 01 → no trailing zeros
    t.ToString("hh:mm:ss.FFF", CultureInfo.InvariantCulture)     |> equal "07:27:15.018"    // 018 → no trailing zeros
    t.ToString("hh:mm:ss.FFFF", CultureInfo.InvariantCulture)    |> equal "07:27:15.018"    // 0180 → trailing 0 stripped
    t.ToString("hh:mm:ss.FFFFF", CultureInfo.InvariantCulture)   |> equal "07:27:15.018"    // 01800 → trailing zeros stripped
    t.ToString("hh:mm:ss.FFFFFF", CultureInfo.InvariantCulture)  |> equal "07:27:15.018"    // 018000 → stripped
    t.ToString("hh:mm:ss.FFFFFFF", CultureInfo.InvariantCulture) |> equal "07:27:15.018"    // 0180000 → stripped
    // all-zero fraction → decimal point and fraction entirely omitted
    let t0 = TimeOnly(7, 27, 15)
    t0.ToString("hh:mm:ss.FFFFFFF", CultureInfo.InvariantCulture) |> equal "07:27:15"

[<Fact>]
let ``TimeOnly.ToString with t and tt specifiers work`` () =
    // t: first character of AM/PM designator
    TimeOnly(6, 0, 0).ToString("h:m:s t", CultureInfo.InvariantCulture)    |> equal "6:0:0 A"
    TimeOnly(18, 0, 0).ToString("h:m:s t", CultureInfo.InvariantCulture)   |> equal "6:0:0 P"
    TimeOnly(0, 0, 0).ToString("h:m:s t", CultureInfo.InvariantCulture)    |> equal "12:0:0 A"
    TimeOnly(12, 0, 0).ToString("h:m:s t", CultureInfo.InvariantCulture)   |> equal "12:0:0 P"
    // tt: full AM/PM designator
    TimeOnly(0, 0, 0).ToString("hh:mm:ss tt", CultureInfo.InvariantCulture)  |> equal "12:00:00 AM"
    TimeOnly(6, 0, 0).ToString("hh:mm:ss tt", CultureInfo.InvariantCulture)  |> equal "06:00:00 AM"
    TimeOnly(12, 0, 0).ToString("hh:mm:ss tt", CultureInfo.InvariantCulture) |> equal "12:00:00 PM"
    TimeOnly(18, 0, 0).ToString("hh:mm:ss tt", CultureInfo.InvariantCulture) |> equal "06:00:00 PM"
    TimeOnly(23, 59, 59).ToString("hh:mm:ss tt", CultureInfo.InvariantCulture) |> equal "11:59:59 PM"

[<Fact>]
let ``TimeOnly.ToString with percent specifier works`` () =
    // %: prefix to use a single custom format specifier without it being
    // interpreted as a standard format specifier
    TimeOnly(13, 45, 30).ToString("%H", CultureInfo.InvariantCulture) |> equal "13"
    TimeOnly(13, 45, 30).ToString("%h", CultureInfo.InvariantCulture) |> equal "1"
    TimeOnly(13, 45, 30).ToString("%m", CultureInfo.InvariantCulture) |> equal "45"
    TimeOnly(13, 45, 30).ToString("%s", CultureInfo.InvariantCulture) |> equal "30"

[<Fact>]
let ``TimeOnly.ToString with escape character works`` () =
    // \: escape the next character so it is treated as a literal
    TimeOnly(13, 45, 30).ToString("h \\h m \\m", CultureInfo.InvariantCulture) |> equal "1 h 45 m"
    TimeOnly(13, 45, 30).ToString("HH\\:mm\\:ss", CultureInfo.InvariantCulture) |> equal "13:45:30"

[<Fact>]
let ``TimeOnly.ToString with literal string delimiters work`` () =
    // 'string' and "string": enclose literal text in single or double quotes
    TimeOnly(13, 45, 30).ToString("'Time:' HH:mm:ss", CultureInfo.InvariantCulture)  |> equal "Time: 13:45:30"
    TimeOnly(13, 45, 30).ToString("\"At\" HH:mm:ss", CultureInfo.InvariantCulture)   |> equal "At 13:45:30"

[<Fact>]
let ``Parse parses valid TimeOnly`` () =
    equal (TimeOnly (23, 0)) (TimeOnly.Parse "23:00:00")
    equal (TimeOnly (23, 0)) (TimeOnly.Parse "23:00")
    equal (TimeOnly (3, 0)) (TimeOnly.Parse "3:00")
    equal (TimeOnly (0, 0, 5)) (TimeOnly.Parse "00:0:5   ")
    equal TimeOnly.MinValue (TimeOnly.Parse "   0   :   0    : 0   ")
    equal (TimeOnly (0, 40, 5, 3)) (TimeOnly.Parse "00:40:5.003")
    equal (TimeOnly (0, 0, 5, 3)) (TimeOnly.Parse "00:0:5.003")
    equal (TimeOnly 50031000L) (TimeOnly.Parse "00:0:5.0031")
    equal (TimeOnly 50031321L) (TimeOnly.Parse "00:0:5.00313213213213")
    equal (TimeOnly (0, 0, 59, 30)) (TimeOnly.Parse "00:  0:  59.03")
    equal (TimeOnly (0, 3, 5, 300)) (TimeOnly.Parse "00 :03:5.3")
    equal (TimeOnly (2, 0, 5, 300)) (TimeOnly.Parse "  02:0:5.30")

[<Fact>]
let ``TryParse returns false for invalid TimeOnly`` () =
    let test (s: string) =
        let isValid, _ = TimeOnly.TryParse s
        equal false isValid

    test "4"
    test "24:00"
    test "22:60"
    test "002:10"
    test "22:50:60"
    test "-04:00"
    // test "02:00:00,333" // not invalid in NET8_0
    test "02:00:00:33"

[<Fact>]
let ``TryParse returns true and correct TimeOnly for valid TimeOnly`` () =
    let test expected (s: string) =
        let isValid, t = TimeOnly.TryParse s
        isValid |> equal true
        t |> equal expected

    test (TimeOnly (23, 0)) "23:00:00"
    test (TimeOnly (23, 0)) "23:00"
    test (TimeOnly (3, 0)) "3:00"
    test (TimeOnly (0, 0, 5)) "00:0:5"
    test TimeOnly.MinValue "0:0:0"
    test (TimeOnly (0, 40, 5, 3)) "00:40:5.003"
    test (TimeOnly (0, 0, 59, 30)) "00:0:59.03"
    test (TimeOnly (0, 3, 5, 300)) "00:03:5.3"
    test (TimeOnly (2, 0, 5, 300)) "02:0:5.30"
    test (TimeOnly (0, 0, 5, 3)) "00:0:5.003"
    test (TimeOnly 50031000L) "00:0:5.0031"
    test (TimeOnly 50031321L) "00:0:5.00313213213213"

[<Fact>]
let ``Comparison works`` () =
    equal true (TimeOnly (20, 1, 1) < TimeOnly (20, 1, 2))
    equal true (TimeOnly (20, 1, 1) <= TimeOnly (20, 1, 1))
    equal false (TimeOnly (20, 1, 1) > TimeOnly (20, 1, 1))

[<Fact>]
let ``Can be used as map key`` () =
    let m = [ TimeOnly (20, 5, 1), () ] |> Map.ofList
    equal true (Map.containsKey (TimeOnly(20, 1, 1).AddMinutes(4.)) m)
    equal false (Map.containsKey (TimeOnly (20, 1, 1)) m)
