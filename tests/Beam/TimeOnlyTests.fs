module Fable.Tests.TimeOnlyTests

open System
open Util.Testing

[<Fact>]
let ``test TimeOnly constructors work`` () =
    let t = TimeOnly(13, 23, 30)
    t.Hour |> equal 13
    t.Minute |> equal 23
    t.Second |> equal 30

[<Fact>]
let ``test TimeOnly constructor with milliseconds works`` () =
    let t = TimeOnly(13, 23, 30, 500)
    t.Hour |> equal 13
    t.Minute |> equal 23
    t.Second |> equal 30
    t.Millisecond |> equal 500

[<Fact>]
let ``test TimeOnly.MinValue works`` () =
    let t = TimeOnly.MinValue
    t.Hour |> equal 0
    t.Minute |> equal 0
    t.Second |> equal 0

[<Fact>]
let ``test TimeOnly.ToTimeSpan works`` () =
    let t = TimeOnly(13, 23, 30)
    let ts = t.ToTimeSpan()
    ts.Hours |> equal 13
    ts.Minutes |> equal 23
    ts.Seconds |> equal 30

[<Fact>]
let ``test TimeOnly.AddHours works`` () =
    let t = TimeOnly(13, 0, 0)
    let t2 = t.AddHours(2.0)
    t2.Hour |> equal 15

[<Fact>]
let ``test TimeOnly.AddMinutes works`` () =
    let t = TimeOnly(13, 30, 0)
    let t2 = t.AddMinutes(45.0)
    t2.Hour |> equal 14
    t2.Minute |> equal 15

[<Fact>]
let ``test TimeOnly.IsBetween works`` () =
    let t = TimeOnly(13, 0, 0)
    let start = TimeOnly(12, 0, 0)
    let finish = TimeOnly(14, 0, 0)
    t.IsBetween(start, finish) |> equal true
    t.IsBetween(finish, start) |> equal false

[<Fact>]
let ``test TimeOnly.ToString with format works`` () =
    let t = TimeOnly(13, 23, 30)
    t.ToString("T", System.Globalization.CultureInfo.InvariantCulture) |> equal "13:23:30"
    t.ToString("t", System.Globalization.CultureInfo.InvariantCulture) |> equal "13:23"
