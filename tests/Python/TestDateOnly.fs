module Fable.Tests.DateOnly

open System
open Util.Testing

[<Fact>]
let ``test DateOnly constructors work`` () =
    let d = DateOnly(2014, 10, 9)
    d.Year |> equal 2014
    d.Month |> equal 10
    d.Day |> equal 9

[<Fact>]
let ``test DateOnly.MinValue works`` () =
    let d = DateOnly.MinValue
    d.Year |> equal 1
    d.Month |> equal 1
    d.Day |> equal 1

[<Fact>]
let ``test DateOnly.MaxValue works`` () =
    let d = DateOnly.MaxValue
    d.Year |> equal 9999
    d.Month |> equal 12
    d.Day |> equal 31

[<Fact>]
let ``test DateOnly.DayOfWeek works`` () =
    let d = DateOnly(2014, 10, 9)
    d.DayOfWeek |> equal DayOfWeek.Thursday

[<Fact>]
let ``test DateOnly.DayOfYear works`` () =
    let d = DateOnly(2014, 10, 9)
    d.DayOfYear |> equal 282

[<Fact>]
let ``test DateOnly.AddDays works`` () =
    let d = DateOnly(2014, 10, 9)
    let d2 = d.AddDays(10)
    d2.Day |> equal 19
    d2.Month |> equal 10

[<Fact>]
let ``test DateOnly.AddMonths works`` () =
    let d = DateOnly(2014, 1, 31)
    let d2 = d.AddMonths(1)
    d2.Month |> equal 2
    d2.Day |> equal 28

[<Fact>]
let ``test DateOnly.AddYears works`` () =
    let d = DateOnly(2016, 2, 29)
    let d2 = d.AddYears(1)
    d2.Year |> equal 2017
    d2.Month |> equal 2
    d2.Day |> equal 28

[<Fact>]
let ``test DateOnly.ToString works`` () =
    let d = DateOnly(2014, 10, 9)
    d.ToString("o", System.Globalization.CultureInfo.InvariantCulture) |> equal "2014-10-09"

[<Fact>]
let ``test DateOnly.FromDateTime works`` () =
    let dt = DateTime(2014, 10, 9, 13, 23, 30)
    let d = DateOnly.FromDateTime(dt)
    d.Year |> equal 2014
    d.Month |> equal 10
    d.Day |> equal 9
