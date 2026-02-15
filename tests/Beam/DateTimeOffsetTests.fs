module Fable.Tests.DateTimeOffsetTests

open System
open Util.Testing
open System.Globalization

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
let ``test DateTimeOffset.ToString() default works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let str = d.ToString()
    str.Length > 0 |> equal true

[<Fact>]
let ``test DateTimeOffset.ToString with custom format works`` () =
    DateTimeOffset(2014, 9, 11, 16, 37, 0, TimeSpan.Zero).ToString("HH:mm", CultureInfo.InvariantCulture)
    |> equal "16:37"

[<Fact>]
let ``test DateTimeOffset constructors work`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Year |> equal 2014
    d.Month |> equal 10
    d.Day |> equal 9
    d.Hour |> equal 13
    d.Minute |> equal 23
    d.Second |> equal 30

[<Fact>]
let ``test DateTimeOffset.Offset works`` () =
    let d1 = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d1.Offset |> equal TimeSpan.Zero

    let d2 = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.FromHours(3.0))
    d2.Offset |> equal (TimeSpan.FromHours(3.0))

[<Fact>]
let ``test DateTimeOffset.DateTime works`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    let dt = d.DateTime
    dt.Year |> equal 2014
    dt.Month |> equal 10
    dt.Day |> equal 9

[<Fact>]
let ``test DateTimeOffset is datetime compatible`` () =
    let d = DateTimeOffset(2014, 10, 9, 13, 23, 30, TimeSpan.Zero)
    d.Year |> equal 2014
    d.Month |> equal 10
