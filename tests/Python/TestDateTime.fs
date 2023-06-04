module Fable.Tests.DateTime

open System
open Util.Testing
open Fable.Tests

let toSigFigs nSigFigs x =
    let absX = abs x
    let digitsToStartOfNumber = floor(log10 absX) + 1. // x > 0 => +ve | x < 0 => -ve
    let digitsToAdjustNumberBy = int digitsToStartOfNumber - nSigFigs
    let scale = pown 10. digitsToAdjustNumberBy
    round(x / scale) * scale

let thatYearSeconds (dt: DateTime) =
    (dt - DateTime(dt.Year, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds

let thatYearMilliseconds (dt: DateTime) =
    (dt - DateTime(dt.Year, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalMilliseconds

[<Fact>]
let ``test DateTime.ToString with custom format works`` () =
    DateTime(2014, 9, 11, 16, 37, 0).ToString("HH:mm", System.Globalization.CultureInfo.InvariantCulture)
    |> equal "16:37"

[<Fact>]
let ``test DateTime.ToString without separator works`` () =
    DateTime(2017, 9, 5).ToString("yyyyMM")
    |> equal "201709"

[<Fact>]
let ``test DateTime.ToString with milliseconds`` () =
    DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("ss.fff")
    |> equal "11.345"

// FIXME: missing regex module
// [<Fact>]
// let ``test DateTime.ToString with Round-trip format works for Utc`` () =
//     let str = DateTime(2014, 9, 11, 16, 37, 2, DateTimeKind.Utc).ToString("O")
//     System.Text.RegularExpressions.Regex.Replace(str, "0{3,}", "000")
//     |> equal "2014-09-11T16:37:02.000Z"

[<Fact>]
let ``test DateTime from Year 1 to 99 works`` () =
    let date = DateTime(1, 1, 2)
    date.Year |> equal 1
    let date = DateTime(99, 1, 2)
    date.Year |> equal 99

[<Fact>]
let ``test DateTime UTC from Year 1 to 99 works`` () =
    let date = DateTime(1, 1, 2, 0, 0, 0, DateTimeKind.Utc)
    date.Year |> equal 1
    let date = DateTime(99, 1, 2, 0, 0, 0, DateTimeKind.Utc)
    date.Year |> equal 99

[<Fact>]
let ``test DateTime.MaxValue works`` () =
    let d1 = DateTime.Now
    let d2 = DateTime.MaxValue
    d1 < d2 |> equal true

[<Fact>]
let ``test DateTime.MinValue works`` () =
    let d1 = DateTime.Now
    let d2 = DateTime.MinValue
    d1 < d2 |> equal false

[<Fact>]
let ``test DateTime.MinValue works in pattern match`` () =
    let d1 = Some DateTime.Now
    match d1 with
    | Some date when date <> DateTime.MinValue -> ()
    | _ -> failwith "expected pattern match above"

// [<Fact>]
// let ``test DateTime.ToLocalTime works`` () =
//     let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
//     let d' = d.ToLocalTime()
//     d.Kind <> d'.Kind
//     |> equal true
