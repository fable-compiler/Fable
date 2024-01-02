#r "nuget:Fable.Python"

open Fable.Core
open Fable.Core.Testing
open Fable.Core.PyInterop
open Fable.Python.Builtins
open System
open System.Globalization

let equal expected actual =
    // According the console log arguments are reversed
    Assert.AreEqual(actual, expected)

[<EntryPoint>]
let main argv =
    let name = Array.tryHead argv |> Option.defaultValue "Guest"
    printfn $"Hello {name}!"

    // Open file with builtin `open`
    // use file = builtins.``open``(StringPath "data.txt")
    // file.read() |> printfn "File contents: %s"

    let thatYearSeconds (dt: DateTime) =
        (dt - DateTime(dt.Year, 1, 1, 0, 0, 0, DateTimeKind.Utc)).TotalSeconds

    let test ms expected =
        let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
        let dt2 = dt1.AddMilliseconds(ms)
        let res1 = compare dt1 dt2
        let res2 = dt1.CompareTo(dt2)
        let res3 = DateTime.Compare(dt1, dt2)
        equal true (res1 = res2 && res2 = res3)
        equal expected res1

    test 1000. -1
    test -1000. 1
    test 0. 0

    0


// TryParse(String, DateTime)
// TryParse(String, IFormatProvider, DateTime)
// TryParse(String, IFormatProvider, DateTimeStyles, DateTime)
