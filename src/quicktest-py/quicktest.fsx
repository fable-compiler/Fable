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
    // Summer time (allowed to detect invalid utcoffset for Europe/Paris)
    // let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
    // d.Hour |> equal 13

    // DateTime(2014, 7, 1, 16, 37, 0, 985, DateTimeKind.Utc).ToString("r f", CultureInfo.InvariantCulture)
    // |> equal "r 9"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r g", CultureInfo.InvariantCulture)
    |> equal "r A.D."

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r gg", CultureInfo.InvariantCulture)
    |> equal "r A.D."

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r f", CultureInfo.InvariantCulture)
    |> equal "r 6"

    DateTime
        .Parse("2009-06-15T13:45:30.05")
        .ToString("r f", CultureInfo.InvariantCulture)
    |> equal "r 0"

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r ff", CultureInfo.InvariantCulture)
    |> equal "r 61"

    DateTime
        .Parse("2009-06-15T13:45:30.0050000")
        .ToString("r ff", CultureInfo.InvariantCulture)
    |> equal "r 00"

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r fff", CultureInfo.InvariantCulture)
    |> equal "r 617"

    DateTime
        .Parse("2009-06-15T13:45:30.0005000")
        .ToString("r fff", CultureInfo.InvariantCulture)
    |> equal "r 000"

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r ffff", CultureInfo.InvariantCulture)
    |> equal "r 6175"

    DateTime
        .Parse("2009-06-15T13:45:30.0000500")
        .ToString("r ffff", CultureInfo.InvariantCulture)
    |> equal "r 0000"

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r fffff", CultureInfo.InvariantCulture)
    |> equal "r 61754"

    DateTime
        .Parse("2009-06-15T13:45:30.0000050")
        .ToString("r fffff", CultureInfo.InvariantCulture)
    |> equal "r 00000"

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r ffffff", CultureInfo.InvariantCulture)
    |> equal "r 617542"

    DateTime
        .Parse("2009-06-15T13:45:30.0000005")
        .ToString("r ffffff", CultureInfo.InvariantCulture)
    |> equal "r 000000"
    // We only have a precision up to the microsecond
    // DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r fffffff", CultureInfo.InvariantCulture)
    // |> equal "r 6175425"

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r F", CultureInfo.InvariantCulture)
    |> equal "r 6"

    DateTime
        .Parse("2009-06-15T13:45:30.05")
        .ToString("r F", CultureInfo.InvariantCulture)
    |> equal "r "

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r FF", CultureInfo.InvariantCulture)
    |> equal "r 61"

    DateTime
        .Parse("2009-06-15T13:45:30.0050000")
        .ToString("r FF", CultureInfo.InvariantCulture)
    |> equal "r "

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r FFF", CultureInfo.InvariantCulture)
    |> equal "r 617"

    DateTime
        .Parse("2009-06-15T13:45:30.0005000")
        .ToString("r FFF", CultureInfo.InvariantCulture)
    |> equal "r "

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r FFFF", CultureInfo.InvariantCulture)
    |> equal "r 6175"

    DateTime
        .Parse("2009-06-15T13:45:30.0000500")
        .ToString("r FFFF", CultureInfo.InvariantCulture)
    |> equal "r "

    DateTime
        .Parse("2009-06-15T13:45:30.6175425")
        .ToString("r FFFFF", CultureInfo.InvariantCulture)
    |> equal "r 61754"

    DateTime
        .Parse("2009-06-15T13:45:30.0000050")
        .ToString("r FFFFF", CultureInfo.InvariantCulture)
    |> equal "r "

    DateTime
        .Parse("2009-06-15T13:45:30.0617425")
        .ToString("r FFFFFF", CultureInfo.InvariantCulture)
    |> equal "r 061742"

    DateTime
        .Parse("2009-06-15T13:45:30.0000005")
        .ToString("r FFFFFF", CultureInfo.InvariantCulture)
    |> equal "r "
    // We only have a precision up to the microsecond
    // DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r fffffff", CultureInfo.InvariantCulture)
    // |> equal "r 6175425"

    DateTime(2014, 7, 1, 16, 37, 1, 2, 3)
        .ToString("r d", CultureInfo.InvariantCulture)
    |> equal "r 1"

    DateTime(2014, 7, 13, 16, 37, 1, 2, 3)
        .ToString("r d", CultureInfo.InvariantCulture)
    |> equal "r 13"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r dd", CultureInfo.InvariantCulture)
    |> equal "r 01"

    DateTime(2014, 7, 21, 16, 37, 0)
        .ToString("r dd", CultureInfo.InvariantCulture)
    |> equal "r 21"

    DateTime(2014, 7, 7, 16, 37, 0)
        .ToString("r ddd", CultureInfo.InvariantCulture)
    |> equal "r Mon"

    DateTime(2014, 7, 8, 16, 37, 0)
        .ToString("r ddd", CultureInfo.InvariantCulture)
    |> equal "r Tue"

    DateTime(2014, 7, 9, 16, 37, 0)
        .ToString("r ddd", CultureInfo.InvariantCulture)
    |> equal "r Wed"

    DateTime(2014, 7, 10, 16, 37, 0)
        .ToString("r ddd", CultureInfo.InvariantCulture)
    |> equal "r Thu"

    DateTime(2014, 7, 11, 16, 37, 0)
        .ToString("r ddd", CultureInfo.InvariantCulture)
    |> equal "r Fri"

    DateTime(2014, 7, 12, 16, 37, 0)
        .ToString("r ddd", CultureInfo.InvariantCulture)
    |> equal "r Sat"

    DateTime(2014, 7, 13, 16, 37, 0)
        .ToString("r ddd", CultureInfo.InvariantCulture)
    |> equal "r Sun"

    DateTime(2014, 7, 7, 16, 37, 0)
        .ToString("r dddd", CultureInfo.InvariantCulture)
    |> equal "r Monday"

    DateTime(2014, 7, 8, 16, 37, 0)
        .ToString("r dddd", CultureInfo.InvariantCulture)
    |> equal "r Tuesday"

    DateTime(2014, 7, 9, 16, 37, 0)
        .ToString("r dddd", CultureInfo.InvariantCulture)
    |> equal "r Wednesday"

    DateTime(2014, 7, 10, 16, 37, 0)
        .ToString("r dddd", CultureInfo.InvariantCulture)
    |> equal "r Thursday"

    DateTime(2014, 7, 11, 16, 37, 0)
        .ToString("r dddd", CultureInfo.InvariantCulture)
    |> equal "r Friday"

    DateTime(2014, 7, 12, 16, 37, 0)
        .ToString("r dddd", CultureInfo.InvariantCulture)
    |> equal "r Saturday"

    DateTime(2014, 7, 13, 16, 37, 0)
        .ToString("r dddd", CultureInfo.InvariantCulture)
    |> equal "r Sunday"

    // I can't find a way to convert unix_epoch_milliseconds_to_ticks and
    // ticks_to_unix_epoch_milliseconds to works on microsecond precision
    // so we can't activate this test yet...
    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r f", CultureInfo.InvariantCulture)
    // |> equal "r 0"
    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r ff", CultureInfo.InvariantCulture)
    // |> equal "r 00"

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r fff", CultureInfo.InvariantCulture)
    // |> equal "r 000"

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r ffff", CultureInfo.InvariantCulture)
    // |> equal "r 0000"

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r fffff", CultureInfo.InvariantCulture)
    // |> equal "r 00000"

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r ffffff", CultureInfo.InvariantCulture)
    // |> equal "r 000000"

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r fffffff", CultureInfo.InvariantCulture)
    // |> equal "r 0000000"

    // DateTime(2014, 7, 1, 16, 37, 11, 23).ToString("r F", CultureInfo.InvariantCulture)
    // |> equal "r "

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r FF", CultureInfo.InvariantCulture)
    // |> equal "r "

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r FFF", CultureInfo.InvariantCulture)
    // |> equal "r "

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r FFFF", CultureInfo.InvariantCulture)
    // |> equal "r "

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r FFFFF", CultureInfo.InvariantCulture)
    // |> equal "r "

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r FFFFFF", CultureInfo.InvariantCulture)
    // |> equal "r "

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r FFFFFFF", CultureInfo.InvariantCulture)
    // |> equal "r "

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r g", CultureInfo.InvariantCulture)
    // |> equal "r A.D."

    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r gg", CultureInfo.InvariantCulture)
    // |> equal "r A.D."

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r h", CultureInfo.InvariantCulture)
    |> equal "r 4"

    DateTime(2014, 7, 1, 4, 37, 0).ToString("r h", CultureInfo.InvariantCulture)
    |> equal "r 4"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r hh", CultureInfo.InvariantCulture)
    |> equal "r 04"

    DateTime(2014, 7, 1, 4, 37, 0)
        .ToString("r hh", CultureInfo.InvariantCulture)
    |> equal "r 04"

    DateTime(2014, 7, 1, 4, 37, 0).ToString("r H", CultureInfo.InvariantCulture)
    |> equal "r 4"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r H", CultureInfo.InvariantCulture)
    |> equal "r 16"

    DateTime(2014, 7, 1, 4, 37, 0)
        .ToString("r HH", CultureInfo.InvariantCulture)
    |> equal "r 04"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r HH", CultureInfo.InvariantCulture)
    |> equal "r 16"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r K", CultureInfo.InvariantCulture)
    |> equal "r "

    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("r K", CultureInfo.InvariantCulture)
    |> equal "r Z"

    // Timezone dependent (test is configured for Europe/Paris timezone)
    // DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r K", CultureInfo.InvariantCulture)
    // |> equal "r +02:00"

    DateTime(2014, 7, 1, 16, 3, 0).ToString("r m", CultureInfo.InvariantCulture)
    |> equal "r 3"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r m", CultureInfo.InvariantCulture)
    |> equal "r 37"

    DateTime(2014, 7, 1, 16, 3, 0)
        .ToString("r mm", CultureInfo.InvariantCulture)
    |> equal "r 03"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r mm", CultureInfo.InvariantCulture)
    |> equal "r 37"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r M", CultureInfo.InvariantCulture)
    |> equal "r 7"

    DateTime(2014, 11, 1, 16, 37, 0)
        .ToString("r M", CultureInfo.InvariantCulture)
    |> equal "r 11"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r MM", CultureInfo.InvariantCulture)
    |> equal "r 07"

    DateTime(2014, 11, 1, 16, 37, 0)
        .ToString("r MM", CultureInfo.InvariantCulture)
    |> equal "r 11"

    DateTime(2014, 1, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Jan"

    DateTime(2014, 2, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Feb"

    DateTime(2014, 3, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Mar"

    DateTime(2014, 4, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Apr"

    DateTime(2014, 5, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r May"

    DateTime(2014, 6, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Jun"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Jul"

    DateTime(2014, 8, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Aug"

    DateTime(2014, 9, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Sep"

    DateTime(2014, 10, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Oct"

    DateTime(2014, 11, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Nov"

    DateTime(2014, 12, 1, 16, 37, 0)
        .ToString("r MMM", CultureInfo.InvariantCulture)
    |> equal "r Dec"

    DateTime(2014, 1, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r January"

    DateTime(2014, 2, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r February"

    DateTime(2014, 3, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r March"

    DateTime(2014, 4, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r April"

    DateTime(2014, 5, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r May"

    DateTime(2014, 6, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r June"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r July"

    DateTime(2014, 8, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r August"

    DateTime(2014, 9, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r September"

    DateTime(2014, 10, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r October"

    DateTime(2014, 11, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r November"

    DateTime(2014, 12, 1, 16, 37, 0)
        .ToString("r MMMM", CultureInfo.InvariantCulture)
    |> equal "r December"

    DateTime(2014, 7, 1, 16, 37, 3)
        .ToString("r s", CultureInfo.InvariantCulture)
    |> equal "r 3"

    DateTime(2014, 7, 1, 16, 37, 31)
        .ToString("r s", CultureInfo.InvariantCulture)
    |> equal "r 31"

    DateTime(2014, 7, 1, 16, 37, 3)
        .ToString("r ss", CultureInfo.InvariantCulture)
    |> equal "r 03"

    DateTime(2014, 7, 1, 16, 37, 31)
        .ToString("r ss", CultureInfo.InvariantCulture)
    |> equal "r 31"

    DateTime(2014, 7, 1, 1, 37, 0).ToString("r t", CultureInfo.InvariantCulture)
    |> equal "r A"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r t", CultureInfo.InvariantCulture)
    |> equal "r P"

    DateTime(2014, 7, 1, 1, 37, 0)
        .ToString("r tt", CultureInfo.InvariantCulture)
    |> equal "r AM"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r tt", CultureInfo.InvariantCulture)
    |> equal "r PM"

    DateTime(1, 1, 1).ToString("r y", CultureInfo.InvariantCulture)
    |> equal "r 1"

    DateTime(0900, 1, 1).ToString("r y", CultureInfo.InvariantCulture)
    |> equal "r 0"

    DateTime(1900, 1, 1).ToString("r y", CultureInfo.InvariantCulture)
    |> equal "r 0"

    DateTime(2009, 1, 1).ToString("r y", CultureInfo.InvariantCulture)
    |> equal "r 9"

    DateTime(2019, 1, 1).ToString("r y", CultureInfo.InvariantCulture)
    |> equal "r 19"

    DateTime(1, 1, 1).ToString("r yy", CultureInfo.InvariantCulture)
    |> equal "r 01"

    DateTime(0900, 1, 1).ToString("r yy", CultureInfo.InvariantCulture)
    |> equal "r 00"

    DateTime(1900, 1, 1).ToString("r yy", CultureInfo.InvariantCulture)
    |> equal "r 00"

    DateTime(2009, 1, 1).ToString("r yy", CultureInfo.InvariantCulture)
    |> equal "r 09"

    DateTime(2019, 1, 1).ToString("r yy", CultureInfo.InvariantCulture)
    |> equal "r 19"

    DateTime(1, 1, 1).ToString("r yyy", CultureInfo.InvariantCulture)
    |> equal "r 001"

    DateTime(0900, 1, 1).ToString("r yyy", CultureInfo.InvariantCulture)
    |> equal "r 900"

    DateTime(1900, 1, 1).ToString("r yyy", CultureInfo.InvariantCulture)
    |> equal "r 1900"

    DateTime(2009, 1, 1).ToString("r yyy", CultureInfo.InvariantCulture)
    |> equal "r 2009"

    DateTime(2019, 1, 1).ToString("r yyy", CultureInfo.InvariantCulture)
    |> equal "r 2019"

    DateTime(1, 1, 1).ToString("r yyyy", CultureInfo.InvariantCulture)
    |> equal "r 0001"

    DateTime(0900, 1, 1).ToString("r yyyy", CultureInfo.InvariantCulture)
    |> equal "r 0900"

    DateTime(1900, 1, 1).ToString("r yyyy", CultureInfo.InvariantCulture)
    |> equal "r 1900"

    DateTime(2009, 1, 1).ToString("r yyyy", CultureInfo.InvariantCulture)
    |> equal "r 2009"

    DateTime(2019, 1, 1).ToString("r yyyy", CultureInfo.InvariantCulture)
    |> equal "r 2019"


    DateTime(1, 1, 1).ToString("r yyyyy", CultureInfo.InvariantCulture)
    |> equal "r 00001"

    DateTime(0900, 1, 1).ToString("r yyyyy", CultureInfo.InvariantCulture)
    |> equal "r 00900"

    DateTime(1900, 1, 1).ToString("r yyyyy", CultureInfo.InvariantCulture)
    |> equal "r 01900"

    DateTime(2009, 1, 1).ToString("r yyyyy", CultureInfo.InvariantCulture)
    |> equal "r 02009"

    DateTime(2019, 1, 1).ToString("r yyyyy", CultureInfo.InvariantCulture)
    |> equal "r 02019"


    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("r z", CultureInfo.InvariantCulture)
    |> equal "r +0"

    // Timezone dependent (test is configured for Europe/Paris timezone)
    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r z", CultureInfo.InvariantCulture)
    // |> equal "r +2"
    // DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r z", CultureInfo.InvariantCulture)
    // |> equal "r +2"


    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("r zz", CultureInfo.InvariantCulture)
    |> equal "r +00"
    // Timezone dependent (test is configured for Europe/Paris timezone)
    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r zz", CultureInfo.InvariantCulture)
    // |> equal "r +02"
    // DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r zz", CultureInfo.InvariantCulture)
    // |> equal "r +02"

    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("r zzz", CultureInfo.InvariantCulture)
    |> equal "r +00:00"
    // Timezone dependent (test is configured for Europe/Paris timezone)
    // DateTime(2014, 7, 1, 16, 37, 0).ToString("r zzz", CultureInfo.InvariantCulture)
    // |> equal "r +02:00"
    // DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r zzz", CultureInfo.InvariantCulture)
    // |> equal "r +02:00"

    // Time separator
    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r :", CultureInfo.InvariantCulture)
    |> equal "r :"

    // Date separator
    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r /", CultureInfo.InvariantCulture)
    |> equal "r /"

    // String quotation
    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r \"hh\" h", CultureInfo.InvariantCulture)
    |> equal "r hh 4"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r 'hh' h", CultureInfo.InvariantCulture)
    |> equal "r hh 4"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r \'hh\'", CultureInfo.InvariantCulture)
    |> equal "r hh"

    // Format character
    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r %h", CultureInfo.InvariantCulture)
    |> equal "r 4"

    DateTime(2014, 7, 1, 16, 37, 0)
        .ToString("r %hh", CultureInfo.InvariantCulture)
    |> equal "r 44"

    // Escape character
    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("r \zz", CultureInfo.InvariantCulture)
    |> equal "r z+0"

    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("r \\zz", CultureInfo.InvariantCulture)
    |> equal "r z+0"

    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("r \\\zz", CultureInfo.InvariantCulture)
    |> equal "r \+00"

    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("r \\z\\z", CultureInfo.InvariantCulture)
    |> equal "r zz"

    // Escape character with verbatim string
    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("""r \zz""", CultureInfo.InvariantCulture)
    |> equal "r z+0"

    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("""r \\zz""", CultureInfo.InvariantCulture)
    |> equal "r \+00"

    DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc)
        .ToString("""r \\\zz""", CultureInfo.InvariantCulture)
    |> equal "r \z+0"

    printfn "All tests passed!"

    0
