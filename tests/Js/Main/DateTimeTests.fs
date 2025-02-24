module Fable.Tests.DateTime

open System
open Util
open Util.Testing
open Fable.Tests
open System.Globalization

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

let tests =
  testList "DateTime" [
    testCase "DateTime.ToString with custom format works" <| fun () ->
        DateTime(2014, 7, 1, 16, 37, 1, 2).ToString("r d", CultureInfo.InvariantCulture)
        |> equal "r 1"
        DateTime(2014, 7, 13, 16, 37, 1, 2).ToString("r d", CultureInfo.InvariantCulture)
        |> equal "r 13"

        DateTime(2014, 7, 1, 16, 37, 0).ToString("r dd", CultureInfo.InvariantCulture)
        |> equal "r 01"
        DateTime(2014, 7, 21, 16, 37, 0).ToString("r dd", CultureInfo.InvariantCulture)
        |> equal "r 21"

        DateTime(2014, 7, 7, 16, 37, 0).ToString("r ddd", CultureInfo.InvariantCulture)
        |> equal "r Mon"
        DateTime(2014, 7, 8, 16, 37, 0).ToString("r ddd", CultureInfo.InvariantCulture)
        |> equal "r Tue"
        DateTime(2014, 7, 9, 16, 37, 0).ToString("r ddd", CultureInfo.InvariantCulture)
        |> equal "r Wed"
        DateTime(2014, 7, 10, 16, 37, 0).ToString("r ddd", CultureInfo.InvariantCulture)
        |> equal "r Thu"
        DateTime(2014, 7, 11, 16, 37, 0).ToString("r ddd", CultureInfo.InvariantCulture)
        |> equal "r Fri"
        DateTime(2014, 7, 12, 16, 37, 0).ToString("r ddd", CultureInfo.InvariantCulture)
        |> equal "r Sat"
        DateTime(2014, 7, 13, 16, 37, 0).ToString("r ddd", CultureInfo.InvariantCulture)
        |> equal "r Sun"

        DateTime(2014, 7, 7, 16, 37, 0).ToString("r dddd", CultureInfo.InvariantCulture)
        |> equal "r Monday"
        DateTime(2014, 7, 8, 16, 37, 0).ToString("r dddd", CultureInfo.InvariantCulture)
        |> equal "r Tuesday"
        DateTime(2014, 7, 9, 16, 37, 0).ToString("r dddd", CultureInfo.InvariantCulture)
        |> equal "r Wednesday"
        DateTime(2014, 7, 10, 16, 37, 0).ToString("r dddd", CultureInfo.InvariantCulture)
        |> equal "r Thursday"
        DateTime(2014, 7, 11, 16, 37, 0).ToString("r dddd", CultureInfo.InvariantCulture)
        |> equal "r Friday"
        DateTime(2014, 7, 12, 16, 37, 0).ToString("r dddd", CultureInfo.InvariantCulture)
        |> equal "r Saturday"
        DateTime(2014, 7, 13, 16, 37, 0).ToString("r dddd", CultureInfo.InvariantCulture)
        |> equal "r Sunday"

        DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r f", CultureInfo.InvariantCulture)
        |> equal "r 6"
        DateTime.Parse("2009-06-15T13:45:30.05").ToString("r f", CultureInfo.InvariantCulture)
        |> equal "r 0"
        DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r ff", CultureInfo.InvariantCulture)
        |> equal "r 61"
        DateTime.Parse("2009-06-15T13:45:30.0050000").ToString("r ff", CultureInfo.InvariantCulture)
        |> equal "r 00"
        DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r fff", CultureInfo.InvariantCulture)
        |> equal "r 617"
        DateTime.Parse("2009-06-15T13:45:30.0005000").ToString("r fff", CultureInfo.InvariantCulture)
        |> equal "r 000"
        // JavaScript Date only support precision to the millisecond so we fill with 0
        DateTime.Parse("2009-06-15T13:45:30.617").ToString("r ffff", CultureInfo.InvariantCulture)
        |> equal "r 6170"
        DateTime.Parse("2009-06-15T13:45:30.000").ToString("r ffff", CultureInfo.InvariantCulture)
        |> equal "r 0000"
        DateTime.Parse("2009-06-15T13:45:30.617").ToString("r fffff", CultureInfo.InvariantCulture)
        |> equal "r 61700"
        DateTime.Parse("2009-06-15T13:45:30.000").ToString("r fffff", CultureInfo.InvariantCulture)
        |> equal "r 00000"
        DateTime.Parse("2009-06-15T13:45:30.617").ToString("r ffffff", CultureInfo.InvariantCulture)
        |> equal "r 617000"
        DateTime.Parse("2009-06-15T13:45:30.000").ToString("r ffffff", CultureInfo.InvariantCulture)
        |> equal "r 000000"
        DateTime.Parse("2009-06-15T13:45:30.617").ToString("r fffffff", CultureInfo.InvariantCulture)
        |> equal "r 6170000"

        DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r F", CultureInfo.InvariantCulture)
        |> equal "r 6"
        DateTime.Parse("2009-06-15T13:45:30.05").ToString("r F", CultureInfo.InvariantCulture)
        |> equal "r "
        DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r FF", CultureInfo.InvariantCulture)
        |> equal "r 61"
        DateTime.Parse("2009-06-15T13:45:30.0050000").ToString("r FF", CultureInfo.InvariantCulture)
        |> equal "r "
        DateTime.Parse("2009-06-15T13:45:30.6175425").ToString("r FFF", CultureInfo.InvariantCulture)
        |> equal "r 617"
        DateTime.Parse("2009-06-15T13:45:30.0005000").ToString("r FFF", CultureInfo.InvariantCulture)
        |> equal "r "
        // JavaScript Date only support precision to the millisecond so we fill with 0
        DateTime.Parse("2009-06-15T13:45:30.617").ToString("r FFFF", CultureInfo.InvariantCulture)
        |> equal "r 617"
        DateTime.Parse("2009-06-15T13:45:30.000").ToString("r FFFF", CultureInfo.InvariantCulture)
        |> equal "r "
        DateTime.Parse("2009-06-15T13:45:30.617").ToString("r FFFFF", CultureInfo.InvariantCulture)
        |> equal "r 617"
        DateTime.Parse("2009-06-15T13:45:30.000").ToString("r FFFFF", CultureInfo.InvariantCulture)
        |> equal "r "
        DateTime.Parse("2009-06-15T13:45:30.061").ToString("r FFFFFF", CultureInfo.InvariantCulture)
        |> equal "r 061"
        DateTime.Parse("2009-06-15T13:45:30.000").ToString("r FFFFFF", CultureInfo.InvariantCulture)
        |> equal "r "
        DateTime.Parse("2009-06-15T13:45:30.617").ToString("r FFFFFFF", CultureInfo.InvariantCulture)
        |> equal "r 617"
        DateTime.Parse("2009-06-15T13:45:30.000").ToString("r FFFFFFF", CultureInfo.InvariantCulture)
        |> equal "r "

        DateTime(2014, 7, 1, 16, 37, 0).ToString("r g", CultureInfo.InvariantCulture)
        |> equal "r A.D."

        DateTime(2014, 7, 1, 16, 37, 0).ToString("r gg", CultureInfo.InvariantCulture)
        |> equal "r A.D."

        DateTime(2014, 7, 1, 16, 37, 0).ToString("r h", CultureInfo.InvariantCulture)
        |> equal "r 4"
        DateTime(2014, 7, 1, 4, 37, 0).ToString("r h", CultureInfo.InvariantCulture)
        |> equal "r 4"
        // Test edge case where hour is 12
        DateTime(2014, 7, 1, 0, 0, 0).ToString("r h", CultureInfo.InvariantCulture)
        |> equal "r 12"
        DateTime(2014, 7, 1, 12, 0, 0).ToString("r h", CultureInfo.InvariantCulture)
        |> equal "r 12"

        DateTime(2014, 7, 1, 16, 37, 0).ToString("r hh", CultureInfo.InvariantCulture)
        |> equal "r 04"
        DateTime(2014, 7, 1, 4, 37, 0).ToString("r hh", CultureInfo.InvariantCulture)
        |> equal "r 04"
        // Test edge case where hour is 12
        DateTime(2014, 7, 1, 0, 0, 0).ToString("r hh", CultureInfo.InvariantCulture)
        |> equal "r 12"
        DateTime(2014, 7, 1, 12, 0, 0).ToString("r hh", CultureInfo.InvariantCulture)
        |> equal "r 12"

        DateTime(2014, 7, 1, 4, 37, 0).ToString("r H", CultureInfo.InvariantCulture)
        |> equal "r 4"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r H", CultureInfo.InvariantCulture)
        |> equal "r 16"

        DateTime(2014, 7, 1, 4, 37, 0).ToString("r HH", CultureInfo.InvariantCulture)
        |> equal "r 04"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r HH", CultureInfo.InvariantCulture)
        |> equal "r 16"

        DateTime(2014, 7, 1, 16, 37, 0).ToString("r K", CultureInfo.InvariantCulture)
        |> equal "r "
        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r K", CultureInfo.InvariantCulture)
        |> equal "r Z"

        // // Timezone dependent (test is configured for Europe/Paris timezone)
        // DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r K", CultureInfo.InvariantCulture)
        // |> equal "r +02:00"

        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Unspecified).ToString("r K", CultureInfo.InvariantCulture)
        |> equal "r "

        DateTime(2014, 7, 1, 16, 3, 0).ToString("r m", CultureInfo.InvariantCulture)
        |> equal "r 3"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r m", CultureInfo.InvariantCulture)
        |> equal "r 37"

        DateTime(2014, 7, 1, 16, 3, 0).ToString("r mm", CultureInfo.InvariantCulture)
        |> equal "r 03"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r mm", CultureInfo.InvariantCulture)
        |> equal "r 37"

        DateTime(2014, 7, 1, 16, 37, 0).ToString("r M", CultureInfo.InvariantCulture)
        |> equal "r 7"
        DateTime(2014, 11, 1, 16, 37, 0).ToString("r M", CultureInfo.InvariantCulture)
        |> equal "r 11"

        DateTime(2014, 7, 1, 16, 37, 0).ToString("r MM", CultureInfo.InvariantCulture)
        |> equal "r 07"
        DateTime(2014, 11, 1, 16, 37, 0).ToString("r MM", CultureInfo.InvariantCulture)
        |> equal "r 11"

        DateTime(2014, 1, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Jan"
        DateTime(2014, 2, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Feb"
        DateTime(2014, 3, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Mar"
        DateTime(2014, 4, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Apr"
        DateTime(2014, 5, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r May"
        DateTime(2014, 6, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Jun"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Jul"
        DateTime(2014, 8, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Aug"
        DateTime(2014, 9, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Sep"
        DateTime(2014, 10, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Oct"
        DateTime(2014, 11, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Nov"
        DateTime(2014, 12, 1, 16, 37, 0).ToString("r MMM", CultureInfo.InvariantCulture)
        |> equal "r Dec"

        DateTime(2014, 1, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r January"
        DateTime(2014, 2, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r February"
        DateTime(2014, 3, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r March"
        DateTime(2014, 4, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r April"
        DateTime(2014, 5, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r May"
        DateTime(2014, 6, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r June"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r July"
        DateTime(2014, 8, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r August"
        DateTime(2014, 9, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r September"
        DateTime(2014, 10, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r October"
        DateTime(2014, 11, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r November"
        DateTime(2014, 12, 1, 16, 37, 0).ToString("r MMMM", CultureInfo.InvariantCulture)
        |> equal "r December"

        DateTime(2014, 7, 1, 16, 37, 3).ToString("r s", CultureInfo.InvariantCulture)
        |> equal "r 3"
        DateTime(2014, 7, 1, 16, 37, 31).ToString("r s", CultureInfo.InvariantCulture)
        |> equal "r 31"

        DateTime(2014, 7, 1, 16, 37, 3).ToString("r ss", CultureInfo.InvariantCulture)
        |> equal "r 03"
        DateTime(2014, 7, 1, 16, 37, 31).ToString("r ss", CultureInfo.InvariantCulture)
        |> equal "r 31"

        DateTime(2014, 7, 1, 1, 37, 0).ToString("r t", CultureInfo.InvariantCulture)
        |> equal "r A"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r t", CultureInfo.InvariantCulture)
        |> equal "r P"
        DateTime(2014, 7, 1, 1, 37, 0).ToString("r tt", CultureInfo.InvariantCulture)
        |> equal "r AM"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r tt", CultureInfo.InvariantCulture)
        |> equal "r PM"

        DateTime(1,1,1).ToString("r y", CultureInfo.InvariantCulture)
        |> equal "r 1"
        DateTime(0900,1,1).ToString("r y", CultureInfo.InvariantCulture)
        |> equal "r 0"
        DateTime(1900,1,1).ToString("r y", CultureInfo.InvariantCulture)
        |> equal "r 0"
        DateTime(2009,1,1).ToString("r y", CultureInfo.InvariantCulture)
        |> equal "r 9"
        DateTime(2019,1,1).ToString("r y", CultureInfo.InvariantCulture)
        |> equal "r 19"

        DateTime(1,1,1).ToString("r yy", CultureInfo.InvariantCulture)
        |> equal "r 01"
        DateTime(0900,1,1).ToString("r yy", CultureInfo.InvariantCulture)
        |> equal "r 00"
        DateTime(1900,1,1).ToString("r yy", CultureInfo.InvariantCulture)
        |> equal "r 00"
        DateTime(2009,1,1).ToString("r yy", CultureInfo.InvariantCulture)
        |> equal "r 09"
        DateTime(2019,1,1).ToString("r yy", CultureInfo.InvariantCulture)
        |> equal "r 19"

        DateTime(1,1,1).ToString("r yyy", CultureInfo.InvariantCulture)
        |> equal "r 001"
        DateTime(0900,1,1).ToString("r yyy", CultureInfo.InvariantCulture)
        |> equal "r 900"
        DateTime(1900,1,1).ToString("r yyy", CultureInfo.InvariantCulture)
        |> equal "r 1900"
        DateTime(2009,1,1).ToString("r yyy", CultureInfo.InvariantCulture)
        |> equal "r 2009"
        DateTime(2019,1,1).ToString("r yyy", CultureInfo.InvariantCulture)
        |> equal "r 2019"

        DateTime(1,1,1).ToString("r yyyy", CultureInfo.InvariantCulture)
        |> equal "r 0001"
        DateTime(0900,1,1).ToString("r yyyy", CultureInfo.InvariantCulture)
        |> equal "r 0900"
        DateTime(1900,1,1).ToString("r yyyy", CultureInfo.InvariantCulture)
        |> equal "r 1900"
        DateTime(2009,1,1).ToString("r yyyy", CultureInfo.InvariantCulture)
        |> equal "r 2009"
        DateTime(2019,1,1).ToString("r yyyy", CultureInfo.InvariantCulture)
        |> equal "r 2019"


        DateTime(1,1,1).ToString("r yyyyy", CultureInfo.InvariantCulture)
        |> equal "r 00001"
        DateTime(0900,1,1).ToString("r yyyyy", CultureInfo.InvariantCulture)
        |> equal "r 00900"
        DateTime(1900,1,1).ToString("r yyyyy", CultureInfo.InvariantCulture)
        |> equal "r 01900"
        DateTime(2009,1,1).ToString("r yyyyy", CultureInfo.InvariantCulture)
        |> equal "r 02009"
        DateTime(2019,1,1).ToString("r yyyyy", CultureInfo.InvariantCulture)
        |> equal "r 02019"


        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r z", CultureInfo.InvariantCulture)
        |> equal "r +0"

        // Timezone dependent (test is configured for Europe/Paris timezone)
        // DateTime(2014, 7, 1, 16, 37, 0).ToString("r z", CultureInfo.InvariantCulture)
        // |> equal "r +2"
        // DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r z", CultureInfo.InvariantCulture)
        // |> equal "r +2"


        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r zz", CultureInfo.InvariantCulture)
        |> equal "r +00"
        // Timezone dependent (test is configured for Europe/Paris timezone)
        // DateTime(2014, 7, 1, 16, 37, 0).ToString("r zz", CultureInfo.InvariantCulture)
        // |> equal "r +02"
        // DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r zz", CultureInfo.InvariantCulture)
        // |> equal "r +02"

        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r zzz", CultureInfo.InvariantCulture)
        |> equal "r +00:00"
        // Timezone dependent (test is configured for Europe/Paris timezone)
        // DateTime(2014, 7, 1, 16, 37, 0).ToString("r zzz", CultureInfo.InvariantCulture)
        // |> equal "r +02:00"
        // DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Local).ToString("r zzz", CultureInfo.InvariantCulture)
        // |> equal "r +02:00"

        // Time separator
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r :", CultureInfo.InvariantCulture)
        |> equal "r :"

        // Date separator
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r /", CultureInfo.InvariantCulture)
        |> equal "r /"

        // String quotation
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r \"hh\" h", CultureInfo.InvariantCulture)
        |> equal "r hh 4"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r 'hh' h", CultureInfo.InvariantCulture)
        |> equal "r hh 4"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r \'hh\'", CultureInfo.InvariantCulture)
        |> equal "r hh"

        // Format character
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r %h", CultureInfo.InvariantCulture)
        |> equal "r 4"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r %hh", CultureInfo.InvariantCulture)
        |> equal "r 44"

        // Escape character
        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r \zz", CultureInfo.InvariantCulture)
        |> equal "r z+0"
        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r \\zz", CultureInfo.InvariantCulture)
        |> equal "r z+0"
        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r \\\zz", CultureInfo.InvariantCulture)
        |> equal "r \+00"
        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("r \\z\\z", CultureInfo.InvariantCulture)
        |> equal "r zz"

        // Escape character with verbatim string
        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("""r \zz""", CultureInfo.InvariantCulture)
        |> equal "r z+0"
        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("""r \\zz""", CultureInfo.InvariantCulture)
        |> equal "r \+00"
        DateTime(2014, 7, 1, 16, 37, 0, DateTimeKind.Utc).ToString("""r \\\zz""", CultureInfo.InvariantCulture)
        |> equal "r \z+0"

        // The tests below are for testing the behaviour when going outside
        // of the standard token ranges.
        // For example, the known tokens range for 'H' are "H", "HH", so
        // we want to test what happens when we do "HHH" or "HHHH" or "HHHHH"
        // In general, the tests below check what happens when right outside of the known
        // token ranges and in another exagerated case

        DateTime(2014, 7, 13, 16, 37, 0).ToString("r dddd dddddd", CultureInfo.InvariantCulture)
        |> equal "r Sunday Sunday"

        throwsAnyError (fun _ ->
            DateTime(2014, 7, 13, 16, 37, 0).ToString("r ffffffff", CultureInfo.InvariantCulture)
        )

        throwsAnyError (fun _ ->
            DateTime(2014, 7, 13, 16, 37, 0).ToString("r FFFFFFFFF", CultureInfo.InvariantCulture)
        )

        DateTime(2014, 7, 1, 12, 0, 0).ToString("r ggg ggggg", CultureInfo.InvariantCulture)
        |> equal "r A.D. A.D."

        DateTime(2014, 7, 1, 12, 0, 0).ToString("r hhh hhhhh", CultureInfo.InvariantCulture)
        |> equal "r 12 12"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r HHH HHHHHHHH", CultureInfo.InvariantCulture)
        |> equal "r 16 16"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r KK KKK")
        |> equal "r  "
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r mmm mmmm", CultureInfo.InvariantCulture)
        |> equal "r 37 37"
        DateTime(2014, 12, 1, 16, 37, 0).ToString("r MMMMM MMMMMMMMM", CultureInfo.InvariantCulture)
        |> equal "r December December"
        DateTime(2014, 7, 1, 16, 37, 31).ToString("r sss ssssss", CultureInfo.InvariantCulture)
        |> equal "r 31 31"
        DateTime(2014, 7, 1, 16, 37, 0).ToString("r ttt ttttttt", CultureInfo.InvariantCulture)
        |> equal "r PM PM"
        DateTime(2019,1,1).ToString("r yyyyyy yyyyyyyyyy", CultureInfo.InvariantCulture)
        |> equal "r 002019 0000002019"

        // Timezone dependent (test is configured for Europe/Paris timezone)
        // DateTime(2014, 7, 1, 16, 37, 0).ToString("r zzzz zzzzzz", CultureInfo.InvariantCulture)
        // |> equal "r +02:00 +02:00"

    testCase "DateTime.ToString without separator works" <| fun () -> // See #1131
        DateTime(2017, 9, 5).ToString("yyyyMM")
        |> equal "201709"

    testCase "DateTime.ToString with milliseconds" <| fun () -> // See #1726
        DateTime(2014, 9, 11, 16, 37, 11, 345).ToString("ss.fff")
        |> equal "11.345"

    testCase "DateTime.ToString with Round-trip format works for Utc" <| fun () ->
        let str = DateTime(2014, 9, 11, 16, 37, 2, DateTimeKind.Utc).ToString("O")
        System.Text.RegularExpressions.Regex.Replace(str, "0{3,}", "000")
        |> equal "2014-09-11T16:37:02.000Z"

    testCase "DateTime.ToString('D') works" <| fun _ ->
        let format (d: DateTime) = d.ToString("D", CultureInfo.InvariantCulture)

        DateTime(2014, 9, 1, 16, 37, 2)
        |> format
        |> equal "Monday, 01 September 2014"

        DateTime(2014, 9, 1, 16, 37, 2, DateTimeKind.Local)
        |> format
        |> equal "Monday, 01 September 2014"

        DateTime(2014, 9, 1, 16, 37, 2, DateTimeKind.Utc)
        |> format
        |> equal "Monday, 01 September 2014"

    testCase "DateTime.ToString('d') works" <| fun _ ->
        let format (d: DateTime) = d.ToString("d", CultureInfo.InvariantCulture)

        DateTime(2014, 9, 1, 16, 37, 2)
        |> format
        |> equal "09/01/2014"

        DateTime(2014, 9, 1, 16, 37, 2, DateTimeKind.Local)
        |> format
        |> equal "09/01/2014"

        DateTime(2014, 9, 1, 16, 37, 2, DateTimeKind.Utc)
        |> format
        |> equal "09/01/2014"

    testCase "DateTime.ToString('T') works" <| fun _ ->
        let format (d: DateTime) = d.ToString("T", CultureInfo.InvariantCulture)

        DateTime(2014, 9, 1, 5, 7, 2)
        |> format
        |> equal "05:07:02"

        DateTime(2014, 9, 1, 5, 7, 2, DateTimeKind.Local)
        |> format
        |> equal "05:07:02"

        DateTime(2014, 9, 1, 5, 7, 2, DateTimeKind.Utc)
        |> format
        |> equal "05:07:02"

    testCase "DateTime.ToString('t') works" <| fun _ ->
        let format (d: DateTime) = d.ToString("t", CultureInfo.InvariantCulture)

        DateTime(2014, 9, 1, 5, 7, 2)
        |> format
        |> equal "05:07"

        DateTime(2014, 9, 1, 5, 7, 2, DateTimeKind.Local)
        |> format
        |> equal "05:07"

        DateTime(2014, 9, 1, 5, 7, 2, DateTimeKind.Utc)
        |> format
        |> equal "05:07"

    // TODO
    // Next test is disabled because it's depends on the time zone of the machine
    //A fix could be to use a regex or detect the time zone
    //
    // testCase "DateTime.ToString with Round-trip format works for local" <| fun () ->
    //     DateTime(2014, 9, 11, 16, 37, 2, DateTimeKind.Local).ToString("O")
    //     |> equal "2014-09-11T16:37:02.000+02:00" // Here the time zone is Europe/Paris (GMT+2)

    testCase "DateTime from Year 1 to 99 works" <| fun () ->
        let date = DateTime(1, 1, 2)
        date.Year |> equal 1
        let date = DateTime(99, 1, 2)
        date.Year |> equal 99

    testCase "DateTime UTC from Year 1 to 99 works" <| fun () ->
        let date = DateTime(1, 1, 2, 3, 4, 5, DateTimeKind.Utc)
        date.Year |> equal 1
        date.Month |> equal 1
        date.Day |> equal 2
        date.Hour |> equal 3
        date.Minute |> equal 4
        date.Second |> equal 5
        let date = DateTime(99, 1, 2, 3, 4, 5, DateTimeKind.Utc)
        date.Year |> equal 99
        date.Month |> equal 1
        date.Day |> equal 2
        date.Hour |> equal 3
        date.Minute |> equal 4
        date.Second |> equal 5

    // TODO: These two tests give different values for .NET and JS because DateTime
    // becomes as a plain JS Date object, so I'm just checking the fields get translated
    testCase "DateTime.MaxValue works" <| fun () ->
        let d1 = DateTime.Now
        let d2 = DateTime.MaxValue
        d1 < d2 |> equal true

    testCase "DateTime.MinValue works" <| fun () ->
        let d1 = DateTime.Now
        let d2 = DateTime.MinValue
        d1 < d2 |> equal false

    testCase "DateTime.MinValue works in pattern match" <| fun () ->
        let d1 = Some DateTime.Now
        match d1 with
        | Some date when date <> DateTime.MinValue -> ()
        | _ -> failwith "expected pattern match above"

    testCase "DateTime.ToLocalTime works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        let d' = d.ToLocalTime()
        d.Kind <> d'.Kind
        |> equal true

    testCase "Creating DateTimeOffset from DateTime and back works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        let dto = DateTimeOffset(d)
        let d' = dto.DateTime
        d' |> equal d

    testCase "Formatting DateTimeOffset works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        let dto = DateTimeOffset(d)
        // dto.ToString() |> equal "2014-10-09 13:23:30 +00:00"
        dto.ToString("HH:mm:ss", CultureInfo.InvariantCulture) |> equal "13:23:30"

    testCase "DateTimeOffset.UtcDateTime works" <| fun () ->
        let timeStr = "2016-07-07T01:00:00.000Z"
        let dtOffset = System.DateTimeOffset.Parse timeStr
        let dt = dtOffset.UtcDateTime
        equal 7 dt.Day
        equal 1 dt.Hour
        equal 0 dt.Minute
        equal DateTimeKind.Utc dt.Kind

    testCase "DateTimeOffset.LocalDateTime works" <| fun () ->
        let timeStr = "2016-07-07T01:00:00.000Z"
        let dtOffset = System.DateTimeOffset.Parse timeStr
        let dt = dtOffset.LocalDateTime
        equal DateTimeKind.Local dt.Kind
        let dt2 = DateTime.Parse(timeStr, CultureInfo.InvariantCulture).ToLocalTime()
        equal dt2.Day dt.Day
        equal dt2.Hour dt.Hour
        equal dt2.Minute dt.Minute

    testCase "DateTime.Hour works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        d.Hour |> equal 13

    // TODO: These four tests don't match exactly between .NET and JS
    // Think of a way to compare the results approximately
    testCase "DateTime.ToLongDateString works" <| fun () ->
        let dt = DateTime(2014, 9, 11, 16, 37, 0)
        let s = dt.ToLongDateString()
        s.Length > 0
        |> equal true

    testCase "DateTime.ToShortDateString works" <| fun () ->
        let dt = DateTime(2014, 9, 11, 16, 37, 0)
        let s = dt.ToShortDateString()
        s.Length > 0
        |> equal true

    testCase "DateTime.ToLongTimeString works" <| fun () ->
        let dt = DateTime(2014, 9, 11, 16, 37, 0)
        let s = dt.ToLongTimeString()
        s.Length > 0
        |> equal true

    testCase "DateTime.ToShortTimeString works" <| fun () ->
        let dt = DateTime(2014, 9, 11, 16, 37, 0)
        let s = dt.ToShortTimeString()
        s.Length > 0
        |> equal true

    // TODO: Unfortunately, JS will happily create invalid dates like DateTime(2014,2,29)
    //       But this problem also happens when parsing, so I haven't tried to fix it
    testCase "DateTime constructors work" <| fun () ->
        let d1 = DateTime(2014, 10, 9)
        let d2 = DateTime(2014, 10, 9, 13, 23, 30)
        let d3 = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        let d4 = DateTime(2014, 10, 9, 13, 23, 30, 500)
        let d5 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
        d1.Day + d2.Second + d3.Second + d4.Millisecond + d5.Millisecond
        |> equal 1069

    testCase "DateTime constructor from Ticks works" <| fun () ->
        let d = DateTime(624059424000000000L, DateTimeKind.Utc)
        equal 1978 d.Year
        equal 7 d.Month
        equal 27 d.Day
        equal 0 d.Hour
        equal 0 d.Minute

        let d = DateTime(624059424000000000L, DateTimeKind.Local)
        equal 1978 d.Year
        equal 7 d.Month
        equal 27 d.Day
        equal 0 d.Hour
        equal 0 d.Minute

        let d = DateTime(624059424000000000L)
        equal 1978 d.Year
        equal 7 d.Month
        equal 27 d.Day
        equal 0 d.Hour
        equal 0 d.Minute

    testCase "DateTime.Ticks does not care about kind" <| fun () ->
        let d1 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Local)
        let d2 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)
        let d3 = DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Unspecified)
        equal d1.Ticks d2.Ticks
        equal d1.Ticks d3.Ticks
        equal d2.Ticks d3.Ticks

        let t = DateTime.UtcNow.Ticks
        let d1 = DateTime(t, DateTimeKind.Local)
        let d2 = DateTime(t, DateTimeKind.Utc)
        let d3 = DateTime(t, DateTimeKind.Unspecified)
        equal d1.Ticks d2.Ticks
        equal d1.Ticks d3.Ticks
        equal d2.Ticks d3.Ticks

    testCase "DateTime <-> Ticks isomorphism" <| fun () ->
        let checkIsomorphism (d: DateTime) =
            try
                let ticks = d.Ticks
                let kind = d.Kind
                let fromTicks = DateTime ticks
                let fromTicksWithKind = DateTime (ticks, kind)

                equal d fromTicks
                equal ticks fromTicks.Ticks
                equal d fromTicksWithKind
                equal ticks fromTicksWithKind.Ticks
                equal kind fromTicksWithKind.Kind
            with e ->
                failwithf "%A: %O" d e

            try
                equal d.Ticks (DateTime d.Ticks).Ticks
            with e ->
                failwithf "%s%O" "replacement bug. " e

        checkIsomorphism DateTime.MinValue
        checkIsomorphism DateTime.MaxValue
        checkIsomorphism DateTime.Now
        checkIsomorphism DateTime.UtcNow
        checkIsomorphism <| DateTime(2014, 10, 9)
        checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30)
        checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500)
        checkIsomorphism <| DateTime(2014, 10, 9, 13, 23, 30, 500, DateTimeKind.Utc)

    testCase "DateTime.IsLeapYear works" <| fun () ->
        DateTime.IsLeapYear(2014) |> equal false
        DateTime.IsLeapYear(2016) |> equal true

    // TODO: Re-enable this test when we can fix it in the CI servers
    // testCase "DateTime.IsDaylightSavingTime works" <| fun () ->
    //     let d1 = DateTime(2017, 7, 18, 2, 0, 0)
    //     let d2 = DateTime(2017, 12, 18, 2, 0, 0)
    //     d1.IsDaylightSavingTime() |> equal true
    //     d2.IsDaylightSavingTime() |> equal false

    testCase "DateTime.DaysInMonth works" <| fun () ->
        DateTime.DaysInMonth(2014, 1) |> equal 31
        DateTime.DaysInMonth(2014, 2) |> equal 28
        DateTime.DaysInMonth(2014, 4) |> equal 30
        DateTime.DaysInMonth(2016, 2) |> equal 29

    testCase "DateTime.Now works" <| fun () ->
        let d = DateTime.Now
        d > DateTime.MinValue |> equal true

    testCase "DateTime.UtcNow works" <| fun () ->
        let d = DateTime.UtcNow
        d > DateTime.MinValue |> equal true

    testCase "DateTime.Parse works" <| fun () ->
        let d =
            DateTime.Parse(
                "2014-09-10T13:50:34.0000000",
                CultureInfo.InvariantCulture
            )

        d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second |> equal 2130

        let d = DateTime.Parse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture)
        d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
        |> equal 2130

        let d = DateTime.Parse("9/10/2014 1:50:34 AM", CultureInfo.InvariantCulture)
        d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
        |> equal 2118

        let d = DateTime.Parse("9/10/2014 13:50:34", CultureInfo.InvariantCulture)
        d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
        |> equal 2130

        let d = DateTime.Parse("9/10/2014 1:50:34", CultureInfo.InvariantCulture)
        d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
        |> equal 2118

        // Disabled because it is timezone dependent
        // I left it here in case, we need to test it in the future
        // Currently, it is setup for Europe/Paris timezone
        // let d = DateTime.Parse("2016-07-07T01:00:00.000Z", CultureInfo.InvariantCulture)
        // d.Year + d.Month + d.Day + d.Hour + d.Minute + d.Second
        // |> equal 2033

    testCase "DateTime.Parse with time-only string works" <| fun () -> // See #1045
        // Time-only should use now as the reference date
        let now = DateTime.Now

        let d = DateTime.Parse("13:50:34", CultureInfo.InvariantCulture)
        d.Year |> equal now.Year
        d.Month |> equal now.Month
        d.Day |> equal now.Day
        d.Hour + d.Minute + d.Second |> equal 97

        let d = DateTime.Parse("1:5:34 AM", CultureInfo.InvariantCulture)
        d.Year |> equal now.Year
        d.Month |> equal now.Month
        d.Day |> equal now.Day
        d.Hour + d.Minute + d.Second |> equal 40

        let d = DateTime.Parse("1:5:34 PM", CultureInfo.InvariantCulture)
        d.Year |> equal now.Year
        d.Month |> equal now.Month
        d.Day |> equal now.Day
        d.Hour + d.Minute + d.Second |> equal 52

        let d = DateTime.Parse("0:5:34 PM")
        d.Hour + d.Minute + d.Second |> equal 51

        let d1 = DateTime.Parse("12:5:34 PM")
        d1.Hour + d1.Minute + d1.Second |> equal 51

        let d2 = DateTime.Parse("3:5:34 PM")
        d2.Hour + d2.Minute + d2.Second |> equal 54

        let d3 = DateTime.Parse("15:5:34 PM")
        d3.Hour + d3.Minute + d3.Second |> equal 54


    testCase "DateTime.TryParse works" <| fun () ->
        let (isSuccess, _) = DateTime.TryParse("foo", CultureInfo.InvariantCulture, DateTimeStyles.None)
        isSuccess |> equal false

        let (isSuccess, dateTime) = DateTime.TryParse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture, DateTimeStyles.None)
        isSuccess |> equal true
        dateTime.Year + dateTime.Month + dateTime.Day + dateTime.Hour + dateTime.Minute + dateTime.Second |> equal 2130

        let (isSuccess, _) = DateTime.TryParse("foo", CultureInfo.InvariantCulture)
        isSuccess |> equal false

        let (isSuccess, dateTime) = DateTime.TryParse("9/10/2014 1:50:34 PM", CultureInfo.InvariantCulture)
        isSuccess |> equal true
        dateTime.Year + dateTime.Month + dateTime.Day + dateTime.Hour + dateTime.Minute + dateTime.Second |> equal 2130

        let (isSuccess, _) = DateTime.TryParse("foo")
        isSuccess |> equal false

        let (isSuccess, dateTime) = DateTime.TryParse("9/10/2014 1:50:34 PM")
        isSuccess |> equal true
        dateTime.Year + dateTime.Month + dateTime.Day + dateTime.Hour + dateTime.Minute + dateTime.Second |> equal 2130


    testCase "Parsing doesn't succeed for invalid dates" <| fun () ->
        let invalidAmericanDate = "13/1/2020"
        let r, _date = DateTime.TryParse(invalidAmericanDate, CultureInfo.InvariantCulture, DateTimeStyles.None)
        r |> equal false

    testCase "DateTime.Today works" <| fun () ->
        let d = DateTime.Today
        equal 0 d.Hour

    testCase "DateTime.ToUniversalTime works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        d.ToUniversalTime().Kind <> d.Kind
        |> equal true

    testCase "DateTime.SpecifyKind works" <| fun () -> // See #1844
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d2 = DateTime.SpecifyKind(d, DateTimeKind.Utc)
        d2.Kind |> equal DateTimeKind.Utc
        d.Ticks = d2.Ticks |> equal true
        // let d3 = d.ToUniversalTime()
        // d.Ticks = d3.Ticks |> equal false

    testCase "DateTime.Date works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30)
        d.Date.Hour |> equal 0
        d.Date.Day |> equal 9

    testCase "DateTime.Day works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        d.Day + d'.Day |> equal 18

    testCase "DateTime.DayOfWeek works" <| fun () ->
        DateTime(2014, 10, 5).DayOfWeek |> equal DayOfWeek.Sunday
        DateTime(2014, 10, 6).DayOfWeek |> equal DayOfWeek.Monday
        DateTime(2014, 10, 7).DayOfWeek |> equal DayOfWeek.Tuesday
        DateTime(2014, 10, 8).DayOfWeek |> equal DayOfWeek.Wednesday
        DateTime(2014, 10, 9).DayOfWeek |> equal DayOfWeek.Thursday
        DateTime(2014, 10, 10).DayOfWeek |> equal DayOfWeek.Friday
        DateTime(2014, 10, 11).DayOfWeek |> equal DayOfWeek.Saturday

    testCase "DateTime.DayOfYear works" <| fun () ->
        // Standard year
        DateTime(2014, 10, 9).DayOfYear |> equal 282
        // Leap year
        DateTime(2020, 10, 9).DayOfYear |> equal 283

    testCase "DateTime.Millisecond works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
        d.Millisecond |> equal 999

    testCase "DateTime.Ticks works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, 999, DateTimeKind.Utc)
        d.Ticks |> equal 635484578109990000L
        let d = DateTime(2014, 10, 9, 13, 23, 30, 999, DateTimeKind.Local)
        d.Ticks |> equal 635484578109990000L
        let d = DateTime(2014, 10, 9, 13, 23, 30, 999)
        d.Ticks |> equal 635484578109990000L

    testCase "DateTime.Minute works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        d.Minute + d'.Minute
        |> equal 46

    testCase "DateTime.Month works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        d.Month + d'.Month
        |> equal 20

    testCase "DateTime.Second works" <| fun () ->
        let d = DateTime(2014,9,12,0,0,30)
        let d' = DateTime(2014,9,12,0,0,59)
        d.Second + d'.Second
        |> equal 89

    testCase "DateTime.Year works" <| fun () ->
        let d = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Local)
        let d' = DateTime(2014, 10, 9, 13, 23, 30, DateTimeKind.Utc)
        d.Year + d'.Year
        |> equal 4028

    testCase "DateTime.AddYears works" <| fun () ->
        let test v expected =
            let dt = DateTime(2016,2,29,0,0,0,DateTimeKind.Utc).AddYears(v)
            equal expected (dt.Month + dt.Day)
        test 100 31
        test 1 30
        test -1 30
        test -100 31
        test 0 31

    testCase "DateTime.AddMonths works" <| fun () ->
        let test v expected =
            let dt = DateTime(2016,1,31,0,0,0,DateTimeKind.Utc).AddMonths(v)
            dt.Year + dt.Month + dt.Day
            |> equal expected
        test 100 2060
        test 20 2056
        test 6 2054
        test 5 2052
        test 1 2047
        test 0 2048
        test -1 2058
        test -5 2054
        test -20 2050
        test -100 2046

    testCase "DateTime.AddDays works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddDays(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 30585600.0
        test -100. 13305600.0
        test 0. 21945600.0

    testCase "DateTime.AddHours works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddHours(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 22305600.0
        test -100. 21585600.0
        test 0. 21945600.0

    testCase "DateTime.AddMinutes works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddMinutes(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 21951600.0
        test -100. 21939600.0
        test 0. 21945600.0

    testCase "DateTime.AddSeconds works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddSeconds(v)
            thatYearSeconds dt
            |> equal expected
        test 100. 21945700.0
        test -100. 21945500.0
        test 0. 21945600.0

    testCase "DateTime.AddMilliseconds works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddMilliseconds(v)
            thatYearMilliseconds dt
            |> equal expected
        test 100. 2.19456001e+10
        test -100. 2.19455999e+10
        test 0. 2.19456e+10

    // NOTE: Doesn't work for values between 10000L (TimeSpan.TicksPerMillisecond) and -10000L, except 0L
    testCase "DateTime.AddTicks works" <| fun () ->
        let test v expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc).AddTicks(v)
            dt.Ticks
            |> equal expected
        let ticks = 635460768000000000L
        test 100000L (ticks + 100000L)
        test -100000L (ticks - 100000L)
        test 0L ticks

    testCase "DateTime Addition works" <| fun () ->
        let test ms expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc)
            let ts = TimeSpan.FromMilliseconds(ms)
            let res1 = dt.Add(ts) |> thatYearSeconds
            let res2 = (dt + ts) |> thatYearSeconds
            equal true (res1 = res2)
            equal expected res1
        test 1000. 21945601.0
        test -1000. 21945599.0
        test 0. 21945600.0

    testCase "DateTime Subtraction with TimeSpan works" <| fun () ->
        let test ms expected =
            let dt = DateTime(2014,9,12,0,0,0,DateTimeKind.Utc)
            let ts = TimeSpan.FromMilliseconds(ms)
            let res1 = dt.Subtract(ts) |> thatYearSeconds
            let res2 = (dt - ts) |> thatYearSeconds
            equal true (res1 = res2)
            equal expected res1
        test 1000. 21945599.0
        test -1000. 21945601.0
        test 0. 21945600.0

    testCase "DateTime Subtraction with DateTime works" <| fun () ->
        let test ms expected =
            let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
            let dt2 = dt1.AddMilliseconds(ms)
            let res1 = dt1.Subtract(dt2).TotalSeconds
            let res2 = (dt1 - dt2).TotalSeconds
            equal true (res1 = res2)
            equal expected res1
        test 1000. -1.0
        test -1000. 1.0
        test 0. 0.0

    testCase "DateTime Comparison works" <| fun () ->
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

    testCase "DateTime GreaterThan works" <| fun () ->
        let test ms expected =
            let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 > dt2 |> equal expected
        test 1000. false
        test -1000. true
        test 0. false

    testCase "DateTime LessThan works" <| fun () ->
        let test ms expected =
            let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 < dt2 |> equal expected
        test 1000. true
        test -1000. false
        test 0. false

    testCase "DateTime Equality works" <| fun () ->
        let test ms expected =
            let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 = dt2 |> equal expected
        test 1000. false
        test -1000. false
        test 0. true

    testCase "DateTime Inequality works" <| fun () ->
        let test ms expected =
            let dt1 = DateTime(2014, 10, 9, 13, 23, 30, 234, DateTimeKind.Utc)
            let dt2 = dt1.AddMilliseconds(ms)
            dt1 <> dt2 |> equal expected
        test 1000. true
        test -1000. true
        test 0. false

    testCase "DateTime TimeOfDay works" <| fun () ->
        let d = System.DateTime(2014, 10, 9, 13, 23, 30, 1, System.DateTimeKind.Utc)
        let t = d.TimeOfDay

        t |> equal (TimeSpan(0, 13, 23, 30, 1))

    testCaseAsync "Timer with AutoReset = true works" <| fun () ->
        async {
            let res = ref 0
            let t = new Timers.Timer(50.)
            t.Elapsed.Add(fun ev -> res := !res + 5)
            t.Start()
            do! Async.Sleep 125
            t.Stop()
            do! Async.Sleep 50
            equal 10 !res
        }

    testCaseAsync "Timer with AutoReset = false works" <| fun () ->
        async {
            let res = ref 0
            let t = new Timers.Timer()
            t.Elapsed.Add(fun ev -> res := !res + 5)
            t.AutoReset <- false
            t.Interval <- 25.
            t.Enabled <- true
            do! Async.Sleep 100
            equal 5 !res
        }

    testCaseAsync "Timer.Elapsed.Subscribe works" <| fun () ->
        async {
            let res = ref 0
            let t = new Timers.Timer(50.)
            let disp = t.Elapsed.Subscribe(fun ev -> res := !res + 5)
            t.Start()
            do! Async.Sleep 125
            disp.Dispose()
            do! Async.Sleep 50
            equal 10 !res
            t.Stop()
        }

    testCaseAsync "Assigning an event to a variable works" <| fun () -> // See #1863
        let createTimerAndObservable timerInterval =
            // setup a timer
            let timer = new System.Timers.Timer(float timerInterval)
            timer.AutoReset <- true
            // events are automatically IObservable
            let observable = timer.Elapsed
            // return an async task
            let task = async {
                timer.Start()
                do! Async.Sleep 200
                timer.Stop()
            }
            // return a async task and the observable
            (task,observable)
        // create the timer and the corresponding observable
        let basicTimer2 , timerEventStream = createTimerAndObservable 50

        let mutable acc = 1
        // register that everytime something happens on the
        // event stream, print the time.
        timerEventStream |> Observable.subscribe (fun _ ->
            acc <- acc + 1) |>ignore

        async {
            do! basicTimer2
            // printfn "%i" acc
            acc > 2 |> equal true
        }

    // In regions with daylight saving time, 20/10/2019 will have different timezone
    // offset than 29/10/2019
    testCase "Adding days to a local date works even if daylight saving time changes" <| fun () ->
        let dt = DateTime(2019, 10, 20, 0, 0, 0, DateTimeKind.Local)
        dt.AddDays(9.).Day |> equal 29
  ]
