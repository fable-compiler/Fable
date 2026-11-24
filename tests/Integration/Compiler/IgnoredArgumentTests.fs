module Fable.Tests.Compiler.IgnoredArgument

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let private compile source = Compiler.Cached.compile Compiler.Settings.standard source

(*
    Verify that warnings are generated when needed, and skipped in the scenario where Fable
    has the same behavior as .NET.

    The later makes the logs less verbose and warnings more meaningful
*)

let tests =
  testList "Ignored Arguments" [
    testCase "Discarding a format provider on a date parse is reported" <| fun _ ->
      // JS, Dart and Rust used to drop this argument silently while Python warned. Aligning them
      // is a behaviour change: code that compiled quietly now raises FABLE0102.
      let source =
        """
open System
open System.Globalization
DateTime.Parse("2026-01-01", CultureInfo.GetCultureInfo "fr-FR") |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0102"
      |> ignore

    testCase "Passing InvariantCulture to a parse is not reported" <| fun _ ->
      // Fable parses with a fixed culture-independent implementation, so InvariantCulture asks
      // for exactly what it gets. Warning here would fire on every correct call - and does, if
      // the exemption is removed: 96 sites across the repo's own test suites.
      let source =
        """
open System
open System.Globalization
DateTime.Parse("2026-01-01", CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0102"
      |> ignore

    testCase "A discarded DateTimeStyles is reported even when the culture is exempt" <| fun _ ->
      // Two codes rather than one bundled message: the style is still discarded regardless of
      // which culture was passed, so exempting the culture must not silence it.
      let source =
        """
open System
open System.Globalization
DateTime.Parse("2026-01-01", CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0104"
      |> Assert.Code.noWarning "FABLE0102"
      |> ignore

    testCase "DateTimeStyles.None is not reported" <| fun _ ->
      // `None` means "no special handling", which is what Fable does - same exemption as
      // InvariantCulture. Every one of the 12 sites in tests/Js passes exactly this.
      let source =
        """
open System
open System.Globalization
DateTime.Parse("2026-01-01", CultureInfo.InvariantCulture, DateTimeStyles.None) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0102"
      |> Assert.Code.noWarning "FABLE0104"
      |> ignore

    testCase "A numeric parse given InvariantCulture is not reported" <| fun _ ->
      let source =
        """
open System
open System.Globalization
Double.Parse("10.5", CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0102"
      |> ignore

    testCase "A numeric parse given a real culture is reported" <| fun _ ->
      let source =
        """
open System
open System.Globalization
Double.Parse("10.5", CultureInfo.GetCultureInfo "fr-FR") |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0102"
      |> ignore

    testCase "A real culture and a style are both reported" <| fun _ ->
      let source =
        """
open System
open System.Globalization
DateTime.Parse("2026-01-01", CultureInfo.GetCultureInfo "fr-FR", DateTimeStyles.AssumeUniversal) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0102"
      |> Assert.Code.warning "FABLE0104"
      |> ignore

    testCase "A date parse with no extra argument discards nothing and is silent" <| fun _ ->
      let source =
        """
open System
DateTime.Parse("2026-01-01") |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0102"
      |> Assert.Code.noWarning "FABLE0104"
      |> ignore

    testCase "A discarded NumberStyles is reported" <| fun _ ->
      let source =
        """
open System
open System.Globalization
Double.Parse("1", NumberStyles.HexNumber, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0103"
      |> ignore

    testCase "A supported NumberStyles on an integer is not reported" <| fun _ ->
      let source =
        """
open System
open System.Globalization
Int32.Parse("1", NumberStyles.Currency, CultureInfo.InvariantCulture) |> ignore
Int64.Parse("1", NumberStyles.Any, CultureInfo.InvariantCulture) |> ignore
Byte.Parse("1", NumberStyles.BinaryNumber, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0103"
      |> ignore

    testCase "A NumberStyles an integer rejects at run time is not reported" <| fun _ ->
      let source =
        """
open System
open System.Globalization
Int32.Parse("1", enum<NumberStyles> 519, CultureInfo.InvariantCulture) |> ignore
Int64.Parse("1", enum<NumberStyles> 0x800, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0103"
      |> ignore

    testCase "A supported NumberStyles on a float is not reported" <| fun _ ->
      let source =
        """
open System
open System.Globalization
Double.Parse("1.5", NumberStyles.Float ||| NumberStyles.AllowParentheses, CultureInfo.InvariantCulture) |> ignore
Decimal.Parse("1.5", NumberStyles.Number, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0103"
      |> ignore

    testCase "A TimeSpan built with microseconds is reported" <| fun _ ->
      // The runtime representation only carries milliseconds, so the finer component is dropped.
      let source =
        """
open System
TimeSpan.FromMilliseconds(1L, 500L) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0105"
      |> ignore

    testCase "A TimeSpan with no microseconds is silent" <| fun _ ->
      let source =
        """
open System
TimeSpan.FromMilliseconds(1L, 0L) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0105"
      |> ignore

    testCase "DateTime from non-constant ticks reports the dropped precision" <| fun _ ->
      let source =
        """
open System
let f (ticks: int64) = DateTime(ticks)
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "DateTime from constant ticks with a sub-millisecond part reports the dropped precision" <| fun _ ->
      let source =
        """
open System
DateTime(630822816000012345L, DateTimeKind.Utc) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "DateTime with non-zero microseconds reports the dropped precision" <| fun _ ->
      let source =
        """
open System
DateTime(2020, 1, 1, 0, 0, 0, 0, 250) |> ignore
DateTime(2020, 1, 1, 0, 0, 0, 0, 250, DateTimeKind.Utc) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "DateTime.AddTicks with non-constant ticks reports the dropped precision" <| fun _ ->
      let source =
        """
open System
let f (d: DateTime) (ticks: int64) = d.AddTicks ticks
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "DateTimeOffset from non-constant ticks reports the dropped precision" <| fun _ ->
      let source =
        """
open System
let f (ticks: int64) = DateTimeOffset(ticks, TimeSpan.Zero)
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "DateTimeOffset with non-constant microseconds reports the dropped precision" <| fun _ ->
      let source =
        """
open System
let f (us: int) = DateTimeOffset(2020, 1, 1, 0, 0, 0, 0, us, TimeSpan.Zero)
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "DateTimeOffset.AddTicks with a sub-millisecond part reports the dropped precision" <| fun _ ->
      let source =
        """
open System
let f (d: DateTimeOffset) = d.AddTicks 5L
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "TimeSpan from non-constant ticks reports the dropped precision" <| fun _ ->
      let source =
        """
open System
let f (ticks: int64) = TimeSpan ticks, TimeSpan.FromTicks ticks
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "TimeSpan with non-zero microseconds reports the dropped precision" <| fun _ ->
      let source =
        """
open System
TimeSpan(0, 0, 0, 0, 1, 250) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "TimeOnly from non-constant ticks reports the dropped precision" <| fun _ ->
      let source =
        """
open System
let f (ticks: int64) = TimeOnly ticks
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "TimeOnly with non-zero microseconds reports the dropped precision" <| fun _ ->
      let source =
        """
open System
TimeOnly(1, 2, 3, 4, 250) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0122"
      |> ignore

    testCase "Date and time values with no sub-millisecond part are silent" <| fun _ ->
      let source =
        """
open System
DateTime(630822816000010000L) |> ignore
DateTime(630822816000010000L, DateTimeKind.Utc) |> ignore
DateTime(2020, 1, 1, 0, 0, 0, 0, 0) |> ignore
DateTime(2020, 1, 1, 0, 0, 0, 0, 0, DateTimeKind.Utc) |> ignore
DateTime(2020, 1, 1, 0, 0, 0, 5, DateTimeKind.Utc) |> ignore
DateTime(2020, 1, 1).AddTicks(10000L) |> ignore
DateTimeOffset(630822816000010000L, TimeSpan.Zero) |> ignore
DateTimeOffset(2020, 1, 1, 0, 0, 0, 0, 0, TimeSpan.Zero) |> ignore
DateTimeOffset(2020, 1, 1, 0, 0, 0, 5, TimeSpan.Zero) |> ignore
DateTimeOffset(2020, 1, 1, 0, 0, 0, TimeSpan.Zero).AddTicks(-20000L) |> ignore
TimeSpan(10000L) |> ignore
TimeSpan.FromTicks(0L) |> ignore
TimeSpan(0, 0, 0, 0, 1, 0) |> ignore
TimeSpan(1, 2, 3) |> ignore
TimeOnly(10000L) |> ignore
TimeOnly(1, 2, 3, 4, 0) |> ignore
TimeOnly(1, 2, 3, 4) |> ignore
let f (ms: int) = DateTime(2020, 1, 1, 0, 0, 0, ms), TimeSpan(0, 0, 0, 0, ms), TimeOnly(1, 2, 3, ms)
let g (d: DateTime) (o: DateTimeOffset) = DateTime(d.Ticks, DateTimeKind.Utc), DateTimeOffset(o.Ticks, o.Offset)
"""
      compile source
      |> Assert.Code.noWarning "FABLE0122"
      |> ignore

    testCase "A private-representation flag on FSharpType is reported" <| fun _ ->
      let source =
        """
open Microsoft.FSharp.Reflection
FSharpType.IsUnion(typeof<int option>, true) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0106"
      |> ignore

    testCase "A private-representation flag on GetRecordFields is reported" <| fun _ ->
      // This one dropped the flag silently while its seven siblings warned, all in one function.
      let source =
        """
open Microsoft.FSharp.Reflection
type R = { A: int }
FSharpType.GetRecordFields(typeof<R>, true) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0106"
      |> ignore

    testCase "FSharpType without the flag is silent" <| fun _ ->
      let source =
        """
open Microsoft.FSharp.Reflection
FSharpType.IsUnion(typeof<int option>) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0106"
      |> ignore

    testCase "Base64 offset and length arguments are reported" <| fun _ ->
      // The slice is silently encoded whole, so this is a wrong value rather than a formatting nit.
      let source =
        """
open System
Convert.ToBase64String([| 1uy; 2uy; 3uy; 4uy |], 0, 2) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0107"
      |> ignore

    testCase "Base64 with only the array is silent" <| fun _ ->
      let source =
        """
open System
Convert.ToBase64String([| 1uy; 2uy; 3uy; 4uy |]) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0107"
      |> ignore
  ]
