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
Double.Parse("1.5", NumberStyles.Currency, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0103"
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
