module Fable.Tests.Compiler.CompilerMessages

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let private compile source = Compiler.Cached.compile Compiler.Settings.standard source

let tests =
  testList "Compiler Messages" [
    testCase "Compile Console.WriteLine" <| fun _ ->
      let source = "Console.WriteLine(\"Hello World\")"
      compile source
      |> Assert.Is.success
      |> ignore

    testCase "Compile printfn" <| fun _ ->
      let source = "printfn \"Hello %s\" \"World\""
      compile source
      |> Assert.Is.success
      |> ignore

    testCase "Compiling printfn with %s placeholder and int argument results in error" <| fun _ ->
      let source = "printfn \"Hello %s\" 42"
      compile source
      |> Assert.Is.Single.error
      |> ignore

    testCase "Compiling incomplete pattern match results in warning" <| fun _ ->
      let source = "match None with | Some n -> 42 |> ignore" // without `ignore`: Warning: Result of Expression is implicitly ignored
      compile source
      |> Assert.Is.Single.warning
      |> ignore

    testCase "Compiling printfn with %s placeholder and int argument results in specific error" <| fun _ ->
      let source = "printfn \"Hello %s\" 42"
      compile source
      |> Assert.Exists.errorWith "This expression was expected to have type"
      |> ignore

    testCase "Compiling incomplete pattern match results in specific warning" <| fun _ ->
      let source = "match None with | Some n -> 42"
      compile source
      |> Assert.Is.success
      |> Assert.Exists.warningWith "Incomplete pattern matches on this expression"
      |> Assert.Exists.warningWith "The result of this expression has type 'int' and is implicitly ignored."
      |> ignore

    testCase "Setting a non-property member in jsOptions results in specific error" <| fun _ ->
      let source =
        """
open Fable.Core.JsInterop

type Response =
    abstract fn: int -> int
    abstract prop: bool with get, set

let res = jsOptions<Response> (fun o -> o.fn <- (fun i -> i))
"""
      compile source
      |> Assert.Exists.errorWith "Cannot set a non-property member in 'jsOptions'"
      |> ignore

    testCase "Setting only settable properties in jsOptions succeeds" <| fun _ ->
      let source =
        """
open Fable.Core.JsInterop

type Response =
    abstract fnProp: (int -> int) with get, set
    abstract prop: bool with get, set

let res = jsOptions<Response> (fun o ->
    o.fnProp <- (fun i -> i)
    o.prop <- false)
"""
      compile source
      |> Assert.Is.success
      |> ignore

    testCase "Inline function referencing private value emits error" <| fun _ ->
      let source =
        """
let private x = 1
let inline y () = x
let z = y ()
"""
      compile source
      |> Assert.Exists.errorWith "was marked inline but its implementation makes use of an internal or private function"
      |> ignore

    testCase "Inline function referencing non-private value succeeds" <| fun _ ->
      let source =
        """
let x = 1
let inline y () = x
let z = y ()
"""
      compile source
      |> Assert.Is.success
      |> ignore

    testCase "Duplicate attached member names emit a warning" <| fun _ ->
      let source =
        """
open Fable.Core

[<AttachMembers>]
type MyClass() =
    member _.Foo(x: int) = x
    member _.Foo(x: string) = x.Length
"""
      compile source
      |> Assert.Exists.warningWith "Overloads are not supported when using [<AttachMembers>]"
      |> ignore

    testCase "Getter and setter pair with same name does not emit a warning" <| fun _ ->
      let source =
        """
open Fable.Core

[<AttachMembers>]
type MyClass() =
    let mutable _x = 0
    member _.Value with get() = _x and set(v) = _x <- v
"""
      compile source
      |> Assert.Is.success
      |> ignore

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

    testCase "The formatted output carries the warning code" <| fun _ ->
      // Without this there is no way to discover which code to put in a directive.
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"""
      let formatted =
        compile source
        |> List.filter (fun log -> log.Code = Some "FABLE0100")
        |> List.map (Fable.Cli.Main.Util.formatLog Compiler.Cached.projDir)

      match formatted with
      | [] -> failwith "Expected a FABLE0100 warning"
      | messages -> equal true (messages |> List.forall (fun m -> m.Contains "warning FABLE FABLE0100:"))
  ]
