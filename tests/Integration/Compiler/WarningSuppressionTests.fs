module Fable.Tests.Compiler.WarningSuppression

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let private compile source = Compiler.Cached.compile Compiler.Settings.standard source

/// Exercises the `// fable-disable` machinery itself - directive parsing, block scoping, which
/// warnings it can reach, and the FABLE0001-band reports about the directives. FABLE0100 stands
/// in as the subject warning throughout; what is under test is the mechanism, not that code.
let tests =
  testList "Warning Suppression" [
    testCase "CultureInfo argument warning is not suppressed by default" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Exists.warningWith "CultureInfo argument is ignored"
      |> ignore

    testCase "The same code covers both StartsWith and EndsWith call sites" <| fun _ ->
      // StartsWith and EndsWith raise the same logical "CultureInfo argument is ignored" warning
      // from two separate call sites in Replacements.fs, sharing WarningCodes.CultureInfoIgnored.
      // One code must suppress both, otherwise the registry has failed at its only job.
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0100
"abc".EndsWith("c", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0100
"""
      compile source
      |> Assert.Are.warnings 0
      |> ignore

    testCase "fable-disable-line suppresses a warning on the same line" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0100
"""
      compile source
      |> Assert.Are.warnings 0
      |> ignore

    testCase "fable-disable-next-line suppresses a warning on the following line" <| fun _ ->
      let source =
        """
open System.Globalization
// fable-disable-next-line FABLE0100
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Are.warnings 0
      |> ignore

    testCase "fable-disable/fable-enable suppresses warnings in a block" <| fun _ ->
      let source =
        """
open System.Globalization
// fable-disable FABLE0100
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"abc".EndsWith("c", true, CultureInfo.InvariantCulture) |> ignore
// fable-enable FABLE0100
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Are.warnings 1
      |> ignore

    testCase "A mismatched code does not suppress the warning" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line SOME_OTHER_CODE
"""
      compile source
      |> Assert.Exists.warningWith "CultureInfo argument is ignored"
      |> ignore

    testCase "A bare fable-disable-line suppresses regardless of code" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line
"""
      compile source
      |> Assert.Are.warnings 0
      |> ignore

    testCase "A string literal that looks like a directive is not treated as one" <| fun _ ->
      let source =
        """
open System.Globalization
let s = "// fable-disable-line FABLE0100"
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Exists.warningWith "CultureInfo argument is ignored"
      |> ignore

    testCase "A directive inside a #if FABLE_COMPILER block is honoured" <| fun _ ->
      let source =
        """
open System.Globalization
#if FABLE_COMPILER
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0100
#endif
"""
      compile source
      |> Assert.Code.noWarning "FABLE0100"
      |> ignore

    testCase "A warning inside a #if FABLE_COMPILER block still fires without a directive" <| fun _ ->
      // Guards the test above from passing vacuously because the block was compiled out.
      let source =
        """
open System.Globalization
#if FABLE_COMPILER
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
#endif
"""
      compile source
      |> Assert.Code.warning "FABLE0100"
      |> ignore

    testCase "Errors are never suppressed, not even by a blanket fable-disable" <| fun _ ->
      let source =
        """
open Fable.Core.JsInterop

type Response =
    abstract fn: int -> int
    abstract prop: bool with get, set

// fable-disable
let res = jsOptions<Response> (fun o -> o.fn <- (fun i -> i))
"""
      compile source
      |> Assert.Exists.errorWith "Cannot set a non-property member in 'jsOptions'"
      |> ignore

    testCase "A directive written as a block comment is honoured" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore (* fable-disable-line FABLE0100 *)
"""
      compile source
      |> Assert.Code.noWarning "FABLE0100"
      |> ignore

    testCase "A trailing directive suppresses a warning spanning several lines" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith(
    "a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0100
"""
      compile source
      |> Assert.Code.noWarning "FABLE0100"
      |> ignore

    testCase "A fable-disable block with no fable-enable runs to the end of the file" <| fun _ ->
      let source =
        """
open System.Globalization
// fable-disable FABLE0100
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"abc".EndsWith("c", true, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0100"
      |> ignore

    testCase "A colon separator and a lower-case code are accepted" <| fun _ ->
      // What people coming from `# noqa: E501` and `@ts-ignore` will write.
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line: fable0100
"""
      compile source
      |> Assert.Code.noWarning "FABLE0100"
      |> ignore

    testCase "A justification after -- is not parsed as codes" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0100 -- culture is irrelevant here
"""
      compile source
      |> Assert.Code.noWarning "FABLE0100"
      |> Assert.Code.noWarning "FABLE0001"
      |> ignore

    testCase "A typo'd code is reported instead of silently suppressing nothing" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABEL0001
"""
      compile source
      |> Assert.Code.warning "FABLE0001"
      |> Assert.Code.warning "FABLE0100"
      // The typo report is the actionable one; don't pile "and it's unused" on top of it.
      |> Assert.Code.noWarning "FABLE0002"
      |> ignore

    testCase "A directive that suppresses nothing is reported as unused" <| fun _ ->
      let source =
        """
open System.Globalization
// fable-disable-next-line FABLE0100
let answer = 42
"""
      compile source
      |> Assert.Code.warning "FABLE0002"
      |> ignore

    testCase "A directive that does its job is not reported as unused" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0100
"""
      compile source
      |> Assert.Code.noWarning "FABLE0002"
      |> ignore

    testCase "A fable-disable block with no codes is reported" <| fun _ ->
      // It would otherwise silence every Fable warning to the end of the file, unnoticed.
      let source =
        """
open System.Globalization
// fable-disable
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Code.warning "FABLE0003"
      |> Assert.Code.noWarning "FABLE0100"
      |> ignore

    testCase "Directive warnings cannot be suppressed by a directive" <| fun _ ->
      // FABLE0001-0099 flag comment text that is wrong to keep, so the only correct answer is to
      // edit it. Silencing "this directive is broken" with another directive is self-defeating.
      let source =
        """
open System.Globalization
// fable-disable-next-line FABLE0002
// fable-disable-next-line FABLE0100
let answer = 42
"""
      compile source
      |> Assert.Code.warning "FABLE0002"
      |> ignore

    testCase "A blanket fable-disable cannot suppress its own report" <| fun _ ->
      let source =
        """
open System.Globalization
// fable-disable
let answer = 42
"""
      compile source
      |> Assert.Code.warning "FABLE0003"
      |> ignore

    testCase "A word merely starting with a directive name is not a directive" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disabled for now
"""
      compile source
      |> Assert.Code.warning "FABLE0100"
      |> ignore

    testCase "Directives are found in CRLF sources" <| fun _ ->
      let source =
        [ ""
          "open System.Globalization"
          "\"abc\".StartsWith(\"a\", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0100"
          "" ]
        |> String.concat "\r\n"

      compile source
      |> Assert.Code.noWarning "FABLE0100"
      |> ignore

    testCase "A warning from an inlined function is suppressed at its definition" <| fun _ ->
      // The warning is attributed to the file the inline function is *defined* in, so that is
      // where the directive has to go - the call site can't suppress it.
      let source =
        """
open System.Globalization
let inline startsWithCulture (s: string) =
    s.StartsWith("a", true, CultureInfo.InvariantCulture) // fable-disable-line FABLE0100

startsWithCulture "abc" |> ignore
"""
      compile source
      |> Assert.Code.noWarning "FABLE0100"
      |> ignore

    testCase "A directive at the call site does not suppress an inlined function's warning" <| fun _ ->
      let source =
        """
open System.Globalization
let inline startsWithCulture (s: string) =
    s.StartsWith("a", true, CultureInfo.InvariantCulture)

startsWithCulture "abc" |> ignore // fable-disable-line FABLE0100
"""
      compile source
      |> Assert.Code.warning "FABLE0100"
      |> ignore
  ]
