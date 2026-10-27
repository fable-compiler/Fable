module Fable.Tests.Compiler.RegexOptions

open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let private compile source = Compiler.Cached.compile Compiler.Settings.standard source

let tests =
  testList "Regex Options" [
    testCase "An unsupported option on the constructor is a compile error" <| fun _ ->
      let source =
        """
open System.Text.RegularExpressions

let r = Regex("a", RegexOptions.RightToLeft)
"""
      compile source
      |> Assert.Is.error
      |> Assert.Exists.errorWith "RegexOptions.RightToLeft is not supported"
      |> ignore

    testCase "Only the unsupported options of a combination are reported" <| fun _ ->
      let source =
        """
open System.Text.RegularExpressions

let found = Regex.IsMatch("abc", "b", RegexOptions.IgnoreCase ||| RegexOptions.ExplicitCapture ||| RegexOptions.CultureInvariant)
"""
      compile source
      |> Assert.Exists.errorWith "RegexOptions.ExplicitCapture, RegexOptions.CultureInvariant is not supported"
      |> ignore

    testCase "Options that only affect performance compile without a diagnostic" <| fun _ ->
      let source =
        """
open System.Text.RegularExpressions

let r = Regex("a", RegexOptions.Compiled ||| RegexOptions.NonBacktracking ||| RegexOptions.IgnoreCase)
let parts = Regex.Split("a1b", "\\d", RegexOptions.ECMAScript)
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "Options that are only known at run time are left to the runtime check" <| fun _ ->
      let source =
        """
open System.Text.RegularExpressions

let make (options: RegexOptions) = Regex("a", options)
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore
  ]
