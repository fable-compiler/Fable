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
      // Regression test for a real bug: StartsWith and EndsWith both raise the same logical
      // "CultureInfo argument is ignored" warning from two separate call sites in
      // Replacements.fs, sharing WarningCodes.CultureInfoIgnored. Both must be suppressible
      // by one code, and (in Python's Replacements.fs) this used to have no code at all.
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0001
"abc".EndsWith("c", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0001
"""
      compile source
      |> Assert.Are.warnings 0
      |> ignore

    testCase "fable-disable-line suppresses a warning on the same line" <| fun _ ->
      let source =
        """
open System.Globalization
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore // fable-disable-line FABLE0001
"""
      compile source
      |> Assert.Are.warnings 0
      |> ignore

    testCase "fable-disable-next-line suppresses a warning on the following line" <| fun _ ->
      let source =
        """
open System.Globalization
// fable-disable-next-line FABLE0001
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Are.warnings 0
      |> ignore

    testCase "fable-disable/fable-enable suppresses warnings in a block" <| fun _ ->
      let source =
        """
open System.Globalization
// fable-disable FABLE0001
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"abc".EndsWith("c", true, CultureInfo.InvariantCulture) |> ignore
// fable-enable FABLE0001
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
let s = "// fable-disable-line FABLE0001"
"abc".StartsWith("a", true, CultureInfo.InvariantCulture) |> ignore
"""
      compile source
      |> Assert.Exists.warningWith "CultureInfo argument is ignored"
      |> ignore
  ]
