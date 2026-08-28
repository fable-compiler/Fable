module Fable.Tests.Compiler.CompilerMessages

open Fable.Core
open Util.Testing
open Fable.Tests.Compiler.Util
open Fable.Tests.Compiler.Util.Compiler

let private compile source = Compiler.Cached.compile Compiler.Settings.standard source

let private compileWithLibrary library source =
  Compiler.Cached.compileWithLibrary Compiler.Settings.standard library source

let private compileWithSignedLibrary signature library source =
  Compiler.Cached.compileWithSignedLibrary Compiler.Settings.standard signature library source

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

    testCase "Referencing a member from a package without F# sources emits an error" <| fun _ ->
      let source =
        """
let version = Semver.SemVersion.Parse("1.0.0", 1024)
"""
      compile source
      |> Assert.Exists.errorWith "Cannot reference member from .dll reference, Fable packages must include F# sources"
      |> ignore

    testCase "Private inline function referencing private value succeeds" <| fun _ ->
      let source =
        """
let private x = 1
let inline private y () = x
let z = y ()
"""
      compile source
      |> Assert.Is.success
      |> ignore

    testCase "Private inline function referencing private function succeeds" <| fun _ ->
      let source =
        """
let private add a b = a + b
let inline private addOne x = add x 1
let z = addOne 41
"""
      compile source
      |> Assert.Is.success
      |> ignore

    testCase "Private inline function in nested module referencing private value succeeds" <| fun _ ->
      let source =
        """
module Nested =
    let private x = 1
    let inline private y () = x
    let z = y ()
"""
      compile source
      |> Assert.Is.success
      |> ignore

    testCase "Private inline function referencing private value errors when inlined in another file" <| fun _ ->
      let library =
        """
module Library

let private x = 1
let inline private y () = x
let inline z () = y ()
"""
      let source = "let res = Library.z ()"
      compileWithLibrary library source
      |> Assert.Are.errors 1
      |> Assert.Exists.errorWith "was marked inline but its implementation makes use of an internal or private function"
      |> ignore

    testCase "Inline function referencing private value is reported once, not once per call site" <| fun _ ->
      let library =
        """
module Library

let private x = 1
let inline w () = x
let inline z () = w ()
"""
      let source =
        """
let a = Library.z ()
let b = Library.z ()
"""
      compileWithLibrary library source
      |> Assert.Are.errors 1
      |> Assert.Exists.errorWith "was marked inline but its implementation makes use of an internal or private function"
      |> ignore

    testCase "Inline function referencing non-private value from another file succeeds" <| fun _ ->
      let library =
        """
module Library

let x = 1
let internal w = 2
let inline private y () = x + w
let inline z () = y ()
"""
      let source = "let res = Library.z ()"
      compileWithLibrary library source
      |> Assert.Is.success
      |> ignore

    testCase "Inline function referencing value hidden by a signature file errors when inlined in another file" <| fun _ ->
      let signature =
        """
module Library

val inline z: unit -> int
"""
      let library =
        """
module Library

let x = 1
let inline z () = x
"""
      let source = "let res = Library.z ()"
      compileWithSignedLibrary signature library source
      |> Assert.Are.errors 1
      |> Assert.Exists.errorWith "was marked inline but its implementation makes use of an internal or private function"
      |> ignore

    testCase "Inline function hidden by a signature file referencing hidden value succeeds" <| fun _ ->
      let signature =
        """
module Library

val res: int
"""
      let library =
        """
module Library

let x = 1
let inline z () = x
let res = z ()
"""
      let source = "let res = Library.res"
      compileWithSignedLibrary signature library source
      |> Assert.Is.success
      |> ignore

    testCase "Inline function referencing value exposed by a signature file succeeds" <| fun _ ->
      let signature =
        """
module Library

val x: int
val inline z: unit -> int
"""
      let library =
        """
module Library

let x = 1
let inline z () = x
"""
      let source = "let res = Library.z ()"
      compileWithSignedLibrary signature library source
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
  ]
