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

    testCase "CaseTest outside a TypeScriptTaggedUnion emits warning" <| fun _ ->
      let source =
        """
open Fable.Core

[<Erase>]
type Id =
    | [<CaseTest("$0.startsWith('n:')")>] Name of string
    | [<CaseTest("$0.startsWith('s:')")>] Slug of string

let isName (id: Id) = match id with Name _ -> true | Slug _ -> false
"""
      compile source
      |> Assert.Exists.warningWith "is ignored: it is only supported on TypeScriptTaggedUnion cases"
      |> ignore

    testCase "CaseTest in a TypeScriptTaggedUnion does not emit warning" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("state")>]
type Terminal =
    | [<CompiledName("done"); CaseTest("'value' in $0")>] Done of value: float
    | [<CompiledName("done"); CaseTest("'error' in $0")>] DoneError of error: string

let isError (t: Terminal) = match t with DoneError _ -> true | Done _ -> false
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "Type test against a Pojo class emits warning" <| fun _ ->
      let source =
        """
open Fable.Core

[<JS.Pojo>]
type Options(name: string) =
    member val name = name

let isOptions (o: obj) = match o with :? Options -> true | _ -> false
"""
      compile source
      |> Assert.Exists.warningWith "Cannot type test (evals to false): Program.Options (pojo)"
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
      | messages ->
        equal true (messages |> List.forall (fun m -> m.Contains "warning FABLE0100:"))
        equal false (messages |> List.exists (fun m -> m.Contains "FABLE FABLE"))

    testCase "TypeScriptTaggedUnion cases sharing a tag compile when their fields tell them apart" <| fun _ ->
      let source =
        """
open Fable.Core

type Payload(x: int) =
    member _.X = x

[<TypeScriptTaggedUnion("state")>]
type Terminal =
    | [<CompiledName("done")>] Done of value: float
    | [<CompiledName("done")>] DoneError of error: string
    | [<CompiledName("log")>] LogString of message: string
    | [<CompiledName("log")>] LogPayload of message: Payload

let describe (t: Terminal) =
    match t with
    | DoneError _ -> "error"
    | LogString _ -> "string"
    | LogPayload _ -> "payload"
    | Done _ -> "done"
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag emit error when their fields cannot tell them apart" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("state")>]
type Result =
    | [<CompiledName("done")>] Ok of ok: bool
    | [<CompiledName("done")>] Failed of ok: bool

let isFailed (r: Result) = match r with Failed _ -> true | Ok _ -> false
"""
      compile source
      |> Assert.Exists.errorWith "of 'Result' share the tag value \"done\" and have the same fields. Add a CaseTest attribute to"
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag compile when the other case has a CaseTest" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("state")>]
type Result =
    | [<CompiledName("done")>] Ok of ok: bool
    | [<CompiledName("done"); CaseTest("$0.ok === false")>] Failed of ok: bool

let isOk (r: Result) = r.IsOk
let isFailed (r: Result) = match r with Failed _ -> true | Ok _ -> false
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag emit error when more than one case has no CaseTest" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("state")>]
type Job =
    | [<CompiledName("run"); CaseTest("$0.kind === 'build'")>] Build of kind: string
    | [<CompiledName("run")>] Test of kind: string
    | [<CompiledName("run")>] Other of kind: string

let isOther (j: Job) = j.IsOther
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Other' and 'Test' of 'Job' share the tag value \"run\" and have the same fields. Add a CaseTest attribute to 'Other' or 'Test'."
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag emit error when one field type includes the other" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("state")>]
type Log =
    | [<CompiledName("log")>] LogString of message: string
    | [<CompiledName("log")>] LogObject of message: obj

let isObject (l: Log) = l.IsLogObject
let isString (l: Log) = l.IsLogString
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'LogObject' and 'LogString' of 'Log' share the tag value \"log\" and cannot be told apart at runtime: field 'message' has type 'obj', which can hold any value. Add a CaseTest attribute to 'LogObject' or 'LogString'."
      |> Assert.Exists.errorWith "Cases 'LogString' and 'LogObject' of 'Log' share the tag value \"log\" and cannot be told apart at runtime: field 'message' has type 'obj', which can hold any value. Add a CaseTest attribute to 'LogString' or 'LogObject'."
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag emit error when one field type is a base class of the other" <| fun _ ->
      let source =
        """
open Fable.Core

type Animal() = class end
type Dog() = inherit Animal()

[<TypeScriptTaggedUnion("kind")>]
type Pet =
    | [<CompiledName("pet")>] PetAnimal of pet: Animal
    | [<CompiledName("pet")>] PetDog of pet: Dog

let isAnimal (p: Pet) = p.IsPetAnimal
let isDog (p: Pet) = p.IsPetDog
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'PetAnimal' and 'PetDog' of 'Pet' share the tag value \"pet\" and cannot be told apart at runtime: field 'pet' has types 'Animal' and 'Dog', and 'Dog' is a subtype of 'Animal'. Add a CaseTest attribute to 'PetAnimal' or 'PetDog'."
      |> Assert.Exists.errorWith "Cases 'PetDog' and 'PetAnimal' of 'Pet' share the tag value \"pet\" and cannot be told apart at runtime: field 'pet' has types 'Dog' and 'Animal', and 'Dog' is a subtype of 'Animal'. Add a CaseTest attribute to 'PetDog' or 'PetAnimal'."
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag compile when a CaseTest narrows a base class field" <| fun _ ->
      let source =
        """
open Fable.Core

type Animal() = class end

[<AttachMembers>]
type Dog() =
    inherit Animal()
    member _.Bark() = "woof"

[<TypeScriptTaggedUnion("kind")>]
type Pet =
    | [<CompiledName("pet")>] PetAnimal of pet: Animal
    | [<CompiledName("pet"); CaseTest("'Bark' in $0.pet")>] PetDog of pet: Dog

let isAnimal (p: Pet) = p.IsPetAnimal
let isDog (p: Pet) = p.IsPetDog
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag emit error when only a Pojo field type tells them apart" <| fun _ ->
      let source =
        """
open Fable.Core

[<JS.Pojo>]
type Options(name: string) =
    member val name = name

[<TypeScriptTaggedUnion("state")>]
type Log =
    | [<CompiledName("log")>] LogString of message: string
    | [<CompiledName("log")>] LogOptions of message: Options

let isOptions (l: Log) = l.IsLogOptions
let isString (l: Log) = l.IsLogString
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'LogOptions' and 'LogString' of 'Log' share the tag value \"log\" and cannot be told apart at runtime: field 'message' has type 'Options', a Pojo that is a plain object at runtime. Add a CaseTest attribute to 'LogOptions' or 'LogString'."
      |> Assert.Exists.errorWith "Cases 'LogString' and 'LogOptions' of 'Log' share the tag value \"log\" and cannot be told apart at runtime: field 'message' has type 'Options', a Pojo that is a plain object at runtime. Add a CaseTest attribute to 'LogString' or 'LogOptions'."
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a tag emit error when only interfaces tell them apart" <| fun _ ->
      let source =
        """
open Fable.Core

type Circle =
    abstract radius: float

type Rect =
    abstract width: float

[<TypeScriptTaggedUnion("kind")>]
type Shape =
    | [<CompiledName("shape")>] Circle of shape: Circle
    | [<CompiledName("shape")>] Rect of shape: Rect

let isRect (s: Shape) = s.IsRect
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Rect' and 'Circle' of 'Shape' share the tag value \"shape\" and cannot be told apart at runtime: field 'shape' has type 'Rect', an interface that does not exist at runtime. Add a CaseTest attribute to 'Rect' or 'Circle'."
      |> ignore
  ]
