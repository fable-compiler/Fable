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

    testCase "CaseTest on a regular union emits warning" <| fun _ ->
      let source =
        """
open Fable.Core

type Id =
    | [<CaseTest("$0.startsWith('n:')")>] Name of string
    | [<CaseTest("$0.startsWith('s:')")>] Slug of string
"""
      compile source
      |> Assert.Exists.warningWith "CaseTest attribute on 'Name' is ignored: it is only supported on TypeScriptTaggedUnion and Erase union cases"
      |> Assert.Exists.warningWith "CaseTest attribute on 'Slug' is ignored: it is only supported on TypeScriptTaggedUnion and Erase union cases"
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

    testCase "TypeScriptTaggedUnion cases sharing a number tag emit error showing the tag value" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("code")>]
type Status =
    | [<CompiledValue(1)>] Ok of value: string
    | [<CompiledValue(1)>] Failed of value: string

let isOk (s: Status) = s.IsOk
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Ok' and 'Failed' of 'Status' share the tag value 1 and have the same fields. Add a CaseTest attribute to 'Ok' or 'Failed'."
      |> ignore

    testCase "TypeScriptTaggedUnion cases sharing a boolean tag emit error showing the tag value" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("ok")>]
type Status =
    | [<CompiledValue(true)>] Ok of value: string
    | [<CompiledValue(true)>] Warn of value: string

let isOk (s: Status) = s.IsOk
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Ok' and 'Warn' of 'Status' share the tag value true and have the same fields. Add a CaseTest attribute to 'Ok' or 'Warn'."
      |> ignore

    testCase "Erased union cases of interface type are told apart by their members" <| fun _ ->
      let source =
        """
open Fable.Core

type Circle =
    abstract radius: float

type Rect =
    abstract width: float
    abstract area: unit -> float

type Animal =
    abstract name: string

type Dog =
    inherit Animal
    abstract bark: string

type Marker = interface end

[<Erase>]
type Shape =
    | Circle of Circle
    | Rect of Rect
    | Count of int
    | Label of string

[<Erase>]
type Pet =
    | Animal of Animal
    | Dog of Dog

[<Erase>]
type Tagged =
    | Mark of Marker
    | Name of string

let describe (s: Shape) = match s with Circle _ -> "circle" | Rect _ -> "rect" | Count _ -> "count" | Label _ -> "label"
let pet (p: Pet) = match p with Animal _ -> "animal" | Dog _ -> "dog"
let tagged (t: Tagged) = match t with Mark _ -> "mark" | Name _ -> "name"
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "Erased union cases of interfaces with the same members emit error" <| fun _ ->
      let source =
        """
open Fable.Core

type Circle =
    abstract size: float

type Rect =
    abstract size: float
    abstract label: string option

[<Erase>]
type Shape =
    | Circle of Circle
    | Rect of Rect

let isRect (s: Shape) = match s with Rect _ -> true | _ -> false
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Rect' and 'Circle' of 'Shape' cannot be told apart at runtime: interfaces 'Rect' and 'Circle' have no required member that tells them apart. Add a CaseTest attribute to 'Rect' or 'Circle'."
      |> ignore

    testCase "Erased union case of mangled interface type emits error" <| fun _ ->
      let source =
        """
open Fable.Core

[<Mangle>]
type Circle =
    abstract radius: float

[<Erase>]
type Shape =
    | Circle of Circle
    | Count of int

let isCircle (s: Shape) = match s with Circle _ -> true | _ -> false
"""
      compile source
      |> Assert.Exists.errorWith "Case 'Circle' of 'Shape' cannot be tested at runtime: type 'Circle' is an interface that does not exist at runtime. Add a CaseTest attribute to 'Circle'."
      |> ignore

    testCase "Erased union case of interface type next to a Pojo case emits error" <| fun _ ->
      let source =
        """
open Fable.Core

type Circle =
    abstract radius: float

[<JS.Pojo>]
type Options(name: string) =
    member val name = name

[<Erase>]
type Shape =
    | Circle of Circle
    | Opts of Options

let isCircle (s: Shape) = match s with Circle _ -> true | _ -> false
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Circle' and 'Opts' of 'Shape' cannot be told apart at runtime: type 'Options' is a Pojo that is a plain object at runtime. Add a CaseTest attribute to 'Circle' or 'Opts'."
      |> ignore

    testCase "Erased union cases of the same type emit error" <| fun _ ->
      let source =
        """
open Fable.Core

[<Erase>]
type Id =
    | Name of string
    | Slug of string

let isName (id: Id) = match id with Name _ -> true | _ -> false
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Name' and 'Slug' of 'Id' cannot be told apart at runtime: both have type 'string'. Add a CaseTest attribute to 'Name' or 'Slug'."
      |> ignore

    testCase "Erased union cases of types with the same runtime test emit error" <| fun _ ->
      let source =
        """
open Fable.Core

[<Erase>]
type Quantity =
    | Count of int
    | Amount of float

let isCount (q: Quantity) = match q with Count _ -> true | _ -> false
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Count' and 'Amount' of 'Quantity' cannot be told apart at runtime: types 'int' and 'float' look the same at runtime. Add a CaseTest attribute to 'Count' or 'Amount'."
      |> ignore

    testCase "Erased union case whose type includes another case emits error" <| fun _ ->
      let source =
        """
open Fable.Core

type Animal() =
    member _.Kind = "animal"

type Dog() =
    inherit Animal()

[<Erase>]
type Pet =
    | Animal of Animal
    | Dog of Dog

let isAnimal (p: Pet) = match p with Animal _ -> true | _ -> false
"""
      compile source
      |> Assert.Exists.errorWith "Cases 'Animal' and 'Dog' of 'Pet' cannot be told apart at runtime: 'Dog' is a subtype of 'Animal'. Add a CaseTest attribute to 'Animal' or 'Dog'."
      |> ignore

    testCase "Erased union cases of distinct types do not emit error" <| fun _ ->
      let source =
        """
open Fable.Core

type Animal() =
    member _.Kind = "animal"

type Dog() =
    inherit Animal()

[<Erase>]
type Pet =
    | Animal of Animal
    | Dog of Dog

[<Erase>]
type Value =
    | Str of string
    | Num of float
    | Flag of bool
    | Many of float[]

let isDog (p: Pet) = match p with Dog _ -> true | _ -> false
let describe (v: Value) = match v with Str _ -> "str" | Num _ -> "num" | Flag _ -> "flag" | Many _ -> "many"
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "Erased union cases with CaseTest do not emit error" <| fun _ ->
      let source =
        """
open Fable.Core

type Circle =
    abstract radius: float

type Rect =
    abstract width: float

[<Erase>]
type Shape =
    | [<CaseTest("'radius' in $0")>] Circle of Circle
    | [<CaseTest("'width' in $0")>] Rect of Rect

[<Erase>]
type Id =
    | [<CaseTest("$0.startsWith('n:')")>] Name of string
    | Slug of string

[<Erase>]
type Raw =
    | [<CaseTest("typeof $0 === 'object'")>] Obj of obj
    | Text of string

let describe (s: Shape) = match s with Circle _ -> "circle" | Rect _ -> "rect"
let kind (id: Id) = match id with Name _ -> "name" | Slug _ -> "slug"
let raw (r: Raw) = match r with Obj _ -> "obj" | Text _ -> "text"
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore
  
    testCase "TS tagged union case with an interface field compiles when the interface declares the tag" <| fun _ ->
      let source =
        """
open Fable.Core

type Take =
    abstract ``type``: string
    abstract fromIndex: int

[<TypeScriptTaggedUnion("type")>]
type Command =
    | Take of Take
    | Save

let fromIndex (c: Command) = match c with Take t -> t.fromIndex | Save -> 0
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "TS tagged union case with an interface field reports an interface without the tag member" <| fun _ ->
      let source =
        """
open Fable.Core

type Take =
    abstract fromIndex: int

[<TypeScriptTaggedUnion("type")>]
type Command =
    | Take of Take
    | Save
"""
      compile source
      |> Assert.Exists.errorWith "Case 'Take' of 'Command' compiles to its 'Take' value, so the interface must declare a 'type' member"
      |> ignore

    testCase "TS tagged union case with a named interface field does not need the tag member" <| fun _ ->
      let source =
        """
open Fable.Core

type Take =
    abstract fromIndex: int

[<TypeScriptTaggedUnion("type")>]
type Command =
    | Take of take: Take
    | Save

let fromIndex (c: Command) = match c with Take t -> t.fromIndex | Save -> 0
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore

    testCase "TS tagged union case with a single primitive field compiles" <| fun _ ->
      let source =
        """
open Fable.Core

[<TypeScriptTaggedUnion("type")>]
type AuthState =
    | Zero
    | One of string

let one = AuthState.One "a"
"""
      compile source
      |> Assert.Is.strictSuccess
      |> ignore
  ]
