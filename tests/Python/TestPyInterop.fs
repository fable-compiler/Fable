module Fable.Tests.PyInterop

open System
open Util.Testing

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.PyInterop
open Fable.Core.DynamicExtensions
open Fable.Core.Experimental

let logs = ResizeArray()

type LogAttribute(msg1: string, code: int) =
    inherit Py.DecoratorAttribute()
    override _.Decorate(fn) =
        let mutable count = 0
        Py.argsFunc(fun args ->
            count <- count + 1
            logs.Add $"LOG1: [%s{msg1} (code %i{code})] called %i{count} time(s)!"
            fn.Invoke(args))

type Log2Attribute() =
    inherit Py.DecoratorAttribute()
    override _.Decorate(fn) =
        let mutable count = 0
        Py.argsFunc(fun args ->
            count <- count + 1
            logs.Add $"LOG2: called %i{count} time(s)!"
            fn.Invoke(args))

type Log3Attribute() =
    inherit Py.ReflectedDecoratorAttribute()
    override _.Decorate(fn, info) =
        logs.Add $"{info.Name}: {info.ReturnType}"
        for p in info.GetParameters() do
            logs.Add $"{p.Name}: {p.ParameterType}"
        Py.argsFunc(fun args ->
            fn.Invoke(args))

[<Log("MATH", 3)>]
[<Log2>]
[<Log3>]
let myComplexAdder x y = x + y

type NameProp =
    { Name: string }

// type Props =
//     | Names of NameProp array
//     | [<Erase>] Custom of key:string * value:obj

[<Global("list")>]
type PyArray =
    [<Emit("$0.append($1)")>]
    abstract push: item: obj -> unit
    [<Emit("len($0)")>]
    abstract length: int

[<Fable.Core.AttachMembers>]
type ClassWithAttachments(v, sign) =
    static let secretSauce = "wasabi"
    let mutable x = v
    member _.Times with get() = x and set(y) = x <- x + y
    member this.SaySomethingTo(name: string, format) =
        let sign = defaultArg sign "!"
        let format = defaultArg format ClassWithAttachments.GreetingFormat
        let format = format + ClassWithAttachments.GetGrettingEnding(this.Times, sign)
        String.Format(format, name)
    static member GreetingFormat = "Hello {0}"
    static member GetGrettingEnding(times, sign) = String.replicate times sign
    member _.WithSecretSauce(food) = $"{food} with lots of {secretSauce}"

[<Erase>]
type ErasedUnion =
    | ErasedInt of int
    | ErasedString of string
    member this.SayHi() =
        match this with
        | ErasedInt i -> sprintf "Hi %i time(s)!" i
        | ErasedString s -> sprintf "Hi %s!" s

[<Erase>]
type ErasedUnionWithMultipleFields =
    | ErasedUnionWithMultipleFields of string * int

[<Erase; RequireQualifiedAccess>]
type MyErasedUnion2 =
    | Foo
    | Ohmy
    | Bar of float
    | Baz of int[]

[<Erase(CaseRules.KebabCase); RequireQualifiedAccess>]
type MyErasedUnion3 =
    | FooBar
    | OhMyDear
    | AnotherNumber of int

[<StringEnum>]
type MyStrings =
    | Vertical
    | [<CompiledName("Horizontal")>] Horizontal

[<StringEnum(CaseRules.SnakeCase); RequireQualifiedAccess>]
type UserInfo =
    | UserLoginCount

[<StringEnum(CaseRules.SnakeCaseAllCaps); RequireQualifiedAccess>]
type UserInfo2 =
    | UserLoginCount

[<StringEnum(CaseRules.KebabCase); RequireQualifiedAccess>]
type MyCssOptions =
    | ContentBox
    | BorderBox

[<StringEnum(CaseRules.LowerAll); RequireQualifiedAccess>]
type LowerAllOptions =
    | ContentBox
    | BorderBox

type Field = OldPassword | NewPassword | ConfirmPassword
    with member this.Kind =
            match this with
            | OldPassword -> "Old"
            | NewPassword -> "New"
            | ConfirmPassword -> "Confirm"
         static member Default = NewPassword

type MyInterface =
    abstract foo: int
    abstract bar: string

// Test interface for createEmpty functionality
type IUser =
    abstract Name: string with get, set
    abstract Age: int with get, set

let validatePassword = function
    | OldPassword -> "op"
    | NewPassword -> "np"
    | ConfirmPassword -> "cp"


type ClassWithAttachmentsChild() =
    inherit ClassWithAttachments(3, Some "?")
    member this.dileHola(name) = this.SaySomethingTo(name, Some "Hola, {0}")

[<Fable.Core.AttachMembers>]
type ClassWithAttachmentsChild2() =
    inherit ClassWithAttachments(3, Some "?")
    member this.dileHola(name) = this.SaySomethingTo(name, Some "Hola, {0}")

[<Fact>]
let ``test Class with attached members works`` () =
    let x = ClassWithAttachments(2, None) // FIXME: remove l arg
    equal 2 x.Times
    x.Times <- 3
    x.SaySomethingTo("Tanaka", None) |> equal "Hello Tanaka!!!!!"

[<Fact>]
let ``test Class with attached members can have static constructors`` () =
    let x = ClassWithAttachments(2, None)
    x.WithSecretSauce("Nuggets")
    |> equal "Nuggets with lots of wasabi"

[<Fact>]
let ``test Class with attached members can be inherited`` () =
    let x = ClassWithAttachmentsChild()
    x.dileHola("Pepe") |> equal "Hola, Pepe???"

[<Fact>]
let ``test Class with attached members can be inherited II`` () =
    let x = ClassWithAttachmentsChild2()
    x.dileHola("Pepe") |> equal "Hola, Pepe???"

[<Fact>]
let ``test Can type test interfaces decorated with Global`` () =
    let ar = ResizeArray [| 1; 2; 3 |] |> box
    match ar with
    | :? PyArray as ar ->
        ar.length |> equal 3
        ar.push("4")
        ar.length |> equal 4
    | _ -> failwith "Not an array"

// FIXME: ImportError: cannot import name 'keyValueList' from 'fable.map_util'
// [<Fact>]
// let ``Array inside keyValueList is preserved`` () =
//     let props = [ Names [| { Name = "name" } |] ]
//     let actual = [ Names [| { Name = "name" } |] ] |> keyValueList CaseRules.LowerFirst |> JS.JSON.stringify
//     let expected = props |> keyValueList CaseRules.LowerFirst |> JS.JSON.stringify
//     actual |> equal expected

[<Fact>]
let ``test Decorators work`` () =
    myComplexAdder 3 4 |> equal 7
    myComplexAdder 6 7 |> equal 13
    logs |> Seq.toList |> equal [
        "my_complex_adder: System.Int32"
        "x: System.Int32"
        "y: System.Int32"
        "LOG1: [MATH (code 3)] called 1 time(s)!"
        "LOG2: called 1 time(s)!"
        "LOG1: [MATH (code 3)] called 2 time(s)!"
        "LOG2: called 2 time(s)!"
    ]

[<Fact>]
let ``test Erased types can have members`` () =
    let x = ErasedString "Patrick"
    x.SayHi() |> equal "Hi Patrick!"

[<Fact>]
let ``test Erased unions with multiple fields work`` () =
    let gimme (ErasedUnionWithMultipleFields(s, i)) =
        sprintf "Gimme %i %ss" i s
    ("apple", 5)
    |> ErasedUnionWithMultipleFields
    |> gimme
    |> equal "Gimme 5 apples"

[<Fact>]
let ``test Erased unions can have cases representing literal strings`` () =
    let getValue = function
        | MyErasedUnion2.Foo -> 5
        | MyErasedUnion2.Ohmy -> 0
        | MyErasedUnion2.Bar f -> int f
        | MyErasedUnion2.Baz xs -> Array.sum xs

    MyErasedUnion2.Bar 4.4 |> getValue |> equal 4
    MyErasedUnion2.Ohmy |> getValue |> equal 0
    MyErasedUnion2.Baz [|1;2;3|] |> getValue |> equal 6
    MyErasedUnion2.Foo |> getValue |> equal 5
    box MyErasedUnion2.Foo |> equal (box "foo")
    box MyErasedUnion2.Ohmy |> equal (box "ohmy")

[<Fact>]
let ``test Erased unions can have case rules`` () =
    let getValue = function
        | MyErasedUnion3.FooBar -> 5
        | MyErasedUnion3.OhMyDear -> 0
        | MyErasedUnion3.AnotherNumber i -> i

    MyErasedUnion3.AnotherNumber 3 |> getValue |> equal 3
    MyErasedUnion3.OhMyDear |> getValue |> equal 0
    MyErasedUnion3.FooBar |> getValue |> equal 5
    box MyErasedUnion3.OhMyDear |> equal (box "oh-my-dear")
    box MyErasedUnion3.FooBar |> equal (box "foo-bar")

[<Fact>]
let ``test emitPyExpr works with parameters`` () =
    let two : int =
        emitPyExpr (1, 1) "$0 + $1"

    two |> equal 2

[<Fact>]
let ``test emitPyExpr works without parameters`` () =
    let hello : string =
        emitPyExpr () "\"Hello\""

    hello |> equal "Hello"

// This function needs to be at the root level to avoid being mangled
let factorial (count : nativeint) : nativeint =
    emitPyStatement
        count
        """if $0 < 2:
        return 1
    else:
        return $0 * factorial($0-1)
    """

[<Fact>]
let ``test emitPyStatement works with parameters`` () =
    factorial 5 |> equal 120


type NativeCode =
    abstract add5: nativeint -> nativeint

[<Fact>]
let ``test importAll`` () =
    let nativeCode: NativeCode = importAll ".py.native_code"
    3n |> nativeCode.add5 |> equal 8n

// Deliberately using legacy relative path syntax to ensure it still works
let add5 (x: nativeint): nativeint = importMember "./py/native_code.py"

[<Fact>]
let ``test importMember`` () =
    add5 -1 |> equal 4

    // Cannot use the same name as Fable will mangle the identifier
    let add7: nativeint -> nativeint = importMember ".py.native_code"
    add7 12 |> equal 19

    let add5': nativeint -> nativeint = import "add5" ".py.native_code"
    add5' 12 |> equal 17

    let multiply3 (x: nativeint): nativeint = importMember ".py.more_native_code"
    multiply3 9 |> equal 27

[<ImportAll(".py.native_code")>]
let nativeCode: NativeCode = nativeOnly

[<Fact>]
let ``test ImportAll works with relative paths`` () = // See #3481
    3n |> nativeCode.add5 |> equal 8n

[<Fact>]
let ``test StringEnum attribute works`` () =
    Vertical |> unbox |> equal "vertical"
    Horizontal |> unbox |> equal "Horizontal"
    Vertical |> string |> equal "vertical"
    Horizontal |> string |> equal "Horizontal"

[<Fact>]
let ``test StringEnum works with CaseRules.SnakeCase`` () =
    UserInfo.UserLoginCount |> unbox |> equal "user_login_count"

[<Fact>]
let ``test StringEnum works with CaseRules.SnakeCaseAllCaps`` () =
    UserInfo2.UserLoginCount |> unbox |> equal "USER_LOGIN_COUNT"

[<Fact>]
let ``test StringEnum works with CaseRules.KebabCase`` () =
    MyCssOptions.BorderBox |> unbox |> equal "border-box"
    MyCssOptions.ContentBox |> unbox |> equal "content-box"

[<Fact>]
let ``test StringEnum works with CaseRules.LowerAll`` () =
    let x = LowerAllOptions.ContentBox
    x |> unbox |> equal "contentbox"

[<Fact>]
let ``test Pattern matching with StringEnum works`` () =
    validatePassword NewPassword
    |> equal "np"

[<Fact>]
let ``test StringEnums can have members`` () =
    let x = ConfirmPassword
    x.Kind |> equal "Confirm"

[<Fact>]
let ``test StringEnums can have static members`` () =
    let x = Field.Default
    validatePassword x |> equal "np"

// Test for ParamObject with EmitMethod issue #3871

[<Erase>]
type ITestService =
    [<ParamObject(1)>]
    [<EmitMethod("process")>]
    abstract proc: data: string * ?verbose:bool -> string

// Create a test implementation that tracks keyword arguments
[<Emit("""
class TestService:
    def process(self, data, **kwargs):
        return f"processed {data} with {kwargs}"
""", isStatement=true)>]
let defineTestService: unit = nativeOnly

defineTestService

[<Emit("TestService()")>]
let testService: ITestService = nativeOnly

[<Fact>]
let ``test ParamObject with EmitMethod preserves arguments`` () =
    // Test that EmitMethod + ParamObject preserves keyword arguments
    let result = testService.proc("test", verbose = true)
    result.Contains("verbose") |> equal true

// Test for ParamObject with EmitConstructor issue #3871 (continuation of PR #4158)

[<Erase>]
type ITestProcess =
    abstract member name: string

[<Erase>]
type ITestProcessType =
    [<EmitConstructor; ParamObject>]
    abstract Create: ?name: string -> ITestProcess

// Create a test class that accepts keyword arguments
[<Emit("""
class MockProcess:
    def __init__(self, **kwargs):
        self._name = kwargs.get('name', 'default')

    @property
    def name(self):
        return self._name
""", isStatement=true)>]
let defineTestProcessClass: unit = nativeOnly

defineTestProcessClass

[<Emit("MockProcess")>]
let TestProcess: ITestProcessType = nativeOnly

[<Fact>]
let ``test ParamObject with EmitConstructor preserves keyword arguments`` () =
    // Test that EmitConstructor + ParamObject generates MockProcess(name="worker")
    // instead of MockProcess() (losing the keyword arguments)
    let proc = TestProcess.Create(name = "worker")
    proc.name |> equal "worker"

    // Also test with default parameter (no arguments)
    let proc2 = TestProcess.Create()
    proc2.name |> equal "default"

[<Fact>]
let ``test createEmpty works with interfaces`` () =
    // Test that createEmpty<T> works with interfaces by creating a SimpleNamespace
    // that can have properties set dynamically
    let user = createEmpty<IUser>
    user.Name <- "Kaladin"
    user.Age <- 20

    // Verify the properties can be accessed
    user.Name |> equal "Kaladin"
    user.Age |> equal 20

// Test for Pydantic compatible classes
[<Erase>]
type Field<'T> = 'T

module Field =
    [<Import("Field", "pydantic")>]
    [<Emit("$0(frozen=$1)")>]
    let Frozen (frozen: bool) : Field<'T> = nativeOnly

    [<Import("Field", "pydantic")>]
    [<Emit("$0(default=$1)")>]
    let Default(value: 'T) : Field<'T> = nativeOnly


[<Import("BaseModel", "pydantic")>]
type BaseModel () = class end

// Test with bigint (transpiles to Python int)
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type PydanticUser(Name: string, Age: bigint, Email: string option, Enabled: bool) =
    inherit BaseModel()
    member val Name: string = Field.Frozen(true) with get, set
    member val Age: bigint = Age with get, set
    member val Email: string option = None with get, set
    member val Enabled: bool = Field.Default(false) with get, set

[<Fact>]
let ``test PydanticUser`` () =
    let user = PydanticUser(Name = "Test User", Age = 25I, Email = Some "test@example.com", Enabled = true)
    user.Name |> equal "Test User"
    user.Age |> equal 25I
    user.Email |> equal (Some "test@example.com")
    user.Enabled |> equal true

// Test with Fable core types (int32, int64, float) that have __get_pydantic_core_schema__
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type PydanticUserWithCoreTypes(Name: string, Age: int, Score: float, Level: int64) =
    inherit BaseModel()
    member val Name: string = Name with get, set
    member val Age: int = Age with get, set           // int32
    member val Score: float = Score with get, set     // float64
    member val Level: int64 = Level with get, set     // int64

[<Fact>]
let ``test PydanticUser with core int32 type`` () =
    let user = PydanticUserWithCoreTypes(Name = "Core User", Age = 30, Score = 95.5, Level = 100L)
    user.Name |> equal "Core User"
    user.Age |> equal 30
    user.Score |> equal 95.5
    user.Level |> equal 100L

// Test with mixed core types including byte/sbyte
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type PydanticMixedTypes(ByteVal: byte, SByteVal: sbyte, Int16Val: int16, UInt16Val: uint16) =
    inherit BaseModel()
    member val ByteVal: byte = ByteVal with get, set
    member val SByteVal: sbyte = SByteVal with get, set
    member val Int16Val: int16 = Int16Val with get, set
    member val UInt16Val: uint16 = UInt16Val with get, set

[<Fact>]
let ``test PydanticUser with byte and int16 types`` () =
    let model = PydanticMixedTypes(ByteVal = 255uy, SByteVal = -128y, Int16Val = -32768s, UInt16Val = 65535us)
    model.ByteVal |> equal 255uy
    model.SByteVal |> equal -128y
    model.Int16Val |> equal -32768s
    model.UInt16Val |> equal 65535us

// Test Pydantic field_validator with @classmethod
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type PydanticUserWithValidator(Name: string) =
    inherit BaseModel()
    member val Name: string = Name with get, set

    [<Py.Decorate("field_validator", importFrom="pydantic", parameters="'Name'")>]
    [<Py.ClassMethod>]
    static member validate_name(cls: obj, v: string) : string =
        v.ToUpper()

[<Fact>]
let ``test Pydantic field_validator with classmethod`` () =
    // Test that @field_validator and @classmethod decorators are applied correctly
    // The validator should transform the Name to uppercase
    let user = emitPyExpr<PydanticUserWithValidator> [] "PydanticUserWithValidator(Name='john')"
    user.Name |> equal "JOHN"

[<Py.Decorate("dataclass", importFrom="dataclasses")>]
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type DecoratedUser() =
    member val Name: string = "" with get, set
    member val Age: int = 0 with get, set

[<Fact>]
let ``test simple decorator without parameters`` () =
    // Test that @dataclass decorator is applied correctly
    let user = DecoratedUser()
    user.Name <- "Test User"
    user.Age <- 25

    user.Name |> equal "Test User"
    user.Age |> equal 25

[<AttachMembers>]
type DecoratedCacheClass() =
    [<Py.Decorate("lru_cache", importFrom="functools", parameters="maxsize=128")>]
    static member expensive_computation(x: int) : int =
        x * x  // Simulated expensive computation

[<Fact>]
let ``test decorator with parameters`` () =
    // Test that decorator with parameters is applied correctly to a method
    let result = DecoratedCacheClass.expensive_computation(5)
    result |> equal 25

[<Py.Decorate("dataclass", importFrom="dataclasses")>]
[<Py.Decorate("total_ordering", importFrom="functools")>]
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type MultiDecoratedClass() =
    member val Priority: int = 0 with get, set
    member val Name: string = "" with get, set

    member this.__lt__(other: MultiDecoratedClass) =
        this.Priority < other.Priority

[<Fact>]
let ``test multiple decorators applied in correct order`` () =
    // Test that multiple decorators are applied bottom-to-top
    let obj = MultiDecoratedClass()
    obj.Priority <- 1
    obj.Name <- "test"

    obj.Priority |> equal 1
    obj.Name |> equal "test"

[<Py.Decorate("define", importFrom="attrs", parameters="auto_attribs=True, slots=True")>]
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type AttrsDecoratedClass() =
    member val Data: string = "attrs_data" with get, set
    member val Count: nativeint = 42n with get, set

[<Fact>]
let ``test complex decorator parameters`` () =
    // Test decorator with complex parameter syntax
    let obj = AttrsDecoratedClass()
    obj.Data |> equal "attrs_data"
    obj.Count |> equal 42n

// Test combining Decorate with existing F# features

[<Py.Decorate("dataclass", "dataclasses")>]
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type InheritedDecoratedClass() =
    inherit DecoratedUser()
    member val Email: string = "" with get, set

[<Fact>]
let ``test decorator with inheritance`` () =
    // Test that decorators work with class inheritance
    let obj = InheritedDecoratedClass()
    obj.Name <- "Inherited"
    obj.Age <- 30
    obj.Email <- "test@example.com"

    obj.Name |> equal "Inherited"
    obj.Age |> equal 30
    obj.Email |> equal "test@example.com"

[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Properties, init=true)>]
type PropertiesUserWithInit(Name: string, Age: bigint, Email: string option, Enabled: bool) =
    member val Name: string = Name with get, set
    member val Age: bigint = Age with get, set
    member val Email: string option = Email with get, set
    member val Enabled: bool = Enabled with get, set

[<Fact>]
let ``test PropertiesUserWithInit`` () =
    let user = PropertiesUserWithInit(Name = "Test User", Age = 25I, Email = Some "test@example.com", Enabled = true)
    user.Name |> equal "Test User"
    user.Age |> equal 25I
    user.Email |> equal (Some "test@example.com")
    user.Enabled |> equal true

// Test Py.Decorate without import (local variable decorator like FastAPI app.get)

// Simulate a decorator factory (like FastAPI's app instance)
let my_decorator (path: string) : (obj -> obj) = import "my_decorator" "./py/native_code.py"

[<AttachMembers>]
type ClassWithLocalDecorator() =
    // Use full decorator expression since my_decorator is a local variable (no import)
    [<Py.Decorate("my_decorator(\"/test\")")>]
    static member decorated_method() : string =
        "result"

[<Fact>]
let ``test Py.Decorate without import (local variable)`` () =
    // Test that decorator is emitted verbatim without auto-import
    // This pattern is used for FastAPI: @app.get("/")
    let result = ClassWithLocalDecorator.decorated_method()
    result |> equal "decorated: result"

// Test Py.Decorate on static methods

[<AttachMembers>]
type ClassWithDecoratedStaticMethod() =
    member val Value: int = 0 with get, set

    [<Py.Decorate("lru_cache", importFrom="functools")>]
    static member cached_function(x: int) : int =
        x * 2

[<Fact>]
let ``test Py.Decorate on static method`` () =
    // Test that @lru_cache decorator is applied to static method
    let result1 = ClassWithDecoratedStaticMethod.cached_function(5)
    let result2 = ClassWithDecoratedStaticMethod.cached_function(5)
    result1 |> equal 10
    result2 |> equal 10

[<AttachMembers>]
type ClassWithDecoratedStaticMethodWithParams() =
    [<Py.Decorate("lru_cache", importFrom="functools", parameters="maxsize=32")>]
    static member cached_with_params(x: int) : int =
        x * 3

[<Fact>]
let ``test Py.Decorate on static method with parameters`` () =
    // Test that decorator with parameters is applied to static method
    let result = ClassWithDecoratedStaticMethodWithParams.cached_with_params(4)
    result |> equal 12

[<AttachMembers>]
type ClassWithClassMethod() =
    member val Name: string = "" with get, set

    // Note: With @classmethod, Python automatically passes `cls` as first argument
    // So from F# we only pass the remaining arguments
    [<Py.ClassMethod>]
    static member create_instance(cls: obj, name: string) : ClassWithClassMethod =
        let instance = ClassWithClassMethod()
        instance.Name <- name
        instance

[<Fact>]
let ``test Py.ClassMethod attribute`` () =
    // Test that @classmethod decorator is applied instead of @staticmethod
    // Note: cls is automatically passed by Python, so we only pass the name argument
    // We use Emit to call the method without the cls argument from F#
    let instance = emitPyExpr<ClassWithClassMethod> [] "ClassWithClassMethod.create_instance('TestName')"
    instance.Name |> equal "TestName"


[<AttachMembers>]
type ClassWithMultipleDecoratedMethods() =
    [<Py.Decorate("lru_cache", importFrom="functools", parameters="maxsize=16")>]
    static member method_a(x: int) : int = x * 2

    [<Py.Decorate("lru_cache", importFrom="functools", parameters="maxsize=32")>]
    static member method_b(x: int) : int = x * 3

[<Fact>]
let ``test multiple static methods with decorators`` () =
    // Test that decorators work on multiple static methods in same class
    let resultA = ClassWithMultipleDecoratedMethods.method_a(5)
    let resultB = ClassWithMultipleDecoratedMethods.method_b(5)
    resultA |> equal 10
    resultB |> equal 15

[<AttachMembers>]
type ClassWithDecoratedInstanceMethod() =
    member val CallCount: int = 0 with get, set

    [<Py.Decorate("lru_cache", importFrom="functools", parameters="maxsize=16")>]
    member this.cached_instance_method(x: int) : int =
        x * 4

[<Fact>]
let ``test Py.Decorate on instance method`` () =
    // Test that decorator works on instance methods too
    let obj = ClassWithDecoratedInstanceMethod()
    let result1 = obj.cached_instance_method(3)
    let result2 = obj.cached_instance_method(3)
    result1 |> equal 12
    result2 |> equal 12

// Import fable_library version to verify we're using local build, not PyPI
let fableLibraryVersion: string = import "__version__" "fable_library._version"

[<Fact>]
let ``test fable_library version is local build`` () =
    // Local builds use version "0.0.0" as placeholder
    // If this test fails, it means tests are running against a PyPI version
    // instead of the locally built fable-library
    equal "0.0.0" fableLibraryVersion

// Test DecorateTemplate attribute for creating custom decorator attributes
// This demonstrates how library authors can create ergonomic FastAPI-style attributes

// Simulated FastAPI app instance (imported from native code)
let app: obj = import "app" "./py/native_code.py"

// Custom decorator attributes using DecorateTemplate
// Library authors define these once, users get clean syntax
[<Erase; Py.DecorateTemplate("app.get('{0}')")>]
type GetAttribute(path: string) =
    inherit Attribute()

[<Erase; Py.DecorateTemplate("app.post('{0}')")>]
type PostAttribute(path: string) =
    inherit Attribute()

[<Erase; Py.DecorateTemplate("app.delete('{0}')")>]
type DeleteAttribute(path: string) =
    inherit Attribute()

// Users get clean, intuitive API similar to Python FastAPI:
[<Py.DataClass>]
type TestAPI() =
    [<Get("/")>]
    static member root() = {| message = "Hello World" |}

    [<Get("/items/{item_id}")>]
    static member get_item(item_id: int) = {| item_id = item_id |}

    [<Post("/items")>]
    static member create_item(name: string) = {| name = name; created = true |}

    [<Delete("/items/{item_id}")>]
    static member delete_item(item_id: int) = {| deleted = item_id |}

[<Fact>]
let ``test DecorateTemplate generates FastAPI-style decorators`` () =
    // Test that @app.get("/") decorator is applied and works
    let result = TestAPI.root()
    result.["message"] |> equal "Hello World"

[<Fact>]
let ``test DecorateTemplate with path parameter`` () =
    // Test that @app.get("/items/{item_id}") decorator works with path params
    let result = TestAPI.get_item(42)
    result.["item_id"] |> equal 42

[<Fact>]
let ``test DecorateTemplate POST endpoint`` () =
    // Test that @app.post("/items") decorator works
    let result = TestAPI.create_item("test-item")
    result.["name"] |> equal "test-item"
    result.["created"] |> equal true

[<Fact>]
let ``test DecorateTemplate DELETE endpoint`` () =
    // Test that @app.delete("/items/{item_id}") decorator works
    let result = TestAPI.delete_item(123)
    result.["deleted"] |> equal 123

// Test Py.DataClass - shorthand for [<Py.ClassAttributes(style = Attributes, init = false)>]
// This generates a class with type-annotated class attributes, suitable for Pydantic, attrs, etc.
[<Py.DataClass>]
type DataClassUser() =
    member val Name: string = "" with get, set
    member val Age: int = 0 with get, set

[<Fact>]
let ``test Py.DataClass shorthand attribute`` () =
    // DataClass generates class-level type annotations without Fable's __init__
    // This style is used by Pydantic, attrs, and other Python frameworks
    let user = DataClassUser()
    user.Name <- "Alice"
    user.Age <- 30
    user.Name |> equal "Alice"
    user.Age |> equal 30

// Test ClassAttributesTemplate - library authors can create custom class attributes
[<Erase; Py.ClassAttributesTemplate(Py.ClassAttributeStyle.Attributes, false)>]
type BaseModelAttribute() =
    inherit Attribute()

// Users get clean syntax - just [<BaseModel>] instead of the verbose version
[<BaseModel>]
type PydanticLikeUser() =
    member val Username: string = "" with get, set
    member val Email: string = "" with get, set

[<Fact>]
let ``test ClassAttributesTemplate for custom class attributes`` () =
    // BaseModel uses ClassAttributesTemplate to define a shorthand for Pydantic-style classes
    // The class should have class-level attributes without Fable-generated __init__
    let user = PydanticLikeUser()
    user.Username <- "testuser"
    user.Email <- "test@example.com"
    user.Username |> equal "testuser"
    user.Email |> equal "test@example.com"

// Test Pydantic serialization of FSharpArray
// This tests that FSharpArray and its subclasses (GenericArray, Int32Array, etc.)
// can be serialized by Pydantic when used in BaseModel classes

[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type PydanticUserWithArray(Name: string, Scores: int[]) =
    inherit BaseModel()
    member val Name: string = Name with get, set
    member val Scores: int[] = Scores with get, set

[<Fact>]
let ``test Pydantic serialization of FSharpArray`` () =
    // Test that FSharpArray can be used in a Pydantic model and serialized
    let user = PydanticUserWithArray(Name = "Test User", Scores = [|95; 87; 92|])
    user.Name |> equal "Test User"
    user.Scores |> Array.sum |> equal 274

[<Fact>]
let ``test Pydantic model_dump with FSharpArray`` () =
    // Test that Pydantic can serialize (dump) a model containing FSharpArray
    let user = PydanticUserWithArray(Name = "Test User", Scores = [|95; 87; 92|])
    // Call model_dump() to serialize to dict
    let dumped: obj = emitPyExpr (user) "$0.model_dump()"
    // The dumped dict should have the scores as a list
    let scores: int[] = emitPyExpr (dumped) "$0['Scores']"
    scores |> Array.sum |> equal 274

[<Fact>]
let ``test Pydantic model_dump_json with FSharpArray`` () =
    // Test that Pydantic can serialize to JSON a model containing FSharpArray
    let user = PydanticUserWithArray(Name = "Test User", Scores = [|95; 87; 92|])
    // Call model_dump_json() to serialize to JSON string
    let json: string = emitPyExpr (user) "$0.model_dump_json()"
    // The JSON should contain the scores
    json.Contains("95") |> equal true
    json.Contains("87") |> equal true
    json.Contains("92") |> equal true

[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type PydanticUserWithStringArray(Name: string, Tags: string[]) =
    inherit BaseModel()
    member val Name: string = Name with get, set
    member val Tags: string[] = Tags with get, set

[<Fact>]
let ``test Pydantic serialization of FSharpArray with strings`` () =
    // Test that string arrays work with Pydantic
    let user = PydanticUserWithStringArray(Name = "Test User", Tags = [|"admin"; "active"; "verified"|])
    user.Name |> equal "Test User"
    user.Tags |> Array.length |> equal 3

[<Fact>]
let ``test Pydantic model_dump with string array`` () =
    // Test that Pydantic can serialize string arrays
    let user = PydanticUserWithStringArray(Name = "Test User", Tags = [|"admin"; "active"|])
    let dumped: obj = emitPyExpr (user) "$0.model_dump()"
    let tags: string[] = emitPyExpr (dumped) "$0['Tags']"
    tags.[0] |> equal "admin"
    tags.[1] |> equal "active"

[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type PydanticUserWithFloatArray(Name: string, Values: float[]) =
    inherit BaseModel()
    member val Name: string = Name with get, set
    member val Values: float[] = Values with get, set

[<Fact>]
let ``test Pydantic serialization of FSharpArray with floats`` () =
    // Test that float arrays work with Pydantic
    let user = PydanticUserWithFloatArray(Name = "Test User", Values = [|1.5; 2.5; 3.5|])
    user.Name |> equal "Test User"
    user.Values |> Array.sum |> equal 7.5

[<Fact>]
let ``test Pydantic model_dump_json with float array`` () =
    // Test that Pydantic can serialize float arrays to JSON
    let user = PydanticUserWithFloatArray(Name = "Test User", Values = [|1.5; 2.5|])
    let json: string = emitPyExpr (user) "$0.model_dump_json()"
    json.Contains("1.5") |> equal true
    json.Contains("2.5") |> equal true

// Regression tests: Array.length/.Length/Array.isEmpty must use len() so they
// work on plain Python lists (e.g. from Emit, unbox, native APIs), not just FSharpArray.

[<Fact>]
let ``test Array.length works on plain Python list`` () =
    let xs: int[] = emitPyExpr () "[1, 2, 3]"
    Array.length xs |> equal 3

[<Fact>]
let ``test .Length works on plain Python list`` () =
    let xs: int[] = emitPyExpr () "[10, 20]"
    xs.Length |> equal 2

[<Fact>]
let ``test Array.isEmpty works on plain Python list`` () =
    let xs: int[] = emitPyExpr () "[1]"
    Array.isEmpty xs |> equal false
    let ys: int[] = emitPyExpr () "[]"
    Array.isEmpty ys |> equal true

#endif
