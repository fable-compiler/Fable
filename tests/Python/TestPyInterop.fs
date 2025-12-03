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
let factorial (count : int) : int =
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
    abstract add5: int -> int

[<Fact>]
let ``test importAll`` () =
    let nativeCode: NativeCode = importAll "./native_code.py"
    3 |> nativeCode.add5 |> equal 8

let add5 (x: int): int = importMember "./native_code.py"

[<Fact>]
let ``test importMember`` () =
    add5 -1 |> equal 4

    // Cannot use the same name as Fable will mangle the identifier
    let add7: int -> int = importMember "./native_code.py"
    add7 12 |> equal 19

    let add5': int -> int = import "add5" "./native_code.py"
    add5' 12 |> equal 17

    let multiply3 (x: int): int = importMember "./more_native_code.py"
    multiply3 9 |> equal 27

[<ImportAll("./native_code.py")>]
let nativeCode: NativeCode = nativeOnly

[<Fact>]
let ``test ImportAll works with relative paths`` () = // See #3481
    3 |> nativeCode.add5 |> equal 8

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
    abstract process: data: string * ?verbose:bool -> string

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
    let result = testService.process("test", verbose = true)
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

[<Py.Decorate("dataclasses.dataclass")>]
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

[<Py.Decorate("functools.lru_cache", "maxsize=128")>]
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type DecoratedCache() =
    member val Value: string = "cached" with get, set

[<Fact>]
let ``test decorator with parameters`` () =
    // Test that decorator with parameters is applied correctly
    let cache = DecoratedCache()
    cache.Value |> equal "cached"

[<Py.Decorate("dataclasses.dataclass")>]
[<Py.Decorate("functools.total_ordering")>]
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

[<Py.Decorate("attrs.define", "auto_attribs=True, slots=True")>]
[<Py.ClassAttributes(style=Py.ClassAttributeStyle.Attributes, init=false)>]
type AttrsDecoratedClass() =
    member val Data: string = "attrs_data" with get, set
    member val Count: int = 42 with get, set

[<Fact>]
let ``test complex decorator parameters`` () =
    // Test decorator with complex parameter syntax
    let obj = AttrsDecoratedClass()
    obj.Data |> equal "attrs_data"
    obj.Count |> equal 42

// Test combining Decorate with existing F# features

[<Py.Decorate("dataclasses.dataclass")>]
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

// Import fable_library version to verify we're using local build, not PyPI
let fableLibraryVersion: string = import "__version__" "fable_library._version"

[<Fact>]
let ``test fable_library version is local build`` () =
    // Local builds use version "0.0.0" as placeholder
    // If this test fails, it means tests are running against a PyPI version
    // instead of the locally built fable-library
    equal "0.0.0" fableLibraryVersion

#endif
