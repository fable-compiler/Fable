[<Util.Testing.TestFixture>]
module Fable.Tests.TypeTests

open System
open Util.Testing
open Fable.Tests.Util

type ITest = interface end
type ITest2 = interface end

type TestType =
    | Union1 of string
    interface ITest

type TestType2 =
    | Union2 of string
    interface ITest

type TestType3() =
    member __.Value = "Hi"
    interface ITest

type TestType4() =
    inherit TestType3()
    member __.Value2 = "Bye"
    interface ITest2

type TestType5(greeting: string) =
    member __.Value = greeting
    member __.Overload(x) = x + x
    member __.Overload(x, y) = x + y

let normalize (x: string) =
    #if FABLE_COMPILER
    x
    #else
    x.Replace("+",".")
    #endif

[<Test>]
let ``Children inherits parent interfaces``() =
    let t4 = TestType4() |> box
    t4 :? ITest |> equal true

type TestType8(?greeting) =
    member __.Value = defaultArg greeting "Hi"

type TestType9() =
    inherit TestType8()
    let foo = TestType8("Hello")
    member __.Greet(name) = foo.Value + " " + name

[<Test>]
let ``Types can instantiate their parent in the constructor``() =
    let t = TestType9()
    t.Greet("Maxime") |> equal "Hello Maxime"

[<Test>]
let ``Type Namespace``() =
    let x = typeof<TestType>.Namespace
    #if FABLE_COMPILER
    equal "Fable.Tests.TypeTests" x
    #else
    equal "Fable.Tests" x
    #endif

[<Test>]
let ``Type FullName``() =
    let x = typeof<TestType>.FullName
    x |> normalize |> equal "Fable.Tests.TypeTests.TestType"

[<Test>]
let ``Type Name``() =
    let x = typeof<TestType>.Name
    equal "TestType" x

[<Test>]
let ``Type testing``() =
    let x = Union1 "test" :> obj
    let y = Union2 "test" :> obj
    x :? TestType |> equal true
    x :? TestType2 |> equal false
    y :? TestType |> equal false
    y :? TestType2 |> equal true

[<Test>]
let ``Interface testing``() =
    let x = Union1 "test" :> obj
    let y = Union2 "test" :> obj
    x :? ITest |> equal true
    x :? ITest2 |> equal false
    y :? ITest |> equal true
    y :? ITest2 |> equal false

[<Test>]
let ``Type testing in pattern matching``() =
    let x = Union1 "test" :> obj
    match x with
    | :? TestType as x -> let (Union1 str) = x in str
    | _ -> "FAIL"
    |> equal "test"
    match x with
    | :? TestType2 as x -> let (Union2 str) = x in str
    | _ -> "FAIL"
    |> equal "FAIL"

[<Test>]
let ``Interface testing in pattern matching``() =
    let x = Union2 "test" :> obj
    match x with | :? ITest -> true | _ -> false
    |> equal true
    match x with | :? ITest2 -> true | _ -> false
    |> equal false

[<Test>]
let ``Type testing with JS primitive types works``() =
    let test (o: obj) =
        match o with
        | :? string -> "string"
        | :? float -> "number"
        | :? bool -> "boolean"
        | :? unit -> "null/undefined"
        | :? (unit->unit) -> "function"
        | :? System.Text.RegularExpressions.Regex -> "RegExp"
        | :? (int[]) | :? (string[]) -> "Array"
        | _ -> "unknown"
    "A" :> obj |> test |> equal "string"
    3. :> obj |> test |> equal "number"
    false :> obj |> test |> equal "boolean"
    () :> obj |> test |> equal "null/undefined"
    (fun()->()) :> obj |> test |> equal "function"
    System.Text.RegularExpressions.Regex(".") :> obj |> test |> equal "RegExp"
    [|"A"|] :> obj |> test |> equal "Array"
    [|1;2|] :> obj |> test |> equal "Array"

let inline fullname<'T> () = typeof<'T>.FullName |> normalize

[<Test>]
let ``Get fullname of generic types with inline function``() =
    fullname<TestType3>() |> equal "Fable.Tests.TypeTests.TestType3"
    fullname<TestType4>() |> equal "Fable.Tests.TypeTests.TestType4"

let inline create<'T when 'T:(new : unit -> 'T)> () = new 'T()

[<Test>]
let ``Create new generic objects with inline function``() =
    create<TestType3>().Value |> equal "Hi"
    create<TestType4>().Value2 |> equal "Bye"
    // create<TestType5>() // Doesn't compile

let inline create2<'T> (args: obj[]) =
    System.Activator.CreateInstance(typeof<'T>, args) :?> 'T

[<Test>]
let ``Create new generic objects with System.Activator``() =
    (create2<TestType3> [||]).Value |> equal "Hi"
    (create2<TestType4> [||]).Value2 |> equal "Bye"
    (create2<TestType5> [|"Yo"|]).Value |> equal "Yo"

type RenderState =
    { Now : int
      Players : Map<int, string>
      Map : string }

[<Test>]
let ``Property names don't clash with built-in JS objects``() = // See #168
    let gameState = {
        Now = 1
        Map = "dungeon"
        Players = Map.empty
    }
    gameState.Players.ContainsKey(1) |> equal false

[<Test>]
let ``Overloads work``() =
    let t = TestType5("")
    t.Overload(2) |> equal 4
    t.Overload(2, 3) |> equal 5

type T4 = TestType4

[<Test>]
let ``Type abbreviation works``() =
    let t = T4()
    t.Value2 |> equal "Bye"

type TestType6(x: int) =
    let mutable i = x
    member val Value1 = i with get, set
    member __.Value2 = i + i
    member __.Value3 with get() = i * i and set(v) = i <- v

[<Test>]
let ``Getter and Setter work``() =
    let t = TestType6(5)
    t.Value1 |> equal 5
    t.Value2 |> equal 10
    t.Value3 |> equal 25
    t.Value3 <- 10
    t.Value1 |> equal 5
    t.Value2 |> equal 20
    t.Value3 |> equal 100
    t.Value1 <- 20
    t.Value1 |> equal 20
    t.Value2 |> equal 20
    t.Value3 |> equal 100

type TestType7(a1, a2, a3) =
    let arr = [|a1; a2; a3|]
    member __.Value with get(i) = arr.[i] and set(i) (v) = arr.[i] <- v

[<Test>]
let ``Getter and Setter with indexer work``() =
    let t = TestType7(1, 2, 3)
    t.Value(1) |> equal 2
    t.Value(2) |> equal 3
    t.Value(1) <- 5
    t.Value(1) |> equal 5
    t.Value(2) |> equal 3

type A  = { thing: int } with
    member x.show() = string x.thing
    static member show (x: A) = "Static: " + (string x.thing)

type B  = { label: string } with
    member x.show() = x.label
    static member show (x: B) = "Static: " + x.label

let inline show< ^T when ^T : (member show : unit -> string)> (x:^T) : string =
   (^T : (member show : unit -> string) (x))

let inline showStatic< ^T when ^T : (static member show : ^T -> string)> (x:^T) : string =
   (^T : (static member show : ^T -> string) (x))

[<Test>]
let ``Statically resolved instance calls work``() =
    let a = { thing = 5 }
    let b = { label = "five" }
    show a |> equal "5"
    show b |> equal "five"

[<Test>]
let ``Statically resolved static calls work``() =
    let a = { thing = 5 }
    let b = { label = "five" }
    showStatic a |> equal "Static: 5"
    showStatic b |> equal "Static: five"

[<Test>]
let ``Guid.NewGuid works``() =
    let g1 = Guid.NewGuid()
    let g2 = Guid.NewGuid()
    g1 = g2 |> equal false
    let s1 = string g1
    equal 36 s1.Length
    Text.RegularExpressions.Regex.IsMatch(
        s1, "^[a-f0-9]{8}(?:-[a-f0-9]{4}){3}-[a-f0-9]{12}$")
    |> equal true
    let g3 = Guid.Parse s1
    g1 = g3 |> equal true

[<Test>]
let ``Guid.Empty works``() =
    let g1 = Guid.Empty
    string g1 |> equal "00000000-0000-0000-0000-000000000000"

[<Test>]
let ``lazy works``() =
    let mutable snitch = 0
    let lazyVal =
        lazy
            snitch <- snitch + 1
            5
    equal 0 snitch
    equal 5 lazyVal.Value
    equal 1 snitch
    lazyVal.Force() |> equal 5
    equal 1 snitch

[<Test>]
let ``Lazy.CreateFromValue works``() =
    let mutable snitch = 0
    let lazyVal =
        Lazy<_>.CreateFromValue(
            snitch <- snitch + 1
            5)
    equal 1 snitch
    equal 5 lazyVal.Value
    equal 1 snitch

[<Test>]
let ``lazy.IsValueCreated works``() =
    let mutable snitch = 0
    let lazyVal =
        Lazy<_>.Create(fun () ->
            snitch <- snitch + 1
            5)
    equal 0 snitch
    equal false lazyVal.IsValueCreated
    equal 5 lazyVal.Value
    equal true lazyVal.IsValueCreated
    lazyVal.Force() |> equal 5
    equal true lazyVal.IsValueCreated

[<Test>]
let ``Lazy constructor works``() =
    let items = Lazy<string list>(fun () -> ["a";"b";"c"])
    let search e = items.Value |> List.tryFind (fun m -> m = e)
    search "b" |> equal (Some "b")
    search "d" |> equal None

[<AllowNullLiteral>]
type Serializable(?i: int) =
    let mutable deserialized = false
    let mutable publicValue = 1
    let mutable privateValue = defaultArg i 0
    member x.PublicValue
        with get() = publicValue
        and set(i) = deserialized <- true; publicValue <- i
    override x.ToString() =
        sprintf "Public: %i - Private: %i - Deserialized: %b"
                publicValue privateValue deserialized

[<Test>]
let ``Classes can be JSON serialized forth and back``() =
    let x = Serializable(5)
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson x
    let x2 = Fable.Core.JsInterop.ofJson<Serializable> json
    string x |> equal "Public: 1 - Private: 5 - Deserialized: false"
    string x2 |> equal "Public: 1 - Private: 0 - Deserialized: true"
    let x2 = Fable.Core.JsInterop.ofJsonAsType json (x.GetType()) :?> Serializable
    string x |> equal "Public: 1 - Private: 5 - Deserialized: false"
    string x2 |> equal "Public: 1 - Private: 0 - Deserialized: true"
    let json = Fable.Core.JsInterop.toJsonWithTypeInfo x
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Serializable> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject x
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Serializable> json
    #endif
    string x |> equal "Public: 1 - Private: 5 - Deserialized: false"
    string x2 |> equal "Public: 1 - Private: 0 - Deserialized: true"

[<Test>]
let ``Null values can be JSON serialized forth and back``() =
    let x: Serializable = null
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson x
    let x2 = Fable.Core.JsInterop.ofJsonAsType json (typedefof<Serializable>)
    equal x2 null
    let json = Fable.Core.JsInterop.toJson x
    let x2 = Fable.Core.JsInterop.ofJson<Serializable> json
    equal x2 null
    let json = Fable.Core.JsInterop.toJsonWithTypeInfo x
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Serializable> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject x
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Serializable> json
    #endif
    equal x2 null

[<Test>]
let ``Classes serialized with Json.NET can be deserialized``() =
    // let x = Serializable(5)
    // let json = JsonConvert.SerializeObject(x, JsonSerializerSettings(TypeNameHandling=TypeNameHandling.All))
    let json = """{"$type":"Fable.Tests.TypeTests+Serializable","PublicValue":1}"""
    #if FABLE_COMPILER
    let x2 = Fable.Core.JsInterop.ofJson<Serializable> json
    string x2 |> equal "Public: 1 - Private: 0 - Deserialized: true"
    let x2 = Fable.Core.JsInterop.ofJsonAsType json typedefof<Serializable>
    string x2 |> equal "Public: 1 - Private: 0 - Deserialized: true"
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Serializable> json
    #else
    let x2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Serializable> json
    #endif
    string x2 |> equal "Public: 1 - Private: 0 - Deserialized: true"

type SecondaryCons(x: int) =
    new () = SecondaryCons(5)
    member __.Value = x

[<Test>]
let ``Secondary constructors work``() =
    let s1 = SecondaryCons(3)
    let s2 = SecondaryCons()
    equal 3 s1.Value
    equal 5 s2.Value

type MultipleCons(x: int, y: int) =
    new () = MultipleCons(2,3)
    new (x:int) = MultipleCons(x,4)
    member __.Value = x + y

[<Test>]
let ``Multiple constructors work``() =
    let m1 = MultipleCons()
    let m2 = MultipleCons(5)
    let m3 = MultipleCons(7,7)
    equal 5 m1.Value
    equal 9 m2.Value
    equal 14 m3.Value

[<AbstractClass>]
type AbstractClassWithDefaults () =
    abstract MethodWithDefault : unit -> string
    default x.MethodWithDefault () = "Hello "

    abstract MustImplement: unit -> string

    member x.CallMethodWithDefault () =
        x.MethodWithDefault() + x.MustImplement()

type ConcreteClass () =
    inherit AbstractClassWithDefaults()
    override x.MustImplement () = "World!!"

type ConcreteClass2 () =
    inherit AbstractClassWithDefaults()
    override x.MethodWithDefault () = "Hi "
    override x.MustImplement () = "World!!"

[<Test>]
let ``Abstract methods with default work``() = // See #505
    let x = ConcreteClass()
    x.MethodWithDefault() |> equal "Hello "
    x.MustImplement() |> equal "World!!"
    x.CallMethodWithDefault() |> equal "Hello World!!"
    let x = ConcreteClass2()
    x.CallMethodWithDefault() |> equal "Hi World!!"

type ISomeInterface =
    abstract OnlyGetProp: int with get
    abstract OnlyProp: int
    abstract Sender : int with get, set

type XISomeInterface () =
    let mutable i = 0
    interface ISomeInterface with
        member x.OnlyGetProp
            with get () = 0
        member x.OnlyProp = 3
        member x.Sender
            with get () = i
            and set i' = i <- i'

[<Test>]
let ``Interface setters don't conflict``() = // See #505
    let x = XISomeInterface () :> ISomeInterface
    x.Sender |> equal 0
    x.Sender <- 5
    x.Sender |> equal 5

type IFoo =
    abstract Foo: unit -> string

let mangleFoo(x: IFoo) = x.Foo()

type FooImplementor() =
    member x.Foo() = "foo"
    interface IFoo with
        member x.Foo() = x.Foo() + "bar"

[<AbstractClass>]
type AbstractFoo() =
    abstract member Foo: unit -> string
    interface IFoo with
        member this.Foo() =
            this.Foo() + "FOO"

type ChildFoo() =
    inherit AbstractFoo()
    override this.Foo() = "BAR"

[<Test>]
let ``A type can overload an interface method``() =
    let foo = FooImplementor()
    foo.Foo() |> equal "foo"
    (foo :> IFoo).Foo() |> equal "foobar"
    mangleFoo foo |> equal "foobar"

[<Test>]
let ``A type overloading an interface method can be inherited``() =
    let foo = ChildFoo() :> AbstractFoo
    foo.Foo() |> equal "BAR"
    (foo :> IFoo).Foo() |> equal "BARFOO"
    mangleFoo foo |> equal "BARFOO"

type BaseClass () =
    abstract member Init: unit -> int
    default self.Init () = 5

type ExtendedClass () =
    inherit BaseClass ()

    override self.Init() =
        base.Init() + 2

[<Test>]
let ``Default implementation of non-abstract class members don't get an overload index``() = // See #701
    ExtendedClass().Init() |> equal 7

type Employee = { name: string; age: float; location: Location }
and Location = { name: string; mutable employees: Employee list }

[<Test>]
let ``Circular dependencies work``() = // See #569
    let location = { name="NY"; employees=[] }
    let alice = { name="Alice"; age=20.0; location=location  }
    location.name |> equal "NY"
    alice.age |> equal 20.

[<Struct>]
type ValueType<'T> =
    new (f) = { foo = f }
    val foo : 'T
    member x.Value = x.foo

[<Struct>]
type ValueType2(i: int, j: int) =
    member x.Value = i + j

type Point2D =
   struct
      val X: float
      val Y: float
      new(xy: float) = { X = xy; Y = xy }
   end

[<Test>]
let ``Value Types work``() = // See #568
    let test = ValueType<_>("foo")
    test.Value |> equal "foo"
    test.foo |> equal "foo"
    let test2 = ValueType2(3, 4)
    test2.Value |> equal 7
    let p = Point2D(2.)
    p.Y |> equal 2.

exception MyEx of int*string

[<Test>]
let ``Custom F# exceptions work``() =
    try
        MyEx(4,"ERROR") |> raise
    with
    | MyEx(4, msg) -> msg + "!!"
    | MyEx(_, msg) -> msg + "??"
    | ex -> "unknown"
    |> equal "ERROR!!"

type MyEx2(f: float) =
  inherit Exception(sprintf "Code: %i" (int f))
  member __.Code = f

[<Test>]
let ``Custom exceptions work``() =
    try
        MyEx2(5.5) |> raise
    with
    | :? MyEx2 as ex -> ex.Message, ex.Code
    | ex -> "unknown", 0.
    |> equal ("Code: 5", 5.5)

[<Test>]
let ``reraise works``() =
    try
        try
            Exception("Will I be reraised?") |> raise
        with _ ->
            try
                reraise()
            with _ -> reraise()
        "foo"
    with ex -> ex.Message
    |> equal "Will I be reraised?"
