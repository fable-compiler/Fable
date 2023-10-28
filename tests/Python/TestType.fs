module Fable.Tests.TypeTests

open System
open Util.Testing

// Check if custom attributes can be created
type MyAttribute() =
    inherit System.Attribute()

type ITest =
    interface
    end

type ITest2 =
    interface
    end

type ITest3 =
    abstract Add2: int * int -> int

type ITest4 =
    inherit ITest3
    abstract Add: int * int -> int

type TypeTest(s: string) =
    member _.Value = s
    interface ITest

type Type2Test(s: string) =
    member _.Value = s
    interface ITest

type Type3Test() =
    member _.Value = "Hi"
    interface ITest

type Type4Test() =
    inherit Type3Test()
    member _.Value2 = "Bye"
    interface ITest2

type Type5Test(greeting: string) =
    member _.Value = greeting
    member _.Overload(x) = x + x
    member _.Overload(x, y) = x + y

type Type8Test(?greeting) =
    member _.Value = defaultArg greeting "Hi"

type Type9Test() =
    inherit Type8Test()
    let foo = Type8Test("Hello")
    member _.Greet(name) = foo.Value + " " + name

type Type10BaseTest() =
    interface ITest4 with
        member _.Add2(x, y) = x - y
        member _.Add(x, y) = x + y

type Type10ChildTest() =
    inherit Type10BaseTest()

type RenderState =
    { Now: int
      Players: Map<int, string>
      Map: string }

type T4 = Type4Test

type Type6Test(x: int) =
    let mutable i = x
    member val Value1 = i with get, set
    member _.Value2 = i + i

    member _.Value3
        with get () = i * i
        and set (v) = i <- v

type Type7Test(a1, a2, a3) =
    let arr = [| a1; a2; a3 |]

    member _.Value
        with get (i) = arr.[i]
        and set (i) (v) = arr.[i] <- v

[<Fable.Core.AttachMembers>]
type TypeAttachedTest(a1, a2, a3) =
    let arr = [| a1; a2; a3 |]
    member _.Value1
        with get () = arr.[1]
        and set (v) = arr.[1] <- v
    member _.Value
        with get (i) = arr.[i]
        and set (i) (v) = arr.[i] <- v
    member _.Item
        with get (i) = arr.[i]
        and set (i) (v) = arr.[i] <- v

type A =
    { thing: int }
    member x.show() = string x.thing
    static member show(x: A) = "Static: " + (string x.thing)

type B =
    { label: string }
    member x.show() = x.label
    static member show(x: B) = "Static: " + x.label

let inline show< ^T when ^T: (member show: unit -> string)> (x: ^T) : string = (^T: (member show: unit -> string) (x))

let inline showStatic< ^T when ^T: (static member show: ^T -> string)> (x: ^T) : string =
    (^T: (static member show: ^T -> string) (x))

[<AllowNullLiteral>]
type Serializable(?i: int) =
    let mutable deserialized = false
    let mutable publicValue = 1
    let mutable privateValue = defaultArg i 0

    member x.PublicValue
        with get () = publicValue
        and set (i) =
            deserialized <- true
            publicValue <- i

    override x.ToString() =
        sprintf "Public: %i - Private: %i - Deserialized: %b" publicValue privateValue deserialized

type SecondaryCons(x: int) =
    new() = SecondaryCons(5)
    member _.Value = x

// type SecondaryConsChild() =
//     inherit SecondaryCons()

type MultipleCons(x: int, y: int) =
    new() = MultipleCons(2, 3)
    new(x: int) = MultipleCons(x, 4)
    member _.Value = x + y

[<AbstractClass>]
type AbstractClassWithDefaults() =
    abstract MethodWithDefault: unit -> string
    default x.MethodWithDefault() = "Hello "

    abstract MustImplement: unit -> string

    member x.CallMethodWithDefault() =
        x.MethodWithDefault() + x.MustImplement()

type ConcreteClass() =
    inherit AbstractClassWithDefaults()
    override x.MustImplement() = "World!!"

type ConcreteClass2() =
    inherit AbstractClassWithDefaults()
    override x.MethodWithDefault() = "Hi "
    override x.MustImplement() = "World!!"

[<AbstractClass>]
type AbstractClass3() =
    abstract MyProp: int with get, set

type ConcreteClass3() =
    inherit AbstractClass3()
    let mutable v = 5

    override __.MyProp
        with get () = v
        and set (v2) = v <- v + v2

type ISomeInterface =
    abstract OnlyGetProp: int
    abstract OnlyProp: int
    abstract Sender: int with get, set

type XISomeInterface() =
    let mutable i = 0

    interface ISomeInterface with
        member x.OnlyGetProp = 0
        member x.OnlyProp = 3

        member x.Sender
            with get () = i
            and set i' = i <- i'

type IFoo =
    abstract Foo: unit -> string
    abstract Bar: string
    abstract MySetter: int with get, set

let mangleFoo (x: IFoo) = x.Foo()

type FooImplementor(i: int) =
    let mutable mut1 = 0
    let mutable mut2 = 5
    new() = FooImplementor(1)

    member x.Foo() = String.replicate i "foo"
    member x.Bar = "he"

    member x.MySetter
        with get () = mut1
        and set (v) = mut1 <- v + 2

    interface IFoo with
        member x.Foo() = x.Foo() + "bar"
        member x.Bar = x.Bar + "ho"

        member x.MySetter
            with get () = mut1 + mut2
            and set (v) = mut2 <- v + 3

type FooImplementorChild() =
    inherit FooImplementor(3)

[<AbstractClass>]
type AbstractFoo() =
    abstract member Foo2: unit -> string

    interface IFoo with
        member this.Foo() = this.Foo2() + "FOO"
        member x.Bar = ""

        member x.MySetter
            with get () = 0
            and set (v) = ()

type ChildFoo() =
    inherit AbstractFoo()
    override this.Foo2() = "BAR"

type BaseClass(x: int) =
    abstract member Init: unit -> int
    default __.Init() = x
    abstract member Prop: string
    default __.Prop = "base"

type ExtendedClass() =
    inherit BaseClass(5)
    override __.Init() = base.Init() + 2
    override __.Prop = base.Prop + "-extension"

type BaseClass2() =
    let field = 1
    member _.A() = field

type ExtendedClass2() =
    inherit BaseClass2()
    member _.A() = 2
    member _.B() = base.A()

type Employee =
    { name: string
      age: float
      location: Location }

and Location =
    { name: string
      mutable employees: Employee list }

[<Struct>]
type ValueType<'T> =
    new(v) = { value = v }
    val value: 'T
    member x.Value = x.value

[<Struct>]
type ValueType1<'T>(value: 'T) =
    member x.Value = value

[<Struct>]
type ValueType2(i: int, j: int) =
    member x.Value = i + j

type ValueType3 =
    struct
        val mutable public X: int
    end

[<Struct>]
type StructUnion = Value of string

type Point2D =
    struct
        val X: float
        val Y: float
        new(xy: float) = { X = xy; Y = xy }
    end

exception MyEx of int * string

type MyEx2(f: float) =
    inherit exn(sprintf "Code: %i" (int f))
    member _.Code = f

type ThisContextInConstructor(v) =
    let f () = v
    member val Value = f

type DowncastTest(value: int) =
    member _.Value = value

    interface System.IDisposable with
        member _.Dispose() = ()

[<Class>]
type TypeWithClassAttribute =
    val Pos: int
    new(pos) = { Pos = pos }

// -------------------------------------------------------------
// Issue #1975: https://github.com/fable-compiler/Fable/issues/1975
// In previous version of Fable, using type with parameterized units of measure was causing an endless loops in the compiler

type TypeWithParameterizedUnitMeasureTest<[<Measure>] 't> =
    private
    | TypeWithParameterizedUnitMeasureTestType of float<'t>

    member this.Value =
        match this with
        | TypeWithParameterizedUnitMeasureTestType value -> value

let makeTypeWithParameterizedUnitMeasureTestType (value: float<_>) : TypeWithParameterizedUnitMeasureTest<_> =
    TypeWithParameterizedUnitMeasureTestType value

open FSharp.Data.UnitSystems.SI.UnitSymbols

type TypeWithParameterizedUnitMeasureTest_Test =
    { Field: TypeWithParameterizedUnitMeasureTest<m> }

// -------------------------------------------------------------

// Tested ported from https://github.com/fable-compiler/Fable/pull/1336/files
type TypeWithDefaultValueTest() =
    [<DefaultValue>]
    val mutable IntValue: int

    [<DefaultValue>]
    val mutable StringValue: string

    [<DefaultValue>]
    val mutable ObjValue: System.Collections.Generic.Dictionary<string, string>

type Default1 = int

type Distinct1 =
    // Overloads only distinguished by generic constrain work, see #1908
    static member inline Distinct1(x: ^``Collection<'T>``, _impl: Default1) : '``Collection<'T>`` =
        (^``Collection<'T>``: (static member Distinct1: _ -> _) x)

    static member inline Distinct1(_: ^t when ^t: null and ^t: struct, _mthd: Default1) = id //must

    // Overloads only distinguished by struct tuple work, see #2417
    static member OfList(_elements: list<'K * 'V>) = ()
    static member OfList(_elements: list<struct ('K * 'V)>) = ()

type InfoA = { Foo: string }

type InfoB = { InfoA: InfoA; Bar: string }

[<AbstractClass>]
type InfoAClass(info: InfoA) =
    abstract WithInfo: InfoA -> InfoAClass
    member _.Foo = info.Foo
    member this.WithFoo foo = this.WithInfo({ info with Foo = foo })

type InfoBClass(info: InfoB) =
    inherit InfoAClass(info.InfoA)

    override this.WithInfo(infoA) =
        InfoBClass({ info with InfoA = infoA }) :> InfoAClass

type FooInterface =
    abstract Foo: string with get, set
    abstract DoSomething: f: (float -> float -> float) * v: float -> float
    abstract Item: int -> char with get, set
    abstract Sum: [<ParamArray>] items: string [] -> string

[<Fable.Core.Mangle>]
type BarInterface =
    abstract Bar: string with get, set
    abstract DoSomething: f: (float -> float -> float) * v: float -> float
    abstract Item: int -> char with get, set
    abstract Item: char -> bool with get
    abstract Sum: [<ParamArray>] items: string [] -> string

[<AbstractClass>]
type FooAbstractClass(x: float) =
    member _.Value = x
    member _.DoSomething(x, y) = x * y
    abstract DoSomething: float -> float

type FooClass(x) =
    inherit FooAbstractClass(5.)
    let mutable x = x
    override this.DoSomething(x) = this.DoSomething(x, this.Value)

    static member ChangeChar(s: string, i: int, c: char) =
        s.ToCharArray()
        |> Array.mapi (fun i2 c2 -> if i = i2 then c else c2)
        |> String

    interface FooInterface with
        member _.Foo
            with get () = x
            and set (y) = x <- y

        member this.DoSomething(f, x) =
            let f = f x
            let x = f 2.
            let y = f 8.
            this.DoSomething(x + y)

        member _.Item
            with get (i) = x.[i]
            and set i c = x <- FooClass.ChangeChar(x, i, c)

        member _.Sum(items) =
            Array.reduce (fun x y -> x + y + x + y) items

[<AbstractClass>]
type BarAbstractClass(x: float) =
    member _.Value = x
    member _.DoSomething(x, y) = x ** y
    abstract DoSomething: float -> float

type BarClass(x) =
    inherit BarAbstractClass(10.)
    let mutable x = x
    override this.DoSomething(x) = this.DoSomething(x, this.Value)

    interface BarInterface with
        member _.Bar
            with get () = x
            and set (y) = x <- y

        member this.DoSomething(f, x) =
            let f = f x
            let x = f 4.5
            let y = f 7.
            this.DoSomething(x - y)

        member _.Item
            with get (i) = x.[i]
            and set i c = x <- FooClass.ChangeChar(x, i + 1, c)

        member _.Item
            with get (c) = x.ToCharArray() |> Array.exists ((=) c)

        member _.Sum(items) =
            Array.reduce (fun x y -> x + x + y + y) items

type Interface2 =
    abstract Value: int
    abstract Add: unit -> int

type Interface1 =
    abstract Create: int -> Interface2

type MixedThese(x: int) =
    member _.Value = x

    interface Interface1 with
        member this1.Create(y: int) =
            { new Interface2 with
                member _.Value = y
                member this2.Add() = this1.Value + this2.Value }

let areEqual (x: obj) (y: obj) = x = y

type MyUnion1 =
    | Foo of int * int
    | Bar of float
    | Baz

type MyUnion2 =
    | Foo of int * int
    override _.ToString() = "ffff"

type MyRecord1 = { Foo: int; Bar: string }
type MyRecord2 = { Foo: int; Bar: string }

type SubclassTest1() =
    class
    end

type SubclassTest2() =
    inherit SubclassTest1()

type SubclassTest3() =
    inherit SubclassTest2()

[<Measure>]
type a

[<Measure>]
type b

[<Measure>]
type c = a * b

[<Measure>]
type d = a / b

type MeasureTest<[<Measure>] 'T> = { X: float<'T> }
type MeasureTestGen<[<Measure>] 'T, 'V> = { X: float<'T>; Y: 'V }

type MeasureTest1_ = { Y: MeasureTestGen<a, int> }
type MeasureTest1 = { Y: MeasureTest<a * b> }
type MeasureTest2 = { Y: float<c> }
type MeasureTest3 = { Y: MeasureTest<a / b> }
type MeasureTest4 = { Y: float<d> }

// Check that types with product measures compile, see #2532
type MeasureTest5 = { Y: MeasureTest<c> }
type MeasureTest6 = { Y: MeasureTest<d> }

type EnumFoo =
    | Foo = 0
    | Bar = 1

[<AbstractClass>]
type MangledAbstractClass1() =
    class
    end

[<AbstractClass>]
type MangledAbstractClass2(v: int) =
    inherit MangledAbstractClass1()
    abstract MyMethod: int -> int
    default _.MyMethod(x: int) = x * v

[<AbstractClass>]
type MangledAbstractClass3(v) =
    inherit MangledAbstractClass2(v + 3)

[<AbstractClass>]
type MangledAbstractClass4(v) =
    inherit MangledAbstractClass3(v + 4)
    override _.MyMethod(x: int) = base.MyMethod(x) - v

[<AbstractClass>]
type MangledAbstractClass5(v) =
    inherit MangledAbstractClass4(v + 5)
    override _.MyMethod(x: int) = base.MyMethod(x) + v + 7

type ConcreteClass1() =
    inherit MangledAbstractClass5(2)

// TODO: This test produces different results in Fable and .NET
// See Fable.Transforms.FSharp2Fable.TypeHelpers.makeTypeGenArgs
// [<Fact>]
//let ``test Reflection for types with measures work`` () =
//     Reflection.FSharpType.GetRecordFields(typeof<MeasureTest1_>)
//     |> Array.item 0
//     |> fun fi -> fi.PropertyType.GetGenericArguments().Length
//     |> equal 1

[<Fact>]
let ``test Types can instantiate their parent in the constructor`` () =
    let t = Type9Test()
    t.Greet("Maxime") |> equal "Hello Maxime"

[<Fact>]
let ``test Type testing`` () =
    let x = TypeTest "test" :> obj
    let y = Type2Test "test" :> obj
    x :? TypeTest |> equal true
    x :? Type2Test |> equal false
    y :? TypeTest |> equal false
    y :? Type2Test |> equal true

[<Fact>]
let ``test Type testing in pattern matching`` () =
    let x = TypeTest "test" :> obj

    match x with
    | :? TypeTest as x -> x.Value
    | _ -> "FAIL"
    |> equal "test"

    match x with
    | :? Type2Test as x -> x.Value
    | _ -> "FAIL"
    |> equal "FAIL"

// TODO: Should we make interface testing work in Fable 2?
// [<Fact>]
//let ``test Children inherits parent interfaces`` () =
//     let t4 = TestType4() |> box
//     t4 :? ITest |> equal true

// [<Fact>]
//let ``test Interface testing`` () =
//     let x = Union1 "test" :> obj
//     let y = Union2 "test" :> obj
//     x :? ITest |> equal true
//     x :? ITest2 |> equal false
//     y :? ITest |> equal true
//     y :? ITest2 |> equal false

// [<Fact>]
//let ``test Interface testing in pattern matching`` () =
//     let x = Union2 "test" :> obj
//     match x with | :? ITest -> true | _ -> false
//     |> equal true
//     match x with | :? ITest2 -> true | _ -> false
//     |> equal false

[<Fact>]
let ``test Type testing with PY primitive types works`` () =
    let test (o: obj) =
        match o with
        | :? string -> "string"
        | :? float -> "number"
        | :? bool -> "boolean"
        | :? unit -> "unit"
        | :? System.Text.RegularExpressions.Regex -> "RegExp"
        | :? (int [])
        | :? (string []) -> "Array"
        | _ -> "unknown"

    "A" :> obj |> test |> equal "string"
    3. :> obj |> test |> equal "number"
    false :> obj |> test |> equal "boolean"
    () :> obj |> test |> equal "unit"
    // Workaround to make sure Fable is passing the argument
    let a = () :> obj in
    test a |> equal "unit"

    System.Text.RegularExpressions.Regex(".") :> obj
    |> test
    |> equal "RegExp"

    [| "A" |] :> obj |> test |> equal "Array"
    [| 1; 2 |] :> obj |> test |> equal "Array"

[<Fact>]
let ``test Type test with Date`` () =
    let isDate (x: obj) =
        match x with
        | :? DateTime -> true
        | _ -> false

    DateTime.Now |> box |> isDate |> equal true
    box 5 |> isDate |> equal false

[<Fact>]
let ``test Type test with Long`` () =
    let isLong (x: obj) =
        match x with
        | :? int64 -> true
        | _ -> false

    box 5L |> isLong |> equal true
//box 50 |> isLong |> equal false

[<Fact>]
let ``test Type test with BigInt`` () =
    let isBigInd (x: obj) =
        match x with
        | :? bigint -> true
        | _ -> false

    box 5I |> isBigInd |> equal true
//box 50 |> isBigInd |> equal false

[<Fact>]
let ``test Property names don't clash with built-in JS objects`` () = // See #168
    let gameState =
        { Now = 1
          Map = "dungeon"
          Players = Map.empty }

    gameState.Players.ContainsKey(1) |> equal false

[<Fact>]
let ``test Overloads work`` () =
    let t = Type5Test("")
    t.Overload(2) |> equal 4
    t.Overload(2, 3) |> equal 5

[<Fact>]
let ``test Type abbreviation works`` () =
    let t = T4()
    t.Value2 |> equal "Bye"

[<Fact>]
let ``test Getter and Setter work`` () =
    let t = Type6Test(5)
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

[<Fact>]
let ``test Getter and Setter with indexer work`` () =
    let t = Type7Test(1, 2, 3)
    t.Value(1) |> equal 2
    t.Value(2) |> equal 3
    t.Value(1) <- 5
    t.Value(1) |> equal 5
    t.Value(2) |> equal 3

[<Fact>]
let ``test Attached Getters Setters and Indexers work`` () =
    let t = TypeAttachedTest(1, 2, 3)
    t.Value1 |> equal 2
    t.Value1 <- 22
    t.Value1 |> equal 22
    t.Value(0) |> equal 1
    t.Value(0) <- 11
    t.Value(0) |> equal 11
    t.[2] |> equal 3
    t.[2] <- 33
    t.[2] |> equal 33

[<Fact>]
let ``test Statically resolved instance calls work`` () =
    let a = { thing = 5 }
    let b = { label = "five" }
    show a |> equal "5"
    show b |> equal "five"

[<Fact>]
let ``test Statically resolved static calls work`` () =
    let a = { thing = 5 }
    let b = { label = "five" }
    showStatic a |> equal "Static: 5"
    showStatic b |> equal "Static: five"

[<Fact>]
let ``test Guid.NewGuid works`` () =
    let g1 = Guid.NewGuid()
    let g2 = Guid.NewGuid()
    g1 = g2 |> equal false
    let s1 = string g1
    equal 36 s1.Length

    Text.RegularExpressions.Regex.IsMatch(s1, "^[a-f0-9]{8}(?:-[a-f0-9]{4}){3}-[a-f0-9]{12}$")
    |> equal true

    let g3 = Guid.Parse s1
    g1 = g3 |> equal true

[<Fact>]
let ``test Guid.Empty works`` () =
    let g1 = Guid.Empty

    string g1
    |> equal "00000000-0000-0000-0000-000000000000"

[<Fact>]
let ``test Guid.ToString works`` () =
    let g1 = Guid.Parse "dec42487-c02b-42a6-9a10-0263a5a7fdf1"

    string g1
    |> equal "dec42487-c02b-42a6-9a10-0263a5a7fdf1"

[<Fact>]
let ``test lazy works`` () =
    let mutable snitch = 0

    let lazyVal =
        lazy
            (snitch <- snitch + 1
             5)

    equal 0 snitch
    equal 5 lazyVal.Value
    equal 1 snitch
    lazyVal.Force() |> equal 5
    equal 1 snitch

[<Fact>]
let ``test Lazy.CreateFromValue works`` () =
    let mutable snitch = 0

    let lazyVal =
        Lazy<_>.CreateFromValue
            (snitch <- snitch + 1
             5)

    equal 1 snitch
    equal 5 lazyVal.Value
    equal 1 snitch

[<Fact>]
let ``test lazy.IsValueCreated works`` () =
    let mutable snitch = 0

    let lazyVal =
        Lazy<_>.Create
            (fun () ->
                snitch <- snitch + 1
                5)

    equal 0 snitch
    equal false lazyVal.IsValueCreated
    equal 5 lazyVal.Value
    equal true lazyVal.IsValueCreated
    lazyVal.Force() |> equal 5
    equal true lazyVal.IsValueCreated

[<Fact>]
let ``test Lazy constructor works`` () =
    let items = Lazy<string list>(fun () -> [ "a"; "b"; "c" ])

    let search e =
        items.Value |> List.tryFind (fun m -> m = e)

    search "b" |> equal (Some "b")
    search "d" |> equal None

[<Fact>]
let ``test Secondary constructors work`` () =
    let s1 = SecondaryCons(3)
    let s2 = SecondaryCons()
    equal 3 s1.Value
    equal 5 s2.Value

// [<Fact>]
//let ``test Inheriting from secondary constructors works`` () =
//     let s = SecondaryConsChild()
//     equal 5 s.Value

[<Fact>]
let ``test Multiple constructors work`` () =
    let m1 = MultipleCons()
    let m2 = MultipleCons(5)
    let m3 = MultipleCons(7, 7)
    equal 5 m1.Value
    equal 9 m2.Value
    equal 14 m3.Value

[<Fact>]
let ``test Abstract methods with default work`` () = // See #505
    let x = ConcreteClass()
    x.MethodWithDefault() |> equal "Hello "
    x.MustImplement() |> equal "World!!"
    x.CallMethodWithDefault() |> equal "Hello World!!"
    let x = ConcreteClass2()
    x.CallMethodWithDefault() |> equal "Hi World!!"

[<Fact>]
let ``test Abstract properties with getters and setters work`` () =
    let x = ConcreteClass3() :> AbstractClass3
    x.MyProp <- 2
    equal 7 x.MyProp

[<Fact>]
let ``test Interface setters don't conflict`` () = // See #505
    let x = XISomeInterface() :> ISomeInterface
    x.Sender |> equal 0
    x.Sender <- 5
    x.Sender |> equal 5

[<Fact>]
let ``test A type can overload an interface method`` () =
    let foo = FooImplementor()
    foo.Foo() |> equal "foo"
    (foo :> IFoo).Foo() |> equal "foobar"
    mangleFoo foo |> equal "foobar"

[<Fact>]
let ``test A child can be casted to parent's interface`` () =
    let foo = FooImplementorChild()
    foo.Foo() |> equal "foofoofoo"
    (foo :> IFoo).Foo() |> equal "foofoofoobar"
    mangleFoo foo |> equal "foofoofoobar"

[<Fact>]
let ``test A type can overload an interface getter`` () =
    let foo = FooImplementor()
    foo.Bar |> equal "he"
    (foo :> IFoo).Bar |> equal "heho"

[<Fact>]
let ``test A type can overload an interface setter`` () =
    let foo = FooImplementor()
    foo.MySetter <- 7
    foo.MySetter |> equal 9
    (foo :> IFoo).MySetter <- 7
    (foo :> IFoo).MySetter |> equal 19

// TODO: Interface and abstract methods with same name clash
[<Fact>]
let ``test A type overloading an interface method can be inherited`` () =
    let foo = ChildFoo() :> AbstractFoo
    foo.Foo2() |> equal "BAR"
    (foo :> IFoo).Foo() |> equal "BARFOO"
    mangleFoo foo |> equal "BARFOO"

[<Fact>]
let ``test Interface casting round-trip`` () = // See #1452
    let d = new DowncastTest(3) :> System.IDisposable
    let t = d :?> DowncastTest
    t.Value |> equal 3

    equal 3
    <| match d with
       | :? DowncastTest as t2 -> t2.Value
       | _ -> 5

[<Fact>]
let ``test Calling default implementation of base members don't cause infinite recursion`` () = // See #701
    let x = ExtendedClass()
    x.Init() |> equal 7
    (x :> BaseClass).Init() |> equal 7

[<Fact>]
let ``test Calling default implementation of base properties don't cause infinite recursion`` () = // See #701
    let x = ExtendedClass()
    x.Prop |> equal "base-extension"
    (x :> BaseClass).Prop |> equal "base-extension"

[<Fact>]
let ``test Calling base members works`` () = // See #1464
    let bar = ExtendedClass2()
    bar.B() |> equal 1

[<Fact>]
let ``test Circular dependencies work`` () = // See #569
    let location = { name = "NY"; employees = [] }

    let alice =
        { name = "Alice"
          age = 20.0
          location = location }

    location.name |> equal "NY"
    alice.age |> equal 20.

[<Fact>]
let ``test Value Type records work`` () = // See #568
    let foo1 = ValueType<_>("foo")
    let foo2 = ValueType<_>("foo")
    foo1.Value |> equal "foo"
    foo1.value |> equal "foo"
    foo1 = foo2 |> equal true

[<Fact>]
let ``test Value Type unions work`` () =
    let du1 = StructUnion.Value "du"
    let du2 = StructUnion.Value "du"
    du1 = du2 |> equal true

[<Fact>]
let ``test Value Type tuples work`` () =
    let tu1 = struct ("a", "b")
    let tu2 = struct ("a", "b")
    tu1 = tu2 |> equal true

[<Fact>]
let ``test Value Types work`` () =
    let bar1 = ValueType1("bar")
    let bar2 = ValueType1("bar")
    bar1.Value |> equal "bar"
    bar1 = bar2 |> equal true

[<Fact>]
let ``test Other Value Types work`` () =
    let test2 = ValueType2(3, 4)
    test2.Value |> equal 7
    let p = Point2D(2.)
    p.Y |> equal 2.

[<Fact>]
let ``test struct without explicit ctor works`` () =
    let t1 = ValueType3(X = 10)
    t1.X |> equal 10
    let mutable t2 = ValueType3()
    t2.X |> equal 0
    t1 |> notEqual t2
    (compare t1 t2) |> equal 1
    t2.X <- 10
    t1 |> equal t2

    (compare t1 t2) |> equal 0

[<Fact>]
let ``test Custom F# exceptions work`` () =
    try
        MyEx(4, "ERROR") |> raise
    with
    | MyEx (4, msg) as e -> (box e :? Exception, msg + "!!")
    | MyEx (_, msg) as e -> (box e :? Exception, msg + "??")
    | ex -> (false, "unknown")
    |> equal (true, "ERROR!!")

[<Fact>]
let ``test Custom exceptions work`` () =
    try
        MyEx2(5.5) |> raise
    with
    | :? MyEx2 as ex -> (box ex :? Exception, ex.Message, ex.Code)
    | ex -> (false, "unknown", 0.)
    |> equal (true, "Code: 5", 5.5)

[<Fact>]
let ``test reraise works`` () =
    try
        try
            Exception("Will I be reraised?") |> raise
        with
        | _ ->
            try
                reraise ()
            with
            | _ -> reraise ()

        "foo"
    with
    | ex -> ex.Message
    |> equal "Will I be reraised?"

[<Fact>]
let ``test This context is not lost in closures within implicit constructor`` () = // See #1444
    ThisContextInConstructor(7).Value() |> equal 7

[<Fact>]
let ``test Type can be casted to an interface implemented by parent`` () =
    let c = Type10ChildTest()
    let f1 = c :> ITest4
    let f2 = c :> ITest3
    f1.Add(4, 5) |> equal 9
    f1.Add2(4, 5) |> equal -1
    f2.Add2(4, 5) |> equal -1

[<Fact>]
let ``test ClassAttribute works`` () = // See #573
    let t1 = TypeWithClassAttribute(8)
    t1.Pos |> equal 8

[<Fact>]
let ``test Issue #1975: Compile type with parameterized units of measure as generic`` () =
    let a = makeTypeWithParameterizedUnitMeasureTestType 2.
    equal 2. a.Value

// Test ported from https://github.com/fable-compiler/Fable/pull/1336/files
//    [<Fact>]
//let ``test default value attributes works`` () =
//        let withDefaultValue = TypeWithDefaultValueTest()
//
//        withDefaultValue.IntValue |> equal Unchecked.defaultof<int>
//        withDefaultValue.IntValue |> equal 0
//
//        withDefaultValue.StringValue |> equal Unchecked.defaultof<string>
//        withDefaultValue.StringValue |> equal null
//
//        withDefaultValue.ObjValue |> equal Unchecked.defaultof<System.Collections.Generic.Dictionary<string, string>>
//        withDefaultValue.ObjValue |> equal null

[<Fact>]
let ``test Private fields don't conflict with parent classes`` () = // See #2070
    let a1 = InfoBClass({ InfoA = { Foo = "foo" }; Bar = "bar" }) :> InfoAClass
    let a2 = a1.WithFoo("foo2")
    a1.Foo |> equal "foo"
    a2.Foo |> equal "foo2"

(* TODO
// See #2084
[<Fact>]
let ``test Non-mangled interfaces work with object expressions`` () =
    let mutable foo = "Foo"

    let foo =
        { new FooInterface with
            member _.Foo = foo

            member _.Foo
                with set x = foo <- x

            member _.DoSomething(f, x) = let f = f 1. in f x * f 0.2

            member _.Item
                with get (i) = foo.[i]

            member _.Item
                with set i c = foo <- FooClass.ChangeChar(foo, i - 1, c)

            member _.Sum(items) = Array.reduce (+) items }

    let addPlus2 x y = x + y + 2.
    let multiplyTwice x y = x * y * y

    foo.[3] <- 'W'

    foo.Foo <-
        foo.Foo
        + foo
            .DoSomething(addPlus2, 3.)
            .ToString("F2")
            .Replace(",", ".")
        + foo.[2].ToString()

    foo.Foo <- foo.Foo + foo.Sum("a", "bc", "d")

    foo.Foo |> equal "FoW19.20Wabcd"
// See #2084
[<Fact>]
let ``test Mangled interfaces work with object expressions`` () =
    let mutable bar = "Bar"

    let bar =
        { new BarInterface with
            member _.Bar = bar

            member _.Bar
                with set x = bar <- x

            member _.DoSomething(f, x) = let f = f 4.3 in f x + f x

            member _.Item
                with get (i) = bar.[i]

            member _.Item
                with set _ c = bar <- FooClass.ChangeChar(bar, 0, c)

            member _.Item
                with get (c) = bar.ToCharArray() |> Array.exists ((=) c)

            member _.Sum(items) = Array.rev items |> Array.reduce (+) }

    let addPlus2 x y = x + y + 2.
    let multiplyTwice x y = x * y * y

    bar.[3] <- 'Z'

    bar.Bar <-
        bar.Bar
        + bar
            .DoSomething(multiplyTwice, 3.)
            .ToString("F2")
            .Replace(",", ".")
        + bar.[2].ToString()
        + (sprintf "%b%b" bar.['B'] bar.['x'])

    bar.Bar <- bar.Bar + bar.Sum("a", "bc", "d")

    bar.Bar |> equal "Zar77.40rfalsefalsedbca"
*)

// See #2084
[<Fact>]
let ``test Non-mangled interfaces work with classes`` () =
    let addPlus2 x y = x + y + 2.
    let multiplyTwice x y = x * y * y
    let foo2 = FooClass("Foo") :> FooInterface
    foo2.[0] <- 'W'

    foo2.Foo <-
        foo2.Foo
        + foo2
            .DoSomething(multiplyTwice, 3.)
            .ToString("F2")
            .Replace(',', '.')
        + foo2.[2].ToString()

    foo2.Foo <- foo2.Foo + foo2.Sum("a", "bc", "d")
    foo2.Foo |> equal "Woo1020.00oabcabcdabcabcd"

// See #2084
[<Fact>]
let ``test Mangled interfaces work with classes`` () =
    let addPlus2 x y = x + y + 2.
    let multiplyTwice x y = x * y * y
    let bar2 = BarClass("Bar") :> BarInterface
    bar2.[0] <- 'Z'

    bar2.Bar <-
        bar2.Bar
        + bar2
            .DoSomething(addPlus2, 3.)
            .ToString("F2")
            .Replace(",", ".")
        + bar2.[2].ToString()
        + (sprintf "%b%b" bar2.['B'] bar2.['x'])

    bar2.Bar <- bar2.Bar + bar2.Sum("a", "bc", "d")

    bar2.Bar
    |> equal "BZr9536.74rtruefalseaabcbcaabcbcdd"

[<Fact>]
let ``test Multiple `this` references work in nested attached members`` () =
    (MixedThese(2) :> Interface1).Create(3).Add()
    |> equal 5

[<Fact>]
let ``test Two unions of different type with same shape are not equal`` () =
    areEqual (MyUnion1.Foo(1, 2)) (MyUnion2.Foo(1, 2))
    |> equal false

    areEqual (MyUnion1.Foo(1, 2)) (MyUnion1.Foo(1, 2))
    |> equal true

[<Fact>]
let ``test Two records of different type with same shape are not equal`` () =
    areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord2.Foo = 2; Bar = "oh" }
    |> equal false

    areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord1.Foo = 2; Bar = "oh" }
    |> equal true

[<Fact>]
let ``test IsSubclassOf checks whole hierarchy`` () =
    typeof<SubclassTest2>.IsSubclassOf (typeof<SubclassTest1>)
    |> equal true

    typeof<SubclassTest3>.IsSubclassOf (typeof<SubclassTest1>)
    |> equal true

[<Fact>]
let ``test IsInstanceOfType works with class types`` () =
    let s1, s2 = SubclassTest1(), SubclassTest2()
    typeof<obj>.IsInstanceOfType (s1) |> equal true

    typeof<SubclassTest1>.IsInstanceOfType (s1)
    |> equal true

    typeof<SubclassTest2>.IsInstanceOfType (s1)
    |> equal false

    typeof<SubclassTest3>.IsInstanceOfType (s1)
    |> equal false

    typeof<SubclassTest1>.IsInstanceOfType (s2)
    |> equal true

    typeof<SubclassTest2>.IsInstanceOfType (s2)
    |> equal true

    typeof<SubclassTest3>.IsInstanceOfType (s2)
    |> equal false

[<Fact>]
let ``test IsInstanceOfType works with nominal records`` () =
    typeof<MyRecord1>.IsInstanceOfType ({ MyRecord1.Foo = 2; Bar = "oh" })
    |> equal true

    typeof<obj>.IsInstanceOfType ({ MyRecord1.Foo = 2; Bar = "oh" })
    |> equal true

    typeof<MyRecord2>.IsInstanceOfType ({ MyRecord1.Foo = 2; Bar = "oh" })
    |> equal false

[<Fact>]
let ``test IsInstanceOfType works with nominal unions`` () =
    typeof<MyUnion1>.IsInstanceOfType (MyUnion1.Foo(1, 2))
    |> equal true

    typeof<obj>.IsInstanceOfType (MyUnion1.Foo(1, 2))
    |> equal true

    typeof<MyUnion2>.IsInstanceOfType (MyUnion1.Foo(1, 2))
    |> equal false

// Expected to always return true for any numeric type, just like :? operator
[<Fact>]
let ``test IsInstanceOfType works with enums`` () =
    typeof<EnumFoo>.IsInstanceOfType (EnumFoo.Foo)
    |> equal true

    typeof<EnumFoo>.IsInstanceOfType (EnumFoo.Bar)
    |> equal true

    typeof<obj>.IsInstanceOfType (EnumFoo.Bar)
    |> equal true

// Expected to always return true for any function and function type, just like :? operator
[<Fact>]
let ``test IsInstanceOfType works with functions`` () =
    typeof<unit -> unit>.IsInstanceOfType (fun () -> ())
    |> equal true

    typeof<obj>.IsInstanceOfType (fun () -> ())
    |> equal true
    //typeof<unit -> int>.IsInstanceOfType(fun () -> ()) |> equal false
    typeof<string -> int>.IsInstanceOfType (String.length)
    |> equal true

    typeof<obj>.IsInstanceOfType (String.length)
    |> equal true
//typeof<int -> int>.IsInstanceOfType(String.length) |> equal false

[<Fact>]
let ``test IsInstanceOfType works with primitives`` () =
    typeof<string>.IsInstanceOfType ("hello")
    |> equal true

    typeof<obj>.IsInstanceOfType ("hello")
    |> equal true

    typeof<string>.IsInstanceOfType (5) |> equal false
    typeof<int>.IsInstanceOfType (5) |> equal true
    typeof<obj>.IsInstanceOfType (5) |> equal true

    typeof<int>.IsInstanceOfType ("hello")
    |> equal false

#if FABLE_COMPILER
[<Fact>]
let ``test Choice with arity 3+ is represented correctly`` () = // See #2485
    Choice2Of3 55
    |> Fable.Core.Reflection.getCaseName
    |> equal "Choice2Of3"

    Choice3Of3 55
    |> Fable.Core.Reflection.getCaseName
    |> equal "Choice3Of3"
#endif

[<Fact>]
let ``test Can call the base version of a mangled abstract method that was declared above in the hierarchy`` () =
    let c = ConcreteClass1()
    c.MyMethod(4) |> equal 58
