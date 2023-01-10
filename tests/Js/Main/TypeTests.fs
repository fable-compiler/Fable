module Fable.Tests.TypeTests

open System
open Util.Testing

// Check if custom attributes can be created
type MyAttribute() =
    inherit System.Attribute()

type ITest = interface end
type ITest2 = interface end

type ITest3 =
    abstract Add2: int * int -> int

type ITest4 =
    inherit ITest3
    abstract Add: int * int -> int

type TestType(s: string) =
    member __.Value = s
    interface ITest

type TestType2(s: string) =
    member __.Value = s
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

type TestType8(?greeting) =
    member __.Value = defaultArg greeting "Hi"

type TestType9() =
    inherit TestType8()
    let foo = TestType8("Hello")
    member __.Greet(name) = foo.Value + " " + name

type TestType10Base() =
    interface ITest4 with
        member __.Add2(x, y) = x - y
        member __.Add(x, y) = x + y

type TestType10Child() =
    inherit TestType10Base()

type RenderState =
    { Now : int
      Players : Map<int, string>
      Map : string }

type T4 = TestType4

type TestType6(x: int) =
    let mutable i = x
    member val Value1 = i with get, set
    member __.Value2 = i + i
    member __.Value3 with get() = i * i and set(v) = i <- v

type TestType7(a1, a2, a3) =
    let arr = [|a1; a2; a3|]
    member __.Value with get(i) = arr.[i] and set(i) (v) = arr.[i] <- v

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

type SecondaryCons(x: int) =
    new () = SecondaryCons(5)
    member __.Value = x

// type SecondaryConsChild() =
//     inherit SecondaryCons()

type MultipleCons(x: int, y: int) =
    new () = MultipleCons(2,3)
    new (x:int) = MultipleCons(x,4)
    member __.Value = x + y

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

[<AbstractClass>]
type AbstractClass3() =
    abstract MyProp: int with get, set

type ConcreteClass3() =
    inherit AbstractClass3()
    let mutable v = 5
    override __.MyProp with get() = v and set(v2) = v <- v + v2

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

type IFoo =
    abstract Foo: unit -> string
    abstract Bar: string
    abstract MySetter: int with get, set

let mangleFoo(x: IFoo) = x.Foo()

type FooImplementor(i: int) =
    let mutable mut1 = 0
    let mutable mut2 = 5
    new () = FooImplementor(1)

    member x.Foo() = String.replicate i "foo"
    member x.Bar = "he"
    member x.MySetter with get() = mut1 and set(v) = mut1 <- v + 2

    interface IFoo with
        member x.Foo() = x.Foo() + "bar"
        member x.Bar = x.Bar + "ho"
        member x.MySetter with get() = mut1 + mut2 and set(v) = mut2 <- v + 3

type FooImplementorChild() =
    inherit FooImplementor(3)

[<AbstractClass>]
type AbstractFoo() =
    abstract member Foo2: unit -> string
    interface IFoo with
        member this.Foo() = this.Foo2() + "FOO"
        member x.Bar = ""
        member x.MySetter with get() = 0 and set(v) = ()

type ChildFoo() =
    inherit AbstractFoo()
    override this.Foo2() = "BAR"

type BaseClass (x: int) =
    abstract member Init: unit -> int
    default __.Init () = x
    abstract member Prop: string
    default __.Prop = "base"

type ExtendedClass () =
    inherit BaseClass(5)
    override __.Init() = base.Init() + 2
    override __.Prop = base.Prop + "-extension"

type BaseClass2() =
    let field = 1
    member __.A() = field

type ExtendedClass2() =
    inherit BaseClass2()
    member __.A() = 2
    member __.B() = base.A()

type Employee = { name: string; age: float; location: Location }
and Location = { name: string; mutable employees: Employee list }

[<Struct>]
type ValueType<'T> =
    new (v) = { value = v }
    val value : 'T
    member x.Value = x.value

[<Struct>]
type ValueType1<'T>(value: 'T) =
    member x.Value = value

[<Struct>]
type ValueType2(i: int, j: int) =
    member x.Value = i + j

type ValueType3 =
  struct
    val mutable public X : int
  end

[<Struct>]
type StructUnion = Value of string

type Point2D =
   struct
      val X: float
      val Y: float
      new(xy: float) = { X = xy; Y = xy }
   end

exception MyEx of int*string

type MyEx2(f: float) =
  inherit exn(sprintf "Code: %i" (int f))
  member __.Code = f

type ThisContextInConstructor(v) =
    let f () = v
    member val Value = f

type DowncastTest(value: int) =
    member __.Value = value
    interface System.IDisposable with
        member __.Dispose() = ()

[<Class>]
type TypeWithClassAttribute =
    val Pos : int
    new (pos) = { Pos=pos }

// -------------------------------------------------------------
// Issue #1975: https://github.com/fable-compiler/Fable/issues/1975
// In previous version of Fable, using type with parameterized units of measure was causing an endless loops in the compiler

type TestTypeWithParameterizedUnitMeasure<[<Measure>] 't> =
    private | TestTypeWithParameterizedUnitMeasureType of float<'t>

    member this.Value =
        match this with
        | TestTypeWithParameterizedUnitMeasureType value -> value

let makeTestTypeWithParameterizedUnitMeasureType (value: float<_>) : TestTypeWithParameterizedUnitMeasure<_> =
    TestTypeWithParameterizedUnitMeasureType value

open FSharp.Data.UnitSystems.SI.UnitSymbols

type Test_TestTypeWithParameterizedUnitMeasure = {
    Field: TestTypeWithParameterizedUnitMeasure<m>
}

// -------------------------------------------------------------

// Tested ported from https://github.com/fable-compiler/Fable/pull/1336/files
type TestTypeWithDefaultValue() =
    [<DefaultValue>] val mutable IntValue: int
    [<DefaultValue>] val mutable StringValue: string
    [<DefaultValue>] val mutable ObjValue: System.Collections.Generic.Dictionary<string, string>

type Default1 = int

type Distinct1 =
    // Overloads only distinguished by generic constrain work, see #1908
    static member inline Distinct1 (x: ^``Collection<'T>``, _impl: Default1) = (^``Collection<'T>`` : (static member Distinct1 : _->_) x) : '``Collection<'T>``
    static member inline Distinct1 (_: ^t when ^t : null and ^t : struct, _mthd: Default1) = id //must

    // Overloads only distinguished by struct tuple work, see #2417
    static member OfList(_elements: list<'K * 'V>) = ()
    static member OfList(_elements: list<struct('K * 'V)>) = ()

type InfoA = {
    Foo: string
}

type InfoB = {
    InfoA: InfoA
    Bar: string
}

[<AbstractClass>]
type InfoAClass(info: InfoA) =
    abstract WithInfo: InfoA -> InfoAClass
    member _.Foo = info.Foo
    member this.WithFoo foo =
        this.WithInfo({ info with Foo = foo })

type InfoBClass(info: InfoB) =
    inherit InfoAClass(info.InfoA)
    override this.WithInfo(infoA) =
        InfoBClass({ info with InfoA = infoA }) :> InfoAClass

type FooInterface =
    abstract Foo: string with get, set
    abstract DoSomething: f: (float -> float -> float) * v: float -> float
    abstract Item: int -> char with get, set
    abstract Sum: [<ParamArray>] items: string[] -> string

[<Fable.Core.Mangle>]
type BarInterface =
    abstract Bar: string with get, set
    abstract DoSomething: f: (float -> float -> float) * v: float -> float
    abstract Item: int -> char with get, set
    abstract Item: char -> bool with get
    abstract Sum: [<ParamArray>] items: string[] -> string

[<AbstractClass>]
type FooAbstractClass(x: float) =
    member _.Value = x
    member _.DoSomething(x, y) = x * y
    abstract DoSomething: float -> float

type FooClass(x) =
    inherit FooAbstractClass(5.)
    let mutable x = x
    override this.DoSomething(x) =
        this.DoSomething(x, this.Value)
    static member ChangeChar(s: string, i: int, c: char) =
        s.ToCharArray() |> Array.mapi (fun i2 c2 -> if i = i2 then c else c2) |> String
    interface FooInterface with
        member _.Foo with get() = x and set(y) = x <- y
        member this.DoSomething(f, x) =
            let f = f x
            let x = f 2.
            let y = f 8.
            this.DoSomething(x + y)
        member _.Item with get(i) = x.[i] and set i c = x <- FooClass.ChangeChar(x, i, c)
        member _.Sum(items) = Array.reduce (fun x y -> x + y + x + y) items

[<AbstractClass>]
type BarAbstractClass(x: float) =
    member _.Value = x
    member _.DoSomething(x, y) = x ** y
    abstract DoSomething: float -> float

type BarClass(x) =
    inherit BarAbstractClass(10.)
    let mutable x = x
    override this.DoSomething(x) =
        this.DoSomething(x, this.Value)
    interface BarInterface with
        member _.Bar with get() = x and set(y) = x <- y
        member this.DoSomething(f, x) =
            let f = f x
            let x = f 4.5
            let y = f 7.
            this.DoSomething(x - y)
        member _.Item with get(i) = x.[i] and set i c = x <- FooClass.ChangeChar(x, i + 1, c)
        member _.Item with get(c) = x.ToCharArray() |> Array.exists ((=) c)
        member _.Sum(items) = Array.reduce (fun x y -> x + x + y + y) items

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

let areEqual (x: obj) (y: obj) =
    x = y

type MyUnion1 = Foo of int * int | Bar of float | Baz
type MyUnion2 = Foo of int * int
    with override _.ToString() = "ffff"

type MyRecord1 = { Foo: int; Bar: string }
type MyRecord2 = { Foo: int; Bar: string }

type SubclassTest1() = class end
type SubclassTest2() = inherit SubclassTest1()
type SubclassTest3() = inherit SubclassTest2()

[<Measure>] type a
[<Measure>] type b
[<Measure>] type c = a * b
[<Measure>] type d = a / b

type MeasureTest<[<Measure>] 'T> = { X: float<'T> }
type MeasureTestGen<[<Measure>] 'T, 'V> = { X: float<'T>; Y: 'V }

type MeasureTest1_ = { Y: MeasureTestGen<a, int> }
type MeasureTest1 = { Y: MeasureTest<a*b> }
type MeasureTest2 = { Y: float<c> }
type MeasureTest3 = { Y: MeasureTest<a/b> }
type MeasureTest4 = { Y: float<d> }

// Check that types with product measures compile, see #2532
type MeasureTest5 = { Y: MeasureTest<c> }
type MeasureTest6 = { Y: MeasureTest<d> }

type EnumFoo =
  | Foo = 0
  | Bar = 1

[<AbstractClass>]
type MangledAbstractClass1() =
    class end

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

type IndexedProps(v: int) =
    let mutable v = v
    member _.Item with get (v2: int) = v + v2 and set v2 (s: string) = v <- v2 + int s
    member _.Item with get (v2: float) = float v + v2 / 2.

type TypeWithByRefMember() =
  static member DoubleIntByRef (x: byref<int>) : unit = x <- 2 * x

let inline doubleIntByRef (x: ^a) (input: int) : int =
    let mutable value = input
    (^a: (static member DoubleIntByRef: byref<int> -> unit) &value)
    value

let byrefFunc(n: byref<int>) =
    n <- n + n

let inline callWithByrefCreatedFromByrefInlined(n: byref<int>) =
    let f = &n
    byrefFunc &f

let tests =
  testList "Types" [
    // TODO: This test produces different results in Fable and .NET
    // See Fable.Transforms.FSharp2Fable.TypeHelpers.makeTypeGenArgs
    // testCase "Reflection for types with measures work" <| fun () ->
    //     Reflection.FSharpType.GetRecordFields(typeof<MeasureTest1_>)
    //     |> Array.item 0
    //     |> fun fi -> fi.PropertyType.GetGenericArguments().Length
    //     |> equal 1

    testCase "Indexed properties work" <| fun () ->
        let f = IndexedProps(5)
        f[4] |> equal 9
        f[3] <- "6"
        f[4] |> equal 13
        f[4.] |> equal 11

    testCase "Types can instantiate their parent in the constructor" <| fun () ->
        let t = TestType9()
        t.Greet("Maxime") |> equal "Hello Maxime"

    testCase "Type testing" <| fun () ->
        let x = TestType "test" :> obj
        let y = TestType2 "test" :> obj
        x :? TestType |> equal true
        x :? TestType2 |> equal false
        y :? TestType |> equal false
        y :? TestType2 |> equal true

    testCase "Type testing in pattern matching" <| fun () ->
        let x = TestType "test" :> obj
        match x with
        | :? TestType as x -> x.Value
        | _ -> "FAIL"
        |> equal "test"
        match x with
        | :? TestType2 as x -> x.Value
        | _ -> "FAIL"
        |> equal "FAIL"

    // TODO: Should we make interface testing work in Fable 2?
    // testCase "Children inherits parent interfaces" <| fun () ->
    //     let t4 = TestType4() |> box
    //     t4 :? ITest |> equal true

    // testCase "Interface testing" <| fun () ->
    //     let x = Union1 "test" :> obj
    //     let y = Union2 "test" :> obj
    //     x :? ITest |> equal true
    //     x :? ITest2 |> equal false
    //     y :? ITest |> equal true
    //     y :? ITest2 |> equal false

    // testCase "Interface testing in pattern matching" <| fun () ->
    //     let x = Union2 "test" :> obj
    //     match x with | :? ITest -> true | _ -> false
    //     |> equal true
    //     match x with | :? ITest2 -> true | _ -> false
    //     |> equal false

    testCase "Type testing with JS primitive types works" <| fun () ->
        let test (o: obj) =
            match o with
            | :? string -> "string"
            | :? float -> "number"
            | :? bool -> "boolean"
            | :? unit -> "unit"
            | :? System.Text.RegularExpressions.Regex -> "RegExp"
            | :? (int[]) | :? (string[]) -> "Array"
            | _ -> "unknown"
        "A" :> obj |> test |> equal "string"
        3. :> obj |> test |> equal "number"
        false :> obj |> test |> equal "boolean"
        () :> obj |> test |> equal "unit"
        // Workaround to make sure Fable is passing the argument
        let a = () :> obj in test a |> equal "unit"
        System.Text.RegularExpressions.Regex(".") :> obj |> test |> equal "RegExp"
        [|"A"|] :> obj |> test |> equal "Array"
        [|1;2|] :> obj |> test |> equal "Array"

    testCase "Type test with Date" <| fun () ->
        let isDate (x: obj) =
            match x with
            | :? DateTime -> true
            | _ -> false
        DateTime.Now |> box |> isDate |> equal true
        box 5 |> isDate |> equal false

    testCase "Type test with Long" <| fun () ->
        let isLong (x: obj) =
            match x with
            | :? int64 -> true
            | _ -> false
        box 5L |> isLong |> equal true
        box 50 |> isLong |> equal false

    testCase "Type test with BigInt" <| fun () ->
        let isBigInd (x: obj) =
            match x with
            | :? bigint -> true
            | _ -> false
        box 5I |> isBigInd |> equal true
        box 50 |> isBigInd |> equal false

    testCase "Property names don't clash with built-in JS objects" <| fun () -> // See #168
        let gameState = {
            Now = 1
            Map = "dungeon"
            Players = Map.empty
        }
        gameState.Players.ContainsKey(1) |> equal false

    testCase "Overloads work" <| fun () ->
        let t = TestType5("")
        t.Overload(2) |> equal 4
        t.Overload(2, 3) |> equal 5

    testCase "Type abbreviation works" <| fun () ->
        let t = T4()
        t.Value2 |> equal "Bye"

    testCase "Getter and Setter work" <| fun () ->
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

    testCase "Getter and Setter with indexer work" <| fun () ->
        let t = TestType7(1, 2, 3)
        t.Value(1) |> equal 2
        t.Value(2) |> equal 3
        t.Value(1) <- 5
        t.Value(1) |> equal 5
        t.Value(2) |> equal 3

    testCase "Statically resolved instance calls work" <| fun () ->
        let a = { thing = 5 }
        let b = { label = "five" }
        show a |> equal "5"
        show b |> equal "five"

    testCase "Statically resolved static calls work" <| fun () ->
        let a = { thing = 5 }
        let b = { label = "five" }
        showStatic a |> equal "Static: 5"
        showStatic b |> equal "Static: five"

    testCase "Guid.NewGuid works" <| fun () ->
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

    testCase "Guid.Empty works" <| fun () ->
        let g1 = Guid.Empty
        string g1 |> equal "00000000-0000-0000-0000-000000000000"

    testCase "Guid.ToString works" <| fun () ->
        let g1 = Guid.Parse "dec42487-c02b-42a6-9a10-0263a5a7fdf1"
        string g1 |> equal "dec42487-c02b-42a6-9a10-0263a5a7fdf1"

    testCase "lazy works" <| fun () ->
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

    testCase "Lazy.CreateFromValue works" <| fun () ->
        let mutable snitch = 0
        let lazyVal =
            Lazy<_>.CreateFromValue(
                snitch <- snitch + 1
                5)
        equal 1 snitch
        equal 5 lazyVal.Value
        equal 1 snitch

    testCase "lazy.IsValueCreated works" <| fun () ->
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

    testCase "Lazy constructor works" <| fun () ->
        let items = Lazy<string list>(fun () -> ["a";"b";"c"])
        let search e = items.Value |> List.tryFind (fun m -> m = e)
        search "b" |> equal (Some "b")
        search "d" |> equal None

    testCase "Secondary constructors work" <| fun () ->
        let s1 = SecondaryCons(3)
        let s2 = SecondaryCons()
        equal 3 s1.Value
        equal 5 s2.Value

    // testCase "Inheriting from secondary constructors works" <| fun () ->
    //     let s = SecondaryConsChild()
    //     equal 5 s.Value

    testCase "Multiple constructors work" <| fun () ->
        let m1 = MultipleCons()
        let m2 = MultipleCons(5)
        let m3 = MultipleCons(7,7)
        equal 5 m1.Value
        equal 9 m2.Value
        equal 14 m3.Value

    testCase "Abstract methods with default work" <| fun () -> // See #505
        let x = ConcreteClass()
        x.MethodWithDefault() |> equal "Hello "
        x.MustImplement() |> equal "World!!"
        x.CallMethodWithDefault() |> equal "Hello World!!"
        let x = ConcreteClass2()
        x.CallMethodWithDefault() |> equal "Hi World!!"

    testCase "Abstract properties with getters and setters work" <| fun () ->
        let x = ConcreteClass3() :> AbstractClass3
        x.MyProp <- 2
        equal 7 x.MyProp

    testCase "Interface setters don't conflict" <| fun () -> // See #505
        let x = XISomeInterface () :> ISomeInterface
        x.Sender |> equal 0
        x.Sender <- 5
        x.Sender |> equal 5

    testCase "A type can overload an interface method" <| fun () ->
        let foo = FooImplementor()
        foo.Foo() |> equal "foo"
        (foo :> IFoo).Foo() |> equal "foobar"
        mangleFoo foo |> equal "foobar"

    testCase "A child can be casted to parent's interface" <| fun () ->
        let foo = FooImplementorChild()
        foo.Foo() |> equal "foofoofoo"
        (foo :> IFoo).Foo() |> equal "foofoofoobar"
        mangleFoo foo |> equal "foofoofoobar"

    testCase "A type can overload an interface getter" <| fun () ->
        let foo = FooImplementor()
        foo.Bar |> equal "he"
        (foo :> IFoo).Bar |> equal "heho"

    testCase "A type can overload an interface setter" <| fun () ->
        let foo = FooImplementor()
        foo.MySetter <- 7
        foo.MySetter |> equal 9
        (foo :> IFoo).MySetter <- 7
        (foo :> IFoo).MySetter |> equal 19

    // TODO: Interface and abstract methods with same name clash
    testCase "A type overloading an interface method can be inherited" <| fun () ->
        let foo = ChildFoo() :> AbstractFoo
        foo.Foo2() |> equal "BAR"
        (foo :> IFoo).Foo() |> equal "BARFOO"
        mangleFoo foo |> equal "BARFOO"

    testCase "Interface casting round-trip" <| fun () -> // See #1452
        let d = new DowncastTest(3) :> System.IDisposable
        let t = d :?> DowncastTest
        t.Value |> equal 3
        equal 3 <|
            match d with
            | :? DowncastTest as t2 -> t2.Value
            | _ -> 5

    testCase "Calling default implementation of base members don't cause infinite recursion" <| fun () -> // See #701
        let x = ExtendedClass()
        x.Init() |> equal 7
        (x :> BaseClass).Init() |> equal 7

    testCase "Calling default implementation of base properties don't cause infinite recursion" <| fun () -> // See #701
        let x = ExtendedClass()
        x.Prop |> equal "base-extension"
        (x :> BaseClass).Prop |> equal "base-extension"

    testCase "Calling base members works" <| fun () -> // See #1464
        let bar = ExtendedClass2()
        bar.B() |> equal 1

    testCase "Circular dependencies work" <| fun () -> // See #569
        let location = { name="NY"; employees=[] }
        let alice = { name="Alice"; age=20.0; location=location  }
        location.name |> equal "NY"
        alice.age |> equal 20.

    testCase "Value Type records work" <| fun () -> // See #568
        let foo1 = ValueType<_>("foo")
        let foo2 = ValueType<_>("foo")
        foo1.Value |> equal "foo"
        foo1.value |> equal "foo"
        foo1 = foo2 |> equal true

    testCase "Value Type unions work" <| fun () ->
        let du1 = StructUnion.Value "du"
        let du2 = StructUnion.Value "du"
        du1 = du2 |> equal true

    testCase "Value Type tuples work" <| fun () ->
        let tu1 = struct ("a","b")
        let tu2 = struct ("a","b")
        tu1 = tu2 |> equal true

    testCase "Value Types work" <| fun () ->
        let bar1 = ValueType1("bar")
        let bar2 = ValueType1("bar")
        bar1.Value |> equal "bar"
        bar1 = bar2 |> equal true

    testCase "Other Value Types work" <| fun () ->
        let test2 = ValueType2(3, 4)
        test2.Value |> equal 7
        let p = Point2D(2.)
        p.Y |> equal 2.

    testCase "struct without explicit ctor works" <| fun () ->
        let t1 = ValueType3(X=10)
        t1.X |> equal 10
        let mutable t2 = ValueType3()
        t2.X |> equal 0
        t1 |> notEqual t2
        (compare t1 t2) |> equal 1
        t2.X <- 10
        t1 |> equal t2

        (compare t1 t2) |> equal 0

    testCase "Custom F# exceptions work" <| fun () ->
        try
            MyEx(4,"ERROR") |> raise
        with
        | MyEx(4, msg) as e -> (box e :? Exception, msg + "!!")
        | MyEx(_, msg) as e -> (box e :? Exception, msg + "??")
        | ex -> (false, "unknown")
        |> equal (true, "ERROR!!")

    testCase "Custom exceptions work" <| fun () ->
        try
            MyEx2(5.5) |> raise
        with
        | :? MyEx2 as ex -> (box ex :? Exception, ex.Message, ex.Code)
        | ex -> (false, "unknown", 0.)
        |> equal (true, "Code: 5", 5.5)

    testCase "reraise works" <| fun () ->
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

    testCase "This context is not lost in closures within implicit constructor" <| fun () -> // See #1444
        ThisContextInConstructor(7).Value() |> equal 7

    testCase "Type can be casted to an interface implemented by parent" <| fun () ->
        let c = TestType10Child()
        let f1 = c :> ITest4
        let f2 = c :> ITest3
        f1.Add(4, 5) |> equal 9
        f1.Add2(4, 5) |> equal -1
        f2.Add2(4, 5) |> equal -1

    testCase "ClassAttribute works" <| fun () -> // See #573
        let t1 = TypeWithClassAttribute(8)
        t1.Pos |> equal 8

    testCase "Issue #1975: Compile type with parameterized units of measure as generic" <| fun () ->
        let a = makeTestTypeWithParameterizedUnitMeasureType 2.
        equal 2. a.Value

    // Test ported from https://github.com/fable-compiler/Fable/pull/1336/files
//    testCase "default value attributes works" <| fun _ ->
//        let withDefaultValue = TestTypeWithDefaultValue()
//
//        withDefaultValue.IntValue |> equal Unchecked.defaultof<int>
//        withDefaultValue.IntValue |> equal 0
//
//        withDefaultValue.StringValue |> equal Unchecked.defaultof<string>
//        withDefaultValue.StringValue |> equal null
//
//        withDefaultValue.ObjValue |> equal Unchecked.defaultof<System.Collections.Generic.Dictionary<string, string>>
//        withDefaultValue.ObjValue |> equal null

    testCase "Private fields don't conflict with parent classes" <| fun _ -> // See #2070
        let a1 = InfoBClass({ InfoA = { Foo = "foo" }; Bar = "bar" }) :> InfoAClass
        let a2 = a1.WithFoo("foo2")
        a1.Foo |> equal "foo"
        a2.Foo |> equal "foo2"

    // See #2084
    testCase "Non-mangled interfaces work with object expressions" <| fun _ ->
        let mutable foo = "Foo"
        let foo = { new FooInterface with
                        member _.Foo with get() = foo and set x = foo <- x
                        member _.DoSomething(f, x) = let f = f 1. in f x * f 0.2
                        member _.Item with get(i) = foo.[i] and set i c = foo <- FooClass.ChangeChar(foo, i - 1, c)
                        member _.Sum(items) = Array.reduce (+) items }

        let addPlus2 x y = x + y + 2.
        let multiplyTwice x y = x * y * y

        foo.[3] <- 'W'
        foo.Foo <- foo.Foo + foo.DoSomething(addPlus2, 3.).ToString("F2").Replace(",", ".") + foo.[2].ToString()
        foo.Foo <- foo.Foo + foo.Sum("a", "bc", "d")

        foo.Foo |> equal "FoW19.20Wabcd"

    // See #2084
    testCase "Mangled interfaces work with object expressions" <| fun _ ->
        let mutable bar = "Bar"
        let bar = { new BarInterface with
                        member _.Bar with get() = bar and set x = bar <- x
                        member _.DoSomething(f, x) = let f = f 4.3 in f x + f x
                        member _.Item with get(i) = bar.[i] and set _ c = bar <- FooClass.ChangeChar(bar, 0, c)
                        member _.Item with get(c) = bar.ToCharArray() |> Array.exists ((=) c)
                        member _.Sum(items) = Array.rev items |> Array.reduce (+)  }

        let addPlus2 x y = x + y + 2.
        let multiplyTwice x y = x * y * y

        bar.[3] <- 'Z'
        bar.Bar <- bar.Bar + bar.DoSomething(multiplyTwice, 3.).ToString("F2").Replace(",", ".") + bar.[2].ToString() + (sprintf "%b%b" bar.['B'] bar.['x'])
        bar.Bar <- bar.Bar + bar.Sum("a", "bc", "d")

        bar.Bar |> equal "Zar77.40rfalsefalsedbca"

    // See #2084
    testCase "Non-mangled interfaces work with classes" <| fun _ ->
        let addPlus2 x y = x + y + 2.
        let multiplyTwice x y = x * y * y
        let foo2 = FooClass("Foo") :> FooInterface
        foo2.[0] <- 'W'
        foo2.Foo <- foo2.Foo + foo2.DoSomething(multiplyTwice, 3.).ToString("F2").Replace(',', '.') + foo2.[2].ToString()
        foo2.Foo <- foo2.Foo + foo2.Sum("a", "bc", "d")
        foo2.Foo |> equal "Woo1020.00oabcabcdabcabcd"

    // See #2084
    testCase "Mangled interfaces work with classes" <| fun _ ->
        let addPlus2 x y = x + y + 2.
        let multiplyTwice x y = x * y * y
        let bar2 = BarClass("Bar") :> BarInterface
        bar2.[0] <- 'Z'
        bar2.Bar <- bar2.Bar + bar2.DoSomething(addPlus2, 3.).ToString("F2").Replace(",", ".") + bar2.[2].ToString() + (sprintf "%b%b" bar2.['B'] bar2.['x'])
        bar2.Bar <- bar2.Bar + bar2.Sum("a", "bc", "d")
        bar2.Bar |> equal "BZr9536.74rtruefalseaabcbcaabcbcdd"

    testCase "Multiple `this` references work in nested attached members" <| fun _ ->
        (MixedThese(2) :> Interface1).Create(3).Add() |> equal 5

    testCase "Two unions of different type with same shape are not equal" <| fun () ->
        areEqual (MyUnion1.Foo(1,2)) (MyUnion2.Foo(1,2)) |> equal false
        areEqual (MyUnion1.Foo(1,2)) (MyUnion1.Foo(1,2)) |> equal true

    testCase "Two records of different type with same shape are not equal" <| fun () ->
        areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord2.Foo = 2; Bar = "oh" } |> equal false
        areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord1.Foo = 2; Bar = "oh" } |> equal true

    testCase "IsSubclassOf checks whole hierarchy" <| fun () ->
        typeof<SubclassTest2>.IsSubclassOf(typeof<SubclassTest1>) |> equal true
        typeof<SubclassTest3>.IsSubclassOf(typeof<SubclassTest1>) |> equal true

    testCase "IsSubclassOf returns true for System.Object" <| fun () ->
        typeof<SubclassTest2>.IsSubclassOf(typeof<obj>) |> equal true
        typeof<string>.IsSubclassOf(typeof<obj>) |> equal true
        typeof<int>.IsSubclassOf(typeof<obj>) |> equal true
        typeof<unit -> unit>.IsSubclassOf(typeof<obj>) |> equal true

    testCase "IsInstanceOfType works with class types" <| fun () ->
        let s1, s2 = SubclassTest1(), SubclassTest2()
        typeof<obj>.IsInstanceOfType(s1) |> equal true
        typeof<SubclassTest1>.IsInstanceOfType(s1) |> equal true
        typeof<SubclassTest2>.IsInstanceOfType(s1) |> equal false
        typeof<SubclassTest3>.IsInstanceOfType(s1) |> equal false
        typeof<SubclassTest1>.IsInstanceOfType(s2) |> equal true
        typeof<SubclassTest2>.IsInstanceOfType(s2) |> equal true
        typeof<SubclassTest3>.IsInstanceOfType(s2) |> equal false

    testCase "IsInstanceOfType works with nominal records" <| fun () ->
        typeof<MyRecord1>.IsInstanceOfType({ MyRecord1.Foo = 2; Bar = "oh" }) |> equal true
        typeof<obj>.IsInstanceOfType({ MyRecord1.Foo = 2; Bar = "oh" }) |> equal true
        typeof<MyRecord2>.IsInstanceOfType({ MyRecord1.Foo = 2; Bar = "oh" }) |> equal false

    testCase "IsInstanceOfType works with nominal unions" <| fun () ->
        typeof<MyUnion1>.IsInstanceOfType(MyUnion1.Foo(1,2)) |> equal true
        typeof<obj>.IsInstanceOfType(MyUnion1.Foo(1,2)) |> equal true
        typeof<MyUnion2>.IsInstanceOfType(MyUnion1.Foo(1,2)) |> equal false

    // Expected to always return true for any numeric type, just like :? operator
    testCase "IsInstanceOfType works with enums" <| fun () ->
        typeof<EnumFoo>.IsInstanceOfType(EnumFoo.Foo) |> equal true
        typeof<EnumFoo>.IsInstanceOfType(EnumFoo.Bar) |> equal true
        typeof<obj>.IsInstanceOfType(EnumFoo.Bar) |> equal true

    // Expected to always return true for any function and function type, just like :? operator
    testCase "IsInstanceOfType works with functions" <| fun () ->
        typeof<unit -> unit>.IsInstanceOfType(fun () -> ()) |> equal true
        typeof<obj>.IsInstanceOfType(fun () -> ()) |> equal true
        //typeof<unit -> int>.IsInstanceOfType(fun () -> ()) |> equal false
        typeof<string -> int>.IsInstanceOfType(String.length) |> equal true
        typeof<obj>.IsInstanceOfType(String.length) |> equal true
        //typeof<int -> int>.IsInstanceOfType(String.length) |> equal false

    testCase "IsInstanceOfType works with primitives" <| fun () ->
        typeof<string>.IsInstanceOfType("hello") |> equal true
        typeof<obj>.IsInstanceOfType("hello") |> equal true
        typeof<string>.IsInstanceOfType(5) |> equal false
        typeof<int>.IsInstanceOfType(5) |> equal true
        typeof<obj>.IsInstanceOfType(5) |> equal true
        typeof<int>.IsInstanceOfType("hello") |> equal false

#if FABLE_COMPILER
    testCase "Choice with arity 3+ is represented correctly" <| fun () -> // See #2485
        Choice2Of3 55 |> Fable.Core.Reflection.getCaseName |> equal "Choice2Of3"
        Choice3Of3 55 |> Fable.Core.Reflection.getCaseName |> equal "Choice3Of3"
#endif

    testCase "Can call the base version of a mangled abstract method that was declared above in the hierarchy" <| fun () ->
        let c = ConcreteClass1()
        c.MyMethod(4) |> equal 58

    // See #3328
    testCase "SRTP works with byref" <| fun () ->
      let result = doubleIntByRef (TypeWithByRefMember()) 7
      result |> equal 14

    // See #3328
    testCase "inline byref works" <| fun () ->
        let mutable an_int = 22
        callWithByrefCreatedFromByrefInlined &an_int
        an_int |> equal 44

    // See #3328
    testCase "inline byref works (with separate binding for reference)" <| fun () ->
        let mutable an_int = 33
        let intRef = &an_int
        ignore intRef
        callWithByrefCreatedFromByrefInlined &intRef
        an_int |> equal 66
  ]
