module Fable.Tests.TypeTests

#nowarn "3535"

// open System.Runtime.InteropServices
open Fable.Core
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
    member _.Value = s
    interface ITest

type TestType2(s: string) =
    member _.Value = s
    interface ITest

type TestType3() =
    member _.Value = "Hi"
    interface ITest

type TestType4() =
    inherit TestType3()
    member _.Value2 = "Bye"
    interface ITest2

type TestType5(greeting: string) =
    member _.Value = greeting
    member _.Overload(x) = x + x
    member _.Overload(x, y) = x + y

// type TestType8(?greeting) =
//     member _.Value = defaultArg greeting "Hi"

// type TestType9() =
//     inherit TestType8()
//     let foo = TestType8("Hello")
//     member _.Greet(name) = foo.Value + " " + name

// type TestType10Base() =
//     interface ITest4 with
//         member _.Add2(x, y) = x - y
//         member _.Add(x, y) = x + y

// type TestType10Child() =
//     inherit TestType10Base()

type RenderState =
    { Now : int
      Players : Map<int, string>
      Map : string }

type T4 = TestType4

type TestType6(x: int) =
    let mutable i = x
    member val Value1 = i with get, set
    member _.Value2 = i + i
    member _.Value3 with get() = i * i and set(v) = i <- v

type TestType7(a1, a2, a3) =
    let arr = [|a1; a2; a3|]
    member _.Value with get(i) = arr[i] and set(i) (v) = arr[i] <- v

[<Fable.Core.AttachMembers>]
type TestTypeAttached(a1, a2, a3) =
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

type ITestProps =
    abstract Value1: float with get, set
    abstract Value: int -> float with get, set
    abstract Item: int -> float with get, set

type TestProps(arr: float[]) =
    interface ITestProps with
        member _.Value1
            with get () = arr.[1]
            and set (v) = arr.[1] <- v
        member _.Value
            with get (i) = arr.[i]
            and set (i) (v) = arr.[i] <- v
        member _.Item
            with get (i) = arr.[i]
            and set (i) (v) = arr.[i] <- v

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
    member _.Value = x

// // type SecondaryConsChild() =
// //     inherit SecondaryCons()

type MultipleCons(x: int, y: int) =
    new () = MultipleCons(2,3)
    new (x:int) = MultipleCons(x,4)
    member _.Value = x + y

// [<AbstractClass>]
// type AbstractClassWithDefaults () =
//     abstract MethodWithDefault : unit -> string
//     default x.MethodWithDefault () = "Hello "

//     abstract MustImplement: unit -> string

//     member x.CallMethodWithDefault () =
//         x.MethodWithDefault() + x.MustImplement()

// type ConcreteClass () =
//     inherit AbstractClassWithDefaults()
//     override x.MustImplement () = "World!!"

// type ConcreteClass2 () =
//     inherit AbstractClassWithDefaults()
//     override x.MethodWithDefault () = "Hi "
//     override x.MustImplement () = "World!!"

// [<AbstractClass>]
// type AbstractClass3() =
//     abstract MyProp: int with get, set

// type ConcreteClass3() =
//     inherit AbstractClass3()
//     let mutable v = 5
//     override _.MyProp with get() = v and set(v2) = v <- v + v2

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

// type FooImplementor(i: int) =
//     let mutable mut1 = 0
//     let mutable mut2 = 5
//     new () = FooImplementor(1)

//     member x.Foo() = String.replicate i "foo"
//     member x.Bar = "he"
//     member x.MySetter with get() = mut1 and set(v) = mut1 <- v + 2

//     interface IFoo with
//         member x.Foo() = x.Foo() + "bar"
//         member x.Bar = x.Bar + "ho"
//         member x.MySetter with get() = mut1 + mut2 and set(v) = mut2 <- v + 3

// type FooImplementorChild() =
//     inherit FooImplementor(3)

// [<AbstractClass>]
// type AbstractFoo() =
//     abstract member Foo2: unit -> string
//     interface IFoo with
//         member this.Foo() = this.Foo2() + "FOO"
//         member x.Bar = ""
//         member x.MySetter with get() = 0 and set(v) = ()

// type ChildFoo() =
//     inherit AbstractFoo()
//     override this.Foo2() = "BAR"

// type BaseClass (x: int) =
//     abstract member Init: unit -> int
//     default _.Init () = x
//     abstract member Prop: string
//     default _.Prop = "base"

// type ExtendedClass () =
//     inherit BaseClass(5)
//     override _.Init() = base.Init() + 2
//     override _.Prop = base.Prop + "-extension"

// type BaseClass2() =
//     let field = 1
//     member _.A() = field

// type ExtendedClass2() =
//     inherit BaseClass2()
//     member _.A() = 2
//     member _.B() = base.A()

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

[<Struct>]
type SimpleRecord = { A: string; B: string }

type Point2D =
   struct
      val X: float
      val Y: float
      new(xy: float) = { X = xy; Y = xy }
   end

// exception MyEx of int*string

// type MyEx2(f: float) =
//   inherit exn(sprintf "Code: %i" (int f))
//   member _.Code = f

// type ThisContextInConstructor(v) =
//     let f () = v
//     member val Value = f

type DowncastTest(value: int) =
    member _.Value = value
    interface System.IDisposable with
        member _.Dispose() = ()

[<Class>]
type TypeWithClassAttribute =
    val Pos : int
    new (pos) = { Pos=pos }

// // -------------------------------------------------------------
// // Issue #1975: https://github.com/fable-compiler/Fable/issues/1975
// // In previous version of Fable, using type with parameterized units of measure was causing an endless loops in the compiler

// type TestTypeWithParameterizedUnitMeasure<[<Measure>] 't> =
//     private | TestTypeWithParameterizedUnitMeasureType of float<'t>

//     member this.Value =
//         match this with
//         | TestTypeWithParameterizedUnitMeasureType value -> value

// let makeTestTypeWithParameterizedUnitMeasureType (value: float<_>) : TestTypeWithParameterizedUnitMeasure<_> =
//     TestTypeWithParameterizedUnitMeasureType value

// open FSharp.Data.UnitSystems.SI.UnitSymbols

// type Test_TestTypeWithParameterizedUnitMeasure = {
//     Field: TestTypeWithParameterizedUnitMeasure<m>
// }

// // -------------------------------------------------------------

// // Tested ported from https://github.com/fable-compiler/Fable/pull/1336/files
// type TestTypeWithDefaultValue() =
//     [<DefaultValue>] val mutable IntValue: int
//     [<DefaultValue>] val mutable StringValue: string
//     [<DefaultValue>] val mutable ObjValue: System.Collections.Generic.Dictionary<string, string>

// type Default1 = int

// type Distinct1 =
//     // Overloads only distinguished by generic constrain work, see #1908
//     static member inline Distinct1 (x: ^``Collection<'T>``, _impl: Default1) = (^``Collection<'T>`` : (static member Distinct1 : _->_) x) : '``Collection<'T>``
//     static member inline Distinct1 (_: ^t when ^t : null and ^t : struct, _mthd: Default1) = id //must

//     // Overloads only distinguished by struct tuple work, see #2417
//     static member OfList(_elements: list<'K * 'V>) = ()
//     static member OfList(_elements: list<struct('K * 'V)>) = ()

// type InfoA = {
//     Foo: string
// }

// type InfoB = {
//     InfoA: InfoA
//     Bar: string
// }

// [<AbstractClass>]
// type InfoAClass(info: InfoA) =
//     abstract WithInfo: InfoA -> InfoAClass
//     member _.Foo = info.Foo
//     member this.WithFoo foo =
//         this.WithInfo({ info with Foo = foo })

// type InfoBClass(info: InfoB) =
//     inherit InfoAClass(info.InfoA)
//     override this.WithInfo(infoA) =
//         InfoBClass({ info with InfoA = infoA }) :> InfoAClass

// type FooInterface =
//     abstract Foo: string with get, set
//     abstract DoSomething: f: (float -> float -> float) * v: float -> float
//     abstract Item: int -> char with get, set
//     abstract Sum: [<ParamArray>] items: string[] -> string

// [<Fable.Core.Mangle>]
// type BarInterface =
//     abstract Bar: string with get, set
//     abstract DoSomething: f: (float -> float -> float) * v: float -> float
//     abstract Item: int -> char with get, set
//     abstract Item: char -> bool with get
//     abstract Sum: [<ParamArray>] items: string[] -> string

// [<AbstractClass>]
// type FooAbstractClass(x: float) =
//     member _.Value = x
//     member _.DoSomething(x, y) = x * y
//     abstract DoSomething: float -> float

// type FooClass(x) =
//     inherit FooAbstractClass(5.)
//     let mutable x = x
//     override this.DoSomething(x) =
//         this.DoSomething(x, this.Value)
//     static member ChangeChar(s: string, i: int, c: char) =
//         s.ToCharArray() |> Array.mapi (fun i2 c2 -> if i = i2 then c else c2) |> String
//     interface FooInterface with
//         member _.Foo with get() = x and set(y) = x <- y
//         member this.DoSomething(f, x) =
//             let f = f x
//             let x = f 2.
//             let y = f 8.
//             this.DoSomething(x + y)
//         member _.Item with get(i) = x[i] and set i c = x <- FooClass.ChangeChar(x, i, c)
//         member _.Sum(items) = Array.reduce (fun x y -> x + y + x + y) items

// [<AbstractClass>]
// type BarAbstractClass(x: float) =
//     member _.Value = x
//     member _.DoSomething(x, y) = x ** y
//     abstract DoSomething: float -> float

// type BarClass(x) =
//     inherit BarAbstractClass(10.)
//     let mutable x = x
//     override this.DoSomething(x) =
//         this.DoSomething(x, this.Value)
//     interface BarInterface with
//         member _.Bar with get() = x and set(y) = x <- y
//         member this.DoSomething(f, x) =
//             let f = f x
//             let x = f 4.5
//             let y = f 7.
//             this.DoSomething(x - y)
//         member _.Item with get(i) = x[i] and set i c = x <- FooClass.ChangeChar(x, i + 1, c)
//         member _.Item with get(c) = x.ToCharArray() |> Array.exists ((=) c)
//         member _.Sum(items) = Array.reduce (fun x y -> x + x + y + y) items

// type Interface2 =
//     abstract Value: int
//     abstract Add: unit -> int

// type Interface1 =
//     abstract Create: int -> Interface2

// type MixedThese(x: int) =
//     member _.Value = x
//     interface Interface1 with
//         member this1.Create(y: int) =
//             { new Interface2 with
//                 member _.Value = y
//                 member this2.Add() = this1.Value + this2.Value }

// let areEqual (x: obj) (y: obj) = x = y
let inline areEqual x y = x = y //TODO: non-inline

type MyUnion1 = Foo of int * int | Bar of float | Baz
type MyUnion2 = Foo of int * int
    with override _.ToString() = "ffff"

type MyRecord1 = { Foo: int; Bar: string }
type MyRecord2 = { Foo: int; Bar: string }

// type SubclassTest1() = class end
// type SubclassTest2() = inherit SubclassTest1()
// type SubclassTest3() = inherit SubclassTest2()

// [<Measure>] type a
// [<Measure>] type b
// [<Measure>] type c = a * b
// [<Measure>] type d = a / b

// type MeasureTest<[<Measure>] 'T> = { X: float<'T> }
// type MeasureTestGen<[<Measure>] 'T, 'V> = { X: float<'T>; Y: 'V }

// type MeasureTest1_ = { Y: MeasureTestGen<a, int> }
// type MeasureTest1 = { Y: MeasureTest<a*b> }
// type MeasureTest2 = { Y: float<c> }
// type MeasureTest3 = { Y: MeasureTest<a/b> }
// type MeasureTest4 = { Y: float<d> }

// // Check that types with product measures compile, see #2532
// type MeasureTest5 = { Y: MeasureTest<c> }
// type MeasureTest6 = { Y: MeasureTest<d> }

// type EnumFoo =
//   | Foo = 0
//   | Bar = 1

// [<AbstractClass>]
// type MangledAbstractClass1() =
//     class end

// [<AbstractClass>]
// type MangledAbstractClass2(v: int) =
//     inherit MangledAbstractClass1()
//     abstract MyMethod: int -> int
//     default _.MyMethod(x: int) = x * v

// [<AbstractClass>]
// type MangledAbstractClass3(v) =
//     inherit MangledAbstractClass2(v + 3)

// [<AbstractClass>]
// type MangledAbstractClass4(v) =
//     inherit MangledAbstractClass3(v + 4)
//     override _.MyMethod(x: int) = base.MyMethod(x) - v

// [<AbstractClass>]
// type MangledAbstractClass5(v) =
//     inherit MangledAbstractClass4(v + 5)
//     override _.MyMethod(x: int) = base.MyMethod(x) + v + 7

// type ConcreteClass1() =
//     inherit MangledAbstractClass5(2)

type IndexedProps(v: int) =
    let mutable v = v
    member _.Item with get (v2: int) = v + v2 and set v2 (s: string) = v <- v2 + int s
    member _.Item with get (v2: float) = float v + v2 / 2.

[<Interface>]
type ITesting =
    static member Testing x = x

type IOne =
    static abstract member GetSum: int * int -> int
    static abstract member GetOne: unit -> int
    static abstract member Two: int

type I32() =
    interface IOne with
        static member GetSum(x, y) = x + y
        static member GetOne() = 1
        static member Two = 2

let getSum<'T when 'T :> IOne>() = 'T.GetSum(1, 2)
let getOne<'T when 'T :> IOne>() = 'T.GetOne()
let getTwo<'T when 'T :> IOne>() = 'T.Two

[<AttachMembersAttribute>]
type MyOptionalClass(?arg1: float, ?arg2: string, ?arg3: int) =
    member val P1 = defaultArg arg1 1.0
    member val P2 = defaultArg arg2 "1"
    member val P3 = defaultArg arg3 1

// type VeryOptionalInterface =
//     abstract Bar: int option
//     abstract Baz: bool option
//     abstract Bax: float option
//     abstract Wrapped: unit option
//     abstract Foo: string option with get, set
//     abstract Fn: (int -> int -> int) option
//     abstract Fn2: (int -> int -> int)

// type VeryOptionalClass () =
//     let mutable foo = "ab"
//     interface VeryOptionalInterface with
//         member _.Bar = Some 3
//         member _.Baz = None
//         member _.Wrapped = Some ()
//         member _.Bax =
//             let mutable h = ["6"] |> List.tryHead
//             h |> Option.map float
//         member _.Foo
//             with get() = Some foo
//             and set(v) = foo <- match v with Some v -> foo + v | None -> foo
//         member _.Fn = Some (+)
//         member _.Fn2 = (*)

type StaticClass =
//     [<NamedParams>]
//     static member NamedParams(foo: string, ?bar: int) = int foo + (defaultArg bar -3)
//     [<NamedParams>]
//     static member NamedParams(?name : string, ?age: int) =
//         let name = defaultArg name "John"
//         let age = defaultArg age 30
//         $"%s{name} is %d{age} years old"
//     static member FSharpOptionalParam(?value: bool) = defaultArg value true
//     static member FSharpOptionalParam2(?value: unit) = Option.isSome value
//     static member DefaultParam([<Optional; DefaultParameterValue(true)>] value: bool) = value
//     static member DefaultParam2([<Optional>] value: Nullable<int>) = if value.HasValue then value.Value + 2 else 3
//     static member DefaultNullParam([<Optional; DefaultParameterValue(null:obj)>] x: obj) = x
    static member inline InlineAdd(x: int, ?y: int) = x + (defaultArg y 2)

[<Fact>]
let ``Optional arguments work`` () =
    let x = MyOptionalClass(?arg2 = Some "2")
    (x.P1, x.P2, x.P3) |> equal (1.0, "2", 1)
    let y = MyOptionalClass(2.0)
    (y.P1, y.P2, y.P3) |> equal (2.0, "1", 1)
    let z = MyOptionalClass(?arg3 = Some 2)
    (z.P1, z.P2, z.P3) |> equal (1.0, "1", 2)

// [<Fact>]
// let ``Can implement interface optional properties`` () =
//     let veryOptionalValue = VeryOptionalClass() :> VeryOptionalInterface
//     veryOptionalValue.Bar |> equal (Some 3)
//     veryOptionalValue.Baz |> Option.isSome |> equal false
//     veryOptionalValue.Wrapped |> Option.isSome |> equal true
//     veryOptionalValue.Bax |> equal (Some 6.)
//     veryOptionalValue.Foo <- Some "z"
//     veryOptionalValue.Foo |> equal (Some "abz")
//     veryOptionalValue.Foo <- None
//     veryOptionalValue.Foo |> equal (Some "abz")
//     let fn = veryOptionalValue.Fn
//     let fn2 = veryOptionalValue.Fn2
//     let f1 = fn |> Option.map (fun f -> f 6 9) |> Option.defaultValue -3
//     let f2 = match fn with Some f -> f 1 8 | None -> -5
//     f1 + f2 - fn2 9 3 |> equal -3

// [<Fact>]
// let ``Can implement interface optional properties with object expression`` () =
//     let veryOptionalValue =
//         let mutable foo = "ab"
//         { new VeryOptionalInterface with
//             member _.Bar = Some 3
//             member _.Baz = None
//             member _.Wrapped = Some ()
//             member _.Bax = ["6"] |> List.tryHead |> Option.map float
//             member _.Foo
//                 with get() = Some foo
//                 and set(v) = foo <- match v with Some v -> foo + v | None -> foo
//             member _.Fn = Some (+)
//             member _.Fn2 = (*)
//         }

//     veryOptionalValue.Bar |> equal (Some 3)
//     veryOptionalValue.Baz |> Option.isSome |> equal false
//     veryOptionalValue.Wrapped |> Option.isSome |> equal true
//     veryOptionalValue.Bax |> equal (Some 6.)
//     veryOptionalValue.Foo <- Some "z"
//     veryOptionalValue.Foo |> equal (Some "abz")
//     veryOptionalValue.Foo <- None
//     veryOptionalValue.Foo |> equal (Some "abz")
//     let fn = veryOptionalValue.Fn
//     let fn2 = veryOptionalValue.Fn2
//     let f1 = fn |> Option.map (fun f -> f 6 9) |> Option.defaultValue -3
//     let f2 = match fn with Some f -> f 1 8 | None -> -5
//     f1 + f2 - fn2 9 3 |> equal -3

// [<Fact>]
// let ``Optional named params work`` () =
//     StaticClass.NamedParams(foo="5", bar=4) |> equal 9
//     StaticClass.NamedParams(foo="3", ?bar=Some 4) |> equal 7
//     StaticClass.NamedParams(foo="14") |> equal 11
//     StaticClass.NamedParams() |> equal "John is 30 years old"

// [<Fact>]
// let ``F# optional param works`` () =
//     let mutable f1 = fun (v: bool) ->
//         StaticClass.FSharpOptionalParam(value=v)
//     let mutable f2 = fun (v: bool option) ->
//         StaticClass.FSharpOptionalParam(?value=v)
//     StaticClass.FSharpOptionalParam() |> equal true
//     StaticClass.FSharpOptionalParam(true) |> equal true
//     StaticClass.FSharpOptionalParam(false) |> equal false
//     StaticClass.FSharpOptionalParam(?value=None) |> equal true
//     StaticClass.FSharpOptionalParam(?value=Some true) |> equal true
//     StaticClass.FSharpOptionalParam(?value=Some false) |> equal false
//     f1 true |> equal true
//     f1 false |> equal false
//     None |> f2 |> equal true
//     Some false |> f2 |> equal false
//     Some true |> f2 |> equal true

// [<Fact>]
// let ``F# optional param works with no-wrappable options`` () =
//     let mutable f1 = fun (v: unit) ->
//         StaticClass.FSharpOptionalParam2(value=v)
//     let mutable f2 = fun (v: unit option) ->
//         StaticClass.FSharpOptionalParam2(?value=v)
//     StaticClass.FSharpOptionalParam2() |> equal false
//     StaticClass.FSharpOptionalParam2(()) |> equal true
//     StaticClass.FSharpOptionalParam2(?value=None) |> equal false
//     StaticClass.FSharpOptionalParam2(?value=Some ()) |> equal true
//     f1 () |> equal true
//     None |> f2 |> equal false
//     Some () |> f2 |> equal true

// [<Fact>]
// let ``DefaultParameterValue works`` () =
//     StaticClass.DefaultParam() |> equal true
//     StaticClass.DefaultParam(true) |> equal true
//     StaticClass.DefaultParam(false) |> equal false

//     StaticClass.DefaultParam2(5) |> equal 7
//     StaticClass.DefaultParam2(Nullable()) |> equal 3
//     StaticClass.DefaultParam2() |> equal 3

// [<Fact>]
// let ``DefaultParameterValue works with null`` () = // See #3326
//     StaticClass.DefaultNullParam() |> isNull |> equal true
//     StaticClass.DefaultNullParam(5) |> isNull |> equal false

[<Fact>]
let ``Inlined methods can have optional arguments`` () =
    StaticClass.InlineAdd(2, 3) |> equal 5
    StaticClass.InlineAdd(5) |> equal 7

// // TODO: This test produces different results in Fable and .NET
// // See Fable.Transforms.FSharp2Fable.TypeHelpers.makeTypeGenArgs
// // [<Fact>]
// // let ``Reflection for types with measures work`` () =
// //     Reflection.FSharpType.GetRecordFields(typeof<MeasureTest1_>)
// //     |> Array.item 0
// //     |> fun fi -> fi.PropertyType.GetGenericArguments().Length
// //     |> equal 1

[<Fact>]
let ``Indexed properties work`` () =
    let f = IndexedProps(5)
    f[4] |> equal 9
    f[3] <- "6"
    f[4] |> equal 13
    f[4.] |> equal 11

[<Fact>]
let ``Static interface members work`` () =
    let a = ITesting.Testing 5
    a |> equal 5

[<Fact>]
let ``Static interface calls work`` () =
    getOne<I32>() |> equal 1
    getTwo<I32>() |> equal 2
    getSum<I32>() |> equal 3

// [<Fact>]
// let ``Types can instantiate their parent in the constructor`` () =
//     let t = TestType9()
//     t.Greet("Maxime") |> equal "Hello Maxime"

[<Fact>]
let ``Type testing`` () =
    let x = TestType "test" :> obj
    let y = TestType2 "test" :> obj
    x :? TestType |> equal true
    x :? TestType2 |> equal false
    y :? TestType |> equal false
    y :? TestType2 |> equal true

[<Fact>]
let ``Type testing in pattern matching`` () =
    let o = TestType "test" :> obj
    match o with
    | :? TestType as x -> x.Value
    | _ -> "FAIL"
    |> equal "test"
    match o with
    | :? TestType2 as x -> x.Value
    | _ -> "FAIL"
    |> equal "FAIL"

// // // TODO: Should we make interface testing work in Fable 2?
// // [<Fact>]
// // let ``Children inherits parent interfaces`` () =
// //     let t4 = TestType4() |> box
// //     t4 :? ITest |> equal true

// // [<Fact>]
// // let ``Interface testing`` () =
// //     let x = Union1 "test" :> obj
// //     let y = Union2 "test" :> obj
// //     x :? ITest |> equal true
// //     x :? ITest2 |> equal false
// //     y :? ITest |> equal true
// //     y :? ITest2 |> equal false

// // [<Fact>]
// // let ``Interface testing in pattern matching`` () =
// //     let x = Union2 "test" :> obj
// //     match x with | :? ITest -> true | _ -> false
// //     |> equal true
// //     match x with | :? ITest2 -> true | _ -> false
// //     |> equal false

[<Fact>]
let ``Type testing with primitive types works`` () =
    let test (o: obj) =
        match o with
        | :? string -> "string"
        | :? int64 -> "int64"
        | :? float -> "float"
        | :? bool -> "boolean"
        | :? unit -> "unit"
        | :? (int[]) -> "IntArray"
        | :? (string[]) -> "StrArray"
        | _ -> "unknown"
    "A" :> obj |> test |> equal "string"
    3L :> obj |> test |> equal "int64"
    3. :> obj |> test |> equal "float"
    false :> obj |> test |> equal "boolean"
    // () :> obj |> test |> equal "unit" //TODO: test for unit type
    // Workaround to make sure Fable is passing the argument
    // let a = () :> obj in test a |> equal "unit"
    [|"A"|] :> obj |> test |> equal "StrArray"
    [|1;2|] :> obj |> test |> equal "IntArray"

// [<Fact>]
// let ``Type testing with Regex`` () =
//     let test (o: obj) =
//         match o with
//         | :? System.Text.RegularExpressions.Regex -> "RegExp"
//         | _ -> "unknown"
//     System.Text.RegularExpressions.Regex(".") :> obj |> test |> equal "RegExp"

// [<Fact>]
// let ``Type test with Date`` () =
//     let isDate (x: obj) =
//         match x with
//         | :? DateTime -> true
//         | _ -> false
//     DateTime.Now |> box |> isDate |> equal true
//     box 5 |> isDate |> equal false

[<Fact>]
let ``Type test with Long`` () =
    let isLong (x: obj) =
        match x with
        | :? int64 -> true
        | _ -> false
    box 5L |> isLong |> equal true
    box 50 |> isLong |> equal false

// [<Fact>]
// let ``Type test with BigInt`` () =
//     let isBigInd (x: obj) =
//         match x with
//         | :? bigint -> true
//         | _ -> false
//     box 5I |> isBigInd |> equal true
//     box 50 |> isBigInd |> equal false

[<Fact>]
let ``Property names don't clash with built-in JS objects`` () = // See #168
    let gameState = {
        Now = 1
        Map = "dungeon"
        Players = Map.empty
    }
    gameState.Players.ContainsKey(1) |> equal false

[<Fact>]
let ``Overloads work`` () =
    let t = TestType5("")
    t.Overload(2) |> equal 4
    t.Overload(2, 3) |> equal 5

[<Fact>]
let ``Type abbreviation works`` () =
    let t = T4()
    t.Value2 |> equal "Bye"

[<Fact>]
let ``Getter and Setter work`` () =
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

[<Fact>]
let ``Getter and Setter with indexer work`` () =
    let t = TestType7(1, 2, 3)
    t.Value(1) |> equal 2
    t.Value(2) |> equal 3
    t.Value(1) <- 5
    t.Value(1) |> equal 5
    t.Value(2) |> equal 3

[<Fact>]
let ``Attached Getters Setters and Indexers work`` () =
    let t = TestTypeAttached(1, 2, 3)
    t.Value1 |> equal 2
    t.Value1 <- 22
    t.Value1 |> equal 22
    t.Value(0) |> equal 1
    t.Value(0) <- 11
    t.Value(0) |> equal 11
    t[2] |> equal 3
    t[2] <- 33
    t[2] |> equal 33

[<Fact>]
let ``Interface Getters Setters and Indexers work`` () =
    let t = TestProps([| 1; 2; 3 |]) :> ITestProps
    t.Value1 |> equal 2
    t.Value1 <- 22
    t.Value1 |> equal 22
    t.Value(0) |> equal 1
    t.Value(0) <- 11
    t.Value(0) |> equal 11
    t[2] |> equal 3
    t[2] <- 33
    t[2] |> equal 33

[<Fact>]
let ``Statically resolved instance calls work`` () =
    let a = { thing = 5 }
    let b = { label = "five" }
    show a |> equal "5"
    show b |> equal "five"

[<Fact>]
let ``Statically resolved static calls work`` () =
    let a = { thing = 5 }
    let b = { label = "five" }
    showStatic a |> equal "Static: 5"
    showStatic b |> equal "Static: five"

[<Fact>]
let ``lazy works`` () =
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

[<Fact>]
let ``Lazy.CreateFromValue works`` () =
    let mutable snitch = 0
    let lazyVal =
        Lazy<_>.CreateFromValue(
            snitch <- snitch + 1
            5)
    equal 1 snitch
    equal 5 lazyVal.Value
    equal 1 snitch

[<Fact>]
let ``lazy.IsValueCreated works`` () =
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

[<Fact>]
let ``Lazy constructor works`` () =
    let items = Lazy<string list>(fun () -> ["a";"b";"c"])
    let search e = items.Value |> List.tryFind (fun m -> m = e)
    search "b" |> equal (Some "b")
    search "d" |> equal None

[<Fact>]
let ``Secondary constructors work`` () =
    let s1 = SecondaryCons(3)
    let s2 = SecondaryCons()
    equal 3 s1.Value
    equal 5 s2.Value

// [<Fact>]
// let ``Inheriting from secondary constructors works`` () =
//     let s = SecondaryConsChild()
//     equal 5 s.Value

[<Fact>]
let ``Multiple constructors work`` () =
    let m1 = MultipleCons()
    let m2 = MultipleCons(5)
    let m3 = MultipleCons(7,7)
    equal 5 m1.Value
    equal 9 m2.Value
    equal 14 m3.Value

// [<Fact>]
// let ``Abstract methods with default work`` () = // See #505
//     let x = ConcreteClass()
//     x.MethodWithDefault() |> equal "Hello "
//     x.MustImplement() |> equal "World!!"
//     x.CallMethodWithDefault() |> equal "Hello World!!"
//     let x = ConcreteClass2()
//     x.CallMethodWithDefault() |> equal "Hi World!!"

// [<Fact>]
// let ``Abstract properties with getters and setters work`` () =
//     let x = ConcreteClass3() :> AbstractClass3
//     x.MyProp <- 2
//     equal 7 x.MyProp

[<Fact>]
let ``Interface setters don't conflict`` () = // See #505
    let x = XISomeInterface () :> ISomeInterface
    x.Sender |> equal 0
    x.Sender <- 5
    x.Sender |> equal 5

// [<Fact>]
// let ``A type can overload an interface method`` () =
//     let foo = FooImplementor()
//     foo.Foo() |> equal "foo"
//     (foo :> IFoo).Foo() |> equal "foobar"
//     mangleFoo foo |> equal "foobar"

// [<Fact>]
// let ``A child can be casted to parent's interface`` () =
//     let foo = FooImplementorChild()
//     foo.Foo() |> equal "foofoofoo"
//     (foo :> IFoo).Foo() |> equal "foofoofoobar"
//     mangleFoo foo |> equal "foofoofoobar"

// [<Fact>]
// let ``A type can overload an interface getter`` () =
//     let foo = FooImplementor()
//     foo.Bar |> equal "he"
//     (foo :> IFoo).Bar |> equal "heho"

// [<Fact>]
// let ``A type can overload an interface setter`` () =
//     let foo = FooImplementor()
//     foo.MySetter <- 7
//     foo.MySetter |> equal 9
//     (foo :> IFoo).MySetter <- 7
//     (foo :> IFoo).MySetter |> equal 19

// // TODO: Interface and abstract methods with same name clash
// [<Fact>]
// let ``A type overloading an interface method can be inherited`` () =
//     let foo = ChildFoo() :> AbstractFoo
//     foo.Foo2() |> equal "BAR"
//     (foo :> IFoo).Foo() |> equal "BARFOO"
//     mangleFoo foo |> equal "BARFOO"

// [<Fact>]
// let ``Interface casting round-trip`` () = // See #1452
//     let d = new DowncastTest(3) :> System.IDisposable
//     let t = d :?> DowncastTest
//     t.Value |> equal 3
//     equal 3 <|
//         match d with
//         | :? DowncastTest as t2 -> t2.Value
//         | _ -> 5

// [<Fact>]
// let ``Calling default implementation of base members don't cause infinite recursion`` () = // See #701
//     let x = ExtendedClass()
//     x.Init() |> equal 7
//     (x :> BaseClass).Init() |> equal 7

// [<Fact>]
// let ``Calling default implementation of base properties don't cause infinite recursion`` () = // See #701
//     let x = ExtendedClass()
//     x.Prop |> equal "base-extension"
//     (x :> BaseClass).Prop |> equal "base-extension"

// [<Fact>]
// let ``Calling base members works`` () = // See #1464
//     let bar = ExtendedClass2()
//     bar.B() |> equal 1

[<Fact>]
let ``Circular dependencies work`` () = // See #569
    let location = { name="NY"; employees=[] }
    let alice = { name="Alice"; age=20.0; location=location  }
    location.name |> equal "NY"
    alice.age |> equal 20.

[<Fact>]
let ``Value Type records work`` () = // See #568
    let foo1 = ValueType<_>("foo")
    let foo2 = ValueType<_>("foo")
    foo1.Value |> equal "foo"
    foo1.value |> equal "foo"
    foo1 = foo2 |> equal true

[<Fact>]
let ``Value Type unions work`` () =
    let du1 = StructUnion.Value "du"
    let du2 = StructUnion.Value "du"
    du1 = du2 |> equal true

[<Fact>]
let ``Value Type tuples work`` () =
    let tu1 = struct ("a","b")
    let tu2 = struct ("a","b")
    tu1 = tu2 |> equal true

[<Fact>]
let ``Value Types work`` () =
    let bar1 = ValueType1("bar")
    let bar2 = ValueType1("bar")
    bar1.Value |> equal "bar"
    bar1 = bar2 |> equal true

[<Fact>]
let ``Other Value Types work`` () =
    let test2 = ValueType2(3, 4)
    test2.Value |> equal 7
    let p = Point2D(2.)
    p.Y |> equal 2.

[<Fact>]
let ``struct without explicit ctor works`` () =
    let t1 = ValueType3(X=10)
    t1.X |> equal 10
    let mutable t2 = ValueType3()
    t2.X |> equal 0
    t1 |> notEqual t2
    (compare t1 t2) |> equal 1
    t2.X <- 10
    t1 |> equal t2
    (compare t1 t2) |> equal 0

[<Fact>]
let ``copying struct records works`` () = // See #3371
    let simple : SimpleRecord = { A = ""; B = "B" }
    let simpleRecord = { simple with A = "A" }
    simpleRecord.A |> equal "A"
    simpleRecord.B |> equal "B"
    simple.A |> equal ""
    simple.B |> equal "B"

// [<Fact>]
// let ``Custom F# exceptions work`` () =
//     try
//         MyEx(4,"ERROR") |> raise
//     with
//     | MyEx(4, msg) as e -> (box e :? Exception, msg + "!!")
//     | MyEx(_, msg) as e -> (box e :? Exception, msg + "??")
//     | ex -> (false, "unknown")
//     |> equal (true, "ERROR!!")

// [<Fact>]
// let ``Custom exceptions work`` () =
//     try
//         MyEx2(5.5) |> raise
//     with
//     | :? MyEx2 as ex -> (box ex :? Exception, ex.Message, ex.Code)
//     | ex -> (false, "unknown", 0.)
//     |> equal (true, "Code: 5", 5.5)

// [<Fact>]
// let ``reraise works`` () =
//     try
//         try
//             Exception("Will I be reraised?") |> raise
//         with _ ->
//             try
//                 reraise()
//             with _ -> reraise()
//         "foo"
//     with ex -> ex.Message
//     |> equal "Will I be reraised?"

// [<Fact>]
// let ``This context is not lost in closures within implicit constructor`` () = // See #1444
//     ThisContextInConstructor(7).Value() |> equal 7

// [<Fact>]
// let ``Type can be casted to an interface implemented by parent`` () =
//     let c = TestType10Child()
//     let f1 = c :> ITest4
//     let f2 = c :> ITest3
//     f1.Add(4, 5) |> equal 9
//     f1.Add2(4, 5) |> equal -1
//     f2.Add2(4, 5) |> equal -1

[<Fact>]
let ``ClassAttribute works`` () = // See #573
    let t1 = TypeWithClassAttribute(8)
    t1.Pos |> equal 8

// [<Fact>]
// let ``Issue #1975: Compile type with parameterized units of measure as generic`` () =
//     let a = makeTestTypeWithParameterizedUnitMeasureType 2.
//     equal 2. a.Value

// // // Test ported from https://github.com/fable-compiler/Fable/pull/1336/files
// // [<Fact>]
// // let ``default value attributes works`` () =
// //     let withDefaultValue = TestTypeWithDefaultValue()

// //     withDefaultValue.IntValue |> equal Unchecked.defaultof<int>
// //     withDefaultValue.IntValue |> equal 0

// //     withDefaultValue.StringValue |> equal Unchecked.defaultof<string>
// //     withDefaultValue.StringValue |> equal null

// //     withDefaultValue.ObjValue |> equal Unchecked.defaultof<System.Collections.Generic.Dictionary<string, string>>
// //     withDefaultValue.ObjValue |> equal null

// [<Fact>]
// let ``Private fields don't conflict with parent classes`` () = // See #2070
//     let a1 = InfoBClass({ InfoA = { Foo = "foo" }; Bar = "bar" }) :> InfoAClass
//     let a2 = a1.WithFoo("foo2")
//     a1.Foo |> equal "foo"
//     a2.Foo |> equal "foo2"

// // See #2084
// [<Fact>]
// let ``Non-mangled interfaces work with object expressions`` () =
//     let mutable foo = "Foo"
//     let foo = { new FooInterface with
//                     member _.Foo with get() = foo and set x = foo <- x
//                     member _.DoSomething(f, x) = let f = f 1. in f x * f 0.2
//                     member _.Item with get(i) = foo[i] and set i c = foo <- FooClass.ChangeChar(foo, i - 1, c)
//                     member _.Sum(items) = Array.reduce (+) items }

//     let addPlus2 x y = x + y + 2.
//     let multiplyTwice x y = x * y * y

//     foo[3] <- 'W'
//     foo.Foo <- foo.Foo + foo.DoSomething(addPlus2, 3.).ToString("F2").Replace(",", ".") + foo[2].ToString()
//     foo.Foo <- foo.Foo + foo.Sum("a", "bc", "d")

//     foo.Foo |> equal "FoW19.20Wabcd"

// // See #2084
// [<Fact>]
// let ``Mangled interfaces work with object expressions`` () =
//     let mutable bar = "Bar"
//     let bar = { new BarInterface with
//                     member _.Bar with get() = bar and set x = bar <- x
//                     member _.DoSomething(f, x) = let f = f 4.3 in f x + f x
//                     member _.Item with get(i) = bar[i] and set _ c = bar <- FooClass.ChangeChar(bar, 0, c)
//                     member _.Item with get(c) = bar.ToCharArray() |> Array.exists ((=) c)
//                     member _.Sum(items) = Array.rev items |> Array.reduce (+)  }

//     let addPlus2 x y = x + y + 2.
//     let multiplyTwice x y = x * y * y

//     bar[3] <- 'Z'
//     bar.Bar <- bar.Bar + bar.DoSomething(multiplyTwice, 3.).ToString("F2").Replace(",", ".") + bar[2].ToString() + (sprintf "%b%b" bar['B'] bar['x'])
//     bar.Bar <- bar.Bar + bar.Sum("a", "bc", "d")

//     bar.Bar |> equal "Zar77.40rfalsefalsedbca"

// // See #2084
// [<Fact>]
// let ``Non-mangled interfaces work with classes`` () =
//     let addPlus2 x y = x + y + 2.
//     let multiplyTwice x y = x * y * y
//     let foo2 = FooClass("Foo") :> FooInterface
//     foo2[0] <- 'W'
//     foo2.Foo <- foo2.Foo + foo2.DoSomething(multiplyTwice, 3.).ToString("F2").Replace(',', '.') + foo2[2].ToString()
//     foo2.Foo <- foo2.Foo + foo2.Sum("a", "bc", "d")
//     foo2.Foo |> equal "Woo1020.00oabcabcdabcabcd"

// // See #2084
// [<Fact>]
// let ``Mangled interfaces work with classes`` () =
//     let addPlus2 x y = x + y + 2.
//     let multiplyTwice x y = x * y * y
//     let bar2 = BarClass("Bar") :> BarInterface
//     bar2[0] <- 'Z'
//     bar2.Bar <- bar2.Bar + bar2.DoSomething(addPlus2, 3.).ToString("F2").Replace(",", ".") + bar2[2].ToString() + (sprintf "%b%b" bar2['B'] bar2['x'])
//     bar2.Bar <- bar2.Bar + bar2.Sum("a", "bc", "d")
//     bar2.Bar |> equal "BZr9536.74rtruefalseaabcbcaabcbcdd"

// [<Fact>]
// let ``Multiple `this` references work in nested attached members`` () =
//     (MixedThese(2) :> Interface1).Create(3).Add() |> equal 5

// [<Fact>]
// let ``Two unions of different type with same shape are not equal`` () =
//     areEqual (MyUnion1.Foo(1,2)) (MyUnion2.Foo(1,2)) |> equal false
//     areEqual (MyUnion1.Foo(1,2)) (MyUnion1.Foo(1,2)) |> equal true

// [<Fact>]
// let ``Two records of different type with same shape are not equal`` () =
//     areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord2.Foo = 2; Bar = "oh" } |> equal false
//     areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord1.Foo = 2; Bar = "oh" } |> equal true

// [<Fact>]
// let ``IsSubclassOf checks whole hierarchy`` () =
//     typeof<SubclassTest2>.IsSubclassOf(typeof<SubclassTest1>) |> equal true
//     typeof<SubclassTest3>.IsSubclassOf(typeof<SubclassTest1>) |> equal true

// [<Fact>]
// let ``IsSubclassOf returns true for System.Object`` () =
//     typeof<SubclassTest2>.IsSubclassOf(typeof<obj>) |> equal true
//     typeof<string>.IsSubclassOf(typeof<obj>) |> equal true
//     typeof<int>.IsSubclassOf(typeof<obj>) |> equal true
//     typeof<unit -> unit>.IsSubclassOf(typeof<obj>) |> equal true

// [<Fact>]
// let ``IsInstanceOfType works with class types`` () =
//     let s1, s2 = SubclassTest1(), SubclassTest2()
//     typeof<obj>.IsInstanceOfType(s1) |> equal true
//     typeof<SubclassTest1>.IsInstanceOfType(s1) |> equal true
//     typeof<SubclassTest2>.IsInstanceOfType(s1) |> equal false
//     typeof<SubclassTest3>.IsInstanceOfType(s1) |> equal false
//     typeof<SubclassTest1>.IsInstanceOfType(s2) |> equal true
//     typeof<SubclassTest2>.IsInstanceOfType(s2) |> equal true
//     typeof<SubclassTest3>.IsInstanceOfType(s2) |> equal false

// [<Fact>]
// let ``IsInstanceOfType works with nominal records`` () =
//     typeof<MyRecord1>.IsInstanceOfType({ MyRecord1.Foo = 2; Bar = "oh" }) |> equal true
//     typeof<obj>.IsInstanceOfType({ MyRecord1.Foo = 2; Bar = "oh" }) |> equal true
//     typeof<MyRecord2>.IsInstanceOfType({ MyRecord1.Foo = 2; Bar = "oh" }) |> equal false

// [<Fact>]
// let ``IsInstanceOfType works with nominal unions`` () =
//     typeof<MyUnion1>.IsInstanceOfType(MyUnion1.Foo(1,2)) |> equal true
//     typeof<obj>.IsInstanceOfType(MyUnion1.Foo(1,2)) |> equal true
//     typeof<MyUnion2>.IsInstanceOfType(MyUnion1.Foo(1,2)) |> equal false

// // Expected to always return true for any numeric type, just like :? operator
// [<Fact>]
// let ``IsInstanceOfType works with enums`` () =
//     typeof<EnumFoo>.IsInstanceOfType(EnumFoo.Foo) |> equal true
//     typeof<EnumFoo>.IsInstanceOfType(EnumFoo.Bar) |> equal true
//     typeof<obj>.IsInstanceOfType(EnumFoo.Bar) |> equal true

// // Expected to always return true for any function and function type, just like :? operator
// [<Fact>]
// let ``IsInstanceOfType works with functions`` () =
//     typeof<unit -> unit>.IsInstanceOfType(fun () -> ()) |> equal true
//     typeof<obj>.IsInstanceOfType(fun () -> ()) |> equal true
//     //typeof<unit -> int>.IsInstanceOfType(fun () -> ()) |> equal false
//     typeof<string -> int>.IsInstanceOfType(String.length) |> equal true
//     typeof<obj>.IsInstanceOfType(String.length) |> equal true
//     //typeof<int -> int>.IsInstanceOfType(String.length) |> equal false

// [<Fact>]
// let ``IsInstanceOfType works with primitives`` () =
//     typeof<string>.IsInstanceOfType("hello") |> equal true
//     typeof<obj>.IsInstanceOfType("hello") |> equal true
//     typeof<string>.IsInstanceOfType(5) |> equal false
//     typeof<int>.IsInstanceOfType(5) |> equal true
//     typeof<obj>.IsInstanceOfType(5) |> equal true
//     typeof<int>.IsInstanceOfType("hello") |> equal false

// #if FABLE_COMPILER
// [<Fact>]
// let ``Choice with arity 3+ is represented correctly`` () = // See #2485
//     Choice2Of3 55 |> Fable.Core.Reflection.getCaseName |> equal "Choice2Of3"
//     Choice3Of3 55 |> Fable.Core.Reflection.getCaseName |> equal "Choice3Of3"
// #endif

// [<Fact>]
// let ``Can call the base version of a mangled abstract method that was declared above in the hierarchy`` () =
//     let c = ConcreteClass1()
//     c.MyMethod(4) |> equal 58
