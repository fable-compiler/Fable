module Fable.Tests.TypeTests

open System
open Util.Testing
open Xunit

type Type5Test(greeting: string) =
    member _.Value = greeting
    member _.Overload(x: int) = x + x
    member _.Overload(x: int, y: int) = x + y

type Type6Test(x: int) =
    let mutable i = x
    member _.Value2 = i + i

    member _.Value3
        with get () = i * i
        and set (v) = i <- v

type SecondaryCons(x: int) =
    new() = SecondaryCons(5)
    member _.Value = x

type MultipleCons(x: int, y: int) =
    new() = MultipleCons(2, 3)
    new(x: int) = MultipleCons(x, 4)
    member _.Value = x + y

type ThisContextInConstructor(v) =
    member _.GetValue() = v

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

[<Fact>]
let ``test class constructor and property getter work`` () =
    let t = Type5Test("hello")
    equal "hello" t.Value

[<Fact>]
let ``test class method overload with one arg works`` () =
    let t = Type5Test("")
    equal 4 (t.Overload(2))

[<Fact>]
let ``test class method overload with two args works`` () =
    let t = Type5Test("")
    equal 5 (t.Overload(2, 3))

[<Fact>]
let ``test class mutable field getter works`` () =
    let t = Type6Test(5)
    equal 10 t.Value2

[<Fact>]
let ``test class mutable field getter and setter work`` () =
    let t = Type6Test(5)
    equal 25 t.Value3
    t.Value3 <- 10
    equal 20 t.Value2
    equal 100 t.Value3

[<Fact>]
let ``test secondary constructor works`` () =
    let s1 = SecondaryCons(3)
    let s2 = SecondaryCons()
    equal 3 s1.Value
    equal 5 s2.Value

[<Fact>]
let ``test multiple constructors work`` () =
    let m1 = MultipleCons()
    let m2 = MultipleCons(5)
    let m3 = MultipleCons(7, 7)
    equal 5 m1.Value
    equal 9 m2.Value
    equal 14 m3.Value

[<Fact>]
let ``test closure in constructor captures value`` () =
    ThisContextInConstructor(7).GetValue() |> equal 7

// Object expression tests

type IGreeter =
    abstract Greet: string -> string

type IAdder =
    abstract Add: int -> int -> int

type IValueHolder =
    abstract Value: int

[<Fact>]
let ``test object expression with single method works`` () =
    let greeter = { new IGreeter with member _.Greet(name) = "Hello, " + name }
    equal "Hello, World" (greeter.Greet("World"))

[<Fact>]
let ``test object expression with curried method works`` () =
    let adder = { new IAdder with member _.Add x y = x + y }
    equal 7 (adder.Add 3 4)

[<Fact>]
let ``test object expression with property getter works`` () =
    let holder = { new IValueHolder with member _.Value = 42 }
    equal 42 holder.Value

[<Fact>]
let ``test object expression passed as function argument works`` () =
    let greet (g: IGreeter) name = g.Greet(name)
    let greeter = { new IGreeter with member _.Greet(name) = "Hi, " + name }
    equal "Hi, Bob" (greet greeter "Bob")

[<Fact>]
let ``test object expression captures closure values`` () =
    let prefix = "Dear"
    let greeter = { new IGreeter with member _.Greet(name) = prefix + " " + name }
    equal "Dear Alice" (greeter.Greet("Alice"))

// Class implementing interface tests

type MyGreeter(prefix: string) =
    interface IGreeter with
        member _.Greet(name) = prefix + ", " + name

type MyAdder(offset: int) =
    interface IAdder with
        member _.Add x y = x + y + offset

type MyValueHolder(v: int) =
    interface IValueHolder with
        member _.Value = v

type ICounter =
    abstract Count: int
    abstract Increment: unit -> int

type SimpleCounter(start: int) =
    let mutable count = start
    interface ICounter with
        member _.Count = count
        member _.Increment() =
            count <- count + 1
            count

type IDescribable =
    abstract Describe: unit -> string

type DescribableImpl(name: string) =
    member _.Describe() = name
    interface IDescribable with
        member x.Describe() = x.Describe() + " (described)"

type ILabeled =
    abstract Label: string

type LabeledImpl(s: string) =
    member _.Label = s
    interface ILabeled with
        member x.Label = x.Label + " (labeled)"

type IReadOnlyProps =
    abstract OnlyGet: int
    abstract OnlyProp: int

type ReadOnlyPropsImpl() =
    interface IReadOnlyProps with
        member _.OnlyGet = 0
        member _.OnlyProp = 3

type IConfigurable =
    abstract Configure: (int -> int) -> int

type ConfigurableImpl(baseValue: int) =
    interface IConfigurable with
        member _.Configure(transform) = transform baseValue

type IApplicator =
    abstract Apply: (string -> string -> string) -> string -> string -> string

type ApplicatorImpl() =
    interface IApplicator with
        member _.Apply(combiner) (a) (b) = combiner a b

type ClassWithFnParam(transform: int -> int) =
    member _.Run(x: int) = transform x

type Employee2 = { empName: string; empAge: float; empLocation: Location2 }
and Location2 = { locName: string; mutable locEmployees: Employee2 list }

type SameCheckRecord = { SCFoo: int; SCBar: string }

type SameCheckUnion =
    | UCFoo of int * int
    | UCBar of float
    | UCBaz

[<Fact>]
let ``test class implementing interface method works`` () =
    let greeter = MyGreeter("Hello") :> IGreeter
    equal "Hello, World" (greeter.Greet("World"))

[<Fact>]
let ``test class implementing interface property works`` () =
    let holder = MyValueHolder(42) :> IValueHolder
    equal 42 holder.Value

[<Fact>]
let ``test class implementing interface with curried method works`` () =
    let adder = MyAdder(0) :> IAdder
    equal 7 (adder.Add 3 4)

[<Fact>]
let ``test class with offset implementing interface works`` () =
    let adder = MyAdder(10) :> IAdder
    equal 17 (adder.Add 3 4)

[<Fact>]
let ``test class instance passed to function expecting interface works`` () =
    let greet (g: IGreeter) name = g.Greet(name)
    let greeter = MyGreeter("Hey")
    equal "Hey, Bob" (greet greeter "Bob")

[<Fact>]
let ``test multiple classes implementing same interface work`` () =
    let g1 = MyGreeter("Hi") :> IGreeter
    let g2 = { new IGreeter with member _.Greet(name) = "Yo, " + name }
    equal "Hi, X" (g1.Greet("X"))
    equal "Yo, X" (g2.Greet("X"))

[<Fact>]
let ``test class implementing interface with closure capture works`` () =
    let makeGreeter prefix = MyGreeter(prefix) :> IGreeter
    let g = makeGreeter "Dear"
    equal "Dear, Alice" (g.Greet("Alice"))

[<Fact>]
let ``test class implementing interface method with mutable state works`` () =
    let counter = SimpleCounter(0) :> ICounter
    let r1 = counter.Increment()
    let r2 = counter.Increment()
    let r3 = counter.Increment()
    equal 1 r1
    equal 2 r2
    equal 3 r3

[<Fact>]
let ``test a type can overload an interface method`` () =
    let obj = DescribableImpl("hello")
    equal "hello" (obj.Describe())
    equal "hello (described)" ((obj :> IDescribable).Describe())

[<Fact>]
let ``test a type can overload an interface getter`` () =
    let obj = LabeledImpl("hi")
    equal "hi" (obj.Label)
    equal "hi (labeled)" ((obj :> ILabeled).Label)

[<Fact>]
let ``test class implementing read-only interface properties works`` () =
    let obj = ReadOnlyPropsImpl() :> IReadOnlyProps
    equal 0 obj.OnlyGet
    equal 3 obj.OnlyProp

[<Fact>]
let ``test interface method that takes function parameter and calls it works`` () =
    let cfg = ConfigurableImpl(10) :> IConfigurable
    cfg.Configure(fun x -> x * 2) |> equal 20
    cfg.Configure(fun x -> x + 5) |> equal 15

[<Fact>]
let ``test interface method with multi-arg function parameter works`` () =
    let app = ApplicatorImpl() :> IApplicator
    app.Apply (fun a b -> a + " " + b) "hello" "world" |> equal "hello world"

[<Fact>]
let ``test class constructor with function parameter works`` () =
    let obj = ClassWithFnParam(fun x -> x * 3)
    obj.Run(7) |> equal 21

// Basic language feature tests

[<Fact>]
let ``test unit arguments work`` () =
    let update () = ((), ())
    update () |> equal ((), ())

// TODO: Mutable record fields use maps:put which returns a new map in Erlang,
// but the original binding is not updated (Erlang maps are immutable).
// Needs process dictionary approach for mutable record fields.
// [<Fact>]
// let ``test circular record dependencies work`` () =
//     let loc = { locName = "NYC"; locEmployees = [] }
//     let emp = { empName = "Joe"; empAge = 30.0; empLocation = loc }
//     loc.locEmployees <- [ emp ]
//     equal "Joe" loc.locEmployees.Head.empName
//     equal "NYC" emp.empLocation.locName

[<Fact>]
let ``test records of same type with same values are equal`` () =
    let r1 = { SCFoo = 2; SCBar = "oh" }
    let r2 = { SCFoo = 2; SCBar = "oh" }
    equal true (r1 = r2)

[<Fact>]
let ``test records of same type with different values are not equal`` () =
    let r1 = { SCFoo = 2; SCBar = "oh" }
    let r2 = { SCFoo = 3; SCBar = "oh" }
    equal true (r1 <> r2)

[<Fact>]
let ``test unions of same type with same values are equal`` () =
    let u1 = UCFoo(1, 2)
    let u2 = UCFoo(1, 2)
    equal true (u1 = u2)

// Type testing tests

[<Fact>]
let ``test type test string works`` () =
    let test (o: obj) =
        match o with
        | :? string -> true
        | _ -> false
    box "hello" |> test |> equal true
    box 5 |> test |> equal false

[<Fact>]
let ``test type test int works`` () =
    let test (o: obj) =
        match o with
        | :? int -> true
        | _ -> false
    box 5 |> test |> equal true
    box "hello" |> test |> equal false

[<Fact>]
let ``test type test float works`` () =
    let test (o: obj) =
        match o with
        | :? float -> true
        | _ -> false
    box 3.14 |> test |> equal true
    box 5 |> test |> equal false

[<Fact>]
let ``test type test bool works`` () =
    let test (o: obj) =
        match o with
        | :? bool -> true
        | _ -> false
    box true |> test |> equal true
    box "true" |> test |> equal false

[<Fact>]
let ``test type test with pattern binding works`` () =
    let test (o: obj) =
        match o with
        | :? string as s -> "string:" + s
        | :? int as i -> "int:" + string i
        | _ -> "other"
    box "hello" |> test |> equal "string:hello"
    box 42 |> test |> equal "int:42"

[<Fact>]
let ``test type test multiple types works`` () =
    let classify (o: obj) =
        match o with
        | :? string -> "string"
        | :? int -> "int"
        | :? float -> "float"
        | :? bool -> "bool"
        | _ -> "unknown"
    box "hi" |> classify |> equal "string"
    box 1 |> classify |> equal "int"
    box 2.5 |> classify |> equal "float"
    box false |> classify |> equal "bool"

[<Fact>]
let ``test type test list works`` () =
    let test (o: obj) =
        match o with
        | :? list<int> -> true
        | _ -> false
    box [ 1; 2; 3 ] |> test |> equal true
    box "hello" |> test |> equal false

[<Fact>]
let ``test box and unbox roundtrip works`` () =
    let x = 42
    let boxed: obj = box x
    let unboxed: int = unbox boxed
    equal 42 unboxed

[<Fact>]
let ``test box and unbox string works`` () =
    let s = "hello"
    let boxed: obj = box s
    let unboxed: string = unbox boxed
    equal "hello" unboxed

[<Fact>]
let ``test unions of same type different cases are not equal`` () =
    let u1 = UCFoo(1, 2)
    let u2 = UCBar(1.0)
    equal true (u1 <> u2)

[<Fact>]
let ``test unions of same type different values are not equal`` () =
    let u1 = UCFoo(1, 2)
    let u2 = UCFoo(3, 4)
    equal true (u1 <> u2)

[<Fact>]
let ``test box and unbox bool works`` () =
    let b = true
    let boxed: obj = box b
    let unboxed: bool = unbox boxed
    equal true unboxed

[<Fact>]
let ``test box and unbox tuple works`` () =
    let t = (1, "a")
    let boxed: obj = box t
    let unboxed: int * string = unbox boxed
    equal (1, "a") unboxed

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

// --- Type test with specific types ---

// TODO: Erlang has a single integer type, so :? int64 and :? bigint can't
// distinguish from :? int. These type tests are not feasible in Beam.
// [<Fact>]
// let ``test Type test with Long`` () =
//     let isLong (x: obj) =
//         match x with
//         | :? int64 -> true
//         | _ -> false
//     box 5L |> isLong |> equal true
//     box 50 |> isLong |> equal false
//
// [<Fact>]
// let ``test Type test with BigInt`` () =
//     let isBigInt (x: obj) =
//         match x with
//         | :? bigint -> true
//         | _ -> false
//     box 5I |> isBigInt |> equal true
//     box 50 |> isBigInt |> equal false

// --- Same shape, different types ---

type MyUnion1 =
    | Foo1 of int * int
    | Bar1 of float
    | Baz1

type MyUnion2 =
    | Foo2 of int * int
    override _.ToString() = "ffff"

type MyRecord1 = { Foo: int; Bar: string }
type MyRecord2 = { Foo2R: int; Bar2R: string }

let areEqual (x: obj) (y: obj) = x = y

// TODO: Different types with same shape comparison requires runtime type info
// [<Fact>]
// let ``test Two unions of different type with same shape are not equal`` () =
//     areEqual (Foo1(1, 2)) (Foo2(1, 2))
//     |> equal false
//     areEqual (Foo1(1, 2)) (Foo1(1, 2))
//     |> equal true

// TODO: Different types with same shape comparison requires runtime type info
// [<Fact>]
// let ``test Two records of different type with same shape are not equal`` () =
//     areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord2.Foo2R = 2; Bar2R = "oh" }
//     |> equal false
//     areEqual { MyRecord1.Foo = 2; Bar = "oh" } { MyRecord1.Foo = 2; Bar = "oh" }
//     |> equal true

// TODO: Downcasting from interface to class, type tests on records/unions/interfaces,
// and typeof comparison require runtime type information not available in Erlang.
// TypeCast is erased, so :?> only works for simple casts (e.g., obj to concrete type via box/unbox).

// TODO: Struct types with val fields generate badmap on undefined
// [<Fact>]
// let ``test Value Type records work`` () =
//     let foo1 = ValueType<_>("foo")
//     let foo2 = ValueType<_>("foo")
//     foo1.Value |> equal "foo"
//     foo1.value |> equal "foo"
//     foo1 = foo2 |> equal true

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

// TODO: Struct types with val fields generate badmap on undefined
// [<Fact>]
// let ``test Value Types work`` () =
//     let bar1 = ValueType1("bar")
//     let bar2 = ValueType1("bar")
//     bar1.Value |> equal "bar"
//     bar1 = bar2 |> equal true

// TODO: Struct types with val fields generate badmap on undefined
// [<Fact>]
// let ``test Other Value Types work`` () =
//     let test2 = ValueType2(3, 4)
//     test2.Value |> equal 7
//     let p = Point2D(2.)
//     p.Y |> equal 2.

// TODO: TypeTestGeneric (box e :? Exception) not supported by Fable Beam
// [<Fact>]
// let ``test Custom F# exceptions work`` () =
//     try
//         MyEx(4, "ERROR") |> raise
//     with
//     | MyEx (4, msg) as e -> (box e :? Exception, msg + "!!")
//     | MyEx (_, msg) as e -> (box e :? Exception, msg + "??")
//     | ex -> (false, "unknown")
//     |> equal (true, "ERROR!!")

// TODO: TypeTestGeneric (:? MyEx2) not supported by Fable Beam
// [<Fact>]
// let ``test Custom exceptions work`` () =
//     try
//         MyEx2(5.5) |> raise
//     with
//     | :? MyEx2 as ex -> (box ex :? Exception, ex.Message, ex.Code)
//     | ex -> (false, "unknown", 0.)
//     |> equal (true, "Code: 5", 5.5)

// TODO: System.Exception..ctor not supported by Fable Beam
// [<Fact>]
// let ``test reraise works`` () =
//     try
//         try
//             Exception("Will I be reraised?") |> raise
//         with
//         | _ ->
//             try
//                 reraise ()
//             with
//             | _ -> reraise ()
//         "foo"
//     with
//     | ex -> ex.Message
//     |> equal "Will I be reraised?"
