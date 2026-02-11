module Fable.Tests.TypeTests

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

// TODO: Downcasting from interface to class, type tests on records/unions/interfaces,
// and typeof comparison require runtime type information not available in Erlang.
// TypeCast is erased, so :?> only works for simple casts (e.g., obj to concrete type via box/unbox).
