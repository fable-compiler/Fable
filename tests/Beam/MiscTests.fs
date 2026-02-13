module Fable.Tests.Misc

#nowarn "40"

open System
open Util.Testing

// ============= Helper Types =============

// Self-referencing class hierarchy
type Base() =
    let mutable x = 5
    member this.Mutate i = x <- x + i
    member _.Value = x

type MyTest(i) as myself =
    inherit Base()
    let mutable y = 12
    do myself.Mutate(i+2)
    do myself.Mutate2(i)
    member this.Mutate2 i = y <- y + i
    member _.Value2 = y
    member _.Foo() = myself.Value * 2

// Partial function helpers
let log2 (a: string) (b: string) = String.Format("a = {0}, b = {1}", a, b)
let logItem1 = log2 "item1"
let logItem2 = log2 "item2"

type PartialFunctions() =
    member _.logItem1 = log2 "item1"
    member _.logItem2 = log2 "item2"

// Computation expression
type MaybeBuilder() =
    member _.Bind(x,f) = Option.bind f x
    member _.Return v = Some v
    member _.ReturnFrom o = o
let maybe = MaybeBuilder()

let riskyOp x y =
    if x + y < 100 then Some(x+y) else None

let execMaybe a = maybe {
    let! b = riskyOp a (a+1)
    let! c = riskyOp b (b+1)
    return c
}

// Units of measure
[<Measure>] type km
[<Measure>] type mi
[<Measure>] type h

[<Measure>] type Measure1
[<Measure>] type Measure2 = Measure1

type MeasureTest() =
    member _.Method(x: float<Measure2>) = x

type Vector3D<[<Measure>] 'u> =
    { x: float<'u>; y: float<'u>; z: float<'u> }
    static member (+) (v1: Vector3D<'u>, v2: Vector3D<'u>) =
        { x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z }

// Inline functions
let inline f x y = x + y
let inline sum x = fun y -> x + y

// Function helpers for delegate tests
let f1 x y z = x + y + z
let f2 x = x + x
let f3 () = 5

type MyDelegate = Func<int>

// Disposable types
type DisposableFoo() =
    member _.Foo() = 5
    interface IDisposable with
        member _.Dispose () = ()

type DisposableBar(v: int ref) =
    do v.Value <- 10
    interface IDisposable with
        member _.Dispose () = v.Value <- 20

let createCellDisposable (cell: int ref) =
    cell.Value <- 10
    { new System.IDisposable with
        member x.Dispose() = cell.Value <- 20 }

// Active pattern
let (|NonEmpty|_|) (s: string) =
    match s.Trim() with "" -> None | s -> Some s

// Object expression interfaces

// TODO: Object expression self-references (x2, this) generate undefined function calls instead of referencing current object
// type IFoo =
//     abstract Bar: string -> string
// type IFoo2 =
//     abstract Value: int with get, set
//     abstract Test: int -> int
//     abstract MakeFoo: unit -> IFoo
// type Foo(i) =
//     let mutable j = 5
//     member x.Value = i + j
//     member x.MakeFoo2() = {
//         new IFoo2 with
//         member x2.Value
//             with get() = x.Value * 2
//             and set(i) = j <- j + i
//         member x2.Test(i) = x2.Value - i
//         member x2.MakeFoo() = {
//             new IFoo with
//             member x3.Bar(s: string) =
//                 sprintf "%s: %i %i %i" s x.Value x2.Value j
//         }
//     }
// type IFoo3 =
//     abstract Bar: int with get, set

// TODO: RecursiveType "as self" constructor — self-reference generates badkey error
// type RecursiveType(subscribe) as self =
//     let foo = 3
//     let getNumber() = 3
//     do subscribe (getNumber >> self.Add2)
//     member _.Add2(i) = self.MultiplyFoo(i) + 2
//     member _.MultiplyFoo(i) = i * foo

// TODO: Interface inheritance in object expressions — self-ref generates this() call
// type Eater =
//     abstract Bite: unit -> int
// type Taster =
//     inherit Eater
//     abstract Starter: float
//     abstract Taste: quality: float * quantity: float -> int
// let taste (com: Taster) qlty qty =
//     com.Starter * qlty + qty |> int

type IRenderer =
    abstract member doWork: unit -> string

type MyComponent(name) as self =
    let work i = sprintf "%s-%i" name i
    let create2 () = { new IRenderer with member _.doWork () = work 2 }
    let create3 = { new IRenderer with member _.doWork () = work 3 }
    let create4 = { new IRenderer with member _.doWork () = self.Work 4 }
    let create5() = { new IRenderer with member _.doWork () = self.Work 5 }
    member _.Work i = work i
    member _.works1 () = { new IRenderer with member _.doWork () = work 1 }
    member _.works2 () = create2()
    member _.works3 () = create3
    member _.works4 () = create4
    member _.works5 () = create5()

// Type extension types
type SomeClass(name: string) =
    member x.Name = name

type AnotherClass(value: int) =
    member x.Value = value

module NestedModule =
    type AnotherClass(value: int) =
        member x.Value = value + 5

// Inlined object expression helpers
type INum = abstract member Num: int
let inline makeNum f = { new INum with member _.Num = f() }

type MyTestClass(n) =
    let addOne x = x + 4
    let inner = makeNum (fun () -> addOne n)
    member _.GetNum() = inner.Num

// Extensions module
module Extensions =
    type IDisposable with
        static member Create(f) =
            { new IDisposable with
                member _.Dispose() = f() }

    type SomeClass with
        member x.FullName = sprintf "%s Smith" x.Name
        member x.NameTimes (i: int, j: int) = String.replicate (i + j) x.Name

    type AnotherClass with
        member x.FullName = sprintf "%i" x.Value
        member x.Overload(i: int) = i * 4
        member x.Overload(s: string) = s + s
        member x.Value2 = x.Value * 2

    type NestedModule.AnotherClass with
        member x.Value2 = x.Value * 4

open Extensions

// Curried interface methods
type IInterface =
    abstract member Member : thing1:string -> thing2:string -> string

module private MyPrivateModule =
    let private bar = "bar"
    let publicFoo() = sprintf "foo %s" bar
    type Concrete() =
        interface IInterface with
            member this.Member (thing1: string) (thing2: string) =
                sprintf "%s %s" thing2 thing1

// Trampoline module
module Trampoline =
    type Result<'a, 'b> =
        | Continue of 'a
        | Break of 'b
    let run func arg =
        let mutable state = arg
        let mutable result = None
        while result.IsNone do
            match func state with
            | Continue r -> state <- r
            | Break r -> result <- Some r
        result.Value

// Style builder helper
module StyleBuilderHelper =
    type StyleBuilderHelper = { TopOffset : int; BottomOffset : int }
    type DomBuilder = { ElementType : string; StyleBuilderHelper : StyleBuilderHelper }
    let test() =
        let helper = { TopOffset = 1; BottomOffset = 2 }
        let builder1 = { ElementType = "test"; StyleBuilderHelper = helper }
        let builder2 = { builder1 with StyleBuilderHelper = { builder1.StyleBuilderHelper with BottomOffset = 3 } }
        match builder1, builder2 with
        | { StyleBuilderHelper = { BottomOffset = 2 } },
          { StyleBuilderHelper = { TopOffset = 1; BottomOffset = 3 } } -> true
        | _ -> false

// Module name shadowing
module Same =
    let a = 5
    module Same =
        module Same =
            let a = 10
        let shouldEqual5 = a
        let shouldEqual10 = Same.a
        let private Same = 20
        let shouldEqual20 = Same
        let shouldEqual30 = let Same = 25 in Same + 5

// Shape types for pattern matching
type Shape =
    | Circle of int
    | Square of int
    | Rectangle of int * int

type MyEnum =
    | One = 1
    | Two = 2

type MyTestRef = TestRef of bool ref

exception MyCustomException of string
exception MyArgumentException of string

let sideEffect() = ()

let setCellBuggy x: (int*int) option =
    Option.map (fun (x, y) -> max (x - 1) 0, y) x

let setCell x: (int*int) option =
    let max = max
    Option.map (fun (x, y) -> max (x - 1) 0, y) x

type MutableFoo =
    { mutable x: int }

// Mutation test class
type InliningMutationTest(l: int, r: int) =
    let mutable left = 0
    let call() =
        left <- l
        r
    member _.Run() =
        let right = call()
        left + right

let ``inlineData PR #2683`` = [3, 2, 5; 5, 10, 15; 10, 20, 30]

// Delegate test helpers
type MyIntDelegate = delegate of unit -> int

let get42 () = 42

let dtest1 (f: MyIntDelegate -> int) =
    f get42

let dtest2 (f: MyIntDelegate -> int) =
    let get43 () = 43
    f get43

let dInvoke (d: MyIntDelegate) =
    d.Invoke ()

let delay (f: unit -> unit) = f

let inline inlineLambdaWithAnonRecord callback =
    fun () -> {| A = 1 |} |> callback

// ============= Tests =============

// -- Partial Functions --

[<Fact>]
let ``test Module members from partial functions work`` () =
    logItem1 "item1" |> equal "a = item1, b = item1"

[<Fact>]
let ``test Class members from partial functions work`` () =
    let x = PartialFunctions()
    x.logItem1 "item1" |> equal "a = item1, b = item1"

[<Fact>]
let ``test Local values from partial functions work`` () =
    let logItem1 = log2 "item1"
    let logItem2 = log2 "item2"
    logItem1 "item1" |> equal "a = item1, b = item1"

// -- Computation Expressions --

[<Fact>]
let ``test Custom computation expressions work`` () =
    execMaybe 5 |> equal (Some 23)
    execMaybe 99 |> equal None

// -- Self References in Constructors --
// TODO: "as myself" self-reference in constructors generates {badkey,x} — class constructor codegen issue

// [<Fact>]
// let ``test Self references in constructors work`` () =
//     let t = MyTest(5)
//     equal 12 t.Value
//     equal 17 t.Value2

// [<Fact>]
// let ``test Using self identifier from class definition in members works`` () =
//     let t = MyTest(5)
//     t.Foo() |> equal 24

// -- Inline Methods --

[<Fact>]
let ``test Inline methods work`` () =
    f 2 3 |> equal 5

[<Fact>]
let ``test Inlined arguments with delayed resolution are only evaluated once`` () =
    let mutable x = 0
    let foo() =
        x <- x + 1
        10
    let f = sum (foo())
    f 20 |> f |> equal 40
    equal 1 x

// -- Delegates --
// TODO: Delegate/Func/Action conversion and .Invoke() not yet supported in Beam Replacements

// [<Fact>]
// let ``test Passing delegate works`` () =
//     dtest1 dInvoke |> equal 42
//     dtest2 dInvoke |> equal 43

// [<Fact>]
// let ``test Conversion to delegate works`` () =
//     (System.Func<_,_,_,_> f1).Invoke(1,2,3) |> equal 6
//     let f = f1
//     (System.Func<_,_,_,_> f).Invoke(1,2,3) |> equal 6
//     let del = System.Func<_,_,_,_>(fun x y z -> x + y + z)
//     del.Invoke(1,2,3) |> equal 6
//     (System.Func<_,_> f2).Invoke(2) |> equal 4
//     let func1 : Func<int> = Func<int>(fun () -> 8)
//     func1.Invoke() |> equal 8
//     let fn2 () = 9
//     let func2 : Func<int> = Func<int>(fn2)
//     func2.Invoke() |> equal 9
//     let func2b = Func<unit, int>(fn2)
//     func2b.Invoke() |> equal 9
//     let fn2c () () = 9
//     let func2c : Func<int> = Func<int>(fn2c())
//     func2c.Invoke() |> equal 9
//     let fn3 i = i + 4
//     let func3 = Func<int, int>(fn3)
//     func3.Invoke(7) |> equal 11
//     let fn4 x y = x * y - 3
//     let func4 = Func<int, int, int>(fn4)
//     func4.Invoke(4, 6) |> equal 21

// [<Fact>]
// let ``test Conversion to Func works`` () =
//     (System.Func<_> f3).Invoke() |> equal 5
//     let f = Func<_>(fun () -> 6)
//     f.Invoke() |> equal 6

// [<Fact>]
// let ``test Conversion to aliased Func works`` () =
//     (MyDelegate f3).Invoke() |> equal 5
//     let f = MyDelegate(fun () -> 6)
//     f.Invoke() |> equal 6

// [<Fact>]
// let ``test Conversion to Action works`` () =
//     let mutable myField = 0
//     let f4' i = myField <- i
//     let f6' i j = myField <- i * j
//     let f1' = Action<int>(fun i -> myField <- i * 2)
//     let f2' = Action<int>(f4')
//     let f3' = Action<_>(f6' 4)
//     f1'.Invoke(1)
//     equal 2 myField
//     f2'.Invoke(8)
//     equal 8 myField
//     f3'.Invoke(10)
//     equal 40 myField

// -- Object Expressions --

// TODO: Object expression self-references (x2) — Foo.MakeFoo2() generates x2_2() undefined
// [<Fact>]
// let ``test Object expression can reference enclosing type and self`` () =
//     let f = Foo(5)
//     let f2 = f.MakeFoo2()
//     f2.Value <- 2
//     f.Value |> equal 12
//     f2.Test(2) |> equal 22

// [<Fact>]
// let ``test Nested object expressions work`` () =
//     let f = Foo(5)
//     let f2 = f.MakeFoo2()
//     f2.MakeFoo().Bar("Numbers") |> equal "Numbers: 10 20 5"

// TODO: Duplicate map keys for getter/setter — IFoo3.Bar get/set both map to 'bar' key
// [<Fact>]
// let ``test Properties in object expression work`` () =
//     let mutable backend = 0
//     let o = { new IFoo3 with member x.Bar with get() = backend and set(v) = backend <- v }
//     o.Bar |> equal 0
//     backend <- 5
//     o.Bar |> equal 5
//     o.Bar <- 10
//     o.Bar |> equal 10

// TODO: Interface inheritance self-ref — this() undefined in Taster object expression
// [<Fact>]
// let ``test Object expressions don't optimize members away`` () =
//     let o =
//         { new Taster with
//             member _.Starter = 5.5
//             member this.Taste(quality, quantity) =
//                 taste this quality quantity
//           interface Eater with
//             member _.Bite() = 25
//         }
//     o.Taste(4., 6.) |> equal 28

// TODO: RecursiveType — do expressions in "as self" constructor body not executed
// [<Fact>]
// let ``test Composition with recursive this works`` () =
//     let mutable x = 0
//     RecursiveType(fun f -> x <- f()) |> ignore
//     equal 11 x

// TODO: MyComponent object expression generates badarith — sprintf in class with "as self" constructor

// [<Fact>]
// let ``test References to enclosing type from object expression work`` () =
//     MyComponent("TestA").works1().doWork() |> equal "TestA-1"
//     MyComponent("TestB").works2().doWork() |> equal "TestB-2"
//     MyComponent("TestC").works3().doWork() |> equal "TestC-3"
//     MyComponent("TestD").works4().doWork() |> equal "TestD-4"
//     MyComponent("TestE").works5().doWork() |> equal "TestE-5"

[<Fact>]
let ``test Inlined object expression doesn't change argument this context`` () =
    let t = MyTestClass(42)
    t.GetNum() |> equal 46

// -- Type Extensions --

[<Fact>]
let ``test Type extension static methods work`` () =
    let disposed = ref false
    let disp = IDisposable.Create(fun () -> disposed.Value <- true)
    disp.Dispose ()
    equal true disposed.Value

[<Fact>]
let ``test Type extension properties work`` () =
    let c = SomeClass("John")
    equal "John Smith" c.FullName

[<Fact>]
let ``test Type extension methods work`` () =
    let c = SomeClass("John")
    c.NameTimes(1,2) |> equal "JohnJohnJohn"

[<Fact>]
let ``test Type extension methods with same name work`` () =
    let c = AnotherClass(3)
    equal "3" c.FullName

[<Fact>]
let ``test Type extension overloads work`` () =
    let c = AnotherClass(3)
    c.Overload("3") |> equal "33"
    c.Overload(3) |> equal 12

[<Fact>]
let ``test Extending different types with same name and same method works`` () =
    AnotherClass(5).Value2 |> equal 10
    NestedModule.AnotherClass(5).Value2 |> equal 40

// -- Pattern Matching --

#nowarn "26"

[<Fact>]
let ``test Pattern matching optimization works (switch statement)`` () =
    let mutable x = ""
    let i = 4
    match i with
    | 1 -> x <- "1"
    | 2 -> x <- "2"
    | 3 | 4 -> x <- "3"
    | 4 -> x <- "4"
    | _ -> x <- "?"
    equal "3" x

    match "Bye" with
    | "Hi" -> x <- "Bye"
    | "Bye" -> let h = "Hi" in x <- sprintf "%s there!" h
    | _ -> x <- "?"
    equal "Hi there!" x

    match 2L with
    | 1L -> x <- "1L"
    | 2L -> x <- "2L"
    | _ -> x <- "?"
    equal "2L" x

    match false with
    | true -> x <- "True"
    | false -> x <- "False"
    equal "False" x

    match MyEnum.One with
    | MyEnum.One -> x <- "One"
    | MyEnum.Two -> x <- "Two"
    | _ -> failwith "never"
    equal "One" x

[<Fact>]
let ``test Pattern matching optimization works (switch expression)`` () =
    let i = 4
    match i with
    | 1 -> "1"
    | 2 -> "2"
    | 3 | 4 -> "3"
    | _ -> "?"
    |> equal "3"

    match "Bye" with
    | "Hi" -> "Bye"
    | "Bye" -> let h = "Hi" in sprintf "%s there!" h
    | _ -> "?"
    |> equal "Hi there!"

    match MyEnum.One with
    | MyEnum.One -> "One"
    | MyEnum.Two -> "Two"
    | _ -> failwith "never"
    |> equal "One"

[<Fact>]
let ``test Pattern matching with same result for last pattern and wildcard works`` () =
    let x = 10
    let y =
        match x with
        | 1
        | 2
        | 3
        | 4
        | _ ->
            sideEffect()
            5
    equal 5 y

[<Fact>]
let ``test Multiple active pattern calls work`` () =
    match " Hello ", " Bye " with
    | NonEmpty "Hello", NonEmpty "Bye" -> true
    | _ -> false
    |> equal true

// -- Dispose / use --

[<Fact>]
let ``test use doesn't return on finally clause`` () =
    let foo() =
        use c = new DisposableFoo()
        c.Foo()
    foo() |> equal 5

[<Fact>]
let ``test use calls Dispose at the end of the scope`` () =
    let cell = ref 0
    let res =
        use c = new DisposableBar(cell)
        cell.Value
    res |> equal 10
    cell.Value |> equal 20

[<Fact>]
let ``test use calls Dispose of an object expression at the end of the scope`` () =
    let cell = ref 0
    let res =
        use c = createCellDisposable cell
        cell.Value
    res |> equal 10
    cell.Value |> equal 20

// -- Exception Handling --

[<Fact>]
let ``test Type of if-then-else expression is correctly determined when then branch throws`` () =
    let f () =
        if false then failwith "error" else 7
    f () |> equal 7

[<Fact>]
let ``test Type of if-then-else expression is correctly determined when else branch throws`` () =
    let f () =
        if true then 7 else failwith "error"
    f () |> equal 7

[<Fact>]
let ``test Type of try-with expression is correctly determined when try block throws`` () =
    let f () =
        try failwith "error" with | _ -> 7
    f () |> equal 7

[<Fact>]
let ``test Type of try-with expression is correctly determined when exception handler throws`` () =
    let f () =
        try 7 with | _ -> failwith "error"
    f () |> equal 7

[<Fact>]
let ``test try-with catches specific exception type`` () =
    let f () =
        try
            raise (MyCustomException "test error")
            0
        with
        | :? MyCustomException as ex ->
            match ex.Data0 with
            | "test error" -> 42
            | _ -> -1
    f () |> equal 42

[<Fact>]
let ``test try-with catches multiple specific exception types`` () =
    let f exnType =
        try
            match exnType with
            | 1 -> raise (MyCustomException "custom")
            | 2 -> raise (MyArgumentException "arg")
            | _ -> failwith "generic"
            0
        with
        | :? MyCustomException -> 1
        | :? MyArgumentException -> 2
        | _ -> 3
    f 1 |> equal 1
    f 2 |> equal 2
    f 3 |> equal 3

[<Fact>]
let ``test try-with with unmatched exception type reraises`` () =
    let mutable caught = ""
    try
        try
            raise (MyArgumentException "inner")
        with
        | :? MyCustomException -> caught <- "custom"
    with
    | :? MyArgumentException -> caught <- "arg"
    | _ -> caught <- "other"
    caught |> equal "arg"

// -- General / Misc --

[<Fact>]
let ``test for downto works`` () =
    let mutable x = ""
    for i = 1 to 5 do
        x <- x + (string i)
    equal "12345" x
    let mutable y = ""
    for i = 5 downto 1 do
        y <- y + (string i)
    equal "54321" y

[<Fact>]
let ``test Values of autogenerated functions are not replaced by optimizations`` () =
    let model = Some (5, 5)
    let model1 = setCellBuggy model
    let model2 = setCell model
    model1 = model2 |> equal true

[<Fact>]
let ``test Module members and properties with same name don't clash`` () =
    StyleBuilderHelper.test() |> equal true

[<Fact>]
let ``test Accessing members of parent module with same name works`` () =
    equal 5 Same.Same.shouldEqual5

[<Fact>]
let ``test Accessing members of child module with same name works`` () =
    equal 10 Same.Same.shouldEqual10

[<Fact>]
let ``test Accessing members with same name as module works`` () =
    equal 20 Same.Same.shouldEqual20

[<Fact>]
let ``test Naming values with same name as module works`` () =
    equal 30 Same.Same.shouldEqual30

[<Fact>]
let ``test Public members of private modules can be accessed`` () =
    MyPrivateModule.publicFoo() |> equal "foo bar"

[<Fact>]
let ``test Public types of private modules can be accessed`` () =
    let thing = MyPrivateModule.Concrete() :> IInterface
    thing.Member "World" "Hello" |> equal "Hello World"

[<Fact>]
let ``test While with isNone doesn't hang with Some unit`` () =
    Trampoline.run (fun _ -> Trampoline.Break "hello") () |> ignore
    Trampoline.run (fun _ -> Trampoline.Break 42) () |> ignore
    Trampoline.run (fun _ -> Trampoline.Break ()) () |> ignore

[<Fact>]
let ``test Ignore shouldn't return value`` () =
    let producer () = 7
    equal (box ()) (box(ignore(producer())))

[<Fact>]
let ``test FSharpRef can be used in properties`` () =
    let r = ref false
    let x = TestRef r
    r.Value <- true
    match x with TestRef r2 -> r2.Value
    |> equal true

[<Fact>]
let ``test Assigning to unit works`` () =
    let mutable value = 2
    let inline doit x (f: 'A -> 'T) =
        let mutable r = Unchecked.defaultof<_>
        r <- f x
        r

    doit "a" (fun a -> a + "b")
    |> equal "ab"

    doit 2 (fun x -> value <- value + x)
    value |> equal 4

[<Fact>]
let ``test Mutating variables is not postponed (functions)`` () =
    let runCase (l: int) (r: int) (expect: int) =
        let mutable left = 0
        let call() =
            left <- l
            r

        let run() =
            let right = call()
            left + right

        run() |> equal expect

    for (l, r, ``l + r``) in ``inlineData PR #2683`` do
        runCase l r ``l + r``

[<Fact>]
let ``test Mutating variables is not postponed (classes)`` () =
    let runCase (l: int) (r: int) (expect: int) =
        InliningMutationTest(l, r).Run() |> equal expect
        InliningMutationTest(l, r).Run() |> equal expect

    for (l, r, ``l + r``) in ``inlineData PR #2683`` do
        runCase l r ``l + r``

#nowarn "3370"
[<Fact>]
let ``test incr and decr works`` () =
    let value = ref 42
    let f x =
        incr x
        x.Value

    let g x =
        decr x
        x.Value

    f value |> equal 43
    g value |> equal 42

[<Fact>]
let ``test lambdas returning member expression accessing anon record work`` () =
    let x = inlineLambdaWithAnonRecord (fun x -> x.A)
    x() |> equal 1

// TODO: Mutable record fields — mutation not working correctly
// [<Fact>]
// let ``test Mutable fields can be passed by reference`` () =
//     let a = 1
//     let foo: MutableFoo = { x = 2 }
//     foo.x <- foo.x + a
//     foo.x |> equal 3
//     foo.x <- 1
//     foo.x |> equal 1

// --- Unchecked.defaultof tests ---

[<Fact>]
let ``test Unchecked.defaultof works`` () =
    Unchecked.defaultof<int> |> equal 0
    Unchecked.defaultof<int64> |> equal 0L
    Unchecked.defaultof<bool> |> equal false

// TODO: Unchecked.defaultof<string> returns empty binary on Beam, not null/undefined
// Unchecked.defaultof<string> |> equal null

// TODO: Unchecked.defaultof for struct tuples not yet supported on Beam
// [<Fact>]
// let ``test Unchecked.defaultof works with tuples`` () =
//     let x: struct (int*int) = Unchecked.defaultof<_>
//     equal (struct (0, 0)) x

// --- Internal members tests ---

module Internal =
    let internal add x y = x + y

    type internal MyType =
        static member Subtract x y = x - y
        static member Add(?x: int, ?y: int) =
            let x = defaultArg x 20
            let y = defaultArg y 50
            x + y

[<Fact>]
let ``test Internal members can be accessed from other modules`` () =
    Internal.add 3 2 |> equal 5

[<Fact>]
let ``test Internal types can be accessed from other modules`` () =
    Internal.MyType.Subtract 3 2 |> equal 1

// --- Binding/shadowing tests ---

let f8 a b = a + b
let mutable topA = 10

module B =
    let c = topA
    do topA <- topA + 5
    let mutable a = 20
    let d = f8 2 2
    let f8 a b = a - b

    module D =
        let d = a
        do a <- a + 5
        let e = f8 2 2

[<Fact>]
let ``test Binding doesn't shadow top-level values`` () =
    equal 10 B.c
    equal 20 B.D.d

[<Fact>]
let ``test Binding doesn't shadow top-level functions`` () =
    equal 4 B.d
    equal 0 B.D.e

// TODO: Module-level `do` side effects on mutable values don't execute during Erlang module load
// [<Fact>]
// let ``test Setting a top-level value doesn't alter values at same level`` () =
//     equal 15 topA
//     equal 25 B.a

// TODO: Recursive value bindings use Lazy internally, which is not yet supported by Fable Beam
// let mutable recMutableValue = 0
// let rec recursive1 = delay (fun () -> recursive2())
// and recursive2 =
//     recMutableValue <- 5
//     fun () -> recMutableValue <- recMutableValue * 2
// [<Fact>]
// let ``test Recursive values work`` () =
//     recMutableValue |> equal 5
//     recursive1()
//     recMutableValue |> equal 10

// --- More tests ported from Python ---

[<Fact>]
let ``test Units of measure work`` () =
    3<km/h> + 2<km/h> |> equal 5<km/h>

[<Fact>]
let ``test Units of measure work with vectors`` () =
    let v1 = { x = 4.3<mi>; y = 5.<mi>; z = 2.8<mi> }
    let v2 = { x = 5.6<mi>; y = 3.8<mi>; z = 0.<mi> }
    let v3 = v1 + v2
    equal 8.8<mi> v3.y

[<Fact>]
let ``test Units of measure work with longs`` () =
    3L<km/h> + 2L<km/h> |> equal 5L<km/h>

[<Fact>]
let ``test Abbreviated measures work`` () =
    let x = 5.<Measure1>
    let c = MeasureTest()
    c.Method(5.<Measure2>) |> equal x

[<Fact>]
let ``test Units of measure work with decimals`` () =
    3M<km/h> + 2M<km/h> |> equal 5M<km/h>

// --- Records as map keys ---

type RecordKey = {
    a : int
    b : int
    c : int
    d : int
    e : int
}

[<Fact>]
let ``test Records are hashable`` () =
    let key1 = {a = 1; b = 2; c = 3; d = 4; e = 5}
    let key2 = {a = 5; b = 4; c = 3; d = 2; e = 1}
    let map =
        Map.empty
        |> Map.add key1 10
        |> Map.add key2 20
    map.[key1] |> equal 10
    map.[key2] |> equal 20

// --- Generic and inline tests ---

let empty<'a> = [Unchecked.defaultof<'a>]

[<Fact>]
let ``test Module generic methods without arguments work`` () =
    let li = empty<string>
    Seq.length li |> equal 1

let inline inlineToString (f: 'T -> string): 'T -> string =
    let unused = f
    fun a -> $"{a}"

[<Fact>]
let ``test Generic unit args work`` () =
    let to_str = inlineToString (fun (props: unit) -> "s")
    to_str () |> equal $"{()}"

// --- Don't inline values that evaluate multiple times ---

module FooModule =
    let mutable genericValueBackend = 0
    let inline genericValue<'T> = genericValueBackend <- genericValueBackend + 1; 5
    let add x y = x + y

// TODO: Inline value side effects don't trigger in Beam (genericValueBackend stays 0)
// [<Fact>]
// let ``test Don't inline values that evaluate multiple times`` () =
//     let li = [1;2;3;4]
//     let res = List.map (FooModule.add FooModule.genericValue) li
//     equal 1 FooModule.genericValueBackend
//     equal [6;7;8;9] res

// --- Optional arguments ---

[<Fact>]
let ``test Removing optional arguments not in tail position works`` () =
    Internal.MyType.Add(y=6) |> equal 26
