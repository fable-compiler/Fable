[<Util.Testing.TestFixture>]
module Fable.Tests.Misc

open System
open Util.Testing
open Fable.Tests.Util

[<Test>]
let ``for .. downto works``() = // See #411
    let mutable x = ""
    for i = 1 to 5 do
        x <- x + (string i)
    equal "12345" x
    let mutable y = ""
    for i = 5 downto 1 do
        y <- y + (string i)
    equal "54321" y

type Base() =
    let mutable x = 5
    member this.Mutate i = x <- x + i
    member __.Value = x

type Test(i) as myself =
    inherit Base()
    let mutable y = 12
    do myself.Mutate(i+2)
    do myself.Mutate2(i)
    member this.Mutate2 i = y <- y + i
    member __.Value2 = y
    member __.Foo() = myself.Value * 2

[<Test>]
let ``Self references in constructors work``() = // See #124
    let t = Test(5)
    equal 12 t.Value
    equal 17 t.Value2

[<Test>]
let ``Using self identifier from class definition in members works``() = // See #124
    let t = Test(5)
    t.Foo() |> equal 24

let log (a: string) (b: string) = String.Format("a = {0}, b = {1}", a, b)
let logItem1 = log "item1"
let logItem2 = log "item2"

type PartialFunctions() =
    member x.logItem1 = log "item1"
    member x.logItem2 = log "item2"

[<Test>]
let ``Module members from partial functions work``() = // See #115
    logItem1 "item1" |> equal "a = item1, b = item1"

[<Test>]
let ``Class members from partial functions work``() = // See #115
    let x = PartialFunctions()
    x.logItem1 "item1" |> equal "a = item1, b = item1"

[<Test>]
let ``Local values from partial functions work``() = // See #115
    let logItem1 = log "item1"
    let logItem2 = log "item2"
    logItem1 "item1" |> equal "a = item1, b = item1"

type MaybeBuilder() =
  member __.Bind(x,f) = Option.bind f x
  member __.Return v = Some v
  member __.ReturnFrom o = o
let maybe = MaybeBuilder()

let riskyOp x y =
  if x + y < 100 then Some(x+y) else None

let execMaybe a = maybe {
  let! b = riskyOp a (a+1)
  let! c = riskyOp b (b+1)
  return c
}

[<Test>]
let ``Custom computation expressions work``() =
    execMaybe 5 |> equal (Some 23)
    execMaybe 99 |> equal None

[<Measure>] type km           // Define the measure units
[<Measure>] type mi           // as simple types decorated
[<Measure>] type h            // with Measure attribute

// Can be used in a generic way
type Vector3D<[<Measure>] 'u> =
    { x: float<'u>; y: float<'u>; z: float<'u> }
    static member (+) (v1: Vector3D<'u>, v2: Vector3D<'u>) =
        { x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z }

[<Test>]
let ``Units of measure work``() =
    3<km/h> + 2<km/h> |> equal 5<km/h>

    let v1 = { x = 4.3<mi>; y = 5.<mi>; z = 2.8<mi> }
    let v2 = { x = 5.6<mi>; y = 3.8<mi>; z = 0.<mi> }
    let v3 = v1 + v2
    equal 8.8<mi> v3.y

type PointWithCounter(a: int, b: int) =
    // A variable i.
    let mutable i = 0
    // A let binding that uses a pattern.
    let (x, y) = (a, b)
    // A private function binding.
    let privateFunction x y = x * x + 2*y
    // A static let binding.
    static let mutable count = 0
    // A do binding.
    do count <- count + 1
    member this.Prop1 = x
    member this.Prop2 = y
    member this.CreatedCount = count
    member this.FunctionValue = privateFunction x y

[<Test>]
let ``Static constructors work``() =
    let point1 = PointWithCounter(10, 52)
    sprintf "%d %d %d %d" (point1.Prop1) (point1.Prop2) (point1.CreatedCount) (point1.FunctionValue)
    |> equal "10 52 1 204"
    let point2 = PointWithCounter(20, 99)
    sprintf "%d %d %d %d" (point2.Prop1) (point2.Prop2) (point2.CreatedCount) (point2.FunctionValue)
    |> equal "20 99 2 598"

[<Test>]
let ``File with single type in namespace compiles``() =
    equal SingleTypeInNamespace.Hello "Hello"

[<Test>]
let ``Type abbreviation in namespace compiles``() = // See #140
    let h = Util2.H(5)
    equal "5" h.Value

let inline f x y = x + y

[<Test>]
let ``Inline methods work``() =
    f 2 3 |> equal 5

module FooModule =
    type FooInline() =
        member __.Bar = "Bar"
        member inline self.Foo = "Foo" + self.Bar
        member inline self.Foofy(i) = String.replicate i self.Bar

open FooModule

[<Test>]
let ``Inline methods with this argument work``() = // See #638
    let x = FooInline()
    x.Foo |> equal "FooBar"
    x.Foofy 4 |> equal "BarBarBarBar"

type FooInline with
    member inline self.Bar2 = "Bar" + self.Bar
    member inline self.FoofyPlus(i) = self.Foofy(i * 2)

[<Test>]
let ``Inline extension methods with this argument work``() = // See #638
    let x = FooInline()
    x.Bar2 |> equal "BarBar"
    x.FoofyPlus 3 |> equal "BarBarBarBarBarBar"

[<Test>]
let ``Calls to core lib from a subfolder work``() =
    Util2.Helper.Format("{0} + {0} = {1}", 2, 4)
    |> equal "2 + 2 = 4"

let f1 x y z = x + y + z
let f2 x = x + x

let f3 () = 5

[<Test>]
let ``Conversion to delegate works``() =
    (System.Func<_,_,_,_> f1).Invoke(1,2,3) |> equal 6

    let f = f1
    (System.Func<_,_,_,_> f).Invoke(1,2,3) |> equal 6

    let del = System.Func<_,_,_,_>(fun x y z -> x + y + z)
    del.Invoke(1,2,3) |> equal 6

    (System.Func<_,_> f2).Invoke(2) |> equal 4

[<Test>]
let ``Conversion to Func<_> works``() =
    (System.Func<_> f3).Invoke() |> equal 5
    let f = Func<_>(fun () -> 6)
    f.Invoke() |> equal 6

open Microsoft.FSharp.Core.OptimizedClosures

[<Test>]
let ``Conversion to FSharpFunc<_,_,_> works``() =
    let f x y = x + y
    let f = FSharpFunc<_,_,_>.Adapt(f)
    f.Invoke(1, 2) |> equal 3

[<Test>]
let ``Conversion to FSharpFunc<_,_,_,_> works``() =
    let f x y z = x + y + z
    let f = FSharpFunc<_,_,_,_>.Adapt(f)
    f.Invoke(1, 2, 3) |> equal 6

let mutable myMutableField = 0

let f4 i = myMutableField <- i
let f5 () = myMutableField <- 5
let f6 i j = myMutableField <- i * j
let f7 i () = myMutableField <- i * 3

[<Test>]
let ``Conversion to Action<_> works``() =
    let f1' = Action<int>(fun i -> myMutableField <- i * 2)
    let f2' = Action<int>(f4)
    let f3' = Action<_>(f6 4)
    f1'.Invoke(1)
    equal 2 myMutableField
    f2'.Invoke(8)
    equal 8 myMutableField
    f3'.Invoke(10)
    equal 40 myMutableField

[<Test>]
let ``Conversion to Action works``() =
    let f4' = Action(fun () -> myMutableField <- 7)
    let f5' = Action(f5)
    let f6' = Action(f7 3)
    let f7' i () = myMutableField <- i * 3
    let f8' = Action(f7' 3)
    f4'.Invoke()
    equal 7 myMutableField
    f5'.Invoke()
    equal 5 myMutableField
    f6'.Invoke()
    equal 9 myMutableField
    f8'.Invoke()
    equal 9 myMutableField

let (|NonEmpty|_|) (s: string) =
    match s.Trim() with "" -> None | s -> Some s

[<Test>]
let ``Multiple active pattern calls work``() =
    match " Hello ", " Bye " with
    | NonEmpty "Hello", NonEmpty "Bye" -> true
    | _ -> false
    |> equal true

open System
type IFoo =
   abstract Bar: s: string * [<ParamArray>] rest: obj[] -> string

[<Test>]
let ``ParamArray in object expression works``() =
   let o = { new IFoo with member x.Bar(s: string, [<ParamArray>] rest: obj[]) = String.Format(s, rest) }
   o.Bar("{0} + {0} = {1}", 2, 4)
   |> equal "2 + 2 = 4"

type IFoo2 =
    abstract Value: int with get, set
    abstract Test: int -> int
    abstract MakeFoo: unit -> IFoo

type Foo(i) =
    let mutable j = 5
    member x.Value = i + j
    member x.MakeFoo2() = {
        new IFoo2 with
        member x2.Value
            with get() = x.Value * 2
            and set(i) = j <- j + i
        member x2.Test(i) = x2.Value - i
        member x2.MakeFoo() = {
            new IFoo with
            member x3.Bar(s: string, [<ParamArray>] rest: obj[]) =
                sprintf "%s: %i %i %i" s x.Value x2.Value j
        }
    }

[<Test>]
let ``Object expression can reference enclosing type and self``() = // See #158
    let f = Foo(5)
    let f2 = f.MakeFoo2()
    f2.Value <- 2
    f.Value |> equal 12
    f2.Test(2) |> equal 22

[<Test>]
let ``Nested object expressions work``() = // See #158
    let f = Foo(5)
    let f2 = f.MakeFoo2()
    f2.MakeFoo().Bar("Numbers") |> equal "Numbers: 10 20 5"

type IRenderer =
  abstract member doWork: unit -> string

type MyComponent(name) as self =
  let work i = sprintf "%s-%i" name i
  let create2 () = { new IRenderer with member __.doWork () = work 2 }
  let create3 = { new IRenderer with member __.doWork () = work 3 }
  let create4 = { new IRenderer with member __.doWork () = self.Work 4 }
  let create5() = { new IRenderer with member __.doWork () = self.Work 5 }
  member __.Work i = work i
  member __.works1 () = { new IRenderer with member __.doWork () = work 1 }
  member __.works2 () = create2()
  member __.works3 () = create3
  member __.works4 () = create4
  member __.works5 () = create5()

[<Test>]
let ``References to enclosing type from object expression work``() = // See #438
    MyComponent("TestA").works1().doWork() |> equal "TestA-1"
    MyComponent("TestB").works2().doWork() |> equal "TestB-2"
    MyComponent("TestC").works3().doWork() |> equal "TestC-3"
    MyComponent("TestD").works4().doWork() |> equal "TestD-4"
    MyComponent("TestE").works5().doWork() |> equal "TestE-5"

type IFoo3 =
   abstract Bar: int with get, set

[<Test>]
let ``Properties in object expression work``() =
    let mutable backend = 0
    let o = { new IFoo3 with member x.Bar with get() = backend and set(v) = backend <- v }
    o.Bar |> equal 0
    backend <- 5
    o.Bar |> equal 5
    o.Bar <- 10
    o.Bar |> equal 10

type SomeClass(name: string) =
    member x.Name = name

type AnotherClass(value: int) =
    member x.Value = value

[<Test>]
let ``Object expression from class works``() =
    let o = { new SomeClass("World") with member x.ToString() = sprintf "Hello %s" x.Name }
    match box o with
    | :? SomeClass as c -> c.ToString()
    | _ -> "Unknown"
    |> equal "Hello World"

type RecursiveType(subscribe) as this =
    let getNumber() = 3
    do subscribe (getNumber >> this.Add2)
    member this.Add2(i) = i + 2

[<Test>]
let ``Composition with recursive `this` works``() =
    let mutable x = 0
    RecursiveType(fun f -> x <- f()) |> ignore
    equal 5 x

module Extensions =
    type IDisposable with
        static member Create(f) =
            { new IDisposable with
                member __.Dispose() = f() }

    type SomeClass with
        member x.FullName = sprintf "%s Smith" x.Name
        member x.NameTimes (i: int, j: int) = String.replicate (i + j) x.Name

    type AnotherClass with
        member x.FullName = sprintf "%i" x.Value
        member x.Overload(i: int) = i * 4
        member x.Overload(s: string) = s + s

open Extensions

[<Test>]
let ``Type extension static methods work``() =
    let disposed = ref false
    let disp = IDisposable.Create(fun () -> disposed := true)
    disp.Dispose ()
    equal true !disposed

[<Test>]
let ``Type extension properties work``() =
    let c = SomeClass("John")
    equal "John Smith" c.FullName

[<Test>]
let ``Type extension methods work``() =
    let c = SomeClass("John")
    c.NameTimes(1,2) |> equal "JohnJohnJohn"

[<Test>]
let ``Type extension methods with same name work``() =
    let c = AnotherClass(3)
    equal "3" c.FullName

[<Test>]
let ``Type extension overloads work``() =
    let c = AnotherClass(3)
    c.Overload("3") |> equal "33"
    c.Overload(3) |> equal 12

module StyleBuilderHelper =
    type StyleBuilderHelper = { TopOffset : int; BottomOffset : int }
    type DomBuilder = { ElementType : string; StyleBuilderHelper : StyleBuilderHelper }
    let test() =
        let helper = { TopOffset = 1; BottomOffset = 2 }
        let builder1 = { ElementType = "test"; StyleBuilderHelper = helper }
        let builder2 = { builder1 with StyleBuilderHelper =  { builder1.StyleBuilderHelper with BottomOffset = 3 } }
        match builder1, builder2 with
        | { StyleBuilderHelper = { BottomOffset = 2 } },
          { StyleBuilderHelper = { TopOffset = 1; BottomOffset = 3 } } -> true
        | _ -> false

[<Test>]
let ``Module, members and properties with same name don't clash``() =
    StyleBuilderHelper.test() |> equal true

module Mutable =
    let mutable prop = 10

[<Test>]
let ``Module mutable properties work``() =
    equal 10 Mutable.prop
    Mutable.prop <- 5
    equal 5 Mutable.prop

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

[<Test>]
let ``Accessing members of parent module with same name works``() =
    equal 5 Same.Same.shouldEqual5

[<Test>]
let ``Accessing members of child module with same name works``() =
    equal 10 Same.Same.shouldEqual10

[<Test>]
let ``Accessing members with same name as module works``() =
    equal 20 Same.Same.shouldEqual20

[<Test>]
let ``Naming values with same name as module works``() =
    equal 30 Same.Same.shouldEqual30

[<Test>]
let ``Can access nested recursive function with mangled name``() =
    Util.Bar.nestedRecursive 3 |> equal 10

[<Test>]
let ``Can access non nested recursive function with mangled name``() =
    Util.nonNestedRecursive "ja" |> equal "jajaja"

[<Test>]
let ``Module members don't conflict with JS names``() =
    Util.Int32Array |> Array.sum |> equal 3

[<Test>]
let ``Modules don't conflict with JS names``() =
    Util.Float64Array.Float64Array |> Array.sum |> equal 7.

let f8 a b = a + b
let mutable a = 10

module B =
  let c = a
  a <- a + 5
  let mutable a = 20
  let d = f8 2 2
  let f8 a b = a - b

  module D =
    let d = a
    a <- a + 5
    let e = f8 2 2

// Modules and TestFixtures bind values in a slightly different way
// Test both cases
[<Test>]
let ``Binding doesn't shadow top-level values``() = // See #130
    equal 10 Util.B.c
    equal 20 Util.B.D.d

[<Test>]
let ``Binding doesn't shadow top-level values (TestFixture)``() = // See #130
    equal 10 B.c
    equal 20 B.D.d

[<Test>]
let ``Binding doesn't shadow top-level functions``() = // See #130
    equal 4 Util.B.d
    equal 0 Util.B.D.e

[<Test>]
let ``Binding doesn't shadow top-level functions (TestFixture)``() = // See #130
    equal 4 B.d
    equal 0 B.D.e

[<Test>]
let ``Setting a top-level value doesn't alter values at same level``() = // See #130
    equal 15 Util.a
    equal 25 Util.B.a

[<Test>]
let ``Setting a top-level value doesn't alter values at same level (TestFixture)``() = // See #130
    equal 15 a
    equal 25 B.a

module Internal =
    let internal add x y = x + y

    type internal MyType =
        static member Subtract x y = x - y

[<Test>]
let ``Internal members can be accessed from other modules``() = // See #163
    Internal.add 3 2 |> equal 5

[<Test>]
let ``Internal types can be accessed from other modules``() = // See #163
    Internal.MyType.Subtract 3 2 |> equal 1

type Point =
    { x: float; y: float }
    static member (+) (p1: Point, p2: Point) = { x=p1.x + p2.x; y=p1.y + p2.y }
    static member (-) (p1: Point, p2: Point) = { x=p1.x - p2.x; y=p1.y - p2.y }
    static member inline (*) (p1: Point, p2: Point) = { x=p1.x * p2.x; y=p1.y * p2.y }

[<Test>]
let ``Custom operators with types work``(): unit =
    let p1 = { x=5.; y=10. }
    let p2 = { x=2.; y=1. }
    equal 7. (p1 + p2).x
    equal 9. (p1 - p2).y

[<Test>]
let ``Inline custom operators with types work``(): unit = // See #230
    let p1 = { x=5.; y=10. }
    let p2 = { x=2.; y=1. }
    equal 10. (p1 * p2).x

let inline genericAdd (x: ^a) (y: ^b): ^c = x + y

type MyRecord =
    { value: int }
    static member (+) (x: MyRecord, y: int) = { value = x.value + y }
    static member (+) (x: int, y: MyRecord) = x + y.value + 2

[<Test>]
let ``Overloads of a custom operators work``(): unit =
    let x = { value = 5 }
    x + 2 |> equal { value = 7 }
    3 + x |> equal 10

[<Test>]
let ``Overloads of a custom operators can be inlined``(): unit =
    let x = { value = 5 }
    genericAdd 4 5 |> equal 9
    genericAdd x 2 |> equal { value = 7 }
    genericAdd 3 x |> equal 10

let (+) x y = x * y

let (-) x y = x / y

let (||||) x y = x + y

let inline (>>) x y = x * y * 2

[<Test>]
let ``Custom operators work``() =
    5 + 5 |> equal 25
    10 - 2 |> equal 5
    2 |||| 2 |> equal 4

[<Test>]
let ``Inline custom operators work``() =
    5 >> 5 |> equal 50

[<Test>]
let ``defaultArg works``() =
    let f o = defaultArg o 5
    f (Some 2) |> equal 2
    f None |> equal 5


// In F# both branches of if-then-else has the same type,
// but this is not always true in Fable AST. For example when
// one branch is a Throw expression, it has always type Unit.
// So we should test that the type of the whole expression is not determined
// by the throw expression in one of its branches.
//
// The same applies to try-with expression.

[<Test>]
let ``Type of if-then-else expression is correctly determined when 'then' branch throws``() =
    let f () =
        if false then failwith "error" else 7

    f () |> equal 7

[<Test>]
let ``Type of if-then-else expression is correctly determined when 'else' branch throws``() =
    let f () =
        if true then 7 else failwith "error"

    f () |> equal 7

[<Test>]
let ``Type of try-with expression is correctly determined when 'try' block throws`` () =
    let f () =
        try failwith "error" with | _ -> 7

    f () |> equal 7

[<Test>]
let ``Type of try-with expression is correctly determined when exception handler throws`` () =
    let f () =
        try 7 with | _ -> failwith "error"

    f () |> equal 7

type DisposableFoo() =
    member __.Foo() = 5
    interface IDisposable with
        member __.Dispose () = ()

[<Test>]
let ``use doesn't return on finally clause`` () = // See #211
    let foo() =
        use c = new DisposableFoo()
        c.Foo()
    foo() |> equal 5

type DisposableBar(v) =
    do v := 10
    interface IDisposable with
        member __.Dispose () = v := 20

[<Test>]
let ``use calls Dispose at the end of the scope`` () =
    let cell = ref 0
    let res =
        use c = new DisposableBar(cell)
        !cell
    res |> equal 10
    !cell |> equal 20

let createCellDiposable cell =
  cell := 10
  { new System.IDisposable with
      member x.Dispose() = cell := 20 }

[<Test>]
let ``use calls Dispose (of an object expression) at the end of the scope`` () =
    let cell = ref 0
    let res =
        use c = createCellDiposable cell
        !cell
    res |> equal 10
    !cell |> equal 20

[<Test>]
let ``Unchecked.defaultof works`` () =
    Unchecked.defaultof<int> |> equal 0
    Unchecked.defaultof<bool> |> equal false
    Unchecked.defaultof<string> |> equal null

type MyEnum =
    | One = 1
    | Two = 2

[<Test>]
let ``Pattern matching optimization works (switch statement)``() =
    let mutable x = ""
    let i = 4
    match i with
    | 1 -> x <- "1"
    | 2 -> x <- "2"
    | 3 | 4 -> x <- "3" // Multiple cases are allowed
    // | 5 | 6 as j -> x <- string j // This prevents the optimization
    | 4 -> x <- "4" // Unreachable cases are removed
    | _ -> x <- "?"
    equal "3" x

    match "Bye" with
    | "Hi" -> x <- "Bye"
    | "Bye" -> let h = "Hi" in x <- sprintf "%s there!" h
    | _ -> x <- "?"
    equal "Hi there!" x

    // Pattern matching with Int64/UInt64 is not converted to switch
    match 2L with
    | 1L -> x <- "1L"
    | 2L -> x <- "2L"
    | _ -> x <- "?"
    equal "2L" x

    // Pattern matching with boolean is not converted to switch
    match false with
    | true -> x <- "True"
    | false -> x <- "False"
    equal "False" x

    match MyEnum.One with
    | MyEnum.One -> x <- "One"
    | MyEnum.Two -> x <- "Two"
    | _ -> failwith "never"
    equal "One" x

[<Test>]
let ``Pattern matching optimization works (switch expression)``() =
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

type TestRef = TestRef of bool ref

[<Test>]
let ``FSharpRef can be used in properties``() = // See #521
    let r = ref false
    let x = TestRef r
    r := true
    match x with TestRef r2 -> !r2
    |> equal true

let delay (f:unit -> unit) = f

let mutable mutableValue = 0

let rec recursive1 = delay (fun () -> recursive2())
and recursive2 =
    mutableValue <- 5
    fun () -> mutableValue <- mutableValue * 2

[<Test>]
let ``Recursive values work``() = // See #237
    mutableValue |> equal 5
    recursive1()
    mutableValue |> equal 10

let empty<'a> = [Unchecked.defaultof<'a>]

[<Test>]
let ``Module generic methods without arguments work``() =
    let li = empty<string>
    Seq.length li |> equal 1

type IInterface =
  abstract member Member : thing1:string -> thing2:string -> string

module private MyPrivateModule =
    let private bar = "bar"
    let publicFoo() = sprintf "foo %s" bar
    type Concrete() =
        interface IInterface with
            member this.Member (thing1: string) (thing2: string) =
                sprintf "%s %s" thing2 thing1

[<Test>]
let ``Public members of private modules can be accessed``() = // See #696
    MyPrivateModule.publicFoo() |> equal "foo bar"

[<Test>]
let ``Public types of private modules can be accessed``() = // See #841
    let thing = MyPrivateModule.Concrete() :> IInterface
    thing.Member "World" "Hello" |> equal "Hello World"

[<Test>]
let ``Types declared in signature file work``() = // See #754
    let t = Spaces.TRec.Create("haha", "hoho")
    t.Value |> equal "hahahoho"

[<Test>]
let ``Two types with same name in different folders work``() = // See #781
    tempet.SayA.hello "Albert"
    |> equal "Hello Albert from SayA"
    tempet.SayB.hello "Albert"
    |> equal "Hello Albert from SayB"

#if FABLE_COMPILER
[<Test>]
let ``System.Environment.NewLine works``() =
      System.Environment.NewLine
      |> equal "\n"
#endif

[<Test>]
let ``Option.defaultValue works``() =
    let a = Some "MyValue"
    let b = None

    a |> Option.defaultValue "" |> equal "MyValue"
    b |> Option.defaultValue "default" |> equal "default"

[<Test>]
let ``System.Uri.UnescapeDataString works``() =
    System.Uri.UnescapeDataString("Kevin%20van%20Zonneveld%21") |> equal "Kevin van Zonneveld!"
    System.Uri.UnescapeDataString("http%3A%2F%2Fkvz.io%2F") |> equal "http://kvz.io/"
    System.Uri.UnescapeDataString("http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a")
    |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

[<Test>]
let ``System.Uri.EscapeDataString works``() =
    System.Uri.EscapeDataString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld%21"
    System.Uri.EscapeDataString("http://kvz.io/") |> equal "http%3A%2F%2Fkvz.io%2F"
    System.Uri.EscapeDataString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
    |> equal "http%3A%2F%2Fwww.google.nl%2Fsearch%3Fq%3DLocutus%26ie%3Dutf-8%26oe%3Dutf-8%26aq%3Dt%26rls%3Dcom.ubuntu%3Aen-US%3Aunofficial%26client%3Dfirefox-a"

[<Test>]
let ``System.Uri.EscapeUriString works``() =
    System.Uri.EscapeUriString("Kevin van Zonneveld!") |> equal "Kevin%20van%20Zonneveld!"
    System.Uri.EscapeUriString("http://kvz.io/") |> equal "http://kvz.io/"
    System.Uri.EscapeUriString("http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a")
    |> equal "http://www.google.nl/search?q=Locutus&ie=utf-8&oe=utf-8&aq=t&rls=com.ubuntu:en-US:unofficial&client=firefox-a"

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

[<Test>]
let ``While with isNone doesn't hang with Some ()``() =
    Trampoline.run (fun _ -> Trampoline.Break "hello") () |> ignore
    Trampoline.run (fun _ -> Trampoline.Break 42) () |> ignore
    Trampoline.run (fun _ -> Trampoline.Break ()) () |> ignore