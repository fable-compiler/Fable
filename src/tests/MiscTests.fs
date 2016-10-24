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

let log (a: string) (b: string) = String.Format("a = {0}, b = {0}", a, b)
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

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

[<Test>]
let ``Dynamic application works``() =
    let dynObj =
        createObj [
            "add" ==> Func<_,_,_>(fun x y -> x + y)
            "fn" ==> fun () ->
                createObj [
                    "subtract" ==> Func<_,_,_>(fun x y -> x - y)
                ]
            "child" ==>
                createObj [
                    "multiply" ==> Func<_,_,_>(fun x y -> x * y)
                ]
            "foo" ==> "foo"
            "apply" ==> fun (del: Func<int,int,int>) -> del.Invoke(2,3)
        ]
    dynObj?add(2,2) |> equal (box 4)
    // Assigning dynamic access result to a value
    let add = dynObj?add
    add(3,4) |> equal (box 7)
    // Accessing 2-level deep property
    dynObj?child?multiply(3,2) |> equal (box 6)
    // Dynamic application chaining
    dynObj?fn()?subtract(3,2) |> equal (box 1)
    // Using $ operator
    dynObj?add $ (2,2) |> equal (box 4)
    dynObj?foo |> unbox |> equal "foo"
    // Delegates are not modified when applied dynamically
    let del = Func<_,_,_>(fun x y -> y - x)
    dynObj?apply(del) |> unbox |> equal 1
    dynObj?apply(Func<_,_,_>(fun x y -> x - y)) |> unbox |> equal -1

let myMeth (x: int) (y: int) = x - y

[<Test>]
let ``Lambdas are converted to delegates with dynamic operators``() =
    let o =
        createObj [
            "bar" ==> fun x y z -> x * y * z
            "bar2" ==> myMeth
            "apply2and5" ==> fun (f: Func<int,int,int>) -> f.Invoke(2,5)
        ]
    o?bar(1,2,3) |> unbox<int> |> equal 6
    o?bar2(5,2) |> unbox<int> |> equal 3

    o?apply2and5(fun x y -> x + y) |> unbox<int> |> equal 7

    let f = unbox<obj> o?apply2and5
    f $ (fun x y -> x * y) |> unbox<int> |> equal 10

    o?foo <- fun x y -> x / y
    o?foo(25, 5) |> unbox<int> |> equal 5

#if !DOTNETCORE
[<Test>]
let ``Symbols in external projects work``() =
    equal "Fable Rocks!" Other.Helper.ConditionalExternalValue
#endif

[<KeyValueList>]
type MyOptions =
    | Flag1
    | Name of string
    | [<CompiledName("QTY")>] QTY of int

[<Test>]
let ``KeyValueList attribute works at compile time``() =
    let opts = [
        Name "Fable"
        QTY 5
        Flag1
    ]
    opts?name |> unbox |> equal "Fable"
    opts?QTY |> unbox |> equal 5
    opts?flag1 |> unbox |> equal true

[<Test>]
let ``KeyValueList attribute works at runtime``() =
    let buildAtRuntime = function
        | null | "" -> Flag1
        | name -> Name name
    let opts = [
        buildAtRuntime "Fable"
        QTY 5
        buildAtRuntime ""
    ]
    opts?name |> unbox |> equal "Fable"
    opts?QTY |> unbox |> equal 5
    opts?flag1 |> unbox |> equal true

[<StringEnum>]
type MyStrings =
    | Vertical
    | [<CompiledName("Horizontal")>] Horizontal

[<Test>]
let ``StringEnum attribute works``() =
    Vertical |> unbox |> equal "vertical"
    Horizontal |> unbox |> equal "Horizontal"

[<StringEnum>]
#endif
type Field = OldPassword | NewPassword | ConfirmPassword

let validatePassword = function
    | OldPassword -> "op"
    | NewPassword -> "np"
    | ConfirmPassword -> "cp"

[<Test>]
let ``Pattern matching with StringEnum works``() =
    validatePassword NewPassword
    |> equal "np"

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

[<Test>]
let ``Calls to core lib from a subfolder work``() =
    Util2.Helper.Format("{0} + {0} = {1}", 2, 4)
    |> equal "2 + 2 = 4"

let f' x y z = x + y + z
let f'' x = x + x

[<Test>]
let ``Conversion to delegate works``() =
    (System.Func<_,_,_,_> f').Invoke(1,2,3) |> equal 6

    let f = f'
    (System.Func<_,_,_,_> f).Invoke(1,2,3) |> equal 6

    let del = System.Func<_,_,_,_>(fun x y z -> x + y + z)
    del.Invoke(1,2,3) |> equal 6

    (System.Func<_,_> f'').Invoke(2) |> equal 4

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
let ``Composition with recursive this works``() =
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
let ``Module members don't conflict with JS names``() =
    Util.Int32Array |> Array.sum |> equal 3

[<Test>]
let ``Modules don't conflict with JS names``() =
    Util.Float64Array.Float64Array |> Array.sum |> equal 7.

let f2 a b = a + b
let mutable a = 10

module B =
  let c = a
  a <- a + 5
  let mutable a = 20
  let d = f2 2 2
  let f2 a b = a - b

  module D =
    let d = a
    a <- a + 5
    let e = f2 2 2

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

#if FABLE_COMPILER
[<Test>]
let ``Referencing a Fable project through a dll works``() =
    Fable.Tests.DllRef.Util.add2 5 |> equal 7

open Fable.Tests.DllRef

[<Test>]
let ``Root members with JS non-valid chars work``() = // See #207
    Lib.足す 3 2 |> equal 5
    Lib.引く 3 2 |> equal 1
    Lib.モジュール.ファンクション 0 |> equal false

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

type IFooImported =
    abstract foo: string

[<Import("*", "./js/foo.js")>]
let fooAll: IFooImported = failwith "JS only"

[<Test>]
let ``Import with relative paths works``() =
    fooAll.foo |> equal "foo"
    let fooAll2: IFooImported = importAll "./js/foo.js"
    fooAll2.foo |> equal "foo"

[<Test>]
let ``Import with relative paths from project subdirectory works``() =
    // Import expression
    Fable.Tests.Util.foo |> equal "foo"
    // Import attribute
    Fable.Tests.Util.foo2 |> equal "foo"

[<Test>]
let ``Import with relative paths from external files works``() =
    // Import expressions
    Fable.Tests.Util4.foo |> equal "foo"
    Fable.Tests.Util4.bar |> equal 5
    // Import attribute
    Fable.Tests.Util4.bar2 |> equal 5

[<Test>]
let ``Import with relative paths from referenced dll works``() =
    DllRef.Lib.モジュール.one |> equal 1
    DllRef.Lib.モジュール.three |> equal 3
    DllRef.Util.two |> equal 2
    DllRef.Util.four |> equal 4

[<Test>]
let ``JS accepts any object as exception``() =
    try
        let o = createObj [ "foo" ==> 3 ]
        raise(unbox o)
    with ex -> ex.Message
    |> equal """{"foo":3}"""


[<Test>]
let ``Identifiers are encoded correctly``() = // See #482
    equal "bar1" Lib.``$5EAfoo``
    equal "bar2" Lib.``$5E$Afoo``
    equal "bar3" Lib.``$5EA$foo``
    equal "bar4" Lib.``^Afoo``
    equal "bar5" Lib.``תfoo``
    equal "bar6" Lib.``foo$5EA``
    equal "bar7" Lib.``foo$5E$A``
    equal "bar8" Lib.``foo$5EA$``
    equal "bar9" Lib.``foo^A``
    equal "bar10" Lib.``fooת``


#endif // FABLE_COMPILER
