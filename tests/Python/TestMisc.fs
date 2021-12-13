module Fable.Tests.Misc

open System
open System.Runtime.InteropServices
open Fable.Core
open Util.Testing

let [<Literal>] LITERAL_JSON = """{
    "widget": {
        "debug": true,
        "window": {
            "title": "Sample Konfabulator Widget",
            "name": "main_window",
            "width": 500,
            "height": 500
        },
        "image": {
            "src": "Images/Sun.png",
            "name": "sun1",
            "hOffset": 250,
            "vOffset": 250,
            "alignment": "center"
        },
        "text": {
            "data": "Click Here",
            "size": [{ "width": 36, "height": 40 }],
            "style": "bold",
            "name": "text1",
            "vOffset": 100,
            "alignment": "center",
            "onMouseUp": "sun1.opacity = (sun1.opacity / 100) * 90;"
        }
    }
}"""

let ANOTHER_JSON = """{
    "widget": {
        "debug": false,
        "text": {
            "data": "lots of",
            "size": [{"width": 5}, { "height": 80 }],
            "vOffset": 500
        }
    }
}"""

// We can have aliases with same name in same file #1662
module One =
    type Id = System.Guid

module Two =
    type Id = System.Guid

type Base() =
    let mutable x = 5
    member this.Mutate i = x <- x + i
    member __.Value = x

type MyTest(i) as myself =
    inherit Base()
    let mutable y = 12
    do myself.Mutate(i+2)
    do myself.Mutate2(i)
    member this.Mutate2 i = y <- y + i
    member __.Value2 = y
    member __.Foo() = myself.Value * 2

let log2 (a: string) (b: string) = String.Format("a = {0}, b = {1}", a, b)
let logItem1 = log2 "item1"
let logItem2 = log2 "item2"

type PartialFunctions() =
    member __.logItem1 = log2 "item1"
    member __.logItem2 = log2 "item2"

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

[<Measure>] type km           // Define the measure units
[<Measure>] type mi           // as simple types decorated
[<Measure>] type h            // with Measure attribute

[<Measure>] type Measure1
[<Measure>] type Measure2 = Measure1

type MeasureTest() =
    member _.Method(x: float<Measure2>) = x

// Can be used in a generic way
type Vector3D<[<Measure>] 'u> =
    { x: float<'u>; y: float<'u>; z: float<'u> }
    static member (+) (v1: Vector3D<'u>, v2: Vector3D<'u>) =
        { x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z }

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


let inline f x y = x + y

let inline sum x = fun y -> x + y

module FooModule =
    let mutable genericValueBackend = 0
    let inline genericValue<'T> = genericValueBackend <- genericValueBackend + 1; 5
    let add x y = x + y

    type FooInline() =
        member __.Bar = "Bar"
        member val Value = 0uy with get, set
        member inline self.Foo = "Foo" + self.Bar
        member inline self.Foofy(i) = String.replicate i self.Bar
        member inline this.PropertyInline
            with get() = this.Value
            and set(v: uint8) = this.Value <- v

type FooModule.FooInline with
    member inline self.Bar2 = "Bar" + self.Bar
    member inline self.FoofyPlus(i) = self.Foofy(i * 2)

let counter =
    let mutable i = 0
    fun () ->
        i <- i + 1
        i

type Type = {
    a : int
    b : int
    c : int
    d : int
    e : int
}
  with
    static member New(n) = {
        a = n
        b = n * 2
        c = n * 3
        d = n * 4
        e = counter()  // <== should only be called twice
      }

    member        this.Method  (v:bool) = { this with a = this.a * if v then 2 else 3 }
    member inline this.MethodI (v:bool) = { this with a = this.a * if v then 2 else 3 }
    member        this.Method  ()       = { this with a = this.a * 10 }
    member inline this.MethodI ()       = { this with a = this.a * 10 }

let f1 x y z = x + y + z
let f2 x = x + x

let f3 () = 5

type MyDelegate = Func<int>

let mutable myMutableField = 0

let f4 i = myMutableField <- i
let f5 () = myMutableField <- 5
let f6 i j = myMutableField <- i * j
let f7 i () = myMutableField <- i * 3

type DisposableFoo() =
    member __.Foo() = 5
    interface IDisposable with
        member __.Dispose () = ()

type DisposableBar(v: int ref) =
    do v.Value <- 10
    interface IDisposable with
        member __.Dispose () = v.Value <- 20

let createCellDiposable (cell: int ref) =
  cell.Value <- 10
  { new System.IDisposable with
      member x.Dispose() = cell.Value <- 20 }

let (|NonEmpty|_|) (s: string) =
    match s.Trim() with "" -> None | s -> Some s

type IFoo =
   abstract Bar: s: string * [<ParamArray>] rest: obj[] -> string

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

type IFoo3 =
   abstract Bar: int with get, set

type SomeClass(name: string) =
    member x.Name = name

type AnotherClass(value: int) =
    member x.Value = value

module NestedModule =
    type AnotherClass(value: int) =
        member x.Value = value + 5

type INum = abstract member Num: int
let inline makeNum f = { new INum with member __.Num = f() }

type MyTestClass(n) =
    let addOne x = x + 4
    let inner = makeNum (fun () -> addOne n)
    member __.GetNum() = inner.Num

type RecursiveType(subscribe) as self =
    let foo = 3
    let getNumber() = 3
    do subscribe (getNumber >> self.Add2)
    member __.Add2(i) = self.MultiplyFoo(i) + 2
    member __.MultiplyFoo(i) = i * foo

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
        member x.Value2 = x.Value * 2

    type NestedModule.AnotherClass with
        member x.Value2 = x.Value * 4

    [<AbstractClass>]
    type ObjectExprBase (x: int ref) as this =
        do x.Value <- this.dup x.contents
        abstract member dup: int -> int

open Extensions


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

module Mutable =
    let mutable prop = 10

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

module Internal =
    let internal add x y = x + y

    type internal MyType =
        static member Subtract x y = x - y
        static member Add(?x: int, ?y: int) =
            let x = defaultArg x 20
            let y = defaultArg y 50
            x + y

type MyEnum =
    | One = 1
    | Two = 2

type MyTestRef = TestRef of bool ref

let delay (f:unit -> unit) = f

let mutable mutableValue = 0

let rec recursive1 = delay (fun () -> recursive2())
and recursive2 =
    mutableValue <- 5
    fun () -> mutableValue <- mutableValue * 2

let empty<'a> = [Unchecked.defaultof<'a>]

type IInterface =
  abstract member Member : thing1:string -> thing2:string -> string

type Taster =
    abstract Starter: float
    abstract Taste: quality: float * quantity: float -> int

type Eater =
    abstract Bite: unit -> int

let taste (com: Taster) qlty qty =
    com.Starter * qlty + qty |> int

module private MyPrivateModule =
    let private bar = "bar"
    let publicFoo() = sprintf "foo %s" bar
    type Concrete() =
        interface IInterface with
            member this.Member (thing1: string) (thing2: string) =
                sprintf "%s %s" thing2 thing1

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

let setCellBuggy x: (int*int) option =
    Option.map (fun (x, y) -> max (x - 1) 0, y) x

let setCell x: (int*int) option =
    let max = max
    Option.map (fun (x, y) -> max (x - 1) 0, y) x

type Shape =
    | Circle of int
    | Square of int
    | Rectangle of int * int

type StaticClass =
    static member DefaultParam([<Optional; DefaultParameterValue(true)>] value: bool) = value

    static member inline Add(x: int, ?y: int) =
        x + (defaultArg y 2)

type ValueType =
  struct
    val public X : int
  end

type MutableFoo =
    { mutable x: int }

let incByRef (a: int) (b: byref<int>) = b <- a + b
let addInRef (a: int) (b: inref<int>) = a + b
let setOutRef (a: int) (b: outref<int>) = b <- a

let mutable mutX = 3

open FSharp.UMX

[<Measure>] type customerId
[<Measure>] type orderId
[<Measure>] type kg

type Order =
    {
        id : string<orderId>
        customer : string<customerId>
        quantity : int<kg>
    }

let inline inlineLambdaWithAnonRecord callback =
    fun () -> {| A = 1 |} |> callback

let sideEffect() = ()

#if FABLE_COMPILER

[<Fact>]
let ``test lambdas returning member expression accessing JS object work`` () = // #2311
    let x = inlineLambdaWithAnonRecord (fun x -> x.A)
    x() |> equal 1

[<Fact>]
let ``test can check compiler version with constant`` () =
    let mutable x = 0
    #if FABLE_COMPILER
    x <- x + 1
    #endif
    #if FABLE_COMPILER_3
    x <- x + 2
    #endif
    #if FABLE_COMPILER_4
    x <- x + 3
    #endif
    #if FABLE_COMPILER_PY
    x <- x + 4
    #endif
    #if FABLE_COMPILER_JS
    x <- x + 5
    #endif

    equal 3 x // FIXME: Fix when FABLE_COMPILER_PY is added

[<Fact>]
let ``test Can check compiler version at runtime`` () =
    Compiler.majorMinorVersion >=  3.0 |> equal true
    Text.RegularExpressions.Regex.IsMatch(Compiler.version, @"^\d+\.\d+") |> equal true

[<Fact>]
let ``test Can access compiler options`` () =
    let canAccessOpts =
        match box Compiler.debugMode, box Compiler.typedArrays with
        | :? bool, :? bool -> true
        | _ -> false
    equal true canAccessOpts

[<Fact>]
let ``test Can access extension for generated files`` () =
    Compiler.extension.EndsWith(".py") |> equal true

#endif

[<Fact>]
let ``test Values of autogenerated functions are not replaced by optimizations`` () = // See #1583
    let model = Some (5, 5)
    let model1 = setCellBuggy model
    let model2 = setCell model
    model1 = model2 |> equal true

[<Fact>]
let ``test Assignment block as expression is optimized`` () =
    let foo x y = x - y
    let mutable x = 15
    let res = A.C.Helper.Add5(let mutable x = 2 in let mutable y = 3 in x + y)
    let test () =
        A.C.Helper.Add5(let mutable x = 4 in let mutable y = 3 in x + y)
        |> equal 12
    test()
    equal 10 res
    foo x 5 |> equal 10

[<Fact>]
let ``test Optimized assignment blocks inside try ... with work`` () =
    let res =
        try A.C.Helper.Add5(let mutable x = 2 in let mutable y = 3 in x + y)
        with _ -> 1
    equal 10 res

[<Fact>]
let ``test for .. downto works`` () = // See #411
    let mutable x = ""
    for i = 1 to 5 do
        x <- x + (string i)
    equal "12345" x
    let mutable y = ""
    for i = 5 downto 1 do
        y <- y + (string i)
    equal "54321" y

[<Fact>]
let ``test Self references in constructors work`` () = // See #124
    let t = MyTest(5)
    equal 12 t.Value
    equal 17 t.Value2

[<Fact>]
let ``test Using self identifier from class definition in members works`` () = // See #124
    let t = MyTest(5)
    t.Foo() |> equal 24

[<Fact>]
let ``test Module members from partial functions work`` () = // See #115
    logItem1 "item1" |> equal "a = item1, b = item1"

[<Fact>]
let ``test Class members from partial functions work`` () = // See #115
    let x = PartialFunctions()
    x.logItem1 "item1" |> equal "a = item1, b = item1"

[<Fact>]
let ``test Local values from partial functions work`` () = // See #115
    let logItem1 = log2 "item1"
    let logItem2 = log2 "item2"
    logItem1 "item1" |> equal "a = item1, b = item1"

[<Fact>]
let ``test Custom computation expressions work`` () =
    execMaybe 5 |> equal (Some 23)
    execMaybe 99 |> equal None

[<Fact>]
let ``test Units of measure work`` () =
    3<km/h> + 2<km/h> |> equal 5<km/h>

    let v1 = { x = 4.3<mi>; y = 5.<mi>; z = 2.8<mi> }
    let v2 = { x = 5.6<mi>; y = 3.8<mi>; z = 0.<mi> }
    let v3 = v1 + v2
    equal 8.8<mi> v3.y

[<Fact>]
let ``test Units of measure work with longs`` () =
    3L<km/h> + 2L<km/h> |> equal 5L<km/h>

[<Fact>]
let ``test Units of measure work with decimals`` () =
    3M<km/h> + 2M<km/h> |> equal 5M<km/h>

[<Fact>]
let ``test Abbreviated measures work`` () = // #2313
    let x = 5.<Measure1>
    let c = MeasureTest()
    c.Method(5.<Measure2>) |> equal x

[<Fact>]
let ``test FSharp.UMX works`` () =
    let lookupById (orders : Order list) (id : string<orderId>) =
        orders |> List.tryFind (fun o -> o.id = id)

    let order =
        {
            id = % "orderId"
            customer = % "customerId"
            quantity = % 42
        }

    // lookupById [] order.customer // compiler error
    let orders = [{ order with quantity = %50 }]
    lookupById orders order.id
    |> Option.map (fun o -> UMX.untag o.quantity)
    |> equal (Some 50)

[<Fact>]
let ``test FSharp.UMX: reflection info`` () =
    let fields = Reflection.FSharpType.GetRecordFields typeof<Order>
    fields.Length |> equal 3

[<Fact>]
let ``test Static constructors work`` () =
    let point1 = PointWithCounter(10, 52)
    sprintf "%d %d %d %d" (point1.Prop1) (point1.Prop2) (point1.CreatedCount) (point1.FunctionValue)
    |> equal "10 52 1 204"
    let point2 = PointWithCounter(20, 99)
    sprintf "%d %d %d %d" (point2.Prop1) (point2.Prop2) (point2.CreatedCount) (point2.FunctionValue)
    |> equal "20 99 2 598"

[<Fact>]
let ``test File with single type in namespace compiles`` () =
    equal SingleTypeInNamespace.Hello "Hello"

[<Fact>]
let ``test Type abbreviation in namespace compiles`` () = // See #140
    let h = Util2.H(5)
    equal "5" h.Value

[<Fact>]
let ``test Multiple namespaces in same file work`` () = // See #1218
    A.C.Helper.Add5(9) |> equal 14

[<Fact>]
let ``test Inline methods work`` () =
    f 2 3 |> equal 5
