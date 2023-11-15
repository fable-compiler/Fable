module Fable.Tests.Misc

open System
open Fable.Core
open Util.Testing
open Util2.Extensions

let mutable private fn: int -> int = id

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
    member _.Value = x

type Test(i) as myself =
    inherit Base()
    let mutable y = 12
    do myself.Mutate(i+2)
    do myself.Mutate2(i)
    member this.Mutate2 i = y <- y + i
    member _.Value2 = y
    member _.Foo() = myself.Value * 2

let log2 (a: string) (b: string) = String.Format("a = {0}, b = {1}", a, b)
let logItem1 = log2 "item1"
let logItem2 = log2 "item2"

type PartialFunctions() =
    member _.logItem1 = log2 "item1"
    member _.logItem2 = log2 "item2"

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

let lerp x (alpha: float<'u>) (delta: MiscTestsHelper.Vector2<'u>) = x + alpha * delta

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
        member _.Bar = "Bar"
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
    member _.Foo() = 5
    interface IDisposable with
        member _.Dispose () = ()

type DisposableBar(v) =
    do v := 10
    interface IDisposable with
        member _.Dispose () = v := 20

let createCellDiposable cell =
  cell := 10
  { new System.IDisposable with
      member x.Dispose() = cell := 20 }

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
let inline makeNum f = { new INum with member _.Num = f() }

// TODO: This test is actually wrong in JS too
// We need to capture this when an inlined function is creating an object expression
#if !FABLE_COMPILER_TYPESCRIPT
type TestClass(n) =
    let addOne x = x + 4
    let inner = makeNum (fun () -> addOne n)
    member _.GetNum() = inner.Num
#endif

type RecursiveType(subscribe) as self =
    let foo = 3
    let getNumber() = 3
    do subscribe (getNumber >> self.Add2)
    member _.Add2(i) = self.MultiplyFoo(i) + 2
    member _.MultiplyFoo(i) = i * foo

type InliningMutationTest(l: int, r: int) =
        let mutable left = 0

        let call() =
            left <- l
            r

        member _.Run() =
            let right = call()
            left + right

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

// TODO: Abstract classes in TypeScript
// - Add abstract modifier to class
// - Declare abstract methods
// - Skip reflection helper and constructor wrapper
#if !FABLE_COMPILER_TYPESCRIPT
    [<AbstractClass>]
    type ObjectExprBase (x: int ref) as this =
        do x := this.dup x.contents
        abstract member dup: int -> int
#endif

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

type TestRef = TestRef of bool ref

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

type ValueType =
  struct
    val public X : int
  end

type MutableFoo =
    { mutable x: int }

let incByRef (a: int) (b: byref<int>) = b <- a + b
// TODO: inref types in TypeScript (compile as FSharpRef)
#if !FABLE_COMPILER_TYPESCRIPT
let addInRef (a: int) (b: inref<int>) = a + b
#endif
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

#if !FABLE_COMPILER_JAVASCRIPT
type LiteralJson = Fable.JsonProvider.Generator<LITERAL_JSON>
#endif

let inline inlineLambdaWithAnonRecord callback =
    fun () -> {| A = 1 |} |> callback

let sideEffect() = ()

let inline inlineToString (f: 'T -> string): 'T -> string =
    let unused = f
    fun a -> $"{a}"

let tests =
  testList "Miscellaneous" [

    testCase "Generic unit args work" <| fun _ -> // #3584
        let to_str = inlineToString (fun (props: unit) -> "s")
        to_str () |> equal $"{()}"

#if FABLE_COMPILER
#if !FABLE_COMPILER_JAVASCRIPT
    testCase "Fable.JsonProvider works" <| fun _ ->
        let parsed = LiteralJson(ANOTHER_JSON)
        parsed.widget.debug |> equal false
        parsed.widget.text.data |> equal "lots of"
        parsed.widget.text.size.[1].height |> equal 80.
        parsed.widget.text.vOffset |> equal 500.
#endif

    testCase "Lambdas returning member expression accessing JS object work" <| fun () -> // #2311
        let x = inlineLambdaWithAnonRecord (fun x -> x.A)
        x() |> equal 1

    testCase "Can check compiler version with constant" <| fun _ ->
        let mutable x = 0
        #if FABLE_COMPILER
        x <- x + 1
        #endif
        #if FABLE_COMPILER_3
        x <- x + 2
        #endif
        #if FABLE_COMPILER_4
        x <- x + 4
        #endif
        #if FABLE_COMPILER_5
        x <- x + 16
        #endif
        equal 5 x

    testCase "Can check compiler version at runtime" <| fun _ ->
        Compiler.majorMinorVersion >= 4.0 |> equal true
        Text.RegularExpressions.Regex.IsMatch(Compiler.version, @"^\d+\.\d+") |> equal true

    testCase "Can access compiler options" <| fun _ ->
        let canAccessOpts =
            match box Compiler.debugMode, box Compiler.typedArrays with
            | :? bool, :? bool -> true
            | _ -> false
        equal true canAccessOpts

    testCase "Can access extension for generated files" <| fun _ ->
        let ext =
#if FABLE_COMPILER_TYPESCRIPT
            ".ts"
#else
            ".js"
#endif
        Compiler.extension.EndsWith(ext) |> equal true
#endif

    testCase "Values of autogenerated functions are not replaced by optimizations" <| fun () -> // See #1583
        let model = Some (5, 5)
        let model1 = setCellBuggy model
        let model2 = setCell model
        model1 = model2 |> equal true

    testCase "Assignment block as expression is optimized" <| fun () ->
        let foo x y = x - y
        let mutable x = 15
        let res = A.C.Helper.Add5(let mutable x = 2 in let mutable y = 3 in x + y)
        let test () =
            A.C.Helper.Add5(let mutable x = 4 in let mutable y = 3 in x + y)
            |> equal 12
        test()
        equal 10 res
        foo x 5 |> equal 10

    testCase "Optimized assignment blocks inside try ... with work" <| fun () ->
        let res =
            try A.C.Helper.Add5(let mutable x = 2 in let mutable y = 3 in x + y)
            with _ -> 1
        equal 10 res

    testCase "for .. downto works" <| fun () -> // See #411
        let mutable x = ""
        for i = 1 to 5 do
            x <- x + (string i)
        equal "12345" x
        let mutable y = ""
        for i = 5 downto 1 do
            y <- y + (string i)
        equal "54321" y

    testCase "Self references in constructors work" <| fun () -> // See #124
        let t = Test(5)
        equal 12 t.Value
        equal 17 t.Value2

    testCase "Using self identifier from class definition in members works" <| fun () -> // See #124
        let t = Test(5)
        t.Foo() |> equal 24

    testCase "Module members from partial functions work" <| fun () -> // See #115
        logItem1 "item1" |> equal "a = item1, b = item1"

    testCase "Class members from partial functions work" <| fun () -> // See #115
        let x = PartialFunctions()
        x.logItem1 "item1" |> equal "a = item1, b = item1"

    testCase "Local values from partial functions work" <| fun () -> // See #115
        let logItem1 = log2 "item1"
        let logItem2 = log2 "item2"
        logItem1 "item1" |> equal "a = item1, b = item1"

    testCase "Custom computation expressions work" <| fun () ->
        execMaybe 5 |> equal (Some 23)
        execMaybe 99 |> equal None

    testCase "Units of measure work" <| fun () ->
        3<km/h> + 2<km/h> |> equal 5<km/h>

        let v1 = { x = 4.3<mi>; y = 5.<mi>; z = 2.8<mi> }
        let v2 = { x = 5.6<mi>; y = 3.8<mi>; z = 0.<mi> }
        let v3 = v1 + v2
        equal 8.8<mi> v3.y

    testCase "Units of measure work with longs" <| fun () ->
        3L<km/h> + 2L<km/h> |> equal 5L<km/h>

    testCase "Units of measure work with decimals" <| fun () ->
        3M<km/h> + 2M<km/h> |> equal 5M<km/h>

    testCase "Units of measure work with unsigned ints" <| fun () -> // See #2955
        let s1 = 1uy<km>
        let s2 = 2us<h>
        let s3 = 3u<km/h>
        let s4 = 4ul<mi>
        $"%i{s1}%i{s2}%i{s3}%i{s4}" |> equal "1234"

    testCase "Abbreviated measures work" <| fun () -> // #2313
        let x = 5.<Measure1>
        let c = MeasureTest()
        c.Method(5.<Measure2>) |> equal x

    testCase "Can resolve trait calls from another file with units of measure" <| fun () -> // See #2880
        let expected = MiscTestsHelper.Vector2 (8.209999999999999<mi^2>, 7.72<mi^2>)
        let x = MiscTestsHelper.Vector2(4.5<mi^2>, 4.5<mi^2>)
        MiscTestsHelper.Vector2(5.3<mi>, 4.6<mi>) |> lerp x 0.7<mi> |> equal expected

    testCase "FSharp.UMX works" <| fun () ->
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

    testCase "FSharp.UMX: reflection info" <| fun () ->
        let fields = Reflection.FSharpType.GetRecordFields typeof<Order>
        fields.Length |> equal 3

    testCase "Static constructors work" <| fun () ->
        let point1 = PointWithCounter(10, 52)
        sprintf "%d %d %d %d" (point1.Prop1) (point1.Prop2) (point1.CreatedCount) (point1.FunctionValue)
        |> equal "10 52 1 204"
        let point2 = PointWithCounter(20, 99)
        sprintf "%d %d %d %d" (point2.Prop1) (point2.Prop2) (point2.CreatedCount) (point2.FunctionValue)
        |> equal "20 99 2 598"

    testCase "File with single type in namespace compiles" <| fun () ->
        equal SingleTypeInNamespace.Hello "Hello"

    testCase "Type abbreviation in namespace compiles" <| fun () -> // See #140
        let h = Util2.H(5)
        equal "5" h.Value

    testCase "Multiple namespaces in same file work" <| fun () -> // See #1218
        A.C.Helper.Add5(9) |> equal 14

    testCase "Inline methods work" <| fun () ->
        f 2 3 |> equal 5

    testCase "Inline methods with this argument work" <| fun () -> // See #638
        let x = FooModule.FooInline()
        x.Foo |> equal "FooBar"
        x.Foofy 4 |> equal "BarBarBarBar"

    testCase "Inline properties work" <| fun () ->
        let x = FooModule.FooInline()
        x.PropertyInline <- 3uy
        x.PropertyInline |> equal 3uy

    testCase "Inline extension methods with this argument work" <| fun () -> // See #638
        let x = FooModule.FooInline()
        x.Bar2 |> equal "BarBar"
        x.FoofyPlus 3 |> equal "BarBarBarBarBarBar"

    testCase "Inline extension methods in other files can be found" <| fun () -> // See #1667
        "HOLA CARACOLA".StartsWith("hola") |> equal false
        "HOLA CARACOLA".StartsWithIgnoreCase("hola") |> equal true

    testCase "Inline overloaded methods work" <| fun () ->
      let res1 = Type.New(5).Method(false).Method(true).Method()
      let res2 = Type.New(5).MethodI(false).MethodI(true).MethodI()
      equal res1.a res2.a
      counter() |> equal 3

    testCase "Inline overloaded methods in other files work" <| fun () ->
      let res1 = MiscTestsHelper.Type.New(5).Method(false).Method(true).Method()
      let res2 = MiscTestsHelper.Type.New(5).MethodI(false).MethodI(true).MethodI()
      equal res1.a res2.a
      MiscTestsHelper.counter() |> equal 3

    testCase "Inlined arguments with delayed resolution are only evaluated once" <| fun () ->
        let mutable x = 0
        let foo() =
            x <- x + 1
            10
        let f = sum (foo())
        f 20 |> f |> equal 40
        equal 1 x

    testCase "Don't inline values that evaluate multiple times" <| fun () ->
        let li = [1;2;3;4]
        let res = List.map (FooModule.add FooModule.genericValue) li
        equal 1 FooModule.genericValueBackend
        equal [6;7;8;9] res

    testCase "Calls to core lib from a subfolder work" <| fun () ->
        Util2.Helper.Format("{0} + {0} = {1}", 2, 4)
        |> equal "2 + 2 = 4"

    testCase "Conversion to delegate works" <| fun () ->
        (System.Func<_,_,_,_> f1).Invoke(1,2,3) |> equal 6

        let f = f1
        (System.Func<_,_,_,_> f).Invoke(1,2,3) |> equal 6

        let del = System.Func<_,_,_,_>(fun x y z -> x + y + z)
        del.Invoke(1,2,3) |> equal 6

        (System.Func<_,_> f2).Invoke(2) |> equal 4

        // See #2400
        let func1 : Func<int> = Func<int>(fun () -> 8)
        func1.Invoke() |> equal 8

        let fn2 () = 9
        let func2 : Func<int> = Func<int>(fn2)
        func2.Invoke() |> equal 9

        let func2b = Func<unit, int>(fn2)
        func2b.Invoke() |> equal 9

        let fn2c () () = 9
        let func2c : Func<int> = Func<int>(fn2c())
        func2c.Invoke() |> equal 9

        let fn3 i = i + 4
        let func3 = Func<int, int>(fn3)
        func3.Invoke(7) |> equal 11

        let fn4 x y = x * y - 3
        let func4 = Func<int, int, int>(fn4)
        func4.Invoke(4, 6) |> equal 21

    testCase "Conversion to Func<_> works" <| fun () ->
        (System.Func<_> f3).Invoke() |> equal 5
        let f = Func<_>(fun () -> 6)
        f.Invoke() |> equal 6

    testCase "Conversion to aliased Func<_> works" <| fun () ->
        (MyDelegate f3).Invoke() |> equal 5
        let f = MyDelegate(fun () -> 6)
        f.Invoke() |> equal 6

    // TODO
    // testCase "Conversion to FSharpFunc<_,_,_> works" <| fun () ->
    //     let f x y = x + y
    //     let f = FSharpFunc<_,_,_>.Adapt(f)
    //     f.Invoke(1, 2) |> equal 3

    // testCase "Conversion to FSharpFunc<_,_,_,_> works" <| fun () ->
    //     let f x y z = x + y + z
    //     let f = FSharpFunc<_,_,_,_>.Adapt(f)
    //     f.Invoke(1, 2, 3) |> equal 6

    testCase "Conversion to Action<_> works" <| fun () ->
        let f1' = Action<int>(fun i -> myMutableField <- i * 2)
        let f2' = Action<int>(f4)
        let f3' = Action<_>(f6 4)
        f1'.Invoke(1)
        equal 2 myMutableField
        f2'.Invoke(8)
        equal 8 myMutableField
        f3'.Invoke(10)
        equal 40 myMutableField

    // TODO!!!
    // testCase "Conversion to Action works" <| fun () ->
    //     let f4' = Action(fun () -> myMutableField <- 7)
    //     let f5' = Action(f5)
    //     let f6' = Action(f7 3)
    //     let f7' i () = myMutableField <- i * 3
    //     let f8' = Action(f7' 3)
    //     f4'.Invoke()
    //     equal 7 myMutableField
    //     f5'.Invoke()
    //     equal 5 myMutableField
    //     f6'.Invoke()
    //     equal 9 myMutableField
    //     f8'.Invoke()
    //     equal 9 myMutableField

    testCase "Multiple active pattern calls work" <| fun () ->
        match " Hello ", " Bye " with
        | NonEmpty "Hello", NonEmpty "Bye" -> true
        | _ -> false
        |> equal true

    testCase "ParamArray in object expression works" <| fun () ->
       let o = { new IFoo with member x.Bar(s: string, [<ParamArray>] rest: obj[]) = String.Format(s, rest) }
       o.Bar("{0} + {0} = {1}", 2, 4)
       |> equal "2 + 2 = 4"

    testCase "Object expression can reference enclosing type and self" <| fun () -> // See #158
        let f = Foo(5)
        let f2 = f.MakeFoo2()
        f2.Value <- 2
        f.Value |> equal 12
        f2.Test(2) |> equal 22

    testCase "Nested object expressions work" <| fun () -> // See #158
        let f = Foo(5)
        let f2 = f.MakeFoo2()
        f2.MakeFoo().Bar("Numbers") |> equal "Numbers: 10 20 5"

    testCase "References to enclosing type from object expression work" <| fun () -> // See #438
        MyComponent("TestA").works1().doWork() |> equal "TestA-1"
        MyComponent("TestB").works2().doWork() |> equal "TestB-2"
        MyComponent("TestC").works3().doWork() |> equal "TestC-3"
        MyComponent("TestD").works4().doWork() |> equal "TestD-4"
        MyComponent("TestE").works5().doWork() |> equal "TestE-5"

    testCase "Properties in object expression work" <| fun () ->
        let mutable backend = 0
        let o = { new IFoo3 with member x.Bar with get() = backend and set(v) = backend <- v }
        o.Bar |> equal 0
        backend <- 5
        o.Bar |> equal 5
        o.Bar <- 10
        o.Bar |> equal 10

#if !FABLE_COMPILER_TYPESCRIPT
    testCase "Object expression from class works" <| fun () ->
        let o = { new SomeClass("World") with member x.ToString() = sprintf "Hello %s" x.Name }
        // TODO: Type testing for object expressions?
        // match box o with
        // | :? SomeClass as c -> c.ToString()
        // | _ -> "Unknown"
        // |> equal "Hello World"
        o.ToString() |> equal "Hello World"

    testCase "Inlined object expression doesn't change argument this context" <| fun () -> // See #1291
        let t = TestClass(42)
        t.GetNum() |> equal 46

    testCase "Object expressions don't optimize members away" <| fun () -> // See #1434
        let o =
            { new Taster with
                member _.Starter = 5.5
                member this.Taste(quality, quantity) =
                    taste this quality quantity
              interface Eater with
                member _.Bite() = 25
            }
        o.Taste(4., 6.) |> equal 28

    testCase "Members are accessible in abstract class constructor inherited by object expr" <| fun () -> // See #2139
        let x = ref 5
        let obj = { new ObjectExprBase(x) with
                        override _.dup x = x * x }
        equal 25 x.contents

    testCase "Composition with recursive `this` works" <| fun () ->
        let mutable x = 0
        RecursiveType(fun f -> x <- f()) |> ignore
        equal 11 x

    testCase "Type extension static methods work" <| fun () ->
        let disposed = ref false
        let disp = IDisposable.Create(fun () -> disposed := true)
        disp.Dispose ()
        equal true !disposed

    testCase "Type extension properties work" <| fun () ->
        let c = SomeClass("John")
        equal "John Smith" c.FullName

    testCase "Type extension methods work" <| fun () ->
        let c = SomeClass("John")
        c.NameTimes(1,2) |> equal "JohnJohnJohn"

    testCase "Type extension methods with same name work" <| fun () ->
        let c = AnotherClass(3)
        equal "3" c.FullName

    testCase "Type extension overloads work" <| fun () ->
        let c = AnotherClass(3)
        c.Overload("3") |> equal "33"
        c.Overload(3) |> equal 12

    testCase "Extending different types with same name and same method works" <| fun () ->
        AnotherClass(5).Value2 |> equal 10
        NestedModule.AnotherClass(5).Value2 |> equal 40

    testCase "Module, members and properties with same name don't clash" <| fun () ->
        StyleBuilderHelper.test() |> equal true

    testCase "Module mutable properties work" <| fun () ->
        equal 10 Mutable.prop
        Mutable.prop <- 5
        equal 5 Mutable.prop

    testCase "Accessing members of parent module with same name works" <| fun () ->
        equal 5 Same.Same.shouldEqual5

    testCase "Accessing members of child module with same name works" <| fun () ->
        equal 10 Same.Same.shouldEqual10

    testCase "Accessing members with same name as module works" <| fun () ->
        equal 20 Same.Same.shouldEqual20

    testCase "Naming values with same name as module works" <| fun () ->
        equal 30 Same.Same.shouldEqual30

    testCase "Can access nested recursive function with mangled name" <| fun () ->
        Util.Bar.nestedRecursive 3 |> equal 10

    testCase "Can access non nested recursive function with mangled name" <| fun () ->
        Util.nonNestedRecursive "ja" |> equal "jajaja"

    testCase "Module members don't conflict with JS names" <| fun () ->
        Util.Int32Array |> Array.sum |> equal 3

    testCase "Modules don't conflict with JS names" <| fun () ->
        Util.Float64Array.Float64Array |> Array.sum |> equal 7.

    // Modules and TestFixtures bind values in a slightly different way
    // Test both cases
    testCase "Binding doesn't shadow top-level values" <| fun () -> // See #130
        equal 10 Util.B.c
        equal 20 Util.B.D.d

    testCase "Binding doesn't shadow top-level values (TestFixture)" <| fun () -> // See #130
        equal 10 B.c
        equal 20 B.D.d

    testCase "Binding doesn't shadow top-level functions" <| fun () -> // See #130
        equal 4 Util.B.d
        equal 0 Util.B.D.e

    testCase "Binding doesn't shadow top-level functions (TestFixture)" <| fun () -> // See #130
        equal 4 B.d
        equal 0 B.D.e

    testCase "Setting a top-level value doesn't alter values at same level" <| fun () -> // See #130
        equal 15 Util.a
        equal 25 Util.B.a

    testCase "Setting a top-level value doesn't alter values at same level (TestFixture)" <| fun () -> // See #130
        equal 15 a
        equal 25 B.a

    testCase "Internal members can be accessed from other modules" <| fun () -> // See #163
        Internal.add 3 2 |> equal 5

    testCase "Internal types can be accessed from other modules" <| fun () -> // See #163
        Internal.MyType.Subtract 3 2 |> equal 1

    // In F# both branches of if-then-else has the same type,
    // but this is not always true in Fable AST. For example when
    // one branch is a Throw expression, it has always type Unit.
    // So we should test that the type of the whole expression is not determined
    // by the throw expression in one of its branches.
    //
    // The same applies to try-with expression.

#if FABLE_COMPILER
    // This test fails if the system language is set to other language than English
    testCase "Pattern-matching against discriminated unions gives proper error message" <| fun () ->
        try
            let unitCircle = Circle 1
            match unitCircle with
            | Rectangle(n, m) -> failwith "Should not happen"
            | Square n -> failwith "Should not happen"
        with
            | ex -> ex.Message.StartsWith "Match failure" |> equal true
#endif

    testCase "Type of if-then-else expression is correctly determined when 'then' branch throws" <| fun () ->
        let f () =
            if false then failwith "error" else 7

        f () |> equal 7

    testCase "Type of if-then-else expression is correctly determined when 'else' branch throws" <| fun () ->
        let f () =
            if true then 7 else failwith "error"

        f () |> equal 7

    testCase "Type of try-with expression is correctly determined when 'try' block throws" <| fun () ->
        let f () =
            try failwith "error" with | _ -> 7

        f () |> equal 7

    testCase "Type of try-with expression is correctly determined when exception handler throws" <| fun () ->
        let f () =
            try 7 with | _ -> failwith "error"

        f () |> equal 7

    testCase "use doesn't return on finally clause" <| fun () -> // See #211
        let foo() =
            use c = new DisposableFoo()
            c.Foo()
        foo() |> equal 5

    testCase "use calls Dispose at the end of the scope" <| fun () ->
        let cell = ref 0
        let res =
            use c = new DisposableBar(cell)
            !cell
        res |> equal 10
        !cell |> equal 20

    testCase "use calls Dispose (of an object expression) at the end of the scope" <| fun () ->
        let cell = ref 0
        let res =
            use c = createCellDiposable cell
            !cell
        res |> equal 10
        !cell |> equal 20

    testCase "Can use `use` with null" <| fun () -> // See #2719
        let mutable x = 0
        let msg =
            use __ = null : IDisposable
            "hooray"
        msg |> equal "hooray"
        x |> equal 0

        let msg =
            use __ = { new IDisposable with member _.Dispose() = x <- 1 }
            "booh"
        msg |> equal "booh"
        x |> equal 1

    testCase "Unchecked.defaultof works" <| fun () ->
        Unchecked.defaultof<int> |> equal 0
        Unchecked.defaultof<int64> |> equal 0L
        Unchecked.defaultof<bigint> |> equal 0I
        Unchecked.defaultof<decimal> |> equal 0M
        Unchecked.defaultof<DateTime> |> equal DateTime.MinValue
        Unchecked.defaultof<TimeSpan> |> equal (TimeSpan.FromMilliseconds 0.)
        Unchecked.defaultof<bool> |> equal false
        Unchecked.defaultof<string> |> equal null
        Unchecked.defaultof<Guid> |> equal Guid.Empty
        let x = ValueType()
        Unchecked.defaultof<ValueType> |> equal x
        x.X |> equal 0

    testCase "Unchecked.defaultof works with tuples" <| fun () -> // See #2491
        // TODO: Non-struct tuples
        // let y: (int*int) = Unchecked.defaultof<_>
        // equal null (box y)
        let x: struct (int*int) = Unchecked.defaultof<_>
        equal (struct (0, 0)) x

    testCase "Pattern matching optimization works (switch statement)" <| fun () ->
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

    testCase "Pattern matching optimization works (switch expression)" <| fun () ->
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

    testCase "Pattern matching with same result for last pattern and wildcard works" <| fun () -> // #2357
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

    testCase "FSharpRef can be used in properties" <| fun () -> // See #521
        let r = ref false
        let x = TestRef r
        r := true
        match x with TestRef r2 -> !r2
        |> equal true

    testCase "Recursive values work" <| fun () -> // See #237
        mutableValue |> equal 5
        recursive1()
        mutableValue |> equal 10

    testCase "Module generic methods without arguments work" <| fun () ->
        let li = empty<string>
        Seq.length li |> equal 1

    testCase "Public members of private modules can be accessed" <| fun () -> // See #696
        MyPrivateModule.publicFoo() |> equal "foo bar"

    testCase "Public types of private modules can be accessed" <| fun () -> // See #841
        let thing = MyPrivateModule.Concrete() :> IInterface
        thing.Member "World" "Hello" |> equal "Hello World"

    testCase "Types declared in signature file work" <| fun () -> // See #754
        let t = Spaces.TRec.Create("haha", "hoho")
        t.Value |> equal "hahahoho"

    testCase "Primary constructor of types from signature files work" <| fun () -> // See #571
        Spaces.Test(true).Status |> equal true
        Spaces.Test(false).Status |> equal false

    testCase "Two types with same name in different folders work" <| fun () -> // See #781
        tempet.SayA.hello "Albert"
        |> equal "Hello Albert from SayA"
        tempet.SayB.hello "Albert"
        |> equal "Hello Albert from SayB"

    testCase "While with isNone doesn't hang with Some ()" <| fun () ->
        Trampoline.run (fun _ -> Trampoline.Break "hello") () |> ignore
        Trampoline.run (fun _ -> Trampoline.Break 42) () |> ignore
        Trampoline.run (fun _ -> Trampoline.Break ()) () |> ignore

    testCase "Removing optional arguments not in tail position works" <| fun () ->
        Internal.MyType.Add(y=6) |> equal 26

    testCase "Ignore shouldn't return value" <| fun () -> // See #1360
        let producer () = 7
        equal (box ()) (box(ignore(producer())))

    testCase "Can import files specified via globbing patterns" <| fun () -> // See #1942
        Glob.hello "Albert"
        |> equal "Hello Albert from Glob"

    testCase "Mutable variables can be passed by reference" <| fun () ->
        let a = 1
        let mutable b = 2
        incByRef a &b
        b |> equal 3
        addInRef a &b |> equal 4
        b |> equal 3
        setOutRef a &b
        b |> equal 1

    testCase "Public mutable variables can be passed by reference" <| fun () ->
        let a = 1
        mutX <- 2
        incByRef a &mutX
        mutX |> equal 3
        addInRef a &mutX |> equal 4
        mutX |> equal 3
        setOutRef a &mutX
        mutX |> equal 1

    testCase "Mutable fields can be passed by reference" <| fun () ->
        let a = 1
        let foo: MutableFoo = { x = 2 }
        incByRef a &foo.x
        foo.x |> equal 3
        addInRef a &foo.x |> equal 4
        foo.x |> equal 3
        setOutRef a &foo.x
        foo.x |> equal 1

    testCase "Assigning to unit works" <| fun () -> // See #2548
        let mutable value = 2
        let inline doit x (f: 'A -> 'T) =
            let mutable r = Unchecked.defaultof<_>
            r <- f x
            r

        doit "a" (fun a -> a + "b")
        |> equal "ab"

        doit 2 (fun x -> value <- value + x)
        value |> equal 4

    testCase "Mutating variables is not postponed (functions)" <| fun () ->
        let ``inlineData PR #2683`` =  [3, 2, 5; 5, 10, 15; 10, 20, 30]

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

    testCase "Mutating variables is not postponed (classes)" <| fun () ->
        let ``inlineData PR #2683`` =  [3, 2, 5; 5, 10, 15; 10, 20, 30]

        let runCase (l: int) (r: int) (expect: int) =
            InliningMutationTest(l, r).Run() |> equal expect
            InliningMutationTest(l, r).Run() |> equal expect

        for (l, r, ``l + r``) in ``inlineData PR #2683`` do
            runCase l r ``l + r``

    testCase "References captured by >> are eagerly evaluated" <| fun () -> // See #2851
        let times2 x = x * 2
        fn <- fn >> times2
        fn 5 |> equal 10
#endif
  ]
