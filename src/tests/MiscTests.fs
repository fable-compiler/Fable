[<NUnit.Framework.TestFixture>] 
module Fable.Tests.Misc

open System
open NUnit.Framework
open Fable.Tests.Util

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

let inline f x y = x + y

[<Test>]
let ``Inline methods work``() =
    f 2 3 |> equal 5

[<Test>]
let ``Calls to core lib from a subfolder work``() =
    Util2.Helper.Format("{0} + {0} = {1}", 2, 4)
    |> equal "2 + 2 = 4"

let f' x y z = x + y + z

[<Test>]
let ``Conversion to delegate work``() =
    let del = System.Func<_,_,_,_> f'
    del.Invoke(1,2,3)
    |> equal 6

let (|NonEmpty|_|) (s: string) =
    match s.Trim() with "" -> None | s -> Some s

[<Test>]
let ``Multiple active pattern calls work``() =
    match " Hello ", " Bye " with
    | NonEmpty "Hello", NonEmpty "Bye" -> true
    | _ -> false
    |> equal true
    
type IFoo =
   abstract Bar: s: string * [<ParamArray>] rest: obj[] -> string
   
[<Test>]
let ``ParamArray in object expression works``() =
   let o = { new IFoo with member x.Bar(s: string, [<ParamArray>] rest: obj[]) = String.Format(s, rest) }
   o.Bar("{0} + {0} = {1}", 2, 4)
   |> equal "2 + 2 = 4"
       
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
        
        let Same = 20
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

type Point =
    { x: float; y: float }
    static member (+) (p1: Point, p2: Point) = { x=p1.x + p2.x; y=p1.y + p2.y }
    static member (-) (p1: Point, p2: Point) = { x=p1.x - p2.x; y=p1.y - p2.y }

[<Test>]
let ``Custom operators with types work``(): unit =
    let p1 = { x=5.; y=10. }
    let p2 = { x=2.; y=1. }
    equal 7. (p1 + p2).x
    equal 9. (p1 - p2).y

let (+) x y = x * y

let (-) x y = x / y

let (||||) x y = x + y

[<Test>]
let ``Custom operators work``() =
    5 + 5 |> equal 25
    10 - 2 |> equal 5
    2 |||| 2 |> equal 4
