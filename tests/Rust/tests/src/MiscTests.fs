module Fable.Tests.MiscTests

open Util.Testing

[<Measure>] type km           // Define the measure units
[<Measure>] type mi           // as simple types decorated
[<Measure>] type h            // with Measure attribute
[<Measure>] type m
[<Measure>] type s

[<Measure>] type Measure1
[<Measure>] type Measure2 = Measure1

type MeasureTest() =
    member _.Method(x: float<Measure2>) = x

// // Can be used in a generic way
// type Vector3D<[<Measure>] 'u> =
//     { x: float<'u>; y: float<'u>; z: float<'u> }
//     static member (+) (v1: Vector3D<'u>, v2: Vector3D<'u>) =
//         { x = v1.x + v2.x; y = v1.y + v2.y; z = v1.z + v2.z }

type TestUnion =
    | UncurryUnion of add: (int -> int -> int)

let applyUncurryUnion x y = function
    | UncurryUnion f -> f x y

type TestClass(add: (int -> int -> int)) =
    member _.Add(x, y) = add x y

module ModuleBindings =
    let inc1 (x: byref<int>) =
        x <- x + 1
    let inc2 x =
        x + 1
    let modx = 3
    let mutable mody = 4

open ModuleBindings

[<Fact>]
let ``Passing byref works`` () =
    let mutable x = 5
    inc1 &x
    let y = inc2 x
    x |> equal 6
    y |> equal 7

[<Fact>]
let ``Module let bindings work`` () =
    mody <- mody + 1
    let z = modx + mody
    z |> equal 8

[<Fact>]
let ``Units of measure work`` () =
    let a = 4<m>
    let b = 2<s>
    let c = a / b
    c |> equal (2<m/s>)

[<Fact>]
let ``Units of measure work II`` () =
    3<km/h> + 2<km/h> |> equal 5<km/h>
    // let v1 = { x = 4.3<mi>; y = 5.<mi>; z = 2.8<mi> }
    // let v2 = { x = 5.6<mi>; y = 3.8<mi>; z = 0.<mi> }
    // let v3 = v1 + v2
    // equal 8.8<mi> v3.y

[<Fact>]
let ``Units of measure work with longs`` () =
    3L<km/h> + 2L<km/h> |> equal 5L<km/h>

// [<Fact>]
// let ``Units of measure work with decimals`` () =
//     3M<km/h> + 2M<km/h> |> equal 5M<km/h>

[<Fact>]
let ``Abbreviated units of measure work`` () =
    let x = 5.<Measure1>
    let c = MeasureTest()
    c.Method(5.<Measure2>) |> equal x

[<Fact>]
let ``Functions in union fields are uncurried`` () =
    let res = UncurryUnion (-) |> applyUncurryUnion 5 2
    res |> equal 3

[<Fact>]
let ``Functions in class fields are uncurried`` () =
    let adder = TestClass((+))
    let res = adder.Add(2, 3)
    res |> equal 5
