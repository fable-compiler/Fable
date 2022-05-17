module Fable.Tests.Dart.Misc

open System
open Util

let increase (x: int ref) =
    x.Value <- x.Value + 1

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

type UncurryUnion =
    | UncurryUnion of add: (int -> int -> int)

let applyUncurryUnion x y = function
    | UncurryUnion f -> f x y

type ElmishWidget<'Model, 'View, 'Msg>(view: 'Model -> ('Msg -> unit) -> 'View) =
    let _view = view
    member _.Render(model, dispatch) = _view model dispatch

let tests() =
    testCase "ref works" <| fun () ->
        let x = ref 5
        increase x
        x.Value |> equal 6

    testCase "Units of measure work" <| fun () ->
        3<km/h> + 2<km/h> |> equal 5<km/h>

        let v1 = { x = 4.3<mi>; y = 5.<mi>; z = 2.8<mi> }
        let v2 = { x = 5.6<mi>; y = 3.8<mi>; z = 0.<mi> }
        let v3 = v1 + v2
        equal 8.8<mi> v3.y

    testCase "Units of measure work with longs" <| fun () ->
        3L<km/h> + 2L<km/h> |> equal 5L<km/h>

    // TODO: decimals
    // testCase "Units of measure work with decimals" <| fun () ->
    //     3M<km/h> + 2M<km/h> |> equal 5M<km/h>

    testCase "Abbreviated measures work" <| fun () -> // #2313
        let x = 5.<Measure1>
        let c = MeasureTest()
        c.Method(5.<Measure2>) |> equal x

    testCase "Can get uncurried functions from unions" <| fun () ->
        UncurryUnion (-) |> applyUncurryUnion 5 2 |> equal 3

    testCase "Class fields are uncurried" <| fun () ->
        let mutable count = 1.2
        let w = ElmishWidget<int, string, float>(fun model dispatch ->
            dispatch 0.5
            (model + 5).ToString())
        w.Render(7, fun f -> count <- count + f) |> equal "12"
        equal 1.7 count
