module Fable.Tests.Dart.Misc

open System
open Util

let increase (x: int ref) =
    x.Value <- x.Value + 1

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

let applyUncurried f x y =
    f x y

let getCurrriedFunctionInTuple (tup: int * (int -> int -> int)) y =
    let x, f = tup
    applyUncurried f x y

let getCurrriedFunctionInTuple2 (tup: int * (int -> int -> int)) =
    let x, f = tup
    applyUncurried f x

let getCurrriedGenericFunctionInTuple (tup: int * (int -> 'a)) =
    let x, f = tup
    f x

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

    testCase "Custom computation expressions work" <| fun () ->
        execMaybe 5 |> equal (Some 23)
        execMaybe 99 |> equal None

    testCase "Can pass a curried function in a tuple to another function as argument" <| fun () ->
        let f = getCurrriedFunctionInTuple (20, (/))
        f 6 |> equal 3

        getCurrriedFunctionInTuple (25, (/)) 6 |> equal 4

        let f = getCurrriedFunctionInTuple2 (4, (+))
        f 6 |> equal 10

        let f2 = getCurrriedGenericFunctionInTuple (4, fun x y z -> x + y * z)
        f2 5 7 |> equal 39

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
