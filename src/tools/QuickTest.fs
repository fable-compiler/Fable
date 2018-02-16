module rec QuickTest

// Use this template to make quick tests when adding new features to Fable.
// You must run a full build at least once (from repo root directory,
// type `sh build.sh` on OSX/Linux or just `build` on Windows). Then:
// - When making changes to Fable.Compiler run `build QuickFableCompilerTest`
// - When making changes to fable-core run `build QuickFableCoreTest`

// Please don't add this file to your commits

// open System
// open Fable.Core
// open Fable.Core.JsInterop
// open Fable.Core.Testing

// let equal expected actual =
//     let areEqual = expected = actual
//     printfn "%A = %A > %b" expected actual areEqual
//     if not areEqual then
//         failwithf "Expected %A but got %A" expected actual

// // Write here your unit test, you can later move it
// // to Fable.Tests project. For example:
// // [<Test>]
// // let ``My Test``() =
// //     Seq.except [2] [1; 3; 2] |> Seq.last |> equal 3
// //     Seq.except [2] [2; 4; 6] |> Seq.head |> equal 4

// // You'll have to run your test manually, sorry!
// // ``My Test``()


// type IFoo =
//     abstract Foo: unit -> int

// type Lib.Foo with
//     member this.XY() = this.Z() + 12

// type Foo2(x: int, y: int) =
//     member __.Z() = x + y
//     member this.Add(x, y) = x + y + this.Z()
//     member __.Add(x) = x * x * x
//     static member Add(x, y) = x - y
//     interface IFoo with
//         member my.Foo() = y

// // let delay (f:unit -> unit) = f
// // let rec a = delay (fun () -> b())
// // and b = delay a

// let test() =
//     let f = Foo2(14, 67)
//     let x = f.Add(4, 5)
//     let y = Lib.Foo.Add(6, 7)
//     let z = { Lib.x = 5 }
//     printfn "RESULT: %i" (x + y)
//     printfn "RESULT2: %i" (f.Add(10))
//     printfn "Extension: %i" (z.XY())

// test()

// let test2(f: IFoo) =
//     f.Foo()

// let print (i: int) = System.Console.WriteLine(i)

// let call2 f g = f 5 (fun x -> g)

// // call2(f, g) => f(5, x => g)

// call2 (fun i f -> f (i + 1)) 3 |> print

// // call2((i, f) => f(i +1))(3)


// call2 (fun i f -> f i 1) (fun x -> x) |> print


// let test_infinity() =
//     [1;2;3;4]
//     |> List.map (fun x -> x + 4)
//     |> List.filter (fun x -> x < 10)
//     |> List.choose (fun x -> Some([x; x]))
//     |> List.concat
//     |> List.fold (fun acc x -> acc + x) 6

let x() = Foo.jar

module Foo =
    let jar = 10