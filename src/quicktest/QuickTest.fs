module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

open System
open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Fable.Core.Testing

let equal expected actual =
    let areEqual = expected = actual
    printfn "%A = %A > 2dd%b" expected actual areEqual

    if not areEqual then
        failwithf "[ASSERT ERROR] Expected %A but got %A" expected actual

// let x = 12
// let nullableX = Nullable<int> x

// let x2 = 12
// let nullableX2 = Nullable x

// let mutable a = Nullable 42
// a <- Nullable()

// open System
open FSharp.Linq

// let nullableFloat = Nullable 10.0
// let standardString = float nullableFloat
//
// // let standardString = string nullableString
// equal (Nullable.float (Nullable 1uy)) (Nullable 1.0)
// equal (Nullable.float (Nullable 2y)) (Nullable 2.0)
// equal (Nullable.float (Nullable 3s)) (Nullable 3.0)
// equal (Nullable.float (Nullable 4us)) (Nullable 4.0)
// equal (Nullable.float (Nullable 5)) (Nullable 5.0)
// equal (Nullable.float (Nullable 6u)) (Nullable 6.0)
// equal (Nullable.float (Nullable 7L)) (Nullable 7.0)
// equal (Nullable.float (Nullable 8UL)) (Nullable 8.0)
// equal (Nullable.float (Nullable 9m)) (Nullable 9.0)
// equal (Nullable.float (Nullable 10.0)) (Nullable 10.0)
// equal (Nullable.float (Nullable 11.0f)) (Nullable 11.0)
// equal (Nullable.float (Nullable 'c')) (Nullable 99.0)
// equal (Nullable.float (Nullable.enum(Nullable 2 ): Nullable<DayOfWeek>)) (Nullable 2.0)
//
//
// equal (Nullable.float32 (Nullable 1uy)) (Nullable 1.0f)
// equal (Nullable.float32 (Nullable 2y)) (Nullable 2.0f)
// equal (Nullable.float32 (Nullable 3s)) (Nullable 3.0f)
// equal (Nullable.float32 (Nullable 4us)) (Nullable 4.0f)
// equal (Nullable.float32 (Nullable 5)) (Nullable 5.0f)
// equal (Nullable.float32 (Nullable 6u)) (Nullable 6.0f)
// equal (Nullable.float32 (Nullable 7L)) (Nullable 7.0f)
// equal (Nullable.float32 (Nullable 8UL)) (Nullable 8.0f)
// equal (Nullable.float32 (Nullable 9m)) (Nullable 9.0f)
// equal (Nullable.float32 (Nullable 10.0)) (Nullable 10.0f)
// equal (Nullable.float32 (Nullable 11.0f)) (Nullable 11.0f)
// equal (Nullable.float32 (Nullable 'c')) (Nullable 99.0f)
// equal (Nullable.float32 (Nullable.enum(Nullable 2 ): Nullable<DayOfWeek>)) (Nullable 2.0f)
//
// let float32Value = float32 2L
// // float32(1y) |> equal 2.0f

// equal (Nullable.char (Nullable 49uy)) (Nullable '1')
// equal (Nullable.char (Nullable 50y)) (Nullable '2')
// equal (Nullable.char (Nullable 51s)) (Nullable '3')
// equal (Nullable.char (Nullable 52us)) (Nullable '4')
// equal (Nullable.char (Nullable 53)) (Nullable '5')
// equal (Nullable.char (Nullable 54u)) (Nullable '6')
// equal (Nullable.char (Nullable 55L)) (Nullable '7')
// equal (Nullable.char (Nullable 56UL)) (Nullable '8')
// equal (Nullable.char (Nullable 57m)) (Nullable '9')
// equal (Nullable.char (Nullable 58.0)) (Nullable ':')
// equal (Nullable.char (Nullable 59.0f)) (Nullable ';')
// equal (Nullable.char (Nullable 'a')) (Nullable 'a')
//

// let c = char 57m
//
// let test : decimal = 57m
//
// let t : char = Decimal.op_Explicit 57m

let test2 = uint16 655356

let test = 65535us

let te = char 57m

printfn "%A" te
