module Fable.Tests.NonRegression

open System
open Fable.Core
open Util.Testing

module Issue3496 =
    type Class(length: int) =
        member x.Length = length
        static member StaticLength(length: int) = length

    let returnLength (length: int) = length


[<Fact>]
let testLengthPassedToCtorIsOk () =
    let c = Issue3496.Class(1)
    equal 1 c.Length
    equal 1 (Issue3496.returnLength 1)
    equal 1 (Issue3496.Class.StaticLength 1)


module Issue3640 =
    type X =
        | A of int
        | B of string

module Issue3648 =
    type X =
        {
            A: int
            B: string
        }

module Issue3674 =
    [<CustomEquality; NoComparison>]
    type X =
        {
            ID: int
            Name: string
        }

        override x.GetHashCode() = x.ID

        override this.Equals(other: obj) =
            match other with
            | :? X as other -> this.ID = other.ID
            | _ -> false

    type Y = | X of X

[<Fact>]
let ``test hashcodes are unique`` () =
    let x = Issue3640.A 1
    let y = Issue3640.B "hello"

    let xHash = x.GetHashCode()
    let yHash = y.GetHashCode()

    notEqual xHash yHash

    let xHash = x.GetHashCode()
    let yHash = y.GetHashCode()
    xHash <> 0 |> equal true
    yHash <> 0 |> equal true

[<Fact>]
let ``test record hashcodes are unique`` () =
    let x =
        {
            Issue3648.A = 1
            Issue3648.B = "hello"
        }

    let y =
        {
            Issue3648.A = 2
            Issue3648.B = "world"
        }

    let xHash = x.GetHashCode()
    let yHash = y.GetHashCode()

    notEqual xHash yHash

    let xHash = x.GetHashCode()
    let yHash = y.GetHashCode()
    xHash <> 0 |> equal true
    yHash <> 0 |> equal true

[<Fact>]
let ``test Nested type with Custom Hashcode works`` () =
    let x =
        {
            Issue3674.ID = 1
            Issue3674.Name = "a"
        }

    let y = Issue3674.X x

    x.GetHashCode() <> 0 |> equal true
    y.GetHashCode() <> 0 |> equal true

module Issue4125 =
    let none () : unit option =
        None

[<Fact>]
let ``test issue 4125`` () =
    let x = Issue4125.none ()
    equal None x

module Issue3912 =
    type X() =
        let mutable _disposed = false

        member this.IsDisposed = _disposed

        interface System.IDisposable with
            member this.Dispose() =
                _disposed <- true

[<Fact>]
let ``test issue 3912`` () =
    let x = new Issue3912.X()

    let () =
        use x = x
        ()
    equal x.IsDisposed true
