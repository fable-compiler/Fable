module Fable.Tests.NonRegression

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

    // In Python we need to make sure that both the objects x and y exist
    // at the same time or else they may end up with the same hash code.
    // The code above may be optimized to only create one object at a time
    // and then the hash code will be the same. So we need to reuse the
    // objects to make sure they are not optimized away.
    let xHash = x.GetHashCode()
    let yHash = y.GetHashCode()
    assert (xHash > 0)
    assert (yHash > 0)

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

    // In Python we need to make sure that both the objects x and y exist
    // at the same time or else they may end up with the same hash code.
    // The code above may be optimized to only create one object at a time
    // and then the hash code will be the same. So we need to reuse the
    // objects to make sure they are not optimized away.
    let xHash = x.GetHashCode()
    let yHash = y.GetHashCode()
    assert (xHash > 0)
    assert (yHash > 0)


let ``test Nested type with Custom Hashcode works`` () =
    let x =
        {
            Issue3674.ID = 1
            Issue3674.Name = "a"
        }

    let y = Issue3674.X x

    assert (x.GetHashCode() > 0)
    assert (y.GetHashCode() > 0)
