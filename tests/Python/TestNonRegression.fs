module Fable.Tests.NonRegression

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

module Issue3811 =
    type FlowchartDirection =
    | TB
    | TD

    [<AttachMembers>]
    type flowchartDirection =
        static member tb = FlowchartDirection.TB
        static member td = FlowchartDirection.TD
        static member tb' () =
            flowchartDirection.tb

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

    // In Python we need to make sure that both the objects x and y exist
    // at the same time or else they may end up with the same hash code.
    // The code above may be optimized to only create one object at a time
    // and then the hash code will be the same. So we need to reuse the
    // objects to make sure they are not optimized away.
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

module Issue3717 =
    [<CustomEquality; NoComparison>]
    type X =
        {
            ID : int
            Name : string
        }

        override x.GetHashCode() = x.ID

        override this.Equals(other : obj) =
            match other with
            | :? X as other -> this.ID = other.ID
            | _ -> false

    // Record type here (unlike issue #3674)
    type Y =
        {X : X}

[<Fact>]
let ``test nested type with custom equality works`` () =

    // Should be equal according to custom equality
    let x1 = {Issue3717.ID = 1; Issue3717.Name = "a"}
    let x2 = {Issue3717.ID = 1; Issue3717.Name = "b"}

    // Should all be equal according to custom equality of inner type
    let y1 = {Issue3717.X = x1}
    let y2 = {Issue3717.X = x2}
    let y3 = {Issue3717.X = x1}

    equal x1 x2
    equal y1 y2
    equal y1 y3

[<CustomEquality>]
[<NoComparison>]
type MyRecord =

    {Name : string; Age: int }

    override this.Equals(that) =
        this.GetHashCode() = that.GetHashCode()

    /// Hash should just return age of person
    override this.GetHashCode() =
        this.Age

[<Fact>]
let ``test custom equality and hashcode works`` () =
    let p1 = {Name = "John"; Age = 30}

    equal 30 (p1.GetHashCode())

[<Fact>]
let ``test class name casing`` () =
    let x = Issue3811.flowchartDirection.tb' ()
    equal Issue3811.FlowchartDirection.TB x

module Issue3972 =
    type IInterface =
        abstract member LOL : int

[<Fact>]
let ``test with interfaces does not not lead to incorrect pattern matching`` () =
    let sideEffect () = ()
    let typeMatchSomeBoxedObject (o:obj) =
        match o with
        | :? int -> 1
        | :? Issue3972.IInterface ->
            sideEffect () // To avoid any code optimizations
            2
        | _ -> 3

    equal (typeMatchSomeBoxedObject "lol") 3
