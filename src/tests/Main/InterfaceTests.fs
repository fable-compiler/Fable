[<Util.Testing.TestFixture>]
module Fable.Tests.Interfaces

open System
open FSharp.Core.LanguagePrimitives
open Util.Testing
open Fable.Tests.Util

// TODO: Moar tests
// Test object expression for interface
// Upcast class to interface
// Downcast class to interface
// Type test class for interface
// Type test interface for interface
// Type test interface for class
// Inherited interfaces
// IEnumerable implementations

type IFoo =
    abstract Foo: int -> string

[<AbstractClass>]
type Abstract() =
    abstract member Foo : float -> string
    interface IFoo with
        member this.Foo i =
            float i * 2. |> this.Foo

type Impl() =
    inherit Abstract()
    override this.Foo(f) =
        f + 5. |> string

[<Test>]
let ``Interfaces in abstract classes work``() =
    let x = Impl()
    x.Foo 3. |> equal "8"
    (x :> IFoo).Foo 3 |> equal "11"