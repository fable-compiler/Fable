module Fable.Tests.RecordTests

open Fable.Tests.Util
open Util.Testing

type Person = { Name: string; Age: int }
type Point = { X: float; Y: float }
type Nested = { Label: string; Location: Point }

type RecursiveRecord =
    { things : RecursiveRecord list }

type JSKiller =
   { ``for`` : float; ``class`` : float }

// Note: Records with spaces/symbols in field names are not supported in Beam
// as Erlang atoms can't contain these characters

type MutatingRecord =
    { uniqueA: int; uniqueB: int }

type CarInterior = { Seats: int }
type Car = { Interior: CarInterior }

type RecordA =
    { OptionalField : string option }

type CasingRecord =
    { firstName: string; FirstName: string }

[<Fact>]
let ``test simple record creation works`` () =
    let p = { Name = "Alice"; Age = 30 }
    equal "Alice" p.Name
    equal 30 p.Age

[<Fact>]
let ``test record update works`` () =
    let p = { Name = "Alice"; Age = 30 }
    let p2 = { p with Age = 31 }
    equal "Alice" p2.Name
    equal 31 p2.Age

[<Fact>]
let ``test record with float fields works`` () =
    let pt = { X = 1.0; Y = 2.5 }
    equal 1.0 pt.X
    equal 2.5 pt.Y

[<Fact>]
let ``test nested record works`` () =
    let n = { Label = "origin"; Location = { X = 0.0; Y = 0.0 } }
    equal "origin" n.Label
    equal 0.0 n.Location.X

[<Fact>]
let ``test record as function parameter works`` () =
    let getName (p: Person) = p.Name
    let alice = { Name = "Alice"; Age = 30 }
    equal "Alice" (getName alice)

[<Fact>]
let ``test anonymous record works`` () =
    let r = {| Name = "Bob"; Score = 100 |}
    equal "Bob" r.Name
    equal 100 r.Score

[<Fact>]
let ``test record structural equality works`` () =
    let p1 = { Name = "Alice"; Age = 30 }
    let p2 = { Name = "Alice"; Age = 30 }
    equal true (p1 = p2)

[<Fact>]
let ``test record structural inequality works`` () =
    let p1 = { Name = "Alice"; Age = 30 }
    let p2 = { Name = "Bob"; Age = 25 }
    equal true (p1 <> p2)

[<Fact>]
let ``test nested record equality works`` () =
    let n1 = { Label = "a"; Location = { X = 1.0; Y = 2.0 } }
    let n2 = { Label = "a"; Location = { X = 1.0; Y = 2.0 } }
    equal true (n1 = n2)

[<Fact>]
let ``test recursive record does not cause issues`` () =
    let r = { things = [ { things = [] } ] }
    equal r.things.Length 1

[<Fact>]
let ``test record expression constructors can be generated`` () =
    let x = { Name = "Alfonso"; Age = 7 }
    let y = { x with Age = 14 }
    equal "Alfonso" y.Name
    equal 14 y.Age

[<Fact>]
let ``test records with key reserved words are mapped correctly`` () =
    let x = { ``for`` = 1.0; ``class`` = 2.0 }
    equal 2. x.``class``

// test records with special characters - skipped for Beam (Erlang atoms can't have spaces/symbols)

[<Fact>]
let ``test mutating records work`` () =
    let x = { uniqueA = 10; uniqueB = 20 }
    equal 10 x.uniqueA
    equal 20 x.uniqueB
    let uniqueB' = -x.uniqueB
    let x' = { x with uniqueB = uniqueB' }
    equal 10 x.uniqueA
    equal 10 x'.uniqueA
    equal -20 x'.uniqueB
    let x'' = { x' with uniqueA = -10 }
    equal -10 x''.uniqueA
    equal -20 x''.uniqueB

[<Fact>]
let ``test record equality when it has optional field`` () =
    let a = { OptionalField = None }
    let b = { OptionalField = None }
    let c = { OptionalField = Some "test" }
    equal a b
    equal true (a = b)
    equal false (a = c)
    equal false (c = b)

// TODO: Erlang converts both firstName and FirstName to first_name (snake_case collision)
// [<Fact>]
// let ``test record with both camel-case and pascal-case fields do not conflict`` () =
//     let x = { firstName = "John"; FirstName = "Jane" }
//     equal "John" x.firstName
//     equal "Jane" x.FirstName

[<Fact>]
let ``test anonymous records work with functions`` () =
    let r = {| X = 5; Y = "Foo"; F = fun x y -> x + y |}
    r.F r.X 3 |> equal 8
    r.Y |> equal "Foo"

[<Fact>]
let ``test anonymous record equality works`` () =
    let x = {| Foo = "baz"; Bar = 23 |}
    let y = {| Foo = "baz"; Bar = 23 |}
    equal true (x = y)
    let z = {| Foo = "baz"; Bar = 14 |}
    equal false (x = z)
