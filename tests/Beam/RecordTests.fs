module Fable.Tests.RecordTests

open Util.Testing
open Xunit

type Person = { Name: string; Age: int }
type Point = { X: float; Y: float }
type Nested = { Label: string; Location: Point }

[<Fact>]
let ``simple record creation works`` () =
    let p = { Name = "Alice"; Age = 30 }
    equal "Alice" p.Name
    equal 30 p.Age

[<Fact>]
let ``record update works`` () =
    let p = { Name = "Alice"; Age = 30 }
    let p2 = { p with Age = 31 }
    equal "Alice" p2.Name
    equal 31 p2.Age

[<Fact>]
let ``record with float fields works`` () =
    let pt = { X = 1.0; Y = 2.5 }
    equal 1.0 pt.X
    equal 2.5 pt.Y

[<Fact>]
let ``nested record works`` () =
    let n = { Label = "origin"; Location = { X = 0.0; Y = 0.0 } }
    equal "origin" n.Label
    equal 0.0 n.Location.X

[<Fact>]
let ``record as function parameter works`` () =
    let getName (p: Person) = p.Name
    let alice = { Name = "Alice"; Age = 30 }
    equal "Alice" (getName alice)

[<Fact>]
let ``anonymous record works`` () =
    let r = {| Name = "Bob"; Score = 100 |}
    equal "Bob" r.Name
    equal 100 r.Score

[<Fact>]
let ``record structural equality works`` () =
    let p1 = { Name = "Alice"; Age = 30 }
    let p2 = { Name = "Alice"; Age = 30 }
    equal true (p1 = p2)

[<Fact>]
let ``record structural inequality works`` () =
    let p1 = { Name = "Alice"; Age = 30 }
    let p2 = { Name = "Bob"; Age = 25 }
    equal true (p1 <> p2)

[<Fact>]
let ``nested record equality works`` () =
    let n1 = { Label = "a"; Location = { X = 1.0; Y = 2.0 } }
    let n2 = { Label = "a"; Location = { X = 1.0; Y = 2.0 } }
    equal true (n1 = n2)
