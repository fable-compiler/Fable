module Fable.Tests.GuidTests

open Util.Testing
open System

[<Fact>]
let ``Guid constructors works`` () =
    let g = new Guid()
    g.ToString() |> equal "00000000-0000-0000-0000-000000000000"
    let g2 = new Guid("a3cc8928-7148-43e2-a863-a3954aea02df")
    g2.ToString() |> equal "a3cc8928-7148-43e2-a863-a3954aea02df"

[<Fact>]
let ``Guid.Empty works`` () =
    let g = Guid.Empty
    g.ToString() |> equal "00000000-0000-0000-0000-000000000000"

[<Fact>]
let ``Guid.NewGuid creates different guids`` () =
    let g = Guid.NewGuid()
    let g2 = Guid.NewGuid()
    g = g2 |> equal false

[<Fact>]
let ``Guid equality works`` () =
    let s = "f46d24f0-183e-4fca-9f47-e382afdfde8b"
    let g = Guid.Parse(s)
    let g2 = Guid.Parse("a3cc8928-7148-43e2-a863-a3954aea02df")
    let g3 = Guid.Parse(s)
    g = g |> equal true
    g = g2 |> equal false
    g = g3 |> equal true

[<Fact>]
let ``Guid.ToString works`` () =
    let g = Guid.Parse("a3cc8928-7148-43e2-a863-a3954aea02df")
    g.ToString() |> equal "a3cc8928-7148-43e2-a863-a3954aea02df"

[<Fact>]
let ``Guid.NewGuid works`` () =
    let g1 = Guid.NewGuid()
    let g2 = Guid.NewGuid()
    g1 = g2 |> equal false
    let s1 = string g1
    equal 36 s1.Length
    Text.RegularExpressions.Regex.IsMatch(
        s1, "^[a-f0-9]{8}(?:-[a-f0-9]{4}){3}-[a-f0-9]{12}$")
    |> equal true
    let g3 = Guid.Parse s1
    g1 = g3 |> equal true

// id is prefixed for guid creation as we check at compile time (if able) to create a string const
[<Fact>]
let ``Guid.Parse works`` () =
    let guids = [
        Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
        Guid.Parse(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")
        Guid.Parse("96258006c4ba4a7f80c4de7f2b2898c5")
        Guid.Parse(id "96258006c4ba4a7f80c4de7f2b2898c5")
        Guid.Parse("{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
        Guid.Parse(id "{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
        // Guid.Parse("(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
        // Guid.Parse(id "(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
        // Guid.Parse("{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
        // Guid.Parse(id "{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
        Guid("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
        Guid(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")
        Guid("96258006c4ba4a7f80c4de7f2b2898c5")
        Guid(id "96258006c4ba4a7f80c4de7f2b2898c5")
        Guid("{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
        Guid(id "{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
        // Guid("(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
        // Guid(id "(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
        // Guid("{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
        // Guid(id "{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
    ]
    guids
    |> List.iter (fun g -> g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5")

[<Fact>]
let ``Guid.Parse fails if string is not well formed`` () =
    throwsAnyError (fun () -> Guid.Parse(id "foo"))

[<Fact>]
let ``Guid.TryParse works`` () =
    let successGuids = [
        Guid.TryParse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
        Guid.TryParse(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")
        Guid.TryParse("96258006c4ba4a7f80c4de7f2b2898c5")
        Guid.TryParse(id "96258006c4ba4a7f80c4de7f2b2898c5")
        Guid.TryParse("{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
        Guid.TryParse(id "{96258006-c4ba-4a7f-80c4-de7f2b2898c5}")
        // Guid.TryParse("(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
        // Guid.TryParse(id "(96258006-c4ba-4a7f-80c4-de7f2b2898c5)")
        // Guid.TryParse("{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
        // Guid.TryParse(id "{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}")
    ]
    let failGuids = [
        Guid.TryParse("96258006-c4ba-4a7f-80c4")
        Guid.TryParse(id "96258006-c4ba-4a7f-80c4")
        Guid.TryParse("96258007f80c4de7f2b2898c5")
        Guid.TryParse(id "96258007f80c4de7f2b2898c5")
        Guid.TryParse("{96258006-c4ba-4a7f-80c4}")
        Guid.TryParse(id "{96258006-c4ba-4a7f-80c4}")
        // Guid.TryParse("(96258006-c4ba-80c4-de7f2b2898c5)")
        // Guid.TryParse(id "(96258006-c4ba-80c4-de7f2b2898c5)")
        // Guid.TryParse("{0x96258006,0xc4ba,{0x80,0xc4,0xde,0x7f,0x28,0x98,0xc5}}")
        // Guid.TryParse(id "{0x96258006,0xc4ba,{0x80,0xc4,0xde,0x7f,0x28,0x98,0xc5}}")
    ]
    successGuids
    |> List.iter (fst >> (equal true))
    failGuids
    |> List.iter (fst >> (equal false))

[<Fact>]
let ``Parsed guids with different case are considered the same`` () = // See #1718
    let aGuid = Guid.NewGuid()
    let lower = aGuid.ToString().ToLower()
    let upper = aGuid.ToString().ToUpper()
    lower = upper |> equal false
    let lowerGuid = Guid.Parse lower
    let upperGuid = Guid.Parse upper
    lowerGuid = upperGuid |> equal true

[<Fact>]
let ``Convert Guid to byte array works`` () =
    let g = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    let g2 = Guid.Parse(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")
    g.ToByteArray() |> equal [|6uy; 128uy; 37uy; 150uy; 186uy; 196uy; 127uy; 74uy; 128uy; 196uy; 222uy; 127uy; 43uy; 40uy; 152uy; 197uy|]
    g2.ToByteArray() |> equal [|6uy; 128uy; 37uy; 150uy; 186uy; 196uy; 127uy; 74uy; 128uy; 196uy; 222uy; 127uy; 43uy; 40uy; 152uy; 197uy|]

[<Fact>]
let ``Convert byte array to Guid works`` () =
    let g = Guid [|6uy; 128uy; 37uy; 150uy; 186uy; 196uy; 127uy; 74uy; 128uy; 196uy; 222uy; 127uy; 43uy; 40uy; 152uy; 197uy|]
    g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"

// [<Fact>]
// let ``Guid.ToString works with formats`` () =
//     let g = Guid.Parse("96258006-c4ba-4a7f-80c4-de7f2b2898c5")
//     let g2 = Guid.Parse(id "96258006-c4ba-4a7f-80c4-de7f2b2898c5")
//     let testGuid (g: Guid) =
//         g.ToString() |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"
//         g.ToString("N") |> equal "96258006c4ba4a7f80c4de7f2b2898c5"
//         g.ToString("D") |> equal "96258006-c4ba-4a7f-80c4-de7f2b2898c5"
//         g.ToString("B") |> equal "{96258006-c4ba-4a7f-80c4-de7f2b2898c5}"
//         g.ToString("P") |> equal "(96258006-c4ba-4a7f-80c4-de7f2b2898c5)"
//         g.ToString("X") |> equal "{0x96258006,0xc4ba,0x4a7f,{0x80,0xc4,0xde,0x7f,0x2b,0x28,0x98,0xc5}}"
//     testGuid g
//     testGuid g2
