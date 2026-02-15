module Fable.Tests.Nullness

open Util.Testing

type Person =
    { First: string
      Last: string }

[<Fact>]
let ``test Lists of nullable records works`` () =
    let args: (Person | null) list =
        [
            { First = "A"; Last = "B" }
            null
            { First = "C"; Last = "D" }
            { First = "E"; Last = "F" }
            null
        ]
    let nullCount = args |> List.filter isNull |> List.length
    nullCount |> equal 2

[<Fact>]
let ``test isNull works with null`` () =
    let s: string | null = null
    isNull s |> equal true

[<Fact>]
let ``test isNull works with non-null`` () =
    let s: string | null = "hello"
    isNull s |> equal false

[<Fact>]
let ``test null record in list works`` () =
    let p: Person | null = null
    isNull p |> equal true
    let p2: Person | null = { First = "A"; Last = "B" }
    isNull p2 |> equal false

[<Fact>]
let ``test Nullness works with generics`` () =
    let findOrNull (index: int) (list: 'T list) : 'T | null when 'T: not struct =
        match List.tryItem index list with
        | Some item -> item
        | None -> null

    equal (findOrNull 1 [ "a"; "b"; "c" ]) "b"
    equal (findOrNull 3 [ "a"; "b"; "c" ]) null
