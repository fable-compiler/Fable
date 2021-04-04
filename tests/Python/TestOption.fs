module Fable.Tests.Option

open Util.Testing


[<Fact>]
let ``test defaultArg works`` () =
    let f o = defaultArg o 5
    f (Some 2) |> equal 2
    f None |> equal 5

[<Fact>]
let ``test Option.defaultValue works`` () =
    let a = Some "MyValue"
    let b = None

    a |> Option.defaultValue "" |> equal "MyValue"
    b |> Option.defaultValue "default" |> equal "default"

[<Fact>]
let ``test Option.defaultValue works II`` () =
    Some 5 |> Option.defaultValue 4 |> equal 5
    None |> Option.defaultValue "foo" |> equal "foo"

[<Fact>]
let ``test Option.orElse works`` () =
    Some 5 |> Option.orElse (Some 4) |> equal (Some 5)
    None |> Option.orElse (Some "foo") |> equal (Some "foo")

[<Fact>]
let ``test Option.defaultWith works`` () =
    Some 5 |> Option.defaultWith (fun () -> 4) |> equal 5
    None |> Option.defaultWith (fun () -> "foo") |> equal "foo"

[<Fact>]
let ``test Option.orElseWith works`` () =
    Some 5 |> Option.orElseWith (fun () -> Some 4) |> equal (Some 5)
    None |> Option.orElseWith (fun () -> Some "foo") |> equal (Some "foo")

[<Fact>]
let ``test Option.isSome/isNone works`` () =
    let o1 = None
    let o2 = Some 5
    Option.isNone o1 |> equal true
    Option.isSome o1 |> equal false
    Option.isNone o2 |> equal false
    Option.isSome o2 |> equal true

[<Fact>]
let ``test Option.IsSome/IsNone works II`` () =
    let o1 = None
    let o2 = Some 5
    o1.IsNone |> equal true
    o1.IsSome |> equal false
    o2.IsNone |> equal false
    o2.IsSome |> equal true

// [<Fact>]
// let ``test Option.iter works`` () =
//     let mutable res = false
//     let getOnlyOnce =
//         let mutable value = Some "Hello"
//         fun () -> match value with Some x -> value <- None; Some x | None -> None
//     getOnlyOnce() |> Option.iter (fun s -> if s = "Hello" then res <- true)
//     equal true res

[<Fact>]
let ``test Option.map works`` () =
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.map ((+) "Hello ") |> equal (Some "Hello Alfonso")
    getOnlyOnce() |> Option.map ((+) "Hello ") |> equal None
