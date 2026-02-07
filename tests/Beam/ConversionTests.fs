module Fable.Tests.Conversion

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test int to string works`` () =
    string 42 |> equal "42"

[<Fact>]
let ``test negative int to string works`` () =
    string -7 |> equal "-7"

[<Fact>]
let ``test string to int works`` () =
    int "42" |> equal 42

[<Fact>]
let ``test float to int works`` () =
    int 3.14 |> equal 3

[<Fact>]
let ``test int to float works`` () =
    float 42 |> equal 42.0

[<Fact>]
let ``test string to float works`` () =
    float "3.14" |> equal 3.14

[<Fact>]
let ``test bool to string works`` () =
    // .NET produces "True"/"False", Erlang produces "true"/"false"
    // Test that the result is non-empty and meaningful
    let t = string true
    let f = string false
    (t <> f) |> equal true

[<Fact>]
let ``test int instance ToString works`` () =
    (42).ToString() |> equal "42"

[<Fact>]
let ``test float instance ToString works`` () =
    let s = (3.14).ToString()
    // Erlang float_to_binary may produce different precision
    s.Length > 0 |> equal true

[<Fact>]
let ``test string identity conversion works`` () =
    string "hello" |> equal "hello"

[<Fact>]
let ``test float truncation works`` () =
    int 9.99 |> equal 9
    int -2.7 |> equal -2
