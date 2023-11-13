module Fable.Tests.AnonRecordTests

open Util.Testing

let anonRecAcceptingFn (x: {|A: int; B: string|}) =
    {|C = x.A + 1; D = "Z"|}

let structAnonRecAcceptingFn (x: struct {|A: int; B: string|}) =
    struct {|C = x.A + 1; D = "Z"|}

[<Fact>]
let ``test Anonimous records work`` () =
    let r = {| A = 1; B = "hello"; X = 3.141; D = 4|}
    r.A |> equal 1
    r.X |> equal 3.141  //just in case alphabetical ordering is going to throw off index
    r.B |> equal "hello"
    r.D |> equal 4

[<Fact>]
let ``test Anonimous records can pe passed as parameters`` () =
    let m = {| A = 1; B = "hello"|}
    let res = anonRecAcceptingFn m
    res.C |> equal 2
    res.D |> equal "Z"

[<Fact>]
let ``test Anonimous records structural equality works`` () =
    let a = {| A = 1; B = "hello"|}
    let b = {| A = 1; B = "hello"|}
    let c = {| A = 2; B = "test"|}
    a = a |> equal true
    a = b |> equal true
    a = c |> equal false
    b = c |> equal false

[<Fact>]
let ``test Struct anonimous records work`` () =
    let r = struct {| A = 1; B = "hello"; X = 3.141; D = 4|}
    r.A |> equal 1
    r.X |> equal 3.141  //just in case alphabetical ordering is going to throw off index
    r.B |> equal "hello"
    r.D |> equal 4

[<Fact>]
let ``test Struct anonimous records can pe passed as parameters`` () =
    let m = struct {| A = 1; B = "hello"|}
    let res = structAnonRecAcceptingFn m
    res.C |> equal 2
    res.D |> equal "Z"

[<Fact>]
let ``test Struct anonimous records structural equality works`` () =
    let a = struct {| A = 1; B = "hello"|}
    let b = struct {| A = 1; B = "hello"|}
    let c = struct {| A = 2; B = "test"|}
    a = a |> equal true
    a = b |> equal true
    a = c |> equal false
    b = c |> equal false
