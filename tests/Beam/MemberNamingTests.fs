module Fable.Tests.MemberNamingTests

open Fable.Tests.Util
open Util.Testing
open Fable.Tests.MemberNameCollisionLibrary

[<Fact>]
let ``test Same-file property setter keeps its own body`` () =
    propertyPath () |> equal 201

[<Fact>]
let ``test Same-file ordinary setter method keeps its own body`` () =
    methodPath () |> equal 202

[<Fact>]
let ``test Same-file ordinary getter method keeps its own body`` () =
    getterMethodPath () |> equal 203

[<Fact>]
let ``test Referenced-library property setter resolves across files`` () =
    let response = Response()
    response.StatusCode <- 201
    response.StatusCode |> equal 201

[<Fact>]
let ``test Referenced-library ordinary setter method resolves across files`` () =
    let response = Response()
    response.SetStatusCode 201
    response.StatusCode |> equal 202

[<Fact>]
let ``test Referenced-library ordinary getter method resolves across files`` () =
    let response = Response()
    response.StatusCode <- 201
    response.GetStatusCode() |> equal 203

[<Fact>]
let ``test Referenced-library method reference keeps the ordinary method body`` () =
    let response = Response()
    let setStatus = response.SetStatusCode
    setStatus 201
    response.StatusCode |> equal 202
