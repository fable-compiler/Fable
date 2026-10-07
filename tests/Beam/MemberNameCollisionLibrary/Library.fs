module Fable.Tests.MemberNameCollisionLibrary

type Response() =
    let mutable status = 0

    member _.StatusCode
        with get () = status
        and set (value: int) = status <- value

    member _.SetStatusCode(value: int) = status <- value + 1
    member _.GetStatusCode() = status + 2

let propertyPath () =
    let response = Response()
    response.StatusCode <- 201
    response.StatusCode

let methodPath () =
    let response = Response()
    response.SetStatusCode 201
    response.StatusCode

let getterMethodPath () =
    let response = Response()
    response.StatusCode <- 201
    response.GetStatusCode()
