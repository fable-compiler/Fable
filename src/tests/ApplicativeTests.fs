[<Util.Testing.TestFixture>]
module Fable.Tests.Applicative
open System
open Util.Testing

type Result<'s, 'f> =
    | Ok of 's
    | Error of 'f

    static member (>>=) (r: Result<'t, 'e>, f: 't -> Result<'u, 'e>) : Result<'u, 'e> =
        match r with
        | Error e -> Error e
        | Ok v -> f v

    static member (<^>) (f: 't -> 'u, r: Result<'t, 'e>) : Result<'u, 'e> =
        r >>= (f >> Ok)

    static member (<*>) (f: Result<('t -> 'u), 'e>, r: Result<'t, 'e>) : Result<'u, 'e> =
        f >>= fun f -> f <^> r

[<Test>]
let ``Infix applicative can be generated``() =
    let r = Ok 1
    let a = Ok string
    let r' = match a <*> r with
             | Ok x -> x
             | _ -> failwith "expected Ok"
    Assert.AreEqual ("1", r' )
