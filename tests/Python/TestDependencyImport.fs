module Fable.Tests.Subdependency


#if FABLE_COMPILER
open System
open Util.Testing
open Thoth.Json.Core
open Thoth.Json.Python
open Fable.Core.PyInterop

/// https://github.com/fable-compiler/Fable/issues/4088#issuecomment-2751410558
[<Fact>]
let ``check correct transpilation of subdependency`` () =
    let decoder = Decode.object (fun get ->
        {|
        name = get.Required.Field "name" Decode.string
        age = get.Required.Field "age" Decode.int
        |}
    )
    let content = @"{""name"":""Alice"",""age"":30}"
    let result = Decode.fromString decoder content
    result
    |> equal (Ok {|name = "Alice"; age = 30|})


/// https://github.com/fable-compiler/Fable/issues/4089
[<Fact>]
let ``check name aliasing of import with name "*" `` () =
    let json = Fable.Python.Json.json.loads @"{""name"":""Alice"",""age"":30}"
    json?name
    |> equal "Alice"

#endif
