module Fable.Tests.ReflectionUnionImports

open FSharp.Reflection
open Util.Testing

type RecordWithResult = { Value: Result<int, string> }

let getGenericTypeArguments (typ: System.Type) = typ.GenericTypeArguments

[<Fact>]
let ``test FSharp.Reflection: record field Result imports its case constructors`` () =
    let field = FSharpType.GetRecordFields(typeof<RecordWithResult>).[0]
    field.PropertyType |> equal typeof<Result<int, string>>

[<Fact>]
let ``test FSharp.Reflection: Choice imports its case constructors`` () =
    getGenericTypeArguments typeof<Choice<int, string>>
    |> Array.length
    |> equal 2
