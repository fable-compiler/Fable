module Fable.Tests.String

open Util.Testing


[<Fact>]
let ``test interpolate works`` () =
  let name = "Phillip"
  let age = 29
  $"Name: {name}, Age: %i{age}"
  |> equal "Name: Phillip, Age: 29"
