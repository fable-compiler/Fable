module Fable.Tests.NonRegression

open Util.Testing

module Issue3496 =
  type Class(length: int) =
    member x.Length = length
    static member StaticLength (length: int) = length
  let returnLength (length: int) = length
  
  
  [<Fact>]
  let testLengthPassedToCtorIsOk() =
    let c = Class(1)
    equal 1 c.Length
    equal 1 (returnLength 1)
    equal 1 (Class.StaticLength 1)