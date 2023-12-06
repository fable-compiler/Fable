module Fable.Tests.NonRegression

open Util.Testing

module Issue3496 =
  type Class(length: int) =
    member x.Length = length
    static member StaticLength (length: int) = length
  let returnLength (length: int) = length


[<Fact>]
let testLengthPassedToCtorIsOk() =
  let c = Issue3496.Class(1)
  equal 1 c.Length
  equal 1 (Issue3496.returnLength 1)
  equal 1 (Issue3496.Class.StaticLength 1)


module Issue3640 =
  type X =
      | A of int
      | B of string

[<Fact>]
let ``test hashcodes are unique`` () =
  let x = Issue3640.A 1
  let y = Issue3640.B "hello"

  let xHash = x.GetHashCode()
  let yHash = y.GetHashCode()

  notEqual xHash yHash

  // In Python we need to make sure that both the objects x and y exist
  // at the same time or else they may end up with the same hash code.
  // The code above may be optimized to only create one object at a time
  // and then the hash code will be the same. So we need to reuse the
  // objects to make sure they are not optimized away.
  let xHash = x.GetHashCode()
  let yHash = y.GetHashCode()
  assert (xHash > 0)
  assert (yHash > 0)
