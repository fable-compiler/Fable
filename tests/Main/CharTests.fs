[<Util.Testing.TestFixture>]
module Fable.Tests.Char
open System
open Util.Testing
open Fable.Tests.Util
open Fable.Core.JsInterop
open System.Collections.Generic

// System.Char

[<Test>]
let ``Char.ToUpper works``() =
      Char.ToUpper('b') |> equal 'B'

[<Test>]
let ``Char.ToLower works``() =
      Char.ToLower('B') |> equal 'b'

[<Test>]
let ``Char.ToUpperInvariant works``() =
      Char.ToUpperInvariant('b') |> equal 'B'

[<Test>]
let ``Char.ToLowerInvariant works``() =
      Char.ToLowerInvariant('B') |> equal 'b'

[<Test>]
let ``Char.IsLetter works``() =
    equal true (Char.IsLetter('a'))
    equal false (Char.IsLetter('1'))
    equal true (Char.IsLetter('β'))
