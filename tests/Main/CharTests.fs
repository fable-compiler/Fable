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
//     equal true (Char.IsLetter('家')) Non-European alphabets don't work

[<Test>]
let ``Char.IsLetter with two args works``() =
    let str = "al1"
    equal true  (Char.IsLetter(str,0))
    equal false (Char.IsLetter(str,2))

[<Test>]
let ``Char.IsDigit works``() =
    equal false (Char.IsDigit('a'))
    equal true  (Char.IsDigit('1'))

[<Test>]
let ``Char.IsDigit with two args works``() =
    let str = "ba6"
    equal false (Char.IsDigit(str,0))
    equal true  (Char.IsDigit(str,2))

[<Test>]
let ``Char.IsLetterOrDigit works``() =
    equal true  (Char.IsLetterOrDigit('a'))
    equal true  (Char.IsLetterOrDigit('1'))
    equal true  (Char.IsLetterOrDigit('β'))
    equal false (Char.IsLetterOrDigit('-'))

[<Test>]
let ``Char.IsLetterOrDigit with two args works``() =
    let str = "x-6"
    equal true  (Char.IsLetterOrDigit(str,0))
    equal false (Char.IsLetterOrDigit(str,1))

[<Test>]
let ``Char.IsUpper works``() =
    equal true  (Char.IsUpper('A'))
    equal false (Char.IsUpper('b'))
    equal false (Char.IsUpper('2'))

[<Test>]
let ``Char.IsUpper with two args works``() =
    let str = "Ab2"
    equal true  (Char.IsUpper(str,0))
    equal false (Char.IsUpper(str,1))

[<Test>]
let ``Char.IsLower works``() =
    equal false (Char.IsLower('A'))
    equal true  (Char.IsLower('b'))
    equal false (Char.IsLower('2'))

[<Test>]
let ``Char.IsLower with two args works``() =
    let str = "Ab2"
    equal false (Char.IsLower(str,0))
    equal true  (Char.IsLower(str,1))

[<Test>]
let ``Char.IsWhitespace works``() = 
    equal true (Char.IsWhiteSpace(' '))
    equal true (Char.IsWhiteSpace('\n'))
    equal true (Char.IsWhiteSpace('\t'))

[<Test>]
let ``Char.IsWhitespace works with two args``() = 
    let input = " \r"
    equal true (Char.IsWhiteSpace(input, 0))
    equal true (Char.IsWhiteSpace(input, 1))