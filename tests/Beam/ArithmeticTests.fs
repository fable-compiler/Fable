module Fable.Tests.Arithmetic

open Fable.Tests.Util
open Util.Testing

[<Fact>]
let ``test Infix add can be generated`` () =
    4 + 2 |> equal 6

[<Fact>]
let ``test Infix subtract can be generated`` () =
    4 - 2 |> equal 2

[<Fact>]
let ``test Infix multiply can be generated`` () =
    4 * 2 |> equal 8

[<Fact>]
let ``test Infix divide can be generated`` () =
    4 / 2 |> equal 2

[<Fact>]
let ``test Integer division doesn't produce floats`` () =
    5. / 2. |> equal 2.5
    5 / 2 |> equal 2
    5 / 3 |> equal 1

[<Fact>]
let ``test Infix modulo can be generated`` () =
    4 % 3 |> equal 1

[<Fact>]
let ``test Evaluation order is preserved by generated code`` () =
    (4 - 2) * 2 + 1 |> equal 5

[<Fact>]
let ``test Adding floats works`` () =
    3.141 + 2.85 |> equal 5.991

#if !FABLE_COMPILER_BEAM // TODO: needs fable-library-beam (int32 module)
[<Fact>]
let ``test Unary negation works`` () =
    let x = 5
    -x |> equal -5
#endif

[<Fact>]
let ``test Unary plus works`` () =
    let x = 5
    +x |> equal 5

[<Fact>]
let ``test Bitwise and can be generated`` () =
    6 &&& 2 |> equal 2

[<Fact>]
let ``test Bitwise or can be generated`` () =
    4 ||| 2 |> equal 6

[<Fact>]
let ``test Bitwise xor can be generated`` () =
    6 ^^^ 2 |> equal 4

[<Fact>]
let ``test Bitwise shift left can be generated`` () =
    4 <<< 2 |> equal 16

[<Fact>]
let ``test Bitwise shift right can be generated`` () =
    4 >>> 2 |> equal 1

[<Fact>]
let ``test Logical and works`` () =
    (true && true) |> equal true
    (true && false) |> equal false
    (false && true) |> equal false
    (false && false) |> equal false

[<Fact>]
let ``test Logical or works`` () =
    (true || true) |> equal true
    (true || false) |> equal true
    (false || true) |> equal true
    (false || false) |> equal false

[<Fact>]
let ``test Logical not works`` () =
    not true |> equal false
    not false |> equal true

#if !FABLE_COMPILER_BEAM // TODO: needs fable-library-beam (big_int module)
[<Fact>]
let ``test Int64 Infix add can be generated`` () =
    4L + 2L |> equal 6L

[<Fact>]
let ``test Int64 Infix subtract can be generated`` () =
    4L - 2L |> equal 2L

[<Fact>]
let ``test Int64 Infix multiply can be generated`` () =
    4L * 2L |> equal 8L

[<Fact>]
let ``test Int64 Infix divide can be generated`` () =
    4L / 2L |> equal 2L
#endif

[<Fact>]
let ``test Equality works`` () =
    (1 = 1) |> equal true
    (1 = 2) |> equal false

[<Fact>]
let ``test Inequality works`` () =
    (1 <> 2) |> equal true
    (1 <> 1) |> equal false

[<Fact>]
let ``test Less than works`` () =
    (1 < 2) |> equal true
    (2 < 1) |> equal false

[<Fact>]
let ``test Greater than works`` () =
    (2 > 1) |> equal true
    (1 > 2) |> equal false

[<Fact>]
let ``test Less than or equal works`` () =
    (1 <= 2) |> equal true
    (1 <= 1) |> equal true
    (2 <= 1) |> equal false

[<Fact>]
let ``test Greater than or equal works`` () =
    (2 >= 1) |> equal true
    (1 >= 1) |> equal true
    (1 >= 2) |> equal false
