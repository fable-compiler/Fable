module Fable.Tests.UnsupportedNumericLiteral

let value = Unchecked.defaultof<System.Half>

type CollidingRecord =
    { ``foo-bar``: int
      foo_bar: int }
