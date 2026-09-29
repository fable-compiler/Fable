module Fable.Tests.UnsupportedNumericLiteral

let value = Unchecked.defaultof<System.Half>

type CollidingRecord =
    { ``foo-bar``: int
      foo_bar: int }

let collidingAnonymousRecord = {| ``foo-bar`` = 1; foo_bar = 2 |}
