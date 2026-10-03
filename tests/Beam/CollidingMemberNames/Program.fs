module Fable.Tests.CollidingMemberNames

type Foo() = class end

let fooBar () = 1
let foo_bar () = 2
let foo_ctor () = 42

let values () = fooBar (), foo_bar (), foo_ctor ()
