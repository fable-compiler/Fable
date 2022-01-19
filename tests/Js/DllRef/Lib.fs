module Fable.Tests.DllRef.Lib

#if FABLE_COMPILER
/// Including JS files in compilation works
let foo: string = Fable.Core.JsInterop.importMember "./js1/lib.js"

/// Including JS files with same name works
[<Fable.Core.Import("fooGenerator","./js2/lib.js")>]
let fooGenerator(i: int): string = Fable.Core.Util.jsNative
#else
let foo = "foo"
let fooGenerator(i: int): string =
    String.replicate i "foo"
#endif

let createClampedArray() = [|5uy|]

// These must be in a separate file from the unit tests (see issue #482)
let ``$5EAfoo``  = "bar1"
let ``$5E$Afoo`` = "bar2"
let ``$5EA$foo`` = "bar3"
let ``^Afoo``    = "bar4"
let ``תfoo``     = "bar5"
let ``foo$5EA``  = "bar6"
let ``foo$5E$A`` = "bar7"
let ``foo$5EA$`` = "bar8"
let ``foo^A``    = "bar9"
let ``fooת``     = "bar10"

let 足す x y = x + y
let 引く x y = x - y

// Check that we can refer to an entity with
// JS non-valid chars from the same file
module internal Foo' =
    let bar' x = if x = 0 then false else true

module モジュール =
    let ファンクション x = Foo'.bar' x

    #if FABLE_COMPILER
    [<Fable.Core.Import("one", "./numbers.js")>]
    let one: int = Fable.Core.Util.jsNative
    let three: int = Fable.Core.JsInterop.importMember "./numbers.js"
    #else
    let one: int = 1
    let three: int = 3
    #endif

