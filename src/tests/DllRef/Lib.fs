module Fable.Tests.DllRef.Lib

let 足す x y = x + y
let 引く x y = x - y

// Check that we can refer to an entity with 
// JS non-valid chars from the same file
module internal Foo' =
    let bar' x = if x = 0 then false else true

module モジュール =
    let ファンクション x = Foo'.bar' x
