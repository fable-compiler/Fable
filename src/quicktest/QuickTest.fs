module QuickTest

// Run `dotnet fsi build.fsx quicktest` and then add tests to this file,
// when you save they will be run automatically with latest changes in compiler.
// When everything works, move the tests to the appropriate file in tests/Main.
// Please don't add this file to your commits.

type U = Z of value: int | Y of name: string

let u1 = Z 1
let u2 = Y "abc"

let a : U = Fable.Core.PyInterop.emitPyExpr 1 "U.Z($0)"
let b : U = Fable.Core.PyInterop.emitPyExpr "abc" "U.Y($0)"
Fable.Core.Testing.Assert.AreEqual(a, u1)
Fable.Core.Testing.Assert.AreEqual(b, u2)

