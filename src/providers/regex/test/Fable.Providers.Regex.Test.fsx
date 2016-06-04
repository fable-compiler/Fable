[<NUnit.Framework.TestFixture>]
module Fable.Providers.Regex.Test

#r "../Fable.Providers.Regex.dll"
#r "../../../../packages/NUnit/lib/nunit.framework.dll"

open NUnit.Framework
open Fable.Providers.Regex

[<Test>]
let ``SafeRegex works``() =
    let reg1 = SafeRegex.Create<"^hello world!?$">()
    let reg2 = SafeRegex.Create<"^hello world!?$", ignoreCase=true>()
    Assert.AreEqual(true, reg1.IsMatch("hello world!"))
    Assert.AreEqual(false, reg1.IsMatch("Hello World!"))
    Assert.AreEqual(true, reg2.IsMatch("Hello World!"))
