#r "../../../packages/NUnit/lib/nunit.framework.dll"
#load "util/util.fs"

open NUnit.Framework

[<Test>]
let ``Util.reverse works``() =
     let res = Util.reverse "yllihP"
     Assert.AreEqual("Philly", res)

