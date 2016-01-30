[<NUnit.Framework.TestFixture>] 
module Fabel.Tests.AsyncTests
open System
open NUnit.Framework

[<Test>]
let ``Async works``() =
    Assert.AreEqual (4 + 2, 6)
