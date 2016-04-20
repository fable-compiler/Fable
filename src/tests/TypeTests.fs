[<NUnit.Framework.TestFixture>]
module Fable.Tests.TypeTests

open NUnit.Framework
open Fable.Tests.Util

type TestType =
    | Union1 of string

[<Test>]
let ``Type Namespace``() =
    let x = typeof<TestType>.Namespace
    equal "Fable.Tests.TypeTests" x

[<Test>]
let ``Type FullName``() =
    let x = typeof<TestType>.FullName
    equal "Fable.Tests.TypeTests.TestType" x
