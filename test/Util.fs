module Fable.Tests.Util

open NUnit.Framework

let equal (expected: 'T) (actual: 'T) =
    Assert.AreEqual(actual, expected)