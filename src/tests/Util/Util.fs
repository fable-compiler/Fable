module Fable.Tests.Util

open System
open NUnit.Framework

type EmitAttribute(s: string) =
    inherit System.Attribute()

let equal (expected: 'T) (actual: 'T) =
    Assert.AreEqual(expected, actual)

