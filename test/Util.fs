module Fabel.Tests.Util

open NUnit.Framework

let equal (o1: 'T) (o2: 'T) =
    Assert.AreEqual(o1, o2)