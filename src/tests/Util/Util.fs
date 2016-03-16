module Fable.Tests.Util

open System
open NUnit.Framework

let equal (expected: 'T) (actual: 'T) =
    Assert.AreEqual(actual, expected)

type Helper =
    static member Format(pattern: string, [<ParamArray>] args: obj[]) =
        String.Format(pattern, args)