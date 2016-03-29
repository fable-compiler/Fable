module Fable.Tests.Util

open System
open NUnit.Framework

let equal (expected: 'T) (actual: 'T) =
    Assert.AreEqual(expected, actual)

type Helper =
    static member Format(pattern: string, [<ParamArray>] args: obj[]) =
        String.Format(pattern, args)