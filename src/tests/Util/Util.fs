module Fable.Tests.Util

open System
open NUnit.Framework

type EmitAttribute(s: string) =
    inherit System.Attribute()

let equal (expected: 'T) (actual: 'T) =
    Assert.AreEqual(expected, actual)

type Helper =
    static member Format(pattern: string, [<ParamArray>] args: obj[]) =
        String.Format(pattern, args)
    
    // Check that project references from folders work
    static member CreateClampedArray() =
        Fable.Tests.Clamp.Helper.CreateClampedArray()
