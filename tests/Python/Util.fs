module Fable.Tests.Util

open System

module Testing =
#if FABLE_COMPILER
    open Fable.Core
    open Fable.Core.PyInterop

    type Assert =
        [<Emit("assert $0 == $1")>]
        static member AreEqual(actual: 'T, expected: 'T, ?msg: string): unit = pyNative
        [<Emit("assert not $0 == $1")>]
        static member NotEqual(actual: 'T, expected: 'T, ?msg: string): unit = pyNative

    let equal expected actual: unit = Assert.AreEqual(actual, expected)
    let notEqual expected actual: unit = Assert.NotEqual(actual, expected)

    type Fact() = inherit System.Attribute()
#else
    open Xunit

    let equal<'T> (expected: 'T) (actual: 'T): unit = Assert.Equal(expected, actual)
    let notEqual<'T> (expected: 'T) (actual: 'T) : unit = Assert.NotEqual(expected, actual)
#endif
