[<NUnit.Framework.TestFixture>] 
module Fable.Tests.RecordTypes
open NUnit.Framework
open Fable.Tests.Util

type Person =
    { name: string; luckyNumber: int }
    member x.LuckyDay = x.luckyNumber % 30
    member x.SignDoc str = str + " by " + x.name

[<Test>]
let ``Record property access can be generated``() =
    let x = { name = "Alfonso"; luckyNumber = 7 }
    equal "Alfonso" x.name
    equal 7 x.luckyNumber

[<Test>]
let ``Record methods can be generated``() =
    let x = { name = "Alfonso"; luckyNumber = 54 }
    equal 24 x.LuckyDay
    x.SignDoc "Hello World!"
    |> equal "Hello World! by Alfonso"

[<Test>]
let ``Record expression constructors can be generated``() =
    let x = { name = "Alfonso"; luckyNumber = 7 }
    let y = { x with luckyNumber = 14 }
    equal "Alfonso" y.name
    equal 14 y.luckyNumber

type JSKiller =
   { ``for`` : float; ``class`` : float }

type JSKiller2 =
   { ``s p a c e`` : float; ``s*y*m*b*o*l`` : float }

[<Test>]
let ``Records with key/reserved words are mapped correctly``() =
    let x = { ``for`` = 1.0; ``class`` = 2.0 }
    equal 2. x.``class``

[<Test>]
let ``Records with special characters are mapped correctly``() =
    let x = { ``s p a c e`` = 1.0; ``s*y*m*b*o*l`` = 2.0 }
    equal 1. x.``s p a c e``
    equal 2. x.``s*y*m*b*o*l``
