module Fable.Tests.RecordTypes

open Util.Testing
#if FABLE_COMPILER
open Fable.Core
#endif

type RecursiveRecord =
    { things : RecursiveRecord list }

type Person =
    { name: string; mutable luckyNumber: int }
    member x.LuckyDay = x.luckyNumber % 30
    member x.SignDoc str = str + " by " + x.name

type JSKiller =
   { ``for`` : float; ``class`` : float }

type JSKiller2 =
   { ``s p a c e`` : float; ``s*y*m*b*o*l`` : float }

type Child =
    { a: string; b: int }
    member x.Sum() = (int x.a) + x.b

type Parent =
    { children: Child[] }
    member x.Sum() = x.children |> Seq.sumBy (fun c -> c.Sum())

type MutatingRecord =
    { uniqueA: int; uniqueB: int }

type Id = Id of string

let inline replaceById< ^t when ^t : (member Id : Id)> (newItem : ^t) (ar: ^t[]) =
    Array.map (fun (x: ^t) -> if (^t : (member Id : Id) newItem) = (^t : (member Id : Id) x) then newItem else x) ar

let makeAnonRec() =
    {| X = 5; Y = "Foo"; F = fun x y -> x + y |}

type Time =
    static member inline duration(value: {| from: int; until: int |}) = value.until - value.from
    static member inline duration(value: {| from: int |}) = Time.duration {| value with until = 10 |}

type CarInterior = { Seats: int }
type Car = { Interior: CarInterior }

let tests =
  testList "RecordTypes" [

    testCase "Anonymous records work" <| fun () ->
        let r = makeAnonRec()
        sprintf "Tell me %s %i times" r.Y (r.F r.X 3)
        |> equal "Tell me Foo 8 times"
        let x = {| Foo = "baz"; Bar = 23 |}
        let y = {| Foo = "baz" |}
        x = {| y with Bar = 23 |} |> equal true
        // x = {| y with Baz = 23 |} |> equal true // Doesn't compile
        x = {| y with Bar = 14 |} |> equal false

    testCase "SRTP works with anonymous records" <| fun () ->
        let ar = [| {|Id=Id"foo"; Name="Sarah"|}; {|Id=Id"bar"; Name="James"|} |]
        replaceById {|Id=Id"ja"; Name="Voll"|} ar |> Seq.head |> fun x -> equal "Sarah" x.Name
        replaceById {|Id=Id"foo"; Name="Anna"|} ar |> Seq.head |> fun x -> equal "Anna" x.Name

    testCase "Overloads with anonymous record arguments don't have same mangled name" <| fun () ->
        Time.duration {| from = 1 |} |> equal 9
        Time.duration {| from = 1; until = 5 |} |> equal 4

    testCase "Anonymous record execution order" <| fun () ->
        let mutable x = 2
        let record =
            {|
                C = (x <- x * 3; x)
                B = (x <- x + 5; x)
                A = (x <- x / 2; x)
            |}
        record.A |> equal 5
        record.B |> equal 11
        record.C |> equal 6

    testCase "Recursive record does not cause issues" <| fun () ->
        let r = { things = [ { things = [] } ] }
        equal r.things.Length 1

    testCase "Record property access can be generated" <| fun () ->
        let x = { name = "Alfonso"; luckyNumber = 7 }
        equal "Alfonso" x.name
        equal 7 x.luckyNumber
        x.luckyNumber <- 14
        equal 14 x.luckyNumber

    testCase "Record methods can be generated" <| fun () ->
        let x = { name = "Alfonso"; luckyNumber = 54 }
        equal 24 x.LuckyDay
        x.SignDoc "Hello World!"
        |> equal "Hello World! by Alfonso"

    testCase "Record expression constructors can be generated" <| fun () ->
        let x = { name = "Alfonso"; luckyNumber = 7 }
        let y = { x with luckyNumber = 14 }
        equal "Alfonso" y.name
        equal 14 y.luckyNumber

    testCase "Records with key/reserved words are mapped correctly" <| fun () ->
        let x = { ``for`` = 1.0; ``class`` = 2.0 }
        equal 2. x.``class``

    testCase "Records with special characters are mapped correctly" <| fun () ->
        let x = { ``s p a c e`` = 1.0; ``s*y*m*b*o*l`` = 2.0 }
        equal 1. x.``s p a c e``
        equal 2. x.``s*y*m*b*o*l``

    testCase "Mutating records work" <| fun () ->
        let x = { uniqueA = 10; uniqueB = 20 }
        equal 10 x.uniqueA
        equal 20 x.uniqueB
        let uniqueB' = -x.uniqueB
        let x' = { x with uniqueB = uniqueB' }
        equal 10 x.uniqueA
        equal 10 x'.uniqueA
        equal -20 x'.uniqueB
        let x'' = { x' with uniqueA = -10 }
        equal -10 x''.uniqueA
        equal -20 x''.uniqueB

    testCase "Nested record field copy and update works for records" <| fun () ->
        let car =
            { Interior = { Seats = 4 } }
        let car2 =
            { car with Interior.Seats = 5 }
        equal 5 car2.Interior.Seats

    testCase "Nested record field copy and update works for anonymous records" <| fun () ->
        let car =
            {| Interior = {| Seats = 4 |} |}
        let car2 =
            {| car with Interior.Seats = 5 |}
        equal 5 car2.Interior.Seats

  ]
