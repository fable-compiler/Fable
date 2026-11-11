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

// Issue #3923 - static let on a record shifts constructor arg indices
type RecordWithStaticLet =
    { id: int }
    static let _a = 2
    static member a = _a

// Issue #4091 - static member val on a record causes shared mutable state
type RecordWithStaticMemberVal =
    { xs: ResizeArray<int> }
    static member val empty = { xs = ResizeArray() }

[<Struct>]
type StructRecord = { Size: int; Seed: int; Path: int list }

type StructRecordHolder = { Tag: int; Data: StructRecord }

let updateStructRecord (data: StructRecord) (path: int list) = { data with Path = List.rev path }

let holdStructRecord (data: StructRecord) (path: int list) =
    Some
        {
            Tag = 1
            Data = { data with Path = List.rev path }
        }

[<Struct>]
type StructRecordCopied = { T: float; S: string }

[<Struct>]
type StructRecordWithMutableField = { mutable MutableN: int; FixedN: int }

[<Struct>]
type StructRecordOrdered = { OrdA: int; OrdB: int; OrdC: int }

type ByRefClassRecord = { BcP: int; BcQ: int }

type ByRefClass =
    val mutable BkF: int
    val BkV: int
    new() = { BkF = 0; BkV = 3 }

let sumStructRecordByRef (r: byref<StructRecordOrdered>) = r.OrdA + r.OrdC

let copyStructRecordByRef (r: byref<StructRecordOrdered>) = { r with OrdB = 99 }

let sumClassRecordByRef (r: byref<ByRefClassRecord>) = r.BcP + r.BcQ

let setClassRecordOutRef (r: outref<ByRefClassRecord>) =
    r <- { BcP = 4; BcQ = 1 }
    r.BcP + r.BcQ

let sumClassByRef (r: byref<ByRefClass>) = r.BkF + r.BkV

let sumAnonymousStructRecordByRef (x: byref<struct {| AnX: int; AnY: int |}>) = x.AnX + x.AnY

let mutable moduleStructRecord = { T = 1.0; S = "a" }

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

    testCase "Anonymous records can have optional fields" <| fun () ->
        let add (o: {| bar: int option; zas: string option; foo: int option option |}) =
            let bar = o.bar |> Option.map string |> Option.defaultValue "-"
            let zas = defaultArg o.zas ""
            let foo = match o.foo with Some(Some i) -> string i | Some None -> "xx" | None -> "x"
            bar + zas + foo

        {| bar = Some 3; zas = Some "ooooo"; foo = Some None |} |> add |> equal "3oooooxx"
        {| bar = Some 22; zas = Some ""; foo = Some(Some 999) |} |> add |> equal "22999"
        {| bar = None; zas = None; foo = None |} |> add |> equal "-x"
        {| foo = Some None; bar = None; zas = None |} |> add |> equal "-xx"

    testCase "Anonymous records can have optional function fields" <| fun () ->
        let add (o: {| bar: (int -> int -> int) option; foo: int -> int -> int |}) =
            let fn = o.bar
            let f1 = fn |> Option.map (fun f -> f 6 9) |> Option.defaultValue -3
            let f2 = match fn with Some f -> f 1 8 | None -> -5
            o.foo 3 4 + f1 + f2

        {| bar = Some (+); foo = (*) |} |> add |> equal 36
        {| bar = None; foo = (+) |} |> add |> equal -1

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

    testCase "Copy-and-update of a struct record argument works" <| fun () ->
        let data: StructRecord = { Size = 1; Seed = 2; Path = [ 3; 4 ] }
        let updated = updateStructRecord data [ 5; 6 ]
        equal 1 updated.Size
        equal 2 updated.Seed
        equal [ 6; 5 ] updated.Path
        let held = holdStructRecord data [ 7; 8 ] |> Option.get
        equal 1 held.Data.Size
        equal 2 held.Data.Seed
        equal [ 8; 7 ] held.Data.Path

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

    // Issue #3923
    testCase "Record with static let has correct constructor" <| fun () ->
        let x = { id = 1 }
        equal 1 x.id
        equal 2 RecordWithStaticLet.a

    // Issue #4091
    testCase "Record with static member val does not share state across instances" <| fun () ->
        let a = RecordWithStaticMemberVal.empty
        let b = RecordWithStaticMemberVal.empty
        a.xs.Add 1
        equal 1 a.xs.Count
        equal 1 b.xs.Count

    testCase "Copy-and-update of a let mutable struct record keeps the unchanged fields" <| fun () ->
        let mutable mark = { T = 1.0; S = "a" }
        let copies = ResizeArray()
        for i in 1..2 do
            mark <- { T = float i; S = "b" }
            copies.Add { mark with S = "c" }
        copies |> Seq.map (fun m -> m.T) |> List.ofSeq |> equal [ 1.0; 2.0 ]
        copies |> Seq.map (fun m -> m.S) |> List.ofSeq |> equal [ "c"; "c" ]

    testCase "Copy-and-update of a let mutable struct record with mutable fields keeps the unchanged fields" <| fun () ->
        let mutable p = { MutableN = 1; FixedN = 2 }
        p.MutableN <- 5
        let q = { p with FixedN = 9 }
        q.MutableN |> equal 5
        q.FixedN |> equal 9
        p.FixedN |> equal 2

    testCase "Copy-and-update of a module-level mutable struct record keeps the unchanged fields" <| fun () ->
        moduleStructRecord <- { moduleStructRecord with T = 7.0 }
        let copy = { moduleStructRecord with S = "b" }
        copy.T |> equal 7.0
        copy.S |> equal "b"
        moduleStructRecord.S |> equal "a"

    testCase "Field reads and copy-and-update through a byref struct record parameter work" <| fun () ->
        let mutable r = { OrdA = 1; OrdB = 2; OrdC = 3 }
        sumStructRecordByRef &r |> equal 4
        let x = copyStructRecordByRef &r
        (x.OrdA, x.OrdB, x.OrdC) |> equal (1, 99, 3)

    testCase "Field reads through a byref or outref to a reference type work" <| fun () ->
        let mutable c = { BcP = 1; BcQ = 2 }
        sumClassRecordByRef &c |> equal 3
        let mutable o = { BcP = 0; BcQ = 0 }
        setClassRecordOutRef &o |> equal 5
        let mutable k = ByRefClass()
        k.BkF <- 4
        sumClassByRef &k |> equal 7

    testCase "Copy-and-update of a let mutable anonymous struct record keeps the unchanged fields" <| fun () ->
        let mutable a = struct {| X = 1; Y = 2 |}
        let b = {| a with Y = 99 |}
        (b.X, b.Y) |> equal (1, 99)

    testCase "Field reads through a byref to an anonymous struct record work" <| fun () ->
        let mutable a = struct {| AnX = 1; AnY = 2 |}
        sumAnonymousStructRecordByRef &a |> equal 3

  ]
