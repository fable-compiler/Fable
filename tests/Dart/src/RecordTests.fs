module Fable.Tests.Dart.Record

open Util

type RecursiveRecord =
    { things : RecursiveRecord list }

type Person =
    { name: string; mutable luckyNumber: int }
    member x.LuckyDay = x.luckyNumber % 30
    member x.SignDoc str = str + " by " + x.name

//type JSKiller =
//   { ``for`` : float; ``class`` : float }

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

type RecordWithIdField = { Id: Id }

type Wrapped = { Value: int }

let addOne (x: int) = x + 1

type IWrap =
    abstract Wrap: unit -> Wrapped
    abstract Add: unit -> int

type RecordShadowingFileScope =
    { Wrapped: int; addOne: int }
    interface IWrap with
        member this.Wrap() = { Value = this.Wrapped }
        member this.Add() = addOne this.addOne

[<Struct>]
type StructRecordCopied = { T: float; S: string }

[<Struct>]
type StructRecordWithMutableField = { mutable MutableN: int; FixedN: int }

let mutable moduleStructRecord = { T = 1.0; S = "a" }

type ByRefReturnHolder() =
    let mutable r = { T = 1.0; S = "a" }
    member _.R : byref<StructRecordCopied> = &r

[<Struct>]
type SpreadPoint = { SpX: int; SpY: int }

[<Struct>]
type SpreadPoint3 = { ...SpreadPoint; SpZ: int }

type SpreadPoint3Ref = { ...SpreadPoint; SpZ: int }

type SpreadZFirst = { SpZ: int; ...SpreadPoint }

let mutable spreadModuleMutable: SpreadPoint = { SpX = 1; SpY = 2 }

type SpreadCell() =
    let mutable s: SpreadPoint = { SpX = 1; SpY = 2 }
    member _.ExplicitFirst() : SpreadZFirst = { SpZ = (s <- { SpX = 7; SpY = 8 }; 3); ...s }

type SpreadCtor() =
    let mutable s: SpreadPoint = { SpX = 1; SpY = 2 }
    let r: SpreadZFirst = { SpZ = (s <- { SpX = 7; SpY = 8 }; 3); ...s }
    member _.R = r

type SpreadImmutable(p: SpreadPoint) =
    let s: SpreadPoint = { SpX = 4; SpY = 5 }
    member _.Field() : SpreadZFirst = { SpZ = 6; ...s }
    member _.Param() : SpreadZFirst = { SpZ = 9; ...p }

let tests() =
    testCase "Anonymous records work" <| fun () ->
        let r = makeAnonRec()
        $"Tell me %s{r.Y} %i{r.F r.X 3} times"
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

    // TODO
//    testCase "Records with key/reserved words are mapped correctly" <| fun () ->
//        let x = { ``for`` = 1.0; ``class`` = 2.0 }
//        equal 2. x.``class``

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

    testCase "Record field can have the same name as its type" <| fun () ->
        let record = { Id = Id "foo" }
        match record.Id with
        | Id value -> value |> equal "foo"

    testCase "Record fields can have the same name as file scope declarations" <| fun () ->
        let record = { Wrapped = 3; addOne = 7 } :> IWrap
        record.Wrap().Value |> equal 3
        record.Add() |> equal 8

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

    testCase "Copy-and-update of a let mutable anonymous struct record keeps the unchanged fields" <| fun () ->
        let mutable a = struct {| X = 1; Y = 2 |}
        let b = {| a with Y = 99 |}
        (b.X, b.Y) |> equal (1, 99)

    testCase "Reads and copy-and-update through a byref property work" <| fun () ->
        let h = ByRefReturnHolder()
        let r = h.R
        (r.T, r.S) |> equal (1.0, "a")
        let u = { h.R with S = "b" }
        (u.T, u.S) |> equal (1.0, "b")

    testCase "Struct record spread into struct, reference and anonymous records works" <| fun () ->
        let p: SpreadPoint = { SpX = 1; SpY = 2 }
        let s: SpreadPoint3 = { ...p; SpZ = 3 }
        (s.SpX, s.SpY, s.SpZ) |> equal (1, 2, 3)
        let r: SpreadPoint3Ref = { ...p; SpZ = 3 }
        (r.SpX, r.SpY, r.SpZ) |> equal (1, 2, 3)
        let a = {| ...p; W = 0 |}
        (a.SpX, a.SpY, a.W) |> equal (1, 2, 0)
        let field = SpreadImmutable({ SpX = 7; SpY = 8 }).Field()
        (field.SpX, field.SpY, field.SpZ) |> equal (4, 5, 6)
        let param = SpreadImmutable({ SpX = 7; SpY = 8 }).Param()
        (param.SpX, param.SpY, param.SpZ) |> equal (7, 8, 9)

    testCase "Struct anonymous record spread works" <| fun () ->
        let st = struct {| X = 1; Y = 2 |}
        let a = struct {| ...st; Z = 3 |}
        (a.X, a.Y, a.Z) |> equal (1, 2, 3)

    testCase "Spread reads a mutable struct record after an explicit field replaces it" <| fun () ->
        let mutable m: SpreadPoint = { SpX = 1; SpY = 2 }
        let local: SpreadZFirst = { SpZ = (m <- { SpX = 7; SpY = 8 }; 3); ...m }
        (local.SpX, local.SpY, local.SpZ) |> equal (7, 8, 3)

        let mutable c: SpreadPoint = { SpX = 1; SpY = 2 }
        let spread () : SpreadZFirst = { SpZ = (c <- { SpX = 7; SpY = 8 }; 3); ...c }
        let captured = spread ()
        (captured.SpX, captured.SpY, captured.SpZ) |> equal (7, 8, 3)

        spreadModuleMutable <- { SpX = 1; SpY = 2 }
        let moduleLevel: SpreadZFirst = { SpZ = (spreadModuleMutable <- { SpX = 7; SpY = 8 }; 3); ...spreadModuleMutable }
        (moduleLevel.SpX, moduleLevel.SpY, moduleLevel.SpZ) |> equal (7, 8, 3)

        let classField = SpreadCell().ExplicitFirst()
        (classField.SpX, classField.SpY, classField.SpZ) |> equal (7, 8, 3)

        let ctorField = SpreadCtor().R
        (ctorField.SpX, ctorField.SpY, ctorField.SpZ) |> equal (7, 8, 3)
