module Fable.Tests.RecordTests

open Util.Testing

type MyRecord = {
    a: int
    b: string
    c: float
}

[<Fact>]
let ``Record fields works`` () =
    let x = { a=1; b="2"; c=3.0 }
    x.a |> equal 1
    x.b |> equal "2"
    x.c |> equal 3.0

[<Fact>]
let ``Record structural equality works`` () =
    let x = { a=1; b="2"; c=3.0 }
    let y = { a=1; b="2"; c=3.0 }
    let z = { a=3; b="4"; c=5.0 }
    x |> equal y
    (x = y) |> equal true
    (y = z) |> equal false
    (x = z) |> equal false

type DeepRecord = {
    d: MyRecord
    s: string
}

[<Fact>]
let ``Deep record fields works`` () =
    let x = { d={ a=1; b="2"; c=3.0 }; s="hello" }
    x.d.a |> equal 1
    x.d.b |> equal "2"
    x.d.c |> equal 3.0
    x.s |> equal "hello"

[<Fact>]
let ``Deep record structural equality works`` () =
    let a = { d={ a=1; b="2"; c=3.0 }; s="hello" }
    let b = { d={ a=2; b="3"; c=4.0 }; s="world" }
    let c = { d={ a=1; b="2"; c=3.0 }; s="hello" }
    (a = a) |> equal true
    (a = b) |> equal false
    (a = c) |> equal true
    (b = c) |> equal false

let transformAddA ar =
    { ar with a = ar.a + 1}

[<Fact>]
let ``Call pass record byref to add fns works with borrow`` () =
    let x = { a=1; b="2"; c=3.0 }
    let z = { a=3; b="4"; c=5.0 }

    let x2 = x |> transformAddA |> transformAddA //borrow x
    let z2 = z |> transformAddA |> transformAddA |> transformAddA
    let x3 = x |> transformAddA //borrow x a second time
    x2.a |> equal 3
    x3.a |> equal 2
    z2.a |> equal 6

let transformDeepAddA ar =
    { d=transformAddA ar.d; s=ar.s } // borrow ar, clone s

[<Fact>]
let ``Call pass record byref deep to add fns works with borrow`` () =
    let x = {d ={ a=1; b="2"; c=3.0 }; s="one"}
    let z = {d ={ a=3; b="4"; c=5.0 }; s="two"}

    let x2 = x |> transformDeepAddA |> transformDeepAddA //borrow x
    let z2 = z |> transformDeepAddA |> transformDeepAddA |> transformDeepAddA
    let x3 = x |> transformDeepAddA //borrow x again
    x2.d.a |> equal 3
    x3.d.a |> equal 2
    z2.d.a |> equal 6

[<Fact>]
let ``Let bindings borrow instead of move`` () =
    let x = { d={ a=1; b="2"; c=3.0 }; s="hello" }
    let y = x
    let z = x.d
    z |> equal x.d // prevents inlining
    y.s |> equal "hello"
    x.d.a |> equal 1
    z.a |> equal 1
    x.s |> equal "hello"
    x.d |> equal y.d

let recordPatternMatchFn = function
    | { b = "hello"; a=x } -> x
    | _ -> -1

let ``Pattern matching works`` () =
    let resA = recordPatternMatchFn { a=1; b="hello"; c=3.0 }
    let resB = recordPatternMatchFn { a=2; b="fail"; c=3.0 }
    resA |> equal 1
    resB |> equal -1

[<Struct>]
type StructRecord = {
    i: int
    s: string
}

let processStructByValue (s: StructRecord) =
    s, s.i + 1

[<Fact>]
let ``Struct record works`` () =
    let r1 = { i=1; s="hello" }
    let r2 = { i=1; s="world" }
    let (sres1, ires1) = r1 |> processStructByValue
    let (sres2, ires2) = r1 |> processStructByValue
    let (sres3, ires3) = r2 |> processStructByValue //cannot actually test this, but since this is the only reference, the output should not .clone()
    ires2 |> equal 2
    sres3.s |> equal "world"

type MutableRecord = {
    mutable MutValue: int
}

[<Fact>]
let ``Records with value-type interior mutability`` () =
    let x = { MutValue = 1 }
    x.MutValue |> equal 1
    x.MutValue <- x.MutValue + 1
    x.MutValue |> equal 2
    x.MutValue <- x.MutValue + 1
    x.MutValue |> equal 3

type MutableRefRecord = {
    mutable MutRefValue: string
}

[<Fact>]
let ``Records with ref-type interior mutability`` () =
    let x = { MutRefValue = "a" }
    x.MutRefValue |> equal "a"
    x.MutRefValue <- x.MutRefValue + "b"
    x.MutRefValue |> equal "ab"
    x.MutRefValue <- x.MutRefValue + "c"
    x.MutRefValue |> equal "abc"

[<Fable.Core.Rust.ReferenceType(Fable.Core.Rust.PointerType.Arc)>]
type ArcRecord = {
    a: int
    b: string
    c: float
}

let add1 (a) =
    { a = a.a + 1; b = a.b + "1"; c = a.c + 1.0 }

[<Fact>]
let ``Arc record fields works`` () =
    let x = { a=1; b="2"; c=3.0 }
    let y = x |> add1
    y.a |> equal 2
    y.b |> equal "21"
    y.c |> equal 4.0

[<Fable.Core.Rust.ReferenceType(Fable.Core.Rust.PointerType.Box)>]
type BoxRecord = {
    a: int
}

let add1Box (a) =
    { a = a.a + 1 }

[<Fact>]
let ``Box record fields works`` () =
    let x = { a=1 }
    let y = x |> add1Box
    y.a |> equal 2

[<Fact>]
let ``Box record multiple owner should clone and fork`` () =
    let x = { a=1 }
    let y = x |> add1Box
    let y2 = x |> add1Box
    y.a |> equal 2
    y2.a |> equal 2

[<Fact>]
let ``Should correctly import record in other file and allow creation`` =
    let r = { Common.Imports.MyRecord.a = 1}
    let expected = Common.Imports.MyRecord.create 1
    r |> equal expected

#if FABLE_COMPILER
open Fable.Core

[<Emit("$0 as LrcPtr<MyRecord>")>]
let ensureMyRecordWrapped s = nativeOnly

[<Emit("$0 as Arc<ArcRecord>")>]
let ensureArcRecordWrappedInArc s = nativeOnly

[<Emit("$0 as Box<BoxRecord>")>]
let ensureBoxRecordWrappedInBox s = nativeOnly

[<Emit("$0 as StructRecord")>]
let ensureIsStructRecordUnwrapped s = nativeOnly

[<Fact>]
let ``Normal record should be wrapped in a Lrc`` () =
    { MyRecord.a=1; b="2"; c=3.0 } |> ensureMyRecordWrapped |> ignore

[<Fact>]
let ``ArcRecord should always be wrapped in Arc`` () =
    { ArcRecord.a=1; b="2"; c=3.0 } |> ensureArcRecordWrappedInArc |> ignore

[<Fact>]
let ``BoxRecord should always be wrapped in Box`` () =
    { BoxRecord.a=1 } |> ensureBoxRecordWrappedInBox |> ignore

[<Fact>]
let ``Struct record should not be wrapped in a Lrc`` () =
    { i=1; s="world" } |> ensureIsStructRecordUnwrapped |> ignore

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

type CarInterior = { Seats: int }
type Car = { Interior: CarInterior }

//TODO:
// let inline replaceById< ^t when ^t : (member Id : Id)> (newItem : ^t) (ar: ^t[]) =
//     Array.map (fun (x: ^t) -> if (^t : (member Id : Id) newItem) = (^t : (member Id : Id) x) then newItem else x) ar

// let inline makeAnonRec() =
//     {| X = 5; Y = "Foo"; F = fun x y -> x + y |}

// [<Fact>]
// let ``Anonymous records with funcs work`` () =
//     let r = makeAnonRec()
//     $"Tell me {r.Y} {r.F r.X 3} times"
//     |> equal "Tell me Foo 8 times"

// [<Fact>]
// let ``SRTP works with anonymous records`` () =
//     let ar = [| {|Id=Id"foo"; Name="Sarah"|}; {|Id=Id"bar"; Name="James"|} |]
//     replaceById {|Id=Id"ja"; Name="Voll"|} ar |> Seq.head |> fun x -> equal "Sarah" x.Name
//     replaceById {|Id=Id"foo"; Name="Anna"|} ar |> Seq.head |> fun x -> equal "Anna" x.Name

type Time =
    static member inline duration(value: {| from: int; until: int |}) = value.until - value.from
    static member inline duration(value: {| from: int |}) = Time.duration {| value with until = 10 |}

[<Fact>]
let ``Anonymous records work`` () =
    let x = {| Foo = "baz"; Bar = 23 |}
    let y = {| Foo = "baz" |}
    x = {| y with Bar = 23 |} |> equal true
    // x = {| y with Baz = 23 |} |> equal true // Doesn't compile
    x = {| y with Bar = 14 |} |> equal false

[<Fact>]
let ``Overloads with anonymous record arguments don't have same mangled name`` () =
    Time.duration {| from = 1 |} |> equal 9
    Time.duration {| from = 1; until = 5 |} |> equal 4

[<Fact>]
let ``Anonymous record execution order`` () =
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

[<Fact>]
let ``Recursive record does not cause issues`` () =
    let r = { things = [ { things = [] } ] }
    equal r.things.Length 1

[<Fact>]
let ``Record property access can be generated`` () =
    let x = { name = "Alfonso"; luckyNumber = 7 }
    equal "Alfonso" x.name
    equal 7 x.luckyNumber
    x.luckyNumber <- 14
    equal 14 x.luckyNumber

[<Fact>]
let ``Record methods can be generated`` () =
    let x = { name = "Alfonso"; luckyNumber = 54 }
    equal 24 x.LuckyDay
    x.SignDoc "Hello World!"
    |> equal "Hello World! by Alfonso"

[<Fact>]
let ``Record expression constructors can be generated`` () =
    let x = { name = "Alfonso"; luckyNumber = 7 }
    let y = { x with luckyNumber = 14 }
    equal "Alfonso" y.name
    equal 14 y.luckyNumber

[<Fact>]
let ``Records with key/reserved words are mapped correctly`` () =
    let x = { ``for`` = 1.0; ``class`` = 2.0 }
    equal 2. x.``class``

[<Fact>]
let ``Records with special characters are mapped correctly`` () =
    let x = { ``s p a c e`` = 1.0; ``s*y*m*b*o*l`` = 2.0 }
    equal 1. x.``s p a c e``
    equal 2. x.``s*y*m*b*o*l``

[<Fact>]
let ``Mutating records work`` () =
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

[<Fact>]
let ``Nested record field copy and update works for records`` =
    let car =
        { Interior = { Seats = 4 } }
    let car2 =
        { car with Interior.Seats = 5 }
    equal 5 car2.Interior.Seats

[<Fact>]
let ``Nested record field copy and update works for anonymous records`` =
    let car =
        {| Interior = {| Seats = 4 |} |}
    let car2 =
        {| car with Interior.Seats = 5 |}
    equal 5 car2.Interior.Seats

module ComplexEdgeCases =
    open Common.Imports.Vectors
    open Fable.Core.Rust

    [<Measure>] type m
    [<Measure>] type Rad

    [<Struct>]
    type SpatialDta = {
        Rotation: float32<Rad>
        Position: Vector2R<m>
    }

    type ItemWithSpatialDta = {
        Name: string
        Spatial: SpatialDta
    }

    let rotate r ([<ByRef>] ent) =
        { ent with Spatial = { ent.Spatial with Rotation = ent.Spatial.Rotation + r } }

    [<Fact>]
    let ``Nested struct records work`` () =
        let res =
            { Name = "A"; Spatial = { Rotation = 1.1f<Rad>; Position = { x = 1f<m>; y = 2f<m> } } }
            |> rotate (1.2f<Rad>)
        res.Spatial.Rotation |> equal (1.1f<Rad> + 1.2f<Rad>)

    [<Fact>]
    let ``Ref tracking should correctly count arm ident usages and clone accordingly`` () =
        let cmpPropLstR = [{ MyRecord.a = 1; b = "2"; c = 3.0 }]
        let add1 (x: MyRecord) = {x with a = x.a + 1}
        let res =
            match cmpPropLstR with
            | ({ MyRecord.a = 1 } as h)::t ->
                let next = add1 h
                next::t
            | _ -> cmpPropLstR
        notEqual res cmpPropLstR

    type ItemWithSpatialDta2 = {
        Name: string
        Spatial: SpatialDta option
    }
    type GameState = {
        Items: ItemWithSpatialDta2 list
    }
    type TestInput = | T_A | T_B | T_C

    let applyFn1 r p : ItemWithSpatialDta2 = { p with Name = "next" }
    let applyFn2 p = p

    let someStateTransform inputs current =
        match current.Items with
        | ({ Spatial = Some sp } as boundItemState)::t ->
            let playerNext =
                match inputs with
                | T_A -> boundItemState |> applyFn1 boundItemState.Spatial
                | T_B -> boundItemState |> applyFn2
                | T_C -> boundItemState
            { current with Items = playerNext::t }
        | _ -> { current with Items = [] }

    // This is mainly about ensuring the idents are correctly counted leading to too little/much cloning and potentially a build error
    [<Fact>]
    let ``Ref tracking should correctly count arm ident usages and clone accordingly II`` () =
        let current = { Items = [{Name = "ab"; Spatial = Some { Rotation = 1.1f<Rad>; Position = { x = 1f<m>; y = 2f<m> } }}]}
        let next = someStateTransform T_A current
        next |> notEqual current
