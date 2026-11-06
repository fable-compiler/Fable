module Fable.Tests.StructuredFormatDisplay

open Util.Testing
open Fable.Tests.Util

[<StructuredFormatDisplay("{Display}")>]
type DisplayRecord =
    { a: int }
    member this.Display = $"DisplayRecord {{ a = {this.a} }}"

[<StructuredFormatDisplay("Point({X}, {Y})")>]
type DisplayPoint = { X: int; Y: string }

[<StructuredFormatDisplay("Shape {Name}")>]
type DisplayShape =
    | Circle of int
    | Square
    member this.Name =
        match this with
        | Circle r -> $"circle {r}"
        | Square -> "square"

[<StructuredFormatDisplay("{A}{B} {S}{S}")>]
type DisplayAdjacent = { A: int; B: int; S: string }

[<StructuredFormatDisplay("\\{{A}\\} {L}")>]
type DisplayEscaped = { A: int; L: int list }

[<StructuredFormatDisplay("Wrapper={Inner}")>]
type DisplayWrapper = { Inner: DisplayPoint }

[<StructuredFormatDisplay("{O}")>]
type DisplayNullValue = { O: obj }

[<StructuredFormatDisplay("open {A")>]
type DisplayStrayBracket = { A: int }

[<StructuredFormatDisplay("{Inc}/{Text}/{Shadow}")>]
type DisplayInline =
    { A: int }
    member inline this.Inc = this.A + 1
    member inline this.Text = sprintf "v%d" this.A
    member inline this.Shadow =
        let self = this.A + 2
        self + 1

[<StructuredFormatDisplay("{Wrapped}")>]
type DisplayInlineGeneric<'T> =
    { V: 'T }
    member inline this.Wrapped = [ this.V ]

[<StructuredFormatDisplay("{IsDisplayCaseA}/{IsDisplayCaseB}")>]
type DisplayCaseTester =
    | DisplayCaseA
    | DisplayCaseB

[<StructuredFormatDisplay("{N}{Boom}")>]
type DisplayNullThenThrow =
    { N: string }
    member _.Boom: string = failwith "boom"

[<StructuredFormatDisplay("a{Boom}b")>]
type DisplayThrow =
    { A: int }
    member _.Boom: string = failwith "boom"

[<StructuredFormatDisplay("n{Inner}")>]
type DisplayNestedThrow = { Inner: DisplayThrow }

[<StructuredFormatDisplay("{Boom}{N}")>]
type DisplayThrowThenNull =
    { N: string }
    member _.Boom: string = failwith "boom"

[<StructuredFormatDisplay("x{Self}")>]
type DisplaySelf =
    { A: int }
    member this.Self = this

[<StructuredFormatDisplay("{Self} {Self}")>]
type DisplaySelfTwice =
    { A: int }
    member this.Self = this

[<StructuredFormatDisplay("node {Link}")>]
type DisplayCycle = { mutable Link: obj }

[<StructuredFormatDisplay("x{Next}")>]
type DisplayEndless =
    { A: int }
    member this.Next = { A = this.A + 1 }

[<StructuredFormatDisplay("{A}:{Next}")>]
type DisplayEndlessValue =
    { A: int }
    member this.Next = { A = this.A + 1 }

[<StructuredFormatDisplay("{Next} {Next}")>]
type DisplayEndlessTwice =
    { A: int }
    member this.Next = { A = this.A + 1 }

[<StructuredFormatDisplay("D{S}")>]
type DisplayDepth4 = { S: string }

[<StructuredFormatDisplay("C{D}")>]
type DisplayDepth3 = { D: DisplayDepth4 }

[<StructuredFormatDisplay("B{C}")>]
type DisplayDepth2 = { C: DisplayDepth3 }

[<StructuredFormatDisplay("A{B}")>]
type DisplayDepth1 = { B: DisplayDepth2 }

[<StructuredFormatDisplay("{Idx}")>]
type DisplayIndexed =
    { A: int }
    member _.Idx with get (i: int) = i + 1

[<StructuredFormatDisplay("{Missing}")>]
type DisplayMissing = { A: int }

[<StructuredFormatDisplay("a{A}{Missing}b")>]
type DisplayMissingAfterValue = { A: int }

[<StructuredFormatDisplay("{N}{Missing}")>]
type DisplayNullThenMissing = { N: string }

[<StructuredFormatDisplay("{}")>]
type DisplayEmptyName = { A: int }

[<StructuredFormatDisplay("{St}")>]
type DisplayStatic =
    { A: int }
    static member St = 1

[<StructuredFormatDisplay("{Meth}")>]
type DisplayMethod =
    { A: int }
    member _.Meth() = 1

[<StructuredFormatDisplay("{Actual}")>]
type DisplayCompiledName =
    { A: int }
    [<CompiledName("Actual")>]
    member _.Declared = 7

[<StructuredFormatDisplay("{Item}")>]
type DisplayItem =
    | DisplayItemA of int
    | DisplayItemB of string
    | DisplayItemC of int * int

[<StructuredFormatDisplay("{r}")>]
type DisplayNamedField =
    | DisplayNamedA of r: int
    | DisplayNamedB of r: string
    | DisplayNamedC

[<StructuredFormatDisplay("{Item1}-{Item2}")>]
type DisplayItems = DisplayItems of int * string

[<StructuredFormatDisplay("{Tag}")>]
type DisplayTag =
    | DisplayTagA
    | DisplayTagB of int

[<StructuredFormatDisplay("{Missing}")>]
type DisplayMissingCases =
    | DisplayMissingA
    | DisplayMissingB of int
    | DisplayMissingC

[<StructuredFormatDisplay("{Missing}")>]
type DisplayMissingManyCases =
    | DisplayManyA
    | DisplayManyB
    | DisplayManyC
    | DisplayManyD
    | DisplayManyE of int

[<StructuredFormatDisplay("{Missing}")>]
type DisplayMissingNullary =
    | DisplayNullaryA
    | DisplayNullaryB

[<StructuredFormatDisplay("{L}{R}")>]
type DisplayPair<'L, 'R> = { L: 'L; R: 'R }

[<StructuredFormatDisplay("{A}{E}{B}")>]
type DisplayTriple<'A, 'B> = { A: 'A; E: string; B: 'B }

[<StructuredFormatDisplay("{S}")>]
type DisplayEndsWithString = { S: string }

[<StructuredFormatDisplay("{X}")>]
type DisplayEndsWithValue = { X: int }

[<StructuredFormatDisplay("{X}Q")>]
type DisplayEndsWithText = { X: int }

[<StructuredFormatDisplay("P{X}")>]
type DisplayStartsWithText = { X: int }

type DisplayPlainUnion =
    | DisplayPlainA
    | DisplayPlainB of int

type DisplayPlainRecord = { F: int }

[<StructuredFormatDisplay("disp u")>]
type DisplayFixedText = DisplayFixedText

[<StructuredFormatDisplay("{A} {A}")>]
type DisplayWithSpace = { A: int }

type DisplayHolder =
    | DisplayHolderOne of DisplayWithSpace
    | DisplayHolderTwo of DisplayFixedText * int

let tests = testList "StructuredFormatDisplay" [
    testCase "StructuredFormatDisplay is used by formatted strings" <| fun () -> // See #4880
        let r: DisplayRecord = { a = 1337 }
        sprintf "%A" r |> equal "DisplayRecord { a = 1337 }"
        $"{r}" |> equal "DisplayRecord { a = 1337 }"
        $"%A{r}" |> equal "DisplayRecord { a = 1337 }"
        string r |> equal "DisplayRecord { a = 1337 }"
        r.ToString() |> equal "DisplayRecord { a = 1337 }"
        sprintf "%O" r |> equal "DisplayRecord { a = 1337 }"
        sprintf "%A" [ r ] |> equal "[DisplayRecord { a = 1337 }]"
        sprintf "%A" ({ X = 1; Y = "y" }: DisplayPoint) |> equal "Point(1, y)"
        sprintf "%A" (Circle 3) |> equal "Shape circle 3"
        string Square |> equal "Shape square"
        sprintf "%A" ({ A = 1; B = 2; S = "s" }: DisplayAdjacent) |> equal "1 2 ss"
        sprintf "%A" ({ A = 1; L = [ 1; 2 ] }: DisplayEscaped) |> equal "{1\\} [1; 2]"
        sprintf "%A" ({ Inner = { X = 1; Y = "y" } }: DisplayWrapper) |> equal "Wrapper=Point(1, y)"
        sprintf "%A" ({ O = null }: DisplayNullValue) |> equal "{ O = null }"
        sprintf "%A" ({ A = 1 }: DisplayStrayBracket) |> equal "{ A = 1 }"

    testCase "StructuredFormatDisplay supports inline properties" <| fun () ->
        sprintf "%A" ({ A = 1 }: DisplayInline) |> equal "2/v1/4"
        sprintf "%A" ({ V = 3 }: DisplayInlineGeneric<int>) |> equal "[3]"

    testCase "StructuredFormatDisplay supports union case testers" <| fun () ->
        sprintf "%A" DisplayCaseA |> equal "true/false"
        sprintf "%A" DisplayCaseB |> equal "false/true"

    testCase "StructuredFormatDisplay stops reading properties at the first null" <| fun () ->
        sprintf "%A" ({ N = null }: DisplayNullThenThrow) |> equal "{ N = null }"

    testCase "StructuredFormatDisplay prints exceptions thrown by properties" <| fun () ->
        let expected = "<StructuredFormatDisplay exception: Exception has been thrown by the target of an invocation.>"
        sprintf "%A" ({ A = 1 }: DisplayThrow) |> equal expected
        string ({ A = 1 }: DisplayThrow) |> equal expected
        $"{({ A = 1 }: DisplayThrow)}" |> equal expected
        sprintf "%A" ({ Inner = { A = 1 } }: DisplayNestedThrow) |> equal ("n" + expected)
        sprintf "%A" ({ N = null }: DisplayThrowThenNull) |> equal expected

    testCase "StructuredFormatDisplay stops at cycles and at the depth limit" <| fun () ->
        sprintf "%A" ({ A = 1 }: DisplaySelf) |> equal "x..."
        string ({ A = 1 }: DisplaySelf) |> equal "x..."
        sprintf "%A" ({ A = 1 }: DisplaySelfTwice) |> equal "... ..."
        let node: DisplayCycle = { Link = null }
        node.Link <- box node
        sprintf "%A" node |> equal "node ..."
        sprintf "%A" ({ A = 1 }: DisplayEndless) |> equal "xxx..."
        sprintf "%A" ({ A = 1 }: DisplayEndlessValue) |> equal "1:2:...:..."
        sprintf "%A" ({ A = 1 }: DisplayEndlessTwice) |> equal "... ... ... ... ... ... ... ..."
        sprintf "%A" ({ C = { D = { S = "s" } } }: DisplayDepth2) |> equal "BCDs"
        sprintf "%A" ({ B = { C = { D = { S = "s" } } } }: DisplayDepth1) |> equal "ABC..."

    testCase "StructuredFormatDisplay reports members it cannot read" <| fun () ->
        let notFound name = $"<StructuredFormatDisplay exception: Method 'Fable.Tests.StructuredFormatDisplay+{name}' not found.>"
        sprintf "%A" ({ A = 1 }: DisplayIndexed) |> equal (notFound "DisplayIndexed.Idx")
        sprintf "%A" ({ A = 1 }: DisplayMissing) |> equal (notFound "DisplayMissing.Missing")
        string ({ A = 1 }: DisplayMissing) |> equal (notFound "DisplayMissing.Missing")
        sprintf "%A" ({ A = 1 }: DisplayMissingAfterValue) |> equal (notFound "DisplayMissingAfterValue.Missing")
        sprintf "%A" ({ N = null }: DisplayNullThenMissing) |> equal "{ N = null }"
        sprintf "%A" ({ A = 1 }: DisplayEmptyName) |> equal (notFound "DisplayEmptyName.ToString")
        sprintf "%A" ({ A = 1 }: DisplayStatic) |> equal (notFound "DisplayStatic.St")
        sprintf "%A" ({ A = 1 }: DisplayMethod) |> equal (notFound "DisplayMethod.Meth")
        sprintf "%A" DisplayMissingA |> equal (notFound "DisplayMissingCases+_DisplayMissingA.Missing")
        sprintf "%A" (DisplayMissingB 1) |> equal (notFound "DisplayMissingCases+DisplayMissingB.Missing")
        sprintf "%A" DisplayManyA |> equal (notFound "DisplayMissingManyCases.Missing")
        sprintf "%A" (DisplayManyE 1) |> equal (notFound "DisplayMissingManyCases+DisplayManyE.Missing")
        sprintf "%A" DisplayNullaryA |> equal (notFound "DisplayMissingNullary.Missing")

    testCase "StructuredFormatDisplay reads properties by compiled name" <| fun () ->
        sprintf "%A" ({ A = 1 }: DisplayCompiledName) |> equal "7"

    testCase "StructuredFormatDisplay reads union fields per case" <| fun () ->
        let notFound name = $"<StructuredFormatDisplay exception: Method 'Fable.Tests.StructuredFormatDisplay+{name}' not found.>"
        sprintf "%A" (DisplayItemA 1) |> equal "1"
        sprintf "%A" (DisplayItemB "x") |> equal "x"
        sprintf "%A" (DisplayItemC(1, 2)) |> equal (notFound "DisplayItem+DisplayItemC.Item")
        sprintf "%A" (DisplayNamedA 1) |> equal "1"
        sprintf "%A" (DisplayNamedB "s") |> equal "s"
        sprintf "%A" DisplayNamedC |> equal (notFound "DisplayNamedField+_DisplayNamedC.r")
        sprintf "%A" (DisplayItems(1, "x")) |> equal "1-x"
        sprintf "%A" DisplayTagA |> equal "0"
        sprintf "%A" (DisplayTagB 1) |> equal "1"

    testCase "StructuredFormatDisplay spaces adjacent placeholders like FSharp.Core" <| fun () ->
        let show l r = sprintf "%A" { L = l; R = r }
        let s: DisplayEndsWithString = { S = "n" }
        let v: DisplayEndsWithValue = { X = 2 }
        let q: DisplayEndsWithText = { X = 2 }
        let p: DisplayStartsWithText = { X = 2 }
        let u = DisplayPlainB 1
        let r: DisplayPlainRecord = { F = 1 }
        show 1 1 |> equal "1 1"
        show 1 "s" |> equal "1s"
        show "s" 1 |> equal "s1"
        show "s" "s" |> equal "ss"
        show 1 "" |> equal "1"
        show "" 1 |> equal "1"
        show 1 s |> equal "1 n"
        show s 1 |> equal "n1"
        show s s |> equal "nn"
        show v 1 |> equal "2 1"
        show q 1 |> equal "2Q1"
        show q q |> equal "2Q2Q"
        show p 1 |> equal "P2 1"
        show 1 q |> equal "1 2Q"
        show 1 p |> equal "1 P2"
        show [ 1 ] 1 |> equal "[1] 1"
        show [ s ] 1 |> equal "[n] 1"
        show true 1 |> equal "true 1"
        show u 1 |> equal "DisplayPlainB 1 1"
        show DisplayPlainA 1 |> equal "DisplayPlainA 1"
        show r 1 |> equal "{ F = 1 } 1"
        sprintf "%A" { A = 1; E = ""; B = 2 } |> equal "1 2"
        sprintf "%A" { A = "s"; E = ""; B = "s" } |> equal "ss"
        sprintf "%A" { A = 1; E = "s"; B = 2 } |> equal "1s2"

    testCase "StructuredFormatDisplay values are not wrapped in union parentheses" <| fun () ->
        sprintf "%A" (Ok DisplayFixedText: Result<DisplayFixedText, int>) |> equal "Ok disp u"
        sprintf "%A" (DisplayHolderOne { A = 1 }) |> equal "DisplayHolderOne 1 1"
        string (DisplayHolderOne { A = 1 }) |> equal "DisplayHolderOne 1 1"
        sprintf "%A" [ DisplayHolderOne { A = 1 } ] |> equal "[DisplayHolderOne 1 1]"
        sprintf "%A" (DisplayHolderTwo(DisplayFixedText, 1)) |> equal "DisplayHolderTwo (disp u, 1)"
]
