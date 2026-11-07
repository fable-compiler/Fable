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

[<StructuredFormatDisplay("{V}")>]
type DisplayGeneric<'T> = { V: 'T }

type DisplayBaseGeneric<'T>(v: 'T) =
    member _.V = v
    member inline this.Inl = [ this.V ]

[<StructuredFormatDisplay("{V}/{Inl}")>]
type DisplayDerivedClosed() =
    inherit DisplayBaseGeneric<int>(1)

[<StructuredFormatDisplay("Disp")>]
type DisplayMoney() =
    override _.ToString() = "tostr"

type DisplayInvoice() =
    override _.ToString() = "Invoice " + string (DisplayMoney())

type DisplayInvoiceFormat() =
    override _.ToString() = sprintf "%O" (DisplayMoney())

type DisplayInvoiceToString() =
    override _.ToString() = (DisplayMoney()).ToString()

type DisplayInvoiceHolder = { H: DisplayInvoice }

type DisplayInvoiceUnion = DisplayInvoiceHeld of DisplayInvoiceFormat

[<StructuredFormatDisplay("{IV}")>]
type DisplayInvoiceValue = { IV: DisplayInvoice }

[<StructuredFormatDisplay("{S}")>]
type DisplayGetterSprintf(deep: obj) =
    member _.S = sprintf "%A" deep

type DisplayToStringSprintf(deep: obj) =
    override _.ToString() = sprintf "%A" deep

[<StructuredFormatDisplay("{K}>{N}")>]
type DisplayRevisit =
    { K: string; mutable Next: DisplayRevisit option }
    member this.N =
        match this.Next with
        | Some next -> sprintf "%A" next
        | None -> "end"

type DisplayBackHolder = { mutable Back: obj }

[<StructuredFormatDisplay("{TH}")>]
type DisplayThroughRecord = { TH: DisplayBackHolder }

[<StructuredFormatDisplay("{R}")>]
type DisplayFormattable =
    { R: int }
    interface System.IFormattable with
        member _.ToString(_, _) = "fmt"

[<StructuredFormatDisplay("{Self}{Self}z")>]
type DisplaySelfTrailing =
    { A: int }
    member this.Self = this

[<StructuredFormatDisplay("G{Name}")>]
type DisplayInlineTypeInfo<'T>() =
    member inline _.Name = typeof<'T>.Name

exception DisplayCarrier of obj

[<StructuredFormatDisplay("A{E}")>]
type DisplayThroughException() as this =
    member _.E = DisplayCarrier(box this)

[<StructuredFormatDisplay("Deep{Next}")>]
type DisplayDeepException(n: int) =
    member _.Next =
        if n > 0 then
            box (DisplayCarrier(DisplayDeepException(n - 1)))
        else
            box "end"

[<StructuredFormatDisplay("{N}")>]
type DisplayFallbackUnion =
    | DisplayFallbackA of string
    member _.N: string = null

type DisplayFallbackHolder = DisplayFallbackHeld of DisplayFallbackUnion

type DisplayUnionCarrier = DisplayUnionCarried of obj

[<StructuredFormatDisplay("U{C}")>]
type DisplayThroughUnion() as this =
    member _.C = DisplayUnionCarried(box this)

[<StructuredFormatDisplay("over {X}")>]
type DisplayOverride =
    { X: int }
    override _.ToString() = "custom"

type DisplayOverrideHolder = { F: DisplayOverride }

type DisplayOverrideUnionHolder = DisplayOverrideHeld of DisplayOverride

[<StructuredFormatDisplay("uo {D}")>]
type DisplayUnionOverride =
    | DisplayUnionOverride of int
    member _.D = 1
    override _.ToString() = "uo-ts"

type IDisplayToString =
    abstract ToString: unit -> string

[<StructuredFormatDisplay("i {A}")>]
type DisplayInterfaceToString =
    { A: int }
    interface IDisplayToString with
        member _.ToString() = "iface"

[<StructuredFormatDisplay("s {Show}")>]
type DisplayGetterCallsString =
    { I: DisplayOverride }
    member this.Show = string this.I

[<StructuredFormatDisplay("{N}")>]
type DisplayNullOverride =
    { N: string }
    override _.ToString() = "null-ts"

[<StructuredFormatDisplay("class {A}")>]
type DisplayClass(a: int) =
    member _.A = a

[<StructuredFormatDisplay("cover {A}")>]
type DisplayClassOverride(a: int) =
    member _.A = a
    override _.ToString() = "cover-ts"

type DisplayClassDerived(a: int) =
    inherit DisplayClass(a)

type DisplayClassDerivedOverride(a: int) =
    inherit DisplayClass(a)
    override _.ToString() = "derived-ts"

[<StructuredFormatDisplay("own {A} {B}")>]
type DisplayClassDerivedOwn(a: int) =
    inherit DisplayClass(a)
    member _.B = 2

[<StructuredFormatDisplay("{N}")>]
type DisplayClassNullOverride() =
    member _.N: string = null
    override _.ToString() = "nullover-ts"

[<StructuredFormatDisplay("{secret}")>]
type DisplayClassLet(a: int) =
    let secret = a
    member _.S = secret

[<StructuredFormatDisplay("{X}")>]
type DisplayClassVal =
    val X: int
    new(x) = { X = x }

[<StructuredFormatDisplay("{P}")>]
type DisplayClassMemberVal() =
    member val P = 5 with get, set

[<Struct; StructuredFormatDisplay("cs {X}")>]
type DisplayStructVal =
    val X: int
    new(x) = { X = x }

[<StructuredFormatDisplay("{Boom}")>]
type DisplayClassThrow() =
    member _.Boom: string = failwith "boom"

[<StructuredFormatDisplay("{Self}")>]
type DisplayClassSelf() =
    member this.Self = this

[<AbstractClass; StructuredFormatDisplay("abs {V}")>]
type DisplayClassAbstract() =
    abstract V: int

type DisplayClassImpl() =
    inherit DisplayClassAbstract()
    override _.V = 9

[<StructuredFormatDisplay("{Missing}")>]
type DisplayClassMissing() = class end

[<StructuredFormatDisplay("g {V}")>]
type DisplayClassGeneric<'T>(v: 'T) =
    member _.V = v

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
#if FABLE_COMPILER
        let isNotFound (text: string) = text.StartsWith "<StructuredFormatDisplay exception: Method '" |> equal true
        sprintf "%A" ({ V = 3 }: DisplayInlineGeneric<int>) |> isNotFound
        sprintf "%A" (DisplayInlineTypeInfo<int>()) |> isNotFound
#else
        sprintf "%A" ({ V = 3 }: DisplayInlineGeneric<int>) |> equal "[3]"
        sprintf "%A" (DisplayInlineTypeInfo<int>()) |> equal "GInt32"
#endif

    testCase "StructuredFormatDisplay keeps union parentheses when a display falls back" <| fun () ->
        sprintf "%A" (DisplayFallbackHeld(DisplayFallbackA "x")) |> equal "DisplayFallbackHeld (DisplayFallbackA \"x\")"
        string (DisplayFallbackHeld(DisplayFallbackA "x")) |> equal "DisplayFallbackHeld (DisplayFallbackA \"x\")"
        sprintf "%A" [ DisplayFallbackHeld(DisplayFallbackA "x") ] |> equal "[DisplayFallbackHeld (DisplayFallbackA \"x\")]"

    testCase "StructuredFormatDisplay stops at cycles through unions" <| fun () ->
        (sprintf "%A" (DisplayThroughUnion())).Length < 200 |> equal true

    testCase "StructuredFormatDisplay stops at cycles through exceptions" <| fun () ->
        (sprintf "%A" (DisplayThroughException())).Length < 200 |> equal true
        (sprintf "%A" (DisplayDeepException 6)).Length < 400 |> equal true

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
        let isNotFound (text: string) = text.StartsWith "<StructuredFormatDisplay exception: Method '" |> equal true
        let notFound name = $"<StructuredFormatDisplay exception: Method 'Fable.Tests.StructuredFormatDisplay+{name}' not found.>"
        sprintf "%A" ({ A = 1 }: DisplayIndexed) |> equal (notFound "DisplayIndexed.Idx")
        sprintf "%A" ({ A = 1 }: DisplayMissing) |> equal (notFound "DisplayMissing.Missing")
        string ({ A = 1 }: DisplayMissing) |> equal (notFound "DisplayMissing.Missing")
        sprintf "%A" ({ A = 1 }: DisplayMissingAfterValue) |> equal (notFound "DisplayMissingAfterValue.Missing")
        sprintf "%A" ({ N = null }: DisplayNullThenMissing) |> equal "{ N = null }"
        sprintf "%A" ({ A = 1 }: DisplayEmptyName) |> isNotFound
        sprintf "%A" ({ A = 1 }: DisplayStatic) |> equal (notFound "DisplayStatic.St")
        sprintf "%A" ({ A = 1 }: DisplayMethod) |> equal (notFound "DisplayMethod.Meth")
        sprintf "%A" DisplayMissingA |> isNotFound
        sprintf "%A" (DisplayMissingB 1) |> isNotFound
        sprintf "%A" DisplayManyA |> equal (notFound "DisplayMissingManyCases.Missing")
        sprintf "%A" (DisplayManyE 1) |> isNotFound
        sprintf "%A" DisplayNullaryA |> equal (notFound "DisplayMissingNullary.Missing")

    testCase "StructuredFormatDisplay reads union fields per case" <| fun () ->
        let isNotFound (text: string) = text.StartsWith "<StructuredFormatDisplay exception: Method '" |> equal true
        sprintf "%A" (DisplayItemA 1) |> equal "1"
        sprintf "%A" (DisplayItemB "x") |> equal "x"
        sprintf "%A" (DisplayItemC(1, 2)) |> isNotFound
        sprintf "%A" (DisplayNamedA 1) |> equal "1"
        sprintf "%A" (DisplayNamedB "s") |> equal "s"
        sprintf "%A" DisplayNamedC |> isNotFound
        sprintf "%A" (DisplayItems(1, "x")) |> equal "1-x"
        sprintf "%A" DisplayTagA |> equal "0"
        sprintf "%A" (DisplayTagB 1) |> equal "1"

    testCase "StructuredFormatDisplay spaces adjacent non-string values" <| fun () ->
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
        sprintf "%A" { A = 1; E = ""; B = 2 } |> equal "1 2"
        sprintf "%A" { A = ""; E = ""; B = 1 } |> equal "1"
        show 1 s |> equal "1 n"
        show v 1 |> equal "2 1"
        show p 1 |> equal "P2 1"
        show 1 q |> equal "1 2Q"
        show 1 p |> equal "1 P2"
        show [ 1 ] 1 |> equal "[1] 1"
        show [ s ] 1 |> equal "[n] 1"
        show true 1 |> equal "true 1"
        show u 1 |> equal "DisplayPlainB 1 1"
        show DisplayPlainA 1 |> equal "DisplayPlainA 1"
        show r 1 |> equal "{ F = 1 } 1"
        sprintf "%A" { A = "s"; E = ""; B = "s" } |> equal "ss"
        sprintf "%A" { A = 1; E = "s"; B = 2 } |> equal "1s2"

    testCase "StructuredFormatDisplay values are not wrapped in union parentheses" <| fun () ->
        sprintf "%A" (Ok DisplayFixedText: Result<DisplayFixedText, int>) |> equal "Ok disp u"
        sprintf "%A" (DisplayHolderOne { A = 1 }) |> equal "DisplayHolderOne 1 1"
        string (DisplayHolderOne { A = 1 }) |> equal "DisplayHolderOne 1 1"
        sprintf "%A" [ DisplayHolderOne { A = 1 } ] |> equal "[DisplayHolderOne 1 1]"
        sprintf "%A" (DisplayHolderTwo(DisplayFixedText, 1)) |> equal "DisplayHolderTwo (disp u, 1)"

    testCase "StructuredFormatDisplay leaves user ToString bodies in normal mode" <| fun () ->
        sprintf "%A" (DisplayInvoice()) |> equal "Invoice tostr"
        sprintf "%A" [ DisplayInvoice() ] |> equal "[Invoice tostr]"
        sprintf "%A" (DisplayInvoiceFormat()) |> equal "tostr"
        sprintf "%A" (DisplayInvoiceToString()) |> equal "tostr"
        string (DisplayInvoice()) |> equal "Invoice tostr"
        string { H = DisplayInvoice() } |> equal "{ H = Invoice tostr }"
        sprintf "%A" { H = DisplayInvoice() } |> equal "{ H = Invoice tostr }"
        string (DisplayInvoiceHeld(DisplayInvoiceFormat())) |> equal "DisplayInvoiceHeld tostr"
        sprintf "%A" (DisplayMoney()) |> equal "Disp"
        string (DisplayMoney()) |> equal "tostr"
        sprintf "%A" { IV = DisplayInvoice() } |> equal "Invoice tostr"

    testCase "StructuredFormatDisplay starts fresh for nested formatting" <| fun () ->
        let deep: DisplayDepth2 = { C = { D = { S = "s" } } }
        sprintf "%A" (DisplayGetterSprintf(box deep)) |> equal "BCDs"
        sprintf "%A" { V = DisplayToStringSprintf(box deep) } |> equal "BCDs"
        let a = { K = "a"; Next = None }
        let b = { K = "b"; Next = Some a }
        a.Next <- Some b
        b.Next <- None
        sprintf "%A" a |> equal "a>b>end"

    testCase "StructuredFormatDisplay keeps the cycle check through library printers" <| fun () ->
        let holder = { Back = null }
        let value: DisplayThroughRecord = { TH = holder }
        holder.Back <- box value
        sprintf "%A" value |> equal "{ Back = ... }"
        sprintf "%A" value |> equal "{ Back = ... }"

    testCase "StructuredFormatDisplay treats IFormattable as not overriding ToString" <| fun () ->
        let v: DisplayFormattable = { R = 1 }
        sprintf "%A" v |> equal "1"
        v.ToString() |> equal "1"
        sprintf "%O" v |> equal "1"

    testCase "StructuredFormatDisplay spaces repeated cycle markers" <| fun () ->
        let v: DisplaySelfTrailing = { A = 1 }
        sprintf "%A" v |> equal "... ...z"
        sprintf "%A" v |> equal "... ...z"

    testCase "StructuredFormatDisplay spaces around an empty nested string" <| fun () ->
        sprintf "%A" { L = 1; R = ({ S = "" }: DisplayEndsWithString) } |> equal "1 "

    testCase "StructuredFormatDisplay prints dates like %A" <| fun () ->
        let date = System.DateTime(2020, 1, 2, 3, 4, 5)
        let time = System.TimeSpan.FromSeconds 1.0
        let offset = System.DateTimeOffset(date, System.TimeSpan.Zero)
        let dateOnly = System.DateOnly(2020, 1, 2)
        let timeOnly = System.TimeOnly(1, 2, 3)
        sprintf "%A" { V = date } |> equal (sprintf "%A" date)
        sprintf "%A" { V = time } |> equal (sprintf "%A" time)
        sprintf "%A" { V = offset } |> equal (sprintf "%A" offset)
        sprintf "%A" { V = dateOnly } |> equal (sprintf "%A" dateOnly)
        sprintf "%A" { V = timeOnly } |> equal (sprintf "%A" timeOnly)
        string { V = date } |> equal (sprintf "%A" date)

    testCase "StructuredFormatDisplay is used by %A when ToString is overridden" <| fun () ->
        let r: DisplayOverride = { X = 1 }
        sprintf "%A" r |> equal "over 1"
        sprintf "%+A" r |> equal "over 1"
        $"%A{r}" |> equal "over 1"
        string r |> equal "custom"
        r.ToString() |> equal "custom"
        sprintf "%O" r |> equal "custom"
        $"{r}" |> equal "custom"
        sprintf "%A" [ r ] |> equal "[over 1]"
        sprintf "%A" (Seq.singleton r) |> equal "seq [over 1]"
        sprintf "%A" (Map.ofList [ 1, r ]) |> equal "map [(1, over 1)]"
        string [ r ] |> equal "[custom]"
        sprintf "%O" [ r ] |> equal "[custom]"
        sprintf "%A" { F = r } |> equal "{ F = over 1 }"
        string { F = r } |> equal "{ F = over 1 }"
        sprintf "%A" (DisplayOverrideHeld r) |> equal "DisplayOverrideHeld over 1"
        string (DisplayOverrideHeld r) |> equal "DisplayOverrideHeld over 1"
        sprintf "%A" (DisplayUnionOverride 1) |> equal "uo 1"
        string (DisplayUnionOverride 1) |> equal "uo-ts"
        sprintf "%A" ({ A = 1 }: DisplayInterfaceToString) |> equal "i 1"
        sprintf "%A" { I = r } |> equal "s custom"
        sprintf "%A" ({ N = null }: DisplayNullOverride) |> equal "{ N = null }"

    testCase "StructuredFormatDisplay is used by %A on classes" <| fun () ->
        let notFound name = $"<StructuredFormatDisplay exception: Method 'Fable.Tests.StructuredFormatDisplay+{name}' not found.>"
        sprintf "%A" (DisplayClass 1) |> equal "class 1"
        sprintf "%A" [ DisplayClass 1 ] |> equal "[class 1]"
        sprintf "%A" (DisplayClassOverride 1) |> equal "cover 1"
        string (DisplayClassOverride 1) |> equal "cover-ts"
        $"{DisplayClassOverride 1}" |> equal "cover-ts"
        sprintf "%A" (DisplayClassDerived 1) |> equal "class 1"
        sprintf "%A" (DisplayClassDerivedOverride 1) |> equal "class 1"
        string (DisplayClassDerivedOverride 1) |> equal "derived-ts"
        sprintf "%A" (DisplayClassDerivedOwn 1) |> equal "own 1 2"
        sprintf "%A" (DisplayClassNullOverride()) |> equal "nullover-ts"
        sprintf "%A" (DisplayClassLet 1) |> equal (notFound "DisplayClassLet.secret")
        sprintf "%A" (DisplayClassVal 1) |> equal "1"
        sprintf "%A" (DisplayClassMemberVal()) |> equal "5"
        sprintf "%A" (DisplayStructVal 1) |> equal "cs 1"
        sprintf "%A" (DisplayClassThrow())
        |> equal "<StructuredFormatDisplay exception: Exception has been thrown by the target of an invocation.>"
        sprintf "%A" (DisplayClassSelf()) |> equal "..."
        sprintf "%A" (DisplayClassImpl()) |> equal "abs 9"
        sprintf "%A" (DisplayClassMissing()) |> equal (notFound "DisplayClassMissing.Missing")
        sprintf "%A" (DisplayClassGeneric 1) |> equal "g 1"
        sprintf "%A" (DisplayDerivedClosed()) |> equal "1/[1]"
]
