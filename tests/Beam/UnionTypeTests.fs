module Fable.Tests.UnionTypes

open Fable.Tests.Util
open Util.Testing

type Gender = Male | Female

type Either<'TL, 'TR> =
    | Left of 'TL
    | Right of 'TR

type Shape =
    | Circle of radius: float
    | Square of side: float
    | Rectangle of width: float * height: float

type MyUnion =
    | Case0
    | Case1 of string
    | Case2 of string * string
    | Case3 of string * string * string

type MyUnion2 =
    | Tag of string
    | NewTag of string

type IntegrityLevel =
    | Untrusted
    | Trusted

    override this.ToString() =
        match this with
        | Untrusted -> "Untrusted"
        | Trusted -> "Trusted"

type IntUnion =
    | IntCase1 of int
    | IntCase2 of int
    | IntCase3 of int

[<Struct>]
type StructPoint2D =
    | Planar2D of float * float
    | Infinity

type WrappedUnion =
    | AString of string

type T1 = T1

type ForwardedValue = ForwardedValue of obj

type GenericForwardedValue<'T> = GenericForwardedValue of 'T

type ForwardedPayload = { Pid: obj; Reason: obj }

type RuntimeTaggedUnion =
    | [<CompiledName("FORWARDED.Empty")>] RuntimeEmpty
    | [<CompiledName("FORWARDED.Payload")>] RuntimePayload of obj
    | [<CompiledName("FORWARDED.Pair")>] RuntimePair of obj * obj

let private unwrapForwardedValue (message: obj) =
    match message with
    | :? ForwardedValue as forwarded ->
        let (ForwardedValue value) = forwarded
        Some value
    | _ -> None

let private unwrapGenericForwardedValue<'T> (message: obj) =
    match message with
    | :? GenericForwardedValue<'T> as forwarded ->
        let (GenericForwardedValue value) = forwarded
        Some value
    | _ -> None

let private isMyUnion (message: obj) = message :? MyUnion
let private isEither (message: obj) = message :? Either<int, string>
let private isGender (message: obj) = message :? Gender
let private isT1 (message: obj) = message :? T1
let private isRuntimeTaggedUnion (message: obj) = message :? RuntimeTaggedUnion

[<Fact>]
let ``test boxed single case union type test unwraps payload`` () =
    unwrapForwardedValue (box (ForwardedValue (box 42))) |> equal (Some (box 42))

[<Fact>]
let ``test boxed generic union type test unwraps payload`` () =
    unwrapGenericForwardedValue<int> (box (GenericForwardedValue 42)) |> equal (Some 42)
    unwrapGenericForwardedValue<string> (box (GenericForwardedValue "value")) |> equal (Some "value")

[<Fact>]
let ``test boxed multi case union type tests recognize every case`` () =
    [ Case0; Case1 "one"; Case2 ("one", "two"); Case3 ("one", "two", "three") ]
    |> List.iter (fun value -> isMyUnion (box value) |> equal true)
    [ Left 42; Right "value" ]
    |> List.iter (fun value -> isEither (box value) |> equal true)

[<Fact>]
let ``test boxed fieldless union type tests recognize atoms`` () =
    isGender (box Male) |> equal true
    isGender (box Female) |> equal true
    isT1 (box T1) |> equal true
    isT1 (box Male) |> equal false
    isGender (box T1) |> equal false

[<Fact>]
let ``test boxed union type tests honor compiled case names`` () =
    [ RuntimeEmpty; RuntimePayload (box 42); RuntimePair (box 42, null) ]
    |> List.iter (fun value -> isRuntimeTaggedUnion (box value) |> equal true)

[<Fact>]
let ``test boxed union type test evaluates its input once`` () =
    let mutable calls = 0
    let next () =
        calls <- calls + 1
        box (RuntimePair (box 42, null))
    (next () :? RuntimeTaggedUnion) |> equal true
    calls |> equal 1

[<Fact>]
let ``test boxed union wrappers preserve null and child exit shaped payloads`` () =
    unwrapForwardedValue (box (ForwardedValue null)) |> equal (Some null)
    unwrapGenericForwardedValue<obj> (box (GenericForwardedValue null)) |> equal (Some null)
    let payload = { Pid = box 42; Reason = null }
    unwrapForwardedValue (box (ForwardedValue (box payload))) |> equal (Some (box payload))
    unwrapGenericForwardedValue<ForwardedPayload> (box (GenericForwardedValue payload)) |> equal (Some payload)
    unwrapForwardedValue (box payload) |> equal None
    unwrapGenericForwardedValue<ForwardedPayload> (box payload) |> equal None

[<Fact>]
let ``test boxed union type tests reject unrelated values`` () =
    [ null; box 42; box true; box "value"; box (ref 42); box [| 42 |]
      box {| Pid = 42; Reason = "normal" |}; box (Map.ofList [ "pid", 42 ])
      box (42, "value"); box Male; box T1 ]
    |> List.iter (fun value ->
        unwrapForwardedValue value |> equal None
        unwrapGenericForwardedValue<int> value |> equal None
        isMyUnion value |> equal false
        isEither value |> equal false
        isRuntimeTaggedUnion value |> equal false)

#if FABLE_COMPILER_BEAM
[<Fable.Core.Emit("[#{pid => erlang:self(), reason => normal}, erlang:make_ref(), unrelated, {}, forwarded_value, {forwarded_value}, {forwarded_value, 42, extra}, generic_forwarded_value, {generic_forwarded_value}, {generic_forwarded_value, 42, extra}, {other, 42}, {42, forwarded_value}, {case0}, case1, {case1}, {case1, 42, extra}, {case2, 42}, {case2, 42, 43, extra}, {case3, 42, 43}, {case3, 42, 43, 44, extra}, {left}, {left, 42, extra}, {'FORWARDED.Empty'}, 'FORWARDED.Payload', {'FORWARDED.Payload'}, {'FORWARDED.Payload', 42, extra}, {'FORWARDED.Pair', 42}]")>]
let private unrelatedNativeUnionTerms () : obj list = Fable.Core.Util.nativeOnly

[<Fable.Core.Emit("['FORWARDED.Empty', {'FORWARDED.Payload', 42}, {'FORWARDED.Pair', 42, undefined}]")>]
let private compiledNativeUnionTerms () : obj list = Fable.Core.Util.nativeOnly

[<Fable.Core.Emit("#{pid => erlang:self(), reason => normal}")>]
let private nativeChildExitPayload () : ForwardedPayload = Fable.Core.Util.nativeOnly

[<Fact>]
let ``test boxed union wrappers preserve native child exit shaped records`` () =
    let payload = nativeChildExitPayload ()
    unwrapForwardedValue (box (ForwardedValue (box payload))) |> equal (Some (box payload))
    unwrapGenericForwardedValue<ForwardedPayload> (box (GenericForwardedValue payload)) |> equal (Some payload)
    unwrapForwardedValue (box payload) |> equal None
    unwrapGenericForwardedValue<ForwardedPayload> (box payload) |> equal None

[<Fact>]
let ``test boxed union type tests reject malformed native terms safely`` () =
    unrelatedNativeUnionTerms ()
    |> List.iter (fun value ->
        unwrapForwardedValue value |> equal None
        unwrapGenericForwardedValue<int> value |> equal None
        isMyUnion value |> equal false
        isEither value |> equal false
        isGender value |> equal false
        isT1 value |> equal false
        isRuntimeTaggedUnion value |> equal false)

[<Fact>]
let ``test boxed union type tests recognize native compiled tags`` () =
    compiledNativeUnionTerms ()
    |> List.iter (fun value -> isRuntimeTaggedUnion value |> equal true)
#endif

type CollectionOrder =
    | Zebra
    | Alpha
    | Middle

type CollectionOrderItem =
    { Key: CollectionOrder
      Value: string }

type CollectionArray = CollectionArray of int array

type DeepRecord = { Value: string }

type DeepWrappedUnion =
    | DeepWrappedA of string * DeepRecord
    | DeepWrappedB of string
    | DeepWrappedC of int
    | DeepWrappedD of DeepRecord
    | DeepWrappedE of int * int
    | DeepWrappedF of WrappedUnion
    | DeepWrappedG of {| X: DeepRecord; Y: int |}

let (|Functional|NonFunctional|) (s: string) =
    match s with
    | "fsharp" | "haskell" | "ocaml" -> Functional
    | _ -> NonFunctional

let (|Small|Medium|Large|) i =
    if i < 3 then Small 5
    elif i >= 3 && i < 6 then Medium "foo"
    else Large

let (|FSharp|_|) (document : string) =
    if document = "fsharp" then Some FSharp else None

let (|A|) n = n

[<Fact>]
let ``test collection ordering uses union declaration order`` () =
    let values = [ Middle; Zebra; Alpha ]
    let expected = [ Zebra; Alpha; Middle ]
    values |> List.sortBy id |> equal expected
    values |> List.sortByDescending id |> equal (List.rev expected)
    values |> List.min |> equal Zebra
    values |> List.max |> equal Middle

    let items =
        [ { Key = Middle; Value = "middle" }
          { Key = Zebra; Value = "zebra" }
          { Key = Alpha; Value = "alpha" } ]

    items
    |> List.sortBy (fun item -> item.Key)
    |> List.map (fun item -> item.Value)
    |> equal [ "zebra"; "alpha"; "middle" ]
    items |> List.minBy (fun item -> item.Key) |> equal items.[1]
    items |> List.maxBy (fun item -> item.Key) |> equal items.[0]

    let arrayValues = values |> List.toArray
    arrayValues |> Array.sortBy id |> equal (List.toArray expected)
    arrayValues |> Array.sortByDescending id |> equal (expected |> List.rev |> List.toArray)
    arrayValues |> Array.min |> equal Zebra
    arrayValues |> Array.max |> equal Middle

    let arrayItems = items |> List.toArray
    arrayItems |> Array.minBy (fun item -> item.Key) |> equal arrayItems.[1]
    arrayItems |> Array.maxBy (fun item -> item.Key) |> equal arrayItems.[0]

    let inPlaceValues = values |> List.toArray
    Array.sortInPlace inPlaceValues
    inPlaceValues |> equal (List.toArray expected)

    let inPlaceItems = items |> List.toArray
    Array.sortInPlaceBy (fun item -> item.Key) inPlaceItems
    inPlaceItems
    |> Array.map (fun item -> item.Value)
    |> equal [| "zebra"; "alpha"; "middle" |]

[<Fact>]
let ``test collection extrema preserve first ties and project each item once`` () =
    let items =
        [ { Key = Zebra; Value = "first" }
          { Key = Zebra; Value = "second" }
          { Key = Alpha; Value = "third" } ]

    let mutable listMinProjections = 0

    let listMin =
        items
        |> List.minBy (fun item ->
            listMinProjections <- listMinProjections + 1
            item.Key)

    listMin.Value |> equal "first"
    listMinProjections |> equal items.Length

    let mutable listMaxProjections = 0

    items
    |> List.maxBy (fun item ->
        listMaxProjections <- listMaxProjections + 1
        item.Key)
    |> ignore

    listMaxProjections |> equal items.Length

    let arrayItems = List.toArray items
    let mutable arrayMinProjections = 0

    let arrayMin =
        arrayItems
        |> Array.minBy (fun item ->
            arrayMinProjections <- arrayMinProjections + 1
            item.Key)

    arrayMin.Value |> equal "first"
    arrayMinProjections |> equal arrayItems.Length

    let mutable arrayMaxProjections = 0

    arrayItems
    |> Array.maxBy (fun item ->
        arrayMaxProjections <- arrayMaxProjections + 1
        item.Key)
    |> ignore

    arrayMaxProjections |> equal arrayItems.Length

    let first = [| 1 |]
    let second = [| 1 |]
    let values = [ CollectionArray first; CollectionArray second ]

    let (CollectionArray listMinValue) = List.min values
    obj.ReferenceEquals(listMinValue, first) |> equal true

    let (CollectionArray arrayMinValue) = values |> List.toArray |> Array.min
    obj.ReferenceEquals(arrayMinValue, first) |> equal true

[<RequireQualifiedAccess>]
type MyUnion3 =
| Case1
| Case2
| Case3

type R = {
    Name: string
    UnionCase: MyUnion3
}

[<Fact>]
let ``test Union cases matches with no arguments can be generated`` () =
    let x = Male
    match x with
    | Female -> true
    | Male -> false
    |> equal false

[<Fact>]
let ``test union ToString override works`` () =
    let value = IntegrityLevel.Untrusted
    value.ToString() |> equal "Untrusted"
    string value |> equal "Untrusted"

[<Fact>]
let ``test percent O uses union ToString override`` () =
    sprintf "%O" IntegrityLevel.Untrusted |> equal "Untrusted"
    sprintf "%d %O" 42 IntegrityLevel.Untrusted |> equal "42 Untrusted"

    let formatIntegrityLevel = sprintf "%O"
    formatIntegrityLevel IntegrityLevel.Trusted |> equal "Trusted"

[<Fact>]
let ``test Union cases matches with one argument can be generated`` () =
    let x = Left "abc"
    match x with
    | Left data -> data
    | Right _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Union case construction works`` () =
    let x = Case1 "hello"
    match x with
    | Case0 -> "zero"
    | Case1 s -> s
    | Case2 _ -> "two"
    | Case3 _ -> "three"
    |> equal "hello"

[<Fact>]
let ``test Union case with no data works`` () =
    let x = Case0
    match x with
    | Case0 -> "zero"
    | Case1 _ -> "one"
    | Case2 _ -> "two"
    | Case3 _ -> "three"
    |> equal "zero"

[<Fact>]
let ``test Shape union works`` () =
    let area shape =
        match shape with
        | Circle r -> 3.14159 * r * r
        | Square s -> s * s
        | Rectangle(w, h) -> w * h
    area (Square 5.0) |> equal 25.0
    area (Rectangle(3.0, 4.0)) |> equal 12.0

[<Fact>]
let ``test Either union works`` () =
    let describe (x: Either<int, string>) =
        match x with
        | Left n -> $"Left: {n}"
        | Right s -> $"Right: {s}"
    describe (Left 42) |> equal "Left: 42"
    describe (Right "hello") |> equal "Right: hello"

[<Fact>]
let ``test Nested match on union works`` () =
    let classify x =
        match x with
        | Left (Left _) -> "left-left"
        | Left (Right _) -> "left-right"
        | Right _ -> "right"
    classify (Left (Left 1)) |> equal "left-left"
    classify (Left (Right "x")) |> equal "left-right"
    classify (Right 0) |> equal "right"

[<Fact>]
let ``test DU structural equality works`` () =
    let x = Left 42
    let y = Left 42
    equal true (x = y)

[<Fact>]
let ``test DU structural inequality works`` () =
    let x = Left 1
    let y = Left 2
    equal true (x <> y)

[<Fact>]
let ``test DU different cases are not equal`` () =
    let x: Either<int, int> = Left 1
    let y: Either<int, int> = Right 1
    equal true (x <> y)

[<Fact>]
let ``test Union cases matches with many arguments can be generated`` () =
    let x = Case3("a", "b", "c")
    match x with
    | Case3(a, b, c) -> a + b + c
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Pattern matching with common targets works`` () =
    let x = MyUnion.Case2("a", "b")
    match x with
    | MyUnion.Case0 -> failwith "unexpected"
    | MyUnion.Case1 _
    | MyUnion.Case2 _ -> "a"
    | MyUnion.Case3(a, b, c) -> a + b + c
    |> equal "a"

[<Fact>]
let ``test Union cases called Tag still work`` () =
    let x = Tag "abc"
    match x with
    | Tag x -> x
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Fact>]
let ``test Comprehensive active patterns work`` () =
    let isFunctional = function
        | Functional -> true
        | NonFunctional -> false
    isFunctional "fsharp" |> equal true
    isFunctional "csharp" |> equal false
    isFunctional "haskell" |> equal true

[<Fact>]
let ``test Comprehensive active patterns can return values`` () =
    let measure = function
        | Small i -> string i
        | Medium s -> s
        | Large -> "bar"
    measure 0 |> equal "5"
    measure 10 |> equal "bar"
    measure 5 |> equal "foo"

[<Fact>]
let ``test Partial active patterns which do not return values work`` () =
    let isFunctional = function
        | FSharp -> "yes"
        | "scala" -> "fifty-fifty"
        | _ -> "dunno"
    isFunctional "scala" |> equal "fifty-fifty"
    isFunctional "smalltalk" |> equal "dunno"
    isFunctional "fsharp" |> equal "yes"

[<Fact>]
let ``test Active patterns can be combined with union case matching`` () =
    let test = function
        | Some(A n, Some(A m)) -> n + m
        | _ -> 0
    Some(5, Some 2) |> test |> equal 7
    Some(5, None) |> test |> equal 0
    None |> test |> equal 0

[<Fact>]
let ``test Equality works in filter`` () =
    let original = [| { Name = "1"; UnionCase = MyUnion3.Case1 } ; { Name = "2"; UnionCase = MyUnion3.Case1 }; { Name = "3"; UnionCase = MyUnion3.Case2 }; { Name = "4"; UnionCase = MyUnion3.Case3 } |]
    original
    |> Array.filter (fun r -> r.UnionCase = MyUnion3.Case1)
    |> Array.length
    |> equal 2

// --- Tests ported from Rust UnionTests ---

[<Fact>]
let ``test Struct unions work`` () =
    let distance p =
        match p with
        | Planar2D (x, y) -> sqrt (x * x + y * y)
        | Infinity -> infinity
    let p = Planar2D (3, 4)
    let res = distance p
    res |> equal 5.

[<Fact>]
let ``test Union case matching works`` () =
    let x = IntCase1 5
    let res =
        match x with
        | IntCase1 a -> a
        | IntCase2 b -> b
        | IntCase3 c -> c
    res |> equal 5

[<Fact>]
let ``test Union case equality works`` () =
    IntCase1 5 = IntCase1 5 |> equal true
    IntCase1 5 = IntCase2 5 |> equal false
    IntCase3 2 = IntCase3 3 |> equal false
    IntCase2 1 = IntCase2 1 |> equal true
    IntCase3 1 = IntCase3 1 |> equal true

let unionFnAlways1 = function
    | IntCase1 x -> x
    | _ -> -1

let unionFnRetNum = function
    | IntCase1 a -> a
    | IntCase2 b -> b
    | IntCase3 c -> c

[<Fact>]
let ``test Union fn call works`` () =
    let x = IntCase1 3
    let res = unionFnAlways1 x
    let res2 = unionFnAlways1 x
    let res3 = unionFnRetNum x
    let res4 = unionFnRetNum (IntCase2 24)
    res |> equal 3
    res2 |> equal 3
    res3 |> equal 3
    res4 |> equal 24

[<Fact>]
let ``test Union with wrapped type works`` () =
    let a = AString "hello"
    let b = match a with AString s -> s + " world"
    b |> equal "hello world"

let matchStrings = function
    | DeepWrappedA (s, d) -> d.Value + s
    | DeepWrappedB s -> s
    | DeepWrappedC _ -> "nothing"
    | DeepWrappedD d -> d.Value
    | DeepWrappedE _ -> "nothing2"
    | DeepWrappedF (AString s) -> s
    | DeepWrappedG x -> x.X.Value

let matchNumbers = function
    | DeepWrappedA _ -> 0
    | DeepWrappedB _ -> 0
    | DeepWrappedC c -> c
    | DeepWrappedD _ -> 0
    | DeepWrappedE(a, b) -> a + b
    | DeepWrappedF _ -> 0
    | DeepWrappedG x -> x.Y

[<Fact>]
let ``test Deep union with wrapped type works`` () =
    let a = DeepWrappedA (" world", { Value = "hello" })
    let b = DeepWrappedB "world"
    let c = DeepWrappedC 42
    let d = DeepWrappedD { Value = "hello" }
    let f = DeepWrappedF (AString "doublewrapped")
    let g = DeepWrappedG {| X = { Value = "G" }; Y = 365 |}
    a |> matchStrings |> equal "hello world"
    b |> matchStrings |> equal "world"
    c |> matchStrings |> equal "nothing"
    d |> matchStrings |> equal "hello"
    f |> matchStrings |> equal "doublewrapped"
    g |> matchStrings |> equal "G"

[<Fact>]
let ``test Deep union with tuped prim type works`` () =
    let e = DeepWrappedE (3, 2)
    let c = DeepWrappedC 42
    let g = DeepWrappedG {| X = { Value = "G" }; Y = 365 |}
    e |> matchNumbers |> equal 5
    c |> matchNumbers |> equal 42
    g |> matchNumbers |> equal 365

let matchStringWhenNotHello = function
    | DeepWrappedB s when s <> "hello" -> "not hello"
    | _ -> "hello"

[<Fact>]
let ``test Match with condition works`` () =
    let b1 = DeepWrappedB "hello"
    let b2 = DeepWrappedB "not"
    b1 |> matchStringWhenNotHello |> equal "hello"
    b2 |> matchStringWhenNotHello |> equal "not hello"

[<Fact>]
let ``test Union cases with no fields are physically equal`` () =
    // Nullary cases compile to plain atoms, which the BEAM VM interns, so this matches fsc.
    obj.ReferenceEquals(Male, Male) |> equal true
    obj.ReferenceEquals(Male, Female) |> equal false
    obj.ReferenceEquals(Case0, Case0) |> equal true
    obj.ReferenceEquals(T1, T1) |> equal true
#if FABLE_COMPILER
    // Unlike fsc, Erlang has no reference identity for compound terms (only atoms are
    // interned), so `=:=` here is structural equality, unlike .NET reference equality.
    obj.ReferenceEquals(Case1 "a", Case1 "a") |> equal true
#else
    obj.ReferenceEquals(Case1 "a", Case1 "a") |> equal false
#endif
