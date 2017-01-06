[<Util.Testing.TestFixture>]
module Fable.Tests.UnionTypes
open Util.Testing
open Fable.Tests.Util

type Gender = Male | Female

[<Test>]
let ``Union cases matches with no arguments can be generated``() =
    let x = Male
    match x with
    | Female -> true
    | Male -> false
    |> equal false

type Either<'TL,'TR> =
    | Left of 'TL
    | Right of 'TR
    override x.ToString() =
      match x with
      | Left y -> y.ToString()
      | Right y -> y.ToString()

[<Test>]
let ``Union cases matches with one argument can be generated``() =
    let x = Left "abc"
    match x with
    | Left data -> data
    | Right _ -> failwith "unexpected"
    |> equal "abc"

[<Test>]
let ``Union methods can be generated``() =
    let x = Left 5
    x.ToString()
    |> equal "5"

[<Test>]
let ``Nested pattern matching works``() =
    let x = Right(Left 5)
    match x with
    | Left _ -> failwith "unexpected"
    | Right x ->
        match x with
        | Left x -> x
        | Right _ -> failwith "unexpected"
    |> equal 5

type TestUnion =
   | Case0
   | Case1 of string
   | Case2 of string * string
   | Case3 of string * string * string

[<Test>]
let ``Union cases matches with many arguments can be generated``() =
    let x = Case3("a", "b", "c")
    match x with
    | Case3(a, b, c) -> a + b + c
    | _ -> failwith "unexpected"
    |> equal "abc"

[<Test>]
let ``Pattern matching with common targets works``() =
    let x = Case2("a", "b")
    match x with
    | Case0 -> failwith "unexpected"
    | Case1 _
    | Case2 _ -> "a"
    | Case3(a, b, c) -> a + b + c
    |> equal "a"

type TestUnion2 =
    | Tag of string
    | NewTag of string

[<Test>]
let ``Union cases called Tag still work (bug due to Tag field)``() =
    let x = Tag "abc"
    match x with
    | Tag x -> x
    | _ -> failwith "unexpected"
    |> equal "abc"

let (|Functional|NonFunctional|) (s: string) =
    match s with
    | "fsharp" | "haskell" | "ocaml" -> Functional
    | _ -> NonFunctional

[<Test>]
let ``Comprehensive active patterns work``() =
    let isFunctional = function
        | Functional -> true
        | NonFunctional -> false
    isFunctional "fsharp" |> equal true
    isFunctional "csharp" |> equal false
    isFunctional "haskell" |> equal true

let (|Small|Medium|Large|) i =
    if i < 3 then Small 5
    elif i >= 3 && i < 6 then Medium "foo"
    else Large

[<Test>]
let ``Comprehensive active patterns can return values``() =
    let measure = function
        | Small i -> string i
        | Medium s -> s
        | Large -> "bar"
    measure 0 |> equal "5"
    measure 10 |> equal "bar"
    measure 5 |> equal "foo"

let (|FSharp|_|) (document : string) =
    if document = "fsharp" then Some FSharp else None

[<Test>]
let ``Partial active patterns which don't return values work``() = // See #478
    let isFunctional = function
        | FSharp -> "yes"
        | "scala" -> "fifty-fifty"
        | _ -> "dunno"
    isFunctional "scala" |> equal "fifty-fifty"
    isFunctional "smalltalk" |> equal "dunno"
    isFunctional "fsharp" |> equal "yes"

let (|A|) n = n

[<Test>]
let ``Active patterns can be combined with union case matching``() = // See #306
    let test = function
        | Some(A n, Some(A m)) -> n + m
        | _ -> 0
    Some(5, Some 2) |> test |> equal 7
    Some(5, None) |> test |> equal 0
    None |> test |> equal 0

#if FABLE_COMPILER
open Fable.Core

type JsonTypeInner = {
    Prop1: string
    Prop2: int
}

type JsonTestUnion =
    | IntType of int
    | StringType of string
    | TupleType of string * int
    | ObjectType of JsonTypeInner

[<Emit("JSON.parse($0)")>]
let jsonParse (json: string) = jsNative

[<Emit("JSON.stringify($0)")>]
let jsonStringify (json): string = jsNative

[<Test>]
let ``Pattern matching json parse union cases still works``() =
    // Test IntType
    match jsonParse """{"Case":"IntType","Fields":[1]}""" with
    | IntType x -> x
    | _ -> failwith "unexpected"
    |> equal 1
    // Test StringType
    match jsonParse """{"Case":"StringType","Fields":["value1"]}""" with
    | StringType x -> x
    | _ -> failwith "unexpected"
    |> equal "value1"
    // Test TupleType
    match jsonParse """{"Case":"TupleType","Fields":["value1",2]}""" with
    | TupleType(x, y) -> x, y
    | _ -> failwith "unexpected"
    |> fun (x, y) ->
        x |> equal "value1"
        y |> equal 2
    // Test ObjectType
    match jsonParse """{"Case":"ObjectType","Fields":[{"Prop1":"value1","Prop2":2}]}""" with
    | ObjectType(x) -> x
    | _ -> failwith "unexpected"
    |> fun x ->
        x.Prop1 |> equal "value1"
        x.Prop2 |> equal 2

[<Test>]
let ``Union cases json stringify is as we expect``() =
    ObjectType({Prop1 = "value1"; Prop2 = 2})
    |> jsonStringify
    |> equal """{"Case":"ObjectType","Fields":[{"Prop1":"value1","Prop2":2}]}"""
#endif

type Tree =
    | Leaf of int
    | Branch of Tree[]
    member this.Sum() =
        match this with
        | Leaf i -> i
        | Branch trees -> trees |> Seq.map (fun x -> x.Sum()) |> Seq.sum

[<Test>]
let ``Unions can be JSON serialized forth and back``() =
    let tree = Branch [|Leaf 1; Leaf 2; Branch [|Leaf 3; Leaf 4|]|]
    let sum1 = tree.Sum()
    #if FABLE_COMPILER
    let json = Fable.Core.JsInterop.toJson tree
    let tree2 = Fable.Core.JsInterop.ofJson<Tree> json
    let sum2 = tree2.Sum()
    equal true (box tree2 :? Tree) // Type is kept
    equal true (sum1 = sum2) // Prototype methods can be accessed
    let json = Fable.Core.JsInterop.toJsonWithTypeInfo tree
    let tree2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Tree> json
    #else
    let json = Newtonsoft.Json.JsonConvert.SerializeObject tree
    let tree2 = Newtonsoft.Json.JsonConvert.DeserializeObject<Tree> json
    #endif
    let sum2 = tree2.Sum()
    equal true (box tree2 :? Tree) // Type is kept
    equal true (sum1 = sum2) // Prototype methods can be accessed


#if !FABLE_COMPILER
open FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

type UnionTypeInfoConverter() =
    inherit JsonConverter()
    override x.CanConvert t = FSharpType.IsUnion t
    override x.ReadJson(reader, t, _, serializer) =
        let token = JObject()
        for prop in JToken.ReadFrom(reader).Children<JProperty>() do
            if prop.Name <> "$type" then
                token.Add(prop)
        token.ToObject(t)
    override x.WriteJson(writer, v, serializer) =
        let t = v.GetType()
        let typeFulName = t.FullName.Substring(0, t.FullName.LastIndexOf("+"))
        let uci, fields = FSharpValue.GetUnionFields(v, t)
        writer.WriteStartObject()
        writer.WritePropertyName("$type")
        writer.WriteValue(typeFulName)
        writer.WritePropertyName("Case")
        writer.WriteValue(uci.Name)
        writer.WritePropertyName("Fields")
        writer.WriteStartArray()
        for field in fields do writer.WriteValue(field)
        writer.WriteEndArray()
        writer.WriteEndObject()
#endif

[<Test>]
let ``Unions serialized with Json.NET can be deserialized``() =
    #if FABLE_COMPILER
    let json = """{"$type":"Fable.Tests.UnionTypes+Tree","Case":"Leaf","Fields":[5]}"""
    let x2 = Fable.Core.JsInterop.ofJsonWithTypeInfo<Tree> json
    #else
    let x = Leaf 5
    let settings =
        JsonSerializerSettings(
            Converters = [|UnionTypeInfoConverter()|],
            TypeNameHandling = TypeNameHandling.All)
    let json = JsonConvert.SerializeObject(x, settings)
    let x2 = JsonConvert.DeserializeObject<Tree>(json, settings)
    #endif
    x2.Sum() |> equal 5

[<Test>]
let ``Option.isSome/isNone works``() =
    let o1 = None
    let o2 = Some 5
    Option.isNone o1 |> equal true
    Option.isSome o1 |> equal false
    Option.isNone o2 |> equal false
    Option.isSome o2 |> equal true

[<Test>]
let ``Option.IsSome/IsNone works``() =
    let o1 = None
    let o2 = Some 5
    o1.IsNone |> equal true
    o1.IsSome |> equal false
    o2.IsNone |> equal false
    o2.IsSome |> equal true

[<Test>]
let ``Option.iter works``() = // See #198
    let mutable res = false
    let getOnlyOnce =
        let mutable value = Some "Hello"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.iter (fun s -> if s = "Hello" then res <- true)
    equal true res

[<Test>]
let ``Option.map works``() =
    let mutable res = false
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.map ((+) "Hello ") |> equal (Some "Hello Alfonso")

[<Test>]
let ``Option.bind works``() =
    let mutable res = false
    let getOnlyOnce =
        let mutable value = Some "Alfonso"
        fun () -> match value with Some x -> value <- None; Some x | None -> None
    getOnlyOnce() |> Option.bind ((+) "Hello " >> Some) |> equal (Some "Hello Alfonso")

[<Test>]
let ``Option.filter works``() = // See #390
    let optionToString opt =
        match opt with
        | None -> "None"
        | Some value -> sprintf "Some %s" value
    Some 7 |> Option.filter (fun _ -> false) |> Option.map string |> optionToString |> equal "None"
    Some 7 |> Option.filter (fun _ -> true)  |> Option.map string |> optionToString |> equal "Some 7"
    Some "A" |> Option.filter (fun _ -> false) |> optionToString |> equal "None"
    Some "A" |> Option.filter (fun _ -> true) |> optionToString |> equal "Some A"

[<Test>]
let ``Option.fold works``() =
    (5, None) ||> Option.fold (*) |> equal 5
    (5, Some 7) ||> Option.fold (*) |> equal 35

[<Test>]
let ``Option.foldBack works``() =
    (None, 5) ||> Option.foldBack (*) |> equal 5
    (Some 7, 5) ||> Option.foldBack (*) |> equal 35

[<Test>]
let ``Option.toArray works``() =
    None |> Option.toArray |> equal [||]
    Some (Leaf 7) |> Option.toArray |> equal [|Leaf 7|]

[<Test>]
let ``Option.toList works``() =
    None |> Option.toList |> equal []
    Some (Leaf 7) |> Option.toList |> equal [Leaf 7]

type OptTest = OptTest of int option

[<Test>]
let ``Different ways of providing None to a union case should be equal``() = // See #231
    let value = None
    equal true ((OptTest None) = (value |> OptTest))

[<Test>]
let ``Different ways of providing None to a function should be equal``() = // See #231
    let f x = x
    let f2 x = x = None
    let value = None
    equal true ((f None) = (value |> f))
    equal true (f2 None)
    equal true (f2 value)
    equal false (Some 5 |> f2)

[<Test>]
let ``Accessing an option value gives correct expression type``() = // See #285
    let test (x: float option) =
        match x with
        | Some y -> y + 3.
        | None -> 0.
    test(Some 4.) |> equal 7.

[<Test>]
let ``Mixing refs and options works``() = // See #238
    let res = ref 0
    let setter, getter =
        let slot = ref None
        (fun f -> slot.Value <- Some f),
        (fun v -> slot.Value.Value v)
    setter (fun i -> res := i + 2)
    getter 5
    equal 7 !res

type MyExUnion = MyExUnionCase of exn

[<Test>]
let ``Types can have Exception fields``() =
    let (MyExUnionCase ex) =
        try
            exn "foo" |> raise
        with ex -> MyExUnionCase ex
    ex.Message |> equal "foo"

#if FABLE_COMPILER
open Fable.Core

type Wrapper(s: string) =
    member x.Value = s |> Seq.rev |> Seq.map string |> String.concat ""

[<Test>]
let ``Erased union type testing works``() =
    let toString (arg: U3<string, int, Wrapper>) =
        match arg with
        | U3.Case1 s -> s
        | U3.Case2 i -> i * 2 |> string
        | U3.Case3 t -> t.Value
    U3.Case1 "HELLO" |> toString |> equal "HELLO"
    U3.Case2 3 |> toString |> equal "6"
    "HELLO" |> Wrapper |> U3.Case3 |> toString |> equal "OLLEH"
#endif

[<RequireQualifiedAccess>]
type MyUnion =
| Case1
| Case2
| Case3

type R = {
    Name: string
    Case: MyUnion
}

[<Test>]
let ``Equality works in filter``() =
    let original = [| { Name = "1"; Case = MyUnion.Case1 } ; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
    original
    |> Array.filter (fun r -> r.Case = MyUnion.Case1)
    |> Array.length
    |> equal 2

[<Test>]
let ``Pattern matching works in filter``() =
    let original = [| { Name = "1"; Case = MyUnion.Case1 } ; { Name = "2"; Case = MyUnion.Case1 }; { Name = "3"; Case = MyUnion.Case2 }; { Name = "4"; Case = MyUnion.Case3 } |]
    original
    |> Array.filter (fun r -> match r.Case with MyUnion.Case1 -> true | _ -> false)
    |> Array.length
    |> equal 2