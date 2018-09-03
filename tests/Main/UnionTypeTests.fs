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
    match jsonParse """{"tag":0,"data":1}""" with
    | IntType x -> x
    | _ -> failwith "unexpected"
    |> equal 1
    // Test StringType
    match jsonParse """{"tag":1,"data":"value1"}""" with
    | StringType x -> x
    | _ -> failwith "unexpected"
    |> equal "value1"
    // Test TupleType
    match jsonParse """{"tag":2,"data":["value1",2]}""" with
    | TupleType(x, y) -> x, y
    | _ -> failwith "unexpected"
    |> fun (x, y) ->
        x |> equal "value1"
        y |> equal 2
    // Test ObjectType
    match jsonParse """{"tag":3,"data":{"Prop1":"value1","Prop2":2}}""" with
    | ObjectType(x) -> x
    | _ -> failwith "unexpected"
    |> fun x ->
        x.Prop1 |> equal "value1"
        x.Prop2 |> equal 2

[<Test>]
let ``Union cases json stringify is as we expect``() =
    ObjectType({Prop1 = "value1"; Prop2 = 2})
    |> jsonStringify
    |> equal """{"tag":3,"data":{"Prop1":"value1","Prop2":2}}"""
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
    let tree2 = Fable.Core.JsInterop.ofJsonAsType json (tree.GetType()) :?> Tree
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
    let json = """{"$type":"Fable.Tests.UnionTypes+Tree","Leaf":5}"""
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

#if FABLE_COMPILER
open Fable.Core

[<Test>]
let ``Case testing with generic erased unions works``() =
    let strify (x: U2<int,string>) =
        match x with
        | U2.Case1 i -> "i: " + string i
        | U2.Case2 s -> "s: " + s
    U2.Case2 "foo" |> strify |> equal "s: foo"
    U2.Case1 42 |> strify |> equal "i: 42"

[<Erase>]
#endif
type DU = Int of int | Str of string

[<Test>]
let ``Case testing with erased unions works``() =
    let strify = function
        | Int i -> "i: " + string i
        | Str s -> "s: " + s
    Str "foo" |> strify |> equal "s: foo"
    Int 42 |> strify |> equal "i: 42"

#if FABLE_COMPILER
open Fable.Core
open Fable.Core.JsInterop

type DUErasedCase =
    | [<Erase>] ErasedCase of string * obj

[<Test>]
let ``Erased union case transforms properly with non const key``() =
    let k = "k"
    let ey = "ey"
    let key = k + ey // ! must be non const
    let result = keyValueList CaseRules.LowerFirst [ ErasedCase (key, "value") ]
    !!result?key |> equal "value"

[<Test>]
let ``Erased union case transorms properly with const key``() =
    let result = keyValueList CaseRules.LowerFirst [ ErasedCase ("key", "value") ]
    !!result?key |> equal "value"
#endif
