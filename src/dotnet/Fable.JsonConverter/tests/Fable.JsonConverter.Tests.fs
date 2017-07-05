namespace Fable.JsonConverter.Tests

open XUnit
open Newtonsoft.Json
open System

type Record = {
    Prop1 : string
    Prop2 : int
    Prop3 : int option
}

type Maybe<'t> =
    | Just of 't
    | Nothing


[<TestFixture>]
module JsonConverterTests =

    let converter = new Fable.JsonConverter()
    let deserialize<'a> (json : string) = JsonConvert.DeserializeObject(json, typeof<'a>, converter) :?> 'a
    let serialize (value: 'a) = JsonConvert.SerializeObject(value, converter)

    [<Fact>]
    let ``DateTime conversion works``() =
        let date = new DateTime(2017, 03, 23, 18, 30, 0)
        let serialized = serialize date
        let deserialized = deserialize<DateTime> serialized
        Assert.AreEqual(30, deserialized.Minute)
        Assert.AreEqual(18, deserialized.Hour)
        Assert.AreEqual(23, deserialized.Day)
        Assert.AreEqual(3, deserialized.Month)
        Assert.AreEqual(2017, deserialized.Year)


    [<Fact>]
    let ``Option<string> convertion works``() =
        let opt = Some "value"
        let serialized = serialize opt
        let deserialized = deserialize<Option<string>> serialized
        match deserialized with
        | Some input -> Assert.AreEqual("value", input)
        | None -> Assert.Fail("Should not happen")

    [<Fact>]
    let ``Option<int> conversion works``() =
        let opt = Some 5
        let serialized = serialize opt
        let deserialized = deserialize<Option<int>> serialized
        match deserialized with
        | Some input -> Assert.AreEqual(5, input)
        | None -> Assert.Fail("Should not happen")

    [<Fact>]
    let ``Option<int> deserialization from raw json works``() =
        // what Fable outputs
        match deserialize<Option<int>> "5" with
        | Some input -> Assert.AreEqual(5, input)
        | None -> Assert.Fail("Should not happen")

        match deserialize<Option<int>> "null" with
        | Some _ -> Assert.Fail("Should not happed")
        | None -> Assert.Pass()

    [<Fact>]
    let ``Nested options conversion works``() =
        let nested = Some(Some (Some 5))
        let serialized = serialize nested
        Assert.AreEqual("5", serialized)
        let deserialized = deserialize<Option<Option<Option<int>>>> serialized
        match deserialized with
        | Some (Some (Some n)) -> Assert.AreEqual(5, n)
        | _ -> Assert.Fail("Should not happed")

    [<Fact>]
    let ``Record conversion works``() =
        let input : Record = { Prop1 = "value"; Prop2 = 5; Prop3 = None }
        let deserialized = deserialize<Record> (serialize input)
        Assert.AreEqual("value", deserialized.Prop1)
        Assert.AreEqual(5, deserialized.Prop2)
        match deserialized.Prop3 with
        | None -> Assert.Pass()
        | _ -> Assert.Fail("Should not happed")


    [<Fact>]
    let ``Record deserialization from raw json works``() =
        // let input : Record = { Prop1 = "value"; Prop2 = 5; Prop3 = None }
        // Fable serializes above record to:
        // "{\"Prop1\":\"value\",\"Prop2\":5,\"Prop3\":null}"
        let serialized = "{\"Prop1\":\"value\",\"Prop2\":5,\"Prop3\":null}"
        let deserialized = deserialize<Record> serialized
        Assert.AreEqual("value", deserialized.Prop1)
        Assert.AreEqual(5, deserialized.Prop2)
        match deserialized.Prop3 with
        | None -> Assert.Pass()
        | _ -> Assert.Fail("Should not happed")

    [<Fact>]
    let ``Generic union types conversion works``() =
        let input = Just "value"
        let serialized = serialize input
        let deserialized = deserialize<Maybe<string>> serialized
        match deserialized with
        | Just x -> Assert.AreEqual("value", x)
        | Nothing -> Assert.Fail("Should not happed")

    [<Fact>]
    let ``Generic union types deserialization from raw json works``() =
        // toJson (Just 5) = "{\"Just\":5}"
        // toJson Nothing = "\"Nothing\""
        // above is Fable output
        match deserialize<Maybe<int>> "{\"Just\":5}" with
        | Just n -> Assert.AreEqual(5, n)
        | Nothing -> Assert.Fail("Should not happed")

        match deserialize<Maybe<int>> "\"Nothing\"" with
        | Just _ -> Assert.Fail("Should not happed")
        | Nothing -> Assert.Pass()

        // Serialized "Nothing" is generic
        match deserialize<Maybe<string>> "\"Nothing\"" with
        | Just _ -> Assert.Fail("Should not happed")
        | Nothing -> Assert.Pass()

    [<Fact>]
    let ``Deserialization with provided type at runtime works``() =
        let inputType = typeof<Option<int>>
        let json = "5"
        let parameterTypes = [| typeof<string>; typeof<System.Type> ; typeof<JsonConverter array> |]
        let deserialize = typeof<JsonConvert>.GetMethod("DeserializeObject", parameterTypes)
        Assert.IsNotNull(deserialize)

        let result = deserialize.Invoke(null, [| json; inputType; [| converter |] |])
        match result with
        | :? Option<int> as opt ->
              match opt with
              | Some n -> Assert.AreEqual(5, n)
              | None -> Assert.Fail("Should not happen")
        | _ -> Assert.Fail("Should not happen")
