namespace Fable.JsonConverter.Tests

open Xunit
open Newtonsoft.Json
open System

type Record = {
    Prop1 : string
    Prop2 : int
    Prop3 : int option
    Prop4 : int64
}

type Maybe<'t> =
    | Just of 't
    | Nothing


module JsonConverterTests =

    let converter = new Fable.JsonConverter()
    let deserialize<'a> (json : string) = JsonConvert.DeserializeObject(json, typeof<'a>, converter) :?> 'a
    let serialize (value: 'a) = JsonConvert.SerializeObject(value, converter)

    [<Fact>]
    let ``DateTime conversion works``() =
        let date = new DateTime(2017, 03, 23, 18, 30, 0)
        let serialized = serialize date
        let deserialized = deserialize<DateTime> serialized
        Assert.Equal(30, deserialized.Minute)
        Assert.Equal(18, deserialized.Hour)
        Assert.Equal(23, deserialized.Day)
        Assert.Equal(3, deserialized.Month)
        Assert.Equal(2017, deserialized.Year)


    [<Fact>]
    let ``Option<string> convertion works``() =
        let opt = Some "value"
        let serialized = serialize opt
        let deserialized = deserialize<Option<string>> serialized
        match deserialized with
        | Some input -> Assert.Equal("value", input)
        | None -> Assert.True(false, "Should not happen")

    [<Fact>]
    let ``Option<int> conversion works``() =
        let opt = Some 5
        let serialized = serialize opt
        let deserialized = deserialize<Option<int>> serialized
        match deserialized with
        | Some input -> Assert.Equal(5, input)
        | None -> Assert.True(false, "Should not happen")

    [<Fact>]
    let ``Option<int> deserialization from raw json works``() =
        // what Fable outputs
        match deserialize<Option<int>> "5" with
        | Some input -> Assert.Equal(5, input)
        | None -> Assert.True(false, "Should not happen")

        match deserialize<Option<int>> "null" with
        | Some _ -> Assert.True(false, "Should not happed")
        | None -> ()

    [<Fact>]
    let ``Nested options conversion works``() =
        let nested = Some(Some (Some 5))
        let serialized = serialize nested
        Assert.Equal("5", serialized)
        let deserialized = deserialize<Option<Option<Option<int>>>> serialized
        match deserialized with
        | Some (Some (Some n)) -> Assert.Equal(5, n)
        | _ -> Assert.True(false, "Should not happed")

    [<Fact>]
    let ``Record conversion works``() =
        let input : Record = { Prop1 = "value"; Prop2 = 5; Prop3 = None; Prop4 = 42L }
        let deserialized = deserialize<Record> (serialize input)
        Assert.Equal("value", deserialized.Prop1)
        Assert.Equal(5, deserialized.Prop2)
        match deserialized.Prop3 with
        | None -> ()
        | _ -> Assert.True(false, "Should not happed")


    [<Fact>]
    let ``Record deserialization from raw json works``() =
        // let input : Record = { Prop1 = "value"; Prop2 = 5; Prop3 = None }
        // Fable serializes above record to:
        // "{\"Prop1\":\"value\",\"Prop2\":5,\"Prop3\":null}"
        let serialized = """{ "Prop1": "value","Prop2":5,"Prop3":null,"Prop4":42}"""
        let deserialized = deserialize<Record> serialized
        Assert.Equal("value", deserialized.Prop1)
        Assert.Equal(5, deserialized.Prop2)
        match deserialized.Prop3 with
        | None -> ()
        | _ -> Assert.True(false, "Should not happed")
        Assert.Equal(42L, deserialized.Prop4)

    [<Fact>]
    let ``Generic union types conversion works``() =
        let input = Just "value"
        let serialized = serialize input
        let deserialized = deserialize<Maybe<string>> serialized
        match deserialized with
        | Just x -> Assert.Equal("value", x)
        | Nothing -> Assert.True(false, "Should not happed")

    [<Fact>]
    let ``Generic union types deserialization from raw json works``() =
        // toJson (Just 5) = "{\"Just\":5}"
        // toJson Nothing = "\"Nothing\""
        // above is Fable output
        match deserialize<Maybe<int>> "{\"Just\":5}" with
        | Just n -> Assert.Equal(5, n)
        | Nothing -> Assert.True(false, "Should not happed")

        match deserialize<Maybe<int>> "\"Nothing\"" with
        | Just _ -> Assert.True(false, "Should not happed")
        | Nothing -> ()

        // Serialized "Nothing" is generic
        match deserialize<Maybe<string>> "\"Nothing\"" with
        | Just _ -> Assert.True(false, "Should not happed")
        | Nothing -> ()

    [<Fact>]
    let ``Deserialization with provided type at runtime works``() =
        let inputType = typeof<Option<int>>
        let json = "5"
        let parameterTypes = [| typeof<string>; typeof<System.Type> ; typeof<JsonConverter array> |]
        let deserialize = typeof<JsonConvert>.GetMethod("DeserializeObject", parameterTypes)
        Assert.NotNull(deserialize)

        let result = deserialize.Invoke(null, [| json; inputType; [| converter |] |])
        match result with
        | :? Option<int> as opt ->
              match opt with
              | Some n -> Assert.Equal(5, n)
              | None -> Assert.True(false, "Should not happen")
        | _ -> Assert.True(false, "Should not happen")
