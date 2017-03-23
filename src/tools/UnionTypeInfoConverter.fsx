open FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Linq

/// This is intended to be used with Newtonsoft.Json and Fable `ofJsonWithTypeInfo`.
/// If you want to use `ofJson` instead, use the `Fable.JsonConverter` (available in Nuget).
/// More info in http://fable.io/blog/Introducing-0-7.html#JSON-Serialization
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

let settings =
    JsonSerializerSettings(
        Converters = [|UnionTypeInfoConverter()|],
        TypeNameHandling = TypeNameHandling.All)

type U = A of int | B of string * float
JsonConvert.SerializeObject(A 4, settings)
JsonConvert.SerializeObject(B("hi",5.6), settings)