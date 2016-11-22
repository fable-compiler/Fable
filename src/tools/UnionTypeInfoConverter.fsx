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

let settings =
    JsonSerializerSettings(
        Converters = [|UnionTypeInfoConverter()|],
        TypeNameHandling = TypeNameHandling.All)

type U = A of int | B of string * float
JsonConvert.SerializeObject(A 4, settings)
JsonConvert.SerializeObject(B("hi",5.6), settings)