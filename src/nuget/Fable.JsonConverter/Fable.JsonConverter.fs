namespace Fable

open System
open FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Converters

/// Converts F# options, tuples and unions to a format understandable
/// by Fable. Code adapted from Lev Gorodinski's original.
/// See https://goo.gl/F6YiQk
type JsonConverter() =
    inherit Newtonsoft.Json.JsonConverter()
    
    let advance(reader: JsonReader) =
        reader.Read() |> ignore
    
    let readElements(reader: JsonReader, itemTypes: Type[], serializer: JsonSerializer) =           
        let rec read index acc =
            match reader.TokenType with
            | JsonToken.EndArray -> acc
            | _ ->
                let value = serializer.Deserialize(reader, itemTypes.[index])
                advance reader
                read (index + 1) (acc @ [value])
        advance reader
        read 0 List.empty
    override x.CanConvert(t) =
        t.Name = "FSharpOption`1"
        || t.FullName.StartsWith("System.Tuple")
        || (FSharpType.IsUnion t && t.Name <> "FSharpList`1")

    override x.WriteJson(writer, value, serializer) =
        if value = null
        then serializer.Serialize(writer, value)
        else
            match value.GetType() with
            | t when t.Name = "FSharpOption`1" ->
                let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
                serializer.Serialize(writer, fields.[0])
            | t when FSharpType.IsTuple t ->
                let values = FSharpValue.GetTupleFields(value)
                serializer.Serialize(writer, values)
            | t -> // Unions
                let uci, fields = FSharpValue.GetUnionFields(value, t)
                if fields.Length = 0
                then serializer.Serialize(writer, uci.Name)
                else
                    writer.WriteStartObject()
                    writer.WritePropertyName(uci.Name)
                    if fields.Length = 1
                    then serializer.Serialize(writer, fields.[0])
                    else serializer.Serialize(writer, fields)
                    writer.WriteEndObject()
    override x.ReadJson(reader, t, existingValue, serializer) =
        match t with
        | t when t.Name = "FSharpOption`1" ->
            let innerType = t.GetGenericArguments().[0]
            let innerType = 
                if innerType.IsValueType
                then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
                else innerType        
            let value = serializer.Deserialize(reader, innerType)
            let cases = FSharpType.GetUnionCases(t)
            if value = null
            then FSharpValue.MakeUnion(cases.[0], [||])
            else FSharpValue.MakeUnion(cases.[1], [|value|])
        | t when FSharpType.IsTuple t ->
            match reader.TokenType with
            | JsonToken.StartArray ->
                let values = readElements(reader, FSharpType.GetTupleElements(t), serializer)
                FSharpValue.MakeTuple(values |> List.toArray, t)
            | _ -> failwith "invalid token"
        | t -> // Unions
            let getUci name =
                FSharpType.GetUnionCases(t)
                |> Array.find (fun uci -> uci.Name = name)
            match reader.TokenType with
            | JsonToken.String ->
                let name = serializer.Deserialize(reader, typeof<string>) |> unbox<string>
                FSharpValue.MakeUnion(getUci name, [||])
            | JsonToken.StartObject ->
                advance reader
                let name = reader.Value |> unbox<string>
                let uci = getUci name
                advance reader
                let itemTypes = uci.GetFields() |> Array.map (fun pi -> pi.PropertyType)
                if itemTypes.Length > 1
                then
                    let values = readElements(reader, itemTypes, serializer)
                    advance reader
                    FSharpValue.MakeUnion(uci, List.toArray values)
                else
                    let value = serializer.Deserialize(reader, itemTypes.[0])
                    advance reader
                    FSharpValue.MakeUnion(uci, [|value|])
            | _ -> failwith "invalid token"
