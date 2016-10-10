namespace Fable

open System
open FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Converters

/// Converts F# options and tuples to a format understandable
/// by Fable. Code adapted from Lev Gorodinski's original.
/// See https://goo.gl/F6YiQk
type JsonConverter() =
    inherit Newtonsoft.Json.JsonConverter()
    
    override x.CanConvert(t) =
        t.Name = "FSharpOption`1"
        || t.FullName.StartsWith("System.Tuple")

    override x.WriteJson(writer, value, serializer) =
        if value = null
        then serializer.Serialize(writer, value)
        elif value.GetType().Name = "FSharpOption`1"
        then
            let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
            serializer.Serialize(writer, fields.[0])
        else // Tuple
            let values = FSharpValue.GetTupleFields(value)
            serializer.Serialize(writer, values)
    override x.ReadJson(reader, t, existingValue, serializer) =
        if t.Name = "FSharpOption`1"
        then
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
        else
            let advance = reader.Read >> ignore
            let deserialize t = serializer.Deserialize(reader, t)
            let itemTypes = FSharpType.GetTupleElements(t)

            let readElements() =
                let rec read index acc =
                    match reader.TokenType with
                    | JsonToken.EndArray -> acc
                    | _ ->
                        let value = deserialize(itemTypes.[index])
                        advance()
                        read (index + 1) (acc @ [value])
                advance()
                read 0 List.empty

            match reader.TokenType with
            | JsonToken.StartArray ->
                let values = readElements()
                FSharpValue.MakeTuple(values |> List.toArray, t)
            | _ -> failwith "invalid token"
