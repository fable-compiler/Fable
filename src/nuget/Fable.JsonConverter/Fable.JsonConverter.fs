#if INTERACTIVE
#r "../../../packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
#else
namespace Fable
#endif

#if DOTNETCORE
[<AutoOpen>]
module ReflectionAdapters =
    open System.Reflection

    type System.Type with
        member this.IsValueType = this.GetTypeInfo().IsValueType
        member this.GetGenericArguments() = this.GetTypeInfo().GenericTypeArguments
        member this.GetCustomAttributes(inherits : bool) : obj[] =
            downcast box(CustomAttributeExtensions.GetCustomAttributes(this.GetTypeInfo(), inherits) |> Seq.toArray)
#endif

open System
open FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Converters
open System.Collections.Generic
open System.Collections.Concurrent

type Kind =
    | Other = 0
    | Option = 1
    | Tuple = 2
    | Union = 3
    | PojoDU = 4
    | StringEnum = 5
    | DateTime = 6

/// Converts F# options, tuples and unions to a format understandable
/// by Fable. Code adapted from Lev Gorodinski's original.
/// See https://goo.gl/F6YiQk
type JsonConverter() =
    inherit Newtonsoft.Json.JsonConverter()

    let [<Literal>] PojoDU_TAG = "type"

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

    let getUnionKind (t: Type) =
        t.GetCustomAttributes(false)
        |> Seq.tryPick (fun o ->
            match o.GetType().FullName with
            | "Fable.Core.PojoAttribute" -> Some Kind.PojoDU
            | "Fable.Core.StringEnumAttribute" -> Some Kind.StringEnum
            | _ -> None)
        |> defaultArg <| Kind.Union

    let getUci t name =
        FSharpType.GetUnionCases(t)
        |> Array.find (fun uci -> uci.Name = name)

    let typeCache = ConcurrentDictionary<Type,Kind>()
    override x.CanConvert(t) =
        let kind =
            typeCache.GetOrAdd(t, fun t ->
                if t.FullName = "System.DateTime"
                then Kind.DateTime
                elif t.Name = "FSharpOption`1"
                then Kind.Option
                elif FSharpType.IsTuple t
                then Kind.Tuple
                elif (FSharpType.IsUnion t && t.Name <> "FSharpList`1")
                then getUnionKind t
                else Kind.Other)
        kind <> Kind.Other

    override x.WriteJson(writer, value, serializer) =
        if isNull value
        then serializer.Serialize(writer, value)
        else
            let t = value.GetType()
            match typeCache.TryGetValue(t) with
            | false, _ ->
                serializer.Serialize(writer, value)
            | true, Kind.DateTime ->
                let dt = value :?> DateTime
                // Make sure the DateTime is saved in UTC and ISO format (see #604)
                let universalTime = if dt.Kind = DateTimeKind.Local then dt.ToUniversalTime() else dt
                serializer.Serialize(writer, universalTime.ToString("O"))
            | true, Kind.Option ->
                let _,fields = FSharpValue.GetUnionFields(value, t)
                serializer.Serialize(writer, fields.[0])
            | true, Kind.Tuple ->
                let values = FSharpValue.GetTupleFields(value)
                serializer.Serialize(writer, values)
            | true, Kind.PojoDU ->
                let uci, fields = FSharpValue.GetUnionFields(value, t)
                writer.WriteStartObject()
                writer.WritePropertyName(PojoDU_TAG)
                writer.WriteValue(uci.Name)
                Seq.zip (uci.GetFields()) fields
                |> Seq.iter (fun (fi, v) ->
                    writer.WritePropertyName(fi.Name)
                    serializer.Serialize(writer, v))
                writer.WriteEndObject()
            | true, Kind.StringEnum ->
                let uci, _ = FSharpValue.GetUnionFields(value, t)
                // TODO: Should we cache the case-name pairs somewhere? (see also `ReadJson`)
                match uci.GetCustomAttributes(typeof<CompiledNameAttribute>) with
                | [|:? CompiledNameAttribute as att|] -> att.CompiledName
                | _ -> uci.Name.Substring(0,1).ToLowerInvariant() + uci.Name.Substring(1)
                |> writer.WriteValue
            | true, Kind.Union ->
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
            | true, _ ->
                serializer.Serialize(writer, value)

    override x.ReadJson(reader, t, existingValue, serializer) =
        match typeCache.TryGetValue(t) with
        | false, _ ->
            serializer.Deserialize(reader, t)
        | true, Kind.DateTime ->
            match reader.Value with
            | :? DateTime -> reader.Value // Avoid culture-sensitive string roundtrip for already parsed dates
            | _ ->
                let json = serializer.Deserialize(reader, typeof<string>) :?> string
                upcast DateTime.Parse(json)
        | true, Kind.Option ->
            let innerType = t.GetGenericArguments().[0]
            let innerType =
                if innerType.IsValueType
                then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
                else innerType
            let value = serializer.Deserialize(reader, innerType)
            let cases = FSharpType.GetUnionCases(t)
            if isNull value
            then FSharpValue.MakeUnion(cases.[0], [||])
            else FSharpValue.MakeUnion(cases.[1], [|value|])
        | true, Kind.Tuple ->
            match reader.TokenType with
            | JsonToken.StartArray ->
                let values = readElements(reader, FSharpType.GetTupleElements(t), serializer)
                FSharpValue.MakeTuple(values |> List.toArray, t)
            | _ -> failwith "invalid token"
        | true, Kind.PojoDU ->
            let dic = serializer.Deserialize(reader, typeof<Dictionary<string,obj>>) :?> Dictionary<string,obj>
            let uciName = dic.[PojoDU_TAG] :?> string
            let uci = getUci t uciName
            let fields = uci.GetFields() |> Array.map (fun fi -> Convert.ChangeType(dic.[fi.Name], fi.PropertyType))
            FSharpValue.MakeUnion(uci, fields)
        | true, Kind.StringEnum ->
            let name = serializer.Deserialize(reader, typeof<string>) :?> string
            FSharpType.GetUnionCases(t)
            |> Array.tryFind (fun uci ->
                // TODO: Should we cache the case-name pairs somewhere? (see also `WriteJson`)
                match uci.GetCustomAttributes(typeof<CompiledNameAttribute>) with
                | [|:? CompiledNameAttribute as att|] -> att.CompiledName = name
                | _ ->
                    let name2 = uci.Name.Substring(0,1).ToLowerInvariant() + uci.Name.Substring(1)
                    name = name2)
            |> function
                | Some uci -> FSharpValue.MakeUnion(uci, [||])
                | None -> failwithf "Cannot find case corresponding to '%s' for `StringEnum` type %s"
                                name t.FullName
        | true, Kind.Union ->
            match reader.TokenType with
            | JsonToken.String ->
                let name = serializer.Deserialize(reader, typeof<string>) :?> string
                FSharpValue.MakeUnion(getUci t name, [||])
            | JsonToken.StartObject ->
                advance reader
                let name = reader.Value :?> string
                let uci = getUci t name
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
            | JsonToken.Null -> null // for { "union": null }
            | _ -> failwith "invalid token"
        | true, _ ->
            serializer.Deserialize(reader, t)

#if INTERACTIVE
#r "../../../build/fable-core/Fable.Core.dll"
open Fable.Core

type [<Pojo>] U = MyCase of ja:int * jo:string
type [<StringEnum>] U2 = Foo | [<CompiledName("B-A-R")>] Bar

module Test =
    let test (o: 'T) =
        let json = JsonConvert.SerializeObject(o, JsonConverter())
        printfn "%s" json
        let o2 = JsonConvert.DeserializeObject<'T>(json, JsonConverter())
        printfn "%A" o2

    test <| MyCase(5,"4")
    test Foo
    test Bar
    test <| DateTime(2016, 12, 13, 8, 00, 0)
#endif
