module Fabel.Util.Json

open FSharp.Reflection
open Newtonsoft.Json

type OptionConverter() =
    inherit JsonConverter()
    override x.CanConvert t = t.Name = "FSharpOption`1"
    override x.ReadJson(reader, t, v, serializer) =
        failwith "Not implemented"
    override x.WriteJson(writer, v, serializer) =
        match FSharpValue.GetUnionFields (v, v.GetType()) with
        | uci, [|v|] when uci.Name = "Some" ->
            serializer.Serialize(writer, v) 
        | _ -> writer.WriteNull()
        
type ErasedUnionConverter() =
    inherit JsonConverter()
    override x.CanConvert t =
        FSharpType.IsUnion t &&
            t.GetCustomAttributes true
            |> Seq.exists (fun a -> (a.GetType ()).Name = "EraseAttribute")
    override x.ReadJson(reader, t, v, serializer) =
        failwith "Not implemented"
    override x.WriteJson(writer, v, serializer) =
        match FSharpValue.GetUnionFields (v, v.GetType()) with
        | _, [|v|] -> serializer.Serialize(writer, v) 
        | _ -> writer.WriteNull()        

type NumberConverter() =
    inherit JsonConverter()
    override x.CanConvert t = t.FullName = "System.Double"
    override x.ReadJson(reader, t, v, serializer) =
        failwith "Not implemented"
    override x.WriteJson(writer, v, serializer) =
        let f = unbox<float> v
        writer.WriteValue(if f % 1. = 0. then box(int f) else box f)
        
let converters: JsonConverter[] = [|
    OptionConverter ()
    ErasedUnionConverter ()
    NumberConverter ()
|]
