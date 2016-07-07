namespace Fable

module Json =
    open FSharp.Reflection
    open Newtonsoft.Json
    
    let isErasedUnion (t: System.Type) =
        t.Name = "FSharpOption`1" ||
        FSharpType.IsUnion t &&
            t.GetCustomAttributes true
            |> Seq.exists (fun a -> (a.GetType ()).Name = "EraseAttribute")
            
    let getErasedUnionValue (v: obj) =
        match FSharpValue.GetUnionFields (v, v.GetType()) with
        | _, [|v|] -> Some v
        | _ -> None
            
    type ErasedUnionConverter() =
        inherit JsonConverter()
        override x.CanConvert t = isErasedUnion t
        override x.ReadJson(reader, t, v, serializer) =
            failwith "Not implemented"
        override x.WriteJson(writer, v, serializer) =
            match getErasedUnionValue v with
            | Some v -> serializer.Serialize(writer, v) 
            | None -> writer.WriteNull()

    type LocationEraser() =
        inherit JsonConverter()
        override x.CanConvert t = typeof<AST.Babel.Node>.IsAssignableFrom(t)
        override x.ReadJson(reader, t, v, serializer) =
            failwith "Not implemented"
        override x.WriteJson(writer, v, serializer) =
            writer.WriteStartObject()
            v.GetType().GetProperties()
            |> Seq.filter (fun p -> p.Name <> "loc")
            |> Seq.iter (fun p ->
                writer.WritePropertyName(p.Name)
                serializer.Serialize(writer, p.GetValue(v)))
            writer.WriteEndObject()

module Inject =
    open Fable.AST

    let createDeclaration (injection: IInjection) =
        match injection.Body with
        | Choice1Of3 str ->
            Fable.Apply(Fable.Emit str |> Fable.Value, [],
                        Fable.ApplyMeth, Fable.UnknownType, None)
        | Choice2Of3 e -> e
        | Choice3Of3 _ -> failwith "Quotations are not yet supported in injections"
        |> fun body ->
            Fable.Member(Fable.Getter (injection.Name, false),
                SourceLocation.Empty, [], body)
            |> Fable.MemberDeclaration
