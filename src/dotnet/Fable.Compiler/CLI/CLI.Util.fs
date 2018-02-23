namespace Fable.CLI

module Literals =

  let [<Literal>] VERSION = "1.3.7"
  let [<Literal>] DEFAULT_PORT = 61225

/// These values must be only set by the Main method
[<RequireQualifiedAccess>]
module GlobalParams =
  open System.IO
  open System.Reflection

  type private TypeInThisAssembly = class end

  let mutable logVerbose = false

  let mutable fableCoreDir =
    let execDir =
      typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location
      |> Path.GetDirectoryName
    Path.Combine(execDir, "..", "..", "fable-core")

[<RequireQualifiedAccess>]
module Log =
  open System

  let logVerbose(msg: Lazy<string>) =
    if GlobalParams.logVerbose then
      try // Some long verbose message may conflict with other processes
        Console.WriteLine(msg.Value)
      with _ -> ()

  let logAlways(msg: string) =
    Console.WriteLine(msg)

module Json =
    open System.Reflection
    open FSharp.Reflection
    open Newtonsoft.Json
    open System.Collections.Concurrent
    open System

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
        let typeCache = ConcurrentDictionary<Type,bool>()
        override __.CanConvert t =
            typeCache.GetOrAdd(t, isErasedUnion)
        override __.ReadJson(_reader, _t, _v, _serializer) =
            failwith "Not implemented"
        override __.WriteJson(writer, v, serializer) =
            match getErasedUnionValue v with
            | Some v -> serializer.Serialize(writer, v)
            | None -> writer.WriteNull()

    type LocationEraser() =
        inherit JsonConverter()
        let typeCache = ConcurrentDictionary<Type,bool>()
        override __.CanConvert t =
            typeCache.GetOrAdd(t, fun t -> typeof<Fable.AST.Babel.Node>.GetTypeInfo().IsAssignableFrom(t))
        override __.ReadJson(_reader, _t, _v, _serializer) =
            failwith "Not implemented"
        override __.WriteJson(writer, v, serializer) =
            writer.WriteStartObject()
            v.GetType().GetTypeInfo().GetProperties()
            |> Seq.filter (fun p -> p.Name <> "loc")
            |> Seq.iter (fun p ->
                writer.WritePropertyName(p.Name)
                serializer.Serialize(writer, p.GetValue(v)))
            writer.WriteEndObject()
