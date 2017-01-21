namespace Fable

#if DOTNETCORE && !FABLE_COMPILER
[<AutoOpen>]
module ReflectionAdapters =
    open System.Reflection

    type System.Type with
        member this.GetCustomAttributes(inherits : bool) : obj[] =
            downcast box(CustomAttributeExtensions.GetCustomAttributes(this.GetTypeInfo(), inherits) |> Seq.toArray)
#endif

[<AutoOpen>]
module Extensions =
    type System.Collections.Generic.Dictionary<'TKey,'TValue> with
        member dic.GetOrAdd(key, addFn) =
            match dic.TryGetValue(key) with
            | true, v -> v
            | false, _ ->
                let v = addFn()
                dic.Add(key, v)
                v  
        member dic.AddOrUpdate(key, addFn, updateFn) =
            let v =
                match dic.TryGetValue(key) with
                | true, v ->
                    dic.Remove(key) |> ignore
                    updateFn key v
                | false, _ -> addFn key
            dic.Add(key, v)
            v

module Map = 

    let findOrRun<'T> (f: unit->'T) (k: string) (m: Map<string, obj>) =
        match Map.tryFind k m with
        | Some x -> downcast x
        | _ -> f()

    // let findOrNew<'T when 'T : (new : unit->'T)> (k: string) (m: Map<string, obj>) =
    //     findOrRun (fun () -> new 'T()) k m

module Option = 
    let toBool (f: 'T->bool) (opt: 'T option) =
        match opt with Some x when f x -> true | _ -> false 

#if !FABLE_COMPILER
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
        override x.CanConvert t =
            typeCache.GetOrAdd(t, isErasedUnion)
        override x.ReadJson(reader, t, v, serializer) =
            failwith "Not implemented"
        override x.WriteJson(writer, v, serializer) =
            match getErasedUnionValue v with
            | Some v -> serializer.Serialize(writer, v) 
            | None -> writer.WriteNull()

    type LocationEraser() =
        inherit JsonConverter()
        let typeCache = ConcurrentDictionary<Type,bool>()
        override x.CanConvert t =
            typeCache.GetOrAdd(t, typeof<AST.Babel.Node>.IsAssignableFrom)
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
#endif //!FABLE_COMPILER

module Plugins =
    let tryPlugin<'T,'V when 'T:>IPlugin> r (f: 'T->'V option) =
        Seq.tryPick (fun (path: string, plugin: 'T) ->
            try f plugin
            with
            | :? AST.FableError as err when err.Range.IsNone -> AST.FableError(err.Message, ?range=r) |> raise
            | :? AST.FableError as err -> raise err
            | ex when Option.isSome r -> System.Exception(sprintf "Error in plugin %s: %s %O" path ex.Message r.Value, ex) |> raise
            | ex -> System.Exception(sprintf "Error in plugin %s: %s" path ex.Message, ex) |> raise)
