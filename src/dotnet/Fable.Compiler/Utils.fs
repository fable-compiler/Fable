namespace Fable

open System.Collections.Generic
open Microsoft.FSharp.Compiler.SourceCodeServices
open Fable.AST

type InlineExpr = IDictionary<FSharpMemberOrFunctionOrValue,int> * FSharpExpr

type ICompilerState =
    abstract ProjectFile: string
    abstract GetRootModule: string -> string
    abstract GetOrAddEntity: string * (unit->Fable.Entity) -> Fable.Entity
    abstract GetOrAddInlineExpr: string * (unit->InlineExpr) -> InlineExpr

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

#if !FABLE_COMPILER
module Reflection =
    open System
    open System.Reflection

    let loadAssembly path =
#if NETFX
        // The assembly is already loaded because it's being referenced
        // by the parsed code, so use `LoadFrom` which takes the copy in memory
        // Unlike `LoadFile`, see: http://stackoverflow.com/a/1477899
        Assembly.LoadFrom(path)
#else
        let globalLoadContext = System.Runtime.Loader.AssemblyLoadContext.Default
        globalLoadContext.LoadFromAssemblyPath(path)
#endif

    /// Prevent ReflectionTypeLoadException
    /// From http://stackoverflow.com/a/7889272
    let getTypes (asm: System.Reflection.Assembly) =
        let mutable types: Option<Type[]> = None
        try
            types <- Some(asm.GetTypes())
        with
        | :? ReflectionTypeLoadException as e -> types <- Some e.Types
        match types with
        | Some types -> types |> Seq.filter ((<>) null)
        | None -> Seq.empty

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
    let tryPlugin<'T,'V when 'T:>IPlugin> (r: SourceLocation option) (f: 'T->'V option) =
        Seq.tryPick (fun (path: string, plugin: 'T) ->
            try f plugin
            with
            | ex when Option.isSome r -> System.Exception(sprintf "Error in plugin %s: %s %O" path ex.Message r.Value, ex) |> raise
            | ex -> System.Exception(sprintf "Error in plugin %s: %s" path ex.Message, ex) |> raise)
