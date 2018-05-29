namespace Fable.CLI

module Literals =

  let [<Literal>] VERSION = "2.0.0-alpha-005"
  let [<Literal>] DEFAULT_PORT = 61225
  let [<Literal>] FORCE = "force:"

/// These values must be only set by the Main method
[<RequireQualifiedAccess>]
module GlobalParams =
  open System.IO
  open System.Reflection

  type private TypeInThisAssembly = class end

  let mutable Verbose = false

  let mutable FableCorePath =
    let execDir =
      typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location
      |> Path.GetDirectoryName
    Path.Combine(execDir, "..", "..", "fable-core")

[<RequireQualifiedAccess>]
module Log =
  open System

  let logVerbose(msg: Lazy<string>) =
    if GlobalParams.Verbose then
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

[<RequireQualifiedAccess>]
module Process =
    open System.Diagnostics

    type Options(?envVars, ?redirectOutput) =
        member val EnvVars = defaultArg envVars Map.empty<string,string>
        member val RedirectOuput = defaultArg redirectOutput false

    let isWindows =
        #if NETFX
        true
        #else
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform
            (System.Runtime.InteropServices.OSPlatform.Windows)
        #endif

    let start workingDir fileName args (opts: Options) =
        let fileName, args =
            if isWindows
            then "cmd", ("/C \"" + fileName + "\" " + args)
            else fileName, args
        printfn "CWD: %s" workingDir
        printfn "%s %s" fileName args
        let p = new Process()
        p.StartInfo.FileName <- fileName
        p.StartInfo.Arguments <- args
        p.StartInfo.WorkingDirectory <- workingDir
        p.StartInfo.RedirectStandardOutput <- opts.RedirectOuput
        #if NETFX
        p.StartInfo.UseShellExecute <- false
        #endif
        opts.EnvVars |> Map.iter (fun k v ->
            p.StartInfo.Environment.[k] <- v)
        p.Start() |> ignore
        p

    let run workingDir fileName args =
        let p =
            Options()
            |> start workingDir fileName args
        p.WaitForExit()
        match p.ExitCode with
        | 0 -> ()
        | c -> failwithf "Process %s %s finished with code %i" fileName args c

    let tryRunAndGetOutput workingDir fileName args =
        try
            let p =
                Options(redirectOutput=true)
                |> start workingDir fileName args
            let output = p.StandardOutput.ReadToEnd()
            // printfn "%s" output
            p.WaitForExit()
            output
        with ex ->
            "ERROR: " + ex.Message
