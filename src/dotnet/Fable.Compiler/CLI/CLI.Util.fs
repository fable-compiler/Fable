namespace Fable.CLI

module Literals =

  let [<Literal>] VERSION = "2.0.0-alpha-023"
  let [<Literal>] DEFAULT_PORT = 61225
  let [<Literal>] FORCE = "force:"

open System.IO
open System.Reflection

type private TypeInThisAssembly = class end

[<RequireQualifiedAccess>]
type GlobalParams private (verbose, fableCorePath, workingDir, replaceFile) =
    static let mutable singleton: GlobalParams option = None
    let mutable _verbose = verbose
    let mutable _fableCorePath = fableCorePath
    let mutable _workingDir = workingDir
    let mutable _replaceFile = replaceFile

    static member Singleton =
        match singleton with
        | Some x -> x
        | None ->
            let workingDir = Directory.GetCurrentDirectory()
            let fableCorePath =
                let execDir =
                  typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location
                  |> Path.GetDirectoryName
                Path.Combine(execDir, "..", "..", "fable-core")
            let p = GlobalParams(false, fableCorePath, workingDir, None)
            singleton <- Some p
            p

    member __.Verbose: bool = _verbose
    member __.FableCorePath: string = _fableCorePath
    member __.WorkingDir: string = _workingDir
    member __.ReplaceFile: string option = _replaceFile

    member __.SetValues(?verbose, ?fableCorePath, ?workingDir, ?replaceFile) =
        _verbose        <- defaultArg verbose _verbose
        _fableCorePath  <- defaultArg fableCorePath _fableCorePath
        _workingDir     <- defaultArg workingDir _workingDir
        _replaceFile    <- replaceFile |> Option.orElse _replaceFile

[<RequireQualifiedAccess>]
module Log =
  open System

  let logVerbose(msg: Lazy<string>) =
    if GlobalParams.Singleton.Verbose then
      try // Some long verbose message may conflict with other processes
        Console.WriteLine(msg.Value)
      with _ -> ()

  let logAlways(msg: string) =
    Console.WriteLine(msg)

module Json =
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
