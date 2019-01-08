namespace Fable.CLI

module Literals =

  let [<Literal>] VERSION = "2.1.9"
  let [<Literal>] CORE_VERSION = "2.0.3"
  let [<Literal>] DEFAULT_PORT = 61225
  let [<Literal>] FORCE = "force:"
  let [<Literal>] EXIT = "exit"

open System.IO
open System.Reflection

type IMessageHandler =
    abstract Message: string
    abstract Respond: write: (TextWriter->unit) -> unit

type AgentMsg =
    | Received of handler: IMessageHandler
    | Respond of response: obj * handler: IMessageHandler

type private TypeInThisAssembly = class end

[<RequireQualifiedAccess>]
type GlobalParams private (verbose, forcePkgs, fableLibraryPath, workingDir) =
    static let mutable singleton: GlobalParams option = None
    let mutable _verbose = verbose
    let mutable _forcePkgs = forcePkgs
    let mutable _fableLibraryPath = fableLibraryPath
    let mutable _workingDir = workingDir
    let mutable _replaceFiles = []
    let mutable _experimental: Set<string> = Set.empty

    static member Singleton =
        match singleton with
        | Some x -> x
        | None ->
            let workingDir = Directory.GetCurrentDirectory()
            let execDir =
              typeof<TypeInThisAssembly>.GetTypeInfo().Assembly.Location
              |> Path.GetDirectoryName
            let defaultFableLibraryPaths =
                [ "../fable-library"                         // running from npm package
                  "../../fable-library/"                     // running from nuget package
                  "../../../../../../build/fable-library/" ] // running from bin/Release/netcoreapp2.0
                |> List.map (fun x -> Path.GetFullPath(Path.Combine(execDir, x)))
            let fableLibraryPath =
                defaultFableLibraryPaths
                |> List.tryFind Directory.Exists
                |> Option.defaultValue (List.last defaultFableLibraryPaths)
            let p = GlobalParams(false, false, fableLibraryPath, workingDir)
            singleton <- Some p
            p

    member __.Verbose: bool = _verbose
    member __.ForcePkgs: bool = _forcePkgs
    member __.FableLibraryPath: string = _fableLibraryPath
    member __.WorkingDir: string = _workingDir
    member __.ReplaceFiles = _replaceFiles
    member __.Experimental = _experimental

    member __.SetValues(?verbose, ?forcePkgs, ?fableLibraryPath, ?workingDir, ?replaceFiles: string, ?experimental: string) =
        _verbose        <- defaultArg verbose _verbose
        _forcePkgs      <- defaultArg forcePkgs _forcePkgs
        _fableLibraryPath  <- defaultArg fableLibraryPath _fableLibraryPath
        _workingDir     <- defaultArg workingDir _workingDir
        _replaceFiles   <-
            match replaceFiles with
            | None -> _replaceFiles
            | Some replaceFiles ->
                replaceFiles.Split([|","; ";"|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.map (fun pair ->
                    let parts = pair.Split(':')
                    parts.[0].Trim(), parts.[1].Trim())
                |> Array.toList
        _experimental   <-
            match experimental with
            | None -> _experimental
            | Some experimental ->
                experimental.Split([|","; ";"|], System.StringSplitOptions.RemoveEmptyEntries) |> Set

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

    // Adapted from https://github.com/enricosada/dotnet-proj-info/blob/1e6d0521f7f333df7eff3148465f7df6191e0201/src/dotnet-proj/Program.fs#L155
    let runCmd log errorLog workingDir exePath args =
        log (workingDir + "> " + exePath + " " + (args |> String.concat " "))

        let logOut = System.Collections.Concurrent.ConcurrentQueue<string>()
        let logErr = System.Collections.Concurrent.ConcurrentQueue<string>()

        let runProcess (workingDir: string) (exePath: string) (args: string) =
            let psi = System.Diagnostics.ProcessStartInfo()
            psi.FileName <- exePath
            psi.WorkingDirectory <- workingDir
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true
            psi.Arguments <- args
            psi.CreateNoWindow <- true
            psi.UseShellExecute <- false

            use p = new System.Diagnostics.Process()
            p.StartInfo <- psi

            p.OutputDataReceived.Add(fun ea -> logOut.Enqueue (ea.Data))
            p.ErrorDataReceived.Add(fun ea -> logErr.Enqueue (ea.Data))

            try
                p.Start() |> ignore
                p.BeginOutputReadLine()
                p.BeginErrorReadLine()
                p.WaitForExit()
                p.ExitCode
            with ex ->
                errorLog ("Cannot run: " + ex.Message)
                -1

        let exitCode =
            String.concat " " args
            |> runProcess workingDir exePath

        for x in logOut.ToArray() do log x
        for x in logErr.ToArray() do errorLog x

        exitCode

[<RequireQualifiedAccess>]
module Async =
    let fold f (state: 'State) (xs: 'T seq) = async {
        let mutable state = state
        for x in xs do
            let! result = f state x
            state <- result
        return state
    }

    let map f x = async {
        let! x = x
        return f x
    }

    let tryPick (f: 'T->Async<'Result option>) xs: Async<'Result option> = async {
        let mutable result: 'Result option = None
        for x in xs do
            match result with
            | Some _ -> ()
            | None ->
                let! r = f x
                result <- r
        return result
    }

    let orElse (f: unit->Async<'T>) (x: Async<'T option>): Async<'T> = async {
        let! x = x
        match x with
        | Some x -> return x
        | None -> return! f ()
    }
