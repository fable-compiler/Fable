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
