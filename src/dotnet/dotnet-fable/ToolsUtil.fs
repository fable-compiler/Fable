namespace Fable.CLI

module Constants =

  let [<Literal>] VERSION = "1.3.0-beta-001"
  let [<Literal>] DEFAULT_PORT = 61225

/// These values must be only set by the Main method
[<RequireQualifiedAccess>]
module Flags =
  let mutable logVerbose = false
  let mutable checkCoreVersion = true

[<RequireQualifiedAccess>]
module Log =
  open System

  let logVerbose(msg: Lazy<string>) =
    if Flags.logVerbose then
      try // Some long verbose message may conflict with other processes
        Console.WriteLine(msg.Value)
      with _ -> ()

  let logAlways(msg: string) =
    Console.WriteLine(msg)
