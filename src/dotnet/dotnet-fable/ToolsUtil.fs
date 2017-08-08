namespace Fable.CLI

module Constants =

  let [<Literal>] VERSION = "1.1.20"
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
      Console.WriteLine(msg.Value)

  let logAlways(msg: string) =
    Console.WriteLine(msg)
