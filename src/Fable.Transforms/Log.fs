[<RequireQualifiedAccess>]
module Log

open System
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions

let mutable logger: ILogger = NullLogger.Instance
let setLogger newLogger = logger <- newLogger
let newLine = Environment.NewLine
let mutable private verbosity = Fable.Verbosity.Normal
let isVerbose () = verbosity = Fable.Verbosity.Verbose
let always (msg: string) = logger.LogInformation msg

let verbose (msg: Lazy<string>) =
    if isVerbose () then
        always msg.Value

let warning (msg: string) = logger.LogWarning msg

let error (msg: string) = logger.LogError msg

let mutable private femtoMsgShown = false

let showFemtoMsg (show: unit -> bool) : unit =
    if not femtoMsgShown && verbosity <> Fable.Verbosity.Silent then
        if show () then
            femtoMsgShown <- true

            "Some Nuget packages contain information about NPM dependencies that can be managed by Femto: https://github.com/Zaid-Ajaj/Femto"
            |> logger.LogInformation
