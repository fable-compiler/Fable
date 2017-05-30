[<RequireQualifiedAccess>]
module Fable.Tools.Log

type private LogActions =
    | SetVerbose
    | SetSilent
    | LogVerbose of string
    | LogAllways of string

let private actor =
    MailboxProcessor.Start(fun box ->
        let mutable verbose = false
        let rec loop () = async {
            let! msg = box.Receive()
            match msg with
            | SetVerbose -> verbose <- true
            | SetSilent -> verbose <- false
            | LogVerbose txt ->
                if verbose then printfn "%s" txt
            | LogAllways txt ->
                printfn "%s" txt
            return! loop()
        }
        loop()
    )

let setVerbose() = actor.Post(SetVerbose)
let setSilent() = actor.Post(SetSilent)
let logVerbose(msg) = actor.Post(LogVerbose msg)
let logAllways(msg) = actor.Post(LogAllways msg)
