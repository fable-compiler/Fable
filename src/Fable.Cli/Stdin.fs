module Fable.Cli.Stdin

open System
open System.IO
open System.Text

type MessageHandler(client: StreamWriter, id: string, msg: string) =
    interface IMessageHandler with
        member __.Message = msg
        member __.Respond(writeTo) =
            lock Log.writerLock (fun () ->
                client.Write("JSON:" + id + ":")
                writeTo client
                client.WriteLine()
                client.Flush()) // Important, don't forget

let readLine(client: BinaryReader): string option =
    let buffer = StringBuilder()
    try
        let mutable endOfLine = false
        while not endOfLine do
            let nextChar = client.ReadChar()
            if nextChar = '\n' then do
                endOfLine <- true
            elif nextChar = '\r' then do
                assert(client.ReadChar() = '\n')
                endOfLine <- true
            else do
                buffer.Append(nextChar) |> ignore
        Some(buffer.ToString())
    with
    | :? EndOfStreamException ->
        if buffer.Length > 0 then
            Some(buffer.ToString())
        else
            None

let tokenize(client: BinaryReader): seq<string> =
    seq {
        let mutable endOfInput = false
        while not endOfInput do
            match readLine(client) with
            | None -> endOfInput <- true
            | Some line -> yield line
    }

let parseMessage (msg: string) =
    let i = msg.IndexOf(':')
    let id = msg.[0..i-1]
    let msg = msg.[i+1..]
    id, msg

let private notExit = function
    | Literals.EXIT -> false
    | _ -> true

let readMessages(receive: BinaryReader) =
    tokenize(receive)
    |> Seq.takeWhile notExit
    |> Seq.map parseMessage

let start (onMessage: IMessageHandler->unit) =
    let read = new BinaryReader(Console.OpenStandardInput())
    let write = new StreamWriter(Console.OpenStandardOutput())
    try
        for (id, msg) in readMessages(read) do
            MessageHandler(write, id, msg) |> onMessage
        0 // return an integer exit code
    with e ->
        Console.Error.WriteLine(e.Message + Console.Error.NewLine + e.StackTrace)
        1
