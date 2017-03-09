module Fable.Client.Webpack.Server

open System
open System.IO
open System.Text
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks

// From http://www.fssnip.net/hx/title/AsyncAwaitTask-with-timeouts
type Microsoft.FSharp.Control.Async with
    static member AwaitTask (t : Task<'T>, timeout : int) =
        async {
            if timeout >= 0
            then
                use cts = new CancellationTokenSource()
                let timer = Task.Delay (timeout, cts.Token)
                let! completed = Async.AwaitTask <| Task.WhenAny(t, timer)
                if completed = (t :> Task) then
                    cts.Cancel ()
                    let! result = Async.AwaitTask t
                    return Some result
                else return None
            else
                let! result = Async.AwaitTask t
                return Some result
        }

let rec private loop timeout (server: TcpListener) (buffer: byte[]) (onMessage: (string*(string->unit)->unit)) = async {
    try
        // printfn "Waiting for connection..."
        let! client = Async.AwaitTask(server.AcceptTcpClientAsync(), timeout)
        match client with
        | Some client ->
            let stream = client.GetStream()
            let i = stream.Read(buffer, 0, buffer.Length)
            let data = Encoding.UTF8.GetString(buffer, 0, i)
            onMessage(data, fun (reply: string) ->
                let msg = Encoding.UTF8.GetBytes(reply)
                stream.Write(msg, 0, msg.Length)
                client.Dispose())
            return! loop timeout server buffer onMessage
        | None ->
            printfn "Timeout (%ims) reached. Closing server..." timeout
            return ()
    with ex ->
        printfn "TCP ERROR: %s" ex.Message
        return ()
}
let start port timeout onMessage =
    let cts = new CancellationTokenSource()
    let buffer = Array.zeroCreate<byte> 8192
    let server = new TcpListener(IPAddress.Parse("127.0.0.1"), port)
    server.Start()
    printfn "Fable server started on port %i" port
    loop timeout server buffer onMessage
