module Fable.Client.Webpack.Server

open System
open System.IO
open System.Text
open System.Net
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks

let rec private loop (server: TcpListener) (buffer: byte[]) (onMessage: (string*(string->unit)->unit)) = async {
    try
        printfn "Waiting for connection..."
        let! client = server.AcceptTcpClientAsync() |> Async.AwaitTask
        let stream = client.GetStream()
        let i = stream.Read(buffer, 0, buffer.Length)
        let data = Encoding.UTF8.GetString(buffer, 0, i)
        onMessage(data, fun (reply: string) ->
            let msg = Encoding.UTF8.GetBytes(reply)
            stream.Write(msg, 0, msg.Length)
            client.Dispose())
        return! loop server buffer onMessage
    with ex ->
        printfn "%s" ex.Message
        return ()
}
let start port onMessage =
    let cts = new CancellationTokenSource()
    let buffer = Array.zeroCreate<byte> 8192
    let server = new TcpListener(IPAddress.Parse("127.0.0.1"), port)
    server.Start()
    printfn "Fable server started on port %i" port
    loop server buffer onMessage
