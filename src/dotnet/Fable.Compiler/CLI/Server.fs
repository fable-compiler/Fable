module Fable.CLI.Server

open System.Net
open System.Net.Sockets

let [<Literal>] SIGTERM = "[SIGTERM]"

type MessageHandler(client: TcpClient, stream: System.IO.Stream, msg: string) =
    member __.Message = msg
    member __.ResponseStream = new System.IO.StreamWriter(stream)
    interface System.IDisposable with
        member __.Dispose() =
            client.Dispose()

let rec private loop (server: TcpListener) (buffer: byte[]) (onMessage: MessageHandler->unit) = async {
    // printfn "Waiting for connection..."
    let! client = Async.AwaitTask(server.AcceptTcpClientAsync())
    let stream = client.GetStream()
    let i = stream.Read(buffer, 0, buffer.Length)
    let data = System.Text.Encoding.UTF8.GetString(buffer, 0, i)
    if data = SIGTERM then
        Log.logAlways("Closing Fable daemon...")
        return ()
    else
        new MessageHandler(client, stream, data) |> onMessage
        return! loop server buffer onMessage
}

let start port onMessage =
    let buffer = Array.zeroCreate<byte> 8192
    let server = TcpListener(IPAddress.Parse("127.0.0.1"), port)
    // This is needed to prevent errors in Unix when Fable server is restarted quickly
    // See https://github.com/fable-compiler/Fable/issues/809#issuecomment-294073328
    server.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true)
    server.Start()
    Log.logAlways(sprintf "Fable (%s) daemon started on port %i" Literals.VERSION port)
    loop server buffer onMessage

let stop port = async {
    use client = new TcpClient()
    do! client.ConnectAsync(IPAddress.Parse("127.0.0.1"), port) |> Async.AwaitTask
    let data = System.Text.Encoding.UTF8.GetBytes(SIGTERM)
    use stream = client.GetStream()
    stream.Write(data, 0, data.Length)
}
