module Fable.Cli.Server

open System
open System.Net
open System.Net.Sockets

type MessageHandler(client: IDisposable, stream: System.IO.Stream, msg: string) =
    interface IMessageHandler with
        member __.Message = msg
        member __.Respond(writeTo) =
            do
                use writer = new System.IO.StreamWriter(stream)
                writeTo writer
            client.Dispose()

let rec private loop (server: TcpListener) (buffer: byte[]) (onMessage: MessageHandler->unit) = async {
    let! client = Async.AwaitTask(server.AcceptTcpClientAsync())
    let stream = client.GetStream()
    let i = stream.Read(buffer, 0, buffer.Length)
    let data = System.Text.Encoding.UTF8.GetString(buffer, 0, i)
    if data = Literals.EXIT then
        Log.always("Closing Fable daemon...")
        client.Dispose()
        return ()
    else
        MessageHandler(client, stream, data) |> onMessage
        return! loop server buffer onMessage
}

let start port (onMessage: IMessageHandler->unit) =
    let buffer = Array.zeroCreate<byte> 8192
    let server = TcpListener(IPAddress.Parse("127.0.0.1"), port)
    // This is needed to prevent errors in Unix when Fable server is restarted quickly
    // See https://github.com/fable-compiler/Fable/issues/809#issuecomment-294073328
    server.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true)
    server.Start()
    Log.always(sprintf "Fable (%s) daemon started on port %i" Literals.VERSION port)
    loop server buffer onMessage

let stop port =
    use client = new TcpClient()
    client.Connect(IPAddress.Parse("127.0.0.1"), port)
    let data = System.Text.Encoding.UTF8.GetBytes(Literals.EXIT)
    use stream = client.GetStream()
    stream.Write(data, 0, data.Length)

