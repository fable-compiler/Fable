module Fable.Client.Webpack.Server

open System
open System.Text
open System.Net
open System.Net.Sockets
open System.Threading

let start port onMessage =
    let cts = new CancellationTokenSource()
    let server = new TcpListener(IPAddress.Parse("127.0.0.1"), port)
    let bytes = Array.zeroCreate<byte> 8192
    server.Start()

    Async.Start(async {
        while true do
            try
                use! client = server.AcceptTcpClientAsync() |> Async.AwaitTask
                let stream = client.GetStream()
                let i = stream.Read(bytes, 0, bytes.Length)
                let data = Encoding.UTF8.GetString(bytes, 0, i)
                onMessage data
            with
            // Recover from failure
            | ex -> printfn "[TCP] %s" ex.Message
    }, cts.Token)

    { new IDisposable with
        member __.Dispose() =
            cts.Cancel()
            server.Stop() }
