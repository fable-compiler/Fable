open System
open System.Text
open System.Net
open System.Net.Sockets

let init port =
    let localAddr = IPAddress.Parse("127.0.0.1")
    let server = new TcpListener(localAddr, port)
    server.Start()
    let bytes = Array.zeroCreate<byte> 1024

    async {
        // try
            while true do
                Console.WriteLine("Waiting for a connection on port {0}...", port)

                use! client = server.AcceptTcpClientAsync() |> Async.AwaitTask
                Console.WriteLine("Connected!")
                let mutable data = ""

                let stream = client.GetStream()
                let i = stream.Read(bytes, 0, bytes.Length)
                let data = Encoding.UTF8.GetString(bytes, 0, i)

                Console.WriteLine("Received: {0}", data)
                let msg = Encoding.UTF8.GetBytes(data.ToUpper())
                stream.Write(msg, 0, msg.Length)

                if data = "Finito" then server.Stop()
        // with
        // | ex -> printfn "%s" ex.Message
    } |> Async.Start

    Console.WriteLine("\nHit enter to continue...")
    Console.Read() |> ignore
    server.Stop()

init 1300
