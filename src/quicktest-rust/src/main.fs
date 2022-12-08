open System

let user = "World"

[<EntryPoint>]
let main argv =
    Console.WriteLine("Hello {0}!", user)
    0