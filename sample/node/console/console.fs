module console

[<EntryPoint>]
let main argv =
    let res =   
        argv.[0]
        |> Seq.rev
        |> Seq.map string
        |> String.concat ""
    printfn "Hello %s!" res
    0
