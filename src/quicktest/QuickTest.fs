module QuickTest

open FSharp.Core

let ``test Seq.collect works with Options`` () =
    let xss = [[Some 1; Some 2]; [None; Some 3]]
    Seq.collect id xss
    |> Seq.sumBy (function
        | Some n -> n
        | None -> 0
    )
    |> ignore

