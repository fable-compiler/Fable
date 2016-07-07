module Util

let reverse (s: string) =
    s
    |> Seq.rev
    |> Seq.map string
    |> String.concat ""

let greet s =
    printfn "Hello %s!" s

let sum (list: int list) =
    List.fold (fun acc i -> acc + i) 0 list
 