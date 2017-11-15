let rec fib = seq {
  yield! [0; 1]
  yield! fib
         |> Seq.pairwise
         |> Seq.map (fun (prev, next) -> prev + next)
}

fib
|> Seq.take 10
|> List.ofSeq
|> printfn "%A"