let factorial n =
    let rec loop i acc =
        match i with
        | 0 | 1 -> acc
        | _ -> loop (i-1) (acc * i)
    loop n 1

factorial 10
|> printfn "%i"
