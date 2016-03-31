let test() =
    let ar = [|5uy|]
    ar.[0] <- ar.[0] + 255uy
    if ar.[0] <> 4uy then
        failwith "Clamped array"

test()