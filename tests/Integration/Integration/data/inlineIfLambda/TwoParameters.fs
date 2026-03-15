module TwoParameters

let inline applyPairTwice ([<InlineIfLambda>] f: int -> int -> int) a b =
    let first = f a b
    f first b

let result =
    applyPairTwice
        (fun a b ->
            printfn "Adding %d and %d" a b
            a + b
        )
        10
        32

printfn "result: %d" result
