module Library

type Counter(n: int) =
    static let zero = 0
    new () = Counter(zero)
    member _.N = n
