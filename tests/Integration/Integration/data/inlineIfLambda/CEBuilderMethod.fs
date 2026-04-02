module CEBuilderMethod

type TwiceBuilder() =
    member inline _.Return(x) = x
    member inline _.Delay([<InlineIfLambda>] f: unit -> 'T) =
        let _ = f ()   // first call: side-effect runs here
        f ()           // second call: result comes from here
    member inline _.Run(x) = x

let twice = TwiceBuilder()

let mutable callCount = 0
let result =
    twice {
        callCount <- callCount + 1
        return callCount
    }
