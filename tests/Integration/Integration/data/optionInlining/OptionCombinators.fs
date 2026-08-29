module OptionCombinators

// Regression fixture: a lambda passed to these Option/ValueOption functions must beta-reduce
// away, not allocate a closure. See `Options` in Fable.Transforms/Replacements.Util.fs.

let iter (opt: int option) (log: int -> unit) =
    opt |> Option.iter (fun x -> log x)

let map (opt: int option) =
    opt |> Option.map (fun x -> x + 1)

let map2 (opt1: int option) (opt2: int option) =
    (opt1, opt2) ||> Option.map2 (fun a b -> a + b)

let map3 (opt1: int option) (opt2: int option) (opt3: int option) =
    (opt1, opt2, opt3) |||> Option.map3 (fun a b c -> a + b + c)

let bind (opt: int option) =
    opt |> Option.bind (fun x -> Some(x + 1))

let filter (opt: int option) =
    opt |> Option.filter (fun x -> x > 0)

let defaultWith (opt: int option) =
    opt |> Option.defaultWith (fun () -> 1)

let orElseWith (opt: int option) =
    opt |> Option.orElseWith (fun () -> Some 1)

let valueOptionIter (opt: int voption) (log: int -> unit) =
    opt |> ValueOption.iter (fun x -> log x)
