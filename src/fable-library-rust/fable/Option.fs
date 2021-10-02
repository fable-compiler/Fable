module Option

// [<Fable.Core.Import("getValue", "Native.rs|Option")>]
// let getValue<'T> (opt: 'T option): 'T = Fable.Core.Util.nativeOnly

let getValue<'T> (opt: 'T option): 'T =
    match opt with
    | Some x -> x
    | None -> failwith "Option has no value"

let bind<'T, 'U> (binder: 'T -> 'U option) (opt: 'T option): 'U option =
    match opt with
    | Some x -> binder x
    | None -> None

let map<'T, 'U> (mapping: 'T -> 'U) (opt: 'T option): 'U option =
    match opt with
    | Some x -> Some (mapping x)
    | None -> None
