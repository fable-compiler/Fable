module OptionCheckNulls

open System.Runtime.CompilerServices

[<AllowNullLiteral>]
type Anl(x: int) =
    member _.X = x

[<MethodImpl(MethodImplOptions.NoInlining)>]
let makeSome (x: 'T) : 'T option = Some x

let anlValue (opt: Anl option) = opt.Value

let anlMatch (opt: Anl option) =
    match opt with
    | Some v -> isNull v
    | None -> false

let anlMadeValue () = (makeSome (null: Anl)).Value

let stringValue (opt: string option) = opt.Value

let stringGet (opt: string option) = Option.get opt

let arrayValue (opt: int[] option) = opt.Value

let resizeArrayValue (opt: ResizeArray<int> option) = opt.Value

let stringMatch (opt: string option) =
    match opt with
    | Some v -> v.Length
    | None -> 0

let nullableStringMatch (opt: (string | null) option) =
    match opt with
    | Some v -> isNull v
    | None -> false
