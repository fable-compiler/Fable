module MonadicTrampoline

type Thunk<'T> =
    | DelayValue of (unit -> Thunk<'T>)
    | ReturnValue of 'T

let rec run = function
    | DelayValue f -> f () |> run
    | ReturnValue x -> x

type TrampolineBuilder() =
    member __.Bind(thunk, f) = DelayValue (fun () -> run thunk |> f)
    member __.Delay f = DelayValue f
    member __.Return a = ReturnValue a
    member __.ReturnFrom (a: Thunk<'T>) = a

let trampoline = new TrampolineBuilder()

let rec trampolineListMapAcc acc f xs =
    trampoline {
        match xs with
        | [] -> return List.rev acc
        | h::t ->
            let! x = f h
            return! trampolineListMapAcc (x::acc) f t
    }

let inline trampolineListMap (f: 'a -> Thunk<'b>) xs =
    trampolineListMapAcc [] f xs

let inline trampolineOptionMap (f: 'a -> Thunk<'b>) opt =
    trampoline {
        match opt with
        | Some e ->
            let! x = f e
            return Some x
        | None -> return None
    }
