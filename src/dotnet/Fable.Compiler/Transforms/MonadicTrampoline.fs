module MonadicTrampoline

#if NETSTANDARD2_0 || !FABLE_COMPILER

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

#else

// Fable cannot tail-call optimize mutually recursive functions, rendering
// the implementation above useless, so we use the following one instead.
// However, take note this uses unsafe boxing/unboxing, which is ok
// in JS but not in .NET (fails with generic types: `obj list :?> Expr list`)
type Thunk =
    | DelayValue of (unit -> Thunk)
    | ReturnValue of obj
    | BindValue of Thunk * f: (obj -> Thunk)

let run thunk =
    let rec runInner (cont: (obj->Thunk) list) = function
        | DelayValue f -> f () |> runInner cont
        | ReturnValue v ->
            match cont with
            | [] -> v
            | f::cont -> f v |> runInner cont
        | BindValue(thunk, f) ->
            runInner (f::cont) thunk
    runInner [] thunk :?> 'T

type TrampolineBuilder() =
    member __.Bind(thunk, f: 'T->Thunk) = BindValue(thunk, unbox >> f)
    member __.Delay f = DelayValue f
    member __.Return a = ReturnValue a
    member __.ReturnFrom (a: Thunk) = a

#endif

let trampoline = TrampolineBuilder()

let rec trampolineListMapAcc acc f xs =
    trampoline {
        match xs with
        | [] -> return List.rev acc
        | h::t ->
            let! x = f h
            return! trampolineListMapAcc (x::acc) f t
    }

let inline trampolineListMap f xs =
    trampolineListMapAcc [] f xs

let inline trampolineOptionMap f opt =
    trampoline {
        match opt with
        | Some e ->
            let! x = f e
            return Some x
        | None -> return None
    }
