// Adapted from:
// https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/ComputationExpressions/Enumerator.fs
// https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/seq.fs
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module SeqModule

open System
open System.Collections.Generic
open Fable.Core

module Enumerator =

    let noReset () =
        raise (NotSupportedException(SR.resetNotSupported))

    let notStarted () =
        raise (InvalidOperationException(SR.enumerationNotStarted))

    let alreadyFinished () =
        raise (InvalidOperationException(SR.enumerationAlreadyFinished))

    [<Sealed>]
    [<CompiledName("Seq")>]
    type Enumerable<'T>(f) =
        interface IEnumerable<'T> with
            member x.GetEnumerator() : IEnumerator<'T> = f ()
            member x.GetEnumerator() : Collections.IEnumerator = f () :> _

        override xs.ToString() =
            let maxCount = 4
            let mutable i = 0
            let mutable str = "seq ["
            use e = (xs :> IEnumerable<'T>).GetEnumerator()

            while (i < maxCount && e.MoveNext()) do
                if i > 0 then
                    str <- str + "; "

                str <- str + (string<'T> e.Current)
                i <- i + 1

            if i = maxCount then
                str <- str + "; ..."

            str + "]"

    type FromFunctions<'T>(_current, _next, _dispose) =
        interface IEnumerator<'T> with
            member _.Current: 'T = _current ()
            member _.Current: obj = box (_current ())
            member _.MoveNext() = _next ()
            member _.Reset() = noReset ()
            member _.Dispose() = _dispose ()

    let inline fromFunctions current next dispose : IEnumerator<'T> =
        new FromFunctions<_>(current, next, dispose) :> IEnumerator<'T>

    //    let cast (e: Collections.IEnumerator): IEnumerator<'T> =
    //        let current() = unbox<'T> e.Current
    //        let next() = e.MoveNext()
    //        let dispose() =
    //            match e with
    //            | :? IDisposable as e -> e.Dispose()
    //            | _ -> ()
    //        fromFunctions current next dispose

    let concat<'T, 'U when 'U :> seq<'T>> (sources: seq<'U>) =
        let mutable outerOpt: IEnumerator<'U> option = None
        let mutable innerOpt: IEnumerator<'T> option = None
        let mutable started = false
        let mutable finished = false
        let mutable curr: 'T = Unchecked.defaultof<'T>

        let current () =
            if not started then
                notStarted ()
            elif finished then
                alreadyFinished ()
            else
                curr

        let finish () =
            finished <- true

            match innerOpt with
            | None -> ()
            | Some inner ->
                try
                    inner.Dispose()
                finally
                    innerOpt <- None

            match outerOpt with
            | None -> ()
            | Some outer ->
                try
                    outer.Dispose()
                finally
                    outerOpt <- None

        let loop () =
            let mutable res = None

            while Option.isNone res do
                match outerOpt, innerOpt with
                | None, _ -> outerOpt <- Some(sources.GetEnumerator())
                | Some outer, None ->
                    if outer.MoveNext() then
                        let ie = outer.Current
                        innerOpt <- Some((ie :> seq<'T>).GetEnumerator())
                    else
                        finish ()
                        res <- Some false
                | Some _, Some inner ->
                    if inner.MoveNext() then
                        curr <- inner.Current
                        res <- Some true
                    else
                        try
                            inner.Dispose()
                        finally
                            innerOpt <- None

            res.Value

        let next () =
            if not started then
                started <- true

            if finished then
                false
            else
                loop ()

        let dispose () =
            if not finished then
                finish ()

        fromFunctions current next dispose

    let enumerateThenFinally f (e: IEnumerator<'T>) : IEnumerator<'T> =
        let current () = e.Current
        let next () = e.MoveNext()

        let dispose () =
            try
                e.Dispose()
            finally
                f ()

        fromFunctions current next dispose

    // We use a tuple here to make sure the generic is wrapped in case it's an option itself
    let generateWhileSome
        (openf: unit -> 'T)
        (compute: 'T -> 'U option)
        (closef: 'T -> unit)
        : IEnumerator<'U>
        =
        let mutable started = false
        let mutable curr: 'U option = None
        let mutable state = Some(openf ())

        let current () =
            if not started then
                notStarted ()

            match curr with
            | None -> alreadyFinished ()
            | Some x -> x

        let dispose () =
            match state with
            | None -> ()
            | Some x ->
                try
                    closef x
                finally
                    state <- None

        let finish () =
            try
                dispose ()
            finally
                curr <- None

        let next () =
            if not started then
                started <- true

            match state with
            | None -> false
            | Some s ->
                match
                    (try
                        compute s
                     with _ ->
                         finish ()
                         reraise ())
                with
                | None ->
                    finish ()
                    false
                | Some _ as x ->
                    curr <- x
                    true

        fromFunctions current next dispose

    let unfold
        (f: 'State -> ('T * 'State) option)
        (state: 'State)
        : IEnumerator<'T>
        =
        let mutable curr: ('T * 'State) option = None
        let mutable acc: 'State = state

        let current () =
            match curr with
            | None -> notStarted ()
            | Some(x, _) -> x

        let next () =
            curr <- f acc

            match curr with
            | None -> false
            | Some(_, st) ->
                acc <- st
                true

        let dispose () = ()
        fromFunctions current next dispose

    let generate
        (create: unit -> 'a)
        (compute: 'a -> 'b option)
        (dispose: 'a -> unit)
        : seq<'b>
        =
        Enumerable(fun () -> generateWhileSome create compute dispose)

    let generateIndexed
        (create: unit -> 'a)
        (compute: int -> 'a -> 'b option)
        (dispose: 'a -> unit)
        : seq<'b>
        =
        Enumerable(fun () ->
            let mutable i = -1

            generateWhileSome
                create
                (fun x ->
                    i <- i + 1
                    compute i x
                )
                dispose
        )

let indexNotFound () =
    raise (KeyNotFoundException(SR.keyNotFoundAlt))

let mkSeq (f: unit -> IEnumerator<'T>) : seq<'T> =
    Enumerator.Enumerable(f) :> IEnumerable<'T>

let ofSeq (xs: seq<'T>) : IEnumerator<'T> = xs.GetEnumerator()

let delay (generator: unit -> seq<'T>) : seq<'T> =
    mkSeq (fun () -> generator().GetEnumerator())

let concat<'Collection, 'T when 'Collection :> seq<'T>>
    (sources: seq<'Collection>)
    : seq<'T>
    =
    mkSeq (fun () -> Enumerator.concat sources)

let unfold
    (generator: 'State -> ('T * 'State) option)
    (state: 'State)
    : seq<'T>
    =
    mkSeq (fun () -> Enumerator.unfold generator state)

let empty () : seq<'T> =
    delay (fun () -> Array.empty :> seq<'T>)

let singleton (x: 'T) : seq<'T> =
    delay (fun () -> (Array.singleton x) :> seq<'T>)

let ofArray (arr: 'T[]) : seq<'T> = arr :> seq<'T>

let toArray (xs: seq<'T>) : 'T[] =
    match xs with
    // | :? array<'T> as a -> Array.ofSeq a
    | :? list<'T> as a -> Array.ofList a
    | _ -> Array.ofSeq xs

let ofList (xs: list<'T>) = (xs :> seq<'T>)

let toList (xs: seq<'T>) : 'T list =
    match xs with
    | :? array<'T> as a -> List.ofArray a
    | :? list<'T> as a -> a
    | _ -> List.ofSeq xs

// let inline generateUsing (openf: unit -> ('U :> IDisposable)) compute =
//     generate openf compute (fun (s: 'U) -> s.Dispose())

let append (xs: seq<'T>) (ys: seq<'T>) =
    concat
        [|
            xs
            ys
        |]

//let cast (xs: Collections.IEnumerable) =
//    mkSeq (fun () ->
//        checkNonNull "source" xs
//        xs.GetEnumerator()
//        |> Enumerator.cast
//    )

let choose (chooser: 'T -> 'U option) (xs: seq<'T>) =
    Enumerator.generate
        (fun () -> ofSeq xs)
        (fun e ->
            let mutable curr = None

            while (Option.isNone curr && e.MoveNext()) do
                curr <-
                    match chooser e.Current with
                    | Some v -> Some v
                    | None -> None

            curr
        )
        (fun e -> e.Dispose())

let compareWith (comparer: 'T -> 'T -> int) (xs: seq<'T>) (ys: seq<'T>) : int =
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable c = 0
    let mutable b1 = e1.MoveNext()
    let mutable b2 = e2.MoveNext()

    while c = 0 && b1 && b2 do
        c <- comparer e1.Current e2.Current

        if c = 0 then
            b1 <- e1.MoveNext()
            b2 <- e2.MoveNext()

    if c <> 0 then
        c
    elif b1 then
        1
    elif b2 then
        -1
    else
        0

let contains
    (value: 'T)
    (xs: seq<'T>)
    ([<Inject>] comparer: IEqualityComparer<'T>)
    =
    use e = ofSeq xs
    let mutable found = false

    while (not found && e.MoveNext()) do
        found <- comparer.Equals(value, e.Current)

    found

let enumerateFromFunctions
    (create: unit -> 'a)
    (moveNext: 'a -> bool)
    (current: 'a -> 'b)
    : seq<'b>
    =
    Enumerator.generate
        create
        (fun x ->
            if moveNext x then
                Some(current x)
            else
                None
        )
        (fun x ->
            match box (x) with
            | :? IDisposable as id -> id.Dispose()
            | _ -> ()
        )

let inline finallyEnumerable<'T>
    (
        compensation: unit -> unit,
        restf: unit -> seq<'T>
    )
    =
    mkSeq (fun () ->
        try
            let e = restf () |> ofSeq
            Enumerator.enumerateThenFinally compensation e
        with _ ->
            compensation ()
            reraise ()
    )

let enumerateThenFinally
    (source: seq<'T>)
    (compensation: unit -> unit)
    : seq<'T>
    =
    finallyEnumerable (compensation, (fun () -> source))

let enumerateUsing
    (resource: 'T :> IDisposable)
    (source: 'T -> #seq<'U>)
    : seq<'U>
    =
    finallyEnumerable (
        // Null checks not necessary because Dart provides null safety
        //        (fun () -> match box resource with null -> () | _ -> resource.Dispose()),
        (fun () -> resource.Dispose()),
        (fun () -> source resource :> seq<_>)
    )

let enumerateWhile (guard: unit -> bool) (xs: seq<'T>) : seq<'T> =
    concat (
        unfold
            (fun i ->
                if guard () then
                    Some(xs, i + 1)
                else
                    None
            )
            0
    )

let filter f (xs: seq<'T>) =
    xs
    |> choose (fun x ->
        if f x then
            Some x
        else
            None
    )

let exists (predicate: 'T -> bool) (xs: seq<'T>) : bool =
    use e = ofSeq xs
    let mutable found = false

    while (not found && e.MoveNext()) do
        found <- predicate e.Current

    found

let exists2
    (predicate: 'T1 -> 'T2 -> bool)
    (xs: seq<'T1>)
    (ys: seq<'T2>)
    : bool
    =
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable found = false

    while (not found && e1.MoveNext() && e2.MoveNext()) do
        found <- predicate e1.Current e2.Current

    found

let exactlyOne (xs: seq<'T>) : 'T =
    use e = ofSeq xs

    if e.MoveNext() then
        let v = e.Current

        if e.MoveNext() then
            invalidArg "source" SR.inputSequenceTooLong
        else
            v
    else
        invalidArg "source" SR.inputSequenceEmpty

let tryExactlyOne (xs: seq<'T>) : 'T option =
    use e = ofSeq xs

    if e.MoveNext() then
        let v = e.Current

        if e.MoveNext() then
            None
        else
            Some v
    else
        None

let find (predicate: 'T -> bool) (xs: seq<'T>) : 'T =
    use e = ofSeq xs
    let mutable found = false
    let mutable res = Unchecked.defaultof<'T>

    while (not found && e.MoveNext()) do
        let c = e.Current

        if predicate c then
            found <- true
            res <- c

    if found then
        res
    else
        indexNotFound ()

let tryFind (predicate: 'T -> bool) (xs: seq<'T>) : 'T option =
    try
        find predicate xs |> Some
    with _ ->
        None

let tryFindBack (predicate: 'T -> bool) (xs: seq<'T>) : 'T option =
    xs |> toArray |> Array.tryFindBack predicate

let findBack (predicate: 'T -> bool) (xs: seq<'T>) : 'T =
    xs |> toArray |> Array.findBack predicate

let findIndex (predicate: 'T -> bool) (xs: seq<'T>) : int =
    use e = ofSeq xs

    let rec loop i =
        if e.MoveNext() then
            if predicate e.Current then
                i
            else
                loop (i + 1)
        else
            indexNotFound ()

    loop 0

let tryFindIndex (predicate: 'T -> bool) (xs: seq<'T>) : int option =
    try
        findIndex predicate xs |> Some
    with _ ->
        None

let tryFindIndexBack (predicate: 'T -> bool) (xs: seq<'T>) : int option =
    xs |> toArray |> Array.tryFindIndexBack predicate

let findIndexBack (predicate: 'T -> bool) (xs: seq<'T>) : int =
    xs |> toArray |> Array.findIndexBack predicate

let fold<'T, 'State>
    (folder: 'State -> 'T -> 'State)
    (state: 'State)
    (xs: seq<'T>)
    : 'State
    =
    use e = ofSeq xs
    let mutable acc = state

    while e.MoveNext() do
        acc <- folder acc e.Current

    acc

let foldBack<'T, 'State>
    (folder: 'T -> 'State -> 'State)
    (xs: seq<'T>)
    (state: 'State)
    : 'State
    =
    Array.foldBack folder (toArray xs) state

let fold2<'T1, 'T2, 'State>
    (folder: 'State -> 'T1 -> 'T2 -> 'State)
    (state: 'State)
    (xs: seq<'T1>)
    (ys: seq<'T2>)
    : 'State
    =
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable acc = state

    while e1.MoveNext() && e2.MoveNext() do
        acc <- folder acc e1.Current e2.Current

    acc

let foldBack2
    (folder: 'T1 -> 'T2 -> 'State -> 'State)
    (xs: seq<'T1>)
    (ys: seq<'T2>)
    (state: 'State)
    : 'State
    =
    Array.foldBack2 folder (toArray xs) (toArray ys) state

let forAll (predicate: 'a -> bool) (xs: seq<'a>) : bool =
    not (exists (fun x -> not (predicate x)) xs)

let forAll2 (predicate: 'a -> 'b -> bool) (xs: seq<'a>) (ys: seq<'b>) : bool =
    not (exists2 (fun x y -> not (predicate x y)) xs ys)

let head (xs: seq<'T>) : 'T =
    match xs with
    | :? array<'T> as a -> Array.head a
    | :? list<'T> as a -> List.head a
    | _ ->
        use e = ofSeq xs

        if e.MoveNext() then
            e.Current
        else
            invalidArg "source" SR.inputSequenceEmpty

let tryHead (xs: seq<'T>) : 'T option =
    try
        head xs |> Some
    with _ ->
        None

let initialize (count: int) (f: int -> 'a) : seq<'a> =
    unfold
        (fun i ->
            if (i < count) then
                Some(f i, i + 1)
            else
                None
        )
        0

let initializeInfinite (f: int -> 'a) : seq<'a> = initialize (Int32.MaxValue) f

let isEmpty (xs: seq<'T>) : bool =
    match xs with
    | :? array<'T> as a -> Array.isEmpty a
    | :? list<'T> as a -> List.isEmpty a
    | _ ->
        use e = ofSeq xs
        not (e.MoveNext())

let item (index: int) (xs: seq<'T>) : 'T =
    match xs with
    | :? array<'T> as a -> Array.item index a
    | :? list<'T> as a -> List.item index a
    | _ ->
        use e = ofSeq xs

        let rec loop index =
            if not (e.MoveNext()) then
                invalidArg "index" SR.notEnoughElements
            elif index = 0 then
                e.Current
            else
                loop (index - 1)

        loop index

let tryItem (index: int) (xs: seq<'T>) : 'T option =
    try
        item index xs |> Some
    with _ ->
        None

let iterate (action: 'a -> unit) (xs: seq<'a>) : unit =
    fold (fun () x -> action x) () xs

let iterate2 (action: 'a -> 'b -> unit) (xs: seq<'a>) (ys: seq<'b>) : unit =
    fold2 (fun () x y -> action x y) () xs ys

let iterateIndexed (action: int -> 'a -> unit) (xs: seq<'a>) : unit =
    fold
        (fun i x ->
            action i x
            i + 1
        )
        0
        xs
    |> ignore

let iterateIndexed2
    (action: int -> 'a -> 'b -> unit)
    (xs: seq<'a>)
    (ys: seq<'b>)
    : unit
    =
    fold2
        (fun i x y ->
            action i x y
            i + 1
        )
        0
        xs
        ys
    |> ignore

let last (xs: seq<'T>) : 'T =
    // if isEmpty xs then None
    // else Some (reduce (fun _ x -> x) xs)
    use e = ofSeq xs

    let rec loop acc =
        if not (e.MoveNext()) then
            acc
        else
            loop e.Current

    if e.MoveNext() then
        loop e.Current
    else
        invalidArg "source" SR.notEnoughElements

let tryLast (xs: seq<'T>) : 'T option =
    try
        last xs |> Some
    with _ ->
        None

let length (xs: seq<'T>) : int =
    match xs with
    | :? array<'T> as a -> Array.length a
    | :? list<'T> as a -> List.length a
    | _ ->
        use e = ofSeq xs
        let mutable count = 0

        while e.MoveNext() do
            count <- count + 1

        count

let map (mapping: 'T -> 'U) (xs: seq<'T>) : seq<'U> =
    Enumerator.generate
        (fun () -> ofSeq xs)
        (fun e ->
            if e.MoveNext() then
                mapping e.Current |> Some
            else
                None
        )
        (fun e -> e.Dispose())

let mapIndexed (mapping: int -> 'T -> 'U) (xs: seq<'T>) : seq<'U> =
    Enumerator.generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if e.MoveNext() then
                mapping i e.Current |> Some
            else
                None
        )
        (fun e -> e.Dispose())

let indexed (xs: seq<'T>) : seq<int * 'T> = xs |> mapIndexed (fun i x -> (i, x))

let map2 (mapping: 'T1 -> 'T2 -> 'U) (xs: seq<'T1>) (ys: seq<'T2>) : seq<'U> =
    Enumerator.generate
        (fun () -> (ofSeq xs, ofSeq ys))
        (fun (e1, e2) ->
            if e1.MoveNext() && e2.MoveNext() then
                mapping e1.Current e2.Current |> Some
            else
                None
        )
        (fun (e1, e2) ->
            try
                e1.Dispose()
            finally
                e2.Dispose()
        )

let mapIndexed2
    (mapping: int -> 'T1 -> 'T2 -> 'U)
    (xs: seq<'T1>)
    (ys: seq<'T2>)
    : seq<'U>
    =
    Enumerator.generateIndexed
        (fun () -> (ofSeq xs, ofSeq ys))
        (fun i (e1, e2) ->
            if e1.MoveNext() && e2.MoveNext() then
                mapping i e1.Current e2.Current |> Some
            else
                None
        )
        (fun (e1, e2) ->
            try
                e1.Dispose()
            finally
                e2.Dispose()
        )

let map3
    (mapping: 'T1 -> 'T2 -> 'T3 -> 'U)
    (xs: seq<'T1>)
    (ys: seq<'T2>)
    (zs: seq<'T3>)
    : seq<'U>
    =
    Enumerator.generate
        (fun () -> (ofSeq xs, ofSeq ys, ofSeq zs))
        (fun (e1, e2, e3) ->
            if e1.MoveNext() && e2.MoveNext() && e3.MoveNext() then
                mapping e1.Current e2.Current e3.Current |> Some
            else
                None
        )
        (fun (e1, e2, e3) ->
            try
                e1.Dispose()
            finally
                try
                    e2.Dispose()
                finally
                    e3.Dispose()
        )

let readOnly (xs: seq<'T>) : seq<'T> =
    //    checkNonNull "source" xs
    map id xs

type CachedSeq<'T>(cleanup, res: seq<'T>) =
    interface IDisposable with
        member _.Dispose() = cleanup ()

    interface IEnumerable<'T> with
        member _.GetEnumerator() = res.GetEnumerator()

    interface Collections.IEnumerable with
        member _.GetEnumerator() =
            (res :> Collections.IEnumerable).GetEnumerator()

    member _.Clear() = cleanup ()

// Adapted from https://github.com/dotnet/fsharp/blob/eb1337f218275da5294b5fbab2cf77f35ca5f717/src/fsharp/FSharp.Core/seq.fs#L971
let cache (source: seq<'T>) =
    //    checkNonNull "source" source
    //  a seq to ensure that it is enumerated just once and only as far as is necessary.
    //
    // This code is required to be thread safe.
    // The necessary calls should be called at most once (include .MoveNext() = false).
    // The enumerator should be disposed (and dropped) when no longer required.
    //------
    // The state is (prefix,enumerator) with invariants:
    //   * the prefix followed by elts from the enumerator are the initial sequence.
    //   * the prefix contains only as many elements as the longest enumeration so far.
    let prefix = ResizeArray<_>()

    // None          = Unstarted.
    // Some(Some e)  = Started.
    // Some None     = Finished.
    let mutable started = false
    let mutable enumeratorR = None

    let oneStepTo i =
        // If possible, step the enumeration to prefix length i (at most one step).
        // Be speculative, since this could have already happened via another thread.
        if i >= prefix.Count then // is a step still required?
            // If not yet started, start it (create enumerator).
            let optEnumerator =
                if not started then
                    let optEnumerator = Some(source.GetEnumerator())
                    enumeratorR <- optEnumerator
                    started <- true
                    optEnumerator
                else
                    enumeratorR

            match optEnumerator with
            | Some enumerator ->
                if enumerator.MoveNext() then
                    prefix.Add(enumerator.Current)
                else
                    enumerator.Dispose() // Move failed, dispose enumerator,
                    enumeratorR <- None // drop it and record finished.
            | None -> ()

    let result =
        unfold
            (fun i ->
                // i being the next position to be returned
                // A lock is needed over the reads to prefix.Count since the list may be being resized
                // NOTE: we could change to a reader/writer lock here
                lock prefix
                <| fun () ->
                    if i < prefix.Count then
                        Some(prefix[i], i + 1)
                    else
                        oneStepTo i

                        if i < prefix.Count then
                            Some(prefix[i], i + 1)
                        else
                            None
            )
            0

    let cleanup () =
        lock prefix
        <| fun () ->
            prefix.Clear()

            match enumeratorR with
            | Some e -> e.Dispose()
            | _ -> ()

            enumeratorR <- None

    (new CachedSeq<_>(cleanup, result) :> seq<_>)

let allPairs (xs: seq<'T1>) (ys: seq<'T2>) : seq<'T1 * 'T2> =
    let ysCache = cache ys

    delay (fun () ->
        let mapping (x: 'T1) : seq<'T1 * 'T2> =
            ysCache |> map (fun y -> (x, y))

        concat (map mapping xs)
    )

let mapFold
    (mapping: 'State -> 'T -> 'Result * 'State)
    (state: 'State)
    (xs: seq<'T>)
    : seq<'Result> * 'State
    =
    let arr, state = Array.mapFold mapping state (toArray xs)
    readOnly arr, state

let mapFoldBack
    (mapping: 'T -> 'State -> 'Result * 'State)
    (xs: seq<'T>)
    (state: 'State)
    : seq<'Result> * 'State
    =
    let arr, state = Array.mapFoldBack mapping (toArray xs) state
    readOnly arr, state

let tryPick (chooser: 'T -> 'a option) (xs: seq<'T>) : 'a option =
    use e = ofSeq xs
    let mutable res = None

    while (Option.isNone res && e.MoveNext()) do
        res <- chooser e.Current

    res

let pick (chooser: 'T -> 'a option) (xs: seq<'T>) : 'a =
    match tryPick chooser xs with
    | Some x -> x
    | None -> indexNotFound ()

let reduce (folder: 'T -> 'T -> 'T) (xs: seq<'T>) : 'T =
    use e = ofSeq xs

    let rec loop acc =
        if e.MoveNext() then
            loop (folder acc e.Current)
        else
            acc

    if e.MoveNext() then
        loop e.Current
    else
        invalidOp SR.inputSequenceEmpty

let reduceBack (folder: 'T -> 'T -> 'T) (xs: seq<'T>) : 'T =
    let arr = toArray xs

    if Array.isEmpty arr then
        invalidOp SR.inputSequenceEmpty

    Array.reduceBack folder arr

let replicate (n: int) (x: 'a) : seq<'a> = initialize n (fun _ -> x)

let reverse (xs: seq<'T>) : seq<'T> =
    delay (fun () -> xs |> toArray |> Array.rev |> ofArray)

let scan<'T, 'State>
    (folder: 'State -> 'T -> 'State)
    (state: 'State)
    (xs: seq<'T>)
    : seq<'State>
    =
    delay (fun () ->
        let first = singleton state
        let mutable acc = state

        let rest =
            xs
            |> map (fun x ->
                acc <- folder acc x
                acc
            )

        [|
            first
            rest
        |]
        |> concat
    )

let scanBack<'T, 'State>
    (folder: 'T -> 'State -> 'State)
    (xs: seq<'T>)
    (state: 'State)
    : seq<'State>
    =
    delay (fun () ->
        let arr = toArray xs
        Array.scanBack folder arr state |> ofArray
    )

let skip (count: int) (source: seq<'T>) : seq<'T> =
    mkSeq (fun () ->
        let e = ofSeq source

        try
            for _ = 1 to count do
                if not (e.MoveNext()) then
                    invalidArg "source" SR.notEnoughElements

            let compensation () = ()
            Enumerator.enumerateThenFinally compensation e
        with _ ->
            e.Dispose()
            reraise ()
    )

let skipWhile (predicate: 'T -> bool) (xs: seq<'T>) : seq<'T> =
    delay (fun () ->
        let mutable skipped = true

        xs
        |> filter (fun x ->
            if skipped then
                skipped <- predicate x

            not skipped
        )
    )

let tail (xs: seq<'T>) : seq<'T> = skip 1 xs

let take (count: int) (xs: seq<'T>) : seq<'T> =
    Enumerator.generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if i < count then
                if e.MoveNext() then
                    Some(e.Current)
                else
                    invalidArg "source" SR.notEnoughElements
            else
                None
        )
        (fun e -> e.Dispose())

let takeWhile (predicate: 'T -> bool) (xs: seq<'T>) : seq<'T> =
    Enumerator.generate
        (fun () -> ofSeq xs)
        (fun e ->
            if e.MoveNext() && predicate e.Current then
                Some(e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let truncate (count: int) (xs: seq<'T>) : seq<'T> =
    Enumerator.generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if i < count && e.MoveNext() then
                Some(e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let zip (xs: seq<'T1>) (ys: seq<'T2>) : seq<'T1 * 'T2> =
    map2 (fun x y -> (x, y)) xs ys

let zip3 (xs: seq<'T1>) (ys: seq<'T2>) (zs: seq<'T3>) : seq<'T1 * 'T2 * 'T3> =
    map3 (fun x y z -> (x, y, z)) xs ys zs

let collect<'T, 'Collection, 'U when 'Collection :> 'U seq>
    (mapping: 'T -> 'Collection)
    (xs: seq<'T>)
    : seq<'U>
    =
    delay (fun () -> xs |> map mapping |> concat)

let where (predicate: 'T -> bool) (xs: seq<'T>) : seq<'T> = filter predicate xs

let pairwise (xs: seq<'T>) : seq<'T * 'T> =
    delay (fun () -> xs |> toArray |> Array.pairwise |> ofArray)

let splitInto (chunks: int) (xs: seq<'T>) : 'T[] seq =
    delay (fun () -> xs |> toArray |> Array.splitInto chunks |> ofArray)

let windowed (windowSize: int) (xs: seq<'T>) : 'T[] seq =
    delay (fun () -> xs |> toArray |> Array.windowed windowSize |> ofArray)

let transpose (xss: seq<#seq<'T>>) : seq<seq<'T>> =
    delay (fun () ->
        xss
        |> toArray
        |> Array.map toArray
        |> Array.transpose
        |> Array.map ofArray
        |> ofArray
    )

let sortWith (comparer: 'T -> 'T -> int) (xs: seq<'T>) =
    delay (fun () ->
        let arr = toArray xs
        Array.sortInPlaceWith comparer arr // Note: In JS this sort is stable
        arr |> ofArray
    )

let sort (xs: seq<'T>) ([<Inject>] comparer: IComparer<'T>) =
    sortWith (fun x y -> comparer.Compare(x, y)) xs

let sortBy
    (projection: 'T -> 'U)
    (xs: seq<'T>)
    ([<Inject>] comparer: IComparer<'U>)
    =
    sortWith (fun x y -> comparer.Compare(projection x, projection y)) xs

let sortDescending (xs: seq<'T>) ([<Inject>] comparer: IComparer<'T>) =
    sortWith (fun x y -> comparer.Compare(x, y) * -1) xs

let sortByDescending
    (projection: 'T -> 'U)
    (xs: seq<'T>)
    ([<Inject>] comparer: IComparer<'U>)
    =
    sortWith (fun x y -> comparer.Compare(projection x, projection y) * -1) xs

let sum (xs: seq<'T>) ([<Inject>] adder: IGenericAdder<'T>) : 'T =
    fold (fun acc x -> adder.Add(acc, x)) (adder.GetZero()) xs

let sumBy
    (f: 'T -> 'U)
    (xs: seq<'T>)
    ([<Inject>] adder: IGenericAdder<'U>)
    : 'U
    =
    fold (fun acc x -> adder.Add(acc, f x)) (adder.GetZero()) xs

let maxBy (projection: 'T -> 'U) xs ([<Inject>] comparer: IComparer<'U>) : 'T =
    reduce
        (fun x y ->
            if comparer.Compare(projection y, projection x) > 0 then
                y
            else
                x
        )
        xs

let max xs ([<Inject>] comparer: IComparer<'T>) : 'T =
    reduce
        (fun x y ->
            if comparer.Compare(y, x) > 0 then
                y
            else
                x
        )
        xs

let minBy (projection: 'T -> 'U) xs ([<Inject>] comparer: IComparer<'U>) : 'T =
    reduce
        (fun x y ->
            if comparer.Compare(projection y, projection x) > 0 then
                x
            else
                y
        )
        xs

let min (xs: seq<'T>) ([<Inject>] comparer: IComparer<'T>) : 'T =
    reduce
        (fun x y ->
            if comparer.Compare(y, x) > 0 then
                x
            else
                y
        )
        xs

let average (xs: seq<'T>) ([<Inject>] averager: IGenericAverager<'T>) : 'T =
    let mutable count = 0

    let folder acc x =
        count <- count + 1
        averager.Add(acc, x)

    let total = fold folder (averager.GetZero()) xs

    if count = 0 then
        invalidArg "source" SR.inputSequenceEmpty
    else
        averager.DivideByInt(total, count)

let averageBy
    (f: 'T -> 'U)
    (xs: seq<'T>)
    ([<Inject>] averager: IGenericAverager<'U>)
    : 'U
    =
    let mutable count = 0

    let inline folder acc x =
        count <- count + 1
        averager.Add(acc, f x)

    let total = fold folder (averager.GetZero()) xs

    if count = 0 then
        invalidArg "source" SR.inputSequenceEmpty
    else
        averager.DivideByInt(total, count)

let permute f (xs: seq<'T>) =
    delay (fun () -> xs |> toArray |> Array.permute f |> ofArray)

let chunkBySize (chunkSize: int) (xs: seq<'T>) : seq<'T[]> =
    delay (fun () -> xs |> toArray |> Array.chunkBySize chunkSize |> ofArray)

let insertAt (index: int) (y: 'T) (xs: seq<'T>) : seq<'T> =
    let mutable isDone = false

    if index < 0 then
        invalidArg "index" SR.indexOutOfBounds

    Enumerator.generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if (isDone || i < index) && e.MoveNext() then
                Some(e.Current)
            elif i = index then
                isDone <- true
                Some y
            else
                if not isDone then
                    invalidArg "index" SR.indexOutOfBounds

                None
        )
        (fun e -> e.Dispose())

let insertManyAt (index: int) (ys: seq<'T>) (xs: seq<'T>) : seq<'T> =
    // incomplete -1, in-progress 0, complete 1
    let mutable status = -1

    if index < 0 then
        invalidArg "index" SR.indexOutOfBounds

    Enumerator.generateIndexed
        (fun () -> ofSeq xs, ofSeq ys)
        (fun i (e1, e2) ->
            if i = index then
                status <- 0

            let inserted =
                if status = 0 then
                    if e2.MoveNext() then
                        Some e2.Current
                    else
                        status <- 1
                        None
                else
                    None

            match inserted with
            | Some inserted -> Some inserted
            | None ->
                if e1.MoveNext() then
                    Some e1.Current
                else
                    if status < 1 then
                        invalidArg "index" SR.indexOutOfBounds

                    None
        )
        (fun (e1, e2) ->
            e1.Dispose()
            e2.Dispose()
        )

let removeAt (index: int) (xs: seq<'T>) : seq<'T> =
    let mutable isDone = false

    if index < 0 then
        invalidArg "index" SR.indexOutOfBounds

    Enumerator.generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if (isDone || i < index) && e.MoveNext() then
                Some(e.Current)
            elif i = index && e.MoveNext() then
                isDone <- true

                if e.MoveNext() then
                    Some(e.Current)
                else
                    None
            else
                if not isDone then
                    invalidArg "index" SR.indexOutOfBounds

                None
        )
        (fun e -> e.Dispose())

let removeManyAt (index: int) (count: int) (xs: seq<'T>) : seq<'T> =
    if index < 0 then
        invalidArg "index" SR.indexOutOfBounds

    Enumerator.generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if i < index then
                if e.MoveNext() then
                    Some(e.Current)
                else
                    invalidArg "index" SR.indexOutOfBounds
            else
                if i = index then
                    for _ = 1 to count do
                        if not (e.MoveNext()) then
                            invalidArg "count" SR.indexOutOfBounds

                if e.MoveNext() then
                    Some(e.Current)
                else
                    None
        )
        (fun e -> e.Dispose())

let updateAt (index: int) (y: 'T) (xs: seq<'T>) : seq<'T> =
    let mutable isDone = false

    if index < 0 then
        invalidArg "index" SR.indexOutOfBounds

    Enumerator.generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if (isDone || i < index) && e.MoveNext() then
                Some(e.Current)
            elif i = index && e.MoveNext() then
                isDone <- true
                Some y
            else
                if not isDone then
                    invalidArg "index" SR.indexOutOfBounds

                None
        )
        (fun e -> e.Dispose())
