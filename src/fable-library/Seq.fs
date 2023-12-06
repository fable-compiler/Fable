// Adapted from:
// https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/ComputationExpressions/Enumerator.fs
// https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/seq.fs
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module SeqModule

open Fable.Core

type IEnumerator<'T> = System.Collections.Generic.IEnumerator<'T>
type IEnumerable<'T> = System.Collections.Generic.IEnumerable<'T>

module SR =
    let enumerationAlreadyFinished = "Enumeration already finished."
    let enumerationNotStarted = "Enumeration has not started. Call MoveNext."
    let inputSequenceEmpty = "The input sequence was empty."

    let inputSequenceTooLong =
        "The input sequence contains more than one element."

    let keyNotFoundAlt =
        "An index satisfying the predicate was not found in the collection."

    let notEnoughElements =
        "The input sequence has an insufficient number of elements."

    let resetNotSupported = "Reset is not supported on this enumerator."

module Enumerator =

    let noReset () =
        raise (System.NotSupportedException(SR.resetNotSupported))

    let notStarted () =
        raise (System.InvalidOperationException(SR.enumerationNotStarted))

    let alreadyFinished () =
        raise (System.InvalidOperationException(SR.enumerationAlreadyFinished))

    [<Sealed>]
    [<CompiledName("Seq")>]
    type Enumerable<'T>(f) =
        interface IEnumerable<'T> with
            member x.GetEnumerator() = f ()

        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = f () :> System.Collections.IEnumerator

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

    type FromFunctions<'T>(current, next, dispose) =
        interface IEnumerator<'T> with
            member _.Current = current ()

        interface System.Collections.IEnumerator with
            member _.Current = box (current ())
            member _.MoveNext() = next ()
            member _.Reset() = noReset ()

        interface System.IDisposable with
            member _.Dispose() = dispose ()

    let inline fromFunctions current next dispose : IEnumerator<'T> =
        new FromFunctions<_>(current, next, dispose) :> IEnumerator<'T>

    // // implementation for languages where arrays are not IEnumerable
    //
    // let empty<'T>(): IEnumerator<'T> =
    //     let mutable started = false
    //     let current() = if not started then notStarted() else alreadyFinished()
    //     let next() = started <- true; false
    //     let dispose() = ()
    //     fromFunctions current next dispose
    //
    // let singleton (x: 'T): IEnumerator<'T> =
    //     let mutable index = -1
    //     let current() =
    //         if index < 0 then notStarted()
    //         if index > 0 then alreadyFinished()
    //         x
    //     let next() = index <- index + 1; index = 0
    //     let dispose() = ()
    //     fromFunctions current next dispose
    //
    // let ofArray (arr: 'T[]): IEnumerator<'T> =
    //     let len = arr.Length
    //     let mutable i = -1
    //     let current() =
    //         if i < 0 then notStarted()
    //         elif i >= len then alreadyFinished()
    //         else arr.[i]
    //     let next() =
    //         if i < len then
    //             i <- i + 1
    //             i < len
    //         else false
    //     let dispose() = ()
    //     fromFunctions current next dispose

    let cast (e: IEnumerator<'T>) : IEnumerator<'T> =
        let current () = unbox<'T> e.Current
        let next () = e.MoveNext()

        let dispose () =
            match e with
            | :? System.IDisposable as e -> e.Dispose()
            | _ -> ()

        fromFunctions current next dispose

    let concat<'T, 'U when 'U :> seq<'T>> (sources: seq<'U>) =
        let mutable outerOpt: IEnumerator<'U> option = None
        let mutable innerOpt: IEnumerator<'T> option = None
        let mutable started = false
        let mutable finished = false
        let mutable curr = None

        let current () =
            if not started then
                notStarted ()
            elif finished then
                alreadyFinished ()

            match curr with
            | None -> alreadyFinished ()
            | Some x -> x

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
                        innerOpt <- Some(ie.GetEnumerator())
                    else
                        finish ()
                        res <- Some false
                | Some _, Some inner ->
                    if inner.MoveNext() then
                        curr <- Some(inner.Current)
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

    let generateWhileSome
        (openf: unit -> 'T)
        (compute: 'T -> 'U option)
        (closef: 'T -> unit)
        : IEnumerator<'U>
        =
        let mutable started = false
        let mutable curr = None
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
            | Some(x, st) -> x

        let next () =
            curr <- f acc

            match curr with
            | None -> false
            | Some(x, st) ->
                acc <- st
                true

        let dispose () = ()
        fromFunctions current next dispose

// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module Seq =

let indexNotFound () =
    raise (System.Collections.Generic.KeyNotFoundException(SR.keyNotFoundAlt))

let checkNonNull argName arg =
    if isNull arg then
        nullArg argName

let mkSeq (f: unit -> IEnumerator<'T>) : seq<'T> =
    Enumerator.Enumerable(f) :> IEnumerable<'T>

let ofSeq (xs: seq<'T>) : IEnumerator<'T> =
    checkNonNull "source" xs
    xs.GetEnumerator()

let delay (generator: unit -> seq<'T>) =
    mkSeq (fun () -> generator().GetEnumerator())

let concat<'Collection, 'T when 'Collection :> seq<'T>>
    (sources: seq<'Collection>)
    : seq<'T>
    =
    mkSeq (fun () -> Enumerator.concat sources)

let unfold (generator: 'State -> ('T * 'State) option) (state: 'State) =
    mkSeq (fun () -> Enumerator.unfold generator state)

let empty () =
    delay (fun () -> Array.empty :> seq<'T>)

let singleton x =
    delay (fun () -> (Array.singleton x) :> seq<'T>)

let ofArray (arr: 'T[]) = arr :> seq<'T>

let toArray (xs: seq<'T>) : 'T[] =
    match xs with
    // | :? array<'T> as a -> Array.ofSeq a
    | :? list<'T> as a -> Array.ofList a
    | _ -> Array.ofSeq xs

let ofList (xs: list<'T>) = (xs :> seq<'T>)

let toList (xs: seq<'T>) : list<'T> =
    match xs with
    | :? array<'T> as a -> List.ofArray a
    | :? list<'T> as a -> a
    | _ -> List.ofSeq xs

let generate create compute dispose =
    mkSeq (fun () -> Enumerator.generateWhileSome create compute dispose)

let generateIndexed create compute dispose =
    mkSeq (fun () ->
        let mutable i = -1

        Enumerator.generateWhileSome
            create
            (fun x ->
                i <- i + 1
                compute i x
            )
            dispose
    )

// let inline generateUsing (openf: unit -> ('U :> System.IDisposable)) compute =
//     generate openf compute (fun (s: 'U) -> s.Dispose())

let append (xs: seq<'T>) (ys: seq<'T>) =
    concat
        [|
            xs
            ys
        |]

let cast (xs: IEnumerable<'T>) =
    mkSeq (fun () ->
        checkNonNull "source" xs
        xs.GetEnumerator() |> Enumerator.cast
    )

let choose (chooser: 'T -> 'U option) (xs: seq<'T>) =
    generate
        (fun () -> ofSeq xs)
        (fun e ->
            let mutable curr = None

            while (Option.isNone curr && e.MoveNext()) do
                curr <- chooser e.Current

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
    ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'T>)
    =
    use e = ofSeq xs
    let mutable found = false

    while (not found && e.MoveNext()) do
        found <- comparer.Equals(value, e.Current)

    found

let enumerateFromFunctions create moveNext current =
    generate
        create
        (fun x ->
            if moveNext x then
                Some(current x)
            else
                None
        )
        (fun x ->
            match box (x) with
            | :? System.IDisposable as id -> id.Dispose()
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

let enumerateThenFinally (source: seq<'T>) (compensation: unit -> unit) =
    finallyEnumerable (compensation, (fun () -> source))

let enumerateUsing
    (resource: 'T :> System.IDisposable)
    (source: 'T -> #seq<'U>)
    : seq<'U>
    =
    finallyEnumerable (
        (fun () ->
            match box resource with
            | null -> ()
            | _ -> resource.Dispose()
        ),
        (fun () -> source resource :> seq<_>)
    )

let enumerateWhile (guard: unit -> bool) (xs: seq<'T>) =
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

let exists predicate (xs: seq<'T>) =
    use e = ofSeq xs
    let mutable found = false

    while (not found && e.MoveNext()) do
        found <- predicate e.Current

    found

let exists2 (predicate: 'T1 -> 'T2 -> bool) (xs: seq<'T1>) (ys: seq<'T2>) =
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable found = false

    while (not found && e1.MoveNext() && e2.MoveNext()) do
        found <- predicate e1.Current e2.Current

    found

let exactlyOne (xs: seq<'T>) =
    use e = ofSeq xs

    if e.MoveNext() then
        let v = e.Current

        if e.MoveNext() then
            invalidArg "source" SR.inputSequenceTooLong
        else
            v
    else
        invalidArg "source" SR.inputSequenceEmpty

let tryExactlyOne (xs: seq<'T>) =
    use e = ofSeq xs

    if e.MoveNext() then
        let v = e.Current

        if e.MoveNext() then
            None
        else
            Some v
    else
        None

let tryFind predicate (xs: seq<'T>) =
    use e = ofSeq xs
    let mutable res = None

    while (Option.isNone res && e.MoveNext()) do
        let c = e.Current

        if predicate c then
            res <- Some c

    res

let find predicate (xs: seq<'T>) =
    match tryFind predicate xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryFindBack predicate (xs: seq<'T>) =
    xs |> toArray |> Array.tryFindBack predicate

let findBack predicate (xs: seq<'T>) =
    match tryFindBack predicate xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryFindIndex predicate (xs: seq<'T>) =
    use e = ofSeq xs

    let rec loop i =
        if e.MoveNext() then
            if predicate e.Current then
                Some i
            else
                loop (i + 1)
        else
            None

    loop 0

let findIndex predicate (xs: seq<'T>) =
    match tryFindIndex predicate xs with
    | Some x -> x
    | None ->
        indexNotFound ()
        -1

let tryFindIndexBack predicate (xs: seq<'T>) =
    xs |> toArray |> Array.tryFindIndexBack predicate

let findIndexBack predicate (xs: seq<'T>) =
    match tryFindIndexBack predicate xs with
    | Some x -> x
    | None ->
        indexNotFound ()
        -1

let fold<'T, 'State>
    (folder: 'State -> 'T -> 'State)
    (state: 'State)
    (xs: seq<'T>)
    =
    use e = ofSeq xs
    let mutable acc = state

    while e.MoveNext() do
        acc <- folder acc e.Current

    acc

let foldBack<'T, 'State> folder (xs: seq<'T>) state =
    Array.foldBack folder (toArray xs) state

let fold2<'T1, 'T2, 'State>
    (folder: 'State -> 'T1 -> 'T2 -> 'State)
    (state: 'State)
    (xs: seq<'T1>)
    (ys: seq<'T2>)
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
    =
    Array.foldBack2 folder (toArray xs) (toArray ys) state

let forAll predicate xs =
    not (exists (fun x -> not (predicate x)) xs)

let forAll2 predicate xs ys =
    not (exists2 (fun x y -> not (predicate x y)) xs ys)

let tryHead (xs: seq<'T>) =
    match xs with
    | :? array<'T> as a -> Array.tryHead a
    | :? list<'T> as a -> List.tryHead a
    | _ ->
        use e = ofSeq xs

        if e.MoveNext() then
            Some(e.Current)
        else
            None

let head (xs: seq<'T>) =
    match tryHead xs with
    | Some x -> x
    | None -> invalidArg "source" SR.inputSequenceEmpty

let initialize count f =
    unfold
        (fun i ->
            if (i < count) then
                Some(f i, i + 1)
            else
                None
        )
        0

let initializeInfinite f = initialize (System.Int32.MaxValue) f

let isEmpty (xs: seq<'T>) =
    match xs with
    | :? array<'T> as a -> Array.isEmpty a
    | :? list<'T> as a -> List.isEmpty a
    | _ ->
        use e = ofSeq xs
        not (e.MoveNext())

let tryItem index (xs: seq<'T>) =
    match xs with
    | :? array<'T> as a -> Array.tryItem index a
    | :? list<'T> as a -> List.tryItem index a
    | _ ->
        use e = ofSeq xs

        let rec loop index =
            if not (e.MoveNext()) then
                None
            elif index = 0 then
                Some e.Current
            else
                loop (index - 1)

        loop index

let item index (xs: seq<'T>) =
    match tryItem index xs with
    | Some x -> x
    | None -> invalidArg "index" SR.notEnoughElements

let iterate action xs = fold (fun () x -> action x) () xs

let iterate2 action xs ys =
    fold2 (fun () x y -> action x y) () xs ys

let iterateIndexed action xs =
    fold
        (fun i x ->
            action i x
            i + 1
        )
        0
        xs
    |> ignore

let iterateIndexed2 action xs ys =
    fold2
        (fun i x y ->
            action i x y
            i + 1
        )
        0
        xs
        ys
    |> ignore

let tryLast (xs: seq<'T>) =
    // if isEmpty xs then None
    // else Some (reduce (fun _ x -> x) xs)
    use e = ofSeq xs

    let rec loop acc =
        if not (e.MoveNext()) then
            acc
        else
            loop e.Current

    if e.MoveNext() then
        Some(loop e.Current)
    else
        None

let last (xs: seq<'T>) =
    match tryLast xs with
    | Some x -> x
    | None -> invalidArg "source" SR.notEnoughElements

let length (xs: seq<'T>) =
    match xs with
    | :? array<'T> as a -> Array.length a
    | :? list<'T> as a -> List.length a
    | _ ->
        use e = ofSeq xs
        let mutable count = 0

        while e.MoveNext() do
            count <- count + 1

        count

let map (mapping: 'T -> 'U) (xs: seq<'T>) =
    generate
        (fun () -> ofSeq xs)
        (fun e ->
            if e.MoveNext() then
                Some(mapping e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let mapIndexed (mapping: int -> 'T -> 'U) (xs: seq<'T>) =
    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if e.MoveNext() then
                Some(mapping i e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let indexed (xs: seq<'T>) = xs |> mapIndexed (fun i x -> (i, x))

let map2 (mapping: 'T1 -> 'T2 -> 'U) (xs: seq<'T1>) (ys: seq<'T2>) =
    generate
        (fun () -> (ofSeq xs, ofSeq ys))
        (fun (e1, e2) ->
            if e1.MoveNext() && e2.MoveNext() then
                Some(mapping e1.Current e2.Current)
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
    =
    generateIndexed
        (fun () -> (ofSeq xs, ofSeq ys))
        (fun i (e1, e2) ->
            if e1.MoveNext() && e2.MoveNext() then
                Some(mapping i e1.Current e2.Current)
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
    =
    generate
        (fun () -> (ofSeq xs, ofSeq ys, ofSeq zs))
        (fun (e1, e2, e3) ->
            if e1.MoveNext() && e2.MoveNext() && e3.MoveNext() then
                Some(mapping e1.Current e2.Current e3.Current)
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

let readOnly (xs: seq<'T>) =
    checkNonNull "source" xs
    map id xs

type CachedSeq<'T>(cleanup, res: seq<'T>) =
    interface System.IDisposable with
        member _.Dispose() = cleanup ()

    interface System.Collections.Generic.IEnumerable<'T> with
        member _.GetEnumerator() = res.GetEnumerator()

    interface System.Collections.IEnumerable with
        member _.GetEnumerator() =
            (res :> System.Collections.IEnumerable).GetEnumerator()

    member _.Clear() = cleanup ()

// Adapted from https://github.com/dotnet/fsharp/blob/eb1337f218275da5294b5fbab2cf77f35ca5f717/src/fsharp/FSharp.Core/seq.fs#L971
let cache (source: seq<'T>) =
    checkNonNull "source" source
    // Wrap a seq to ensure that it is enumerated just once and only as far as is necessary.
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
    let mutable enumeratorR = None

    let oneStepTo i =
        // If possible, step the enumeration to prefix length i (at most one step).
        // Be speculative, since this could have already happened via another thread.
        if i >= prefix.Count then // is a step still required?
            // If not yet started, start it (create enumerator).
            let optEnumerator =
                match enumeratorR with
                | None ->
                    let optEnumerator = Some(source.GetEnumerator())
                    enumeratorR <- Some optEnumerator
                    optEnumerator
                | Some optEnumerator -> optEnumerator

            match optEnumerator with
            | Some enumerator ->
                if enumerator.MoveNext() then
                    prefix.Add(enumerator.Current)
                else
                    enumerator.Dispose() // Move failed, dispose enumerator,
                    enumeratorR <- Some None // drop it and record finished.
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
                        Some(prefix.[i], i + 1)
                    else
                        oneStepTo i

                        if i < prefix.Count then
                            Some(prefix.[i], i + 1)
                        else
                            None
            )
            0

    let cleanup () =
        lock prefix
        <| fun () ->
            prefix.Clear()

            match enumeratorR with
            | Some(Some e) -> e.Dispose()
            | _ -> ()

            enumeratorR <- None

    (new CachedSeq<_>(cleanup, result) :> seq<_>)

let allPairs (xs: seq<'T1>) (ys: seq<'T2>) : seq<'T1 * 'T2> =
    let ysCache = cache ys

    delay (fun () ->
        let mapping (x: 'T1) = ysCache |> map (fun y -> (x, y))
        concat (map mapping xs)
    )

let mapFold (mapping: 'State -> 'T -> 'Result * 'State) state (xs: seq<'T>) =
    let arr, state = Array.mapFold mapping state (toArray xs)
    readOnly arr, state

let mapFoldBack
    (mapping: 'T -> 'State -> 'Result * 'State)
    (xs: seq<'T>)
    state
    =
    let arr, state = Array.mapFoldBack mapping (toArray xs) state
    readOnly arr, state

let tryPick chooser (xs: seq<'T>) =
    use e = ofSeq xs
    let mutable res = None

    while (Option.isNone res && e.MoveNext()) do
        res <- chooser e.Current

    res

let pick chooser (xs: seq<'T>) =
    match tryPick chooser xs with
    | Some x -> x
    | None -> indexNotFound ()

let reduce folder (xs: seq<'T>) =
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

let reduceBack folder (xs: seq<'T>) =
    let arr = toArray xs

    if arr.Length > 0 then
        Array.reduceBack folder arr
    else
        invalidOp SR.inputSequenceEmpty

let replicate n x = initialize n (fun _ -> x)

let reverse (xs: seq<'T>) =
    delay (fun () -> xs |> toArray |> Array.rev |> ofArray)

let scan folder (state: 'State) (xs: seq<'T>) =
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

let scanBack folder (xs: seq<'T>) (state: 'State) =
    delay (fun () ->
        let arr = toArray xs
        Array.scanBack folder arr state |> ofArray
    )

let skip count (source: seq<'T>) =
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

let skipWhile predicate (xs: seq<'T>) =
    delay (fun () ->
        let mutable skipped = true

        xs
        |> filter (fun x ->
            if skipped then
                skipped <- predicate x

            not skipped
        )
    )

let tail (xs: seq<'T>) = skip 1 xs

let take count (xs: seq<'T>) =
    generateIndexed
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

let takeWhile predicate (xs: seq<'T>) =
    generate
        (fun () -> ofSeq xs)
        (fun e ->
            if e.MoveNext() && predicate e.Current then
                Some(e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let truncate count (xs: seq<'T>) =
    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if i < count && e.MoveNext() then
                Some(e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let zip (xs: seq<'T1>) (ys: seq<'T2>) = map2 (fun x y -> (x, y)) xs ys

let zip3 (xs: seq<'T1>) (ys: seq<'T2>) (zs: seq<'T3>) =
    map3 (fun x y z -> (x, y, z)) xs ys zs

let collect<'T, 'Collection, 'U when 'Collection :> 'U seq>
    (mapping: 'T -> 'Collection)
    (xs: seq<'T>)
    : seq<'U>
    =
    delay (fun () -> xs |> map mapping |> concat)

let where predicate (xs: seq<'T>) = filter predicate xs

let pairwise (xs: seq<'T>) =
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

let sort
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>)
    =
    sortWith (fun x y -> comparer.Compare(x, y)) xs

let sortBy
    (projection: 'T -> 'U)
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>)
    =
    sortWith (fun x y -> comparer.Compare(projection x, projection y)) xs

let sortDescending
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>)
    =
    sortWith (fun x y -> comparer.Compare(x, y) * -1) xs

let sortByDescending
    (projection: 'T -> 'U)
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>)
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

let maxBy
    (projection: 'T -> 'U)
    xs
    ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>)
    : 'T
    =
    reduce
        (fun x y ->
            if comparer.Compare(projection y, projection x) > 0 then
                y
            else
                x
        )
        xs

let max
    xs
    ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>)
    : 'T
    =
    reduce
        (fun x y ->
            if comparer.Compare(y, x) > 0 then
                y
            else
                x
        )
        xs

let minBy
    (projection: 'T -> 'U)
    xs
    ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>)
    : 'T
    =
    reduce
        (fun x y ->
            if comparer.Compare(projection y, projection x) > 0 then
                x
            else
                y
        )
        xs

let min
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>)
    : 'T
    =
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

// let init = initialize
// let initInfinite = initializeInfinite
// let iter = iterate
// let iter2 = iterate2
// let iteri = iterateIndexed
// let iteri2 = iterateIndexed2
// let forall = forAll
// let forall2 = forAll2
// let mapi = mapIndexed
// let mapi2 = mapIndexed2
// let readonly = readOnly
// let rev = reverse

let insertAt (index: int) (y: 'T) (xs: seq<'T>) : seq<'T> =
    let mutable isDone = false

    if index < 0 then
        invalidArg "index" SR.indexOutOfBounds

    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if (isDone || i < index) && e.MoveNext() then
                Some e.Current
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

    generateIndexed
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

    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if (isDone || i < index) && e.MoveNext() then
                Some e.Current
            elif i = index && e.MoveNext() then
                isDone <- true

                if e.MoveNext() then
                    Some e.Current
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

    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if i < index then
                if e.MoveNext() then
                    Some e.Current
                else
                    invalidArg "index" SR.indexOutOfBounds
            else
                if i = index then
                    for _ = 1 to count do
                        if not (e.MoveNext()) then
                            invalidArg "count" SR.indexOutOfBounds

                if e.MoveNext() then
                    Some e.Current
                else
                    None
        )
        (fun e -> e.Dispose())

let updateAt (index: int) (y: 'T) (xs: seq<'T>) : seq<'T> =
    let mutable isDone = false

    if index < 0 then
        invalidArg "index" SR.indexOutOfBounds

    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if (isDone || i < index) && e.MoveNext() then
                Some e.Current
            elif i = index && e.MoveNext() then
                isDone <- true
                Some y
            else
                if not isDone then
                    invalidArg "index" SR.indexOutOfBounds

                None
        )
        (fun e -> e.Dispose())
