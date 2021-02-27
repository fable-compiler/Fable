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
    let inputSequenceTooLong = "The input sequence contains more than one element."
    let keyNotFoundAlt = "An index satisfying the predicate was not found in the collection."
    let notEnoughElements = "The input sequence has an insufficient number of elements."
    let resetNotSupported = "Reset is not supported on this enumerator."

module Enumerator =

    let noReset() = raise (new System.NotSupportedException(SR.resetNotSupported))
    let notStarted() = raise (new System.InvalidOperationException(SR.enumerationNotStarted))
    let alreadyFinished() = raise (new System.InvalidOperationException(SR.enumerationAlreadyFinished))

    type Enumerable<'T>(f) =
        interface IEnumerable<'T> with
            member x.GetEnumerator() = f()
        interface System.Collections.IEnumerable with
            member x.GetEnumerator() = f() :> System.Collections.IEnumerator
        override xs.ToString() =
            let maxCount = 4
            let mutable i = 0
            let mutable str = "seq ["
            use e = (xs :> IEnumerable<'T>).GetEnumerator()
            while (i < maxCount && e.MoveNext()) do
                if i > 0 then str <- str + "; "
                str <- str + (string e.Current)
                i <- i + 1
            if i = maxCount then
                str <- str + "; ..."
            str + "]"

    // type FromFunctions<'T>(get, next, reset, dispose) =
    //     interface IEnumerator<'T> with
    //         member __.Current = get()
    //     interface System.Collections.IEnumerator with
    //         member __.Current = box (get())
    //         member __.MoveNext() = next()
    //         member __.Reset() = reset()
    //     interface System.IDisposable with
    //         member __.Dispose() = dispose()

    // let inline fromFunctions get next reset dispose: IEnumerator<'T> =
    //     new FromFunctions<_>(get, next, reset, dispose) :> IEnumerator<'T>

    let fromFunctions get next reset dispose: IEnumerator<'T> =
        { new IEnumerator<'T> with
                member __.Current = get()
            interface System.Collections.IEnumerator with
                member __.Current = box (get())
                member __.MoveNext() = next()
                member __.Reset() = reset()
            interface System.IDisposable with
                member __.Dispose() = dispose() }

    // // for languages where arrays are not IEnumerable
    //
    // let ofArray (arr: 'T[]): IEnumerator<'T> =
    //     let len = arr.Length
    //     let mutable i = -1
    //     let get() =
    //         if i < 0 then notStarted()
    //         elif i >= len then alreadyFinished()
    //         else arr.[i]
    //     let next() =
    //         if i < len then
    //             i <- i + 1
    //             i < len
    //         else false
    //     let reset() = noReset() // i <- -1
    //     let dispose() = ()
    //     fromFunctions get next reset dispose
    //
    // let empty<'T>(): IEnumerator<'T> =
    //     let mutable started = false
    //     let get() = if not started then notStarted() else alreadyFinished()
    //     let next() = started <- true; false
    //     let reset() = noReset()
    //     let dispose() = ()
    //     fromFunctions get next reset dispose
    //
    // let singleton (x: 'T): IEnumerator<'T> =
    //     let mutable index = -1
    //     let get() =
    //         if index < 0 then notStarted()
    //         if index > 0 then alreadyFinished()
    //         x
    //     let next() = index <- index + 1; index = 0
    //     let reset() = noReset()
    //     let dispose() = ()
    //     fromFunctions get next reset dispose

    let cast (e: System.Collections.IEnumerator): IEnumerator<'T> =
        let get() = unbox<'T> e.Current
        let next() = e.MoveNext()
        let reset() = noReset()
        let dispose() =
            match e with
            | :? System.IDisposable as e -> e.Dispose()
            | _ -> ()
        fromFunctions get next reset dispose

    let choose (f: int -> 'T -> 'U option) (e: IEnumerator<'T>): IEnumerator<'U> =
        let mutable index = -1
        let mutable curr = None
        let get() =
            if index < 0 then notStarted()
            match curr with
            | None -> alreadyFinished()
            | Some x -> x
        let next() =
            curr <- None
            index <- index + 1
            while (Option.isNone curr && e.MoveNext()) do
                curr <- f index e.Current
                if Option.isNone curr then index <- index + 1
            let finished = Option.isNone curr
            not finished
        let reset() = noReset()
        let dispose() =
            try e.Dispose()
            finally ()
        fromFunctions get next reset dispose

    let choose2 (f: int -> 'T1 -> 'T2 -> 'U option) (e1: IEnumerator<'T1>) (e2: IEnumerator<'T2>): IEnumerator<'U> =
        let mutable index = -1
        let mutable curr = None
        let get() =
            if index < 0 then notStarted()
            match curr with
            | None -> alreadyFinished()
            | Some x -> x
        let next() =
            curr <- None
            index <- index + 1
            while (Option.isNone curr && e1.MoveNext() && e2.MoveNext()) do
                curr <- f index e1.Current e2.Current
                if Option.isNone curr then index <- index + 1
            let finished = Option.isNone curr
            not finished
        let reset() = noReset()
        let dispose() =
            try e1.Dispose()
            finally
                try e2.Dispose()
                finally ()
        fromFunctions get next reset dispose

    let choose3 (f: int -> 'T1 -> 'T2 -> 'T3 -> 'U option) (e1: IEnumerator<'T1>) (e2: IEnumerator<'T2>) (e3: IEnumerator<'T3>): IEnumerator<'U> =
        let mutable index = -1
        let mutable curr = None
        let get() =
            if index < 0 then notStarted()
            match curr with
            | None -> alreadyFinished()
            | Some x -> x
        let next() =
            curr <- None
            index <- index + 1
            while (Option.isNone curr && e1.MoveNext() && e2.MoveNext() && e3.MoveNext()) do
                curr <- f index e1.Current e2.Current e3.Current
                if Option.isNone curr then index <- index + 1
            let finished = Option.isNone curr
            not finished
        let reset() = noReset()
        let dispose() =
            try e1.Dispose()
            finally
                try e3.Dispose()
                finally
                    try e3.Dispose()
                    finally ()
        fromFunctions get next reset dispose

    let concat<'T,'U when 'U :> seq<'T>> (sources: seq<'U>) =
        let mutable outerOpt: IEnumerator<'U> option = None
        let mutable innerOpt: IEnumerator<'T> option = None
        let mutable started = false
        let mutable finished = false
        let mutable currElement = None
        let get() =
            if not started then notStarted()
            elif finished then alreadyFinished()
            match currElement with
            | None -> alreadyFinished()
            | Some x -> x
        let finish() =
            finished <- true
            try
                match innerOpt with
                | None -> ()
                | Some inner ->
                    try inner.Dispose()
                    finally innerOpt <- None
            finally
                match outerOpt with
                | None -> ()
                | Some outer ->
                    try outer.Dispose()
                    finally outerOpt <- None
        let loop () =
            let mutable res = None
            while Option.isNone res do
                match outerOpt, innerOpt with
                | None, _ ->
                    outerOpt <- Some (sources.GetEnumerator())
                | Some outer, None ->
                    if outer.MoveNext() then
                        let ie = outer.Current
                        innerOpt <- Some (ie.GetEnumerator())
                    else
                        finish()
                        res <- Some false
                | Some _, Some inner ->
                    if inner.MoveNext() then
                        currElement <- Some (inner.Current)
                        res <- Some true
                    else
                        try inner.Dispose()
                        finally innerOpt <- None
            res.Value
        let next() =
            if not started then started <- true
            if finished then false
            else loop ()
        let reset() = noReset()
        let dispose() = if not finished then finish()
        fromFunctions get next reset dispose

    let generateWhileSome (openf: unit -> 'T) (compute: 'T -> 'U option) (closef: 'T -> unit): IEnumerator<'U> =
        let mutable started = false
        let mutable curr = None
        let mutable state = Some (openf())
        let get() =
            if not started then notStarted()
            match curr with
            | None -> alreadyFinished()
            | Some x -> x
        let reset() = noReset()
        let dispose() =
            match state with
            | None -> ()
            | Some x ->
                try closef x
                finally state <- None
        let finish() =
            try dispose()
            finally curr <- None
        let next() =
            if not started then started <- true
            match state with
            | None -> false
            | Some s ->
                match (try compute s with _ -> finish(); reraise()) with
                | None -> finish(); false
                | Some _ as x -> curr <- x; true
        fromFunctions get next reset dispose

    let enumerateThenFinally f (e: IEnumerator<'T>): IEnumerator<'T> =
        let get() = e.Current
        let next() = e.MoveNext()
        let reset() = noReset()
        let dispose() = try e.Dispose() finally f()
        fromFunctions get next reset dispose

    let map (f: 'T -> 'U) (e: IEnumerator<'T>): IEnumerator<'U> =
        choose (fun _i x -> Some (f x)) e

    let mapi (f: int -> 'T -> 'U) (e: IEnumerator<'T>): IEnumerator<'U> =
        choose (fun i x -> Some (f i x)) e

    let map2 (f: 'T1 -> 'T2 -> 'U) (e1: IEnumerator<'T1>) (e2: IEnumerator<'T2>): IEnumerator<'U> =
        choose2 (fun _i x y -> Some (f x y)) e1 e2

    let mapi2 (f: int -> 'T1 -> 'T2 -> 'U) (e1: IEnumerator<'T1>) (e2: IEnumerator<'T2>): IEnumerator<'U> =
        choose2 (fun i x y -> Some (f i x y)) e1 e2

    let map3 (f: 'T1 -> 'T2 -> 'T3 -> 'U) (e1: IEnumerator<'T1>) (e2: IEnumerator<'T2>) (e3: IEnumerator<'T3>): IEnumerator<'U> =
        choose3 (fun _i x y z -> Some (f x y z)) e1 e2 e3

    let unfold (f: 'State -> ('T * 'State) option) (state: 'State): IEnumerator<'T> =
        let mutable curr: ('T * 'State) option = None
        let mutable acc: 'State = state
        let get() =
            match curr with
            | None -> notStarted()
            | Some (x, st) -> x
        let next() =
            curr <- f acc
            match curr with
            | None -> false
            | Some (x, st) ->
                acc <- st
                true
        let reset() = noReset()
        let dispose() = ()
        fromFunctions get next reset dispose

// module Seq =

let indexNotFound() = raise (new System.Collections.Generic.KeyNotFoundException(SR.keyNotFoundAlt))

let checkNonNull argName arg = if isNull arg then nullArg argName

let mkSeq (f: unit -> IEnumerator<'T>): seq<'T> =
    Enumerator.Enumerable(f) :> IEnumerable<'T>

let ofSeq (xs: seq<'T>): IEnumerator<'T> =
    checkNonNull "source" xs
    xs.GetEnumerator()

let delay (generator: unit -> seq<'T>) =
    mkSeq (fun () -> generator().GetEnumerator())

let concat (sources: seq<#seq<'T>>) =
    mkSeq (fun () -> Enumerator.concat sources)

let unfold (generator: 'State -> ('T * 'State) option) (state: 'State) =
    mkSeq (fun () -> Enumerator.unfold generator state)

let ofArray (arr: 'T[]) =
    arr :> seq<'T>

let toArray (xs: seq<'T>): 'T[] =
    match xs with
    | :? array<'T> as a -> a
    | :? list<'T> as a -> Array.ofList a
    | _ -> Array.ofSeq xs

let ofList (xs: 'T list) =
    (xs :> seq<'T>)

let toList (xs: seq<'T>): 'T list =
    match xs with
    | :? array<'T> as a -> List.ofArray a
    | :? list<'T> as a -> a
    | _ -> List.ofSeq xs

let empty () =
    delay (fun () -> Array.empty :> seq<'T>) // Enumerator.empty<_>())

let singleton x =
    delay (fun () -> (Array.singleton x) :> seq<'T>) // Enumerator.singleton x)

let append (xs: seq<'T>) (ys: seq<'T>) =
    concat [| xs; ys |]

let cast (xs: System.Collections.IEnumerable) =
    mkSeq (fun () ->
        checkNonNull "source" xs
        xs.GetEnumerator()
        |> Enumerator.cast
    )

let choose chooser (xs: seq<'T>) =
    mkSeq (fun () ->
        ofSeq xs
        |> Enumerator.choose (fun _i x -> chooser x)
    )

let compareWith (comparer: 'T -> 'T -> int) (xs: seq<'T>) (ys: seq<'T>): int =
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
    if c <> 0 then c
    elif b1 then 1
    elif b2 then -1
    else 0

let contains (value: 'T) (xs: seq<'T>) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
    use e = ofSeq xs
    let mutable found = false
    while (not found && e.MoveNext()) do
        found <- eq.Equals(value, e.Current)
    found

let inline Generate openf compute closef =
    mkSeq (fun () -> Enumerator.generateWhileSome openf compute closef)

let inline GenerateUsing (openf: unit -> ('U :> System.IDisposable)) compute =
    Generate openf compute (fun (s: 'U) -> s.Dispose())

let inline FinallyEnumerable<'T> (compensation: unit -> unit, restf: unit -> seq<'T>) =
    mkSeq (fun () ->
        try
            let e = restf() |> ofSeq
            Enumerator.enumerateThenFinally compensation e
        with _ ->
            compensation()
            reraise()
    )

let enumerateFromFunctions create moveNext current =
    Generate
        create
        (fun x -> if moveNext x then Some(current x) else None)
        (fun x -> match box(x) with :? System.IDisposable as id -> id.Dispose() | _ -> ())

let enumerateThenFinally (source: seq<'T>) (compensation: unit -> unit) =
    FinallyEnumerable(compensation, (fun () -> source))

let enumerateUsing (resource: 'T :> System.IDisposable) (source: 'T -> #seq<'U>) =
    FinallyEnumerable(
        (fun () -> match box resource with null -> () | _ -> resource.Dispose()),
        (fun () -> source resource :> seq<_>))

let enumerateWhile (guard: unit -> bool) (xs: seq<'T>) =
    concat (unfold (fun i -> if guard() then Some(xs, i + 1) else None) 0)

let except (itemsToExclude: seq<'T>) (xs: seq<'T>) ([<Inject>] eq: System.Collections.Generic.IEqualityComparer<'T>) =
    mkSeq (fun () ->
        let hashSet = System.Collections.Generic.HashSet(itemsToExclude, eq) //, HashIdentity.Structural)
        Enumerator.choose (fun _i x -> if hashSet.Add(x) then Some x else None) (ofSeq xs)
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
        if e.MoveNext()
        then invalidArg "source" SR.inputSequenceTooLong
        else v
    else
        invalidArg "source" SR.inputSequenceEmpty

let tryExactlyOne (xs: seq<'T>) =
    use e = ofSeq xs
    if e.MoveNext() then
        let v = e.Current
        if e.MoveNext()
        then None
        else Some v
    else
        None

let filter f (xs: seq<'T>) =
    xs |> choose (fun x -> if f x then Some x else None)

let tryFind predicate (xs: seq<'T>)  =
    use e = ofSeq xs
    let mutable res = None
    while (Option.isNone res && e.MoveNext()) do
        let c = e.Current
        if predicate c then res <- Some c
    res

let find predicate (xs: seq<'T>) =
    match tryFind predicate xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFindBack predicate (xs: seq<'T>) =
    xs
    |> toArray
    |> Array.tryFindBack predicate

let findBack predicate (xs: seq<'T>) =
    match tryFindBack predicate xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFindIndex predicate (xs: seq<'T>) =
    use e = ofSeq xs
    let rec loop i =
        if e.MoveNext() then
            if predicate e.Current then Some i
            else loop (i + 1)
        else
            None
    loop 0

let findIndex predicate (xs: seq<'T>) =
    match tryFindIndex predicate xs with
    | Some x -> x
    | None -> indexNotFound()

let tryFindIndexBack predicate (xs: seq<'T>) =
    xs
    |> toArray
    |> Array.tryFindIndexBack predicate

let findIndexBack predicate (xs: seq<'T>) =
    match tryFindIndexBack predicate xs with
    | Some x -> x
    | None -> indexNotFound()

let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: seq<'T>) =
    use e = ofSeq xs
    let mutable acc = state
    while e.MoveNext() do
        acc <- folder acc e.Current
    acc

let foldBack folder (xs: seq<'T>) state =
    Array.foldBack folder (toArray xs) state

let fold2 (folder: 'State -> 'T1 -> 'T2 -> 'State) (state: 'State) (xs: seq<'T1>) (ys: seq<'T2>) =
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable acc = state
    while e1.MoveNext() && e2.MoveNext() do
        acc <- folder acc e1.Current e2.Current
    acc

let foldBack2 (folder: 'T1 -> 'T2 -> 'State -> 'State) (xs: seq<'T1>) (ys: seq<'T2>) (state: 'State) =
    Array.foldBack2 folder (toArray xs) (toArray ys) state

let forAll predicate xs =
    // use e = ofSeq xs
    // let mutable ok = true
    // while (ok && e.MoveNext()) do
    //     ok <- predicate e.Current
    // ok
    not (exists (fun x -> not (predicate x)) xs)

let forAll2 predicate xs ys =
    // use e1 = ofSeq xs
    // use e2 = ofSeq ys
    // let mutable ok = true
    // while (ok && e1.MoveNext() && e2.MoveNext()) do
    //     ok <- predicate e1.Current e2.Current
    // ok
    not (exists2 (fun x y -> not (predicate x y)) xs ys)

let tryHead (xs: seq<'T>) =
    match xs with
    | :? array<'T> as a -> Array.tryHead a
    | :? list<'T> as a -> List.tryHead a
    | _ ->
        use e = ofSeq xs
        if e.MoveNext()
        then Some (e.Current)
        else None

let head (xs: seq<'T>) =
    match tryHead xs with
    | Some x -> x
    | None -> invalidArg "source" SR.inputSequenceEmpty

let indexed (xs: seq<'T>) =
    mkSeq (fun () ->
        ofSeq xs
        |> Enumerator.mapi (fun i x -> (i, x))
    )

let initialize count f =
    unfold (fun i -> if (i < count) then Some(f i, i + 1) else None) 0

let initializeInfinite f =
    initialize (System.Int32.MaxValue) f

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
            if not (e.MoveNext()) then None
            elif index = 0 then Some e.Current
            else loop (index - 1)
        loop index

let item index (xs: seq<'T>) =
    match tryItem index xs with
    | Some x -> x
    | None -> invalidArg "index" SR.notEnoughElements

let iterate action xs =
    fold (fun () x -> action x) () xs

let iterate2 action xs ys =
    fold2 (fun () x y -> action x y) () xs ys

let iterateIndexed action xs =
    fold (fun i x -> action i x; i + 1) 0 xs |> ignore

let iterateIndexed2 action xs ys =
    fold2 (fun i x y -> action i x y; i + 1) 0 xs ys |> ignore

let tryLast (xs: seq<'T>) =
    // if isEmpty xs then None
    // else Some (reduce (fun _ x -> x) xs)
    use e = ofSeq xs
    let rec loop acc =
        if not (e.MoveNext()) then acc
        else loop e.Current
    if e.MoveNext()
    then Some (loop e.Current)
    else None

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
    mkSeq (fun () ->
        ofSeq xs
        |> Enumerator.map mapping
    )

let mapIndexed (mapping: int -> 'T -> 'U) (xs: seq<'T>) =
    mkSeq (fun () ->
        ofSeq xs
        |> Enumerator.mapi mapping
    )

let map2 (mapping: 'T1 -> 'T2 -> 'U) (xs: seq<'T1>) (ys: seq<'T2>) =
    mkSeq (fun () ->
        (ofSeq xs, ofSeq ys)
        ||> Enumerator.map2 mapping
    )

let mapIndexed2 (mapping: int -> 'T1 -> 'T2 -> 'U) (xs: seq<'T1>) (ys: seq<'T2>) =
    mkSeq (fun () ->
        (ofSeq xs, ofSeq ys)
        ||> Enumerator.mapi2 (fun i x y -> Some (mapping i x y))
    )

let map3 (mapping: 'T1 -> 'T2 -> 'T3 -> 'U) (xs: seq<'T1>) (ys: seq<'T2>) (zs: seq<'T3>) =
    mkSeq (fun () ->
        (ofSeq xs, ofSeq ys, ofSeq zs)
        |||> Enumerator.map3 mapping
    )

let readOnly (xs: seq<'T>) =
    checkNonNull "source" xs
    map id xs

let mapFold (mapping: 'State -> 'T -> 'Result * 'State) state (xs: seq<'T>) =
    let arr, state = Array.mapFold mapping state (toArray xs)
    readOnly arr, state

let mapFoldBack (mapping: 'T -> 'State -> 'Result * 'State) (xs: seq<'T>) state =
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
    | None -> indexNotFound()

let reduce folder (xs: seq<'T>) =
    use e = ofSeq xs
    let rec loop acc =
        if e.MoveNext()
        then loop (folder acc e.Current)
        else acc
    if e.MoveNext()
    then loop e.Current
    else invalidOp SR.inputSequenceEmpty

let reduceBack folder (xs: seq<'T>) =
    let arr = toArray xs
    if arr.Length > 0
    then Array.reduceBack folder arr
    else invalidOp SR.inputSequenceEmpty

let replicate n x =
    initialize n (fun _ -> x)

let reverse (xs: seq<'T>) =
    delay (fun () ->
        xs
        |> toArray
        |> Array.rev
        |> ofArray
    )

let scan folder (state: 'State) (xs: seq<'T>) =
    delay (fun () ->
        let first = singleton state
        let mutable acc = state
        let rest = xs |> map (fun x -> acc <- folder acc x; acc)
        [| first; rest |] |> concat
    )

let scanBack folder (xs: seq<'T>) (state: 'State) =
    delay (fun () ->
        let arr = toArray xs
        Array.scanBack folder arr state
        |> ofArray
    )

let skip count (xs: seq<'T>) =
    mkSeq (fun () ->
        let e = ofSeq xs
        for i = 1 to count do
            if not (e.MoveNext()) then
                invalidArg "source" SR.notEnoughElements
        e
    )

let skipWhile predicate (xs: seq<'T>) =
    delay (fun () ->
        let mutable skipped = true
        xs |> filter (fun x ->
            if skipped then
                skipped <- predicate x
            not skipped)
    )

let tail (xs: seq<'T>) =
    skip 1 xs

let take count (xs: seq<'T>) =
    mkSeq (fun () ->
        let mutable i = 0
        ofSeq xs
        |> Enumerator.unfold (fun e ->
            i <- i + 1
            if i <= count then
                if e.MoveNext()
                then Some (e.Current, e)
                else invalidArg "source" SR.notEnoughElements
            else None)
    )

let takeWhile predicate (xs: seq<'T>) =
    mkSeq (fun () ->
        ofSeq xs
        |> Enumerator.unfold (fun e ->
            if e.MoveNext() && predicate e.Current
            then Some (e.Current, e)
            else None)
    )

let truncate count (xs: seq<'T>) =
    mkSeq (fun () ->
        let mutable i = 0
        ofSeq xs
        |> Enumerator.unfold (fun e ->
            i <- i + 1
            if i <= count && e.MoveNext()
            then Some (e.Current, e)
            else None)
    )

let zip (xs: seq<'T1>) (ys: seq<'T2>) =
    mkSeq (fun () ->
        (ofSeq xs, ofSeq ys)
        ||> Enumerator.map2 (fun x y -> (x, y))
    )

let zip3 (xs: seq<'T1>) (ys: seq<'T2>) (zs: seq<'T3>) =
    mkSeq (fun () ->
        (ofSeq xs, ofSeq ys, ofSeq zs)
        |||> Enumerator.map3 (fun x y z -> (x, y, z))
    )

let collect (mapping: 'T -> 'U seq) (xs: seq<'T>) =
    delay (fun () ->
        xs
        |> map mapping
        |> concat
    )

let groupBy (projection: 'T -> 'Key) (xs: seq<'T>) =
    delay (fun () ->
        xs
        |> toArray
        |> Array.groupBy projection
        |> ofArray
    )

let countBy (projection: 'T -> 'Key) (xs: seq<'T>) =
    delay (fun () ->
        xs
        |> toArray
        |> Array.countBy projection
        |> ofArray
    )

let where predicate (xs: seq<'T>) =
    filter predicate xs

let pairwise (xs: seq<'T>) =
    delay (fun () ->
        xs
        |> toArray
        |> Array.pairwise
        |> ofArray
    )

let splitInto (chunks: int) (xs: seq<'T>): 'T seq seq =
    delay (fun () ->
        xs
        |> toArray
        |> Array.splitInto chunks
        |> Array.map ofArray
        |> ofArray
    )

let windowed windowSize (xs: seq<'T>): 'T seq seq =
    delay (fun () ->
        xs
        |> toArray
        |> Array.windowed windowSize
        |> Array.map ofArray
        |> ofArray
    )

let transpose (xss: seq<#seq<'T>>) =
    delay (fun () ->
        xss
        |> toArray
        |> Array.map toArray
        |> Array.transpose
        |> Array.map ofArray
        |> ofArray
    )

let distinct (xs: seq<'T>) =
    delay (fun () ->
        let hashSet = System.Collections.Generic.HashSet(HashIdentity.Structural)
        xs |> choose (fun x -> if hashSet.Add(x) then Some x else None)
    )

let distinctBy (projection: 'T -> 'Key) (xs: seq<'T>) =
    delay (fun () ->
        let hashSet = System.Collections.Generic.HashSet(HashIdentity.Structural)
        xs |> choose (fun x -> if hashSet.Add(projection x) then Some x else None)
    )

let sortWith (comparer: 'T -> 'T -> int) (xs: seq<'T>) =
    delay (fun () ->
        let arr = toArray xs
        Array.sortInPlaceWith comparer arr // TODO: use stable sort (in JS it already is)
        arr |> ofArray
    )

let sort (xs: seq<'T>) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>) =
    sortWith (fun x y -> comparer.Compare(x, y)) xs

let sortBy (projection: 'T -> 'U) (xs: seq<'T>) ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>) =
    sortWith (fun x y -> comparer.Compare(projection x, projection y)) xs

let sortDescending (xs: seq<'T>) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>) =
    sortWith (fun x y -> comparer.Compare(x, y) * -1) xs

let sortByDescending (projection: 'T -> 'U) (xs: seq<'T>) ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>) =
    sortWith (fun x y -> comparer.Compare(projection x, projection y) * -1) xs

let sum (xs: seq<'T>) ([<Inject>] adder: IGenericAdder<'T>): 'T =
    fold (fun acc x -> adder.Add(acc, x)) (adder.GetZero()) xs

let sumBy (f: 'T -> 'U) (xs: seq<'T>) ([<Inject>] adder: IGenericAdder<'U>): 'U =
    fold (fun acc x -> adder.Add(acc, f x)) (adder.GetZero()) xs

let maxBy (projection: 'T -> 'U) xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>): 'T =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then y else x) xs

let max xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then y else x) xs

let minBy (projection: 'T -> 'U) xs ([<Inject>] comparer: System.Collections.Generic.IComparer<'U>): 'T =
    reduce (fun x y -> if comparer.Compare(projection y, projection x) > 0 then x else y) xs

let min (xs: seq<'T>) ([<Inject>] comparer: System.Collections.Generic.IComparer<'T>): 'T =
    reduce (fun x y -> if comparer.Compare(y, x) > 0 then x else y) xs

let average (xs: seq<'T>) ([<Inject>] averager: IGenericAverager<'T>): 'T =
    let mutable count = 0
    let folder acc x = count <- count + 1; averager.Add(acc, x)
    let total = fold folder (averager.GetZero()) xs
    averager.DivideByInt(total, count)

let averageBy (f: 'T -> 'U) (xs: seq<'T>) ([<Inject>] averager: IGenericAverager<'U>): 'U =
    let mutable count = 0
    let inline folder acc x = count <- count + 1; averager.Add(acc, f x)
    let total = fold folder (averager.GetZero()) xs
    averager.DivideByInt(total, count)

let permute f (xs: seq<'T>) =
    delay (fun () ->
        xs
        |> toArray
        |> Array.permute f
        |> ofArray
    )

let chunkBySize (chunkSize: int) (xs: seq<'T>): seq<seq<'T>> =
    delay (fun () ->
        xs
        |> toArray
        |> Array.chunkBySize chunkSize
        |> Array.map ofArray
        |> ofArray
    )

let cache (xs: seq<'T>) =
    let mutable cached = false
    let xsCache = ResizeArray()
    delay (fun () ->
        if not cached then
            cached <- true
            xs |> map (fun x -> xsCache.Add(x); x)
        else
            xsCache :> seq<'T>
    )

let allPairs (xs: seq<'T1>) (ys: seq<'T2>): seq<'T1 * 'T2> =
    let ysCache = cache ys
    delay (fun () ->
        let mapping x = ysCache |> map (fun y -> (x, y))
        concat (map mapping xs)
    )

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
