// Adapted from:
// https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/ComputationExpressions/Enumerator.fs
// https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/seq.fs
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module Seq_

open Global_
open Interfaces_
//open System.Collections.Generic

type IEnumerable<'T> = System.Collections.Generic.IEnumerable<'T>
type IEnumerator<'T> = System.Collections.Generic.IEnumerator<'T>

type 'T seq = IEnumerable<'T>

let inline indexNotFound () = failwith SR.keyNotFoundAlt

module Enumerable =

    let inline noReset () = failwith SR.resetNotSupported
    let inline notStarted () = failwith SR.enumerationNotStarted
    let inline alreadyFinished () = failwith SR.enumerationAlreadyFinished

    // [<Sealed>]
    [<CompiledName("Seq")>]
    type Enumerable<'T>(f) =
        interface IEnumerable<'T> with
            member _.GetEnumerator() = f ()
    // interface System.Collections.IEnumerable with
    //     member _.GetEnumerator() = f() :> System.Collections.IEnumerator
    // override xs.ToString() =
    //     let maxCount = 4
    //     let mutable i = 0
    //     let mutable str = "seq ["
    //     use e: IEnumerator<'T> = f() // (xs :> IEnumerable<'T>).GetEnumerator()
    //     while (i < maxCount && e.MoveNext()) do
    //         if i > 0 then str <- str + "; "
    //         str <- str + (string e.Current)
    //         i <- i + 1
    //     if i = maxCount then
    //         str <- str + "; ..."
    //     str + "]"

    [<CompiledName("Enumerator")>]
    type FromFunctions<'T>(next: unit -> 'T option, dispose: unit -> unit) =
        let mutable curr: 'T option = None

        interface IEnumerator<'T> with
            member _.Current = curr.Value

            member _.MoveNext() =
                curr <- next ()
                curr.IsSome

            member _.Reset() = ()
            member _.Dispose() = dispose ()
    // interface System.Collections.IEnumerator with
    //     member _.Current =
    //         curr.Value
    //     member _.MoveNext() =
    //         curr <- next()
    //         curr.IsSome
    //     member _.Reset() = ()
    //     member _.Dispose() = dispose()
    // interface System.IDisposable with
    //     member _.Dispose() = dispose()

    let fromFunction next : IEnumerator<'T> =
        let dispose () = ()
        new FromFunctions<'T>(next, dispose) :> IEnumerator<'T>

    let fromFunctions next dispose : IEnumerator<'T> =
        new FromFunctions<'T>(next, dispose) :> IEnumerator<'T>

    let empty<'T> () : IEnumerator<'T> =
        let next () : 'T option = None
        fromFunction next

    let singleton (x: 'T) : IEnumerator<'T> =
        let mutable i = 0

        let next () =
            if i < 1 then
                i <- i + 1
                Some x
            else
                None

        fromFunction next

    let ofArray (arr: 'T[]) : IEnumerator<'T> =
        let len = arr.Length
        let mutable i = 0

        let next () =
            if i < len then
                i <- i + 1
                Some(arr[i - 1])
            else
                None

        fromFunction next

    let ofList (xs: 'T list) : IEnumerator<'T> =
        let mutable it = xs

        let next () =
            match it with
            | head :: tail ->
                it <- tail
                Some head
            | _ -> None

        fromFunction next

    let append (xs: 'T seq) (ys: 'T seq) : IEnumerator<'T> =
        let mutable i = -1
        let mutable innerOpt: IEnumerator<'T> option = None
        let mutable finished = false

        let next () =
            let mutable moveNext = false

            while not finished && not moveNext do
                match innerOpt with
                | Some inner ->
                    if inner.MoveNext() then
                        moveNext <- true
                    else
                        innerOpt <- None
                | None ->
                    if i < 1 then
                        i <- i + 1

                        let it =
                            if i = 0 then
                                xs
                            else
                                ys

                        innerOpt <- Some(it.GetEnumerator())
                    else
                        finished <- true

            if not finished && moveNext then
                Some(innerOpt.Value.Current)
            else
                None

        fromFunction next

    let concat (sources: 'T seq seq) : IEnumerator<'T> =
        let mutable outerOpt: IEnumerator<'T seq> option = None
        let mutable innerOpt: IEnumerator<'T> option = None
        let mutable finished = false

        let next () =
            let mutable moveNext = false

            while not finished && not moveNext do
                match outerOpt with
                | Some outer ->
                    match innerOpt with
                    | Some inner ->
                        if inner.MoveNext() then
                            moveNext <- true
                        else
                            innerOpt <- None
                    | None ->
                        if outer.MoveNext() then
                            let it = outer.Current
                            innerOpt <- Some(it.GetEnumerator())
                        else
                            finished <- true
                | None -> outerOpt <- Some(sources.GetEnumerator())

            if not finished && moveNext then
                Some(innerOpt.Value.Current)
            else
                None

        fromFunction next

    let enumerateThenFinally f (e: IEnumerator<'T>) : IEnumerator<'T> =
        let next () =
            if e.MoveNext() then
                Some(e.Current)
            else
                None

        let dispose () =
            try
                e.Dispose()
            finally
                f ()

        fromFunctions next dispose

    let generateWhileSome
        (openf: unit -> 'T)
        (compute: 'T -> 'U option)
        (closef: 'T -> unit)
        : IEnumerator<'U>
        =
        let mutable finished = false
        let mutable state = None

        let dispose () =
            match state with
            | None -> ()
            | Some x ->
                try
                    closef x
                finally
                    state <- None

        let next () =
            if finished then
                None
            else
                if Option.isNone state then
                    state <- Some(openf ())

                let res = compute state.Value

                if Option.isNone res then
                    finished <- true

                res

        fromFunctions next dispose

    let unfold
        (f: 'State -> ('T * 'State) option)
        (state: 'State)
        : IEnumerator<'T>
        =
        let mutable acc: 'State = state

        let next () =
            match f acc with
            | Some(x, st) ->
                acc <- st
                Some x
            | None -> None

        fromFunction next

(*
    // Alternative implementation

    [<CompiledName("Enumerator")>]
    type FromFunctions<'T>(current, movenext, dispose) =
        interface IEnumerator<'T> with
            member _.Current = current()
        // interface System.Collections.IEnumerator with
        //     member _.Current = box (current())
            member _.MoveNext() = movenext()
        //     member _.Reset() = noReset()
        // interface System.IDisposable with
            member _.Dispose() = dispose()

    let fromFunctions current movenext dispose: IEnumerator<'T> =
        new FromFunctions<'T>(current, movenext, dispose) :> IEnumerator<'T>

    // implementation for languages where arrays are not IEnumerable

    let empty<'T>(): IEnumerator<'T> =
        let mutable started = false
        let current() = if not started then notStarted() else alreadyFinished()
        let movenext() = started <- true; false
        let dispose() = ()
        fromFunctions current movenext dispose

    let singleton (x: 'T): IEnumerator<'T> =
        let len = 1
        let mutable i = -1
        let current() =
            if i < 0 then notStarted()
            elif i >= len then alreadyFinished()
            else x
        let movenext() =
            if i < len then
                i <- i + 1
                i < len
            else false
        let dispose() = ()
        fromFunctions current movenext dispose

    let ofArray (arr: 'T[]): IEnumerator<'T> =
        let len = arr.Length
        let mutable i = -1
        let current() =
            if i < 0 then notStarted()
            elif i >= len then alreadyFinished()
            else arr[i]
        let movenext() =
            if i < len then
                i <- i + 1
                i < len
            else false
        let dispose() = ()
        fromFunctions current movenext dispose

    let ofList (xs: 'T list): IEnumerator<'T> =
        let mutable it = xs
        let mutable started = false
        let current() =
            if not started then notStarted()
            elif List.isEmpty it then alreadyFinished()
            else List.head it
        let movenext() =
            if List.isEmpty it then
                started <- true
                false
            elif not started then
                started <- true
                true
            else
                it <- List.tail it
                true
        let disposable() = ()

    let cast (e: System.Collections.IEnumerator): IEnumerator<'T> =
        let current() = unbox<'T> e.Current
        let movenext() = e.MoveNext()
        let dispose() =
            match e with
            // | :? System.IDisposable as e -> e.Dispose()
            | _ -> ()
        fromFunctions current movenext dispose

    let concat<'T,'U when 'U :> 'T seq> (sources: 'U seq) =
        let mutable outerOpt: IEnumerator<'U> option = None
        let mutable innerOpt: IEnumerator<'T> option = None
        let mutable started = false
        let mutable finished = false
        let mutable curr = None
        let current() =
            if not started then notStarted()
            elif finished then alreadyFinished()
            match curr with
            | None -> alreadyFinished()
            | Some x -> x
        let finish() =
            finished <- true
            match innerOpt with
            | None -> ()
            | Some inner ->
                try inner.Dispose()
                finally innerOpt <- None
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
                        curr <- Some (inner.Current)
                        res <- Some true
                    else
                        try inner.Dispose()
                        finally innerOpt <- None
            res.Value
        let movenext() =
            started <- true
            if finished then false
            else loop ()
        let dispose() = if not finished then finish()
        fromFunctions current movenext dispose

    let enumerateThenFinally f (e: IEnumerator<'T>): IEnumerator<'T> =
        let current() = e.Current
        let movenext() = e.MoveNext()
        let dispose() = try e.Dispose() finally f()
        fromFunctions current movenext dispose

    let generateWhileSome (openf: unit -> 'T) (compute: 'T -> 'U option) (closef: 'T -> unit): IEnumerator<'U> =
        let mutable started = false
        let mutable curr = None
        let mutable state = Some (openf())
        let current() =
            if not started then notStarted()
            match curr with
            | None -> alreadyFinished()
            | Some x -> x
        let dispose() =
            match state with
            | None -> ()
            | Some x ->
                try closef x
                finally state <- None
        let finish() =
            try dispose()
            finally curr <- None
        let movenext() =
            if not started then started <- true
            match state with
            | None -> false
            | Some s ->
                match (try compute s with _ -> finish(); reraise()) with
                | None -> finish(); false
                | Some _ as x -> curr <- x; true
        fromFunctions current movenext dispose

    let unfold (f: 'State -> ('T * 'State) option) (state: 'State): IEnumerator<'T> =
        let mutable curr: ('T * 'State) option = None
        let mutable acc: 'State = state
        let current() =
            match curr with
            | None -> notStarted()
            | Some (x, st) -> x
        let movenext() =
            curr <- f acc
            match curr with
            | None -> false
            | Some (x, st) ->
                acc <- st
                true
        let dispose() = ()
        fromFunctions current movenext dispose
*)

// [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
// [<RequireQualifiedAccess>]
// module Seq =

// let checkNonNull argName arg = () //if isNull arg then nullArg argName

let mkSeq (f: unit -> IEnumerator<'T>) : 'T seq =
    Enumerable.Enumerable(f) :> 'T seq

let ofSeq (xs: 'T seq) : IEnumerator<'T> =
    // checkNonNull "source" xs
    xs.GetEnumerator()

let delay (generator: unit -> 'T seq) =
    mkSeq (fun () -> generator().GetEnumerator())

let concat (sources: 'T seq seq) =
    mkSeq (fun () -> Enumerable.concat sources)

let unfold (generator: 'State -> ('T * 'State) option) (state: 'State) =
    mkSeq (fun () -> Enumerable.unfold generator state)

let empty () =
    // delay (fun () -> Array.empty :> 'T seq)
    mkSeq (fun () -> Enumerable.empty ())

let singleton (x: 'T) =
    // delay (fun () -> (Array.singleton x) :> 'T seq)
    mkSeq (fun () -> Enumerable.singleton x)

let ofArray (arr: 'T[]) =
    // arr :> 'T seq
    mkSeq (fun () -> Enumerable.ofArray arr)

let ofList (xs: 'T list) =
    // xs :> 'T seq
    mkSeq (fun () -> Enumerable.ofList xs)

let generate create compute dispose =
    mkSeq (fun () -> Enumerable.generateWhileSome create compute dispose)

let generateIndexed create compute dispose =
    mkSeq (fun () ->
        let mutable i = -1

        Enumerable.generateWhileSome
            create
            (fun x ->
                i <- i + 1
                compute i x
            )
            dispose
    )

// // let inline generateUsing (openf: unit -> ('U :> System.IDisposable)) compute =
// //     generate openf compute (fun (s: 'U) -> s.Dispose())

let append (xs: 'T seq) (ys: 'T seq) =
    // concat [| xs; ys |]
    mkSeq (fun () -> Enumerable.append xs ys)

// let cast (xs: System.Collections.IEnumerable) =
//     mkSeq (fun () ->
//         checkNonNull "source" xs
//         xs.GetEnumerator()
//         |> Enumerable.cast
//     )

let choose (chooser: 'T -> 'U option) (xs: 'T seq) =
    generate
        (fun () -> ofSeq xs)
        (fun e ->
            let mutable curr = None

            while (Option.isNone curr && e.MoveNext()) do
                curr <- chooser e.Current

            curr
        )
        (fun e -> e.Dispose())

let compareWith (comparer: 'T -> 'T -> int) (xs: 'T seq) (ys: 'T seq) : int =
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

let compareTo (xs: 'T seq) (ys: 'T seq) =
    // LanguagePrimitives.GenericComparison xs ys
    compareWith compare xs ys

let equals (xs: 'T seq) (ys: 'T seq) =
    // LanguagePrimitives.GenericEquality xs ys
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable res = true
    let mutable b1 = e1.MoveNext()
    let mutable b2 = e2.MoveNext()

    while res && b1 && b2 do
        res <- e1.Current = e2.Current

        if res then
            b1 <- e1.MoveNext()
            b2 <- e2.MoveNext()

    res

// let enumerateFromFunctions create moveNext current =
//     generate
//         create
//         (fun x -> if moveNext x then Some(current x) else None)
//         (fun x -> match box(x) with :? System.IDisposable as id -> id.Dispose() | _ -> ())

let finallyEnumerable<'T> (compensation: unit -> unit, restf: unit -> 'T seq) =
    mkSeq (fun () ->
        try
            let e = restf () |> ofSeq
            Enumerable.enumerateThenFinally compensation e
        with ex ->
            compensation ()
            // reraise()
            failwith ex.Message
    )

let enumerateThenFinally (source: 'T seq) (compensation: unit -> unit) =
    finallyEnumerable (compensation, (fun () -> source))

let enumerateUsing
    (resource: 'T :> System.IDisposable)
    (sourceGen: 'T -> 'U seq)
    =
    finallyEnumerable (
        (fun () -> resource.Dispose()),
        (fun () -> sourceGen resource :> seq<_>)
    )

let enumerateWhile (guard: unit -> bool) (xs: 'T seq) =
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

let exactlyOne (xs: 'T seq) =
    use e = ofSeq xs

    if e.MoveNext() then
        let v = e.Current

        if e.MoveNext() then
            invalidArg "source" SR.inputSequenceTooLong
        else
            v
    else
        invalidArg "source" SR.inputSequenceEmpty

let tryExactlyOne (xs: 'T seq) =
    use e = ofSeq xs

    if e.MoveNext() then
        let v = e.Current

        if e.MoveNext() then
            None
        else
            Some v
    else
        None

let exists predicate (xs: 'T seq) =
    use e = ofSeq xs
    let mutable found = false

    while (not found && e.MoveNext()) do
        found <- predicate e.Current

    found

let exists2 (predicate: 'T1 -> 'T2 -> bool) (xs: 'T1 seq) (ys: 'T2 seq) =
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable found = false

    while (not found && e1.MoveNext() && e2.MoveNext()) do
        found <- predicate e1.Current e2.Current

    found

let contains (value: 'T) (xs: 'T seq) = xs |> exists (fun x -> x = value)

let filter f (xs: 'T seq) =
    xs
    |> choose (fun x ->
        if f x then
            Some x
        else
            None
    )

let tryFind predicate (xs: 'T seq) =
    use e = ofSeq xs
    let mutable res = None

    while (Option.isNone res && e.MoveNext()) do
        let c = e.Current

        if predicate c then
            res <- Some c

    res

let find predicate (xs: 'T seq) =
    match tryFind predicate xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryFindIndex predicate (xs: 'T seq) =
    let rec inner_loop i predicate (e: IEnumerator<'T>) =
        if e.MoveNext() then
            if predicate e.Current then
                Some i
            else
                inner_loop (i + 1) predicate e
        else
            None

    use e = ofSeq xs
    inner_loop 0 predicate e

let findIndex predicate (xs: 'T seq) =
    match tryFindIndex predicate xs with
    | Some x -> x
    | None -> indexNotFound ()

let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T seq) =
    let mutable acc = state

    for x in xs do
        acc <- folder acc x

    acc

// Redirected from Array.ofSeq (see Replacements)
let toArray (xs: 'T seq) : 'T[] =
    let res = ResizeArray<_>()

    for x in xs do
        res.Add(x)

    res |> asArray

// Redirected to List.ofSeq (see Replacements)
// let toList (xs: 'T seq): 'T list =
//     use e = ofSeq xs
//     List.unfold (fun (e: IEnumerator<'T>) ->
//         if e.MoveNext()
//         then Some(e.Current, e)
//         else None) e

let foldBack folder (xs: 'T seq) state =
    Array.foldBack folder (toArray xs) state

let fold2
    (folder: 'State -> 'T1 -> 'T2 -> 'State)
    (state: 'State)
    (xs: 'T1 seq)
    (ys: 'T2 seq)
    =
    use e1 = ofSeq xs
    use e2 = ofSeq ys
    let mutable acc = state

    while e1.MoveNext() && e2.MoveNext() do
        acc <- folder acc e1.Current e2.Current

    acc

let foldBack2
    (folder: 'T1 -> 'T2 -> 'State -> 'State)
    (xs: 'T1 seq)
    (ys: 'T2 seq)
    (state: 'State)
    =
    Array.foldBack2 folder (toArray xs) (toArray ys) state

let forAll predicate (xs: 'T seq) =
    not (exists (fun x -> not (predicate x)) xs)

let forAll2 predicate (xs: 'T1 seq) (ys: 'T2 seq) =
    not (exists2 (fun x y -> not (predicate x y)) xs ys)

let tryFindBack predicate (xs: 'T seq) =
    xs |> toArray |> Array.tryFindBack predicate

let findBack predicate (xs: 'T seq) =
    match tryFindBack predicate xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryFindIndexBack predicate (xs: 'T seq) =
    xs |> toArray |> Array.tryFindIndexBack predicate

let findIndexBack predicate (xs: 'T seq) =
    match tryFindIndexBack predicate xs with
    | Some x -> x
    | None -> indexNotFound ()

let tryHead (xs: 'T seq) =
    match xs with
    // | :? array<'T> as a -> Array.tryHead a
    // | :? list<'T> as a -> List.tryHead a
    | _ ->
        use e = ofSeq xs

        if e.MoveNext() then
            Some(e.Current)
        else
            None

let head (xs: 'T seq) =
    match tryHead xs with
    | Some x -> x
    | None -> invalidArg "source" SR.inputSequenceEmpty

let initialize count f =
    let gen i =
        if (i < count) then
            Some(f i, i + 1)
        else
            None

    unfold gen 0

let initializeInfinite f = initialize (System.Int32.MaxValue) f

let isEmpty (xs: 'T seq) =
    match xs with
    // | :? array<'T> as a -> Array.isEmpty a
    // | :? list<'T> as a -> List.isEmpty a
    | _ ->
        use e = ofSeq xs
        not (e.MoveNext())

let tryItem index (xs: 'T seq) =
    match xs with
    // | :? array<'T> as a -> Array.tryItem index a
    // | :? list<'T> as a -> List.tryItem index a
    | _ ->
        let mutable i = index

        if i < 0 then
            None
        else
            use e = ofSeq xs

            while i >= 0 && e.MoveNext() do
                i <- i - 1

            if i >= 0 then
                None
            else
                Some e.Current

let item index (xs: 'T seq) =
    match tryItem index xs with
    | Some x -> x
    | None -> invalidArg "index" SR.notEnoughElements

let iterate action (xs: 'T seq) = fold (fun () x -> action x) () xs

let iterate2 action (xs: 'T1 seq) (ys: 'T2 seq) =
    fold2 (fun () x y -> action x y) () xs ys

let iterateIndexed action (xs: 'T seq) =
    fold
        (fun i x ->
            action i x
            i + 1
        )
        0
        xs
    |> ignore

let iterateIndexed2 action (xs: 'T1 seq) (ys: 'T2 seq) =
    fold2
        (fun i x y ->
            action i x y
            i + 1
        )
        0
        xs
        ys
    |> ignore

let tryLast (xs: 'T seq) =
    use e = ofSeq xs

    if e.MoveNext() then
        let mutable acc = e.Current

        while e.MoveNext() do
            acc <- e.Current

        Some acc
    else
        None

let last (xs: 'T seq) =
    match tryLast xs with
    | Some x -> x
    | None -> invalidArg "source" SR.notEnoughElements

let length (xs: 'T seq) =
    match xs with
    // | :? array<'T> as a -> Array.length a
    // | :? list<'T> as a -> List.length a
    | _ ->
        let mutable count = 0
        use e = ofSeq xs

        while e.MoveNext() do
            count <- count + 1

        count

let map (mapping: 'T -> 'U) (xs: 'T seq) =
    generate
        (fun () -> ofSeq xs)
        (fun e ->
            if e.MoveNext() then
                Some(mapping e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let mapIndexed (mapping: int -> 'T -> 'U) (xs: 'T seq) =
    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if e.MoveNext() then
                Some(mapping i e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let indexed (xs: 'T seq) = xs |> mapIndexed (fun i x -> (i, x))

let map2 (mapping: 'T1 -> 'T2 -> 'U) (xs: 'T1 seq) (ys: 'T2 seq) =
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

let mapIndexed2 (mapping: int -> 'T1 -> 'T2 -> 'U) (xs: 'T1 seq) (ys: 'T2 seq) =
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
    (xs: 'T1 seq)
    (ys: 'T2 seq)
    (zs: 'T3 seq)
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

let readOnly (xs: 'T seq) =
    // checkNonNull "source" xs
    map id xs

let mapFold (mapping: 'State -> 'T -> 'U * 'State) state (xs: 'T seq) =
    let arr, state = Array.mapFold mapping state (toArray xs)
    readOnly (ofArray arr), state

let mapFoldBack (mapping: 'T -> 'State -> 'U * 'State) (xs: 'T seq) state =
    let arr, state = Array.mapFoldBack mapping (toArray xs) state
    readOnly (ofArray arr), state

let collect (mapping: 'T -> 'U seq) (xs: 'T seq) =
    delay (fun () -> concat (map mapping xs))

// Adapted from https://github.com/dotnet/fsharp/blob/eb1337f218275da5294b5fbab2cf77f35ca5f717/src/fsharp/FSharp.Core/seq.fs#L971
let cache (xs: 'T seq) : 'T seq =
    let prefix = ResizeArray<_>()
    let mutable enumOpt = None
    let mutable finished = false

    let result i =
        // TODO: enable lock in multi-threading context
        // lock prefix <| fun () ->
        if i < prefix.Count then
            Some(prefix[i], i + 1)
        else
            if enumOpt.IsNone then
                enumOpt <- Some(xs.GetEnumerator())

            match enumOpt with
            | Some e when not finished ->
                if e.MoveNext() then
                    prefix.Add(e.Current)
                    Some(e.Current, i + 1)
                else
                    finished <- true
                    None
            | _ -> None

    unfold result 0

let allPairs (xs: 'T1 seq) (ys: 'T2 seq) : seq<'T1 * 'T2> =
    let ysCache = cache ys

    delay (fun () ->
        let mapping (x: 'T1) = ysCache |> map (fun y -> (x, y))
        concat (map mapping xs)
    )

let tryPick chooser (xs: 'T seq) =
    use e = ofSeq xs
    let mutable res = None

    while (Option.isNone res && e.MoveNext()) do
        res <- chooser e.Current

    res

let pick chooser (xs: 'T seq) =
    match tryPick chooser xs with
    | Some x -> x
    | None -> indexNotFound ()

let reduce folder (xs: 'T seq) =
    use e = ofSeq xs

    if e.MoveNext() then
        let mutable acc = e.Current

        while e.MoveNext() do
            acc <- folder acc e.Current

        acc
    else
        invalidOp SR.inputSequenceEmpty

let reduceBack folder (xs: 'T seq) =
    let arr = toArray xs

    if arr.Length > 0 then
        Array.reduceBack folder arr
    else
        invalidOp SR.inputSequenceEmpty

let replicate n x = initialize n (fun _ -> x)

let reverse (xs: 'T seq) =
    delay (fun () -> xs |> toArray |> Array.rev |> ofArray)

let scan (folder: 'State -> 'T -> 'State) (state: 'State) (xs: 'T seq) =
    delay (fun () ->
        let first = singleton state
        let mutable acc = state

        let rest =
            xs
            |> map (fun x ->
                acc <- folder acc x
                acc
            )

        append first rest
    )

let scanBack (folder: 'T -> 'State -> 'State) (xs: 'T seq) (state: 'State) =
    delay (fun () ->
        let arr = toArray xs
        Array.scanBack folder arr state |> ofArray
    )

let skip count (xs: 'T seq) =
    mkSeq (fun () ->
        let e = ofSeq xs

        for i = 1 to count do
            if not (e.MoveNext()) then
                invalidArg "source" SR.notEnoughElements

        e
    )

// let skip count (xs: 'T seq) =
//     mkSeq (fun () ->
//         let e = ofSeq xs
//         try
//             for i = 1 to count do
//                 if not (e.MoveNext()) then
//                     invalidArg "source" SR.notEnoughElements
//             let compensation () = ()
//             Enumerable.enumerateThenFinally compensation e
//         with _ ->
//             e.Dispose()
//             reraise()
//     )

let skipWhile predicate (xs: 'T seq) =
    delay (fun () ->
        let mutable skipped = true

        xs
        |> filter (fun x ->
            if skipped then
                skipped <- predicate x

            not skipped
        )
    )

let tail (xs: 'T seq) = skip 1 xs

let take count (xs: 'T seq) =
    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if i < count then
                if not (e.MoveNext()) then
                    invalidArg "source" SR.notEnoughElements

                Some(e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let takeWhile predicate (xs: 'T seq) =
    generate
        (fun () -> ofSeq xs)
        (fun e ->
            if e.MoveNext() && predicate e.Current then
                Some(e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let truncate count (xs: 'T seq) =
    generateIndexed
        (fun () -> ofSeq xs)
        (fun i e ->
            if i < count && e.MoveNext() then
                Some(e.Current)
            else
                None
        )
        (fun e -> e.Dispose())

let zip (xs: 'T1 seq) (ys: 'T2 seq) = map2 (fun x y -> (x, y)) xs ys

let zip3 (xs: 'T1 seq) (ys: 'T2 seq) (zs: 'T3 seq) =
    map3 (fun x y z -> (x, y, z)) xs ys zs

let pairwise (xs: 'T seq) =
    delay (fun () -> xs |> toArray |> Array.pairwise |> ofArray)

let splitInto (chunks: int) (xs: 'T seq) : 'T[] seq =
    delay (fun () -> xs |> toArray |> Array.splitInto chunks |> ofArray)

let where predicate (xs: 'T seq) = filter predicate xs

let windowed windowSize (xs: 'T seq) : 'T[] seq =
    delay (fun () -> xs |> toArray |> Array.windowed windowSize |> ofArray)

// let transpose (xss: seq<#'T seq>) = //TODO:
let transpose (xss: 'T seq seq) =
    delay (fun () ->
        xss
        |> toArray
        |> Array.map toArray
        |> Array.transpose
        |> Array.map ofArray
        |> ofArray
    )

let sortWith (comparer: 'T -> 'T -> int) (xs: 'T seq) =
    delay (fun () ->
        let arr = toArray xs
        Array.sortInPlaceWith comparer arr
        ofArray arr
    )

let sort (xs: 'T seq) = sortWith compare xs

let sortBy (projection: 'T -> 'U) (xs: 'T seq) =
    sortWith (fun x y -> compare (projection x) (projection y)) xs

let sortDescending (xs: 'T seq) =
    sortWith (fun x y -> (compare x y) * -1) xs

let sortByDescending (projection: 'T -> 'U) (xs: 'T seq) =
    sortWith (fun x y -> (compare (projection x) (projection y)) * -1) xs

[<CompiledName("sum")>]
let inline sum (xs: 'T seq) : 'T =
    let zero = LanguagePrimitives.GenericZero
    fold (fun acc x -> acc + x) zero xs

[<CompiledName("sumBy")>]
let inline sumBy (projection: 'T -> 'U) (xs: 'T seq) : 'U =
    let zero = LanguagePrimitives.GenericZero
    fold (fun acc x -> acc + (projection x)) zero xs

let maxBy (projection: 'T -> 'U) (xs: 'T seq) : 'T =
    reduce
        (fun x y ->
            if (projection x) > (projection y) then
                x
            else
                y
        )
        xs

let max (xs: 'T seq) : 'T =
    reduce
        (fun x y ->
            if x > y then
                x
            else
                y
        )
        xs

let minBy (projection: 'T -> 'U) (xs: 'T seq) : 'T =
    reduce
        (fun x y ->
            if (projection x) < (projection y) then
                x
            else
                y
        )
        xs

let min (xs: 'T seq) : 'T =
    reduce
        (fun x y ->
            if x < y then
                x
            else
                y
        )
        xs

[<CompiledName("average")>]
let inline average (xs: 'T seq) : 'T =
    let mutable count = 0
    let zero = LanguagePrimitives.GenericZero

    let folder acc x =
        count <- count + 1
        acc + x

    let total = fold folder zero xs

    if count = 0 then
        invalidOp SR.inputSequenceEmpty

    LanguagePrimitives.DivideByInt total count

[<CompiledName("averageBy")>]
let inline averageBy (projection: 'T -> 'U) (xs: 'T seq) : 'U =
    let mutable count = 0
    let zero = LanguagePrimitives.GenericZero

    let folder acc x =
        count <- count + 1
        acc + (projection x)

    let total = fold folder zero xs

    if count = 0 then
        invalidOp SR.inputSequenceEmpty

    LanguagePrimitives.DivideByInt total count

let permute f (xs: 'T seq) =
    delay (fun () -> xs |> toArray |> Array.permute f |> ofArray)

let chunkBySize (chunkSize: int) (xs: 'T seq) : 'T[] seq =
    delay (fun () -> xs |> toArray |> Array.chunkBySize chunkSize |> ofArray)

let distinct<'T when 'T: equality> (xs: 'T seq) =
    delay (fun () ->
        let hashSet = System.Collections.Generic.HashSet<'T>()
        xs |> filter (fun x -> hashSet.Add(x))
    )

let distinctBy<'T, 'Key when 'Key: equality>
    (projection: 'T -> 'Key)
    (xs: 'T seq)
    =
    delay (fun () ->
        let hashSet = System.Collections.Generic.HashSet<'Key>()
        xs |> filter (fun x -> hashSet.Add(projection x))
    )

let except<'T when 'T: equality> (itemsToExclude: 'T seq) (xs: 'T seq) =
    delay (fun () ->
        let hashSet =
            System.Collections.Generic.HashSet<'T>(toArray itemsToExclude)

        xs |> filter (fun x -> hashSet.Add(x))
    )

let countBy<'T, 'Key when 'Key: equality>
    (projection: 'T -> 'Key)
    (xs: 'T seq)
    : ('Key * int) seq
    =
    delay (fun () ->
        let dict = System.Collections.Generic.Dictionary<'Key, int>()
        let keys = ResizeArray<'Key>()

        for x in xs do
            let key = projection x

            match dict.TryGetValue(key) with
            | true, prev -> dict[key] <- prev + 1
            | false, _ ->
                dict[key] <- 1
                keys.Add(key)

        keys |> asArray |> Array.map (fun key -> key, dict[key]) |> ofArray
    )

let groupBy<'T, 'Key when 'Key: equality>
    (projection: 'T -> 'Key)
    (xs: 'T seq)
    : ('Key * 'T seq) seq
    =
    delay (fun () ->
        let dict =
            System.Collections.Generic.Dictionary<'Key, ResizeArray<'T>>()

        let keys = ResizeArray<'Key>()

        for x in xs do
            let key = projection x

            match dict.TryGetValue(key) with
            | true, prev -> prev.Add(x)
            | false, _ ->
                dict.Add(key, ResizeArray [| x |])
                keys.Add(key)

        keys
        |> asArray
        |> Array.map (fun key -> key, dict[key] |> asArray |> ofArray)
        |> ofArray
    )

let insertAt (index: int) (y: 'T) (xs: 'T seq) : 'T seq =
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

let insertManyAt (index: int) (ys: 'T seq) (xs: 'T seq) : 'T seq =
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

let removeAt (index: int) (xs: 'T seq) : 'T seq =
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

let removeManyAt (index: int) (count: int) (xs: 'T seq) : 'T seq =
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
                    for _i = 1 to count do
                        if not (e.MoveNext()) then
                            invalidArg "count" SR.indexOutOfBounds

                if e.MoveNext() then
                    Some e.Current
                else
                    None
        )
        (fun e -> e.Dispose())

let updateAt (index: int) (y: 'T) (xs: 'T seq) : 'T seq =
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

// // let init = initialize
// // let initInfinite = initializeInfinite
// // let iter = iterate
// // let iter2 = iterate2
// // let iteri = iterateIndexed
// // let iteri2 = iterateIndexed2
// // let forall = forAll
// // let forall2 = forAll2
// // let mapi = mapIndexed
// // let mapi2 = mapIndexed2
// // let readonly = readOnly
// // let rev = reverse
