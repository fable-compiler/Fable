// Adapted from:
// https://github.com/fsprojects/FSharpx.Extras/blob/master/src/FSharpx.Extras/ComputationExpressions/Enumerator.fs
// https://github.com/dotnet/fsharp/blob/main/src/fsharp/FSharp.Core/seq.fs
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module SeqModule

open System.Collections.Generic

module SR =
    let enumerationAlreadyFinished = "Enumeration already finished."
    let enumerationNotStarted = "Enumeration has not started. Call MoveNext."
    let inputSequenceEmpty = "The input sequence was empty."
    let inputSequenceTooLong = "The input sequence contains more than one element."
    let keyNotFoundAlt = "An index satisfying the predicate was not found in the collection."
    let notEnoughElements = "The input sequence has an insufficient number of elements."
    let resetNotSupported = "Reset is not supported on this enumerator."

module Enumerator =

    let noReset() = raise (System.NotSupportedException(SR.resetNotSupported))
    let notStarted() = raise (System.InvalidOperationException(SR.enumerationNotStarted))
    let alreadyFinished() = raise (System.InvalidOperationException(SR.enumerationAlreadyFinished))

    [<Sealed>]
    [<CompiledName("Seq")>]
    type Enumerable<'T>(f) =
        interface IEnumerable<'T> with
            member x.GetEnumerator(): IEnumerator<'T> = f()
            member x.GetEnumerator(): System.Collections.IEnumerator = f() :> _
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

    type FromFunctions<'T>(_current, _next, _dispose) =
        interface IEnumerator<'T> with
            member _.Current: 'T = _current()
            member _.Current: obj = box (_current())
            member _.MoveNext() = _next()
            member _.Reset() = noReset()
            member _.Dispose() = _dispose()

    let inline fromFunctions current next dispose: IEnumerator<'T> =
        new FromFunctions<_>(current, next, dispose) :> IEnumerator<'T>

    let cast (e: System.Collections.IEnumerator): IEnumerator<'T> =
        let current() = unbox<'T> e.Current
        let next() = e.MoveNext()
        let dispose() =
            match e with
            | :? System.IDisposable as e -> e.Dispose()
            | _ -> ()
        fromFunctions current next dispose

    let concat<'T,'U when 'U :> seq<'T>> (sources: seq<'U>) =
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
                        innerOpt <- Some ((ie :> seq<'T>).GetEnumerator())
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
        let next() =
            if not started then started <- true
            if finished then false
            else loop ()
        let dispose() = if not finished then finish()
        fromFunctions current next dispose

    let enumerateThenFinally f (e: IEnumerator<'T>): IEnumerator<'T> =
        let current() = e.Current
        let next() = e.MoveNext()
        let dispose() = try e.Dispose() finally f()
        fromFunctions current next dispose

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
        let next() =
            if not started then started <- true
            match state with
            | None -> false
            | Some s ->
                match (try compute s with _ -> finish(); reraise()) with
                | None -> finish(); false
                | Some _ as x -> curr <- x; true
        fromFunctions current next dispose

    let unfold (f: 'State -> ('T * 'State) option) (state: 'State): IEnumerator<'T> =
        let mutable curr: ('T * 'State) option = None
        let mutable acc: 'State = state
        let current() =
            match curr with
            | None -> notStarted()
            | Some (x, _) -> x
        let next() =
            curr <- f acc
            match curr with
            | None -> false
            | Some (_, st) ->
                acc <- st
                true
        let dispose() = ()
        fromFunctions current next dispose
