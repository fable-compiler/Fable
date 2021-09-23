namespace System.Collections.Generic

open System

type Comparer<'T when 'T : comparison>() =
    static member Default =
        { new IComparer<'T> with
            member __.Compare(x, y) = LanguagePrimitives.GenericComparison x y }
    interface IComparer<'T> with
        member __.Compare(x, y) = LanguagePrimitives.GenericComparison x y

type EqualityComparer<'T when 'T : equality>() =
    static member Default =
        { new IEqualityComparer<'T> with
            member __.Equals(x, y) = LanguagePrimitives.GenericEquality x y
            member __.GetHashCode(x) = LanguagePrimitives.GenericHash x }
    interface IEqualityComparer<'T> with
        member __.Equals(x, y) = LanguagePrimitives.GenericEquality x y
        member __.GetHashCode(x) = LanguagePrimitives.GenericHash x

type Stack<'t> private (initialContents, initialCount) =
    let mutable contents = initialContents
    let mutable count = initialCount

    new(initialCapacity : int) = Stack<'t>(Array.zeroCreate<'t>(initialCapacity), 0)

    new() = Stack<'t>(4)

    new(xs : IEnumerable<'t>) =
        let arr = Array.ofSeq xs
        Stack<'t>(arr, arr.Length)

    member this.Ensure(newSize) =
        let oldSize = contents.Length
        if newSize > oldSize then
            let old = contents
            contents <- Array.zeroCreate (max newSize (oldSize * 2))
            Array.blit old 0 contents 0 count

    member this.Count = count

    member this.Pop() =
        count <- count - 1
        contents.[count]

    member this.Peek() =
        contents.[count - 1]

    member this.Contains(x : 't) =
        let mutable found = false
        let mutable i = 0

        while i < count && not found do
            if Object.Equals(x, contents.[i]) then
                found <- true
            else
                i <- i + 1

        found

    member this.TryPeek(result : 't byref) =
        if count > 0
        then
            result <- this.Peek()
            true
        else
            false

    member this.TryPop(result : 't byref) =
        if count > 0
        then
            result <- this.Pop()
            true
        else
            false

    member this.Push(x) =
        this.Ensure(count + 1)
        contents.[count] <- x
        count <- count + 1

    member this.Clear() =
        count <- 0
        Array.fill contents 0 contents.Length Unchecked.defaultof<_>

    member this.TrimExcess() =
        if float count / float contents.Length > 0.9
        then
            this.Ensure(count)

    member this.ToArray() =
        Array.init count (fun i -> contents.[count - 1 - i])

    interface IEnumerable<'t> with
        member this.GetEnumerator() =
            (seq {
                let mutable index = count - 1

                while index >= 0 do
                    yield contents.[index]
                    index <- index - 1
            }).GetEnumerator()

        member this.GetEnumerator() =
            (this :> IEnumerable<'t>).GetEnumerator()
            :> System.Collections.IEnumerator
