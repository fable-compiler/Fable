// split from Seq to remove circular dependencies
module SeqModule2

open Fable.Core

let distinct
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'T>)
    =
    Seq.delay (fun () ->
        let hashSet = System.Collections.Generic.HashSet<'T>(comparer)
        xs |> Seq.filter (fun x -> hashSet.Add(x))
    )

let distinctBy
    (projection: 'T -> 'Key)
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
    =
    Seq.delay (fun () ->
        let hashSet = System.Collections.Generic.HashSet<'Key>(comparer)
        xs |> Seq.filter (fun x -> hashSet.Add(projection x))
    )

let except
    (itemsToExclude: seq<'T>)
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'T>)
    =
    Seq.delay (fun () ->
        let hashSet =
            System.Collections.Generic.HashSet<'T>(itemsToExclude, comparer)

        xs |> Seq.filter (fun x -> hashSet.Add(x))
    )

let countBy
    (projection: 'T -> 'Key)
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
    : ('Key * int) seq
    =
    Seq.delay (fun () ->
        let dict = System.Collections.Generic.Dictionary<'Key, int>(comparer)
        let keys = ResizeArray<'Key>()

        for x in xs do
            let key = projection x

            match dict.TryGetValue key with
            | true, v -> dict[key] <- v + 1
            | false, _ ->
                dict[key] <- 1
                keys.Add(key)

        Seq.map (fun key -> key, dict[key]) keys
    )

let groupBy
    (projection: 'T -> 'Key)
    (xs: seq<'T>)
    ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
    : ('Key * seq<'T>) seq
    =
    Seq.delay (fun () ->
        let dict =
            System.Collections.Generic.Dictionary<'Key, ResizeArray<'T>>(
                comparer
            )

        let keys = ResizeArray<'Key>()

        for x in xs do
            let key = projection x

            if dict.ContainsKey(key) then
                dict[key].Add(x)
            else
                dict[key] <- ResizeArray [ x ]
                keys.Add(key)

        Seq.map (fun key -> key, dict[key] :> seq<'T>) keys
    )

module Array =

    let distinct
        (xs: 'T[])
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'T>)
        : 'T[]
        =
        distinct xs comparer |> Seq.toArray

    let distinctBy
        (projection: 'T -> 'Key)
        (xs: 'T[])
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
        : 'T[]
        =
        distinctBy projection xs comparer |> Seq.toArray

    let except
        (itemsToExclude: seq<'T>)
        (xs: 'T[])
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'T>)
        : 'T[]
        =
        except itemsToExclude xs comparer |> Seq.toArray

    let countBy
        (projection: 'T -> 'Key)
        (xs: 'T[])
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
        : ('Key * int)[]
        =
        countBy projection xs comparer |> Seq.toArray

    let groupBy
        (projection: 'T -> 'Key)
        (xs: 'T[])
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
        : ('Key * 'T[])[]
        =
        groupBy projection xs comparer
        |> Seq.map (fun (key, values) -> key, Seq.toArray values)
        |> Seq.toArray

module List =

    let distinct
        (xs: 'T list)
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'T>)
        : 'T list
        =
        distinct xs comparer |> Seq.toList

    let distinctBy
        (projection: 'T -> 'Key)
        (xs: 'T list)
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
        : 'T list
        =
        distinctBy projection xs comparer |> Seq.toList

    let except
        (itemsToExclude: seq<'T>)
        (xs: 'T list)
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'T>)
        : 'T list
        =
        except itemsToExclude xs comparer |> Seq.toList

    let countBy
        (projection: 'T -> 'Key)
        (xs: 'T list)
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
        : ('Key * int) list
        =
        countBy projection xs comparer |> Seq.toList

    let groupBy
        (projection: 'T -> 'Key)
        (xs: 'T list)
        ([<Inject>] comparer: System.Collections.Generic.IEqualityComparer<'Key>)
        : ('Key * 'T list) list
        =
        groupBy projection xs comparer
        |> Seq.map (fun (key, values) -> key, Seq.toList values)
        |> Seq.toList
