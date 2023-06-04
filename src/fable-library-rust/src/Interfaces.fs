module Interfaces_

module System =

    type IDisposable =
        abstract Dispose: unit -> unit

    type IEquatable<'T> =
        abstract Equals: 'T -> bool

    type IComparable =
        abstract CompareTo: obj -> int

    type IComparable<'T> =
        abstract CompareTo: 'T -> int

    type IFormatProvider =
        abstract GetFormat: System.Type -> obj

    type IFormattable =
        abstract ToString: string * IFormatProvider -> string

    module Collections =

        type IComparer =
            abstract Compare: obj * obj -> int

        type IEqualityComparer =
            abstract Equals: obj * obj -> bool
            abstract GetHashCode: obj -> int

        type IEnumerator =
            inherit System.IDisposable
            abstract Current: obj
            abstract MoveNext: unit -> bool
            abstract Reset: unit -> unit

        type IEnumerable =
            abstract GetEnumerator: unit -> IEnumerator

        type IStructuralComparable =
            abstract CompareTo: obj * IComparer -> int

        type IStructuralEquatable =
            abstract Equals: obj * IEqualityComparer -> bool
            abstract GetHashCode: IEqualityComparer -> int

        module Generic =

            type IComparer<'T> =
                abstract Compare: 'T * 'T -> int

            type IEqualityComparer<'T> =
                abstract Equals: 'T * 'T -> bool
                abstract GetHashCode: 'T -> int

            type IEnumerator<'T> =
                inherit System.IDisposable
                // inherit System.Collections.IEnumerator
                abstract Current: 'T
                abstract MoveNext: unit -> bool
                abstract Reset: unit -> unit

            type IEnumerable<'T> =
                // inherit System.Collections.IEnumerable
                abstract GetEnumerator: unit -> IEnumerator<'T>

            type ICollection<'T> =
                abstract Count: int
                abstract IsReadOnly: bool
                abstract Contains: 'T -> bool
                abstract Add: 'T -> unit
                abstract Remove: 'T -> bool
                abstract Clear: unit -> unit
                abstract CopyTo: 'T[] * int -> unit

            type IDictionary<'K, 'V> =
                abstract Item: 'K -> 'V with get //, set //TODO: support property setters
                abstract Keys: ICollection<'K>
                abstract Values: ICollection<'V>
                abstract Add: 'K * 'V -> unit
                abstract ContainsKey: 'K -> bool
                abstract TryGetValue: 'K * byref<'V> -> bool
                abstract Remove: 'K -> bool
