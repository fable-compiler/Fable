namespace System

type IDisposable =
    abstract Dispose: unit -> unit

type IObserver<'T> =
    abstract OnNext: 'T -> unit
    abstract OnError: exn -> unit
    abstract OnCompleted: unit -> unit

type IObservable<'T> =
    abstract Subscribe: IObserver<'T> -> IDisposable

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


namespace System.Collections

type IComparer =
    abstract Compare: obj * obj -> int

type IEqualityComparer =
    abstract Equals: obj * obj -> bool
    abstract GetHashCode: obj -> int

type IEnumerator =
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


namespace System.Collections.Generic

type IComparer<'T> =
    abstract Compare: 'T * 'T -> int

type IEqualityComparer<'T> =
    abstract Equals: 'T * 'T -> bool
    abstract GetHashCode: 'T -> int

type IEnumerator<'T> =
    inherit System.IDisposable
    inherit System.Collections.IEnumerator
    abstract Current: 'T

type IEnumerable<'T> =
    inherit System.Collections.IEnumerable
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
    abstract Item: 'K -> 'V with get, set
    abstract Keys: ICollection<'K>
    abstract Values: ICollection<'V>
    abstract Add: 'K * 'V -> unit
    abstract ContainsKey: 'K -> bool
    abstract TryGetValue: 'K * byref<'V> -> bool
    abstract Remove: 'K -> bool


namespace Microsoft.FSharp.Control

type IDelegateEvent<'Delegate> =
    abstract AddHandler: 'Delegate -> unit
    abstract RemoveHandler: 'Delegate -> unit

// NOTE: the second type parameter is named 'T (not 'Args) on purpose: when the
// Rust backend flattens the inherited System.IObservable<_>.Subscribe method into
// this trait it keeps IObservable's original parameter name ('T) verbatim instead
// of substituting the actual argument, so the names must line up or the generated
// trait references an undeclared `T`.
type IEvent<'Delegate, 'T> =
    inherit IDelegateEvent<'Delegate>
    inherit System.IObservable<'T>
