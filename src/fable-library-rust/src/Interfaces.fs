module Interfaces

type IEnumerator<'T> =
    inherit System.IDisposable
    abstract Current: 'T
    abstract MoveNext: unit -> bool

type IEnumerable<'T> =
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
