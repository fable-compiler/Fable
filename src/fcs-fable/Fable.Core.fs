namespace Fable.Core

module JS =

    type Map<'K, 'V> =
        abstract size: int
        abstract clear: unit -> unit
        abstract delete: key: 'K -> bool
        abstract entries: unit -> seq<'K * 'V>

        abstract forEach: callbackfn: ('V -> 'K -> Map<'K, 'V> -> unit) * ?thisArg: obj -> unit

        abstract get: key: 'K -> 'V
        abstract has: key: 'K -> bool
        abstract keys: unit -> seq<'K>
        abstract set: key: 'K * value: 'V -> Map<'K, 'V>
        abstract values: unit -> seq<'V>
