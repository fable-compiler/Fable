//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System.Collections.Generic

type Queue<'T> =
    inherit List<'T>

    new () = Queue()

    member x.Enqueue (item: 'T) =
        x.Add(item)

    member x.Dequeue () =
        let item = x.Item(0)
        x.RemoveAt(0)
        item
