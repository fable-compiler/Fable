namespace System.Collections.Generic

type KeyNotFoundException(message: string) =
    inherit System.Exception(message)
    new() = KeyNotFoundException(SR.Arg_KeyNotFound)

type Comparer<'T when 'T: comparison and 'T: null>(comparison: 'T -> 'T -> int) =

    static member Default = Comparer<'T>(LanguagePrimitives.GenericComparison)

    // Same overload suffix as System.Comparison<'T>, which is what callers of Comparer.Create pass
    static member Create(comparison: System.Func<'T, 'T, int>) =
        Comparer<'T>(fun x y -> comparison.Invoke(x, y))

    // Exported for Python output compiled by older Fable versions
    static member Create(comparison: 'T -> 'T -> int) = Comparer<'T>(comparison)

    member _.Compare(x, y) = comparison x y

    interface IComparer<'T> with
        member _.Compare(x, y) =
            match x, y with
            | null, null -> 0
            | null, y -> -1
            | x, null -> 1
            | x, y -> comparison x y

type EqualityComparer<'T when 'T: equality and 'T: null>(equals: 'T -> 'T -> bool, getHashCode: 'T -> int) =

    static member Default =
        EqualityComparer<'T>(LanguagePrimitives.GenericEquality, LanguagePrimitives.GenericHash)

    static member Create(equals: System.Func<'T, 'T, bool>, getHashCode: System.Func<'T, int>) =
        EqualityComparer<'T>((fun x y -> equals.Invoke(x, y)), (fun x -> getHashCode.Invoke x))

    // Exported for Python output compiled by older Fable versions
    static member Create(equals: 'T -> 'T -> bool, getHashCode: 'T -> int) =
        EqualityComparer<'T>(equals, getHashCode)

    member _.Equals(x, y) = equals x y

    member _.GetHashCode(x) = getHashCode x

    interface IEqualityComparer<'T> with
        member _.Equals(x, y) =
            match x, y with
            | null, null -> true
            | null, y -> false
            | x, null -> false
            | x, y -> equals x y

        member _.GetHashCode(x) = getHashCode x

type Stack<'T> private (initialContents, initialCount) =
    let mutable contents = initialContents
    let mutable count = initialCount

    new(initialCapacity: int) = Stack<'T>(Array.zeroCreate<'T>(initialCapacity), 0)

    new() = Stack<'T>(4)

    new(xs: IEnumerable<'T>)
        =
        let arr = Array.ofSeq xs
        Stack<'T>(arr, arr.Length)

    member _.Ensure(newSize) =
        let oldSize = contents.Length

        if newSize > oldSize then
            let old = contents
            contents <- Array.zeroCreate (max newSize (oldSize * 2))
            Array.blit old 0 contents 0 count

    member _.Count = count

    member _.Pop() =
        count <- count - 1
        contents.[count]

    member _.Peek() = contents.[count - 1]

    member _.Contains(x: 'T) =
        let mutable found = false
        let mutable i = 0

        while i < count && not found do
            if System.Object.Equals(x, contents.[i]) then
                found <- true
            else
                i <- i + 1

        found

    member this.TryPeek(result: 'T byref) =
        if count > 0 then
            result <- this.Peek()
            true
        else
            false

    member this.TryPop(result: 'T byref) =
        if count > 0 then
            result <- this.Pop()
            true
        else
            false

    member this.Push(x) =
        this.Ensure(count + 1)
        contents.[count] <- x
        count <- count + 1

    member _.Clear() =
        count <- 0
        Array.fill contents 0 contents.Length Unchecked.defaultof<_>

    member this.TrimExcess() =
        if float count / float contents.Length > 0.9 then
            this.Ensure(count)

    member _.ToArray() =
        Array.init count (fun i -> contents.[count - 1 - i])

    interface IEnumerable<'T> with
        member _.GetEnumerator() =
            (seq {
                let mutable index = count - 1

                while index >= 0 do
                    yield contents.[index]
                    index <- index - 1
            })
                .GetEnumerator()

        member this.GetEnumerator() =
            (this :> IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator

type Queue<'T> private (initialContents, initialCount) =
    let mutable contents: 'T array = initialContents
    let mutable count = initialCount
    let mutable head = 0

    let mutable tail =
        if initialCount = contents.Length then
            0
        else
            initialCount

    let size () = contents.Length

    let toIndex i = (head + i) % size ()

    let ensure (requiredSize: int) =
        let newBuffer: 'T array = Array.zeroCreate requiredSize

        if head < tail then
            Array.blit contents head newBuffer 0 count
        else
            Array.blit contents head newBuffer 0 (size () - head)
            Array.blit contents 0 newBuffer (size () - head) tail

        head <- 0
        contents <- newBuffer

        tail <-
            if count = size () then
                0
            else
                count

    let toSeq () =
        seq {
            let mutable i = 0

            while i < count do
                yield contents.[i |> toIndex]
                i <- i + 1
        }

    new(initialCapacity: int)
        =
        if initialCapacity < 0 then
            raise (System.ArgumentOutOfRangeException("capacity is less than 0"))

        Queue<'T>(Array.zeroCreate<'T>(initialCapacity), 0)

    new() = Queue<'T>(4)

    new(xs: IEnumerable<'T>)
        =
        let arr = Array.ofSeq xs
        Queue<'T>(arr, arr.Length)

    member _.Count = count

    member _.Enqueue(value: 'T) =
        if count = size () then
            ensure (count + 1)

        contents.[tail] <- value
        tail <- (tail + 1) % size ()
        count <- count + 1

    member _.Dequeue() : 'T =
        if count = 0 then
            invalidOp "Queue is empty"

        let value = contents.[head]
        head <- (head + 1) % size ()
        count <- count - 1
        value

    member _.Peek() : 'T =
        if count = 0 then
            invalidOp "Queue is empty"

        contents.[head]

    member this.TryDequeue(result: 'T byref) : bool =
        if count = 0 then
            false
        else
            result <- this.Dequeue()
            true

    member this.TryPeek(result: 'T byref) : bool =
        if count = 0 then
            false
        else
            result <- this.Peek()
            true

    member _.Contains(x: 'T) =
        let mutable found = false
        let mutable i = 0

        while i < count && not found do
            if System.Object.Equals(x, contents.[i |> toIndex]) then
                found <- true
            else
                i <- i + 1

        found

    member _.Clear() =
        count <- 0
        head <- 0
        tail <- 0
        Array.fill contents 0 (size ()) Unchecked.defaultof<_>

    member _.TrimExcess() =
        if float count / float contents.Length > 0.9 then
            ensure (count)

    member _.ToArray() = toSeq () |> Seq.toArray

    member _.CopyTo(target: 'T array, start: int) =
        let mutable i = start

        for item in toSeq () do
            target.[i] <- item
            i <- i + 1

    interface IEnumerable<'T> with
        member _.GetEnumerator() = toSeq().GetEnumerator()

        member this.GetEnumerator() =
            (this :> IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator

type LinkedListNode<'T> =
    val mutable internal list: LinkedList<'T>
    val mutable internal next: LinkedListNode<'T>
    val mutable internal prev: LinkedListNode<'T>
    val mutable internal item: 'T

    new(value: 'T)
        =
        {
            list = Unchecked.defaultof<_>
            next = Unchecked.defaultof<_>
            prev = Unchecked.defaultof<_>
            item = value
        }

    member this.List = this.list

    member this.Next = this.next

    member this.Previous = this.prev

    member this.Value
        with get () = this.item
        and set (value: 'T) = this.item <- value

    member internal this.Invalidate() =
        this.list <- Unchecked.defaultof<_>
        this.next <- Unchecked.defaultof<_>
        this.prev <- Unchecked.defaultof<_>

// Unlike .NET's circular list, the first node has no previous node and the last node no next one
and LinkedList<'T> =
    val mutable private head: LinkedListNode<'T>
    val mutable private tail: LinkedListNode<'T>
    val mutable private count: int

    new()
        =
        {
            head = Unchecked.defaultof<_>
            tail = Unchecked.defaultof<_>
            count = 0
        }

    new(collection: IEnumerable<'T>) as this
        =
        LinkedList<'T>()

        then
            if isNull (box collection) then
                raise (System.ArgumentNullException("collection"))

            for item in collection do
                this.InsertLast(LinkedListNode<'T>(item))

    member this.Count = this.count

    member this.First = this.head

    member this.Last = this.tail

    member this.AddFirst(value: 'T) : LinkedListNode<'T> =
        let node = LinkedListNode<'T>(value)
        this.InsertFirst(node)
        node

    member this.AddFirst(node: LinkedListNode<'T>) : unit =
        LinkedList<'T>.ValidateNewNode(node)
        this.InsertFirst(node)

    member this.AddLast(value: 'T) : LinkedListNode<'T> =
        let node = LinkedListNode<'T>(value)
        this.InsertLast(node)
        node

    member this.AddLast(node: LinkedListNode<'T>) : unit =
        LinkedList<'T>.ValidateNewNode(node)
        this.InsertLast(node)

    member this.AddBefore(node: LinkedListNode<'T>, value: 'T) : LinkedListNode<'T> =
        this.ValidateNode(node)
        let newNode = LinkedListNode<'T>(value)
        this.InsertBefore(node, newNode)
        newNode

    member this.AddBefore(node: LinkedListNode<'T>, newNode: LinkedListNode<'T>) : unit =
        this.ValidateNode(node)
        LinkedList<'T>.ValidateNewNode(newNode)
        this.InsertBefore(node, newNode)

    member this.AddAfter(node: LinkedListNode<'T>, value: 'T) : LinkedListNode<'T> =
        this.ValidateNode(node)
        let newNode = LinkedListNode<'T>(value)
        this.InsertAfter(node, newNode)
        newNode

    member this.AddAfter(node: LinkedListNode<'T>, newNode: LinkedListNode<'T>) : unit =
        this.ValidateNode(node)
        LinkedList<'T>.ValidateNewNode(newNode)
        this.InsertAfter(node, newNode)

    member this.Remove(value: 'T) : bool =
        let node = this.Find(value)

        if isNull (box node) then
            false
        else
            this.RemoveNode(node)
            true

    member this.Remove(node: LinkedListNode<'T>) : unit =
        this.ValidateNode(node)
        this.RemoveNode(node)

    member this.RemoveFirst() =
        if isNull (box this.head) then
            invalidOp SR.LinkedListEmpty

        this.RemoveNode(this.head)

    member this.RemoveLast() =
        if isNull (box this.tail) then
            invalidOp SR.LinkedListEmpty

        this.RemoveNode(this.tail)

    member this.Find(value: 'T) : LinkedListNode<'T> =
        let mutable node = this.head

        while not (isNull (box node)) && not (System.Object.Equals(node.item, value)) do
            node <- node.next

        node

    member this.FindLast(value: 'T) : LinkedListNode<'T> =
        let mutable node = this.tail

        while not (isNull (box node)) && not (System.Object.Equals(node.item, value)) do
            node <- node.prev

        node

    member this.Contains(value: 'T) = not (isNull (box (this.Find(value))))

    member this.Clear() =
        let mutable node = this.head

        while not (isNull (box node)) do
            let next = node.next
            node.Invalidate()
            node <- next

        this.head <- Unchecked.defaultof<_>
        this.tail <- Unchecked.defaultof<_>
        this.count <- 0

    member this.CopyTo(array: 'T[], index: int) =
        if isNull (box array) then
            raise (System.ArgumentNullException("array"))

        if index < 0 then
            raise (System.ArgumentOutOfRangeException("index"))

        if index > array.Length then
            raise (System.ArgumentOutOfRangeException("index", SR.ArgumentOutOfRange_BiggerThanCollection))

        if array.Length - index < this.count then
            raise (System.ArgumentException(SR.Arg_InsufficientSpace))

        let mutable node = this.head
        let mutable i = index

        while not (isNull (box node)) do
            array.[i] <- node.item
            node <- node.next
            i <- i + 1

    static member private ValidateNewNode(node: LinkedListNode<'T>) =
        if isNull (box node) then
            raise (System.ArgumentNullException("node"))

        if not (isNull (box node.list)) then
            invalidOp SR.LinkedListNodeIsAttached

    member private this.ValidateNode(node: LinkedListNode<'T>) =
        if isNull (box node) then
            raise (System.ArgumentNullException("node"))

        if not (obj.ReferenceEquals(node.list, this)) then
            invalidOp SR.ExternalLinkedListNode

    member private this.InsertFirst(newNode: LinkedListNode<'T>) =
        if isNull (box this.head) then
            this.InsertIntoEmpty(newNode)
        else
            this.InsertBefore(this.head, newNode)

    member private this.InsertLast(newNode: LinkedListNode<'T>) =
        if isNull (box this.tail) then
            this.InsertIntoEmpty(newNode)
        else
            this.InsertAfter(this.tail, newNode)

    member private this.InsertIntoEmpty(newNode: LinkedListNode<'T>) =
        newNode.list <- this
        this.head <- newNode
        this.tail <- newNode
        this.count <- this.count + 1

    member private this.InsertBefore(node: LinkedListNode<'T>, newNode: LinkedListNode<'T>) =
        newNode.list <- this
        newNode.next <- node
        newNode.prev <- node.prev

        if isNull (box node.prev) then
            this.head <- newNode
        else
            node.prev.next <- newNode

        node.prev <- newNode
        this.count <- this.count + 1

    member private this.InsertAfter(node: LinkedListNode<'T>, newNode: LinkedListNode<'T>) =
        newNode.list <- this
        newNode.prev <- node
        newNode.next <- node.next

        if isNull (box node.next) then
            this.tail <- newNode
        else
            node.next.prev <- newNode

        node.next <- newNode
        this.count <- this.count + 1

    member private this.RemoveNode(node: LinkedListNode<'T>) =
        if isNull (box node.prev) then
            this.head <- node.next
        else
            node.prev.next <- node.next

        if isNull (box node.next) then
            this.tail <- node.prev
        else
            node.next.prev <- node.prev

        node.Invalidate()
        this.count <- this.count - 1

    interface IEnumerable<'T> with
        member this.GetEnumerator() =
            (seq {
                let mutable node = this.head

                while not (isNull (box node)) do
                    yield node.item
                    node <- node.next
            })
                .GetEnumerator()

        member this.GetEnumerator() =
            (this :> IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator
