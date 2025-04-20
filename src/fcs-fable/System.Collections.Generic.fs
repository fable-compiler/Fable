//------------------------------------------------------------------------
// shims for things not yet implemented in Fable
//------------------------------------------------------------------------

namespace System.Collections.Generic

[<AllowNullLiteral>]
type LinkedListNode<'T>(value: 'T) =
    member val Value = value with get, set
    member val Previous: LinkedListNode<'T> = null with get, set
    member val Next: LinkedListNode<'T> = null with get, set

type LinkedList<'T>() =
    let mutable head: LinkedListNode<'T> = null
    let mutable tail: LinkedListNode<'T> = null

    // Get the first node in the list
    member _.First = head

    // Get the last node in the list
    member _.Last = tail

    // Get the number of nodes in the list
    member _.Count =
        let rec loop (currentNode: LinkedListNode<'T>) count =
            if currentNode = null then count
            else loop currentNode.Next (count + 1)
        loop head 0

    // Clear the list
    member _.Clear() =
        head <- null
        tail <- null

    // Add a new node to the end of the list
    member _.AddLast(value: 'T) =
        let newNode = LinkedListNode(value)
        if tail = null then
            head <- newNode
            tail <- newNode
        else
            tail.Next <- newNode
            newNode.Previous <- tail
            tail <- newNode
        newNode

    // Add a node to the end of the list
    member _.AddLast(node: LinkedListNode<'T>) =
        if tail = null then
            node.Next <- null
            node.Previous <- null
            head <- node
            tail <- node
        else
            tail.Next <- node
            node.Next <- null
            node.Previous <- tail
            tail <- node

    // Add a new node to the beginning of the list
    member _.AddFirst(value: 'T) =
        let newNode = LinkedListNode(value)
        if head = null then
            head <- newNode
            tail <- newNode
        else
            head.Previous <- newNode
            newNode.Next <- head
            head <- newNode
        newNode

    // Add a node to the beginning of the list
    member _.AddFirst(node: LinkedListNode<'T>) =
        if head = null then
            node.Next <- null
            node.Previous <- null
            head <- node
            tail <- node
        else
            head.Previous <- node
            node.Next <- head
            node.Previous <- null
            head <- node

    // Remove a node from the list
    member _.Remove(node: LinkedListNode<'T>) =
        match node.Previous, node.Next with
        | null, null ->
            head <- null
            tail <- null
        | null, nextNode ->
            nextNode.Previous <- null
            head <- nextNode
        | prevNode, null ->
            prevNode.Next <- null
            tail <- prevNode
        | prevNode, nextNode ->
            prevNode.Next <- nextNode
            nextNode.Previous <- prevNode

    // Find a node by value
    member _.Find(value: 'T) =
        let rec loop (currentNode: LinkedListNode<'T>) =
            if currentNode = null then null
            elif Unchecked.equals currentNode.Value value then currentNode
            else loop currentNode.Next
        loop head

    // Implement IEnumerable interface
    interface System.Collections.Generic.IEnumerable<'T> with
        member _.GetEnumerator() =
            let rec loop (currentNode: LinkedListNode<'T>) =
                seq {
                    if currentNode <> null then
                        yield currentNode.Value
                        yield! loop currentNode.Next
                }
            (loop head).GetEnumerator()

        member this.GetEnumerator() : System.Collections.IEnumerator =
            (this :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator
