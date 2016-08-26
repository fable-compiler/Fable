[<RequireQualifiedAccess>]
module Forge.ResizeArray

/// <summary>
/// Functional operators related to the System.Collections.Generic.List&lt;T&gt; type (called ResizeArray in F#).
/// </summary>

open System.Collections.Generic
open LanguagePrimitives
open OptimizedClosures


/// Determines if a reference is a null reference, and if it is, throws an <see cref="System.ArgumentNullException"/>.
let inline checkNonNull paramName arg =
    if isNull arg then
        if System.String.IsNullOrWhiteSpace paramName then
            raise ^ System.ArgumentNullException ()
        else
            raise ^ System.ArgumentNullException paramName

let keyNotFound (message : string) : 'T =
    if System.String.IsNullOrEmpty message then
        raise <| System.Collections.Generic.KeyNotFoundException ()
    else
        raise <| System.Collections.Generic.KeyNotFoundException message

    
let argOutOfRange (paramName : string) (message : string) : 'T =
    match System.String.IsNullOrEmpty paramName, System.String.IsNullOrEmpty message with
    | false, false ->
        raise <| System.ArgumentOutOfRangeException (paramName, message)
    | false, true ->
        raise <| System.ArgumentOutOfRangeException (paramName)
    | true, true ->
        raise <| System.ArgumentOutOfRangeException ()
    | true, false ->
        raise <| System.ArgumentOutOfRangeException ("(Unspecified parameter)", message)

/// Return the length of the collection.
let inline length (resizeArray : ResizeArray<'T>) : int =
    resizeArray.Count

/// Return true if the given array is empty, otherwise false.
let inline isEmpty (resizeArray : ResizeArray<'T>) : bool =
    resizeArray.Count = 0

/// Fetch an element from the collection.
let inline get index (resizeArray : ResizeArray<'T>) : 'T =
    resizeArray.[index]

/// Set the value of an element in the collection.
let inline set index value (resizeArray : ResizeArray<'T>)  : unit =
    resizeArray.[index] <- value

/// Create a ResizeArray whose elements are all initially the given value.
let create count value : ResizeArray<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The number of elements may not be negative."

    let resizeArray = ResizeArray (count)
    for i = 0 to count - 1 do
        resizeArray.Add value
    resizeArray

/// Create a ResizeArray by calling the given generator on each index.
let init count initializer : ResizeArray<'T> =
    // Preconditions
    if count < 0 then
        invalidArg "count" "The number of elements may not be negative."

    let resizeArray = ResizeArray (count)
    for i = 0 to count - 1 do
        resizeArray.Add <| initializer count
    resizeArray

/// Adds an object to the end of the ResizeArray.
let inline add item (resizeArray : ResizeArray<'T>) =
    resizeArray.Add item
    resizeArray


let inline insert index item (resizeArray : ResizeArray<'T>) =
    resizeArray.Insert(index, item)
    resizeArray


let inline remove item (resizeArray : ResizeArray<'T>) =
    resizeArray.Remove item |> ignore
    resizeArray


/// Determines whether an element is in the ResizeArray.
let inline contains (value : 'T) (resizeArray : ResizeArray<'T>) : bool =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    resizeArray.Contains value

/// Build a ResizeArray from the given sequence.
let inline ofSeq (sequence : seq<'T>) : ResizeArray<'T> =
    ResizeArray (sequence)

/// Build a ResizeArray from the given list.
let ofList (list : 'T list) : ResizeArray<'T> =

    let len = list.Length
    let res = ResizeArray<_>(len)
    let rec add = function
        | [] -> ()
        | e::l -> res.Add(e); add l
    add list
    res

/// Build a ResizeArray from the given array.
let inline ofArray (arr : 'T[]) : ResizeArray<'T> =
    ResizeArray (arr)



/// Return a view of the ResizeArray as an enumerable object.
let toSeq (resizeArray : ResizeArray<'T>) : seq<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    Seq.readonly resizeArray


/// Build a list from the given ResizeArray.
let toList (resizeArray : ResizeArray<'T>) : 'T list =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let mutable res = []
    for i = length resizeArray - 1 downto 0 do
        res <- resizeArray.[i] :: res
    res


/// Return a fixed-length array containing the elements of the input ResizeArray.
let inline toArray (resizeArray : ResizeArray<'T>) : 'T[] =
    resizeArray.ToArray ()
    

/// Sorts the elements of the ResizeArray by mutating the ResizeArray in-place.
/// Elements are compared using Operators.compare.
let inline sortInPlace<'T when 'T : comparison> (resizeArray : ResizeArray<'T>) : unit =
    resizeArray.Sort ()
        

/// Sort the elements using the key extractor and generic comparison on the keys.
let inline sortInPlaceBy<'T, 'Key when 'Key : comparison>
        (projection : 'T -> 'Key) (resizeArray : ResizeArray<'T>) =
    resizeArray.Sort (fun x y ->
        compare (projection x) (projection y))


/// Sort the elements using the given comparison function.
let inline sortInPlaceWith (comparer : 'T -> 'T -> int) (resizeArray : ResizeArray<'T>) : unit =
    resizeArray.Sort (comparer)


/// Build a new ResizeArray that contains the elements of the given ResizeArray.
let inline copy (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
    ResizeArray (resizeArray)


/// Return an array containing the given element.
let singleton value : ResizeArray<'T> =
    let resizeArray = ResizeArray ()
    resizeArray.Add value
    resizeArray


/// Build a new ResizeArray that contains the elements of each of the given sequence of ResizeArrays.
let concat (resizeArrays : seq<ResizeArray<'T>>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArrays" resizeArrays

    let flattened = ResizeArray ()
    for resizeArray in resizeArrays do
        flattened.AddRange resizeArray
    flattened
    

/// Build a new ResizeArray that contains the elements of the first ResizeArray followed by
/// the elements of the second ResizeArray.
let append (resizeArray1 : ResizeArray<'T>) (resizeArray2 : ResizeArray<'T>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray1" resizeArray1
    checkNonNull "resizeArray2" resizeArray2

    let combined = ResizeArray (resizeArray1.Count + resizeArray2.Count)
    combined.AddRange resizeArray1
    combined.AddRange resizeArray2
    combined


/// Return a new ResizeArray with the elements in reverse order.
let rev (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let len = length resizeArray
    let result = ResizeArray (len)
    for i = len - 1 downto 0 do
        result.Add resizeArray.[i]
    result



/// Test if any element of the array satisfies the given predicate.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> 
/// then computes <c>p i0 or ... or p iN</c>.
let inline exists (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : bool =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    resizeArray.Exists (System.Predicate predicate)


/// Test if all elements of the array satisfy the given predicate.
/// If the input function is <c>f</c> and the elements are <c>i0...iN</c> and "j0...jN"
/// then computes <c>p i0 && ... && p iN</c>.
let inline forall (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : bool =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    resizeArray.TrueForAll (System.Predicate predicate)



/// Return a new collection containing only the elements of the collection
/// for which the given predicate returns <c>true</c>.
let inline filter (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    resizeArray.FindAll (System.Predicate predicate)


/// <summary>
/// Apply the given function to each element of the array. Return
/// the array comprised of the results "x" for each element where
/// the function returns <c>Some(x)</c>.
/// </summary>
let choose (chooser : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : ResizeArray<'U> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    // OPTIMIZATION : If the input list is empty return immediately.
    if isEmpty resizeArray then
        ResizeArray ()
    else
        let result = ResizeArray ()
        let count = resizeArray.Count

        for i = 0 to count - 1 do
            match chooser resizeArray.[i] with
            | None -> ()
            | Some value ->
                result.Add value

        result


/// <summary>
/// Return the first element for which the given function returns <c>true</c>.
/// Return <c>None</c> if no such element exists.
/// </summary>
let tryFind (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : 'T option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    match resizeArray.FindIndex ^ System.Predicate predicate with
    | -1    -> None
    | index -> Some resizeArray.[index]


/// <summary>
/// Return the first element for which the given function returns <c>true</c>.
/// Raise <c>KeyNotFoundException</c> if no such element exists.
/// </summary>
let find (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : 'T =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    match resizeArray.FindIndex (System.Predicate predicate) with
    | -1    -> raise <| System.Collections.Generic.KeyNotFoundException ()
    | index -> resizeArray.[index]


/// Return the index of the first element in the array
/// that satisfies the given predicate.
let tryFindIndex (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : int option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    match resizeArray.FindIndex ^ System.Predicate predicate with
    | -1    ->  None
    | index -> Some index

        
/// <summary>
/// Return the index of the first element in the array
/// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
/// none of the elements satisfy the predicate.
/// </summary>
let findIndex (predicate : 'T -> bool) (resizeArray : ResizeArray<'T>) : int =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    match resizeArray.FindIndex ^ System.Predicate predicate with
    | -1    ->  raise <| System.Collections.Generic.KeyNotFoundException ()
    | index -> index


let indexOf elem (resizeArray : ResizeArray<'T>) =
    resizeArray.FindIndex ^ System.Predicate ((=) elem)


/// Return the index of the first element in the array
/// that satisfies the given predicate.
let tryFindIndexi predicate (resizeArray : ResizeArray<'T>) : int option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let predicate = FSharpFunc<_,_,_>.Adapt predicate

    let lastIndex = length resizeArray - 1
    let mutable index = -1
    let mutable foundMatch = false
    while index < lastIndex && not foundMatch do
        let i = index + 1
        index <- i
        foundMatch <- predicate.Invoke (i, resizeArray.[i])

    if foundMatch then
        Some index
    else None

/// <summary>
/// Return the index of the first element in the array
/// that satisfies the given predicate. Raise <c>KeyNotFoundException</c> if 
/// none of the elements satisfy the predicate.
/// </summary>
let findIndexi predicate (resizeArray : ResizeArray<'T>) : int =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    match tryFindIndexi predicate resizeArray with
    | Some index ->
        index
    | None ->
        keyNotFound "An element satisfying the predicate was not found in the collection."

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where function returns <c>Some(x)</c> for some x. If the function
/// never returns <c>Some(x)</c>, returns <c>None</c>.
/// </summary>
let tryPick (picker : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : 'U option =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let count = resizeArray.Count
    let mutable result = None
    let mutable index = 0

    while index < count && Option.isNone result do
        result <- picker resizeArray.[index]
        index <- index + 1
    result


let swap (index1:int) (index2:int) (resizeArray : ResizeArray<'T>) =
    checkNonNull "resizeArray" resizeArray
    if index1 < 0 || index1 > resizeArray.Count then
        argOutOfRange "index1" (sprintf "'%i' - is outside the range of the resizeArray" index1)
    if index2 < 0 || index2 > resizeArray.Count then
        argOutOfRange "index1" (sprintf "'%i' - is outside the range of the resizeArray" index2)

    let t1,t2 = resizeArray.[index1], resizeArray.[index2]
    resizeArray.[index1] <- t2
    resizeArray.[index2] <- t1
    resizeArray

/// <summary>
/// Applies the given function to successive elements, returning the first
/// result where function returns <c>Some(x)</c> for some x. If the function
/// never returns <c>Some(x)</c>, raises KeyNotFoundException.
/// </summary>
let pick (picker : 'T -> 'U option) (resizeArray : ResizeArray<'T>) : 'U =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let count = resizeArray.Count
    let mutable result = None
    let mutable index = 0

    while index < count && Option.isNone result do
        result <- picker resizeArray.[index]
        index <- index + 1

    match result with
    | Some result ->
        result
    | None ->
        // TODO : Return a better error message
        //keyNotFound ""
        raise <| System.Collections.Generic.KeyNotFoundException ()

/// Apply the given function to each element of the array.
let iter (action : 'T -> unit) (resizeArray : ResizeArray<'T>) : unit =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let count = resizeArray.Count
    for i = 0 to count - 1 do
        action resizeArray.[i]

/// Apply the given function to each element of the array. The integer passed to the
/// function indicates the index of element.
let iteri (action : int -> 'T -> unit) (resizeArray : ResizeArray<'T>) : unit =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let action = FSharpFunc<_,_,_>.Adapt action

    let count = resizeArray.Count
    for i = 0 to count - 1 do
        action.Invoke (i, resizeArray.[i])



/// <summary>
/// Build a new array whose elements are the results of applying the given function
/// to each of the elements of the array.
/// </summary>
/// <param name="mapping"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
let inline map (mapping : 'T -> 'U) (resizeArray : ResizeArray<'T>) : ResizeArray<'U> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let len = length resizeArray
    let res = ResizeArray<_>(len)
    for i = 0 to len - 1 do
        res.Add(mapping resizeArray.[i])
    res    



/// <summary>
/// Split the collection into two collections, containing the elements for which
/// the given predicate returns <c>true</c> and <c>false</c> respectively.
/// </summary>
/// <param name="predicate"></param>
/// <param name="resizeArray"></param>
/// <returns></returns>
let partition predicate (resizeArray : ResizeArray<'T>) : ResizeArray<'T> * ResizeArray<'T> =
    // Preconditions
    checkNonNull "resizeArray" resizeArray

    let trueResults = ResizeArray ()
    let falseResults = ResizeArray ()

    let len = length resizeArray
    for i = 0 to len - 1 do
        let el = resizeArray.[i]
        if predicate el then
            trueResults.Add el
        else
            falseResults.Add el

    trueResults, falseResults


/// <summary>Returns the greatest of all elements of the ResizeArray, compared via Operators.max on the function result.</summary>
/// <param name="resizeArray">The input ResizeArray.</param>
/// <returns>The maximum element.</returns>
/// <exception cref="System.ArgumentException">Thrown when <paramref name="resizeArray"/> is empty.</exception>
let inline max (resizeArray : ResizeArray<'T>) =
    // Preconditions
    checkNonNull "resizeArray" resizeArray
    if resizeArray.Count = 0 then
        invalidArg "resizeArray" "The input collection is empty."

    let mutable acc = resizeArray.[0]
    for i = 1 to resizeArray.Count - 1 do
        let curr = resizeArray.[i]
        if curr > acc then
            acc <- curr
    acc
