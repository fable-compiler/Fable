module Aether

open System

//  Optics

/// Lens from 'a -> 'b.
type Lens<'a,'b> =
    ('a -> 'b) * ('b -> 'a -> 'a)

/// Prism from 'a -> 'b.
type Prism<'a,'b> =
    ('a -> 'b option) * ('b -> 'a -> 'a)

//  Morphisms

/// Isomorphism between 'a <> 'b.
type Isomorphism<'a,'b> =
    ('a -> 'b) * ('b -> 'a)

/// Epimorphism between 'a <> 'b.
type Epimorphism<'a,'b> =
    ('a -> 'b option) * ('b -> 'a)

/// Functions for composing lenses and prisms with other optics, which
/// returns a new lens or prism based on the optic composed. Open `Aether.Operators`
/// to use the infix operator forms of these compositions, which is significantly
/// less verbose.
[<RequireQualifiedAccess>]
module Compose =

    /// Static overloads of the composition function for lenses (>->).
    /// These functions do not generally need to be called directly, but will
    /// be used when calling Compose.optic.
    type Lens =
        | Lens with

        static member (>->) (Lens, (g2, s2): Lens<'b,'c>) =
            fun ((g1, s1): Lens<'a,'b>) ->
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a) : Lens<'a,'c>

        static member (>->) (Lens, (g2, s2): Prism<'b,'c>) =
            fun ((g1, s1): Lens<'a,'b>) ->
                (fun a -> g2 (g1 a)),
                (fun c a -> s1 (s2 c (g1 a)) a) : Prism<'a,'c>

        static member (>->) (Lens, (f, t): Isomorphism<'b,'c>) =
            fun ((g, s): Lens<'a,'b>) ->
                (fun a -> f (g a)),
                (fun c a -> s (t c) a) : Lens<'a,'c>

        static member (>->) (Lens, (f, t): Epimorphism<'b,'c>) =
            fun ((g, s): Lens<'a,'b>) ->
                (fun a -> f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

    /// Compose a lens with an optic or morphism.
    let inline lens l o =
        (Lens >-> o) l

    /// Static overloads of the composition function for prisms (>?>).
    /// These functions do not generally need to be called directly, but will
    /// be used when calling Compose.optic.
    type Prism =
        | Prism with

        static member (>?>) (Prism, (g2, s2): Lens<'b,'c>) =
            fun ((g1, s1): Prism<'a,'b>) ->
                (fun a -> Option.map g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Prism<'a,'c>

        static member (>?>) (Prism, (g2, s2): Prism<'b,'c>) =
            fun ((g1, s1): Prism<'a,'b>) ->
                (fun a -> Option.bind g2 (g1 a)),
                (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a
                                                                 | _ -> a) : Prism<'a,'c>

        static member (>?>) (Prism, (f, t): Isomorphism<'b,'c>) =
            fun ((g, s): Prism<'a,'b>) ->
                (fun a -> Option.map f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

        static member (>?>) (Prism, (f, t): Epimorphism<'b,'c>) =
            fun ((g, s): Prism<'a,'b>) ->
                (fun a -> Option.bind f (g a)),
                (fun c a -> s (t c) a) : Prism<'a,'c>

    /// Compose a prism with an optic or morphism.
    let inline prism p o =
        (Prism >?> o) p

/// Functions for using optics to operate on data structures, using the basic optic
/// operations of get, set and map. The functions are overloaded to take either lenses or
/// prisms, with the return type being inferred.
[<RequireQualifiedAccess>]
module Optic =

    /// Static overloads of the optic get function (^.). These functions do not generally
    /// need to be called directly, but will be used when calling Optic.get.
    type Get =
        | Get with

        static member (^.) (Get, (g, _): Lens<'a,'b>) =
            fun (a: 'a) ->
                g a : 'b

        static member (^.) (Get, (g, _): Prism<'a,'b>) =
            fun (a: 'a) ->
                g a : 'b option

    /// Get a value using an optic.
    let inline get optic target =
        (Get ^. optic) target

    /// Static overloads of the optic set function (^=). These functions do
    /// not generally need to be called directly, but will be used when calling
    /// Optic.set.
    type Set =
        | Set with

        static member (^=) (Set, (_, s): Lens<'a,'b>) =
            fun (b: 'b) ->
                s b : 'a -> 'a

        static member (^=) (Set, (_, s): Prism<'a,'b>) =
            fun (b: 'b) ->
                s b : 'a -> 'a

    /// Set a value using an optic.
    let inline set optic value =
        (Set ^= optic) value

    /// Static overloads of the optic map function (%=). These functions do not generally
    /// need to be called directly, but will be used when calling Optic.map.
    type Map =
        | Map with

        static member (^%) (Map, (g, s): Lens<'a,'b>) =
            fun (f: 'b -> 'b) ->
                (fun a -> s (f (g a)) a) : 'a -> 'a

        static member (^%) (Map, (g, s): Prism<'a,'b>) =
            fun (f: 'b -> 'b) ->
                (fun a -> Option.map f (g a) |> function | Some b -> s b a
                                                         | _ -> a) : 'a -> 'a

    /// Modify a value using an optic.
    let inline map optic f =
        (Map ^% optic) f

/// Functions for creating or using lenses.
[<RequireQualifiedAccess>]
module Lens =

    /// Converts an isomorphism into a lens.
    let ofIsomorphism ((f, t): Isomorphism<'a,'b>) : Lens<'a,'b> =
        f, (fun b _ -> t b)

/// Functions for creating or using prisms.
[<RequireQualifiedAccess>]
module Prism =

    /// Converts an epimorphism into a prism.
    let ofEpimorphism ((f, t): Epimorphism<'a,'b>) : Prism<'a,'b> =
        f, (fun b _ -> t b)

/// Various optics implemented for common types such as tuples,
/// lists and maps, along with an identity lens.
[<AutoOpen>]
module Optics =

    // Lens for the identity function (does not change the focus of operation).
    let id_ : Lens<'a,'a> =
        (fun x -> x),
        (fun x _ -> x)

    /// Isomorphism between a boxed and unboxed type.
    let box_<'a> : Isomorphism<obj,'a> =
        unbox<'a>, box

    /// Lens to the first item of a tuple.
    let fst_ : Lens<('a * 'b),'a> =
        fst,
        (fun a t -> a, snd t)

    /// Lens to the second item of a tuple.
    let snd_ : Lens<('a * 'b),'b> =
        snd,
        (fun b t -> fst t, b)

    [<RequireQualifiedAccess>]
    module Array =

        /// Isomorphism to an list.
        let list_ : Isomorphism<'v[], 'v list> =
            Array.toList,
            Array.ofList

    [<RequireQualifiedAccess>]
    module Choice =

        /// Prism to Choice1Of2.
        let choice1Of2_ : Prism<Choice<_,_>, _> =
            (fun x ->
                match x with
                | Choice1Of2 v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Choice1Of2 _ -> Choice1Of2 v
                | _ -> x)

        /// Prism to Choice2Of2.
        let choice2Of2_ : Prism<Choice<_,_>, _> =
            (fun x ->
                match x with
                | Choice2Of2 v -> Some v
                | _ -> None),
            (fun v x ->
                match x with
                | Choice2Of2 _ -> Choice2Of2 v
                | _ -> x)

    [<RequireQualifiedAccess>]
    module List =

        /// Prism to the head of a list.
        let head_ : Prism<'v list, 'v> =
            (function | h :: _ -> Some h
                      | _ -> None),
            (fun v ->
                function | _ :: t -> v :: t
                         | l -> l)

        /// Prism to an indexed position in a list.
        let pos_ (i: int) : Prism<'v list, 'v> =
#if NET35
            (function | l when List.length l > i -> Some (List.nth l i)
#else
            (function | l when List.length l > i -> Some (List.item i l)
#endif
                      | _ -> None),
            (fun v l ->
                List.mapi (fun i' x -> if i = i' then v else x) l)

        /// Prism to the tail of a list.
        let tail_ : Prism<'v list, 'v list> =
            (function | _ :: t -> Some t
                      | _ -> None),
            (fun t ->
                function | h :: _ -> h :: t
                         | [] -> [])

        /// Isomorphism to an array.
        let array_ : Isomorphism<'v list, 'v[]> =
            List.toArray,
            List.ofArray

    [<RequireQualifiedAccess>]
    module Map =

        /// Prism to a value associated with a key in a map.
        let key_ (k: 'k) : Prism<Map<'k,'v>,'v> =
            Map.tryFind k,
            (fun v x ->
                if Map.containsKey k x then Map.add k v x else x)

        /// Lens to a value option associated with a key in a map.
        let value_ (k: 'k) : Lens<Map<'k,'v>, 'v option> =
            Map.tryFind k,
            (fun v x ->
                match v with
                | Some v -> Map.add k v x
                | _ -> Map.remove k x)

        /// Weak Isomorphism to an array of key-value pairs.
        let array_ : Isomorphism<Map<'k,'v>, ('k * 'v)[]> =
            Map.toArray,
            Map.ofArray

        /// Weak Isomorphism to a list of key-value pairs.
        let list_ : Isomorphism<Map<'k,'v>, ('k * 'v) list> =
            Map.toList,
            Map.ofList

    [<RequireQualifiedAccess>]
    module Option =

        /// Prism to the value in an Option.
        let value_ : Prism<'v option, 'v> =
            id,
            (fun v ->
                function | Some _ -> Some v
                         | None -> None)

/// Optional custom operators for working with optics. Provides more concise
/// syntactic options for working with the functions in the `Compose` and
/// `Optic` modules.
module Operators =

    /// Compose a lens with an optic or morphism.
    let inline (>->) l o =
        Compose.lens l o

    /// Compose a prism with an optic or morphism.
    let inline (>?>) p o =
        Compose.prism p o

    /// Get a value using an optic.
    let inline (^.) target optic =
        Optic.get optic target

    /// Set a value using an optic.
    let inline (^=) value optic =
        Optic.set optic value

    /// Modify a value using an optic.
    let inline (^%) f optic =
        Optic.map optic f
