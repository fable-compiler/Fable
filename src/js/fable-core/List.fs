module List

let rec FoldIndexedAux f i acc = function
   | [] -> acc
   | x::xs -> FoldIndexedAux f (i+1) (f i acc x) xs

let FoldIndexed<'a,'acc> f (seed:'acc) (xs: 'a list) =
   FoldIndexedAux f 0 seed xs

let Fold<'a,'acc> f (seed:'acc) (xs: 'a list) =
   FoldIndexed (fun _ acc x -> f acc x) seed xs

let Reverse xs =
   Fold (fun acc x -> x::acc) [] xs

let FoldBack<'a,'acc> f (xs: 'a list) (seed:'acc) =
   Fold (fun acc x -> f x acc) seed (Reverse xs)

let rec FoldIndexed2Aux f i acc bs cs =
   match bs, cs with
   | [], [] -> acc
   | x::xs, y::ys -> FoldIndexed2Aux f (i+1) (f i acc x y) xs ys
   | _ -> invalidOp "Lists had different lengths"

let FoldIndexed2<'a, 'b, 'acc> f (seed:'acc) (xs: 'a list) (ys: 'b list) =
   FoldIndexed2Aux f 0 seed xs ys

let Fold2<'a, 'b, 'acc> f (seed:'acc) (xs: 'a list) (ys: 'b list) =
   FoldIndexed2 (fun _ acc x y -> f acc x y) seed xs ys

let FoldBack2<'a, 'b, 'acc> f (xs: 'a list) (ys: 'b list) (seed:'acc) =
   Fold2 f seed (Reverse xs) (Reverse ys)

let rec FoldIndexed3Aux f i acc bs cs ds =
   match bs, cs, ds with
   | [], [], [] -> acc
   | x::xs, y::ys, z::zs -> FoldIndexed3Aux f (i+1) (f i acc x y z) xs ys zs
   | _ -> invalidOp "Lists had different lengths"

let FoldIndexed3<'a, 'b, 'c, 'acc> f (seed:'acc) (xs: 'a list) (ys: 'b list) (zs: 'c list) =
   FoldIndexed3Aux f 0 seed xs ys zs

let Fold3<'a, 'b, 'c, 'acc> f (seed:'acc) (xs: 'a list) (ys: 'b list) (zs: 'c list) =
   FoldIndexed3 (fun _ acc x y z -> f acc x y z) seed xs ys zs

let Scan<'a, 'acc> f (seed:'acc) (xs: 'a list) =
   Fold (fun acc x ->
      match acc with
      | [] -> failwith "never"
      | y::_ -> f y x::acc) [seed] xs
   |> Reverse

let ScanBack<'a, 'acc> f (xs: 'a list) (seed:'acc) =
   Scan (fun acc x -> f x acc) seed (Reverse xs)
   |> Reverse

let Length xs =
   Fold (fun acc _ -> acc + 1) 0 xs

let Append xs ys =
   Fold (fun acc x -> x::acc) ys (Reverse xs)

let Collect f xs =
   Fold (fun acc x -> Append (f x) acc) [] (Reverse xs)

let Map f xs =
   Fold (fun acc x -> (f x::acc)) [] xs
   |> Reverse

let MapIndexed f xs =
   FoldIndexed (fun i acc x -> f i x::acc) [] xs
   |> Reverse

let Map2 f xs ys =
   Fold2 (fun acc x y -> f x y::acc) [] xs ys
   |> Reverse

let MapIndexed2 f xs ys =
   FoldIndexed2 (fun i acc x y  -> f i x y:: acc) [] xs ys
   |> Reverse

let Map3 f xs ys zs =
   Fold3 (fun acc x y z -> f x y z::acc) [] xs ys zs
   |> Reverse

let MapIndexed3 f xs ys zs =
   FoldIndexed3 (fun i acc x y z -> f i x y z:: acc) [] xs ys zs
   |> Reverse

let Iterate f xs =
   Fold (fun () x -> f x) () xs

let Iterate2 f xs ys =
   Fold2 (fun () x y -> f x y) () xs ys

let IterateIndexed f xs =
   FoldIndexed (fun i () x -> f i x) () xs

let IterateIndexed2 f xs ys =
   FoldIndexed2 (fun i () x y -> f i x y) () xs ys

// TODO
// let OfArray xs =
//    Array.FoldBack (fun x acc -> x::acc) xs []

// let ToArray xs =
//    let size = Length xs
//    let ys = Array.ZeroCreate size
//    IterateIndexed (fun i x -> ys.[i] <- x) xs
//    ys

let Empty<'a> : 'a list = []

let IsEmpty = function
   | [] -> true
   | _ -> false

let rec TryPickIndexedAux f i = function
   | [] -> None
   | x::xs ->
      let result = f i x
      match result with
      | Some _ -> result
      | None -> TryPickIndexedAux f (i+1) xs

let TryPickIndexed f xs =
   TryPickIndexedAux f 0 xs

let TryPick f xs =
   TryPickIndexed (fun _ x -> f x) xs

let Pick f xs =
   match TryPick f xs with
   | None -> invalidOp "List did not contain any matching elements"
   | Some x -> x

let TryFindIndexed f xs =
   TryPickIndexed (fun i x -> if f i x then Some x else None) xs

let TryFind f xs =
   TryPickIndexed (fun _ x -> if f x then Some x else None) xs

let FindIndexed f xs =
   match TryFindIndexed f xs with
   | None -> invalidOp "List did not contain any matching elements"
   | Some x -> x

let Find f xs =
   FindIndexed (fun _ x -> f x) xs

let TryFindIndex f xs =
   TryPickIndexed (fun i x -> if f x then Some i else None) xs

let FindIndex f xs =
   match TryFindIndex f xs with
   | None -> invalidOp "List did not contain any matching elements"
   | Some x -> x

let Item n xs =
   FindIndexed (fun i _ -> n = i) xs

let Filter f xs =
   FoldBack (fun x acc ->
      if f x then x::acc
      else acc) xs []

let Partition f xs =
   Fold (fun (lacc, racc) x ->
      if f x then x::lacc, racc
      else lacc,x::racc) ([],[]) xs

let Choose f xs =
   Fold (fun acc x ->
      match f x with
      | Some y -> y:: acc
      | None -> acc) [] xs |> Reverse

let Initialize n f =
   let mutable xs = []
   for i = 1 to n do xs <- f (n - i):: xs
   xs

let Replicate n x =
   Initialize n (fun _ -> x)

let Reduce f = function
   | [] -> invalidOp "List was empty"
   | h::t -> Fold f h t

let ReduceBack f = function
   | [] -> invalidOp "List was empty"
   | h::t -> FoldBack f t h

let ForAll f xs =
   Fold (fun acc x -> acc && f x) true xs

let ForAll2 f xs ys =
   Fold2 (fun acc x y -> acc && f x y) true xs ys

let rec Exists f = function
   | [] -> false
   | x::xs -> f x || Exists f xs

let rec Exists2 f bs cs =
   match bs, cs with
   | [], [] -> false
   | x::xs, y::ys -> f x y || Exists2 f xs ys
   | _ -> invalidOp "Lists had different lengths"

let Unzip xs =
   FoldBack (fun (x, y) (lacc, racc) -> x::lacc, y::racc) xs ([],[])

let Unzip3 xs =
   FoldBack (fun (x, y, z) (lacc, macc, racc) -> x::lacc, y::macc, z::racc) xs ([],[],[])

let Zip xs ys =
   Map2 (fun x y -> x, y) xs ys

let Zip3 xs ys zs =
   Map3 (fun x y z -> x, y, z) xs ys zs

(*
let Sort xs =
   xs
   |> ToArray
   |> Array.Sort
   |> OfArray

let SortWith f xs =
   xs
   |> ToArray
   |> Array.SortWith f
   |> OfArray

let inline Sum (xs: ^a list) : ^a =
   Fold (fun acc x -> acc + x) GenericConstants.Zero xs

let inline SumBy (f:^a -> ^b) (xs: ^a list) : ^b =
   Fold (fun acc x -> acc + f x) GenericConstants.Zero xs

let inline MaxBy f xs =
   Reduce (fun x y -> if f y > f x then y else x) xs

let inline Max xs =
   Reduce max xs

let inline MinBy f xs =
   Reduce (fun x y -> if f y > f x then x else y) xs

let inline Min xs =
   Reduce min xs

let inline Average (zs: ^a list) : ^a =
   let total = Sum zs
   let count = SumBy (fun _ -> GenericConstants.One< ^a >) zs
   total / count

let inline AverageBy (g: ^a -> ^b ) (zs: ^a list) : ^b =
   let total = SumBy g zs
   let count = SumBy (fun _ -> GenericConstants.One< ^a >) zs
   total / count

let Permute f xs =
   xs
   |> ToArray
   |> Array.Permute f
   |> OfArray

let SortBy f xs =
   let ys = xs |> ToArray
   Array.sortInPlaceBy f ys
   ys |> OfArray
*)