/// This module tests Seq operations in a complex, realistic setting:
/// a constraint-propagation Sudoku solver.
module Fable.Tests.Sudoku

open Util.Testing

type Box = int
type Sudoku = Box array array

let rows sudoku = sudoku
let cols (sudoku:Sudoku) =
    sudoku
    |> Array.mapi (fun a row -> row |> Array.mapi (fun b cell -> sudoku.[b].[a]))

let getBoxIndex count row col =
   let n = row/count
   let m = col/count
   n * count + m

let boxes (sudoku:Sudoku) =
    let size = Array.length sudoku
    let d = size |> float |> sqrt |> int
    [| for boxIdx in 0..size-1 ->
        [| for a in 0..size-1 do
            for b in 0..size-1 do
                if getBoxIndex d a b = boxIdx then
                    yield sudoku.[a].[b] |] |]
    |> Seq.ofArray

let toSudoku x : Sudoku =
    x
    |> Seq.map Seq.toArray
    |> Seq.toArray

let allUnique numbers =
    let filtered = numbers |> Seq.filter (fun x -> x <> 0) |> Seq.toList
    let distinct = filtered |> Seq.distinct |> Seq.toList
    List.length filtered = List.length distinct

let solvable sudoku =
    rows sudoku
    |> Seq.append (cols sudoku)
    |> Seq.append (boxes sudoku)
    |> Seq.forall allUnique

let replaceAtPos (x:Sudoku) row col newValue :Sudoku =
    [| for a in 0..(Array.length x - 1) ->
        [| for b in 0..(Array.length x - 1) ->
            if a = row && b = col then newValue else x.[a].[b] |] |]

let rec substitute row col (x:Sudoku) =
    let a,b = if col >= Array.length x then row+1,0 else row,col
    if a >= Array.length x then seq { yield x } else
    if x.[a].[b] = 0 then
        [1..Array.length x]
            |> Seq.map (replaceAtPos x a b)
            |> Seq.filter solvable
            |> Seq.map (substitute a (b+1))
            |> Seq.concat
     else substitute a (b+1) x

let getFirstSolution sudoku = sudoku |> substitute 0 0 |> Seq.head

[<Fact>]
let ``test Sudoku solver works`` () =
    let solution =
        [[0; 0; 8;  3; 0; 0;  6; 0; 0]
         [0; 0; 4;  0; 0; 0;  0; 1; 0]
         [6; 7; 0;  0; 8; 0;  0; 0; 0]

         [0; 1; 6;  4; 3; 0;  0; 0; 0]
         [0; 0; 0;  7; 9; 0;  0; 2; 0]
         [0; 9; 0;  0; 0; 0;  4; 0; 1]

         [0; 0; 0;  9; 1; 0;  0; 0; 5]
         [0; 0; 3;  0; 5; 0;  0; 0; 2]
         [0; 5; 0;  0; 0; 0;  0; 7; 4]]
            |> toSudoku
            |> getFirstSolution

    let expectedSolution =
        [[1; 2; 8;  3; 4; 5;  6; 9; 7]
         [5; 3; 4;  6; 7; 9;  2; 1; 8]
         [6; 7; 9;  1; 8; 2;  5; 4; 3]

         [2; 1; 6;  4; 3; 8;  7; 5; 9]
         [4; 8; 5;  7; 9; 1;  3; 2; 6]
         [3; 9; 7;  5; 2; 6;  4; 8; 1]

         [7; 6; 2;  9; 1; 4;  8; 3; 5]
         [9; 4; 3;  8; 5; 7;  1; 6; 2]
         [8; 5; 1;  2; 6; 3;  9; 7; 4]]
            |> toSudoku

    solution = expectedSolution |> equal true
