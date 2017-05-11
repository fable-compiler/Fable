module SameGame.Domain

open System
open SameGame.Types

let private square x = x * x

let private bonus = 1000

let private calcScore groupSize =
    square (groupSize - 2)

let private penalty stonesLeft =
    -(square (stonesLeft - 2))

let private getCellState (board:Board) pos =
    let colCount = board |> List.length
    if pos.Col < colCount
        && pos.Col >= 0
        && pos.Row < board.[pos.Col].Length
        && pos.Row >= 0
    then board.[pos.Col].[pos.Row]
    else Empty

let private findAdjacentWithSameColor board col (pos:Position) =
    [pos.Up; pos.Right; pos.Down; pos.Left]
    |> List.map (fun p ->  getCellState board p, p)
    |> List.filter (fun cell -> fst cell = Stone col)
    |> List.map snd

let private hasValidMoves board =
    board
    |> Seq.mapi (fun i col ->
        col
        |> Seq.mapi (fun j cell ->
            { Position = { Col = i; Row = j }; State = cell}))
    |> Seq.exists (fun col ->
        col
        |> Seq.exists (fun cell ->
            match cell.State with
            | Stone c ->
                cell.Position
                |> findAdjacentWithSameColor board c
                |> (not << List.isEmpty)
            | _ -> false))

let private numberOfStones board =
    let numOfStonesInCol =
        List.sumBy (function Stone c -> 1 | Empty -> 0)
    board |> List.map numOfStonesInCol |> List.sum

let private isEmpty (board:Board) =
    board |> List.forall (List.head >> ((=) Empty))

let private evaluateGameState gameState =
    if gameState.Board |> hasValidMoves
    then InProgress gameState
    elif gameState.Board |> isEmpty
    then Finished { gameState with Score = gameState.Score + bonus }
    else
        let score =
            gameState.Score
            + (gameState.Board |> numberOfStones |> penalty)
        Finished { gameState with Score = score }

let private getGroup board position =
    let rec find (ps:Position list) col (group:Position list) =
        match ps with
        | [] -> group
        | x::xs ->
            let cells =
                x
                |> findAdjacentWithSameColor board col
                |> List.filter (fun pos ->
                    not (List.exists ((=) pos) (xs @ group) ))
            find (cells @ xs) col (x :: group)

    getCellState board position
    |> function
        | Stone c ->
            let positions = find [position] c []
            if positions |> List.length > 1
            then Some { Color = c; Positions = positions }
            else None
        | _ -> None

let private removeGroup group board =
    board
    |> List.mapi (fun i col ->
        col
        |> List.mapi (fun j cell ->
            { Position = { Col = i; Row = j }; State = cell})
        |> List.filter (fun cell ->
            group.Positions
            |> (not << List.exists ((=) cell.Position)))
        |> List.map (fun cell -> cell.State)
        |> fun col' ->
            col' @ List.replicate (col.Length - col'.Length) Empty)
    |> List.filter (List.head >> ((<>) Empty))
    |> fun cols -> cols @ List.replicate
                    (board.Length - cols.Length)
                    (List.replicate (board.[0].Length) Empty)

let private play gameState pos =
    getGroup gameState.Board pos
    |> function
        | Some g ->
            let newBoard = gameState.Board |> removeGroup g
            { Board = newBoard
              Score = gameState.Score + calcScore g.Positions.Length }
        | _ -> gameState

let private playIfRunning game pos =
    match game with
    | InProgress gameState ->
        play gameState pos |> evaluateGameState
    | _ -> game

let private isValid conf =
    if conf.NumberOfColumns < 3 || conf.NumberOfColumns > 15
    then false
    elif conf.NumberOfRows < 3 || conf.NumberOfRows > 15
    then false
    else true

let private newGame config =
    let createBoard config =
        List.init config.NumberOfColumns (fun _ ->
            List.init config.NumberOfRows (fun _ ->
                config.StoneGenerator()))
        |> fun board -> { Board = board; Score = 0 }
        |> evaluateGameState |> Some
    if config |> isValid
    then createBoard config
    else None

let api = {
    NewGame = newGame
    Play = playIfRunning }