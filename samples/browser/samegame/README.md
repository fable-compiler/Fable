# SameGame with Fable
This demo shows a Fable implementation of SameGame. The functional implementation of the game follows the *type-first design* approach and consists of three main components: **types**, **game logic** and **front end**. This sample has been contributed by [Leif Battermann](https://twitter.com/leifbattermann).


## Build and running the app

1. Install npm dependencies: `npm install` or `yarn install`
2. Install dotnet dependencies: `dotnet restore`
3. Start Fable server and Webpack dev server: `dotnet fable npm-run start`
4. In your browser, open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

# Rules
SameGame is a single player game. It is played on a two-dimensional board filled with stones of different colors.

The player may remove groups of stones from the board.

A group of stones is defined by two or more orthogonally connected identical-colored stones.

After a group is removed, all the stones above will fall down.

If a column is cleared completely, the columns to the right will slide to the left to close the gap.

The game ends when the board is either empty or the remaining stones cannot be removed.
# Scoring
Removing a group of *n* stones will result in a score of *(n-2)<sup>2</sup>* points.

If all stones are removed from the board, the player receives a bonus of 1000 points.

If the game ends without clearing the board, the player will receive a penalty. The penalty is computed according to *(n-2)<sup>2</sup>* where *n* is the number of stones left on the board.

# Type-first design
The module `SameGame.Types` contains the definitions of all the types needed for the game. There is no implementation yet.

It is the goal to enforce the rules of the game through the types and make invalid state unrepresentable as much as possible.
```fs
module SameGame.Types

type Position = {
    Col:int
    Row:int } with

    member this.Left = { this with Col = this.Col  - 1 }
    member this.Right = { this with Col = this.Col  + 1 }
    member this.Up = { this with Row = this.Row  + 1 }
    member this.Down = { this with Row = this.Row  - 1 }

type Color = Color of int

type CellState =
    | Stone of Color
    | Empty

type Column = CellState list

type Board = Column list

type Cell = {
    Position:Position
    State:CellState }

type Group = {
    Color:Color
    Positions: Position list }

type Game =
    | InProgress of GameState
    | Finished of GameState

and GameState = {
    Board:Board
    Score:int }

/// This is usually a function that produces (pseudo) random colors.
/// It can also be used to create a specific initial board position.
type StoneGenerator = unit-> CellState

type GameConfig = {
    NumberOfColumns:int
    NumberOfRows:int
    StoneGenerator:StoneGenerator }

type SameGameApi = {
    NewGame: GameConfig -> Game option
    Play: Game -> Position -> Game }
```
# Implementation of the game logic
The module `SameGame.Domain` contains the actual implementation of the game logic.

It exposes a public property `api` of type `SameGameApi` that provides an API for player interactions.

Note that the implementation of the game logic doesn't contain any front end code.
```fs
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
```
