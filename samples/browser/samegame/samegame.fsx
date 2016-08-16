(**
 - title: SameGame
 - tagline: The famous tile-matching puzzle
 - intro: This demo shows a Fable implementation of SameGame. The functional implementation of the game follows the *type-first design* approach and consists of three main components: **types**, **game logic** and **front end**.
   This sample has been contributed by [Leif Battermann](https://twitter.com/leifbattermann).
   You can find the [full source code on GitHub](https://github.com/fable-compiler/Fable/blob/master/samples/browser/samegame/samegame.fsx).

*)

(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"

module PresetGames =
  let games = [
    [5;3;3;4;2;3;4;2;3;3;2;5;3;2;3;4;5;5;5;5;1;1;3;5;4;1;3;1;2;3;1;3;1;3;2;1;2;3;4;3;2;2;1;4;5;5;3;3;3;2;3;3;1;1;5;5;5;4;5;1;1;3;5;5;3;4;4;5;5;2;2;3;1;2;3;4;1;5;1;4;4;5;4;1;1;3;3;1;4;3;2;4;3;3;3;5;3;1;2;1;2;3;3;2;5;1;2;4;3;1;4;1;3;1;3;1;5;5;5;5;2;4;2;3;1;3;5;4;5;4;2;1;4;1;3;1;3;3;1;5;2;4;3;3;4;1;1;2;1;2;5;4;1;2;4;3;2;1;1;2;1;4;5;4;5;4;3;3;4;2;4;3;4;5;4;4;1;4;4;5;3;3;4;4;5;4;5;2;2;5;5;3;2;5;5;3;5;4;4;1;4;4;4;4;4;2;1;1;4;1;3;4;2;1;5;3;5;2;5;2;4;1;2;4;3];
    [4;1;3;4;4;2;5;3;2;5;2;3;2;3;3;5;4;5;5;1;4;1;1;4;3;5;4;1;2;5;1;5;1;3;4;3;3;3;4;1;3;1;1;1;2;3;4;4;5;1;1;1;4;1;1;3;1;5;1;5;4;4;2;2;5;3;3;5;5;5;3;1;1;4;4;3;5;5;2;1;1;1;1;1;5;5;1;5;2;1;2;3;3;5;3;4;5;5;2;5;5;4;5;1;4;5;5;1;3;4;4;2;4;1;4;4;4;1;2;5;3;4;4;4;3;1;4;2;1;1;3;5;4;2;1;3;1;1;2;5;1;5;2;3;2;1;4;2;1;5;5;4;3;2;3;2;5;1;3;4;1;3;5;3;2;4;4;2;1;1;3;4;4;3;5;3;2;2;3;3;2;2;2;2;1;4;3;2;5;4;4;1;3;4;5;3;1;5;2;3;3;3;2;5;2;2;5;1;2;1;1;4;4;1;2;5;2;4;3;3;2;2;3;2;3];
    [4;2;3;1;5;4;1;4;4;4;4;2;1;4;5;5;3;4;1;3;5;3;5;4;2;1;4;2;2;5;2;2;4;4;4;4;3;2;5;4;5;5;2;4;2;1;1;2;1;5;4;5;1;1;5;2;2;5;5;4;1;5;3;5;3;3;4;3;5;2;2;1;4;2;3;1;1;2;3;1;1;2;1;2;1;3;1;4;4;3;2;4;3;1;3;1;2;2;1;4;3;4;2;5;3;3;1;4;3;5;1;5;3;4;4;5;4;1;4;5;3;1;4;3;5;4;4;3;5;3;4;5;2;4;4;3;5;1;5;4;3;2;1;5;2;2;1;4;3;4;2;1;3;1;1;3;5;4;1;4;5;3;5;1;1;2;4;5;1;2;5;4;2;1;3;2;5;5;2;4;4;5;1;2;1;3;2;1;3;2;3;2;1;2;4;5;2;1;4;1;3;2;4;2;5;3;5;2;4;5;3;1;3;2;1;1;2;4;5;4;5;4;2;5;4];
    [2;2;4;1;1;4;3;5;4;2;5;5;1;5;3;3;4;4;2;3;1;1;1;5;2;3;4;4;3;2;3;3;5;1;3;1;2;1;3;2;1;4;4;5;1;4;5;2;3;4;5;3;5;2;3;1;1;5;5;3;1;4;2;3;5;3;5;1;3;2;2;4;1;1;3;2;2;2;5;3;4;2;2;5;2;5;3;4;4;2;1;2;1;1;1;2;3;2;5;4;4;5;4;1;2;2;3;5;4;1;5;4;1;2;5;4;2;3;1;5;5;2;3;2;2;5;3;5;5;1;5;1;2;1;4;4;4;1;3;1;3;4;1;3;3;2;2;5;2;2;5;5;1;1;4;3;3;2;5;2;4;4;5;3;1;4;2;5;3;1;1;2;5;1;3;2;2;5;5;5;4;4;4;4;1;1;5;2;2;2;2;3;1;2;3;4;5;3;2;4;5;2;2;2;2;4;1;1;3;3;2;4;2;1;4;5;5;1;3;1;4;5;3;2;5];
    [5;3;5;3;4;4;3;3;5;5;3;2;2;5;1;2;3;2;3;2;5;1;4;5;2;1;4;1;2;4;1;2;2;3;1;3;3;4;1;4;4;4;1;1;3;4;2;4;3;1;4;3;5;5;4;4;4;5;1;3;1;2;2;1;4;1;5;5;4;4;4;4;1;4;4;1;5;4;2;3;5;1;4;1;1;3;3;3;4;3;4;3;3;4;3;2;3;1;2;2;5;3;5;4;2;5;2;1;2;4;4;1;4;3;4;1;4;1;5;2;4;3;1;2;5;1;2;3;2;3;4;5;2;1;4;2;1;1;5;2;1;2;1;1;4;3;5;5;5;2;3;3;1;5;4;2;3;4;4;1;4;5;5;5;1;5;5;3;5;3;1;4;2;4;1;2;2;5;4;3;1;2;4;5;5;1;5;2;2;3;4;3;2;4;4;2;3;2;1;5;4;3;1;3;2;5;1;3;4;2;5;4;1;2;1;2;2;5;1;5;1;3;1;3;5];
    [3;3;3;4;5;5;2;4;4;3;4;2;2;2;1;1;4;4;3;4;2;2;1;1;4;5;3;2;4;2;3;2;4;3;4;3;4;3;1;5;4;4;1;2;1;4;1;3;3;3;4;3;2;3;4;2;2;3;1;5;2;5;5;3;5;3;4;4;4;3;2;1;4;4;5;4;1;5;5;5;4;1;5;2;4;1;1;1;5;3;1;2;2;1;3;5;4;4;2;4;1;2;5;5;2;3;4;3;4;1;1;3;3;2;2;2;5;4;5;5;2;4;5;1;2;1;5;3;1;5;5;3;3;2;4;3;1;1;1;1;2;3;3;5;3;4;5;1;5;2;5;1;5;3;2;2;2;5;3;1;4;2;2;4;1;3;5;1;3;4;1;5;4;4;4;5;2;1;4;4;1;4;3;5;1;4;3;1;5;2;1;2;3;5;2;5;1;4;4;5;5;4;3;1;1;5;3;1;5;3;1;2;1;5;5;5;4;3;3;2;1;5;2;1;4];
    [4;1;1;4;1;1;2;3;5;5;3;4;2;2;5;1;3;1;1;4;2;4;2;2;3;3;1;4;5;2;3;5;1;1;1;1;2;3;1;4;3;2;2;3;5;4;2;4;1;4;1;5;4;4;2;1;4;5;2;3;5;3;4;4;2;3;4;2;1;5;4;1;1;3;1;5;3;5;3;4;2;3;4;1;1;3;2;3;4;1;2;3;2;4;4;1;4;3;1;2;1;1;4;2;1;1;5;2;3;2;3;4;2;3;1;3;5;4;3;5;2;1;3;1;3;1;5;3;4;1;2;5;5;2;4;2;4;3;5;1;4;5;1;3;3;3;2;4;3;1;2;5;5;1;5;1;3;3;4;5;2;2;3;2;5;3;5;1;5;5;5;2;4;3;1;2;5;1;4;1;5;1;5;4;5;2;1;4;2;4;4;2;5;1;4;2;3;4;3;5;4;1;4;4;2;5;2;2;5;4;1;2;5;4;5;1;3;5;3;4;3;1;5;1;1];
    [3;4;1;3;1;3;3;1;4;2;3;5;5;4;5;2;2;3;1;1;3;5;2;5;5;4;3;3;4;3;5;4;2;2;5;3;5;3;2;2;4;5;2;4;2;3;3;1;4;3;2;1;4;4;2;3;3;1;4;3;1;5;2;2;3;4;4;4;4;3;2;4;2;5;3;4;3;4;3;2;2;3;5;2;1;4;1;3;1;3;5;5;5;4;2;5;3;5;1;4;5;5;3;2;1;3;5;4;2;4;1;5;3;2;2;4;1;1;1;5;3;1;4;2;3;3;5;3;4;3;1;3;3;5;2;3;1;3;2;4;4;2;2;2;2;5;2;3;3;3;4;3;4;2;1;4;1;1;5;3;4;3;1;4;3;3;2;4;5;5;4;4;5;3;1;2;5;2;5;3;2;2;5;1;5;4;4;2;5;4;2;5;4;4;5;1;1;3;4;5;4;4;3;4;5;1;3;2;1;4;5;5;1;2;2;3;3;1;5;5;4;1;4;2;4];
    [2;1;1;5;1;5;3;5;1;2;2;3;3;1;2;4;3;5;5;1;1;1;1;2;3;4;3;1;5;5;4;4;2;2;5;2;5;4;5;4;4;5;5;1;3;4;5;3;3;1;5;4;2;3;3;2;1;4;3;2;3;3;5;1;4;3;2;1;1;5;5;4;5;2;3;1;1;5;5;4;5;1;3;4;1;1;2;5;5;4;1;2;2;4;2;1;1;5;2;5;2;4;2;3;2;1;4;1;5;2;1;1;4;4;2;2;5;2;5;4;1;3;1;3;5;3;3;2;1;2;1;2;3;5;2;1;5;5;3;5;3;2;5;4;4;1;3;3;2;5;2;4;5;4;5;5;3;5;4;4;3;2;5;4;1;4;3;5;2;2;3;5;4;5;2;5;4;3;4;5;1;1;5;1;1;2;2;5;1;3;3;5;1;4;3;1;2;1;3;5;3;3;1;1;3;2;1;1;3;5;2;1;1;5;4;3;2;5;3;4;3;4;5;5;4];
    [5;3;1;1;5;5;1;2;4;2;5;5;5;5;2;2;5;5;1;2;1;1;2;3;4;1;5;3;5;2;1;4;2;1;1;4;4;4;5;3;3;2;3;4;1;1;3;4;5;1;4;1;1;4;4;5;2;3;2;5;5;4;3;2;2;1;3;4;4;4;3;2;1;4;4;5;4;4;2;3;3;2;4;5;4;1;4;3;2;2;2;1;2;1;3;5;5;1;3;1;4;5;5;2;3;3;1;2;1;5;1;4;1;4;2;4;5;3;4;2;3;5;2;3;2;2;5;1;2;4;1;3;1;2;5;4;2;3;2;1;1;2;3;3;1;4;5;3;4;4;5;2;2;2;4;4;3;2;5;1;3;5;1;3;4;4;3;5;5;4;2;3;2;4;2;1;3;4;3;3;2;2;3;3;4;5;2;5;3;2;1;1;3;1;4;1;1;5;5;3;1;1;4;1;2;3;1;4;1;3;1;5;5;3;4;2;2;4;5;5;3;3;2;3;4]
  ]

(**
Rules
-----

SameGame is a single player game. It is played on a two-dimensional board filled with stones of different colors.

The player may remove groups of stones from the board.

A group of stones is defined by two or more orthogonally connected identical-colored stones.

After a group is removed, all the stones above will fall down.

If a column is cleared completely, the columns to the right will slide to the left to close the gap.

The game ends when the board is either empty or the remaining stones cannot be removed.

Scoring
-------

Removing a group of *n* stones will result in a score of *(n-2)<sup>2</sup>* points.

If all stones are removed from the board, the player receives a bonus of 1000 points.

If the game ends without clearing the board, the player will receive a penalty. The penalty is computed according to *(n-2)<sup>2</sup>* where *n* is the number of stones left on the board.
*)


(**

Type-first design
-----------------

The module `SameGameTypes` contains the definitions of all the types needed for the game. There is no implementation yet.

It is the goal to enforce the rules of the game through the types and make invalid state unrepresentable as much as possible.
*)

module SameGameTypes =

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

(**

Implementation of the game logic
--------------------------------

The module `SameGameDomain` contains the actual implementation of the game logic.

It exposes a public property `api` of type `SameGameApi` that provides an API for player interactions.

Note that the implementation of the game logic doesn't contain any front end code.
*)

module SameGameDomain =

    open System
    open SameGameTypes

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

(**
Front end with Fable
--------------------

The UI implementation is based on HTML and style sheets. The SameGame board e.g. is rendered as an HTML table.

This is the function that renders a board to an HTML string:

*)

open Fable.Core 
open Fable.Import.Browser
open SameGameTypes

let api = SameGameDomain.api

// val renderBoardToHtmlString : board:Board -> string
let renderBoardToHtmlString (board:Board) =
    let renderCell x y col =
        "<td class='sg-td'>"
        + sprintf "<a href='javaScript:void(0);' id='cell-%d-%d'>" x y 
        + sprintf "<div class='sg-cell sg-color%d'>" col
        + "</div></a></td>"

    let makeBoard (board: int list list) = 
        "<table class='sg-table horiz-centered'>"
        + String.concat "" [
            for y in [board.[0].Length - 1 .. -1 .. 0] do
                yield "<tr class='sg-tr'>"
                    + ([0..(board.Length - 1)]
                        |> List.map (fun x -> renderCell x y board.[x].[y])
                        |> String.concat "")
                    + "</tr>"
            ]
        + "</table>"

    makeBoard (board |> List.map (fun col ->
        col |> List.map (function Stone (Color c) -> c | Empty -> 0)))

(** 

The function `updateUi` is responsible for displaying the game and integrating the user interactions. These are the steps for updating the UI:

1. The HTML elements for displaying the board and the score are obtained.
2. A nested function `addListeners` for adding listeners for click events for all table cells is defined. The handlers will play a move and then recursively call `updateUi` again to update the UI with the new game state.
3. A pattern match of the game state is performed. Depending on the state, the board will be updated, event listeners will be added, and the score will be updated.

*)

let getById<'T when 'T :> HTMLElement> id =
    document.getElementById(id) :?> 'T

// val updateUi : game:Game option -> unit
let rec updateUi game =
    let boardElement = getById<HTMLDivElement>("sg-board")
    let scoreElement = getById<HTMLDivElement> ("sg-score")

    let play game (x,y) =
        game
        |> Core.Option.map (fun g ->
            api.Play g { Col = x; Row = y })
        |> updateUi

    let addListeners maxColIndex maxRowIndex  =
        [0..maxColIndex] |> List.iter (fun x ->
            [0..maxRowIndex] |> List.iter (fun y -> 
                let cellId = sprintf "cell-%d-%d" x y
                let el = getById<HTMLButtonElement>(cellId)
                el.addEventListener_click(fun _ ->
                    play game (x,y); null)))
    
    match game with
    | Some (InProgress gs) -> 
        let board = renderBoardToHtmlString gs.Board
        boardElement.innerHTML <- board
        addListeners (gs.Board.Length - 1) (gs.Board.[0].Length - 1)
        scoreElement.innerText <- sprintf "%i point(s)." gs.Score
    | Some (Finished gs) -> 
        let board = renderBoardToHtmlString gs.Board
        boardElement.innerHTML <- board
        scoreElement.innerText <- "No more moves. " +
            sprintf "Your final score is %i point(s)." gs.Score
    | _ -> boardElement.innerText <-
            "Sorry, an error occurred while rendering the board."

(**

The configuration of the board is obtained by parsing the `class` attribute of the `div` element that contains the board. E.g. `<div id="sg-board" class="15-15-5"></div>` will be parsed to a 15 x 15 board with 5 different colors.

*)

let rndColorGtor i = 
    let rnd = new System.Random()
    fun () -> rnd.Next(i) + 1 |> Color |> Stone 

let defaultConfig =  
    (getById<HTMLDivElement>("sg-board")).className
    |> fun className -> className.Split('-') 
    |> Array.map int
    |> fun arr ->
        { NumberOfColumns = arr.[0]
          NumberOfRows = arr.[1]
          StoneGenerator = rndColorGtor arr.[2] }

(**

The handlers for starting a new game and for selecting a game from a list of preset initial game positions are defined and added.

*)

let buttonNewGame = getById<HTMLButtonElement>("new-game") 
let selectGame = getById<HTMLSelectElement>("sg-select-game") 
let selectWidth = getById<HTMLSelectElement>("sg-select-w") 
let selectHeight = getById<HTMLSelectElement>("sg-select-h") 
let selectColors = getById<HTMLSelectElement>("sg-select-col") 

let config() =
    { NumberOfColumns = int selectWidth.value
      NumberOfRows = int selectHeight.value
      StoneGenerator = int selectColors.value |> rndColorGtor }

let newGameOnClick() =
    let game = config() |> api.NewGame
    selectGame.selectedIndex <- 0.0 
    updateUi game
    
let selectGameOnChange () =
    let presetGtor gameNum =
        let mutable index = 0;
        let game = PresetGames.games.[gameNum]
        fun () ->
            index <- index + 1
            game.[index-1] |> Color |> Stone

    let gameIndex = int selectGame.value
    if gameIndex >= 0 then
        { config() with StoneGenerator = presetGtor gameIndex }
        |> api.NewGame
        |> updateUi

selectGame.addEventListener_change(fun _ -> selectGameOnChange(); null)
buttonNewGame.addEventListener_click(fun _ -> newGameOnClick(); null)

(**

Finally the game is initialized with:

*)

api.NewGame defaultConfig |> updateUi 
