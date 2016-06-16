(**
 - title: SameGame
 - tagline: The famous tile-matching puzzle
 - intro: This demo shows a Fable implementation of SameGame. The functional implementation of the game follows the *type-first design* approach and consists of three main components: **types**, **game logic** and **front end**.

   You can find the [full source code on GitHub](https://github.com/fsprojects/Fable/blob/master/samples/browser/samegame/samegame.fsx).

*)

(*** hide ***)
#r "node_modules/fable-core/Fable.Core.dll"
#load "games/games.fsx"
open Games

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

For the sake of brevity the code is not shown here but can be viewed on [GitHub](https://github.com/fsprojects/Fable/blob/master/samples/browser/samegame/samegame.fsx).

Note that the implementation of the game logic doesn't contain any front end code.
*)

(*** hide ***)
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
        if pos.Col < colCount && pos.Col >= 0 && pos.Row < board.[pos.Col].Length && pos.Row >= 0 then
            board.[pos.Col].[pos.Row]
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
            |> Seq.mapi (fun j cell -> { Position = { Col = i; Row = j }; State = cell}))
        |> Seq.exists (fun col -> 
            col 
            |> Seq.exists (fun cell -> 
                match cell.State with 
                | Stone c -> cell.Position |> findAdjacentWithSameColor board c  |> (not << List.isEmpty) 
                | _       -> false))

    let private numberOfStones board =
        let numOfStonesInCol = List.sumBy (function Stone c -> 1 | Empty -> 0)
        board |> List.map numOfStonesInCol |> List.sum

    let private isEmpty (board:Board) = board |> List.forall (List.head >> ((=) Empty))

    let private evaluateGameState gameState =
        if gameState.Board |> hasValidMoves then
            InProgress gameState 
        elif gameState.Board |> isEmpty then
            Finished { gameState with Score = gameState.Score + bonus }
        else
            Finished { gameState with Score = gameState.Score + (gameState.Board |> numberOfStones |> penalty) }

    let private getGroup board position =
        let rec find (ps:Position list) col (group:Position list) =
            match ps with
            | []    -> group
            | x::xs -> 
                let cells = x |> findAdjacentWithSameColor board col
                            |> List.filter (fun pos -> not (List.exists ((=) pos) (xs @ group) ))
                find (cells @ xs) col (x :: group)

        getCellState board position
        |> function 
            | Stone c -> 
                let positions = find [position] c []
                if positions |> List.length > 1 then
                    Some { Color = c; Positions = positions }
                else None
            | _ -> None

    let private removeGroup group board =
        board
        |> List.mapi (fun i col -> 
            col 
            |> List.mapi (fun j cell -> { Position = { Col = i; Row = j }; State = cell}) 
            |> List.filter (fun cell -> group.Positions |> (not << List.exists ((=) cell.Position)))
            |> List.map (fun cell -> cell.State)
            |> fun col' -> col' @ List.replicate (col.Length - col'.Length) Empty)
        |> List.filter (List.head >> ((<>) Empty))
        |> fun cols -> cols @ List.replicate (board.Length - cols.Length) (List.replicate (board.[0].Length) Empty)

    let private play gameState pos = 
        getGroup gameState.Board pos
        |> function 
            | Some g -> 
                let newBoard = gameState.Board |> removeGroup g
                { Board = newBoard; Score = gameState.Score + calcScore g.Positions.Length }
            | _ -> gameState

    let private playIfRunning game pos =
        match game with
        | InProgress gameState -> play gameState pos |> evaluateGameState
        | _                    -> game

    let private isValid conf =
        if conf.NumberOfColumns < 3 || conf.NumberOfColumns > 15 then
            false
        elif conf.NumberOfRows < 3 || conf.NumberOfRows > 15 then
            false
        else
            true

    let private newGame config = 
        let createBoard config =
            List.init config.NumberOfColumns (fun _ -> List.init config.NumberOfRows (fun _ -> config.StoneGenerator()))
            |> fun board -> { Board = board; Score = 0 }
            |> evaluateGameState |> Some
        if config |> isValid then
            createBoard config
        else None

    let api = {
        NewGame = newGame
        Play = playIfRunning }

open FSharp
open Fable.Core 
open Fable.Import.Browser
open System
open SameGameTypes

Fable.Import.Node.require.Invoke("core-js") |> ignore

let api = SameGameDomain.api

(**
Front end with Fable
--------------------

The UI implementation is based on HTML and style sheets. The SameGame board e.g. is rendered as an HTML table.

This is the function that renders a board to an HTML string:

*)

// val renderBoardToHtmlString : board:Board -> string
let renderBoardToHtmlString (board:Board) =
    let renderCell x y col = sprintf "<td class='sg-td'><a href='javaScript:void(0);' id='cell-%d-%d'><div class='sg-cell sg-color%d'></div></a></td>" x y col

    let makeBoard (board: int list list) = 
        "<table class='sg-table horiz-centered'>"
        + String.concat "" [for y in [(board.[0].Length - 1)..(-1)..0] do yield "<tr class='sg-tr'>" + String.concat "" ([0..(board.Length - 1)] |> List.map (fun x -> renderCell x y board.[x].[y])) + "</tr>"]
        + "</table>"

    makeBoard (board |> List.map (fun col -> col |> List.map (function Stone (Color c) -> c | Empty -> 0)))

(** 

The function `updateUi` is responsible for displaying the game and integrating the user interactions. These are the steps for updating the UI:

1. The HTML elements for displaying the board and the score are obtained.
2. A nested function `addListeners` for adding listeners for click events for all table cells is defined. The handlers will play a move and then recursively call `updateUi` again to update the UI with the new game state.
3. A pattern match of the game state is performed. Depending on the state, the board will be updated, event listeners will be added, and the score will be updated.

*)

// val updateUi : game:Game option -> unit
let rec updateUi game =
    let boardElement = document.getElementById("sg-board") :?> HTMLDivElement
    let scoreElement = document.getElementById ("sg-score") :?> HTMLDivElement

    let play game (x,y) =
        game
        |> Core.Option.map (fun g -> api.Play g { Col = x; Row = y })
        |> updateUi

    let addListeners maxColIndex maxRowIndex  =
        [0..maxColIndex] |> List.iter (fun x ->
            [0..maxRowIndex] |> List.iter (fun y -> 
                let cellId = sprintf "cell-%d-%d" x y
                let el = document.getElementById(cellId) :?> HTMLButtonElement
                el.addEventListener_click((fun _ -> play game (x,y); null))))
    
    match game with
    | Some (InProgress gs) -> 
        let board = renderBoardToHtmlString gs.Board
        boardElement.innerHTML <- board
        addListeners (gs.Board.Length - 1) (gs.Board.[0].Length - 1)
        scoreElement.innerText <- sprintf "%i point(s)." gs.Score
    | Some (Finished gs)   -> 
        let board = renderBoardToHtmlString gs.Board
        boardElement.innerHTML <- board
        scoreElement.innerText <- sprintf "No more moves. Your final score is %i point(s)." gs.Score
    | _ -> boardElement.innerText <- "Sorry, an error occurred while rendering the board."

(**

The configuration of the board is obtained by parsing the `class` attribute of the `div` element that contains the board. E.g. `<div id="sg-board" class="15-15-5"></div>` will be parsed to a 15 x 15 board with 5 different colors.

*)

// no fable support for System.Int32.Parse
let strToInt (str:string) =
    [for c in str -> c] 
    |> List.rev 
    |> List.mapi (fun i c -> (10.0**(float i)) * (float c)) 
    |> List.sum
    |> int

let rndColorGtor i = 
    let rnd = new System.Random()
    fun () -> rnd.Next(i) + 1 |> Color |> Stone 

let defaultConfig =  
    (document.getElementById("sg-board") :?> HTMLDivElement).className
    |> fun className -> className.Split('-') 
    |> Array.map strToInt
    |> fun arr -> { NumberOfColumns =  arr.[0]; NumberOfRows = arr.[1]; StoneGenerator = rndColorGtor arr.[2] }

(**

The handlers for starting a new game and for selecting a game from a list of preset initial game positions are defined and added.

*)

let buttonNewGame = document.getElementById("new-game") :?> HTMLButtonElement
let selectGame = document.getElementById("sg-select-game") :?> HTMLSelectElement
let selectWidth = document.getElementById("sg-select-w") :?> HTMLSelectElement
let selectHeight = document.getElementById("sg-select-h") :?> HTMLSelectElement
let selectColors = document.getElementById("sg-select-col") :?> HTMLSelectElement

let config() = { NumberOfColumns = selectWidth.value |> strToInt
                 NumberOfRows = selectHeight.value |> strToInt
                 StoneGenerator = selectColors.value |> strToInt |> rndColorGtor }

let newGameOnClick() =
    let game = config() |> api.NewGame
    selectGame.selectedIndex <- 0.0 
    updateUi game
    
let selectGameOnChange () =
    let presetGtor gameNum =
        let mutable index = 0;
        let game = PresetGames.games.[gameNum]
        (fun () -> index <- index + 1; game.[index-1] |> Color |> Stone)

    let gameIndex = selectGame.value |> strToInt;
    if gameIndex >= 0 then
        let game = api.NewGame { config() with StoneGenerator = presetGtor gameIndex }
        updateUi game

selectGame.addEventListener_change((fun _ -> selectGameOnChange(); null))
buttonNewGame.addEventListener_click((fun _ -> newGameOnClick(); null))

(**

Finally the game is initialized with:

*)

api.NewGame defaultConfig |> updateUi 