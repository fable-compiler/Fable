module SameGame.App

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open SameGame.Types
open SameGame.Domain
open SameGame.PresetGames

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


let getById<'T when 'T :> Browser.HTMLElement> id =
    Browser.document.getElementById(id) :?> 'T


// val updateUi : game:Game option -> unit
let rec updateUi game =
    let boardElement = getById<Browser.HTMLDivElement>("sg-board")
    let scoreElement = getById<Browser.HTMLDivElement> ("sg-score")

    let play game (x,y) =
        game
        |> Core.Option.map (fun g ->
            api.Play g { Col = x; Row = y })
        |> updateUi

    let addListeners maxColIndex maxRowIndex  =
        [0..maxColIndex] |> List.iter (fun x ->
            [0..maxRowIndex] |> List.iter (fun y ->
                let cellId = sprintf "cell-%d-%d" x y
                let el = getById<Browser.HTMLButtonElement>(cellId)
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


let rndColorGtor i =
    let rnd = new System.Random()
    fun () -> rnd.Next(i) + 1 |> Color |> Stone

let defaultConfig =
    (getById<Browser.HTMLDivElement>("sg-board")).className
    |> fun className -> className.Split('-')
    |> Array.map int
    |> fun arr ->
        { NumberOfColumns = arr.[0]
          NumberOfRows = arr.[1]
          StoneGenerator = rndColorGtor arr.[2] }    



let buttonNewGame = getById<Browser.HTMLButtonElement>("new-game")
let selectGame = getById<Browser.HTMLSelectElement>("sg-select-game")
let selectWidth = getById<Browser.HTMLSelectElement>("sg-select-w")
let selectHeight = getById<Browser.HTMLSelectElement>("sg-select-h")
let selectColors = getById<Browser.HTMLSelectElement>("sg-select-col")

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
        let game = games.[gameNum]
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

// Finally the game is initialized with
api.NewGame defaultConfig |> updateUi