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