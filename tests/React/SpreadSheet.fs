module SpreadSheet

// Build your own Excel 365 in an hour with F# by Tomas Petricek!
// Watch the video of the talk here: https://www.youtube.com/watch?v=Bnm71YEt_lI

module Parsec =
    type ParseStream<'T> = int * list<'T>
    type Parser<'T, 'R> = Parser of (ParseStream<'T> -> option<ParseStream<'T> * 'R>)

    /// Returned by the `slot` function to create a parser slot that is filled later
    type ParserSetter<'T, 'R> =
      { Set : Parser<'T, 'R> -> unit }

    /// Ignore the result of the parser
    let ignore (Parser p) = Parser(fun input ->
      p input |> Option.map (fun (i, r) -> i, ()))

    /// Creates a delayed parser whose actual parser is set later
    let slot () =
      let mutable slot = None
      { Set = fun (Parser p) -> slot <- Some p },
      Parser(fun input -> slot.Value input)

    /// If the input matches the specified prefix, produce the specified result
    let prefix (prefix:list<'C>) result = Parser(fun (offset, input) ->
      let rec loop (word:list<'C>) input =
        match word, input with
        | c::word, i::input when c = i -> loop word input
        | [], input -> Some(input)
        | _ -> None

      match loop prefix input with
      | Some(input) -> Some((offset+List.length prefix, input), result)
      | _ -> None)

    /// Parser that succeeds when either of the two arguments succeed
    let (<|>) (Parser p1) (Parser p2) = Parser(fun input ->
      match p1 input with
      | Some(input, res) -> Some(input, res)
      | _ -> p2 input)

    /// Run two parsers in sequence and return the result as a tuple
    let (<*>) (Parser p1) (Parser p2) = Parser(fun input ->
      match p1 input with
      | Some(input, res1) ->
          match p2 input with
          | Some(input, res2) -> Some(input, (res1, res2))
          | _ -> None
      | _ -> None)

    /// Transforms the result of the parser using the specified function
    let map f (Parser p) = Parser(fun input ->
      p input |> Option.map (fun (input, res) -> input, f res))

    /// Run two parsers in sequence and return the result of the second one
    let (<*>>) p1 p2 = p1 <*> p2 |> map snd

    /// Run two parsers in sequence and return the result of the first one
    let (<<*>) p1 p2 = p1 <*> p2 |> map fst

    /// Succeed without consuming input
    let unit res = Parser(fun input -> Some(input, res))

    /// Parse using the first parser and then call a function to produce
    /// next parser and parse the rest of the input with the next parser
    let bind f (Parser p) = Parser(fun input ->
      match p input with
      | Some(input, res) ->
          let (Parser g) = f res
          match g input with
          | Some(input, res) -> Some(input, res)
          | _ -> None
      | _ -> None)

    /// Parser that tries to use a specified parser, but returns None if it fails
    let optional (Parser p) = Parser(fun input ->
      match p input with
      | None -> Some(input, None)
      | Some(input, res) -> Some(input, Some res) )

    /// Parser that succeeds if the input matches a predicate
    let pred p = Parser(function
      | offs, c::input when p c -> Some((offs+1, input), c)
      | _ -> None)

    /// Parser that succeeds if the predicate returns Some value
    let choose p = Parser(function
      | offs, c::input -> p c |> Option.map (fun c -> (offs + 1, input), c)
      | _ -> None)

    /// Parse zero or more repetitions using the specified parser
    let zeroOrMore (Parser p) =
      let rec loop acc input =
        match p input with
        | Some(input, res) -> loop (res::acc) input
        | _ -> Some(input, List.rev acc)
      Parser(loop [])

    /// Parse one or more repetitions using the specified parser
    let oneOrMore p =
      (p <*> (zeroOrMore p))
      |> map (fun (c, cs) -> c::cs)


    let anySpace = zeroOrMore (pred (fun t -> t = ' '))

    let char tok = pred (fun t -> t = tok)

    let separated sep p =
      p <*> zeroOrMore (sep <*> p)
      |> map (fun (a1, args) -> a1::(List.map snd args))

    let separatedThen sep p1 p2 =
      p1 <*> zeroOrMore (sep <*> p2)
      |> map (fun (a1, args) -> a1::(List.map snd args))

    let separatedOrEmpty sep p =
      optional (separated sep p)
      |> map (fun l -> defaultArg l [])

    let number = pred (fun t -> t <= '9' && t >= '0')

    let integer = oneOrMore number |> map (fun nums ->
      nums |> List.fold (fun res n -> res * 10 + (int n - int '0')) 0)

    let letter = pred (fun t ->
      (t <= 'Z' && t >= 'A') || (t <= 'z' && t >= 'a'))

    let run (Parser(f)) input =
      match f (0, List.ofSeq input) with
      | Some((i, _), res) when i = Seq.length input -> Some res
      | _ -> None

module Evaluator =
    open Parsec

    // ----------------------------------------------------------------------------
    // DOMAIN MODEL
    // ----------------------------------------------------------------------------

    type Position = char * int

    type Expr =
      | Reference of Position
      | Number of int
      | Binary of Expr * char * Expr

    // ----------------------------------------------------------------------------
    // PARSER
    // ----------------------------------------------------------------------------

    // Basics: operators (+, -, *, /), cell reference (e.g. A10), number (e.g. 123)
    let operator = char '+' <|> char '-' <|> char '*' <|> char '/'
    let reference = letter <*> integer |> map Reference
    let number = integer |> map Number

    // Nested operator uses need to be parethesized, for example (1 + (3 * 4)).
    // <expr> is a binary operator without parentheses, number, reference or
    // nested brackets, while <term> is always bracketed or primitive. We need
    // to use `expr` recursively, which is handled via mutable slots.
    let exprSetter, expr = slot ()
    let brack = char '(' <*>> anySpace <*>> expr <<*> anySpace <<*> char ')'
    let term = number <|> reference <|> brack
    let binary = term <<*> anySpace <*> operator <<*> anySpace <*> term |> map (fun ((l,op), r) -> Binary(l, op, r))
    let exprAux = binary <|> term
    exprSetter.Set exprAux

    // Formula starts with `=` followed by expression
    // Equation you can write in a cell is either number or a formula
    let formula = char '=' <*>> anySpace <*>> expr
    let equation = anySpace <*>> (formula <|> number) <<*> anySpace

    // Run the parser on a given input
    let parse input = run equation input

    // ----------------------------------------------------------------------------
    // EVALUATOR
    // ----------------------------------------------------------------------------

    let rec evaluate visited (cells:Map<Position, string>) expr =
      match expr with
      | Number num ->
          Some num

      | Binary(l, op, r) ->
          let ops = dict [ '+', (+); '-', (-); '*', (*); '/', (/) ]
          evaluate visited cells l |> Option.bind (fun l ->
            evaluate visited cells r |> Option.map (fun r ->
              ops.[op] l r ))

      | Reference pos when Set.contains pos visited ->
          None

      | Reference pos ->
          cells.TryFind pos |> Option.bind (fun value ->
            parse value |> Option.bind (fun parsed ->
              evaluate (Set.add pos visited) cells parsed))

open Fable.React
open Fable.React.Props
open Fable.Core.JsInterop

open Evaluator

// ----------------------------------------------------------------------------
// DOMAIN MODEL
// ----------------------------------------------------------------------------

type Event =
  | UpdateValue of Position * string
  | StartEdit of Position

type State =
  { Rows : int list
    Active : Position option
    Cols : char list
    Cells : Map<Position, string> }

type Movement =
    | MoveTo of Position
    | Invalid

type Direction = Up | Down | Left | Right

let KeyDirection : Map<float,Direction> = Map.ofList [ (37.0, Left); (38.0, Up); (39.0, Right); (40.0, Down) ]

// ----------------------------------------------------------------------------
// EVENT HANDLING
// ----------------------------------------------------------------------------

let update msg state =
  match msg with
  | StartEdit(pos) ->
      { state with Active = Some pos }

  | UpdateValue(pos, value) ->
      let newCells =
          if value = ""
              then Map.remove pos state.Cells
              else Map.add pos value state.Cells
      { state with Cells = newCells }

// ----------------------------------------------------------------------------
// RENDERING
// ----------------------------------------------------------------------------

let getDirection (ke: Browser.Types.KeyboardEvent) : Option<Direction> =
    Map.tryFind ke.keyCode KeyDirection

let getPosition ((col, row): Position) (direction: Direction) : Position =
    match direction with
        | Up -> (col, row - 1)
        | Down -> (col, row + 1)
        | Left -> (char((int col) - 1), row)
        | Right -> (char((int col) + 1), row)

let getMovement (state: State) (direction: Direction) : Movement =
    match state.Active with
        | None -> Invalid
        | (Some position) ->
            let (col, row) = getPosition position direction
            if List.contains col state.Cols && List.contains row state.Rows
                then MoveTo (col, row)
                else Invalid

let getKeyPressEvent state trigger = fun ke ->
    match getDirection ke with
        | None -> ()
        | Some direction ->
            match getMovement state direction with
                | Invalid -> ()
                | MoveTo position -> trigger(StartEdit(position))

let renderEditor (trigger:Event -> unit) pos state vale =
  td [ Class "selected"] [
    input [
      Alt "cell editor"
      AutoFocus true
      OnKeyDown (getKeyPressEvent state trigger)
      OnInput (fun e -> trigger (UpdateValue (pos, e.target ? value )))
      Value vale ]
  ]

let renderView trigger pos (value:option<_>) =
  td
    [ Style (if value.IsNone then [Background "#ffb0b0"] else [Background "white"])
      OnClick (fun _ -> trigger(StartEdit(pos)) ) ]
    [ str (Option.defaultValue "#ERR" value) ]

let renderCell trigger pos state =
  let value = Map.tryFind pos state.Cells
  if state.Active = Some pos then
    renderEditor trigger pos state (Option.defaultValue "" value)
  else
    let value =
      match value with
      | Some value ->
          parse value |> Option.bind (evaluate Set.empty state.Cells) |> Option.map string
      | _ -> Some ""
    renderView trigger pos value

let view state trigger =
  let empty = td [] []
  let header h = th [] [str h]
  let headers = state.Cols |> List.map (fun h -> header (string h))
  let headers = empty::headers

  let row cells = tr [] cells
  let cells n =
    let cells = state.Cols |> List.map (fun h -> renderCell trigger (h, n) state)
    header (string n) :: cells
  let rows = state.Rows |> List.map (fun r -> tr [] (cells r))

  table [] [
    thead [] [tr [] headers]
    tbody [] rows
  ]

// ----------------------------------------------------------------------------
// ENTRY POINT
// ----------------------------------------------------------------------------

let initial () =
  { Cols = ['A' .. 'K']
    Rows = [1 .. 15]
    Active = None
    Cells = Map.empty }

open Feliz

[<ReactComponent>]
let SpreadSheet() =
    let (state, setState) = React.useState(initial)
    let trigger ev =
      update ev state |> setState
    view state trigger
