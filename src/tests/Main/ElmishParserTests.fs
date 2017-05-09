[<Util.Testing.TestFixture>]
module Fable.Tests.ParserTests


[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =

    let tuple a b =
        match (a,b) with
        | Some a, Some b -> Some (a,b)
        | _ -> None

    let ofFunc f arg =
        try
            Some (f arg)
        with _ ->
            None
            
(* This port of the Elm library helps you turn URLs into nicely structured data.
It is designed to be used with Browser.Navigation module to help folks create
single-page applications (SPAs) where you manage browser navigation yourself.
*)

type State<'v> =
  { visited : string list
    unvisited : string list
    args : Map<string,string>
    value : 'v }

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module State =
  let mkState visited unvisited args value =
        { visited = visited
          unvisited = unvisited
          args = args
          value = value }
          
  let map f { visited = visited; unvisited = unvisited; args = args; value = value } =
        { visited = visited
          unvisited = unvisited
          args = args
          value = f value }


/// Turn URLs like `/blog/42/cat-herding-techniques` into nice data.
type Parser<'a,'b> = State<'a> -> State<'b> list


// PARSE SEGMENTS

(* Create a custom path segment parser. Here is how it is used to define the
`i32` and `str` parsers:
    i32 state =
      custom "NUMBER" (int >> Ok) state
    str state =
      custom "string" Ok state
You can use it to define something like “only CSS files” like this:
    css =
      custom "CSS_FILE" <| fun segment ->
        if String.EndsWith ".css" then
          Ok segment
        else
          Error "Does not end with .css"
*)
let custom tipe stringToSomething : Parser<_,_> =
    let inner { visited = visited; unvisited = unvisited; args = args; value = value } =
        match unvisited with
        | [] -> []
        | next :: rest ->
            match stringToSomething next with
            | Ok nextValue ->
                [ State.mkState (next :: visited) rest args (value nextValue) ]

            | Error msg ->
                []
    inner


(* Parse a segment of the path as a `string`.
    parsePath string location
    /alice/  ==>  Some "alice"
    /bob     ==>  Some "bob"
    /42/     ==>  Some "42"
*)
let str state =
    custom "string" Ok state


(* Parse a segment of the path as an `int`.
    parsePath int location
    /alice/  ==>  None
    /bob     ==>  None
    /42/     ==>  Some 42
*)
let i32 state =
    custom "i32" (System.Int32.Parse >> Ok) state


(* Parse a segment of the path if it matches a given string.
    s "blog"  // can parse /blog/
              // but not /glob/ or /42/ or anything else
*)
let s str : Parser<_,_> =
    let inner { visited = visited; unvisited = unvisited; args = args; value = value } =
        match unvisited with
        | [] -> []
        | next :: rest ->
            if next = str then
                [ State.mkState (next :: visited) rest args value ]
            else
                []
    inner


// COMBINING PARSERS

(* Parse a path with multiple segments.
    parsePath (s "blog" </> i32) location
    /blog/35/  ==>  Some 35
    /blog/42   ==>  Some 42
    /blog/     ==>  None
    /42/       ==>  None
    parsePath (s "search" </> str) location
    /search/cats/  ==>  Some "cats"
    /search/frog   ==>  Some "frog"
    /search/       ==>  None
    /cats/         ==>  None
*)
let inline (</>) (parseBefore:Parser<_,_>) (parseAfter:Parser<_,_>) =
  fun state ->
    List.collect parseAfter (parseBefore state)


(* Transform a path parser.
    type Comment = { author : string; id : int }
    rawComment =
      s "user" </> str </> s "comments" </> i32
    comment =
      map (fun a id -> { author = a; id = id }) rawComment
    parsePath comment location
    /user/bob/comments/42  ==>  Some { author = "bob"; id = 42 }
    /user/tom/comments/35  ==>  Some { author = "tom"; id = 35 }
    /user/sam/             ==>  None
*)
let map (subValue:'a) (parse:Parser<'a,'b>) : Parser<'b->'c,'c> =
    let inner { visited = visited; unvisited = unvisited; args = args; value = value } =
        List.map (State.map value) 
        <| parse { visited = visited
                   unvisited = unvisited
                   args = args
                   value = subValue }
    inner


// PARSER HELPERS

let rec internal parseHelp states =
    match states with
    | [] ->
        None
    | state :: rest ->
        match state.unvisited with
        | [] ->
            Some state.value
        | [""] ->
            Some state.value
        | _ ->
            parseHelp rest

let internal splitUrl (url:string) =
    match List.ofArray <| url.Split([|'/'|]) with
    | "" :: segments ->
        segments
    | segments ->
        segments

let internal parse (parser:Parser<'a->'a,'a>) url args =
    { visited = []
      unvisited = splitUrl url
      args = args
      value = id }
    |> parser
    |> parseHelp

type Page =
  | Samples of (int * string) option

let pageParser: Parser<Page->Page,_> =
  let curry f a b = f (a,b)
  map (curry (Some >> Samples)) (s "samples" </> i32 </> str)
  
open Fable.Tests.Util
open Util.Testing

[<Test>]
let ``Parses``() =
    parse pageParser "samples/400/test" (Map [])
    |> equal (Some (Samples (Some (400, "test"))))