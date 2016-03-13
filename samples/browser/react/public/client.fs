module Client

module Util =
    open Fable.Core
    open Fable.Import
    open Fable.Import.Browser

    let (|NonEmpty|_|) (s: string) =
        match s.Trim() with "" -> None | s -> Some s

    type HttpMethod<'T> =
        | Get of url: string
        | Post of url: string * data: 'T
        
    exception MyEx of pachin:int*string
    
    let ajax meth onSuccess onError =
        let url, meth, data =
            match meth with
            | Get url -> url, "GET", None
            | Post (url, data) ->
                url, "POST", Some(JS.Globals.JSON.stringify data)
        let req = Globals.XMLHttpRequest.createNew()
        req.onreadystatechange <- fun _ ->
            if req.readyState = 4. then
                match req.status with
                | 200. | 0. ->
                    JS.Globals.JSON.parse req.responseText
                    |> unbox |> onSuccess
                | _ -> onError req.status
            null
        req.``open``(meth, url, true)
        req.setRequestHeader("Content-Type", "application/json")
        req.send(data)

open System
open Fable.Core
open Fable.Import

open Util
open Models
module R = ReactHelper

[<Import("marked?asDefault=true")>]
let marked (s: string) (opts: obj): string = failwith "JS only"

type CVProps = {author: string; key: DateTime option}

type CBProps = { url: string; pollInterval: float }
type CBState = { data: Comment[] }

type CFProps = { onCommentSubmit: Comment -> unit }
type CFState = { author: string option; text: string option }

// type CommentView(props) =
//     inherit R.Component<CVProps,obj>(props)
//     member x.rawMarkup () =
//         let rawMarkup =
//             marked (string x.props?children) (createObj ["sanitize"==>true])
//         createObj ["__html" ==> rawMarkup]
//     member x.render() =
//         R.div ["className" ==> "comment"] [
//             R.h2 ["className" ==> "commentAuthor"] [unbox x.props.author]
//             R.span ["dangerouslySetInnerHTML" ==> x.rawMarkup()] []
//         ]
        
let CommentView (props: CVProps) =
    let rawMarkup =
        let m = marked (string props?children) (createObj ["sanitize"==>true])
        createObj [ "__html" ==> m ]
    R.div ["className" ==> "comment"] [
        R.h2 ["className" ==> "commentAuthor"] [unbox props.author]
        R.span ["dangerouslySetInnerHTML" ==> rawMarkup] []
    ]

// type CommentList(props) =
//     inherit R.Component<CBState, obj>(props)
//     member x.render () =
//         let commentNodes =
//             x.props.data
//             |> Array.map (fun comment ->
//                 R.fn CommentView {
//                     author = comment.author
//                     key = comment.id
//                 } [ unbox comment.text ])
//         R.div ["className" ==> "commentList"] [unbox commentNodes]
        
let CommentList(props: CBState) =
    let commentNodes =
        props.data
        |> Array.map (fun comment ->
            R.fn CommentView {
                author = comment.author
                key = comment.id
            } [ unbox comment.text ])
    R.div ["className" ==> "commentList"] [unbox commentNodes]

type CommentForm(props) =
    inherit R.Component<CFProps, CFState>(props, { author = None; text = None })

    member x.handleAuthorChange (e: React.SyntheticEvent) =
        let str = unbox e.target?value
        x.setState { x.state with author = str}

    member x.handleTextChange (e: React.SyntheticEvent) =
        let str = unbox e.target?value
        x.setState { x.state with text = str}

    member x.handleSubmit (e: React.SyntheticEvent) =
        e.preventDefault()
        match x.state with
        // | {author = Some(NonEmpty author); text = Some(NonEmpty text)} ->
        | { author = Some author; text = Some text }
                when author.Trim() <> "" && text.Trim() <> "" ->
            x.props.onCommentSubmit { id = None; author = author; text = text }
            x.setState { author = None; text = None }
        | _ -> ()
    
    member x.render () =
        R.form [
            "className" ==> "commentForm"
            "onSubmit" ==> x.handleSubmit
            ] [
                R.input [
                    "type" ==> "text"
                    "placeholder" ==> "Your name"
                    "value" ==> x.state.author
                    "onChange" ==> x.handleAuthorChange
                ] []
                R.input [
                    "type" ==> "text"
                    "placeholder" ==> "Say something..."
                    "value" ==> x.state.text
                    "onChange" ==> x.handleTextChange
                ] []
                R.input [
                    "type" ==> "submit"
                    "value" ==> "Post"
                ] []
            ]

type CommentBox(props) =
    inherit R.Component<CBProps, CBState>(props, { data = [||] })
    
    member x.loadCommentsFromServer () =
        ajax (Get x.props.url)
            (fun data -> x.setState { data = data })
            (fun status ->
                Browser.Globals.console.error(x.props.url, status))
    
    member x.handleCommentSubmit (comment: Comment) =
        let comments = x.state.data
        // Optimistically set an id on the new comment. It will be replaced by an
        // id generated by the server. In a production application you would likely
        // not use Date.now() for this and would have a more robust system in place.
        let comment = { comment with id = Some DateTime.Now }
        let newComments = Array.append comments [|comment|]
        x.setState { data = newComments }
        ajax (Post (x.props.url, comment))
            (fun data -> x.setState { data = data })
            (fun status ->
                x.setState { data = comments }
                Browser.Globals.console.error(x.props.url, status))

    member x.componentDidMount () =
        x.loadCommentsFromServer ()
        Browser.Globals.window.setInterval(
            x.loadCommentsFromServer, x.props.pollInterval)
            
    member x.render () =
        R.div ["className" ==> "commentBox"] [
            R.h1 [] [unbox "Comments"]
            R.fn CommentList {data = x.state.data} []
            R.com<CommentForm,_,_> {onCommentSubmit = x.handleCommentSubmit} []
        ]
        
ReactDom.Globals.render(
    R.com<CommentBox,_,_> { url="/api/comments"; pollInterval=2000. } [],
    Browser.Globals.document.getElementById "content")
|> ignore