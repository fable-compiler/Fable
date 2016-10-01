module Components

module Util =
    open Fable.Core
    open Fable.Import
    open Fable.Import.Browser

    let (|NonEmpty|_|) (s: string) =
        match s.Trim() with "" -> None | s -> Some s

    type HttpMethod<'T> =
        | Get of url: string
        | Post of url: string * data: 'T
        
    let ajax meth onSuccess onError =
        let url, meth, data =
            match meth with
            | Get url -> url, "GET", None
            | Post (url, data) ->
                url, "POST", Some(JS.JSON.stringify data)
        let req = XMLHttpRequest.Create()
        req.onreadystatechange <- fun _ ->
            if req.readyState = 4. then
                match req.status with
                | 200. | 0. ->
                    JS.JSON.parse req.responseText
                    |> unbox |> onSuccess
                | _ -> onError req.status
            null
        req.``open``(meth, url, true)
        req.setRequestHeader("Content-Type", "application/json")
        req.send(data)

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Util
open Models

// ReactHelper defines a DSL to make it easier to build
// React components from F#
module R = Fable.Helpers.React
open R.Props

[<Import("default","marked")>]
let marked (s: string) (opts: obj): string = jsNative

// Check we can also use interfaces as props, see #192
// type CVProps = {author: string; key: DateTime option}
type CVProps =
    abstract author: string
    abstract key: DateTime option

type CVPropsImpl(pAuthor, pKey) =
    interface CVProps with
        member __.author = pAuthor
        member __.key = pKey

type CBProps = { url: string; pollInterval: float }
type CBState = { data: Comment[] }

type CFProps = { onCommentSubmit: Comment -> unit }
type CFState = { author: string option; text: string option }
        
// For simple components we can just use just a render function
// This is the recommended way by React team
let CommentView (props: CVProps) =
    let rawMarkup =
        let m = marked (string props?children) (createObj ["sanitize"==>true])
        createObj [ "__html" ==> m ]
    // ReactHelper provides functions to build components from HTML tags
    // We can build the props list using union types from ReactHelper.Props
    R.div [
        ClassName "comment"
        Style [
            Border "3px dotted blue"
            Margin "5px 0"
            Padding "0 5px"
        ]
    ] [
        R.h2 [ClassName "commentAuthor"] [unbox props.author]
        R.span [DangerouslySetInnerHTML rawMarkup] []
    ]

let CommentList(props: CBState) =
    let commentNodes =
        props.data
        |> Array.mapi (fun i comment ->
            let cvProps =
                // Check that both types implementing interfaces and object expressions work
                match i % 2 with
                | 0 -> { new CVProps with
                            member x.author = comment.author
                            member x.key = comment.id }
                | _ -> CVPropsImpl(comment.author, comment.id) :> CVProps
            // Use ReactHelper.fn to render a component from a function
            R.fn CommentView cvProps [ unbox comment.text ])
    R.div [ClassName "commentList"] [unbox commentNodes]

type CommentForm(props) as this =
    inherit React.Component<CFProps, CFState>(props)
    do this.state <- { author = None; text = None }

    member x.handleAuthorChange (e: React.SyntheticEvent) =
        let str = unbox e.target?value
        x.setState { x.state with author = str}

    member x.handleTextChange (e: React.SyntheticEvent) =
        let str = unbox e.target?value
        x.setState { x.state with text = str}

    member x.handleSubmit (e: React.SyntheticEvent) =
        e.preventDefault()
        match x.state with
        | {author = Some(NonEmpty author); text = Some(NonEmpty text)} ->
            x.props.onCommentSubmit { id = None; author = author; text = text }
            x.setState { author = None; text = None }
        | _ -> ()
    
    member x.render () =
        R.form [
            ClassName "commentForm"
            OnSubmit x.handleSubmit
        ] [
            R.input [
                Type "text"
                Placeholder "Your name"
                Value (U2.Case1 x.state.author.Value)
                OnChange x.handleAuthorChange
            ] []
            R.input [
                Type "text"
                Placeholder "Say something..."
                Value (U2.Case1 x.state.text.Value)
                OnChange x.handleTextChange
            ] []
            R.input [
                Type "submit"
                Value (U2.Case1 "Post")
            ] []
        ]

type CommentBox(props) as this =
    inherit React.Component<CBProps, CBState>(props)
    do this.state <- { data = [||] }
    
    member x.loadCommentsFromServer () =
        ajax (Get x.props.url)
            (fun data -> x.setState { data = data })
            (fun status ->
                Browser.console.error(x.props.url, status))
    
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
                Browser.console.error(x.props.url, status))

    member x.componentDidMount () =
        x.loadCommentsFromServer ()
        Browser.window.setInterval(
             x.loadCommentsFromServer, x.props.pollInterval)
            
    member x.render () =
        R.div [ClassName "commentBox"] [
            R.h1 [] [unbox "Comments"]
            R.fn CommentList {data = x.state.data} []
            // Use ReactHelper.com to build a React Component from a type
            R.com<CommentForm,_,_> {onCommentSubmit = x.handleCommentSubmit} []
        ]
