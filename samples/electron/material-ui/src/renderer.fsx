// Load Fable.Core and bindings to JS global objects
#r "../../../../build/fable/bin/Fable.Core.dll"
#load "../../../../import/react/Fable.Import.React.fs"
#load "../../../../import/react/Fable.Helpers.React.fs"

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Node

module R = Fable.Helpers.React
open R.Props

type RCom = React.ComponentClass<obj>

let deepOrange500 = importMember<string> "material-ui/styles/colors"
let RaisedButton = importDefault<RCom> "material-ui/RaisedButton"
let Dialog = importDefault<RCom> "material-ui/Dialog"
let FlatButton = importDefault<RCom> "material-ui/FlatButton"
let MuiThemeProvider = importDefault<RCom> "material-ui/styles/MuiThemeProvider"
let getMuiTheme = importDefault<obj->obj> "material-ui/styles/getMuiTheme"

let inline (!!) x = createObj x
let inline (=>) x y = x ==> y

let muiTheme =
    !!["palette" =>
        !!["accent1Color" => deepOrange500]]
    |> getMuiTheme

type MainState = { isOpen: bool; secret: string }

type Main(props, ctx) as this =
    inherit React.Component<obj,MainState>(props, ctx)
    do this.state <- {isOpen=false; secret=""}

    member this.handleRequestClose() =
        this.setState({isOpen=false; secret=""})

    member this.handleTouchTap() =
         this.setState({isOpen=true; secret="1-2-3-4-5"})
        // fs.readFile(__dirname + "/data/secret.txt", fun err buffer ->
        //     if (box err) <> null then
        //         failwith "Couldn't read file"
        //     this.setState({isOpen=true; secret=string buffer}))

    member this.render() =
        let standardActions =
            R.from FlatButton
                !!["label" => "Ok"
                   "primary" => true
                   "onTouchTap" => this.handleRequestClose] []
        R.from MuiThemeProvider
            !!["muiTheme" => muiTheme] [
                R.div [Style [TextAlign "center"
                              PaddingTop 200]] [
                    R.from Dialog
                        !!["open" => this.state.isOpen
                           "title" => "Super Secret Password"
                           "actions" => standardActions
                           "onRequestClose" => this.handleRequestClose]
                        [unbox this.state.secret]
                    R.h1 [] [unbox "Material-UI"]
                    R.h2 [] [unbox "example project"]
                    R.from RaisedButton
                        !!["label" => "Super Secret Password"
                           "secondary" => true
                           "onTouchTap" => this.handleTouchTap] []
                ]
            ]

(importDefault "react-tap-event-plugin")()

ReactDom.render(R.com<Main,_,_> None [], Browser.document.getElementById("app"))