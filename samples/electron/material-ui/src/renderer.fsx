// Load Fable.Core and bindings to JS global objects
#r "../../../../build/fable/bin/Fable.Core.dll"
#load "../../../../import/react/Fable.Import.React.fs"
#load "../../../../import/react/Fable.Helpers.React.fs"

open System
open Fable.Core
open Fable.Import

module R = Fable.Helpers.React
open R.Props

type RCom = React.ComponentClass<obj>

let deepOrange500= importMemberFrom<string> "material-ui/styles/colors"
let RaisedButton = importDefaultFrom<RCom> "material-ui/RaisedButton"
let Dialog = importDefaultFrom<RCom> "material-ui/Dialog"
let FlatButton = importDefaultFrom<RCom> "material-ui/FlatButton"
let MuiThemeProvider = importDefaultFrom<RCom> "material-ui/styles/MuiThemeProvider"
let getMuiTheme = importDefaultFrom<obj->obj> "material-ui/styles/getMuiTheme"

let inline (!!) x = createObj x
let inline (=>) x y = x ==> y

let muiTheme =
    !!["palette" =>
        !!["accent1Color" => deepOrange500]]
    |> getMuiTheme

type MainState = { isOpen: bool }

type Main(props, ctx) as this =
    inherit React.Component<obj,MainState>(props, ctx)
    do this.state <- {isOpen=false}

    member this.handleRequestClose() =
        this.setState({isOpen=false})

    member this.handleTouchTap() =
        this.setState({isOpen=true})

    member this.render() =
        let standardActions =
            R.from FlatButton
                !!["label" => "Ok"
                   "primary" => true
                   "onTouchTap" => this.handleRequestClose] []
        R.from MuiThemeProvider
            !!["mutiTheme"=>muiTheme] [
                R.div [Style [TextAlign "center"
                              PaddingTop 200]] [
                    R.from Dialog
                        !!["open" => this.state.isOpen
                           "title" => "Super Secret Password"
                           "onRequestClose" => this.handleRequestClose]
                        [unbox "1-2-3-4-5"]
                    R.h1 [] [unbox "Material-UI"]
                    R.h2 [] [unbox "example project"]
                    R.from RaisedButton
                        !!["label" => "Super Secret Password"
                           "secondary" => true
                           "onTouchTap" => this.handleTouchTap] []
                ]
            ]

let injectTapEventPlugin: unit->unit = importDefaultFrom "react-tap-event-plugin"
injectTapEventPlugin()

ReactDom.render(R.com<Main,_,_> None [], Browser.document.getElementById("app"))