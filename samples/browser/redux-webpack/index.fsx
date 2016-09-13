#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react/Fable.Import.React.fs"
#load "node_modules/fable-import-react/Fable.Helpers.React.fs"
#load "components.fsx"

open Fable.Import
module R = Fable.Helpers.React
open Components

ReactDom.render(
    R.com<App,_,_> { Store=store } [],
    Browser.document.getElementsByClassName("todoapp").[0]
) |> ignore
