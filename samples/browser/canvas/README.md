## Canvas sample
This sample demonstrates using the HTML5 Canvas in Fable. It only contains one file `src/App.fs` which is the main starting point of the application.

```fs
module Program

open Fable.Core
open Fable.Core.JsInterop // for the !^ operator
open Fable.Import // to access the Browser api

let init() =
    // use the browser API to use the current document 
    // then get all elements with a tag "canvas", this returns an array
    // get the first element of that array (index = 0)
    let canvas = Browser.document.getElementsByTagName_canvas().[0]
    canvas.width <- 1000.
    canvas.height <- 800.
    let ctx = canvas.getContext_2d()
    // The (!^) operator checks and casts a value to an Erased Union type
    // Used for overloading types
    // See http://fable.io/docs/interacting.html#Erase-attribute for more info
    ctx.fillStyle <- !^"rgb(200,0,0)"
    ctx.fillRect (10., 10., 55., 50.)
    ctx.fillStyle <- !^"rgba(0, 0, 200, 0.5)"
    ctx.fillRect (30., 30., 55., 50.)

init()
```

## Installation
1. Install npm dependencies: `npm install` or `yarn install`
2. Install dotnet dependencies: `dotnet restore`
3. Start Fable server and Webpack dev server: `dotnet fable npm-run start`
4. In your browser, open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.