
// Load Fable.Core and bindings to JS global objects
#r "../node_modules/fable-core/Fable.Core.dll"
#load "../node_modules/fable-import-electron/Fable.Import.Electron.fs"

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Node

module Electron =
    // Module to control application life.
    let app = electron.app

    // Keep a global reference of the window object, if you don't, the window will
    // be closed automatically when the JavaScript object is garbage collected.
    let mutable mainWindow: Electron.BrowserWindow option = None

    let createWindow () =
        // Create the browser window.
        mainWindow <-
            createObj [ "width" ==> 800; "height" ==> 600 ]
            |> unbox |> electron.BrowserWindow.Create |> Some

        // and load the index.html of the app.
        mainWindow.Value.loadURL("file://" + Node.__dirname + "/../index.html");

        #if DEBUG
        fs.watch(Node.__dirname + "/renderer.js", fun _ ->
            mainWindow.Value.webContents.reloadIgnoringCache()
            |> ignore
        ) |> ignore
        #endif

        // Emitted when the window is closed.
        mainWindow.Value.on("closed", unbox(fun () ->
            // Dereference the window object, usually you would store windows
            // in an array if your app supports multi windows, this is the time
            // when you should delete the corresponding element.
            mainWindow <- None
        ))
        |> ignore

    // This method will be called when Electron has finished
    // initialization and is ready to create browser windows.
    app.on("ready", unbox createWindow)

    // Quit when all windows are closed.
    app.on("window-all-closed", unbox(fun () ->
        // On OS X it is common for applications and their menu bar
        // to stay active until the user quits explicitly with Cmd + Q
        if Node.``process``.platform <> "darwin" then
            app.quit()
    ))

    app.on("activate", unbox(fun () ->
        // On OS X it's common to re-create a window in the app when the
        // dock icon is clicked and there are no other windows open.
        if mainWindow.IsNone then
            createWindow()
    ))
