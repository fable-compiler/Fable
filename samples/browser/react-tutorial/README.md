# Fable React example

This is just an adaptation of [React tutorial]() and [react-hot-boilerplate](https://github.com/gaearon/react-hot-boilerplate)
to show how the workflow of building a React app with Fable would be.

Use `fable -d` to build the project with the `debug` target and wait a few seconds until the
server launches. Visit `http://localhost:8080/` and then try modifying the `render`
function of one of the components in `public/components.fs`. See how the page loads new 
code **without refreshing** on file saving. It's magic! The necessary configuration is defined
in `fableconfig.json` and `webpack.config.js`

## Server

An [express](http://expressjs.com) server is available to interact with the app. When debugging,
a webpack dev server will also be launched to rebuild the app bundle on
changes and allow HMR (see below). Check `server.fs`.

## Client

The same F# project includes the code for client-side making it easier to
share code like business models (see `public/models.fs`). The project includes
a ReactHelper API with a DSL to make it easier to build React components from F#.
Check `public/components.fs`.

## Hot Module Reloading
The app uses [webpack](https://webpack.github.io) and a variation of [react-hot-loader](https://www.npmjs.com/package/react-hot-loader)
to allow HMR and improve the debug process. 

> Give also a try to [React Developer Tools](https://github.com/facebook/react-devtools)!
