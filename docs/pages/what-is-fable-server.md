# Fable daemon

When you build your Fable app or run the app on development mode, a Fable daemon will run in the background. This daemon will work and orchestrate the compilation process with Webpack.

At first you have the `fable-loader`. A loader in Webpack tells Webpack how to read and `require` certain types of files. In the case of `fable-loader`, it tells Webpack how to deal with files of type `*.fsproj`, `*.fs` or `*.fsx`. Namely by sending these files off to the Fable daemon to compile them. Because Fable only generates an AST (Abstract Syntax Tree), Babel will take care of the output to generate the eventual javascript code that runs on your browser or on Node.js.

## Why

Since there isn't a "Babel daemon" for the `babel-loader`, one might ask why do we need a "Fable daemon" running in the background?

The answer is incremental compilation. When Fable daemon compiles your app, it keeps track of the _state_ of the compilation such that, when a file is edited during development mode (using webpack-dev-server), only a portion of your app will be recompiled instead of a full recompilation which is a lot faster. Only the first compilation is relatively slow because all the dependencies will also get compiled and the state of these is kept.

## Not only Webpack

The Fable daemon can communicate with other tools besides Webpack. There's also a [Rollup plugin](https://www.npmjs.com/package/rollup-plugin-fable) available if you prefer to bundle your JS code with Rollup instead of Webpack.
