# Fable Server
When you build your Fable app or run the app on developement mode, a Fable server will run in the background. This server will work and orchestrate the compilation process with webpack. 

At first you have the `fable-loader`. A loader in webpack tells webpack how to read and `require` certain types of files. In the case of `fable-loader`, it tells webpack how to deal with files of type `*.fsproj`, `*.fs` or `*.fsx`. Namely by sending these files off to the Fable server to compile them. Because the Fable server only compiles F# to Babel, a `babel-loader` will take care of the output of Fable to generate the eventual javascript output that runs on your browser or on Node.

## Why
Since there isn't a "Babel server" for the `bable-loader`, one might ask why do we need a "Fable server" running in the background? 

The asnwer is incremental compilation. When Fable server compiles your app, it keeps track of the _state_ of the compilation such that, when a file is edited during developement mode (using webpack-dev-server), only a portion of your app will be recompiled instead of a full recompilation which a lot faster becuase on the first full compilation, all the dependencies will also get compiled and the state of these is kept during while you make changes to your app.