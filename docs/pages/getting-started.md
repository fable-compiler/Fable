# Getting started
When you have installed all the [prerequisites](/prerequisites.md) you are good to go.

Start by installing the Simple Fable template:
```
dotnet new --install "Fable.Template::*"
```
Create a directory for your project:
```
mkdir FableApp
cd FableApp
```
Scaffold a Fable app from the template:
```
dotnet new Fable
```
This will create Fable project in this directory. Now install the npm dependencies using yarn:
```
yarn install
```
Install Fable and dotnet dependencies using Paket:
```
dotnet restore
```
At this point, when you navigate through your code using Visual Studio Code or Atom with the Ionide extension, you should have tooltips and auto-completion working. 

To run the app in developement mode, you run:
```
dotnet fable npm-run start
```
This will do two things, first, it will run the Fable server/daemon in the background. Also, it will run the webpack developement server.

You can navigate to `http://localhost:8080` in your browser to see your simple Fable running. At this point, when you make changes to your files,webpack developement server will detect these changes, recompile your app and refreshes the browser on your behalf. 

To build your app for production without running webpack developement server, you can run:
```
dotnet fable npm-run build
```
which, depending on how you configured webpack inside `webpack.config.js` will compile you app and bundle it into a single file, ready for proeuction. By default, that file is compiled to the `public` directory and is named `bundle.js`.