# Getting started
When you have installed all the [prerequisites](https://github.com/fable-compiler/Fable/blob/master/docs/pages/prerequisites.md) you are good to go.

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
dotnet new fable
```
You can create a directory and scaffold a project in one command:
```
dotnet new fable -n FableApp
```
This will create Fable project in this directory with the build instructions within the README.md file.

Install npm dependencies using yarn:
```
yarn install
```
Install Fable and dotnet dependencies using Paket:
```
dotnet restore
```
At this point, when you navigate through your code, you should have tooltips and auto-completion working. 

To run the app in developement mode, you run:
```
dotnet fable npm-run start
```
This will start the Fable daemon and run an npm script in parallel (npm scripts are listed in the package.json file). In this case, `start` will run a Webpack development server. You could run the Fable daemon and webpack developement server in seperate commands:
```
# Fable daemon
dotnet fable start
# Weback developement server
npm run start
```

You can navigate to `http://localhost:8080` in your browser to see your simple Fable running. At this point, when you make changes to your files,webpack developement server will detect these changes, recompile your app and refreshes the browser on your behalf. 

To build your app for production without running webpack developement server, you can run:
```
dotnet fable npm-run build
```
which, depending on how you configured webpack inside `webpack.config.js` will compile you app and bundle it into a single file, ready for production. By default, that file is compiled to the `public` directory and is named `bundle.js`.
