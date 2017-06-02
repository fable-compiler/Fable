# Node Console App
This sample demonstrates a simple console app to be run in node. 

Restore dependencies
--------------------
```
npm install
dotnet restore
```
Build the app
-------------------
This will start the Fable server and invoke webpack to build and bundle the app.
```
dotnet fable npm-run build
```
Run with Node
-------------
Run the generated file `app.js` in the build directory
```
node build/app.js
``` 