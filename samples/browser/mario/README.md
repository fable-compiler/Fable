## Build and running the app

1. Install npm dependencies: `npm install`
2. Install dotnet dependencies: `dotnet restore`
3. Start Fable server and Webpack dev server: `dotnet fable npm-run start`
4. In your browser, open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

> NOTE: In Windows you may have to press Ctrl+C twice to kill both Webpack and Fable processes.