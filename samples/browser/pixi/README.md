# Pixi + Fable 
Sample code demonstrates using [Pixi](http://www.pixijs.com/) v3 with Fable

Notice that becuase we are using a script tag on index.html for pixi.js, we need to instruct webpack to make calls to the library via the global reference. See the following part in `webpack.config.js`:
```js
externals: {
  "PIXI": "PIXI"
}
``` 
## Build and running the app

1. Install npm dependencies: `npm install`
2. Install dotnet dependencies: `dotnet restore`
3. Start Fable server and Webpack dev server: `dotnet fable npm-run start`
4. In your browser, open: http://localhost:8080/

Any modification you do to the F# code will be reflected in the web page after saving.

> NOTE: In Windows you may have to press Ctrl+C twice to kill both Webpack and Fable processes.