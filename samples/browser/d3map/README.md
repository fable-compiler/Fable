# Instructions

Make sure you have a recent version of [dotnet SDK](https://www.microsoft.com/net/core) and [node.js](https://nodejs.org) installed. Then to run the sample:

- First, you must install shared npm dependencies in **`samples` directory** (two levels above this one):

```shell
npm install
```

- After that, install dotnet dependencies for **this directory** (where this README file is):

```shell
dotnet restore
```

- The previous steps only need to be completed once (unless dependencies change). During development, you can start the Fable and Webpack Dev Server in watch mode using the following command (in this directory):

```shell
dotnet fable webpack-dev-server
```

After the bundle is finished, you can open `http://localhost:8080/` in your browser and you'll see the sample in action. Any change in the F# code will automatically refresh the page.

> The previous command just generates the bundle in memory. If you want to write it to disk use `dotnet fable webpack` instead. (You can also minify the JS code by passing the `-p` argument to Webpack like `dotnet fable webpack --args "-p"`.)