# Getting Started

## Requirements

First of all, you need a couple of things installed on your computer for F# & Fable development.

* [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
* [node.js](https://nodejs.org) 6.11 or higher
* A JS package manager: [yarn](https://yarnpkg.com) or [npm](http://npmjs.com/)
* An F# editor like like VS Code or Atom with [Ionide](http://ionide.io/), [Visual Studio](https://www.visualstudio.com/) for Windows or macOS, Emacs with [fsharp-mode](https://github.com/fsharp/emacs-fsharp-mode) or [Rider](https://www.jetbrains.com/rider/)

> npm comes bundled with node.js, but we recommend at least version 5. If necessary, upgrade it by running `npm install -g npm`. For F# dependencies, [Paket](https://fsprojects.github.io/Paket/) will be automatically installed by a bootstraper in Fable templates.

> Although is not a Fable requirement, on macOS and Linux you will need [Mono](http://www.mono-project.com/) for other F# tooling like Paket or editor support.

## TL;DR

After installing the requirements, the easiest way to get up and running is to use a Fable template. Below we will see how to install and run the basic [Fable.Template](https://www.nuget.org/packages/Fable.Template/) to create a simple web app.

Install the template (the same command can be used to update the template to latest version):

```shell
dotnet new -i Fable.Template
```

After that, if you type `dotnet new -h` you should see "fable" among the list of available templates. Let's call it to scaffold a Fable app:

```shell
dotnet new fable -n FableApp
cd FableApp
```

This will create Fable project in a directory with the given name. Below we will see the steps to run the app, but **always check the README file** in case instructions change in future versions of the template.

After scaffolding the project, install JS and F# dependencies. This is only needed once or whenever dependencies change.

> In the samples below, yarn is the tool of choice. If you want to use npm, just replace `yarn` by `npm` in the commands.

```shell
yarn install
```

Install Fable and dotnet dependencies using Paket:

```shell
dotnet restore
cd src
```

Start Fable and [Webpack](https://webpack.js.org/) dev server:

```shell
dotnet fable yarn-start
```

> This command is used to start the Fable daemon and run a script in package.json concurrently. It's a shortcut of `dotnet fable yarn-run [SCRIPT_NAME]`.

Navigate to `http://localhost:8080/` URL in your browser to see the (dead simple) web app in action. Any modification you do to the F# code will be reflected in the web page after saving. When you want to output the JS code to disk, run `dotnet fable yarn-build` and you'll get an optimized JS bundle in the `public` folder.

> You can learn more about the structure of the project generated in the README file.

Now you are a Fable developer! This is all you need to start writing web apps in F#: go on and experiment for yourself.

## A bit longer explanation

Fable has two "hearts" or sides: the .NET side is a daemon (a lightweight TCP server listening by default on port 61225) that waits for messages containing the F# source (.fs) or project (.fsproj) file to parse together with other options, and returns its contents in the form of a [Babel compliant AST](https://github.com/babel/babylon/blob/master/ast/spec.md).

On the other side, there is a JS client that takes care of watching the files, communicating with the Fable daemon, resolving npm dependencies and outputting the actual JS code. At the time of writing there are three JS clients for Fable available:

* [fable-loader](https://www.npmjs.com/package/fable-loader) is a plugin for [Webpack](https://webpack.js.org/), a powerful JS bundler with many handy features for development, like live reloading.
* [rollup-plugin-fable](https://www.npmjs.com/package/rollup-plugin-fable) for [Rollup](https://rollupjs.org/), another bundler focused on tree shaking.
* [fable-splitter](https://www.npmjs.com/package/fable-splitter) is a standalone tool which, unlike the previous ones, outputs separated files instead of a single bundle.

All the clients need a configuration file, click the links above for more details. Also note that, unlike most JS projects, Fable will reference files outside the local folder (like the [NuGet cache](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-nuget-locals)). To prevent conflicts, be sure to resolve all paths in the configuration [as well as Babel options](https://github.com/fable-compiler/fable-templates/blob/213bf4027e512751c44d6a0a6114701ae7f3343f/simple/Content/webpack.config.js#L9-L12).

The usual way to run a JS tool is a [package.json script](https://docs.npmjs.com/misc/scripts), so when you type `yarn run build` this will invoke a command named "build" within the "scripts" property of the package.json file.

It is actually possible to run the Fable daemon and the JS clients separately:

```shell
dotnet fable start

# In a different shell
yarn run start
```

> The Fable daemon must be invoked in a directory with an .fsproj including a reference to the `dotnet-fable` package, while the JS script is to be called from the folder containing the `package.json` file.

For convenience it is possible to tell Fable to automatically start the package.json script and stop whenever it finishes. The commands above can be also executed as:

```shell
dotnet fable yarn-run start
```

> Run `dotnet fable --help` to know more about the Fable daemon specific options.
