# Getting started

When you have installed all the [prerequisites](prerequisites.md) you are good to go.

Start by installing the Simple Fable template:

```shell
dotnet new -i "Fable.Template::*"
```

Scaffold a Fable app from the template:

```shell
dotnet new fable -n FableApp
cd FableApp
```

This will create Fable project in this directory with the build instructions within the README.md file.

Install npm dependencies using yarn:

```shell
yarn install
```

Install Fable and dotnet dependencies using Paket:

```shell
dotnet restore
```

At this point, when you navigate through your code, you should have tooltips and auto-completion working.

To run the app in development mode, you run:

```shell
dotnet fable yarn-run start
```

This will start the Fable daemon and run a package.json script in parallel. In this case, `start` will run a Webpack development server. You could run the Fable daemon and webpack development server in separate commands:

```shell
# Fable daemon
dotnet fable start

# Webpack development server
yarn run start
```

You can navigate to `http://localhost:8080` in your browser to see your simple Fable running. At this point, when you make changes to your files,webpack development server will detect these changes, recompile your app and refreshes the browser on your behalf.

To build your app for production without running webpack development server, you can run:

```shell
dotnet fable yarn-run build
```

which, depending on how you configured webpack inside `webpack.config.js` will compile you app and bundle it into a single file, ready for production. By default, that file is compiled to the `public` directory and is named `bundle.js`.
