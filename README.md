# F# |> BABEL

### The compiler that emits JavaScript you can be proud of!

[![Mono Build Status](https://travis-ci.org/fable-compiler/Fable.svg?branch=master "Mono Build Status")](https://travis-ci.org/fable-compiler/Fable) [![.NET Build Status](https://ci.appveyor.com/api/projects/status/vlmyxg64my74sik5/branch/master?svg=true ".NET Build Status")](https://ci.appveyor.com/project/alfonsogarciacaro/fable) [![npm version](https://badge.fury.io/js/fable-compiler.svg)](https://www.npmjs.com/package/fable-compiler) [![Join the chat at https://gitter.im/fable-compiler/Fable](https://badges.gitter.im/fable-compiler/Fable.svg)](https://gitter.im/fable-compiler/Fable?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[Follow us on Twitter!](https://twitter.com/FableCompiler)

Fable is an F# to JavaScript compiler powered by [Babel](https://babeljs.io/), designed to produce readable and standard code. [Check the website](http://fable.io) for more information and if you find the project useful, don't forget to give us a star!

## Getting started

Check [this page](https://fable.io/docs/getting_started.html).

## Building

Make sure the following **requirements** are installed in your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core)
- [node.js](https://nodejs.org) with npm

Then run `npm install` to install dependencies and `npm run build` to start the build. Check [build.fsx](https://github.com/fable-compiler/Fable/blob/4839311afe4cfc3fd0849915c7cdf831ca1ab74c/build.fsx#L218) for other build targets. 
For example: `npm run build compiler`.

After that, if you want to quickly try changes to Fable source, please check `src/quicktest/QuickTest.fs`.

## Using your local build in your projects

Many of you are making really useful contributions that you also need for your own projects, however a new release may take several days. If you need the latest features you can tell `fable-compiler` to use a local build. For this, use the `cli.path` option. For example when using the `fable-loader` you can include the following in your webpack.config.js:

```js
  module: {
    rules: [
      {
        test: /\.fs(x|proj)?$/,
        use: {
            loader: "fable-loader",
            options: {
                cli: {
                    // This should be the path to your local clone of Fable
                    path: "../Fable/src/Fable.Cli"
                }
            }
        }
      }
    ]
  }
```


**ATTENTION**: Remember to **build fable-library files beforehand**. This can be done just by building the whole project (see "Building" above) or running the `FableLibrary` FAKE target (after this, if you edit one of the src/js/fable-library JS or TS files, you can run the `FableLibraryTypescriptOnly` which is faster).

## Contributing

Just by using Fable you're already contributing! You can help a lot the community by sharing examples and experiences in your personal blog and sending a PR to [fable-awesome](https://github.com/kunjee17/awesome-fable).

Send bug reports (ideally with minimal code to reproduce the problem) and feature requests to the [GitHub repository](https://github.com/fable-compiler/Fable/issues). To interact with the community you can use the [Gitter chat](https://gitter.im/fable-compiler/Fable) but please note maintainers are not checking the chat regularly.

If you are up to contribute a fix or a feature yourself, you're more than welcome! Please send first an issue or a minimal Work In Progess PR so we can discuss the implementation details in advance.
