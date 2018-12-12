#  F# |> BABEL

### The compiler that emits JavaScript you can be proud of!

[![Mono Build Status](https://travis-ci.org/fable-compiler/Fable.svg "Mono Build Status")](https://travis-ci.org/fable-compiler/Fable) [![.NET Build Status](https://ci.appveyor.com/api/projects/status/vlmyxg64my74sik5?svg=true ".NET Build Status")](https://ci.appveyor.com/project/alfonsogarciacaro/fable) [![NuGet Status](https://img.shields.io/nuget/v/dotnet-fable.svg?style=flat)](https://www.nuget.org/packages/dotnet-fable/) [![Join the chat at https://gitter.im/fable-compiler/Fable](https://badges.gitter.im/fable-compiler/Fable.svg)](https://gitter.im/fable-compiler/Fable?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[RELEASE NOTES](https://github.com/fable-compiler/Fable/blob/master/src/dotnet/Fable.Compiler/RELEASE_NOTES.md) · [Follow us on Twitter!](https://twitter.com/FableCompiler)

Fable is an F# to JavaScript compiler powered by [Babel](https://babeljs.io/), designed to produce readable and standard code. [Check the website](http://fable.io) for more information and if you find the project useful, don't forget to give us a star!

## Getting started

Check [this document](docs/getting_started.md).

## Building

Make sure the following **requirements** are installed in your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
- [node.js](https://nodejs.org) 6.11 or higher
- [yarn](https://yarnpkg.com)
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

Then you just need to type `./build.cmd` or `./build.sh` depending on your system to build a local copy of Fable and run the test suite. After that, if you want to quickly try changes to Fable source, please check `src/tools/QuickTest.fs`.

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
                    path: "../Fable/src/dotnet/Fable.Compiler"
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

Send bug reports (ideally with minimal code to reproduce the problem) and feature requests to the [GitHub repository](https://github.com/fable-compiler/Fable/issues). Issues with the label `discussion` will be also added to ask the opinion of the community on different topics like roadmap, etc. For more immediate comments you can use the [Gitter chat](https://gitter.im/fable-compiler/Fable).

If you are up to contribute a fix or a feature yourself, you're more than welcome! Please send first an issue or a minimal Work In Progess PR so we can discuss the implementation details in advance. We're planning to add documentation for developers directly in the code, either as README files in each directory or as comments in the source. This is currently an ongoing effort.
