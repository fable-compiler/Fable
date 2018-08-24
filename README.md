#  F# |> BABEL

### The compiler that emits JavaScript you can be proud of!

[![Mono Build Status](https://travis-ci.org/fable-compiler/Fable.svg "Mono Build Status")](https://travis-ci.org/fable-compiler/Fable) [![.NET Build Status](https://ci.appveyor.com/api/projects/status/vlmyxg64my74sik5?svg=true ".NET Build Status")](https://ci.appveyor.com/project/alfonsogarciacaro/fable) [![NuGet Status](https://img.shields.io/nuget/v/Fable.Compiler.svg?style=flat)](https://www.nuget.org/packages/Fable.Compiler/) [![Join the chat at https://gitter.im/fable-compiler/Fable](https://badges.gitter.im/fable-compiler/Fable.svg)](https://gitter.im/fable-compiler/Fable?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[RELEASE NOTES](https://github.com/fable-compiler/Fable/blob/master/src/dotnet/dotnet-fable/RELEASE_NOTES.md) · [Follow us on Twitter!](https://twitter.com/FableCompiler)

Fable is an F# to JavaScript compiler powered by [Babel](https://babeljs.io/), designed to produce readable and standard code. [Check the website](http://fable.io) for more information and if you find the project useful, don't forget to give us a star!

## Getting started

- Fable 1: Check the [simple app template](https://github.com/fable-compiler/fable-templates/blob/d1759f7058fc519485ce8bcd62521890573539c6/simple/Content/README.md).
- Fable 2 (beta): Check the [Fable 2 minimal sample](https://github.com/fable-compiler/fable2-samples/tree/0b4c6c011c1d8836cd3a650c08ab5a6ccc473ba7/minimal) and the [blog post announcing Fable 2](http://fable.io/blog/Introducing-2-0-beta.html).

## Building

Make sure the following **requirements** are installed in your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 2.0 or higher
- [node.js](https://nodejs.org) 6.11 or higher
- [yarn](https://yarnpkg.com)
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

Then you just need to type `./build.cmd` or `./build.sh` depending on your system to build a local copy of Fable and run the test suite. After that, if you want to quickly try changes to Fable source, please check `src/tools/QuickTest.fs`.

## Using your local build in your projects

Many of you are making really useful contributions that you also need for your own projects, however a new release may take several days. If you need the latest features the easiest way is to use `dotnet run` that will automatically build and run Fable based on latest code. For this, in macOS/Linux you can write a `fable-next` script as follows:

```shell
#!/bin/sh

dotnet --version
dotnet run -c Release -p /Users/alfonso/Fable/src/dotnet/Fable.Compiler $@ --force-pkgs
```

> Note you need the **full path to Fable.Compiler**. The `--force-pkgs` option is used to force a new copy of package sources (including fable-core) in the hidden `.fable` folder.

Make it executable (`chmod +x fable-next`) and put it somewhere included in your PATH (e.g. in macOS `/usr/local/bin`). Then in your projects, instead of running `dotnet fable webpack-dev-server`, use `fable-next webpack-dev-server`.

In Windows, you would write a `fable-next.cmd` script as follows:

```cmd
dotnet --version
dotnet run -c Release -p C:\Users\alfonso\Documents\Fable\src\dotnet\Fable.Compiler %* --force-pkgs
```

**ATTENTION**: Remember to **build fable-core JS files beforehand**. This can be done just by building the whole project (see "Building" above) or running the `FableCoreJs` FAKE target (after this, if you edit one of the src/js/fablecore JS or TS files, you can run the `FableCoreJsTypescriptOnly` which is faster).

## Contributing

Just by using Fable you're already contributing! You can help a lot the community by sharing examples and experiences in your personal blog and sending a PR to [fable-awesome](https://github.com/kunjee17/awesome-fable).

Send bug reports (ideally with minimal code to reproduce the problem) and feature requests to the [GitHub repository](https://github.com/fable-compiler/Fable/issues). Issues with the label `discussion` will be also added to ask the opinion of the community on different topics like roadmap, etc. For more immediate comments you can use the [Gitter chat](https://gitter.im/fable-compiler/Fable).

If you are up to contribute a fix or a feature yourself, you're more than welcome! Please send first an issue or a minimal Work In Progess PR so we can discuss the implementation details in advance. We're planning to add documentation for developers directly in the code, either as README files in each directory or as comments in the source. This is currently an ongoing effort.