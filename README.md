# Fable: F# |> JS

[![Nuget](https://img.shields.io/nuget/v/Fable.svg?maxAge=0&colorB=brightgreen)](https://www.nuget.org/packages/Fable) [![Build](https://github.com/fable-compiler/Fable/actions/workflows/build.yml/badge.svg)](https://github.com/fable-compiler/Fable/actions/workflows/build.yml) [![Join the chat at https://gitter.im/fable-compiler/Fable](https://badges.gitter.im/fable-compiler/Fable.svg)](https://gitter.im/fable-compiler/Fable?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[Follow us on Twitter!](https://twitter.com/FableCompiler)

Fable is an F# to JavaScript compiler powered [FSharp Compiler Services](https://fsharp.github.io/fsharp-compiler-docs/fcs/), designed to make F# a first-class citizen of the JavaScript ecosystem. [Check the website](http://fable.io) for more information and if you find the project useful, don't forget to give us a star!

> Fable actually uses a fork of FCS with a few tweaks. Binaries are in `lib/fcs` folder. See [this PR](https://github.com/ncave/fsharp/pull/2) for more info.

## Getting started

Check [this page](https://fable.io/docs/2-steps/setup.html).

## Building

### Requirements

#### Use VSCode Dev Container

You can use VSCode Dev Container to get a preconfigured environment both with requirements and VSCode extensions.

1. You need to have docker installed and running.
2. [Install the Dev Container extension in VSCode](vscode:extension/ms-vscode-remote.remote-containers)
3. Open the project in VSCode and click on the green button in the bottom left corner.

#### Use your machine

Make sure the following **requirements** are installed in your system:

- [dotnet SDK 6 or higher](https://dotnet.microsoft.com/download)
- [node.js](https://nodejs.org) with npm
- [Python 3](https://www.python.org/) is installed and available as `python`
- [Poetry](https://python-poetry.org/)
- [Rust](https://www.rust-lang.org/tools/install)
- [Dart](https://dart.dev/get-dart)

### Build

Run `./build.sh` or `./build.cmd` to see the build options.

When using VSCode, you can also run the build tasks from the command palette (Ctrl+Shift+P) by typing `Run Task` and selecting the task you want to run.

We also configured several debug configurations that you can use from the debug panel (Ctrl+Shift+D). This is useful as you can attach the debugger to the Fable compiler process to check what's going on.

## Contributing

Just by using Fable you're already contributing! You can help the community a lot by sharing examples and experiences in your personal (or Fable's) blog and/or by editing the [Fable Resources](https://fable.io/resources.html) page.

Send bug reports (ideally with minimal code to reproduce the problem) and feature requests to this [GitHub repository](https://github.com/fable-compiler/Fable/issues). To interact with the community you can use the [Gitter chat](https://gitter.im/fable-compiler/Fable) but please note maintainers are not checking the chat regularly.

If you are up to contribute a fix or a feature yourself, you're more than welcome! Please send first an issue or a minimal Work In Progess PR so we can discuss the implementation details in advance.

### List of changelogs

- [fable-standalone](src/fable-standalone/CHANGELOG.md)
- [Fable.Core](src/Fable.Core/CHANGELOG.md)
- [Fable.Cli](src/Fable.Cli/CHANGELOG.md)
- [fable-library](src/fable-library-ts/CHANGELOG.md)
- [fable-metadata](src/fable-metadata/CHANGELOG.md)
- [fable-compiler](src/fable-compiler-js/CHANGELOG.md)
- [Fable.AST](src/Fable.AST/CHANGELOG.md)
- [Fable.PublishUtils](src/Fable.PublishUtils/CHANGELOG.md)
- [Fable.Compiler](src/Fable.Compiler/CHANGELOG.md)
