# Code Structure

This directory contains the projects that will be compiled as netstandard/netcore apps. This is the .NET part of Fable compiler. Fable compiler is structured in three projects:

- `Fable.Core`: This library is referenced by most Fable projects as it contains the JS, Browser and Node API bindings as well as some Fable-specific helpers. At the same time, it contains the Fable AST so it's also referenced by the compiler itself. Why do it like this? Because at the beginning I thought having the AST available in Fable.Core would make it easier for plugin authors, but it seems it's not being often used for this purpose. This project has _no dependencies_.
> Note the calls to the helpers are actually solved by the _Replacements_ module (see below) as Fable cannot access to the .dll contents.

- `Fable.Compiler`: This contains the operations to make the AST transformations (F# > Fable > Babel) as well as the _Replacements_ module (that replaces calls to the BCL of FSharp.Core). Most importantly, this has the FSharp.Compiler.Service dependency and can be compiled to JS.

- `dotnet-fable`: Besides the CLI tool for dotnet SDK, this contains code that uses .NET libraries that are not compatible yet with Fable, like filesystem I/O or XML.Linq.

- `Fable.JsonConverter`: Converter to make Newtonsoft.Json compatible with Fable JSON serialization.

- `Fable.JS`: Project to compile FCS/Fable itself to JS, so it can embedded in a web page or used as a node app.
