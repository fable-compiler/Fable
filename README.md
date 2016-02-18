## F# |> BABEL

[![Build status](https://ci.appveyor.com/api/projects/status/kjo95sx0k5js50m7?svg=true)](https://ci.appveyor.com/project/alfonsogarciacaro/fable)

Fable brings together the power of the [F# compiler](http://fsharp.github.io/FSharp.Compiler.Service/) and [Babel](http://babeljs.io) to make JavaScript a true backend for F#. Some of its main features are:

- Works directly on F# source code, no compilation to .NET bytecode needed
- Optimizes F# code to generate as clean JavaScript as possible
- Passes location data to Babel to generate source maps
- Compatible with all Babel plugins and other JS development tools, like Webpack
- [Support for most of the F# core library and a bit of .NET Base Class Library](docs/compatibility.md)
- Tiny core library included without runtime
- Organizes code using ES6 modules 
- Interacts seamlessly with other JavaScript libraries
- Bonus: [compile NUnit tests to Mocha](docs/testing.md)

## Philosophy

- **Clean code**: The compiler must emit JavaScript as readable as possible which plays well with other JS code and development tools, even if this means sacrificing a bit F# semantics. 
- **Unopinionated**: The project shouldn't compromise with a specific platform and adapt to standard JavaScript in any environment: browser or node.
- **Open to the community**: If the project is to survive, it needs the support of the community. For that, maintainers should compromise to keep the project documented and make it easy for people to contribute. A plugin system is also planned.

## Installing

The project is still in a very early phase and a beta-ish version will be announced in the next few days, but if you want to tinker around you must have [F# 4.0 with .NET/Mono](http://fsharp.org) and [node.js](https://nodejs.org) installed, download the repo and then run:
```
> build.cmd Release   // on windows    
$ ./build.sh Release  // on unix
```
If everything works, follow [these instructions](docs/compiling.md) to compile a F# project or script file to JS.

## Caveats

- **Options are erased** in compiled code. This have several benefits like removing overhead and interact with native JS functions in a safer way (`null` will be `None`). However, it will lead to unexpected results if you do weird things like `Some null`. For practical purposes, Fable considers `null`, `None`, `undefined` and `F# unit` to be the same thing.

- **Information about generic types is not included** in the generated JavaScript, so code that depends on this information to be known at runtime for method dispatching may have unexpected behaviour.

- At the moment, comparison for objects defaults to JS reference comparison so there's **no structural comparison** for records and unions, but this may change in the future upon users' feedback.

- **Map and Set** default to the new ES6 Map and Set classes which must increase performance, but the downside is adding and removing operations are **mutable**.

- When debugging the F# code using source maps you may see the debugger jump directly to the last expression in many functions. This is normal and due to the optimizations performed on the generated code.

To know more, read [Semantics](docs/semantics.md).
    