# F# |> BABEL

### The compiler that emits JavaScript you can be proud of!

[![Mono Build Status](https://travis-ci.org/fable-compiler/Fable.svg "Mono Build Status")](https://travis-ci.org/fable-compiler/Fable) [![.NET Build Status](https://ci.appveyor.com/api/projects/status/vlmyxg64my74sik5?svg=true ".NET Build Status")](https://ci.appveyor.com/project/alfonsogarciacaro/fable) [![npm](https://img.shields.io/npm/v/fable-compiler.svg)](https://www.npmjs.com/package/fable-compiler) [![Join the chat at https://gitter.im/fable-compiler/Fable](https://badges.gitter.im/fable-compiler/Fable.svg)](https://gitter.im/fable-compiler/Fable?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[RELEASE NOTES](https://github.com/fable-compiler/Fable/blob/master/RELEASE_NOTES_COMPILER.md) · [Follow us on Twitter!](https://twitter.com/FableCompiler)

Fable brings together the power of the [F# compiler](http://fsharp.github.io/FSharp.Compiler.Service/)
and [Babel](http://babeljs.io) to make JavaScript a true backend for F#.
Some of its main features are:

- Works directly on F# source code, no compilation to .NET bytecode needed
- Optimizes F# code to generate as clean JavaScript as possible
- Passes location data to Babel to generate source maps
- Compatible with all Babel plugins and other JS development tools, like [Webpack](https://webpack.github.io)
- Support for most of the [F# core library](http://fable-compiler.github.io/docs/compatibility.html) and a bit of .NET Base Class Library
- Tiny core library included (around 20KB minified and gzipped) with no runtime
- Organizes code using ES6 modules
- Interacts seamlessly with other [JavaScript libraries](http://fable-compiler.github.io/docs/interacting.html)
- Bonus: compile [NUnit tests to Mocha](http://fable-compiler.github.io/docs/compiling.html#Testing)

## Usage

Fable is distributed through [npm](https://www.npmjs.com/package/fable-compiler)! You can install and run it just by typing:

```shell
npm install -g fable-compiler
fable path/to/your/project.fsproj
```

> Note the package name is `fable-compiler` while the command is just `fable`

You can find more detailed instructions about F# to JS compilation in the [docs](http://fable-compiler.github.io/docs/compiling.html).
Also you can have a look at the [samples](http://fable-compiler.github.io/samples.html) or download them from [here](https://ci.appveyor.com/api/projects/alfonsogarciacaro/fable/artifacts/samples.zip).

## Requirements

You must have [F# 4](http://fsharp.org) and [node 4.4](https://nodejs.org/en/) or higher installed in your computer.

If you are having problems to use Fable under Windows, try the following:

- Make sure `fsc.exe` is added to the path.
- Install MSBuild tools 2013.

## Contributing

Just by using Fable you're already contributing! You can help a lot the community
by sharing examples and experiences in your personal blog or by sending a PR to Fable's
website ([see this](https://github.com/fable-compiler/Fable/issues/162) for more info).

Send bug reports (ideally with minimal code to reproduce the problem) and feature requests
to the [GitHub repository](https://github.com/fable-compiler/Fable/issues). Issues with the label `discussion` will be also added to ask the opinion of the community
on different topics like the logo, roadmap, etc. For more immediate comments you can use the [Gitter chat](https://gitter.im/fable-compiler/Fable).

A [plugin system](http://fable-compiler.github.io/docs/plugins.html) is also available
to allow you extend Fable according to you needs.

## Caveats

- **Options are erased** in compiled code. This has several benefits like removing overhead
  and interacting with native JS functions in a safer way (`null` will be `None`).
  However, it will lead to unexpected results if you do weird things like wrapping `null` in `Some`.
  For practical purposes, Fable considers `null`, `undefined`, `None` and `unit` to be the same thing.

- **Information about generic types is not included** in the generated JavaScript, so code that
  depends on this information to be known at runtime for method dispatching may have unexpected behaviour.

To know more, read [Compatibility](http://fable-compiler.github.io/docs/compatibility.html).

## Acknowledgements

Of course, this project wouldn't have been possible without the fantastic work of the [F# compiler](http://fsharp.github.io/FSharp.Compiler.Service/)
and [Babel](http://babeljs.io) teams. I hope they feel proud seeing how their work has met in
a very unexpected way, giving developers even more possibilities to build great apps.

The awesome F# community has played a big role in making this possible. I've met incredible
people and it's impossible to list all the names without forgetting anyone, but I'd like to
give a particular mention to [Zach Bray](https://github.com/ZachBray) for his work on [FunScript](http://funscript.info/), [Don Syme](https://github.com/dsyme) (the fact that the designer
of the language himself shows interest in your work, no matter how humble it is, is really a big push!)
and [Krzysztof Cieślak](https://github.com/Krzysztof-Cieslak) (I always have to look up the name to spell it correctly) because he's shown that
F# is a perfect fit for a [big project targeting JS](http://ionide.io/).

And finally I'd like to thank my partner (is it too old-fashioned to say wife?) for bearing with me
everyday.
