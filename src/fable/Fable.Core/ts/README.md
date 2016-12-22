# fable-core

Fable core lib and bindings for native JS objects, browser and Node APIs

[![npm](https://img.shields.io/npm/v/fable-core.svg)](https://www.npmjs.com/package/fable-compiler) [![Join the chat at https://gitter.im/fable-compiler/Fable](https://badges.gitter.im/fable-compiler/Fable.svg)](https://gitter.im/fable-compiler/Fable?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[RELEASE NOTES](https://github.com/fable-compiler/Fable/blob/master/RELEASE_NOTES_CORE.md) · [Follow us on Twitter!](https://twitter.com/FableCompiler)

## Installation

```sh
npm install --save fable-core
```

## Usage

For general information on how to use Fable, please check the [documentation](http://fable.io/docs).

`fable-core` uses ES5 syntax but it calls some ES2015 APIs (`Symbol`, `Map`, `Set`...),
so you will need a polyfill like [core-js](https://github.com/zloirock/core-js) to use it
in environments that don't support these APIs.

The default distribution uses ES2015 modules in order to produce smaller sizes with bundlers
like [Rollup](http://rollupjs.org/) (embedded with `fable-compiler`) or [Webpack 2](https://webpack.js.org/).
If you are not bundling your app, you'll probably need the UMD distribution instead (see below).

### F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
  </ItemGroup>
```

### F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"

open Fable.Core
open Fable.Import
```

## Usage without bundling

### Node

If you're writing a [Node](https://nodejs.org/en/) application and don't use a module bundler, you just need
to instruct `fable-compiler` to use `fable-core` UMD distribution by passing `--coreLib fable-core/umd`
among the compiler options.

### require.js

If you target the browser and prefer to load JS dependencies asynchronously instead of bundling,
you can easily load `fable-core` files with [require.js](http://requirejs.org) as follows:

```html
<script src="node_modules/requirejs/require.js"></script>
<script>
requirejs.config({
    paths: {
        'fable-core': 'node_modules/fable-core/umd'
    }
});
</script>
```
