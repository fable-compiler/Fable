# fable-core

Fable core lib and bindings for native JS objects, browser and node APIs

[![npm](https://img.shields.io/npm/v/fable-core.svg)](https://www.npmjs.com/package/fable-compiler) [![Join the chat at https://gitter.im/fable-compiler/Fable](https://badges.gitter.im/fable-compiler/Fable.svg)](https://gitter.im/fable-compiler/Fable?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

[RELEASE NOTES](https://github.com/fable-compiler/Fable/blob/master/RELEASE_NOTES_CORE.md) · [Follow us on Twitter!](https://twitter.com/FableCompiler)

## Installation

```sh
npm install --save fable-core
```

## Usage

For general information on how to use Fable, check the [docs](http://fable-compiler.github.io/docs/compiling.html).

The default file when you import `fable-core` uses universal modules (UMD)
which can be understood by node (commonjs) or require.js (amd). There's also
a minified version `fable-core.min.js`, and if you need the library in other
module formats you can import `fable-core/es2015` or `fable-core/commonjs` instead.

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

### require.js

When targeting the browser and using AMD instead of a module bundler,
you can load Fable's core lib with [require.js](http://requirejs.org) as follows:

```html
<script src="node_modules/requirejs/require.js"></script>
<script>
requirejs.config({
    // Set the baseUrl to the path of the compiled JS code
    baseUrl: 'out',
    paths: {
        // Explicit path to core lib (relative to baseUrl, omit .js)
        'fable-core': '../node_modules/fable-core/fable-core.min'
    }
});
// Load the entry file of the app (use array, omit .js)
requirejs(["app"]);
</script>
```

## Development

The source is written in TypeScript. The use of a TypeScript-aware editor
(like [VSCode](https://code.visualstudio.com) or [ALM](http://alm.tools/))
is highly recommended.

Output files are generated as shown below:

- `fable-core.ts` (source)
  - `es2015.js` (Created from `fable-core.ts |> tsc --target ES2015`)
    - `fable-core.js` (Created from `es2015.js |> babel --plugins ...umd`)
      - `fable-core.min.js` (Created from `fable-core.js |> uglifyjs`)
    - `commonjs.js` (Created from `es2015.js |> babel --plugins ...commonjs`)

### To build

From Fable root project folder, simply run:

```sh
build FableCore
```
