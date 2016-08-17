# fable-import-mocha

Fable bindings for Mocha

## Installation

```sh
$ npm install --save fable-core
$ npm install --save-dev fable-import-mocha
```

## Usage

Check [the sample](http://fable-compiler.github.io/samples/mocha/) for more info.

### With an F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
  </ItemGroup>
  <ItemGroup>
    <Compile Include="node_modules/fable-import-mocha/Fable.Import.Mocha.fs" />
  </ItemGroup>
```

### With an F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-mocha/Fable.Import.Mocha.fs"

open Fable.Core
open Fable.Import
```
