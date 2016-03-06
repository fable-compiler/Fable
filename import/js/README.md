# fable-import-js

Fable bindings for native JS objects and functions

## Installation

```sh
$ npm install --save-dev fable-core fable-import-js
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Compile Include="node_modules/fable-core/Fable.Core.fs" />
    <Compile Include="node_modules/fable-import-js/Fable.Import.JS.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#load "node_modules/fable-core/Fable.Core.fs"
#load "node_modules/fable-import-js/Fable.Import.JS.fs"

open Fable.Core
open Fable.Import.JS
```
