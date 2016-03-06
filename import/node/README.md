# fable-import-node

Fable bindings for native Node objects and functions

## Installation

```sh
$ npm install --save-dev fable-core fable-import-js fable-import-node
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Compile Include="node_modules/fable-core/Fable.Core.fs" />
    <Compile Include="node_modules/fable-import-js/Fable.Import.JS.fs" />
    <Compile Include="node_modules/fable-import-node/Fable.Import.Node.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#load "node_modules/fable-core/Fable.Core.fs"
#load "node_modules/fable-import-js/Fable.Import.JS.fs"
#load "node_modules/fable-import-node/Fable.Import.Node.fs"

open Fable.Core
open Fable.Import.JS
open Fable.Import.Node
```
