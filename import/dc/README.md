# fable-import-dc

Fable bindings for dc.js

## Installation

```sh
$ npm install --save fable-core d3@^3.0.0 crossfilter dc
$ npm install --save-dev fable-import-dc
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-dc/Fable.Import.DC.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-dc/Fable.Import.DC.fs"

open Fable.Core
open Fable.Import
```
