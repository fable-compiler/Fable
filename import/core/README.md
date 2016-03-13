# fable-import

Fable bindings for native JS objects, browser and node APIs

## Installation

```sh
$ npm install --save-dev fable-import
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-import/Fable.Import.dll" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-import/Fable.Import.dll"

open Fable.Core
open Fable.Import
```
