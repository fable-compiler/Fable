# fable-import-d3

Fable bindings for D3

## Installation

```sh
$ npm install --save fable-core d3
$ npm install --save-dev fable-import-d3
```

## Usage

Note: To prevent naming conflicts, module names start with upper case,
while variables containing global methods are lower case.

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-d3/Fable.Import.D3.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-d3/Fable.Import.D3.fs"

open Fable.Core
open Fable.Import
```
