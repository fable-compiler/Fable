# fable-import-express

Fable bindings for Express

## Installation

```sh
$ npm install --save express fable-core
$ npm install --save-dev fable-import-express
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
  </ItemGroup>
  <ItemGroup>
    <Compile Include="node_modules/fable-import-express/Fable.Import.Express.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-express/Fable.Import.Express.fs"

open Fable.Core
open Fable.Import
```
