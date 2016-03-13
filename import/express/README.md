# fable-import-react

Fable bindings for React

## Installation

```sh
$ npm install --save express
$ npm install --save-dev fable-import fable-import-express
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-import/Fable.Import.dll" />
    <Compile Include="node_modules/fable-import-express/Fable.Import.Express.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-import/Fable.Import.dll"
#load "node_modules/fable-import-express/Fable.Import.Express.fs"

open Fable.Core
open Fable.Import
```
