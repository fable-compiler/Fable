# fable-import-react

Fable bindings for React

## Installation

```sh
$ npm install --save react react-dom
$ npm install --save-dev fable-import fable-import-react
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-import/Fable.Import.dll" />
    <Compile Include="node_modules/fable-import-react/Fable.Import.React.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-import/Fable.Import.dll"
#load "node_modules/fable-import-react/Fable.Import.React.fs"

open Fable.Core
open Fable.Import
```
