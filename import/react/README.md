# fable-import-react

Fable bindings for React

## Installation

```sh
$ npm install --save react react-dom fable-core
$ npm install --save-dev fable-import-react
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-react/Fable.Import.React.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-react/Fable.Import.React.fs"

open Fable.Core
open Fable.Import
```
