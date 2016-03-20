# fable-import-vscode

Fable bindings for Visual Studio Code

## Installation

```sh
$ npm install --save-dev fable-import fable-import-vscode
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-import/Fable.Import.dll" />
    <Compile Include="node_modules/fable-import-vscode/Fable.Import.VSCode.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-import/Fable.Import.dll"
#load "node_modules/fable-import-vscode/Fable.Import.VSCode.fs"

open Fable.Core
open Fable.Import
```
