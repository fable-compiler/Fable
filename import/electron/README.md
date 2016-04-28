# fable-import-electron

Fable bindings for Electron

## Installation

```sh
$ npm install --save fable-core
$ npm install --save-dev electron-prebuilt fable-import-electron
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-electron/Fable.Import.Electron.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-electron/Fable.Import.Electron.fs"

open Fable.Core
open Fable.Import
```
