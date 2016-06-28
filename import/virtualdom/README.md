# fable-import-virtualdom

Fable bindings for virtual-dom

## Installation

```sh
$ npm install --save virtual-dom fable-core
$ npm install --save-dev fable-import-virtualdom
```

## Usage

### In an F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-virtualdom/Fable.Helpers.Virtualdom.fs" />
  </ItemGroup>
```

### In an F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-virtualdom/Fable.Helpers.Virtualdom.fs"

open Fable.Core
open Fable.Import
open Fable.Helpers.VirtualDom
```
