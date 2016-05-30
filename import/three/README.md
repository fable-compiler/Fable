# fable-import-three

Fable bindings for three.js

## Installation

```shell
$ npm install --save fable-core three
$ npm install --save-dev fable-import-three
```

## Usage

Note: To prevent naming conflicts, `THREE` as a module is upper case,
while `three` as a value containing global methods is lower case.

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-three/Fable.Import.Three.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-three/Fable.Import.Three.fs"

open Fable.Core
open Fable.Import
```
