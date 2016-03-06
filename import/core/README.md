# fable-core

Core definitions for fable-compiler (F# to JS): attributes, dynamic operators...

## Installation

```sh
$ npm install --save-dev fable-core
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Compile Include="node_modules/fable-core/Fable.Core.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#load "node_modules/fable-core/Fable.Core.fs"

open Fable.Core
```
