# fable-providers-regex

Regex Type Providers for Fable

## Installation

```sh
$ npm install --save-dev fable-providers-regex
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-providers-regex/Fable.Providers.Regex.dll" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-providers-regex/Fable.Providers.Regex.dll"

open Fable.Providers.Regex

let reg = SafeRegex.Create<"my\s+regexP?", ignoreCase=true>()
```
