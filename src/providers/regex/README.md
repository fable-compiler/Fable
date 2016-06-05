# fable-providers-regex

Regex Type Providers for Fable. At the moment there's a `SafeRegex` type
which provides you a way to create regular expression and tell you
immediately if the pattern is malformed.

Please note that only **compatible type providers** like this one can be used
with Fable. But don't worry, more providers are coming soon to Fable!

## Installation

```sh
$ npm install --save-dev fable-providers-regex
```

> You need **fable-compiler@0.3.16** or higher to use the provider.

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
