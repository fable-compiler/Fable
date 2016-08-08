# fable-import-fetch

Fable bindings for Fetch API
See: https://developer.mozilla.org/en/docs/Web/API/Fetch_API

## Installation

```sh
$ npm install --save fable-core
$ npm install --save-dev fable-import-fetch
```

## Usage

### In a F# project (.fsproj)

```xml
  <ItemGroup>
    <Reference Include="node_modules/fable-core/Fable.Core.dll" />
    <Compile Include="node_modules/fable-import-fetch/Fable.Import.Fetch.fs" />
    <Compile Include="node_modules/fable-import-fetch/Fable.Helpers.Fetch.fs" />
  </ItemGroup>
```

### In a F# script (.fsx)

```fsharp
#r "node_modules/fable-core/Fable.Core.dll"
#load "node_modules/fable-import-fetch/Fable.Import.Fetch.fs"
#load "node_modules/fable-import-fetch/Fable.Helpers.Fetch.fs"

```

### Using Fetch from F#

```fsharp
open Fable.Core
open Fable.Import.Fetch
open Fable.Helpers.Fetch

// getting data and parsing from JSON
async { 
    try 
        let! records =
            fetchAs<MyRecord[]>(RequestInfo.Url "http://www.server.com/data.json" 
        // ...
    with
    | error -> ...
} 

// posting data to a server
async { 
    let! response =
        fetchAsyncWithInit(
            RequestInfo.Url "http://www.server.com/data.json" , 
            [ RequestProperties.Method HttpMethod.POST
              RequestProperties.Body (unbox "hello world3!!")])

        // ...              
}

```
