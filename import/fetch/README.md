# fable-import-fetch

Fable bindings for Fetch API


The Fetch API provides a JavaScript interface for accessing and manipulating parts of the HTTP pipeline, such as requests and responses. 
It also provides a global fetch() method that provides an easy, logical way to fetch resources asynchronously across the network.

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
        let! records = fetchAs<MyRecord[]>("http://www.server.com/data.json" , [])
        // ...
    with
    | error -> ...
} 

// posting data to a server
async { 
    let! response = postRecord(
                      "http://www.server.com/data.json", 
                      myRecord,
                      [ RequestProperties.Headers [ 
                          HttpRequestHeaders.Accept "application/xml" ]
                      ])

    if response.ok then
        // ...
}

```
