# FAQ

This is the section for quick questions or to miss the holes in the documentation. Check the first question below to learn how you can help make the FAQ a great resource for Fable users.

## General

* How can I add a new question?

Make sure the question is not answered already either here or in the documentation. Then just [click here to edit this page in Github](https://github.com/fable-compiler/Fable/edit/master/docs/FAQ.md), add your question to the bottom and create a PR. Maintainers will write an answer and ask you if that solves the issue. If it does, the PR will be merged and the question will be added to the page so it can be referenced by other users.

## Compiler

* How can I run a certain piece of code only in my production build?

Using a [Compiler Directive](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/compiler-directives).

```fsharp
#if !DEBUG
printfn "only printing in production bundle"
#endif
```

You can define compilation directives using the `define` option in your Fable client. Latest [fable-loader](https://www.npmjs.com/package/fable-loader) will automatically define `DEBUG` when running Webpack in development mode.

* How are numbers compiled to JS?

All numeric types including decimal become `JS number` (64-bit floating type), except for longs and big integers. Tom Clarke has documented in much more detail the differences in numeric types between .NET and JS, [check it out](../docs/numbers.md).

## Special symbols

* ? (property accessor)

Allows you to dynamically access a property of an object.

```fsharp
let jqElement = Browser.window?jQuery
```

* !^ (erased upcast operator)

Syntax sugar so you don't have to type `U2.Case1`, `U3.Case2`, etc...

```fsharp
let map (userPosition : Leaflet.LatLngTuple) =
    RL.map [ RL.MapProps.Center !^userPosition
             RL.MapProps.Zoom 14.
             RL.MapProps.Key "map"
             RL.MapProps.Style [ Height "100%"; Width "100%" ] ]
```

* !! (dynamic casting erased)

This is the equivalent of `unbox`.
Can be interpreted as `I don't care what the type checker tells me, I'm writing JS!`

```fsharp
!!myObj?bar(5) : float
```

## Library authors

* Which files need to be included in the nuget package?

The F# source code and the F# project file have to be included in the Nuget Package in a folder named fable within the package. This may sound complicated but it's only a matter of adding a couple of lines to your project file and let the `dotnet pack` command do all the rest.

```fsharp
<!-- Add source files to "fable" folder in Nuget package -->
<ItemGroup>
    <Content Include="*.fsproj; **\*.fs" PackagePath="fable\" />
</ItemGroup>
```

Possible cryptic error message when you don't include the project file and/or source files is `Cannot find root module`.