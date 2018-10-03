# FAQ

This is the section for quick questions or to miss the holes in the documentation. Check the first question below to learn how you can help make the FAQ a great resource for Fable users.

## General

#### How can I add a new question?

Make sure the question is not answered already either here or in the documentation. Then just [click here to edit this page in Github](https://github.com/fable-compiler/Fable/blob/master/docs/FAQ.md), add your question to the bottom and create a PR. Maintainers will write an answer and ask you if that solves the issue. If it does, the PR will be merged and the question will be added to the page so it can be referenced by other users.

## Compiler

#### Can I use Type Providers with Fable?

Erasure Type Providers compatible with netstandard should be compatible with Fable, however they must generate code that doesn't call .NET APIs unsupported by Fable. At the time of writing there are no Type Providers designed for Fable available.

#### How can I run a certain piece of code only in my production build?

Using a [Compiler Directive](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/compiler-directives).

```fsharp
#if !DEBUG
printfn "only printing in production bundle"
#endif
```

You can define compilation directives using the `define` option in your Fable client. Latest [fable-loader](https://www.npmjs.com/package/fable-loader) will automatically define `DEBUG` when running Webpack in development mode.

#### How can I make the compilation fail on incomplete pattern matches?

Fable emits a warning on incomplete pattern matching expressions by default. To turn this particular warning into an error, add the following block to your project file:

```xml
<Project Sdk="Microsoft.NET.Sdk">
  <!-- ... -->

  <PropertyGroup>
    <!-- FS0025: Incomplete pattern matches on this expression. -->
    <WarningsAsErrors>25</WarningsAsErrors>
  </PropertyGroup>

  <!-- ... -->
</Project>
```

This setting will not only make your compilation process fail on the command line, but will also make your IDE show an error at the location in the source code.

To turn more warnings into errors, separate them with commas or semicolons.

#### How are numbers compiled to JS?

All numeric types including decimal become `JS number` (64-bit floating type), except for longs and big integers. Tom Clarke has documented in much more detail the differences in numeric types between .NET and JS, [check it out](../docs/numbers.md).

## Fable.Core

#### Which operators are used in Fable.Core?

The `Fable.Core.JsInterop` module provides three main operators to interact with JS (all of them erased in generated JS code): `?` to dynamically access a property of an object, `!!` for unsafe casting (equivalent to `unbox`) and `!^` to pass arguments to a method accepting [Erased Unions](../docs/interacting.md#erase-attribute).

```fsharp
open Fable.Core.JsInterop

let jqElement = Browser.window?jQuery
let result: float = !!myObj?bar(5)
myObject?aProperty <- 30

let myMethod (arg: U3<string, int, Test>) = ...
myMethod !^5 // Same as: myMethod (U3.Case2 5)
```

#### How can I create a JS object?

The `Fable.Core.JsInterop` module provides three helpers to create JS object. The first one, `createObj`, is used to dynamically create a JS object with string keys:

```fsharp
open Fable.Core.JsInterop

// { a: 5, b: aValue }
createObj [
    "a" ==> 5
    "b" ==> aValue
]
```

If you have an interface definition for the JS object you can just use an [F# Object Expression](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/object-expressions). However, in many cases you only want to set a few fields, as when you pass options to a JS library, in those cases you can use `jsOptions` and a lambda to declare your object and have the compiler type-check the fields:

```fsharp
type IFoo =
    abstract foo: string with get, set
    abstract bar: int with get, set
    abstract optionalField: obj with get, set

let myJsApi (options: IFoo) = importMember "foo-module"

// JS: { foo: "foo", bar: 5 }
myJsApi (jsOptions(fun (o: IFoo) ->
    o.foo <- "foo"
    o.bar <- 5
))
```

If you use these helpers multiple times, you may want to define custom prefix operators for a terser syntax. For example:

```fsharp
let inline (~%) x = createObj x
let inline (~%%) x = jsOptions x

anotherJsApi %["bar" ==> 10]
myJsApi %%(fun o -> o.foo <- "foo")
```

Finally, some libraries (like React bindings) use F# unions to represent fields of JS objects. In those cases you can use `keyValueList` to transform a list of union cases to a JS object.

```fsharp
type MyOptions =
    | Foo of string
    | Bar of int

// JS: { foo: "foo", bar: 5 }
// Note we use the caseRules argument to change the first letter
keyValueList CaseRules.LowerFirst [
    Foo "foo"
    Bar 5
]
```

## Library authors

#### Which files need to be included in the nuget package?

The F# source code and the F# project file have to be included in the Nuget Package in a folder named `fable` within the package. This may sound complicated but it's only a matter of adding a couple of lines to your project file and let the `dotnet pack` command do all the rest.

```xml
<!-- Add source files to "fable" folder in Nuget package -->
<ItemGroup>
    <Content Include="*.fsproj; **\*.fs" PackagePath="fable\" />
</ItemGroup>
```

Possible cryptic error message when you don't include the project file and/or source files is `Cannot find root module`.

