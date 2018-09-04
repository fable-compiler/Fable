# .NET and F# Compatibility

> This document applies to Fable 1. It will be updated soon with Fable 2 information.

Fable provides support for some classes of .NET BCL (Base Class Library) and most of FSharp.Core library. When possible, Fable translates .NET types and methods to native JavaScript APIs for minimum overhead.

## .NET Base Class Library

The following classes are translated to JS and most of their methods (static and instance) should be available in Fable.

.NET                                  | JavaScript
--------------------------------------|----------------------------
Numeric Types                         | number
Arrays                                | Array / Typed Arrays
Events                                | fable-core/Event
System.Boolean                        | boolean
System.Char                           | string
System.String                         | string
System.Guid                           | string
System.TimeSpan                       | number
System.DateTime                       | Date
System.DateTimeOffset                 | Date
System.Timers.Timer                   | fable-core/Timer
System.Collections.Generic.List       | Array
System.Collections.Generic.HashSet    | Set
System.Collections.Generic.Dictionary | Map
System.Text.RegularExpressions.Regex  | RegExp
System.Lazy                           | fable-core/Lazy
System.Random                         | {}
System.Math                           | (native JS functions)

The following static methods are also available:

- `System.Console.WriteLine` (also with formatting)
- `System.Diagnostics.Debug.WriteLine` (also with formatting)
- `System.Diagnostics.Debug.Assert(condition: bool)`
- `System.Diagnostics.Debugger.Break()`
- `System.Activator.CreateInstance<'T>()`

There is also support to convert between numeric types and to parse strings, check [the convert tests](https://github.com/fable-compiler/Fable/blob/master/tests/Main/ConvertTests.fs).

### Caveats

- All numeric types including `decimal` become JS `number` (64-bit floating type), except for `int64`, `uint64` and `bigint`. Check [this document](numbers.md) to learn more about the differences in numeric types between .NET and JS.
- Numeric arrays are compiled to [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) when possible.
- No bound checks for numeric types (unless you do explicit conversions like `byte 500`) nor for array indices.
- `Regex` will always behave as if passed `RegexOptions.ECMAScript` flag (e.g., no negative look-behind or named groups).

## FSharp.Core

Most of FSharp.Core operators are supported, as well as formatting with `sprintf`, `printfn` or `failwithf` (`String.Format` is also available).
The following types and/or corresponding modules from FSharp.Core lib will likewise translate to JS:

.NET              | JavaScript
------------------|----------------------------------------------------------
Tuples            | Array
Option            | (erased)
Choice            | fable-core/Choice
Result            | fable-core/Result
String            | fable-core/String (module)
Seq               | [Iterable](http://babeljs.io/docs/learn-es2015/#iterators-for-of)
List              | fable-core/List
Map               | fable-core/Map
Set               | fable-core/Set
Async             | fable-core/Async
Event             | fable-core/Event (module)
Observable        | fable-core/Observable (module)
Arrays            | Array / Typed Arrays
Events            | fable-core/Event
MailboxProcessor  | fable-core/MailboxProcessor (limited support)

The following F# semantic and syntactic features are also available:

- Records and Unions
- Structural Equality/Comparison
- Comprehensions (seq, array, list)
- Computation Expressions
- Pattern Matching
- Active Patterns
- Object Expressions
- Units of measure

### Caveats

- Options are **erased** in JS (`Some 5` becomes just `5` in JS and `None` translates to `null`). This is needed for example, to represent TypeScript [optional properties](https://www.typescriptlang.org/docs/handbook/interfaces.html#optional-properties). However in a few cases (like nested options) there is an actual representation of the option in the runtime.
- `Async.RunSynchronously` is not supported.
- `MailboxProcessor` is single-threaded in JS and currently only `Start`, `Receive`, `Post` and `PostAndAsyncReply` are implemented (`cancellationToken` or `timeout` optional arguments are not supported).

## Object Oriented Programming

**Classes** translate to [ES6 classes](https://github.com/lukehoban/es6features#classes) and most of their .NET/F# characteristics are available: properties, overloads, secondary constructors, custom operators, etc.

**Inheritance** is possible and conforms to [ES2015 inheritance](https://github.com/lukehoban/es6features#classes) but must be done by calling the _primary constructor_ of the base class. Methods can be overridden and call the base implementation. Just note it won't be possible to access the base implementation from outside by casting the object. Example:

```fsharp
type A() =
    member x.Foo() = "Hello"

type B() =
    inherit A()
    member x.Foo() = base.Foo() + " World!"

// This prints "Hello World!" both in .NET and JS
B().Foo() |> printfn "%s"

// This prints "Hello" in .NET and "Hello World!" in JS
(B() :> A).Foo() |> printfn "%s"
```

**Interface** methods are compiled as normal object methods and it's possible to test against an interface (e.g. `x :? IComparable`) for types defined in F# code.

## Generics

Generics are erased by default in generated code. However, it is still possible to access generic information in _inline_ functions (like `typeof<'T>`) or methods decorated with `PassGenerics` attribute.

> **Caveat**: Functions decorated with `PassGenericsAttribute` may work unexpectedly if called from external JS code.

## Reflection

Fable keeps metadata information in the declared types to allow limited support for .NET/F# reflection. Please check the [Fable reflection tests](https://github.com/fable-compiler/Fable/blob/master/tests/Main/ReflectionTests.fs) to see which methods are available in latest versions.
