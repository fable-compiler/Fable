 - tagline: Supported F# language features and libraries

# Compatibility

The compiler follows two rough guidelines when transforming the code:

* Keep the [core library](https://github.com/fsprojects/Fable/blob/master/import/core/fable-core.js) small, falling back to native JS methods when possible.
* If it makes the JS code cleaner and more idiomatic, make small changes in F# semantics
  that don't have a big impact on developers' expectations.

## Primitives

`string` and `char` compile to JS "string" while `bool` becomes "boolean".
All numeric primitives compile to JS "number" (but see _Arrays_ below).

## Arrays

`ResizeArray` (alias for `System.Generic.Collection.List`) and non-numeric
arrays are compiled to native JS arrays. Numeric arrays are compiled to
[Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray)
which should provide a performance boost when interacting with HTML5 canvas or WebGL.

> If you pass the `--clamp` argument to the compiler, byte arrays will be compiled as `Uint8ClampedArray`.

## String printing

The usual string format (with a few limitations) and printing methods in F# and .NET are available:
`Console/Debug.WriteLine`, `String.Format`, `printfn`, `sprintf`... as well as the
string instance methods.

## Regular expressions

You can use the `Regex` class in the same way as .NET, but the regex will always
behave as if passed `RegexOptions.ECMAScript` flag (e.g., no negative look-behind
or named groups).

## DateTime

You can use `DateTime` and `TimeSpan` with the same semantics as in .NET.
`TimeSpan` will just be the number of milliseconds in JS, and `DateTime` will
compile down to native JS `Date` with a `kind` property attached.

## Tuples

Tuples compile to native arrays. Desestructuring, `fst`, `snd`... works normally.

## Records

Records are compiled as [ES2015 classes](http://babeljs.io/docs/learn-es2015/#classes)
and they can be used with pattern matching (type information is available in runtime).
Record properties will be attached directly to the object instead of the prototype
making them compatible with `JSON.parse` or any other function accepting plain JS objects.

## Unions

Unions are also compiled as classes with the case name held in a `Case` property.
Particular cases: Lists are a bit more optimized (they don't have a tag) and options are erased.

> Serialized unions can be read directly with Json.NET

## Enumerable

All enumerables compile to [ES2015 iterable interface](http://babeljs.io/docs/learn-es2015/#iterators-for-of)
which means they're fully compatible with compliant JS code and native methods.
The downside is you cannot implement the `IEnumerable` interface in a data structure,
but you can use `seq`, `array` and `list` comprehensions normally.

## Seq, List and Array modules

All methods in F# `Seq` module have been implemented. There may be some still missing in `List`
and `Array` modules, but in that case the compiler will default to the corresponding
`Seq` function and build a new list or array from the response if necessary.

## Map, Set and Dictionary

Maps and Sets fall back to the [ES2015 corresponding classes](http://babeljs.io/docs/learn-es2015/#map-set-weak-map-weak-set)
for performance. Adding and removing will create new objects. `System.Collections.Generic.Dictionary` compile to ES2015 `Map` too
and allows mutable operations. Same for `System.Collections.Generic.HashSet`.

## Async

`async` computation expressions work as expected. However, `RunSynchronously` is not available and,
as JS is single-threaded, `Start` and `StartImmediate` will have the same effect.
Methods to convert to and from [JavaScript Promises](http://babeljs.io/docs/learn-es2015/#promises)
should be simple to do and are planned for the core library.

## Custom computation expression

It's possible to define custom computation expressions normally.

## Pattern matching

Pattern matching will work normally in JS and it will generate optimized
code to prevent overhead. You can match union types, records, classes or
interfaces (with `:? MyClass as x`), lists, etc. Destructuring, guards and
multiple targets are also fine.

## Active patterns

Active patterns can be used normally.

## Generics

Generic information disappears in generated code. However, it's accessible
to the compiler, so calls like `typeof<MyType>` are possible with concrete
types or with generics in **inline** functions.

## Attributes

Decorators are coming to JavaScript. However, there are competing proposals
and it's not yet clear how the definitive specs will be. For now, attributes
are only visible to the compiler which uses them, for example, when defining
[foreign interfaces](interacting.md).

## Interfaces

Interface methods are compiled to normal object methods (there's no explicit
implementation so names may collide). The interface names will be attached
to the type constructor as a Symbol-keyed property, making interface type testing
possible at runtime to do patterns like [this one proposed by Yan Cui](http://theburningmonk.com/2012/03/f-extending-discriminated-unions-using-marker-interfaces/)
to extend union types.

## Overloads

Overloads are allowed in class implementations (not for interfaces), but they'll
have a suffix attached (`_1`, `_2`...) in generated code and are not recommended.
The behaviour is similar for secondary constructors.

## Custom operators

Custom operators are possible, just note it won't be idiomatic to call
them from JS if necessary (e.g., `Time.op_Addition(ts1, ts2)`).

## Inheritance

Inheritance is possible and conforms to [ES2015 inheritance](https://github.com/lukehoban/es6features#classes).
Just be careful to use the primary constructor of the base class,
not a secondary one. Methods can be overridden too, but you won't
be able to access the base methods by casting the object.

## Lambdas

Anonymous lambdas will be curried by default. If you want to pass a callback with
more than one argument to JS code, please wrap it in a delegate first (e.g.,
`Func<_,_,_>(fun x y -> x + y)`). If the signature of the method expects a
delegate, this will be done automatically by the compiler.

## Event and Observable

Both [Events]((https://github.com/fsprojects/Fable/blob/master/src/tests/EventTests.fs) and [Observable](https://github.com/fsprojects/Fable/blob/master/src/tests/ObservableTests.fs) are supported.

## Object Expressions

Object expressions are compatible for the most general cases.

## Units of measure

Units of measure are compatible (at least for `int` and `float`) but they will
be erased from the generated JS code.


You can check the [tests](https://github.com/fsprojects/Fable/tree/master/src/tests) when in doubt. If there's a test for something,
it's supported :)
