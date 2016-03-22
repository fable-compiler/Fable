# Compatibility

The compiler follows two rough guidelines when transforming the code:

* Keep the [core library](/src/fable-js/fable-core.js) small, so fall back to native JS methods when possible.
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
Records are compiled as [ES6 classes](http://babeljs.io/docs/learn-es2015/#classes)
and they can be used with pattern matching (type information is available in runtime).
At the moment, there's no structural comparison for records.

## Unions
Unions are also compiled as classes with the case name held in a `tag` property.
Particular cases: Lists are a bit more optimized (they don't have a tag) and options are erased.

## Enumerable
All enumerables compile to [ES6 iterable interface](http://babeljs.io/docs/learn-es2015/#iterators-for-of)
which means they're fully compatible with compliant JS code and native methods.
The downside is you cannot implement the `IEnumerable` interface in a data structure,
but you can use `seq, array and list comprehensions` normally.

## Seq, List and Array modules
All methods in F# `Seq` module have been implemented. There may be some methods still missing
in `List` and `Array` modules, but in that case the compiler will default to the corresponding
`Seq` function and build a new list or array from the response if necessary.

## Map, Set and Dictionary
Maps and Sets fall back to the [ES6 corresponding classes](http://babeljs.io/docs/learn-es2015/#map-set-weak-map-weak-set)
for performance. Adding and removing will create new objects. `System.Collections.Generic.Dictionary` compile to ES6 `Map` too
and allows mutable operations.

## Async
`async computation expressions` work as expected. However, `RunSynchronously` is not available and,
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
[foreign interfaces](interacting.md). An important note about attributes is
the compiler will only read the name and won't care about the type. Thanks
to that it's not necessary to add a dependency just to use, say, the `Import`
attribute (it can be defined ad hoc in the same module/namespace).

> It's planned to allow compilation of attributes in the near future
using custom Babel plugins like [babel-plugin-angular2-annotations](https://github.com/shuhei/babel-plugin-angular2-annotations).

## Interfaces
Interface methods are compiled to normal object methods (there's no explicit
implementation so names may collide). The interface names will be attached
to the type constructor as a Symbol-keyed property, so interface type testing
will be possible at runtime to do patterns like [this one proposed by Yan Cui](http://theburningmonk.com/2012/03/f-extending-discriminated-unions-using-marker-interfaces/)
to extend union types.

## Overloads
Overloads are allowed in class implementations (not for interfaces), but they'll
have a suffix attached (`_1`, `_2`...) in generated code and are not recommended.
The behaviour is similar for secondary constructors.

## Custom operators
Custom operators are possible, just note it won't be idiomatic to call
them from JS if necessary (e.g., `Time["+"](ts1, ts2)`).

## Inheritance
Inheritance is possible and conforms to [ES6 inheritance](https://github.com/lukehoban/es6features#classes).
Just be careful to use the primary constructor of the base class,
not a secondary one. Methods can be overridden too, but you won't
be able to access the base methods by casting the object. 

## Lambdas
Anonymous lambdas will be curried by default. If you want to pass a callback with
more than one argument to JS code, please wrap it in a delegate first (e.g.,
`Func<_,_,_>(fun x y -> x + y)`). If the signature of the method expects a
delegate, this will be done automatically by the compiler.

## Event and Observable
Events are not implemented yet, please use [Observable](/test/ObservableTests.fs) instead.

## Object Expressions
Object expressions are compatible for the most general cases.

## Units of measure
Units of measure should be compatible (at least for `int` and `float`) but they will
be erased from the generated JS code.

You can check the [tests](/test) when in doubt. If there's a test for something,
it's supported :)