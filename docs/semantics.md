# Semantics

Fabel makes some changes in the semantics to make the result much closer to the JavaScript world, but hopefully for practical purposes most of them should be transparent to the programmer. However, it's good to know about them when interacting with JS.

## Primitives
`string` and `char` compile to JS "string" while `bool` becomes "boolean". All numeric primitives compile to JS "number" (but see _Arrays_ below).

## Arrays
`ResizeArray` (alias for `System.Generic.Collection.List`) and non-numeric arrays are compiled to native JS arrays. Numeric arrays are compiled to [Typed Arrays](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray) which should provide a performance boost when interacting with HTML5 canvas or WebGL.

## Tuples
Tuples compiled to native arrays too.

## Records
Records are compiled as [ES6 classes](http://babeljs.io/docs/learn-es2015/#classes) and they can be used with pattern matching (type information is available in runtime). At the moment, there's no structural comparison for records.

## Unions
Unions are also compiled as classes with the case name held in a `tag` property. Particular cases: Lists are a bit more optimized (they don't have a tag) and options are erased.

## Enumerable
All enumerables compile to [ES6 iterable interface](http://babeljs.io/docs/learn-es2015/#iterators-for-of) which means they're fully compatible with compliant JS code and native methods. The downside is you cannot implement the `IEnumerable` interface in a data structure, but you can use `seq, array and list comprehensions` normally.

## Map and Set
Maps and Sets fall back to the [ES6 corresponding classes](http://babeljs.io/docs/learn-es2015/#map-set-weak-map-weak-set) for performance, but this means adding and removing operations are mutable, so a bit of care is needed when manipulating them.

## Async
`async computation expressions` work as expected. However, `RunSynchronously` is not available and, as JS is single-threaded, `Start` and `StartImmediate` will have the same effect. Methods to convert to and from [JavaScript Promises](http://babeljs.io/docs/learn-es2015/#promises) are planned for the core library.

## Generics
Generic information disappears in generated code. However, they're accessible to the compiler so functions like `typeof<MyType>` are possible with concrete types (they'll be accessible to plugins too).

## Attributes
Decorators are coming to JavaScript. However, there are competing proposals and it's not yet clear how the definitive specs will be. For now, attributes are only visible to the compiler which uses them, for example, when defining [foreign interfaces](interacting.md). An important note about attributes is the compiler will only read the name and won't care about the type. Thanks to that it's not necessary to add a dependency just to use, say, the `Import` attribute (it can be defined ad hoc in the same module/namespace). 

## Interfaces
Interface methods are compiled to normal object methods (there's no explicit implementation so names may collide). The interface names will be attached to the type constructor as a Symbol-keyed property, so interface type testing will be possible in runtime. 

## Overloads
Overloads are allowed in class implementations (no for interfaces), but they'll have a suffix attached (`_1`, `_2`...) in generated code and are not recommended.

## Secondary constructors
TODO

## Inheritance
TODO: Overridden methods, custom operators

## Lambdas
TODO