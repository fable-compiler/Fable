# Fable Python

This experimental support for Python in Fable. Transforming the Fable AST into a Python AST, then printing to untyped
Python source code.

## Current Design

| F#           |      Python      | Comment                                                                           |
|--------------|:----------------:|-----------------------------------------------------------------------------------|
| List (F#)    |     List.fs      | F# immutable list                                                                 |
| ResizeArray  |      `list`      | Python [list](https://docs.python.org/3/library/stdtypes.html#typesseq-list)      |
| Map          |      Map.fs      | F# immutable map                                                                  |
| Record       |     types.py     | Custom Record class. Replace with `dict`?                                         |
| Option       |      Erased      | F# `None` will be translated to Python `None`                                     |
| An. Record   |      `dict`      | Python [dict](https://docs.python.org/3/library/stdtypes.html#mapping-types-dict) |
| dict         |      `dict`      |                                                                                   |
| Dictionary   |      `dict`      | MutableMap if comparer                                                            |
| tuple        |     `tuple`      | Python [tuple](https://docs.python.org/3/library/stdtypes.html#tuples)            |
| Decimal      |    `decimal`     |                                                                                   |
| DateTime     |    `datetime`    |                                                                                   |
| string       |     `string`     |                                                                                   |
| char         |     `string`     |                                                                                   |
| `[]<byte>`   |   `bytearray`    | Python mutable byte string                                                        |
| `[]<sbyte>`  | `array("b", [])` | Python array module                                                               |
| `[]<int16>`  | `array("h", [])` | Python array module                                                               |
| `[]<uint16>` | `array("H", [])` | Python array module                                                               |
| `[]<int>`    | `array("i", [])` | Python array module                                                               |
| `[]<uint32>` | `array("I", [])` | Python array module                                                               |
| `[]<long>`   | `array("l", [])` | Python array module                                                               |
| `[]<uint64>` | `array("L", [])` | Python array module                                                               |
| `[]<float>`  | `array("d", [])` | Python array module                                                               |
| `[]<single>` | `array("f", [])` | Python array module                                                               |
| `[]<'T>`     |      `list`      | Python list module                                                                |

## Numerics

Currently, every number (`int`, `int32`, `int64`, bigint) will all be translated to Python `int`. Python ints are variable in
length and there is no unsigned alternative. For more information see https://docs.python.org/3.9/library/stdtypes.html
and https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/basic-types.

| F#               | .NET    | Python |
|:-----------------|:--------|--------|
| bool             | Boolean | bool   |
| int              | Int32   | int    |
| byte             | Byte    | int    |
| sbyte            | SByte   | int    |
| int16            | Int16   | int    |
| int64            | Int64   | int    |
| uint16           | Uint16  | int    |
| uint32           | Uint32  | int    |
| uint64           | Uint64  | int    |
| float / double   | Double  | float  |
| float32 / single | Single  | float  |

## Interfaces and Protocols

| .NET          |         Python          | Comment                                                                                           |
|---------------|:-----------------------:|---------------------------------------------------------------------------------------------------|
| `IEquatable`  |        `__eq__`         | for determining equality of instances with method `Equals`                                        |
| `IEnumerator` |       `__next__`        |                                                                                                   |
| `IEnumerable` |       `__iter__`        |                                                                                                   |
| `IComparable` |   `__lt__`+ `__eq__`    | Method `CompareTo` returns 0, 1 or -1 and is implemented for types that can be ordered or sorted. |
| `IDisposable` | `__exit__` + `__exit__` | Every IDisposable will (and should) also implement a resource manager.                            |
| `ToString`    |        `__str__`        | Calls to `x.ToString` will be translated to `str(x)`.                                             |

## Arrow Functions

Python do not support multi-line lambdas. Currently we transform any arrow function into a separate function that is
lifted up into the nearest statement block.

## Object Expressions

Currently translated to classes since they can be used to implement an interface and have methods etc. They are more like
anonymous classes.

## Sequence Expressions

Translated to nested functions. Python has some support for named expressions (`:=`) but only for naming new
expressions. You cannot assign to e.g. an object property.

## Type Annotations

The generated Python code is now with type annotation. Python types and F# types are not fully compatible (Ref:
https://github.com/microsoft/pyright/issues/1264)

E.g:

```fs
let length(xs: 'TSource list) =
    42
```

Would translate to:

```py
def length(source: List[TSource]) -> int:
    return 42
```

However, this gives errors in [Pyright](https://github.com/microsoft/pyright) type checker (used by Pylance):

TypeVar "TSource" appears only once in generic function signature Pylance(reportInvalidTypeVarUse)
(type variable) TSource

Thus we currently check that the generic parameters are used more than once. If not they will be translated as `Any`.

```py
def length(source: List[Any]) -> int:
    return 42
```

## Program vs Library

Fable projects compiling to Python should set `OutputType` to `Exe` for projects having the main `EntryPoint`.

```xml
<OutputType>Exe</OutputType>
```

Such projects will then be compiled with absolute imports. Python programs are not allowed to do relative imports. If
the project is compiled as a `Library` (default) it will use relative imports. This is important since the library
modules do not know the path where they are mounted by the application using them.
