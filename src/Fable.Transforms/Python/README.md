# Fable to Python

This experimental support for Python in Fable. Transforming the Fable AST into a Python AST, then printing to untyped
Python source code.

## Current Design

| F#       |   Python   | Comment                                           |
|----------|:----------:|---------------------------------------------------|
| List     |  List.fs   | F# immutable list                                 |
| Map      |   Map.fs   | F# immutable map                                  |
| Array    |   `list`   | TODO: Python has arrays for numeric types         |
| Option   |   Erased   | F# `None` will be translated to Python `None`     |
| dict     |    dict    |                                                   |
| Decimal  | `decimal`  | A call to `ToString` will be translated to `str`. |
| DateTime | `datetime` |                                                   |
| string   |  `string`  |                                                   |
| char     |  `string`  |                                                   |

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

| .NET          |       Python       | Comment                                                                                           |
|---------------|:------------------:|---------------------------------------------------------------------------------------------------|
| `IEquatable`  |      `__eq__`      | for determining equality of instances with method `Equals`                                        |
| `IEnumerator` |     `__next__`     |                                                                                                   |
| `IEnumerable` |     `__iter__`     |                                                                                                   |
| `IComparable` | `__lt__`+ `__eq__` | Method `CompareTo` returns 0, 1 or -1 and is implemented for types that can be ordered or sorted. |
| `ToString`    |     `__str__`      | Calls to `x.ToString` will be translated to `str(x)`.                                                 |

## Arrow Functions

Python do not support multi-line lambdas. Currently we transform any arrow function into a separate function that is
lifted up into the nearest statement block. TODO: translate single line arrow functions to Python lambda.

## Object Expressions

Currently translated to classes. TODO: should we use dicts here?

## Sequence Expressions

Translated to nested functions. Python has some support for named expressions (`:=`) but only for naming new
expressions. You cannot assign to e.g. an object property.

## Type Annotations

The generated Python code is currently without type annotation. Python types and F# types are not fully compatible (Ref:
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

But I expect that Python will eventually get things right, so we should start generating type annotations.