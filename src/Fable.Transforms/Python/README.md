# Fable Python

Adds beta  support for Python in Fable. Transforming the Fable AST into a Python AST, then printing to typed
Python source code.

## Current Design

| F#           |      Python      | Comment                                                                           |
|--------------|:----------------:|-----------------------------------------------------------------------------------|
| List (F#)    |     List.fs      | F# immutable list                                                                 |
| ResizeArray  |      `list`      | Python [list](https://docs.python.org/3/library/stdtypes.html#typesseq-list)      |
| Map          |      Map.fs      | F# immutable map                                                                  |
| Record       |     types.py     | dataclasses.dataclass decorated Record type                                       |
| Option       |      Erased      | F# `None` will be translated to Python `None`                                     |
| An. Record   |      `dict`      | Python [dict](https://docs.python.org/3/library/stdtypes.html#mapping-types-dict) |
| dict         |      `dict`      |                                                                                   |
| Dictionary   |      `dict`      | MutableMap if comparer                                                            |
| tuple        |     `tuple`      | Python [tuple](https://docs.python.org/3/library/stdtypes.html#tuples)            |
| Decimal      |    `decimal`     |                                                                                   |
| DateTime     |    `datetime`    |                                                                                   |
| string       |     `string`     |                                                                                   |
| char         |     `string`     |                                                                                   |
| `[]<byte>`   |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<sbyte>`  |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<int16>`  |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<uint16>` |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<int>`    |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<uint32>` |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<long>`   |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<uint64>` |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<float>`  |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<single>` |  `FSharpArray`   | Custom pyo3 wrapper (array.rs)                                                    |
| `[]<'T>`     |      `list`      | Python list module                                                                |

## Numerics

Most numeric types are now implemented using custom pyo3 wrapper types that maintain F#-style semantics while integrating with Python. Only `bigint` is still translated to Python's native `int` type. The wrapper types provide proper overflow behavior, type safety, and performance optimization while remaining compatible with Python code.

| F#               | .NET       | Python  | Implementation                  |
|:-----------------|:-----------|---------|---------------------------------|
| bool             | Boolean    | bool    | Native Python type              |
| int              | Int32      | Int32   | Custom pyo3 wrapper (ints.rs)   |
| byte             | Byte       | UInt8   | Custom pyo3 wrapper (ints.rs)   |
| sbyte            | SByte      | Int8    | Custom pyo3 wrapper (ints.rs)   |
| int16            | Int16      | Int16   | Custom pyo3 wrapper (ints.rs)   |
| int64            | Int64      | Int64   | Custom pyo3 wrapper (ints.rs)   |
| uint16           | Uint16     | UInt16  | Custom pyo3 wrapper (ints.rs)   |
| uint32           | Uint32     | UInt32  | Custom pyo3 wrapper (ints.rs)   |
| uint64           | Uint64     | UInt64  | Custom pyo3 wrapper (ints.rs)   |
| float / double   | Double     | Float64 | Custom pyo3 wrapper (floats.rs) |
| float32 / single | Single     | Float32 | Custom pyo3 wrapper (floats.rs) |
| bigint           | BigInteger | int     | Native Python type              |

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

Currently translated to classes since they can be used to implement an interface and have methods etc. They are more
like anonymous classes.

## Sequence Expressions

Translated to nested functions. Python has some support for named expressions (`:=`) but only for naming new
expressions. You cannot assign to e.g. an object property.

## Type Annotations

The generated Python code is now with type annotation. Python types and F# types are not fully compatible (Ref:
<https://github.com/microsoft/pyright/issues/1264>)

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
