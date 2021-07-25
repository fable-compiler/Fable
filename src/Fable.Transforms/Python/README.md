# Fable to Python

This experimental support for Python in Fable. Transforming the Fable AST into a Python AST and printing to Python source code.

## Current Design

- F# List - is implemented by `List.fs`.
- Map, is implemented by `Map.fs`
- Array, backed by and translated to Python list. Python has array support for numeric types so this is a consideration
  we may do later.
- Option, similar to JS, options will be erased in runtime. `None` will be translated to `None`.
- Decimal, is translated to Python decimal.

## Numerics

Currently, every number (int, int32, int64, bigint) will be compiled to Python int. Python ints are variable in length.

## Interfaces and Protocols

- `IEquatable`, with method `Equals` matches with Python `__eq__`
- `IEnumerator`, with methods `MoveNext`, `Reset` and `Current` matches Python `__iter__` protocol.
- `IComparable`, with method `CompareTo` has no natural match in Python. The method returns 0, 1 or -1 and is
  implemented for types that can be ordered or sorted. This matches the protocols `__lt__` and `__eq__`

The `ToString`, will be translated to Python `__str__` method. A call to `ToString` will be translated to `str`.

## Arrow functions

Python do not support multi-line lambdas. Currently we transform any
arrow function into a separate function that is lifted out into the
nearest statement block.