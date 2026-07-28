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

`FSharpArray` stores elements unboxed and hands them back in their Python representation, so an
`int[]` yields plain `int`s. The element type is spelled with the Python representation:
`Array[int]` and `Array[float]`, with `Array[uint8]` and friends for the wrapped widths.

## Numerics

F#'s two default numeric types are represented natively: `int` is a plain Python `int` and `float` is a
plain Python `float`. The other widths use custom pyo3 wrapper types, which is what keeps a bare `int`
or `float` an unambiguous runtime tag for the default width.

| F#               | .NET       | Python  | Implementation                  |
|:-----------------|:-----------|---------|---------------------------------|
| bool             | Boolean    | bool    | Native Python type              |
| int              | Int32      | int     | Native Python type              |
| float / double   | Double     | float   | Native Python type              |
| bigint           | BigInteger | int     | Native Python type              |
| byte             | Byte       | UInt8   | Custom pyo3 wrapper (ints.rs)   |
| sbyte            | SByte      | Int8    | Custom pyo3 wrapper (ints.rs)   |
| int16            | Int16      | Int16   | Custom pyo3 wrapper (ints.rs)   |
| uint16           | Uint16     | UInt16  | Custom pyo3 wrapper (ints.rs)   |
| uint32           | Uint32     | UInt32  | Custom pyo3 wrapper (ints.rs)   |
| int64            | Int64      | Int64   | Custom pyo3 wrapper (ints.rs)   |
| uint64           | Uint64     | UInt64  | Custom pyo3 wrapper (ints.rs)   |
| float32 / single | Single     | Float32 | Custom pyo3 wrapper (floats.rs) |

### Int32 normalization

A Python `int` is arbitrary precision, so the compiler normalizes results that can leave the 32-bit
range, emitting `int32(...)` from `fable_library.core`. Two properties keep this cheap:

- Normalization commutes with `+ - * << & | ^`, so **one normalization per expression tree** is
  equivalent to one per operation. `int32(a * b + c)` and `int32(int32(a * b) + c)` always agree.
- Only `+ - * <<` and unary `-` can leave the range at all. `& | ^ ~ >> / %`, comparisons, indexing
  and literals map in-range operands to in-range results and are emitted bare.

`transformOperation` implements this as a strip-then-wrap peephole keyed on the *Fable* operand node,
never on the emitted Python — so it cannot strip a meaningful `int32(someFloat)` truncation, which
does not commute with arithmetic. A four-operation tree emits a single wrap:

```py
def nested(a: int, b: int, c: int) -> int:
    return int32(((a * b) + (c * 2)) - 1)
```

Float64 needs none of this: a Python `float` *is* an IEEE double, so arithmetic, overflow-to-infinity,
NaN comparison, banker's rounding and signed zero already match .NET bit for bit.

### Operators that diverge from Python

| Expression | Python | .NET | Emitted |
| --- | --- | --- | --- |
| `-7 / 2` | `-4` (floors) | `-3` (truncates) | `int(a / b)` |
| `-5 % 3` | `1` (sign of divisor) | `-2` (sign of dividend) | `op_remainder_int32(a, b)` |
| `1 <<< 32` | `4294967296` | `1` (count masked) | `a << (b & 31)` |
| `1.0 / 0.0` | `ZeroDivisionError` | `infinity` | `op_division_float64(a, b)` |
| `-5.0 % 3.0` | `1.0` | `-2.0` | `op_remainder_float64(a, b)` |
| `str(5.0)` | `'5.0'` | `'5'` | `exceptions.to_string` |
| `str(inf)` | `'inf'` | `'Infinity'` | `exceptions.to_string` |

Float division keeps a bare `/` when the divisor is a non-zero literal.

`exceptions.to_string` must not call `int()` on a non-finite double — that raises `OverflowError` for
the infinities and `ValueError` for NaN — so it checks `math.isfinite` before taking the whole-number
path. The printf machinery in `strings.rs` spells the same three values out.

**Known divergence:** `Int32.MinValue / -1` yields `2147483648` rather than throwing
`OverflowException`. `/` cannot leave the 32-bit range for any other operand pair, so it carries no
normalization, and adding one just for this case would tax every integer division.

### Conversions between widths

A widening conversion such as `int (x: sbyte)` or `float (x: float32)` reaches codegen as a bare
`TypeCast` — `Replacements.needToCast` is false, because on .NET the value always fits. On Python the
cast is not free even so: the source is a wrapper object, and leaving it in place would keep the
arithmetic at the *source* width, so `Int8(100) + 100` would wrap to -56 instead of widening to 200.
`transformCast` therefore emits `int32(...)` (which also truncates, matching an unchecked .NET cast)
or the builtin `float(...)` whenever the source is not already the target's representation. Casting
between two values that are both plain `int`, or both plain `float`, stays a no-op.

`Int32.Parse`/`TryParse` go through the `int32` *module* rather than the `Int32` class, because the
class's static methods return — and store into the ref cell — an `Int32` object.

### Set annotations

`Set<'T>` is annotated as `FSharpSet[T]`, where the other builtin entities still annotate as `Any`.
With `int` emitted as a bare literal, Pyright solves the element type of `singleton(1, cmp)` to
`Literal[1]`; the comparer argument cannot widen it, because `IComparer_1` is contravariant and so
`IComparer_1[int]` already satisfies `IComparer_1[Literal[1]]`. `FSharpSet` is invariant — its `T`
appears in both `Contains(value: T)` and `GetEnumerator() -> IEnumerator[T]` — so
`FSharpSet[Literal[1]]` will not unify with the `FSharpSet[Literal[2]]` of a sibling set. Annotating
the binding gives bidirectional inference something to push down into those calls, which resolves both
to `int`.

### Runtime type tests

`:? int` and `:? float` compile to exact `type(x) is int` / `type(x) is float` checks rather than
`isinstance`, because `bool` subclasses `int` — `isinstance` would report `box true :? int` as true.
Exact matching also keeps `float` distinct from the `Float32` wrapper, which is not a `float` subclass.

**Known limitation:** Python's `int` is already arbitrary precision, so a boxed `int` cannot be told
apart from a `bigint` at runtime. One assertion in `tests/Python/TestType.fs` is guarded with
`#if !FABLE_COMPILER_PYTHON`. JS avoids this only because it has a native `bigint` primitive.

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
