# Python Type Annotations in Fable

This document explains how Fable generates type-safe Python code, the challenges involved, and the solutions implemented.

## Overview

Fable compiles F# to Python with full type annotations for Pyright/Pylance compatibility. This is challenging because:

1. **F# uses curried functions** (`int -> int -> int`) while Python uses multi-argument callables (`Callable[[int, int], int]`)
2. **F# `Option<T>` has different representations** depending on nesting and whether `T` is concrete or generic
3. **F# interfaces compile to Python Protocols** with different method signatures
4. **Python ABC classes** require specific dunder methods for collection interop

## Key Files

|                           File                           |                Purpose                 |
| -------------------------------------------------------- | -------------------------------------- |
| [Fable2Python.Annotation.fs](Fable2Python.Annotation.fs) | Type annotation generation             |
| [Fable2Python.Transforms.fs](Fable2Python.Transforms.fs) | AST transformations                    |
| [Fable2Python.Util.fs](Fable2Python.Util.fs)             | Helper functions and pattern detection |
| `fable_library/option.py`                                | `erase()` and `widen()` functions      |
| `fable_library/curry.py`                                 | Curry/uncurry runtime functions        |

## Numeric Type Mappings

F# numeric types map to Python types with important distinctions:

|  F# Type   | Python Type |                              Notes                               |
| ---------- | ----------- | ---------------------------------------------------------------- |
| `int`      | `int32`     | F#'s default integer is 32-bit, not Python's arbitrary-precision |
| `int32`    | `int32`     | Explicit 32-bit integer                                          |
| `int64`    | `int64`     | 64-bit integer                                                   |
| `nativeint`| `int`       | Maps to Python's native arbitrary-precision integer              |
| `float`    | `float64`   | F#'s default float is 64-bit                                     |
| `float32`  | `float32`   | 32-bit float                                                     |
| `decimal`  | `Decimal`   | Python's `decimal.Decimal`                                       |

**Key implications:**

1. **`int` vs `int32`**: Python's built-in `int` has arbitrary precision, but F#'s `int` is 32-bit. Fable uses `int32` (a wrapper class) to preserve F# semantics. This affects `len()` which returns Python `int`, but F# code expects `int32`.

2. **Array length**: `Array.length` returns `int32`, not Python `int`. The compiler wraps `len()` calls: `int32(len(array))`.

3. **Interop boundaries**: When calling Python libraries that return `int`, you may need explicit conversion to `int32` for F# code.

## Option Type Handling

### The Erasure Strategy

Fable erases `Option<T>` to `T | None` when safe, but must wrap in `SomeWrapper` when the option could be nested or the inner type is unknown:

|       Context       |        F# Type        |      Python Type      |       Representation       |
| ------------------- | --------------------- | --------------------- | -------------------------- |
| Non-nested concrete | `Option<int>`         | `int \| None`         | Bare value or `None`       |
| Nested option       | `Option<Option<int>>` | `Option[Option[int]]` | `SomeWrapper` wrapping     |
| Generic parameter   | `Option<'T>`          | `Option[T]`           | `SomeWrapper[T]` or `None` |
| Option-like inner   | `Option<Result<_,_>>` | `Option[Result[...]]` | `SomeWrapper` wrapping     |

**Why wrap?**

1. **Nested options**: `Some(None)` vs `None` - without wrapping, both would be `None` in Python
2. **Generic types**: When `'T` could itself be `Option<_>`, we can't safely erase
3. **Option-like types**: `Result`, `ValueOption`, etc. have similar ambiguity

The `mustWrapOption` function in [Fable2Python.Util.fs](Fable2Python.Util.fs) determines when wrapping is required by checking for nested options, generic parameters, and option-like declared types.

### The Boundary Problem

When callbacks cross boundaries between generic library code and concrete caller code, types mismatch. A library function with signature `change: K -> (Option[V] -> Option[V]) -> Dict[K,V] -> Dict[K,V]` expects a wrapped Option callback, but callers with concrete types generate callbacks with erased signatures like `int | None -> int | None`.

### Solution: `erase()` and `widen()`

Two zero-cost type adapter functions (identity at runtime):

| Function |     Signature            |      Purpose                                   |
| -------- | ------------------------ | -----------------------------------------------|
| `erase`  | `Option[T] -> T \| None` | When crossing from generic to concrete context |
| `widen`  | `T \| None -> Option[T]` | When crossing from concrete to generic context |

The compiler automatically inserts `widen()` when passing callbacks to generic functions that expect wrapped Option types. This is handled by `needsOptionWidenForArg` in [Fable2Python.Util.fs](Fable2Python.Util.fs) and the argument transformation in [Fable2Python.Transforms.fs](Fable2Python.Transforms.fs).

### When to Skip Type Annotations

Some contexts require omitting annotations to let Python infer correct types:

1. **Invariant containers with Options**: When a call returns `Array[tuple[K, Option[V]]]`, annotating the variable would cause a type mismatch due to array invariance
2. **Wrapped option narrowing**: After `if x is not None:`, assigning `x` to a new variable should let the narrowed type be inferred

The helpers `valueExtractsFromInvariantContainer` and `isWrappedOptionNarrowingAssignment` in [Fable2Python.Util.fs](Fable2Python.Util.fs) detect these cases.

## Currying and Uncurrying

### The Problem

F# functions are curried by default (`let add x y = x + y` has type `int -> int -> int`), but Python doesn't have native currying - functions take all arguments at once.

### Runtime Curry/Uncurry

The `fable_library/curry.py` module provides `curry` and `uncurry` functions that convert between:

- **Curried**: Nested lambdas `x -> (y -> x + y)`
- **Uncurried**: Multi-argument function `(x, y) -> x + y`

These use memoization via `WeakKeyDictionary` to enable roundtrip conversions without data loss.

### Type Annotations for Curried Functions

**Challenge:** Deeply nested `Callable[[A], Callable[[B], Callable[[C], D]]]` types confuse type checkers and are impractical to verify through curry/uncurry transformations.

**Solution:** Smart simplification based on nesting depth:

| Nesting Depth |      F# Type       |         Python Annotation         |
| ------------- | ------------------ | --------------------------------- |
| Depth 1       | `A -> B`           | `Callable[[A], B]`                |
| Depth 2       | `A -> B -> C`      | `Callable[..., Callable[..., C]]` |
| Depth 3+      | `A -> B -> C -> D` | `Callable[..., Any]`              |

The key insight is to preserve concrete return types where possible (so Pyright can still catch real bugs like `Option[T]` vs `T | None` mismatches) while simplifying argument types that are impractical to track.

This is implemented in `makeLambdaTypeAnnotation` in [Fable2Python.Annotation.fs](Fable2Python.Annotation.fs).

### Field Type Uncurrying

Class/record fields store **uncurried** functions at runtime, so annotations must match. A field declared as `fn: unit -> string -> int` in F# generates `fn_: Callable[[None, str], int]` (uncurried), not `Callable[..., Callable[..., int]]` (curried).

This follows the same pattern as the TypeScript backend and is handled in `declareDataClassType` by applying `FableTransforms.uncurryType` to lambda-typed fields.

## Python Match Statements

Fable generates Python 3.10+ `match` statements for pattern matching when the decision tree structure allows it.

### Supported Patterns

- **Literal patterns**: Integer, string, char matching → `case 1:`, `case "hello":`
- **Or-patterns**: `| 1 | 2 | 3 ->` → `case 1 | 2 | 3:`
- **Union case matching**: Matching on `.tag` property
- **Guard expressions**: `when` clauses → `case n if n > 0:`
- **Tuple patterns with guards**: `| true, _, i when i > -1 ->` → `case [True, _, i] if i > -1:`

### Guards and Walrus Operators

For Option patterns in guards, Fable uses walrus operators for proper type narrowing. When matching `Some x :: t`, the generated code uses `(x := head(l)) is not None` which both assigns and tests, allowing Pyright to narrow `x` from `T | None` to `T`.

This is implemented in `transformDecisionTreeAsMatchWithGuards` in [Fable2Python.Transforms.fs](Fable2Python.Transforms.fs).

### Fallback

When match statement conversion isn't possible (complex nested patterns, certain decision tree structures), Fable falls back to `if/elif/else` chains.

## ABC Base Classes for Collections

F# collection types implement Python ABC protocols for native Python interop (`in`, `len()`, `[]`, iteration).

### The Hybrid Approach

Following the TypeScript pattern where F# explicitly implements `JS.Map` and the compiler generates `Symbol.iterator`:

1. **F# interface implementation** → methods become attached class methods (e.g., `get_Item`, `ContainsKey`, `Count`)
2. **Compiler generates dunders** → `__getitem__`, `__len__`, `__contains__`, `__iter__` that call the attached methods
3. **Direct ABC inheritance** → Classes inherit from `Mapping`, `Set`, etc. directly

### Supported ABCs

|           F# Interface            |      Python ABC       |                  Dunders Generated                   |
| --------------------------------- | --------------------- | ---------------------------------------------------- |
| `Py.Mapping.IMapping<K,V>`        | `Mapping[K,V]`        | `__getitem__`, `__contains__`, `__len__`, `__iter__` |
| `Py.Mapping.IMutableMapping<K,V>` | `MutableMapping[K,V]` | + `__setitem__`, `__delitem__`                       |
| `Py.Set.ISet<T>`                  | `Set[T]`              | `__contains__`, `__len__`, `__iter__`                |
| `Py.Set.IMutableSet<T>`           | `MutableSet[T]`       | + `add`, `discard`                                   |

### `__iter__` Special Handling

- **For Mapping**: The enumerator yields `(key, value)` tuples, so `__iter__` extracts keys: `yield kv[0]`
- **For Set**: Yield items directly from the enumerator

The `to_iterator` helper in `fable_library/util.py` wraps `IEnumerator` with proper `Dispose` handling via `try/finally`.

### Dictionary vs dict vs Mapping

There's an inherent tension between F#'s `Dictionary<K,V>`, .NET's `IDictionary<K,V>`, Python's built-in `dict`, and the `collections.abc.Mapping` protocol.

**The type hierarchy:**

|   F# / .NET Type   |       Python Type       |              Relationship               |
| ------------------ | ----------------------- | --------------------------------------- |
| `Dictionary<K,V>`  | `Dictionary[K,V]` class | Fable's mutable dictionary              |
| `IDictionary<K,V>` | `IDictionary` protocol  | .NET interface for mutable dictionaries |
| `dict<K,V>`        | `dict[K,V]`             | Python's built-in dictionary            |
| -                  | `Mapping[K,V]`          | Python ABC for read-only dict-like      |
| -                  | `MutableMapping[K,V]`   | Python ABC for mutable dict-like        |

**Challenges:**

1. **`Dictionary` is not `dict`**: Fable's `Dictionary` class is a separate type from Python's `dict`. Helper functions accepting `dict[K,V]` won't accept `Dictionary[K,V]` without using `Mapping[K,V]` as the parameter type.

2. **Dunder method signatures**: F# interface methods compile with different signatures than Python expects:
   - `__len__` gets an extra `__unit=UNIT` parameter and returns `int32` (should take no params, return `int`)
   - `__getitem__` and `__contains__` get default parameters `= UNIT` (should have no defaults)
   - `__iter__` returns `IEnumerator[Any]` (should return `Iterator[KEY]`)

3. **Invariance issues**: `dict` is invariant in its type parameters, so `dict[str, int]` is not compatible with `dict[str, object]`. This affects functions that want to accept "any dictionary".

**Current approach:**

- Helper functions like `get_item_from_dict` use `Mapping[K,V]` parameter type to accept both `dict` and `Dictionary`
- The ABC base class approach (see above) provides Python-compatible dunders that delegate to F# methods
- Some signature mismatches are accepted as limitations of the F#→Python compilation model

## Protocol Generation

F# interfaces compile to Python Protocols with special handling for type checker compatibility.

### Positional-Only Parameters

Protocol parameters are positional-only (using `/`) to avoid name mismatch errors. This allows implementations to use different parameter names (e.g., `value_1` instead of `value` due to closure capture renaming) without violating the protocol.

### Mangled Interface Names

When interfaces have `[<Mangle>]` attribute, method names include the full entity name and an overload hash suffix (e.g., `Fable_Tests_TypeTests_BarInterface_DoSomething205A44C0`). Protocols must use these mangled names to match implementations.

### Uncurried Method Signatures

Protocol methods use uncurried types to match runtime behavior. A method parameter `f: A -> B -> C` becomes `f: Callable[[A, B], C]`, not `Callable[[A], Callable[[B], C]]`.

## Common Patterns and Solutions

|          Pattern          |                               Problem                                |                      Solution                      |
| ------------------------- | -------------------------------------------------------------------- | -------------------------------------------------- |
| Generic→Concrete Option   | Generic function returns `Option[T]`, caller expects `T \| None`     | Insert `erase()` call                              |
| Concrete→Generic Callback | Callback with `T \| None` passed to function expecting `Option[T]`   | Insert `widen()` call                              |
| Two-Switch Decision Tree  | Pattern matching without default case                                | Make last case default (`else` instead of `elif`)  |
| Invariant Container       | Annotating `Array[Option[T]]` causes type error                      | Skip annotation, let type be inferred              |

## Testing and Validation

### Running Pyright

```bash
# Test rebuilding everything (fable-library + fable.library.core + tests)
./build.sh test python
# Test rebuilding, but skipping fable-library rebuild
./build.sh test python --skip-fable-library
# Test rebuilding tests and fable-library but skipping fable-library-core rebuild
./build.sh test python --skip-fable-library-core
```

```bash
# Check fable-library
uv run pyright temp/fable-library-py

# Check generated test code
uv run pyright temp/tests/Python
```

### Pyright Baseline

The project maintains a pyright error baseline. New code should not introduce regressions. The `pyrightconfig.ci.json`
file is used in CI to enforce this, and will exclude files with known errors.

## References

- [PYTHON-TYPING.md](../../../PYTHON-TYPING.md) - Detailed session-by-session progress
- [PYTHON-ABC.md](../../../PYTHON-ABC.md) - ABC base class implementation details
- [PYTHON-MATCH.md](../../../PYTHON-MATCH.md) - Match statement generation
- [PYTHON-OPTIONS.md](../../../PYTHON-OPTIONS.md) - Option type handling details
