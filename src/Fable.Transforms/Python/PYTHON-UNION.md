# Python Union Type Implementation for Fable

This document describes the implemented discriminated union design for Fable's Python output.

## Background

F# discriminated unions are a powerful type system feature that allows defining types with multiple named cases, each potentially carrying different data. When compiling F# to Python, we need to represent these unions in a way that:

1. **Preserves type safety** - Python type checkers (pyright, mypy) should understand the union structure
2. **Enables pattern matching** - Python 3.10+ `match` statements should work naturally with case extraction
3. **Provides good IDE support** - Autocomplete, go-to-definition, and hover documentation should work
4. **Maintains backwards compatibility** - Existing Fable Python code using `.tag` and `.fields` should continue to work
5. **Follows Python conventions** - The generated code should look and feel like idiomatic Python

The original Fable implementation used a single class with factory functions, which worked but provided poor type checker and IDE support. This new design uses separate dataclasses for each case, connected by a type alias, giving us the best of both worlds: full type safety and Pythonic ergonomics.

## F# Source

```fsharp
type MyUnion =
    | CaseA of int
    | CaseB of string
    | CaseC of x: float * y: float
```

## Generated Python Output

```python
from fable_library.union import Union, tagged_union

# Base class with underscore prefix (private/internal)
class _MyUnion(Union):
    """Base class inheriting from Union for compatibility."""

    @staticmethod
    def cases() -> list[str]:
        return ["CaseA", "CaseB", "CaseC"]


@tagged_union(0)
class MyUnion_CaseA(_MyUnion):
    item: int


@tagged_union(1)
class MyUnion_CaseB(_MyUnion):
    item: str


@tagged_union(2)
class MyUnion_CaseC(_MyUnion):
    x: float
    y: float


# Type alias - THE public union type for annotations
type MyUnion = (MyUnion_CaseA | MyUnion_CaseB) | MyUnion_CaseC
```

## Key Design Decisions

### Naming Convention: Base Class vs Type Alias

The naming convention uses underscore prefix for the base class (private) and clean name for the type alias (public API):

|   Element    |      Name       |                           Purpose                            |
| ------------ | --------------- | ------------------------------------------------------------ |
| Base class   | `_MyUnion`      | Internal, has `cases()` method, used for `isinstance` checks |
| Case classes | `MyUnion_CaseA` | Prefixed with scoped union name to avoid collisions          |
| Type alias   | `MyUnion`       | Public API, used for type annotations                        |

This design follows Python conventions where leading underscore indicates private/internal usage.

### Why This Naming?

1. **Base class is private (`_MyUnion`)**: Users don't instantiate the base class directly - they use case constructors
2. **Type alias is public (`MyUnion`)**: This is what users see in type annotations and IDE hints
3. **Case classes use full scoped prefix (`MyUnion_CaseA`)**: Prevents collisions when multiple unions have same case names, includes module scope

### Case Class Naming: `{ScopedUnionName}_{CaseName}`

Case classes are prefixed with the **scoped union name** (including module prefix) to prevent naming collisions. This ensures consistency between the base class and case classes:

```fsharp
// F# - unions in different modules
module ModuleA =
    type Result = | Ok of int | Error of string

module ModuleB =
    type Result = | Ok of string | Error of int
```

```python
# Python - module-scoped names prevent collision
class _ModuleA_Result(Union): ...

@tagged_union(0)
class ModuleA_Result_Ok(_ModuleA_Result):
    item: int

@tagged_union(1)
class ModuleA_Result_Error(_ModuleA_Result):
    item: str

class _ModuleB_Result(Union): ...

@tagged_union(0)
class ModuleB_Result_Ok(_ModuleB_Result):
    item: str

@tagged_union(1)
class ModuleB_Result_Error(_ModuleB_Result):
    item: int
```

The scoped name is derived from `FSharp2Fable.Helpers.getEntityDeclarationName`, which includes the module path to ensure unique names across the entire compilation.

### Library Types Keep Simple Names

F# core library types (`Result`, `FSharpChoice`) use simple case names without prefix for cleaner interop:

```python
# Result type - base class is private
class _FSharpResult_2[T, TERROR](Union):
    @staticmethod
    def cases() -> list[str]:
        return ["Ok", "Error"]

@tagged_union(0)
class Ok[T, TERROR](_FSharpResult_2[T, TERROR]):
    result_value: T

@tagged_union(1)
class Error[T, TERROR](_FSharpResult_2[T, TERROR]):
    error_value: TERROR

# Type alias is the public API
type FSharpResult_2[T, TERROR] = Ok[T, TERROR] | Error[T, TERROR]
```

### Type Annotation Context

Type annotations use different names depending on context:

|             Context              |        Name Used        |         Reason         |
| -------------------------------- | ----------------------- | ---------------------- |
| Function parameters/returns      | `MyUnion` (type alias)  | Public API             |
| `self` inside base class methods | `_MyUnion` (base class) | Actual type of `self`  |
| Case class field types           | `MyUnion` (type alias)  | Public API             |
| Reflection `construct` parameter | `_MyUnion` (base class) | Needs `cases()` method |

### The `@tagged_union` Decorator

Located in [src/fable-library-py/fable_library/union.py](src/fable-library-py/fable_library/union.py):

```python
from dataclasses import dataclass, fields as dataclass_fields
from typing import Any, dataclass_transform

from .array_ import Array

@dataclass_transform()
def tagged_union(tag: int):
    """Decorator for union case classes.

    Uses @dataclass_transform() so type checkers understand:
    - Field annotations become constructor parameters
    - __match_args__ is generated
    - __eq__, __repr__, __hash__ are generated

    Additionally sets:
    - cls.tag = tag (numeric case discriminator)
    - cls.fields property (Array of field values for backwards compat)
    """
    def decorator[T](cls: type[T]) -> type[T]:
        # Apply dataclass internally
        dc_cls: Any = dataclass(cls)

        # Set the tag
        dc_cls.tag = tag

        # Generate fields property from dataclass fields
        field_names = [f.name for f in dataclass_fields(dc_cls)]

        @property
        def fields(self) -> Array[Any]:
            return Array[Any]([getattr(self, name) for name in field_names])

        dc_cls.fields = fields
        return dc_cls

    return decorator
```

### Reflection Support

The `CaseInfo` class in [reflection.py](src/fable-library-py/fable_library/reflection.py) includes a `case_constructor` field for dynamic union construction:

```python
@dataclass
class CaseInfo:
    declaringType: TypeInfo
    tag: int
    name: str
    fields: list[FieldInfo]
    case_constructor: type[Any] | None = None
```

The `union_type` function accepts the **base class** (with underscore prefix) as the `construct` parameter:

```python
def union_type(
    fullname: str,
    generics: Array[TypeInfo],
    construct: type[Union],  # Must be the base class, e.g., _MyUnion
    cases: Callable[[], list[list[FieldInfo]]],
    case_constructors: list[type[Any]] | None = None,
) -> TypeInfo:
```

And `make_union` uses them:

```python
def make_union(uci: CaseInfo, values: Array[Any]) -> Any:
    if uci.case_constructor is not None:
        return uci.case_constructor(*values)
    return uci.declaringType.construct(uci.tag, *values)
```

## Usage Examples

```python
# Construction - use case classes directly
u = MyUnion_CaseA(42)
u = MyUnion_CaseC(1.0, 2.0)

# Field access - direct attributes with F# names
print(u.item)      # For CaseA/CaseB
print(u.x, u.y)    # For CaseC

# Pattern matching - __match_args__ automatic from dataclass
match u:
    case MyUnion_CaseA(item=value):
        print(f"CaseA: {value}")
    case MyUnion_CaseC(x=x, y=y):
        print(f"CaseC: {x}, {y}")

# isinstance works with base class (underscore-prefixed)
isinstance(u, _MyUnion)  # True for all cases
isinstance(u, Union)     # True - inherits from Union
isinstance(u, MyUnion_CaseA)  # True for CaseA only

# Tag still available for compatibility
print(u.tag)       # 0, 1, or 2
print(u.fields)    # Array([42]) or Array([1.0, 2.0])

# cases() method on base class
print(_MyUnion.cases())  # ["CaseA", "CaseB", "CaseC"]
```

## Type Annotations

Use the type alias (clean name) for public API annotations:

```python
def process(value: MyUnion) -> str:
    match value:
        case MyUnion_CaseA(item=i): return f"int: {i}"
        case MyUnion_CaseB(item=s): return f"str: {s}"
        case MyUnion_CaseC(x=x, y=y): return f"point: ({x}, {y})"

def create() -> MyUnion:
    return MyUnion_CaseA(42)
```

Inside base class methods, use the base class name:

```python
class _MyUnion(Union):
    def GetHashCode(self) -> int:
        x: _MyUnion = self  # Use base class for self
        return safe_hash(x)
```

## What This Provides

- `isinstance(u, _MyUnion)` works for all cases (base class)
- `isinstance(u, Union)` works (inherits from Union)
- `tag` field for numeric case discrimination (backwards compatible)
- `fields` property returning `Array[Any]` for indexed access (backwards compatible)
- `cases()` method on base class (backwards compatible)
- `__match_args__` automatic from `@dataclass` (via decorator)
- `__eq__`, `__repr__`, `__hash__` from `@dataclass` (via decorator)
- Type alias `MyUnion` for precise type annotations (public API)
- `@dataclass_transform()` makes type checkers understand the class structure
- Prefixed case names prevent naming collisions between unions
- Library types use simple names for cleaner interop
- Private base class follows Python underscore convention

## Compiler Implementation Details

### Context Tracking

The compiler tracks `EnclosingUnionBaseClass` in the context to determine when we're inside a base class definition:

```fsharp
type Context = {
    // ... other fields
    /// When inside a union base class definition, this holds the entity name.
    /// Used to determine whether to use base class name or type alias for annotations.
    EnclosingUnionBaseClass: string option
}
```

### Type Annotation Logic

In `Fable2Python.Annotation.fs`, the `makeEntityTypeAnnotation` function:

1. If inside the same union base class (`ctx.EnclosingUnionBaseClass = Some name`): use base class name with underscore
2. Otherwise: use type alias (strip underscore prefix)

### Reflection Constructor

In `Replacements.fs`, `tryConstructor` adds underscore prefix for union types so reflection gets the base class:

```fsharp
| Some(IdentExpr ident) when ent.IsFSharpUnion ->
    Some(IdentExpr { ident with Name = "_" + ident.Name })
```

## Files Modified

1. [src/fable-library-py/fable_library/union.py](src/fable-library-py/fable_library/union.py) - `@tagged_union` decorator
2. [src/fable-library-py/fable_library/reflection.py](src/fable-library-py/fable_library/reflection.py) - `case_constructor` in `CaseInfo`, `union_type` and `make_union`
3. [src/Fable.Transforms/Python/Fable2Python.Types.fs](src/Fable.Transforms/Python/Fable2Python.Types.fs) - Added `EnclosingUnionBaseClass` to `Context`
4. [src/Fable.Transforms/Python/Fable2Python.Transforms.fs](src/Fable.Transforms/Python/Fable2Python.Transforms.fs) - `transformUnion`, context tracking
5. [src/Fable.Transforms/Python/Fable2Python.Annotation.fs](src/Fable.Transforms/Python/Fable2Python.Annotation.fs) - Type annotation logic for union types
6. [src/Fable.Transforms/Python/Fable2Python.Reflection.fs](src/Fable.Transforms/Python/Fable2Python.Reflection.fs) - Case constructor generation
7. [src/Fable.Transforms/Python/Replacements.fs](src/Fable.Transforms/Python/Replacements.fs) - `tryConstructor` adds underscore for unions

## Comparison with Original Fable Design

The original Fable Python union implementation used a single class with a factory method pattern:

### Original Design

```python
class MyUnion(Union):
    def __init__(self, tag: int, *fields: Any) -> None:
        super().__init__()
        self.tag = tag
        self.fields = fields

    @staticmethod
    def cases() -> list[str]:
        return ["CaseA", "CaseB", "CaseC"]

# Construction via factory methods
def MyUnion_CaseA(item: int) -> MyUnion:
    return MyUnion(0, item)

def MyUnion_CaseB(item: str) -> MyUnion:
    return MyUnion(1, item)

def MyUnion_CaseC(x: float, y: float) -> MyUnion:
    return MyUnion(2, x, y)
```

### Problems with Original Design

|            Issue             |                                      Description                                       |
| ---------------------------- | -------------------------------------------------------------------------------------- |
| **No type discrimination**   | All cases have the same type `MyUnion`, so `isinstance(x, MyUnion_CaseA)` doesn't work |
| **Field access via index**   | Must use `u.fields[0]` instead of `u.item` or `u.x`                                    |
| **Poor pattern matching**    | Python's `match` statement can't distinguish cases by type                             |
| **No IDE support**           | Type checkers can't infer field types or provide autocomplete                          |
| **No `@dataclass` benefits** | No automatic `__eq__`, `__repr__`, `__hash__`, `__match_args__`                        |

### New Design Benefits

|      Aspect      |             Original              |                   New                    |
| ---------------- | --------------------------------- | ---------------------------------------- |
| Case types       | Single class, all cases same type | Separate class per case                  |
| Type checking    | `isinstance(u, MyUnion)` only     | `isinstance(u, MyUnion_CaseA)` works     |
| Field access     | `u.fields[0]`, `u.fields[1]`      | `u.item`, `u.x`, `u.y`                   |
| Pattern matching | Match on `.tag` only              | Match on case type with field extraction |
| IDE support      | Minimal                           | Full autocomplete and type inference     |
| Type annotations | `MyUnion` (opaque)                | `MyUnion` (union of case types)          |

### Pattern Matching Comparison

```python
# Original - must match on tag
match u.tag:
    case 0: print(f"CaseA: {u.fields[0]}")
    case 1: print(f"CaseB: {u.fields[0]}")
    case 2: print(f"CaseC: {u.fields[0]}, {u.fields[1]}")

# New - match on type with field extraction
match u:
    case MyUnion_CaseA(item=i): print(f"CaseA: {i}")
    case MyUnion_CaseB(item=s): print(f"CaseB: {s}")
    case MyUnion_CaseC(x=x, y=y): print(f"CaseC: {x}, {y}")
```

### Backwards Compatibility

The new design maintains backwards compatibility:

- `u.tag` still works (set by `@tagged_union` decorator)
- `u.fields` still works (generated property returning `Array[Any]`)
- `MyUnion.cases()` still works (on base class `_MyUnion`)
- `isinstance(u, Union)` still works (case classes inherit from `Union` via base class)

### Type Annotation Improvement

```python
# Original - return type is opaque
def process(value: MyUnion) -> str: ...  # No way to know what cases exist

# New - type alias shows all cases
type MyUnion = MyUnion_CaseA | MyUnion_CaseB | MyUnion_CaseC
def process(value: MyUnion) -> str: ...  # IDE shows the union of cases
```

## Verification

```bash
# Generate output
./build.sh quicktest python

# Check generated code
cat src/quicktest-py/quicktest.py

# Run tests
./build.sh test python
```
