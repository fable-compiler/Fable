# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Fable is an F# to multi-language compiler that transpiles F# code to JavaScript, TypeScript, Python, Rust, Dart, PHP, and Erlang/BEAM. It uses a fork of F# Compiler Services (FCS) with custom tweaks, located in `lib/fcs`.

## Build Commands

The build system is implemented in F# at `src/Fable.Build/`. All commands go through `build.sh` (or `build.bat` on Windows):

```bash
./build.sh                                        # Show all available commands

# Build runtime libraries
./build.sh fable-library --javascript             # Also: --typescript, --python, --dart, --rust, --beam

# Run tests (targets: javascript, typescript, python, dart, rust, beam, integration)
./build.sh test javascript
./build.sh test javascript --watch                # Watch mode (recompile on changes)
./build.sh test javascript --force-fable-library  # Force fable-library rebuild (needed if you modified Fable compiler to fix the generation of `src/fable-library-*/**/*.fs` files)

# Python-specific options
./build.sh test python --skip-fable-library-core  # Skip slow Rust extension + .pyi rebuild (Python only)
./build.sh test python --type-check               # Run Pyright type checking on Python output

# Quick iteration (test on a minimal project)
./build.sh quicktest javascript                   # Also: typescript, python, dart, rust, beam
```

In most cases, you should not need to use `--force-fable-library`. Only use it if you modified compiler code that affects how F# files used in `src/fable-library-*/` are generated (e.g., changes to `FSharp2Fable.fs` that affect how F# AST is transformed into Fable AST for library files).

Build output goes to `temp/`: transpiled runtime libraries in `temp/fable-library-<target>/` and test output in `temp/tests/<target>/` (e.g., `temp/fable-library-beam/` and `temp/tests/beam/`).

Test runners by target: JavaScript/TypeScript use Mocha, Python uses pytest (with `uv`, not pip), Rust uses cargo test, Dart uses `dart test`.

## Architecture

### Compiler Pipeline

```text
F# Source → FCS Parser → F# AST → FSharp2Fable → Fable AST → FableTransforms → Fable2Target → Target AST → Printer → Output
```

1. **FSharp2Fable** (`src/Fable.Transforms/FSharp2Fable.fs`): F# AST → Fable intermediate AST
2. **FableTransforms** (`src/Fable.Transforms/FableTransforms.fs`): Optimizations on Fable AST (tail-call detection, monadic trampolines, etc.)
3. **Fable2Target**: Language-specific code generators, each in their own subdirectory:
   - `Fable2Babel.fs` → JavaScript/TypeScript (outputs Babel AST, printed by `BabelPrinter.fs`)
   - `Python/Fable2Python.*.fs` → Python (AST in `Python.AST.fs`, printed by `PythonPrinter.fs`)
   - `Rust/Fable2Rust.fs` → Rust
   - `Dart/Fable2Dart.fs` → Dart
   - `Beam/Fable2Beam.fs` → Erlang/BEAM

### Replacements System

`Replacements.fs` files map .NET BCL method calls to target language implementations. This is how `System.String.Contains()` becomes the appropriate call in each target. Each target has its own replacements file:

- `src/Fable.Transforms/Replacements.fs` — JavaScript/TypeScript (default)
- `src/Fable.Transforms/Python/Replacements.fs`
- `src/Fable.Transforms/Rust/Replacements.fs`
- `src/Fable.Transforms/Dart/Replacements.fs`
- `src/Fable.Transforms/Beam/Replacements.fs`

Shared replacement utilities are in `Replacements.Util.fs` and `Replacements.Api.fs`.

### Key Source Directories

- `src/Fable.AST/` — Core AST definitions (`Fable.fs` defines the Fable intermediate representation with types like `Expr`, `Type`, `MemberDecl`)
- `src/Fable.Core/` — Attributes and interop types used in F# source code targeting Fable (e.g., `[<Emit>]`, `[<Import>]`)
- `src/Fable.Transforms/` — All compilation stages, organized by target language in subdirectories
- `src/Fable.Compiler/` — Compiler library (project cracking, file watching)
- `src/Fable.Cli/` — CLI entry point; `Pipeline.fs` orchestrates file compilation and output
- `src/fable-library-{ts,py,dart,rust,beam}/` — Runtime libraries for each target. Parts are written in F# (e.g., `src/fable-library-ts/Seq.fs`) and transpiled to the target language during build. Other targets may share F# sources from `fable-library-ts/` via linked files in their `.fsproj` (e.g., `src/fable-library-beam/Fable.Library.Beam.fsproj` links `Seq.fs` from `fable-library-ts/`). The transpiled output goes to `temp/fable-library-<target>/`
- `src/quicktest*/` — Minimal projects for rapid development iteration

### Per-Target Structure

Each target language follows a consistent pattern within `src/Fable.Transforms/`:

- `Fable2<Target>.fs` — Main compilation from Fable AST to target AST
- `<Target>.AST.fs` — Target language AST definition
- `<Target>Printer.fs` — Target AST → source code string
- `Replacements.fs` — .NET BCL → target language mappings

## Development Guidelines

When investigating an issue or implementing a feature for a specific target, always look at how other targets handle the same problem. Targets should stay aligned in how they solve similar features — check the equivalent `Fable2<Target>.fs` and `Replacements.fs` files for other languages before implementing.

The main transform files (`Fable2Babel.fs`, `Fable2Beam.fs`, `Fable2Python.Transforms.fs`, etc.) should contain only Fable AST to target AST transformations. Helper functions and utilities belong in the corresponding `.Util.fs`, `.Reflection.fs`, or similar files.

Be careful when modifying shared files like `FableTransforms.fs`, `Transforms.Util.fs`, `FSharp2Fable.fs`, `Replacements.Api.fs`, or `Replacements.Util.fs` — these are used by all targets, and changes can introduce regressions for targets other than the one being fixed.

- **`src/Fable.AST/`** — Do not modify. Changes are breaking and require a new major version of Fable.
- **`src/Fable.Core/`** — Distributed on NuGet. Must not have breaking changes within a major version.

`src/fable-library-py/` is distributed on PyPI and must be backward-compatible within minor versions. Do not change the semantics of existing functions — instead, create a new function with a new name and update `Replacements.fs` to call it.

All code in `fable-library-py` must be fully type-annotated using Python 3.12+ syntax (PEP 695). Avoid `Any`, `cast()`, and `# type: ignore` — these require justification. Keep functions small and simple: extract helpers and use early returns. The Rust extension modules (`fable-library-core`) exist to provide correct .NET semantics for numerics, arrays, etc. — not for performance (the FFI crossing is expensive).

All transpiled Python code is type-checked with Pyright at standard settings (`./build.sh test python --type-check`). Do not introduce type-checking regressions without justification.

## Development Workflow

**Quicktest** is the fastest way to iterate (seconds vs minutes for full tests). Use it to investigate complex problems:

1. Edit `src/quicktest/QuickTest.fs` (or `src/quicktest-py/QuickTest.fs`, etc.)
2. Transpiled output goes to the quicktest directory itself (e.g., `src/quicktest-py/`)

Quicktest is also preferred when adding debug output (e.g., `printfn` in compiler code) since running full tests with debug prints produces too much output.

**Test suites** are in `tests/<target>/` (e.g., `tests/Python/`, `tests/Js/Main/`). Tests are first run on .NET, then transpiled to `temp/tests/<target>/` (e.g., `temp/tests/py/`, `temp/tests/js/`) and executed with the target's test runner. Full test runs take several minutes.

When adding a test, check if other targets already have a test for the same case (e.g., look in `tests/Js/Main/` before adding to `tests/Python/`) — reuse or adapt existing test patterns where possible.

**Never modify a test to make it pass on a target if it already passes on .NET.** If a test passes on .NET but fails on a target, the compiler or runtime library needs to be fixed — not the test.

## Changelogs

PRs must update changelogs. Always update both `src/Fable.Cli/CHANGELOG.md` and `src/Fable.Compiler/CHANGELOG.md` for changes to Fable.Transforms or fable-library. If the change touches `Fable.Core` (attributes, interop types), also update `src/Fable.Core/CHANGELOG.md`. Add entries under the `## Unreleased` section following the Keep a Changelog format with target prefix (e.g., `* [Python] Fix ...`, `* [Beam] Add ...`, `* [All] Fix ...`).

## Requirements

- .NET SDK 10+
- Node.js with npm
- Python 3.12+ with [uv](https://docs.astral.sh/uv/) (3.12+ required for PEP 695 type parameter syntax)
- Rust toolchain (for Rust target)
- Dart SDK (for Dart target)
- Erlang/OTP with rebar3 (for BEAM target)
