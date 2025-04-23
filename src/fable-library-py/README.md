# Fable Library for Python

This module is used as the [Fable](https://fable.io/) library for Python.

This document outlines the distribution strategy for the Fable Python
target, which includes both Python code and Rust-based extensions for
features like unsigned integers that aren't natively supported in
Python.

The code should be annotated using [type
hints](https://docs.python.org/3/library/typing.html) and statically
type checked using
[pylance](https://marketplace.visualstudio.com/items?itemName=ms-python.vscode-pylance)
at `strict` setting.

The code should be formatted using the
[ruff](https://docs.astral.sh/ruff/) formatter with default settings.

## Fable Python Target Distribution Design

### Design Goals

- Provide proper type support for F# (including uint8, etc.) and
  performance using Rust extensions
- Support cross-platform usage of libraries compiled with Fable
- Offer flexibility for library authors (bundled vs PyPI dependency)
- Minimize duplication while ensuring stability

### Architecture

The system uses a two-tier architecture:

1. **Python components** - The main Fable library functionality
2. **Rust extensions** - Native code for performance and proper type support

### Distribution Approach

The `fable-library` package will be available through two methods:

1. **Bundled** - Generated and included directly with compiled F# code
2. **PyPI** - Installable as a dependency with wheels for all major platforms

Library authors can choose which approach to use based on their distribution needs. Note
that libraries made for distribution must use the PyPI approach since the bundled
version will only work locally (single architecture).

### Version Management

#### Major Versions

- Versions are aligned with main Fable releases and a new release will
  be generated for every tagged release
- Older versions for compatibility remain available through version pinning

### Implementation Notes

1. All Rust extensions are compiled for multiple platforms during the PyPI package build
