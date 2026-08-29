# fable-standalone

Fable bootstrapping itself to compile F# code in JS-only environments (browser, node.js). Note this doesn't output JS code, but a JSON AST that must be transformed using Babel.

## `fableVersion`

`package.json` carries the version of Fable this bundle was built from:

```json
{
    "version": "3.0.0",
    "fableVersion": "5.14.0"
}
```

A precompiled library can only be read by the Fable that wrote it, so a host that passes
`precompiledInfo` to `CreateChecker` has to run `fable precompile` with that exact version. The npm
version does not say which one it is, and `manager.Version` only answers once the checker is already
loaded - too late to choose a compiler. Read this field instead, and pin `dotnet fable` to it.
