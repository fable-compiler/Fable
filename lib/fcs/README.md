# FSharp.Compiler.Service

These binaries come from Fickle's FCS fork, <https://github.com/shayanhabibi/fickle-fsharp>, branch
`fickle_slim`. That branch follows `ncave/fsharp@service_slim` (Fable's FCS) and targets net10.0 only.

Regenerate them with:

```bash
dotnet run --project src/Fickle.Build -- fcs-fork
```

It clones the fork next to this repository if it is missing (`--repo` points elsewhere), builds
`src/Compiler` in Release, and copies `FSharp.Compiler.Service.*`, `FSharp.DependencyManager.Nuget.*` and
`FSharp.Core.*` from `artifacts/bin/FSharp.Compiler.Service/Release/net10.0/` into this folder.
