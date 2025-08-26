# CLI

This reworked CLI uses Spectre.Console to manage the CLI interface.
Using the framework, we are able to reason about the settings more
intuitively, although it uses a C# like OOP approach.

The value provided by offloading the CLI abstraction to a library for
the Fable CLI tool is worth this cost.

## Backwards Compatibility

This implementation permits all operations being handled from the default
command, except for `clean`. This maintains backwards compatibility.

All previous switches and commands are supported the same.

Deprecated options such as `--verbose` and `--silent` are still functional,
although now hidden. These switches are superseded by the `--verbosity`
option. `--verbosity` takes precedence over the deprecated switches.

## Future Proofing

As more targets mature in Fable, the need to separate CLI options and switches
for the different targets will grow.

While the backing compilation system is not changed, the CLI interface is
designed for this eventuality.

```mermaid
flowchart
    subgraph Interfaces
        subgraph targetAgnostic [Target Agnostic Args]
            ICommonArgs
            ICompilingArgs
        end
        subgraph targetSpecific [Target Specific Args]
            IJavaScriptArgs
            ITypeScriptArgs
            IPythonArgs
            IRustArgs
            IPhpArgs
            IDartArgs
        end
        ICommonArgs --> ICompilingArgs
        ICompilingArgs --> IJavaScriptArgs
        ICompilingArgs --> ITypeScriptArgs
        ICompilingArgs --> IPythonArgs
        ICompilingArgs --> IRustArgs
        ICompilingArgs --> IPhpArgs
        ICompilingArgs --> IDartArgs
        IJavaScriptArgs --> ICliArgs
        ITypeScriptArgs --> ICliArgs
        IPythonArgs --> ICliArgs
        IRustArgs --> ICliArgs
        IPhpArgs --> ICliArgs
        IDartArgs --> ICliArgs
    end
    subgraph Settings
        ICliArgs --Superset of interfaces implemented
        by base settings class without
        exposing any options--> FableBase
        FableBase --Exposes all
        interface args
        as options--> Fable
        FableBase --Same as Fable
        but has watch switch on--> Watch
        FableBase --Only exposes
        the ICommonArgs subset
        of args as options--> Clean
        FableBase --Exposes common
        compiling args as
        options, but excludes
        options that are preset
        by commands like language
        and watch related options--> CompilingBase
        CompilingBase --Exposes JavaScript
        specific args as options--> JavaScript
        CompilingBase --> TypeScript
        JavaScript --Exposes watch related
        options--> JavaScriptWatch
        TypeScript --> TypeScriptWatch
        ...Others --> ...OthersWatch
        CompilingBase --> ...Others
    end
    Interfaces ---> Settings
```

This provides a structure that is compatible with the current
compiler backing, as all `Setting` types
are compatible with the superset of args.
This is a non issue for the CLI consumers,
as each `Setting` type only exposes their
specific options via attributes.

The structure lends itself to iteration
towards allowing targets to safely add
their own switches as desired, and also
create consuming functions of the CLI interface
for different targets to handle their
subset of Cli args only, with intellisense
completions that are not polluted by other
target language options.
