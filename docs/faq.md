 - tagline: Frequently asked questions about Fable
 
# FAQ

## How does this relate to FunScript?

Fable takes a lot from FunScript and probably it wouldn't have been possible if FunScript didn't come first.
But besides that it's completely a different project, the main differences are:

- Fable uses F# Compiler Services to generate the AST instead of quotations. This means it's not necessary to
  compile to .NET bytecode first.
- Fable doesn't generate JS code directly, rather transforms the F# AST into a new one which is passed to
  Babel.js. This way it's easier to target different EcmaScript versions or module systems when needed.
- One of the main goals of FunScript was to keep the full F# semantics, like generics or structural comparison.
  Fable sacrifices some of the semantics to generate cleaner and more idiomatic JS code.
- The generated code is structured using ES6 modules, making it much easier to interact with other JS
  development tools.
- Like FunScript, Fable _translates_ TypeScript definition files to build foreign interfaces, but the latter uses
  plain .fs files instead of .dll assemblies, making it easier to edit the files if needed.

In any case, I want to show my admiration to FunScript (which was created before the F# compiler and Babel
exposed the language AST) and my gratitude to @ZachBray for his work :)

## What was the best moment during development?

The best moment was when, after going through three different AST and JSON serialization,
the compiler correctly transformed `1 + 1` into `1 + 1`.

## What lessons have you learned developing the project?

While I was coding I prepared a cup of tea and drank it too quickly, burning my tongue.
I learnt I must check first the temperature of the cup before drinking.
