# FAQ

## How does this relate to FunScript?

Fabel takes a lot from FunScript and probably it wouldn't have been possible if FunScript didn't come first.
But besides that it's completely a different project, the main differences are:

- Uses F# Compiler Services to generate the AST instead of quotations, this means it's not necessary to
  compile to .NET bytecode first.
- It doesn't generate JS code directly, but rather transforms the F# into a new one which is passed to
  Babel.js. This way it's easier to target different EcmaScript versions or module systems when needed. 
- One of the main goals of FunScript was to keep the full F# semantics, like generics or structural comparison.
  Fabel sacrifices some of the semantics to generate cleaner and more idiomatic JS code.
- The generated code is structured using ES6 modules, making it much easier to interact with other JS
  development tools.
  
In any case, I want to show my admiration to FunScript (which was created before the F# compiler and Babel
exposed the language AST) and my gratitude to @ZachBray for his work :)