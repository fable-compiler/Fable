# FAQ

This is the section for quick questions or to miss the holes in the documentation. Check the first question below to learn how you can help make the FAQ a great resource for Fable users.

## General

* How can I add a new question?

Make sure the question is not answered already either here or in the documentation. Then just [click here to edit this page in Github](https://github.com/fable-compiler/Fable/edit/master/docs/FAQ.md), add your question to the bottom and create a PR. Maintainers will write an answer and ask you if that solves the issue. If it does, the PR will be merged and the question will be added to the page so it can be referenced by other users.

## Compiler

* How can I run a certain piece of code only in my production build?

Using a [Compiler Directive](https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/compiler-directives).

```fsharp
#if !DEBUG
printfn "only printing in production bundle"
#endif
```

You can define compilation directives using the `define` option in your Fable client. Latest [fable-loader](https://www.npmjs.com/package/fable-loader) will automatically define `DEBUG` when running Webpack in development mode.
