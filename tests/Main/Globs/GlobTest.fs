// Check that files can be imported via fsproj globbing patterns
// https://github.com/fable-compiler/Fable/issues/1942
module Fable.Tests.Glob

let hello name = sprintf "Hello %s from Glob" name