module Build.Main

open Fun.Build
open Build.FableLibrary

// Register fable-library pipelines
BuildFableLibraryJavaScript().Pipeline()
BuildFableLibraryRust().Pipeline()
BuildFableLibraryDart().Pipeline()
BuildFableLibraryPython().Pipeline()
BuildFableLibraryTypeScript().Pipeline()
BuildFableLibraryJavaScript().Pipeline()

// Register main tests
JavaScript.Tests.registerPipelines()
Python.Tests.registerPipelines()
Rust.Tests.registerPipelines()

// Register quick tests

tryPrintPipelineCommandHelp()
