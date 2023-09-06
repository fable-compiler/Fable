module Program

open Fun.Build
open Build.FableLibrary
open Build.Tests

BuildFableLibraryRust().Pipeline
BuildFableLibraryDart().Pipeline
BuildFableLibraryPython().Pipeline
BuildFableLibraryTypeScript().Pipeline
BuildFableLibraryJavaScript().Pipeline

TestsPython().Pipeline
TestsJavaScript().Pipeline

tryPrintPipelineCommandHelp()