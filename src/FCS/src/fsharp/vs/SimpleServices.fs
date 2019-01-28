// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.SimpleSourceCodeServices

    open System
    open System.IO
    open Microsoft.FSharp.Compiler.SourceCodeServices
    open Microsoft.FSharp.Compiler.Ast

    /// Provides simple services for checking and compiling F# scripts
    type public SimpleSourceCodeServices(?msbuildEnabled) =

        let checker = FSharpChecker.Create(?msbuildEnabled=msbuildEnabled)

        member x.TokenizeLine (line, state) =  checker.TokenizeLine (line, state) 
        member x.TokenizeFile source = checker.TokenizeFile (source)
        member x.Compile (argv)  = checker.Compile (argv) |> Async.RunSynchronously

        member x.Compile (ast, assemblyName, outFile, dependencies, ?pdbFile, ?executable, ?noframework) =
            checker.Compile (ast, assemblyName, outFile, dependencies, ?pdbFile=pdbFile, ?executable=executable, ?noframework=noframework) |> Async.RunSynchronously

        member x.CompileToDynamicAssembly (otherFlags, execute)  = 
            checker.CompileToDynamicAssembly (otherFlags, execute) |> Async.RunSynchronously

        member x.CompileToDynamicAssembly (asts, assemblyName, dependencies, execute, ?debug, ?noframework) =
            checker.CompileToDynamicAssembly (asts, assemblyName, dependencies, execute, ?debug=debug, ?noframework=noframework) |> Async.RunSynchronously
