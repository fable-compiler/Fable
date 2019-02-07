//----------------------------------------------------------------------------
// Copyright (c) 2002-2012 Microsoft Corporation. 
//
// This source code is subject to terms and conditions of the Apache License, Version 2.0. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// By using this source code in any fashion, you are agreeing to be bound 
// by the terms of the Apache License, Version 2.0.
//
// You must not remove this notice, or any other, from this software.
//----------------------------------------------------------------------------

//----------------------------------------------------------------------------
// SimpleSourceCodeServices API to the compiler is a simplified service for parsing,
// type checking, intellisense and compilation.
//----------------------------------------------------------------------------

namespace Microsoft.FSharp.Compiler.SimpleSourceCodeServices

open System.IO
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices


/// Provides simpler version of services for checking and compiling F# scripts
[<System.Obsolete("Please create an instance of FSharpChecker and use the corresponding method on that instance")>]
type SimpleSourceCodeServices = 

    [<System.Obsolete("Please create an instance of FSharpChecker and use the corresponding method on that instance")>]
    new: ?msbuildEnabled: bool -> SimpleSourceCodeServices

    [<System.Obsolete("Please create an instance of FSharpChecker and use the corresponding method on that instance")>]
    member TokenizeLine: line:string * state:int64 -> FSharpTokenInfo [] * int64

    [<System.Obsolete("Please create an instance of FSharpChecker and use the corresponding method on that instance")>]
    member TokenizeFile: source:string -> FSharpTokenInfo [] []

    [<System.Obsolete("Please create an instance of FSharpChecker and use the corresponding method on that instance")>]
    member Compile: argv:string [] -> FSharpErrorInfo [] * int
    
    [<System.Obsolete("Please create an instance of FSharpChecker and use the corresponding method on that instance")>]
    member Compile: ast:ParsedInput list * assemblyName:string * outFile:string * dependencies:string list * ?pdbFile:string * ?executable:bool * ?noframework:bool -> FSharpErrorInfo [] * int

    [<System.Obsolete("Please create an instance of FSharpChecker and use the corresponding method on that instance")>]
    member CompileToDynamicAssembly: otherFlags:string [] * execute:(TextWriter * TextWriter) option -> FSharpErrorInfo [] * int * System.Reflection.Assembly option

    [<System.Obsolete("Please create an instance of FSharpChecker and use the corresponding method on that instance")>]
    member CompileToDynamicAssembly: ast:ParsedInput list * assemblyName:string * dependencies:string list * execute:(TextWriter * TextWriter) option * ?debug:bool * ?noframework:bool -> FSharpErrorInfo [] * int * System.Reflection.Assembly option
            
