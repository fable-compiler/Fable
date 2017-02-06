#if FABLE_COMPILER && !DOTNETCORE && !DOTNET40
//#r "System.Threading.dll"
#r "System.Text.RegularExpressions.dll"
#endif

#load
        "../../../../FSharp.Compiler.Service_fable/src/fsharp/fcs-fable/fcs-fable.fsx"

        "../Fable.Core/Compiler.fs"
        "../Fable.Core/Util.fs"
        "../Fable.Core/AST/AST.Common.fs"
        "../Fable.Core/AST/AST.Fable.fs"
        "../Fable.Core/AST/AST.Fable.Util.fs"
        "../Fable.Core/AST/AST.Babel.fs"
        "../Fable.Core/Plugins.fs"
        "../Fable.Core/Fable.Core.fs"
        // "../Fable.Core/Import/Fable.Import.JS.fs"
        // "../Fable.Core/Import/Fable.Import.Browser.fs"
        // "../Fable.Core/Import/Fable.Import.Node.fs"
        // "../Fable.Core/Fable.Core.Extensions.fs"

        "../Fable.Compiler/Utils.fs"
        "../Fable.Compiler/Replacements.fs"
        "../Fable.Compiler/FSharp2Fable.Util.fs"
        "../Fable.Compiler/FSharp2Fable.fs"
        "../Fable.Compiler/Fable2Babel.fs"

        "Fable.Client.fs"
