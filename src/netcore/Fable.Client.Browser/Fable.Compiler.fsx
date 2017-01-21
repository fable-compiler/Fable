#if FABLE_COMPILER && !DOTNETCORE && !DOTNET40
//#r "System.Threading.dll"
#r "System.Text.RegularExpressions.dll"
#endif

#load
        "../../../../FSharp.Compiler.Service_fable/src/fsharp/Fable.FCS/Fable.FCS.fsx"

        "../../fable/Fable.Core/Compiler.fs"
        "../../fable/Fable.Core/Util.fs"
        "../../fable/Fable.Core/AST/AST.Common.fs"
        "../../fable/Fable.Core/AST/AST.Fable.fs"
        "../../fable/Fable.Core/AST/AST.Fable.Util.fs"
        "../../fable/Fable.Core/AST/AST.Babel.fs"
        "../../fable/Fable.Core/Plugins.fs"
        "../../fable/Fable.Core/Fable.Core.fs"
        // "../../fable/Fable.Core/Import/Fable.Import.JS.fs"
        // "../../fable/Fable.Core/Import/Fable.Import.Browser.fs"
        // "../../fable/Fable.Core/Import/Fable.Import.Node.fs"
        // "../../fable/Fable.Core/Fable.Core.Extensions.fs"

        "../../fable/Fable.Compiler/Utils.fs"
        "../../fable/Fable.Compiler/Replacements.fs"
        "../../fable/Fable.Compiler/FSharp2Fable.Util.fs"
        "../../fable/Fable.Compiler/FSharp2Fable.fs"
        "../../fable/Fable.Compiler/Fable2Babel.fs"

        "Fable.Client.fs"
