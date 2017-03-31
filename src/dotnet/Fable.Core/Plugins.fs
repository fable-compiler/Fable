namespace Fable
open Fable.AST

type IReplacePlugin =
    inherit IPlugin
    abstract TryReplace: com: ICompiler -> info: Fable.ApplyInfo -> Fable.Expr option
