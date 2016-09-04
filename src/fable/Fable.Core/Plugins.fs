namespace Fable
open Fable.AST

type IReplacePlugin =
    inherit IPlugin
    abstract TryReplace: com: ICompiler -> info: Fable.ApplyInfo -> Fable.Expr option

type IInjection =
    /// Must be a name with low probabilities of being duplicated
    abstract member Name: string
    abstract member ArgumentsLength: int
    abstract member GetBody: args: Fable.Expr list -> Fable.Expr

type IInjectPlugin =
    inherit IPlugin
    abstract Inject: com: ICompiler -> IInjection list

type IRewritePlugin = 
    inherit IPlugin
    abstract Rewrite : seq<Fable.File> -> seq<Fable.File>