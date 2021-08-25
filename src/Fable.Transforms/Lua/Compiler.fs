module rec Fable.Compilers.Lua

open Fable.AST
open Fable.AST.Fable

type LuaCompiler(com: Fable.Compiler) =
    let mutable types = Map.empty
    member this.Com = com
    member this.AddClassDecl (c: ClassDecl) =
        types <- types |> Map.add c.Entity c
    member this.GetByRef (e: EntityRef) =
        types |> Map.tryFind e
