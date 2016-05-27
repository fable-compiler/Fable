namespace Fable.Plugins

#r "../../build/fable/bin/Fable.exe"

open Fable.AST
open Fable.FSharp2Fable

type LogAsyncErrorsPlugin() =
    let macro =
        """$0.start(function (builder_) {
            return builder_.delay(function () {
            return builder_.tryWith(builder_.delay(function () {
                return builder_.bind($1, function () {
                    return builder_.return();
                });
            }), function (ex) {
                console.log(console.log("Error in Async Workflow " + ex));
                return builder_.zero();
            });
          });
        }($0))"""
    interface IReplacePlugin with
        member x.TryReplace (com: Fable.ICompiler) (info: Fable.ApplyInfo) =
            match info.ownerFullName with
            | "Microsoft.FSharp.Control.Async" ->
                match info.methodName with
                | "start" | "startImmediate" ->
                    let import =
                        Fable.ImportRef ("Async", com.Options.coreLib)
                        |> Fable.Value
                    Fable.Replacements.Util.emit info macro (import::info.args)
                    |> Some
                | _ -> None
            | _ -> None
