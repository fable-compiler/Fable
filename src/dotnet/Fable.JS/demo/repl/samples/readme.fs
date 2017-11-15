(*
# FABLE ONLINE REPL

Here you can have a quick look at how Fable translates
F# into JS without installing anything in your system.
The whole compilation is happening in your browser, the
code is not sent to any server. This is achieved by
compiling the whole F# compiler and Fable itself into JS
using Fable. Have fun!

Some notes:

- There may be some discrepancies between the online REPL
  and Fable CLI when versions differ.
- The samples just show some of Fable features and are not
  intended for learning F#.
- To bootstrap the compilation, it's necessary to download
  a few binary files containing F# metadata (primitive types,
  etc). Some firewalls may block this.
*)

let FSharp, JS = "F#", "JS"
let ♥ x y = x + " loves " + y

JS
|> ♥ FSharp
|> printfn "FABLE: %s"